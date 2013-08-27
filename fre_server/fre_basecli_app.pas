unit fre_basecli_app;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CustApp,
  FRE_SYSTEM,FOS_DEFAULT_IMPLEMENTATION,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,
  FRE_DB_CORE,fre_db_serverdefaults,FRE_DB_SYSRIGHT_CONSTANTS,

  fre_dbbase,

  FRE_APS_IMPL_LE, FOS_FCOM_DEFAULT,FRE_DB_EMBEDDED_IMPL,
  FRE_CONFIGURATION,FRE_BASE_SERVER
  ;

type

  { TFRE_CLISRV_APP }

  TFRE_CLISRV_APP = class(TCustomApplication)
  private
    FAvailExtensionList  : IFOS_STRINGS;
    FChosenExtensionList : IFOS_STRINGS;
    FDefaultExtensions: String;
    procedure SetDefaultExtensions(AValue: String);
  protected
    fapplication                   : string;
    filename                       : string;
    FUser                          : string;
    FPass                          : string;
    FDBName                        : string;
    FOnlyInitDB                    : boolean;

    procedure  _CheckDBNameSupplied;
    procedure  _CheckUserSupplied;
    procedure  _CheckPassSupplied;

    function    ListFromString (const str :string) : IFOS_STRINGS;
    procedure   DoRun              ; override ;
    procedure   WriteHelp          ;
    procedure   WriteVersion       ;
    procedure   ReCreateDB         ;
    procedure   ReCreateSysDB      ;
    procedure   ReCreateTestUser   ;
    procedure   ShowTestUserRoles  ;
    procedure   InitExtensions     ;
    procedure   RemoveExtensions   ;
    procedure   RegisterExtensions ;
    procedure   VerifyExtensions   ;
    procedure   ListExtensions     ;
    procedure   PrepareStartup     ;
    procedure   FinishStartup      ;
    procedure   DumpDB             ;
    procedure   CfgTestLog         ;
    procedure   SchemeDump         ;
  public
    constructor Create             (TheOwner: TComponent); override;
    destructor  Destroy            ; override;
    property    DefaultExtensions  : String read FDefaultExtensions write SetDefaultExtensions;
  end;


implementation

{$I fos_version_helper.inc}


{ TFRE_CLISRV_APP }

function TFRE_CLISRV_APP.ListFromString(const str: string): IFOS_STRINGS;
begin
  result := GFRE_TF.Get_FOS_Strings;
  result.SetCommatext(str);
end;

procedure TFRE_CLISRV_APP.SetDefaultExtensions(AValue: String);
begin
  FDefaultExtensions:=AValue;
end;

procedure TFRE_CLISRV_APP._CheckDBNameSupplied;
begin
  if (FDBName='') then begin
    writeln('no database name supplied for extension initialization');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckUserSupplied;
begin
  if (FUser='') then begin
    writeln('no username supplied for extension initialization');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckPassSupplied;
begin
  if (FPass='') then begin
    writeln('no password supplied for extension initialization');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP.DoRun;
var ErrorMsg : String;
begin
  // OPTIONS without args are first then OPTIONS with arguments are listed, same order for full and one letter options, watch the colon count
  ErrorMsg:=CheckOptions('hvirlgxytqDf:e:u:p:d:s:U:H:',['help','version','init','remove','list','graph','forcedb','forcesysdb','testuser','dumpdb','debugger','file:','extensions:','user:','pass:','database:','style:','remoteuser:','remotehost:','drop-wal','show-users','test-log']);
  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('D','debugger') then
    G_NO_INTERRUPT_FLAG:=true;

  if HasOption('f','file') then begin
    filename := GetOptionValue('f','filename');
  end else begin
    filename := 'output';
  end;


  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;
  if HasOption('v','version') then begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  if HasOption('e','extensions') then begin // has to be after possible recreate db
    FChosenExtensionList :=  ListFromString(GetOptionValue('e','extensions'));
    VerifyExtensions;
  end else begin
    FChosenExtensionList := ListFromString(DefaultExtensions);
    VerifyExtensions;
  end;

  if HasOption('u','user') then begin
    FUser := GetOptionValue('u','user');
    cG_OVERRIDE_USER := FUser;
  end;

  if HasOption('p','pass') then begin
    FPass := GetOptionValue('p','pass');
    cG_OVERRIDE_PASS := FPass;
  end;

  if HasOption('d','database') then begin
    FDBName := GetOptionValue('d','database');
  end else begin
    FDBName := 'ADMIN_DB';
  end;

  if HasOption('l','list') then begin
    ListExtensions;
    Terminate;
    Exit;
  end;


  // NOW The initial startup is done (connections can be made, but no extensions initialized)
  PrepareStartup;
  RegisterExtensions;

  if HasOption('U','remoteuser') then begin
    cFRE_REMOTE_USER := GetOptionValue('U','remoteuser');
  end;

  if HasOption('H','remotehost') then begin
    cFRE_REMOTE_HOST:= GetOptionValue('H','remotehost');
  end else begin
    cFRE_REMOTE_HOST:= '127.0.0.1';
  end;

  FinishStartup;

  if HasOption('x','forcedb') then begin
    FOnlyInitDB:=true;
    ReCreateDB;
  end;

  if HasOption('y','forcesysdb') then begin
    FOnlyInitDB:=true;
    ReCreateSysDB;
  end;

  if HasOption('t','testuser') then begin
    FOnlyInitDB:=true;
    ReCreateTestUser;
    ShowTestUserRoles;
  end;

  if HasOption('s','style') then begin
    cFRE_WEB_STYLE := GetOptionValue('s','style');
  end;

  if HasOption('i','init') then begin
    FOnlyInitDB:=true;
    InitExtensions;
    ShowTestUserRoles;
    GFRE_DB_DEFAULT_PS_LAYER.SyncSnapshot(true);
    writeln('>INITIALIZATION DONE');
  end;

  if HasOption('r','remove') then begin
    FOnlyInitDB:=true;
    RemoveExtensions;
    ShowTestUserRoles;
    Terminate;
    Exit;
  end;

  if HasOption('*','drop-wal') then
    begin
      writeln('REQUESTED TO FORCE DROP WAL');
      GDROP_WAL := true;
    end;

  if HasOption('*','show-users') then
    begin
      ShowTestUserRoles;
      Terminate;
      Exit;
    end;

  if HasOption('*','test-log') then
    begin
      writeln('configuring testlogging');
      CfgTestLog;
    end;



  if HasOption('q','dumpdb') then begin
    DumpDB;
  end;

  //if FOnlyInitDB then begin
  //  Terminate;
  //  exit;
  //end;

  if HasOption('g','graph') then begin
    SchemeDump;
    Terminate;
    Exit;
  end;
  SetupAPS;
  GFRE_S.Start(TFRE_BASE_SERVER.Create(FDBName));
  GFRE_S.Run;
  TearDownAPS;
  Shutdown_Done;
  Terminate;
  exit;
end;

procedure TFRE_CLISRV_APP.WriteHelp;
begin
  WriteVersion;
  writeln('Usage: ',ExtractFileName(ExeName),' [OPTIONS]');
  writeln(' OPTIONS');
  writeln('  -h            | --help                 : print this help');
  writeln('  -e <ext,..>   | --extensions <ext,..>  : load database extensions');
  writeln('  -g            | --graph                : graphical dump (system without extensions)');
  writeln('  -f <filename> | --file <filename>      : filename for dump');
  writeln('  -u <user>     | --user <user>          : specify user');
  writeln('  -p <password> | --pass <password>      : specify password');
  writeln('  -d <database> | --database <database>  : specify database');
  writeln('  -q            | --dumpdb               : dump systemdb and (specified) db');
  writeln('  -x            | --forcedb              : recreates specified database (CAUTION)');
  writeln('  -y            | --forcesysdb           : recreates system database (CAUTION)');
  writeln('  -t            | --testuser             : creates test user');
  writeln('  -i            | --init                 : init a new database with extensions');
  writeln('  -r            | --remove               : remove extensions from system database (CAUTION)');
  writeln('  -l            | --list                 : list available applications');
  writeln('  -s            | --style                : use the given style (default: firmos)');
  writeln('  -U            | --remoteuser           : user for remote commands');
  writeln('  -H            | --remotehost           : host for remote commands');
  writeln('  -v            | --version              : print version info');
  writeln('  -D            | --debugger             : debugger (NOINT)');
  writeln;
end;

procedure TFRE_CLISRV_APP.WriteVersion;
begin
  writeln(GFOS_VHELP_GET_VERSION_STRING);
end;

procedure TFRE_CLISRV_APP.ReCreateDB;
begin
  _CheckDBNameSupplied;
  writeln('DELETE DB ',FDBName,' ',GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase(FDBName));
  writeln('CREATE DB ',FDBName,' ',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase(FDBName));
end;

procedure TFRE_CLISRV_APP.ReCreateSysDB;
begin
  writeln('DELETE SYSTEM ',GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase('SYSTEM'));
  writeln('CREATE SYSTEM ',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('SYSTEM'));
  gFRE_InstallServerDefaults;
end;

procedure TFRE_CLISRV_APP.ReCreateTestUser;

type _tusertype = (utguest,utuser,utadmin,utdemo);

var conn  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;

    procedure _AddUser(const user: string; const domain : TFRE_DB_NameType; const usertype:_tusertype;firstname:string='';lastname: string='');
    var passwd : string;
        login  : string;
        ims    : TFRE_DB_Stream;
    begin
      login := user+'@'+domain;
      case usertype of
       utadmin: passwd:='a1234';
       utuser:  passwd:='u1234';
       utdemo:  passwd:='demo1234';
      end;
      if lastname='' then lastname:='Lastname '+user;
      if firstname='' then firstname:='Firstname '+user;

      if conn.UserExists(login) then CheckDbResult(conn.DeleteUser(login),'cannot delete user '+login);
      CheckDbResult(conn.AddUser(login,passwd,firstname,lastname),'cannot add user '+login);

      ims := TFRE_DB_Stream.Create;
      ims.LoadFromFile(cFRE_SERVER_WWW_ROOT_DIR+'/fre_css/'+ cFRE_WEB_STYLE + '/images/LOGIN.png');
      conn.ModifyUserImage(login,ims);

      case usertype of
        utadmin: begin
          CheckDbResult(conn.ModifyUserGroups(login,TFRE_DB_StringArray.Create(cSYSUG_ADMIN_USERS+'@'+domain)),'cannot set usergroups '+login);
        end;
        utuser,utdemo: begin
          CheckDbResult(conn.ModifyUserGroups(login,TFRE_DB_StringArray.Create(cSYSUG_DB_USERS+'@'+domain)),'cannot set usergroups '+login);
        end;
      end;
    end;

begin
  _CheckDBNameSupplied;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  try
    res  := CONN.Connect('admin@'+cSYS_DOMAIN,'admin');
    if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);
      writeln('delete test domains');
      if conn.DomainExists('firmos') then CheckDbResult(conn.DeleteDomain('firmos'),'cannot delete domain firmos');
      if conn.DomainExists('fpc') then CheckDbResult(conn.DeleteDomain('fpc'),'cannot delete domain fpc');
      if conn.DomainExists('guest') then CheckDbResult(conn.DeleteDomain('guest'),'cannot delete domain guest');

      CheckDbResult(conn.AddDomain('firmos','FirmOS Domain','FirmOS Domain'),'cannot add domain firmos');
      CheckDbResult(conn.AddDomain('fpc','FPC Domain','FPC Domain'),'cannot add domain fpc');
      CheckDbResult(conn.AddDomain('demo','Demo Domain','Demo Domain'),'cannot add domain demo');

      writeln('create test users');

      _AddUser('admin1',cSYS_DOMAIN,utadmin);
      _AddUser('admin2',cSYS_DOMAIN,utadmin);
      _AddUser('feeder',cSYS_DOMAIN,utadmin);

      _AddUser('user1',cSYS_DOMAIN,utuser);
      _AddUser('user2',cSYS_DOMAIN,utuser);

      _AddUser('demo1',cSYS_DOMAIN,utdemo);
      _AddUser('demo2',cSYS_DOMAIN,utdemo);

      _AddUser('myadmin','demo',utadmin);
      _AddUser('user1','demo',utuser);
      _AddUser('user2','demo',utuser);

      _AddUser('myadmin','fpc',utadmin);
      _AddUser('user1','fpc',utuser);
      _AddUser('user2','fpc',utuser);

      _AddUser('myadmin','firmos',utadmin);
      _AddUser('user1','firmos',utuser);
      _AddUser('user2','firmos',utuser);
      _AddUser('hhartl','firmos',utadmin,'Helmut','Hartl');
      _AddUser('fschober','firmos',utadmin,'Franz','Schober');
      _AddUser('ckoch','firmos',utadmin,'Christian','Koch');

      CheckDbResult(conn.ModifyUserGroups('guest'+'@'+cSYS_DOMAIN,TFRE_DB_StringArray.Create(cSYSUG_DB_GUESTS+'@'+cSYS_DOMAIN)),'cannot set usergroups guest');

    finally
      conn.Finalize;
    end;
end;

procedure TFRE_CLISRV_APP.ShowTestUserRoles;
var conn  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;
    user  : IFRE_DB_USER;
    i     : integer;
    login : string;

begin
  CONN := GFRE_DBI.NewSysOnlyConnection;
  try
    res  := CONN.Connect('admin@'+cSYS_DOMAIN,'admin');
    if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);
    for i:= 1 to 3 do begin
      login  := 'admin'+inttostr(i)+'@'+cSYS_DOMAIN;
      if conn.fetchuser(login,user)=edb_OK then begin
        writeln(login+' : ');
        writeln('IMG : ',(user.Implementor_HC as TFRE_DB_Object).field('picture').AsString);
        writeln(GFRE_DBI.StringArray2String(user.GetRightsArray));
        writeln('');
      end;
    end;

    for i:= 1 to 3 do begin
      login  := 'user'+inttostr(i)+'@'+cSYS_DOMAIN;
      if conn.fetchuser(login,user)=edb_OK then begin
        writeln(login+' : ');
        writeln('IMG : ',(user.Implementor_HC as TFRE_DB_Object).field('picture').AsString);
        writeln(GFRE_DBI.StringArray2String(user.GetRightsArray));
      end;
     end;

    if conn.fetchuser('guest'+'@'+cSYS_DOMAIN,user)=edb_OK then begin
      writeln('GUEST : ');
      writeln('IMG : ',(user.Implementor_HC as TFRE_DB_Object).field('picture').AsString);
      writeln(GFRE_DBI.StringArray2String(user.GetRightsArray));
    end;
  finally
    conn.Finalize;
  end;
end;

procedure TFRE_CLISRV_APP.InitExtensions;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  //_CheckUserSupplied;
  //_CheckPassSupplied;
  writeln('InitDB for extensions :'+uppercase(FChosenExtensionList.Commatext));
  GFRE_DBI_REG_EXTMGR.InitDatabase4Extensions(FChosenExtensionList,FDBName,FUser,FPass);
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect('admin@'+cSYS_DOMAIN,'admin'),'cannot connect system db');
  GFRE_DBI.DBInitializeAllExClasses(conn);
  conn.Finalize;
end;


procedure TFRE_CLISRV_APP.RemoveExtensions;
begin
  _CheckDBNameSupplied;
  //_CheckUserSupplied;
  //_CheckPassSupplied;
  writeln('Remove apps for extensions :'+uppercase(FChosenExtensionList.Commatext));
  GFRE_DBI_REG_EXTMGR.Remove4Extensions(FChosenExtensionList,FDBName,FUser,FPass);
end;


procedure TFRE_CLISRV_APP.RegisterExtensions;
begin
  FRE_DBBASE.Register_DB_Extensions;
  GFRE_DBI_REG_EXTMGR.RegisterExtensions4DB(FChosenExtensionList);
end;

procedure TFRE_CLISRV_APP.VerifyExtensions;
var i          : integer;
    FCleanList : IFOS_STRINGS;
begin
  FCleanList := GFRE_TF.Get_FOS_Strings;
  for i:=0 to FChosenExtensionList.Count-1 do begin
    if FAvailExtensionList.IndexOf(FChosenExtensionList[i])>-1 then begin
      FCleanList.Add(uppercase(FChosenExtensionList[i]));
    end else begin
      writeln('ignoring invalid extension : ',FChosenExtensionList[i]);
    end;
  end;
  FChosenExtensionList := FCleanList;
end;

procedure TFRE_CLISRV_APP.ListExtensions;
begin
  writeln('Available Extensions:');
  writeln(FAvailExtensionList.Commatext);
  writeln('Chosen Extensions:');
  writeln(FChosenExtensionList.Commatext);
end;

procedure TFRE_CLISRV_APP.PrepareStartup;
begin
  Initialize_Read_FRE_CFG_Parameter;
  InitEmbedded;
  Init4Server;
  GFRE_DBI.LocalZone := cFRE_SERVER_DEFAULT_TIMEZONE;
  writeln('STARTUP @LOCAL TIME :',GFRE_DT.ToStrFOS(GFRE_DT.UTCToLocalTime(GFRE_DT.Now_UTC,GFRE_DBI.LocalZone)),'  UTC TIME :',GFRE_DT.ToStrFOS(GFRE_DT.Now_UTC));
//TODO SUPRESS INTERRUPTS (lazarus?)->  if CMD_CheckParam('NOINT') then G_NO_INTERRUPT_FLAG := true;
end;

procedure TFRE_CLISRV_APP.FinishStartup;
begin
  FRE_BASE_SERVER.RegisterLogin;
end;

procedure TFRE_CLISRV_APP.DumpDB;
var
  CONN: IFRE_DB_SYS_CONNECTION;
begin
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect('admin@'+cSYS_DOMAIN,'admin'),'cannot connect system db');
  CONN.DumpSystem;
  CONN.Finalize;
end;

procedure TFRE_CLISRV_APP.CfgTestLog;
begin
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry);  // Server / Connection Start/Close
  GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTPSRV],fll_Info,'*',flra_DropEntry); // Http/Header / Content
  GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTPSRV],fll_Debug,'*',flra_DropEntry); // Http/Header / Content
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry); // Server / Dispatch / Input Output
  GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WEBSOCK],fll_Debug,'*',flra_DropEntry); // Websock / JSON / IN / OUT
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSITANCE],fll_Debug,'*',flra_DropEntry); // Persistance Layer Debugging
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Debug,'*',flra_DropEntry); // Database /Filter / Layer Debugging
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_LogToOnConsole,false); // All To Console
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_DropEntry); // No File  Logging
  GFRE_LOG.DisableSyslog;
end;

procedure TFRE_CLISRV_APP.SchemeDump;
var lconn   : IFRE_DB_CONNECTION;
    sconn   : IFRE_DB_SYS_CONNECTION;
    res     : TFRE_DB_Errortype;
    mems    : TMemorystream;
    system  : boolean;
begin
  _CheckDBNameSupplied;

  if filename='' then begin
    writeln('No filename for graph set !');
    exit;
  end;

  if (FChosenExtensionList.Count=0) then begin
    writeln('SchemeDump for system database');
    system := true;
  end else begin
    writeln('SchemeDump for extensions :'+uppercase(FChosenExtensionList.Commatext));
    system := false;
  end;

  mems  := TMemorystream.Create;
  try
    if system then begin
      sconn := GFRE_DBI.NewSysOnlyConnection();
      sconn.Connect('admin','admin');
      //sconn.DrawScheme(mems);
    end else begin
      lconn := GFRE_DBI.NewConnection;
      res   := lconn.Connect(FDBName,'admin','admin');
      if res<>edb_OK then begin
        WriteLn('SCHDUMP CHECK CONNECT FAILED : ',CFRE_DB_Errortype[res]);
      end;
      //lconn.DrawScheme(mems);
      lconn.Finalize;
    end;
    mems.SaveToFile(filename);
  finally
    mems.free;
  end;
end;

constructor TFRE_CLISRV_APP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException      :=True;
  FAvailExtensionList  := GFRE_DBI_REG_EXTMGR.GetExtensionList;
  FAvailExtensionList.SetCaseSensitive(false);
  FChosenExtensionList := GFRE_TF.Get_FOS_Strings;
end;

destructor TFRE_CLISRV_APP.Destroy;
begin
  inherited Destroy;
end;

end.

