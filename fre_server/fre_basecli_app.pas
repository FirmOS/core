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
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils, CustApp,
  FRE_SYSTEM,FOS_DEFAULT_IMPLEMENTATION,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,
  FRE_DB_CORE,fre_db_serverdefaults,

  fre_dbbase,fre_openssl_cmd,

  fre_aps_comm_impl,
  FRE_DB_EMBEDDED_IMPL,
  FRE_CONFIGURATION,FRE_BASE_SERVER
  ;

type

  { TFRE_CLISRV_APP }

  TFRE_CLISRV_APP = class(TCustomApplication)
  private
    FAvailExtensionList  : IFOS_STRINGS;
    FChosenExtensionList : IFOS_STRINGS;
    FDefaultExtensions   : String;
    FDefaultStyle        : String;
    procedure SetDefaultExtensions(AValue: String);
    procedure SetDefaultStyle     (AValue: String);
  protected
    fapplication                   : string;
    filename                       : string;
    FUser                          : string;
    FPass                          : string;
    FDBName                        : string;
    FOnlyInitDB                    : boolean;
    FBaseServer                    : TFRE_BASE_SERVER;

    procedure  _CheckDBNameSupplied;
    procedure  _CheckUserSupplied;
    procedure  _CheckPassSupplied;

    function    ListFromString     (const str :string) : IFOS_STRINGS;
    procedure   ConvertDbo         (const file_name:string ; const to_json:boolean);
    procedure   DoRun              ; override ;
    procedure   PrintTimeZones     ;
    procedure   WriteHelp          ;
    procedure   WriteVersion       ;
    procedure   ReCreateDB         ;
    procedure   ReCreateSysDB      ;
    procedure   GenerateTestdata   ;
    procedure   DoUnitTest         ;
    procedure   ShowUserRoles      ;
    procedure   InitExtensions     ;
    procedure   ShowVersions       ;
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
    property    DefaultStyle       : String read FDefaultStyle      write SetDefaultStyle;
  end;


implementation

{$I fos_version_helper.inc}

{ TFRE_CLISRV_APP }

function TFRE_CLISRV_APP.ListFromString(const str: string): IFOS_STRINGS;
begin
  result := GFRE_TF.Get_FOS_Strings;
  result.SetCommatext(str);
end;

procedure TFRE_CLISRV_APP.ConvertDbo(const file_name: string; const to_json: boolean);
var dbo   : TFRE_DB_Object;
    fp,fn : string;
    res   : string;
begin
  if not FileExists(file_name) then
    begin
      writeln('the given file [',file_name,'] does not exist!');
      terminate;
      halt(1);
    end;
   if to_json then
     begin
       fp  := ExtractFilePath(file_name);
       fn  := ExtractFileName(file_name);
       fn  := copy(fn,1,Length(fn)-Length(ExtractFileExt(fn)));
       fn  := fp+DirectorySeparator+fn+'.frejson';
       if FileExists(fn) and
          not DeleteFile(fn) then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not delete convert file : '+fn);
       dbo := TFRE_DB_Object.CreateFromFile(file_name);
       res := dbo.GetAsJSONString(false,true,nil);
       GFRE_BT.StringToFile(fn,res);
       dbo.Finalize;
     end
   else
     begin
         fp  := ExtractFilePath(file_name);
         fn  := ExtractFileName(file_name);
         fn  := copy(fn,1,Length(fn)-Length(ExtractFileExt(fn)));
         fn  := fp+DirectorySeparator+fn+'.dbo';
         if FileExists(fn) and
            not DeleteFile(fn) then
              raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not delete convert file : '+fn);
         res := GFRE_BT.StringFromFile(filename);
         dbo := TFRE_DB_Object.CreateFromJSONString(res);
         dbo.SaveToFile(fn);
         dbo.Finalize;
     end;
end;

procedure TFRE_CLISRV_APP.SetDefaultExtensions(AValue: String);
begin
  FDefaultExtensions:=AValue;
end;

procedure TFRE_CLISRV_APP.SetDefaultStyle(AValue: String);
begin
  FDefaultStyle:=AValue;
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
  ErrorMsg:=CheckOptions('hvirlgxytqDf:e:u:p:d:s:U:H:',
                          ['help','version','init','remove','list','graph','forcedb','forcesysdb','testdata','dumpdb','debugger','file:','extensions:','user:','pass:',
                           'database:','style:','remoteuser:','remotehost:','drop-wal','show-users','test-log','disable-wal','disable-sync','dont-start','unittests','printtz','cleanzip','nozip','nocache','jsdebug','dbo2json:','json2dbo:','showinstalled']);

  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('*','printtz') then
    PrintTimeZones;
  if HasOption('D','debugger') then
    G_NO_INTERRUPT_FLAG:=true;

  if HasOption('*','drop-wal') then
    begin
      writeln('REQUESTED TO FORCE DROP WAL');
      GDROP_WAL := true;
    end;

  if HasOption('*','disable-wal') then
  begin
    writeln('GLOBALLY DISABLING WAL WRITES');
    GDISABLE_WAL := true;
  end;

  if HasOption('*','disable-sync') then
  begin
    writeln('GLOBALLY DISABLING SYNC WRITES');
    GDISABLE_SYNC := true;
  end;

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
  if HasOption('*','cleanzip') then
    cFRE_FORCE_CLEAN_ZIP_HTTP_FILES := true;
  if HasOption('*','nozip') then
    cFRE_BUILD_ZIP_HTTP_FILES := false;
  if HasOption('*','nocache') then
    cFRE_USE_STATIC_CACHE := false;

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


  // Diasable WAL MODE - DEBUG SETUP - FORCE SIMPLE DEPLOYMENT MODE

  GDISABLE_WAL              := TRUE;
  GDBPS_TRANS_WRITE_THROUGH := TRUE;
  GDISABLE_SYNC             := TRUE;
  GDROP_WAL                 := TRUE;
  //


  // NOW The initial startup is done (connections can be made, but no extensions initialized)
  PrepareStartup;

  if HasOption('*','test-log') then
    begin
      CfgTestLog;
    end;

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

  if HasOption('*','dbo2json') then begin
    filename := GetOptionValue('*','dbo2json');
    ConvertDbo(filename,true);
    Terminate;
    halt(0);
  end;

  if HasOption('*','json2dbo') then begin
    filename := GetOptionValue('*','json2dbo');
    ConvertDbo(filename,false);
    Terminate;
    halt(0);
  end;

  if HasOption('x','forcedb') then begin
    FOnlyInitDB:=true;
    ReCreateDB;
  end;

  if HasOption('y','forcesysdb') then begin
    FOnlyInitDB:=true;
    ReCreateSysDB;
  end;

  if HasOption('i','init') then begin
    FOnlyInitDB:=true;
    InitExtensions;
    GFRE_DB_PS_LAYER.SyncSnapshot(true);
  end;

  if HasOption('*','showinstalled') then
    begin
      ShowVersions;
      Terminate;
      Exit;
    end;

  if HasOption('r','remove') then begin
    FOnlyInitDB:=true;
    RemoveExtensions;
    Terminate;
    Exit;
  end;

  if HasOption('t','testdata') then begin
    FOnlyInitDB:=true;
    GenerateTestdata;
  end;

  if HasOption('*','unittests') then begin
    FOnlyInitDB:=true;
    DoUnitTest;
    Terminate;
    exit;
  end;

  if HasOption('s','style') then begin
    cFRE_WEB_STYLE := GetOptionValue('s','style');
  end else begin
    cFRE_WEB_STYLE := FDefaultStyle;
  end;

  if HasOption('*','jsdebug') then begin
    cFRE_JS_DEBUG := true;
  end else begin
    cFRE_JS_DEBUG := false;
  end;

  if HasOption('*','show-users') then
    begin
      ShowUserRoles;
      Terminate;
      Exit;
    end;


  if HasOption('q','dumpdb') then
    DumpDB;

  if HasOption('*','dont-start') then
    begin
      Terminate;
      exit;
    end;

  if HasOption('g','graph') then begin
    SchemeDump;
    Terminate;
    Exit;
  end;

  Setup_SSL_Interface;
  Setup_APS_Comm;
  FBaseServer := TFRE_BASE_SERVER.create(FDBName);
  FBaseServer.Setup;
  GFRE_SC.RunUntilTerminate;


  Teardown_APS_Comm;
  Terminate;
  FBaseServer.Free;
  GFRE_DB_PS_LAYER.Finalize;
  Cleanup_SSL_Interface;
  GFRE_BT.DeactivateJack;
  exit;
end;

procedure TFRE_CLISRV_APP.PrintTimeZones;
var cnt : NativeInt;
    sl  : TStringlist;
  procedure PrintTZ(const zonename, timezoneletters : String ; const sec_offset,fixed_offset : integer ; const valid_to_year,valid_to_month,valid_to_day,valid_to_secs_in_day:Integer ; const valid_until_gmt : boolean);
  begin
    inc(cnt);
    writeln(cnt:3,':',zonename:12,' ',timezoneletters:12,' offset ',sec_offset,' fixed ',fixed_offset,' valid until ',valid_to_year,':',valid_to_month,':',valid_to_day,':',valid_to_secs_in_day,' until GMT ',valid_until_gmt);
  end;
begin
  cnt:=0;
  GFRE_DT.ForAllTimeZones(@PrintTZ,true);
  writeln('----');
  sl:=TStringList.Create;
  try
    GFRE_DT.GetTimeZoneNames(sl);
    writeln(sl.Text);
  finally
    sl.free;
  end;
  halt(0);
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
  if GFRE_DB_PS_LAYER.DatabaseExists(FDBName) then
    CheckDbResult(GFRE_DB_PS_LAYER.DeleteDatabase(FDBName),'DELETE DB FAILED : '+FDBName);
  CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase(FDBName),'CREATE DB FAILED : '+FDBName);
end;

procedure TFRE_CLISRV_APP.ReCreateSysDB;
begin
  if GFRE_DB_PS_LAYER.DatabaseExists('SYSTEM') then
    CheckDbResult(GFRE_DB_PS_LAYER.DeleteDatabase('SYSTEM'),'DELETE SYSTEM DB FAILED');
  CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase('SYSTEM'),'CREATE SYSTEM DB FAILED');
  gFRE_InstallServerDefaults;
end;

procedure TFRE_CLISRV_APP.GenerateTestdata;
begin
  _CheckDBNameSupplied;
  //_CheckUserSupplied;
  //_CheckPassSupplied;
  GFRE_DBI_REG_EXTMGR.GenerateTestData4Exts(FChosenExtensionList,FDBName,FUser,FPass);
end;

procedure TFRE_CLISRV_APP.DoUnitTest;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  //_CheckUserSupplied;
  //_CheckPassSupplied;
  GFRE_DBI_REG_EXTMGR.GenerateUnitTestsdata(FChosenExtensionList,FDBName,FUser,FPass);
end;

procedure TFRE_CLISRV_APP.ShowUserRoles;
var conn  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;

begin
  CONN := GFRE_DBI.NewSysOnlyConnection;
  try
    res  := CONN.Connect('admin@'+CFRE_DB_SYS_DOMAIN_NAME,'admin');
    if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);
    abort;

    // implement generic for all users

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
  //writeln('InitDB for extensions :'+uppercase(FChosenExtensionList.Commatext));
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect('admin@'+CFRE_DB_SYS_DOMAIN_NAME,'admin'),'cannot connect system db');
  GFRE_DBI.DBInitializeAllExClasses(conn);
  conn.Finalize;
  GFRE_DBI_REG_EXTMGR.InitDatabase4Extensions(FChosenExtensionList,FDBName,FUser,FPass);
end;

procedure TFRE_CLISRV_APP.ShowVersions;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckUserSupplied;
  _CheckPassSupplied;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect(FUser,FPass),'cannot connect system db');
  writeln(conn.GetClassesVersionDirectory.DumpToString);
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
  //writeln('STARTUP @LOCAL TIME :',GFRE_DT.ToStrFOS(GFRE_DT.UTCToLocalTime(GFRE_DT.Now_UTC,GFRE_DBI.LocalZone)),'  UTC TIME :',GFRE_DT.ToStrFOS(GFRE_DT.Now_UTC));
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
  CheckDbResult(CONN.Connect('admin@'+CFRE_DB_SYS_DOMAIN_NAME,'admin'),'cannot connect system db');
  CONN.DumpSystem;
  CONN.Finalize;
end;

procedure TFRE_CLISRV_APP.CfgTestLog;

  procedure Setup_HTTP_Request_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_ZIP],fll_Debug,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_CACHE],fll_Debug,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_REQ],fll_Info,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_REQ],fll_Debug,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_RES],fll_Info,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_RES],fll_Debug,'*',flra_DropEntry);
  end;

  procedure Setup_DB_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Debug,'*',flra_DropEntry);
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Warning,'*',flra_DropEntry);
  end;

  procedure Setup_Server_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry);     // DROP : Server / DEBUG
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER_DATA],fll_Debug,'*',flra_DropEntry);// DROP : Server / Dispatch / Input Output
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Info,'*',flra_DropEntry);      // DROP : Server / INFO
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Notice,'*',flra_DropEntry);    // DROP : Server / NOTICE
  end;

  procedure Setup_WS_Session_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WEBSOCK],fll_Debug,'*',flra_DropEntry);    // DROP : Websock / JSON / IN / OUT
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WS_JSON],fll_Debug,'*',flra_DropEntry);    // DROP : JSON
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SESSION],fll_Debug,'*',flra_DropEntry);    // DROP SESSION  DEBUG
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SESSION],fll_Info,'*',flra_DropEntry);     // DROP SESSION INFO
  end;

  procedure Setup_APS_COMM_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Info,'*',flra_DropEntry);     // DROP APSCOMM INFO
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Debug,'*',flra_DropEntry);    // DROP APSCOMM DEBUG
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Notice,'*',flra_DropEntry);
  end;

  procedure Setup_Persistance_Layer_Logging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSITANCE],fll_Info,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSITANCE],fll_Debug,'*',flra_DropEntry);
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSITANCE_NOTIFY],fll_Info,'*',flra_DropEntry);
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSITANCE_NOTIFY],fll_Debug,'*',flra_DropEntry);
  end;

  procedure Setup_FlexcomLogging;
  begin
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_FLEXCOM],fll_Debug,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_FLEX_IO],fll_Debug,'*',flra_DropEntry);
  end;

begin
  Setup_DB_Logging;
  Setup_HTTP_Request_Logging;
  Setup_Server_Logging;
  Setup_WS_Session_Logging;
  Setup_APS_COMM_Logging;
  Setup_Persistance_Layer_Logging;
  Setup_FlexcomLogging;
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
      sconn.Connect('admin@'+CFRE_DB_SYS_DOMAIN_NAME,'admin');
      sconn.DrawScheme(mems);
    end else begin
      lconn := GFRE_DBI.NewConnection;
      res   := lconn.Connect(FDBName,'admin@'+CFRE_DB_SYS_DOMAIN_NAME,'admin');
      if res<>edb_OK then begin
        WriteLn('SCHDUMP CHECK CONNECT FAILED : ',CFRE_DB_Errortype[res]);
      end;
      lconn.sys.DrawScheme(mems);
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
  FDefaultStyle        := 'firmos';
  FChosenExtensionList := GFRE_TF.Get_FOS_Strings;
end;

destructor TFRE_CLISRV_APP.Destroy;
begin
  inherited Destroy;
end;

end.

