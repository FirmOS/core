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
  FRE_DB_CORE,

  fre_dbbase,fre_openssl_cmd,

  fre_aps_comm_impl,
  fre_net_pl_client, { network ps layer}
  fre_db_persistance_fs_simple, { filesystem ps layer}
  FRE_CONFIGURATION,FRE_BASE_SERVER,
  fre_db_core_transdata
  ;

type

  { TFRE_CLISRV_APP }

  TFRE_CLISRV_APP = class(TCustomApplication)
  private
    FAvailExtensionList  : IFOS_STRINGS;
    FChosenExtensionList : IFOS_STRINGS;
    FShortOpts           : IFOS_STRINGS;
    FLongOpts            : IFOS_STRINGS;
    FHelpOpts            : IFOS_STRINGS;
    FDefaultExtensions   : String;
    FDefaultStyle        : String;
    procedure   SetDefaultExtensions(AValue: String);
    procedure   SetDefaultStyle     (AValue: String);
  protected
    fapplication                   : string;
    FDBName                        : string;
    FOnlyInitDB                    : boolean;
    FBaseServer                    : TFRE_BASE_SERVER;
    FLimittransfer                 : integer;

    procedure   AddCheckOption                          (const short_option,long_option,helpentry : string);
    procedure   AddHelpOutLine                          (const msg:String='');
    function    GetShortCheckOptions                    : String; virtual;    { Setup of cmd line options short }
    function    GetLongCheckOptions                     : TStrings; virtual;  { Setup of cmd line options long }

    procedure  _CheckDBNameSupplied;
    procedure  _CheckAdminUserSupplied;
    procedure  _CheckAdminPassSupplied;
    procedure  _CheckUserSupplied;
    procedure  _CheckPassSupplied;

    procedure   DoRun                   ; override ;
    procedure   OrderedShutDown         ;

    function    PreStartupTerminatingCommands: boolean        ; virtual;            { cmd's that should be executed without db(ple), they terminate}
    function    AfterConfigStartupTerminatingCommands:boolean ; virtual;            { cmd's that should be executed after the reading of cfg file, but before db core init}
    function    AfterStartupTerminatingCommands:boolean       ; virtual;            { cmd's that should be executed with db core init, they terminate}
    function    AfterInitDBTerminatingCommands:boolean        ; virtual;            { cmd's that should be executed with full db init, they terminate}
    procedure   ParseSetSystemFlags                           ; virtual;            { Setting of global flags before startup go here }
    procedure   AddCommandLineOptions                         ; virtual;            { override for custom options/flags/commands}

    procedure   WriteHelp                               ; virtual;
    procedure   WriteVersion                            ; virtual;

    function    ListFromString          (const str :string) : IFOS_STRINGS;
    procedure   ConvertDbo              (const file_name:string ; const to_json:boolean);
    procedure   PrintTimeZones     ;
    procedure   ReCreateDB         ;
    procedure   ReCreateSysDB      ;
    procedure   BackupDB           (const adb, sdb: boolean; const dir: string);
    procedure   RestoreDB          (const adb, sdb: boolean; const dir: string);
    procedure   GenerateTestdata   ;
    procedure   DoUnitTest         ;
    procedure   InitExtensions     ;
    procedure   ShowVersions       ;
    procedure   ShowRights         ;
    procedure   RemoveExtensions   ;
    procedure   RegisterExtensions ;
    procedure   VerifyExtensions   ;
    procedure   ListExtensions     ;
    procedure   PrepareStartup     ;
    procedure   CfgTestLog         ;
    procedure   EndlessLogTest     ;
    procedure   SchemeDump         (const filename:string='output');
    procedure   DumpAll            (const filterstring: string);
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
         res := GFRE_BT.StringFromFile(file_name);
         dbo := TFRE_DB_Object.CreateFromJSONString(res);
         dbo.SaveToFile(fn);
         dbo.Finalize;
     end;
end;

procedure TFRE_CLISRV_APP.ParseSetSystemFlags;
begin
   if HasOption('*','setasyncwt') then
     begin
       GDBPS_TRANS_WRITE_ASYNC := GetOptionValue('*','setasyncwt')='on';
       writeln('SETTING GDBPS_TRANS_WRITE_ASYNC TO : ',GDBPS_TRANS_WRITE_ASYNC);
     end;
   if HasOption('*','dropwal') then
     begin
       writeln('REQUESTED TO FORCE DROP WAL');
       GDROP_WAL := true;
     end;

   FLimittransfer := 0;
   if HasOption('*','limittransfer') then
     begin
       FLimittransfer:=StrToIntDef(GetOptionValue('*','limittransfer'),-1);
       if FLimittransfer=-1 then
         begin
           writeln('TRANSFER LIMITING FAILED, SYNTAX');
           FLimittransfer:=0;
         end;
     end;
   if HasOption('*','cleanzip') then
     cFRE_FORCE_CLEAN_ZIP_HTTP_FILES := true;
   if HasOption('*','nozip') then
     cFRE_BUILD_ZIP_HTTP_FILES := false;
   if HasOption('*','nocache') then
     cFRE_USE_STATIC_CACHE := false;
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
end;

function TFRE_CLISRV_APP.GetShortCheckOptions: String;
var
  i: NativeInt;
begin
  for i:=0 to FShortOpts.Count-1 do
    result := result+FShortOpts[i];
end;

function TFRE_CLISRV_APP.GetLongCheckOptions: TStrings;
begin
  result := FLongOpts.AsTStrings;
end;

procedure TFRE_CLISRV_APP.AddCheckOption(const short_option, long_option, helpentry: string);
begin
  FShortOpts.Add(short_option);
  FLongOpts.Add(long_option);
  FHelpOpts.Add(helpentry);
end;

procedure TFRE_CLISRV_APP.AddHelpOutLine(const msg: String);
begin
  FHelpOpts.Add(msg);
end;

procedure TFRE_CLISRV_APP.AddCommandLineOptions;
begin
  AddCheckOption('v' ,'version'      ,'  -v            | --version                      : print version information');
  AddCheckOption('h' ,'help'         ,'  -h            | --help                         : print this help');
  AddCheckOption('e:','extensions:'  ,'  -e <ext,..>   | --extensions=<ext,..>          : load database extensions');
  AddCheckOption('l' ,'list'         ,'  -l            | --list                         : list available extensions');
  AddCheckOption('s:','style:'       ,'  -s <style>    | --style <style>                : use the given css style (default: firmos)');
  AddCheckOption('d:','database:'    ,'  -d <database> | --database=<database>          : specify database, default is "ADMIN_DB"');
  AddCheckOption('x' ,'forcedb'      ,'  -x            | --forcedb                      : recreates specified database (CAUTION)');
  AddCheckOption('y' ,'forcesysdb'   ,'  -y            | --forcesysdb                   : recreates system database (CAUTION)');
  AddCheckOption('i' ,'init'         ,'  -i            | --init                         : init a new database with the chosen extensions');
  AddCheckOption('r' ,'remove'       ,'  -r            | --remove                       : remove extensions from system database (CAUTION)');
  AddCheckOption('t' ,'testdata'     ,'  -t            | --testdata                     : creates test data for extensions');
  AddCheckOption('u:','user:'        ,'  -u <user>     | --user=<user>                  : specify autologin (debug) user');
  AddCheckOption('p:','pass:'        ,'  -p <password> | --pass=<password>              : specify autologin (debug) password');
  AddCheckOption('U:','remoteuser:'  ,'  -U            | --remoteuser=<user>            : user for remote commands');
  AddCheckOption('H:','remotehost:'  ,'  -H            | --remotehost=<pass>            : host for remote commands');
  AddCheckOption('g:','graph:'       ,'  -g <filename> | --graph=<filename>             : graphical dump (system without extensions)');
  AddCheckOption('z' ,'testdump:'      ,'  -z [Scheme,..]| --testdump=[Scheme,..]         : dump class and uid of all objects');
  AddHelpOutLine;
  AddCheckOption('*','ple'           ,'                | --ple                          : use embedded persistence layer');
  AddCheckOption('*','plhost:'       ,'                | --plhost=<dnsname>             : use dns host for pl net connection');
  AddCheckOption('*','plip:'         ,'                | --plip=<numeric ip>            : use ip  host for pl net connection');
  AddCheckOption('*','plport:'       ,'                | --plport=<portnum>             : use port for pl net connection');
  AddHelpOutLine;
  AddCheckOption('*','unittest'      ,'                | --unittest                     : perform the unit test function for extensions');
  AddCheckOption('*','printtz'       ,'                | --printtz                      : print debug timezone information / known timezones');
  AddCheckOption('*','cleanzip'      ,'                | --cleanzip                     : force delete all prezipped webroot files');
  AddCheckOption('*','nozip'         ,'                | --nozip                        : don''t zip webroot files, the server still uses files that are availlable');
  AddCheckOption('*','nocache'       ,'                | --nocache                      : disable memory caching of whole webroot on startup');
  AddCheckOption('*','jsdebug'       ,'                | --jsdebug                      : enable javascript debug/develop mode');
  AddCheckOption('*','dbo2json:'     ,'                | --dbo2json=/path2/dbo          : convert a dbo to json represantation');
  AddCheckOption('*','json2dbo:'     ,'                | --json2dbo=/path2/json         : convert a json to dbo represantation');
  AddCheckOption('*','showinstalled' ,'                | --showinstalled                : show installed versions of all database objects');
  AddCheckOption('*','showrights'    ,'                | --showrights                   : show rights of specified user & and check login');
  AddCheckOption('*','backupdb:'     ,'                | --backupdb=</path2/dir>        : backup database interactive');
  AddCheckOption('*','restoredb:'    ,'                | --restoredb=</path2/dir>       : restore database interactive');
  AddCheckOption('*','backupsys:'    ,'                | --backupsys=</path2/dir>       : backup only sys database interactive');
  AddCheckOption('*','restoresys:'   ,'                | --restoresys=</path2/dir>      : restore only sys database interactive');
  AddCheckOption('*','backupapp:'    ,'                | --backupapp=</path2/dir>       : backup only app database interactive');
  AddCheckOption('*','restoreapp:'   ,'                | --restoreapp=</path2/dir>      : restore only app database interactive');
  AddCheckOption('*','limittransfer:','                | --limittransfer=<10>           : sleep <x ms> during backup and restore, to limit bandwidth');

  AddCheckOption('*','adminuser:'    ,'                | --adminuser=<user>             : specify user for admin options');
  AddCheckOption('*','adminpass:'    ,'                | --adminpass=<password>         : specify password for admin options');
  AddCheckOption('*','testlog'       ,'                | --testlog                      : enable fixed (debug-cfg) logging to console');
  AddCheckOption('*','testlogcfg'    ,'                | --testlogcfg                   : do an endless logging test');
  AddHelpOutLine;
  AddCheckOption('*','setasyncwt:'   ,'                | --setasyncwt=<on/off>          : in write through mode do the writes sync or async');
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

procedure TFRE_CLISRV_APP._CheckAdminUserSupplied;
begin
  if (cFRE_ADMIN_USER='') then begin
    writeln('no admin username supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckAdminPassSupplied;
begin
  if (cFRE_ADMIN_PASS='') then begin
    writeln('no admin password supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckUserSupplied;
begin
  if (cG_OVERRIDE_USER='') then begin
    writeln('no override/login username supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckPassSupplied;
begin
  if (cG_OVERRIDE_PASS='') then begin
    writeln('no override/login password supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP.DoRun;
var ErrorMsg : String;

    procedure ParsePersistanceLayerParams;
    begin
      if HasOption('*','plhost') then begin
        cFRE_PS_LAYER_HOST := GetOptionValue('*','plhost');
      end;
      if HasOption('*','plip') then begin
        cFRE_PS_LAYER_IP := GetOptionValue('*','plip');
      end;
      if HasOption('*','plport') then begin
        cFRE_PS_LAYER_PORT := GetOptionValue('*','plport');
      end;
      if HasOption('*','ple') then begin
        cFRE_PS_LAYER_USE_EMBEDDED := true;
      end;
    end;

    procedure ParseExtensionsUserPassDB;
    begin
      if HasOption('e','extensions') then begin // has to be after possible recreate db
        FChosenExtensionList :=  ListFromString(GetOptionValue('e','extensions'));
        VerifyExtensions;
      end else begin
        FChosenExtensionList := ListFromString(DefaultExtensions);
        VerifyExtensions;
      end;
      if HasOption('u','user') then begin
        cG_OVERRIDE_USER := GetOptionValue('u','user');
      end;

      if HasOption('p','pass') then begin
        cG_OVERRIDE_PASS := GetOptionValue('p','pass');
      end;

      if HasOption('*','adminuser') then begin
        cFRE_ADMIN_USER := GetOptionValue('*','adminuser');
      end;

      if HasOption('*','adminpass') then begin
        cFRE_ADMIN_PASS := GetOptionValue('*','adminpass');
      end;

      if HasOption('U','remoteuser') then begin
        cFRE_REMOTE_USER := GetOptionValue('U','remoteuser');
      end;

      if HasOption('H','remotehost') then begin
        cFRE_REMOTE_HOST:= GetOptionValue('H','remotehost');
      end else begin
        cFRE_REMOTE_HOST:= '127.0.0.1';
      end;
      if HasOption('d','database') then begin
        FDBName := GetOptionValue('d','database');
      end else begin
        FDBName := 'ADMIN_DB';
      end;
    end;

    procedure CheckTestLogging;
    begin
      if HasOption('*','testlog') then
       CfgTestLog;
    end;

    procedure ProcessInitRecreateOptions;
    begin
      if HasOption('x','forcedb') then
        begin
          FOnlyInitDB:=true;
          ReCreateDB;
        end;
      if HasOption('y','forcesysdb') then
        begin
          FOnlyInitDB:=true;
          ReCreateSysDB;
        end;
      if HasOption('i','init') then
        begin
          FOnlyInitDB:=true;
          InitExtensions;
          GFRE_DB_PS_LAYER.SyncSnapshot(true);
        end;
      if HasOption('t','testdata') then
        begin
          FOnlyInitDB:=true;
          GenerateTestdata;
        end;
      if HasOption('r','remove') then
        begin
          FOnlyInitDB:=true;
          RemoveExtensions;
          Terminate;
          Exit;
      end;
      if HasOption('*','unittests') then
        begin
          FOnlyInitDB:=true;
          DoUnitTest;
          Terminate;
          exit;
        end;
    end;

begin
  AddCommandLineOptions;
  ErrorMsg:=CheckOptions(GetShortCheckOptions,FLongOpts.AsTStrings);
  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    WriteHelp;
    Terminate;
    Exit;
  end;

  GDBPS_TRANS_WRITE_THROUGH := TRUE;
  GDBPS_TRANS_WRITE_ASYNC   := FALSE; { ASYNC Writethrough may be error prone (!) and unsave }
  GDISABLE_WAL              := TRUE;
  GDISABLE_SYNC             := TRUE;
  GDROP_WAL                 := TRUE;
  GDBPS_SKIP_STARTUP_CHECKS := FALSE;

  ParsePersistanceLayerParams;
  if PreStartupTerminatingCommands then
   halt(0);
  ParseSetSystemFlags;
  ParseExtensionsUserPassDB;
  Initialize_Read_FRE_CFG_Parameter;
  if AfterConfigStartupTerminatingCommands then
   halt(0);
  PrepareStartup;      { The initial startup is done (connections can be made, but no extensions initialized }
  CheckTestLogging;    { CFG File reading done}
  RegisterExtensions;

  if AfterStartupTerminatingCommands then
    begin
      OrderedShutDown;
      exit;
    end;

  ProcessInitRecreateOptions;

  if AfterInitDBTerminatingCommands then
   begin
     Terminate;
     exit;
   end;

  if HasOption('*','dontstart')
     or FOnlyInitDB then
      begin
        Terminate;
      end;

  FBaseServer := TFRE_BASE_SERVER.create(FDBName);
  FBaseServer.Setup;
  if not Terminated then
    GFRE_SC.RunUntilTerminate;
  OrderedShutDown;
end;

procedure TFRE_CLISRV_APP.OrderedShutDown;
begin
  Teardown_APS_Comm;
  Terminate;
  FBaseServer.Free;
  GFRE_DB_PS_LAYER.Finalize;
  Cleanup_SSL_CMD_CA_Interface;
  FinalizeTransformManager;
  GFRE_BT.DeactivateJack;
end;

function TFRE_CLISRV_APP.PreStartupTerminatingCommands:boolean;
begin
  result := false;
  if HasOption('*','printtz') then
    begin
      result := true;
      PrintTimeZones;
    end;
  if HasOption('h','help') then
    begin
      result := true;
      WriteHelp;
    end;
  if HasOption('v','version') then
    begin
      result := true;
      WriteVersion;
    end;
  if HasOption('l','list') then
    begin
      result := true;
      ListExtensions;
    end;
end;

function TFRE_CLISRV_APP.AfterConfigStartupTerminatingCommands: boolean;
begin
  result := false;
  if HasOption('*','testlogcfg') then
    EndlessLogTest;
end;

function TFRE_CLISRV_APP.AfterStartupTerminatingCommands: boolean; { if true then shutdown, don't start }
begin
  result := false;
  if HasOption('*','dbo2json') then
    begin
      result := true;
      ConvertDbo(GetOptionValue('*','dbo2json'),true);
    end;
  if HasOption('*','json2dbo') then
    begin
      result := true;
      ConvertDbo(GetOptionValue('*','json2dbo'),false);
    end;
  if HasOption('*','backupdb') then
    begin
      result := true;
      BackupDB(true,true,GetOptionValue('*','backupdb'));
    end;
  if HasOption('*','backupsys') then
    begin
      result := true;
      BackupDB(false,true,GetOptionValue('*','backupsys'));
    end;
  if HasOption('*','backupapp') then
    begin
      result := true;
      BackupDB(true,false,GetOptionValue('*','backupapp'));
    end;
  if HasOption('*','restoredb') then
    begin
      result := true;
      RestoreDB(true,true,GetOptionValue('*','restoredb'));
    end;
end;

function TFRE_CLISRV_APP.AfterInitDBTerminatingCommands: boolean;
begin
  result := false;
  if HasOption('*','showinstalled') then
    begin
      result := true;
      ShowVersions;
    end;
  if HasOption('*','showrights') then
    begin
      result := true;
      ShowRights;
    end;
  if HasOption('g','graph') then
    begin
      result := true;
      SchemeDump(GetOptionValue('g','graph'));
    end;
  if HasOption('z','testdump') then
    begin
      result := true;
      DumpAll(GetOptionValue('z','testdump'));
    end;
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
end;

procedure TFRE_CLISRV_APP.WriteHelp;
var
  i: NativeInt;
begin
  WriteVersion;
  writeln('Usage: ',ExtractFileName(ExeName),' [OPTIONS]');
  writeln(' OPTIONS');
  for i := 0 to FHelpOpts.Count-1 do
    writeln(FHelpOpts[i]);
end;

procedure TFRE_CLISRV_APP.WriteVersion;
begin
  writeln('');
  writeln('---');
  writeln(GFOS_VHELP_GET_VERSION_STRING);
  writeln('---');
  writeln('Default extension : ',FDefaultExtensions);
  writeln('Default style     : ',FDefaultStyle);
  writeln('---');
  writeln('');
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
end;

procedure TFRE_CLISRV_APP.BackupDB(const adb,sdb:boolean ; const dir: string);
var s     : string;
    conn  : IFRE_DB_CONNECTION;
    scon  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;
    sfs   : TFileStream;
    dbfs  : TFileStream;
    sysfn : String;
    dbfn  : String;

  procedure ProgressCB(const phase,detail,header : ShortString ; const cnt,max: integer);
  var outs :string;
  begin
    if header<>'' then
      begin
        writeln(header);
        exit;
      end;
    WriteStr(outs,'  > ',phase,' [',detail,'] : ',cnt,'/',max,' Items','                                                                                                          ');
    write(outs);
    if cnt<>max then
      begin
        write(StringOfChar(#8,length(outs)));
      end
    else
      begin
        writeln;
      end;
    if FLimittransfer>0 then
      sleep(FLimittransfer);
  end;

begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  ForceDirectories(dir);
  if not DirectoryExists(dir) then
    begin
      writeln('cannot backup / could not create directory ['+dir+'] !');
      abort;
    end;
  if adb and not sdb then
    writeln('Backup of Database ['+FDBName+'] into ['+dir+'] (y/N)');
  if sdb and not adb then
    writeln('Backup of SYSTEM DB into ['+dir+'] (y/N)');
  if adb and sdb then
    writeln('Backup of Database ['+FDBName+'] + [SYSTEM DB] into ['+dir+'] (y/N)');
  ReadLn(s);
  //s:='y';
  if s='y' then
    begin
      write('CONNECTING ['+FDBName+'] ');
      if adb then
        begin
          conn := GFRE_DBI.NewConnection;
          res  := conn.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
        end
      else
        begin
          scon := GFRE_DBI.NewSysOnlyConnection;
          res  := scon.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
        end;
      if res<>edb_OK then
        begin
          writeln(CFRE_DB_Errortype[res]);
          abort;
        end;
      writeln('OK');
      if sdb then
        begin
          sysfn := dir+DirectorySeparator+'sys.fdbb';
          write('Opening file :'+sysfn);
          try
            sfs := TFileStream.Create(sysfn,fmCreate+fmShareExclusive);
          except
            writeln('FAILED');
            abort;
          end;
          writeln(' OK');
        end
      else
        sfs := nil;
      if adb then
        begin
          dbfn  := dir+DirectorySeparator+'usr.fdbb';
          write('Opening file :'+dbfn);
          try
            dbfs := TFileStream.Create(dbfn,fmCreate+fmShareExclusive);
          except
            writeln('FAILED');
            abort;
          end;
          writeln(' OK');
        end
      else
       dbfs := nil;
      if adb then
        conn.SYS.BackupDatabaseReadable(sfs,dbfs,@ProgressCB)
      else
        scon.BackupDatabaseReadable(sfs,nil,@ProgressCB);
    end
  else
    begin
     writeln('ABORTED');
    end;
end;

procedure TFRE_CLISRV_APP.RestoreDB(const adb, sdb: boolean; const dir: string);
var s     : string;
    conn  : IFRE_DB_CONNECTION;
    scon  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;
    sfs   : TFileStream;
    dbfs  : TFileStream;
    sysfn : String;
    dbfn  : String;

  procedure ProgressCB(const phase,detail,header : ShortString ; const cnt,max: integer);
  var outs :string;
  begin
    if header<>'' then
      begin
        writeln(header);
        exit;
      end;
    WriteStr(outs,'  > ',phase,' [',detail,'] : ',cnt,'/',max,' Items','                                                                                                          ');
    write(outs);
    if cnt<>max then
      begin
        write(StringOfChar(#8,length(outs)));
      end
    else
      begin
        writeln;
      end;
    if FLimittransfer>0 then
      sleep(FLimittransfer);
  end;

begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  if not DirectoryExists(dir) then
    begin
      writeln('the backup directory does not exist['+dir+'] !');
      abort;
    end;
  if adb and not sdb then
    writeln('Restore of database ['+FDBName+'] (y/N)');
  if sdb and not adb then
    writeln('Restore of SYSTEM DB (y/N)');
  if adb and sdb then
    writeln('Restore backup as database ['+FDBName+'] + [SYSTEM DB]  (y/N)');
  ReadLn(s);
  //s:='y'; // ignore force lazarusdebug
  if s='y' then
    begin
      write('RECREATING / CONNECTING ['+FDBName+'] ');
      if adb then
        begin
          GFRE_DB_PS_LAYER.DeleteDatabase('SYSTEM');
          GFRE_DB_PS_LAYER.DeleteDatabase(FDBName);
          CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase('SYSTEM'));
          CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase(FDBName));
          conn := GFRE_DBI.NewConnection;
          res  := conn.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
        end
      else
        begin
          scon := GFRE_DBI.NewSysOnlyConnection;
          res  := scon.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
        end;
      if res<>edb_OK then
        begin
          writeln(CFRE_DB_Errortype[res]);
          abort;
        end;
      writeln('OK');
      if sdb then
        begin
          sysfn := dir+DirectorySeparator+'sys.fdbb';
          write('Opening file :'+sysfn);
          try
            sfs := TFileStream.Create(sysfn,fmOpenRead+fmShareExclusive);
          except
            writeln('FAILED');
            abort;
          end;
          writeln(' OK');
        end
      else
        sfs := nil;
      if adb then
        begin
          dbfn  := dir+DirectorySeparator+'usr.fdbb';
          write('Opening file :'+dbfn);
          try
            dbfs := TFileStream.Create(dbfn,fmOpenRead+fmShareExclusive);
          except
            writeln('FAILED');
            abort;
          end;
          writeln(' OK');
        end
      else
       dbfs := nil;
      if adb then
        conn.SYS.RestoreDatabaseReadable(sfs,dbfs,FDBName,@ProgressCB)
      else
        scon.RestoreDatabaseReadable(sfs,nil,'',@ProgressCB);
    end
  else
    begin
     writeln('ABORTED');
    end;
end;

procedure TFRE_CLISRV_APP.GenerateTestdata;
var
  conn    : IFRE_DB_CONNECTION;
  domainId: TGuid;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;

  { a testdomain is created globally for all extensiosn}
  conn := GFRE_DBI.NewConnection;
  try
    CheckDbResult(conn.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS),' could not login into '+FDBName+' for testdomain creation');
    if not conn.SYS.DomainExists('test') then begin
      CheckDbResult(conn.AddDomain('test','This domain is for testing only','Test Domain'));
    end;
    domainId:=conn.SYS.DomainID('test');
    if not conn.SYS.UserExists('admin',domainId) then begin
      CheckDbResult(conn.SYS.AddUser('admin',domainId,'test','admin','test'));
    end;
    if not conn.SYS.UserExists('manager',domainId) then begin
      CheckDbResult(conn.SYS.AddUser('manager',domainId,'test','manager','test'));
    end;
    if not conn.SYS.UserExists('viewer',domainId) then begin
      CheckDbResult(conn.SYS.AddUser('viewer',domainId,'test','viewer','test'));
    end;
  finally
    conn.Finalize;
  end;
  GFRE_DBI_REG_EXTMGR.GenerateTestData4Exts(FChosenExtensionList,FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
end;

procedure TFRE_CLISRV_APP.DoUnitTest;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  GFRE_DBI_REG_EXTMGR.GenerateUnitTestsdata(FChosenExtensionList,FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
end;

procedure TFRE_CLISRV_APP.InitExtensions;
var conn : IFRE_DB_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  //writeln('InitDB for extensions :'+uppercase(FChosenExtensionList.Commatext));
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS),'cannot connect system db');
  GFRE_DBI.DBInitializeAllSystemClasses(conn);
  GFRE_DBI.DBInitializeAllExClasses(conn);
  conn.Finalize;
  GFRE_DBI_REG_EXTMGR.InitDatabase4Extensions(FChosenExtensionList,FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
end;

procedure TFRE_CLISRV_APP.ShowVersions;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS),'cannot connect system db');
  writeln(conn.GetClassesVersionDirectory.DumpToString);
  conn.Finalize;
end;

procedure TFRE_CLISRV_APP.ShowRights;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckUserSupplied;
  _CheckPassSupplied;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect(cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect system db');
  writeln(conn.DumpUserRights);
  conn.Finalize;
end;


procedure TFRE_CLISRV_APP.RemoveExtensions;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  writeln('Remove apps for extensions :'+uppercase(FChosenExtensionList.Commatext));
  GFRE_DBI_REG_EXTMGR.Remove4Extensions(FChosenExtensionList,FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
end;


procedure TFRE_CLISRV_APP.RegisterExtensions;
begin
  FRE_DBBASE.Register_DB_Extensions;
  GFRE_DBI_REG_EXTMGR.RegisterExtensions4DB(FChosenExtensionList);
  FRE_BASE_SERVER.RegisterLogin;
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
  Setup_SSL_CMD_CA_Interface;
  Setup_APS_Comm;

  if cFRE_PS_LAYER_USE_EMBEDDED then
    GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'db')
  else
    GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Net(cFRE_PS_LAYER_HOST,cFRE_PS_LAYER_IP,cFRE_PS_LAYER_PORT);

  InitTransfromManager;
  Init4Server;
  GFRE_DBI.LocalZone := cFRE_SERVER_DEFAULT_TIMEZONE;
  //writeln('STARTUP @LOCAL TIME :',GFRE_DT.ToStrFOS(GFRE_DT.UTCToLocalTime(GFRE_DT.Now_UTC,GFRE_DBI.LocalZone)),'  UTC TIME :',GFRE_DT.ToStrFOS(GFRE_DT.Now_UTC));
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
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry);     // DROP : Server / DEBUG
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER_DATA],fll_Debug,'*',flra_DropEntry);// DROP : Server / Dispatch / Input Output
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
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Info,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Debug,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE_NOTIFY],fll_Info,'*',flra_DropEntry);
    GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE_NOTIFY],fll_Debug,'*',flra_DropEntry);
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

procedure TFRE_CLISRV_APP.EndlessLogTest;
var
  cat: TFRE_DB_LOGCATEGORY;
begin
  while true do
    for cat in TFRE_DB_LOGCATEGORY do
      begin
        GFRE_DB.LogDebug(cat,' <DEBUG LOG ENTRY> ');
        GFRE_DB.LogInfo(cat,' <INFO LOG ENTRY> ');
        GFRE_DB.LogEmergency(cat,' <EMERGENCY LOG ENTRY>');
        GFRE_DB.LogWarning(cat,' <WARNING LOG ENTRY> ');
        GFRE_DB.LogError (cat,' <ERROR LOG ENTRY> ');
        GFRE_DB.LogNotice(cat,' <NOTICE LOG ENTRY> ');
     end;
end;

procedure TFRE_CLISRV_APP.SchemeDump(const filename: string);
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
      sconn.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
      sconn.DrawScheme(mems);
    end else begin
      lconn := GFRE_DBI.NewConnection;
      res   := lconn.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
      if res<>edb_OK then begin
        WriteLn('SCHEME DUMP CHECK CONNECT FAILED : ',CFRE_DB_Errortype[res]);
      end;
      lconn.sys.DrawScheme(mems);
      lconn.Finalize;
    end;
    mems.SaveToFile(filename);
  finally
    mems.free;
  end;
end;

procedure TFRE_CLISRV_APP.DumpAll(const filterstring : string);
var conn   : IFRE_DB_CONNECTION;
    filter : TFRE_DB_StringArray;

    procedure Local(const obj:IFRE_DB_Object; var halt:boolean ; const current,max : NativeInt);
    begin
      writeln('DUMPING OBJ : ',current,'/',max,' ',obj.UID_String,' ',obj.SchemeClass);
      obj.Finalize;
    end;

begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cFRE_ADMIN_USER,cFRE_ADMIN_PASS),'cannot connect db');
  if filterstring<>'' then
    FREDB_SeperateString(filterstring,',',filter);
  writeln('');
  writeln('DUMPING ALL OBJECTS FROM ',FDBName,' FILTER [',filterstring,']');
  writeln('');
  conn.ForAllDatabaseObjectsDo(@local,filter);
  writeln('');
  writeln('DUMPING ALL SYSTEM OBJECTS FILTER [',filterstring,']');
  writeln('');
  conn.sys.ForAllDatabaseObjectsDo(@local,filter);
  conn.Finalize;
end;

constructor TFRE_CLISRV_APP.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException      :=True;
  FAvailExtensionList  := GFRE_DBI_REG_EXTMGR.GetExtensionList;
  FAvailExtensionList.SetCaseSensitive(false);
  FDefaultStyle        := 'firmos';
  FChosenExtensionList := GFRE_TF.Get_FOS_Strings;
  FShortOpts           := GFRE_TF.Get_FOS_Strings;
  FLongOpts            := GFRE_TF.Get_FOS_Strings;
  FHelpOpts            := GFRE_TF.Get_FOS_Strings;
end;

destructor TFRE_CLISRV_APP.Destroy;
begin
  inherited Destroy;
end;

end.

