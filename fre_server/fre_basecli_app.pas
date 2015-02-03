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
  fre_system,fos_default_implementation,fos_tool_interfaces,fos_fcom_types,fre_aps_interface,fre_db_interface,
  fre_db_core,

  fre_dbbase,fre_openssl_cmd,

  fre_aps_comm_impl,
  fre_net_pl_client, { network ps layer}
  fre_db_persistance_fs_simple, { filesystem ps layer}
  fre_configuration,fre_base_server,
  fre_db_core_transdata
  ;

type

  { TFRE_CLISRV_APP }

  TFRE_CLISRV_APP = class(TCustomApplication)
  private
    FAvailExtensionList  : IFOS_STRINGS;
    FChosenExtensionList : IFOS_STRINGS;
    FCustomExtensionSet  : boolean;
    FDeployed            : boolean;
    FShortOpts           : IFOS_STRINGS;
    FLongOpts            : IFOS_STRINGS;
    FHelpOpts            : IFOS_STRINGS;
    FDefaultStyle        : String;
    procedure   SetDefaultStyle     (AValue: String);
  protected
    fapplication                   : string;
    FDBName                        : string;
    FOnlyInitDB                    : boolean;
    FBaseServer                    : TFRE_BASE_SERVER;
    FLimittransfer                 : integer;
    FSystemConnection              : TFRE_DB_SYSTEM_CONNECTION;

    procedure   AddCheckOption                          (const short_option,long_option,helpentry : string);
    procedure   AddHelpOutLine                          (const msg:String='');
    function    GetShortCheckOptions                    : String; virtual;    { Setup of cmd line options short }
    function    GetLongCheckOptions                     : TStrings; virtual;  { Setup of cmd line options long }

    procedure  _CheckDBNameSupplied;
    procedure  _CheckAdminUserSupplied;
    procedure  _CheckAdminPassSupplied;
    procedure  _CheckPLAdminUserSupplied;
    procedure  _CheckPLAdminPassSupplied;
    procedure  _CheckUserSupplied;
    procedure  _CheckPassSupplied;
    procedure  _CheckNoCustomextensionsSet;

    procedure   DoRun                   ; override ;
    procedure   OrderedShutDown         ;
    procedure   DebugTestProcedure      ;                                           { copy startup test code here}

    function    PreStartupTerminatingCommands: boolean        ; virtual;            { cmd's that should be executed without db(ple), they terminate}
    function    AfterConfigStartupTerminatingCommands:boolean ; virtual;            { cmd's that should be executed after the reading of cfg file, but before db core init}
    function    AfterStartupTerminatingCommands:boolean       ; virtual;            { cmd's that should be executed with db core init, they terminate}
    function    AfterInitDBTerminatingCommands:boolean        ; virtual;            { cmd's that should be executed with full db init, they terminate}
    function    AfterSysDBConnectTerminatingCommands:boolean  ; virtual;            { cmd's that should be executed after the connection to the system db is done}
    procedure   ParseSetSystemFlags                           ; virtual;            { Setting of global flags before startup go here }
    procedure   AddCommandLineOptions                         ; virtual;            { override for custom options/flags/commands}

    procedure   WriteHelp                               ; virtual;
    procedure   WriteVersion                            ; virtual;

    function    ListFromString          (const str :string) : IFOS_STRINGS;
    procedure   ConvertDbo              (const file_name:string ; const to_json:boolean);
    procedure   CalcSHA1                (pw : string);
    procedure   DumpDBO                 (const uid_hex : string);
    procedure   PrintTimeZones          ;
    procedure   ReCreateDB              ;
    procedure   ReCreateSysDB           ;
    procedure   BackupDB                (const adb, sdb: boolean; const dir: string);
    procedure   RestoreDB               (const adb, sdb: boolean; const dir: string ; const override_with_live_scheme : boolean);
    procedure   AddUser                 (const userencoding : string);
    procedure   GenerateTestdata        ;
    procedure   DoUnitTest              ;
    procedure   InitExtensions          ;
    procedure   ShowVersions            ;
    procedure   ShowRights              ;
    procedure   ShowApps                ;
    procedure   ShowDeploy              ;
    procedure   DumpScheme              ;
    procedure   RemoveExtensions        ;
    procedure   RegisterExtensions      ;
    procedure   DeployDatabaseScheme    (deploy_revision : string);
    procedure   VerifyExtensions        ;
    procedure   ListExtensions          ;
    procedure   PrepareStartup          ;
    procedure   CfgTestLog              ; virtual;
    procedure   EndlessLogTest          ;
    procedure   SchemeDump              (const filename:string;const classfile:string);
    procedure   DumpAll                 (const filterstring: string);
    procedure   OverviewDump            ;
    procedure   ExpandReferences        (const input:string);
  public
    constructor Create                  (TheOwner: TComponent); override;
    destructor  Destroy                 ; override;
    property    DefaultStyle            : String read FDefaultStyle      write SetDefaultStyle;
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
         fn  := fp+DirectorySeparator+fn+'.fdbo';
         if FileExists(fn) and
            not DeleteFile(fn) then
              raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not delete convert file : '+fn);
         res := GFRE_BT.StringFromFile(file_name);
         dbo := TFRE_DB_Object.CreateFromJSONString(res);
         dbo.SaveToFile(fn);
         dbo.Finalize;
     end;
end;

procedure TFRE_CLISRV_APP.CalcSHA1(pw: string);
var len : NativeInt;
begin
  Randomize;
  len:=StrToIntDef(pw,-1);
  if pw='' then
    pw := FREDB_GetRandomChars(10)
  else
  if (len>0) and (len<65) then
    pw := FREDB_GetRandomChars(len);
  writeln('Calculating Salted SHA1 SCHEME for ['+pw+'] = ['+GFRE_BT.CalcSaltedSH1Password(pw,FREDB_GetRandomChars(8))+']');
end;

procedure TFRE_CLISRV_APP.DumpDBO(const uid_hex: string);
var conn : IFRE_DB_CONNECTION;
    uid  : TFRE_DB_GUID;
    dbo  : IFRE_DB_Object;
    refs : TFRE_DB_ObjectReferences;
    i    : NativeInt;
begin
   uid := FREDB_H2G(uid_hex);
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  _CheckNoCustomextensionsSet;
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect system db');
  CheckDbResult(conn.fetch(uid,dbo));
  refs := conn.GetReferencesDetailed(uid,false);
  writeln('');
  writeln(dbo.DumpToString(2));
  writeln('');
  for i:=0 to high(refs) do
    begin
      writeln('Scheme:',refs[i].schemename,'.',refs[i].fieldname,'(',refs[i].linked_uid.AsHexString,')');
    end;
  conn.Finalize;
end;

procedure TFRE_CLISRV_APP.ParseSetSystemFlags;
  procedure SetDatetimeTag;
  var y,m,d,hh,mm,ss,mmm : LongInt;
  begin
    GFRE_DT.DecodeTime(GFRE_DT.Now_UTC,y,m,d,hh,mm,ss,mmm);
    cFRE_DB_CACHETAG := Format('%2.2d%2.2d%2.2d%2.2d%2.2d%2.2d',[hh,mm,ss,d,m,y-2000]);
  end;

begin
   SetDatetimeTag;
   if HasOption('*','tryrecovery') then
     begin
       GDBPS_SKIP_STARTUP_CHECKS := true;
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

   if HasOption('s','style') then
     cFRE_WEB_STYLE := GetOptionValue('s','style')
   else
     cFRE_WEB_STYLE := FDefaultStyle;

   if HasOption('*','jsdebug') then
     cFRE_JS_DEBUG := true
   else
     cFRE_JS_DEBUG := false;

  if HasOption('*','webdev') then
    begin
      cFRE_JS_DEBUG                    := true;
      cFRE_USE_STATIC_CACHE            := false;
      cFRE_BUILD_ZIP_HTTP_FILES        := false;
      cFRE_FORCE_CLEAN_ZIP_HTTP_FILES  := true;
    end;

  if HasOption('*','cmddebug') then
      cFRE_CMDLINE_DEBUG := GetOptionValue('*','cmddebug');


   if HasOption('*','filterapps') then
     cFRE_DB_ALLOWED_APPS := GetOptionValue('*','filterapps');

   if GDBPS_SKIP_STARTUP_CHECKS then
     writeln('>>> !!!!  WARNING : SKIPPING STARTUP CHECKS (only possible on embedded) !!!! ');
end;

function TFRE_CLISRV_APP.GetShortCheckOptions: String;
var
  i: NativeInt;
begin
  result := '';
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
  AddCheckOption('v'  ,'version'      ,'  -v            | --version                      : print version information');
  AddCheckOption('h'  ,'help'         ,'  -h            | --help                         : print this help');
  AddCheckOption('e:' ,'extensions:'  ,'  -e <ext,..>   | --extensions=<ext,..>          : load database extensions');
  AddCheckOption('l'  ,'list'         ,'  -l            | --list                         : list available extensions');
  AddCheckOption('s:' ,'style:'       ,'  -s <style>    | --style <style>                : use the given css style (default: firmos)');
  AddCheckOption('d:' ,'database:'    ,'  -d <database> | --database=<database>          : specify database, default is "ADMIN_DB"');
  AddCheckOption('x'  ,'forcedb'      ,'  -x            | --forcedb                      : recreates specified database (CAUTION)');
  AddCheckOption('y'  ,'forcesysdb'   ,'  -y            | --forcesysdb                   : recreates system database (CAUTION)');
  AddCheckOption('i'  ,'init'         ,'  -i            | --init                         : init a new database with the chosen extensions');
  AddCheckOption('r'  ,'remove'       ,'  -r            | --remove                       : remove extensions from system database (CAUTION)');
  AddCheckOption('t'  ,'testdata'     ,'  -t            | --testdata                     : creates test data for extensions');
  AddCheckOption('u:' ,'user:'        ,'  -u <user>     | --user=<user>                  : specify autologin (debug) user');
  AddCheckOption('p:' ,'pass:'        ,'  -p <password> | --pass=<password>              : specify autologin (debug) password');
  AddCheckOption('U:' ,'remoteuser:'  ,'  -U            | --remoteuser=<user>            : user for remote commands');
  AddCheckOption('H:' ,'remotehost:'  ,'  -H            | --remotehost=<pass>            : host for remote commands');
  AddCheckOption('g:' ,'graph:'       ,'  -g <filename> | --graph=<filename>             : graphical dump (system without extensions)');
  AddCheckOption('*'  ,'classfile:'   ,'                | --classfile=<filename>         : filter graphical dump to classes listed in classfile and related');
  AddCheckOption('z'  ,'testdump:'    ,'  -z [Scheme,..]| --testdump=[Scheme,..]         : dump class and uid of all objects');
  AddHelpOutLine;
  AddCheckOption('*'  ,'ple'           ,'                | --ple                          : use embedded persistence layer');
  AddCheckOption('*'  ,'plhost:'       ,'                | --plhost=<dnsname>             : use dns host for pl net connection');
  AddCheckOption('*'  ,'plip:'         ,'                | --plip=<numeric ip>            : use ip  host for pl net connection');
  AddCheckOption('*'  ,'plport:'       ,'                | --plport=<portnum>             : use port for pl net connection');
  AddHelpOutLine;
  AddCheckOption('*'  ,'unittest'      ,'                | --unittest                     : perform the unit test function for extensions');
  AddCheckOption('*'  ,'printtz'       ,'                | --printtz                      : print debug timezone information / known timezones');
  AddCheckOption('*'  ,'cleanzip'      ,'                | --cleanzip                     : force delete all prezipped webroot files');
  AddCheckOption('*'  ,'nozip'         ,'                | --nozip                        : don''t zip webroot files, the server still uses files that are availlable');
  AddCheckOption('*'  ,'nocache'       ,'                | --nocache                      : disable memory caching of whole webroot on startup');
  AddCheckOption('*'  ,'jsdebug'       ,'                | --jsdebug                      : enable javascript debug/develop mode');
  AddCheckOption('*'  ,'webdev'        ,'                | --webdev                       : shortcut: cleanzip,nozip,jsdebug,nocache');
  AddCheckOption('*'  ,'dbo2json:'     ,'                | --dbo2json=/path2/dbo          : convert a dbo to json representation');
  AddCheckOption('*'  ,'json2dbo:'     ,'                | --json2dbo=/path2/json         : convert a json to dbo representation');
  AddCheckOption('*'  ,'dumpdbo:'      ,'                | --dumpdbo=uid_hex              : direct dump of a dbo');
  AddCheckOption('*'  ,'expandrefs:'   ,'                | --expandrefs=uid_hex,[..]/RL,..: expand referenceslist');
  AddCheckOption('*'  ,'tryrecovery'   ,'                | --tryrecovery                  : try recovery of a bad db by skipping checks / start with option / make backup / have luck');
  AddCheckOption('*'  ,'showinstalled' ,'                | --showinstalled                : show installed versions of all database objects');
  AddCheckOption('*'  ,'showrights'    ,'                | --showrights                   : show rights of specified user & and check login');
  AddCheckOption('*'  ,'showapps'      ,'                | --showapps                     : show the actual available (filtered) app classes');
  AddCheckOption('*'  ,'showdeploy'    ,'                | --showdeploy                   : show the deployment information');
  AddCheckOption('*'  ,'filterapps:'   ,'                | --filterapps=class,class       : allow only the specified apps');
  AddCheckOption('A'  ,'adduser:'      ,'                | --adduser=name@dom,pass,class  : add a user with a specified class (WEBUSER,FEEDER),password and domain (SYSTEM)');
  AddCheckOption('*'  ,'showscheme'    ,'                | --showscheme                   : dump the whole database scheme definition');
  AddCheckOption('*'  ,'backupdb:'     ,'                | --backupdb=</path2/dir>        : backup database interactive');
  AddCheckOption('*'  ,'restoredb:'    ,'                | --restoredb=</path2/dir>       : restore database interactive');
  AddCheckOption('*'  ,'restoredbsch:' ,'                | --restoredbsch=</path2/dir>    : restore database interactive, but ignore backup schemes and use live schemes');
  AddCheckOption('*'  ,'backupsys:'    ,'                | --backupsys=</path2/dir>       : backup only sys database interactive');
  AddCheckOption('*'  ,'restoresys:'   ,'                | --restoresys=</path2/dir>      : restore only sys database interactive');
  AddCheckOption('*'  ,'backupapp:'    ,'                | --backupapp=</path2/dir>       : backup only app database interactive');
  AddCheckOption('*'  ,'restoreapp:'   ,'                | --restoreapp=</path2/dir>      : restore only app database interactive');
  AddCheckOption('D::','deploy::'      ,' -D [revision]  | --deploy[=revision]            : build and deploy databasescheme for the chosen extensions to persistence layer, optional tag with revision');
  AddCheckOption('*'  ,'limittransfer:','                | --limittransfer=<10>           : sleep <x ms> during backup and restore, to limit bandwidth');

  AddCheckOption('*'  ,'adminuser:'    ,'                | --adminuser=<user>             : specify user for db admin options');
  AddCheckOption('*'  ,'adminpass:'    ,'                | --adminpass=<password>         : specify password for db admin options');
  AddCheckOption('*'  ,'pladmin:'      ,'                | --pladmin=<user>               : specify user for pl admin options');
  AddCheckOption('*'  ,'plpass:'       ,'                | --plpass=<password>            : specify password for pl admin options');
  AddCheckOption('*'  ,'testlog'       ,'                | --testlog                      : enable fixed (debug-cfg) logging to console');
  AddCheckOption('*'  ,'testlogcfg'    ,'                | --testlogcfg                   : do an endless logging test');
  AddHelpOutLine;
  AddCheckOption('*'  ,'resetadmin'    ,'                | --resetadmin                   : reset the admin@system and the guest@system accounts to default. => (admin and "")');
  AddCheckOption('*'  ,'cmddebug:'     ,'                | --cmddebug=<param>             : set an arbitrary cmd line debug option');
  AddCheckOption('o'  ,'overviewdump'  ,' -o             | --overviewdump                 : do a overview dump of the database');
  AddCheckOption('Z::','calcsha::'     ,' -Z             | --calcsha[=<password>]         : generate/calc a safe salted sha1 password');


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

procedure TFRE_CLISRV_APP._CheckPLAdminUserSupplied;
begin
  if (cFRE_PL_ADMIN_USER='') then begin
    writeln('no admin username supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckPLAdminPassSupplied;
begin
  if (cFRE_PL_ADMIN_PASS='') then begin
    writeln('no pl admin username supplied');
    Terminate;
    halt(1);
  end;
end;

procedure TFRE_CLISRV_APP._CheckUserSupplied;
begin
  if (cG_OVERRIDE_USER='') then begin
    writeln('no pl admin password supplied');
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

procedure TFRE_CLISRV_APP._CheckNoCustomextensionsSet;
begin
  if FCustomExtensionSet then begin
    writeln('the custom extensions option is only allowed in conjunction with --deploy option.');
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
        FCustomExtensionSet  := True;
        VerifyExtensions;
      //end else begin
      //  FChosenExtensionList := ListFromString(DefaultExtensions);
      //  VerifyExtensions;
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

      if HasOption('*','pladmin') then begin
        cFRE_PL_ADMIN_USER := GetOptionValue('*','pladmin');
      end;

      if HasOption('*','plpass') then begin
        cFRE_PL_ADMIN_PASS := GetOptionValue('*','plpass');
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
      if HasOption('y','forcesysdb') then
        begin
          FOnlyInitDB:=true;
          ReCreateSysDB;
        end;
      if HasOption('x','forcedb') then
        begin
          FOnlyInitDB:=true;
          ReCreateDB;
        end;
      if HasOption('D','deploy') then
        begin
          FOnlyInitDB := true;
          DeployDatabaseScheme(GetOptionValue('D','deploy'));
        end;
      if HasOption('i','init') then
        begin
          FOnlyInitDB:=true;
          InitExtensions;
          GFRE_DB_PS_LAYER.SyncSnapshot;
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

    procedure SetupSystemConnection;
    var fdbs : IFRE_DB_Object;
        res  : TFRE_DB_Errortype;
    begin
      FSystemConnection := GFRE_DB.NewDirectSysConnection;
      res := FSystemConnection.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS);
      if res<>edb_OK then begin
        FSystemConnection.Free;
        FSystemConnection := nil;
        GFRE_DB.LogError(dblc_SERVER,'SERVING SYSTEM DATABASE failed due to [%s]',[CFRE_DB_Errortype[res]]);
        GFRE_BT.CriticalAbort('CANNOT SERVE SYSTEM DB [%s]',[CFRE_DB_Errortype[res]]);
      end;
      res := GFRE_DB_PS_LAYER.GetDatabaseScheme(fdbs);
      if res = edb_OK then
       begin
          GFRE_DB.SetDatabasescheme(fdbs);
          FChosenExtensionList := ListFromString(GFRE_DB.GetDeployedExtensionlist);
          RegisterExtensions;
          GFRE_DB.InstantiateApplicationObjects;
          GFRE_DB.LogInfo(dblc_SERVER,'SERVING SYSTEM DATABASE : [%s]',[GFRE_DB.GetDeploymentInfo]);
          writeln(format('SERVING SYSTEM DATABASE : [%s]',[GFRE_DB.GetDeploymentInfo]));
          if FCustomExtensionSet then
            writeln('WARNING: ignoring specified extensions from commandline, they are only used for the deploy case');
       end
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'could not fetch the database scheme [%s] ',[res.Msg]);
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
  GDISABLE_SYNC             := TRUE;
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

  DebugTestProcedure;

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

  if not FOnlyInitDB then
    SetupSystemConnection;

  if AfterSysDBConnectTerminatingCommands then
    begin
      Terminate;
      exit;
    end;

  if HasOption('*','dontstart')
     or FOnlyInitDB then
      begin
        Terminate;
        exit;
      end;

  FBaseServer := TFRE_BASE_SERVER.create(FDBName);
  FBaseServer.Setup(FSystemConnection);
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

procedure TFRE_CLISRV_APP.DebugTestProcedure;
begin
  //RangeManager_TestSuite;
  //sleep(2000);
  //halt;
end;

function TFRE_CLISRV_APP.PreStartupTerminatingCommands:boolean;
begin
  result := false;
  if HasOption('Z','calcsha') then
    begin
      result := true;
      CalcSHA1(GetOptionValue('Z','calcsha'));
    end;
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
  if HasOption('*','resetadmin') then
    cFRE_DB_RESET_ADMIN:=true;
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
  if HasOption('*','dumpdbo') then
    begin
      result := true;
      DumpDBO(GetOptionValue('*','dumpdbo'));
    end;
  if HasOption('*','expandrefs') then
    begin
      result := true;
      ExpandReferences(GetOptionValue('*','expandrefs'));
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
      RestoreDB(true,true,GetOptionValue('*','restoredb'),false);
    end;
  if HasOption('*','restoredbsch') then
    begin
      result := true;
      RestoreDB(true,true,GetOptionValue('*','restoredbsch'),true);
    end;
end;

function TFRE_CLISRV_APP.AfterInitDBTerminatingCommands: boolean;
begin
  result := false;
  if HasOption('A','adduser') then
    begin
      result:=true;
      AddUser(GetOptionValue('A','adduser'));
    end;
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
      SchemeDump(GetOptionValue('g','graph'),GetOptionValue('*','classfile'));
    end;
  if HasOption('z','testdump') then
    begin
      result := true;
      DumpAll(GetOptionValue('z','testdump'));
    end;
  if HasOption('o','overviewdump') then
    begin
      result := true;
      OverviewDump;
    end;
end;

function TFRE_CLISRV_APP.AfterSysDBConnectTerminatingCommands: boolean;
begin
  result := false;
  if HasOption('*','showapps') then
    begin
      result := true;
      ShowApps;
    end;
  if HasOption('*','showscheme') then
    begin
      result := true;
      DumpScheme;
    end;
  if HasOption('*','showdeploy') then
    begin
      result := true;
      ShowDeploy;
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
  writeln('Default style     : ',FDefaultStyle);
  writeln('---');
  writeln('');
end;

procedure TFRE_CLISRV_APP.ReCreateDB;
var conn : IFRE_DB_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  if GFRE_DB_PS_LAYER.DatabaseExists(FDBName) then
    begin
      writeln('>DROP USER DB '+FDBName);
      CheckDbResult(GFRE_DB_PS_LAYER.DeleteDatabase(FDBName,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS),'DELETE DB FAILED : '+FDBName);
      writeln('>USER DB '+FDBName+' DROPPED');
    end;
  writeln('>CREATE USER DB '+FDBName);
  CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase(FDBName,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS),'CREATE DB FAILED : '+FDBName);
  writeln('>USER DB '+FDBName+' CREATED');
  writeln('>CONNECT USER DB '+FDBName);
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect user db'); { initial admin (!) }
  writeln('>USER DB '+FDBName+' CONNECTED,DEFAULT COLLECTION INITIALIZED,DONE');
end;

procedure TFRE_CLISRV_APP.ReCreateSysDB;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  if GFRE_DB_PS_LAYER.DatabaseExists('SYSTEM') then
    begin
      writeln('>DROP SYSTEM DB');
      CheckDbResult(GFRE_DB_PS_LAYER.DeleteDatabase('SYSTEM',cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS),'DELETE SYSTEM DB FAILED');
      writeln('>SYSTEM DB DROPPED');
    end;
  writeln('>CREATE SYSTEM DB');
  CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase('SYSTEM',cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS),'CREATE SYSTEM DB FAILED');
  writeln('>SYSTEM DB CREATED');
  writeln('>CONNECT SYSTEM DB');
  GFRE_DB.Initialize_Extension_ObjectsBuild;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect('admin@system','admin'),'cannot connect system db'); { initial admin (!) }
  conn.Finalize;
  cG_OVERRIDE_USER:='admin@system';
  cG_OVERRIDE_PASS:='admin';
  writeln('>SYSTEM CONNECTED, DEFAULT COLLECTIONS INITIALIZED, DONE');
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
    schfn : String;
    fdbs  : IFRE_DB_Object;

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

  procedure SaveScheme;
  var sch : TFRE_DB_String;
      f   : TFileStream;

    procedure DWriteln(const msg:TFRE_DB_String);
     var line  : TFRE_DB_String;
         len   : integer;
         lens  : TFRE_DB_String;
     begin
       line := msg+#13#10;
       len  := Length(line);
       lens := IntToStr(len)+'C';
       f.Write(Pointer(@lens[1])^,Length(lens));
       f.Write(Pointer(@line[1])^,Length(line));
     end;

  begin
    write('FETCHING SERVER DATABASE SCHEME ');
    res := GFRE_DB_PS_LAYER.GetDatabaseScheme(fdbs);
    if res<>edb_OK then
      begin
        writeln(' ['+CFRE_DB_Errortype[res]+']');
        abort;
      end;
    write('DONE');
    sch := fdbs.GetAsJSONString(false,true);
    f := TFileStream.Create(schfn,fmCreate+fmOpenWrite);
    try
      DWriteln(sch);
    finally
      f.Free;
    end;
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
      sysfn := dir+DirectorySeparator+'sys.fdbb';
      dbfn  := dir+DirectorySeparator+'usr.fdbb';
      schfn := dir+DirectorySeparator+'sch.fdbb';
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
          writeln(res.AsString);
          abort;
        end;
      writeln('OK');
      SaveScheme;
      if sdb then
        begin
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
        conn.SYS.BackupDatabaseReadable(sfs,dbfs,@ProgressCB,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS)
      else
        scon.BackupDatabaseReadable(sfs,nil,@ProgressCB,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS);
    end
  else
    begin
     writeln('ABORTED');
    end;
end;

procedure TFRE_CLISRV_APP.RestoreDB(const adb, sdb: boolean; const dir: string; const override_with_live_scheme: boolean);
var s     : string;
    conn  : IFRE_DB_CONNECTION;
    scon  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;
    sfs   : TFileStream;
    dbfs  : TFileStream;
    sysfn : String;
    dbfn  : String;
    schfn : string;

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

  procedure CheckExistUserDB;
  begin
    if not FileExists(dbfn) then
      begin
        writeln('the user databasefile does not exist['+dbfn+'] !');
        abort;
      end;
  end;

  procedure CheckExistSysDB;
  begin
    if not FileExists(sysfn) then
      begin
        writeln('the system databasefile does not exist['+sysfn+'] !');
        abort;
      end;
  end;

  procedure CheckScheme;
  begin
    if override_with_live_scheme then
      exit;
    if not FileExists(schfn) then
      begin
        writeln('the scheme databasefile does not exist['+schfn+'] !');
        abort;
      end;
  end;

  procedure LoadScheme;
  var fdbs : IFRE_DB_Object;
      f    : TFileStream;

      function ReadElement : TFRE_DB_Object;
      var line  : TFRE_DB_String;
          elem  : Byte;
          count : Integer;
          pos   : integer;
      begin
        pos := 1;
        repeat
          elem := f.ReadByte;
          inc(pos);
          if char(elem)<>'C' then begin
            line := line + char(elem);
          end else break;
        until false;
        count := StrToInt(line);
        SetLength(line,count-2);
        f.ReadBuffer(line[1],count-2);
        f.ReadByte;f.ReadByte;
        result := TFRE_DB_Object.CreateFromJSONString(line);
      end;

  begin
    f := TFileStream.Create(schfn,fmOpenRead);
    try
      fdbs := ReadElement;
    finally
      f.Free;
    end;
    writeln('SETTING SCHEME');
    GFRE_DB.SetDatabasescheme(fdbs);
    //DumpScheme;
    writeln('SETTING SCHEME DONE');
  end;


begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  sysfn := dir+DirectorySeparator+'sys.fdbb';
  dbfn  := dir+DirectorySeparator+'usr.fdbb';
  schfn := dir+DirectorySeparator+'sch.fdbb';
  if not DirectoryExists(dir) then
    begin
      writeln('the backup directory does not exist['+dir+'] !');
      abort;
    end;
  if adb and not sdb then
    begin
      CheckExistUserDB;
      CheckScheme;
      writeln('Restore of database ['+FDBName+'] (y/N)');
    end;
  if sdb and not adb then
    begin
      CheckExistSysDB;
      CheckScheme;
      writeln('Restore of SYSTEM DB (y/N)');
    end;
  if adb and sdb then
    begin
      CheckExistUserDB;
      CheckExistSysDB;
      CheckScheme;
      writeln('Restore backup as database ['+FDBName+'] + [SYSTEM DB]  (y/N)');
    end;
  ReadLn(s);
  //s:='y'; // ignore force lazarusdebug
   if s='y' then
    begin
      if override_with_live_scheme then
        begin
          writeln('INTERNAL BUILDING SCHEMES');
          GFRE_DB.Initialize_Extension_ObjectsBuild;
        end
      else
        begin
          writeln('LOADING DB METADATA SCHEMES');
          LoadScheme;
        end;
      write('RECREATING / CONNECTING ['+FDBName+'] ');
      if adb then
        begin
          GFRE_DB_PS_LAYER.DeleteDatabase('SYSTEM',cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS);
          GFRE_DB_PS_LAYER.DeleteDatabase(FDBName,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS);
          CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase('SYSTEM',cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS));
          CheckDbResult(GFRE_DB_PS_LAYER.CreateDatabase(FDBName,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS));
          conn := GFRE_DBI.NewConnection;
          res  := conn.Connect(FDBName,'admin@system','admin');
        end
      else
        begin
          scon := GFRE_DBI.NewSysOnlyConnection;
          res  := scon.Connect('admin@system','admin');
        end;
      if res<>edb_OK then
        begin
          writeln(res.AsString);
          abort;
        end;
      writeln('OK');
      if sdb then
        begin
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
        conn.SYS.RestoreDatabaseReadable(sfs,dbfs,FDBName,@ProgressCB,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS)
      else
        scon.RestoreDatabaseReadable(sfs,nil,'',@ProgressCB,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS);

      CheckDbResult(GFRE_DB_PS_LAYER.DeployDatabaseScheme(GFRE_DB.GetDatabasescheme,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS));
    end
  else
    begin
      writeln('ABORTED');
    end;
end;

procedure TFRE_CLISRV_APP.AddUser(const userencoding: string);
var param    : TFRE_DB_StringArray;
    conn     : IFRE_DB_SYS_CONNECTION;
    username : TFRE_DB_String;
    domain   : TFRE_DB_String;
    domuid   : TFRE_DB_GUID;
    pass     : TFRE_DB_String;
    uclass   : TFRE_DB_String;
    internal : boolean;
begin
  FREDB_SeperateString(userencoding,',',param); { must be username@domain, password, class}
  if Length(param)<>3 then
    begin
      writeln('syntax error, must have 3 parameters');
      exit;
    end;
  username := param[0];
  pass     := param[1];
  uclass   := uppercase(param[2]);
  case uclass of
    'FEEDER','MIGHTYFEEDER' : internal := true;
    'WEBUSER'               : internal := false;
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'userclass must be WEBUSER or FEEDER');
  end;
  FREDB_SplitLocalatDomain(username,username,domain);
  try
    conn := GFRE_DBI.NewSysOnlyConnection;
    CheckDbResult(conn.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS));
    domuid := conn.GetCurrentUserTokenRef.GetDomainID(domain);
    GFRE_DB.Initialize_Extension_ObjectsBuild;
    CheckDbResult(conn.AddUser(username,domuid,pass,'Auto','Auto',nil,'',internal,'cli added user','cli added user',uclass));
  finally
    conn.Finalize;
  end;
end;

procedure TFRE_CLISRV_APP.GenerateTestdata;
var
  conn     : IFRE_DB_CONNECTION;
  domainId : TFRE_DB_Guid;
  res      : TFRE_DB_Errortype;
  fdbs     : IFRE_DB_Object;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  if not FDeployed then
    _CheckNoCustomextensionsSet;

  res := GFRE_DB_PS_LAYER.GetDatabaseScheme(fdbs);
  if res = edb_OK then
   begin
      GFRE_DB.SetDatabasescheme(fdbs);
      FChosenExtensionList := ListFromString(GFRE_DB.GetDeployedExtensionlist);
      RegisterExtensions;
      GFRE_DB.InstantiateApplicationObjects;
   end;

  { a testdomain is created globally for all extensiosn}
  conn := GFRE_DBI.NewConnection;
  try
    CheckDbResult(conn.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),' could not login into '+FDBName+' for testdomain creation');
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
  GFRE_DBI_REG_EXTMGR.GenerateTestData4Exts(FChosenExtensionList,FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
end;

procedure TFRE_CLISRV_APP.DoUnitTest;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  GFRE_DBI_REG_EXTMGR.GenerateUnitTestsdata(FChosenExtensionList,FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
end;

procedure TFRE_CLISRV_APP.InitExtensions;
var conn : IFRE_DB_CONNECTION;
    res  : TFRE_DB_Errortype;
    fdbs : IFRE_DB_Object;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  if not FDeployed then
    _CheckNoCustomextensionsSet;

  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect system db');

  res := GFRE_DB_PS_LAYER.GetDatabaseScheme(fdbs);
  if res = edb_OK then
    GFRE_DB.SetDatabasescheme(fdbs)
  else
    begin
      writeln('Could not get the databasescheme ['+CFRE_DB_Errortype[res]+'], you need to deploy first');
      abort;
    end;
  writeln('INITIALIZING DATABASE FOR EXTENSIONS : '+GFRE_DB.GetDeploymentInfo);
  GDBPS_DISABLE_NOTIFY := true;
  FChosenExtensionList := ListFromString(GFRE_DB.GetDeployedExtensionlist);
  RegisterExtensions;
  GFRE_DB.InstantiateApplicationObjects;
  GFRE_DBI.DBInitializeAllSystemClasses(conn);
  GFRE_DBI.DBInitializeAllExClasses(conn);
  conn.Finalize;
  GFRE_DBI_REG_EXTMGR.InitDatabase4Extensions(FChosenExtensionList,FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
end;

procedure TFRE_CLISRV_APP.ShowVersions;
var conn : IFRE_DB_SYS_CONNECTION;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  CONN := GFRE_DBI.NewSysOnlyConnection;
  CheckDbResult(CONN.Connect(cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect system db');
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

procedure TFRE_CLISRV_APP.ShowApps;
var apa : TFRE_DB_APPLICATION_ARRAY;
      i : integer;
begin
  writeln('AVAILABLE APPS:');
  apa := GFRE_DB.GetApps;
  for i:=0 to high(apa) do
    begin
      writeln(i,' : ',apa[i].AppClassName,' ',apa[i].ObjectName);
    end;
end;

procedure TFRE_CLISRV_APP.ShowDeploy;
begin
  exit; { deploy info is written on start in any case }
end;

procedure TFRE_CLISRV_APP.DumpScheme;
begin
  writeln(GFRE_DB.GetDatabasescheme.DumpToString);
end;


procedure TFRE_CLISRV_APP.RemoveExtensions;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  writeln('Remove apps for extensions :'+uppercase(FChosenExtensionList.Commatext));
  GFRE_DBI_REG_EXTMGR.Remove4Extensions(FChosenExtensionList,FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
end;


procedure TFRE_CLISRV_APP.RegisterExtensions;
begin
  FRE_DBBASE.Register_DB_Extensions;
  GFRE_DBI_REG_EXTMGR.RegisterExtensions4DB(FChosenExtensionList);
  FRE_BASE_SERVER.RegisterLogin;
end;

procedure TFRE_CLISRV_APP.DeployDatabaseScheme(deploy_revision: string);
var conn : IFRE_DB_CONNECTION;
    extl : string;
begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  CheckDbResult(GFRE_DB.ClearSystemSchemes);
  RegisterExtensions;
  extl := FChosenExtensionList.Commatext;
  if extl='' then
    begin
      writeln('you have to choose some extensions to deploy');
      abort;
    end;
  //CONN := GFRE_DBI.NewConnection;
  //CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS));
  if deploy_revision='' then
    deploy_revision:=GetEnvironmentVariable('LOGNAME')+'/'+ApplicationName;
  writeln('>BUILDING METADATA SCHEMES : ',extl);
  GFRE_DB.Initialize_Extension_ObjectsBuild;
  GFRE_DB.SetupDeploymentInfo(extl,deploy_revision);
  writeln('>BUILDING SCHEME DONE');
  writeln('>DEPLOYING SCHEME');
  CheckDbResult(GFRE_DB_PS_LAYER.DeployDatabaseScheme(GFRE_DB.GetDatabasescheme,cFRE_PL_ADMIN_USER,cFRE_PL_ADMIN_PASS));
  writeln('>METADATA DEPLOYMENT DONE');
  writeln(GFRE_DB.GetDeploymentInfo);
  FDeployed := true;
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
  InitMinimal;
  if cFRE_PS_LAYER_USE_EMBEDDED then
    begin
      GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'db')
      //GFRE_DB_P.S_LAYER := Get_PersistanceLayer_PS_Net(cFRE_PS_LAYER_HOST,cFRE_PS_LAYER_IP,cFRE_PS_LAYER_PORT,true);
    end
  else
    GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Net(cFRE_PS_LAYER_HOST,cFRE_PS_LAYER_IP,cFRE_PS_LAYER_PORT,false);

  InitTransfromManager;
  Init4Server;
  GFRE_DBI.LocalZone := cFRE_SERVER_DEFAULT_TIMEZONE;
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
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Info,'*',flra_DropEntry);
    //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Debug,'*',flra_DropEntry);
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
  //Setup_APS_COMM_Logging;
  //Setup_Persistance_Layer_Logging;
  //Setup_FlexcomLogging;
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

procedure TFRE_CLISRV_APP.SchemeDump(const filename: string; const classfile: string);
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
      sconn.Connect(cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
      GFRE_DB.Initialize_Extension_ObjectsBuild;
      sconn.DrawScheme(mems,classfile);
    end else begin
      lconn := GFRE_DBI.NewConnection;
      res   := lconn.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS);
      GFRE_DB.Initialize_Extension_ObjectsBuild;
      if res<>edb_OK then begin
        WriteLn('SCHEME DUMP CHECK CONNECT FAILED : ',CFRE_DB_Errortype[res]);
      end;
      lconn.DrawScheme(mems,classfile);
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
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect db');
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

procedure TFRE_CLISRV_APP.OverviewDump;
var conn   : IFRE_DB_CONNECTION;

    procedure WriteALine(const line:string);
    begin
      writeln(line);
    end;

begin
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect db');
  conn.OverviewDump(@WriteALine);
end;

procedure TFRE_CLISRV_APP.ExpandReferences(const input: string);
var conn : IFRE_DB_CONNECTION;
    i    : NativeInt;
    uids : TFRE_DB_GUIDArray;
    refs : TFRE_DB_GUIDArray;
    u,r  : string;
    ua,rl: TFRE_DB_StringArray;
    rlcs : TFRE_DB_NameTypeRLArray;
    uido : IFRE_DB_ObjectArray;
    refo : IFRE_DB_ObjectArray;

begin
  if not GFRE_BT.SepLeftRight(input,'/',u,r) then
    begin
      writeln('You must provide uidlist/RLC e.g 01a0baf86f35800dfc7c1efd7b9ef165,3fb4cc3eb902d947600de91e78592943/DATALINKPARENT>>TFRE_DB_ZONE,SERVICEPARENT>>,..');
      exit;
    end;
  FREDB_SeperateString(u,',',ua);
  uids := FREDB_StringArray2UidArray(ua);
  FREDB_SeperateString(r,',',rl);
  rlcs := FREDB_StringArray2NametypeRLArray(rl);
  _CheckDBNameSupplied;
  _CheckAdminUserSupplied;
  _CheckAdminPassSupplied;
  _CheckNoCustomextensionsSet;
  CONN := GFRE_DBI.NewConnection;
  CheckDbResult(CONN.Connect(FDBName,cG_OVERRIDE_USER,cG_OVERRIDE_PASS),'cannot connect system db');
  conn.ExpandReferences(uids,rlcs,refs);
  conn.BulkFetch(uids,uido);
  conn.BulkFetch(refs,refo);
  writeln('UIDS');
  writeln(FREDB_GuidArray2String(uids));
  for i:=0 to high(uido) do
    begin
      writeln(i,':>----');
      writeln(uido[i].DumpToString());
      writeln(i,':<----');
    end;
  writeln('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>');
  writeln('RESOLVE TO');
  writeln(FREDB_GuidArray2String(refs));
  for i:=0 to high(refo) do
    begin
      writeln(i,':>----');
      writeln(refo[i].DumpToString());
      writeln(i,':<----');
    end;
  writeln('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<');
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

