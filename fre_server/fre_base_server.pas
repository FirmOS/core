unit fre_base_server;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
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

uses  classes, sysutils,fre_aps_interface,fos_fcom_types,fos_tool_interfaces,baseunix,
      fre_http_srvhandler,fre_db_interface,fre_system,fos_interlocked,
      fre_db_core,fre_webssocket_baseserver,fre_db_common,fre_fcom_ssl,
      fre_http_tools,fos_redblacktree_gen,fre_sys_base_cs,zstream,fos_art_tree,
      fre_wapp_dojo,fre_db_tasker,fre_db_login;

procedure RegisterLogin;

type
  { TFRE_BASE_SERVER }

  TFRE_DB_LOG_TYPE=(log_Warning,log_Error,log_INFO);

  TFRE_UserSession_Tree         = specialize TGFOS_RBTree<string,TFRE_DB_UserSession>;

  TFRE_BASE_SERVER=class(TObject,IFRE_HTTP_BASESERVER,IFRE_DB_NetServer)
  private
    FOpenDatabaseList  : OFRE_DB_ConnectionArr;
    FUserSessionsTree  : TFRE_UserSession_Tree;  //TODO-> make array
    FDispatcher        : TFRE_HTTP_URL_DISPATCHER;
    FSessionTreeLock   : IFOS_LOCK;
    FSessiontimer      : IFRE_APSC_TIMER;
    FTaskerTimer       : IFRE_APSC_TIMER;
    FStaticHTTPMeta    : TFRE_ART_TREE;
    FTotalZippedFiles  : NativeInt;
    FTotalBytesCached  : NativeInt;
    FTotalZFilesCache  : NativeInt;
    FTotalPFilesCache  : NativeInt;
    FTaskerSession     : TFRE_DB_UserSession;
    FTaskerApp         : TFRE_DB_TASKER;
    FTaskerAPP_UID     : TGuid;
    FTaskerAPP_Class   : Shortstring;

    cont               : boolean;
    Foutput            : String;
    FDefaultAPP_UID    : TGuid;
    FDefaultAPP_Class  : Shortstring;
    FSystemConnection  : TFRE_DB_SYSTEM_CONNECTION;

    FSSL_CTX             : PSSL_CTX;
    FTerminating         : boolean;

    procedure      _SetupHttpBaseServer                      ;
    function       BindInitialSession                        (const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; out   session : TFRE_DB_UserSession;const old_session_id:string;const interactive_session:boolean):boolean;
    function       GetImpersonatedDatabaseConnection         (const dbname,username,pass:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       GetDBWithServerRights                     (const dbname:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       GetImpersonatedDatabaseConnectionSession  (const dbname,username,pass,default_app : string;const default_uid_path : TFRE_DB_GUIDArray ; const session_net_desc : String ; out dbs:TFRE_DB_UserSession):TFRE_DB_Errortype;
    function       CheckUserNamePW                           (username,pass:TFRE_DB_String) : TFRE_DB_Errortype;
    procedure      TIM_SessionHandler                        (const timer : IFRE_APSC_TIMER;const flaf1,flag2:boolean);
    procedure      TIM_TaskerHandler                         (const timer : IFRE_APSC_TIMER;const flag1,flag2:boolean);
    function       ExistsUserSessionForUserLocked            (const username:string;out other_session:TFRE_DB_UserSession):boolean;
    function       ExistsUserSessionForKeyLocked             (const key     :string;out other_session:TFRE_DB_UserSession):boolean;
    function       FetchPublisherSessionLocked               (const rcall,rmeth:TFRE_DB_NameType;out ses : TFRE_DB_UserSession ; out right:TFRE_DB_String):boolean;
    function       FetchSessionByIdLocked                    (const sesid : TFRE_DB_String ; var ses : TFRE_DB_UserSession):boolean;
    procedure      ForAllSessionsLocked                      (const iterator : TFRE_DB_SessionIterator ; var halt : boolean); // If halt, then the dir and the session remain locked!
    function       SendDelegatedContentToClient              (sessionID : TFRE_DB_String ; const content : TFRE_DB_CONTENT_DESC):boolean;

    function       FetchStreamDBO                            (const enc_sessionid, enc_uid: string; var end_field: TFRE_DB_NameTypeRL; var lcontent: TFRE_DB_RawByteString; var stored_ct: TFRE_DB_String; var stored_etag: TFRE_DB_String): boolean;
    function       GetETag                                   (const filename: string; const filesize: NativeUint;const moddate: TFRE_DB_DateTime64):String;
  public
    DefaultDatabase              : String;
    TransFormFunc                : TFRE_DB_TRANSFORM_FUNCTION;

    HTTPWS_Listener              : IFRE_APSC_LISTENER;
    HTTPSWSS_Listener            : IFRE_APSC_LISTENER;
    FLEX_Listener                : IFRE_APSC_LISTENER;

    constructor Create           (const defaultdbname : string);
    destructor  Destroy           ; override;
    procedure   HandleSignals     (const signum : NativeUint);
    procedure   Setup             ;
    procedure   Terminate         ;
    procedure   ReInit            ;
    procedure   DeploymentDump    ;
    procedure   Interrupt         ;
    function    GetName           : String;

    procedure Finalize              ;
    function  FetchMetaEntry        (file_path:String;var metae : TFRE_HTTP_METAENTRY):boolean; // Concurrent Entry (!)

    procedure DispatchHTTPRequest   (const connection_object:TObject;const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);

    procedure APSC_NewListener      (const LISTENER : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure APSC_NewChannel       (const channel  : IFRE_APSC_CHANNEL  ; const state : TAPSC_ChannelState);
  end;


implementation

//uses FRE_DB_LOGIN;
{ TFRE_BASE_SERVER }





procedure TFRE_BASE_SERVER._SetupHttpBaseServer;
var dummy:TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;

  procedure _SetupZippingAndCaching;
  var list    : IFOS_STRINGS;
         i    : NativeInt;
     cachexts : TFRE_DB_StringArray;

    function CheckFilenameExtension(const filename : string ; const exclude_zip : boolean):boolean;
    begin
      result :=  (pos('.git',filename)>0) or
                 (pos('.DS_Store',filename)>0) or
                 (pos('.sh',filename)>0) or
                 ((pos('.gzip',filename)>0) and exclude_zip);
    end;

    procedure CleanZipFiles(filename:string);
    var ext   : String;
         fn   : string;
       path   : String;
       fnbase : String;
       info   : stat;
       gzip   : TGZFileStream;
       plain  : TFileStream;

    begin
      if CheckFilenameExtension(filename,false) then
        exit;
      if pos('.gzip',filename)>0 then
        begin
          GFRE_DB.LogInfo(dblc_HTTP_ZIP,'FORCE DELETING ZIPPED FILE [%s]',[filename]);
          if not DeleteFile(filename) then
            GFRE_DB.LogError(dblc_HTTP_ZIP,'(!) FAILED : FORCE DELETING ZIPPED FILE [%s]',[filename]);
        end;
    end;

    procedure BuildMetaStructure(filename:string ; const idx : NativeInt);
    var dummy : NativeUint;
        ext   : String;
         fn   : string;
        fnz   : string;
       path   : String;
       fnbase : String;
       info   : stat;
       zinfo  : stat;
       metae  : TFRE_HTTP_METAENTRY;

    begin
      if CheckFilenameExtension(filename,true) then
        exit;
      ext    := ExtractFileExt(filename);
      fn     := ExtractFileName(filename);
      SetLength(fn,Length(fn)-Length(ext));
      path   := ExtractFilePath(filename);
      fnbase := path+fn;
      fnz    := filename+'.gzip';
      if fpstat(fnbase+ext,info)<>0 then
        GFRE_BT.CriticalAbort('Process www content cannot stat : '+filename+' '+fnbase+ext);
      metae := TFRE_HTTP_METAENTRY.create;
      metae.Filename         := filename;
      metae.FileExtension    := ext;
      metae.ModificationDate := GFRE_DT.DateTimeToDBDateTime64(FileDateToDateTime(info.st_mtime));
      metae.ModFileDate      := info.st_mtime;
      metae.Size             := info.st_size;
      metae.Cached           := false;
      metae.ZippedExist      := false;
      metae.ETag             := GetETag(filename,metae.size,metae.ModificationDate);
      metae.MimeType         := FREDB_Filename2MimeType(metae.Filename);
      metae.ZippedExist := FileExists(fnz);
      if metae.ZippedExist then
        begin
          if fpstat(fnz,zinfo)<>0 then
            GFRE_BT.CriticalAbort('Process www content cannot stat zipped : '+fnz);
          if zinfo.st_mtime<>info.st_mtime then
            begin
              GFRE_DB.LogNotice(dblc_HTTP_ZIP,'Zipped file modification dates differ, must delete : '+fnz+' Originaldate: '+GFRE_DT.ToStrFOS(GFRE_DT.DateTimeToDBDateTime64(FileDateToDateTime(info.st_mtime)))+' Zipped: '+GFRE_DT.ToStrFOS(GFRE_DT.DateTimeToDBDateTime64(FileDateToDateTime(zinfo.st_mtime))));
              if not DeleteFile(fnz) then
                gfre_bt.CriticalAbort('cannot delete zipped http server file : '+fnz);
              metae.ZippedExist := false;
            end
          else
            begin
              metae.ZippedSize  := zinfo.st_size;
            end;
        end
      else
        begin
          metae.ZippedSize  := 0;
        end;
      metae.CalcRatio;
      if not FStaticHTTPMeta.InsertStringKey(filename,FREDB_ObjectToPtrUInt(metae)) then
        GFRE_BT.CriticalAbort('cannot add metaentry '+filename+' to metacache');
      if not FStaticHTTPMeta.ExistsStringKey(filename,dummy) then
        GFRE_BT.CriticalAbort('paranoia check failed, add metaentry '+filename+' does not exist in metacache');
    end;

    procedure ZipFiles(var value : NativeUint);
    var metae  : TFRE_HTTP_METAENTRY;
        gzip   : TGZFileStream;
        plain  : TFileStream;
        fnz,fn : string;
        info   : stat;
    begin
      metae := TFRE_HTTP_METAENTRY(FREDB_PtrUIntToObject(value));
      if metae.ZippedExist=false then
        begin
          fn  := metae.Filename;
          fnz := metae.Filename+'.gzip';
          GFRE_DB.LogDebug(dblc_HTTP_ZIP,'ZIPPING FILE [%s]',[fn]);
          try
            try
              plain := TFileStream.Create(fn,fmOpenRead);
              gzip := TGZFileStream.create(fnz,gzopenwrite);
              gzip.CopyFrom(plain,0);
            except
              on e:exception do
               begin
                 fn := fn+' '+e.Message;
                 GFRE_BT.CriticalAbort('cannot zip file : '+fn);
               end;
            end;
          finally
            plain.free;
            gzip.free;
          end;
          if FileSetDate(fnz,metae.ModFileDate)<>0 then
            GFRE_BT.CriticalAbort('cannot change modification timestamp of zip file : '+fnz);
          if FpStat(fnz,info)<>0 then
            GFRE_BT.CriticalAbort('cannot stat zip file : '+fnz);
          metae.ZippedSize:=info.st_size;
          metae.CalcRatio;
          if info.st_mtime<>metae.ModFileDate then
            GFRE_BT.CriticalAbort('paranoia modification time change failed : '+fnz);
          GFRE_DB.LogInfo(dblc_HTTP_ZIP,'ZIPPED FILE [%s Size %d kb changed to %d kb, ratio %s]',[metae.Filename,metae.Size div 1024,metae.ZippedSize div 1024,FormatFloat('##0.00%',metae.ZipRatio)]);
          metae.ZippedExist:=true;
        end;
      inc(FTotalZippedFiles);
    end;

    procedure CacheFiles(var value : NativeUint);
    var metae : TFRE_HTTP_METAENTRY;
        gzip  : TGZFileStream;
        plain : TFileStream;
        fnz   : string;
        info  : stat;
    begin
      metae := TFRE_HTTP_METAENTRY(FREDB_PtrUIntToObject(value));
      if FREDB_StringInArray(LowerCase(metae.FileExtension),cachexts) then
        begin
          if (metae.ZippedExist) and
             (metae.ZipRatio > 5) then
            begin
              inc(FTotalZFilesCache);
              GFRE_DB.LogDebug(dblc_HTTP_CACHE,'Selected zipped file [%s] for caching size=[%2.2f kb] Ratio %s ',[metae.Filename+'.gzip',metae.ZippedSize / 1024,FormatFloat('##0.00%',metae.ZipRatio)]);
              metae.Cached           := true;
              metae.HasZippedCache   := true;
              metae.HasUnZippedCache := false;
              inc(FTotalBytesCached,metae.ZippedSize);
              metae.ContentZipped := TMemoryStream.Create;
              metae.ContentZipped.LoadFromFile(metae.Filename+'.gzip');
              if metae.ContentZipped.Size<>metae.ZippedSize then
                GFRE_BT.CriticalAbort('zipped content caching failed for %s, file size and stream size differ [%d<>%d]',[metae.Filename,metae.ZippedSize,metae.ContentZipped.Size]);
            end
          else
            begin
              inc(FTotalPFilesCache);
              GFRE_DB.LogDebug(dblc_HTTP_CACHE,'Selected plain file [%s] for caching size=[%2.2f kb] Ratio %s ',[metae.Filename,metae.Size / 1024,FormatFloat('##0.00%',metae.ZipRatio)]);
              metae.Cached           := true;
              metae.HasZippedCache   := false;
              metae.HasUnZippedCache := true;
              metae.ContentUnZipped := TMemoryStream.Create;
              metae.ContentUnZipped.LoadFromFile(metae.Filename);
              if metae.ContentUnZipped.Size<>metae.Size then
                GFRE_BT.CriticalAbort('plain content caching failed for %s, file size and stream size differ [%d<>%d]',[metae.Filename,metae.Size,metae.ContentUnZipped.Size]);
              inc(FTotalBytesCached,metae.Size);
            end;
        end;
    end;

  begin
    list := GFRE_TF.Get_FOS_Strings;
    GFRE_BT.List_Files(cFRE_SERVER_WWW_ROOT_DIR,list,10000,true);
    if cFRE_FORCE_CLEAN_ZIP_HTTP_FILES then
      begin
        GFRE_DB.LogNotice(dblc_HTTP_ZIP,'FORCE CLEANING ALL GZIPPED FILES');
        for i:=0 to list.Count-1 do
          CleanZipFiles(list[i]);
      end;
    for i:=0 to list.Count-1 do
      BuildMetaStructure(list[i],i);
    if cFRE_BUILD_ZIP_HTTP_FILES then
      begin
        GFRE_DB.LogNotice(dblc_HTTP_ZIP,'GZIPPING FILES FOR HTTP TRANSFER');
        FTotalZippedFiles:=0;
        FStaticHTTPMeta.LinearScan(@Zipfiles);
        GFRE_DB.LogNotice(dblc_HTTP_ZIP,'[%d] GZIPED FILES READY',[FTotalZippedFiles]);
      end;
    if cFRE_USE_STATIC_CACHE then
      begin
        FREDB_SeperateString(cFRE_STATIC_HTTP_CACHE_EXTS,',',cachexts);
        for i:=0 to high(cachexts) do
          cachexts[i] := LowerCase(cachexts[i]);
        GFRE_DB.LogNotice(dblc_HTTP_CACHE,'BUILDING STATIC CACHE FOR TYPES [%s]',[cFRE_STATIC_HTTP_CACHE_EXTS]);
        FStaticHTTPMeta.LinearScan(@Cachefiles);
        GFRE_DB.LogNotice(dblc_HTTP_CACHE,'TOTALLY CACHED PLAIN/ZIPPED [%d / %d] FILES AND %s [MB]',[FTotalPFilesCache,FTotalZFilesCache,FormatFloat('###,###,###,##0.00',FTotalBytesCached / (1024*1024))]);
      end
    else
      GFRE_DB.LogNotice(dblc_HTTP_CACHE,'NOT USING STATIC CACHING');
  end;

  procedure InitHullHTML;
  var res_main        :  TFRE_DB_MAIN_DESC;
      FHull_HTML      : string;
      FHull_CT        : string;
      FDefaultsession : TFRE_DB_UserSession;

  begin
//       res_main  := TFRE_DB_MAIN_DESC.create.Describe(cFRE_WEB_STYLE,'https://tracker.firmos.at/s/en_UK-wu9k4g-1988229788/6097/12/1.4.0-m2/_/download/batch/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs.js?collectorId=5e38a693');
    res_main  := TFRE_DB_MAIN_DESC.create.Describe(cFRE_WEB_STYLE);
    CheckDbResult(GetImpersonatedDatabaseConnectionSession(DefaultDatabase,'GUEST'+'@'+CFRE_DB_SYS_DOMAIN_NAME,'',FDefaultAPP_Class,TFRE_DB_GUIDArray.Create(FDefaultAPP_UID),'NONET',FDefaultSession));
    TransFormFunc(FDefaultSession,fct_SyncReply,res_main,FHull_HTML,FHull_CT,false,fdbtt_get2html);
    FDefaultsession.Free;
    GFRE_BT.StringToFile(cFRE_SERVER_WWW_ROOT_DIR+DirectorySeparator+cFRE_SERVER_WWW_ROOT_FILENAME,FHull_HTML);
  end;


begin
  FDispatcher  := TFRE_HTTP_URL_DISPATCHER.Create;
  FREDB_LoadMimetypes('');
  InitHullHTML;
  FDispatcher.RegisterDefaultProvider('',@dummy.Default_Provider);
  _SetupZippingAndCaching;
end;

destructor TFRE_BASE_SERVER.Destroy;
  procedure _DisconnectAllDatabases;
    procedure Disconnect(const conn : TFRE_DB_CONNECTION);
    begin
      conn.Free;
    end;
  begin
    FOpenDatabaseList.ForAll(@Disconnect);
  end;

  procedure StoreSessionData(const session:TFRE_DB_UserSession);
  begin
    GFRE_DBI.LogDebug(dblc_SERVER,'SERVER : FINALIZING SESSION : [%s] USER[%s]',[session.GetSessionID,session.GetUsername]);
    session.Free;
  end;

  function CloseAll(const dbc : TFRE_DB_CONNECTION):boolean;
  begin
    result := false;
    GFRE_DBI.LogDebug(dblc_SERVER,'FINALIZING DATBASE DBC [%s]',[dbc.ConnectedName]);
    dbc.Free;
  end;

  var dbc : IFRE_DB_CONNECTION;

begin
  GFRE_DBI.LogDebug(dblc_SERVER,'ENTER CLOSING SEQUENCE');
  FTerminating := true;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItems(@StoreSessionData);
  finally
    FSessionTreeLock.Release;
  end;
  FDispatcher.Free;
  FUserSessionsTree.Free;
  FSessionTreeLock.Finalize;

  FSystemConnection.Free;
  FSystemConnection:=nil;
  GFRE_DBI.LogInfo(dblc_SERVER,'SERVER SHUTDOWN DONE');
  GFRE_DB_PS_LAYER.SyncSnapshot(true);

  GFRE_DBI.LogDebug(dblc_SERVER,'DATABASE COUNT [%d]',[FOpenDatabaseList.Count]);
  FOpenDatabaseList.ForAllBrk(@CloseAll);

  inherited Destroy;
end;

procedure TFRE_BASE_SERVER.HandleSignals(const signum: NativeUint);
begin
  case signum of
    SIGTERM : Terminate;
    SIGHUP  : ReInit;
    SIGUSR1 : DeploymentDump;
    SIGINT  : Interrupt;
    SIGALRM : Interrupt;
    else
      writeln('UNHANDLED CATCHED SIGNAL ',signum);
  end;
end;

procedure TFRE_BASE_SERVER.Setup;

     procedure _ConnectAllDatabases;
     var i       : Integer;
         dblist  : IFOS_STRINGS;
         ndbc    : TFRE_DB_CONNECTION;
         dbname  : string;
         res     : TFRE_DB_Errortype;
         log_txt : string;
         app     : TFRE_DB_APPLICATION;
     begin
       dblist := GFRE_DB_PS_LAYER.DatabaseList;
       GFRE_DB.LogInfo(dblc_SERVER,'START SERVING DATABASES [%s]',[dblist.Commatext]);
       FSystemConnection := GFRE_DB.NewDirectSysConnection;
       res := FSystemConnection.Connect(cFRE_ADMIN_USER,cFRE_ADMIN_PASS);  // direct admin connect
       if res<>edb_OK then begin
         FSystemConnection.Free;
         FSystemConnection := nil;
         GFRE_DB.LogError(dblc_SERVER,'SERVING SYSTEM DATABASE failed due to [%s]',[CFRE_DB_Errortype[res]]);
         GFRE_BT.CriticalAbort('CANNOT SERVE SYSTEM DB [%s]',[CFRE_DB_Errortype[res]]);
       end;

       if not GFRE_DB.GetAppInstanceByClass(TFRE_DB_LOGIN,app) then
         GFRE_BT.CriticalAbort('cannot fetch login app')
       else
         begin
           FDefaultAPP_UID       := app.UID;
           FDefaultAPP_Class     := app.ClassName;
           cFRE_DB_LOGIN_APP_UID := FDefaultAPP_UID;
           cFRE_DB_LOGIN_APP     := app;
         end;
       if not GFRE_DB.GetAppInstanceByClass(TFRE_DB_TASKER,app) then
         GFRE_BT.CriticalAbort('cannot fetch tasker app')
       else
         begin
           FTaskerApp       := app as TFRE_DB_TASKER;
           FTaskerAPP_UID   := app.UID;
           FTaskerAPP_Class := app.ClassName;
         end;

       for i := 0 to dblist.Count-1 do begin
         dbname := Uppercase(dblist[i]);
         if dbname='SYSTEM' then begin
           ;
         end else begin
           //InfoLog(5,'CONNECTING [%s]',[dbname]);
           ndbc := GFRE_DB.NewConnection(true);
           res  := ndbc.Connect(dbname,cFRE_ADMIN_USER,cFRE_ADMIN_PASS,FSystemConnection); // direct admin connect
           if res<>edb_OK then begin
             GFRE_DB.LogError(dblc_EXCEPTION,'SERVING DATABASE [%s] failed due to [%s]',[dbname,CFRE_DB_Errortype[res]]);
           end else begin
             //InfoLog(5,'CONNECTED [%s]',[dbname]);
             FOpenDatabaseList.Add2Array(ndbc);
           end;
         end;
       end;
       //res := GetImpersonatedDatabaseConnectionSession(DefaultDatabase,'GUEST'+'@'+CFRE_DB_SYS_DOMAIN_NAME,'',FDefaultAPP_Class,TFRE_DB_GUIDArray.Create(FDefaultAPP_UID),FDefaultSession);
       //CheckDbResult(res,'COULD NOT CONNECT DEFAULT DB / APP / WITH GUEST ACCESS');
     end;

     procedure _ServerInitializeApps;
     var    apps : IFRE_DB_APPLICATION_ARRAY;
         loginapp: IFRE_DB_APPLICATION;
               i : Integer;
             dbs : IFRE_DB_CONNECTION;
     begin
       GFRE_DBI.FetchApplications(apps,loginapp);
       for i:=0 to high(apps) do begin
         if GetDBWithServerRights(DefaultDatabase,dbs)=edb_OK then begin // May be an array with db per app
           (apps[i].Implementor_HC as TFRE_DB_APPLICATION).ServerInitialize(dbs);
         end else begin
           GFRE_BT.CriticalAbort('CANNOT SERVERINITIALIZE APPLICATION [APP:%s DB: %s]',[apps[i].AppClassName,DefaultDatabase]);
         end;
       end;
     end;

     function MyGetPW:string;
     begin
       result := '0000';
     end;

     procedure _SetupSSL_Ctx;
     var fre_ssl_i     : TFRE_SSL_INFO;
     begin
       with fre_ssl_i do
         begin
           ssl_type := fssl_TLSv1;
           cerifificate_file     := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_CERT_FILE;
           private_key_file      := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_PRIVATE_KEY_FILE;
           root_ca_file          := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_ROOT_CA_FILE;
           fail_no_peer_cert     := false;
           verify_peer           := false;
           verify_peer_cert_once := false;
           IsServer              := true;
           cipher_suites         := 'DEFAULT';
         end;
       FSSL_CTX := FRE_Setup_SSL_Context(@fre_ssl_i);
     end;

     procedure InitializeTaskerSession; { the tasker session is not in the session tree  (unbound - free at server terminate ) }
     var res : TFRE_DB_Errortype;
     begin
       res := GetImpersonatedDatabaseConnectionSession(DefaultDatabase,'tasker@'+CFRE_DB_SYS_DOMAIN_NAME,cFRE_TASKER_PASS,FTaskerAPP_Class,TFRE_DB_GUIDArray.create(FTaskerAPP_UID),'NONET',FTaskerSession);
       if res<>edb_OK then
         GFRE_BT.CriticalAbort('could not initialize tasker session : '+CFRE_DB_Errortype[res]);
       FTaskerApp.InitSession(FTaskerSession); { HACK: Session Initialization should have happened 3 lines above due to proper rights}
     end;

begin
  TransFormFunc     := @FRE_WAPP_DOJO.TransformInvocation;
  FOpenDatabaseList.init;

  GFRE_DB.LogNotice(dblc_SERVER,'FirmOS System Base Server Node Startup');

  FUserSessionsTree  := TFRE_UserSession_Tree.Create(@Default_RB_String_Compare);
  GFRE_TF.Get_Lock(FSessionTreeLock);
  GFRE_SC.SetSingnalCB(@HandleSignals);

  _ConnectAllDatabases;
  _ServerinitializeApps;
  _SetupSSL_Ctx;
  _SetupHttpBaseServer;

  InitializeTaskerSession;

  GFRE_SC.SetNewListenerCB(@APSC_NewListener);
  GFRE_SC.AddListener_TCP ('*','44000','HTTP/WS');
  GFRE_SC.SetNewChannelCB(@APSC_NewChannel);

  GFRE_SC.AddListener_TCP ('*','44001','FLEX');
  GFRE_SC.SetNewListenerCB(@APSC_NewListener);
  GFRE_SC.SetNewChannelCB(@APSC_NewChannel);

  GFRE_SC.AddListener_TCP ('*','44003','HTTPS/WSS');
  GFRE_SC.SetNewListenerCB(@APSC_NewListener);
  GFRE_SC.SetNewChannelCB(@APSC_NewChannel);

  FSessiontimer := GFRE_SC.AddTimer('SESSION',1000,@TIM_SessionHandler);
  FTaskerTimer  := GFRE_SC.AddTimer('TASKER',1000,@TIM_TaskerHandler);
end;


procedure TFRE_BASE_SERVER.Terminate;
begin
  GFRE_DB.LogNotice(dblc_SERVER,'TERMINATE SIGNAL RECEIVED',[]);
  GFRE_SC.RequestTerminate;
end;

procedure TFRE_BASE_SERVER.ReInit;
begin
  GFRE_DB_PS_LAYER.SyncSnapshot(false);
end;

procedure TFRE_BASE_SERVER.DeploymentDump;
var deployexts : TFRE_DB_StringArray;
    deployccat : TFRE_DB_StringArray;
    ext_count  : TFRE_DB_Int32Array;
    pos,i      : NativeInt;
    stats      : string;
    sorttree   : TFRE_ART_TREE;
    fn         : string;

  procedure DeploymentStage1(var value : NativeUint);
  var metae : TFRE_HTTP_METAENTRY;
  begin
     metae := TFRE_HTTP_METAENTRY(FREDB_PtrUIntToObject(value));
     pos := FREDB_StringInArrayIdx(LowerCase(metae.FileExtension),deployexts);
     if pos<>-1 then
       begin
         if metae.AccessOrder>0 then
           begin
             GFRE_DB.LogDebug(dblc_SERVER,'>DEPLOYMENT SCAN : [%s] [%d]',[metae.Filename,metae.AccessOrder]);
             inc(ext_count[pos]);
             if not sorttree.InsertUInt64Key(metae.AccessOrder,value) then
               GFRE_DB.LogError(dblc_SERVER,'>DEPLOYMENT SCAN : [%s] [%d] NON UNIQUE ACCESS ORDER !!!! (?)',[metae.Filename,metae.AccessOrder]);
           end
         else
           GFRE_DB.LogDebug(dblc_SERVER,'>DEPLOYMENT SCAN : SKIPPING, NOT ACCESSED [%s] [%d]',[metae.Filename,metae.AccessOrder]);
       end;
  end;

  procedure DeploymentStage2(var value : NativeUint);
  var metae   : TFRE_HTTP_METAENTRY;
      content : string;
  begin
     metae := TFRE_HTTP_METAENTRY(FREDB_PtrUIntToObject(value));
     pos := FREDB_StringInArrayIdx(LowerCase(metae.FileExtension),deployexts);
     if pos=-1 then
       GFRE_BT.CriticalAbort('logic - deployment case');
     GFRE_DB.LogDebug(dblc_SERVER,'>DEPLOYMENT PHASE 2 AGGREGATING : FILE [%s] [%d]',[metae.Filename,metae.AccessOrder]);
     content := GFRE_BT.StringFromFile(metae.Filename);
     if content<>'' then
       deployccat[pos] := deployccat[pos]+LineEnding+Format('// CONCAT FILE Name : [%s] Accessorder [%d] ',[metae.Filename,metae.AccessOrder])+LineEnding
     else
       GFRE_DB.LogWarning(dblc_SERVER,'>DEPLOYMENT PHASE 2 AGGREGATING : FILE [%s] [%d] - FILE IS EMPTY',[metae.Filename,metae.AccessOrder]);
  end;

begin
  GFRE_DB.LogNotice(dblc_SERVER,'GOT AN DEPLOYMENT DUMP REQUEST');
  sorttree := TFRE_ART_TREE.Create;
  try
    FREDB_SeperateString(cFRE_DEPLOY_CONTENT_EXTS,',',deployexts);
    SetLength(ext_count,Length(deployexts));
    SetLength(deployccat,Length(deployexts));
    FStaticHTTPMeta.LinearScan(@DeploymentStage1);
    for i:=0 to high(ext_count) do
      stats := stats + deployexts[i]+' => '+inttostr(ext_count[i])+' Files ';
    GFRE_DB.LogNotice(dblc_SERVER,'CONCATENATING THE FOLLOWING TYPES [%s]',[stats]);
    sorttree.LinearScan(@DeploymentStage2,true);
    for i:=0 to high(deployccat) do
      begin
        fn := cFRE_SERVER_WWW_ROOT_DIR+DirectorySeparator+'fos_deploy'+deployexts[i];
        DeleteFile(fn);
        GFRE_BT.StringToFile(fn,deployccat[i]);
      end;
    GFRE_DB.LogNotice(dblc_SERVER,'DONE PROCESSING DEPLOYMENT FILES');
  finally
    sorttree.free;
  end;
end;

procedure TFRE_BASE_SERVER.Interrupt;
begin
  GFRE_DB.LogNotice(dblc_SERVER,'**** INTERRUPT REQUESTED (CTRL-C)');
  if G_NO_INTERRUPT_FLAG THEN exit;
  GFRE_BT.ActivateJack(30000);
  GFRE_SC.RequestTerminate;
end;

function TFRE_BASE_SERVER.GetName: String;
begin
  result := 'FRE-Server';
end;

procedure TFRE_BASE_SERVER.Finalize;
begin
  free;
end;

var lGAE : NativeUInt;

function TFRE_BASE_SERVER.FetchMetaEntry(file_path: String; var metae: TFRE_HTTP_METAENTRY): boolean;
var dummy : PtrUInt;
begin
  result :=  FStaticHTTPMeta.ExistsStringKey(file_path,dummy);
  if result then
    begin
      metae := TFRE_HTTP_METAENTRY(FREDB_PtrUIntToObject(dummy));
      if metae.AccessOrder=0 then
        metae.AccessOrder := FOS_IL_INC_NATIVE(lGAE);
    end
  else
    metae := nil;
end;



procedure TFRE_BASE_SERVER.DispatchHTTPRequest(const connection_object: TObject; const uri: string; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
begin
  FDispatcher.DispatchRequest(connection_object,uri,method);
end;

procedure TFRE_BASE_SERVER.APSC_NewListener(const LISTENER: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var lid : String;
begin
  lid :=listener.GetID;
  if lid='HTTPS/WSS' then
    begin
      if state =als_EVENT_NEW_LISTENER then
        begin
          if LISTENER.GetState=als_STOPPED then
            begin
              HTTPSWSS_Listener := listener;
              HTTPSWSS_Listener.EnableSSL(FSSL_Ctx);
              HTTPSWSS_Listener.Start;
            end
          else
            GFRE_BT.CriticalAbort('CANNOT ACTIVATE HTTPS/WSS SERVER : '+LISTENER.GetErrorString);
        end;
    end
  else
  if lid='HTTP/WS' then
    begin
      if state =als_EVENT_NEW_LISTENER then
        begin
          if LISTENER.GetState=als_STOPPED then
            begin
              HTTPWS_Listener := listener;
              HTTPWS_Listener.Start;
            end
          else
            GFRE_BT.CriticalAbort('CANNOT ACTIVATE HTTP/WS SERVER : '+LISTENER.GetErrorString);
        end;
    end
  else
  if lid='FLEX' then
    begin
      if state =als_EVENT_NEW_LISTENER then
        begin
          if LISTENER.GetState=als_STOPPED then
            begin
              FLEX_Listener := listener;
              FLEX_Listener.Start;
            end
          else
            GFRE_BT.CriticalAbort('CANNOT ACTIVATE FLEX SERVER : '+LISTENER.GetErrorString);
        end;
    end
  else
    GFRE_BT.CriticalAbort('unknown listener ?');
end;

procedure TFRE_BASE_SERVER.APSC_NewChannel(const channel: IFRE_APSC_CHANNEL; const state: TAPSC_ChannelState);
var lid : String;

  procedure _Setup_New_HTTP_WS_Channel;
  var lServerHandler : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
  begin
    lServerHandler := TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Create(channel,self);
    lServerHandler.OnBindInitialSession := @BindInitialSession;
    channel.SetVerboseDesc('HTTP ['+inttostr(channel.GetHandleKey)+']'+'('+channel.GetConnSocketAddr+')');
    channel.SetOnReadData(@lServerHandler.ReadChannelData);
    channel.SetOnDisconnnect(@lServerHandler.DisconnectChannel);
    channel.CH_Enable_Reading;
  end;

  procedure _Setup_New_HTTPS_WSS_Channel;
  var lServerHandler : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
  begin
    lServerHandler := TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Create(channel,self,true);
    lServerHandler.OnBindInitialSession := @BindInitialSession;
    channel.SetVerboseDesc('HTTPS ['+inttostr(channel.GetHandleKey)+']'+'('+channel.GetConnSocketAddr+')');
    channel.SetOnReadData(@lServerHandler.ReadChannelData);
    channel.SetOnDisconnnect(@lServerHandler.DisconnectChannel);
    channel.CH_Enable_Reading;
  end;


  procedure _Setup_New_Flex_Channel;
  var bc : TFRE_SERVED_BASE_CONNECTION;
      us : TFRE_DB_UserSession;
  begin
    bc                      :=TFRE_SERVED_BASE_CONNECTION.Create;
    bc.SetChannel(channel);
    bc.OnBindInitialSession := @BindInitialSession;
    channel.SetVerboseDesc('FLEX ['+inttostr(channel.GetHandleKey)+']'+'('+channel.GetConnSocketAddr+')');
    channel.SetOnReadData(@bc.ReadChannelData);
    channel.SetOnDisconnnect(@bc.DisconnectChannel);
    channel.CH_Enable_Reading;
  end;

begin
  lid := channel.GetListener.GetID;
  if lid ='HTTP/WS' then
    _Setup_New_HTTP_WS_Channel
  else
  if lid ='HTTPS/WSS' then
    _Setup_New_HTTPS_WSS_Channel
  else
  if lid = 'FLEX' then
    _Setup_New_Flex_Channel
  else
    GFRE_BT.CriticalAbort('unexpected listener id in new channel?');
end;

//procedure TFRE_BASE_SERVER.WFE_DispatchTimerEvent(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
//var FSession_dispatch_array   : Array of TFRE_DB_UserSession;
//    i                         : NativeInt;
//    lSession                  : TFRE_DB_UserSession;
//begin
//  FSessionTreeLock.Acquire;
//  try
//    //if FUserSessionsTree.QueryTreeChange then begin
//      FSession_dispatch_array := FUserSessionsTree.GetAllItemsAsArray;
//    //end;
//    for i := 0 to high(FSession_dispatch_array) do
//      begin
//        if FSession_dispatch_array[i].CheckUnboundSessionForPurge then
//          begin
//            if FUserSessionsTree.Delete(FSession_dispatch_array[i].GetSessionID,lSession) then
//              begin
//                writeln('FREEING USER SESSION ',lSession.GetSessionID,' user=',lSession.GetUsername,' from ',lSession.GetClientDetails);
//                try
//                  lSession.free;
//                except on e:exception do
//                  writeln('SESSION FREE FAILED : ',e.Message);
//                end;
//                FSession_dispatch_array[i]:=nil;
//              end
//            else
//              begin
//                writeln('--CRITICAL ?? - cannot delete unbound session ? ',FSession_dispatch_array[i].GetSessionID);
//              end;
//          end;
//      end;
//  finally
//    FSessionTreeLock.Release;
//  end;
//end;



function TFRE_BASE_SERVER.GetImpersonatedDatabaseConnectionSession(const dbname, username, pass, default_app: string; const default_uid_path: TFRE_DB_GUIDArray; const session_net_desc: String; out dbs: TFRE_DB_UserSession): TFRE_DB_Errortype;
var found : boolean;
    res   : TFRE_DB_Errortype;
    dbc   : IFRE_DB_CONNECTION;
begin
  result:= GetImpersonatedDatabaseConnection(dbname,username,pass,dbc);
  if result=edb_OK then begin
    dbs := TFRE_DB_UserSession.Create(username,'',default_app,default_uid_path,dbc); { default logins, guest will not bind the session to the DBC, so no updates for this sessions by now}
    dbs.SetClientDetails(session_net_desc);
  end;
end;

function TFRE_BASE_SERVER.ExistsUserSessionForUserLocked(const username: string; out other_session: TFRE_DB_UserSession): boolean;
var fsession : TFRE_DB_UserSession;

  function SearchUser(const session:TFRE_DB_UserSession):boolean;
  begin
    if uppercase(session.GetUsername)=uppercase(username) then
      begin
        fsession := session;
        result   := true;
      end
    else
      result := false;
  end;

begin
  result        := false;
  fsession      := nil;
  other_session := nil;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItemsBrk(@SearchUser);
    if assigned(fsession) then begin
      other_session     := fsession;
      other_session.LockSession;
      result            := true;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;

function TFRE_BASE_SERVER.ExistsUserSessionForKeyLocked(const key: string; out other_session: TFRE_DB_UserSession): boolean;
var fsession : TFRE_DB_UserSession;

  function SearchKey(const session:TFRE_DB_UserSession):boolean;
  begin
    if session.GetTakeOverKey=key then
      begin
        fsession := session;
        result   := true;
      end
    else
      result := false;
  end;

begin
  result        := false;
  fsession      := nil;
  other_session := nil;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItemsBrk(@SearchKey);
    if assigned(fsession) then begin
      other_session     := fsession;
      other_session.LockSession;
      result            := true;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;

function TFRE_BASE_SERVER.CheckUserNamePW(username, pass: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FSystemConnection.CheckLogin(username,pass);
end;

function TFRE_BASE_SERVER.FetchPublisherSessionLocked(const rcall, rmeth: TFRE_DB_NameType; out ses: TFRE_DB_UserSession ; out right:TFRE_DB_String): boolean;

  function SearchRAC(const session:TFRE_DB_UserSession):boolean;
  var arr : TFRE_DB_RemoteReqSpecArray;
      i   : NAtiveint;
  begin
    arr := session.GetPublishedRemoteMeths;
    for i:=0 to high(arr) do
      begin
        if (arr[i].classname=rcall)
           and (arr[i].methodname=rmeth) then
             begin
               right := arr[i].invokationright;
               ses   := session;
               result := true;
               exit;
             end;
      end;
    result := false;
  end;

begin
  result   := false;
  ses      := nil;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItemsBrk(@SearchRac);
    if assigned(ses) then begin
      ses.LockSession;
      result := true;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;

function TFRE_BASE_SERVER.FetchSessionByIdLocked(const sesid: TFRE_DB_String; var ses: TFRE_DB_UserSession): boolean;

  function SearchSession(const session:TFRE_DB_UserSession):boolean;
  var arr : TFRE_DB_RemoteReqSpecArray;
      i   : NAtiveint;
  begin
    if session.GetSessionID = sesid then
      begin
        result := true;
        ses := session;
      end
    else
      result := false;
  end;

begin
  result   := false;
  ses      := nil;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItemsBrk(@SearchSession);
    if assigned(ses) then begin
      ses.LockSession;
      result := true;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;

var test_ses_deb     : integer=0;

procedure TFRE_BASE_SERVER.TIM_SessionHandler(const timer: IFRE_APSC_TIMER; const flaf1, flag2: boolean);
var myhalt       : boolean;
    purgesession : TFRE_DB_UserSession;
    ses          : TFRE_DB_UserSession;

  procedure IterateSession(const session : TFRE_DB_UserSession ; var halt:boolean);
  begin
    //writeln('  SESSION: ',session.GetSessionID,' ',session.GetClientDetails);
    if session.CheckUnboundSessionForPurge then
      begin
        halt := true;
        purgesession := session;
      end;
  end;

  procedure TestSessionCreationFree;
  var
    testses  : TFRE_DB_UserSession;
    err      : TFRE_DB_String;
    ses      : TFRE_DB_UserSession;
    x        : TFRE_DB_CONTENT_DESC;
    dbo,dbo2 : IFRE_DB_Object;
   lServerHandler : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
  begin
    inc(test_ses_deb);
    if test_ses_deb=1 then
      begin
        //lServerHandler := TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Create(nil,self);
        //testses := FDefaultSession.CloneSession('WURZ');
        //writeln(testses.Promote('admin@system','admin',err,false,false,x,ses,false));
        //lServerHandler.Free;
        //testses.Free;
        //dbo := TFRE_DB_LOGIN.CreateForDB;
        //dbo2 := dbo.CloneToNewObject();
        //dbo2.Finalize;
        //dbo.Finalize;
      end;
  end;

begin
  //writeln('SESSIONS - TIMER ',test_ses_deb);
  //TestSessionCreationFree;

  myhalt := false;

  TFRE_DB_UserSession.HandleContinuationTimeouts(@FetchSessionByIdLocked);

  myhalt := false;
  ForAllSessionsLocked(@IterateSession,myhalt);
  if myhalt then
    begin
      try
        GFRE_DBI.LogInfo(dblc_SESSION,'SUSPENDING SESSION : [%s]',[purgesession.GetSessionID+'/'+purgesession.GetUsername]);
        FUserSessionsTree.Delete(purgesession.GetSessionID,ses);
        purgesession.Free;
      finally
        FSessionTreeLock.Release;
      end;
    end;
end;

procedure TFRE_BASE_SERVER.TIM_TaskerHandler(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
begin
  FTaskerSession.LockSession;
  try
    if (flag1 or flag2) then
      FTaskerApp.TASKER_REQUEST(FTaskerSession,flag1,flag2)
    else
      FTaskerApp.TASKER_METHOD(FTaskerSession);
  finally
    FTaskerSession.UnlockSession;
  end;
end;

procedure TFRE_BASE_SERVER.ForAllSessionsLocked(const iterator : TFRE_DB_SessionIterator ; var halt : boolean);

  function Iterate(const ses : TFRE_DB_UserSession):boolean;
  begin
    result := false;
    ses.LockSession;
    try
      iterator(ses,halt);
    finally
      if halt<>true then
        ses.UnlockSession;
    end;
    if halt then
      result := true;
  end;

begin
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItemsBrk(@Iterate);
  finally
    if halt<>true then
      FSessionTreeLock.Release;
  end;
end;

function TFRE_BASE_SERVER.SendDelegatedContentToClient(sessionID: TFRE_DB_String; const content: TFRE_DB_CONTENT_DESC): boolean;
var session : TFRE_DB_UserSession;
begin
  result := GFRE_DBI.NetServ.FetchSessionByIdLocked(sessionID,session);
  if result then
    begin
      try
        result := session.DispatchCoroutine(@session.COR_SendContentOnBehalf,content);
      finally
        session.UnlockSession;
      end;
    end
  else
    try
      content.Finalize;
    except
      on E:Exception do
        raise EFRE_DB_Exception.Create(edb_ERROR,'content free for send delegated, seesion not found, failed [%s]',[e.Message]);
    end;
end;

function TFRE_BASE_SERVER.FetchStreamDBO(const enc_sessionid, enc_uid: string; var end_field: TFRE_DB_NameTypeRL; var lcontent: TFRE_DB_RawByteString ; var stored_ct : TFRE_DB_String; var stored_etag : TFRE_DB_String): boolean;
var fetch_uid : TGuid;
    ses       : TFRE_DB_UserSession;
begin
  try
    fetch_uid := GFRE_BT.HexString_2_GUID(enc_uid);
  except
    exit(false);
  end;
  result := FetchSessionByIdLocked(enc_sessionid,ses);
  if assigned(ses) then
    try
      result := ses.FetchStreamDBO_OTCU(fetch_uid,end_field,lcontent,stored_ct,stored_etag);
    finally
      ses.UnlockSession;
    end;
end;

function TFRE_BASE_SERVER.GetETag(const filename: string; const filesize: NativeUint; const moddate: TFRE_DB_DateTime64): String;
begin
  result := GFRE_BT.HashString_MD5_HEX(filename+':'+inttostr(filesize)+':'+inttostr(moddate));
end;

constructor TFRE_BASE_SERVER.Create(const defaultdbname: string);
begin
  DefaultDatabase   := defaultdbname;
  FStaticHTTPMeta   := TFRE_ART_TREE.Create;
  GFRE_DB.NetServer := self;
end;

function  TFRE_BASE_SERVER.BindInitialSession(const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; out  session: TFRE_DB_UserSession; const old_session_id: string;  const interactive_session: boolean):boolean;
var ws         : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
    SessionKey : String;
    found      : boolean;
    reuse_ses  : boolean;
    res        : TFRE_DB_Errortype;
begin
  session   := nil;
  found     := false;
  reuse_ses := (old_session_id<>'NEW') and (old_session_id<>'');
  GFRE_DBI.LogInfo(dblc_SESSION,'BindInitialSession ['+old_session_id+'] ');
  if reuse_ses then
    begin
      if FetchSessionByIdLocked(old_session_id,session) then
        begin
          GFRE_DBI.LogDebug(dblc_SESSION,'REUSING SESSION [%s]',[old_session_id]);
          found:=true;
          session.SetSessionState(sta_REUSED);
          session.SetServerClientInterface(back_channel,interactive_session);
          session.UnlockSession;
          GFRE_DBI.LogDebug(dblc_SESSION,'REUSING SESSION SET [%s]',[old_session_id]);
          exit(true);
        end
      else
        begin
          GFRE_DBI.LogDebug(dblc_SESSION,'OLD REQUESTED SESSION NOT FOUND [%s]',[old_session_id]);
          exit(false);
        end;
    end
  else
    begin
      res := GetImpersonatedDatabaseConnectionSession(DefaultDatabase,'GUEST'+'@'+CFRE_DB_SYS_DOMAIN_NAME,'',FDefaultAPP_Class,TFRE_DB_GUIDArray.Create(FDefaultAPP_UID),back_channel.GetInfoForSessionDesc,session);
      CheckDbResult(res,'COULD NOT CONNECT DEFAULT DB / APP / WITH GUEST ACCESS');

      session.SetSessionState(sta_ActiveNew);
      FSessionTreeLock.Acquire;
      try
        FUserSessionsTree.Add(session.GetSessionID,session);
        session.SetServerClientInterface(back_channel,interactive_session);
      finally
        FSessionTreeLock.Release;
      end;
      GFRE_DBI.LogDebug(dblc_SESSION,'STARTING NEW SESSION [%s]',[session.GetSessionID]);
      exit(true);
    end;
end;


function TFRE_BASE_SERVER.GetImpersonatedDatabaseConnection(const dbname, username, pass: TFRE_DB_String; out dbs: IFRE_DB_CONNECTION): TFRE_DB_Errortype;
var found : boolean;
    res   : TFRE_DB_Errortype;

  function FindDb(const dbconnection:TFRE_DB_CONNECTION):boolean;
  var l_dbc : TFRE_DB_CONNECTION;
  begin
    result := false;
    if uppercase(dbconnection.ConnectedName) = uppercase(dbname) then begin
      res    :=dbconnection.ImpersonateClone(username,pass,l_dbc);
      dbs    := l_dbc;
      result := true;
    end;
  end;

begin
  dbs    := nil;
  found  := FOpenDatabaseList.ForAllBrk(@FindDb);
  if found then begin
    result := res;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;

function TFRE_BASE_SERVER.GetDBWithServerRights(const dbname: TFRE_DB_String; out dbs: IFRE_DB_CONNECTION): TFRE_DB_Errortype;
var found : boolean;
    res   : TFRE_DB_Errortype;

  function FindDb(const dbconnection:TFRE_DB_CONNECTION):boolean;
  begin
    result := false;
    if uppercase(dbconnection.ConnectedName) = uppercase(dbname) then begin
      dbs    := dbconnection;
      result := true;
    end;
  end;

begin
  dbs    := nil;
  found  := FOpenDatabaseList.ForAllBrk(@FindDb);
  if found then begin
    result := edb_OK;
  end else begin
    result := edb_NOT_FOUND;
  end;
end;


procedure RegisterLogin;
begin
  GFRE_DB.RegisterObjectClassEx(TFRE_DB_LOGIN);
  GFRE_DB.RegisterObjectClassEx(TFRE_DB_TASKER);
  GFRE_DBI.Initialize_Extension_Objects;
end;


end.

