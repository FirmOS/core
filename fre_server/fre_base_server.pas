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

//TODO : EVENT - QUEUE DISPATCHING INTERINKTIMER ??? BAD DESIGN

interface

uses  classes, sysutils,fre_aps_interface,fos_fcom_interfaces,fos_fcom_types,fos_tool_interfaces,
      fre_http_srvhandler,fre_db_interface,fre_system,fos_interlocked,
      fre_db_core,fre_webssocket_baseserver,fre_db_common,FRE_DB_SYSRIGHT_CONSTANTS,
      fre_http_tools,fos_redblacktree_gen,fre_sys_base_cs,
      fre_wapp_dojo;

var cAdminUser:string='admin'+'@'+cSYS_DOMAIN;
    cAdminPass:string='admin';
    cVersion  :string='0.1 alpha';

procedure RegisterLogin;

type
  { TFRE_BASE_SERVER }

  TFRE_DB_LOG_TYPE=(log_Warning,log_Error,log_INFO);

  TFRE_UserSession_Tree         = specialize TGFOS_RBTree<string,TFRE_DB_UserSession>;

  TFRE_BASE_SERVER=class(TObject,IFRE_APS_PROCESS,IFRE_HTTP_BASESERVER)
  private
    FOpenDatabaseList  : OFRE_DB_ConnectionArr;
    FUserSessionsTree  : TFRE_UserSession_Tree;
    FDispatcher        : TFRE_HTTP_URL_DISPATCHER;
    FSessionTreeLock   : IFOS_LOCK;

    FMimeList          : TFOS_RB_Tree_SS;
    cont               : boolean;
    flistener_es       : IFRE_APS_SOCKET_EVENTSOURCE;
    flistener_es_ws    : IFRE_APS_SOCKET_EVENTSOURCE;

    FIL_ListenSock     : IFRE_APS_SOCKET_EVENTSOURCE;
    FIL_ListenSockSSL  : IFRE_APS_SOCKET_EVENTSOURCE;
    Foutput            : String;
    FLoginApp          : TFRE_DB_APPLICATION;
    FSystemConnection  : TFRE_DB_SYSTEM_CONNECTION;

    FInterLinkEvent    : IFRE_APS_TIMER;

    FWFE_Scheduler     : IFRE_APS_TIMER;
    FWFE_InTimer       : NativeInt;

    FDefaultSession           : TFRE_DB_UserSession;
    FSession_dispatch_array   : Array of TFRE_DB_UserSession;
    FHull_HTML,FHull_CT       : String;
    FTerminating              : boolean;

    procedure      _CloseAll              ;
    procedure      _SetupHttpBaseServer   ;
    procedure      InterLinkDispatchTimer (const ES : IFRE_APS_EVENTSOURCE ; const TID:integer;const Data:Pointer;const cp:integer=0);
    procedure      WFE_DispatchTimerEvent (const ES : IFRE_APS_EVENTSOURCE ; const TID:integer;const Data:Pointer;const cp:integer=0);
    procedure      NewConnection          (const session_id:string);
    procedure      ConnectionDropped      (const session_id:string);
    procedure      WorkCommandsCallback   (sender:Tobject);
    procedure      BindInitialSession     (const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC ; out   session : TFRE_DB_UserSession;const old_session_id:string;const interactive_session:boolean);
    procedure      UnBindSession          (const sender:TObject ; const session : TFRE_DB_UserSession);
    function       GetImpersonatedDatabaseConnection         (const dbname,username,pass:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       GetDBWithServerRights                     (const dbname:TFRE_DB_String ; out dbs:IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       RestoreDefaultConnection                  (out  username : TFRE_DB_String ; out conn : IFRE_DB_CONNECTION):TFRE_DB_Errortype;
    function       GetImpersonatedDatabaseConnectionSession  (const dbname,username,pass,default_app : string;const default_uid_path : TFRE_DB_GUIDArray ; out dbs:TFRE_DB_UserSession):TFRE_DB_Errortype;
    function       ExistsUserSessionForUser                  (const username:string;out other_session:TFRE_DB_UserSession):boolean;
    function       CheckUserNamePW                           (username,pass:TFRE_DB_String) : TFRE_DB_Errortype;

  public
    DefaultDatabase              : String;
    TransFormFunc                : TFRE_DB_TRANSFORM_FUNCTION;

    constructor create           (const defaultdbname : string);
    destructor Destroy           ; override;
    procedure  Setup             ;
    procedure  Terminate         ;
    procedure  ReInit            ;
    procedure  Interrupt         ;
    function   GetName           : String;


    function  ServerHandlerWS       (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
    procedure InitServerSockWS      (const SOCK:IFCOM_SOCK);
    procedure TearDownServerSockWS  (const Sock:IFCOM_SOCK);
    procedure ListenerErrorWS       (const listener_sock:IFCOM_SOCK;const Error:EFOS_OS_ERROR); // Listener had an error and will be shutdown

    function  ServerHandlerFC       (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
    procedure InitServerSockFC      (const SOCK:IFCOM_SOCK);
    procedure TearDownServerSockFC  (const Sock:IFCOM_SOCK);
    procedure ListenerErrorFC       (const listener_sock:IFCOM_SOCK;const Error:EFOS_OS_ERROR); // Listener had an error and will be shutdown

    procedure Finalize              ;
    function  FetchFileCached       (file_path:String;var data:TFRE_DB_RawByteString):boolean;
    procedure FetchHullHTML         (var lContent:TFRE_DB_RawByteString;var lContentType:string);
    function  LookupMimeType        (const extension:string):string;
    procedure DispatchHTTPRequest   (const connection_object:TObject;const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
  end;


implementation

uses FRE_DB_LOGIN;
{ TFRE_BASE_SERVER }



procedure TFRE_BASE_SERVER._CloseAll;

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

begin
  GFRE_DBI.LogDebug(dblc_SERVER,'ENTER CLOSING SEQUENCE');
  FTerminating := true;
  FSessionTreeLock.Acquire;
  try
    FUserSessionsTree.ForAllItems(@StoreSessionData);
  finally
    FSessionTreeLock.Release;
  end;
  GFRE_DBI.LogDebug(dblc_SERVER,'DATABASE COUNT [%d]',[FOpenDatabaseList.Count]);
  FOpenDatabaseList.ForAllBrk(@CloseAll);
  FSystemConnection.Free;
  FSystemConnection:=nil;
  GFRE_DBI.LogDebug(dblc_SERVER,'SERVER SHUTDOWN DONE');
  writeln('Syncdb');
  GFRE_DB_DEFAULT_PS_LAYER.SyncSnapshot(true);
  writeln('Syncdb-done');
end;

procedure TFRE_BASE_SERVER._SetupHttpBaseServer;
var dummy:TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
begin
  FMimeList    := TFOS_RB_Tree_SS.Create(@Default_RB_String_Compare);
  FDispatcher  := TFRE_HTTP_URL_DISPATCHER.Create;
  FMimeList.add('.js','application/javascript');
  FMimeList.add('.html','text/html');
  FMimeList.add('.css','text/css');
  FMimeList.add('.gif','image/gif');
  FMimeList.add('.jpg','image/jpeg');
  FMimeList.add('.png','image/png');
  FMimeList.add('.tiff','image/tiff');
  FMimeList.add('.txt','text/plain');
  FMimeList.add('.svg','image/svg+xml');
  FMimeList.add('.swf','application/x-shockwave-flash');
  FMimeList.add('.woff','application/font-woff');
  FMimeList.add('.ttf','application/octet-stream');
  FMimeList.add('.otf','font/opentype');
  FMimeList.add('.eot','application/vnd.ms-fontobject');

//  FDispatcher.RegisterVirtualProvider('','rest/fre/',@dummy.FREDB_Provider);
  FDispatcher.RegisterDefaultProvider('',@dummy.Default_Provider);
  GFRE_tF.Get_Lock(G_SerializeLock);
end;

destructor TFRE_BASE_SERVER.Destroy;

  //procedure _CleanQ;
  //var sendback:TFRE_DB_SEND_BACK_DELAYED_ENCAPSULATION;
  //begin
  //  repeat
  //    sendback := TFRE_DB_SEND_BACK_DELAYED_ENCAPSULATION(FInterLinkQ.Pop);
  //  until sendback=nil;
  //end;

begin
  //if assigned(FInterLinkQ) then begin
  //  _CleanQ;
  //  FInterLinkQ := nil;
  //end;
  inherited Destroy;
end;

procedure TFRE_BASE_SERVER.Setup;
var  me           : EFOS_FCOM_MULTIERROR;
     lsock        : IFCOM_SOCK;
     result       : TFRE_DB_Errortype;
     WWWDBOx      : TFRE_DB_Object;
     shandler     : IR_FRE_APS_FCOM_SERVER_HANDLER;
     flex_handler : IR_FRE_APS_FCOM_SERVER_HANDLER;
     ssldir       : string;

     procedure _ConnectAllDatabases;
     var i       : Integer;
         dblist  : IFOS_STRINGS;
         ndbc    : TFRE_DB_CONNECTION;
         dbname  : string;
         res     : TFRE_DB_Errortype;
         log_txt : string;
         app     : IFRE_DB_APPLICATION;
     begin
       dblist := GFRE_DB_DEFAULT_PS_LAYER.DatabaseList;
       GFRE_DB.LogInfo(dblc_SERVER,'START SERVING DATABASES [%s]',[dblist.Commatext]);
       FSystemConnection := GFRE_DB.NewDirectSysConnection;
       res := FSystemConnection.Connect(cAdminUser,cAdminPass);  // direct admin connect
       if res<>edb_OK then begin
         FSystemConnection.Free;
         FSystemConnection := nil;
         GFRE_DB.LogError(dblc_SERVER,'SERVING SYSTEM DATABASE failed due to [%s]',[CFRE_DB_Errortype[res]]);
         GFRE_BT.CriticalAbort('CANNOT SERVE SYSTEM DB [%s]',[CFRE_DB_Errortype[res]]);
       end;
       if not FSystemConnection.ApplicationByName('LOGIN',app) then GFRE_BT.CriticalAbort('cannot fetch login app');
       FLoginApp := App.Implementor_HC as TFRE_DB_LOGIN;
       if not assigned(FLoginApp) then begin
         GFRE_BT.CriticalAbort('could not preload login app / not found');
       end;
       GFRE_DB.LogInfo(dblc_SERVER,'>LOGIN APP DONE',[]);
       for i := 0 to dblist.Count-1 do begin
         dbname := Uppercase(dblist[i]);
         if dbname='SYSTEM' then begin
           ;
         end else begin
           //InfoLog(5,'CONNECTING [%s]',[dbname]);
           ndbc := GFRE_DB.NewConnection(true);
           res  := ndbc.Connect(dbname,cAdminUser,cAdminPass,FSystemConnection); // direct admin connect
           if res<>edb_OK then begin
             GFRE_DB.LogError(dblc_EXCEPTION,'SERVING DATABASE [%s] failed due to [%s]',[dbname,CFRE_DB_Errortype[res]]);
           end else begin
             //InfoLog(5,'CONNECTED [%s]',[dbname]);
             FOpenDatabaseList.Add2Array(ndbc);
           end;
         end;
       end;
       res := GetImpersonatedDatabaseConnectionSession(DefaultDatabase,'GUEST'+'@'+cSYS_DOMAIN,'','TFRE_DB_LOGIN',GFRE_DB.ConstructGuidArray([FLoginApp.UID]),FDefaultSession);
       CheckDbResult(res,'COULD NOT CONNECT DEFAULT DB / APP / WITH GUEST ACCESS');
     end;

     procedure _ServerInitializeApps;
     var apps : IFRE_DB_APPLICATION_ARRAY;
            i : Integer;
          dbs : IFRE_DB_CONNECTION;
     begin
       GFRE_DBI.FetchApplications(apps);
       for i:=0 to high(apps) do begin
         if apps[i].ObjectName<>'LOGIN' then begin
           if GetDBWithServerRights(DefaultDatabase,dbs)=edb_OK then begin // May be an array with db per app
             (apps[i].Implementor_HC as TFRE_DB_APPLICATION).ServerInitialize(dbs);
           end else begin
             GFRE_BT.CriticalAbort('CANNOT SERVERINITIALIZE APPLICATION [APP:%s DB: %s]',[apps[i].ObjectName,DefaultDatabase]);
           end;
         end;
       end;
     end;

     function MyGetPW:string;
     begin
       result := '0000';
     end;

     procedure InitHullHTML;
     var res_main : TFRE_DB_MAIN_DESC;
     begin
       res_main  := TFRE_DB_MAIN_DESC.create.Describe(cFRE_WEB_STYLE,'https://tracker.firmos.at/s/en_UK-wu9k4g-1988229788/6097/12/1.4.0-m2/_/download/batch/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs/com.atlassian.jira.collector.plugin.jira-issue-collector-plugin:issuecollector-embededjs.js?collectorId=5e38a693');
       TransFormFunc(FDefaultSession,fct_SyncReply,res_main,FHull_HTML,FHull_CT,false,fdbtt_get2html);
     end;

begin
  TransFormFunc     := @FRE_WAPP_DOJO.TransformInvocation;
  FOpenDatabaseList.init;

  GFRE_DB.LogNotice(dblc_SERVER,'FirmOS System Base Server Node Startup Vesion (%s)',[cVersion]);

  FUserSessionsTree  := TFRE_UserSession_Tree.Create(@Default_RB_String_Compare);
  GFRE_TF.Get_Lock(FSessionTreeLock);

  shandler.ListenerError      := @ListenerErrorWS;
  shandler.ServerHandler      := @ServerHandlerWS;
  shandler.InitServerSock     := @InitServerSockWS;
  shandler.TearDownServerSock := @TearDownServerSockWS;

  me:=GFRE_S.AddSocketListener('*',44000,fil_IPV4,fsp_TCP,shandler,true,flistener_es);
  if me<>ese_OK then begin
    GFRE_BT.CriticalAbort('Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
    exit;
  end;
  ssldir := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/server_files/');
  GFRE_DB.LogInfo(dblc_SERVER,'HTTP (MAINTENANCE/SERVICE) SERVER SSL LISTENING ON (%s) ',[flistener_es.GetSocket.Get_AI.SocketAsString]);
  me:=GFRE_S.AddSocketListener_SSL('*',44443,fil_IPV4,fsp_TCP,shandler,true,flistener_es,fssl_TLSv1,ssldir+cFRE_SSL_CERT_FILE,ssldir+cFRE_SSL_PRIVATE_KEY_FILE,ssldir+cFRE_SSL_ROOT_CA_FILE,@MyGetPW,false,false,false,'DEFAULT');
  if me<>ese_OK then begin
    GFRE_BT.CriticalAbort('Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
    exit;
  end;
  GFRE_DB.LogInfo(dblc_SERVER,'HTTP (MAINTENANCE/SERVICE) SERVER LISTENING ON (%s) ',[flistener_es.GetSocket.Get_AI.SocketAsString]);

  flex_handler.ListenerError      := @ListenerErrorFC;
  flex_handler.ServerHandler      := @ServerHandlerFC;
  flex_handler.InitServerSock     := @InitServerSockFC;
  flex_handler.TearDownServerSock := @TearDownServerSockFC;

  me:=GFRE_S.AddSocketListener('*',44001,fil_IPV4,fsp_TCP,flex_handler,true,FIL_ListenSock);
  if me<>ese_OK then begin
    GFRE_BT.CriticalAbort('EVENT SERVER Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
  end;
  GFRE_DB.LogInfo(dblc_SERVER,'NODE INTERLINK (SERVER) LISTENING ON (%s) ',[FIL_ListenSock.GetSocket.Get_AI.SocketAsString]);
  me:=GFRE_S.AddSocketListener_SSL('*',44002,fil_IPV4,fsp_TCP,flex_handler,true,FIL_ListenSockSSL,fssl_TLSv1,ssldir+cFRE_SSL_CERT_FILE,ssldir+cFRE_SSL_PRIVATE_KEY_FILE,ssldir+cFRE_SSL_ROOT_CA_FILE,@MyGetPW,false,false,false,'DEFAULT');
  if me<>ese_OK then begin
    GFRE_BT.CriticalAbort('EVENT SERVER Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
  end;
  GFRE_DB.LogInfo(dblc_SERVER,'NODE INTERLINK (SERVER/SSL) LISTENING ON (%s) ',[FIL_ListenSockSSL.GetSocket.Get_AI.SocketAsString]);

  FInterLinkEvent := GFRE_S.AddPeriodicSignalTimer(1,@InterLinkDispatchTimer,nil,dm_OneWorker);
  FWFE_Scheduler  := GFRE_S.AddPeriodicTimer(1000,@WFE_DispatchTimerEvent,nil,dm_OneWorker);

  _SetupHttpBaseServer;
  _ConnectAllDatabases;
  _ServerinitializeApps;

  InitHullHTML;

  writeln('SERVER INITIALIZED, running');
end;


procedure TFRE_BASE_SERVER.Terminate;
begin
  GFRE_DB.LogNotice(dblc_SERVER,'TERMINATE SIGNAL RECEIVED',[]);
  _CloseAll;
  GFRE_S.Quit;
end;

procedure TFRE_BASE_SERVER.ReInit;
begin
  GFRE_DB_DEFAULT_PS_LAYER.SyncSnapshot(false);
end;

procedure TFRE_BASE_SERVER.Interrupt;
begin
  if G_NO_INTERRUPT_FLAG THEN exit;
  writeln('INTERRUPT');
  _CloseAll;
  GFRE_S.Quit;
end;

function TFRE_BASE_SERVER.GetName: String;
begin
  result := 'FRE-Server';
end;

function TFRE_BASE_SERVER.ServerHandlerWS(const Event: EFOS_FCOM_MULTIEVENT; const SOCK: IFCOM_SOCK; const Datacount: Integer): boolean;
var s              : String;
    sr             : integer;
    sw             : integer;
    ose            : EFOS_OS_ERROR;
    ssl_pend       : integer;
    lServerHandler : TFRE_HTTP_CONNECTION_HANDLER;
    want           : TFRE_FCOM_SSL_WANTS;
    amount         : integer;
    mydatacount    : integer;
begin
  result := true;
  try
    sock.SSL_Wants(want,amount);
    //writeln('*** SOCK EVENT ',event,' ', want,' ',amount);
    case event of
      esv_SOCKERROR: begin
        GFRE_DB.LogError(dblc_SERVER,'SOCKET ERROR  [%s]',[SOCK.GetVerboseDesc]);
      end;
      esv_SOCKCONNECTED: begin
        GFRE_DB.LogDebug(dblc_SERVER,'CONNECTION STARTED [%s]',[SOCK.GetVerboseDesc]);
      end;
      esv_SOCKREAD: begin
        ose := sock.ReceiveString(s,Datacount,sr);
        case ose of
          EFOS_OS_OK: begin
            if sr>0 then begin
              lServerHandler := TFRE_HTTP_CONNECTION_HANDLER(sock.Data);
              lServerHandler.ReadSocketData(sock,s);
            end;
          end;
          EFOS_CONNECTION_CLOSED : begin
            sock.CloseEnqueue(99);
            result:=false;
          end;
          EFOS_SSL_WANT_READ: begin
            writeln('****************************** SSL READ MORE ',sock.GetHandleKey);
            result := true; // read more
          end;
          EFOS_OS_SSL_ERROR: begin
            //writeln('-esv_SOCK_READ EVENT SSL ERROR ',Datacount,' ', sr,' ',sock.Get_SSL_ErrorString);
            sock.CloseEnqueue(99);
          end;
          else begin
            writeln('esv_SOCKREAD SOCK RECEIVESTRING ERR ',ose,' ',s);
            abort;
          end;
        end;
      end;
      esv_SOCKWRITE: begin
        writeln('WRITE REQUESTED');
        abort;
      end;
      esv_SOCKCLOSED: begin
        GFRE_DB.LogDebug(dblc_SERVER,'CONNECTION CLOSED [%s]',[SOCK.GetVerboseDesc]);
        //lServerHandler := TFRE_HTTP_CONNECTION_HANDLER(sock.Data);
        //lServerHandler.ReadSocketData(sock,s);
        result:=false;
      end;
      esv_SOCKEXCEPT,
      esv_SOCKCANTCONNECT,
      esv_SOCKCONNREFUSED,
      esv_SOCKCONNTIMEDOUT: begin
        writeln('ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
      end;
      else GFRE_BT.CriticalAbort('UNKNOWN STATE IN SERVER HANDLER');
    end;
  except on e:exception do begin
    writeln('SOCKET/COMMON READ:',e.Message);
    SOCK.CloseEnqueue(2);
    result:=false;
  end;end;
end;


procedure TFRE_BASE_SERVER.InitServerSockWS(const SOCK: IFCOM_SOCK);
var lServerHandler : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
begin
  lServerHandler                      := TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Create(sock,self);
  lServerHandler.OnBindInitialSession := @BindInitialSession;
  lServerHandler.OnUnbindSession      := @UnbindSession;
  sock.SetNoDelay(true);
  SOCK.Data        := lServerHandler;
end;


procedure TFRE_BASE_SERVER.TearDownServerSockWS(const Sock: IFCOM_SOCK);
var lServerHandler : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
begin
  writeln('TEAR DOWN WEBSOCKET/HTTP SERVER HANDLER');
  lServerHandler   := TObject(SOCK.Data) as TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
  lServerHandler.Free;
end;


procedure TFRE_BASE_SERVER.ListenerErrorWS(const listener_sock: IFCOM_SOCK; const Error: EFOS_OS_ERROR);
begin
  writeln('LISTENER ERROR < '+listener_sock.Get_AI.SocketAsString+'>');
end;

function TFRE_BASE_SERVER.ServerHandlerFC(const Event: EFOS_FCOM_MULTIEVENT; const SOCK: IFCOM_SOCK; const Datacount: Integer): boolean;
begin
  result := TFRE_SERVED_BASE_CONNECTION(Sock.Data).Handler(Event,Datacount);
end;

procedure TFRE_BASE_SERVER.InitServerSockFC(const SOCK: IFCOM_SOCK);
var bc : TFRE_SERVED_BASE_CONNECTION;
    us : TFRE_DB_UserSession;
begin
  bc                      :=TFRE_SERVED_BASE_CONNECTION.Create;
  bc.OnBindInitialSession := @BindInitialSession;
  bc.OnUnbindSession      := @UnbindSession;
  SOCK.Data               :=bc;
  bc.SOCK                 :=SOCK;
  SOCK.SetNoDelay(True);
end;

procedure TFRE_BASE_SERVER.TearDownServerSockFC(const Sock: IFCOM_SOCK);
begin
  writeln('TEAR DOWN ',TFRE_SERVED_BASE_CONNECTION(Sock.Data).SessionID);
  TFRE_SERVED_BASE_CONNECTION(sock.data).Free;
end;

procedure TFRE_BASE_SERVER.ListenerErrorFC(const listener_sock: IFCOM_SOCK; const Error: EFOS_OS_ERROR);
begin
  writeln('LISTENER ERROR FC < '+listener_sock.Get_AI.SocketAsString+'>');
end;

procedure TFRE_BASE_SERVER.Finalize;
begin
  free;
end;

function TFRE_BASE_SERVER.FetchFileCached(file_path: String; var data: TFRE_DB_RawByteString): boolean;
var fn : String;
    fh : THandle;
    fs : int64;
    fs2: int64;
begin
  fn := cFRE_SERVER_WWW_ROOT_DIR+DirectorySeparator+file_path;
  if FileExists(fn) then begin
    fh := FileOpen(fn,fmOpenRead+fmShareDenyNone);
    fs := FileSeek(fh,0,fsFromEnd);
    SetLength(data,fs);
    FileSeek(fh,0,fsFromBeginning);
    fs2 := FileRead(fh,data[1],fs);
    if fs2<>fs then abort;
    FileClose(fh);
    result := true;
  end else begin
    result := false;
  end;
end;

procedure TFRE_BASE_SERVER.FetchHullHTML(var lContent: TFRE_DB_RawByteString; var lContentType: string);
begin
  lContent     := FHull_HTML;
  lContentType := FHull_CT;
end;


function TFRE_BASE_SERVER.LookupMimeType(const extension: string): string;
begin
  if not FMimeList.Find(extension,Result) then begin
    result:='';
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'UNKNOWN/UNREGISTERED MIMETYPE [%s]',[extension]);
  end;
end;

procedure TFRE_BASE_SERVER.DispatchHTTPRequest(const connection_object: TObject; const uri: string; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
begin
  FDispatcher.DispatchRequest(connection_object,uri,method);
end;


procedure TFRE_BASE_SERVER.InterLinkDispatchTimer(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var
    CMD_Answer      : IFRE_DB_COMMAND;
    i               : Integer;
begin
  if FTerminating then exit;
  //writeln('Interlinkdispatch Timer ',TiD,' ',GFRE_BT.Get_Ticks_ms,'   ',cp);
  FSessionTreeLock.Acquire;
  try
    if FUserSessionsTree.QueryTreeChange then begin
      FSession_dispatch_array := FUserSessionsTree.GetAllItemsAsArray;
    end;
    for i := 0 to high(FSession_dispatch_array) do begin
      if FSession_dispatch_array[i].Session_Has_CMDS then begin
        CMD_Answer := FSession_dispatch_array[i].WorkSessionCommand;
        if assigned(CMD_Answer) then begin
          CMD_Answer.GetAnswerInterface.Send_ServerClient(CMD_Answer); //Debug Breakpoint
        end;
      end;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;


procedure TFRE_BASE_SERVER.WFE_DispatchTimerEvent(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
begin
 // GFRE_DT.Now_UTC;
//  writeln('WFE Dispatch TIMER ',TiD,' ',GFRE_BT.Get_Ticks_ms,'   ',cp);
end;

procedure TFRE_BASE_SERVER.NewConnection(const session_id: string);
begin
  writeln('NEW INTERLINK CONNECTION : ',session_id);
end;

procedure TFRE_BASE_SERVER.ConnectionDropped(const session_id: string);
begin
  writeln('DROPPED INTERLINK CONNECTION : ',session_id);
end;

procedure TFRE_BASE_SERVER.WorkCommandsCallback(sender: Tobject);
begin
  FInterLinkEvent.FireEventManual(false);
end;


function TFRE_BASE_SERVER.GetImpersonatedDatabaseConnectionSession(const dbname, username, pass,default_app: string;const default_uid_path : TFRE_DB_GUIDArray; out dbs: TFRE_DB_UserSession): TFRE_DB_Errortype;
var found : boolean;
    res   : TFRE_DB_Errortype;
    dbc   : IFRE_DB_CONNECTION;
begin
  result:= GetImpersonatedDatabaseConnection(dbname,username,pass,dbc);
  if result=edb_OK then begin
    dbs := TFRE_DB_UserSession.Create(username,'',default_app,default_uid_path,dbc);
    dbs.OnGetImpersonatedDBC := @GetImpersonatedDatabaseConnection;
    dbs.OnRestoreDefaultDBC  := @RestoreDefaultConnection;
    dbs.OnExistsUserSession  := @ExistsUserSessionForUser;
    dbs.OnCheckUserNamePW    := @CheckUserNamePW;
  end;
end;

function TFRE_BASE_SERVER.ExistsUserSessionForUser(const username: string; out other_session: TFRE_DB_UserSession): boolean;
var fsession : TFRE_DB_UserSession;

  function SearchUser(const session:TFRE_DB_UserSession):boolean;
  begin
    if uppercase(session.GetUsername)=uppercase(username) then begin
      fsession := session;
      result   := true;
    end;
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
      result            := true;
    end;
  finally
    FSessionTreeLock.Release;
  end;
end;

function TFRE_BASE_SERVER.CheckUserNamePW(username, pass: TFRE_DB_String): TFRE_DB_Errortype;
begin
  result := FDefaultSession.GetDBConnection.CheckLogin(username,pass);
end;

constructor TFRE_BASE_SERVER.create(const defaultdbname: string);
begin
  DefaultDatabase := defaultdbname;
end;

procedure TFRE_BASE_SERVER.BindInitialSession(const back_channel: IFRE_DB_COMMAND_REQUEST_ANSWER_SC; out  session: TFRE_DB_UserSession; const old_session_id: string;  const interactive_session: boolean);
var ws         : TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY;
    SessionKey : String;
    found      : boolean;
begin
  found:=false;
  if old_session_id<>'' then begin
    FSessionTreeLock.Acquire;
    try
      if FUserSessionsTree.Find(old_session_id,session) then begin
        GFRE_DBI.LogDebug(dblc_SESSION,'REUSING SESSION [%s]',[old_session_id]);
        found:=true;
        session.SetServerClientInterface(back_channel,interactive_session);
      end else begin
        GFRE_DBI.LogDebug(dblc_SESSION,'OLD REQUESTED SESSION NOT FOUND [%s]',[old_session_id]);
      end;
    finally
      FSessionTreeLock.Release;
    end;
  end;
  if not found then begin
    session := FDefaultSession.CloneSession(back_channel.GetInfoForSessionDesc); //  (sender as TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY).GetSocketDesc
    session.OnWorkCommandsEvent := @WorkCommandsCallback;
    FSessionTreeLock.Acquire;
    try
      FUserSessionsTree.Add(session.GetSessionID,session);
      session.SetServerClientInterface(back_channel,interactive_session);
    finally
      FSessionTreeLock.Release;
    end;
    GFRE_DBI.LogDebug(dblc_SESSION,'STARTING NEW SESSION [%s]',[session.GetSessionID]);
  end;
end;

procedure TFRE_BASE_SERVER.UnBindSession(const sender: TObject; const session: TFRE_DB_UserSession);
var lSession : TFRE_DB_UserSession;
    sid      : string;
begin
  sid := session.GetSessionID;
  writeln('->>> Unbind SESSION : ',sid);
  FSessionTreeLock.Acquire;
  try
    if FUserSessionsTree.Delete(sid,lSession) then begin
      writeln('SESSION DISPATCH REMOVED');
    end else begin
      writeln('UNBIND SESSION ? NOT FOUND : ',sid);
    end;
  finally
    FSessionTreeLock.Release;
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

function TFRE_BASE_SERVER.RestoreDefaultConnection(out username: TFRE_DB_String; out conn: IFRE_DB_CONNECTION): TFRE_DB_Errortype;
begin
  conn     := FDefaultSession.GetDBConnection;
  username := FDefaultSession.GetUsername;
  result   := edb_OK;
end;

procedure RegisterLogin;
begin
  GFRE_DB.RegisterObjectClassEx(TFRE_DB_LOGIN);
  GFRE_DBI.Initialize_Extension_Objects;
end;


end.

