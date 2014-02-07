unit fre_aps_comm_impl;

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

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_INTERLOCKED,FRE_FCOM_SSL,
  FRE_LIBEVENT_CORE,sockets,errors,fre_system,fre_db_interface,fos_sparelistgen,fre_http_client
  {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  {$IFDEF windows}
  ,windows
  {$ENDIF}
  ;

const C_CHANNEL_RUNNER_THREADS = 16;
var
    cAPSC_JACK_TIMEOUT:integer= 5000;

procedure Setup_APS_Comm;
procedure Teardown_APS_Comm;

procedure Test_APSC(const what:string='');

implementation

type
  TAPSC_CMD  = (cb_NEW_LISTENER=1,cb_START_LISTENER=2,cb_STOP_LISTENER=3,cb_FINALIZE_LISTENER=4,cb_NEW_SOCK_ACCEPTED=5,cb_FINALIZE_MAIN=6,cb_FINALIZE_CHANNEL_MGR=7,cb_FINALIZE_CHANNEL=8,
                cb_NEW_CLIENT_SOCK=9,cb_ADD_GLOBAL_TIMER=10,cb_SCHED_COROUTINE=11,cb_NEW_LISTENER_UX=12,cb_FIRE_GLOBAL_TIMER=13);

function  APSC_CheckResultSetError (const res : cInt ; var error : string ; var os_ecode : NativeInt ; const prefix : string='' ; const postfix : string =''):boolean;forward;
procedure APSC_WriteCommpacket     (const cmd : TAPSC_CMD ; data:ShortString ; const fd : cint);forward;
function  APSC_SetNoDelay          (const socket:fcom_int;const bOn: Boolean):fcom_int; forward;


const
   AI_PASSIVE      = $00000001;
   AI_CANONNAME    = $00000002;
   AI_NUMERICHOST  = $00000004;
   AI_NUMERICSERV  = $00000008;
   AI_ALL          = $00000100;
   AI_V4MAPPED_CFG = $00000200;
   AI_ADDRCONFIG   = $00000400;
   AI_V4MAPPED     = $00000800;
   AI_DEFAULT      = AI_V4MAPPED_CFG or AI_ADDRCONFIG;
   AI_MASK         = AI_PASSIVE or AI_CANONNAME or AI_NUMERICHOST or AI_NUMERICSERV or AI_ADDRCONFIG;

type

  TAPSC_EV_TYPES = (acev_READ,acev_WRITE,acev_TIMEOUT,acev_SIGNAL);
  TAPSC_EV_TYP   = set of TAPSC_EV_TYPES;
  TAPSC_CtrCMD   = procedure  (const cmd : TAPSC_CMD ; const data : PShortString) of object;

  { TFRE_APS_LL_EvBaseController }
  TFRE_APS_LL_EvBaseController=class
  private
  type
    TCommreadstate = (cr_BAD,cr_WAIT_LEN,cr_WAITDATA,cr_DISPATCH);
  var
    FGoDown        : Boolean; // FLah when the llop was requested to stop
    FCommPair      : tevutil_socket_t_pair;
    state          : TCommreadstate;
    cmd            : Byte;
    rest_len       : Integer;
    data           : Shortstring;
    data_pos       : NativeInt;
    data_len       : NativeInt;
    FControlEvent  : PEvent;
    FTimeoutE      : PEvent;
    FEventBase     : PEvent_base;
    FDnsBase       : Pevdns_base;
    FonDispatch    : TAPSC_CtrCMD;
    FCreateDNS     : Boolean;
    FId            : String;
    Ftimeout       : TFCOM_TimeVal;
    procedure  _CommEventfired    (what : TAPSC_EV_TYP);
    procedure  _TimeoutEventfired (what : TAPSC_EV_TYP);
  public
    function    SourceFD : cInt;
    function    SinkFD   : cInt;
    constructor Create (const mydispatch : TAPSC_CtrCMD ; const create_dns_base : boolean ; const id : string);
    procedure   Loop;
    procedure   FinalizeLoop;
  end;


  { TFRE_APSC_Listener }

  TFRE_APS_COMM_MAIN_THREAD=class;

  PFRE_APSC_Listener=^TFRE_APSC_Listener;

  TFRE_APSC_Listener=class(TObject,IFRE_APSC_LISTENER)
  private
    procedure AcceptCB (const what : cshort); // InSync with MAIN!
  var
    FState        : TAPSC_ListenerState;
    Fsockaddr     : TFCOM_SOCKADDRSTORAGE;
    Fsockaddr_len : cInt;
    FListenAddr   : string;
    FListensock   : cInt;
    FError        : String;
    FEcode        : NativeInt;
    FEvent        : PEvent;
    FLock         : IFOS_LOCK;
    FId           : String;
    FSSL_CTX      : PSSL_CTX;
    FSSL_Enabled  : Boolean;
    FsockaddrUnix : sockaddr_un;
  protected
    procedure  SetupEvent              (const sa_data : PShortString ; const event_base : PEvent_base);
    procedure  SetupUnixSocketListener (const event_base: PEvent_base);
    function   GetState            : TAPSC_ListenerState;
    function   GetErrorString      : string;
    function   GetListeningAddress : string;
    procedure  _InSync_Start;
    procedure  _InSync_Stop;
    procedure  _InSync_Finalize;
    function   IsIPListener : boolean;
  public
    procedure  EnableSSL           (const server_ctx : PSSL_CTX);
    procedure  Stop;
    procedure  Start;
    procedure  Finalize; // called over manager
    destructor Destroy;override;
    function   GetID  : String;
  end;

  { TFRE_APS_COMM_MAIN_THREAD }
  TFRE_APSC_CHANNEL         = class;
  TFRE_APSC_CHANNEL_MANAGER = class;
  TFRE_APSC_TIMER           = class;

  OFOS_SL_TFRE_APSC_Listener = specialize OFOS_SpareList<TFRE_APSC_LISTENER>;
  OFOS_SL_TFRE_APSC_TIMER    = specialize OFOS_SpareList<TFRE_APSC_TIMER>;

  TFRE_APS_COMM_MAIN_THREAD = class(TThread) // Manages all Listeners / main sync base
  private
    FCB               : TFRE_APS_LL_EvBaseController;
    FChannelManagers  : Array [0..C_CHANNEL_RUNNER_THREADS-1] of TFRE_APSC_CHANNEL_MANAGER;
    FListenerlList    : OFOS_SL_TFRE_APSC_Listener;
    FTimerList        : OFOS_SL_TFRE_APSC_Timer;
    function    _GetManagerMinimumConnsIDX : NativeInt;
    function    _GetManagerIDX             (const chanman : IFRE_APSC_CHANNEL_MANAGER): NativeInt;
    procedure   GotCtrCMD (const cmd : TAPSC_CMD ; const data : PShortString);
    procedure   _InSyncDistributeNewAccept(list : TFRE_APSC_Listener ; new_fd : cint; sa : PFCOM_SOCKADDRSTORAGE ; len : socklen_t);
    procedure   _DistributeNewClientSock(const channel : TFRE_APSC_CHANNEL ; const chanman : IFRE_APSC_CHANNEL_MANAGER = nil);
    procedure   _InSync_FinalizeMain;
  public
    constructor Create   ;
    destructor  Destroy  ; override;
    procedure   Execute  ; override;
  end;

  TFRE_APS_COMM_DNS_ANSWER=class
  end;

  { TFRE_APSC_TIMER }

  PFRE_APSC_TIMER = ^TFRE_APSC_TIMER;
  TFRE_APSC_TIMER = class(TObject,IFRE_APSC_TIMER)
  private
    FGlobal            : Boolean;
    FManager           : TFRE_APSC_CHANNEL_MANAGER;
    FEvent             : PEvent;
    FCallMethod        : TMethod;
    FCallback          : TFRE_APSC_TIMER_CALLBACK;
    FId                : string;
    FInterval          : TTimeVal;
    FCreateThreadID    : TThreadID;
    FNewTimerCB        : TOnNew_APSC_Timer;
    procedure   TimerCallback(const what : cshort);
    procedure   ThreadCheck;
    procedure   InternalFireTimer(const flag1,flag2:boolean);
   public
    constructor Create          (man : TFRE_APSC_CHANNEL_MANAGER ; const base :PEvent_base ; const interval : NativeUint);
    constructor CreateGlobal    (const id : ShortString ; const interval : NativeUint ; const timernewcb : TOnNew_APSC_Timer ; const timercb: TFRE_APSC_TIMER_CALLBACK);
    procedure   SetupGlobal     (const base : PEvent_base);
    procedure   CallbackNewTimer;
    procedure   TIM_Start;
    procedure   TIM_Stop;
    procedure   TIM_SetInterval (const interval_ms : NativeUint);
    procedure   TIM_SetCallback (cb : TFRE_APSC_TIMER_CALLBACK);
    procedure   TIM_SetID       (const ID:String);
    function    TIM_GetID       : string;
    procedure   TIM_SetMethod   (const m : TMethod);
    function    TIM_GetMethod   :TMethod;
    procedure   TIM_Trigger     (const flag1:boolean=false ; const flag2:boolean=false); // Must be called in same MANAGER CONTEXT (THREAD) or on GLOBAL TIMER !!
    procedure   Finalize        ; // only in SYNC !!!
    destructor  Destroy        ; override;
  end;

  { TFRE_APSC_CHANNEL }
  PFRE_APSC_CHANNEL = ^TFRE_APSC_CHANNEL;

  TFRE_APSC_CHANNEL=class(TObject,IFRE_APSC_CHANNEL)
  private
  type
    TSAFE_WRITE_ENCAP=class
      FData : Pointer;
      FLen  : NativeInt;
    end;
  var
    FState            : TAPSC_ChannelState;
    FClient           : Boolean;
    FDoDNS            : Boolean;
    FIsIPSocket       : Boolean;
    Fsockaddr         : TFCOM_SOCKADDRSTORAGE;
    Fsockaddr_len     : cInt;
    Fsockaddrbind     : TFCOM_SOCKADDRSTORAGE;
    Fsockaddrb_len    : cInt;
    FsockaddrUnix     : sockaddr_un;
    FsockaddrUnix_len : cInt;
    FSocketAddr       : string;
    Fsocket           : cInt;
    FChanError        : String;
    FChanECode        : NativeInt;
    FBufEvent         : PBufferevent;
    FManager          : TFRE_APSC_CHANNEL_MANAGER;
    FListener         : TFRE_APSC_Listener;
    FInputBuf         : PEvbuffer;
    FOutputBuf        : PEvbuffer;
    FInBufCB          : Pevbuffer_cb_entry;
    FOutBufCB         : Pevbuffer_cb_entry;
    FTotalInRead      : NativeUint;
    FTotalOutWrite    : NativeUint;
    FVerboseID        : String;
    FConnectHost      : String; // do it via DNS or unix socket
    FConnectPort      : Cardinal;
    FConnectFam       : cInt;
    FDataTag          : PtrUInt;

    FonRead       : TFRE_APSC_CHANNEL_EVENT;
    FOnDisco      : TFRE_APSC_CHANNEL_EVENT;
    FId           : ShortString;
    FnewChanCB    : TOnNew_APSC_Channel;
    FCreateThreadID : TThreadID;
    FClientSSL_CTX  : PSSL_CTX;
    FSSL_Enabled    : Boolean;

    FFinalizecalled    : Boolean;
    FDisconnectHandled : Boolean;

    procedure   ReadDataEvent;
    procedure   WriteDataEvent;
    procedure   InputBufferEvent  (const bufinfo : Pevbuffer_cb_info);
    procedure   OutputBufferEvent (const bufinfo : Pevbuffer_cb_info);
    procedure   GenericEvent      (what : cShort);
    procedure   _InSync_Finalize;
    procedure   ThreadCheck;
    procedure   EventDisconnectOnce;
    function    _GetDebugID         : String;

  protected
    procedure   SetupServedSocket      (fd : cint ; new_sa : PFCOM_SOCKADDRSTORAGE ; new_sal : cInt ; const base : PEvent_base ; newchannelcb : TOnNew_APSC_Channel;const is_ip_socket:boolean); // a server socket
    procedure   SetupClientSocketTCP   (new_sa : PFCOM_SOCKADDRSTORAGE ; new_sal : cInt ; bind_sa : PFCOM_SOCKADDRSTORAGE ; bind_sal : cInt ; const id:ShortString ; newchannelcb : TOnNew_APSC_Channel ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT); // a client socket part 1
    procedure   StartClientSockConnect (const base : PEvent_base ; const dnsbase : Pevdns_base  ;manager : TFRE_APSC_CHANNEL_MANAGER); // a client socket part 2
    procedure   SetupClientSocketDNS   (const host : string ; port : NativeInt ; bind_sa : PFCOM_SOCKADDRSTORAGE ; bind_sal : cInt ; const id : ShortString ; newchannelcb : TOnNew_APSC_Channel ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT); // a client socket part 1
    procedure   SetupClientSocketUX    (const special_file : Shortstring ; const id : ShortString ; newchannelcb : TOnNew_APSC_Channel ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT);

    procedure   COR_SafeWriteBuffer(const data: pointer);
  public
    function    GetChannelManager   : IFRE_APSC_CHANNEL_MANAGER;
    function    GetListener         : IFRE_APSC_LISTENER;
    function    GetConnSocketAddr   : String;
    function    GetHandleKey        : cInt;

    procedure   SetOnReadData     (on_read  : TFRE_APSC_CHANNEL_EVENT);
    procedure   SetOnDisconnnect  (on_disco : TFRE_APSC_CHANNEL_EVENT);

    function    GetVerboseDesc      : String;
    procedure   SetVerboseDesc      (const desc:string);

    procedure   CH_WriteString      (const str : String);
    procedure   CH_WriteBuffer      (const data : Pointer ; const len : NativeInt);
    procedure   CH_SAFE_WriteBuffer (const data : Pointer ; const len : NativeInt); // data gets copied ...
    procedure   CH_WriteOpenedFile  (const fd : cInt ; const offset,len : NativeInt);
    procedure   CH_Enable_Reading ;
    procedure   CH_Enable_Writing ;
    function    CH_GetDataCount   : NativeInt;
    function    CH_ReadString     : String;
    function    CH_ReadBuffer     (const data : Pointer ; const len : NativeInt) : NativeInt;
    function    CH_GetErrorString : String;
    function    CH_GetErrorCode   : NativeInt;
    function    CH_IsClientChannel: Boolean;
    function    CH_GetState       : TAPSC_ChannelState;
    function    CH_GetID          : ShortString;
    procedure   CH_AssociateData    (const data : PtrUInt);
    function    CH_GetAssociateData : PtrUInt;

    constructor Create  (manager : TFRE_APSC_CHANNEL_MANAGER ; listener : TFRE_APSC_LISTENER);
    destructor  Destroy;override;
    procedure   Finalize; // called from IF - need to go over manager ...
  end;

  OFOS_SL_TFRE_APSC_CHANNEL  = specialize OFOS_SpareList<TFRE_APSC_CHANNEL>;


  { TFRE_APSC_CHANNEL_MANAGER }
  TFRE_APSC_CHANNEL_MANAGER = class(TThread,IFRE_APSC_CHANNEL_MANAGER)
  private
    FChannelMgrID : NativeInt;
    FCMVerbose    : String[8];
    FChanBaseCtrl : TFRE_APS_LL_EvBaseController;
    FChannelList  : OFOS_SL_TFRE_APSC_CHANNEL;
    FChanTimerList: OFOS_SL_TFRE_APSC_TIMER;
    FChannelCount : NativeInt;
    procedure   GotChannelCtrCMD (const cmd  : TAPSC_CMD ; const data : PShortString);
    procedure   _FinalizeChannel (const chan : TFRE_APSC_CHANNEL);
  public
    function    GetID                   : NativeInt;
    function    AddTimer                (interval_ms : NativeUint) : IFRE_APSC_TIMER; // Must be called in sync with CHANNEL MANAGER
    procedure   ScheduleCoRoutine       (const method : TFRE_APSC_CoRoutine ; const data : Pointer); // From another thread context to this context

    constructor Create   (const ID :Nativeint);
    destructor  Destroy  ; override;
    procedure   Execute  ; override;
  end;


  { TFRE_APS_COMM }
  TFRE_APS_COMM=class(Tobject,IFRE_APSC)
  private
    FSSL_CTX             : PSSL_CTX;
    FMainThread          : TFRE_APS_COMM_MAIN_THREAD;
    FOnNew_APSC_Listener : TOnNew_APSC_Listener;
    FonNew_APSC_Channel  : TOnNew_APSC_Channel;
    FOnNew_APSC_Timer    : TOnNew_APSC_Timer;
    FOnNew_APSC_Signal   : TOnNew_APSC_Signal;
    TEST_Listener        : IFRE_APSC_LISTENER;
    procedure _CallbackSignal        (const signal : cint);
    procedure _CallbackManagerSocket (const channel  : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure _CallbackListener      (const listener : TFRE_APSC_Listener ; const state : TAPSC_ListenerState);
    procedure _CallbackTimer         (const timer    : IFRE_APSC_Timer);
    procedure _ActivateListener      (const listener : TFRE_APSC_Listener);
    procedure _AddUnixSockListener   (const listener : TFRE_APSC_Listener);
    procedure _StopListener          (const listener : TFRE_APSC_Listener);
    procedure _FinalizeMain          ;
    procedure _FinalizeListener      (const listener : TFRE_APSC_Listener);
    procedure _FireGlobalTimer       (const timer    : TFRE_APSC_Timer ; const flag1,flag2:boolean);
  protected

    procedure   TEST_ListenerCB         (const listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure   TEST_ListenerCB_SSL     (const listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure   TEST_ConnectManSockCB   (const channel  : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure   TEST_NewCLientSock2     (const channel  : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure   TEST_DiscoClientChannel (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_ReadClientChannel  (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_ReadClientChannel2 (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_NewTimerCB         (const timer    : IFRE_APSC_TIMER);
    procedure   TEST_TIMER_TIME         (const timer    : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
  public
    constructor create            ;
    destructor  Destroy           ; override;
    procedure   RunUntilTerminate ;
    procedure   RequestTerminate  ;
    function    AddTimer          (const timer_id: ShortString ; interval_ms : NativeUint ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; local_new_timercb : TOnNew_APSC_Timer=nil) : IFRE_APSC_TIMER; // Must be used in sync with MAIN EVENT LOOP


    procedure   AddListener_TCP   (Bind_IP,Bind_Port:String ; const ID:ShortString);// is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, *4, and *6 (which use all addresses of the host)
    procedure   AddListener_UX    (const special_file:shortstring ; const id:shortstring);
    procedure   AddClient_TCP     (IP,Port : String; const ID:ShortString ; const channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localNewChannelCB : TOnNew_APSC_Channel = nil ;  localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String='');
    procedure   AddClient_TCP_DNS (Host,Port : String; const ID:ShortString ; const channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localNewChannelCB : TOnNew_APSC_Channel = nil ;  localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String='');
    procedure   AddClient_UX      (const special_file:shortstring ; const ID:Shortstring ; const channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localNewChannelCB : TOnNew_APSC_Channel = nil ;  localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil);
    procedure   ResolveDNS_TCP    (const addrstring : String);
    procedure   SetNewListenerCB  (const lcb : TOnNew_APSC_Listener);
    procedure   SetNewChannelCB   (const chancb   : TOnNew_APSC_Channel);
    procedure   SetNewTimerCB     (const timercb  : TOnNew_APSC_Timer);
    procedure   SetSingnalCB      (const signalcb : TOnNew_APSC_Signal);
  end;

var GAPSC : TFRE_APS_COMM;

Procedure DoSig(sig : cint);cdecl;
begin
  GAPSC._CallbackSignal(sig);
end;


procedure Setup_APS_Comm;
begin
  if assigned(GAPSC) then
    raise Exception.Create('double setup APS_COMM');
  GAPSC   := TFRE_APS_COMM.Create;
  GFRE_SC := GAPSC;
end;


procedure Teardown_APS_Comm;
begin
  GAPSC.free;
  GAPSC:=nil;
end;

procedure Test_APSC(const what:string);
var
     i  : Integer;
    hc  : TFRE_SIMPLE_HTTP_CLIENT;
    cnt : integer;


    procedure GotHttpAnswer(const sender : TFRE_SIMPLE_HTTP_CLIENT ; const http_status,content_len : NativeInt ;  const contenttyp : string ; content : PByte);
    begin
      writeln(cnt,' Answer : ',http_status,' ',content_len,' ',contenttyp);
      inc(cnt);
      writeln(copy(PChar(content),1,maxint));
      sender.Free;
    end;

begin
  case  lowercase(what) of
     '' :
         begin
           GFRE_SC.SetNewTimerCB(@GAPSC.TEST_NewTimerCB);
           GFRE_SC.SetNewListenerCB(@GAPSC.TEST_ListenerCB);
           GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);

           GFRE_SC.AddTimer('TEST',1000,nil);
           GFRE_SC.AddListener_TCP('[::1]','44000','IP6L');
           //GFRE_SC.AddListener_TCP('[fd9e:21a7:a92c:2323::1]','44000','IP6');
           GFRE_SC.AddListener_TCP('*','44000','I4L');
           for i:=1 to 3 do
               GFRE_SC.AddClient_TCP('[::1]','44000','CL'+inttostr(i));
            repeat
             sleep(100);
            until assigned(GAPSC.TEST_Listener);
            writeln('ASYNC : ',GAPSC.TEST_Listener.GetState);
        end;
     'echounix': begin
                    GFRE_SC.SetNewListenerCB(@GAPSC.TEST_ListenerCB);
                    GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);
                    GFRE_SC.AddListener_UX('/tmp/uxtest','UXS');
                 end;
     'echossl':  begin
                       GFRE_SC.SetNewListenerCB(@GAPSC.TEST_ListenerCB_SSL);
                       GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);
                       GFRE_SC.AddListener_TCP('*','44000','IP46-ECHO');
                    end;
     'unixclient' : begin
                      GFRE_SC.AddClient_UX('/tmp/uxtest','UXS',nil,@GAPSC.TEST_NewCLientSock2,@GAPSC.TEST_ReadClientChannel2,@GAPSC.TEST_DiscoClientChannel);
                    end;
     'httpclient' : begin
                      cnt := 0;
                      for i:=0 to 100 do
                          begin
                            hc := TFRE_SIMPLE_HTTP_CLIENT.create;
                            //hc.SetHost('www.firmos.at');
                            hc.SetHost('80.123.225.59');
                            hc.SetPort('80');
                            hc.SetIP_Mode(true);
                            hc.GetMethod('/test/test.json',@GotHttpAnswer);
                          end;
                    end;
  end;
end;

procedure   LogInfo(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Info,CFOS_LL_Target[fll_Info],false);
end;

procedure   LogNotice(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Notice,CFOS_LL_Target[fll_Notice],false);
end;


procedure LogDebug(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Debug,CFOS_LL_Target[fll_Debug],false);
end;

procedure   LogWarning(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Warning,CFOS_LL_Target[fll_Warning],false);
end;

procedure   LogError(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Error,CFOS_LL_Target[fll_Error],false);
end;


function APSC_TranslateOsError(const os_error: cint; const prefix: string=''; postfix: string=''): string;
begin
  result := prefix+inttostr(os_error)+':'+StrError(os_error)+postfix;
end;

procedure APSC_CheckRaiseOSCall(const os_result: cint; const prefix: string; postfix: string; const crit_abort: boolean);
var err : cInt;
begin
  if os_result<>0 then
    begin
      err := fpgeterrno;
      if crit_abort then
        GFRE_BT.CriticalAbort(APSC_TranslateOsError(err,prefix,postfix))
      else
        raise exception.Create(APSC_TranslateOsError(err,prefix,postfix));
    end;
end;

procedure APSC_CheckRaise(const os_result: cint; const crit_abort: boolean; const file_nr: cInt=-1);
var postfix :string;
begin
  if file_nr<>-1 then
    postfix := ' ('+inttostr(file_nr)+')';
  APSC_CheckRaiseOSCall(os_result,'APS/FCOMM CRITICAL: ',postfix,crit_abort);
end;


function APSC_le_evtyp2TAPSC_EV_TYP(const what : cshort):TAPSC_EV_TYP;
begin
  result := [];
  if (what and EV_READ) >0 then
    Include(result,acev_READ);
  if (what and EV_WRITE) >0 then
    Include(result,acev_WRITE);
  if (what and EV_TIMEOUT) >0 then
    Include(result,acev_TIMEOUT);
  if (what and EV_SIGNAL) >0 then
    Include(result,acev_SIGNAL);
end;

procedure APSC_SetupTimeout(const time_ms : NativeInt ; var time : TFCOM_TimeVal);
begin
  Time.tv_usec  := (time_ms mod 1000) * 1000;
  Time.tv_sec   := time_ms div 1000;
end;

function APSC_typ2string(const typ:TAPSC_EV_TYP):ShortString;
begin
  result := '[';
  if acev_READ in typ
    then result := result +'R';
  if acev_WRITE in typ
    then result := result +'W';
  if acev_TIMEOUT in typ
    then result := result +'T';
  if acev_SIGNAL in typ
    then result := result +'S';
  result := result+']';
end;

function APSC_sa2string(const sa : PFCOM_SOCKADDRSTORAGE;const with_port:boolean=true):string;
var nullpos : NativeInt;
begin
  SetLength(result,256);
  case sa^.sin_family of
    FCOM_AF_INET : evutil_inet_ntop(AF_INET,@sa^.sin_addr,@Result[1],Length(Result));
    FCOM_AF_INET6: evutil_inet_ntop(AF_INET6,@sa^.sin6_addr,@Result[1],Length(Result));
  end;
  nullpos := pos(#0,Result);
  SetLength(result,nullpos-1);
  if with_port then
    case sa^.sin_family of
      FCOM_AF_INET : result:=result+':'+inttostr(BEtoN(sa^.sin_port));
      FCOM_AF_INET6: result:='['+result+']:'+inttostr(BEtoN(sa^.sin6_port));
    end;
end;


function  GetANewEventBase : PEvent_base;
var cfg  : Pevent_config;
    feat : Integer;
begin
  //cfg      := event_config_new;
  //event_config_require_features(cfg, EV_FEATURE_O1);
  //result  := event_base_new_with_config(cfg);
  //event_config_free(cfg);
  //if not assigned(result) then begin
  //  LogWarning('Cannot get features O1, using fallback',[]);
  //  result:= event_base_new;
  //  if not assigned(result) then
  //    GFRE_BT.CriticalAbort('COULD NOT AQUIRE ANY EVENTBASE!');
  //end;
  result := event_base_new;
  feat := event_base_get_features(result);
  //writeln('Libevent Chosen Method : ',string(event_base_get_method(result)),' Features : ',event_base_get_features(result));
end;

{ TFRE_APSC_Listener }

function APSC_CheckResultSetError(const res : cInt ; var error : string ; var os_ecode : NativeInt ; const prefix : string='' ; const postfix : string =''):boolean;
begin
  if res<0 then
    begin
      os_ecode := fpgeterrno;
      error    := APSC_TranslateOsError(os_ecode,prefix,postfix);
      result   := true;
      exit;
    end
  else
   os_ecode:=0;
  result := false;
end;

procedure INT_AcceptCB(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APSC_Listener(data).AcceptCB(short);
end;

{ TFRE_APS_CHANNEL }

procedure bufev_read(const bev  : PBufferevent ; const ctx : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(ctx).ReadDataEvent;
end;
procedure bufev_write(const bev  : PBufferevent ; const ctx : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(ctx).WriteDataEvent;
end;
procedure bufev_event(const bev  : PBufferevent ; what: cshort ; const ctx : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(ctx).GenericEvent(what);
end;

{ TFRE_APSC_TIMER }

procedure loc_timer_cb(fd : evutil_socket_t ; short: cshort ; data:pointer) ;cdecl;
begin
  TFRE_APSC_TIMER(data).TimerCallback(short);
end;

procedure TFRE_APSC_TIMER.TimerCallback(const what: cshort);
var flg1,flg2:Boolean;
begin
  if assigned(FCallback) then
    begin
      flg1 := what and EV_READ>0;
      flg2 := what and EV_WRITE>0;
      FCallback(self,flg1,flg2);
    end;
end;

procedure TFRE_APSC_TIMER.ThreadCheck;
begin
  if GetThreadID<>FCreateThreadID then
    GFRE_BT.CriticalAbort('timer [%s] thread context violation [%s] vs [%s] ',[FId,inttostr(NativeUint(GetThreadID)),inttostr(NativeUint(FCreateThreadID))]);
end;

procedure TFRE_APSC_TIMER.InternalFireTimer(const flag1, flag2: boolean);
var what : cshort;
begin
  ThreadCheck;
  if flag1 then
    what := what or EV_READ;
  if flag2 then
    what := what or EV_WRITE;
  event_active(FEvent,what,1);
end;

constructor TFRE_APSC_TIMER.Create(man: TFRE_APSC_CHANNEL_MANAGER; const base: PEvent_base; const interval: NativeUint);
begin
  FManager := man;
  FGlobal  := false;
  FEvent   := event_new(base,-1,EV_READ or EV_WRITE or EV_TIMEOUT or EV_PERSIST,@loc_timer_cb,self);
  APSC_SetupTimeout(interval,FInterval);
  FCreateThreadID := GetThreadID;
end;

constructor TFRE_APSC_TIMER.CreateGlobal(const id: ShortString; const interval: NativeUint; const timernewcb: TOnNew_APSC_Timer; const timercb: TFRE_APSC_TIMER_CALLBACK);
begin
  FManager    := nil;
  FGlobal     := true;
  FId         := id;
  APSC_SetupTimeout(1000,FInterval);
  FNewTimerCB := timernewcb;
  FCallback   := timercb;
end;

procedure TFRE_APSC_TIMER.SetupGlobal(const base: PEvent_base);
begin
  FEvent          := event_new(base,-1,EV_READ or EV_WRITE or EV_TIMEOUT or EV_PERSIST,@loc_timer_cb,self);
  FCreateThreadID := GetThreadID;
end;

procedure TFRE_APSC_TIMER.CallbackNewTimer;
begin
  if assigned(FNewTimerCB) then
    FNewTimerCB(self)
  else
    TIM_Start;
end;

procedure TFRE_APSC_TIMER.TIM_Start;
begin
  ThreadCheck;
  event_add(FEvent,@FInterval);
end;

procedure TFRE_APSC_TIMER.TIM_Stop;
begin
  ThreadCheck;
  event_del(FEvent);
end;

procedure TFRE_APSC_TIMER.TIM_SetInterval(const interval_ms: NativeUint);
begin
  ThreadCheck;
  APSC_SetupTimeout(interval_ms,FInterval);
end;

procedure TFRE_APSC_TIMER.TIM_SetCallback(cb: TFRE_APSC_TIMER_CALLBACK);
begin
  ThreadCheck;
  FCallback := cb;
end;

procedure TFRE_APSC_TIMER.TIM_SetID(const ID: String);
begin
  ThreadCheck;
  FId := Id;
end;

function TFRE_APSC_TIMER.TIM_GetID: string;
begin
  ThreadCheck;
  result := FId;
end;

procedure TFRE_APSC_TIMER.TIM_SetMethod(const m: TMethod);
begin
  ThreadCheck;
  FCallMethod := m;
end;

function TFRE_APSC_TIMER.TIM_GetMethod: TMethod;
begin
  ThreadCheck;
  result := FCallMethod;
end;

procedure TFRE_APSC_TIMER.TIM_Trigger(const flag1: boolean; const flag2: boolean);
begin
  if FGlobal then
    GAPSC._FireGlobalTimer(self,flag1,flag2)
  else
    InternalFireTimer(flag1,flag2);
end;

destructor TFRE_APSC_TIMER.Destroy;
begin
  ThreadCheck;
  if FManager=nil then
    GAPSC.FMainThread.FTimerList.Delete(self)
  else
    FManager.FChanTimerList.Delete(self);
  if assigned(FEvent) then
    begin
      event_del(FEvent);
      event_free(Fevent);
    end;
  inherited Destroy;
end;

procedure TFRE_APSC_TIMER.Finalize;
begin
  Free;
end;

procedure TFRE_APSC_CHANNEL.ReadDataEvent;
begin
  LogDebug('READ ON CHANNEL '+_GetDebugID,[]);
  if Assigned(FonRead) then
    FonRead(self);
end;

procedure TFRE_APSC_CHANNEL.WriteDataEvent;
begin
//  writeln('WRITE ON SOCK ',Fsocket);
end;

procedure TFRE_APSC_CHANNEL.InputBufferEvent(const bufinfo: Pevbuffer_cb_info);
begin
  inc(FTotalInRead,bufinfo^.n_added);
end;

procedure TFRE_APSC_CHANNEL.OutputBufferEvent(const bufinfo: Pevbuffer_cb_info);
begin
  inc(FTotalInRead,bufinfo^.n_deleted);
end;

procedure TFRE_APSC_CHANNEL.GenericEvent(what: cShort);
var dnsres:CInt;

    procedure BailOut;
    begin
      EventDisconnectOnce;
      bufferevent_flush(FBufEvent,EV_WRITE,BEV_FLUSH);
      Finalize;
    end;

begin
  if (what and BEV_EVENT_EOF)>0 then
    begin
      LogDebug('READ EOF (CLOSE) on CHANNEL '+_GetDebugID+' FLAGS '+inttostr(what),[]);
      FState := ch_EOF;
      EventDisconnectOnce;
      bufferevent_flush(FBufEvent,EV_WRITE,BEV_FLUSH);
      Finalize;
    end
  else
  if (what and BEV_EVENT_CONNECTED)>0 then
    begin
      FState     := ch_ACTIVE;
      Fsocket    := bufferevent_getfd(FBufEvent);
      if FIsIPSocket then
        begin
          if APSC_CheckResultSetError(APSC_SetNoDelay(Fsocket,true),FChanError,FChanECode,'CLIENT/CONN/SETNODELAY: ') then
            BailOut
        end;
      if APSC_CheckResultSetError(evutil_make_socket_closeonexec(Fsocket),FChanError,FChanECode,'CLIENT/CONN/SETCLOSEONEX: ') then
        BailOut
      else
      if APSC_CheckResultSetError(evutil_make_socket_nonblocking(Fsocket),FChanError,FChanECode,'CLIENT/CONN/SETNONBLOCK: ') then
        BailOut
      else
        begin
          Fsockaddr_len:=SizeOf(FSockAddr);
          if FIsIPSocket then
            begin
              dnsres := fpgetpeername(Fsocket,@Fsockaddr,@Fsockaddr_len);
              FSocketAddr := APSC_sa2string(@Fsockaddr);
            end;
          FnewChanCB(self,ch_NEW_CS_CONNECTED);
        end;
    end
  else
  if (what and BEV_EVENT_ERROR)>0 then
    begin
      FState := ch_BAD;
      dnsres :=  bufferevent_socket_get_dns_error(FBufEvent);
      if not APSC_CheckResultSetError(dnsres,FChanError,FChanECode,'DNS:','') then
        APSC_CheckResultSetError(-1,FChanError,FChanECode,'SOCK:','');
      if FChanECode=0 then
        begin
          FChanECode := -1;
          FChanError := 'no extended error info available';
        end;
      BailOut;
    end
  else
    GFRE_BT.CriticalAbort('how to handle '+inttostR(what)+' on '+GetVerboseDesc+'   : '+GetConnSocketAddr);
end;

procedure TFRE_APSC_CHANNEL._InSync_Finalize;
begin
  Free;
end;

procedure TFRE_APSC_CHANNEL.ThreadCheck;
begin
  if GetThreadID<>FCreateThreadID then
    GFRE_BT.CriticalAbort('channel [%s] thread context violation [%s] vs [%s] ',[GetVerboseDesc,inttostr(NativeUint(GetThreadID)),inttostr(NativeUint(FCreateThreadID))]);
end;

procedure TFRE_APSC_CHANNEL.EventDisconnectOnce;
begin
  if (not FDisconnectHandled) and FFinalizecalled then
    begin
      LogDebug('FINALIZE CALLED -> SUPRESS DISCONNECT EVENT '+_GetDebugID,[]);
      FDisconnectHandled := true;
    end;
  if FDisconnectHandled then
    LogDebug('EVENT DISCONNECT ONCE (ALREADY HANDLED/SUPRESSING) FIRED ON '+_GetDebugID,[])
  else
    LogDebug('EVENT DISCONNECT ONCE FIRED ON '+_GetDebugID,[]);
  if not FDisconnectHandled then
    begin
      if assigned(FOnDisco) then
        FOnDisco(self);
    end;
  FDisconnectHandled := true;
end;

function TFRE_APSC_CHANNEL._GetDebugID: String;
begin
  if FClient then
    result := 'CS'+inttostr(Fsocket)+'#'+FSocketAddr+' '+FVerboseID
  else
    result := 'SS'+inttostr(Fsocket)+'#'+FSocketAddr+' '+FVerboseID;
end;

procedure loc_buffer_cb_in(buffer : PEvbuffer ; info : Pevbuffer_cb_info ; arg : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(arg).InputBufferEvent(info);
end;

procedure loc_buffer_cb_out(buffer : PEvbuffer ; info : Pevbuffer_cb_info ; arg : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(arg).OutputBufferEvent(info);
end;


function APSC_SetNoDelay(const socket:fcom_int;const bOn: Boolean):fcom_int;
var  opt: longint;
begin
  if bON then opt:=1 else opt:=0;
  result:=fpsetsockopt(socket,FCOM_IPPROTO_TCP,FCOM_TCP_NODELAY,@opt,SizeOf(opt));
end;


procedure TFRE_APSC_CHANNEL.SetupServedSocket(fd: cint; new_sa: PFCOM_SOCKADDRSTORAGE; new_sal: cInt; const base: PEvent_base; newchannelcb: TOnNew_APSC_Channel;const is_ip_socket:boolean);
begin
  FClient:=false;
  move(new_sa^,Fsockaddr,new_sal);
  Fsockaddr_len := new_sal;
  Fsocket       := fd;
  FnewChanCB    := newchannelcb;
  FIsIPSocket   := is_ip_socket;
  try
    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(Fsocket),FChanError,FChanECode,'SETCLOSEONEX: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(Fsocket),FChanError,FChanECode,'SETNONBLOCK: ') then
      exit;
    if is_ip_socket and
       APSC_CheckResultSetError(APSC_SetNoDelay(Fsocket,true),FChanError,FChanECode,'SETNODELAY: ') then
        exit;
    FSocketAddr := APSC_sa2string(@Fsockaddr);
    FVerboseID  := FSocketAddr;
    if FSSL_Enabled then
      begin
        //FBufEvent := bufferevent_openssl_socket_new(base, Fsocket, FClientSSL_CTX,BUFFEREVENT_SSL_ACCEPTING,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
        FBufEvent := bufferevent_openssl_socket_new(base, Fsocket, FClientSSL_CTX,BUFFEREVENT_SSL_ACCEPTING,BEV_OPT_CLOSE_ON_FREE);
      end
    else
      begin
        //FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
        FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE);
      end;
    if not assigned(FBufEvent) then
      begin
        FChanError:='did not get a bufferevent';
        exit;
      end;
    bufferevent_setcb(FBufEvent,@bufev_read,nil,@bufev_event,self); // @bufev_write not used by now
    FInputBuf  := bufferevent_get_input(FBufEvent);
    FOutputBuf := bufferevent_get_output(FBufEvent);
    FInBufCB   := evbuffer_add_cb(FInputBuf,@loc_buffer_cb_in,self);
    FOutBufCB  := evbuffer_add_cb(FOutputBuf,@loc_buffer_cb_out,self);
    FState := ch_ACTIVE;
    LogInfo('CONNECTED SERVED CHANNEL : '+_GetDebugID,[]);
  finally
    FnewChanCB(self,ch_NEW_CS_CONNECTED);
  end;
end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketTCP(new_sa: PFCOM_SOCKADDRSTORAGE; new_sal: cInt; bind_sa: PFCOM_SOCKADDRSTORAGE; bind_sal: cInt; const id: ShortString; newchannelcb: TOnNew_APSC_Channel; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT);
begin
  FClient:=true;
  move(new_sa^,Fsockaddr,new_sal);
  Fsockaddr_len := new_sal;
  FSocketAddr   := APSC_sa2string(@Fsockaddr);
  FVerboseID    := FSocketAddr;
  FId           := id;
  FnewChanCB    := newchannelcb;
  FOnDisco      := discoevent;
  FonRead       := readevent;
  FIsIPSocket   := true;
end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketDNS(const host: string; port: NativeInt; bind_sa: PFCOM_SOCKADDRSTORAGE; bind_sal: cInt; const id: ShortString; newchannelcb: TOnNew_APSC_Channel; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT);
begin
  FClient       := True;
  FDoDNS        := True;
  Fsockaddr_len := 0;
  FSocketAddr   := '';
  FVerboseID    := '';
  FConnectHost  := host;
  FConnectPort  := port;
  FId           := id;
  FnewChanCB    := newchannelcb;
  FOnDisco      := discoevent;
  FonRead       := readevent;
end;

procedure TFRE_APSC_CHANNEL.StartClientSockConnect(const base: PEvent_base; const dnsbase: Pevdns_base; manager: TFRE_APSC_CHANNEL_MANAGER);
begin
  try
    Fsocket := -1;
    FManager:= manager;
    FCreateThreadID := GetThreadID;
    //FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
    FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE);
    if not assigned(FBufEvent) then
      begin
        FChanError:='did not get a bufferevent';
        exit;
      end;
    FInputBuf  := bufferevent_get_input(FBufEvent);
    FOutputBuf := bufferevent_get_output(FBufEvent);
    FInBufCB   := evbuffer_add_cb(FInputBuf,@loc_buffer_cb_in,self);
    FOutBufCB  := evbuffer_add_cb(FOutputBuf,@loc_buffer_cb_out,self);
    bufferevent_setcb(FBufEvent,@bufev_read,nil,@bufev_event,self); // @bufev_write not used by now
    if FDoDNS then
      begin
        if APSC_CheckResultSetError(bufferevent_socket_connect_hostname(FBufEvent,dnsbase,AF_UNSPEC,Pchar(FConnectHost),FConnectPort),FChanError,FChanECode,'','') then
          exit;
      end
    else
      begin
        if FIsIPSocket then
          begin
            if APSC_CheckResultSetError(bufferevent_socket_connect(FBufEvent,@Fsockaddr,Fsockaddr_len),FChanError,FChanECode,'','') then
              exit;
          end
        else
          begin
            if APSC_CheckResultSetError(bufferevent_socket_connect(FBufEvent,@FsockaddrUnix,FsockaddrUnix_len),FChanError,FChanECode,'','') then
              exit;
          end
      end;
    FState     := ch_WAIT;
    LogInfo('STARTED CLIENT CHANNEL : '+_GetDebugID,[]);
  finally
    if FState=ch_BAD then
      begin
        //BAD STATE
        FnewChanCB(self,ch_NEW_CHANNEL_FAILED);
      end;
  end;
end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketUX(const special_file: Shortstring; const id: ShortString; newchannelcb: TOnNew_APSC_Channel; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT);
begin
  if Length(special_file)>=108 then
    raise Exception.Create('the unix socket path has to be shorter then 108 chars');
  if not FileExists(special_file) then
    raise Exception.Create('could not connect path does not exist : ['+special_file+']');

  FsockaddrUnix.sun_path   := trim(special_file);
  FsockaddrUnix.sun_path[Length(special_file)]:=#0;
  FsockaddrUnix.sun_family := PF_UNIX;

  FClient       := True;
  FDoDNS        := false;
  Fsockaddr_len := 0;
  FSocketAddr   := '';
  FsockaddrUnix.sun_family := AF_UNIX;
  FsockaddrUnix.sun_path   := special_file;
  FsockaddrUnix_len := 128;
  FVerboseID    := '';
  FIsIPSocket   := false;
  FConnectHost  := special_file;
  FConnectPort  := 0;
  FId           := id;
  FnewChanCB    := newchannelcb;
  FOnDisco      := discoevent;
  FonRead       := readevent;
end;

procedure TFRE_APSC_CHANNEL.COR_SafeWriteBuffer(const data: pointer);
var senc : TSAFE_WRITE_ENCAP;
begin
  try
    senc := TSAFE_WRITE_ENCAP(data);
    CH_WriteBuffer(senc.fdata,senc.flen);
    Freemem(senc.FData);
    senc.Free;
  except on e:Exception do
    GFRE_BT.CriticalAbort('COR_SafeWritebuffer failed : '+e.Message);
  end;
end;

function TFRE_APSC_CHANNEL.GetChannelManager: IFRE_APSC_CHANNEL_MANAGER;
begin
  result := FManager;
end;

function TFRE_APSC_CHANNEL.GetListener: IFRE_APSC_LISTENER;
begin
  result := FListener;
end;

function TFRE_APSC_CHANNEL.GetConnSocketAddr: String;
begin
  result := FSocketAddr;
end;

function TFRE_APSC_CHANNEL.GetHandleKey: cInt;
begin
  result := Fsocket;
end;

procedure TFRE_APSC_CHANNEL.SetOnReadData(on_read: TFRE_APSC_CHANNEL_EVENT);
begin
  FonRead := on_read;
end;

procedure TFRE_APSC_CHANNEL.SetOnDisconnnect(on_disco: TFRE_APSC_CHANNEL_EVENT);
begin
  FOnDisco := on_disco;
end;

function TFRE_APSC_CHANNEL.GetVerboseDesc: String;
begin
  result := FVerboseID;
end;

procedure TFRE_APSC_CHANNEL.SetVerboseDesc(const desc: string);
begin
  FVerboseID := desc;
end;

procedure TFRE_APSC_CHANNEL.CH_WriteString(const str: String);
begin
  ThreadCheck;
  if bufferevent_write(FBufEvent,@str[1],Length(str))<>0 then
    GFRE_BT.CriticalAbort('critical: channel write string failed ?');
end;

procedure TFRE_APSC_CHANNEL.CH_WriteBuffer(const data: Pointer; const len: NativeInt);
begin
  ThreadCheck;
  if bufferevent_write(FBufEvent,data,len)<>0 then
    GFRE_BT.CriticalAbort('critical: channel write  buffer failed ?');
end;

procedure TFRE_APSC_CHANNEL.CH_SAFE_WriteBuffer(const data: Pointer; const len: NativeInt);
var senc : TSAFE_WRITE_ENCAP;
begin
  senc := TSAFE_WRITE_ENCAP.Create;
  senc.FData := Getmem(len);
  senc.FLen  := len;
  move(data^,senc.FData^,len);
  FManager.ScheduleCoRoutine(@COR_SafeWriteBuffer,senc);
end;

procedure TFRE_APSC_CHANNEL.CH_WriteOpenedFile(const fd: cInt; const offset, len: NativeInt);
begin
  ThreadCheck;
  if evbuffer_add_file(FOutputBuf,fd,offset,len)<>0 then
    GFRE_BT.CriticalAbort('critical: channel write  buffer failed ?');
end;

procedure TFRE_APSC_CHANNEL.CH_Enable_Reading;
var what : cSHort;
begin
  ThreadCheck;
  what := bufferevent_get_enabled(FBufEvent);
  what := what or EV_READ;
  bufferevent_enable(FBufEvent, what);
end;

procedure TFRE_APSC_CHANNEL.CH_Enable_Writing;
var what : cSHort;
begin
  ThreadCheck;
  what := bufferevent_get_enabled(FBufEvent);
  what := what or EV_WRITE;
  bufferevent_enable(FBufEvent, what);
end;

function TFRE_APSC_CHANNEL.CH_GetDataCount: NativeInt;
begin
  ThreadCheck;
  result := evbuffer_get_length(FInputBuf);
end;

function TFRE_APSC_CHANNEL.CH_ReadString: String;
var read_len,alen : NativeInt;
begin
  ThreadCheck;
  read_len := CH_GetDataCount;
  SetLength(result,read_len);
  alen := evbuffer_remove(FInputBuf,@result[1],read_len);
  if alen<>read_len then
    GFRE_BT.CriticalAbort('intenal error / read_len <> alen');
end;

function TFRE_APSC_CHANNEL.CH_ReadBuffer(const data: Pointer; const len: NativeInt): NativeInt;
begin
  ThreadCheck;
  result   := evbuffer_remove(FInputBuf,data,len);
end;

function TFRE_APSC_CHANNEL.CH_GetErrorString: String;
begin
  ThreadCheck;
  result := FChanError;
end;

function TFRE_APSC_CHANNEL.CH_GetErrorCode: NativeInt;
begin
  ThreadCheck;
  result := FChanECode;
end;

function TFRE_APSC_CHANNEL.CH_IsClientChannel: Boolean;
begin
  ThreadCheck;
  result := FClient;
end;

function TFRE_APSC_CHANNEL.CH_GetState: TAPSC_ChannelState;
begin
  ThreadCheck;
  result := FState;
end;

function TFRE_APSC_CHANNEL.CH_GetID: ShortString;
begin
  result := FId;
end;

procedure TFRE_APSC_CHANNEL.CH_AssociateData(const data: PtrUInt);
begin
  FDataTag := data;
end;

function TFRE_APSC_CHANNEL.CH_GetAssociateData: PtrUInt;
begin
  result := FDataTag;
end;


constructor TFRE_APSC_CHANNEL.Create(manager: TFRE_APSC_CHANNEL_MANAGER ; listener: TFRE_APSC_LISTENER);
begin
  FState    := ch_BAD;
  FManager  := manager;
  FListener := listener;
  if assigned(listener) then // case serversocket
    begin
      FCreateThreadID := GetThreadID;
      if FListener.FSSL_Enabled then
        begin
          FSSL_Enabled:=true;
          FClientSSL_CTX :=  SSL_new(FListener.FSSL_CTX);
        end;
    end;
end;

destructor TFRE_APSC_CHANNEL.Destroy;
begin
  LogInfo('DESTROY CHANNEL : '+_GetDebugID,[]);
  EventDisconnectOnce;
  if assigned(FBufEvent) then
    begin
      bufferevent_disable(FBufEvent,EV_READ+EV_WRITE);
      bufferevent_free(FBufEvent);
    end;
  inherited;
end;

procedure TFRE_APSC_CHANNEL.Finalize;
begin
  if FFinalizecalled then
    exit;
  FFinalizecalled := true;
  LogInfo('FINALIZE CHANNEL CALLED - '+_GetDebugID,[]);
  FManager._FinalizeChannel(self);
end;

{ TFRE_APSC_CHANNEL_MANAGER }

procedure TFRE_APSC_CHANNEL_MANAGER.GotChannelCtrCMD(const cmd: TAPSC_CMD; const data: PShortString); // in sync with this channel manager

  procedure _NewSockAccepted;
  var new_fd  : cint;
      new_sa  : PFCOM_SOCKADDRSTORAGE;
      new_sal : NativeInt;
      newchan : TFRE_APSC_CHANNEL;
      list    : TFRE_APSC_Listener;
  begin
    list      := TFRE_APSC_Listener(PPtrUInt(@data^[1])^);
    newchan   := TFRE_APSC_CHANNEL.Create(self,list);
    new_fd    := pcint(@data^[1+sizeof(PtrUInt)])^;
    new_sa    := @data^[1+sizeof(PtrUInt)+SizeOf(cint)];
    new_sal   := Length(data^)-SizeOf(cint);
    if FChannelList.Add(newchan)=-1 then
      GFRE_BT.CriticalAbort('critical: channel double add ?');
    newchan.SetupServedSocket(new_fd,new_sa,new_sal,FChanBaseCtrl.FEventBase,@GAPSC._CallbackManagerSocket,list.IsIPListener);
    Inc(FChannelCount);
  end;

  procedure _NewClientSockStart;
  var newchan : TFRE_APSC_CHANNEL;
  begin
    newchan   := TFRE_APSC_CHANNEL(PPtrUInt(@data^[1])^);
    newchan.StartClientSockConnect(FChanBaseCtrl.FEventBase,FChanBaseCtrl.FDnsBase,self);
    if FChannelList.Add(newchan)=-1 then
      GFRE_BT.CriticalAbort('critical: channel double add ?');
    Inc(FChannelCount);
  end;


  procedure _FinalizeSingleChannel;
  var chan : TFRE_APSC_CHANNEL;
  begin
    chan := TFRE_APSC_CHANNEL(PPtrUInt(@data^[1])^);
    if not FChannelList.Delete(chan) then
      GFRE_BT.CriticalAbort('channel delete / not found');
    chan._InSync_Finalize;
  end;

  procedure _FinalizeAllChannels;
    procedure FinalizeChans(var chan : TFRE_APSC_CHANNEL ; const idx :NativeInt ; var halt : boolean);
    begin
      LogDebug('Finalizing channel %d',[chan.GetHandleKey]);
      chan.Free;
      chan := nil;
    end;
    procedure FinalizeTimers(var tim : TFRE_APSC_TIMER ; const idx :NativeInt ; var halt : boolean);
    begin
      tim.Free;
      tim := nil;
    end;
  begin
    LogDebug('FINALIZE ALL CHANNELS ON MGR %d',[FChannelMgrID]);
    try
      FChannelList.ForAllBreak(@FinalizeChans);
    except on e:exception do
      LogError('CHANNEL FINALIZE EXCEPTION: %s',[e.Message]);
    end;
    LogDebug('FINALIZING TIMERS',[]);
    try
      FChanTimerList.ForAllBreak(@FinalizeTimers);
    except on e:exception do
      LogError('TIMER FINALIZE EXCEPTION: %s',[e.Message]);
    end;
    LogDebug('FINALIZE BASELOOP ON MGR %d',[FChannelMgrID]);
    FChanBaseCtrl.FinalizeLoop;
    LogDebug('FINALIZE BASELOOP ON MGR %d DONE',[FChannelMgrID]);
  end;

  //TODO Beautify CoRoutine Encap with private Record Type
  //type
  //RCoRoutineEncap=record
  //  Meth : TMethod;
  //  Data : Pointer;
  //end;
  //PCoRoutineEncap=^RCoRoutineEncap;

  procedure _RunCoroutine;
  var pack  : ShortString;
      m     : TMethod;
      mdata : Pointer;
  begin
    m.code := Pointer(PPtrUInt(@data^[1])^);
    m.data := Pointer(PPtrUInt(@data^[1+SizeOf(NativeUint)])^);
    mdata  := Pointer(PPtrUInt(@data^[1+2*SizeOf(NativeUint)])^);
    try
      TFRE_APSC_CoRoutine(m)(mdata);
    except on e:exception do
      begin
        writeln('**** APSC: Coroutine Failed ',FChannelMgrID,' ',e.Message);
      end;
    end;
  end;

begin
  case cmd of
    cb_NEW_SOCK_ACCEPTED    :  _NewSockAccepted;
    cb_NEW_CLIENT_SOCK      :  _NewClientSockStart;
    cb_FINALIZE_CHANNEL     :  _FinalizeSingleChannel;
    cb_FINALIZE_CHANNEL_MGR :  _FinalizeAllChannels;
    cb_SCHED_COROUTINE      :  _RunCoRoutine;
    else
      GFRE_BT.CriticalAbort('unknown listener control command byte ' + inttostr(ord(cmd)));
  end;
end;

procedure TFRE_APSC_CHANNEL_MANAGER._FinalizeChannel(const chan: TFRE_APSC_CHANNEL);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(chan);
  assert(assigned(chan));
  APSC_WriteCommpacket(cb_FINALIZE_CHANNEL,pack,FChanBaseCtrl.SourceFD); // send to myself
end;

function TFRE_APSC_CHANNEL_MANAGER.GetID: NativeInt;
begin
  result := FChannelMgrID;
end;

function TFRE_APSC_CHANNEL_MANAGER.AddTimer(interval_ms: NativeUint): IFRE_APSC_TIMER;
var tim : TFRE_APSC_TIMER;
begin
  tim := TFRE_APSC_TIMER.Create(self,FChanBaseCtrl.FEventBase,interval_ms);
  FChanTimerList.Add(tim);
  result := tim;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.ScheduleCoRoutine(const method: TFRE_APSC_CoRoutine; const data: Pointer);
var pack:ShortString;
    m   : TMethod;
begin
  m := TMethod(method);
  SetLength(pack,3*sizeof(NativeUint));
  PPtrUInt(@pack[1])^                      := PtrUint(m.Code);
  PPtrUInt(@pack[1+SizeOf(NativeUint)])^   := PtrUint(m.Data);
  PPtrUInt(@pack[1+2*SizeOf(NativeUint)])^ := PtrUint(Data);
  APSC_WriteCommpacket(cb_SCHED_COROUTINE,pack,FChanBaseCtrl.SourceFD); // send to myself
end;

constructor TFRE_APSC_CHANNEL_MANAGER.Create(const ID: Nativeint);
begin
   FChannelMgrID := ID;
   FCMVerbose    := 'CM#'+inttostr(ID);
   FChanBaseCtrl := TFRE_APS_LL_EvBaseController.Create(@GotChannelCtrCMD,true,'C'+inttostr(id));
   FChannelList.InitSparseListPtrCmp;
   FChanTimerList.InitSparseListPtrCmp;
   inherited create(false);
end;

destructor TFRE_APSC_CHANNEL_MANAGER.Destroy;
begin
  FChanBaseCtrl.Free;
  inherited Destroy;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.Execute;
begin
  try
    GFRE_LOG.RegisterThread('CM#'+inttostr(FChannelMgrID));
    LogDebug(FCMVerbose+' STARTUP',[]);
    FChanBaseCtrl.Loop;
    LogDebug(FCMVerbose+' LOOP TERMINATED',[]);
  except on E:Exception do begin
    try
      GFRE_LOG.Log('APSCOMM CHANNEL MANAGER '+inttostr(FChannelMgrID)+' ['+e.Message+']','',fll_Emergency,'APSW',true);
      writeln(GFRE_BT.DumpExceptionsBacktrace);
    except on e:exception do begin
      GFRE_LOG.LogEmergency('LOGGING EXCEPTION : APSCOMM '+inttostr(FChannelMgrID)+' : '+e.Message);
    end;end;
  end;end;
end;

procedure TFRE_APSC_Listener.AcceptCB(const what: cshort);
var req_typ : TAPSC_EV_TYP;
    new_fd  : cint;
    sa      : TFCOM_SOCKADDRSTORAGE;
    len     : socklen_t;
begin
  req_typ := APSC_le_evtyp2TAPSC_EV_TYP(what);
  if  req_typ=[acev_READ] then
    begin
      len    := sizeof(sa);
      new_fd := fpaccept(FListensock,@sa,@len);
      if APSC_CheckResultSetError(new_fd,FError,FEcode,'ACCEPT ') then
        begin
          _InSync_Stop;
          try
            GAPSC._CallbackListener(self,als_LISTEN_ERROR);
          except on e:exception do begin
            LogWarning('ACCEPT CALLBACK FAILED :: '+e.Message,[]);
          end;end;
        end;
      GAPSC.FMainThread._InSyncDistributeNewAccept(self,new_fd,@sa,len);
      event_add(FEvent,nil);
    end
  else
    begin
      writeln('HANDLE UNEXPECTED LISTENER EVENT TYPE ',APSC_typ2string(APSC_le_evtyp2TAPSC_EV_TYP(what)));
    end;
end;

procedure TFRE_APSC_Listener.SetupEvent(const sa_data: PShortString; const event_base: PEvent_base);
var len : NativeInt;
    res : cInt;
begin
  GFRE_TF.Get_Lock(FLock);
  FState := als_BAD;
  SetLength(FID,30);
  Move(sa_data^[1],FID[1],30);
  FId:=trim(FID);
  len := Length(sa_data^)-30;
  if len<=128 then
    begin
      move(sa_data^[31],Fsockaddr,len);
      Fsockaddr_len := len;
      FListenAddr := APSC_sa2string(@Fsockaddr);
      case Fsockaddr.sin_family of
        AF_INET:
          begin
            FListensock := fpsocket(PF_INET,SOCK_STREAM,0);
          end;
        AF_INET6:
          begin
            FListensock := fpsocket(PF_INET6,SOCK_STREAM,0);
          end;
        else
          begin
            FError:='unsupported address family ('+inttostr(Fsockaddr.sin_family)+')';
            exit;
          end;
      end;
    end
  else
    begin
      FError := 'unexpected sock address len ('+inttostr(len)+')';
      exit;
    end;
  if FListensock = -1 then
    begin
      FError := APSC_TranslateOsError(fpgeterrno,'CREATE');
      exit;
    end;
  try
    if APSC_CheckResultSetError(evutil_make_listen_socket_reuseable(FListensock),FError,FEcode,'SETREUSEABLE: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(FListensock),FError,FEcode,'SETCLOSEONEX: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(FListensock),FError,FEcode,'SETNONBLOCK: ') then
      exit;
    if APSC_CheckResultSetError(fpbind(FListensock,@Fsockaddr,len),FError,FEcode,'BIND: ') then
      exit;
    if APSC_CheckResultSetError(fplisten(FListensock,10),FError,FEcode,'LISTEN: ') then
      exit;
    FEvent := event_new(event_base,FListensock,EV_READ,@INT_AcceptCB,self);
    if not assigned(FEvent) then
      begin
        FError:='did not get an event';
        exit;
      end;
  FState := als_STOPPED;
  finally
    GAPSC._CallbackListener(self,als_EVENT_NEW_LISTENER);
  end;
end;

procedure TFRE_APSC_Listener.SetupUnixSocketListener(const event_base: PEvent_base);
var len   : NativeInt;
begin
  GFRE_TF.Get_Lock(FLock);
  FState := als_BAD;
  try
    len := 128; //strlen(FsockaddrUnix.sun_path) + sizeof(FsockaddrUnix.sun_family);
    FListensock := fpsocket(PF_UNIX,SOCK_STREAM,0);

  if FListensock = -1 then
    begin
      FError := APSC_TranslateOsError(fpgeterrno,'CREATE');
      exit;
    end;
    //if APSC_CheckResultSetError(evutil_make_listen_socket_reuseable(FListensock),FError,FEcode,'SETREUSEABLE: ') then
    //  exit;
    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(FListensock),FError,FEcode,'SETCLOSEONEX: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(FListensock),FError,FEcode,'SETNONBLOCK: ') then
      exit;
    if APSC_CheckResultSetError(fpbind(FListensock,@FsockaddrUnix,len),FError,FEcode,'BIND: ') then
      exit;
    if APSC_CheckResultSetError(fplisten(FListensock,10),FError,FEcode,'LISTEN: ') then
      exit;
    FEvent := event_new(event_base,FListensock,EV_READ,@INT_AcceptCB,self);
    if not assigned(FEvent) then
      begin
        FError:='did not get an event';
        exit;
      end;
    FListenAddr := FsockaddrUnix.sun_path;
    FState := als_STOPPED;
  finally
    GAPSC._CallbackListener(self,als_EVENT_NEW_LISTENER);
  end;
end;

function TFRE_APSC_Listener.GetState: TAPSC_ListenerState;
begin
  FLock.Acquire;
  try
    result := FState;
  finally
    FLock.Release;
  end;
end;

function TFRE_APSC_Listener.GetErrorString: string;
begin
  FLock.Acquire;
  try
    result := FError;
  finally
    FLock.Release;
  end;
end;

function TFRE_APSC_Listener.GetListeningAddress: string;
begin
  result := FListenAddr;
end;

procedure TFRE_APSC_Listener._InSync_Start;
var res : cint;
begin
  if assigned(FEvent) then
    begin
      FLock.Acquire;
      res := event_add(FEvent,nil);
      try
        FState := als_LISTENING;
        LogNotice('LISTENER STARTED ON '+FListenAddr,[]);
      finally
        FLock.Release;
      end;
    end;
end;

procedure TFRE_APSC_Listener._InSync_Stop;
begin
  if assigned(FEvent) then
    begin
      event_del(FEvent);
      FLock.Acquire;
      try
        FState := als_STOPPED;
        LogInfo('LISTENER STOPPED ON '+FListenAddr,[]);
      finally
        FLock.Release;
      end;
    end;
end;

procedure TFRE_APSC_Listener._InSync_Finalize;
begin
  Free;
end;

function TFRE_APSC_Listener.IsIPListener: boolean;
begin
  Result := FsockaddrUnix.sun_path='';
end;

procedure TFRE_APSC_Listener.EnableSSL(const server_ctx: PSSL_CTX);
begin
  FSSL_Enabled := true;
  FSSL_CTX := server_ctx;
end;

procedure TFRE_APSC_Listener.Stop;
begin
  GAPSC._StopListener(self);
end;

procedure TFRE_APSC_Listener.Start;
begin
  GAPSC._ActivateListener(self);
end;

procedure TFRE_APSC_Listener.Finalize;
begin
  GAPSC._FinalizeListener(self);
end;

destructor TFRE_APSC_Listener.Destroy;
begin
  if FListensock<>-1 then
    FpClose(FListensock);
  if assigned(FEvent) then
    begin
      event_del(FEvent);
      event_free(FEvent);
    end;
  if assigned(FLock) then
    FLock.Finalize;
  inherited Destroy;
end;

function TFRE_APSC_Listener.GetID: String;
begin
  result := FId;
end;

{ TFRE_APS_LL_EvBaseController }

procedure TFRE_APS_LL_EvBaseController._CommEventfired(what: TAPSC_EV_TYP);
var rb        : NativeInt;
    size_read : NativeInt;
    lcmd      : array [0..4] of byte;
begin
  case state of
    cr_BAD: GFRE_BT.CriticalAbort('invalid state APSC basecontroller');
    cr_WAIT_LEN:
      begin
        rb := FpRead(SinkFD,lcmd,5);
        if rb<>5 then
          GFRE_BT.CriticalAbort('only read '+inttostr(rb)+' bytes but expected 4');
        cmd      := lcmd[0];
        rest_len := PInteger(@lcmd[1])^;
        state    := cr_WAITDATA;
        data_len := rest_len;
        data_pos := 0;
      end;
    cr_WAITDATA:
      begin
        size_read := FpRead(SinkFD,data[data_pos+1],rest_len);
        dec(rest_len,size_read);
        inc(data_pos,size_read);
        if rest_len=0 then
          begin
            SetLength(data,data_len);
            FOnDispatch(TAPSC_CMD(cmd),@data);
            state := cr_WAIT_LEN;
          end
        else
         state := cr_WAIT_LEN;
      end;
  end;
  //event_add(FControlEvent,nil);
end;

procedure TFRE_APS_LL_EvBaseController._TimeoutEventfired(what: TAPSC_EV_TYP);
begin
  //writeln('TIMEOUT FIRED '+fid+' ',APSC_typ2string(what));
end;

function TFRE_APS_LL_EvBaseController.SourceFD: cInt;
begin
  result := FCommPair[0];
end;

function TFRE_APS_LL_EvBaseController.SinkFD: cInt;
begin
  result := FCommPair[1];
end;

var g_cnt : qword = 0;

procedure EventCB_TFRE_APS_LL_EvBaseController(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._CommEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;

procedure EventCB_TFRE_APS_LL_EvBaseController_TO(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._TimeoutEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;


constructor TFRE_APS_LL_EvBaseController.Create(const mydispatch: TAPSC_CtrCMD; const create_dns_base: boolean; const id: string);
var res     : cInt;
begin
  FId := id;
  APSC_CheckRaise(evutil_socketpair(PF_UNIX,SOCK_STREAM,0, FCommPair),true);
  APSC_CheckRaise(evutil_make_socket_nonblocking(FCommPair[0]),true,FCommPair[0]);
  APSC_CheckRaise(evutil_make_socket_nonblocking(FCommPair[1]),true,FCommPair[1]);
  APSC_CheckRaise(evutil_make_socket_closeonexec(FCommPair[0]),true,FCommPair[0]);
  APSC_CheckRaise(evutil_make_socket_closeonexec(FCommPair[1]),true,FCommPair[1]);
  FonDispatch := mydispatch;
  FCreateDNS  := create_dns_base;

  FEventBase    := GetANewEventBase;
  if FCreateDNS then
    FDnsBase := evdns_base_new(FEventBase,1);
  FControlEvent := event_new (FEventBase,SinkFD,EV_READ+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController,self);
  FTimeoutE     := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController_TO,self);
  //FControlEvent := event_new (FEventBase,SinkFD,EV_READ,@EventCB_TFRE_APS_LL_EvBaseController,self);
  //FTimeoutE     := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT,@EventCB_TFRE_APS_LL_EvBaseController_TO,self);
  event_add(FControlEvent,nil);
  if not assigned(FControlEvent) then
    GFRE_BT.CriticalAbort('APSCL - cannot init control event');
  APSC_SetupTimeout(1000,Ftimeout);
  res := event_add(FTimeoutE,@Ftimeout);
  if not assigned(FTimeoutE) then
    GFRE_BT.CriticalAbort('APSCL - cannot init timeout event');
  state := cr_WAIT_LEN;
end;


procedure TFRE_APS_LL_EvBaseController.Loop;
var res     : cInt;
begin
  res := event_base_loop(FEventBase,0);
  if res<>0 then
    GFRE_BT.CriticalAbort('APSCL EVENT LOOP FAILED ('+inttostr(res)+')');
end;

procedure TFRE_APS_LL_EvBaseController.FinalizeLoop;
begin
  FGoDown := true;
  event_base_loopexit(FEventBase,nil);
end;


{ TFRE_APS_COMM_LISTENERS_THREADS }

function TFRE_APS_COMM_MAIN_THREAD._GetManagerMinimumConnsIDX: NativeInt;
var  i  : NativeInt;
    min : NativeInt;
begin
  min := maxint;
  for i:=0 to high(FChannelManagers) do
    begin
      if FChannelManagers[i].FChannelCount<min then
        begin
          result := i;
          min    := FChannelManagers[i].FChannelCount;
        end;
    end;
end;

function TFRE_APS_COMM_MAIN_THREAD._GetManagerIDX(const chanman: IFRE_APSC_CHANNEL_MANAGER): NativeInt;
var  i    : NativeInt;
begin
  for i:=0 to high(FChannelManagers) do
    if FChannelManagers[i] = chanman then
      exit(i);
  GFRE_BT.CriticalAbort('try to get an index for a nonexisting channel manager');
end;

procedure TFRE_APS_COMM_MAIN_THREAD.GotCtrCMD(const cmd: TAPSC_CMD; const data: PShortString);

  procedure _AddListenerSocket;
  var   nsl : TFRE_APSC_Listener;
  begin
    nsl := TFRE_APSC_Listener.Create;
    if FListenerlList.Add(nsl)=-1 then
      GFRE_BT.CriticalAbort('critical: double add listener');
    nsl.SetupEvent(data,FCB.FEventBase);
  end;

  procedure _AddListenerSocketUnix(const ls : TFRE_APSC_Listener);
  begin
    ls.SetupUnixSocketListener(FCB.FEventBase);
    if FListenerlList.Add(ls)=-1 then
      GFRE_BT.CriticalAbort('critical: double add unix listener');
  end;

  procedure _AddTimer(tim : TFRE_APSC_TIMER);
  begin
    if FTimerList.Add(tim)=-1 then
      GFRE_BT.CriticalAbort('critical: double add timer?');
    tim.SetupGlobal(FCB.FEventBase);
    tim.CallbackNewTimer;
  end;

  procedure _FinalizeListener(const listener:TFRE_APSC_Listener);
  begin
    if not FListenerlList.Delete(listener) then
      GFRE_BT.CriticalAbort('listener delete / not found');
    listener._InSync_Finalize;
  end;

begin
  case cmd of
    cb_NEW_LISTENER      : _AddListenerSocket;
    cb_NEW_LISTENER_UX   : _AddListenerSocketUnix(TFRE_APSC_Listener(PPtrUInt(@data^[1])^));
    cb_START_LISTENER    : TFRE_APSC_Listener(PPtrUInt(@data^[1])^)._InSync_Start;
    cb_STOP_LISTENER     : TFRE_APSC_Listener(PPtrUInt(@data^[1])^)._InSync_Stop;
    cb_FINALIZE_LISTENER : _FinalizeListener(TFRE_APSC_Listener(PPtrUInt(@data^[1])^));
    cb_ADD_GLOBAL_TIMER  : _AddTimer(TFRE_APSC_TIMER(PPtrUInt(@data^[1])^));
    cb_FINALIZE_MAIN     : _InSync_FinalizeMain;
    cb_FIRE_GLOBAL_TIMER : TFRE_APSC_TIMER(PPtrUInt(@data^[3])^).InternalFireTimer(PChar(@data^[1])^='T',Pchar(@data^[2])^='T');
    else
      GFRE_BT.CriticalAbort('unknown listener control command byte ' + inttostr(ord(cmd)));
  end;
end;

procedure TFRE_APS_COMM_MAIN_THREAD._InSyncDistributeNewAccept(list: TFRE_APSC_Listener; new_fd: cint; sa: PFCOM_SOCKADDRSTORAGE; len: socklen_t);
var idx  : NativeInt;
    pack : shortstring;
begin
  idx  := _GetManagerMinimumConnsIDX;
  SetLength(pack,SizeOf(PtrUInt)+sizeof(cint)+len);
  PPtrUInt(@pack[1])^ := PtrUint(list);
  pcint   (@pack[1+SizeOf(PtrUInt)])^:=new_fd;
  Move( sa^,pack[1+SizeOf(PtrUInt)+sizeof(cint)],len);
  APSC_WriteCommpacket(cb_NEW_SOCK_ACCEPTED,pack,FChannelManagers[idx].FChanBaseCtrl.SourceFD);
end;

procedure TFRE_APS_COMM_MAIN_THREAD._DistributeNewClientSock(const channel: TFRE_APSC_CHANNEL; const chanman: IFRE_APSC_CHANNEL_MANAGER);
var idx  : NativeInt;
    pack : shortstring;
begin
  if chanman=nil then
    begin
      idx  := _GetManagerMinimumConnsIDX;
    end
  else
    begin
      idx := _GetManagerIdx(chanman);
    end;
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(channel);
  APSC_WriteCommpacket(cb_NEW_CLIENT_SOCK,pack,FChannelManagers[idx].FChanBaseCtrl.SourceFD);
end;


procedure TFRE_APS_COMM_MAIN_THREAD._InSync_FinalizeMain;

  procedure FinalizeListener(var list : TFRE_APSC_Listener ; const idx : NativeInt ; var halt : boolean);
  begin
    try
      list.Free;
    except on e:Exception do
      writeln('FAILURE FREEING LISTENER '+e.Message);
    end;
  end;

  procedure FinalizeTimer(var tim : TFRE_APSC_TIMER ; const idx : NativeInt ; var halt : boolean);
  begin
    try
      tim.Free;
    except on e:Exception do
      writeln('FAILURE FREEING TIMER '+e.Message);
    end;
  end;


begin
  FCB.FinalizeLoop;
  FListenerlList.ForAllBreak(@FinalizeListener);
  FTimerList.ForAllBreak(@FinalizeTimer);
end;

constructor TFRE_APS_COMM_MAIN_THREAD.create;
var i : NativeInt;
begin
  FCB := TFRE_APS_LL_EvBaseController.Create(@GotCtrCMD,true,'M0');
  for i := 0 to C_CHANNEL_RUNNER_THREADS-1 do
    FChannelManagers[i] := TFRE_APSC_CHANNEL_MANAGER.Create(i);
  FListenerlList.InitSparseListPtrCmp;
  FTimerList.InitSparseListPtrCmp;
  inherited create(false);
end;

destructor TFRE_APS_COMM_MAIN_THREAD.Destroy;

  procedure Shutdown;
  var i : NativeInt;
  begin
    for i:=0 to high(FChannelManagers) do
      APSC_WriteCommpacket(cb_FINALIZE_CHANNEL_MGR,'',FChannelManagers[i].FChanBaseCtrl.SourceFD);
    for i:=0 to high(FChannelManagers) do
      begin
        FChannelManagers[i].WaitFor;
        FChannelManagers[i].Free;
      end;
  end;

begin
  Shutdown;
  FCB.Free;
  inherited Destroy;
end;


procedure TFRE_APS_COMM_MAIN_THREAD.Execute;
begin
  try
    GFRE_LOG.RegisterThread('ACMAIN');
    FCB.Loop;
  except on E:Exception do begin
    try
      GFRE_LOG.Log('APSCOMM M ['+e.Message+']','',fll_Emergency,'APSW',true);
      writeln('APS THREAD EXCEPTION ::: ',e.Message);
      writeln(GFRE_BT.DumpExceptionsBacktrace);
    except on e:exception do begin
      GFRE_LOG.LogEmergency('LOGGING EXCEPTION : APSCOMM M : '+e.Message);
    end;end;
  end;end;
end;

{ TFRE_APS_COMM }

procedure TFRE_APS_COMM._CallbackSignal(const signal: cint);
begin
  if assigned(FOnNew_APSC_Signal) then
    try
      FOnNew_APSC_Signal(signal);
    except on e:exception do
      LogWarning('New signal CB Exception : '+e.Message,[]);
    end
  else
    try
      RequestTerminate;
    except on e:exception do
      LogWarning('signal CB Exception : '+e.Message,[]);
    end;
end;

procedure TFRE_APS_COMM._CallbackManagerSocket(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
begin
  if assigned(FonNew_APSC_Channel) then
    try
      FonNew_APSC_Channel(channel,channel_event);
    except on e:exception do
      LogWarning('New channel CB Exception : '+e.Message,[]);
    end
  else
    try
      channel.finalize;
    except on e:exception do
      LogWarning('Channel CB Exception : '+e.Message,[]);
    end;
end;

procedure TFRE_APS_COMM._CallbackListener(const listener: TFRE_APSC_Listener; const state: TAPSC_ListenerState);
begin
  if assigned(FOnNew_APSC_Listener) then
    try
      FOnNew_APSC_Listener(listener,state);
    except on e:exception do
      LogWarning('New listener CB Exception : '+e.Message,[]);
    end
  else
  try
    listener.Finalize;
  except on e:exception do
    LogWarning('listener CB Exception : '+e.Message,[]);
  end;
end;

procedure TFRE_APS_COMM._CallbackTimer(const timer: IFRE_APSC_Timer);
begin
  if assigned(FOnNew_APSC_Timer) then
    try
      FOnNew_APSC_Timer(timer);
    except on e:exception do
      LogWarning('New timer CB Exception : '+e.Message,[]);
    end
end;

procedure TFRE_APS_COMM._ActivateListener(const listener: TFRE_APSC_Listener);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(listener);
  APSC_WriteCommpacket(cb_START_LISTENER,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM._AddUnixSockListener(const listener: TFRE_APSC_Listener);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(listener);
  APSC_WriteCommpacket(cb_NEW_LISTENER_UX,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM._StopListener(const listener: TFRE_APSC_Listener);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(listener);
  APSC_WriteCommpacket(cb_STOP_LISTENER,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM._FinalizeMain;
var pack:ShortString;
begin
  pack := '';
  APSC_WriteCommpacket(cb_FINALIZE_MAIN,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM._FinalizeListener(const listener: TFRE_APSC_Listener);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(listener);
  APSC_WriteCommpacket(cb_FINALIZE_LISTENER,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM._FireGlobalTimer(const timer: TFRE_APSC_Timer; const flag1, flag2: boolean);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint)+2);
  if flag1 then
    pack[1] := 'T'
  else
    pack[1] := 'F';
  if flag2 then
    pack[2] := 'T'
  else
    pack[2] := 'F';
  PPtrUInt(@pack[3])^ := PtrUint(timer);
  APSC_WriteCommpacket(cb_FIRE_GLOBAL_TIMER,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM.TEST_ListenerCB(const listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  err := listener.GetErrorString;
  if state =als_EVENT_NEW_LISTENER then
    begin
      TEST_Listener := listener;
      TEST_Listener.Start;
    end;
end;

procedure TFRE_APS_COMM.TEST_ListenerCB_SSL(const listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  err := listener.GetErrorString;
  writeln('LISTENER STATE ',listener.Getstate,' ',listener.GetListeningAddress,' ',state,' ',err);
  if state =als_EVENT_NEW_LISTENER then
    begin
      TEST_Listener := listener;
      TEST_Listener.EnableSSL(FSSL_CTX);
      TEST_Listener.Start;
    end;
end;

procedure TFRE_APS_COMM.TEST_ConnectManSockCB(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
begin
  if channel.CH_IsClientChannel then
    begin
      writeln('CLIENT CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID);
      channel.CH_Enable_Reading;
      channel.SetOnDisconnnect(@TEST_DiscoClientChannel);
      channel.SetOnReadData(@TEST_ReadClientChannel);
    end
  else
    begin
      writeln('CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID,' via LISTENR ',channel.GetListener.GetListeningAddress,' PARTNER=',channel.GetConnSocketAddr);
      channel.CH_WriteString('HELLO ['+channel.GetConnSocketAddr+']');
      channel.CH_Enable_Reading;
      channel.SetOnDisconnnect(@TEST_DiscoClientChannel);
      channel.SetOnReadData(@TEST_ReadClientChannel);
    end;
end;

procedure TFRE_APS_COMM.TEST_NewCLientSock2(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
begin
  writeln('CLIENT CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID,' ',channel_event);
  if channel_event=ch_NEW_CS_CONNECTED then
    begin
      channel.CH_WriteString('HELLO SERVER');
      channel.CH_Enable_Reading;
      channel.SetOnDisconnnect(@TEST_DiscoClientChannel);
      //channel.SetOnReadData(@TEST_ReadClientChannel2);
    end
  else
    begin
      channel.Finalize;
    end;
end;

procedure TFRE_APS_COMM.TEST_DiscoClientChannel(const channel: IFRE_APSC_CHANNEL);
begin
  if channel.CH_IsClientChannel then
    writeln('** CLIENT CHANNEL DISCONNECT ',channel.GetConnSocketAddr)
  else
    writeln('SERVED CHANNEL DISCONNECT ',channel.GetConnSocketAddr);
end;

procedure TFRE_APS_COMM.TEST_ReadClientChannel(const channel: IFRE_APSC_CHANNEL);
var data : string;
begin
  if channel.CH_IsClientChannel then
    begin
      writeln('GOT ON  CS: ',channel.GetVerboseDesc,' ',channel.CH_ReadString);
      channel.CH_WriteString('DONE! '+inttostr(channel.GetHandleKey));
      channel.Finalize;
    end
  else
    begin
      data := channel.CH_ReadString;
      writeln('GOT ON  SS: ',channel.GetVerboseDesc,' ',channel.CH_ReadString);
      data := 'ECHO : '+data;
      writeln(data);
      channel.CH_WriteString(data);
      //channel.Finalize;
    end;
end;

procedure TFRE_APS_COMM.TEST_ReadClientChannel2(const channel: IFRE_APSC_CHANNEL);
var reads:string;
begin
  if channel.CH_GetState=ch_ACTIVE then
    begin
      reads := channel.CH_ReadString;
      writeln(reads);
      channel.CH_WriteString('GOT FROM SERVER: '+reads);
      writeln('FINALIZING CHANNEL');
      channel.Finalize;
    end;
end;

procedure TFRE_APS_COMM.TEST_NewTimerCB(const timer: IFRE_APSC_TIMER);
begin
  if timer.TIM_GetID='TEST' then
    begin
      timer.TIM_SetInterval(100);
      timer.TIM_SetCallback(@TEST_TIMER_TIME);
      timer.TIM_Start;
    end;
end;

var GTEST_TIMER_CNT : NativeInt=0;

procedure TFRE_APS_COMM.TEST_TIMER_TIME(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
begin
  inc(GTEST_TIMER_CNT);
  writeln(TIMER.TIM_GetID,' FIRED ',flag1,' ',flag2,'  ',GTEST_TIMER_CNT);
  if GTEST_TIMER_CNT=10 then
    timer.Finalize;
end;

constructor TFRE_APS_COMM.create;
var ign,dummy: SigactionRec;
    na       : SigactionRec;

  procedure SetupSSL_Ctx;
  var fre_ssl_i     : TFRE_SSL_INFO;
  begin
    with fre_ssl_i do
      begin
        ssl_type := fssl_TLSv1;
        cerifificate_file := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_CERT_FILE; //'/fre/ssl/server_files/server_cert.pem';
        private_key_file  := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_PRIVATE_KEY_FILE; //'/fre/ssl/server_files/server_key.pem';
        root_ca_file      := cFRE_SERVER_DEFAULT_SSL_DIR+DirectorySeparator+cFRE_SSL_ROOT_CA_FILE;//'/fre/ssl/server_files/ca_cert.pem';
        fail_no_peer_cert := false;
        verify_peer       := false;
        verify_peer_cert_once:= false;
        IsServer          := true;
        cipher_suites     := 'DEFAULT';
      end;
    FSSL_CTX := FRE_Setup_SSL_Context(@fre_ssl_i);
  end;

begin
  SetupSSL_Ctx;
  FMainThread:=TFRE_APS_COMM_MAIN_THREAD.Create;

  na.sa_Handler:=SigActionHandler(@DoSig);
  na.sa_flags:=0;
  fpsigemptyset(na.sa_mask);

  //ign.sa_handler := SigActionHandler(SIG_IGN);
  //ign.sa_flags   := 0;
  //FpsigEmptySet(ign.sa_mask);

  FPsigaction(SIGPIPE, @na, @dummy);
  FPSigaction(SIGUSR1, @na, @dummy);
  FPSigaction(SIGUSR2, @na, @dummy);
  FPsigaction(SIGINT,  @na, @dummy);
  FPSigaction(SIGHUP,  @na, @dummy);
  FPSigaction(SIGTERM, @na, @dummy);
end;

destructor TFRE_APS_COMM.Destroy;
begin
  _FinalizeMain;
  FMainThread.WaitFor;
  FMainThread.Free;
  inherited Destroy;
end;

procedure TFRE_APS_COMM.RunUntilTerminate;
begin
  FMainThread.WaitFor;
end;

procedure TFRE_APS_COMM.RequestTerminate;
begin
   LogInfo('TERMINATE REQUESTED',[]);
  _FinalizeMain;
  GFRE_BT.ActivateJack(cAPSC_JACK_TIMEOUT);
end;

function TFRE_APS_COMM.AddTimer(const timer_id: ShortString; interval_ms: NativeUint; timer_callback: TFRE_APSC_TIMER_CALLBACK; local_new_timercb: TOnNew_APSC_Timer): IFRE_APSC_TIMER;
var tim  : TFRE_APSC_TIMER;
    pack : shortstring;
begin
  tim  := TFRE_APSC_TIMER.CreateGlobal(timer_id,interval_ms,local_new_timercb,timer_callback);
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(tim);
  APSC_WriteCommpacket(cb_ADD_GLOBAL_TIMER,pack,FMainThread.FCB.SourceFD);
  result := tim;
end;


procedure APSC_WriteCommpacket(const cmd : TAPSC_CMD ; data:ShortString ; const fd : cint);
var pack : ShortString;
    len  : integer;
    plen : NativeInt;
    err  : cint;
    i    : NativeInt;
begin
  plen:= Length(data);
  if plen=0 then
    begin
      data :='*';
      plen :=1;
    end;
  pack                 := Char(cmd)+#0#0#0#0+data;
  PCardinal(@pack[2])^ := plen;
  for i:=0 to 10 do
    begin
      len := FpWrite(fd,pack[1],Length(pack));
      if len=-1 then
        begin
          err := fpgeterrno;
          if err=35 then
            begin
              sleep(100);
              continue
            end;
        end
      else
        break;
    end;
  if len<>Length(pack) then
    raise exception.Create('failed to send comm packet '+inttostr(len)+'/'+inttostr(length(pack))+' '+APSC_TranslateOsError(err,'',''));
end;

procedure TFRE_APS_COMM.AddListener_TCP(Bind_IP, Bind_Port: String; const ID: ShortString);
var IP4Only : Boolean;
    IP6Only : boolean;
    sa      : TFCOM_SOCKADDRSTORAGE;
    len     : cInt;
    res     : cInt;
    parse   : string;
    pack    : shortstring;
begin
  if (Bind_IP='')
     or (Bind_Port='') then
       raise exception.create('neither ip nor port can be empty, use something like *, *4, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
  if Bind_IP[1]='*' then
    Bind_IP := '0.0.0.0';
  if Bind_IP[2]='4' then
    Ip4Only := true;
  if Bind_IP[2]='6' then
    Ip6Only := true;
  len := sizeof(sa);
  parse := Bind_IP+':'+Bind_Port;
  res := evutil_parse_sockaddr_port(Pchar(parse),@sa,len);
  if res<>0 then
    raise exception.Create('could not parse given address and port, use for the ip something like *, *4, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');

  pack := Copy(ID+StringOfChar(' ',30),1,30);
  SetLength(pack,len+30);
  move(sa,pack[30+1],len);
  APSC_WriteCommpacket(cb_NEW_LISTENER,pack,FMainThread.FCB.SourceFD);
end;

procedure TFRE_APS_COMM.AddListener_UX(const special_file: shortstring; const id: shortstring);
var uxl : TFRE_APSC_Listener;
begin
  if Length(special_file)>=108 then
    raise Exception.Create('the unix socket path has to be shorter then 108 chars');
  if FileExists(special_file) then
    APSC_CheckRaiseOSCall(FpUnlink(special_file),'unlink unix socket','',false);
  uxl:=TFRE_APSC_Listener.Create;
  uxl.FId := id;
  uxl.FsockaddrUnix.sun_path   := trim(special_file);
  uxl.FsockaddrUnix.sun_path[Length(special_file)]:=#0;
  uxl.FsockaddrUnix.sun_family := PF_UNIX;
  _AddUnixSockListener(uxl);
end;

procedure TFRE_APS_COMM.AddClient_TCP(IP, Port: String; const ID: ShortString; const channelmanager: IFRE_APSC_CHANNEL_MANAGER; localNewChannelCB: TOnNew_APSC_Channel; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT; Bind_IP: string; Bind_Port: String);
var IP4Only : Boolean;
    IP6Only : boolean;
    sa      : TFCOM_SOCKADDRSTORAGE;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    len     : cInt;
    bindlen : cInt;
    res     : cInt;
    parse   : string;
    pack    : shortstring;
    chan    : TFRE_APSC_CHANNEL;
begin
  if (IP='')
     or (Port='') then
       raise exception.create('neither ip nor port can be empty, use something like *, *4, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
  len := sizeof(sa);
  parse := IP+':'+Port;
  res := evutil_parse_sockaddr_port(Pchar(parse),@sa,len);
  if res<>0 then
    raise exception.Create('could not parse given IP address and port, use for the ip something like *, *4, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');

  bindlen := 0;
  if Bind_ip<>'' then
    begin
      parse := Bind_IP+':'+Bind_Port;
      res := evutil_parse_sockaddr_port(Pchar(parse),@sabind,bindlen);
      if res<>0 then
        raise exception.Create('could not parse given bind address and port, use for the ip something like 127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
    end;

  chan := TFRE_APSC_CHANNEL.Create(nil,nil);

  if not assigned(localNewChannelCB) then
    localNewChannelCB := @_CallbackManagerSocket;

  chan.SetupClientSocketTCP(@sa,len,@sabind,bindlen,id,localNewChannelCB,localRead,localDisconnect);

  FMainThread._DistributeNewClientSock(chan,channelmanager);
end;

procedure TFRE_APS_COMM.AddClient_TCP_DNS(Host, Port: String; const ID: ShortString; const channelmanager: IFRE_APSC_CHANNEL_MANAGER; localNewChannelCB: TOnNew_APSC_Channel; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT; Bind_IP: string; Bind_Port: String);
var chan    : TFRE_APSC_CHANNEL;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    bindlen : cInt;
begin
  chan := TFRE_APSC_CHANNEL.Create(nil,nil);
  if not assigned(localNewChannelCB) then
    localNewChannelCB := @_CallbackManagerSocket;
  chan.SetupClientSocketDNS(Host,strtoint(port),@sabind,bindlen,id,localNewChannelCB,localRead,localDisconnect);
  FMainThread._DistributeNewClientSock(chan,channelmanager);
end;

procedure TFRE_APS_COMM.AddClient_UX(const special_file: shortstring; const ID: Shortstring; const channelmanager: IFRE_APSC_CHANNEL_MANAGER; localNewChannelCB: TOnNew_APSC_Channel; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT);
var chan    : TFRE_APSC_CHANNEL;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    bindlen : cInt;
begin
  chan := TFRE_APSC_CHANNEL.Create(nil,nil);
  try
    if not assigned(localNewChannelCB) then
      localNewChannelCB := @_CallbackManagerSocket;
    chan.SetupClientSocketUX(special_file,id,localNewChannelCB,localRead,localDisconnect);
  except
    chan.free;
    raise;
  end;
  FMainThread._DistributeNewClientSock(chan,channelmanager);
end;

//procedure callb(dns_result : cint ; typ : cchar ; count : cint ; ttl : cint ;  addresses : pointer ; arg : pointer);cdecl;
procedure callb(dns_result : cint ; res : Pevutil_addrinfo ; arg : Pointer);cdecl;
var i    : integer;
    buf  : array [0..127] of char;
    in_a : PInAddr;
    in_6 : PIn6Addr;
    s    : PChar;
begin
  i := 0;


  while assigned(res) do
    begin
      if res^.ai_family = AF_INET then
        begin
          in_a := @PFCOM_SockAddrIn(res^.ai_addr)^.sin_addr;
          s := evutil_inet_ntop(res^.ai_family,in_a,buf,128);
          writeln(i,' DNSR ',dns_result,' ',res^.ai_canonname,' * ',s);
        end
      else
      if res^.ai_family = AF_INET6 then
        begin
          in_6 := @PFCOM_SockAddrIn6(res^.ai_addr)^.sin6_addr;
          s := evutil_inet_ntop(res^.ai_family,in_6,buf,128);
          writeln(i,' DNSR ',dns_result,' ',res^.ai_canonname,' * ',s);
        end;
        res := res^.ai_next;
        inc(i);
    end;
end;

procedure TFRE_APS_COMM.ResolveDNS_TCP(const addrstring: String);
var hints : evutil_addrinfo;
    req   : Pevdns_getaddrinfo_request;
begin
  FillByte(hints,sizeof(hints),0);
  hints.ai_family   := AF_UNSPEC;
  hints.ai_flags    := AI_CANONNAME;
  hints.ai_socktype := SOCK_STREAM;
  hints.ai_protocol := IPPROTO_TCP;
  evdns_getaddrinfo(FMainThread.FCB.FDnsBase,pchar(addrstring),nil,@hints,@callb,self);
end;

procedure TFRE_APS_COMM.SetNewListenerCB(const lcb: TOnNew_APSC_Listener);
begin
  FOnNew_APSC_Listener := lcb;
end;

procedure TFRE_APS_COMM.SetNewChannelCB(const chancb: TOnNew_APSC_Channel);
begin
  FonNew_APSC_Channel:=chancb;
end;

procedure TFRE_APS_COMM.SetNewTimerCB(const timercb: TOnNew_APSC_Timer);
begin
  FOnNew_APSC_Timer := timercb;
end;

procedure TFRE_APS_COMM.SetSingnalCB(const signalcb: TOnNew_APSC_Signal);
begin
  FOnNew_APSC_Signal := signalcb;
end;


end.
