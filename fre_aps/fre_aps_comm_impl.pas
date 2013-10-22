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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_INTERLOCKED,FRE_FCOM_SSL,fos_fcom_handles,
  FRE_LIBEVENT_CORE,sockets,errors,fre_system,fre_db_interface,fos_sparelistgen
  {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  {$IFDEF windows}
  ,windows
  {$ENDIF}
  ;

const C_CHANNEL_RUNNER_THREADS = 4;

procedure Setup_APS_Comm;
procedure Teardown_APS_Comm;

procedure Test_APSC;

implementation

type
  TAPSC_CMD  = (cb_NEW_LISTENER=1,cb_START_LISTENER=2,cb_STOP_LISTENER=3,cb_FINALIZE_LISTENER=4,cb_NEW_SOCK_ACCEPTED=5,cb_FINALIZE_MAIN=6,cb_FINALIZE_CHANNEL_MGR=7,cb_FINALIZE_CHANNEL=8);

function  APSC_CheckResultSetError (const res : cInt ; var error : string ; const prefix : string='' ; const postfix : string =''):boolean;forward;
procedure APSC_WriteCommpacket     (const cmd : TAPSC_CMD ; data:ShortString ; const fd : cint);forward;

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
    FEvent        : PEvent;
    FLock         : IFOS_LOCK;
  protected
    procedure  SetupEvent          (const sa_data : PShortString ; const event_base : PEvent_base);
    function   GetState            : TAPSC_ListenerState;
    function   GetErrorString      : string;
    function   GetListeningAddress : string;
    procedure  _InSync_Start;
    procedure  _InSync_Stop;
    procedure  _InSync_Finalize;
  public
    procedure  Stop;
    procedure  Start;
    procedure  Finalize; // called over manager
    destructor Destroy;override;
  end;

  { TFRE_APS_COMM_MAIN_THREAD }
  TFRE_APSC_CHANNEL_MANAGER = class;

  OFOS_SL_TFRE_APSC_Listener = specialize OFOS_SpareList<TFRE_APSC_LISTENER>;

  TFRE_APS_COMM_MAIN_THREAD = class(TThread) // Manages all Listeners / main sync base
  private
    FCB               : TFRE_APS_LL_EvBaseController;
    FChannelManagers  : Array [0..C_CHANNEL_RUNNER_THREADS-1] of TFRE_APSC_CHANNEL_MANAGER;
    FListenerlList    : OFOS_SL_TFRE_APSC_Listener;
    function    _GetManagerMinimumConnsIDX : NativeInt;
    procedure   GotCtrCMD (const cmd : TAPSC_CMD ; const data : PShortString);
    procedure   _InSyncDistributeNewAccept(list : TFRE_APSC_Listener ; new_fd : cint; sa : PFCOM_SOCKADDRSTORAGE ; len : socklen_t);
    procedure   _InSync_FinalizeMain;
  public
    constructor Create   ;
    destructor  Destroy  ; override;
    procedure   Execute  ; override;
  end;

  TFRE_APS_COMM_DNS_ANSWER=class
  end;


  { TFRE_APSC_CHANNEL }
  PFRE_APSC_CHANNEL = ^TFRE_APSC_CHANNEL;

  TFRE_APSC_CHANNEL=class(TObject,IFRE_APSC_CHANNEL)
  private
    FState        : TAPSC_ChannelState;
    Fsockaddr     : TFCOM_SOCKADDRSTORAGE;
    Fsockaddr_len : cInt;
    FSocketAddr   : string;
    Fsocket       : cInt;
    FError        : String;
    FBufEvent     : PBufferevent;
    FLock         : IFOS_LOCK;
    FManager      : TFRE_APSC_CHANNEL_MANAGER;
    FListener     : TFRE_APSC_Listener;
    FInputBuf     : PEvbuffer;
    FOutputBuf    : PEvbuffer;
    FInBufCB      : Pevbuffer_cb_entry;
    FOutBufCB     : Pevbuffer_cb_entry;
    FTotalInRead  : NativeUint;
    FTotalOutWrite: NativeUint;
    procedure  ReadDataEvent;
    procedure  WriteDataEvent;
    procedure  InputBufferEvent  (const bufinfo : Pevbuffer_cb_info);
    procedure  OutputBufferEvent (const bufinfo : Pevbuffer_cb_info);
    procedure  GenericEvent(what : cShort);
    procedure  _InSync_Finalize;
  protected
    procedure   SetupServedSocket (fd : cint ; new_sa : PFCOM_SOCKADDRSTORAGE ; new_sal : cInt ; const base : PEvent_base); // a server socket
  public
    function    GetChannelManager   : IFRE_APSC_CHANNEL_MANAGER;
    function    GetListener         : IFRE_APSC_LISTENER;
    function    GetConnSocketAddr   : String;

    procedure   CH_WriteString    (const str : String);
    procedure   Enable_Reading    ;
    procedure   Enable_Writing    ;

    constructor Create  (manager : TFRE_APSC_CHANNEL_MANAGER ; listener : TFRE_APSC_LISTENER);
    destructor  Destroy;override;
    procedure   Finalize; // called from IF - need to go over manager ...
  end;

  OFOS_SL_TFRE_APS_CHANNEL  = specialize OFOS_SpareList<TFRE_APSC_CHANNEL>;

  { TFRE_APSC_CHANNEL_MANAGER }
  TFRE_APSC_CHANNEL_MANAGER = class(TThread,IFRE_APSC_CHANNEL_MANAGER)
  private
    FChannelMgrID : NativeInt;
    FChanBaseCtrl : TFRE_APS_LL_EvBaseController;
    FChannelList  : OFOS_SL_TFRE_APS_CHANNEL;
    FChannelCount : NativeInt;
    procedure   GotChannelCtrCMD (const cmd  : TAPSC_CMD ; const data : PShortString);
    procedure   _FinalizeChannel (const chan : TFRE_APSC_CHANNEL);
  public
    function GetID       : NativeInt;

    constructor Create   (const ID :Nativeint);
    destructor  Destroy  ; override;
    procedure   Execute  ; override;
  end;


  { TFRE_APS_COMM }
  TFRE_APS_COMM=class(Tobject,IFRE_APSC)
  private
    FMainThread          : TFRE_APS_COMM_MAIN_THREAD;
    FOnNew_APSC_Listener : TOnNew_APSC_Listener;
    FonNew_APSC_Channel  : TOnNew_APSC_Channel;
    TEST_Listener        : IFRE_APSC_LISTENER;
    procedure _CallbackManagerSocket (const channel : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure _CallbackListener      (const listener : TFRE_APSC_Listener ; const state : TAPSC_ListenerState);
    procedure _ActivateListener      (const listener : TFRE_APSC_Listener);
    procedure _StopListener          (const listener : TFRE_APSC_Listener);
    procedure _FinalizeMain          ;
    procedure _FinalizeListener      (const listener : TFRE_APSC_Listener);
  protected

    procedure   TEST_ListenerCB       (const listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure   TEST_ConnectManSockCB (const channel : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
  public
    constructor create;
    destructor  Destroy; override;
    procedure   AddListener_TCP  (Bind_IP, Bind_Port:String);
    procedure   ResolveDNS_TCP   (const addrstring : String);
    procedure   SetNewListenerCB (const lcb : TOnNew_APSC_Listener);
    procedure   SetNewChannelCB  (const chancb : TOnNew_APSC_Channel);
  end;

var GAPSC : TFRE_APS_COMM;

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

procedure Test_APSC;
begin
  GFRE_SC.SetNewListenerCB(@GAPSC.TEST_ListenerCB);
  GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);

  GFRE_SC.AddListener_TCP('[::1]','44000');
  GFRE_SC.AddListener_TCP('[fd9e:21a7:a92c:2323::1]','44000');
  GFRE_SC.AddListener_TCP('*','44000');
  repeat
   sleep(1);
  until assigneD(GAPSC.TEST_Listener);
  writeln('ASYNC : ',GAPSC.TEST_Listener.GetState);
  ////AddListener_TCP('www.orf.at',44000);
  ////AddListener_TCP('0.0.0.0','44000');
  ////AddListener_TCP('[fe80::ca2a:14ff:fe14]','44010');
  ////AddListener_TCP('0.0.0.0','44000');
  //AddListener_TCP('[::1]','44000');
end;

procedure   LogInfo(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Info,'APSCOMM');
end;

procedure   LogWarning           (const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Warning,'APSCOMM');
end;

function APSC_TranslateOsError(const os_error: cint; const prefix: string=''; postfix: string=''): string;
begin
  result := prefix+StrError(os_error)+postfix;
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
  cfg      := event_config_new;
  event_config_require_features(cfg, EV_FEATURE_O1);
  result  := event_base_new_with_config(cfg);
  event_config_free(cfg);
  if not assigned(result) then begin
    LogWarning('Cannot get features O1, using fallback',[]);
    result:= event_base_new;
    if not assigned(result) then
      GFRE_BT.CriticalAbort('COULD NOT AQUIRE ANY EVENTBASE!');
  end;
  //feat := event_base_get_features(FLE_Base);
  //if (feat and EV_FEATURE_ET )>0 then  Include(FLE_Features,evf_EDGE_TRIGGERED);
  //if (feat and EV_FEATURE_O1 )>0 then  Include(FLE_Features,evf_O1);
  //if (feat and EV_FEATURE_FDS)>0 then  Include(FLE_Features,evf_FILE_DESCRIPTORS);
  //writeln('');LogInfo('Eventbase Method : [%s %s  %s  %s ]',[string(event_base_get_method(FLE_Base)),BoolToStr(evf_EDGE_TRIGGERED in FLE_Features,'EDGE TRIGGERED','NOT EDGE TRIGGERED'),
end;

{ TFRE_APSC_Listener }

function APSC_CheckResultSetError(const res : cInt ; var error : string ; const prefix : string='' ; const postfix : string =''):boolean;
begin
  if res<0 then
    begin
      error  := APSC_TranslateOsError(fpgeterrno,prefix,postfix);
      result := true;
      exit;
    end;
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

procedure TFRE_APSC_CHANNEL.ReadDataEvent;
var pev :PEvbuffer;
begin
  writeln('READ ON SOCK ',Fsocket);
  pev := bufferevent_get_input(FBufEvent);
  if assigned(pev) then
    begin
      writeln('EVB :',evbuffer_get_length(pev));
    end;
end;

procedure TFRE_APSC_CHANNEL.WriteDataEvent;
begin
  writeln('WRITE ON SOCK ',Fsocket);
end;

procedure TFRE_APSC_CHANNEL.InputBufferEvent(const bufinfo: Pevbuffer_cb_info);
begin
  writeln('IN BUFER CB ',bufinfo^.n_added,' ',bufinfo^.n_deleted,' ',bufinfo^.orig_size);
  inc(FTotalInRead,bufinfo^.n_added);
end;

procedure TFRE_APSC_CHANNEL.OutputBufferEvent(const bufinfo: Pevbuffer_cb_info);
begin
  writeln('OUT BUFER CB ',bufinfo^.n_added,' ',bufinfo^.n_deleted,' ',bufinfo^.orig_size);
  inc(FTotalInRead,bufinfo^.n_deleted);
end;

procedure TFRE_APSC_CHANNEL.GenericEvent(what: cShort);
begin
  writeln('GENERIC ON SOCK ',Fsocket,' ',APSC_typ2string(APSC_le_evtyp2TAPSC_EV_TYP(what)),' ',what);
  if (what and BEV_EVENT_EOF)>0 then
    begin
      CH_WriteString('DOWN-!');
      bufferevent_flush(FBufEvent,EV_WRITE,BEV_FLUSH);
      Finalize;
    end;
end;

procedure TFRE_APSC_CHANNEL._InSync_Finalize;
begin
  Free;
end;

procedure loc_buffer_cb_in(buffer : PEvbuffer ; info : Pevbuffer_cb_info ; arg : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(arg).InputBufferEvent(info);
end;

procedure loc_buffer_cb_out(buffer : PEvbuffer ; info : Pevbuffer_cb_info ; arg : Pointer); cdecl;
begin
  TFRE_APSC_CHANNEL(arg).OutputBufferEvent(info);
end;


procedure TFRE_APSC_CHANNEL.SetupServedSocket(fd: cint; new_sa: PFCOM_SOCKADDRSTORAGE; new_sal: cInt; const base: PEvent_base);
begin
  move(new_sa^,Fsockaddr,new_sal);
  Fsockaddr_len := new_sal;
  Fsocket       := fd;
  try
    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(Fsocket),FError,'SETCLOSEONEX: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(Fsocket),FError,'SETNONBLOCK: ') then
      exit;
    FSocketAddr := APSC_sa2string(@Fsockaddr);
    FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
    if not assigned(FBufEvent) then
      begin
        FError:='did not get a bufferevent';
        exit;
      end;
  bufferevent_setcb(FBufEvent,@bufev_read,@bufev_write,@bufev_event,self);
  FInputBuf  := bufferevent_get_input(FBufEvent);
  FOutputBuf := bufferevent_get_output(FBufEvent);
  FInBufCB   := evbuffer_add_cb(FInputBuf,@loc_buffer_cb_in,self);
  FOutBufCB  := evbuffer_add_cb(FOutputBuf,@loc_buffer_cb_out,self);
  FState := ch_STOPPED;
  finally
    GAPSC._CallbackManagerSocket(self,ch_NEW_SS_CONNECTED);
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

procedure TFRE_APSC_CHANNEL.CH_WriteString(const str: String);
begin
  if bufferevent_write(FBufEvent,@str[1],Length(str))<>0 then
    GFRE_BT.CriticalAbort('critical: channel write failed ?');
end;

procedure TFRE_APSC_CHANNEL.Enable_Reading;
var what : cSHort;
begin
  what := bufferevent_get_enabled(FBufEvent);
  what := what or EV_READ;
  bufferevent_enable(FBufEvent, what);
end;

procedure TFRE_APSC_CHANNEL.Enable_Writing;
var what : cSHort;
begin
  what := bufferevent_get_enabled(FBufEvent);
  what := what or EV_WRITE;
  bufferevent_enable(FBufEvent, what);
end;

constructor TFRE_APSC_CHANNEL.Create(manager: TFRE_APSC_CHANNEL_MANAGER; listener: TFRE_APSC_LISTENER);
begin
  GFRE_TF.Get_Lock(FLock);
  FState    := ch_BAD;
  FManager  := manager;
  FListener := listener;
end;

destructor TFRE_APSC_CHANNEL.Destroy;
begin
  bufferevent_disable(FBufEvent,EV_READ+EV_WRITE);
  if assigned(FBufEvent) then
    bufferevent_free(FBufEvent);
  FLock.Finalize;
  writeln('CHANNEL ',Fsocket,' FREED');
  inherited;
end;

procedure TFRE_APSC_CHANNEL.Finalize;
begin
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
//    SizeOf(PtrUInt)+sizeof(cint)+len
    list      := TFRE_APSC_Listener(PPtrUInt(@data^[1])^);
    newchan   := TFRE_APSC_CHANNEL.Create(self,list);
    new_fd    := pcint(@data^[1+sizeof(PtrUInt)])^;
    new_sa    := @data^[1+sizeof(PtrUInt)+SizeOf(cint)];
    new_sal   := Length(data^)-SizeOf(cint);
    newchan.SetupServedSocket(new_fd,new_sa,new_sal,FChanBaseCtrl.FEventBase);
    if not FChannelList.Add(newchan) then
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
      chan.Free;
      chan := nil;
    end;
  begin
    writeln('FINALIZE CHANNEL ON MGR ',FChannelMgrID);
    FChannelList.ForAllBreak(@FinalizeChans);
    FChanBaseCtrl.FinalizeLoop;
  end;

begin
  case cmd of
    cb_NEW_SOCK_ACCEPTED    :  _NewSockAccepted;
    cb_FINALIZE_CHANNEL     :  _FinalizeSingleChannel;
    cb_FINALIZE_CHANNEL_MGR :  _FinalizeAllChannels;
    else
      GFRE_BT.CriticalAbort('unknown listener control command byte ' + inttostr(ord(cmd)));
  end;
end;

procedure TFRE_APSC_CHANNEL_MANAGER._FinalizeChannel(const chan: TFRE_APSC_CHANNEL);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(chan);
  APSC_WriteCommpacket(cb_FINALIZE_CHANNEL,pack,FChanBaseCtrl.SourceFD); // send to myself
end;

function TFRE_APSC_CHANNEL_MANAGER.GetID: NativeInt;
begin
  result := FChannelMgrID;
end;

function SL_ChannelNull(const chan : PFRE_APSC_CHANNEL):boolean;
begin
  result := not assigned(chan^);
end;

function SL_ChannelCompare(const chan1,chan2 : PFRE_APSC_CHANNEL):boolean;
begin
  result := chan1^=chan2^;
end;

constructor TFRE_APSC_CHANNEL_MANAGER.Create(const ID: Nativeint);
begin
   FChannelMgrID := ID;
   FChanBaseCtrl := TFRE_APS_LL_EvBaseController.Create(@GotChannelCtrCMD,false,'C'+inttostr(id));
   FChannelList.InitSparseList(nil,@SL_ChannelNull,@SL_ChannelCompare);
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
    FChanBaseCtrl.Loop;
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
  writeln('ACCEPT ON LISTENER ',what);
  req_typ := APSC_le_evtyp2TAPSC_EV_TYP(what);
  if  req_typ=[acev_READ] then
    begin
      len    := sizeof(sa);
      new_fd := fpaccept(FListensock,@sa,@len);
      if APSC_CheckResultSetError(new_fd,FError,'ACCEPT ') then
        begin
          _InSync_Stop;
          try
            GAPSC._CallbackListener(self,als_LISTEN_ERROR);
          except on e:exception do begin
            LogWarning('ACCEPT CALLBACK FAILED :: '+e.Message,[]);
          end;end;
        end;
      GAPSC.FMainThread._InSyncDistributeNewAccept(self,new_fd,@sa,len);
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
  len := Length(sa_data^);
  if len<=128 then
    begin
      move(sa_data^[1],Fsockaddr,len);
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
    if APSC_CheckResultSetError(evutil_make_listen_socket_reuseable(FListensock),FError,'SETREUSEABLE: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(FListensock),FError,'SETCLOSEONEX: ') then
      exit;
    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(FListensock),FError,'SETNONBLOCK: ') then
      exit;
    if APSC_CheckResultSetError(fpbind(FListensock,@Fsockaddr,len),FError,'BIND: ') then
      exit;
    if APSC_CheckResultSetError(fplisten(FListensock,10),FError,'LISTEN: ') then
      exit;
    FEvent := event_new(event_base,FListensock,EV_READ+EV_PERSIST,@INT_AcceptCB,self);
    if not assigned(FEvent) then
      begin
        FError:='did not get an event';
        exit;
      end;
  FState := als_STOPPED;
  finally
    GAPSC._CallbackListener(self,als_NEW_LISTENER);
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
      res := event_add(FEvent,nil);
      FLock.Acquire;
      try
        FState := als_LISTENING;
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
      finally
        FLock.Release;
      end;
    end;
end;

procedure TFRE_APSC_Listener._InSync_Finalize;
begin
  Free;
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
  FLock.Finalize;
  inherited Destroy;
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

constructor TFRE_APS_LL_EvBaseController.Create(const mydispatch: TAPSC_CtrCMD; const create_dns_base: boolean; const id: string);
begin
  FId := id;
  APSC_CheckRaise(evutil_socketpair(PF_LOCAL,SOCK_STREAM,0, FCommPair),true);
  //APSC_CheckRaise(evutil_make_socket_nonblocking(FCommPair[0]),true,FCommPair[0]);
  APSC_CheckRaise(evutil_make_socket_nonblocking(FCommPair[1]),true,FCommPair[1]);
  APSC_CheckRaise(evutil_make_socket_closeonexec(FCommPair[0]),true,FCommPair[0]);
  APSC_CheckRaise(evutil_make_socket_closeonexec(FCommPair[1]),true,FCommPair[1]);
  FonDispatch := mydispatch;
  FCreateDNS  := create_dns_base;
end;

procedure EventCB_TFRE_APS_LL_EvBaseController(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._CommEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;

procedure EventCB_TFRE_APS_LL_EvBaseController_TO(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._TimeoutEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;

procedure TFRE_APS_LL_EvBaseController.Loop;
var res     : cInt;
    timeout : TFCOM_TimeVal;
begin
  FEventBase    := GetANewEventBase;
  if FCreateDNS then
    FDnsBase := evdns_base_new(FEventBase,1);
  FControlEvent := event_new (FEventBase,SinkFD,EV_READ+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController,self);
  FTimeoutE     := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController_TO,self);
  res := event_add(FControlEvent,nil);
  if not assigned(FControlEvent) then
    GFRE_BT.CriticalAbort('APSCL - cannot init control event');
  APSC_SetupTimeout(1000,timeout);
  res := event_add(FTimeoutE,@timeout);
  if not assigned(FTimeoutE) then
    GFRE_BT.CriticalAbort('APSCL - cannot init timeout event');
  state := cr_WAIT_LEN;
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

procedure TFRE_APS_COMM_MAIN_THREAD.GotCtrCMD(const cmd: TAPSC_CMD; const data: PShortString);

  procedure _AddListenerSocket;
  var sa  : TFCOM_SOCKADDRSTORAGE;
      len : NativeInt;
      nsl : TFRE_APSC_Listener;
  begin
    nsl := TFRE_APSC_Listener.Create;
    nsl.SetupEvent(data,FCB.FEventBase);
    if not FListenerlList.Add(nsl) then
      GFRE_BT.CriticalAbort('critical: double add listener');
  end;

begin
  case cmd of
    cb_NEW_LISTENER      : _AddListenerSocket;
    cb_START_LISTENER    : TFRE_APSC_Listener(PPtrUInt(@data^[1])^)._InSync_Start;
    cb_STOP_LISTENER     : TFRE_APSC_Listener(PPtrUInt(@data^[1])^)._InSync_Stop;
    cb_FINALIZE_LISTENER : TFRE_APSC_Listener(PPtrUInt(@data^[1])^)._InSync_Finalize;
    cb_FINALIZE_MAIN     : _InSync_FinalizeMain;
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

procedure TFRE_APS_COMM_MAIN_THREAD._InSync_FinalizeMain;

  procedure FinalizeListener(var list : TFRE_APSC_Listener ; const idx : NativeInt ; var halt : boolean);
  begin
    list.Free;
  end;

begin
  FCB.FinalizeLoop;
  FListenerlList.ForAllBreak(@FinalizeListener);
end;

function SL_Listenernull (const listener : PFRE_APSC_Listener):boolean;
begin
  result := not assigned(listener^);
end;

function SL_Listenercompare (const l1,l2 : PFRE_APSC_Listener):boolean;
begin
  result := l1^=l2^;
end;

constructor TFRE_APS_COMM_MAIN_THREAD.create;
var i : NativeInt;
begin
  FCB := TFRE_APS_LL_EvBaseController.Create(@GotCtrCMD,true,'M0');
  for i := 0 to C_CHANNEL_RUNNER_THREADS-1 do
    FChannelManagers[i] := TFRE_APSC_CHANNEL_MANAGER.Create(i);
  FListenerlList.InitSparseList(nil,@SL_Listenernull,@SL_Listenercompare);
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
    listener.free;
  except on e:exception do
    LogWarning('listener CB Exception : '+e.Message,[]);
  end;
end;

procedure TFRE_APS_COMM._ActivateListener(const listener: TFRE_APSC_Listener);
var pack:ShortString;
begin
  SetLength(pack,sizeof(NativeUint));
  PPtrUInt(@pack[1])^ := PtrUint(listener);
  APSC_WriteCommpacket(cb_START_LISTENER,pack,FMainThread.FCB.SourceFD);
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

procedure TFRE_APS_COMM.TEST_ListenerCB(const listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  err := listener.GetErrorString;
  writeln('LISTENER STATE ',listener.Getstate,' ',listener.GetListeningAddress,' ',state,' ',err);
  if state =als_NEW_LISTENER then
    begin
      TEST_Listener := listener;
      TEST_Listener.Start;
    end;
  //writeln('LISTENER STATE ',listener.Getstate,' ',listener.GetListeningAddress);
  //listener.finalize;
end;

procedure TFRE_APS_COMM.TEST_ConnectManSockCB(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
begin
  writeln('CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID,' via LISTENR ',channel.GetListener.GetListeningAddress,' PARTNER=',channel.GetConnSocketAddr);
  channel.CH_WriteString('HELLO ['+channel.GetConnSocketAddr+']');
  channel.Enable_Reading;
end;

constructor TFRE_APS_COMM.create;
 begin
  FMainThread:=TFRE_APS_COMM_MAIN_THREAD.Create;
end;

destructor TFRE_APS_COMM.Destroy;
begin
  _FinalizeMain;
  FMainThread.WaitFor;
  FMainThread.Free;
  inherited Destroy;
end;

procedure APSC_WriteCommpacket(const cmd : TAPSC_CMD ; data:ShortString ; const fd : cint);
var pack : ShortString;
    len  : integer;
    plen : NativeInt;
    err  : cint;
begin
  plen:= Length(data);
  if plen=0 then
    begin
      data :='*';
      plen :=1;
    end;
  pack                 := Char(cmd)+#0#0#0#0+data;
  PCardinal(@pack[2])^ := plen;
  len := FpWrite(fd,pack[1],Length(pack));
  if len=-1 then
    begin
      err := fpgeterrno;
      writeln('ERR: '+APSC_TranslateOsError(err,'',''));
    end
  else
    if len<>Length(pack) then
      raise exception.Create('failed to send comm packet '+inttostr(len)+'/'+inttostr(length(pack)));
end;

procedure TFRE_APS_COMM.AddListener_TCP(Bind_IP, Bind_Port: String);
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
  SetLength(pack,len);
  move(sa,pack[1],len);
  APSC_WriteCommpacket(cb_NEW_LISTENER,pack,FMainThread.FCB.SourceFD);
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


end.
