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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_INTERLOCKED,FRE_FCOM_SSL,contnrs,
  FRE_LIBEVENT_CORE,errors,fre_system,fre_db_interface,fos_sparelistgen,fre_http_client,math
  {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  //{$IFDEF windows}
  //,windows
  //{$ENDIF}
  ;


{$DEFINE APS_LOG_ERROR}
{.$DEFINE APS_LOG_NOTICE}
{.$DEFINE APS_LOG_DEBUG}
{.$DEFINE APS_LOG_WARNING}
{.$DEFINE APS_LOG_INFO}
{$DEFINE DEBUG_WL}

{
  TODO:
   * Kill defered eventing (not implemented), do it with microstate(interlocked) methods
     no eventing when the ev base is working, just interlocked increment/cas a new work indicator

}

var C_CHANNEL_RUNNER_THREADS : integer = 4;

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

  TAPSC_EV_TYPES = (acev_READ,acev_WRITE,acev_TIMEOUT,acev_SIGNAL,acev_GO_DOWN);
  TAPSC_EV_TYP   = set of TAPSC_EV_TYPES;



  TFRE_APSC_BaseObject          = class;
  TFRE_APSC_Listener            = class;
  TFRE_APSC_CHANNEL             = class;
  TFRE_APSC_TIMER               = class;
  TFRE_APSC_CHANNELGROUP_THREAD = class;
  TFRE_APSC_CTX_THREAD          = class;
  TFRE_APS_CMD_BASE_CLASS       = class of TFRE_APS_CMD_BASE;

  TFRE_APSC_CTX_Callback        = procedure (const ctx : TFRE_APSC_CTX_THREAD) is nested;

  TFRE_APSC_CHANNEL_TYPE        = (act_BAD,act_UNIX,act_TCP,act_UDP,act_SCTP,act_CPU_WORK);
  TFRE_APSC_CHANNEL_SUB_TYPE    = (acst_BAD,acst_IP4,acst_IP6,acst_UX);


  { TFRE_APS_CMD_BASE }

  TFRE_APS_CMD_BASE=class(TFOS_FactorableBase)
  protected
    FDontFinalize : boolean;
  public
    procedure         Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);virtual;abstract;
  end;

  { TFRE_APS_CMD_ADD_TIMER }

  TFRE_APS_CMD_ADD_TIMER=class(TFRE_APS_CMD_BASE)
  private
    FTimer      : TFRE_APSC_TIMER;
    FStartTimer : Boolean;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_ADD_TIMER; inline;
    function       Setup         (tim : TFRE_APSC_TIMER ; const starttimer : boolean): TFRE_APS_CMD_ADD_TIMER;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;


  { TFRE_APS_CMD_ADD_LISTENER }

  TFRE_APS_CMD_ADD_LISTENER=class(TFRE_APS_CMD_BASE)
  private
    FListener      : TFRE_APSC_Listener;
    FStartListener : Boolean;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_ADD_LISTENER; inline;
    function       Setup         (lis : TFRE_APSC_Listener ; const startlistener : boolean): TFRE_APS_CMD_ADD_LISTENER;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_LISTENER_CRTL }

  TFRE_APS_CMD_LISTENER_CRTL=class(TFRE_APS_CMD_BASE)
  private
    type
      TFRE_APS_CMD_LISTENER_CRTL_TYP=(lc_Start,lc_Stop);
    var
      FListener      : TFRE_APSC_Listener;
      FCmdType       : TFRE_APS_CMD_LISTENER_CRTL_TYP;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_LISTENER_CRTL; inline;
    function       Setup         (const lis : TFRE_APSC_Listener ; const cmd : TFRE_APS_CMD_LISTENER_CRTL_TYP): TFRE_APS_CMD_LISTENER_CRTL;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_CHANNEL_CRTL }

  TFRE_APS_CMD_CHANNEL_CRTL=class(TFRE_APS_CMD_BASE)
  private
    type
      TFRE_APS_CMD_CHAN_CRTL_TYP=(cc_Connect,cc_Disconnect);
    var
      FChannel       : TFRE_APSC_CHANNEL;
      FCmdType       : TFRE_APS_CMD_CHAN_CRTL_TYP;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_CHANNEL_CRTL; inline;
    function       Setup         (const chan : TFRE_APSC_CHANNEL ; const cmd : TFRE_APS_CMD_CHAN_CRTL_TYP): TFRE_APS_CMD_CHANNEL_CRTL;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_DO_WORK_CHUNK }
  TFRE_APS_CMD_DO_WORKABLE=class;

  TFRE_APS_CMD_DO_WORK_CHUNK=class(TFRE_APS_CMD_BASE) { Distribute a Workable to a compute chnannelgroup and let the work be done }
  private
    Fstart_ix,
    Fend_ix,
    Fmax_ix   : NativeInt;
    FWorkable : IFRE_APSC_WORKABLE;
    FWorkCmd  : TFRE_APS_CMD_DO_WORKABLE;
    FWorkerID : NativeInt;
    FMyCgt    : TFRE_APSC_CHANNELGROUP_THREAD;
  public
    class function FactoryCreate (const Factory   : IFOS_FactoryInterface): TFRE_APS_CMD_DO_WORK_CHUNK; inline;
    function       Setup         (const start_ix,end_ix,max_ix : NativeInt ; const fwid : NativeInt ; const workcmd : TFRE_APS_CMD_DO_WORKABLE ; const mycg : TFRE_APSC_CHANNELGROUP_THREAD): TFRE_APS_CMD_DO_WORK_CHUNK;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_DO_WORKABLE }

  TFRE_APS_CMD_DO_WORKABLE=class(TFRE_APS_CMD_BASE) { Distribute a Workable to a compute chnannelgroup and let the work be done }
  private
    FWorkable    : IFRE_APSC_WORKABLE;
    FEndEvent    : IFOS_E;
    FWorking     : longint; { Interlocked Decrement to zero, then work is done }
    FReturnCM    : IFRE_APSC_CHANNEL_MANAGER;
    FReturnCG    : IFRE_APSC_CHANNEL_GROUP;
  public
    procedure      a_SendWorkDone ;
    class function FactoryCreate  (const Factory   : IFOS_FactoryInterface): TFRE_APS_CMD_DO_WORKABLE; inline;
    function       Setup          (const workable  : IFRE_APSC_WORKABLE ; const endevent : IFOS_E ; const return_ctx : IFRE_APSC_CHANNEL_MANAGER ; const ReturnCG : IFRE_APSC_CHANNEL_GROUP): TFRE_APS_CMD_DO_WORKABLE;inline;
    procedure      Execute        (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_SWITCH_CTX }

  TFRE_APS_CMD_SWITCH_CTX=class(TFRE_APS_CMD_BASE)
  private
    type
      TCoMode  = (simple,simplenested,ex,exnested);
      TCoMethod=record
        data : Pointer;
        case mode:TCoMode of
          0 :  (SimpleCo : TFRE_APSC_CoRoutineSimple);
          1 :  (SimpleNe : TFRE_APSC_CoRoutineSimpleNested);
          2 :  (ExCo     : TFRE_APSC_CoRoutine);
          3 :  (ExNe     : TFRE_APSC_CoRoutineNested);
      end;
  var
    Fmethod : TCoMethod;
  public
    class function FactoryCreate  (const Factory : IFOS_FactoryInterface) : TFRE_APS_CMD_SWITCH_CTX; inline;
    function       Setup          (const method  : TFRE_APSC_CoRoutineSimple) : TFRE_APS_CMD_SWITCH_CTX;inline;
    function       Setup          (const method  : TFRE_APSC_CoRoutineSimpleNested) : TFRE_APS_CMD_SWITCH_CTX;inline;
    function       Setup          (const method  : TFRE_APSC_CoRoutine ; const data:Pointer) : TFRE_APS_CMD_SWITCH_CTX;inline;
    function       Setup          (const method  : TFRE_APSC_CoRoutineNested ; const data : pointer) : TFRE_APS_CMD_SWITCH_CTX;inline;
    procedure      Execute        (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;


  { TFRE_APS_CMD_WORK_DONE }

  TFRE_APS_CMD_WORK_DONE=class(TFRE_APS_CMD_BASE)  { use case singleton / (simplerecylce) }
  private
    class var
     FSingleton : TFRE_APS_CMD_WORK_DONE;
  public
    class procedure   InitSingleton        ; override;
    class procedure   DestroySingleton     ; override;
    class function    ClassIsSingleton     : Boolean; override;
    class function    GetFactoredSingleton : TFOS_FactorableBase; override;
    class function    FactoryCreate        (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_WORK_DONE;inline;
    procedure         Execute              (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_ADD_NEW_SRV_SOCKET }

  TFRE_APS_CMD_ADD_NEW_SRV_SOCKET=class(TFRE_APS_CMD_BASE)
  private var
    Ffd     : cint;
    Fsa     : TFCOM_SOCKADDRSTORAGE;
    Flen    : socklen_t;
    FSSl    : boolean;
    FSSLCtx : PSSL_CTX;
    FListID : TFRE_APSC_ID;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_ADD_NEW_SRV_SOCKET; inline;
    function       Setup         (new_fd: cint; sa: TFCOM_SOCKADDRSTORAGE; len: socklen_t; const ssl_enabled: boolean; const ssl_ctx: PSSL_CTX; const listenerid: TFRE_APSC_ID): TFRE_APS_CMD_ADD_NEW_SRV_SOCKET;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  { TFRE_APS_CMD_FINALIZE }

  TFRE_APS_CMD_FINALIZE=class(TFRE_APS_CMD_BASE)
  private
    FWhat       : TFRE_APSC_BaseObject;
  public
    class function FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_FINALIZE; inline;
    function       Setup         (const what:TFRE_APSC_BaseObject):TFRE_APS_CMD_FINALIZE;inline;
    procedure      Execute       (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;


  { TFRE_APS_CMD_REQ_CTX_TERMINATE }

  TFRE_APS_CMD_REQ_CTX_TERMINATE=class(TFRE_APS_CMD_BASE)  { use case singleton / (simplerecylce) }
  private
    class var
     FSingleton : TFRE_APS_CMD_REQ_CTX_TERMINATE;
  public
    class procedure   InitSingleton        ; override;
    class procedure   DestroySingleton     ; override;
    class function    ClassIsSingleton     : Boolean; override;
    class function    GetFactoredSingleton : TFOS_FactorableBase; override;
    class function    FactoryCreate (const Factory:IFOS_FactoryInterface): TFRE_APS_CMD_REQ_CTX_TERMINATE; inline;
    procedure         Execute (const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP);override;
  end;

  TAPSC_DispatchInSync = procedure  (const cmd : TFRE_APS_CMD_BASE ; const ctx_owner : TFRE_APSC_CTX_THREAD; const what : TAPSC_EV_TYP) of object;

  { TFRE_APS_LL_EvBaseController }
  TFRE_APS_LL_EvBaseController=class
  private
  var
    FId            : TFRE_APSC_ID;
    FGoDown        : Boolean; // FLag when the loop was requested to stop
    FControlEvent  : PEvent;
    FTimeoutE      : PEvent;
    FEventBase     : PEvent_base;
    FDnsBase       : PEvdns_base;
    FOwnerCTX      : TFRE_APSC_CTX_THREAD;
    FDeferredCTX   : TFRE_APSC_CTX_THREAD;
    FCreateDNS     : Boolean;
    FTimeout       : TFCOM_TimeVal;
    FWorkQ         : IFOS_LFQ;
    procedure   _CommEventfired    (what : TAPSC_EV_TYP); { from event loop }
    procedure   _TimeoutEventfired (what : TAPSC_EV_TYP); { from event loop }
  public
    procedure   csPushDirectCommand (const cmd : TFRE_APS_CMD_BASE ; const event_indirect : boolean=false); { event direct   = make the control event active from this ctx,
                                                                                                              may be lock contention if the current thread is working
                                                                                                              event_indirect = delegate the event notification to a specialiced eventer,
                                                                                                              the eventer should check if the current base is active ( push only on LFQ )
                                                                                                              or realy event the current base if the base is idling. ( CAS Mechanism ?  )
                                                                                                            }
    constructor Create              (const ctx_owner : TFRE_APSC_CTX_THREAD ; const create_dns_base : boolean ; const id : TFRE_APSC_ID ; const need_timeout_interval : NativeUInt = 0 ; const deferred_ctx_owner : TFRE_APSC_CTX_THREAD=nil);
    destructor  Destroy             ;override;
    procedure   Loop                ;
    procedure   FinalizeLoop        ;
  end;

  { TFRE_APSC_BaseObject }

  TFRE_APSC_BaseObject=class(TObject) { a thread context managed object }
  private
    FOwnerCTX          : TFRE_APSC_CTX_THREAD; { the object }
    FAssignedThreadID  : TThreadID;            { the real id of the running thread }
    FEvBase            : PEvent_base;
    FEvDNSBase         : Pevdns_base;
    FEvent             : PEvent;
  protected
    function    Implementor        : TObject;
    procedure   cs_Finalize        ; virtual;
  end;

  { TFRE_APSC_CHANNELGROUP_THREAD }
  TFRE_APSC_CHANNEL_MANAGER = class;

  OFOS_SL_TFRE_APSC_Listener        = specialize OFOS_SpareList<TFRE_APSC_LISTENER>;     // small count
  OFOS_SL_TFRE_APSC_TIMER           = specialize OFOS_SpareList<TFRE_APSC_TIMER>;        // small count
  OFOS_SL_TFRE_APSC_CTX_THREADS     = specialize OFOS_SpareList<TFRE_APSC_CTX_THREAD>;   // small count

  { TFRE_APSC_CTX_THREAD }

  TFRE_APSC_CTX_THREAD = class(TThread)
  private
    FCtxChangeLock    : IFOS_LOCK;
    FCB               : TFRE_APS_LL_EvBaseController;
    FMyThreadID       : TThreadID;
    FMyIndent         : Shortstring;
    FChildContexts    : OFOS_SL_TFRE_APSC_CTX_THREADS;     { Communication Channel Managers }
    FListenerList     : OFOS_SL_TFRE_APSC_Listener;        { channel group listeners        }
    FTimerList        : OFOS_SL_TFRE_APSC_Timer;           { channel group timers           }
  protected
    procedure   s_AddTimer              (const timer : TFRE_APSC_TIMER    ; const immediate_start : boolean);
    procedure   s_AddListener           (const list  : TFRE_APSC_Listener ; const immediate_start : boolean);
    procedure   a_PushFinalize          (const what : TFRE_APSC_BaseObject);
    procedure   s_TerminateAndWaitfor   ; virtual;
    procedure   a_RequestTerminate      ;
    procedure   s_FinalizeObject        (const what : TFRE_APSC_BaseObject); virtual;

  public
    procedure   cs_PushDirectCommand    (const cmd : TFRE_APS_CMD_BASE ; const event_indirect : boolean=false); { event direct   = make the control event active from this ctx,
                                                                                                                   may be lock contention if the current thread is working
                                                                                                                   event_indirect = delegate the event notification to a specialiced eventer,
                                                                                                                   the eventer should check if the current base is active ( push only on LFQ )
                                                                                                                   or realy event the current base if the base is idling. ( CAS Mechanism ?  )
                                                                                                                 }
    procedure   cs_LockContextChange    ;
    procedure   cs_UnlockContectChange  ;
    function    Implementor             : TObject;
    procedure   SetupCommon             (const id : TFRE_APSC_ID ; const need_timeout_interval : NativeUInt = 0 ; const deferred_ctx_owner : TFRE_APSC_CTX_THREAD=nil);
    function    GetID                   : TFRE_APSC_ID;
    procedure   DispatchDoInSync        (const cmd : TFRE_APS_CMD_BASE ; const ctx_owner : TFRE_APSC_CTX_THREAD ; const what : TAPSC_EV_TYP); virtual; { i am a thread, i am dispatching the inbound command in my ctx }

    procedure   SwitchToContext          (const object_co : TFRE_APSC_CoRoutineSimple);
    procedure   SwitchToContextNe        (const nested_co : TFRE_APSC_CoRoutineSimpleNested);
    procedure   SwitchToContextEx        (const object_co : TFRE_APSC_CoRoutine       ; const data : Pointer);
    procedure   SwitchToContextExNe      (const nested_co : TFRE_APSC_CoRoutineNested ; const data : Pointer);

    procedure   Execute                 ; override;
    destructor  Destroy                 ; override;
  end;

  TFRE_APSC_CM_DISTRIBUTION_CB=procedure (const start_ix,end_ix,max_ix,wid : NativeInt ; const cm : TFRE_APSC_CHANNEL_MANAGER) is nested;

  { TFRE_APSC_SIMPLE_WORKABLE }

  TFRE_APSC_SIMPLE_WORKABLE=class(IFRE_APSC_WORKABLE)
  private
    FMeth : TFRE_APSC_CoRoutine;
    FData  : Pointer;
  public
    constructor Create                   (const method:TFRE_APSC_CoRoutine ; const data : Pointer);
    procedure   SetupWorkerCount_WIF     (const wc : NativeInt);
    function    GetAsyncDoneContext_WIF  : IFRE_APSC_CHANNEL_MANAGER;
    function    GetAsyncDoneChannelGroup_WIF : IFRE_APSC_CHANNEL_GROUP;
    function    StartGetMaximumChunk_WIF : NativeInt;
    procedure   ParallelWorkIt_WIF       (const startchunk,endchunk : Nativeint ; const wid : NativeInt);
    procedure   WorkNextCyle_WIF         (var continue : boolean);                                         { set to true to get a recall                                      }
    procedure   WorkDone_WIF             ;
    procedure   ErrorOccurred_WIF        (const ec : NativeInt ; const em : string);
  end;

  TFRE_APSC_CHANNELGROUP_THREAD = class(TFRE_APSC_CTX_THREAD,IFRE_APSC_CHANNEL_GROUP)       { One Dispatching (eventing / main thread per channel group) }
  private
    FWork       : TFRE_APS_CMD_DO_WORKABLE     ;
    FWorkQ      : IFOS_LFQ;
    procedure   s_WorkDone                     (const ask_next_cycle : boolean);
    function    s_CallChannelMgrsDistribute    (const cb : TFRE_APSC_CM_DISTRIBUTION_CB ; const max_chunks : NativeInt):boolean;
    procedure   s_WorkItWork                   (const work : TFRE_APS_CMD_DO_WORKABLE);
    procedure   s_DoOrEnqueueWorkable          (const work : TFRE_APS_CMD_DO_WORKABLE);
    function    s_GetManagerWithMinimumConns   : TFRE_APSC_CHANNEL_MANAGER;     { LOAD BASED FAIR DISTRIBUTION }
    procedure   s_DistributeNewAcceptedChannel (new_fd : cint; sa : PFCOM_SOCKADDRSTORAGE ; len : socklen_t ; const ssl_enabled : boolean ; const ssl_ctx : PSSL_CTX ; const listenerid : TFRE_APSC_ID); { usage from the listener, owned by the cg (in sync)}
    function    s_GetChannelManagerByID        (cm_id   : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    function    s_GetChannelManagerIDs         : TFRE_APSC_ID_Array;
    function    s_CreateNewChannelManager      (const cm_id   : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    function    s_GetFirstChannelManager       : TFRE_APSC_CHANNEL_MANAGER;
  public
    function    GetCGID                   : TFRE_APSC_ID;
    function    GetChannelManagerIDs      : TFRE_APSC_ID_Array;
    function    GetChannelManagerCount    : NativeInt;
    function    GetDefaultChannelManager : IFRE_APSC_CHANNEL_MANAGER;
    function    GetChannelManagerByID     (const cm_id   : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    function    GetChannelManagerMinChans : IFRE_APSC_CHANNEL_MANAGER;
    function    CreateNewChannelManager   (const cm_id   : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    function    AddChannelGroupTimer      (const timer_id: TFRE_APSC_ID ; interval_ms : NativeInt ; timer_callback         : TFRE_APSC_TIMER_CALLBACK    ; const start_timer    : boolean = true ; const asc_meth_code : CodePointer =nil ; const asc_meth_data : Pointer =nil) : IFRE_APSC_TIMER;
    function    AddListenerTCP            (Bind_IP,Bind_Port :String     ; const ID:TFRE_APSC_ID     ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK ; const start_listener : boolean = true ; const enable_ssl : boolean = false ; const special_ssl_ctx : PSSL_CTX =nil ): IFRE_APSC_LISTENER; // is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, and *6 (which use all addresses of the host)
    function    AddListenerUX             (special_file      : ShortString ; const ID :TFRE_APSC_ID ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK ; const start_listener : boolean = true ; const enable_ssl : boolean = false ; const special_ssl_ctx : PSSL_CTX =nil ): IFRE_APSC_LISTENER;
    procedure   DoAsyncWork               (const workable : IFRE_APSC_WORKABLE); { needs an "pingback" continuation context }
    procedure   DoSyncedWork              (const workable : IFRE_APSC_WORKABLE); { does a "hard" wait in the current context (event) }
    procedure   DoAsyncWorkSimpleMethod   (const method   : TFRE_APSC_CoRoutine ; const data : Pointer); { simple encapsulation of a workable                }
    constructor Create                    (const up_cg_name  : ShortString; const channel_worker_cnt: NativeInt);
    destructor  Destroy                   ;override;
  end;

  { TFRE_APSC_CHANNEL_MANAGER }

  TFRE_APSC_CHANNEL_CB     = procedure (const channel      : TFRE_APSC_CHANNEL) is nested;

  TFRE_APSC_CHANNEL_MANAGER = class(TFRE_APSC_CTX_THREAD,IFRE_APSC_CHANNEL_MANAGER)
  private
    FMyCGT            : TFRE_APSC_CHANNELGROUP_THREAD;
    FChannelCount     : NativeUInt;
    FChannellist      : TFPList;

    function    cs_GetActiveChannelCount : Nativeint;
    procedure   s_IncActiveChannelCount  ;
    procedure   s_DecActiveChannelcount  ;

    function    GetChannelGroup          : IFRE_APSC_CHANNEL_GROUP;
    function    GetCGT                   : TFRE_APSC_CHANNELGROUP_THREAD;

    procedure   ForAllChannels           (const ccb : TFRE_APSC_CHANNEL_CB);
    procedure   _DoForChannel            (cp, arg: Pointer);


    procedure   s_TerminateAndWaitfor    ; override;

    procedure   s_AddNewServedChannel    (const fd : cint ;  sa : TFCOM_SOCKADDRSTORAGE ; const salen : socklen_t ;const use_ssl : boolean ; const SSLCtx : PSSL_CTX ; const listenerid: TFRE_APSC_ID);
    procedure   s_AddNewClientChannel    (const cl_channel : TFRE_APSC_CHANNEL);
    procedure   s_FinalizeObject         (const what : TFRE_APSC_BaseObject); override;
    procedure   a_AddClientChannel       (const cl_channel : IFRE_APSC_CHANNEL);

  public
    function    GetThreadID              : TThreadID;
    function    AddChannelManagerTimer   (const timer_id: TFRE_APSC_ID ; interval_ms : NativeUint ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; const start_timer : boolean = false ; const asc_meth_code : CodePointer =nil ; const asc_meth_data : Pointer =nil) : IFRE_APSC_TIMER;

    constructor Create                   (const ID :TFRE_APSC_ID ; const OwnerChannelGroupThread : TFRE_APSC_CHANNELGROUP_THREAD);
    destructor  Destroy                  ; override;
  end;

  { TFRE_APS_COMM }

  TFRE_APS_COMM=class(TFRE_APSC_BaseObject,IFRE_APSC,IFOS_FactoryInterface)
  private
    FSSL_CTX                 : PSSL_CTX;
    FDefaultCG               : TFRE_APSC_CHANNELGROUP_THREAD;  { TODO: rename to default }
    FDedicatedChannelGroups  : TFPHashList;
    FAPSC_ListenerCB         : TFRE_APSC_LISTENER_CALLBACK;
    FonNew_APSC_Channel      : TFRE_APSC_CHANNEL_CHANGE_EVENT;
    FOnNew_APSC_Signal       : TOnNew_APSC_Signal;
    FDoneEvent               : IFOS_E;
    FChannelGroupsChangeLock : IFOS_LOCK;
    TEST_Listener            : IFRE_APSC_LISTENER;

    function    s_AddNewChannelGroup       (cg_name : TFRE_APSC_ID ; out cg : TFRE_APSC_CHANNELGROUP_THREAD ; const channel_worker_cnt: NativeInt):boolean;
    function    s_DeleteChannelGroup       (cg_name : TFRE_APSC_ID):boolean;
    function    s_GetChannelgroup          (cg_name : TFRE_APSC_ID ; out cg : TFRE_APSC_CHANNELGROUP_THREAD ):boolean;
    procedure   s_ForAllChannelGroups      (const cg_cb : TFRE_APSC_CTX_Callback);
    procedure   s__DoForChannelGroup       (cgp,arg : Pointer);

    procedure   _CallbackSignal            (const signal   : cint);                                                           { directly from the signal handler                      }
    procedure   _CallbackChannelEvent      (const channel  : IFRE_APSC_CHANNEL  ; const channel_event  : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt); { after client launch in the ctx of the CHANNEL MANAGER }
    procedure   _CallbackListenerEvent     (const listener : IFRE_APSC_LISTENER ; const listener_state : TAPSC_ListenerState);

    procedure   s_FinalizeMain             ;
  protected

    procedure   TEST_ListenerCB            (const listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure   TEST_ListenerCB_SSL        (const listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);

    procedure   TEST_ConnectManSockCB      (const channel  : IFRE_APSC_CHANNEL  ; const channel_event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt);
    procedure   TEST_NewCLientSock2        (const channel  : IFRE_APSC_CHANNEL  ; const channel_event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt);
    procedure   TEST_DiscoClientChannel    (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_ReadClientChannel     (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_ReadClientChannel2    (const channel  : IFRE_APSC_CHANNEL);
    procedure   TEST_NewTimerCB            (const timer    : IFRE_APSC_TIMER);
    procedure   TEST_TIMER_TIME            (const timer    : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
  public
    constructor Create                     ;
    destructor  Destroy                    ; override;
    procedure   RunUntilTerminate          ;
    procedure   RequestTerminate           (const no_jack:boolean=false);

    function    GetDefaultCG               : IFRE_APSC_CHANNEL_GROUP;
    function    GetChannelGroupByID        (CGID : TFRE_APSC_ID ; out cg : IFRE_APSC_CHANNEL_GROUP):boolean;
    function    GetChannelGroupIDs         : TFRE_APSC_ID_Array;
    function    CreateNewChannelGroup      (const cg_id : TFRE_APSC_ID     ; out cm : IFRE_APSC_CHANNEL_GROUP ; const auto_workercnt : NativeInt=0) : boolean;

    function    AddDefaultGroupTimer       (const timer_id: TFRE_APSC_ID   ; interval_ms : NativeUint  ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; const start_timer : boolean = false ; const asc_meth_code : CodePointer =nil ; const asc_meth_data : Pointer =nil    ) : IFRE_APSC_TIMER;
    function    AddDefaultGroupListenerTCP (Bind_IP,Bind_Port:String       ; const ID:TFRE_APSC_ID     ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK ; const start_listener : boolean = true ; const enable_ssl : boolean=false ; const special_ssl_ctx : PSSL_CTX =nil): IFRE_APSC_LISTENER; // is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, and *6 (which use all addresses of the host)
    function    AddDefaultGroupListenerUX  (const special_file:shortstring ; const ID:TFRE_APSC_ID     ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK = nil ; const start_listener : boolean = true ; const enable_ssl : boolean=false ; const special_ssl_ctx : PSSL_CTX =nil): IFRE_APSC_LISTENER;

    function    AddClient_TCP              (IP,Port   : String             ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ; localChEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String=''):IFRE_APSC_CHANNEL;
    function    AddClient_TCP_DNS          (Host,Port : String             ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ; localChEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String=''):IFRE_APSC_CHANNEL;
    function    AddClient_UX               (const special_file:shortstring ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ; localChEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil):IFRE_APSC_CHANNEL;
    procedure   ResolveDNS_TCP             (const addrstring : String);

    procedure   SetListenerCB              (const lcb       : TFRE_APSC_LISTENER_CALLBACK);
    procedure   SetNewChannelCB            (const chancb    : TFRE_APSC_CHANNEL_CHANGE_EVENT);
    procedure   SetSingnalCB               (const signalcb  : TOnNew_APSC_Signal);
    function    Factor                     (const class_type : TFOS_FactorableClass):TFOS_FactorableBase;
    procedure   Recycle                    (const obj        : TFOS_FactorableBase);
  end;



  TFRE_APS_COMM_DNS_ANSWER=class
  end;

  { TFRE_APSC_TIMER }

  TFRE_APSC_TIMER = class(TFRE_APSC_BaseObject,IFRE_APSC_TIMER)
  private
    FCallMethod        : TMethod;
    FCallback          : TFRE_APSC_TIMER_CALLBACK;
    FId                : TFRE_APSC_ID;
    FInterval          : TTimeVal;
    FIntervalms        : NativeInt;
    FLock              : IFOS_LOCK;
    procedure   TimerCallback      (const what : cshort);
    procedure   ThreadCheck;
    procedure   s_SetUpEventBase   (const base : PEvent_base ; const immediate_start : boolean ; const ctx_owner : TFRE_APSC_CTX_THREAD);
   public
    constructor Create             (const id: ShortString; const interval: Nativeint; const timercb: TFRE_APSC_TIMER_CALLBACK ; const asc_meth_code: CodePointer; const asc_meth_data: Pointer);
    function    cs_GetID           : TFRE_APSC_ID;
    procedure   cs_Trigger         (const flag1:boolean=false ; const flag2:boolean=false);
    function    cs_Start           (const interval_ms : NativeInt=0):boolean;
    function    cs_Stop            :boolean;
    procedure   cs_ChangeCallback  (cb : TFRE_APSC_TIMER_CALLBACK);
    procedure   cs_SetMethod       (const m : TMethod);
    function    cs_GetMethod       :TMethod;
    destructor  Destroy            ; override;
  end;

  { TFRE_APSC_Listener }

  TFRE_APSC_Listener=class(TFRE_APSC_BaseObject,IFRE_APSC_LISTENER)
  private
  var
    FState        : TAPSC_ListenerState;
    Fsockaddr     : TFCOM_SOCKADDRSTORAGE;
    Fsockaddr_len : cInt;
    FListenAddr   : string;
    FListensock   : cInt;
    FError        : String;
    FEcode        : NativeInt;
    FId           : TFRE_APSC_ID;
    FSSL_CTX      : PSSL_CTX;
    FSSL_Enabled  : Boolean;
    //FsockaddrUnix : TFCOM_sockaddr_un;
  protected
    procedure   s_AcceptCB              (const what : cshort); { from loop }
    procedure   s_SetupEventBase        (const base : PEvent_base ; const immediate_start : boolean); { ctx owner is setup on create, its always a cg thread }
    procedure   s_Start                 ;
    procedure   s_Stop                  ;
    procedure   spc_SetupSocketData     (const id:string ; const sa : TFCOM_SOCKADDRSTORAGE ; const sa_len : cInt ; const enable_ssl : boolean ; const ssl_ctx : PSSL_CTX);
    function    GetState                : TAPSC_ListenerState;
    function    GetErrorString          : string;
    function    GetListeningAddress     : string;
  public
    constructor Create              (const channelgroup : TFRE_APSC_CHANNELGROUP_THREAD);
    procedure   cs_Start            ;
    procedure   cs_Stop             ;
    function    cs_GetID            :TFRE_APSC_ID;
    destructor  Destroy             ;override;
  end;

  { TFRE_APSC_CHANNEL }

  TFRE_APSC_CHANNEL=class(TFRE_APSC_BaseObject,IFRE_APSC_CHANNEL)
  private
  type
    TSAFE_WRITE_ENCAP=class
      FData : Pointer;
      FLen  : NativeInt;
    end;
  var
    FState             : TAPSC_ChannelState;
    FServed            : Boolean;
    FChannelType       : TFRE_APSC_CHANNEL_TYPE;
    FChannelSubType    : TFRE_APSC_CHANNEL_SUB_TYPE;
    FClient            : Boolean;
    FDoDNS             : Boolean;
    Fsockaddr          : TFCOM_SOCKADDRSTORAGE;
    Fsockaddr_len      : cInt;
    Fsockaddrbind      : TFCOM_SOCKADDRSTORAGE;
    Fsockaddrb_len     : cInt;
    FSocketAddr        : string;
    Fsocket            : cInt;
    //FChanError         : String;
    //FChanECode         : NativeInt;
    FBufEvent          : PBufferevent;
    FInputBuf          : PEvbuffer;
    FOutputBuf         : PEvbuffer;
    FInBufCB           : Pevbuffer_cb_entry;
    FOutBufCB          : Pevbuffer_cb_entry;
    FTotalInRead       : NativeUint;
    FTotalOutWrite     : NativeUint;
    FVerboseID         : String;
    FConnectHost       : String; // do it via DNS or unix socket
    FConnectPort       : Cardinal;
    FConnectFam        : cInt;
    FDataTag           : PtrUInt;
    FOnRead            : TFRE_APSC_CHANNEL_EVENT;
    FOnDisco           : TFRE_APSC_CHANNEL_EVENT;
    FOnStatus          : TFRE_APSC_CHANNEL_CHANGE_EVENT;
    FId                : TFRE_APSC_ID;
    FFromListenerID    : TFRE_APSC_ID;
    FClientSSL         : Pssl_st;
    FSSL_Enabled       : Boolean;
    FFinalizecalled    : Boolean;
    FDisconnectHandled : Boolean;
    procedure   ReadDataEvent          ;
    procedure   WriteDataEvent         ;
    procedure   InputBufferEvent       (const bufinfo : Pevbuffer_cb_info);
    procedure   OutputBufferEvent      (const bufinfo : Pevbuffer_cb_info);
    procedure   GenericEvent           (what : cShort);
    procedure   ThreadCheck            ;
    procedure   EventDisconnectOnce    ;
    procedure   DoStatusCallback       (const channel: IFRE_APSC_CHANNEL; const ev_Type: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
    function    _GetDebugID             : String;

  protected
    procedure   SetupServedSocketEvBase (fd : cint ; new_sa : TFCOM_SOCKADDRSTORAGE ; new_sal : cInt ; newchannelcb : TFRE_APSC_CHANNEL_CHANGE_EVENT ; const ctx_owner : TFRE_APSC_CTX_THREAD);
    procedure   SetupClientSocketTCP    (new_sa : TFCOM_SOCKADDRSTORAGE ; new_sal : cInt ; bind_sa : TFCOM_SOCKADDRSTORAGE ; bind_sal : cInt ; const id: TFRE_APSC_ID ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT ; const status_ev : TFRE_APSC_CHANNEL_CHANGE_EVENT ; const ctx_owner : TFRE_APSC_CTX_THREAD);
    procedure   SetupClientSocketDNS    (const host : string ; port : NativeInt          ; bind_sa : TFCOM_SOCKADDRSTORAGE ; bind_sal : cInt ; const id: TFRE_APSC_ID ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT ; const status_ev : TFRE_APSC_CHANNEL_CHANGE_EVENT ; const ctx_owner : TFRE_APSC_CTX_THREAD);
    procedure   SetupClientSocketUX     (const special_file : Shortstring ; const id : ShortString ; readevent,discoevent : TFRE_APSC_CHANNEL_EVENT ; const status_ev : TFRE_APSC_CHANNEL_CHANGE_EVENT);
    //procedure   StartClientSockConnect  (const base : PEvent_base ; const dnsbase : Pevdns_base  ;manager : TFRE_APSC_CHANNEL_MANAGER); // a client socket part 2

    procedure   s_Connect               ;
    procedure   s_Disconnect            ;

    procedure   COR_SafeWriteBuffer     (const data: pointer);
  public
    function    cs_GetChannelManager    : IFRE_APSC_CHANNEL_MANAGER; { unsafe, when cs is changed sometimes (migration) }
    procedure   cs_StartConnect         ;
    procedure   cs_Disconnect           ;
    function    CH_GetConnSocketAddr    : String;
    function    CH_GetHandleKey         : cInt;

    procedure   CH_SetOnReadData        (on_read  : TFRE_APSC_CHANNEL_EVENT);
    procedure   CH_SetOnDisconnnect     (on_disco : TFRE_APSC_CHANNEL_EVENT);

    function    CH_GetVerboseDesc       : String;
    procedure   CH_SetVerboseDesc       (const desc:string);

    procedure   CH_WriteString          (const str : RawByteString);
    procedure   CH_WriteBuffer          (const data : Pointer ; const len : NativeInt);

    procedure   cs_WriteBuffer          (const data : Pointer ; const len : NativeInt);
    procedure   cs_WriteString          (const str : RawByteString);

    procedure   CH_WriteOpenedFile      (const fd : cInt ; const offset,len : NativeInt);
    procedure   CH_Enable_Reading       ;
    procedure   CH_Enable_Writing       ;
    function    CH_GetDataCount         : NativeInt;
    function    CH_ReadString           : RawByteString;
    function    CH_ReadBuffer           (const data : Pointer ; const len : NativeInt) : NativeInt;
    function    CH_IsClientChannel      : Boolean;
    function    CH_GetState             : TAPSC_ChannelState;
    function    CH_GetID                : TFRE_APSC_ID;
    function    ch_GetListenerID        : TFRE_APSC_ID;
    procedure   CH_AssociateData        (const data : PtrUInt);
    function    CH_GetAssociateData     : PtrUInt;

    constructor Create                  (const served_channel: boolean; const enable_ssl: boolean; const ssl_ctx: PSSL_CTX ; const listenerid: TFRE_APSC_ID='');
    destructor  Destroy                 ;override;
    procedure   cs_Finalize             ;
    procedure   cs_FinalizeFromPartner  ;
  end;

var GAPSC : TFRE_APS_COMM;

Procedure DoSig(sig : cint);cdecl;
begin
  if assigned(GAPSC) then
    GAPSC._CallbackSignal(sig)
  else
    begin
      writeln('------');
      writeln('> GOT SIGNAL ',sig,' BUT IN SHUTDOWN MODE !');
      writeln('------');
    end;
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

type

  { TTestWorkable }

  TTestWorkable=class(IFRE_APSC_WORKABLE)
  private
    FWorksize : Integer;
    FAdone    : IFRE_APSC_CHANNEL_MANAGER;
    Fid       : string;
  public
    constructor Create                   (const id:string);
    procedure   SetWorkSize              (const ws:integer);
    procedure   SetupWorkerCount_WIF     (const wc : NativeInt);                                           { gives a hint how many workers will do the load                   }
    function    StartGetMaximumChunk_WIF : NativeInt;                                                      { tell the cpu cg how much work is to be done parallel             }
    procedure   ParallelWorkIt_WIF       (const startchunk,endchunk : Nativeint ; const wid : NativeInt);  { the working callback, gives chunk id, and the parallel worker id }
    procedure   WorkDone_WIF             ;                                                                 { the workers have finishe, join the results                       }
    procedure   WorkNextCyle_WIF         (var continue : boolean);                                         { set to true to get a recall                                      }
    function    GetAsyncDoneContext_WIF  : IFRE_APSC_CHANNEL_MANAGER;                                      { if nil then the work does not need a callback                    }
    function    GetAsyncDoneChannelGroup_WIF : IFRE_APSC_CHANNEL_GROUP;
    procedure   SetAsyncDoneContext_WIF  (const cm:IFRE_APSC_CHANNEL_MANAGER);
    procedure   ErrorOccurred_WIF        (const ec : NativeInt ; const em : string);
    destructor  Destroy                  ; override;
  end;

var     CPUCG        : IFRE_APSC_CHANNEL_GROUP;

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

var
    TEST_Timer   : IFRE_APSC_TIMER;
    TEST_TIMER2  : IFRE_APSC_TIMER;
    DEF          : IFRE_APSC_CHANNEL_GROUP;
    L_V4         : IFRE_APSC_LISTENER;
    L_V6         : IFRE_APSC_LISTENER;
    L_V4_SSL     : IFRE_APSC_LISTENER;
    L_UX         : IFRE_APSC_LISTENER;
    CL4,CL6,CLUX : IFRE_APSC_CHANNEL;
    CLDNS        : IFRE_APSC_CHANNEL;
    TESTWORKS    : TTestWorkable;


    procedure PushAWork;
    var TESTWORKA     : TTestWorkable;
    begin
      writeln('ASYNC PUSH ',NativeUInt(GetThreadID));
      TESTWORKA := TTestWorkable.Create('ASY ');
      TESTWORKA.SetAsyncDoneContext_WIF(GFRE_SC.GetDefaultCG.GetDefaultChannelManager);
      TESTWORKA.SetWorkSize(StrToIntDef(paramstr(3),1000));
      CPUCG.DoAsyncWork(TESTWORKA);
    end;

begin
  case  lowercase(what) of
     '' :
         begin
           GFRE_SC.SetListenerCB(@GAPSC.TEST_ListenerCB);
           GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);

           def := GFRE_SC.GetDefaultCG;
           TEST_Timer := def.AddChannelGroupTimer('TEST',-1000,nil,true);
           TEST_Timer.cs_ChangeCallback(@GAPSC.TEST_TIMER_TIME);

           if not TEST_Timer.cs_Start then
             begin
               writeln('could not activate timer, not setup, use sync start (!)');
               sleep(10);
               if not TEST_Timer.cs_Start(10) then
                 writeln('unexpected start failure');
             end;

           L_V4 := GFRE_SC.AddDefaultGroupListenerTCP('*','44000','I4L',nil,false);    { nc 127.0.0.1 44000}
           L_V4.cs_Start;
           L_V4.cs_Stop;
           L_V4.cs_Start;
           //
           L_V6     := GFRE_SC.AddDefaultGroupListenerTCP('*6','44000','I6L');                 { nc ::1 44000}
           L_V4_SSL := GFRE_SC.AddDefaultGroupListenerTCP('*','44001','IP4SSL',nil,true,true); { Test with : openssl s_client -tls1 -host 127.0.0.1 -port 44001 }
           L_UX     := GFRE_SC.AddDefaultGroupListenerUX('/tmp/uxtest','UXS');                 { nc -U /tmp/uxtest }

           //CL4  := GFRE_SC.AddClient_TCP('127.0.0.1','44000','CL1_V4');
           //CL6  := GFRE_SC.AddClient_TCP('[::1]','44000','CL1_V6');
           CLDNS := GFRE_SC.AddClient_TCP_DNS('localhost.lulu','44000','CL1_DNS');
           CLDNS := GFRE_SC.AddClient_TCP_DNS('localhost','44000','CL2_DNS');
            //writeln('ASYNC : ',GAPSC.TEST_Listener.GetState);
        end;
     'echounix': begin
                    GFRE_SC.SetListenerCB(@GAPSC.TEST_ListenerCB);
                    GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);
                    //GFRE_SC.AddListener_UX('/tmp/uxtest','UXS');
                 end;
     'echossl':  begin
                     GFRE_SC.SetListenerCB(@GAPSC.TEST_ListenerCB_SSL);
                     GFRE_SC.SetNewChannelCB(@GAPSC.TEST_ConnectManSockCB);
                     //GFRE_SC.AddListener_TCP('*','44000','IP46-ECHO');
                    end;
     'unixclient' : begin
                      GFRE_SC.AddClient_UX('/tmp/uxtest','UXS',true,nil,@GAPSC.TEST_NewCLientSock2,@GAPSC.TEST_ReadClientChannel2,@GAPSC.TEST_DiscoClientChannel);
                    end;
     'httpclient' : begin
                      cnt := 0;
                      for i:=0 to 100 do
                          begin
                            hc := TFRE_SIMPLE_HTTP_CLIENT.create;
                            hc.SetHost('www.firmos.at');
                            //hc.SetHost('80.123.225.59');
                            //hc.SetPort('80');
                            //hc.SetIP_Mode(true);
                            //hc.GetMethod('/test/test.json',@GotHttpAnswer);
                            hc.GetMethod('/test/test.json',@GotHttpAnswer);
                          end;
                    end;
     'cpuchannels' : begin
                       GFRE_SC.CreateNewChannelGroup('CPU',CPUCG,StrToIntDef(paramstr(2),4));
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       GFRE_SC.GetDefaultCG.GetDefaultChannelManager.SwitchToContextNe(@PushAWork);
                       TESTWORKS := TTestWorkable.Create('SYWO 1');
                       TESTWORKS.SetWorkSize(StrToIntDef(paramstr(3),1000)+1000);
                       CPUCG.DoSyncedWork(TESTWORKS);
                       TESTWORKS.Free;

                       writeln('WAITED SYNC');
                       sleep(10000);
                       GAPSC.RequestTerminate();
                     end;
  end;
end;


procedure   LogInfo(const s:String;const Args : Array of const); inline;
begin
  {$IFDEF APS_LOG_INFO}
  {$IFDEF DEBUG_WL}
     writeln('##---------------------------');
     writeln(Format('##INFO >> '+s,args));
     writeln('##---------------------------');
  {$ENDIF}
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Info,CFOS_LL_Target[fll_Info],false);
  {$ENDIF}
end;

procedure   LogNotice(const s:String;const Args : Array of const); inline;
begin
  {$IFDEF APS_LOG_NOTICE}
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Notice,CFOS_LL_Target[fll_Notice],false);
  {$ENDIF}
end;


procedure LogDebug(const s:String;const Args : Array of const); inline;
begin
  {$IFDEF APS_LOG_DEBUG}
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Debug,CFOS_LL_Target[fll_Debug],false);
  {$ENDIF}
end;

procedure   LogWarning(const s:String;const Args : Array of const); inline;
begin
  {$IFDEF APS_LOG_WARNING}
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Warning,CFOS_LL_Target[fll_Warning],false);
  {$ENDIF}
end;

procedure   LogError(const s:String;const Args : Array of const); inline;
begin
  {$IFDEF APS_LOG_ERROR}
    {$IFDEF DEBUG_WL}
       writeln('##---------------------------');
       writeln(Format('##ERROR >> '+s,args));
       writeln('##---------------------------');
    {$ENDIF}
  GFRE_LOG.Log(s,Args,CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Error,CFOS_LL_Target[fll_Error],false);
  {$ENDIF}
end;


function APSC_TranslateOsError(const os_error: cint; const prefix: string=''; postfix: string=''): string;
begin
  result := prefix+inttostr(os_error)+':'+StrError(os_error)+postfix;
end;

procedure APSC_CheckRaiseOSCall(const os_result: cint; const prefix: string; postfix: string=''; const crit_abort: boolean=false);
var err : cInt;
begin
  if os_result<>0 then
    begin
      err := 0;
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
    FCOM_AF_INET : evutil_inet_ntop(FCOM_AF_INET,@sa^.sin_addr,@Result[1],Length(Result));
    FCOM_AF_INET6: evutil_inet_ntop(FCOM_AF_INET6,@sa^.sin6_addr,@Result[1],Length(Result));
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
    //feat : Integer;
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
  evthread_make_base_notifiable(Result); { make the base notifyable, just to be sure }

  //feat   := event_base_get_features(result);
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
  TFRE_APSC_Listener(data).s_AcceptCB(short);
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

{ TFRE_APSC_SIMPLE_WORKABLE }

constructor TFRE_APSC_SIMPLE_WORKABLE.Create(const method: TFRE_APSC_CoRoutine; const data: Pointer);
begin
  FMeth := method;
  FData := data;
end;

procedure TFRE_APSC_SIMPLE_WORKABLE.SetupWorkerCount_WIF(const wc: NativeInt);
begin
  // ignore
end;

function TFRE_APSC_SIMPLE_WORKABLE.GetAsyncDoneContext_WIF: IFRE_APSC_CHANNEL_MANAGER;
begin
  result := nil;
end;

function TFRE_APSC_SIMPLE_WORKABLE.GetAsyncDoneChannelGroup_WIF: IFRE_APSC_CHANNEL_GROUP;
begin
  result := nil
end;

function TFRE_APSC_SIMPLE_WORKABLE.StartGetMaximumChunk_WIF: NativeInt;
begin
  result := 1;
end;


procedure TFRE_APSC_SIMPLE_WORKABLE.WorkNextCyle_WIF(var continue: boolean);
begin
  continue := false;
end;

procedure TFRE_APSC_SIMPLE_WORKABLE.ParallelWorkIt_WIF(const startchunk, endchunk: Nativeint; const wid: NativeInt);
begin
  FMeth(Fdata);
end;

procedure TFRE_APSC_SIMPLE_WORKABLE.WorkDone_WIF;
begin
   Free;
end;

procedure TFRE_APSC_SIMPLE_WORKABLE.ErrorOccurred_WIF(const ec: NativeInt; const em: string);
begin
  // ignore
end;

{ TFRE_APS_CMD_SWITCH_CTX }

class function TFRE_APS_CMD_SWITCH_CTX.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_SWITCH_CTX;
begin
  result := TFRE_APS_CMD_SWITCH_CTX(Factory.Factor(TFRE_APS_CMD_SWITCH_CTX));
end;

function TFRE_APS_CMD_SWITCH_CTX.Setup(const method: TFRE_APSC_CoRoutineSimple): TFRE_APS_CMD_SWITCH_CTX;
begin
  result  := self;
  Fmethod.SimpleCo := method;
  Fmethod.mode     := simple;
end;

function TFRE_APS_CMD_SWITCH_CTX.Setup(const method: TFRE_APSC_CoRoutineSimpleNested): TFRE_APS_CMD_SWITCH_CTX;
begin
  result  := self;
  Fmethod.SimpleNe := method;
  Fmethod.mode     := simplenested;
end;

function TFRE_APS_CMD_SWITCH_CTX.Setup(const method: TFRE_APSC_CoRoutine; const data: Pointer): TFRE_APS_CMD_SWITCH_CTX;
begin
  result  := self;
  Fmethod.data     := data;
  Fmethod.ExCo     := method;
  Fmethod.mode     := ex;
end;

function TFRE_APS_CMD_SWITCH_CTX.Setup(const method: TFRE_APSC_CoRoutineNested; const data: pointer): TFRE_APS_CMD_SWITCH_CTX;
begin
  result  := self;
  Fmethod.data     := data;
  Fmethod.ExNe     := method;
  Fmethod.mode     := exnested;
end;

procedure TFRE_APS_CMD_SWITCH_CTX.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  try
    with Fmethod do
      case mode of
        simple:       SimpleCo();
        simplenested: SimpleNe();
        ex:           ExCo(data);
        exnested:     ExNe(data);
      end;
  except
    on e:Exception do
      LogError('SWITCH COROUTINE EXCEPTION ON [%s] FAILED [%s]',[ctx_owner.GetID,e.Message]);
  end;
end;



{ TFRE_APS_CMD_WORK_DONE }

class procedure TFRE_APS_CMD_WORK_DONE.InitSingleton;
begin
  FSingleton := TFRE_APS_CMD_WORK_DONE.Create;
end;

class procedure TFRE_APS_CMD_WORK_DONE.DestroySingleton;
begin
  FreeAndNil(FSingleton);
end;

class function TFRE_APS_CMD_WORK_DONE.ClassIsSingleton: Boolean;
begin
  Result:=true
end;

class function TFRE_APS_CMD_WORK_DONE.GetFactoredSingleton: TFOS_FactorableBase;
begin
  Result := FSingleton;
end;

class function TFRE_APS_CMD_WORK_DONE.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_WORK_DONE;
begin
  result := TFRE_APS_CMD_WORK_DONE(Factory.Factor(TFRE_APS_CMD_WORK_DONE));
end;

procedure TFRE_APS_CMD_WORK_DONE.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  (ctx_owner as TFRE_APSC_CHANNELGROUP_THREAD).s_WorkDone(true);
end;

{ TTestWorkable }

constructor TTestWorkable.Create(const id: string);
begin
  Fid := id;
end;

procedure TTestWorkable.SetWorkSize(const ws: integer);
begin
  FWorksize := ws;
end;

procedure TTestWorkable.SetupWorkerCount_WIF(const wc: NativeInt);
begin
  writeln(FId,'--> Setup Workercount to ',wc,' ',nativeuint(GetThreadID));
end;

function TTestWorkable.StartGetMaximumChunk_WIF: NativeInt;
begin
  result := FworkSize;
end;

procedure TTestWorkable.ParallelWorkIt_WIF(const startchunk, endchunk: Nativeint; const wid: NativeInt);
var wait : integer;
begin
  wait := (endchunk-startchunk+1)*2;
  writeln(FId,' ', wid,' ',' >> ',startchunk,' ',endchunk,' wait : ',wait,' ',NativeUInt(GetCurrentThreadId));
  sleep(wait);
  writeln(FId,' ',wid,' ',' << ',startchunk,' ',endchunk,' DONE');
end;

procedure TTestWorkable.WorkDone_WIF;
begin
  writeln(FId,' ','||>>ASYNC --- WORK DONE ',nativeuint(GetThreadID));
  Free;
end;

procedure TTestWorkable.WorkNextCyle_WIF(var continue: boolean);
begin

end;

function TTestWorkable.GetAsyncDoneContext_WIF: IFRE_APSC_CHANNEL_MANAGER;
begin
  result := FAdone;
end;

function TTestWorkable.GetAsyncDoneChannelGroup_WIF: IFRE_APSC_CHANNEL_GROUP;
begin
  result := nil;
end;

procedure TTestWorkable.SetAsyncDoneContext_WIF(const cm: IFRE_APSC_CHANNEL_MANAGER);
begin
  FAdone := cm;
end;

procedure TTestWorkable.ErrorOccurred_WIF(const ec: NativeInt; const em: string);
begin

end;

destructor TTestWorkable.Destroy;
begin
  writeln('>>Destroying ',Fid);
end;

{ TFRE_APS_CMD_DO_WORK_CHUNK }

class function TFRE_APS_CMD_DO_WORK_CHUNK.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_DO_WORK_CHUNK;
begin
  result := TFRE_APS_CMD_DO_WORK_CHUNK(Factory.Factor(TFRE_APS_CMD_DO_WORK_CHUNK));
end;

function TFRE_APS_CMD_DO_WORK_CHUNK.Setup(const start_ix, end_ix, max_ix: NativeInt; const fwid: NativeInt; const workcmd: TFRE_APS_CMD_DO_WORKABLE; const mycg: TFRE_APSC_CHANNELGROUP_THREAD): TFRE_APS_CMD_DO_WORK_CHUNK;
begin
  Fstart_ix := start_ix;
  Fend_ix   := end_ix;
  Fmax_ix   := max_ix;
  FWorkerID := fwid;
  FWorkable := workcmd.fworkable;
  FWorkCmd  := workcmd;
  FMyCgt    := mycg;
  result    := self;
end;

procedure TFRE_APS_CMD_DO_WORK_CHUNK.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
var value : longint;
begin
  try
    FWorkable.ParallelWorkIt_WIF(Fstart_ix,Fend_ix,FWorkerID);
  finally
    value := FOS_IL_Decrement(FWorkCmd.FWorking);
    if value=0 then
      begin
        FMyCgt.cs_PushDirectCommand(TFRE_APS_CMD_WORK_DONE.FactoryCreate(GAPSC));
      end;
  end;
end;

{ TFRE_APS_CMD_DO_WORKABLE }

procedure TFRE_APS_CMD_DO_WORKABLE.a_SendWorkDone;
begin
  if assigned(FEndEvent) then
    begin
      FEndEvent.SetEvent;
    end
  else
    if Assigned(FReturnCM) then
      begin
        FReturnCM.SwitchToContext(@FWorkable.WorkDone_WIF);
      end
    else
    if Assigned(FReturnCG) then
      begin
        FReturnCG.SwitchToContext(@FWorkable.WorkDone_WIF);
      end
    else
      FWorkable.WorkDone_WIF;
end;

class function TFRE_APS_CMD_DO_WORKABLE.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_DO_WORKABLE;
begin
  result := TFRE_APS_CMD_DO_WORKABLE(Factory.Factor(TFRE_APS_CMD_DO_WORKABLE));
end;

function TFRE_APS_CMD_DO_WORKABLE.Setup(const workable: IFRE_APSC_WORKABLE; const endevent: IFOS_E; const return_ctx: IFRE_APSC_CHANNEL_MANAGER; const ReturnCG: IFRE_APSC_CHANNEL_GROUP): TFRE_APS_CMD_DO_WORKABLE;
begin
  FWorkable     := workable;
  FEndEvent     := endevent;
  FDontFinalize := true;
  FReturnCM     := return_ctx;
  FReturnCG     := ReturnCG;
  if assigned(FReturnCG) and assigned(FReturnCM) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'only one return context may be specified !');
  result        := self;
end;

procedure TFRE_APS_CMD_DO_WORKABLE.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
var cgt : TFRE_APSC_CHANNELGROUP_THREAD;
begin
  cgt        := ctx_owner as TFRE_APSC_CHANNELGROUP_THREAD; { check dont'finalize }
  cgt.s_DoOrEnqueueWorkable(self);
end;

{ TFRE_APS_CMD_CHANNEL_CRTL }

class function TFRE_APS_CMD_CHANNEL_CRTL.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_CHANNEL_CRTL;
begin
  result := TFRE_APS_CMD_CHANNEL_CRTL(Factory.Factor(TFRE_APS_CMD_CHANNEL_CRTL));
end;

function TFRE_APS_CMD_CHANNEL_CRTL.Setup(const chan: TFRE_APSC_CHANNEL; const cmd: TFRE_APS_CMD_CHAN_CRTL_TYP): TFRE_APS_CMD_CHANNEL_CRTL;
begin
  FChannel := chan;
  FCmdType := cmd;
  result   := self;
end;

procedure TFRE_APS_CMD_CHANNEL_CRTL.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  case FCmdType of
    cc_Connect  :  FChannel.s_Connect;
    cc_Disconnect: FChannel.s_Disconnect;
  end;
end;

{ TFRE_APS_CMD_ADD_NEW_SRV_SOCKET }

class function TFRE_APS_CMD_ADD_NEW_SRV_SOCKET.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_ADD_NEW_SRV_SOCKET;
begin
  result := TFRE_APS_CMD_ADD_NEW_SRV_SOCKET(Factory.Factor(TFRE_APS_CMD_ADD_NEW_SRV_SOCKET));
end;

function TFRE_APS_CMD_ADD_NEW_SRV_SOCKET.Setup(new_fd: cint; sa: TFCOM_SOCKADDRSTORAGE; len: socklen_t; const ssl_enabled: boolean; const ssl_ctx: PSSL_CTX; const listenerid: TFRE_APSC_ID): TFRE_APS_CMD_ADD_NEW_SRV_SOCKET;
begin
  Ffd     := new_fd;
  Fsa     := sa;
  Flen    := len;
  FSSl    := ssl_enabled;
  FSSLCtx := ssl_ctx;
  FListID := listenerid;
  result  := self;
end;

procedure TFRE_APS_CMD_ADD_NEW_SRV_SOCKET.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  (ctx_owner as TFRE_APSC_CHANNEL_MANAGER).s_AddNewServedChannel(Ffd,fsa,flen,fssl,FSSLCtx,FListID);
end;

{ TFRE_APS_CMD_LISTENER_CRTL }

class function TFRE_APS_CMD_LISTENER_CRTL.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_LISTENER_CRTL;
begin
  result := TFRE_APS_CMD_LISTENER_CRTL(Factory.Factor(TFRE_APS_CMD_LISTENER_CRTL));
end;

function TFRE_APS_CMD_LISTENER_CRTL.Setup(const lis: TFRE_APSC_Listener; const cmd: TFRE_APS_CMD_LISTENER_CRTL_TYP): TFRE_APS_CMD_LISTENER_CRTL;
begin
  FListener := lis;
  FCmdType  := cmd;
  result    := self;
end;

procedure TFRE_APS_CMD_LISTENER_CRTL.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  case FCmdType of
    lc_Start: FListener.s_Start;
    lc_Stop:  FListener.s_Stop;
  end;
end;

{ TFRE_APS_CMD_ADD_LISTENER }

class function TFRE_APS_CMD_ADD_LISTENER.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_ADD_LISTENER;
begin
  result := TFRE_APS_CMD_ADD_LISTENER(Factory.Factor(TFRE_APS_CMD_ADD_LISTENER));
end;

function TFRE_APS_CMD_ADD_LISTENER.Setup(lis: TFRE_APSC_Listener; const startlistener: boolean): TFRE_APS_CMD_ADD_LISTENER;
begin
  FListener      := lis;
  FStartListener := startlistener;
  result         := self;
end;

procedure TFRE_APS_CMD_ADD_LISTENER.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  ctx_owner.s_AddListener(FListener,FStartListener); // self.flistener
end;

{ TFRE_APS_CMD_FINALIZE }

class function TFRE_APS_CMD_FINALIZE.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_FINALIZE;
begin
  result := TFRE_APS_CMD_FINALIZE(Factory.Factor(TFRE_APS_CMD_FINALIZE));
end;

function TFRE_APS_CMD_FINALIZE.Setup(const what: TFRE_APSC_BaseObject): TFRE_APS_CMD_FINALIZE;
begin
  FWhat  := what;
  result := self;
end;

procedure TFRE_APS_CMD_FINALIZE.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  ctx_owner.s_FinalizeObject(Fwhat);
end;

{ TFRE_APSC_BaseObject }

function TFRE_APSC_BaseObject.Implementor: TObject;
begin
  result := self;
end;

procedure TFRE_APSC_BaseObject.cs_Finalize;
begin
  FOwnerCTX.a_PushFinalize(self);
end;

{ TFRE_APSC_CTX_THREAD }


procedure TFRE_APSC_CTX_THREAD.s_AddTimer(const timer: TFRE_APSC_TIMER; const immediate_start: boolean);
begin
  if FTimerList.Add(timer)=-1 then
    GFRE_BT.CriticalAbort('critical: double add timer?');
  Timer.s_SetupEventBase(FCB.FEventBase,immediate_start,self);
end;

procedure TFRE_APSC_CTX_THREAD.s_AddListener(const list: TFRE_APSC_Listener; const immediate_start: boolean);
begin
  if FListenerList.Add(list)=-1 then
    GFRE_BT.CriticalAbort('critical: double add timer?');
  list.s_SetupEventBase(FCB.FEventBase,immediate_start); { owner context set on creation = always a cg}
end;

procedure TFRE_APSC_CTX_THREAD.a_PushFinalize(const what: TFRE_APSC_BaseObject);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_FINALIZE.FactoryCreate(GAPSC).Setup(what));
end;

procedure TFRE_APSC_CTX_THREAD.s_TerminateAndWaitfor;

  procedure TerminateAndWait(var chld : TFRE_APSC_CTX_THREAD);
  begin
    try
      chld.a_RequestTerminate;
      chld.WaitFor;
      chld.free;
    except
      on e: exception do
        begin
          LogError('CHILD FINALIZATION ERROR OCCURRED IN [%s]',[GetID]);
        end;
    end;
  end;

  procedure FreeListener(var l:TFRE_APSC_Listener);
  begin
    try
      l.Free;
      l:=nil;
    except
      on e: exception do
        begin
          LogError('LISTENER FINALIZATION ERROR OCCURRED IN [%s]',[GetID]);
        end;
    end;
  end;

  procedure FreeTimer(var t:TFRE_APSC_TIMER);
  begin
    try
      t.free;
      t:=nil;
    except
      on e: exception do
        begin
          LogError('TIMER FINALIZATION ERROR OCCURRED IN [%s]',[GetID]);
        end;
    end;
  end;

begin
  writeln(FMyIndent+'>TERMINATING ',ClassName,' --> ',GetID);
  writeln(FMyIndent+FMyIndent,'Listeners');
  FListenerList.ForAll(@FreeListener);
  writeln(FMyIndent+FMyIndent,'Timers');
  FTimerList.ForAll(@FreeTimer);
  writeln(FMyIndent+FMyIndent,'Childs');
  FChildContexts.ForAll(@TerminateAndWait);
  writeln(FMyIndent+FMyIndent,'Event Loop');
  FCB.FinalizeLoop;
  writeln(FMyIndent+'>TERMINATING DONE ',ClassName,' <-- ',GetID);
  inherited;
end;


procedure TFRE_APSC_CTX_THREAD.a_RequestTerminate;
begin
  cs_PushDirectCommand(TFRE_APS_CMD_REQ_CTX_TERMINATE.FactoryCreate(GAPSC));
end;

procedure TFRE_APSC_CTX_THREAD.s_FinalizeObject(const what: TFRE_APSC_BaseObject);

  procedure _FinTimer;
  begin
    if not FTimerList.Delete(TFRE_APSC_TIMER(what)) then
      LogError('unexpected timer free event/not in list [%s]',[TFRE_APSC_TIMER(what).cs_GetID]);
    what.Free;
  end;

  procedure _FinListener;
  begin
    if not FListenerList.Delete(TFRE_APSC_Listener(what)) then
      LogError('unexpected timer free event/not in list [%s]',[TFRE_APSC_Listener(what).cs_GetID]);
    what.Free;
  end;

begin
  if what is TFRE_APSC_TIMER then
    _FinTimer
  else
  if what is TFRE_APSC_Listener then
    _FinListener
  else
    GFRE_BT.CriticalAbort('invalid object to finalize [%s]',[what.ClassName]);
end;

procedure TFRE_APSC_CTX_THREAD.cs_PushDirectCommand(const cmd: TFRE_APS_CMD_BASE; const event_indirect: boolean);
begin
  FCB.csPushDirectCommand(cmd,event_indirect); //self
end;

procedure TFRE_APSC_CTX_THREAD.cs_LockContextChange;
begin
  FCtxChangeLock.Acquire;
end;

procedure TFRE_APSC_CTX_THREAD.cs_UnlockContectChange;
begin
  FCtxChangeLock.Release;
end;

function TFRE_APSC_CTX_THREAD.Implementor: TObject;
begin
  result := self;
end;

procedure TFRE_APSC_CTX_THREAD.SetupCommon(const id: TFRE_APSC_ID; const need_timeout_interval: NativeUInt; const deferred_ctx_owner: TFRE_APSC_CTX_THREAD);
begin
  GFRE_TF.Get_Lock(FCtxChangeLock);
  FListenerList.InitSparseListPtrCmp;
  FTimerList.InitSparseListPtrCmp;
  FChildContexts.InitSparseListPtrCmp();
  FCB := TFRE_APS_LL_EvBaseController.Create(self,true,id,need_timeout_interval,deferred_ctx_owner);
end;

function TFRE_APSC_CTX_THREAD.GetID: TFRE_APSC_ID;
begin
  result := FCB.FId;
end;

procedure TFRE_APSC_CTX_THREAD.DispatchDoInSync(const cmd: TFRE_APS_CMD_BASE; const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);

  procedure SafeLog(const msg:String);
  var cmdclass : ShortString;
  begin
    try
      cmdclass := cmd.ClassName;
    except
      cmdclass := 'BAD/EXCEPT';
    end;
    LogError('DISPATCHINSYNC CTX [%s], CMDCLASS [%s] FAILED WITH [%s]',[GetID,cmdclass,msg]);
  end;

begin
  try
    try
      cmd.Execute(ctx_owner,what);
    finally
      if cmd.FDontFinalize=false then
        GAPSC.Recycle(cmd);
    end;
  except
    on e:exception do
      SafeLog(e.Message)
  end;
end;


procedure TFRE_APSC_CTX_THREAD.SwitchToContext(const object_co: TFRE_APSC_CoRoutineSimple);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_SWITCH_CTX.FactoryCreate(GAPSC).Setup(object_co));
end;

procedure TFRE_APSC_CTX_THREAD.SwitchToContextNe(const nested_co: TFRE_APSC_CoRoutineSimpleNested);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_SWITCH_CTX.FactoryCreate(GAPSC).Setup(nested_co));
end;

procedure TFRE_APSC_CTX_THREAD.SwitchToContextEx(const object_co: TFRE_APSC_CoRoutine; const data: Pointer);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_SWITCH_CTX.FactoryCreate(GAPSC).Setup(object_co,data));
end;

procedure TFRE_APSC_CTX_THREAD.SwitchToContextExNe(const nested_co: TFRE_APSC_CoRoutineNested; const data: Pointer);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_SWITCH_CTX.FactoryCreate(GAPSC).Setup(nested_co,data));
end;


procedure TFRE_APSC_CTX_THREAD.Execute;
begin
  try
    GFRE_LOG.RegisterThread(GetID);
    FMyThreadID := GetThreadID;
    LogDebug(GetID+' STARTUP',[]);
    FCB.Loop;
    LogDebug(GetID+' LOOP TERMINATED',[]);
  except on E:Exception do begin
    try
      GFRE_LOG.Log('APSCOMM '+ClassName+' | '+GetID+' ['+e.Message+']','',fll_Emergency,'APSW',true);
      writeln(GFRE_BT.DumpExceptionsBacktrace);
    except on e:exception do begin
      GFRE_LOG.LogEmergency('LOGGING EXCEPTION : APSCOMM '+ClassName+' | '+GetID+' : '+e.Message);
    end;end;
  end;end;
end;

destructor TFRE_APSC_CTX_THREAD.Destroy;
begin
  FCB.Free;
  FCtxChangeLock.Finalize;
  inherited Destroy;
end;

{ TFRE_APS_REQ_TERMINATE_CMD }

class procedure TFRE_APS_CMD_REQ_CTX_TERMINATE.InitSingleton;
begin
  FSingleton := TFRE_APS_CMD_REQ_CTX_TERMINATE.Create;
end;

class procedure TFRE_APS_CMD_REQ_CTX_TERMINATE.DestroySingleton;
begin
  FreeAndNil(FSingleton);
end;

class function TFRE_APS_CMD_REQ_CTX_TERMINATE.ClassIsSingleton: Boolean;
begin
  result := true;
end;

class function TFRE_APS_CMD_REQ_CTX_TERMINATE.GetFactoredSingleton: TFOS_FactorableBase;
begin
  result := FSingleton;
end;

class function TFRE_APS_CMD_REQ_CTX_TERMINATE.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_REQ_CTX_TERMINATE;
begin
  result := TFRE_APS_CMD_REQ_CTX_TERMINATE(Factory.Factor(TFRE_APS_CMD_REQ_CTX_TERMINATE));
end;

procedure TFRE_APS_CMD_REQ_CTX_TERMINATE.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
var classn : shortstring;
begin
  classn := ctx_owner.ClassName;
  ctx_owner.s_TerminateAndWaitfor;
end;

{ TFRE_APS_ADD_TIMER_CMD }

class function TFRE_APS_CMD_ADD_TIMER.FactoryCreate(const Factory: IFOS_FactoryInterface): TFRE_APS_CMD_ADD_TIMER;
begin
  result := TFRE_APS_CMD_ADD_TIMER(Factory.Factor(TFRE_APS_CMD_ADD_TIMER));
end;

function TFRE_APS_CMD_ADD_TIMER.Setup(tim: TFRE_APSC_TIMER; const starttimer: boolean): TFRE_APS_CMD_ADD_TIMER;
begin
  FTimer      := tim;
  FStartTimer := starttimer;
  result      := self;
end;

procedure TFRE_APS_CMD_ADD_TIMER.Execute(const ctx_owner: TFRE_APSC_CTX_THREAD; const what: TAPSC_EV_TYP);
begin
  ctx_owner.s_AddTimer(FTimer,FStartTimer);
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
  if NativeUint(FAssignedThreadID)=0 then
    GFRE_BT.CriticalAbort('usage before creation failure timer [%s] no assigned thread vs creation thread [%s] ',[FId,inttostr(NativeUint(FAssignedThreadID))]);
  if GetThreadID<>FAssignedThreadID then
    GFRE_BT.CriticalAbort('timer [%s] thread context violation [%s] vs [%s] ',[FId,inttostr(NativeUint(GetThreadID)),inttostr(NativeUint(FAssignedThreadID))]);
end;

constructor TFRE_APSC_TIMER.Create(const id: ShortString; const interval: Nativeint; const timercb: TFRE_APSC_TIMER_CALLBACK; const asc_meth_code: CodePointer; const asc_meth_data: Pointer);
begin
  FId                       := id;
  FIntervalms               := interval;
  FCallback                 := timercb;
  TMethod(FCallMethod).Code := asc_meth_code;
  TMethod(FCallMethod).Data := asc_meth_data;
  GFRE_TF.Get_Lock(FLock);
end;

procedure TFRE_APSC_TIMER.s_SetUpEventBase(const base: PEvent_base; const immediate_start: boolean; const ctx_owner: TFRE_APSC_CTX_THREAD);
begin
  FLock.Acquire;
  try
    FAssignedThreadID := GetCurrentThreadId;
    FOwnerCTX         := ctx_owner;
    FEvBase           := base;
    FEvent            := nil;
    if immediate_start then
      cs_Start(FIntervalms);
  finally
    FLock.Release;
  end;
end;

function TFRE_APSC_TIMER.cs_Start(const interval_ms: NativeInt): boolean;
begin
  FLock.Acquire;
  try
    if not assigned(FEvBase) then
      exit(false);
    if (not ASSIGNED(FEvent)) or
       (interval_ms<>0)  then
          begin
            if interval_ms<>0 then
              FIntervalms := interval_ms;
            APSC_SetupTimeout(abs(FIntervalms),FInterval);
            if assigned(FEvent) then
              begin
                event_free(FEvent);
                FEvent:=nil;
              end;
            if FIntervalms>0 then { periodic }
              FEvent := event_new(Fevbase,-1,EV_READ or EV_WRITE or EV_TIMEOUT or EV_PERSIST,@loc_timer_cb,self)
            else
              FEvent := event_new(FEvBase,-1,EV_READ or EV_WRITE or EV_TIMEOUT,@loc_timer_cb,self);
          end;
    result := event_add(FEvent,@FInterval)=0; { make it pending }
  finally
    FLock.Release;
  end;
end;

function TFRE_APSC_TIMER.cs_Stop:boolean;
begin
  FLock.Acquire;
  try
    result := true;
    if assigned(FEvent) then
      event_del(FEvent);
  finally
    FLock.Release;
  end;
end;

procedure TFRE_APSC_TIMER.cs_ChangeCallback(cb: TFRE_APSC_TIMER_CALLBACK);
begin
  FLock.Acquire;
  try
    FCallback := cb;
  finally
    FLock.Release;
  end;
end;

function TFRE_APSC_TIMER.cs_GetID: TFRE_APSC_ID;
begin
  result := FId; { read only }
end;

procedure TFRE_APSC_TIMER.cs_SetMethod(const m: TMethod);
begin
  Flock.Acquire;
  try
    FCallMethod := m;
  finally
    FLock.Finalize;
  end;
end;

function TFRE_APSC_TIMER.cs_GetMethod: TMethod;
begin
  FLock.Acquire;
  try
    result := FCallMethod;
  finally
    FLock.Release;
  end;
end;

procedure TFRE_APSC_TIMER.cs_Trigger(const flag1: boolean; const flag2: boolean);
var what : cshort;
begin
  if flag1 then
    what := what or EV_READ;
  if flag2 then
    what := what or EV_WRITE;
  event_active(FEvent,what,1); { is thread safe }
end;

destructor TFRE_APSC_TIMER.Destroy;
begin
  ThreadCheck;
  FLock.Finalize;
  if assigned(FEvent) then
    begin
      event_del(FEvent);
      event_free(Fevent);
    end;
  inherited Destroy;
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
var dnsres     : CInt;
    FChanError : String;
    FChanECode : NativeInt;

    procedure BailOut(const finalize_srv:boolean=false);
    begin
      EventDisconnectOnce;
      bufferevent_flush(FBufEvent,EV_WRITE,BEV_FLUSH);
      if finalize_srv then
        cs_FinalizeFromPartner; { in a served socket scenario, something gone wrong}
    end;

begin
  //writeln('*** -> GENEVENT ',FId,' ', what);
  if (what and BEV_EVENT_EOF)>0 then
    begin
      LogDebug('READ EOF (CLOSE) on CHANNEL '+_GetDebugID+' FLAGS '+inttostr(what),[]);
      FState := ch_EOF;
      EventDisconnectOnce;
      bufferevent_flush(FBufEvent,EV_WRITE,BEV_FLUSH);
      cs_FinalizeFromPartner;
    end
  else
  if (what and BEV_EVENT_CONNECTED)>0 then
    begin
      FState     := ch_ACTIVE;
      Fsocket    := bufferevent_getfd(FBufEvent);
      if FChannelType=act_TCP then
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
          Fsockaddr_len := SizeOf(FSockAddr); // self.fsockaddr
          if  FChannelType=act_TCP then
            begin
              dnsres      := fcom_fpgetpeername(Fsocket,@Fsockaddr,@Fsockaddr_len);
              FSocketAddr := APSC_sa2string(@Fsockaddr);
            end;
          DoStatusCallback(self,ch_NEW_CS_CONNECTED,'',0);
        end;
    end
  else
  if (what and BEV_EVENT_ERROR)>0 then
    begin
      if FState=ch_WAIT then
        FState := ch_NEW_CHANNEL_FAILED
      else
        FState := ch_ErrorOccured;
      dnsres :=  bufferevent_socket_get_dns_error(FBufEvent);
      if dnsres<>0 then
        begin
          FChanError := evutil_gai_strerror(dnsres);
          FChanECode := dnsres;
        end
      else
        APSC_CheckResultSetError(-1,FChanError,FChanECode,'SOCK:','');
      if FChanECode=0 then
        begin
          FChanECode := -1;
          FChanError := 'no extended error info available';
        end;
      DoStatusCallback(self,FState,FChanError,FChanECode);
      BailOut;
    end
  else
    GFRE_BT.CriticalAbort('how to handle '+inttostR(what)+' on '+CH_GetVerboseDesc+'   : '+CH_GetConnSocketAddr);
end;

procedure TFRE_APSC_CHANNEL.ThreadCheck;
begin
  if GetThreadID<>FAssignedThreadID then
    GFRE_BT.CriticalAbort('channel [%s] thread context violation [%s] vs [%s] ',[FVerboseID,inttostr(NativeUint(GetThreadID)),inttostr(NativeUint(FAssignedThreadID))]);
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
        try
          FOnDisco(self);
        except
          on e:Exception do
            begin
              LogError('EventDisconnectonce failed [%s] channel : [%d]',[e.Message,_GetDebugID]);
            end;
        end;
    end;
  FDisconnectHandled := true;
end;

procedure TFRE_APSC_CHANNEL.DoStatusCallback(const channel: IFRE_APSC_CHANNEL; const ev_Type: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  try
    FOnStatus(channel,ev_Type,errorstring,errorcode);
  except on e:exception do
    LogError('CHANNEL DoStatus Callback Failed : '+e.Message,[]);
  end
end;

function TFRE_APSC_CHANNEL._GetDebugID: String;
begin
  try
    if FClient then
      result := 'CS'+inttostr(Fsocket)+'#'+FSocketAddr+' '+FVerboseID+'@'+inttostr(NativeUint(self))
    else
      result := 'SS'+inttostr(Fsocket)+'#'+FSocketAddr+' '+FVerboseID+'@'+inttostr(NativeUint(self));
  except
    result := '??MEMBAD/ Debugid 4 channel';
  end;
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
  result:=fcom_fpsetsockopt(socket,FCOM_IPPROTO_TCP,FCOM_TCP_NODELAY,@opt,SizeOf(opt));
end;


procedure TFRE_APSC_CHANNEL.SetupServedSocketEvBase(fd: cint; new_sa: TFCOM_SOCKADDRSTORAGE; new_sal: cInt; newchannelcb: TFRE_APSC_CHANNEL_CHANGE_EVENT; const ctx_owner: TFRE_APSC_CTX_THREAD);
var fam        : cint;
    FChanECode : NativeInt;
    FChanError : String;

begin
  FClient           := false;
  Fsockaddr         := new_sa;
  Fsockaddr_len     := new_sal;
  Fsocket           := fd;
  FOnStatus         := newchannelcb;
  FOwnerCTX         := ctx_owner;
  FEvBase           := FOwnerCTX.FCB.FEventBase;
  FEvDNSBase        := FOwnerCTX.FCB.FDnsBase;
  FAssignedThreadID := GetCurrentThreadId;

  { determine socket type }
  fam := Fsockaddr.sun_family;
  case fam of
    FCOM_AF_INET  :
        begin
          FChannelType    := act_TCP;
          FChannelSubType := acst_IP4;
        end;
    FCOM_AF_INET6 :
        begin
          FChannelType     := act_TCP;
          FChannelSubType  := acst_IP6;
        end;
    FCOM_AF_UNIX  :
        begin
          FChannelType     := act_UNIX;
          FChannelSubType  := acst_UX;
        end;
    else
      begin
        DoStatusCallback(self,ch_ErrorOccured,'unsupported address family/setupserved socket',-1);
        exit;
      end;
  end;
  if APSC_CheckResultSetError(evutil_make_socket_closeonexec(Fsocket),FChanError,FChanECode,'SETCLOSEONEX: ') then
    begin
      DoStatusCallback(self,ch_ErrorOccured,' failed/setupserved socket',-1);
      exit;
    end;
  if APSC_CheckResultSetError(evutil_make_socket_nonblocking(Fsocket),FChanError,FChanECode,'SETNONBLOCK: ') then
    begin
      DoStatusCallback(self,ch_ErrorOccured,' failed/setupserved socket',-1);
      exit;
    end;
  if (FChannelType=act_TCP) and
     APSC_CheckResultSetError(APSC_SetNoDelay(Fsocket,true),FChanError,FChanECode,'SETNODELAY: ') then
       begin
         DoStatusCallback(self,ch_ErrorOccured,' failed/setupserved socket',-1);
         exit;
       end;
  FSocketAddr := APSC_sa2string(@Fsockaddr); //self.Fsocketaddr;
  FVerboseID  := FSocketAddr;
  if FSSL_Enabled then
    begin
      FBufEvent := bufferevent_openssl_socket_new(FEvBase, Fsocket, FClientSSL,BUFFEREVENT_SSL_ACCEPTING,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
      //FBufEvent := bufferevent_openssl_socket_new(base, Fsocket, FClientSSL_CTX,BUFFEREVENT_SSL_ACCEPTING,BEV_OPT_CLOSE_ON_FREE); { investigate relation to dbl add }
    end
  else
    begin
      FBufEvent := bufferevent_socket_new(FEvBase,Fsocket,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
      //FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE); { investigate relation to dbl add }
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
  if FState=ch_ACTIVE then
    DoStatusCallback(self,ch_NEW_SS_CONNECTED,'',0); { else the channel would get finalized}
end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketTCP(new_sa: TFCOM_SOCKADDRSTORAGE; new_sal: cInt; bind_sa: TFCOM_SOCKADDRSTORAGE; bind_sal: cInt; const id: TFRE_APSC_ID; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT; const status_ev: TFRE_APSC_CHANNEL_CHANGE_EVENT; const ctx_owner: TFRE_APSC_CTX_THREAD);
begin
  FClient           := true;
  FDoDNS            := false;
  Fsockaddr         := new_sa;
  Fsockaddr_len     := new_sal;
  FSocketAddr       := APSC_sa2string(@Fsockaddr);
  FVerboseID        := FSocketAddr;
  FId               := id;
  FOnStatus         := status_ev;
  FOnDisco          := discoevent;
  FOnRead           := readevent;
  FOnStatus         := status_ev;
  FOwnerCTX         := ctx_owner;
  FEvBase           := FOwnerCTX.FCB.FEventBase;
  FEvDNSBase        := FOwnerCTX.FCB.FDnsBase;
  FChannelType      := act_TCP;
  FAssignedThreadID := FOwnerCTX.FMyThreadID;
end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketDNS(const host: string; port: NativeInt; bind_sa: TFCOM_SOCKADDRSTORAGE; bind_sal: cInt; const id: TFRE_APSC_ID; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT; const status_ev: TFRE_APSC_CHANNEL_CHANGE_EVENT; const ctx_owner: TFRE_APSC_CTX_THREAD);
begin
  FClient           := true;
  FDoDNS            := True;
  Fsockaddr_len     := 0;
  FSocketAddr       := APSC_sa2string(@Fsockaddr);
  FId               := id;
  FSocketAddr       := '';
  FVerboseID        := '';
  FConnectHost      := host;
  FConnectPort      := port;
  FOnStatus         := status_ev;
  FOnDisco          := discoevent;
  FOnRead           := readevent;
  FOnStatus         := status_ev;
  FOwnerCTX         := ctx_owner;
  FEvBase           := FOwnerCTX.FCB.FEventBase;
  FEvDNSBase        := FOwnerCTX.FCB.FDnsBase;
  FChannelType      := act_TCP;
  FAssignedThreadID := FOwnerCTX.FMyThreadID;
end;

//procedure TFRE_APSC_CHANNEL.StartClientSockConnect(const base: PEvent_base; const dnsbase: Pevdns_base; manager: TFRE_APSC_CHANNEL_MANAGER);
//var
//    FChanECode : NativeInt;
//    FChanError : String;
//begin
//  try
//    Fsocket           := -1;
//    abort;
//    //FManager          := manager;
//    FAssignedThreadID := GetThreadID;
//    //FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE+BEV_OPT_DEFER_CALLBACKS);
//    FBufEvent := bufferevent_socket_new(base,Fsocket,BEV_OPT_CLOSE_ON_FREE);
//    if not assigned(FBufEvent) then
//      begin
//        FChanError:='did not get a bufferevent';
//        exit;
//      end;
//    FInputBuf  := bufferevent_get_input(FBufEvent);
//    FOutputBuf := bufferevent_get_output(FBufEvent);
//    FInBufCB   := evbuffer_add_cb(FInputBuf,@loc_buffer_cb_in,self);
//    FOutBufCB  := evbuffer_add_cb(FOutputBuf,@loc_buffer_cb_out,self);
//    bufferevent_setcb(FBufEvent,@bufev_read,nil,@bufev_event,self); // @bufev_write not used by now
//    if FDoDNS then
//      begin
//        if APSC_CheckResultSetError(bufferevent_socket_connect_hostname(FBufEvent,dnsbase,FCOM_AF_UNSPEC,Pchar(FConnectHost),FConnectPort),FChanError,FChanECode,'','') then
//          exit;
//      end
//    else
//      begin
//        if (FChannelType=act_TCP) then
//          begin
//            if APSC_CheckResultSetError(bufferevent_socket_connect(FBufEvent,@Fsockaddr,Fsockaddr_len),FChanError,FChanECode,'','') then
//              exit;
//          end
//        else
//          begin
//            abort;
//            //if APSC_CheckResultSetError(bufferevent_socket_connect(FBufEvent,@FsockaddrUnix,FsockaddrUnix_len),FChanError,FChanECode,'','') then
//            //  exit;
//          end
//      end;
//    FState     := ch_WAIT;
//    LogInfo('STARTED CLIENT CHANNEL : '+_GetDebugID,[]);
//  finally
//    if FState=ch_BAD then
//      begin
//        //BAD STATE
//        DoStatusCallback(self,ch_NEW_CHANNEL_FAILED,'',0);
//      end;
//  end;
//end;

procedure TFRE_APSC_CHANNEL.SetupClientSocketUX(const special_file: Shortstring; const id: ShortString; readevent, discoevent: TFRE_APSC_CHANNEL_EVENT; const status_ev: TFRE_APSC_CHANNEL_CHANGE_EVENT);
begin
  abort;
  if Length(special_file)>=108 then
    raise Exception.Create('the unix socket path has to be shorter then 108 chars');
  if not FileExists(special_file) then
    raise Exception.Create('could not connect path does not exist : ['+special_file+']');

  Fsockaddr.sun_family := FCOM_AF_UNIX;
  Fsockaddr.sun_path   := special_file;
  Fsockaddr_len        := 128;

  //Fsockaddr.sun_path  := trim(special_file);
  //Fsockaddr.sun_path[Length(special_file)]:=#0;
  //Fsockaddr.sun_family := FCOM_PF_UNIX; // PF_UNIX

  FClient       := True;
  FDoDNS        := false;
  Fsockaddr_len := 0;
  FSocketAddr   := '';
  FVerboseID    := '';
  FConnectHost  := special_file;
  FConnectPort  := 0;
  FId           := id;
  //FnewChanCB    := newchannelcb;
  FOnDisco      := discoevent;
  FonRead       := readevent;
end;

procedure TFRE_APSC_CHANNEL.s_Connect;  // self.FServed
var
  FChanECode : NativeInt;
  FChanError : String;

begin
  if FServed then
    begin
      DoStatusCallback(self,ch_InvalidOperation,'cannot connect a served socket',0);
      exit;
    end
  else
    begin
      try
        if Fsocket <> -1 then
          begin
            FChanError := 'a socket fd exists';
            FChanECode := -1;
            exit;
          end;
        FBufEvent := bufferevent_socket_new(FEvBase,Fsocket,BEV_OPT_CLOSE_ON_FREE);
        if not assigned(FBufEvent) then
          begin
            FChanError:='did not get a bufferevent';
            FChanECode := -1;
            exit;
          end;
        FInputBuf  := bufferevent_get_input(FBufEvent);
        FOutputBuf := bufferevent_get_output(FBufEvent);
        FInBufCB   := evbuffer_add_cb(FInputBuf,@loc_buffer_cb_in,self);
        FOutBufCB  := evbuffer_add_cb(FOutputBuf,@loc_buffer_cb_out,self);
        bufferevent_setcb(FBufEvent,@bufev_read,nil,@bufev_event,self); // @bufev_write not used by now
        if FDoDNS then
          begin
            if not assigned(FEvDNSBase) then
              GFRE_BT.CriticalAbort('must use async dns recovery');
            if APSC_CheckResultSetError(bufferevent_socket_connect_hostname(FBufEvent,FEvDNSBase,FCOM_AF_UNSPEC,Pchar(FConnectHost),FConnectPort),FChanError,FChanECode,'','') then
              exit;
          end
        else
          begin
            case FChannelType of
              act_UNIX,
              act_TCP:
                if APSC_CheckResultSetError(bufferevent_socket_connect(FBufEvent,@Fsockaddr,Fsockaddr_len),FChanError,FChanECode,'','') then
                  exit;
              //act_UDP: ;
              //act_SCTP: ;
              //act_CPU_WORK: ;
              else
                begin
                  FChanError := 's_connect / unknown channel type';
                  FChanECode := -1;
                  exit;
                end;
              end
          end;
        FState     := ch_WAIT;
        LogInfo('STARTED CLIENT CHANNEL : '+_GetDebugID,[]);
      finally
        if FState=ch_BAD then
          begin
            //BAD STATE
            DoStatusCallback(self,ch_NEW_CHANNEL_FAILED,FChanError,FChanECode);
          end;
      end;
    end;
end;

procedure TFRE_APSC_CHANNEL.s_Disconnect;
begin

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

function TFRE_APSC_CHANNEL.cs_GetChannelManager: IFRE_APSC_CHANNEL_MANAGER;
begin
  result := FOwnerCTX as TFRE_APSC_CHANNEL_MANAGER;
end;

procedure TFRE_APSC_CHANNEL.cs_StartConnect;
begin
  FOwnerCTX.cs_PushDirectCommand(TFRE_APS_CMD_CHANNEL_CRTL.FactoryCreate(GAPSC).Setup(self,cc_Connect));
end;

procedure TFRE_APSC_CHANNEL.cs_Disconnect;
begin
  FOwnerCTX.cs_PushDirectCommand(TFRE_APS_CMD_CHANNEL_CRTL.FactoryCreate(GAPSC).Setup(self,cc_Disconnect));
end;

function TFRE_APSC_CHANNEL.CH_GetConnSocketAddr: String;
begin
  ThreadCheck;
  result := FSocketAddr;
end;

function TFRE_APSC_CHANNEL.CH_GetHandleKey: cInt;
begin
  ThreadCheck;
  result := Fsocket;
end;

procedure TFRE_APSC_CHANNEL.CH_SetOnReadData(on_read: TFRE_APSC_CHANNEL_EVENT);
begin
  ThreadCheck;
  FonRead := on_read;
end;

procedure TFRE_APSC_CHANNEL.CH_SetOnDisconnnect(on_disco: TFRE_APSC_CHANNEL_EVENT);
begin
  ThreadCheck;
  FOnDisco := on_disco;
end;


function TFRE_APSC_CHANNEL.CH_GetVerboseDesc: String;
begin
  ThreadCheck;
  result := FVerboseID;
end;

procedure TFRE_APSC_CHANNEL.CH_SetVerboseDesc(const desc: string);
begin
  ThreadCheck;
  FVerboseID := desc;
end;

procedure TFRE_APSC_CHANNEL.CH_WriteString(const str: RawByteString);
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

procedure TFRE_APSC_CHANNEL.cs_WriteBuffer(const data: Pointer; const len: NativeInt);
var senc : TSAFE_WRITE_ENCAP;
begin
  abort;
  senc := TSAFE_WRITE_ENCAP.Create;
  senc.FData := Getmem(len);
  senc.FLen  := len;
  move(data^,senc.FData^,len);
  abort;
  //FManager.ScheduleCoRoutine(@COR_SafeWriteBuffer,senc);
end;

procedure TFRE_APSC_CHANNEL.cs_WriteString(const str: RawByteString);
var senc : TSAFE_WRITE_ENCAP;
    len  : NativeInt;
begin
  abort;
  senc := TSAFE_WRITE_ENCAP.Create;
  len  := length(str);
  senc.FData := Getmem(len);
  senc.FLen  := len;
  move(str[1],senc.FData^,len);
  //FManager.ScheduleCoRoutine(@COR_SafeWriteBuffer,senc);
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

function TFRE_APSC_CHANNEL.CH_ReadString: RawByteString;
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

function TFRE_APSC_CHANNEL.CH_IsClientChannel: Boolean;
begin
  ThreadCheck;
  result := FClient; //self
end;

function TFRE_APSC_CHANNEL.CH_GetState: TAPSC_ChannelState;
begin
  ThreadCheck;
  result := FState;
end;

function TFRE_APSC_CHANNEL.CH_GetID: TFRE_APSC_ID;
begin
  ThreadCheck;
  result := FId;
end;

function TFRE_APSC_CHANNEL.ch_GetListenerID: TFRE_APSC_ID;
begin
  result := FFromListenerID;
end;

procedure TFRE_APSC_CHANNEL.CH_AssociateData(const data: PtrUInt);
begin
  ThreadCheck;
  FDataTag := data;
end;

function TFRE_APSC_CHANNEL.CH_GetAssociateData: PtrUInt;
begin
  ThreadCheck;
  result := FDataTag;
end;


constructor TFRE_APSC_CHANNEL.Create(const served_channel: boolean; const enable_ssl: boolean; const ssl_ctx: PSSL_CTX; const listenerid: TFRE_APSC_ID);
begin
  FState          := ch_BAD;
  FSSL_Enabled    := enable_ssl;
  FServed         := served_channel;
  Fsocket         := -1;
  FFromListenerID := listenerid;
  if FServed and enable_ssl then
    FClientSSL :=  SSL_new(ssl_ctx);
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

procedure TFRE_APSC_CHANNEL.cs_Finalize;
begin
  if FFinalizecalled then
    exit;
  FFinalizecalled := true;
  LogInfo('FINALIZE CHANNEL CALLED - '+_GetDebugID,[]);
  inherited cs_Finalize;
end;

procedure TFRE_APSC_CHANNEL.cs_FinalizeFromPartner;
begin
  if FFinalizecalled then
    exit;
  inherited cs_Finalize; { a served socket where finalize is called true EOF/ERROR on socket}
end;

{ TFRE_APSC_CHANNEL_MANAGER }

function TFRE_APSC_CHANNEL_MANAGER.cs_GetActiveChannelCount: Nativeint;
begin
  cs_LockContextChange;
  try
    result := FChannelCount;
  finally
    cs_UnlockContectChange;
  end;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_IncActiveChannelCount;
begin
  inc(FChannelCount);
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_DecActiveChannelcount;
begin
  dec(FChannelCount);
end;

function TFRE_APSC_CHANNEL_MANAGER.GetChannelGroup: IFRE_APSC_CHANNEL_GROUP;
begin
  result := GetCGT;
end;

function TFRE_APSC_CHANNEL_MANAGER.GetCGT: TFRE_APSC_CHANNELGROUP_THREAD;
begin
  result := FMyCGT;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.ForAllChannels(const ccb: TFRE_APSC_CHANNEL_CB);
begin
  FChannellist.ForEachCall(@_DoForChannel,@ccb);
end;

procedure TFRE_APSC_CHANNEL_MANAGER._DoForChannel(cp, arg: Pointer);
begin
  TFRE_APSC_CHANNEL_CB(arg^)(TObject(cp) as TFRE_APSC_CHANNEL);
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_TerminateAndWaitfor;

  procedure FinalizeChannel(const chan : TFRE_APSC_CHANNEL);
  begin
    chan.Free;
  end;

begin
  inherited;
  ForAllChannels(@FinalizeChannel);
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_AddNewServedChannel(const fd: cint; sa: TFCOM_SOCKADDRSTORAGE; const salen: socklen_t; const use_ssl: boolean; const SSLCtx: PSSL_CTX; const listenerid: TFRE_APSC_ID);
var newchan : TFRE_APSC_CHANNEL;
begin
  s_IncActiveChannelCount;
  newchan   := TFRE_APSC_CHANNEL.Create(true,use_ssl,SSLCtx,listenerid);
  try
    newchan.SetupServedSocketEvBase(fd,sa,salen,@GAPSC._CallbackChannelEvent,self);
  except
    on e:exception do
      begin
        LogError('Setup served channel [%d] failed [%s]',[fd,e.Message]);
      end;
  end;
  if newchan.FState=ch_ACTIVE then
    begin
      if FChannelList.Add(newchan)=-1 then
        GFRE_BT.CriticalAbort('critical: channel double add ?');
    end
  else
    begin
      newchan.Free; { sends a disconnect event ... }
    end;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_AddNewClientChannel(const cl_channel: TFRE_APSC_CHANNEL);
begin
  if FChannelList.Add(cl_channel)=-1 then
    GFRE_BT.CriticalAbort('critical: channel double add ?');
  s_IncActiveChannelCount;
end;

procedure TFRE_APSC_CHANNEL_MANAGER.s_FinalizeObject(const what: TFRE_APSC_BaseObject);
begin
  if what is TFRE_APSC_CHANNEL then
    begin
      try
        if FChannellist.Remove(what)=-1 then
          LogError('Remove of channel [%s] failed [not found]',[(what as TFRE_APSC_CHANNEL).CH_GetID]);
        what.Free;
      except
        on e:exception do
          begin
            LogError('Finalize of channel [%s] failed [%s]',[(what as TFRE_APSC_CHANNEL).CH_GetID,e.Message]);
          end;
      end;
    end
  else
    inherited s_FinalizeObject(what);
end;

procedure TFRE_APSC_CHANNEL_MANAGER.a_AddClientChannel(const cl_channel: IFRE_APSC_CHANNEL);
begin
  cs_LockContextChange;
  try
    s_AddNewClientChannel(cl_channel.implementor as TFRE_APSC_CHANNEL);
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNEL_MANAGER.GetThreadID: TThreadID;
begin
  result := FMyThreadID;
end;

function TFRE_APSC_CHANNEL_MANAGER.AddChannelManagerTimer(const timer_id: TFRE_APSC_ID; interval_ms: NativeUint; timer_callback: TFRE_APSC_TIMER_CALLBACK; const start_timer: boolean; const asc_meth_code: CodePointer; const asc_meth_data: Pointer): IFRE_APSC_TIMER;
var tim : TFRE_APSC_TIMER;
begin
  tim    := TFRE_APSC_TIMER.Create(timer_id,interval_ms,timer_callback,asc_meth_code,asc_meth_data);
  result := tim;
  cs_PushDirectCommand(TFRE_APS_CMD_ADD_TIMER.FactoryCreate(GAPSC).Setup(tim,start_timer));
end;


constructor TFRE_APSC_CHANNEL_MANAGER.Create(const ID: TFRE_APSC_ID; const OwnerChannelGroupThread: TFRE_APSC_CHANNELGROUP_THREAD);
begin
  FMyIndent:='    ';
  FMyCGT       := OwnerChannelGroupThread;
  SetupCommon(id,0,FMyCGT);
  FChannellist := TFPList.Create;
  inherited create(false);
end;

destructor TFRE_APSC_CHANNEL_MANAGER.Destroy;
begin
  FChannellist.Free;
  inherited Destroy;
end;

procedure TFRE_APSC_Listener.s_AcceptCB(const what: cshort);
var req_typ : TAPSC_EV_TYP;
    new_fd  : cint;
    sa      : TFCOM_SOCKADDRSTORAGE;
    len     : socklen_t;
begin
  req_typ := APSC_le_evtyp2TAPSC_EV_TYP(what);
  if  req_typ=[acev_READ] then
    begin
      len    := sizeof(sa);
      new_fd := fcom_fpaccept(FListensock,@sa,@len);
      if APSC_CheckResultSetError(new_fd,FError,FEcode,'ACCEPT ') then
        begin
          s_Stop;
          try
             GAPSC._CallbackListenerEvent(self,als_LISTEN_ERROR);
          except on e:exception do begin
            LogError('ACCEPT CALLBACK FAILED :: '+e.Message,[]);
          end;end;
          exit;
        end;
      (FOwnerCTX as TFRE_APSC_CHANNELGROUP_THREAD).s_DistributeNewAcceptedChannel(new_fd,@sa,len,FSSL_Enabled,FSSL_CTX,FId);
    end
  else
    begin
      writeln('HANDLE UNEXPECTED LISTENER EVENT TYPE ',APSC_typ2string(APSC_le_evtyp2TAPSC_EV_TYP(what)));
    end;
end;

procedure TFRE_APSC_Listener.s_SetupEventBase(const base: PEvent_base; const immediate_start: boolean);
begin
  FAssignedThreadID := GetCurrentThreadId; // self
  FEvBase           := base;
  if immediate_start then
    s_Start;
end;

procedure TFRE_APSC_Listener.s_Start;
var res : cint;
begin
  if not assigned(FEvent)then
    FEvent := event_new(FEvBase,FListensock,EV_READ+EV_PERSIST,@INT_AcceptCB,self);
  res := event_add(FEvent,nil);
  FState := als_LISTENING;
  LogInfo('LISTENER [%s] STARTED ON '+FListenAddr,[FId]);
end;

procedure TFRE_APSC_Listener.s_Stop;
begin
  if assigned(FEvent) then
    begin
       event_del(FEvent);
       FState := als_STOPPED;
       LogInfo('LISTENER [%s] STOPPED ON '+FListenAddr,[FId]);
    end;
end;

//procedure TFRE_APSC_Listener.SetupEvent(const sa_data: PShortString; const event_base: PEvent_base);
//var len : NativeInt;
//    res : cInt;
//begin
//  FState := als_BAD;
//  SetLength(FID,30);
//  Move(sa_data^[1],FID[1],30);
//  FId:=trim(FID);
//  len := Length(sa_data^)-30;
//  if len<=128 then
//    begin
//      move(sa_data^[31],Fsockaddr,len);
//      Fsockaddr_len := len;
//      FListenAddr := APSC_sa2string(@Fsockaddr);
//      case Fsockaddr.sin_family of
//        FCOM_AF_INET:
//          begin
//            FListensock := fcom_fpsocket(FCOM_PF_INET,FCOM_SOCK_STREAM,0);
//          end;
//        FCOM_AF_INET6:
//          begin
//            FListensock := fcom_fpsocket(FCOM_PF_INET6,FCOM_SOCK_STREAM,0);
//          end;
//        else
//          begin
//            FError:='unsupported address family ('+inttostr(Fsockaddr.sin_family)+')';
//            exit;
//          end;
//      end;
//    end
//  else
//    begin
//      FError := 'unexpected sock address len ('+inttostr(len)+')';
//      exit;
//    end;
//  if FListensock = -1 then
//    begin
//      FError := APSC_TranslateOsError(fpgeterrno,'CREATE');
//      exit;
//    end;
//  try
//    if APSC_CheckResultSetError(evutil_make_listen_socket_reuseable(FListensock),FError,FEcode,'SETREUSEABLE: ') then
//      exit;
//    if APSC_CheckResultSetError(evutil_make_socket_closeonexec(FListensock),FError,FEcode,'SETCLOSEONEX: ') then
//      exit;
//    if APSC_CheckResultSetError(evutil_make_socket_nonblocking(FListensock),FError,FEcode,'SETNONBLOCK: ') then
//      exit;
//    if APSC_CheckResultSetError(fcom_fpbind(FListensock,@Fsockaddr,len),FError,FEcode,'BIND: ') then
//      exit;
//    if APSC_CheckResultSetError(fcom_fplisten(FListensock,10),FError,FEcode,'LISTEN: ') then
//      exit;
//    FEvent := event_new(event_base,FListensock,EV_READ,@INT_AcceptCB,self);
//    if not assigned(FEvent) then
//      begin
//        FError:='did not get an event';
//        exit;
//      end;
//  FState := als_STOPPED;
//  finally
//    //GAPSC._CallbackNewListener(self,als_EVENT_NEW_LISTENER);
//  end;
//end;

procedure TFRE_APSC_Listener.spc_SetupSocketData(const id: string; const sa: TFCOM_SOCKADDRSTORAGE; const sa_len: cInt; const enable_ssl: boolean; const ssl_ctx: PSSL_CTX);
begin
  if enable_ssl and (ssl_ctx=nil) then
    raise Exception.Create('ssl should be enabled on listener, but no ssl context assigned');
  FSSL_Enabled  := enable_ssl;
  FSSL_CTX      := ssl_ctx;
  FState        := als_BAD;
  Fid           := id;
  Fsockaddr     := sa;
  Fsockaddr_len := sa_len;
  case Fsockaddr.sin_family of
    FCOM_AF_INET:
        begin
          FListensock := fcom_fpsocket(FCOM_PF_INET,FCOM_SOCK_STREAM,0);
          FListenAddr := APSC_sa2string(@Fsockaddr); // self
        end;
    FCOM_AF_INET6:
        begin
         FListensock := fcom_fpsocket(FCOM_PF_INET6,FCOM_SOCK_STREAM,0);
         FListenAddr := APSC_sa2string(@Fsockaddr); // self
        end;
    FCOM_AF_UNIX:
        begin
          FListensock := fcom_fpsocket(FCOM_PF_UNIX,FCOM_SOCK_STREAM,0);
          FListenAddr := Fsockaddr.sun_path;
        end
    else
      raise Exception.Create('unsupported address family ('+inttostr(Fsockaddr.sin_family)+')');
  end;
  if FListensock = -1 then
    raise Exception.Create(APSC_TranslateOsError(fpgeterrno,'LISTEN SOCK CREATE'));
  APSC_CheckRaiseOSCall(evutil_make_listen_socket_reuseable(FListensock),'SETREUSEABLE: ');
  APSC_CheckRaiseOSCall(evutil_make_socket_closeonexec(FListensock),'SETCLOSEONEX: ');
  APSC_CheckRaiseOSCall(evutil_make_socket_nonblocking(FListensock),'SETNONBLOCK: ');
  APSC_CheckRaiseOSCall(fcom_fpbind(FListensock,@Fsockaddr,Fsockaddr_len),'BIND: ');
  APSC_CheckRaiseOSCall(fcom_fplisten(FListensock,10),'LISTEN: ');
  FState := als_STOPPED;

  //  FEvent := event_new(event_base,FListensock,EV_READ,@INT_AcceptCB,self);
  //  if not assigned(FEvent) then
  //    begin
  //      FError:='did not get an event';
  //      exit;
  //    end;
  //  FState := als_STOPPED;
  //finally
  //  //GAPSC._CallbackNewListener(self,als_EVENT_NEW_LISTENER);
  //end;

end;

procedure TFRE_APSC_Listener.cs_Start;
begin
  FOwnerCTX.cs_PushDirectCommand(TFRE_APS_CMD_LISTENER_CRTL.FactoryCreate(GAPSC).Setup(self,lc_Start));
end;

procedure TFRE_APSC_Listener.cs_Stop;
begin
  FOwnerCTX.cs_PushDirectCommand(TFRE_APS_CMD_LISTENER_CRTL.FactoryCreate(GAPSC).Setup(self,lc_Stop));
end;

function TFRE_APSC_Listener.GetState: TAPSC_ListenerState;
begin
  result := FState;
end;

function TFRE_APSC_Listener.GetErrorString: string;
begin
  result := FError;
end;

function TFRE_APSC_Listener.GetListeningAddress: string;
begin
  result := FListenAddr;
end;


constructor TFRE_APSC_Listener.Create(const channelgroup: TFRE_APSC_CHANNELGROUP_THREAD);
begin
  inherited Create;
  FOwnerCTX  := channelgroup;
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
  inherited Destroy;
end;

function TFRE_APSC_Listener.cs_GetID: TFRE_APSC_ID;
begin
  result := FId;
end;

{ TFRE_APS_LL_EvBaseController }

procedure TFRE_APS_LL_EvBaseController._CommEventfired(what: TAPSC_EV_TYP);
var cmd : TFRE_APS_CMD_BASE;
begin
  while(true) do
    begin
      try
        cmd := TObject(FWorkQ.Pop) as TFRE_APS_CMD_BASE;
        if assigned(cmd) then
          begin
            if FGoDown then
              Include(what,acev_GO_DOWN);
            FOwnerCTX.DispatchDoInSync(cmd,FOwnerCTX,what)
          end
        else
          break;
      except on
        e: exception do
          begin
            GFRE_LOG.Log('APSCOMM FATAL | '+FId+' ['+e.Message+']','',fll_Alert,'APSW',true);
            writeln('FATAL ERROR > ',FID,e.Message);
          end;
      end;
    end;
end;

procedure TFRE_APS_LL_EvBaseController._TimeoutEventfired(what: TAPSC_EV_TYP);
begin
  abort;
  //ScheduleOverload;
  //writeln('TIMEOUT FIRED '+fid+' ',APSC_typ2string(what));
end;

procedure TFRE_APS_LL_EvBaseController.csPushDirectCommand(const cmd: TFRE_APS_CMD_BASE; const event_indirect: boolean);
begin
  if cmd=nil then
    GFRE_BT.CriticalAbort('NIL PUSH');
  //writeln('PUSH ',cmd.ClassName);
  if event_indirect then
    begin
      GFRE_BT.CriticalAbort('implement indirect eventing(!)');
    end
  else
    begin
      try
        FWorkQ.Push(cmd);
        event_active(FControlEvent,EV_READ,0);
      except
        FWorkQ.Push(cmd);
        event_active(FControlEvent,EV_READ,0);
      end;
    end;
end;

procedure EventCB_TFRE_APS_LL_EvBaseController(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._CommEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;

procedure EventCB_TFRE_APS_LL_EvBaseController_TO(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._TimeoutEventfired(APSC_le_evtyp2TAPSC_EV_TYP(short));
end;


constructor TFRE_APS_LL_EvBaseController.Create(const ctx_owner: TFRE_APSC_CTX_THREAD; const create_dns_base: boolean; const id: TFRE_APSC_ID ; const need_timeout_interval: NativeUInt; const deferred_ctx_owner: TFRE_APSC_CTX_THREAD);
begin
  GFRE_TF.Get_LFQ(FWorkQ);
  FId           := uppercase(id);
  FCreateDNS    := create_dns_base;
  FEventBase    := GetANewEventBase;
  if FCreateDNS then
    FDnsBase := evdns_base_new(FEventBase,1);
  FControlEvent := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT,@EventCB_TFRE_APS_LL_EvBaseController,self);
  FTimeoutE     := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController_TO,self);
  FOwnerCTX     := ctx_owner;
  FDeferredCTX  := deferred_ctx_owner;
  event_add(FControlEvent,nil);
  if not assigned(FControlEvent) then
    GFRE_BT.CriticalAbort('APSCL - cannot init control event');
  APSC_SetupTimeout(1000,Ftimeout);
  //res := event_add(FTimeoutE,@Ftimeout);
  if not assigned(FTimeoutE) then
    GFRE_BT.CriticalAbort('APSCL - cannot init timeout event');
end;

destructor TFRE_APS_LL_EvBaseController.Destroy;
begin
  if assigned(FControlEvent) then
    event_free(FControlEvent);
  if assigned(FTimeoutE) then
    event_free(FTimeoutE);
  if assigned(FDnsBase) then
    evdns_base_free(FDnsBase,1);
  if assigned(FEventBase) then
    event_base_free(FEventBase);
  FWorkQ.Finalize;
  inherited Destroy;
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

procedure TFRE_APSC_CHANNELGROUP_THREAD.s_WorkItWork(const work: TFRE_APS_CMD_DO_WORKABLE);
var
    max : NativeInt;

  procedure MyChunking(const start_ix,end_ix,max_ix,wid: NativeInt ; const cm : TFRE_APSC_CHANNEL_MANAGER);
  begin
    FOS_IL_Increment(FWork.FWorking);
    cm.cs_PushDirectCommand(TFRE_APS_CMD_DO_WORK_CHUNK.FactoryCreate(GAPSC).Setup(start_ix,end_ix,max_ix,wid,work,self),false); { FIXXME : Make Eventindirect working for CPU CHANNELS }
  end;

begin
  if assigned(FWork) then
    GFRE_BT.CriticalAbort('apsc critical failure / unexpected work set');

  FWork          := work;
  FWork.FWorking := 0;
  max   := work.FWorkable.StartGetMaximumChunk_WIF;
  if max=0 then
    begin
      work.FWorkable.ErrorOccurred_WIF(1,'max work chunk size must be >0');
      exit;
    end;
  if max=-1 then
    s_WorkDone(false)
  else
    s_CallChannelMgrsDistribute(@MyChunking,max);
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.s_DoOrEnqueueWorkable(const work: TFRE_APS_CMD_DO_WORKABLE);
begin
  if assigned(FWork) then
    FWorkQ.Push(work)
  else
    s_WorkItWork(Work);
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_GetManagerWithMinimumConns: TFRE_APSC_CHANNEL_MANAGER;
var min : NativeInt;

    procedure findmin(var x : TFRE_APSC_CTX_THREAD);
    var acc : NativeInt;
    begin
      acc := (x as TFRE_APSC_CHANNEL_MANAGER).cs_GetActiveChannelCount;
      if acc<min then
        begin
          result := x as TFRE_APSC_CHANNEL_MANAGER;
          min    := acc;
        end;
    end;

begin
  min := maxint;
  FChildContexts.ForAll(@findmin);
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.s_DistributeNewAcceptedChannel(new_fd: cint; sa: PFCOM_SOCKADDRSTORAGE; len: socklen_t; const ssl_enabled: boolean; const ssl_ctx: PSSL_CTX; const listenerid: TFRE_APSC_ID);
var cm      : TFRE_APSC_CHANNEL_MANAGER;
begin
  { Find a CM that serves the channel }
   cm := s_GetManagerWithMinimumConns;
   cm.cs_PushDirectCommand(TFRE_APS_CMD_ADD_NEW_SRV_SOCKET.FactoryCreate(GAPSC).Setup(new_fd,sa^,len,ssl_enabled,ssl_ctx,listenerid));
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_GetChannelManagerByID(cm_id: TFRE_APSC_ID; out cm: IFRE_APSC_CHANNEL_MANAGER): boolean;

    procedure Search(var cc : TFRE_APSC_CTX_THREAD;const idx:NativeInt ; var halt_flag:boolean);
    begin
      if cc.GetID=cm_id then
        begin
          cm        := cc as TFRE_APSC_CHANNEL_MANAGER;
          halt_flag := true;
        end;
    end;

begin
  cm_id  := uppercase(cm_id);
  cm     := nil;
  result := false;
  FChildContexts.ForAllBreak2(@Search,result);
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_GetChannelManagerIDs: TFRE_APSC_ID_Array;
var idx : NativeInt;

    procedure Add(var cc : TFRE_APSC_CTX_THREAD);
    begin
      result[idx] := cc.GetID;
    end;

begin
  idx := 0;
  SetLength(result,FChildContexts.Count);
  FChildContexts.ForAll(@Add);
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_CreateNewChannelManager(const cm_id: TFRE_APSC_ID; out cm: IFRE_APSC_CHANNEL_MANAGER): boolean;
begin
  if s_GetChannelManagerByID(cm_id,cm) then
    exit(false);
  FChildContexts.Add(TFRE_APSC_CHANNEL_MANAGER.Create(cm_id,Self));
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_GetFirstChannelManager: TFRE_APSC_CHANNEL_MANAGER;
var halt : boolean;
    procedure Search(var cc : TFRE_APSC_CTX_THREAD;const idx:NativeInt ; var halt_flag:boolean);
    begin
      result    := cc as TFRE_APSC_CHANNEL_MANAGER;
      halt_flag := true;
    end;

begin
  result := nil;
  halt   := true;
  FChildContexts.ForAllBreak2(@Search,halt);
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.s_WorkDone(const ask_next_cycle: boolean);
var FNextTask : TFRE_APS_CMD_DO_WORKABLE;
    cont      : boolean;
begin
  if not assigned(FWork) then
    GFRE_BT.CriticalAbort('apsc critical failure / unexpected work is not set');
  cont := false;
  if ask_next_cycle then
    FWork.FWorkable.WorkNextCyle_WIF(cont);
  if cont then
    begin
      FNextTask := FWork;
      FWork     := nil;
    end
  else
    begin
      FWork.a_SendWorkDone;
      GAPSC.Recycle(Fwork);
      FWork     := nil;
      FNextTask := Tobject(FWorkQ.Pop) as TFRE_APS_CMD_DO_WORKABLE;
    end;
  if assigned(FNextTask) then
    s_DoOrEnqueueWorkable(FNextTask);
end;

function TFRE_APSC_CHANNELGROUP_THREAD.s_CallChannelMgrsDistribute(const cb: TFRE_APSC_CM_DISTRIBUTION_CB; const max_chunks: NativeInt): boolean;
var wid,mgrs    : NativeInt;
    chunk_sz    : NativeInt;
    partlow     : NativeInt;
    parthi      : NativeInt;
    needed_wks  : NativeInt;
    halt        : boolean;

  procedure callit(var x : TFRE_APSC_CTX_THREAD ; const idx : NativeInt ; var halt : boolean);
  begin
    if parthi>=max_chunks then
      begin
        halt := true;
        parthi := max_chunks;
      end;
    cb(partlow-1,parthi-1,max_chunks,wid-1,(x as TFRE_APSC_CHANNEL_MANAGER)); { zero base the wid, and indices }
    inc(wid);
    inc(partlow,chunk_sz);
    inc(parthi,chunk_sz);
  end;

begin
  if max_chunks=0 then
    GFRE_LOG.LogEmergency('DISTRIBUTE WORK EXCEPTION : TFRE_APSC_CHANNELGROUP_THREAD max_chunks=0');
  wid          := 1;
  partlow      := 1;
  halt         := false;
  cs_LockContextChange;
  try
    mgrs         := FChildContexts.Count;
    chunk_sz     := max_chunks div mgrs;   { 9 div 6 = 1 / 9 div 2 = 4 ; 1 div 4 = 0 ; 1 div 1 = 1 // 100 div 6 = 16 wks = 6 }
    inc(chunk_sz);                         { increment the chunksize by one, so that for 0 one chunk is worked, and that the last thread has less to do }
    needed_wks   := min(chunk_sz,mgrs);
    try
      Fwork.FWorkable.SetupWorkerCount_WIF(needed_wks);
    except
      exit(false);
    end;
    parthi       := chunk_sz;
    FChildContexts.ForAllBreak2(@callit,halt);
  finally
    cs_UnlockContectChange;
  end;
  result := true;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetCGID: TFRE_APSC_ID;
begin
  result := GetID;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetChannelManagerIDs: TFRE_APSC_ID_Array;
begin
  cs_LockContextChange;
  try
    result := s_GetChannelManagerIDs;
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetChannelManagerCount: NativeInt;
begin
  cs_LockContextChange;
  try
    result := FChildContexts.Count;
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetDefaultChannelManager: IFRE_APSC_CHANNEL_MANAGER;
begin
  cs_LockContextChange;
  try
    result := s_GetFirstChannelManager;
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetChannelManagerByID(const cm_id: TFRE_APSC_ID; out cm: IFRE_APSC_CHANNEL_MANAGER): boolean;
begin
  cs_LockContextChange;
  try
    result := s_GetChannelManagerByID(cm_id,cm);
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.GetChannelManagerMinChans: IFRE_APSC_CHANNEL_MANAGER;
begin
  cs_LockContextChange;
  try
    result := s_GetManagerWithMinimumConns;
  finally
    cs_UnlockContectChange;
  end;
end;

function TFRE_APSC_CHANNELGROUP_THREAD.CreateNewChannelManager(const cm_id: TFRE_APSC_ID; out cm: IFRE_APSC_CHANNEL_MANAGER): boolean;
begin
  cs_LockContextChange;
  try
    result := s_CreateNewChannelManager(cm_id,cm);
  finally
    cs_UnlockContectChange;
  end;
end;




function TFRE_APSC_CHANNELGROUP_THREAD.AddChannelGroupTimer(const timer_id: TFRE_APSC_ID; interval_ms: NativeInt; timer_callback: TFRE_APSC_TIMER_CALLBACK; const start_timer: boolean; const asc_meth_code: CodePointer; const asc_meth_data: Pointer): IFRE_APSC_TIMER;
var tim : TFRE_APSC_TIMER;
begin
  tim    := TFRE_APSC_TIMER.Create(timer_id,interval_ms,timer_callback,asc_meth_code,asc_meth_data);
  result := tim;
  cs_PushDirectCommand(TFRE_APS_CMD_ADD_TIMER.FactoryCreate(GAPSC).Setup(tim,start_timer));
end;

function TFRE_APSC_CHANNELGROUP_THREAD.AddListenerTCP(Bind_IP, Bind_Port: String; const ID: TFRE_APSC_ID; const spec_listener_cb: TFRE_APSC_LISTENER_CALLBACK; const start_listener: boolean; const enable_ssl: boolean; const special_ssl_ctx: PSSL_CTX): IFRE_APSC_LISTENER;
var sa      : TFCOM_SOCKADDRSTORAGE;
    len     : cInt;
    res     : cInt;
    parse   : string;
    nsl     : TFRE_APSC_Listener;
    ssl_ctx : PSSL_CTX;

begin
  ssl_ctx := GAPSC.FSSL_CTX;
  if (Bind_IP='')
     or (Bind_Port='') then
       raise exception.create('neither ip nor port can be empty, use something like *, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
  if Bind_IP='*' then
    Bind_IP := '0.0.0.0'
  else
  if Bind_IP='*6' then
    begin
      Bind_IP:='[::]';
    end;
  len   := sizeof(sa);
  parse := Bind_IP+':'+Bind_Port;
  res   := evutil_parse_sockaddr_port(Pchar(parse),@sa,len);
  if res<>0 then
    raise exception.Create('could not parse given address and port, use for the ip something like *, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
  nsl := TFRE_APSC_Listener.Create(self);
  try
    if assigned(special_ssl_ctx) then
      ssl_ctx := special_ssl_ctx;
    nsl.spc_SetupSocketData(id,sa,len,enable_ssl,ssl_ctx);
  except
    nsl.free;
    raise;
  end;
  result := nsl;
  cs_PushDirectCommand(TFRE_APS_CMD_ADD_LISTENER.FactoryCreate(GAPSC).Setup(nsl,start_listener));
end;

function TFRE_APSC_CHANNELGROUP_THREAD.AddListenerUX(special_file: ShortString; const ID: TFRE_APSC_ID; const spec_listener_cb: TFRE_APSC_LISTENER_CALLBACK; const start_listener: boolean; const enable_ssl: boolean; const special_ssl_ctx: PSSL_CTX): IFRE_APSC_LISTENER;
var ssl_ctx : PSSL_CTX;
    uxl     : TFRE_APSC_Listener;
    fcsa    : TFCOM_SOCKADDRSTORAGE;
begin
  ssl_ctx := GAPSC.FSSL_CTX;
  uxl:=TFRE_APSC_Listener.Create(self);
  try
    if assigned(special_ssl_ctx) then
      ssl_ctx := special_ssl_ctx;
    fcsa.sun_family:= FCOM_AF_UNIX;
    fcsa.sun_path   := trim(special_file);
    fcsa.sun_path[Length(special_file)]:=#0;
    if Length(special_file)>=108 then
      raise Exception.Create('the unix socket path has to be shorter then 108 chars');
    if FileExists(special_file) then
      APSC_CheckRaiseOSCall(FpUnlink(special_file),'unlink unix socket','',false);
    uxl.spc_SetupSocketData(id,fcsa,128,enable_ssl,ssl_ctx);
  except
    uxl.free;
    raise;
  end;
  result := uxl;
  cs_PushDirectCommand(TFRE_APS_CMD_ADD_LISTENER.FactoryCreate(GAPSC).Setup(uxl,start_listener));
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.DoAsyncWork(const workable: IFRE_APSC_WORKABLE);
begin
  cs_PushDirectCommand(TFRE_APS_CMD_DO_WORKABLE.FactoryCreate(GAPSC).Setup(workable,nil,workable.GetAsyncDoneContext_WIF,workable.GetAsyncDoneChannelGroup_WIF));
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.DoSyncedWork(const workable: IFRE_APSC_WORKABLE);
var fsyncdev : IFOS_E;
begin
  GFRE_TF.Get_Event(fsyncdev);
  try
    cs_PushDirectCommand(TFRE_APS_CMD_DO_WORKABLE.FactoryCreate(GAPSC).Setup(workable,fsyncdev,nil,nil));
    fsyncdev.WaitFor;
  finally
    fsyncdev.Finalize;
  end;
end;

procedure TFRE_APSC_CHANNELGROUP_THREAD.DoAsyncWorkSimpleMethod(const method: TFRE_APSC_CoRoutine; const data: Pointer);
var w : TFRE_APSC_SIMPLE_WORKABLE;
begin
  w := TFRE_APSC_SIMPLE_WORKABLE.Create(method,data);
  cs_PushDirectCommand(TFRE_APS_CMD_DO_WORKABLE.FactoryCreate(GAPSC).Setup(w,nil,nil,nil));
end;


constructor TFRE_APSC_CHANNELGROUP_THREAD.Create(const up_cg_name: ShortString ;  const channel_worker_cnt : NativeInt );
var i  : NativeInt;
    cm : IFRE_APSC_CHANNEL_MANAGER;
begin
  FMyIndent:='  ';
  GFRE_TF.Get_LFQ(FWorkQ);
  SetupCommon(up_cg_name);
  for i := 0 to channel_worker_cnt-1 do
     s_CreateNewChannelManager(up_cg_name+'/#'+inttostr(i),cm);
  inherited Create(false);
end;

destructor TFRE_APSC_CHANNELGROUP_THREAD.Destroy;
begin
  FWorkQ.Finalize;
  inherited Destroy;
end;

{ TFRE_APS_COMM }

function TFRE_APS_COMM.s_AddNewChannelGroup(cg_name: TFRE_APSC_ID; out cg: TFRE_APSC_CHANNELGROUP_THREAD; const channel_worker_cnt: NativeInt): boolean;
begin
  cg_name := uppercase(cg_name);
  if s_GetChannelgroup(cg_name,cg) then
    begin
      cg := nil;
      exit(false);
    end;
  cg := TFRE_APSC_CHANNELGROUP_THREAD.Create(cg_name,channel_worker_cnt);
  FDedicatedChannelGroups.Add(cg_name,cg);
  result:=true;
end;

function TFRE_APS_COMM.s_DeleteChannelGroup(cg_name: TFRE_APSC_ID): boolean;
var cg : TFRE_APSC_CHANNELGROUP_THREAD;
begin
  cg := TObject(FDedicatedChannelGroups.Find(uppercase(cg_name))) as TFRE_APSC_CHANNELGROUP_THREAD;
  if assigned(cg) then
    begin
      if FDedicatedChannelGroups.Remove(cg)=-1 then
        raise Exception.Create('delete channel group '+cg_name+' failed, unexpected');
      try
        cg.Free;
      except on e:Exception do
        LogError('DeleteChannelGroup Exception : %s > %s',[cg_name,e.Message]);
      end;
    end
  else
    result:=false;
end;

function TFRE_APS_COMM.s_GetChannelgroup(cg_name: TFRE_APSC_ID; out cg: TFRE_APSC_CHANNELGROUP_THREAD): boolean;
var idx : NativeInt;
begin
  cg     := TObject(FDedicatedChannelGroups.Find(uppercase(cg_name))) as TFRE_APSC_CHANNELGROUP_THREAD;
  result := assigned(cg);
end;

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
      begin
        LogError('signal CB Exception : '+e.Message,[]);
        writeln('signal CB Exception : '+e.Message);
      end;
    end;
end;

procedure TFRE_APS_COMM._CallbackChannelEvent(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  if assigned(FonNew_APSC_Channel) then
    try
      FonNew_APSC_Channel(channel,channel_event,errorstring,errorcode);
    except on e:exception do
      LogWarning('New channel CB Exception : '+e.Message,[]);
    end
  else
    try
      channel.cs_Finalize;
    except on e:exception do
      LogWarning('Channel CB Exception : '+e.Message,[]);
    end;
end;

procedure TFRE_APS_COMM._CallbackListenerEvent(const listener: IFRE_APSC_LISTENER; const listener_state: TAPSC_ListenerState);
begin

end;

//procedure TFRE_APS_COMM._CallbackNewListener(const listener: TFRE_APSC_Listener; const state: TAPSC_ListenerState);
//begin
//  if assigned(FOnNew_APSC_Listener) then
//    try
//      FOnNew_APSC_Listener(listener,state);
//    except on e:exception do
//      LogWarning('New listener CB Exception : '+e.Message,[]);
//    end
//  else
//  try
//    listener.Finalize;
//  except on e:exception do
//    LogWarning('listener CB Exception : '+e.Message,[]);
//  end;
//end;

procedure TFRE_APS_COMM.s_FinalizeMain;

  procedure RequestTerminateCG(const cg : TFRE_APSC_CTX_THREAD);
  var cgid : Shortstring;
  begin
    cgid := cg.GetID;
    writeln;
    writeln('>TERMINATING CHANNELGROUP <'+cgid+'>');
    cg.a_RequestTerminate;
    writeln('>WAITING FOR CHANNELGROUP <'+cgid+'>');
    cg.WaitFor;
    cg.Free;
    writeln('DONE <'+cgid+'>');
  end;

begin
  s_ForAllChannelGroups(@RequestTerminateCG);
  FDoneEvent.SetEvent;
end;


procedure TFRE_APS_COMM.TEST_ListenerCB(const listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  err := listener.GetErrorString;
  if state =als_EVENT_NEW_LISTENER then
    begin
      TEST_Listener := listener;
      TEST_Listener.cs_Start;
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
      TEST_Listener.cs_Start;
    end;
end;

procedure TFRE_APS_COMM.TEST_ConnectManSockCB(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  writeln('** CHANNEL EVENT : ',channel.CH_GetID,' ',channel_event,' ERR : ',errorstring,' EC : ',errorcode);
  if channel.CH_IsClientChannel then
    begin
      writeln('>>> CLIENT CHANNEL CONNECT ON MGR ',channel.cs_GetChannelManager.GetID);
      channel.CH_Enable_Reading;
      channel.ch_SetOnDisconnnect(@TEST_DiscoClientChannel);
      channel.ch_SetOnReadData(@TEST_ReadClientChannel);
    end
  else
    begin
      writeln('<<<< CHANNEL CONNECT ON MGR ',channel.cs_GetChannelManager.GetID,' PARTNER=',channel.ch_GetConnSocketAddr);
      channel.CH_WriteString('HELLO ['+channel.ch_GetConnSocketAddr+']');
      channel.CH_Enable_Reading;
      channel.CH_SetOnDisconnnect(@TEST_DiscoClientChannel);
      channel.CH_SetOnReadData(@TEST_ReadClientChannel);
    end;
end;

procedure TFRE_APS_COMM.TEST_NewCLientSock2(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  writeln('CLIENT CHANNEL CONNECT ON MGR ',channel.cs_GetChannelManager.GetID,' ',channel_event);
  if channel_event=ch_NEW_CS_CONNECTED then
    begin
      channel.CH_WriteString('HELLO SERVER');
      channel.CH_Enable_Reading;
      channel.CH_SetOnDisconnnect(@TEST_DiscoClientChannel);
      //channel.SetOnReadData(@TEST_ReadClientChannel2);
    end
  else
    begin
      channel.cs_Finalize;
    end;
end;

procedure TFRE_APS_COMM.TEST_DiscoClientChannel(const channel: IFRE_APSC_CHANNEL);
begin
  if channel.CH_IsClientChannel then
    writeln('** CLIENT CHANNEL DISCONNECT ',channel.CH_GetConnSocketAddr)
  else
    writeln('SERVED CHANNEL DISCONNECT ',channel.CH_GetConnSocketAddr);
end;

procedure TFRE_APS_COMM.TEST_ReadClientChannel(const channel: IFRE_APSC_CHANNEL);
var data : string;
begin
  if channel.CH_IsClientChannel then
    begin
      writeln('>>>> GOT ON  CS: ',channel.CH_GetVerboseDesc,' ',channel.CH_ReadString);
      channel.CH_WriteString('DONE! '+inttostr(channel.CH_GetHandleKey));
      channel.cs_Finalize;
    end
  else
    begin
      data := channel.CH_ReadString;
      writeln('<<<< GOT ON  SS: ',channel.ch_GetVerboseDesc,' ',channel.CH_ReadString);
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
      channel.cs_Finalize;
    end;
end;

procedure TFRE_APS_COMM.TEST_NewTimerCB(const timer: IFRE_APSC_TIMER);
begin

end;

var GTEST_TIMER_CNT : NativeInt=0;

procedure TFRE_APS_COMM.TEST_TIMER_TIME(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
begin
  inc(GTEST_TIMER_CNT);
  writeln(TIMER.cs_GetID,' FIRED ',flag1,' ',flag2,'  ',GTEST_TIMER_CNT);
  if GTEST_TIMER_CNT=10 then
    timer.cs_Finalize;
end;

procedure TFRE_APS_COMM.s_ForAllChannelGroups(const cg_cb: TFRE_APSC_CTX_Callback);
begin
  FDedicatedChannelGroups.ForEachCall(@s__DoForChannelGroup,@cg_cb);
end;

procedure TFRE_APS_COMM.s__DoForChannelGroup(cgp, arg: Pointer);
begin
  TFRE_APSC_CTX_Callback(arg^)(TObject(cgp) as TFRE_APSC_CHANNELGROUP_THREAD);
end;

function TFRE_APS_COMM.GetDefaultCG: IFRE_APSC_CHANNEL_GROUP;
begin
  result := FDefaultCG;
end;

function TFRE_APS_COMM.GetChannelGroupByID(CGID: TFRE_APSC_ID; out cg: IFRE_APSC_CHANNEL_GROUP): boolean;
var mcg : TFRE_APSC_CHANNELGROUP_THREAD;
begin
  FChannelGroupsChangeLock.Acquire;
  try
    cg := nil;
    result := s_GetChannelgroup(CGID,mcg);
    if result then
      cg := mcg;
  finally
    FChannelGroupsChangeLock.Release;
  end;
end;

function TFRE_APS_COMM.GetChannelGroupIDs: TFRE_APSC_ID_Array;
var i : NativeInt;
begin
  FChannelGroupsChangeLock.Acquire;
  try
    for i:= 0 to FDedicatedChannelGroups.Count-1 do
       begin
         FDedicatedChannelGroups[i]
       end;
  finally
    FChannelGroupsChangeLock.Release;
  end;
end;

function TFRE_APS_COMM.CreateNewChannelGroup(const cg_id: TFRE_APSC_ID; out cm: IFRE_APSC_CHANNEL_GROUP; const auto_workercnt: NativeInt): boolean;
var cmg : TFRE_APSC_CHANNELGROUP_THREAD;
begin
  FChannelGroupsChangeLock.Acquire;
  try
    cmg    := nil;
    result := s_AddNewChannelGroup(cg_id,cmg,auto_workercnt);
    if result then
      cm := cmg;
  finally
    FChannelGroupsChangeLock.Release;
  end;
end;

constructor TFRE_APS_COMM.Create;
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
  TFRE_APS_CMD_REQ_CTX_TERMINATE.InitSingleton;
  TFRE_APS_CMD_WORK_DONE.InitSingleton;
  GFRE_TF.Get_Event(FDoneEvent);
  GFRE_TF.Get_Lock(FChannelGroupsChangeLock);
  FDedicatedChannelGroups := TFPHashList.Create;
  if evthread_use_pthreads<>0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'could not initialize libevent to use pthreads');
  try
    SetupSSL_Ctx;
  except
    on e:Exception do
      begin
        FSSL_CTX:=nil;
        writeln('SSL Setup Failed '+e.Message);
      end;
  end;
  if not s_AddNewChannelGroup('D',FDefaultCG,C_CHANNEL_RUNNER_THREADS) then
    raise Exception.Create('could not start default channel group');

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
  FPSigaction(SIGALRM, @na, @dummy);
  FPSigaction(SIGTERM, @na, @dummy);
end;

destructor TFRE_APS_COMM.Destroy;
begin
  FDoneEvent.Finalize;
  if assigned(FSSL_CTX) then
    SSL_CTX_free(FSSL_CTX);
  TFRE_APS_CMD_REQ_CTX_TERMINATE.DestroySingleton;
  TFRE_APS_CMD_WORK_DONE.DestroySingleton;
  FDedicatedChannelGroups.Free;
  FChannelGroupsChangeLock.Finalize;
  inherited Destroy;
end;

procedure TFRE_APS_COMM.RunUntilTerminate;
begin
  FDoneEvent.WaitFor;
end;

procedure TFRE_APS_COMM.RequestTerminate(const no_jack: boolean);
begin
  LogInfo('TERMINATE REQUESTED',[]);
  if not no_jack then
    GFRE_BT.ActivateJack(cAPSC_JACK_TIMEOUT);
  s_FinalizeMain;
end;

function TFRE_APS_COMM.AddDefaultGroupTimer(const timer_id: TFRE_APSC_ID; interval_ms: NativeUint; timer_callback: TFRE_APSC_TIMER_CALLBACK; const start_timer: boolean; const asc_meth_code: CodePointer; const asc_meth_data: Pointer): IFRE_APSC_TIMER;
begin
   result :=  GetDefaultCG.AddChannelGroupTimer(timer_id,interval_ms,timer_callback,start_timer,asc_meth_code,asc_meth_data);
end;

function TFRE_APS_COMM.AddDefaultGroupListenerTCP(Bind_IP, Bind_Port: String; const ID: TFRE_APSC_ID; const spec_listener_cb: TFRE_APSC_LISTENER_CALLBACK; const start_listener: boolean; const enable_ssl: boolean; const special_ssl_ctx: PSSL_CTX): IFRE_APSC_LISTENER;
begin
  result :=  GetDefaultCG.AddListenerTCP(Bind_IP,Bind_Port,ID,spec_listener_cb,start_listener,enable_ssl,special_ssl_ctx);
end;

function TFRE_APS_COMM.AddDefaultGroupListenerUX(const special_file: shortstring; const ID: TFRE_APSC_ID; const spec_listener_cb: TFRE_APSC_LISTENER_CALLBACK; const start_listener: boolean; const enable_ssl: boolean; const special_ssl_ctx: PSSL_CTX): IFRE_APSC_LISTENER;
begin
  result :=  GetDefaultCG.AddListenerUX(special_file,id, spec_listener_cb,start_listener,enable_ssl,special_ssl_ctx);
end;

function TFRE_APS_COMM.AddClient_TCP(IP, Port: String; const ID: TFRE_APSC_ID; const auto_finalize: boolean; channelmanager: IFRE_APSC_CHANNEL_MANAGER; localChEvent: TFRE_APSC_CHANNEL_CHANGE_EVENT; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT; Bind_IP: string; Bind_Port: String): IFRE_APSC_CHANNEL;
var sa      : TFCOM_SOCKADDRSTORAGE;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    len     : cInt;
    bindlen : cInt;
    res     : cInt;
    parse   : string;
    chan    : TFRE_APSC_CHANNEL;
begin
  if (IP='')
     or (Port='') then
       raise exception.create('neither ip nor port can be empty, use something like *, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
  len := sizeof(sa);
  parse := IP+':'+Port;
  res := evutil_parse_sockaddr_port(Pchar(parse),@sa,len);
  if res<>0 then
    raise exception.Create('could not parse given IP address and port, use for the ip something like *, *6 ,127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');

  bindlen := 0;
  if Bind_ip<>'' then
    begin
      parse := Bind_IP+':'+Bind_Port;
      res := evutil_parse_sockaddr_port(Pchar(parse),@sabind,bindlen);
      if res<>0 then
        raise exception.Create('could not parse given bind address and port, use for the ip something like 127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
    end;

  if not assigned(channelmanager) then
    channelmanager := FDefaultCG.GetChannelManagerMinChans;

  if not assigned(localChEvent) then
    localChEvent := @_CallbackChannelEvent;

  chan := TFRE_APSC_CHANNEL.Create(false,false,nil);
  chan.SetupClientSocketTCP(sa,len,sabind,bindlen,id,localRead,localDisconnect,localChEvent,channelmanager.Implementor as TFRE_APSC_CHANNEL_MANAGER);
  (channelmanager.Implementor as TFRE_APSC_CHANNEL_MANAGER).a_AddClientChannel(chan);
  chan.cs_StartConnect;
end;

function TFRE_APS_COMM.AddClient_TCP_DNS(Host, Port: String; const ID: TFRE_APSC_ID; const auto_finalize: boolean; channelmanager: IFRE_APSC_CHANNEL_MANAGER; localChEvent: TFRE_APSC_CHANNEL_CHANGE_EVENT; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT; Bind_IP: string; Bind_Port: String): IFRE_APSC_CHANNEL;
var sa      : TFCOM_SOCKADDRSTORAGE;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    len     : cInt;
    bindlen : cInt;
    res     : cInt;
    parse   : string;
    chan    : TFRE_APSC_CHANNEL;
begin
  if (host='')
     or (Port='') then
       raise exception.create('neither host nor port can be empty');

  bindlen := 0;
  if Bind_ip<>'' then
    begin
      parse := Bind_IP+':'+Bind_Port;
      res := evutil_parse_sockaddr_port(Pchar(parse),@sabind,bindlen);
      if res<>0 then
        raise exception.Create('could not parse given bind address and port, use for the ip something like 127.0.0.1 or [fe80::ca2a:14ff:fe14] and a port in range');
    end;

  if not assigned(channelmanager) then
    channelmanager := FDefaultCG.GetChannelManagerMinChans;

  if not assigned(localChEvent) then
    localChEvent := @_CallbackChannelEvent;

  chan := TFRE_APSC_CHANNEL.Create(false,false,nil);
  chan.SetupClientSocketDNS(Host,strtoint(port),sabind,bindlen,id,localRead,localDisconnect,localChEvent,channelmanager.Implementor as TFRE_APSC_CHANNEL_MANAGER);
  (channelmanager.Implementor as TFRE_APSC_CHANNEL_MANAGER).a_AddClientChannel(chan);
  chan.cs_StartConnect;
end;

//var chan    : TFRE_APSC_CHANNEL;
//    sabind  : TFCOM_SOCKADDRSTORAGE;
//    bindlen : cInt;
//begin
//  abort;
//  //chan := TFRE_APSC_CHANNEL.Create(nil,nil,false,nil);
//  if not assigned(localChEvent) then
//    localChEvent := @_CallbackChannelEvent;
//  chan.SetupClientSocketDNS(Host,strtoint(port),@sabind,bindlen,id,localRead,localDisconnect,localChEvent);
//end;

function TFRE_APS_COMM.AddClient_UX(const special_file: shortstring; const ID: TFRE_APSC_ID; const auto_finalize: boolean; channelmanager: IFRE_APSC_CHANNEL_MANAGER; localChEvent: TFRE_APSC_CHANNEL_CHANGE_EVENT; localRead: TFRE_APSC_CHANNEL_EVENT; localDisconnect: TFRE_APSC_CHANNEL_EVENT): IFRE_APSC_CHANNEL;
var chan    : TFRE_APSC_CHANNEL;
    sabind  : TFCOM_SOCKADDRSTORAGE;
    bindlen : cInt;
begin
  abort;
  //chan := TFRE_APSC_CHANNEL.Create(nil,nil,false,nil);
  try
    if not assigned(localChEvent) then
      localChEvent := @_CallbackChannelEvent;
    chan.SetupClientSocketUX(special_file,id,localRead,localDisconnect,localChEvent);
  except
    chan.free;
    raise;
  end;
  //FDefaultCG._DistributeNewClientSock(chan,channelmanager);
end;

//procedure callb(dns_result : cint ; typ : cchar ; count : cint ; ttl : cint ;  addresses : pointer ; arg : pointer);cdecl;
procedure callb(dns_result : cint ; res : Pevutil_addrinfo ; arg : Pointer);cdecl;
var i    : integer;
    buf  : array [0..127] of char;
    in_a : PFCOM_InAddr;
    in_6 : PFCOM_In6Addr;
    s    : PChar;
begin
  i := 0;


  while assigned(res) do
    begin
      if res^.ai_family = FCOM_AF_INET then
        begin
          abort; // retest
          in_a := @PFCOM_SockAddrIn(res^.ai_addr)^.sin_addr;
          s    := evutil_inet_ntop(res^.ai_family,in_a,buf,128);
          writeln(i,' DNSR ',dns_result,' ',res^.ai_canonname,' * ',s);
        end
      else
      if res^.ai_family = FCOM_AF_INET6 then
        begin
          abort; // retest
          in_6 := @PFCOM_SockAddrIn6(res^.ai_addr)^.sin6_addr;
          s    := evutil_inet_ntop(res^.ai_family,in_6,buf,128);
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
  hints.ai_family   := FCOM_AF_UNSPEC;
  hints.ai_flags    := AI_CANONNAME;
  hints.ai_socktype := FCOM_SOCK_STREAM;
  hints.ai_protocol := FCOM_IPPROTO_TCP;
  evdns_getaddrinfo(FDefaultCG.FCB.FDnsBase,pchar(addrstring),nil,@hints,@callb,self);
end;

procedure TFRE_APS_COMM.SetListenerCB(const lcb: TFRE_APSC_LISTENER_CALLBACK);
begin
  FAPSC_ListenerCB := lcb;
end;

procedure TFRE_APS_COMM.SetNewChannelCB(const chancb: TFRE_APSC_CHANNEL_CHANGE_EVENT);
begin
  FonNew_APSC_Channel:=chancb;
end;

procedure TFRE_APS_COMM.SetSingnalCB(const signalcb: TOnNew_APSC_Signal);
begin
  FOnNew_APSC_Signal := signalcb;
end;

function TFRE_APS_COMM.Factor(const class_type: TFOS_FactorableClass): TFOS_FactorableBase;
var cn : ShortString;
begin { semi stupid factoy }
  cn := class_type.ClassName;
  if class_type.ClassIsSingleton then
    begin
      result := class_type.GetFactoredSingleton
    end
  else
    begin
      //writeln('FACTOR ',class_type.ClassName);
      result := class_type.Create;
    end;
end;

procedure TFRE_APS_COMM.Recycle(const obj: TFOS_FactorableBase);
begin { semi stupid factory}
  if obj.ClassIsSingleton then
    begin
     // nothing
    end
  else
    begin
      //writeln('RECYCLE ',obj.ClassName);
      obj.Free;
    end;
end;


end.
