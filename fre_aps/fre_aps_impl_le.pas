unit fre_aps_impl_le;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_INTERLOCKED,math,FRE_FCOM_SSL,
  FRE_LIBEVENT_CORE,FOS_REDBLACKTREE_GEN,FOS_GENQ,FOS_FCOM_HANDLES
  {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  {$IFDEF windows}
  ,windows
  {$ENDIF}
  ;

{.$DEFINE DEBUG_APS_THREADS}
{.$DEFINE DEBUG_APS_SCHEDULER}

{$MACRO ON}
{$IFDEF DEBUG_APS_THREADS}
  {$DEFINE DBGLOGT:=GFRE_LOG}
  {$DEFINE DBGCALC:=}
{$ELSE}
  {$DEFINE DBGLOGT:=//}
  {$DEFINE DBGCALC:=//}
{$ENDIF}
{$IFDEF DEBUG_APS_SCHEDULER}
  {$DEFINE DBGLOGS:=GFRE_LOG}
{$ELSE}
  {$DEFINE DBGLOGS:=//}
{$ENDIF}


const cMaxReportEvents        = 10;
      cMAX_SOCK_WRITE_AMOUNT  = 1024*1024*10;
      cMaxAPS_Threads         = 64;

var G_SourceID               : int64   = 1;
    C_Configured_APS_Threads : integer = 4;

type

     {$IFDEF UNIX}
     TFRE_APS_SCHEDULEREVENTS = (se_SCHEDULER,se_HUP,se_TERM,se_INT,se_PIPE);
     {$ENDIF}
     {$IFDEF windows}
     TFRE_APS_SCHEDULEREVENTS = (se_SCHEDULER);
     {$ENDIF}


//TFRE_EventSignal         =  procedure (const PES:PFRE_APS_EVENTSOURCE) of Object; // Called in Context of Scheduler (never of the Working Thread)

     TFRE_DispatchType        = (dt_SignalMethod,dt_PeriodicTimer,dt_Oneshottimer,dt_ClientSocketFunction_R,dt_ClientSocketFunction_W,
                                 dt_ServerSocketFunction_R,dt_ServerSocketFunction_W,dt_ListenSocketFunction,dt_CustomHandle,dt_ConditionFunction,
                                 dt_ConditionFunctionSpec);
     TFRE_GenericCMDType      = (dgt_Generic4One,dgt_Generic4All,dgt_Generic4Spec,dgt_Generic4OneP,dgt_Generic4AllP,dgt_Generic4SpecP);


{ TFRE_APS_EVENTSOURCE }

    TFRE_EVENT_HANDLE_RESULT = (ehr_INVALID,ehr_REENABLE,ehr_DISABLE,ehr_FREE);
    TFRE_APS_EVENT_TYPE      = (et_INVALID,et_READ,et_WRITE,et_SIGNAL,et_TIMEOUT);

    TFRE_APS_EVENTSOURCE     = class(TObject,IFRE_APS_EVENTSOURCE)
    private
      FSourceID          : QWord;
      FDispatchmode      : TFRE_DispatchMode;
      FHandleDoubleEvent : Boolean;
      FQuiesce           : NativeUint;
      FMarkFinalize      : boolean;
      FEnqueueCount      : NativeUInt;
      procedure          _Cleanup       ; virtual;
    protected

      function    _GetEventBase         : PEvent_base;
      procedure   ErrorEx               (const msg:string);
      function    IsQuiesced            : Boolean;
      function    QuiesceCheck          : Boolean;
    public
      function    GetDispatchMode       : TFRE_DispatchMode;
      constructor create                ;
      destructor  Destroy               ; override ;
      function    SourceID              : QWord;
      procedure   EventReported         (const THREAD_ID:integer;const etype:TFRE_APS_EVENT_TYPE);virtual;abstract;
      procedure   MarkFinalize          ;
      procedure   FinalizeIt            ; virtual ;

      procedure   QuiesceEventSource    ;
      procedure   Enable_Read_Pending   ; virtual ; abstract;
      procedure   Enable_Write_Pending  ; virtual ; abstract;
      procedure   Enable_Initialize     ; virtual ; abstract;
      procedure   Delete_Source         ; virtual ; abstract;
      procedure   FireEventManual       (const write:boolean=false); virtual ; abstract;
      function    EnqueCheck            : boolean; // Check if it should get enqued / not marked as to get finalized / should not happen
      procedure   IncrementEnqueueings  ; // It is enqued for a Thread
      function    DecrementEnqueueings  : NativeInt;
    end;

     { TFRE_APSES_SIGNAL }

    TFRE_APSES_SIGNAL = class(TFRE_APS_EVENTSOURCE)
    private
      FSigNum        : Integer;
      FSigEvent      : PEvent;
    public
      constructor create            (const sig_num:integer);
      procedure   EventReported     (const THREAD_ID: integer;const etype:TFRE_APS_EVENT_TYPE); override;
      destructor  Destroy           ; override ;
      procedure   Enable_Initialize ; override;
      procedure   Delete_Source     ; override;
    end;

    { TFRE_APSES_TIMER }

    TFRE_APS_TIMERMODE= (etm_INVALID,etm_ONESHOT,etm_RECURRING,etm_RECURRING_ALERTABLE);


    TFRE_APSES_TIMER = class(TFRE_APS_EVENTSOURCE,IFRE_APS_TIMER)
    private
      FEvent        : PEvent;
      FDataFunction : TFRE_DataMethod;
      FTime         : TFCOM_TimeVal;
      FData         : Pointer;
      Fhandle       : int64;
      FTimerMode    : TFRE_APS_TIMERMODE;
      FRunning      : NativeUint;
      FTimerLost    : TFRE_Method;
    public
      constructor Create               (const mode:TFRE_APS_TIMERMODE;const handle:int64; const time_ms: cardinal; const Func: TFRE_DataMethod; const Data: Pointer;const dispatch_mode:TFRE_DispatchMode;const TimerLostEv:TFRE_Method);
      procedure   EventReported        (const THREAD_ID: integer;const etype:TFRE_APS_EVENT_TYPE); override;
      procedure   ReArm                ;
      destructor  Destroy              ;override;
      procedure   FireEventManual      (const write:boolean=false);
      procedure   Enable_Initialize    ; override;
      procedure   Delete_Source        ; override;
    end;

    { TFRE_APSES_SOCKETSOURCE }

    TFRE_APSES_SOCKETSOURCE = class(TFRE_APS_EVENTSOURCE,IFRE_APS_SOCKET_EVENTSOURCE)
    private
      FSocket       : TFCOM_SOCK;
      FHandler      : TFRE_FCOM_APS_Event; //Server or Client Handler
      FIsServerSock : Boolean;
    public
      function    GetSocket           :IFCOM_SOCK;
      constructor Create              (const sock:TFCOM_SOCK ; const is_server_socket:boolean);virtual;
    end;

    { TFRE_APSES_LISTENER }

    TFRE_APSES_LISTENER = class(TFRE_APSES_SOCKETSOURCE)
    private
      FEvent                 : PEvent;
      FIsSSL                 : Boolean;
      FSSL_CTX               : PSSL_CTX;
      FInit                  : TFRE_FCOM_InitEvent;
      FTearDown              : TFRE_FCOM_TearDownEvent; //propagate to new socket
      FListenErr             : TFRE_FCOM_SocketError;
      FMySSLInfo             : TFRE_SSL_INFO;
      procedure  _CommonCreate(const init: TFRE_FCOM_InitEvent; const handler: TFRE_FCOM_APS_Event; const teardown: TFRE_FCOM_TearDownEvent; const listenerror: TFRE_FCOM_SocketError);
    public
      procedure   EventReported       (const THREAD_ID: integer;const etype:TFRE_APS_EVENT_TYPE); override;
      constructor Create              (const sock:TFCOM_SOCK;const server_sock:boolean;const init:TFRE_FCOM_InitEvent;const handler:TFRE_FCOM_APS_Event;const teardown:TFRE_FCOM_TearDownEvent;const listenerror:TFRE_FCOM_SocketError);reintroduce;
      constructor Create_SSL          (const sock:TFCOM_SOCK;const server_sock:boolean;const init:TFRE_FCOM_InitEvent;const handler:TFRE_FCOM_APS_Event;const teardown:TFRE_FCOM_TearDownEvent;const listenerror:TFRE_FCOM_SocketError ;
                                       const ssl_type: TFRE_FCOM_SSL_Type; const cerifificate_file, private_key_file, root_ca_file: string; const PasswordCB: TFRE_SSL_Callback; const verify_peer: boolean; const fail_no_peer_cert: boolean; const verify_peer_cert_once: boolean; const cipher_suites: string);reintroduce;
      destructor  Destroy             ; override;
      procedure   Enable_Initialize   ; override;
      procedure   Delete_Source       ; override;
      procedure   Enable_Read_Pending ; override;
    end;


    { TFRE_APSES_RW_SOCKET }

    TFRE_APSES_RW_SOCKET = class(TFRE_APSES_SOCKETSOURCE)
    private
      FEventRead     : PEvent;
      FEventWrite    : PEvent;
      FTearDown      : TFRE_FCOM_TearDownEvent;

      FReadRunning   : NativeUint;
      FWriteRunning  : NativeUint;
      FWriteRequests : NativeUint;
      FReadRequests  : NativeUint;
      FIsFinalizingFromClientSock : Boolean;

    public
      constructor create                (const sock:TFCOM_SOCK;const Handler:TFRE_FCOM_APS_Event;const TearDown:TFRE_FCOM_TearDownEvent;const is_server_socket:boolean);reintroduce;
      procedure   EventReported         (const THREAD_ID: integer;const etype:TFRE_APS_EVENT_TYPE); override;
      destructor  Destroy               ; override;

      procedure   Enable_Initialize     ; override;
      procedure   Enable_Read_Pending   ; override;
      procedure   Enable_Write_Pending  ; override;
      procedure   Delete_Source         ; override;
    end;

     TFRE_APS_Gen_CMD = record
       hdl  : TFRE_Generic_CMD_Handler;
       hdlP : TFRE_DataMethod;
       //cmd  : IFOS_NPS;
       cmdP : Pointer;
       cp   : integer;
     end;

     PFRE_APS_Gen_CMD=^TFRE_APS_Gen_CMD;


   type
     TFRE_APS_RUN_STATE = (frs_Idle,frs_Running,frs_Running_Prepared);
     TFRE_APS_SCHED     = class;

     { TFRE_APSTHREAD }

     TFRE_APSTHREAD=class(TThread)
     private
     type
       TFOS_WORK_E = record
         es : TFRE_APS_EVENTSOURCE;
         et : TFRE_APS_EVENT_TYPE;
       end;
       TFOS_WORK_Q = specialize TFOS_Q<TFOS_WORK_E>;
     var
       FID             : Integer;
       FCPU_SOCKET_ID  : Integer;
       FStarttime      : int64;
       Scheduler       : TFRE_APS_SCHED;
       FStartEvent     : IFOS_E;
       FMyTerminate    : boolean;

       FWorkLoadQ      : TFOS_WORK_Q;
       FWorkWorkQ      : TFOS_WORK_Q;
       FWorkLock       : IFOS_LOCK;

       FRun_Lock       : TFOS_NATIVE_LOCK;

       procedure   PushWork          (const ES:TFRE_APS_EVENTSOURCE ; RunType: TFRE_APS_EVENT_TYPE);
       procedure   CopyWork          ;
     public
      constructor  Create            (const ID:cardinal;const sch:TFRE_APS_SCHED);
      destructor   Destroy           ;override;
      procedure    Execute           ;override;
      procedure    TerminateGraceFul ;
     end;

     TFRE_ASYNC_KILL_THREAD=class(TThread)
      FTimeout : integer;
      FPid     : TPID;
      FTE      : IFOS_TE;
      FShutDownOK : boolean;
     public
      constructor create    (const timeout:integer;const pid:THandle);
      procedure   ShutDownOK;
      procedure   Execute   ;override;
     end;

     RCondEvaluations=record
       Condition  : TFRE_ConditionFunctionEvent;
       TodoMethod : TFRE_DataMethod;
       Data       : Pointer;
       CP         : Integer;
     end;

     { TFRE_APS_SCHED }
     TFRE_APS_INTERNAL_ERRORS = (GOT_NO_SOCKET,CANNOT_SET_SOCKET_BLOCKING,CANNOT_ADD_TO_WE_KQ1,CANNOT_ADD_TO_RE_KQ1,
                                 GOT_BAD_DESCRIPTOR1);

     TFRE_APS_EVENT_FEATURES   = (evf_O1,evf_EDGE_TRIGGERED,evf_FILE_DESCRIPTORS);
     TFRE_APS_EVENT_FEATURESET = set of TFRE_APS_EVENT_FEATURES;

     const
       CFRE_APS_INTERNAL_ERRORS :array [TFRE_APS_INTERNAL_ERRORS] of String = ('GOT_NO_SOCKET','CANNOT_SET_SOCKET_BLOCKING','CANNOT_ADD_TO_WE_KQ1','CANNOT_ADD_TO_RE_KQ1','GOT_BAD_DESCRIPTOR1');
     type

     TFOS_SCHEDULING_EVENTQ=specialize TFOS_Q<TFRE_SimpleCallback>;


     TFRE_APS_SCHED=class(TObject,IFRE_APS)
     private
     var
       FLE_Base              : PEvent_base;
       FLE_Features          : TFRE_APS_EVENT_FEATURESET;
       FAPSErrorCnt          :Array [TFRE_APS_INTERNAL_ERRORS] of Cardinal;
       FCTLock               :IFOS_LOCK;
       FLastproc             :Cardinal;
       FLastConcurrencyToken :TFRE_ConcurrencyToken;
       FRUN_LOCKS            :Array of TFOS_NATIVE_LOCK;
       FRUN_STATES           :Array of TFRE_APS_RUN_STATE;
       {$IFDEF UNIX}
       FSchedulerHUP         :TFRE_APSES_SIGNAL;
       FSchedulerINT         :TFRE_APSES_SIGNAL;
       FSchedulerTERM        :TFRE_APSES_SIGNAL;
       FSchedulerDISP        :TFRE_APSES_SIGNAL;
       FSchedulerPIPE        :TFRE_APSES_SIGNAL;
       {$ENDIF}
       FCondfunction         :array of RCondEvaluations;
       FThrCondfunction      :array of RCondEvaluations;
       FThrCondfunction4Thrd :array of array of RCondEvaluations;
       FChangeCount          :Cardinal;
       FBlockedTokens        :array of TFRE_ConcurrencyToken;
       FBlockedRevision      :Longint;
       //FSchedulingQ          :IFOS_LFQ;
       THRDS                 :Array [0..cMaxAPS_Threads] of TFRE_APSTHREAD;
       T_WorkDistribution    :Array [0..cMaxAPS_Threads] of NativeUint;
       FPROCESSLIST          :ARRAY of IFRE_APS_PROCESS;
       FSchedUlerID          :integer;
       FTerminate            :boolean;
       FInternalTimerhandles :int64;
       FOnQuit               :TProcedure;
       Fstate                : RFRE_APS_STATUS;
       FStarted              : boolean;
       FStartFail            : boolean;

       FSCHEDEVS             :ARRAY [low(TFRE_APS_SCHEDULEREVENTS)..high(TFRE_APS_SCHEDULEREVENTS)] of TFRE_APS_EVENTSOURCE; // Handle Sched,INT,HUP etc

     protected

       //procedure             _Schedule       ;//(const ST:PFOS_APS_SCHEDULEEVENT); // Some work is to do
       function              _TryBlockCT(const TID:integer;const CT:TFRE_ConcurrencyToken):boolean;
       procedure             _UnblockCT(const TID:integer;const CT:TFRE_ConcurrencyToken);
       procedure             _DumpTokens(const ss:string);
       procedure             _DeliverSignal(const st:integer);
       procedure             _CntErr             (const error:TFRE_APS_INTERNAL_ERRORS);inline;
       procedure            Timer_Internal_LM    (const es:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
       //procedure            PushSchedulingMethod (const Method : TFRE_SimpleCallback;const deferred_schedule:boolean=false);
       procedure            EventDispatch             (fd : evutil_socket_t ; short: cshort ; data:pointer);
       procedure            EventDispatch_DelayedFree (fd : evutil_socket_t ; short: cshort ; data:pointer);
       //procedure            HandleSchedulingEvent;
     public
       constructor Create                          ;
       destructor  Destroy                         ;override;
       function    NewEventCondition               (const continuation_method:TFRE_ConditionEventContinuation;const dm:TFRE_DispatchMode):IFRE_EVENTCONDITION;
       function    GetNewConcurrencyResourceToken  :TFRE_ConcurrencyToken;
       procedure   DropConcurrencyResourceToken    (const ct:TFRE_ConcurrencyToken);

       procedure   DC                              (const P:TFRE_ConcurrentMethod;const used_token:TFRE_ConcurrencyToken); //Do Concurrent
       procedure   DCL                             (const P:TFRE_ConcurrentLocalM;const used_token:TFRE_ConcurrencyToken); //Do Concurrent Local Method
       procedure   Start                           (const P:IFRE_APS_PROCESS);
       procedure   Run                             ; // Wait for quit signal ...
       procedure   Quit                            ;
       procedure   GetStatus                       (var state:RFRE_APS_STATUS);
       procedure   Stopthreads                     ;

       function    AddPeriodicTimer        (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer;const dispatch_mode:TFRE_DispatchMode;const TimerLostEvent:TFRE_Method=nil):IFRE_APS_TIMER;
       function    AddPeriodicSignalTimer  (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer=nil;const dispatch_mode:TFRE_DispatchMode=dm_OneWorker):IFRE_APS_TIMER;
       function    AddOneShotTimer         (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer;const dispatch_mode:TFRE_DispatchMode):IFRE_APS_TIMER;
       function    AddSocketClient         (const Target_IP: String;const TargetPort: integer; const IP_Layer: FCOM_IP_LAYER;const PROTOCOL: FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_CLIENT_HANDLER;const BindIP: String=''; const BindPort: integer=0): EFOS_FCOM_MULTIERROR;
       function    AddSocketListener       (const Bind_IP:String;const Bind_Port:integer;const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_SERVER_HANDLER;const listener_reuse:boolean;out source:IFRE_APS_SOCKET_EVENTSOURCE): EFOS_FCOM_MULTIERROR;
       function    AddSocketListener_SSL   (const Bind_IP:String;const Bind_Port:integer;const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_SERVER_HANDLER;const listener_reuse:boolean;out source:IFRE_APS_SOCKET_EVENTSOURCE ;
                                           const ssl_type:TFRE_FCOM_SSL_Type;const cerifificate_file,private_key_file,root_ca_file:string ; const PasswordCB:TFRE_SSL_Callback=nil; const verify_peer:boolean=false;const fail_no_peer_cert:boolean=false;const verify_peer_cert_once:boolean=true;const cipher_suites:string='DEFAULT'):EFOS_FCOM_MULTIERROR;

       procedure   AddCondFunctionEvent (const Condition:TFRE_ConditionFunctionEvent;const Func:TFRE_DataMethod;const Data:Pointer;const dispatch_mode:TFRE_DispatchMode=dm_Sync);
       procedure   AddCondFuncEventSpec (const Condition:TFRE_ConditionFunctionEvent;const Func:TFRE_DataMethod;const cp:integer;const TID:integer;const Data:Pointer);

       procedure   BroadCastCondition   ;
       procedure   BroadCastCondition   (const TID:integer);
       procedure   AsyncKill            ;
       procedure   Schedule_Timed_LNM    (const in_milliseconds:cardinal;const LocalNestedProc : TFRE_Local_Nested_Method;const Data:TObject);

       procedure   LogInfo              (const s:String;const Args : Array of const);
       procedure   LogWarning           (const s:String;const Args : Array of const);
       procedure   LogError             (const s:String;const Args : Array of const);
       procedure   SetOnQuitProcedure   (const M:TProcedure);

       procedure   Start_As_Thread      ;
     end;
     { TFRE_EVENTCONDITION }

     TFRE_EVENTCONDITION=class(TObject,IFRE_EVENTCONDITION) // Local Method Continuation Construct
     private
       FChanged            :Integer; // InterlockedCheck ?
       FData               :Pointer;
       FContinuationMethod :TFRE_ConditionEventContinuation;
       FContinuationLabel  :cardinal;
       FDispatchMode       :TFRE_DispatchMode;
     public
       procedure   Finalize             ;
       constructor Create               (const continuation_method:TFRE_ConditionEventContinuation;const dm:TFRE_DispatchMode);
       destructor  Destroy              ; override;
       procedure   ConditionHasChanged  ;
       function    QueryConditionChange :boolean;
       procedure   WaitCondition        (const cont_point:cardinal);inline;
       function    ContPoint            :cardinal;
       procedure   SetData              (const d:pointer);
       function    GetData              :Pointer;
     end;

     { TFRE_APS_THREAD }

     TFRE_APS_RUNNER_THREAD=class(TThread)
       constructor Create;
       procedure   Execute;override;
     end;

     procedure SetupAPS;
     procedure TearDownAPS;
     procedure Shutdown_Done;


implementation


var         FAPST           : TFRE_APS_RUNNER_THREAD;
            FGlobalBaseTime : cardinal;
            FGFRE_S         : TFRE_APS_SCHED;
            {$IFDEF UNIX}
            FKiller         :TFRE_ASYNC_KILL_THREAD;
            {$ENDIF}

type
     TLNP_Wrapper=class
       LocalNP : TFRE_Local_Nested_Method;
       Data    : TObject;
     end;

procedure EventCB (fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  //writeln('CB ',fd,' ',short);
  FGFRE_S.EventDispatch(fd,short,data);
end;
procedure EventCB_DelayedFree (fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  //writeln('CB ',fd,' ',short);
  FGFRE_S.EventDispatch_DelayedFree(fd,short,data);
end;

constructor TFRE_ASYNC_KILL_THREAD.create(const timeout: integer; const pid: THandle);
begin
  FTimeout := timeout;
  FPid     := pid;
  GFRE_TF.Get_TimedEvent(FTE);
  inherited create(false);
end;

procedure TFRE_ASYNC_KILL_THREAD.ShutDownOK;
begin
  FShutDownOK:=true;
  FTE.SetEvent;
end;

procedure TFRE_ASYNC_KILL_THREAD.Execute;
begin
  FreeOnTerminate:=true;
  FTE.WaitFor(FTimeout);
  if not FShutDownOK then begin
    writeln('GRACEFUL SHUTDOWN FAILED - KILLING');
    FpKill(FPid,SIGKILL);
  end;
  FKiller:=nil;
end;

{ TFRE_APSES_WRITE_SOCKET }


{ TFRE_APSES_RW_SOCKET }







constructor TFRE_APSES_RW_SOCKET.create(const sock: TFCOM_SOCK; const Handler: TFRE_FCOM_APS_Event; const TearDown: TFRE_FCOM_TearDownEvent;  const is_server_socket: boolean);
begin
  inherited create(sock,is_server_socket);
  if is_server_socket then begin
    FOS_IL_Increment64(FGFRE_S.Fstate.CreatedServerRES);
  end else begin
    FOS_IL_Increment64(FGFRE_S.Fstate.CreatedClientRES);
  end;
  FHandler        := Handler;
  FTearDown       := TearDown;
  FEventRead      := event_new(_GetEventBase,FSocket.GetHandleKey,EV_READ,@EventCB,self);
  if not assigned(FEventRead) then ErrorEx('could not get new read event');
  FEventWrite     := event_new(_GetEventBase,FSocket.GetHandleKey,EV_WRITE,@EventCB,self);
  if not assigned(FEventWrite) then ErrorEx('could not get new write event');
  if not FIsServerSock then begin
    Enable_Write_Pending;
  end;
end;

procedure TFRE_APSES_RW_SOCKET.EventReported(const THREAD_ID: integer; const etype: TFRE_APS_EVENT_TYPE);
var
    lOSERROR       : EFOS_OS_ERROR;
    lSOCKERR       : EFOS_OS_ERROR;
    lReadDatacount : Integer;
    continue       : boolean;
    lOffloadStatus : integer;

  function _SSL_Accept:boolean;
  var mydatacount : integer;
      want        : TFRE_FCOM_SSL_WANTS;
      amount      : integer;
  begin
    result      := true;
    mydatacount := lReadDataCount;
    repeat
      lOSERROR := FSocket.SSL_Accept;
      //writeln('SSL ACCEPT ',FSocket.GetHandleKey,' ERR ',ose,' ',mydatacount,' / ',FCurrentEventData.Data);
      case lOSERROR of
       EFOS_OS_SSL_ERROR:   begin
                              writeln(FSocket.GetHandleKey,' SSL FAIL : ',FSocket.Get_SSL_ErrorString);
                              FSocket.CloseEnqueue(99);
                              result:=false;
                              break;
                            end;
       EFOS_SSL_WANT_READ:  begin
                              if mydatacount=0 then break;
                              FSocket.SSL_ReadTransfer(mydatacount);
                              //dec(mydatacount,_SocketReadableAmount);
                              abort;
                            end;
       EFOS_SSL_WANT_WRITE: begin
                              FSocket.SSL_WriteTransfer;
                              break;
                            end;
       EFOS_OS_OK         : begin
                               FSocket.SSL_Wants(want,amount);
                               case want of
                                  fsw_NOTHING: begin
                                    result := FHandler(esv_SOCKREAD,FSocket,mydatacount);
                                    break;
                                  end;
                                  fsw_BAD,fsw_x509 : begin
                                    writeln(FSocket.GetHandleKey,'> SSL ACCEPT PROBLEM / UNEXPECTED STATE 2 ',Want);
                                    FSocket.CloseEnqueue(98);
                                    FSocket.SSL_Pending;
                                    break;
                                  end;
                                  fsw_WRITING: begin
                                    FSocket.SSL_WriteTransfer;
                                    break;
                                  end;
                                  fsw_READING: begin
                                    writeln('SSL ACCEPT PROBLEM / UNEXPECTED STATE 3',Want);
                                    FSocket.CloseEnqueue(97);
                                    break;
                                  end;
                               end;
                            end;
       else begin
          GFRE_BT.CriticalAbort('ssl-accept - logic failure');
       end;
      end;
    until false;
  end;

  procedure _ServerRead;
  begin
    lOSERROR := FSocket.Datacount(lReadDatacount);
    if lOSERROR<>EFOS_OS_OK then gfre_bt.CriticalAbort('DATACOUNT/READ ERROR %s ',[CFOS_OS_ERROR[lOSERROR]]);
    if lReadDatacount=0 then begin
      FHandler(esv_SOCKCLOSED,FSocket,0);
      exit;
    end;
    case FSocket.GetSocketReadState of
      esrs_CONNECTED: begin
        case FSocket.SSL_State of
          fss_NO_SSL : begin
            continue := FHandler(esv_SOCKREAD,FSocket,lReadDatacount);
          end;
          fss_SERVER_NOT_SETUP : begin
            continue := _SSL_Accept;
          end;
          fss_SERVER_OK: begin
            continue := FHandler(esv_SOCKREAD,FSocket,lReadDatacount);
          end;
        end;
      end
      else begin
        GFRE_BT.CriticalAbort('HANDLE IT RIGHT "GOOD READ" IN STATE = %s',[inttostr(ord(FSocket.GetSocketReadState))]);
      end;
    end;
  end;

  procedure _ClientRead;
  begin
     lOSERROR := FSocket.Datacount(lReadDatacount);
     if lOSERROR<>EFOS_OS_OK then gfre_bt.CriticalAbort('CLIENT READ DATACOUNT/READ ERROR %s ',[CFOS_OS_ERROR[lOSERROR]]);
     if lReadDatacount=0 then begin
       FHandler(esv_SOCKCLOSED,FSocket,0);
       FSocket._SetReadClosed;
       exit;
     end;
     case FSocket.GetSocketReadState of
       esrs_CONNECTED: begin
         case FSocket.SSL_State of
           fss_NO_SSL : begin
             try
               continue := FHandler(esv_SOCKREAD,FSocket,lReadDatacount);
             except
               continue := FHandler(esv_SOCKREAD,FSocket,lReadDatacount);
             end;
           end;
           fss_SERVER_OK: begin
             continue := FHandler(esv_SOCKREAD,FSocket,lReadDatacount);
           end;
           else GFRE_BT.CriticalAbort('UNKNOWN CLIENT READ SSL STATE = %s',[inttostr(ord(FSocket.SSL_State))]);
         end;
       end
       else begin
         GFRE_BT.CriticalAbort('HANDLE IT RIGHT "GOOD READ" IN STATE = %s',[inttostr(ord(FSocket.GetSocketReadState))]);
       end;
     end;
  end;

  procedure _DoOffloadWrite;
  begin
    try
      lOffloadStatus := FSocket.Offload_Write_TS(THREAD_ID);

    finally
      FOS_IL_DEC_NATIVE(FWriteRequests);
      if not FOS_IL_CAS_NATIVE(FWriteRunning,1,0) then GFRE_BT.CriticalAbort('UNEXPECTED VALUE, STOP WRITING !!!!');
    end;
    case lOffloadStatus of
      0 : ; // OK
      1 : begin
            QuiesceEventSource;
            MarkFinalize;
          end;
      4 : FSocket._RequestWriteEV; // AGAIN
      else begin
        writeln('Offloadwrite HANDLE THIS ---: ',lOffloadStatus);
      end;
    end;
  end;

  procedure _ClientWrite;
  begin
    case FSocket.GetSocketWriteState of
      esws_CONNECTING  : begin
                           FOS_IL_DEC_NATIVE(FWriteRequests);
                           try
                             lOSERROR:=FSocket.GetSocketErrorState(lSOCKERR);
                             if lOSERROR<>EFOS_OS_OK then gfre_bt.CriticalAbort('SOCKERRORSTATE/READ ERROR %s ',[CFOS_OS_ERROR[lOSERROR]]);
                             case lSOCKERR of
                               EFOS_OS_OK                 : begin
                                                              FSocket._SetReadConnected;
                                                              FSocket._SetWriteConnected;
                                                              continue := FHandler(esv_SOCKCONNECTED,FSocket,0);
                                                              FSocket._RequestReadEV;
                                                              exit;
                                                            end;
                               EFOS_OS_TIMEDOUT           : FHandler(esv_SOCKCONNTIMEDOUT,FSocket,0);
                               EFOS_OS_CONNECTION_REFUSED : FHandler(esv_SOCKCONNREFUSED, FSocket,0);
                               else                       begin
                                                            FHandler(esv_SOCKCANTCONNECT, FSocket,0);
                                                          end;
                             end;
                             //Failure - Mark ES for finalization
                             MarkFinalize;
                           finally
                             if not FOS_IL_CAS_NATIVE(FWriteRunning,1,0) then GFRE_BT.CriticalAbort('UNEXPECTED VALUE, STOP WRITING !!!!');
                           end;
                         end;
      esws_CONNECTED   : begin
                           _DoOffloadWrite;
                         end;
      else GFRE_BT.CriticalAbort('UNEXPECTED STATE IN CLIENT WRITE %d',[ord(FSocket.GetSocketReadState)]);
    end;
  end;

begin
 //.
   if QuiesceCheck then begin
     writeln('**************** --- QUISCESED ? ',FSocket.GetHandleKey);
     exit;
   end;
  try
    continue := false;
    if FIsServerSock then begin
      if etype=et_READ then begin
        if not FOS_IL_CAS_NATIVE(FReadRunning,0,1) then GFRE_BT.CriticalAbort('DOUBLE EVENTING READ !!!!');
          _ServerRead;
          FOS_IL_DEC_NATIVE(FReadRequests);
        if not FOS_IL_CAS_NATIVE(FReadRunning,1,0) then GFRE_BT.CriticalAbort('UNEXPECTED VALUE, STOP READING !!!!');
        if continue then begin
          FSocket._RequestReadEV;
        end else begin
          FSocket._SetReadClosed;
          FSocket.Offload_Close(88);
        end;
      end else
      if etype=et_WRITE then begin
        _DoOffloadWrite;
      end;
    end else begin // Client Sock
      if etype=et_READ then begin
        if not FOS_IL_CAS_NATIVE(FReadRunning,0,1) then GFRE_BT.CriticalAbort('DOUBLE EVENTING CLIENT READ !!!!');
          _ClientRead;
          FOS_IL_DEC_NATIVE(FReadRequests);
        if not FOS_IL_CAS_NATIVE(FReadRunning,1,0) then GFRE_BT.CriticalAbort('UNEXPECTED VALUE, STOP CLIENT READING !!!!');
        if continue then begin
          FSocket._RequestReadEV;
        end else begin
          FSocket._SetReadClosed;
          FSocket.Offload_Close(88);
        end;
      end else
      if etype=et_WRITE then begin
        _ClientWrite;
      end;
    end;
  except on e:exception do begin
    writeln('***SOCKET EXCEPTION |-> EXC - ',E.Message);
  end;end;
end;



destructor TFRE_APSES_RW_SOCKET.Destroy;
begin
  //writeln('**RW SOCKET FREE  ',FSocket.GetVerboseDesc);
  assert(FQuiesce>0);
  if FIsServerSock then begin
    FOS_IL_Increment64(FGFRE_S.Fstate.FreedServerRES);
  end else begin
    FOS_IL_Increment64(FGFRE_S.Fstate.FreedClientRES);
  end;
  if not FIsFinalizingFromClientSock then begin
    try
     FTearDown(FSocket);
    except on e:exception do begin
      writeln('********* SOCKET FREE ERROR -> RW SOCKET FREE  ',e.Message);
    end;end;
    FSocket.Finalize;
  end;
  Delete_Source;
  inherited Destroy;
end;

procedure TFRE_APSES_RW_SOCKET.Enable_Initialize;
begin
  Enable_Read_Pending;
end;

procedure TFRE_APSES_RW_SOCKET.Enable_Read_Pending;
begin
  FOS_IL_INC_NATIVE(FReadRequests);
  if FReadRequests<>1 then begin
    GFRE_BT.CriticalAbort('READ REQUESTS UNEXPECTED, SHOULD ONLY BE ONE');
  end;
  if FReadRunning=0 then begin
    if event_add(FEventRead,nil)<>0 then ErrorEx('could not enable socket read event');
  end;
end;

procedure TFRE_APSES_RW_SOCKET.Enable_Write_Pending;
begin
  FOS_IL_INC_NATIVE(FWriteRequests);
  if FOS_IL_CAS_NATIVE(FWriteRunning,0,1) then begin // ; (( then GFRE_BT.CriticalAbort('DOUBLE EVENTING WRITE !!!!');
    if event_add(FEventWrite,nil)<>0 then ErrorEx('could not enable socket write event');
  end else begin
    writeln('SKIPPING ENABLE_WRITE_PENDING, ALREADY IN IT ',FSocket.GetVerboseDesc);
  end;
end;

procedure TFRE_APSES_RW_SOCKET.Delete_Source;
begin
  assert(FQuiesce<>0);
  if assigned(FEventRead) then begin
    event_free(FEventRead); event_free(FEventWrite);
    FEventRead  := nil;
    FEventWrite := nil;
  end;
end;




{ TFRE_APSES_LISTENER }

function TFRE_APSES_SOCKETSOURCE.GetSocket: IFCOM_SOCK;
begin
  result := FSocket;
end;

procedure TFRE_APSES_LISTENER._CommonCreate(const init: TFRE_FCOM_InitEvent; const handler: TFRE_FCOM_APS_Event; const teardown: TFRE_FCOM_TearDownEvent; const listenerror: TFRE_FCOM_SocketError);
begin
  FEvent    := event_new(_GetEventBase,FSocket.GetHandleKey,EV_READ,@EventCB,self);
  if not assigned(FEvent) then ErrorEx('could not get new event');
  FInit     := init;
  FHandler  := handler;
  FTearDown := teardown;
  FListenErr:= listenerror;
  //FEventFinalize  := event_new(_GetEventBase,-1,0,@EventCB_DelayedFree,self);
  //if not assigned(FEventFinalize) then ErrorEx('could not get new finalize event');
end;

procedure TFRE_APSES_LISTENER.EventReported(const THREAD_ID: integer; const etype: TFRE_APS_EVENT_TYPE);

  function _FetchNewServerSock:boolean;
  var FOSError : EFOS_OS_ERROR;
      NSS      : TFCOM_SOCK;
      lOSERROR : EFOS_OS_ERROR;
  begin
     result := false;
     FOSError:=FSocket.Accept(NSS);
     if FOSError=EFOS_OS_OK then begin
      FOSError := FSocket.SetBlocking(false);
      if FOSError<>EFOS_OS_OK then begin
        GFRE_BT.CriticalAbort('SET TO NON BLOCKING ERROR IN LISTENER/ACCEPTed SOCK !!');
      end;
      FInit(NSS);
      if FIsSSL then begin
        NSS.Set_SSL_CTX(FSSL_CTX,true);
      end;
      NSS.SetEs(TFRE_APSES_RW_SOCKET.create(NSS,FHandler,FTearDown,true));
      NSS.GetES.Enable_Initialize;
      result := FHandler(esv_SOCKCONNECTED,NSS,0);
     end else begin
       writeln('******** Listener ACCEPT ERROR !!!');
       FGFRE_S._CntErr(GOT_NO_SOCKET);
       FListenErr(FSocket,FOSError);
     end;
  end;

begin
  //.
    if QuiesceCheck then exit;
    if _FetchNewServerSock then begin
      FSocket._RequestReadEV;
    end else begin
      writeln('LISTEN SERVER FAIL COULD NOT GET SOCKET');
    end;
end;

constructor TFRE_APSES_LISTENER.Create(const sock: TFCOM_SOCK; const server_sock: boolean; const init: TFRE_FCOM_InitEvent; const handler: TFRE_FCOM_APS_Event; const teardown: TFRE_FCOM_TearDownEvent; const listenerror: TFRE_FCOM_SocketError);
begin
  inherited create(sock,true);
  _CommonCreate(init,handler,teardown,listenerror);
end;

constructor TFRE_APSES_LISTENER.Create_SSL(const sock: TFCOM_SOCK; const server_sock: boolean; const init: TFRE_FCOM_InitEvent; const handler: TFRE_FCOM_APS_Event; const teardown: TFRE_FCOM_TearDownEvent; const listenerror: TFRE_FCOM_SocketError;
                                           const ssl_type: TFRE_FCOM_SSL_Type; const cerifificate_file, private_key_file, root_ca_file: string; const PasswordCB: TFRE_SSL_Callback; const verify_peer: boolean; const fail_no_peer_cert: boolean; const verify_peer_cert_once: boolean; const cipher_suites: string);
begin
  FIsSSL                            := true;
  FMySSLInfo.ssl_type               := ssl_type;
  FMySSLInfo.cerifificate_file      := cerifificate_file;
  FMySSLInfo.private_key_file       := private_key_file;
  FMySSLInfo.root_ca_file           := root_ca_file;
  FMySSLInfo.password_cb            := PasswordCB;
  FMySSLInfo.verify_peer            := verify_peer;
  FMySSLInfo.fail_no_peer_cert      := fail_no_peer_cert;
  FMySSLInfo.verify_peer_cert_once  := verify_peer_cert_once;
  FMySSLInfo.cipher_suites          := cipher_suites;
  FMySSLInfo.IsServer               := true;
  FSSL_CTX                          := FRE_Setup_SSL_Context(@FMySSLInfo);
  inherited create(sock,true);
  _CommonCreate(init,handler,teardown,listenerror);
end;

destructor TFRE_APSES_LISTENER.Destroy;
begin
  if assigned(FEvent) then begin
    event_free(FEvent);
    FEvent := nil;
  end;
  if assigned(FSocket) then begin
    FSocket.Finalize;
    FSocket:=nil;
  end;
  inherited Destroy;
end;


procedure TFRE_APSES_LISTENER.Enable_Initialize;
begin
  if event_add(FEvent,nil)<>0 then ErrorEx('could not enable event');
end;

procedure TFRE_APSES_LISTENER.Delete_Source;
begin
  if assigned(FEvent) then begin
    event_free(FEvent);
    FEvent := nil;
  end;
  FMarkFinalize := true;
end;

procedure TFRE_APSES_LISTENER.Enable_Read_Pending;
var res:cInt;
begin
  res := event_add(FEvent,nil);
  if res<>0 then begin
     GFRE_BT.CriticalAbort('Listener enable Read Failed %d',[res]);
  end;
end;





{ TFRE_APSES_SOCKETSOURCE }


constructor TFRE_APSES_SOCKETSOURCE.create(const sock: TFCOM_Sock; const is_server_socket: boolean);
begin
  Inherited Create;
  FSocket       := sock;
  FIsServerSock := is_server_socket;
end;



//procedure TFRE_APSES_SOCKETSOURCE.RemoveEventSource(const read: boolean);
//var FChangeEvent : TKEvent;
//    res          : integer;
//    Fhandle      : FCOM_MonitorHandleKey;
//begin
//  FHandle := FSocket.GetMonitorHandleKey;
//  if read then begin
//    EV_SET(@FChangeEvent,  FHandle, EVFILT_READ,EV_DELETE, 0, 0,self);
//  end else begin
//    EV_SET(@FChangeEvent,  FHandle, EVFILT_WRITE,EV_DELETE, 0, 0,self);
//  end;
//  res := kevent(FKQueue,@FChangeEvent,1,nil,0,nil);
//  if res=-1 then begin
//    res := fpgeterrno;
//    GFRE_BT.CriticalAbort('FAILURE cannot remove socket from kqueue '+CFOS_OS_ERROR[fcom_interpret_OS_Error(res)]);
//  end;
//end;



constructor TFRE_APSES_TIMER.Create(const mode: TFRE_APS_TIMERMODE; const handle: int64; const time_ms: cardinal; const Func: TFRE_DataMethod; const Data: Pointer; const dispatch_mode: TFRE_DispatchMode; const TimerLostEv: TFRE_Method);
begin
  inherited          Create;
  FTimerMode         := mode;
  Fhandle            := handle;
  FTime.tv_usec      := (time_ms mod 1000) * 1000;
  FTime.tv_sec       := time_ms div 1000;
  FDataFunction      := Func;
  FData              := Data;
  FDispatchmode      := dispatch_mode;
  FHandleDoubleEvent := true;
  FTimerLost         := TimerLostEv;
  case FTimerMode of
    etm_ONESHOT:             FEvent := event_new (_GetEventBase, -1, 0, @EventCB, self);
    etm_RECURRING:           FEvent := event_new (_GetEventBase, -1, EV_PERSIST, @EventCB, self);
    etm_RECURRING_ALERTABLE: FEvent := event_new (_GetEventBase, -1, EV_READ+EV_WRITE+EV_PERSIST, @EventCB, self);
    else abort;
  end;
  Enable_Initialize;
  event_active(FEvent,EV_TIMEOUT,0);
end;

procedure TFRE_APSES_TIMER.EventReported(const THREAD_ID: integer;const etype:TFRE_APS_EVENT_TYPE);
begin
  if QuiesceCheck then exit;
  if FTimerMode=etm_RECURRING_ALERTABLE then begin
    try
      case etype of
        et_TIMEOUT: FDataFunction(self,THREAD_ID,FData,0);
        et_READ:    FDataFunction(self,THREAD_ID,FData,1);
        et_WRITE:   FDataFunction(self,THREAD_ID,FData,2);
        else abort;
      end;
    except on e:Exception do begin
      writeln('************************** SUPER FISH RECURRING ALERTABLE -> TIMER FUNC EVENT '+e.Message);
    end;end;
  end else begin
    try
      if not FOS_IL_CAS_NATIVE(FRunning,0,1) then begin
         if assigned(FTimerLost) then begin
            FTimerLost;
         end;
         exit;
      end;
      case etype of
        et_TIMEOUT: FDataFunction(self,THREAD_ID,FData,0);
        et_READ:    FDataFunction(self,THREAD_ID,FData,1);
        et_WRITE:   FDataFunction(self,THREAD_ID,FData,2);
        else abort;
      end;
    except on e:Exception do begin
      writeln('************************** SUPER FISH : TIMER FUNC EVENT '+e.Message);
    end;end;
    if not FOS_IL_CAS_NATIVE(FRunning,1,0) then begin
      GFRE_BT.CriticalAbort('unexpected cas value [%d][ID %d] ',[Fhandle,integer(FEvent)]);
    end;
  end;
end;


procedure TFRE_APSES_TIMER.ReArm;
begin
  GFRE_BT.CriticalAbort('REARM CALLED');
end;

destructor TFRE_APSES_TIMER.Destroy;
begin
  //writeln('TIMER DESTROY');
  inherited Destroy;
end;



procedure TFRE_APSES_TIMER.FireEventManual(const write: boolean);
begin
  if write then begin
    event_active(FEvent,EV_WRITE,0);
  end else begin
    event_active(FEvent,EV_READ,0);
  end;
end;

procedure TFRE_APSES_TIMER.Enable_Initialize;
begin
  if event_add(FEvent,@FTime)<>0 then ErrorEx('timer enabling failed');
end;

procedure TFRE_APSES_TIMER.Delete_Source;
begin
  event_free(FEvent);
  FMarkFinalize := true;
end;

{ TFRE_APS_EVENTSOURCE }

//FINThreadID := THREAD_ID;
//result := true;
////  writeln('<SETDISPATCHING T ',FSourceID,' ',THREAD_ID);
//procedure TFRE_APS_EVENTSOURCE.ClearDispatching(const THREAD_ID: integer);
//begin
//  //  writeln('>CLEARDISPATCHING  ',FSourceID,' ',THREAD_ID);
//  if FINThreadID<>THREAD_ID then begin
//    writeln('V**************************** very very strange ',FINThreadID,'<>',THREAD_ID);
//  end;
//  if not FOS_IL_CAS32(FDispatching,1,0) then begin
//    GFRE_BT.CriticalAbort('DOUBLE EVENTING FAILURE -> DISPATCH NOT WORKING (B) '+ClassName);
//  end;
//  //  writeln('<CLEARDISPATCHING  ',FSourceID,' ',THREAD_ID);
//end;


procedure TFRE_APS_EVENTSOURCE._Cleanup;
begin

end;


function TFRE_APS_EVENTSOURCE._GetEventBase: PEvent_base;
begin
  result := FGFRE_S.FLE_Base;
end;

procedure TFRE_APS_EVENTSOURCE.ErrorEx(const msg: string);
begin
  raise Exception.Create(msg+' '+inttostr(FSourceID)+':'+ClassName);
end;

function TFRE_APS_EVENTSOURCE.IsQuiesced: Boolean;
begin
  result := FQuiesce>0;
end;

function TFRE_APS_EVENTSOURCE.QuiesceCheck: Boolean;
begin
  result:=false;
  if IsQuiesced then begin
    writeln('>> IGNORING QUIESCED ES ',ClassName,' ',FSourceID);
    exit(true);
  end;
end;

function TFRE_APS_EVENTSOURCE.GetDispatchMode: TFRE_DispatchMode;
begin
  result := FDispatchmode;
end;

constructor TFRE_APS_EVENTSOURCE.create;
begin
  FSourceID     := FOS_IL_Increment64(G_SourceID);
  FDispatchmode := dm_OneWorker;
end;

destructor TFRE_APS_EVENTSOURCE.Destroy;
begin
  inherited Destroy;
end;

function TFRE_APS_EVENTSOURCE.SourceID: QWord;
begin
  result := FSourceID;
end;


procedure TFRE_APS_EVENTSOURCE.MarkFinalize;
begin
  QuiesceEventSource;
  Delete_Source;
  FMarkFinalize := true; // Mark for free after the eventloop run
end;

procedure TFRE_APS_EVENTSOURCE.FinalizeIt;
begin
  FRee;
end;


procedure TFRE_APS_EVENTSOURCE.QuiesceEventSource;
var Has      : NativeUint;
begin
  //writeln('QUIESCE ',ClassName,' ');
  if not FOS_IL_CEX_NATIVE_CHK(FQuiesce,Has,1,0) then begin
   // writeln('Unexpected QuiesceES Value ',has);
  end;
end;

function TFRE_APS_EVENTSOURCE.EnqueCheck: boolean;
begin
  result := not FMarkFinalize;
  if not result then begin
    writeln('******************************************************************');
    writeln('*** ENQUEUE CHECK TRIGGERED ',classname,' ',nativeint(self),'   **');
    writeln('******************************************************************');
  end;
end;

procedure TFRE_APS_EVENTSOURCE.IncrementEnqueueings;
begin
  FOS_IL_INC_NATIVE(FEnqueueCount);
  if FEnqueueCount>1 then begin
    //writeln('******************************************************************');
    //writeln('*** ENQUEUE COUNT ',FEnqueueCount ,'                            **');
    //writeln('******************************************************************');
  end;
end;

function TFRE_APS_EVENTSOURCE.DecrementEnqueueings: NativeInt;
begin
  result := FOS_IL_DEC_NATIVE(FEnqueueCount);
end;




{ TFRE_APSES_SIGNAL }

constructor TFRE_APSES_SIGNAL.create(const sig_num: integer);
begin
  inherited Create;
  FSigNum        := sig_num;
  FSigEvent      := event_new(_GetEventBase,sig_num,EV_SIGNAL or EV_PERSIST,@EventCB,self);
  if not assigned(FSigEvent) then raise Exception.create('failed to add signal event for ('+inttostr(sig_num)+')');
  FDispatchmode  := dm_Sync;
end;


procedure TFRE_APSES_SIGNAL.EventReported(const THREAD_ID: integer; const etype: TFRE_APS_EVENT_TYPE);
begin
  if QuiesceCheck then exit;
  FGFRE_S._DeliverSignal(FSigNum);
end;


destructor TFRE_APSES_SIGNAL.Destroy;
begin
  Delete_Source;
  inherited;
end;

procedure TFRE_APSES_SIGNAL.Enable_Initialize;
begin
  if event_add(FSigEvent,nil)<>0 then ErrorEx('signal enabling failed');
end;

procedure TFRE_APSES_SIGNAL.Delete_Source;
begin
  if assigned(FSigEvent) then begin
    event_free(FSigEvent);
    FSigEvent := nil;
  end;
end;

{ TFRE_APS_THREAD }

constructor TFRE_APS_RUNNER_THREAD.Create;
begin
  inherited Create(false);
  FreeOnTerminate:=true;
end;

procedure TFRE_APS_RUNNER_THREAD.Execute;
begin
  GFRE_S.Run;
  FAPST:=nil;
end;

//procedure TFRE_APS_SCHED._Schedule;
//begin
//{$IFDEF UNIX}
//  FpKill(FSchedUlerID,SIGUSR1); // Schedule it in Sync
//{$ENDIF}
//{$IFDEF WINDOWS}
//  SetEvent(FSchedEvent);
//{$ENDIF}
//end;


//procedure TFRE_APS_SCHED._SyncSchedule(const ES: PFRE_APS_EVENTSOURCE);//inline;
//begin
// try
//  case es^.typ of
//    dt_SignalMethod   : ES^.Dispatch(ES);
//    dt_PeriodicTimer  : begin
//      ES^.DataMethod(-1,ES^.Data);
//    end;
//    dt_Oneshottimer   : begin
//      ES^.DataMethod(-1,ES^.Data);
//    end;
//    dt_ClientSocketFunction_R,dt_ClientSocketFunction_W,dt_ServerSocketFunction_R,dt_ServerSocketFunction_W,dt_ListenSocketFunction: begin
//      Event_HandleSock(-1,ES);
//    end;
//    dt_CustomHandle: begin
//      ES^.DataMethod(-1,ES^.Data);
//    end;
//    else begin
//      GFRE_BT.CriticalAbort('SyncSchedule unhandled Eventsourcetype');
//    end;
//  end;
//  ES^.Eventing:=false;
// except on e:exception do begin
//   writeln('HANDLING APS EVENT ERROR ',es^.Handle,' => ',e.Message);
//   WriteLn(GFRE_BT.DumpExceptionsBacktrace);
//   ES^.free_dynamic:=true;
// end;end;
//end;



function TFRE_APS_SCHED._TryBlockCT(const TID:integer;const CT: TFRE_ConcurrencyToken): boolean;
var i:integer;
begin
  if TID=-1 then GFRE_BT.CriticalAbort('TRYBLOCK CT NOT IN THREAD');
  if ct=0 then GFRE_BT.CriticalAbort('TRYBLOCK CT -> EMPTY CT');
  FCTLock.Acquire;
  try
    _DumpTokens('TB>');
    for i:=0 to High(FBlockedTokens) do begin
      if FBlockedTokens[i]=ct then begin
        writeln('TOKEN IN USE');
        exit(false);
      end;
    end;
    if FBlockedTokens[TID]<>0 then GFRE_BT.CriticalAbort('CONCURRENCY TOKEN NOT FREED ? CT=%d TID=%d',[FBlockedTokens[TID],TID]);
    FBlockedTokens[TID]:=ct;
    result:=true;
    _DumpTokens('<TB');
  finally
    FCTLock.Release;
  end;
end;

procedure TFRE_APS_SCHED._UnblockCT(const TID:integer;const CT: TFRE_ConcurrencyToken);
begin
  if TID=-1 then GFRE_BT.CriticalAbort('UNBLOCK CT NOT IN THREAD');
  FCTLock.Acquire;
  _DumpTokens('>UB');
  try
    if FBlockedTokens[TID]<>CT then GFRE_BT.CriticalAbort('CONCURRENCY TOKEN INVALID IN UNBLOCK ? CT=%d REPORTED CT=%d TID=%d',[FBlockedTokens[TID],CT,TID]);
    FBlockedTokens[TID]:=0;
    _DumpTokens('<UB');
  finally
    FCTLock.Release;
  end;
end;

procedure TFRE_APS_SCHED._DumpTokens(const ss: string);
var s:string;
    i:integer;
begin
  for i:=0 to High(FBlockedTokens) do begin
    s:=s+' '+inttostr(FBlockedTokens[i]);
  end;
  DBGLOGT.Log('%s>TOKEN VIEW [%s]',[ss,s],'APSW',10);
end;

procedure TFRE_APS_SCHED._DeliverSignal(const st: integer);
var i:integer;
    delivered:boolean;
begin
   delivered:=false;
   for i:=0 to Length(FPROCESSLIST)-1 do begin
     try
       case st of
         SIGHUP  : begin FPROCESSLIST[i].ReInit    ; end;
         SIGINT  : begin FPROCESSLIST[i].Interrupt ; end;
         SIGTERM : begin FPROCESSLIST[i].Terminate ; end;
         SIGUSR1 : begin
                     GFRE_BT.CriticalAbort('Thanks for choosing the self destruction programme. Commencing suicide now.');
                   end;
       end;
       delivered:=true;
     except on e:exception do begin
       writeln('SIG DISPATCH EX ',e.Message);
       writeln(GFRE_BT.DumpExceptionsBacktrace);
     end;end;
   end;
   if not delivered then Quit;
end;


procedure TFRE_APS_SCHED._CntErr(const error: TFRE_APS_INTERNAL_ERRORS);
begin
  FOS_IL_Increment(FAPSErrorCnt[error]);
end;

procedure TFRE_APS_SCHED.Timer_Internal_LM(const es: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var lnp : TLNP_Wrapper;
begin
  try
   try
       lnp := TLNP_Wrapper(data);
       lnp.LocalNP(lnp.Data);
   except
   end;
  finally
    lnp.free;
  end;
end;

//procedure TFRE_APS_SCHED.PushSchedulingMethod(const Method: TFRE_SimpleCallback;const deferred_schedule:boolean=false);
//begin
//  FScheduleLock.Acquire;
//  try
//    case FLockedSchedQ.Push(Method) of
//      cs_OK      : ;
//      cs_NOSPACE : raise Exception.Create('Pushschedulingmethod failed / no space used is ='+inttostr(FLockedSchedQ.Count));
//      else raise Exception.Create('Pushschedulingmethod failed / unexpected LockedQ result');
//    end;
//  finally
//    FScheduleLock.Release;
//  end;
//  if not deferred_schedule then begin
//    _Schedule;
//  end;
//end;

procedure  TFRE_APS_SCHED.EventDispatch(fd : evutil_socket_t ; short: cshort ; data:pointer);
var ES          : TFRE_APS_EVENTSOURCE;
     k          : integer;

    procedure PushDistributedToThread(event_type  : TFRE_APS_EVENT_TYPE);
    var i           : integer;
        min_load    : NativeUint;
        sel_thread  : integer;
        T           : TFRE_APSTHREAD;
    begin
      min_load := high(NativeUint);
      for i :=0 to C_Configured_APS_Threads-1 do begin
        if T_WorkDistribution[i]<min_load then begin
          min_load    := T_WorkDistribution[i];
          sel_thread  := i;
        end;
      end;
      if (sel_thread<0) or (sel_thread>C_Configured_APS_Threads) then raise Exception.Create('scheduling fish selected thread '+inttostr(sel_thread));
      inc(T_WorkDistribution[sel_thread]);
      if T_WorkDistribution[sel_thread]=0 then begin
        for i:=0 to C_Configured_APS_Threads-1 do begin
          writeln('<HARD RESETTING WORK DISTRIBUTION ',i,' CNT = ',T_WorkDistribution[i]);
          T_WorkDistribution[i] := 0;
          writeln('>HARD RESETTING WORK DISTRIBUTION ',i,' CNT = ',T_WorkDistribution[i]);
        end;
      end;
      es.EnqueCheck;
      es.IncrementEnqueueings;
      THRDS[sel_thread].PushWork(es,event_type);
    end;

begin
  k:=0;
  es := TObject(data) as TFRE_APS_EVENTSOURCE;
  case es.GetDispatchMode of
    dm_Sync      : begin
                     es.EventReported(0,et_SIGNAL);
                     inc(k);
                   end;
    dm_OneWorker : begin
                      if (short and EV_READ)    >0 then begin
                         PushDistributedToThread(et_READ);
                         inc(k);
                      end;
                      if (short and EV_WRITE)   >0 then begin
                         PushDistributedToThread(et_WRITE);
                         inc(k);
                      end;
                      if (short and EV_TIMEOUT) >0 then begin
                         PushDistributedToThread(et_TIMEOUT);
                         inc(k);
                      end;
                      if (short and EV_SIGNAL)  >0 then begin
                         PushDistributedToThread(et_SIGNAL);
                         inc(k);
                      end;
                   end;
    else GFRE_BT.CriticalAbort('invalid dispatching mode set %d',[ord(es.GetDispatchMode)]);
  end;
  if k>1 then begin
    writeln('**********************');
    writeln('!!EVENT ',es.ClassName,' ',nativeuint(es),' enqueued ',k,' times.');
    writeln('**********************');
  end;
end;

procedure TFRE_APS_SCHED.EventDispatch_DelayedFree(fd: evutil_socket_t; short: cshort; data: pointer);
var es:TFRE_APS_EVENTSOURCE;
begin
  es := TObject(data) as TFRE_APS_EVENTSOURCE;
  es.free;
end;

//procedure TFRE_APS_SCHED.HandleSchedulingEvent;
//var method      : TFRE_SimpleCallback;
//    obj         : TFRE_APSES_RW_SOCKET;
//    m           : Tmethod;
//    lSearchSock : pointer;
//
//    procedure MySearch(const item : TFRE_SimpleCallback);
//    var searchm :TMethod;
//    begin
//      searchm := Tmethod(item);
//      writeln('  > ',TObject(searchm.Data).ClassName);
//      if searchm.Data=lSearchSock then begin
//         writeln('FOUND PPPPPPPPPPPPPPPPPPPPPPPPPPPPPP');
//      end;
//    end;
//
//begin
//  FScheduleLock.Acquire;
//  try
//    repeat
//      case FLockedSchedQ.Pop(method) of
//        cs_OK      : begin
//                       case FWorkSchedQ.Push(method) of
//                         cs_OK      : ;
//                         cs_NOSPACE : raise Exception.Create('FWorkSchedQ dimesioning error too less space count='+inttostr(FWorkSchedQ.Count));
//                         else raise Exception.Create('FWorkSchedQ unexpected result');
//                       end;
//                     end;
//        cs_ZEROPOP : break;
//        else raise Exception.Create('FLockedSchedQ unexpected result');
//      end;
//    until false;
//  finally
//    FScheduleLock.Release;
//  end;
//  repeat
//    case FWorkSchedQ.Pop(method) of
//       cs_OK      : begin
//                      m := TMethod(method);
//                      if TObject(m.Data) is TFRE_APSES_RW_SOCKET then begin
//                        if TMethod(@TFRE_APSES_RW_SOCKET(m.Data).SchedulingFreeDelayed).Code=m.Code then begin
//                          //writeln('GOT IT');
//                          //writeln('-- SEARCHRUN -- ',FWorkSchedQ.Count);
//                            lSearchSock := m.data;
//                            FWorkSchedQ.SearchForAllItems(@MySearch);
//                          //writeln('-- SEARCHRUN --');
//                        end;
//                      end;
//                      method();
//                    end;
//       cs_ZEROPOP : break;
//       cs_NOSPACE : raise Exception.Create('FWorkSchedQ dimesioning error too less space count='+inttostr(FWorkSchedQ.Count));
//       else raise Exception.Create('FWorkSchedQ unexpected result');
//    end;
//  until false;
//end;

constructor TFRE_APS_SCHED.Create;
var
  i: Integer;

    {$IFDEF UNIX}
      procedure _setupUnix;
      begin
        FSchedulerID   := FpGetpid;
        FSchedulerHUP  := TFRE_APSES_SIGNAL.Create(SIGHUP);
        FSchedulerINT  := TFRE_APSES_SIGNAL.Create(SIGINT);
        FSchedulerTERM := TFRE_APSES_SIGNAL.Create(SIGTERM);
        FSchedulerPIPE := TFRE_APSES_SIGNAL.Create(SIGPIPE);
        FSchedulerDISP := TFRE_APSES_SIGNAL.Create(SIGUSR1);
        FSchedulerHUP.Enable_Initialize;
        FSchedulerINT.Enable_Initialize;
        FSchedulerTERM.Enable_Initialize;
        FSchedulerDISP.Enable_Initialize;
        FSchedulerPIPE.Enable_Initialize;
      end;
    {$ENDIF}

  procedure _SetupEventBase;
  var cfg  : Pevent_config;
      res  : Integer;
      feat : Integer;
  begin
    //LogInfo('SETTING UP EVENT BASE - LIBEVENT (%s)',[event_get_version]);
    res      := evthread_use_pthreads;
    if res<>0 then begin
      GFRE_BT.CriticalAbort('cannot enable thread support for libevent');
    end;
    cfg      := event_config_new;
    res      := event_config_require_features(cfg, EV_FEATURE_ET or EV_FEATURE_O1);
    FLE_Base := event_base_new_with_config(cfg);
    event_config_free(cfg);
    if not assigned(FLE_Base) then begin
      LogWarning('Cannot get features O1, ET, using fallback',[]);
      FLE_Base := event_base_new;
      if not assigned(FLE_Base) then GFRE_BT.CriticalAbort('COULD NOT AQUIRE EVENTBASE!');
    end;
    feat := event_base_get_features(FLE_Base);
    if (feat and EV_FEATURE_ET )>0 then  Include(FLE_Features,evf_EDGE_TRIGGERED);
    if (feat and EV_FEATURE_O1 )>0 then  Include(FLE_Features,evf_O1);
    if (feat and EV_FEATURE_FDS)>0 then  Include(FLE_Features,evf_FILE_DESCRIPTORS);
    //LogInfo('Eventbase Method : [%s %s  %s  %s ]',[string(event_base_get_method(FLE_Base)),BoolToStr(evf_EDGE_TRIGGERED in FLE_Features,'EDGE TRIGGERED','NOT EDGE TRIGGERED'),
                                                                                           //BoolToStr(evf_O1 in FLE_Features,'O(1)','O(?)'),
                                                                                           //BoolToStr(evf_FILE_DESCRIPTORS in FLE_Features,'WITH FILEDESCRIPTORS','NO FILEDESCRIPTORS')]);
  end;



begin
  inherited create;
  FGFRE_S := self;
  GFRE_TF.Get_Lock(FCTLock);

  //GFRE_TF.Get_Lock(FScheduleLock);
  //FLockedSchedQ.Initialize(1024);
  //FWorkSchedQ.Initialize(1024);

  FGlobalBaseTime  :=GFRE_BT.Get_Ticks_ms;
  FInternalTimerhandles:=0;
  FLastproc:=0;
  {$IFDEF UNIX}
  FSchedUlerID:=FpGetpid;
  {$ENDIF}
  _SetupEventBase;
  {$IFDEF UNIX}
  _setupUnix;
  {$ENDIF}
  FLastConcurrencyToken:=0;
  SetLength(FRUN_LOCKS,length(THRDS));
  SetLength(FRUN_STATES,Length(THRDS));
  for i:=0 to C_Configured_APS_Threads-1 do begin
    THRDS[i]  := TFRE_APSTHREAD.Create(i,self);
  end;
  setlength(FBlockedTokens,GFRE_CPU.Logical);
end;


//function foreach_event_cb(const base : PEvent_base ; const event : PEvent ; const data : Pointer):cInt; cdecl;
//var ES:TFRE_APS_EVENTSOURCE;
//begin
//  ES := TFRE_APS_EVENTSOURCE(event_get_callback_arg(event));
//  writeln('ES ',es.ClassName);
//  TList(data).Add(ES);
//end;

destructor TFRE_APS_SCHED.Destroy;
var i   : Integer;
    err : TFRE_APS_INTERNAL_ERRORS;
    L   : TList;

begin
  for i:=0 to C_Configured_APS_Threads-1 do begin
    THRDS[i].TerminateGraceFul;
    THRDS[i].WaitFor;
    THRDS[i].Free;
  end;
  for i:=0 to High(FPROCESSLIST) do begin
    FPROCESSLIST[i].Finalize;
  end;

  //L:=TList.Create;
  //event_base_foreach_event(FLE_Base,nil,@L);
  //L.free;

  event_base_free(FLE_Base);
  LogInfo('APS LE INTERNAL ERROR STATS',[]);
  for err:=low(TFRE_APS_INTERNAL_ERRORS) to high(TFRE_APS_INTERNAL_ERRORS) do begin
    LogInfo('   %30.30s : %d',[CFRE_APS_INTERNAL_ERRORS[err],FAPSErrorCnt[err]]);
  end;
  {$IFDEF UNIX}
  FSchedulerDISP.Free;
  FSchedulerHUP.Free;
  FSchedulerINT.Free;
  FSchedulerTERM.Free;
  FSchedulerPIPE.Free;
  {$ENDIF}
  FCTLock.Finalize;
  //FScheduleLock.Finalize;
  inherited;
end;

function TFRE_APS_SCHED.NewEventCondition(const continuation_method: TFRE_ConditionEventContinuation; const dm: TFRE_DispatchMode): IFRE_EVENTCONDITION;
begin
  Result:=TFRE_EVENTCONDITION.Create(continuation_method,dm);
end;

{ TFRE_APS_SCHED }

function TFRE_APS_SCHED.GetNewConcurrencyResourceToken: TFRE_ConcurrencyToken;
begin
  FOS_IL_Increment64(Int64(FLastConcurrencyToken));
  result:=FLastConcurrencyToken;
end;

procedure TFRE_APS_SCHED.DropConcurrencyResourceToken(const ct: TFRE_ConcurrencyToken);
begin
 //ignore
end;

procedure TFRE_APS_SCHED.DC(const P: TFRE_ConcurrentMethod;const used_token: TFRE_ConcurrencyToken);
begin

end;

procedure TFRE_APS_SCHED.DCL(const P: TFRE_ConcurrentLocalM;const used_token: TFRE_ConcurrencyToken);
begin

end;

procedure TFRE_APS_SCHED.Start(const P: IFRE_APS_PROCESS);
begin
  try
   SetLength(FPROCESSLIST,Length(FPROCESSLIST)+1);
   FPROCESSLIST[FLASTPROC]:=P;
   P.Setup;
   inc(FLASTPROC);
  except on e:Exception do begin
    FStartFail:=true;
    GFRE_BT.CriticalAbort('Cant launch a <%s> process due to [%s] ',[P.GetName,e.Message],true);
  end;end;
end;

procedure TFRE_APS_SCHED.Run;
var res         : integer;
    i: Integer;

  procedure _SetupSignals;
  var ign,dummy: SigactionRec;
  begin
    ign.sa_handler:=SigActionHandler(SIG_IGN);
    ign.sa_flags:=0;
    fpsigemptyset(ign.sa_mask);
    FPsigaction(SIGPIPE, @ign, @dummy);
    FPsigaction(SIGINT,  @ign, @dummy);
    //FPSigaction(SIGUSR1, @ign, @dummy);
    //FPSigaction(SIGHUP,  @ign, @dummy);
    //FPSigaction(SIGTERM, @ign, @dummy);
  end;

begin
  if FStartFail then begin
    writeln('START FAILURE');
    exit;
  end;
  FStarted:=True;
  {$IFDEF UNIX}
  //_SetupSignals;
  {$ENDIF}
  res := event_base_loop(FLE_Base,0);
  if res<>0 then GFRE_BT.CriticalAbort('MAIN EVENT LOOP FAILED '+inttostr(res));
  if FTerminate then begin
    LogInfo('TERMINATE REQUESTED',[]);
  end;
  Stopthreads;
  if assigned(FOnQuit) then begin
    try
      FOnQuit;
    except
    end;
  end;
end;

procedure TFRE_APS_SCHED.Quit;
var i: Integer;
begin
  if FTerminate then exit;
  FTerminate:=true;
  event_base_loopbreak(FLE_Base);
end;

procedure TFRE_APS_SCHED.GetStatus(var state: RFRE_APS_STATUS);
begin
  state := Fstate;
end;

procedure TFRE_APS_SCHED.Stopthreads;
var i:integer;
begin
  for i:=0 to C_Configured_APS_Threads-1 do begin
    THRDS[i].TerminateGraceFul;
  end;
end;





//procedure TFRE_APS_SCHED.AddCustomHandle(const Handle: THandle;const Func: TFRE_DataMethod; const Data: Pointer;const dispatch_mode: TFRE_DispatchMode);
//var PES:PFRE_APS_EVENTSOURCE;
//begin
//  if not assigned(func) then GFRE_BT.CriticalAbort('must provide a function pointer');
//  PES:=NewEventSource;
//  PES^.typ:=dt_CustomHandle;
//  PES^.mode:=dispatch_mode;
//  PES^.Handle:=Handle;
//  PES^.DataMethod :=Func;
//  PES^.Data:=Data;
//  AddEventSource(PES);
//end;



function TFRE_APS_SCHED.AddPeriodicTimer(const time_ms: cardinal; const Func: TFRE_DataMethod; const Data: Pointer; const dispatch_mode: TFRE_DispatchMode; const TimerLostEvent: TFRE_Method): IFRE_APS_TIMER;
var Timer  : TFRE_APSES_TIMER;
    handle : int64;
begin
  handle := FOS_IL_Increment64(FInternalTimerhandles);
  Timer  := TFRE_APSES_TIMER.Create(etm_RECURRING,handle,time_ms,func,data,dispatch_mode,TimerLostEvent);
  result := Timer;
end;

function TFRE_APS_SCHED.AddPeriodicSignalTimer(const time_ms: cardinal; const Func: TFRE_DataMethod; const Data: Pointer; const dispatch_mode: TFRE_DispatchMode): IFRE_APS_TIMER;
var Timer  : TFRE_APSES_TIMER;
    handle : int64;
begin
  handle := FOS_IL_Increment64(FInternalTimerhandles);
  Timer  := TFRE_APSES_TIMER.Create(etm_RECURRING_ALERTABLE,FInternalTimerhandles,time_ms,func,data,dispatch_mode,nil);
  result := Timer;
end;

function TFRE_APS_SCHED.AddOneShotTimer(const time_ms: cardinal; const Func: TFRE_DataMethod; const Data: Pointer;const dispatch_mode:TFRE_DispatchMode):IFRE_APS_TIMER;
var Timer  : TFRE_APSES_TIMER;
    handle : int64;
begin
  handle := FOS_IL_Increment64(FInternalTimerhandles);
  Timer  := TFRE_APSES_TIMER.Create(etm_ONESHOT,FInternalTimerhandles,time_ms,func,data,dispatch_mode,nil);
  result := Timer;
end;

function TFRE_APS_SCHED.AddSocketClient(const Target_IP: String; const TargetPort: integer; const IP_Layer: FCOM_IP_LAYER; const PROTOCOL: FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_CLIENT_HANDLER;const BindIP: String=''; const BindPort: integer=0): EFOS_FCOM_MULTIERROR;
var AI:IFCOM_AI;
      sw,sr:fcom_int;
      ra:boolean;
      ip:string;
      port:integer;
      CLIENT_SOCK:TFCOM_SOCK;
      FOSError:EFOS_OS_ERROR;
      ct:TFRE_ConcurrencyToken;
begin
  AI:=GFRE_FF.New_FCOM_AI;
  case AI.ResolveandSet(Target_IP,TargetPort) of
    fat_INVALID,fat_UNSPEC: begin
     result:=ese_CANNOT_RESOLVE;
     AI.Finalize;
     exit;
    end;
  end;
  CLIENT_SOCK:=GFRE_FF.New_FCOM_NETSOCK(IP_Layer,PROTOCOL,FOSError).GetImplementor as TFCOM_SOCK;
  if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_CREATESOCKET;_CntErr(GOT_NO_SOCKET);exit;end;
  FOSError:=CLIENT_SOCK.SetBlocking(false);
  if FOSError<>EFOS_OS_OK then begin // got OS_IN_PROGRESS on darwin
    result:=ese_CANNOT_CREATESOCKET;
    AI:=nil;
    CLIENT_SOCK.SockClose;
    CLIENT_SOCK.Finalize;
    CLIENT_SOCK := nil;
    _CntErr(CANNOT_SET_SOCKET_BLOCKING);
    exit;
  end;
  Handler.InitClientSock(CLIENT_SOCK);
  FOSERROR:=CLIENT_SOCK.Connect(AI); // HANDLE CONNECT ERRORS HERE -> OS dependencies !
  case FOSError of
    EFOS_OS_OK,EFOS_OS_WOULD_BLOCK,EFOS_OS_IN_PROGRESS: begin
      CLIENT_SOCK.EventSource  := TFRE_APSES_RW_SOCKET.create(CLIENT_SOCK,Handler.ClientHandler,Handler.TearDownClientSock,false);
      //with TFRE_APSE S_RW_SOCKET(CLIENT_SOCK.EventSource) do begin
      //  CLIENT_SOCK.SetupReadWriteCBS(@SchedulingEnableRead,@SchedulingEnableWrite,@SchedulingFreeDelayed,@QuiesceEventSource);
      //end;
      Result:=ese_OK;
    end;
    EFOS_OS_ADDR_NOT_AVAIL: begin
      Result:=ese_BADADDRESS;
    end;
    EFOS_OS_ADDR_IN_USE: begin
      result:=ese_IN_USE;
    end;
    EFOS_OS_BAD_DESCRIPTOR: begin
      result:=ese_CANNOT_CREATESOCKET;
      _CntErr(GOT_BAD_DESCRIPTOR1);
      writeln('>> SOCKET CREATION PROBLEM2 (',FOSError,') ', CLIENT_SOCK.GetHandleKey);
      TObject(CLIENT_SOCK.GetData).Free;
      TFRE_APSES_RW_SOCKET(CLIENT_SOCK.EventSource).Free; // frees writesource too
      CLIENT_SOCK.EventSource:=nil;
      //FFcomHandler.TearDownClientSock(CLIENT_SOCK);
      result := ese_INTERNAL;
      exit;
    end;

    else begin
      result:=ese_INTERNAL;
      gfre_bt.CriticalAbort('handle all possible good cases for all supported os''es <%s>',[CFOS_OS_ERROR[FOSError]]);
    end;
  end;
end;

function TFRE_APS_SCHED.AddSocketListener(const Bind_IP: String; const Bind_Port: integer; const IP_Layer: FCOM_IP_LAYER; const PROTOCOL: FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_SERVER_HANDLER; const listener_reuse: boolean;out source:IFRE_APS_SOCKET_EVENTSOURCE): EFOS_FCOM_MULTIERROR;
var AI       : IFCOM_AI;
    LISTENER : TFCOM_SOCK;
    FOSError : EFOS_OS_ERROR;
    LSOCKES  : TFRE_APSES_LISTENER;
begin
  AI:=GFRE_FF.New_FCOM_AI;
  case AI.ResolveandSet(Bind_IP,Bind_Port) of
   fat_INVALID,fat_UNSPEC: begin
    result:=ese_CANNOT_RESOLVE;
    AI:=nil;
    exit;
   end;
  end;
  LISTENER:=GFRE_FF.New_FCOM_NETSOCK(IP_Layer,PROTOCOL,FOSError).GetImplementor as TFCOM_SOCK;
  if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_CREATESOCKET;exit;end;
  FOSError:=LISTENER.SetBlocking(false);
  if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
  if listener_reuse then begin
    FOSError:=LISTENER.SetListenerReuse(true);
    if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
  end;
  FOSError:=LISTENER.Bind(AI);
  if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_BIND;exit; end;
  FOSError:=LISTENER.Listen(10);
  if FOSError<>EFOS_OS_OK then begin
   result:=ese_CANNOT_LISTEN;
   exit;
  end;
  LSOCKES  := TFRE_APSES_LISTENER.Create(LISTENER,true,Handler.InitServerSock,Handler.ServerHandler,Handler.TearDownServerSock,Handler.ListenerError);
  LISTENER.SetES(LSOCKES);
  LSOCKES.Enable_Initialize;
  result  := ese_OK;
  source  := LSOCKES;
end;

function TFRE_APS_SCHED.AddSocketListener_SSL(const Bind_IP: String; const Bind_Port: integer; const IP_Layer: FCOM_IP_LAYER; const PROTOCOL: FCOM_SOCKET_PROTCOL; const Handler: IR_FRE_APS_FCOM_SERVER_HANDLER; const listener_reuse: boolean; out source: IFRE_APS_SOCKET_EVENTSOURCE; const ssl_type: TFRE_FCOM_SSL_Type; const cerifificate_file, private_key_file, root_ca_file: string; const PasswordCB: TFRE_SSL_Callback; const verify_peer: boolean; const fail_no_peer_cert: boolean; const verify_peer_cert_once: boolean; const cipher_suites: string): EFOS_FCOM_MULTIERROR;
var AI       : IFCOM_AI;
    LISTENER : TFCOM_SOCK;
    FOSError : EFOS_OS_ERROR;
    LSOCKES  : TFRE_APSES_LISTENER;
begin
  AI:=GFRE_FF.New_FCOM_AI;
  case AI.ResolveandSet(Bind_IP,Bind_Port) of
   fat_INVALID,fat_UNSPEC: begin
    result:=ese_CANNOT_RESOLVE;
    AI:=nil;
    exit;
   end;
  end;
  LISTENER:=GFRE_FF.New_FCOM_NETSOCK(IP_Layer,PROTOCOL,FOSError).GetImplementor as TFCOM_SOCK;
  if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_CREATESOCKET;exit;end;
  FOSError:=LISTENER.SetBlocking(false);
  if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
  if listener_reuse then begin
    FOSError:=LISTENER.SetListenerReuse(true);
    if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
  end;
  FOSError:=LISTENER.Bind(AI);
  if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_BIND;exit; end;
  FOSError:=LISTENER.Listen(10);
  if FOSError<>EFOS_OS_OK then begin
   result:=ese_CANNOT_LISTEN;
   exit;
  end;
  LSOCKES       := TFRE_APSES_LISTENER.Create_SSL(LISTENER,true,Handler.InitServerSock,Handler.ServerHandler,Handler.TearDownServerSock,Handler.ListenerError,ssl_type,cerifificate_file,private_key_file,root_ca_file,PasswordCB,verify_peer,fail_no_peer_cert,verify_peer_cert_once,cipher_suites);
  LISTENER.SetES(LSOCKES);
  LSOCKES.Enable_Initialize;
  result        := ese_OK;
  source        := LSOCKES;
end;

procedure TFRE_APS_SCHED.AddCondFunctionEvent(const Condition: TFRE_ConditionFunctionEvent; const Func: TFRE_DataMethod;const Data: Pointer; const dispatch_mode: TFRE_DispatchMode);
//var PES:PFRE_APS_EVENTSOURCE;
begin
  //if not assigned(func) then GFRE_BT.CriticalAbort('must provide a function pointer');
  //PES:=NewEventSource;
  //PES^.typ:=dt_ConditionFunction;
  //PES^.mode:=dispatch_mode;
  //PES^.CondDataMethod :=Func;
  //PES^.CondFunction:=Condition;
  //PES^.CondData:=Data;
  //AddEventSource(PES);
end;

procedure TFRE_APS_SCHED.AddCondFuncEventSpec(const Condition: TFRE_ConditionFunctionEvent; const Func: TFRE_DataMethod; const cp: integer; const TID: integer; const Data: Pointer);
//var PES:PFRE_APS_EVENTSOURCE;
begin
  //if not assigned(func) then GFRE_BT.CriticalAbort('must provide a function pointer');
  //PES                   := NewEventSource;
  //PES^.typ              := dt_ConditionFunctionSpec;
  //PES^.mode             := dm_Worker;
  //PES^.CondSpecMethod   := Func;
  //PES^.CondSpecFunction := Condition;
  //PES^.CondSpecData     := Data;
  //PES^.CP               := cp;
  //PES^.Handle           := TID; // ThreadID
  //AddEventSource(PES);
end;

//procedure TFRE_APS_SCHED.ReqSocketWriteEvent(const SOCK: IFCOM_SOCK);
//var lES : TFRE_APSES_RW_SOCKET;
//begin
//  lES := TFRE_APSES_RW_SOCKET(sock.EventSource);
//  if assigned(lES) then begin
//    PushSchedulingMethod(@lES.SchedulingEnableWrite);
//  end else begin
//    GFRE_BT.CriticalAbort('ReqSocketWrite but no WriteES');
//  end;
//end;

procedure TFRE_APS_SCHED.BroadCastCondition;
var  i: Integer;
begin
  for i :=0 to C_Configured_APS_Threads-1 do begin
//    THRDS[i].SignalSelf(inttostR(i)[1]);
  end;
end;

procedure TFRE_APS_SCHED.BroadCastCondition(const TID: integer);
begin
//  _LaunchWorker(tid);
  abort;
end;

{$IFDEF UNIX}
procedure TFRE_APS_SCHED.AsyncKill;
var pid:TPid;
begin
   pid        := FpGetpid;
   fpKill(pid,SIGTERM);
   FTerminate := True;
   FKiller    := TFRE_ASYNC_KILL_THREAD.Create(5000,pid);
   StopThreads;
end;


procedure TFRE_APS_SCHED.Schedule_Timed_LNM(const in_milliseconds: cardinal; const LocalNestedProc: TFRE_Local_Nested_Method; const Data: TObject);
var lnp_wrap : TLNP_wrapper;
begin
  lnp_wrap := TLNP_Wrapper.Create;
  lnp_wrap.LocalNP:=LocalNestedProc;
  lnp_wrap.Data:=Data;
  AddOneShotTimer(in_milliseconds,@Timer_Internal_LM,lnp_wrap,dm_OneWorker);
end;


{$else}
procedure TFRE_APS_SCHED.AsyncKill;
begin
  halt;
end;
{$endif}


procedure TFRE_APS_SCHED.LogInfo(const s: String; const Args: array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Info,'APS');
end;

procedure TFRE_APS_SCHED.LogWarning(const s: String; const Args: array of const);
begin
   GFRE_LOG.Log(s,args,'',fll_Warning,'APS');
end;

procedure TFRE_APS_SCHED.LogError(const s: String; const Args: array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Error,'APS');
end;



procedure TFRE_APS_SCHED.SetOnQuitProcedure(const M: TProcedure);
begin
  FOnquit:=M;
end;

procedure TFRE_APS_SCHED.Start_As_Thread;
begin
  if not assigned(FAPST) then begin
    FAPST := TFRE_APS_RUNNER_THREAD.Create;
  end;
end;


procedure SetupAPS;
begin
  FGFRE_S := TFRE_APS_SCHED.Create;
  GFRE_S  := FGFRE_S;
end;

procedure TearDownAPS;
begin
  GFRE_S.Quit;
  FGFRE_S.Free;
end;

procedure Shutdown_Done;
begin
  try
   if assigned(FKiller) then FKiller.ShutDownOK;
  except
  end;
  GFRE_LOG.Sync_Logger;
end;

{ TFOS_APSTHREAD }

procedure TFRE_APSTHREAD.PushWork(const ES: TFRE_APS_EVENTSOURCE; RunType: TFRE_APS_EVENT_TYPE);
var work_e : TFOS_WORK_E;
         i : integer;
begin
  FWorkLock.Acquire;
  try
    work_e.es := es;
    work_e.et := RunType;
    case FWorkLoadQ.Push(work_e) of
      cs_OK      : ;
      cs_NOSPACE : begin
                     for i :=0 to FWorkLoadQ.Count-1 do begin
                       FWorkLoadQ.Pop(work_e);
                       writeln('WORKQ THREAD Overload POPPED : ',i,' ',work_e.es.ClassName,' ',work_e.et,' ',work_e.es.FSourceID);
                     end;
                     raise Exception.create('aps/thread/workq dimensioned too small '+inttostr(FWorkLoadQ.Count));
                   end;
      else raise Exception.create('internal/aps/pushwork');
    end;
  finally
    FWorkLock.Release;
  end;
  FStartEvent.SetEvent;
end;

procedure TFRE_APSTHREAD.CopyWork;
var work_e : TFOS_WORK_E;
begin
  FWorkLock.Acquire;
  try
    repeat
      case FWorkLoadQ.Pop(work_e) of
        cs_OK     : case FWorkWorkQ.Push(work_e) of
                      cs_OK      : ;
                      cs_NOSPACE : raise Exception.Create('WorkWorkQ dimensioned too small '+IntToStr(FWorkWorkQ.Count));
                    end;
        cs_ZEROPOP: break;
      end;
    until false;
  finally
    FWorkLock.Release;
  end;
end;

constructor TFRE_APSTHREAD.Create(const ID: cardinal; const sch: TFRE_APS_SCHED);
begin
  FID             := ID;
  FCPu_SOCKET_ID  := -1;
  Scheduler       := sch;
  GFRE_TF.Get_Event(FStartEvent);

  FWorkLoadQ.Initialize(1024);
  FWorkWorkQ.Initialize(1024);
  GFRE_TF.Get_Lock(FWorkLock);

  inherited Create(false);
end;

destructor TFRE_APSTHREAD.Destroy;
begin
  FWorkLock.Finalize;
  FStartEvent.Finalize;
  Scheduler:=nil;
  inherited Destroy;
end;

procedure TFRE_APSTHREAD.Execute;
var es        : TFOS_WORK_E;
    res       : integer;
    i         : integer;
    os_res    : EFOS_OS_ERROR;
    deq_cnt   : integer;

    procedure Bind;
    var error:string;
    begin
      if not  GFRE_CPU.Bind_to_logical(FID,error) then GFRE_LOG.Log('aps worker, could not set cpuaffinity '+error,catWarning);
      DBGLOGT.Log('INIT CURRENT THREAD <%d> SOCK <%d> ID=[%d]',[FID,FSOCKET_ID,FID],'APSW',10);
      GFRE_LOG.RegisterThread('T'+inttostr(FID));
    end;

begin
  try
    Bind;
    repeat
      try
        repeat
          FStartEvent.WaitFor;
          if FMyTerminate then exit;
          CopyWork;
          for i:=1 to FWorkWorkQ.Count do begin
            case FWorkWorkQ.Pop(es) of
              cs_OK      : es.es.EventReported(FID,es.et);
              cs_ZEROPOP : break ;
            end;
            deq_cnt := es.es.DecrementEnqueueings;
            if es.es.FMarkFinalize then begin
              if deq_cnt=0 then begin // It's safe to free the ES now, it's deleted from the base loop, and not enqueded anymore
               es.es.FinalizeIt; //
              end;
            end;
          end;
        until false;
      except on E:Exception do begin
        try
          GFRE_LOG.Log('APS exception <%s>',[e.Message],'',fll_Emergency,'APSW',true);
          writeln('APS THREAD EXCEPTION ::: ',e.Message);
          writeln(GFRE_BT.DumpExceptionsBacktrace);
        except on e:exception do begin
          GFRE_LOG.LogEmergency('LOGGING EXCEPTION : APSLE '+e.Message);
        end;end;
      end;end;
    until false;
  finally
    //writeln('T',FID,' EXIT');
  end;
end;


procedure TFRE_APSTHREAD.TerminateGraceFul;
begin
  FMyTerminate:=true;
  FStartEvent.SetEvent;
end;



function TFRE_EVENTCONDITION.QueryConditionChange: boolean;
begin
  result:=FChanged=1;
end;

procedure TFRE_EVENTCONDITION.ConditionHasChanged;
begin
  if InterLockedExchange(FChanged,1)=1 then begin
    writeln('EVENT CONDITION : Change Already Signalled / silent ignore / this is almost certainly a logic error in your statemachine');
    exit; // Change Already Signalled
  end;
  case FDispatchMode of
    dm_Sync: begin
      FContinuationMethod(-1,self);
    end;
    else begin
      GFRE_BT.CriticalAbort('NOT IMPLEMENTED');
      GFRE_S.BroadCastCondition;
    end
  end;
end;

procedure TFRE_EVENTCONDITION.WaitCondition(const cont_point: cardinal);inline;
begin
  FContinuationLabel:=cont_point;
  if InterLockedExchange(FChanged,0)=0 then begin
    gfre_bt.CriticalAbort('WAITING ON UNSIGNALLED CONDITION ? / RETHINK IF OK');
  end;
  //raise Exception.Create('CONT'); ..
end;

function TFRE_EVENTCONDITION.ContPoint: cardinal;
begin
  result:=FContinuationLabel;
end;

procedure TFRE_EVENTCONDITION.SetData(const d: pointer);
begin
  FData:=d;
end;

function TFRE_EVENTCONDITION.GetData: Pointer;
begin
  result:=FData;
end;

procedure TFRE_EVENTCONDITION.Finalize;
begin
  free;
end;

constructor TFRE_EVENTCONDITION.Create(const continuation_method:TFRE_ConditionEventContinuation;const dm:TFRE_DispatchMode);
begin
  inherited Create;
  FContinuationMethod := continuation_method;
  FContinuationLabel  := 0;
  FDispatchMode       := dm;
end;

destructor TFRE_EVENTCONDITION.Destroy;
begin
  inherited Destroy;
end;



initialization



end.

