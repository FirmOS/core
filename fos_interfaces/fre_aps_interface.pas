unit fre_aps_interface;

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

{$interfaces CORBA}  // Non Refcounted IF's

interface

uses
  Classes, SysUtils,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,FOS_FCOM_INTERFACES;

var G_NO_INTERRUPT_FLAG : Boolean = false;

type
  IFRE_APS_EVENTSOURCE            = interface;
  TFRE_DispatchMode               = (dm_INVALID,dm_Sync,dm_OneWorker);
  IFRE_EVENTCONDITION             = interface;
  TFRE_ConcurrencyToken           = QWord;
  TFRE_TimerHandle                = int64;

  TFRE_ConcurrentMethod           = procedure (const TID:integer) of object;
  TFRE_ConcurrentLocalM           = procedure (const TID:integer);
  TFRE_DataMethod                 = procedure (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0) of object;
  TFRE_ConditionFunctionEvent     = function  (const TID:integer):boolean of object;
  TFRE_ConditionEventContinuation = procedure (const TID:integer;const EC:IFRE_EVENTCONDITION) of object;
  TFRE_Generic_CMD_Handler        = procedure (const TID:integer;const CMD:IFOS_NPS;const cp:integer=0) of object;
  TFRE_Local_Nested_Method        = procedure (const Data:Tobject) is nested;
  TFRE_Method                     = procedure of object;

  RFRE_APS_STATUS=record
    CreatedServerRES  : int64;
    CreatedClientRES  : int64;
    CreatedServerWES  : int64;
    CreatedClientWES  : int64;
    FreedServerRES    : int64;
    FreedClientRES    : int64;
    FreedServerWES    : int64;
    FreedClientWES    : int64;
  end;

  IFRE_APS_EVENTSOURCE=interface
    procedure  MarkFinalize          ;
    procedure  FinalizeIt            ;
    procedure  FireEventManual       (const write:boolean=false);
    procedure  Enable_Read_Pending   ;
    procedure  Enable_Write_Pending  ;
    procedure  Enable_Initialize     ;
    procedure  QuiesceEventSource    ;
  end;

  IFRE_APS_SOCKET_EVENTSOURCE=interface(IFRE_APS_EVENTSOURCE)
    function    GetSocket           :IFCOM_SOCK;
  end;

  IFRE_APS_TIMER=interface(IFRE_APS_EVENTSOURCE)
  end;

  //IFRE_APS_ONESHOT_TIMER=interface(IFRE_APS_EVENTSOURCE)
  //  procedure Rearm;
  //end;

  { TFRE_EVENTCONDITION }
  IFRE_EVENTCONDITION=interface
    procedure SetData              (const d:pointer);
    function  GetData              :Pointer;
    function  QueryConditionChange :boolean;
    procedure WaitCondition        (const cont_point:cardinal);
    function  ContPoint            :cardinal;
    procedure ConditionHasChanged  ;
    procedure Finalize             ;
  end;

  IFRE_APS_PROCESS = interface
    procedure Setup       ;
    procedure Terminate   ; // KILL
    procedure ReInit      ; // HUP
    procedure Interrupt   ; // CTRL-C
    function  GetName     : String;
    procedure Finalize    ;
  end;

  IR_FRE_APS_FCOM_CLIENT_HANDLER=record
    ClientHandler      : TFRE_FCOM_APS_Event; //function  ClientHandler      (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
    InitClientSock     : TFRE_FCOM_InitEvent; //procedure InitClientSock     (const SOCK:IFCOM_SOCK);
    TearDownClientSock : TFRE_FCOM_TearDownEvent; //procedure TearDownClientSock (const Sock:IFCOM_SOCK);
  end;

  IR_FRE_APS_FCOM_SERVER_HANDLER=record
    ServerHandler      : TFRE_FCOM_APS_Event; //  function  ServerHandler      (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean; // true -> reenable read
    InitServerSock     : TFRE_FCOM_InitEvent;  //  procedure InitServerSock     (const SOCK:IFCOM_SOCK);
    TearDownServerSock : TFRE_FCOM_TearDownEvent;  //  procedure TearDownServerSock (const Sock:IFCOM_SOCK);
    ListenerError      : TFRE_FCOM_SocketError;  // procedure ListenerError      (const listener_sock:IFCOM_SOCK;const Error:EFOS_OS_ERROR); // Listener had an error and will be shutdown
  end;


  { IFRE_APS }

  IFRE_APS=interface
    procedure SetOnQuitProcedure              (const M:TProcedure);
    function  GetNewConcurrencyResourceToken  :TFRE_ConcurrencyToken;
    procedure DropConcurrencyResourceToken    (const ct:TFRE_ConcurrencyToken);
    function  NewEventCondition               (const continuation_method:TFRE_ConditionEventContinuation;const dm:TFRE_DispatchMode):IFRE_EVENTCONDITION;
    procedure DC                              (const P:TFRE_ConcurrentMethod;const used_token:TFRE_ConcurrencyToken); //Do Concurrent
    procedure DCL                             (const P:TFRE_ConcurrentLocalM;const used_token:TFRE_ConcurrencyToken); //Do Concurrent Local Method
    procedure Start                           (const P:IFRE_APS_PROCESS);
    function  AddPeriodicTimer                (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer=nil;const dispatch_mode:TFRE_DispatchMode=dm_OneWorker;const TimerLostEvent:TFRE_Method=nil):IFRE_APS_TIMER;
    function  AddPeriodicSignalTimer          (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer=nil;const dispatch_mode:TFRE_DispatchMode=dm_OneWorker):IFRE_APS_TIMER;
    function  AddOneShotTimer                 (const time_ms:cardinal;const Func:TFRE_DataMethod;const Data:Pointer=nil;const dispatch_mode:TFRE_DispatchMode=dm_OneWorker):IFRE_APS_TIMER;
    function  AddSocketClient                 (const Target_IP: String;const TargetPort: integer; const IP_Layer: FCOM_IP_LAYER;const PROTOCOL: FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_CLIENT_HANDLER;const BindIP: String=''; const BindPort: integer=0): EFOS_FCOM_MULTIERROR;
    function  AddSocketListener               (const Bind_IP:String;const Bind_Port:integer;const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_SERVER_HANDLER;const listener_reuse:boolean;out sock:IFRE_APS_SOCKET_EVENTSOURCE):EFOS_FCOM_MULTIERROR;
    function  AddSocketListener_SSL           (const Bind_IP:String;const Bind_Port:integer;const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;const Handler:IR_FRE_APS_FCOM_SERVER_HANDLER;const listener_reuse:boolean;out sock:IFRE_APS_SOCKET_EVENTSOURCE ;
                                               const ssl_type:TFRE_FCOM_SSL_Type;const cerifificate_file,private_key_file,root_ca_file:string ; const PasswordCB:TFRE_SSL_Callback=nil; const verify_peer:boolean=false;const fail_no_peer_cert:boolean=false;const verify_peer_cert_once:boolean=true;const cipher_suites:string='DEFAULT'):EFOS_FCOM_MULTIERROR;

    procedure AddCondFunctionEvent            (const Condition:TFRE_ConditionFunctionEvent;const Func:TFRE_DataMethod;const Data:Pointer;const dispatch_mode:TFRE_DispatchMode=dm_Sync);
    procedure AddCondFuncEventSpec            (const Condition:TFRE_ConditionFunctionEvent;const Func:TFRE_DataMethod;const cp:integer;const TID:integer;const Data:Pointer);
    procedure BroadCastCondition              ;
    procedure BroadCastCondition              (const TID:integer);

    procedure AsyncKill                       ;

    procedure Schedule_Timed_LNM              (const in_milliseconds:cardinal;const LocalNestedProc : TFRE_Local_Nested_Method;const Data:TObject);

    procedure Run                             ;
    procedure Quit                            ;
    procedure Start_As_Thread                 ;
    procedure GetStatus                       (var state:RFRE_APS_STATUS);
    //procedure PushSchedulingMethod            (const Method : TFRE_SimpleCallback;const deferred_schedule:boolean=false);
  end;


  TAPSC_ListenerState = (als_BAD,als_LISTENING,als_STOPPED,als_LISTEN_ERROR,als_NEW_LISTENER);
  TAPSC_ChannelState  = (ch_BAD,ch_NEW_SS_CONNECTED,ch_END_CLOSED,ch_STOPPED,ch_ACTIVE);

  IFRE_APSC_DNS_ANSWER=interface
  end;

  IFRE_APSC_LISTENER=interface
    function  GetState            : TAPSC_ListenerState;
    function  GetErrorString      : string;
    function  GetListeningAddress : string;
    procedure Stop;
    procedure Start;
    procedure Finalize;
  end;

  IFRE_APSC_CHANNEL_MANAGER = interface;
  IFRE_APSC_CHANNEL         = interface;

  TOnNew_APSC_Listener = procedure (const new_listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState) of object;
  TOnNew_APSC_Channel  = procedure (const channel : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState) of object;

  IFRE_APSC=interface
    procedure   AddListener_TCP  (Bind_IP,Bind_Port:String);// is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, *4, and *6 (which use all addresses of the host)
    procedure   SetNewListenerCB (const lcb    : TOnNew_APSC_Listener);
    procedure   SetNewChannelCB  (const chancb : TOnNew_APSC_Channel);
  end;

  IFRE_APSC_CHANNEL_MANAGER=interface // = Thread bound to CPU
    function GetID                    : NativeInt;
  end;

  IFRE_APSC_CHANNEL_GROUP=interface // = Session Group, VNC Group / Upload / Download Group / HTTP Requests
  end;

  { IFRE_APSC_CHANNEL }

  IFRE_APSC_CHANNEL=interface // Session , VNC , UP/DOWN Load, HTTP Requests
    function  GetChannelManager : IFRE_APSC_CHANNEL_MANAGER;
    function  GetListener       : IFRE_APSC_LISTENER;
    function  GetConnSocketAddr : String;

    procedure CH_WriteString       (const str : String);
    procedure Enable_Reading    ;
    procedure Enable_Writing    ;
    procedure Finalize;
  end;

  IFRE_APS_COMM_SERVER=interface
     //procedure GotANewCommChannel ;
  end;


var
  GFRE_S  : IFRE_APS;
  GFRE_SC : IFRE_APSC;

implementation

end.

