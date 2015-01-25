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
  Classes, SysUtils,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,ctypes;

var G_NO_INTERRUPT_FLAG : Boolean = false;

type
  TFRE_APSC_ID        =  String[15];
  TFRE_APSC_ID_Array  =  Array of TFRE_APSC_ID;

  TAPSC_ListenerState = (als_BAD,als_LISTENING,als_STOPPED,als_LISTEN_ERROR,als_EVENT_NEW_LISTENER);
  TAPSC_ChannelState  = (ch_BAD,ch_WAIT,ch_NEW_SS_CONNECTED,ch_ACTIVE,ch_NEW_CS_CONNECTED,ch_NEW_CHANNEL_FAILED,ch_EOF,ch_InvalidOperation,ch_ErrorOccured);

  IFRE_APSC_DNS_ANSWER=interface
  end;

  IFRE_APSC_CHANNEL_MANAGER = interface;
  IFRE_APSC_CHANNEL         = interface;
  IFRE_APSC_TIMER           = interface;
  IFRE_APSC_CHANNEL_GROUP   = interface;
  IFRE_APSC_LISTENER        = interface;

  TOnNew_APSC_Signal       = procedure (const signal       : NativeUint) of object;

  TFRE_APSC_LISTENER_CALLBACK     = procedure (const listener     : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState) of object;
  TFRE_APSC_TIMER_CALLBACK        = procedure (const timer        : IFRE_APSC_TIMER ; const flag1,flag2 : boolean) of object;
  TFRE_APSC_CHANNEL_EVENT         = procedure (const channel      : IFRE_APSC_CHANNEL) of object;
  TFRE_APSC_CHANNEL_EVENT_NESTED  = procedure (const channel      : IFRE_APSC_CHANNEL) is nested;
  TFRE_APSC_CHANNEL_CHANGE_EVENT  = procedure (const channel      : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt) of object;
  TFRE_APSC_CoRoutine             = procedure (const Data         : Pointer) of Object;
  TFRE_APSC_CoRoutineNested       = procedure (const Data         : Pointer) is nested;
  TFRE_APSC_CoRoutineSimple       = procedure  of Object;
  TFRE_APSC_CoRoutineSimpleNested = procedure  is nested;

  { IFRE_APSC }

  IFRE_APSC=interface
    function    AddDefaultGroupTimer       (const timer_id    : TFRE_APSC_ID ; interval_ms : NativeUint ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; const start_timer : boolean = false ; const asc_meth_code : CodePointer =nil ; const asc_meth_data : Pointer =nil) : IFRE_APSC_TIMER;
    function    AddDefaultGroupListenerTCP (Bind_IP,Bind_Port:String       ; const ID:TFRE_APSC_ID ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK = nil ; const start_listener : boolean = true ; const enable_ssl : boolean=false ; const special_ssl_ctx : PSSL_CTX =nil): IFRE_APSC_LISTENER; // is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, and *6 (which use all addresses of the host)
    function    AddDefaultGroupListenerUX  (const special_file:shortstring ; const ID:TFRE_APSC_ID ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK = nil ; const start_listener : boolean = true ; const enable_ssl : boolean=false ; const special_ssl_ctx : PSSL_CTX =nil): IFRE_APSC_LISTENER;
    function    AddClient_TCP              (IP,Port   : String             ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String=''):IFRE_APSC_CHANNEL;
    function    AddClient_TCP_DNS          (Host,Port : String             ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String=''):IFRE_APSC_CHANNEL;
    function    AddClient_UX               (const special_file:shortstring ; const ID:TFRE_APSC_ID ; const auto_finalize : boolean=true ; channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localEvent :  TFRE_APSC_CHANNEL_CHANGE_EVENT=nil ; localRead :  TFRE_APSC_CHANNEL_EVENT=nil ;  localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil):IFRE_APSC_CHANNEL;
    procedure   SetListenerCB              (const lcb      : TFRE_APSC_LISTENER_CALLBACK);
    procedure   SetNewChannelCB            (const chancb   : TFRE_APSC_CHANNEL_CHANGE_EVENT);
    procedure   SetSingnalCB               (const signalcb : TOnNew_APSC_Signal);
    function    GetDefaultCG               : IFRE_APSC_CHANNEL_GROUP;
    function    GetChannelGroupByID        (CGID : TFRE_APSC_ID ; out cg : IFRE_APSC_CHANNEL_GROUP):boolean;
    function    GetChannelGroupIDs         : TFRE_APSC_ID_Array;
    function    CreateNewChannelGroup      (const cg_id : TFRE_APSC_ID     ; out cm : IFRE_APSC_CHANNEL_GROUP ; const auto_workercnt : NativeInt=0) : boolean;
    procedure   RunUntilTerminate          ;
    procedure   RequestTerminate           (const no_jack:boolean=false);
  end;


  IFRE_APSC_TIMER=interface
    function  cs_GetID           : TFRE_APSC_ID;
    procedure cs_Trigger         (const flag1:boolean=false ; const flag2:boolean=false);
    function  cs_Start           (const interval_ms : NativeInt=0):boolean;               { negative intervals are oneshots, 0 starts the timer (again) but does change the interval }
    function  cs_Stop            : boolean;
    procedure cs_ChangeCallback  (cb : TFRE_APSC_TIMER_CALLBACK);
    procedure cs_SetMethod       (const m : TMethod);
    function  cs_GetMethod       :TMethod;
    procedure cs_Finalize        ;
  end;

  IFRE_APSC_LISTENER=interface
    { All Listeners work in the context of the Main APS Thread, a fork new channles bound to RR Channelmanagers or dedicated CM's }
    function  GetState            : TAPSC_ListenerState;
    function  GetErrorString      : string;
    function  GetListeningAddress : string;
    function  cs_GetID            : TFRE_APSC_ID;
    procedure cs_Stop             ;
    procedure cs_Start            ;
    procedure cs_Finalize         ;
  end;

  IFRE_APSC_WORKABLE = interface
    procedure  SetupWorkerCount_WIF         (const wc : NativeInt);                                           { gives a hint how many workers will do the load                   }
    function   GetAsyncDoneContext_WIF      : IFRE_APSC_CHANNEL_MANAGER;                                      { who gets the result a cm                                         }
    function   GetAsyncDoneChannelGroup_WIF : IFRE_APSC_CHANNEL_GROUP;                                        { or a cg - only one is allowed                                    }
    function   StartGetMaximumChunk_WIF     : NativeInt;                                                      { tell the cpu cg how much work is to be done parallel             }
    procedure  ParallelWorkIt_WIF           (const startchunk,endchunk : Nativeint ; const wid : NativeInt);  { the working callback, gives chunk id, and the parallel worker id }
    procedure  WorkNextCyle_WIF             (var continue : boolean);                                         { set to true to get a recall                                      }
    procedure  WorkDone_WIF                 ;
    procedure  ErrorOccurred_WIF            (const ec : NativeInt ; const em : string);
  end;

  { IFRE_APSC_CHANNEL_GROUP }

  IFRE_APSC_CHANNEL_GROUP=interface    { group channel manager together, and be the "main" eventer for them }
    function  Implementor                 : TObject;
    function  AddChannelGroupTimer        (const timer_id: TFRE_APSC_ID ; interval_ms : NativeInt ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; { Add a Timer to a channel group, should be immediatly started, }
                                          const start_timer : boolean = false ; const asc_meth_code : CodePointer =nil ;                        { callbacks should be set, negative interval=oneshot timer      }
                                          const asc_meth_data : Pointer =nil) : IFRE_APSC_TIMER;
    function  AddListenerTCP              (Bind_IP,Bind_Port : String     ;  const ID :TFRE_APSC_ID ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK ; const start_listener : boolean = true ; const enable_ssl : boolean = false ; const special_ssl_ctx : PSSL_CTX =nil ): IFRE_APSC_LISTENER; // is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, and *6 (which use all addresses of the host)
    function  AddListenerUX               (special_file      : ShortString ; const ID :TFRE_APSC_ID ; const spec_listener_cb : TFRE_APSC_LISTENER_CALLBACK ; const start_listener : boolean = true ; const enable_ssl : boolean = false ; const special_ssl_ctx : PSSL_CTX =nil ): IFRE_APSC_LISTENER;
    function  GetChannelManagerCount      : NativeInt;
    function  GetDefaultChannelManager    : IFRE_APSC_CHANNEL_MANAGER;
    function  GetCGID                     : TFRE_APSC_ID;
    function  GetChannelManagerIDs        : TFRE_APSC_ID_Array;
    function  GetChannelManagerByID       (const cm_id : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    function  CreateNewChannelManager     (const cm_id : TFRE_APSC_ID ; out cm : IFRE_APSC_CHANNEL_MANAGER) : boolean;
    procedure DoAsyncWork                 (const workable : IFRE_APSC_WORKABLE);  { needs an "pingback" continuation context          }
    procedure DoSyncedWork                (const workable : IFRE_APSC_WORKABLE);  { does a "hard" wait in the current context (event) }
    procedure DoAsyncWorkSimpleMethod     (const method   : TFRE_APSC_CoRoutine ; const data : Pointer); { simple encapsulation of a workable                }

    procedure SwitchToContext             (const object_co : TFRE_APSC_CoRoutineSimple);
    procedure SwitchToContextNe           (const nested_co : TFRE_APSC_CoRoutineSimpleNested);
    procedure SwitchToContextEx           (const object_co : TFRE_APSC_CoRoutine       ; const data : Pointer);
    procedure SwitchToContextExNe         (const nested_co : TFRE_APSC_CoRoutineNested ; const data : Pointer);
end;

  IFRE_APSC_CHANNEL_MANAGER=interface  { Thread bound to CPU, grouped by a IFRE_APSC_CHANNEL_GROUP }
    function  Implementor                 : TObject;
    function  GetID                       : TFRE_APSC_ID;
    function  AddChannelManagerTimer      (const timer_id: TFRE_APSC_ID ; interval_ms : NativeUint ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; const periodic :boolean = false ; const start_timer : boolean = false ; const asc_meth_code : CodePointer =nil ; const asc_meth_data : Pointer =nil) : IFRE_APSC_TIMER;
    function  GetChannelGroup             : IFRE_APSC_CHANNEL_GROUP;
    procedure SwitchToContext             (const object_co : TFRE_APSC_CoRoutineSimple);
    procedure SwitchToContextNe           (const nested_co : TFRE_APSC_CoRoutineSimpleNested);
    procedure SwitchToContextEx           (const object_co : TFRE_APSC_CoRoutine       ; const data : Pointer);
    procedure SwitchToContextExNe         (const nested_co : TFRE_APSC_CoRoutineNested ; const data : Pointer);
  end;


  { IFRE_APSC_CHANNEL }

  IFRE_APSC_CHANNEL=interface // Session , VNC , UP/DOWN Load, HTTP Requests
    { Context Critical Methods }
    function   Implementor          : TObject;
    function   CH_GetConnSocketAddr : String;

    function   CH_GetVerboseDesc    : String;
    procedure  CH_SetVerboseDesc    (const desc:string);
    function   CH_GetHandleKey      : cInt;

    procedure  CH_Enable_Reading    ;
    procedure  CH_Enable_Writing    ;

    procedure  CH_SetOnReadData     (on_read : TFRE_APSC_CHANNEL_EVENT);
    procedure  CH_SetOnDisconnnect  (on_disc : TFRE_APSC_CHANNEL_EVENT);

    procedure  CH_WriteString       (const str : RawByteString);
    procedure  CH_WriteBuffer       (const data : Pointer ; const len : NativeInt);
    procedure  CH_WriteOpenedFile   (const fd : cInt ; const offset,len : NativeInt);
    function   CH_GetDataCount      : NativeInt;
    function   CH_ReadString        : RawByteString;
    function   CH_ReadBuffer        (const data : Pointer ; const len : NativeInt) : NativeInt;
    function   CH_IsClientChannel   : Boolean;
    function   CH_GetState          : TAPSC_ChannelState;
    function   CH_GetID             : TFRE_APSC_ID;
    function   ch_GetListenerID     : TFRE_APSC_ID;
    procedure  CH_AssociateData     (const data : PtrUInt);
    function   CH_GetAssociateData  : PtrUInt;

    { Context Safe Methods }
    procedure  cs_Finalize          ; // Calling Finalize on channel will close it, but no Disconnect event gets fired, only when the partner socket diconnects
    procedure  cs_WriteBuffer       (const data : Pointer ; const len : NativeInt); // data gets copied ...
    procedure  cs_WriteString       (const str : RawByteString); { to use from wrong/other thread contexr ... }
    function   cs_GetChannelManager : IFRE_APSC_CHANNEL_MANAGER;
  end;

  IFRE_APS_COMM_SERVER=interface
     //procedure GotANewCommChannel ;
  end;


var
  GFRE_SC : IFRE_APSC;

implementation

end.

