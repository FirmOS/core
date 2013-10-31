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

  TAPSC_ListenerState = (als_BAD,als_LISTENING,als_STOPPED,als_LISTEN_ERROR,als_EVENT_NEW_LISTENER);
  TAPSC_ChannelState  = (ch_BAD,ch_NEW_SS_CONNECTED,ch_ACTIVE,ch_NEW_CS_CONNECTED,ch_EOF);

  IFRE_APSC_DNS_ANSWER=interface
  end;

  IFRE_APSC_LISTENER=interface
    function  GetState            : TAPSC_ListenerState;
    function  GetErrorString      : string;
    function  GetListeningAddress : string;
    function  GetID               : string;
    procedure Stop;
    procedure Start;
    procedure Finalize;
  end;

  IFRE_APSC_CHANNEL_MANAGER = interface;
  IFRE_APSC_CHANNEL         = interface;
  IFRE_APSC_TIMER           = interface;

  TOnNew_APSC_Listener     = procedure (const new_listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState) of object;
  TOnNew_APSC_Channel      = procedure (const channel      : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState) of object;
  TOnNew_APSC_Timer        = procedure (const timer        : IFRE_APSC_TIMER) of object;
  TOnNew_APSC_Signal       = procedure (const signal       : NativeUint) of object;
  TFRE_APSC_TIMER_CALLBACK = procedure (const timer        : IFRE_APSC_TIMER ; const flag1,flag2 : boolean) of object;
  TFRE_APSC_CHANNEL_EVENT  = procedure (const channel      : IFRE_APSC_CHANNEL) of object;
  TFRE_APSC_CoRoutine      = procedure (const Data         : Pointer) of Object;

  IFRE_APSC=interface
    procedure   AddListener_TCP   (Bind_IP,Bind_Port:String ; const ID:ShortString);// is interpreted as numerical ipv4 or ipv6 address, adds a listener for this ip, special cases are *, *4, and *6 (which use all addresses of the host)
    procedure   AddClient_TCP     (Host,Port : String; const ID:ShortString ; const channelmanager: IFRE_APSC_CHANNEL_MANAGER = nil ;  localNewChannelCB : TOnNew_APSC_Channel = nil ;  localRead :  TFRE_APSC_CHANNEL_EVENT=nil ; localDisconnect :  TFRE_APSC_CHANNEL_EVENT=nil ; Bind_IP:string='' ; Bind_Port:String='');
    function    AddTimer          (const timer_id: ShortString ; interval_ms : NativeUint ; timer_callback : TFRE_APSC_TIMER_CALLBACK ; local_new_timercb : TOnNew_APSC_Timer=nil) : IFRE_APSC_TIMER; // Must be called in sync with MAIN EVENT LOOP
    procedure   SetNewListenerCB  (const lcb    : TOnNew_APSC_Listener);
    procedure   SetNewChannelCB   (const chancb : TOnNew_APSC_Channel);
    procedure   SetNewTimerCB     (const timercb : TOnNew_APSC_Timer);
    procedure   SetSingnalCB      (const signalcb : TOnNew_APSC_Signal);
    procedure   RunUntilTerminate ;
    procedure   RequestTerminate  ;
  end;


  IFRE_APSC_TIMER=interface
    procedure TIM_Start;
    procedure TIM_Stop;
    procedure TIM_SetInterval (const interval_ms : NativeUint);
    procedure TIM_SetCallback (cb : TFRE_APSC_TIMER_CALLBACK);
    procedure TIM_SetID       (const ID:String);
    function  TIM_GetID       : string;
    procedure TIM_SetMethod   (const m : TMethod);
    function  TIM_GetMethod   :TMethod;
    procedure TIM_Trigger     (const flag1:boolean=false ; const flag2:boolean=false); // Must be called in same MANAGER CONTEXT (THREAD)
    procedure Finalize        ;
  end;


  IFRE_APSC_CHANNEL_MANAGER=interface // = Thread bound to CPU
    function  GetID                    : NativeInt;
    function  AddTimer                 (interval_ms : NativeUint) : IFRE_APSC_TIMER;
    procedure ScheduleCoRoutine        (const method : TFRE_APSC_CoRoutine ; const data : Pointer);
  end;

  IFRE_APSC_CHANNEL_GROUP=interface // = Session Group, VNC Group / Upload / Download Group / HTTP Requests
  end;

  { IFRE_APSC_CHANNEL }

  IFRE_APSC_CHANNEL=interface // Session , VNC , UP/DOWN Load, HTTP Requests
    function  GetChannelManager : IFRE_APSC_CHANNEL_MANAGER;
    function  GetListener       : IFRE_APSC_LISTENER;
    function  GetConnSocketAddr : String;

    function  GetVerboseDesc    : String;
    procedure SetVerboseDesc    (const desc:string);
    function  GetHandleKey      : cInt;

    procedure SetOnReadData     (on_read : TFRE_APSC_CHANNEL_EVENT);
    procedure SetOnDisconnnect  (on_disc : TFRE_APSC_CHANNEL_EVENT);

    procedure  CH_WriteString    (const str : String);
    procedure  CH_WriteBuffer    (const data : Pointer ; const len : NativeInt);
    function   CH_GetDataCount   : NativeInt;
    function   CH_ReadString     : String;
    function   CH_ReadBuffer     (const data : Pointer ; const len : NativeInt) : NativeInt;
    function   CH_GetErrorString : String;
    function   CH_GetErrorCode   : NativeInt;
    function   CH_IsClientChannel: Boolean;
    function   CH_GetState       : TAPSC_ChannelState;

    procedure CH_Enable_Reading    ;
    procedure CH_Enable_Writing    ;
    procedure Finalize;
  end;

  IFRE_APS_COMM_SERVER=interface
     //procedure GotANewCommChannel ;
  end;


var
  //GFRE_S  : IFRE_APS;
  GFRE_SC : IFRE_APSC;

implementation

end.

