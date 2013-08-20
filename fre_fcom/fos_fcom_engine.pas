unit fos_fcom_engine;

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

interface

uses
  Classes, SysUtils,FOS_APS,FOS_FCOM_INTERFACES,FOS_FCOM_TYPES,FOS_FCOM_DEFAULT,BSD,FOS_BASIS_TOOLS;

 type

    { TFCOM_ENGINE }

    TFCOM_ENGINE=class(TFOS_APS_PROCESS) // Base Class for APS Scheduled FCOM Client/Server Apps
     private
       procedure _ProcessSocketEvents (const TID:TAPS_THREAD_ID;const E:PFOS_APS_EVENTSOURCE;var   Phase:RPHASE_CONTROL);
     public
       procedure ClientHandler  (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;var Request_RD,Request_WR:Boolean;const Datacount:Integer);virtual;
       procedure ServerHandler  (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;var Request_RD,Request_WR:Boolean;const Datacount:Integer);virtual;
       procedure NewServersock  (out PES:PFOS_APS_EVENTSOURCE);virtual;
       function  AddClient      (out PES:PFOS_APS_EVENTSOURCE;const Target_IP: String;const TargetPort: integer; const IP_Layer: FCOM_IP_LAYER;const PROTOCOL: FCOM_SOCKET_PROTCOL; const BindIP: String=''; const BindPort: integer=0): EFOS_FCOM_MULTIERROR;
       function  AddListener    (out PES:PFOS_APS_EVENTSOURCE;const Bind_IP:String;const Bind_Port:integer;const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;const listener_reuse:boolean): EFOS_FCOM_MULTIERROR;
    end;

implementation

{ TFCOM_ENGINE }


procedure TFCOM_ENGINE._ProcessSocketEvents(const TID: TAPS_THREAD_ID;  const E: PFOS_APS_EVENTSOURCE; var Phase: RPHASE_CONTROL);
var LSOCK_STATE:EFOS_FCOM_SOCKSTATE;
    FHandler:TFCOM_OnMultiEvent;

  function  _Error:Boolean;inline;
  begin
   result:=(E^.Return_Flags and EV_ERROR)>0;
  end;
  function  _EOF:Boolean;inline;
  begin
   result:=(E^.Return_Flags and EV_EOF)>0
  end;
  procedure _HandleEOF;
  begin
   case LSOCK_STATE of
     ess_CONNECTING: begin
       case fcom_interpret_OS_Error(E^.Return_FFlags) of
         EFOS_OS_TIMEDOUT           : FHandler(esv_SOCKCONNTIMEDOUT,E^.Socket,E^.Enable_RD,E^.Enable_WR,0);
         EFOS_OS_CONNECTION_REFUSED : FHandler(esv_SOCKCONNREFUSED,E^.Socket,E^.Enable_RD,E^.Enable_WR,0);
         else                         FHandler(esv_SOCKCANTCONNECT,E^.Socket,E^.Enable_RD,E^.Enable_WR,0);
       end;
     end;
     ess_CONNECTED: begin
       E^.Socket._SetClosed;
       FHandler(esv_SOCKCLOSED,E^.Socket,E^.Enable_RD,E^.Enable_WR,E^.Return_Data);
     end;
     else DBT.CriticalAbort('HANDLE IT RIGHT (EV_EOF IN STATE) = %s',[CFOS_FCOM_SOCKSTATE[LSOCK_STATE]]);
   end;
  end;
  procedure _CloseSocketAndFreeEventSource;
  begin
   E^.Free_Dynamic_Src:=true;
   E^.Socket.Close;
   E^.Socket:=nil;
  end;
  procedure _FetchNewServerSock;
  var FOSError:EFOS_OS_ERROR;
      PES:PFOS_APS_EVENTSOURCE;
      NSS:IFCOM_SOCK;
  begin
     FOSError:=E^.Socket.Accept(NSS);
     if FOSError=EFOS_OS_OK then begin
      PES:=AllocateNewES;
      with PES^ do begin
        APF       := @_ProcessSocketEvents;
        Handle    := NSS.GetMonitorHandleKey;
        Socket    := NSS;
        Typ       := apse_SERVER_SOCKET;
        NewServersock(PES);
        ServerHandler(esv_SOCKCONNECTED,NSS,Enable_RD,Enable_WR,0);
        AddEventSource(PES);
      end;
     end else begin
      LogError('ACCEPT ERROR FILT=%d Data=%d Flags=%d FFLags%d',[E^.Return_Filter,E^.Return_Data,E^.Return_Flags,E^.Return_FFlags]);
     end;
  end;

begin
   case Phase.PHASE of
    pfp_Announce_Resources: ;
    pfp_Run: begin
     LogInfo(' PROCESS SOCKET EVENTS FILT=%d FLAGS=%x FFLAGS=%x DATA=%x',[E^.Return_Filter,E^.Return_Flags,E^.Return_FFlags,E^.Return_Data],TID);
     if (E^.Return_Flags and EV_ERROR)>0 then begin
      LogError('HARD ERROR CONDITION - SOCKET ERROR -> %s',[CFOS_OS_ERROR[fcom_interpret_OS_Error(E^.Return_Data)]],TID);
      DBT.CriticalAbort('HARD ERROR CONDITION - SOCKET ERROR -> %s',[CFOS_OS_ERROR[fcom_interpret_OS_Error(E^.Return_Data)]]);
     end;
     if E^.Return_FFlags<>0 then begin
      LogError('ERROR CONDITION - SOCKET ERROR -> %s',[CFOS_OS_ERROR[fcom_interpret_OS_Error(E^.Return_FFlags)]],TID);
     end;
     LSOCK_STATE:=E^.Socket.GetSocketState;
     case E^.Typ of
       apse_CLIENT_SOCK,apse_SERVER_SOCKET: begin
         if E^.Typ=apse_CLIENT_SOCK then FHandler:=@ClientHandler else FHandler:=@ServerHandler;
         if _Error then begin
           FHandler(esv_SOCKEXCEPT,E^.Socket,E^.Enable_RD,E^.Enable_WR,0);
           _CloseSocketAndFreeEventSource;
           exit;
         end else
         if _EOF then begin // EOF STATE
           _HandleEOF;
           _CloseSocketAndFreeEventSource;
           exit;
         end;
         case LSOCK_STATE of
           ess_CONNECTING: begin
             E^.Socket._SetConnected;
             FHandler(esv_SOCKCONNECTED,E^.Socket,E^.Enable_RD,E^.Enable_WR,0);
             if E^.Return_Filter=EVFILT_READ then begin
               FHandler(esv_SOCKREAD,E^.Socket,E^.Enable_RD,E^.Enable_WR,E^.Return_Data);
             end else begin
               FHandler(esv_SOCKWRITE,E^.Socket,E^.Enable_RD,E^.Enable_WR,E^.Return_Data);
             end;
             exit;
           end;
           ess_CONNECTED: begin
             if E^.Return_Filter=EVFILT_READ then begin
               FHandler(esv_SOCKREAD,E^.Socket,E^.Enable_RD,E^.Enable_WR,E^.Return_Data);
             end else begin
               FHandler(esv_SOCKWRITE,E^.Socket,E^.Enable_RD,E^.Enable_WR,E^.Return_Data);
             end;
             exit;
           end;
           else begin
             if E^.Return_Filter=EVFILT_READ then begin
               DBT.CriticalAbort('HANDLE IT RIGHT "GOOD READ" IN STATE = %s',[CFOS_FCOM_SOCKSTATE[LSOCK_STATE]]);
             end else begin
               DBT.CriticalAbort('HANDLE IT RIGHT "GOOD WRITE" IN STATE = %s',[CFOS_FCOM_SOCKSTATE[LSOCK_STATE]]);
             end;
           end;
         end;
       end;
       apse_LISTEN_SOCKET: begin _FetchNewServerSock; exit; end;
       else begin DBT.CriticalAbort('processsocketevents unknown eventtype %d',[E^.Typ]); end;
      end;
      DBT.CriticalAbort('PANIC - Unhandled Socket Event %d',[E^.Typ]);
    end;
  end;
end;

procedure TFCOM_ENGINE.ClientHandler(const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;var Request_RD,Request_WR:Boolean;const Datacount:Integer);
begin
  LogInfo('CLIENT HANDLER HANDLE (%d) / <%s> - RD(%s) WR(%s) - Data( %d) ',[Sock.GetMonitorHandleKey,CFOS_FCOM_MULTIEVENT[Event],CFOS_BOOL[Request_RD],CFOS_BOOL[Request_WR],Datacount]);
end;

procedure TFCOM_ENGINE.ServerHandler(const Event: EFOS_FCOM_MULTIEVENT;
  const SOCK: IFCOM_SOCK; var Request_RD, Request_WR: Boolean;
  const Datacount: Integer);
begin
  LogInfo('SERVER HANDLER HANDLE (%d) / <%s> - RD(%s) WR(%s) - Data( %d) ',[Sock.GetMonitorHandleKey,CFOS_FCOM_MULTIEVENT[Event],CFOS_BOOL[Request_RD],CFOS_BOOL[Request_WR],Datacount]);
end;

procedure TFCOM_ENGINE.NewServersock(out PES: PFOS_APS_EVENTSOURCE);
begin
  LogInfo('NEW SERVER SOCKET (%d)',[PES^.Socket.GetMonitorHandleKey]);
end;


function TFCOM_ENGINE.AddClient(out   PES:PFOS_APS_EVENTSOURCE;
  const Target_IP: String;
  const TargetPort: integer; const IP_Layer: FCOM_IP_LAYER;
  const PROTOCOL: FCOM_SOCKET_PROTCOL;const BindIP: String;
  const BindPort: integer
  ): EFOS_FCOM_MULTIERROR;

var AI:IFCOM_AI;
      sw,sr:fcom_int;
      ra:boolean;
      ip:string;
      port:integer;
      CLIENT_SOCK:IFCOM_SOCK;
      FOSError:EFOS_OS_ERROR;
begin
   PES:=nil;
   AI:=Get_FCOM_AI;
   case AI.ResolveandSet(Target_IP,TargetPort) of
    fat_INVALID,fat_UNSPEC: begin
     result:=ese_CANNOT_RESOLVE;
     AI:=nil;
     exit;
    end;
   end;
   CLIENT_SOCK:=Get_FCOM_NETSOCK(fil_IPV4,fsp_TCP,FOSError);
   if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_CREATESOCKET;exit;end;
   FOSError:=CLIENT_SOCK.SetBlocking(false);
   if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
   PES:=AllocateNewES;
   with PES^ do begin
     APF       := @_ProcessSocketEvents;
     Handle    := CLIENT_SOCK.GetMonitorHandleKey;
     Socket    := CLIENT_SOCK;
     Typ       := apse_CLIENT_SOCK;
     Enable_RD := True;
     Enable_WR := True;
   end;
   FOSError:=CLIENT_SOCK.Connect(AI);
   AddEventSource(PES);
   Result:=ese_OK;
end;

function TFCOM_ENGINE.AddListener(out PES:PFOS_APS_EVENTSOURCE;
  const Bind_IP: String;
  const Bind_Port: integer; const IP_Layer: FCOM_IP_LAYER;
  const PROTOCOL: FCOM_SOCKET_PROTCOL; const listener_reuse: boolean
  ): EFOS_FCOM_MULTIERROR;
var AI:IFCOM_AI;
    LISTENER:IFCOM_SOCK;
    FOSError:EFOS_OS_ERROR;
begin
    PES:=nil;
    AI:=Get_FCOM_AI;
    case AI.ResolveandSet(Bind_IP,Bind_Port) of
     fat_INVALID,fat_UNSPEC: begin
      result:=ese_CANNOT_RESOLVE;
      AI:=nil;
      exit;
     end;
    end;
    LISTENER:=Get_FCOM_NETSOCK(IP_Layer,PROTOCOL,FOSError);
    if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_CREATESOCKET;exit;end;
    FOSError:=LISTENER.SetBlocking(false);
    if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
    if listener_reuse then begin
      FOSError:=LISTENER.SetListenerReuse(true);
      if FOSError<>EFOS_OS_OK then begin result:=ese_INTERNAL;exit;end;
    end;
    FOSError:=LISTENER.Bind(AI);
    if FOSError<>EFOS_OS_OK then begin result:=ese_CANNOT_BIND;exit; end;
    FOSError:=LISTENER.Listen(100);
    if FOSError<>EFOS_OS_OK then begin
     result:=ese_CANNOT_LISTEN;
    end;
    PES:=AllocateNewES;
    with PES^ do begin
      APF       := @_ProcessSocketEvents;
      Handle    := LISTENER.GetMonitorHandleKey;
      Socket    := LISTENER;
      Typ       := apse_LISTEN_SOCKET;
      Enable_RD := True;
      Enable_WR := False; //
    end;
    AddEventSource(PES);
    Result:=ese_OK;
end;

end.

