unit fos_session_engine;

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

uses Classes, SysUtils,FOS_FCOM_ENGINE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_APS,FOS_BASIS_TOOLS;

 type

    { TFOS_SESSION_PROCESS }

    TFOS_SESSION_PROCESS=class (TFCOM_ENGINE)
    private
      LST,CLI,SER:PFOS_APS_EVENTSOURCE;
    public
      procedure Setup                      (const SD:RFOS_APS_PROCESS_SETUP_DATA);override;
      procedure Teardown                   ;override;
      procedure ClientHandler              (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;var Request_RD,Request_WR:Boolean;const Datacount:Integer);override;
      procedure ServerHandler              (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;var Request_RD,Request_WR:Boolean;const Datacount:Integer);override;
      procedure NewServersock              (out PES: PFOS_APS_EVENTSOURCE);override;
      procedure TerminateSignal            ;override;
      procedure InterruptSignal            ;override;
      procedure HangupSignal               ;override;
    end;

implementation

{ TFOS_SESSION_PROCESS }

procedure TFOS_SESSION_PROCESS.Setup(const SD: RFOS_APS_PROCESS_SETUP_DATA);
  procedure Testclient;
  var res:EFOS_FCOM_MULTIERROR;
  begin
  //     res:=AddClient('10.220.250.128',81,fil_IPV4,fsp_TCP,MY_CLIENT);
  //     res:=AddClient('10.220.250.128',80,fil_IPV4,fsp_TCP,MY_CLIENT);
  //     res:=AddClient('127.0.0.1',81,fil_IPV4,fsp_TCP,MY_CLIENT);
   res:=AddClient(CLI,'127.0.0.1',44001,fil_IPV4,fsp_TCP);
   if res<>ese_OK then begin
    DBT.CriticalAbort('CLIENT ADD FAILED WITH > %s',[CFOS_FCOM_MULTIERROR[res]]);
   end;
  end;

  procedure TestServer;
  var res:EFOS_FCOM_MULTIERROR;
  begin
    res:=AddListener(LST,'0.0.0.0',44002,fil_IPV4,fsp_TCP,true);
    if res<>ese_OK then begin
      DBT.CriticalAbort('SERVER ADD FAILED WITH > %s',[CFOS_FCOM_MULTIERROR[res]]);
    end;
  end;
begin
  inherited Setup(SD);
  TestServer;
  LogInfo('SETUP APS-SESSION PROCESS with %d CPU''s and %d THREADS',[sd.CPU_COUNT,sd.THREAD_COUNT]);
end;

procedure TFOS_SESSION_PROCESS.Teardown;
begin
  if assigned(LST) then Dispose(LST);
  if assigned(CLI) then Dispose(CLI);
  if assigned(SER) then Dispose(SER);
  inherited Teardown;
end;

procedure TFOS_SESSION_PROCESS.ClientHandler(const Event: EFOS_FCOM_MULTIEVENT;
  const SOCK: IFCOM_SOCK; var Request_RD, Request_WR: Boolean;
  const Datacount: Integer);
var SR:integer;
     s:string;
begin
 inherited;
 case Event of
  esv_SOCKCONNECTED:begin
    Request_RD:=true;
    Request_WR:=false;
  end;
  esv_SOCKREAD:begin
    SOCK.ReceiveString(s,Datacount,sr);
    LogInfo(s);
  end;
 end;
end;

procedure TFOS_SESSION_PROCESS.ServerHandler(const Event: EFOS_FCOM_MULTIEVENT;
  const SOCK: IFCOM_SOCK; var Request_RD, Request_WR: Boolean;
  const Datacount: Integer);
var s:String;
      sw:integer;
begin
    inherited;
    case Event of
     esv_SOCKCONNECTED:begin
       Request_RD:=true;
       Request_WR:=true;
     end;
     esv_SOCKREAD:begin
       SOCK.ReceiveString(s,Datacount,sw);
       LogInfo(s);
     end;
     esv_SOCKWRITE:begin
       SOCK.SendString('HELLO <CL-1>',sw);
       Request_RD:=true;
       Request_WR:=false;
     end;
    end;
end;

procedure TFOS_SESSION_PROCESS.NewServersock(out PES: PFOS_APS_EVENTSOURCE);
begin
  inherited NewServersock(PES);
end;

procedure TFOS_SESSION_PROCESS.TerminateSignal;
begin
  writeln('SESSION GOT TERM');
  ExitProcess;
end;

procedure TFOS_SESSION_PROCESS.InterruptSignal;
begin
  writeln('SESSION GOT INTERRUPT');
  ExitProcess;
end;

procedure TFOS_SESSION_PROCESS.HangupSignal;
begin
  writeln('SESSION GOT HUP');
end;

end.

