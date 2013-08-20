unit fos_fcom_socket;

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

{$MINENUMSIZE 4}
{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

interface

uses FOS_FCOM_TYPES
     {$IFDEF FPC}
       {$IFDEF FREEBSD}
        ,initc
       {$ELSE}
        ,Sockets
       {$ENDIF}
      {$IFDEF UNIX}
//       ,BaseUnix,
      {$ENDIF}
      {$IFDEF WIN32}
      ,windows,WinSock2
      {$ENDIF}
     {$ELSE} // Delphi
      Winsock
     {$ENDIF}
     ;



function  fcom_socket           (af:fcom_int; structure:fcom_int; protocol: fcom_int):fcom_int;
function  fcom_connect          (s: fcom_int; const sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
function  fcom_listen           (s: fcom_int; backlog: fcom_int): fcom_int;
function  fcom_accept           (s: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; var sockaddrlen: fcom_socklen_t): fcom_int;
function  fcom_bind             (s: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
function  fcom_close            (s: fcom_int):fcom_int;
function  fcom_shutdown         (s: fcom_int; how: fcom_int): fcom_int;

function  fcom_sendto           (s: fcom_int; const Buf; len, flags: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
function  fcom_recvfrom         (s: fcom_int; var Buf; len, flags: Integer; sockaddr: PFCOM_SOCKADDRSTORAGE; var sockaddrlen: fcom_socklen_t): fcom_int;
function  fcom_select           (nfds: fcom_int; readfds, writefds, exceptfds: PFCOM_FDSet; timeout: PFCOM_TimeVal): Longint;

function  fcom_send             (s: integer; var Buf; len, flags: Integer): Integer;
function  fcom_recv             (s: integer; var Buf; len, flags: Integer): Integer;

procedure fcom_FD_CLR           (Socket: fcom_int; var FDSet: TFCOM_FDSet);
function  fcom_FD_ISSET         (Socket: fcom_int; var FDSet: TFCOM_FDSet): Boolean;
procedure fcom_FD_SET           (Socket: fcom_int; var FDSet: TFCOM_FDSet);
procedure fcom_FD_ZERO          (var FDSet: TFCOM_FDSet);

function  fcom_GetLastSockError: fcom_int;

function fcom_SetBlocking(const socket:fcom_int;const bOn: Boolean):fcom_int;
function fcom_Datacount(const socket: fcom_int; out DataCount:fcom_int):fcom_int;
function fcom_SetListenerReuse(const socket:fcom_int;const bOn: Boolean):fcom_int;
function fcom_SetNoDelay(const socket:fcom_int;const bOn: Boolean):fcom_int;
function fcom_Get_SOERROR(const socket: fcom_int; out Error:fcom_int):fcom_int;


procedure fcom_InitSocketInterface;
procedure fcom_DestroySocketInterface;

procedure fcom_socket_testsuite;

implementation


uses {$IFDEF UNIX}
      BaseUnix,Termio,
     {$ENDIF}
     sysutils;

{$IFDEF WIN32}
procedure win_FD_CLR(Socket: TSocket; var FDSet: TFCOM_FDSet);
var  I: cardinal;
begin
  I := 0;
  while I < FDSet.fd_count do  begin
    if FDSet.fd_array[I] = Socket then begin
      while I < FDSet.fd_count - 1 do begin
        FDSet.fd_array[I] := FDSet.fd_array[I + 1];
        Inc(I);
      end;
      Dec(FDSet.fd_count);
      Break;
    end;
    Inc(I);
  end;
end;

procedure win_FD_SET(Socket: TSocket; var FDSet: TFCOM_FDSet);
begin
  if FDSet.fd_count < fcom_FD_SETSIZE then begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure win_FD_ZERO(var FDSet: TFCOM_FDSet);
begin
  FDSet.fd_count := 0;
end;

function __MyWSAFDIsSet(s: TSOcket; var FDSet: TFCOM_FDSet): Bool; stdcall; external WINSOCK2_DLL name '__WSAFDIsSet';

function win_FD_ISSET(Socket: TSocket; var FDSet: TFCOM_FDSet): Boolean;
begin
  win_FD_ISSET := __MyWSAFDIsSet(Socket, FDSet);
end;
{$ENDIF}

{$IFDEF FREEBSD}
  type
    psockaddr=PFCOM_SOCKADDRSTORAGE;

  //FPC Freebsd Compatibility Block - FPC Syscalls/Errno not working right on freebsd
  function  fpSocket      (domain,typ,protocol:cint):cint ; cdecl ; external 'c' name 'socket';
  function  fprecv        (s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; cdecl ; external 'c' name 'recv';
  function  fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; cdecl ; external 'c' name 'recvfrom';
  function  fpsend        (s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; cdecl ; external 'c' name 'send';
  function  fpsendto      (s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;cdecl ; external 'c' name 'sendto';
  function  fpbind        (s:cint; addrx : psockaddr; addrlen : tsocklen):cint; cdecl ; external 'c' name 'bind';
  function  fplisten      (s:cint; backlog : cint):cint; cdecl ; external 'c' name 'listen';
  function  fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; cdecl ; external 'c' name 'accept';
  function  fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint; cdecl ; external 'c' name 'connect';
  function  fpshutdown    (s:cint; how:cint):cint; cdecl ; external 'c' name 'shutdown';
  //function  fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint;
  //function  fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint;
  function  fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; cdecl ; external 'c' name 'getsockopt';
  function  fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : tsocklen):cint; cdecl ; external 'c' name 'setsockopt';
{$ENDIF}

function  fcom_socket(af:fcom_int; structure:fcom_int; protocol: fcom_int):fcom_int;
begin
 {$IFDEF FPC}
  result:=fpSocket(af,structure,protocol);
 {$ELSE}
  result:=Socket(af,structure,protocol);
 {$ENDIF}
end;

function  fcom_connect(s: fcom_int; const sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
begin
 {$IFDEF FPC}
  result:=fpConnect(s, pointer(sockaddr), sockaddrlen);
 {$ELSE}
  result:=Connect(s, PSOCKAddr(sockaddr)^, sockaddrlen);
 {$ENDIF}
end;

function  fcom_listen(s: fcom_int; backlog: fcom_int): fcom_int;
begin
 {$IFDEF FPC}
  result:=fpListen(s,backlog);
 {$ELSE}
  result:=Listen(s,backlog);
 {$ENDIF}
end;

function  fcom_accept(s: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; var sockaddrlen: fcom_socklen_t): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpAccept(s,pointer(sockaddr),@sockaddrlen);
 {$ELSE}
 result:=Accept(s,PSOCKADDR(sockaddr),@sockaddrlen);
 {$ENDIF}
end;

function  fcom_bind(s: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpBind(s,pointer(sockaddr),sockaddrlen);
 {$ELSE}
 result:=Bind(s,PSockaddr(sockaddr)^,sockaddrlen);
 {$ENDIF}
end;

function  fcom_close(s: fcom_int):fcom_int;
begin
 {$IFDEF FPC}
 result:=fpClose(s);
 {$ELSE}
 result:=CloseSocket(s);
 {$ENDIF}
end;

function  fcom_shutdown(s: fcom_int; how: fcom_int): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpshutdown(s,how);
 {$ELSE}
 result:=Shutdown(s,how);
 {$ENDIF}
end;

function  fcom_sendto(s: fcom_int; const Buf; len, flags: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; sockaddrlen: fcom_socklen_t): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpSendTo(s,@buf,len,flags,pointer(sockaddr),sockaddrlen);
 {$ELSE}
 result:=SendTo(s,Pointer(buf)^,len,flags,PSOCKADDR(sockaddr)^,sockaddrlen);
 {$ENDIF}
end;

function  fcom_recvfrom(s: fcom_int; var Buf; len, flags: fcom_int; sockaddr: PFCOM_SOCKADDRSTORAGE; var sockaddrlen: fcom_socklen_t): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpRecvFrom(s,@buf,len,flags,pointer(sockaddr),psocklen(sockaddrlen));
 {$ELSE}
 result:=RecvFrom(s,buf,len,flags,PSOCKADDR(sockaddr)^,sockaddrlen);
 {$ENDIF}
end;

function  fcom_send(s: fcom_int; var Buf; len, flags: fcom_int): fcom_int;
begin
 {$IFDEF FPC}
  result:=fpsend(s,@buf,len,flags);
 {$ELSE}
  result:=Send(s,buf,len,flags);
 {$ENDIF}
end;

function  fcom_recv(s: fcom_int; var Buf; len, flags: fcom_int): fcom_int;
begin
 {$IFDEF FPC}
 result:=fpRecv(s,@buf,len,flags);
 {$ELSE}
 result:=Recv(s,buf,len,flags);
 {$ENDIF}
end;

function  fcom_select(nfds: Integer; readfds, writefds, exceptfds: PFCOM_FDSet; timeout: PFCOM_TimeVal): Longint;
begin
 {$IFDEF UNIX}
  result:=fpselect(nfds,PFDSet(readfds),PFDSet(writefds),PFDSet(exceptfds),PTimeval(timeout));
 {$ELSE}
  result:=select(nfds,PFDSet(readfds),PFDSet(writefds),PFDSet(exceptfds),PTimeval(timeout));
 {$ENDIF}
end;

procedure fcom_FD_CLR(Socket: fcom_int; var FDSet: TFCOM_FDSet);
begin
 {$IFDEF UNIX}
  fpFD_CLR(Socket,FDSet);
 {$ELSE}
  win_FD_CLR(Socket,FDSet);
 {$ENDIF}
end;

function  fcom_FD_ISSET(Socket: fcom_int; var FDSet: TFCOM_FDSet): Boolean;
begin
 {$IFDEF UNIX}
 result := fpFD_ISSET(Socket,FDSet)<>0;
 {$ELSE}
 result:= win_FD_ISSET(Socket,FDSet);
 {$ENDIF}
end;

procedure fcom_FD_SET(Socket: fcom_int; var FDSet: TFCOM_FDSet);
begin
 {$IFDEF UNIX}
  fpFD_SET(Socket,FDSet);
 {$ELSE}
  win_FD_SET(Socket,FDSet);
 {$ENDIF}
end;

procedure fcom_FD_ZERO(var FDSet: TFCOM_FDSet);
begin
 {$IFDEF UNIX}
  fpFD_ZERO(FDSet);
 {$ELSE}
  win_FD_ZERO(FDSet);
 {$ENDIF}
end;


function  fcom_ioctlsocket(s: fcom_int; cmd: fcom_uint; var arg: fcom_int): fcom_int;
begin
 {$IFDEF UNIX}
 result:=FpIOCtl(s,cmd,@arg);
 {$ELSE}
 result:=IoctlSocket(s,cmd,u_long(arg));
 {$ENDIF}
end;


function  fcom_GetLastSockError:fcom_int;
begin
 {$IFDEF FPC}
   {$IFDEF FREEBSD}
    //writeln('CERRNO ',fpgetCerrno,'  ',fpgeterrno);
    result:=fpgetCerrno;
   {$ELSE}
    result:=socketerror;
   {$ENDIF}
 {$ELSE}
 result:=WSAGetLastError;
 {$ENDIF}
end;

function fcom_SetBlocking(const socket:fcom_int;const bOn: Boolean):fcom_int;
var opt:fcom_int;
begin
 if bON then opt:=0 else opt:=1;
 result:=fcom_IoctlSocket(Socket, FIONBIO,opt);
end;

function fcom_Datacount(const socket: fcom_int; out DataCount: fcom_int):fcom_int;
begin
 Datacount:=-1;
 result:=fcom_ioctlsocket(socket,FIONREAD,DataCount);
end;

function fcom_SetListenerReuse(const socket:fcom_int;const bOn: Boolean):fcom_int;
var  opt: longint;
begin
  if bON then opt:=1 else opt:=0;
  {$IFDEF FPC}
   result:=fpsetsockopt(socket,FCOM_SOL_SOCKET,FCOM_SO_REUSEADDR,@opt,SizeOf(opt));
  {$ELSE}
   result:=Setsockopt(socket,FCOM_SOL_SOCKET,FCOM_SO_REUSEADDR,@opt,SizeOf(opt));
  {$ENDIF}
end;

function fcom_SetNoDelay(const socket:fcom_int;const bOn: Boolean):fcom_int;
var  opt: longint;
begin
  if bON then opt:=1 else opt:=0;
  {$IFDEF FPC}
   result:=fpsetsockopt(socket,FCOM_IPPROTO_TCP,FCOM_TCP_NODELAY,@opt,SizeOf(opt));
  {$ELSE}
   result:=setsockopt(socket,FCOM_IPPROTO_TCP,TCP_NODELAY,@opt,SizeOf(opt));
  {$ENDIF}
end;

function fcom_Get_SOERROR(const socket: fcom_int; out Error: fcom_int): fcom_int;
var  opt: Integer;
       l: fcom_socklen_t;
begin
 error:=0;
 l:=sizeof(opt);
  {$IFDEF FPC}
   result:=fpgetsockopt(socket,FCOM_SOL_SOCKET,FCOM_SO_ERROR,@opt,@l);
  {$ELSE}
   result:=getsockopt(socket,FCOM_SOL_SOCKET,SO_ERROR,@opt,@l);
  {$ENDIF}
 error:=opt;
end;


{
function  fcom_inet_pton(family:fcom_int;const strptr:PAnsiChar; const Addrinfo: PFCOM_InAddr):fcom_int;
begin
end;

function  fcom_inet_ntop(family:fcom_int;const Addrinfo: PFCOM_InAddr;const dst:PAnsiChar;const buflen:fcom_int):PAnsiChar;
begin
end;
}
{$IFNDEF FPC}
const WinsockLevel = $0202;
var   WsaDataOnce:TWSADATA;
{$ENDIF}

procedure fcom_InitSocketInterface;
begin
 {$IFDEF FPC}
  //Already done in sockets
 {$ELSE}
  WSAStartup($0202,WsaDataOnce);
 {$ENDIF}
end;
procedure fcom_DestroySocketInterface;
begin
 {$IFDEF FPC}
  //Already done in sockets
 {$ELSE}
 WSACleanup;
 {$ENDIF}
end;

procedure fcom_socket_testsuite;
var res:fcom_int;
begin
 res:=fcom_socket(0,0,0);
 if res=FCOM_SOCKET_ERROR then begin
  res:=fcom_GetLastSockError;
  if fcom_interpret_OS_Error(res)<>EFOS_OS_EINVAL then raise Exception.create('BASIC OS EINVAL TEST FAILED');
 end;
end;

initialization

finalization


end.