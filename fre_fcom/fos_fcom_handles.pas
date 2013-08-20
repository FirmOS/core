unit fos_fcom_handles;

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
{$MODE objfpc}
{$modeswitch nestedprocvars}
{$H+}

interface

uses FOS_FCOM_INTERFACES,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,FRE_FCOM_SSL,FRE_SYSTEM,FRE_APS_INTERFACE,FOS_INTERLOCKED,classes,ctypes,sockets;


//TODO
//* IPV6 Resolving 

type
    EFRE_SSL_Exception=class(EFRE_Exception);

    { TFCOM_AI }
    TFCOM_BASE=class(TObject,IFCOM_BASE)
      procedure Finalize       (const finalize_key:integer=0);virtual;
      function  GetImplementor :TObject;
    end;

    TFCOM_AI=class(TFCOM_BASE,IFCOM_AI)
     private
      Address     : TFCOM_SOCKADDRSTORAGE;
      Address_Len : fcom_socklen_t;
     public
      constructor Create;
      function  ResolveandSet  (addr:string;port:fcom_u_short):FCOM_ADDRESS_TYPE; // Tries to set Numerical IP4 / IP6 or symbolic addresses
      function  GetAddressType :FCOM_ADDRESS_TYPE;
      function  GetAddress     :PFCOM_SOCKADDRSTORAGE; // Get in sockaddr_storage format
      function  GetAddressLen  :fcom_socklen_t; // Length of sockaddr_storage address
      function  IPasString     :String;
      function  PortasString   :String;
      function  SocketAsString :String;
      procedure SetAddressLen  (const len:fcom_socklen_t); // Length of sockaddr_storage address
    end;

    { TFCOM_SOCK }

    { TFCOM_Handle }

    { TOffloadWriteObject }

    TOffloadWriteObject = class(TMemoryStream)
      FatalClose  : integer;
      FSSLEncode  : boolean;
      constructor CreateFinalizeCode(const hint:integer);
      constructor create(const data : string;const ssl_encode:boolean);
      constructor create(const buf:pointer;const w_size:integer;const ssl_encode:boolean);
    end;

    TFCOM_Handle=class(TFCOM_BASE,IFCOM_HANDLE)
    private
      fHandle                : fcom_int;
      fData                  : pointer;
      fESData                : IFRE_APS_EVENTSOURCE;
      //FRead_Event_CB         : TFRE_SimpleCallback;
      //FWrite_Event_CB        : TFRE_SimpleCallback;
      //FDisable_free_Event_CB : TFRE_SimpleCallback;
      //FQuiesce_EventSource   : TFRE_SimpleCallback;
    public
      function    GetHandleKey        :FCOM_HandleKey;
      procedure   SetData             (const Data:Pointer);
      function    GetData             :Pointer;
      procedure   SetES               (const Data:IFRE_APS_EVENTSOURCE);
      function    GetES               :IFRE_APS_EVENTSOURCE;
      property    EventSource         :IFRE_APS_EVENTSOURCE read GetES write SetES;
    end;


     TFCOM_SOCK=class(TFCOM_Handle,IFCOM_SOCK)
     private
      const
        cSSL_WBUF_SIZE = 20*1024;
        cSSL_RBUF_SIZE = 20*1024;
     protected
       FWriteQ         : IFOS_LFQ;
      _SSL             : PSSL;
      _SSL_CTX         : PSSL_CTX;
      FSSL_WRITEBUFFER : Pointer; // allocated on setup ssl context
      FSSL_READBUFFER  : Pointer;


      FSockReadState   : EFOS_FCOM_READ_SOCKSTATE;
      FSockWriteState  : EFOS_FCOM_WRITE_SOCKSTATE;

      fIPLayer         : FCOM_IP_LAYER;
      fProto           : FCOM_SOCKET_PROTCOL;
      fTyp             : FCOM_SOCKET_TYPE;
      fMyAI            : IFCOM_AI;
      Finternal_bio,
      Fnetwork_bio     : PBIO;
      fSSL_Error       : string;
      FSSL_State       : TFRE_FCOM_SSL_STATE;
      FSSL_SetupDone   : boolean;
      FCurrentOffloadWorkObject            : TOffloadWriteObject;
      FCurrentSSLEncodeObject              : TOffloadWriteObject;
      FIL_IsOffloading                     : NativeUint;
      FIL_IsClosing                        : NativeUint;

      FIL_ScheduledOffloadwrites           : NativeUint;

      procedure   _Initialize              (const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL);
      function    _CheckSocketResult       (const res:fcom_int):EFOS_OS_ERROR;
      procedure   _CheckSocketType         (const wish_type:FCOM_SOCKET_TYPE;const err:string);
      function    _CheckSocketResultValue  (const res:fcom_int;out value:fcom_int):EFOS_OS_ERROR;
      function    Is_SSL_Enabled_Socket    :boolean;
      function    IFCOM_SOCK.Accept        = AcceptI;
     public
      procedure   _RequestWriteEV          ;
      procedure   _RequestReadEV           ;
      function    Offload_Write_TS    (const TID:NAtiveUint):integer;
      function    _Handle_SSL_Error   (res:integer):EFOS_OS_ERROR;
      constructor AcceptSocketCreate  (const Socket:fcom_int;IPLayer:FCOM_IP_LAYER;Protocol:FCOM_SOCKET_PROTCOL;const AI: TFCOM_AI);
      constructor Create              (const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR);
      destructor  Destroy             ;override;
      function    GetSocketType       :FCOM_SOCKET_TYPE;
      function    SetBlocking         (const bON:Boolean):EFOS_OS_ERROR;
      function    SetNoDelay          (const bON:Boolean):EFOS_OS_ERROR;
      function    SetListenerReuse    (const bON:Boolean):EFOS_OS_ERROR;
      function    Connect             (const AI:IFCOM_AI):EFOS_OS_ERROR;
      function    Bind                (const AI:IFCOM_AI):EFOS_OS_ERROR;
      function    Listen              (const Backlog:fcom_int):EFOS_OS_ERROR;
      function    Accept              (out   NewSock:TFCOM_SOCK):EFOS_OS_ERROR;

      function    Shutdown            (const How:FCOM_SHUTDOWN_TYPE):EFOS_OS_ERROR;
      function    GetSocketErrorstate (out errorstate:EFOS_OS_ERROR):EFOS_OS_ERROR;
      function    SockClose           :EFOS_OS_ERROR;
      function    CloseEnqueue        (const hint:integer):EFOS_OS_ERROR; // "Offload Write a Shutdown"
      function    Datacount           (out Count:fcom_int):EFOS_OS_ERROR;
      function    Readable            (const Timeout:fcom_int;out rdable:boolean):EFOS_OS_ERROR;
      function    Get_AI              :IFCOM_AI;

      function    GetSocketReadState  :EFOS_FCOM_READ_SOCKSTATE;
      function    GetSocketWriteState :EFOS_FCOM_WRITE_SOCKSTATE;

      function    GetVerboseDesc      : String;

      procedure   RequestClose        ;

      procedure   _SetReadConnected;   //Sets internal Read State to "Connected"
      procedure   _SetWriteConnected;   //Sets internal Read State to "Connected"
      procedure   _SetReadClosed;      //Sets internal State to "Closed"
      procedure   _SetWriteClosed;

      procedure   Set_SSL_CTX         (const SSL_CTX : PSSL_CTX;const IsServer:boolean);
      function    SSL_Accept          :EFOS_OS_ERROR;
      function    SSL_Connect         :EFOS_OS_ERROR;
      function    SSL_State           :TFRE_FCOM_SSL_STATE;
      function    SSL_Pending         :integer;
      function    SSL_Wants           (out wants : TFRE_FCOM_SSL_WANTS;var amount:integer):EFOS_OS_ERROR;
      function    SSL_ReadTransfer    (const amount:integer):EFOS_OS_ERROR;
      function    SSL_WriteTransfer   :EFOS_OS_ERROR;
      function    SSL_ReadFromSSL     (const buf: pointer; const max_len: integer; out act_len: integer): EFOS_OS_ERROR;
      function    SSL_WriteToSSL      (const buf: pointer; const max_len: integer; out act_len: integer): EFOS_OS_ERROR;
      function    SSL_ShutDown        : EFOS_OS_ERROR;
      function    Get_SSL_ErrorString : string;

      procedure   Offload_Close       (const hint:integer);

      procedure   Offload_Write       (const data:String); // Send direct or prepare write completion via APS
      procedure   Offload_WriteBuf    (const data_ptr: Pointer; const size: integer; const ssl_encode: boolean ; const AI:IFCOM_AI=nil); // Send direct or prepare write completion via APS

      function    AcceptI            (out   NewSock:IFCOM_SOCK):EFOS_OS_ERROR;


      function    Send                (const Data:Pointer; const Size: fcom_int;out SizeWritten:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
      function    SendString          (const Data:AnsiString; out SizeWritten:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
      function    Receive             (const Data:Pointer; const Size: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
      function    ReceiveString       (var   Data:Ansistring; const Size: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
      function    ReceiveString_TO    (var   Data:Ansistring; const Timeout: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
    end;

    function FOS_DNSQuery(Name: AnsiString; QType: TFCOM_DNS_QTYPES;var Reply:String;const WorkSock: IFCOM_SOCK): Boolean;
    function FCOM_AI_from_SOCKADDR(const sa:PFCOM_SOCKADDRSTORAGE):IFCOM_AI;

    function FCOM_Ip4ToStr(value: TFCOM_InAddr): string;
    function FCOM_Ip6ToStr(value: TFCOM_InAddr6): string;

    function fcom_hton4(host : cardinal):cardinal; inline;
    function fcom_ntoh4 (Net : cardinal) : cardinal; inline;
    function fcom_hton2( host : fcom_u_short):fcom_u_short; inline;
    function fcom_ntoh2 (Net : word):word; inline;


implementation
uses Sysutils,FOS_FCOM_SOCKET;
{ TFCOM_SOCK }

type tbytearr4= packed array [1..4] of byte;

function fcom_hton4(host : cardinal):cardinal; inline;
begin
{$ifdef FCOM_BE}
  result:=host;
{$else}
  result:=Tbytearr4(host)[4];
  result:=result or ((Tbytearr4(host)[3]) shl 8);
  result:=result or ((Tbytearr4(host)[2]) shl 16);
  result:=result or ((Tbytearr4(host)[1]) shl 24);
{$endif}
end;

function fcom_ntoh4 (Net : cardinal) : cardinal; inline;
begin
{$ifdef FCOM_BE}
  result:=net;
{$else}
  result:=Tbytearr4(Net)[4];
  result:=result or ( (Tbytearr4(Net)[3]) shl 8);
  result:=result or ( (Tbytearr4(Net)[2]) shl 16);
  result:=result or ( (Tbytearr4(Net)[1]) shl 24);
{$endif}
end;

function fcom_hton2( host : fcom_u_short):fcom_u_short; inline;
begin
{$ifdef FCOM_BE}
  result:=host;
{$else}
  result:=swap(host);
{$endif}
end;

function fcom_ntoh2 (Net : word):word; inline;
begin
{$ifdef FCOM_BE}
  result:=net;
{$else}
  result:=swap(net);
{$endif}
end;

procedure FOS_WordAtPos(var str:AnsiString;const W:WORD;const pos:cardinal);
begin
 str[pos  ]:=AnsiChar(W div 256);
 str[pos+1]:=AnsiChar(W mod 256);
end;

function FOS_CodeWord(Value: Word): Ansistring;
begin
  setlength(result, 2);
  result[1] := AnsiChar(Value div 256);
  result[2] := AnsiChar(Value mod 256);
end;

function FOS_DecodeInt(const Value: Ansistring; Index: Integer): Word;
var x, y: Byte;
begin
  if Length(Value) > Index then
    x := Ord(Value[Index])
  else
    x := 0;
  if Length(Value) >= (Index + 1) then
    y := Ord(Value[Index + 1])
  else
    y := 0;
  Result := x * 256 + y;
end;

function FCOM_Test_IP4(IP:String;const port:fcom_u_short;const Address:PFCOM_SOCKADDRSTORAGE;var Addrlen:fcom_socklen_t):Boolean; //UNIT TEST NEEDED
Var  D     : String;
     I,j,k : Longint;
begin
  result:=false;
  if assigned(Address) then begin
    Address^.sin_family:=FCOM_AF_UNSPEC;
    Address^.sin_addr.s_addr:=0;
    Address^.sin_port:=0;
    Addrlen:=0;
  end;
  For I:=0 to 3 do begin
    If I<3 Then begin
      J:=Pos('.',IP);If J=0 then exit;D:=Copy(IP,1,J-1);Delete(IP,1,J);
    end else begin
      D:=IP;
    end;
    Val(D,k,J);
    if assigned(Address) then begin
     Address^.sin_addr.s_bytes[4-i]:=k;
    end;
    If J<>0 then Exit;
   end;
   if assigned(Address) then begin
     Address^.sin_addr.s_addr:=fcom_ntoh4(Address^.sin_addr.s_addr);
     Address^.sin_port:=fcom_hton2(port);
     Address^.sin_family:=FCOM_AF_INET;
     Addrlen:=SizeOf(TFCOM_SOCKADDRSTORAGE);
   end;
   result:=true;
end;

function FCOM_Test_IP6(IP:String;const port:fcom_u_short;const address:PFCOM_SOCKADDRSTORAGE;var Addrlen:fcom_socklen_t): boolean;  //UNIT TEST NEEDED
Var Part:String;
    P,J,Index,ZeroAt:Integer;
    W:Word;
Begin
  result:=false;
  if assigned(address) then begin
    Address^.sin6_family:=FCOM_AF_UNSPEC;
    Address^.sin6_port:=0;
    Address^.sin6_flowinfo:=0;
    FillChar(Address^.sin6_addr,Sizeof(TFCOM_InAddr6),0);
    Addrlen:=0;
  end;
  Index := 0; ZeroAt := -1;
  J := 0;
  P := Pos(':',IP);
  while (P > 0) and (Length(IP) > 0) and (Index < 8) do begin
    Part := '$'+Copy(IP,1,P-1);
    Delete(IP,1,P);
    if Length(Part) > 1 then  { is there a digit after the '$'? }
      Val(Part,W,J)
    else W := 0;
    if assigned(address) then begin
      Address^.sin6_addr.u6_addr16[Index] := fcom_hton2(W);
    end;
    if J <> 0 then exit;
    if IP='' then begin
     inc(index);
     break;
    end;
    if IP[1] = ':' then begin
      ZeroAt := Index;
      Delete(IP,1,1);
    end;
    Inc(Index);
    P := Pos(':',IP); if P = 0 then P := Length(IP)+1;
  end;
  if p>0 then result:=true else exit;
  if assigned(address) then begin
    if ZeroAt >= 0 then begin
      Move(Address^.sin6_addr.u6_addr16[ZeroAt+1],Address^.sin6_addr.u6_addr16[(8-Index)+ZeroAt+1],2*(Index-ZeroAt-1));
      FillChar(Address^.sin6_addr.u6_addr16[ZeroAt+1],2*(8-Index),0);
    end;
    Address^.sin_port:=fcom_hton2(port);
    Address^.sin_family:=FCOM_AF_INET6;
    Addrlen:=SizeOf(TFCOM_SOCKADDRSTORAGE);
  end;
End;



function FCOM_ReverseIP(Value: AnsiString): AnsiString;
var x: Integer;
begin
  Result := '';
  repeat
   x := LastDelimiter('.', Value);
   Result := Result + '.' + Copy(Value, x + 1, Length(Value) - x);
   Delete(Value, x, Length(Value) - x + 1);
  until x < 1;
  if Length(Result) > 0 then begin
    if Result[1] = '.' then begin
      Delete(Result, 1, 1);
    end;
  end;
end;


function FCOM_ReverseIP6(Value: AnsiString): AnsiString;
var ip6: TFCOM_SockAddrIn6;
      i:integer;
     al:fcom_socklen_t;
begin
  al:=0;
  FCOM_Test_IP6(Value,0,@ip6,al);
  i:=16;
  result:=AnsiChar(ip6.sin6_addr.u6_addr8[i]);
  while i>0 do begin
   dec(i);
   result:=result+'.'+AnsiChar(ip6.sin6_addr.u6_addr8[i]);
  end;
  {
  Result := ip6.sin6_addr.S_un_b.s_b16
    + '.' + ip6.sin6_addr.S_un_b.s_b15
    + '.' + ip6.sin6_addr.S_un_b.s_b14
    + '.' + ip6.sin6_addr.S_un_b.s_b13
    + '.' + ip6.sin6_addr.S_un_b.s_b12
    + '.' + ip6.sin6_addr.S_un_b.s_b11
    + '.' + ip6.sin6_addr.S_un_b.s_b10
    + '.' + ip6.sin6_addr.S_un_b.s_b9
    + '.' + ip6.sin6_addr.S_un_b.s_b8
    + '.' + ip6.sin6_addr.S_un_b.s_b7
    + '.' + ip6.sin6_addr.S_un_b.s_b6
    + '.' + ip6.sin6_addr.S_un_b.s_b5
    + '.' + ip6.sin6_addr.S_un_b.s_b4
    + '.' + ip6.sin6_addr.S_un_b.s_b3
    + '.' + ip6.sin6_addr.S_un_b.s_b2
    + '.' + ip6.sin6_addr.S_un_b.s_b1;
  }
end;


function FCOM_Ip4ToStr(value: TFCOM_InAddr): string;
begin
 result:=Format('%d.%d.%d.%d',[value.s_bytes[1],value.s_bytes[2],value.s_bytes[3],value.s_bytes[4]]);
end;

function FCOM_Ip6ToStr(value: TFCOM_InAddr6): string;  //Routine from FPC Team
var
  i: byte;
  zr1,zr2: set of byte;
  zc1,zc2: byte;
  have_skipped: boolean;
  ip6w: array [0..7] of Word;
begin
  zr1 := [];
  zr2 := [];
  zc1 := 0;
  zc2 := 0;
  for i := 0 to 7 do begin
    ip6w[i] := value.u6_addr16[i];
    if ip6w[i] = 0 then begin
      include(zr2, i);
      inc(zc2);
    end else begin
      if zc1 < zc2 then begin
        zc1 := zc2;
        zr1 := zr2;
        zc2 := 0;
        zr2 := [];
      end;
    end;
  end;
  if zc1 < zc2 then begin
    zr1 := zr2;
  end;
  SetLength(Result, 8*5-1);
  SetLength(Result, 0);
  have_skipped := false;
  for i := 0 to 7 do
  begin
    if not(i in zr1) then
    begin
      if have_skipped then
      begin
        if Result = '' then
          Result := '::'
        else
          Result := Result + ':';
        have_skipped := false;
      end;
      Result := Result + IntToHex(fcom_ntoh2(Ip6w[i]), 1) + ':';
    end
    else
    begin
      have_skipped := true;
    end;
  end;
  if have_skipped then
    if Result = '' then
      Result := '::0'
    else
      Result := Result + ':';
  if Result = '' then
    Result := '::0';
  if not (7 in zr1) then
    SetLength(Result, Length(Result)-1);
//  Result := LowerCase(result);
end;


function FOS_DNSQuery(Name: AnsiString; QType: TFCOM_DNS_QTYPES;var Reply:String;const WorkSock: IFCOM_SOCK): Boolean;
var FID: Word;
    FRCode,wb,rb: Integer;
    FBuffer: AnsiString;
    FReply:TStringlist;
    FAuthoritative: Boolean;
    FTruncated: Boolean;
    temp_al:fcom_socklen_t;


      function Query(const Name: AnsiString; QType:TFCOM_DNS_QTYPES): AnsiString;
      var n: Integer;
          s: AnsiString;
      begin
        result:=StringOfChar(#0,12);
        FID:=Random(32767);
        FOS_WordAtPos(result,FID,1);//ID
        FOS_WordAtPos(result,$0100,3);//FLAGS
        FOS_WordAtPos(result,$0001,5);//QDCOUNT=1 | ANCOUNT=NSCOUNT=ARCOUNT=0
        if Name = '' then begin
          Result := #0
        end else begin
          s := '';
          for n := 1 to Length(Name) do begin
            if Name[n] = '.' then begin
              Result := Result + Char(Length(s)) + s;
              s := '';
            end else begin
              s := s + Name[n];
            end;
          end;
          if s <> '' then begin
            Result := Result + Char(Length(s)) + s;
          end;
          Result := Result + #0;
        end;
        Result := Result + FOS_CodeWord(ord(QType))+FOS_CodeWord(1);
      end;

      function DecodeString(var From: Integer): AnsiString;
      var Len: integer;
      begin
        Len := Ord(FBuffer[From]);
        Inc(From);
        Result := Copy(FBuffer, From, Len);
        Inc(From, Len);
      end;

      function DecodeLabels(var From: Integer): AnsiString;
      var l, f: Integer;
      begin
        Result := '';
        while True do begin
          if From >= Length(FBuffer) then Break;
          l := Ord(FBuffer[From]);
          Inc(From);
          if l = 0 then Break;
          if Result <> '' then Result := Result + '.';
          if (l and $C0) = $C0 then begin
            f := l and $3F;
            f := f * 256 + Ord(FBuffer[From]) + 1;
            Inc(From);
            Result := Result + DecodeLabels(f);
            Break;
          end else begin
            Result := Result + Copy(FBuffer, From, l);
            Inc(From, l);
          end;
        end;
      end;

      function DecodeResource(var i: Integer; const Info: TStringList;Qype: TFCOM_DNS_QTYPES): AnsiString;
      var Rname: AnsiString;
          RType:TFCOM_DNS_QTYPES;
          Len, j, x, y, z, n, k: Integer;
          R: AnsiString;
          t1, t2, ttl: integer;
          ip6: TFCOM_InAddr6;
      begin
        Result := '';
        R := '';
        Rname := DecodeLabels(i);
        RType := TFCOM_DNS_QTYPES(FOS_DecodeInt(FBuffer, i));
        Inc(i, 4);
        t1 := FOS_DecodeInt(FBuffer, i);
        Inc(i, 2);
        t2 := FOS_DecodeInt(FBuffer, i);
        Inc(i, 2);
        ttl := t1 * 65536 + t2;
        Len := FOS_DecodeInt(FBuffer, i);
        Inc(i, 2); // i point to begin of data
        j := i;
        i := i + len; // i point to next record
        if Length(FBuffer) >= (i - 1) then
          case RType of
            fdnsqt_A: begin
                R := IntToStr(Ord(FBuffer[j]));
                Inc(j);
                R := R + '.' + IntToStr(Ord(FBuffer[j]));
                Inc(j);
                R := R + '.' + IntToStr(Ord(FBuffer[j]));
                Inc(j);
                R := R + '.' + IntToStr(Ord(FBuffer[j]));
              end;
            fdnsqt_AAAA: begin
               k:=1;
               while k<=16 do begin
                ip6.u6_addr8[k]:=Byte(FBuffer[j+k-1]);
                inc(k);
               end;
               {
                ip6.sin6_addr.S_un_b.s_b1 := Char(FBuffer[j]);
                ip6.sin6_addr.S_un_b.s_b2 := Char(FBuffer[j + 1]);
                ip6.sin6_addr.S_un_b.s_b3 := Char(FBuffer[j + 2]);
                ip6.sin6_addr.S_un_b.s_b4 := Char(FBuffer[j + 3]);
                ip6.sin6_addr.S_un_b.s_b5 := Char(FBuffer[j + 4]);
                ip6.sin6_addr.S_un_b.s_b6 := Char(FBuffer[j + 5]);
                ip6.sin6_addr.S_un_b.s_b7 := Char(FBuffer[j + 6]);
                ip6.sin6_addr.S_un_b.s_b8 := Char(FBuffer[j + 7]);
                ip6.sin6_addr.S_un_b.s_b9 := Char(FBuffer[j + 8]);
                ip6.sin6_addr.S_un_b.s_b10 := Char(FBuffer[j + 9]);
                ip6.sin6_addr.S_un_b.s_b11 := Char(FBuffer[j + 10]);
                ip6.sin6_addr.S_un_b.s_b12 := Char(FBuffer[j + 11]);
                ip6.sin6_addr.S_un_b.s_b13 := Char(FBuffer[j + 12]);
                ip6.sin6_addr.S_un_b.s_b14 := Char(FBuffer[j + 13]);
                ip6.sin6_addr.S_un_b.s_b15 := Char(FBuffer[j + 14]);
                ip6.sin6_addr.S_un_b.s_b16 := Char(FBuffer[j + 15]);
                ip6.sin6_family := word(AF_INET6);
                ip6.sin6_port := 0;
                ip6.sin6_flowinfo := 0;
                ip6.sin6_scope_id := 0;
                R := FOS_IP6ToStr(ip6);
                }
            end;
            fdnsqt_NS, fdnsqt_MD, fdnsqt_MF, fdnsqt_CNAME, fdnsqt_MB,fdnsqt_MG, fdnsqt_MR, fdnsqt_PTR, fdnsqt_X25, fdnsqt_NSAP,fdnsqt_NSAPPTR:
              R := DecodeLabels(j);
            fdnsqt_SOA: begin
                R := DecodeLabels(j);
                R := R + ',' + DecodeLabels(j);
                for n := 1 to 5 do begin
                  x := FOS_DecodeInt(FBuffer, j) * 65536 + FOS_DecodeInt(FBuffer, j + 2);
                  Inc(j, 4);
                  R := R + ',' + IntToStr(x);
                end;
            end;
            fdnsqt_NULL,fdnsqt_WKS: ;
            fdnsqt_HINFO:
              begin
                R := DecodeString(j);
                R := R + ',' + DecodeString(j);
              end;
            fdnsqt_MINFO, fdnsqt_RP, fdnsqt_ISDN:
              begin
                R := DecodeLabels(j);
                R := R + ',' + DecodeLabels(j);
              end;
            fdnsqt_MX, fdnsqt_AFSDB, fdnsqt_RT, fdnsqt_KX:
              begin
                x := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                R := IntToStr(x);
                R := R + ',' + DecodeLabels(j);
              end;
            fdnsqt_TXT:
              begin
                R := '';
                while j < i do
                  R := R + DecodeString(j);
              end;
            fdnsqt_GPOS:
              begin
                R := DecodeLabels(j);
                R := R + ',' + DecodeLabels(j);
                R := R + ',' + DecodeLabels(j);
              end;
            fdnsqt_PX:
              begin
                x := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                R := IntToStr(x);
                R := R + ',' + DecodeLabels(j);
                R := R + ',' + DecodeLabels(j);
              end;
            fdnsqt_SRV: begin
                x := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                y := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                z := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                R := IntToStr(x);                     // Priority
                R := R + ',' + IntToStr(y);           // Weight
                R := R + ',' + IntToStr(z);           // Port
                R := R + ',' + DecodeLabels(j);       // Server DNS Name
              end;
            fdnsqt_NAPTR:
              begin
                x := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                y := FOS_DecodeInt(FBuffer, j);
                Inc(j, 2);
                R := IntToStr(x);                     // Order
                R := R + ',' + IntToStr(y);           // Preference
                R := R + ',' +DecodeString(j);             // Flags
                R := R + ',' +DecodeString(j);             // Service
                R := R + ',' +DecodeString(j);             // Regexpr
                R := R + ',' +DecodeLabels(j);             // Replacement
              end;

          end;
        if R <> '' then
          Info.Add(RName + ',' + IntToStr(Ord(RType)) + ',' + IntToStr(ttl) + ',' + R);
        if QType = RType then
          Result := R;
      end;


      function DecodeResponse(const Buf: AnsiString; const Reply: TStrings; QType: TFCOM_DNS_QTYPES):boolean;
      var n, i: Integer;
          flag, qdcount, ancount, nscount, arcount: Integer;
          s: AnsiString;
          FAnsferInfo: TStringList;
          FNameserverInfo: TStringList;
          FAdditionalInfo: TStringList;

      begin
        Result := False;
        FAnsferInfo:=TStringList.create;
        FNameserverInfo:=TStringList.create;
        FAdditionalInfo:=TStringList.create;
        try
          Reply.Clear;
          FAuthoritative := False;
          if (Length(Buf) > 13) and (FID = FOS_DecodeInt(Buf, 1)) then
          begin
            Result := True;
            flag := FOS_DecodeInt(Buf, 3);
            FRCode := Flag and $000F;
            FAuthoritative := (Flag and $0400) > 0;
            FTruncated := (Flag and $0200) > 0;
            if FRCode = 0 then begin
              qdcount := FOS_DecodeInt(Buf, 5);
              ancount := FOS_DecodeInt(Buf, 7);
              nscount := FOS_DecodeInt(Buf, 9);
              arcount := FOS_DecodeInt(Buf, 11);
              i := 13; //begin of body
              if (qdcount > 0) and (Length(Buf) > i) then //skip questions
                for n := 1 to qdcount do begin
                  while (Buf[i] <> #0) and ((Ord(Buf[i]) and $C0) <> $C0) do Inc(i);
                  Inc(i, 5);
                end;
              if (ancount > 0) and (Length(Buf) > i) then // decode reply
                for n := 1 to ancount do begin
                  s := DecodeResource(i, FAnsferInfo, QType);
                  if s <> '' then FReply.Add(s);
                end;
              if (nscount > 0) and (Length(Buf) > i) then begin // decode nameserver info
                for n := 1 to nscount do DecodeResource(i, FNameserverInfo, QType);
              end;
              if (arcount > 0) and (Length(Buf) > i) then begin// decode additional info
                for n := 1 to arcount do DecodeResource(i, FAdditionalInfo, QType);
              end;
            end;
          end;
        finally
          FAnsferInfo.Free;
          FNameserverInfo.Free;
          FAdditionalInfo.Free;
        end;
      end;




begin
  temp_al:=0;
  if Name='' then begin
   Reply:='';
   exit; //TBI
  end else
  if FCOM_Test_IP4(Name,0,nil,temp_al) then begin
    Name:=FCOM_ReverseIP(name)+'.in-addr.arpa';
  end else
  if FCOM_Test_IP6(name,0,nil,temp_al) then begin
    Name:=FCOM_ReverseIP6(name)+'.ip6.arpa';
  end;
  FBuffer := Query(Name, QType);
  Worksock.SendString(FBuffer,wb);
  WorkSock.ReceiveString_TO(FBuffer,10000,rb);
  FReply:=TStringlist.create;
  try
   Result := DecodeResponse(FBuffer, FReply, QType);
   Reply:=FReply.CommaText;
  finally
   FReply.Free;
  end;
end;

constructor TOffloadWriteObject.CreateFinalizeCode(const hint: integer);
begin
  if hint = 0 then raise Exception.Create('invalid createfinalizecode hint '+inttostr(hint));
  FatalClose := hint;
end;

constructor TOffloadWriteObject.create(const data: string; const ssl_encode: boolean);
begin
 Write(data[1],Length(data));
 Position:=0;
 FSSLEncode:=ssl_encode;
end;

constructor TOffloadWriteObject.create(const buf: pointer; const w_size: integer; const ssl_encode: boolean);
begin
  Write(buf^,w_size);
  Position:=0;
  FSSLEncode:=ssl_encode;
end;

procedure TFCOM_BASE.Finalize(const finalize_key:integer);
begin
  Free;
end;

function TFCOM_BASE.GetImplementor: TObject;
begin
  result := self;
end;


{
function FOS_GetMailServers(const DNSHost, Domain: AnsiString; var Servers: String): Boolean;
var  t,FServers: TStringList;
     n, m, x: Integer;
     Reply:TFOS_NPS;
     s:string;
begin
  Result := False;
  FServers:=TStringList.Create;
  t := TStringList.Create;
  try
    if FOS_DNSQuery(Domain,QTYPE_MX,Reply,DNSHost) then  begin
      Reply.GetProp('Result',s);
      t.Commatext:=s;
      for n := 0 to t.Count - 1 do begin
        x := Pos(',', t[n]);
        if x > 0 then begin
          for m := 1 to 6 - x do t[n] := '0' + t[n];
        end;
      end;
      t.Sorted := True;
      for n := 0 to t.Count - 1 do begin
        x := Pos(',', t[n]);
        FServers.Add(Copy(t[n], x + 1, Length(t[n]) - x));
      end;
      Result := True;
      Servers:=FServers.CommaText;
    end;
  finally
   t.Free;
   Reply.Free;
   FServers.Free;
  end;
end;
}



function TFCOM_SOCK.Listen(const Backlog: fcom_int): EFOS_OS_ERROR;
begin
 result := _CheckSocketResult(fcom_listen(fHandle, Backlog));
 fTyp   := ft_LISTEN;
 //_LockedSetSocketState(ess_LISTENING);
end;

function TFCOM_SOCK.Readable(const Timeout:fcom_int;out rdable:boolean):EFOS_OS_ERROR;
var  TimeVal: PFCOM_TimeVal;
     TimeV  : TFCOM_TimeVal;
     FDSet  : TFCOM_FDSet;
     val:fcom_int;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then TimeVal := nil;
  fcom_FD_ZERO(FDSet);
  fcom_FD_SET(fHandle, FDSet);
  result:= _CheckSocketResultValue(fcom_select(fHandle + 1, @FDSet, nil, nil, TimeVal),val);
  rdable:=val>0;
end;

function TFCOM_SOCK.Get_AI: IFCOM_AI;
begin
  result:=fMyAI;
end;

function TFCOM_SOCK.GetSocketReadState: EFOS_FCOM_READ_SOCKSTATE;
begin
  result := FSockReadState;
end;

function TFCOM_SOCK.GetSocketWriteState: EFOS_FCOM_WRITE_SOCKSTATE;
begin
  result := FSockWriteState;
end;

//function TFCOM_SOCK.GetSocketState: EFOS_FCOM_SOCKSTATE;
//var MK : NativeUint;
//begin
//  MK:=Enter_Sockstate;
//    result:=fState;
//  Leave_Sockstate(mk);
//end;

function TFCOM_SOCK.GetVerboseDesc: String;
begin
  result := Format('SOCKET:[%d][%s][%s]',[GetHandleKey,CFCOM_SOCKET_TYPE[fTyp],Get_AI.SocketAsString]);
end;

procedure TFCOM_SOCK.RequestClose;
begin
  _SetReadClosed;
  _SetWriteClosed;
end;

procedure TFCOM_SOCK._SetReadConnected;
begin
  FSockReadState := esrs_CONNECTED;
end;

procedure TFCOM_SOCK._SetWriteConnected;
begin
  FSockWriteState := esws_CONNECTED;
end;


procedure TFCOM_SOCK._SetReadClosed; // do not request free
begin
  Shutdown(fst_RD);
  FSockReadState := esrs_READ_CLOSED;
end;

procedure TFCOM_SOCK._SetWriteClosed; // do not request free
begin
  Shutdown(fst_WR);
  FSockWriteState := esws_WRITE_CLOSED;
end;




procedure TFCOM_SOCK.Set_SSL_CTX(const SSL_CTX: PSSL_CTX;const IsServer:boolean);
begin
  _SSL_CTX     := SSL_CTX;
  if IsServer then begin
    FSSL_State := fss_SERVER_NOT_SETUP;
  end else begin
    FSSL_State := fss_CLIENT_NOT_SETUP;
  end;
  FSSL_WRITEBUFFER := Getmem(cSSL_WBUF_SIZE);
  FSSL_READBUFFER  := Getmem(cSSL_RBUF_SIZE);
end;


//filetype must be PEM !
function TFCOM_SOCK.SSL_Accept:EFOS_OS_ERROR;
var flag    : cint;
     res    : integer;
     want   : TFRE_FCOM_SSL_WANTS;
     amount : integer;
begin
  if not FSSL_SetupDone then begin
    res  := BIO_new_bio_pair(Finternal_bio,0,Fnetwork_bio,0);
    _SSL := SSL_New(_SSL_CTX);
    if _SSL = nil then begin
      if FRE_SSL_ERR_Errorcheck(fSSL_Error,False) then exit(EFOS_OS_SSL_ERROR);
    end;
    SSL_set_bio(_SSL,Finternal_bio,Finternal_bio);
    FSSL_SetupDone:=true;
  end;
  res := FRE_FCOM_SSL.SSL_accept(_SSL);
  if res<>1 then begin
    Exit(_Handle_SSL_Error(res));
  end;
  result       := EFOS_OS_OK;
  FSSL_State   := fss_SERVER_OK;
end;

function TFCOM_SOCK.SSL_Connect: EFOS_OS_ERROR;
begin
  if FRE_FCOM_SSL.SSL_connect(_SSL)<>1 then begin
    FRE_SSL_ERR_Errorcheck(fSSL_Error,false);
    exit(EFOS_OS_SSL_ERROR);
  end;
  result := EFOS_OS_OK;
end;

function TFCOM_SOCK.SSL_State: TFRE_FCOM_SSL_STATE;
begin
  result := FSSL_State;
end;

function TFCOM_SOCK.SSL_Pending: integer;
var s:String;
    r:integer;
    wants: TFRE_FCOM_SSL_WANTS;
    amount:integer;
begin
  SSL_Wants(wants,amount);
  BIO_flush(Fnetwork_bio);
  SSL_Wants(wants,amount);
  result := FRE_FCOM_SSL.SSL_pending(_SSL);
  result := BIO_ctrl_wpending(Fnetwork_bio);
  result := BIO_ctrl_pending(Fnetwork_bio);
  SSL_Wants(wants,amount);
  SetLength(s,10000);
  r := SSL_read(_SSL,pchar(s),10000);
  SSL_Wants(wants,amount);
end;

function TFCOM_SOCK.SSL_Wants(out wants: TFRE_FCOM_SSL_WANTS;var amount:integer): EFOS_OS_ERROR;
var i:integer;
    res:integer;
begin
  result := EFOS_OS_OK;
  amount := 0;
  if not FSSL_SetupDone then begin
    wants:=fsw_NOTHING;
    exit;
  end;
  res := BIO_ctrl_pending(Fnetwork_bio);
  if res>0 then begin
    wants  := fsw_WRITING;
    amount := res;
    exit(EFOS_OS_OK);
  end;
  i := FRE_FCOM_SSL.SSL_Want(_SSL);
  case i of
    SSL_NOTHING                : wants := fsw_NOTHING;
    SSL_READING                : begin
                                   wants := fsw_READING;
                                   amount := BIO_ctrl_get_read_request(Fnetwork_bio);
                                   writeln('BIO READ REQ : ',amount);
                                   if amount=0 then begin
                                     //amount:=-1;
                                     //abort;
                                   end;
                                 end;
    SSL_WRITING                : wants := fsw_WRITING;
    SSL_X509_LOOKUP            : wants := fsw_x509;
    else raise EFRE_SSL_Exception.Create('unexpected result code '+IntToStr(i)+' for SSL_want');
  end;
end;

function TFCOM_SOCK.SSL_ReadTransfer(const amount: integer): EFOS_OS_ERROR; //TODO Single Buffer - Speedup
var size_read : integer;
begin
  result := Receive   (FSSL_READBUFFER,amount,size_read);
  if result <> EFOS_OS_OK then exit;
  result := SSL_WriteToSSL(FSSL_READBUFFER,amount,size_read);
  if size_read<>amount then begin
    GFRE_BT.CriticalAbort('could not feed ssl with all data wanted=%d, could only=%d',[amount,size_read]);
  end;
end;

function TFCOM_SOCK.SSL_ReadFromSSL(const buf:pointer;const max_len:integer;out act_len:integer):EFOS_OS_ERROR;
var s:string;
    res:integer;
begin
  res := BIO_ctrl_wpending(Fnetwork_bio);
  act_len := BIO_read(Fnetwork_bio,buf,max_len);
  if act_len>0 then begin
    exit(EFOS_OS_OK);
  end else begin
    exit(EFOS_OS_SSL_ERROR);
  end;
end;

function TFCOM_SOCK.SSL_WriteTransfer: EFOS_OS_ERROR;
var size_read,size_done : integer;
begin
  result := SSL_ReadFromSSL(FSSL_WRITEBUFFER,cSSL_WBUF_SIZE,size_read);
  if result <> EFOS_OS_OK then exit;
  Offload_WriteBuf(FSSL_WRITEBUFFER,size_read,false);
  result := EFOS_OS_OK;
end;

function TFCOM_SOCK.SSL_WriteToSSL(const buf: pointer; const max_len: integer; out act_len: integer): EFOS_OS_ERROR;
var res:integer;
begin
  act_len := 0;
  res := BIO_write(Fnetwork_bio,buf,max_len);
  if res>0 then begin
    result := EFOS_OS_OK;
    act_len:=res;
  end else begin
    Result := EFOS_OS_SSL_ERROR;
  end;
  //res := BIO_flush(Fnetwork_bio);
  //res := BIO_flush(Finternal_bio);
end;

function TFCOM_SOCK.SSL_ShutDown: EFOS_OS_ERROR;
var res   : cint;
    wants : TFRE_FCOM_SSL_WANTS;
    amount: integer;
begin
  res := FRE_FCOM_SSL.SSL_shutdown(_SSL);
  case res of
    1  : result := EFOS_OS_OK;
    0  : result := EFOS_OS_SSL_ERROR; // again
    -1 : _Handle_SSL_Error(res);
  end;
  SSL_Wants(wants,amount);
  case wants of
    fsw_BAD:     result := EFOS_OS_SSL_ERROR;
    fsw_NOTHING: result := EFOS_OS_OK;
    fsw_READING: result := EFOS_SSL_WANT_READ;
    fsw_WRITING: result := SSL_WriteTransfer;
    fsw_x509:    result := EFOS_SSL_WANT_X509;
  end;
end;

function TFCOM_SOCK.Get_SSL_ErrorString: string;
begin
  result := fSSL_Error;
  fSSL_Error:='';
end;

procedure TFCOM_SOCK.Offload_Close(const hint: integer);
begin
  if FOS_IL_CAS_NATIVE(FIL_IsClosing,0,1) then begin // Preventing Double Close
    FOS_IL_INC_NATIVE(FIL_ScheduledOffloadwrites);
    FWriteQ.Push(TOffloadWriteObject.CreateFinalizeCode(hint));
    EventSource.Enable_Write_Pending;
  end;
end;

procedure TFCOM_SOCK.Offload_Write(const data: String);
begin
  Offload_WriteBuf(@data[1],length(data),false);
end;

procedure TFCOM_SOCK.Offload_WriteBuf(const data_ptr: Pointer; const size: integer; const ssl_encode: boolean; const AI: IFCOM_AI);
var send_res     : EFOS_OS_ERROR;
    bytes_left   : fcom_int;
    size_written : fcom_int;
    ptr          : PByte;

begin
  ptr          := data_ptr;
  size_written := 0;
  bytes_left   := size;

  if Assigned(ai) then begin
    send_res := _CheckSocketResultValue(fcom_sendto(fHandle,ptr^,bytes_left,0,AI.GetAddress,AI.GetAddressLen),size_written);
  end else begin
    if assigned(fMyAI) then begin
      send_res := _CheckSocketResultValue(fcom_sendto(fHandle,ptr^,bytes_left,0,fMyAI.GetAddress,fMyAI.GetAddressLen),size_written);
    end else begin
      send_res := _CheckSocketResultValue(fcom_send(fHandle,ptr^,bytes_left,0),size_written);
    end;
  end;
  case send_res of
    EFOS_OS_OK: ;
    EFOS_OS_WOULD_BLOCK: begin
      if size_written<>-1 then begin
        // If EFOS_OS_WOULD_BLOCK -> Size Should be -1 !!!
        GFRE_BT.CriticalAbort('OffloadWriteBuf : CHECK THIS SW<>-1 SW=%d , sendres=%d',[size_written,ord(send_res)]);
      end;
      size_written := 0; // Continue sending
    end;
    EFOS_OS_INTERRUPTED: begin
      size_written := 0;
      writeln('SEND/OS INTERRuPTED');
    end;
    EFOS_OS_NOT_CONNECTED: begin
      size_written:=0;
      writeln('OS NOT CONNECTED');
    end;
    else begin
      writeln('UNEXPECTED IN OFFLOADING ',send_res);
      size_written := 0;
      GFRE_BT.CriticalAbort('UNEXPECTED FAILURE IN OFFLOADING');
      halt;
    end;
  end;
  Dec(bytes_left,size_written);
  if bytes_left>0 then begin
    Inc(ptr,size_written);
    FOS_IL_INC_NATIVE(FIL_ScheduledOffloadwrites);
    FWriteQ.Push(TOffloadWriteObject.Create(ptr,bytes_left,ssl_encode));
    _RequestWriteEV;
  end;
end;

function TFCOM_SOCK.AcceptI(out NewSock: IFCOM_SOCK): EFOS_OS_ERROR;
var ns : TFCOM_SOCK;
begin
  result  := Accept(ns);
  NewSock := ns;
end;





function TFCOM_SOCK.Receive(const Data: Pointer; const Size: fcom_int; out SizeRead: fcom_int;const AI:IFCOM_AI=nil): EFOS_OS_ERROR;
var nleft,nread:fcom_int;
    alen:fcom_socklen_t;
    ptr:PByte;
begin
  nleft:=size;
  ptr:=Data;
  SizeRead:=0;
  result:=EFOS_OS_EINVAL;
  while nleft>0 do begin
    if Assigned(ai) then begin
      alen:=0;
      result:=_CheckSocketResultValue(fcom_recvfrom(fHandle,ptr^,Size,0,AI.GetAddress,alen),nread);
      AI.SetAddressLen(alen);
    end else begin
      if assigned(fMyAI) then begin
        alen:=0;
        result:=_CheckSocketResultValue(fcom_recvfrom(fHandle,ptr^,Size,0,fMyAI.GetAddress,alen),nread);
        fMyAI.SetAddressLen(alen);
      end else begin
        result:=_CheckSocketResultValue(fcom_recv(fHandle,ptr^,Size,0),nread);
      end;
    end;
    case Result of
      EFOS_OS_OK: ;
      EFOS_OS_INTERRUPTED: nread:=0; // Again
      else exit;
    end;
    if nread=0 then break;
    dec(nleft,nread);
    inc(ptr,nread);
    inc(SizeRead,nread);
  end;
end;



function TFCOM_SOCK.ReceiveString(var Data: Ansistring; const Size: fcom_int;out SizeRead: fcom_int;const AI:IFCOM_AI=nil): EFOS_OS_ERROR;
var res : integer;
begin
  if Is_SSL_Enabled_Socket then begin
    Result := SSL_ReadTransfer(Size);
    //writeln('SSL RECEIVESTRING STATUSCODE ',result);
    if (result <> EFOS_OS_OK) and not (result=EFOS_OS_WOULD_BLOCK) then exit;
    SetLength(data,Size);
    SizeRead := SSL_read(_SSL,Pchar(data),size);
    if SizeRead>0 then begin
      Setlength(data,SizeRead);
      exit(EFOS_OS_OK);
    end else begin
      res := SSL_get_shutdown(_SSL);
      case res of
         SSL_SENT_SHUTDOWN     : GFRE_BT.CriticalAbort('handle SSL_SENT_SHUTDOWN in Receivestring');
         SSL_RECEIVED_SHUTDOWN : result := EFOS_CONNECTION_CLOSED;
         else  exit(_Handle_SSL_Error(SizeRead));
       end;
    end;
  end else begin
    SetLength(data,Size);
    result:=Receive(PChar(Data),size,SizeRead,ai);
    Setlength(data,SizeRead);
  end;
end;

function TFCOM_SOCK.ReceiveString_TO(var Data: Ansistring;const Timeout: fcom_int;out SizeRead: fcom_int;const AI:IFCOM_AI=nil): EFOS_OS_ERROR;
var x:fcom_int;
    ra:boolean;
begin
 Data:='';
 SizeRead:=0;
 result:=DataCount(x);
 if Result<>EFOS_OS_OK then raise EFCOM_SOCKET_EXCEPTION.Create('internal socket error receivestring_to (DC_A) '+CFOS_OS_ERROR[result]);
 if x>0 then begin
  result:=ReceiveString(Data,x,SizeRead,ai);
  if Result<>EFOS_OS_OK then raise EFCOM_SOCKET_EXCEPTION.Create('internal socket error receivestring_to (RS_A) '+CFOS_OS_ERROR[result]);
 end else begin
  result:=Readable(Timeout,ra);
  if Result<>EFOS_OS_OK then raise EFCOM_SOCKET_EXCEPTION.Create('internal socket error receivestring_to (RA_A) '+CFOS_OS_ERROR[result]);
  if ra=true then begin
   result:=DataCount(x);
   if Result<>EFOS_OS_OK then raise EFCOM_SOCKET_EXCEPTION.Create('internal socket error receivestring_to (DC_B) '+CFOS_OS_ERROR[result]);
   if x<>0 then begin
    result:=ReceiveString(Data,x,SizeRead,ai);
   end else begin
    result:=EFOS_CONNECTION_CLOSED;
   end;
  end else begin
   Data:='';
  end;
 end;
end;

//procedure TFCOM_Handle.SetSessionData(const Key: Ansistring);
//begin
//  fSessionData:=Key;
//end;
//
//function TFCOM_Handle.GetSessionData: String;
//begin
// result:=fSessionData;
//end;
//
//procedure TFCOM_Handle.SetUserState(const State: Integer);
//begin
// FUserState:=State;
//end;
//
//function TFCOM_Handle.GetUserState: Integer;
//begin
// result:=FUserState;
//end;
//
//procedure TFCOM_Handle.SetUserData(const Intf: IInterface);
//begin
// FUserIF:=Intf;
//end;
//
//function TFCOM_Handle.GetUserData: IInterface;
//begin
// result:=fUserIF;
//end;

procedure TFCOM_Handle.SetData(const Data: Pointer);
begin
  if assigned(fData) then  GFRE_BT.CriticalAbort('DOUBLE DATA ASSIGNMENT1');
  fData:=data;
end;

function TFCOM_Handle.GetData: Pointer;
begin
  result:=fData;
end;


procedure TFCOM_Handle.SetES(const Data: IFRE_APS_EVENTSOURCE);
begin
  if assigned(fESData) then GFRE_BT.CriticalAbort('DOUBLE DATA ASSIGNMENT3');
  fESData:=Data;
end;

function TFCOM_Handle.GetES: IFRE_APS_EVENTSOURCE;
begin
 result:=fESData;
end;


function TFCOM_SOCK.Send(const Data:Pointer; const Size: fcom_int;out SizeWritten:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
var nleft,sw:fcom_int;
      ptr:PByte;
begin
   nleft:=Size;
   ptr:=Data;
   SizeWritten:=0;
   result:=EFOS_OS_EINVAL;
   while nleft>0 do begin
     if Assigned(ai) then begin
      result:=_CheckSocketResultValue(fcom_sendto(fHandle,ptr^,nleft,0,AI.GetAddress,AI.GetAddressLen),sw);
     end else begin
      if assigned(fMyAI) then begin
       result:=_CheckSocketResultValue(fcom_sendto(fHandle,ptr^,nleft,0,fMyAI.GetAddress,fMyAI.GetAddressLen),sw);
      end else begin
       result:=_CheckSocketResultValue(fcom_send(fHandle,ptr^,nleft,0),sw);
      end;
     end;
     case Result of
       EFOS_OS_OK: ;
       EFOS_OS_INTERRUPTED: begin
         sw:=0; // Again
         writeln('SEND/OS INTERRuPTED');
       end
       else begin
         exit(result);
         if sw<>-1 then GFRE_BT.CriticalAbort('CHECK THIS SW<>-1 SW=%d',[sw]);
       end;
     end;
     Dec(nleft,sw);
     Inc(ptr,sw);
     Inc(SizeWritten,sw);
   end;
   if Size<>SizeWritten then begin
     writeln('>>>>>>>!!!');
     writeln('>>>>>>>!!!');
     writeln('>>>>>>>!!!');
     writeln(format('>>>>>>>>>>>>>>>>>> basic send failure tfcom_sock.send sw=%d SizeWritten=%d nleft=%d',[sw,SizeWritten,nleft]),' ',Result);
     writeln('>>>>>>>!!!');
     writeln('>>>>>>>!!!');
     writeln('>>>>>>>!!!');
   end;
end;


function TFCOM_SOCK.SendString(const Data:AnsiString;out SizeWritten: fcom_int;const AI:IFCOM_AI=nil): EFOS_OS_ERROR;
begin
  result:=Send(Pchar(Data),Length(Data),SizeWritten,AI);
end;

function TFCOM_SOCK.SetBlocking(const bON: Boolean):EFOS_OS_ERROR;
begin
  result:=_CheckSocketResult(fcom_SetBlocking(fHandle,bon));
end;

function TFCOM_SOCK.SetListenerReuse(const bON: Boolean): EFOS_OS_ERROR;
begin
  result:=_CheckSocketResult(fcom_SetListenerReuse(fHandle,bon));
end;


function TFCOM_SOCK.SetNoDelay(const bON: Boolean): EFOS_OS_ERROR;
begin
 result:=_CheckSocketResult(fcom_SetNoDelay(FHandle,bon));
end;


function TFCOM_SOCK.Shutdown(const How: FCOM_SHUTDOWN_TYPE): EFOS_OS_ERROR;
var show:longint;
begin
 case how of
   fst_RD:   begin show:=0; end;
   fst_WR:   begin show:=1; end;
   fst_RDWR: begin show:=2; end;
   else      show:=2;
 end;
 result:=_CheckSocketResult(fcom_shutdown(fHandle,show));
end;

function TFCOM_SOCK.GetSocketErrorstate(out errorstate:EFOS_OS_ERROR):EFOS_OS_ERROR;
var res:fcom_int;
begin
 result:=_CheckSocketResult(fcom_Get_SOERROR(fHandle,res));
 if res<>0 then begin
  errorstate:=fcom_interpret_OS_Error(res);
 end else begin
  errorstate:=EFOS_OS_OK;
 end;
end;

//procedure TFCOM_SOCK._LockedSetSocketState(const sock_state: EFOS_FCOM_SOCKSTATE);
//var MK:NativeInt;
//begin
//  abort;
//  //MK := Enter_Sockstate;
//  //  fState:=sock_state;
//  //Leave_Sockstate(MK);
//end;


//function TFCOM_SOCK.Enter_Sockstate:TFOS_NATIVE_LOCK_KEY;
//begin
//  FOS_N_LockEnterSpin(_MLocks[ml_Sock_State],result);
//  //writeln('ENTER_SS');
//  //writeln(GFRE_BT.DumpCurrentBacktrace);
//end;
//
//procedure TFCOM_SOCK.Leave_Sockstate(const MLKEY:TFOS_NATIVE_LOCK_KEY);
//begin
//  //writeln('LEAVE_SS');
//  //writeln(GFRE_BT.DumpCurrentBacktrace);
//  FOS_N_LockLeave(_MLocks[ml_Sock_State],MLKEY);
//end;


procedure TFCOM_SOCK._Initialize(const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL);
begin
 fProto     := PROTOCOL;
 fIPlayer   := IP_Layer;
 fTyp       := ft_INVALID;
 fMyAI      := nil;
 FSSL_State := fss_NO_SSL;
 GFRE_TF.Get_LFQ(FWriteQ);
end;

function TFCOM_SOCK._CheckSocketResult(const res: fcom_int): EFOS_OS_ERROR;
begin
 if res<>FCOM_SOCKET_ERROR then begin
   result:=EFOS_OS_OK;
 end else begin
   result:=fcom_interpret_OS_Error(fcom_GetLastSockError);
 end;
end;

procedure TFCOM_SOCK._CheckSocketType(const wish_type: FCOM_SOCKET_TYPE; const err: string);
begin
  if fTyp<>wish_type then raise EFRE_Exception.Create(err);
end;

function TFCOM_SOCK._CheckSocketResultValue(const res: fcom_int;out value: fcom_int): EFOS_OS_ERROR;
begin
 value:=res;
 result:=_CheckSocketResult(res);
end;

const cMAX_SOCK_WRITE_AMOUNT = 1024*64; // 64 KB

function TFCOM_SOCK.Offload_Write_TS(const TID: NativeUint): integer;
var towrite,incr_pos,sw : integer;
    ose                 : EFOS_OS_ERROR;
    wants               : TFRE_FCOM_SSL_WANTS;
    samount             : integer;

    function _SSL_Encode : EFOS_OS_ERROR;
    var res,ret     : integer;
      status        : EFOS_OS_ERROR;
      w_guarantee   : integer;
      ssl_to_write  : integer;
      ssl_req_write : integer;
      ssl_incr_pos  : integer;
      ssl_read_size : integer;
    begin
      if assigned(FCurrentOffloadWorkObject) then GFRE_BT.CriticalAbort('logic fail');
      repeat
        w_guarantee   := BIO_ctrl_get_write_guarantee(Finternal_bio);
        ssl_to_write  := FCurrentSSLEncodeObject.Size-FCurrentSSLEncodeObject.Position;
        ssl_req_write := GFRE_BT.Min(ssl_to_write,w_guarantee-1000);
        res           := SSL_write(_SSL,FCurrentSSLEncodeObject.Memory+FCurrentSSLEncodeObject.Position,ssl_req_write);
        if res>0 then begin
          ssl_incr_pos    := FCurrentSSLEncodeObject.Position+res;
          status          := SSL_ReadFromSSL(FSSL_WRITEBUFFER,cSSL_WBUF_SIZE,ssl_read_size);
          if status <> EFOS_OS_OK then GFRE_BT.CriticalAbort('logic fail 2');
          if not assigned(FCurrentOffloadWorkObject) then begin
             FCurrentOffloadWorkObject := TOffloadWriteObject.create(FSSL_WRITEBUFFER,ssl_read_size,false);
             FCurrentOffloadWorkObject.Position:=ssl_read_size;
          end else begin
             FCurrentOffloadWorkObject.Write (FSSL_WRITEBUFFER^,ssl_read_size); // continue write
          end;
          if ssl_incr_pos = FCurrentSSLEncodeObject.Size then begin
            FCurrentSSLEncodeObject.Free;
            FCurrentSSLEncodeObject            := nil;
            FCurrentOffloadWorkObject.Position := 0;
            break;
          end else
          if ssl_incr_pos<FCurrentSSLEncodeObject.Size then begin
            FCurrentSSLEncodeObject.Position:=ssl_incr_pos;
          end else begin
            GFRE_BT.CriticalAbort('INVALID INCREMENT IN OFFLOADING ?');
          end;
        end else begin
          writeln('SSL WRite PROBLEM ',res);
          ret := SSL_get_error(_SSL,res);
          GFRE_BT.CriticalAbort('-- HANDLE SSL ERROR SSL_WRITE RES=%d /SSL=> %d',[res,ret]);
          case ret of
            SSL_ERROR_SSL       : begin
              FRE_SSL_ERR_Errorcheck(fSSL_Error,False);
              raise EFRE_SSL_Exception.Create(format('could not ssl write len=%d res=%d | ',[ssl_read_size,res]));
              exit(EFOS_OS_SSL_ERROR);
            end;
            SSL_ERROR_WANT_READ  : begin
                                     exit(EFOS_OS_WOULD_BLOCK);
                                   end;
            SSL_ERROR_WANT_WRITE : begin
                                     exit(EFOS_OS_WOULD_BLOCK);
                                   end
            else begin
              GFRE_BT.CriticalAbort('HANDLE SSL ERROR %d',[res]);
            end;
          end;
          raise EFRE_SSL_Exception.Create(format('could not ssl write len=%d res=%d | ',[ssl_read_size,res]));
        end;
      until false;
    end;

    var x       : pointer;
        has_val : NativeUint;
        myTID   : NativeUint;

//        exit(0); // queue empty nothing to write
//        _RequestFreeEvent; exit(1); // Fatal close requested in queue ... (write is closed now)
//        _RequestWriteEV;   exit(2); // Continue Writeing
//        exit(3) // broken pipe
// exit(4); // must wait for finishing move

//

begin
  myTID := TID+1000+(random(36)*100000);
  if not FOS_IL_CEX_NATIVE_CHK(FIL_IsOffloading,has_val,myTID,0) then begin
    writeln('ERROR DOUBLE OFFLOADING EVENT FOR ',GetVerboseDesc,' TID=',TID,' Offloading TID=',has_val,' double eventing ???');
    halt;
  end;
  try
    repeat
      if not assigned(FCurrentOffloadWorkObject) then begin // Event has no current data enqueued to send, look at queue
          x := FWriteQ.Pop;
          if assigned(x) then begin
            FCurrentSSLEncodeObject := TOffloadWriteObject(x);
          end else begin
            FCurrentSSLEncodeObject := nil;
          end;
          if not assigned(FCurrentSSLEncodeObject) then begin
            exit(0); // queue empty nothing to write
          end;
          if FCurrentSSLEncodeObject.FatalClose>0 then begin
            if FCurrentSSLEncodeObject.FatalClose<>1 then begin
              //writeln('OFFLOAD CLOSING SOCKET HINT=',FCurrentSSLEncodeObject.FatalClose);
            end;
            FCurrentSSLEncodeObject.Free;
            FCurrentSSLEncodeObject:=nil;
            Shutdown(fst_WR); // shutdown and close later
            exit(1);
          end;
          if FCurrentSSLEncodeObject.FSSLEncode then begin
            SSL_Wants(wants,samount);
            //writeln('OFLL WRITE SSL_ENCODE ',wants,amount);
            _SSL_Encode;
            //writeln('SSL WANTS XX ',wants,' ',samount);
          end else begin
            FCurrentOffloadWorkObject := FCurrentSSLEncodeObject;
            FCurrentSSLEncodeObject   := nil;
          end;
      end;
      if not assigned(FCurrentOffloadWorkObject) then begin
        Exit(0); // othing to do write finished
      end;
      towrite := FCurrentOffloadWorkObject.Size - FCurrentOffloadWorkObject.Position;
      ose     := Send(FCurrentOffloadWorkObject.Memory+FCurrentOffloadWorkObject.Position,GFRE_BT.Min(towrite,cMAX_SOCK_WRITE_AMOUNT),sw);
      //writeln('REAL SEND ',GetHandleKey,' ',sw);
      case ose of
        EFOS_OS_OK: ;
        EFOS_OS_WOULD_BLOCK: begin
          writeln('OFFLOAD WOULD BLOCK ? ',GFRE_BT.Min(towrite,cMAX_SOCK_WRITE_AMOUNT),' ',cMAX_SOCK_WRITE_AMOUNT,' ',sw);
          if sw=0 then begin
            exit(2); // Continue Writeing
          end else begin
            writeln('OFFLOADING NOT ALL WRITTEN ',sw,' ',towrite);
            //continue with sw ...
          end;
        end;
        EFOS_OS_BROKEN_PIPE: begin
          writeln('OFFLOAD STOP -> BROKEN Pipe ..');
          exit(3);
        end
        else begin
          writeln('OFFLOAD SEND ERROR');
          writeln('OFFLOAD SEND ERROR ',CFOS_OS_ERROR[ose]);
          writeln(format('TO WRITE :%d POS: %d SIZE:%d Amount:%d SW:%d',[towrite,FCurrentOffloadWorkObject.Position,FCurrentOffloadWorkObject.Size,cMAX_SOCK_WRITE_AMOUNT,sw]));
          writeln('OFFLOST SOCK IS ',GetHandleKey);
          exit;
        end;
      end;
      incr_pos:=FCurrentOffloadWorkObject.Position+sw;
      if incr_pos=FCurrentOffloadWorkObject.Size then begin
        if FCurrentOffloadWorkObject.fatalclose>0 then begin
         writeln('FINISH FATAL OFFLOADSTREAM ',NAtiveUInt(FCurrentOffloadWorkObject ));
          //GFRE_BT.CriticalAbort('SOCK CLOSE');
          SockClose; // A fatal error occured
        end;
        FCurrentOffloadWorkObject.Free;
        FCurrentOffloadWorkObject :=nil;
      end else
      if incr_pos<FCurrentOffloadWorkObject.Size then begin
        //writeln('WE EXIT ',incr_pos,'  ',FCurrentOffloadWorkObject .Size);
        FCurrentOffloadWorkObject.Position:=incr_pos;
        exit(4); // must wait for finishing move
      end else begin
        GFRE_BT.CriticalAbort('INVALID INCREMENT IN OFFLOADING ?  [%d,%d]',[incr_pos,FCurrentOffloadWorkObject.Size]);
      end;
    until false;
  finally
     if not FOS_IL_CEX_NATIVE_CHK(FIL_IsOffloading,has_val,0,myTID) then begin
       GFRE_BT.CriticalAbort('START / OFFLOADING UNEXPECTED VALUE = %s SHOULD BE=%s',[inttostr(has_val),inttostr(myTID)]);
     end;
  end;
end;

//var towrite,incr_pos,sw:integer;
//    ose:EFOS_OS_ERROR;
//begin
//  repeat
//    if not assigned(offloadworkstream) then begin
//      offloadworkstream:=TErrMemStream(offloadlist.Pop); // Try pop a new one
//    //  writeln('POPPED NEW ',integer(offloadworkstream));
////      if assigneD(offloadworkstream) then InterLockedDecrement(offloading);
//    end else begin
//     // writeln('USING EXISTING ',integer(offloadworkstream));
//    end;
//    if not assigned(offloadworkstream) then begin
//      //writeln('OFFLOADING DONE');
//      Exit(false);
//    end;
//    towrite:=offloadworkstream.Size-offloadworkstream.Position;
//    //writeln('GOT OFFLOAD STREAM POS=',offloadworkstream.Position,' SZ=',offloadworkstream.Size,' DC=',Datacount,' TOWRITE=',towrite);
//    ose:=SOCK.Send(offloadworkstream.Memory+offloadworkstream.Position,GFRE_BT.Min(towrite,Datacount),sw);
//    //writeln('SW = ',sw);
//    case ose of
//      EFOS_OS_OK: ;
//      EFOS_OS_WOULD_BLOCK: begin
//        writeln('OFFLOAD WOULD BLOCK ? ');
//        exit(true);
//      end;
//      else GFRE_BT.CriticalAbort('OFFLOAD SEND ERROR <%s>',[CFOS_OS_ERROR[ose]]);
//    end;
//    incr_pos:=offloadworkstream.Position+sw;
//    if incr_pos=offloadworkstream.Size then begin
//      if offloadworkstream.fatalclose then begin
//       writeln('FINISH FATAL OFFLOADSTREAM ',integer(offloadworkstream));
//        //GFRE_BT.CriticalAbort('SOCK CLOSE');
//        sock.Close; // AN fatal error occured
//      end;
//      offloadworkstream.Free;
//      offloadworkstream:=nil;
//    end else
//    if incr_pos<offloadworkstream.Size then begin
//     // writeln('WE EXIT ',incr_pos,'  ',offloadworkstream.Size);
//      offloadworkstream.Position:=incr_pos;
//      exit(true); // must wait for finishing move
//    end else begin
//      GFRE_BT.CriticalAbort('INVALID INCREMENT IN OFFLOADING ?');
//    end;
//  until false;
//end;




{ TFCOM_SOCKV4 }

function TFCOM_SOCK.Accept(out NewSock: TFCOM_SOCK): EFOS_OS_ERROR;
var AI      : TFCOM_AI;
    nsock   : fcom_int;
begin
  NewSock:=nil;
  AI:=TFCOM_AI.Create;
  AI.Address_Len:=SizeOf(TFCOM_SOCKADDRSTORAGE);
  result:=_CheckSocketResultValue(fcom_accept(fHandle,@AI.Address,AI.Address_Len),nsock);
  if result=EFOS_OS_OK then begin
    NewSock := TFCOM_SOCK.AcceptSocketCreate(nsock,fIPLayer,fProto,AI); // Same Protocol and Layer as Accepting socket
  end else begin
    AI.Free;
  end;
end;

function TFCOM_SOCK.Bind(const AI: IFCOM_AI): EFOS_OS_ERROR;
begin
 fMyAI:=AI;
 result:=_CheckSocketResult(fcom_bind(fHandle,AI.GetAddress,AI.GetAddressLen));
end;

function TFCOM_SOCK.SockClose: EFOS_OS_ERROR;
begin
 //MLK := Enter_Sockstate;
 // // writeln('--- CLOSE ---- ',GetHandleKey,' ',fState);
 //  //writeln(GFRE_BT.DumpCurrentBacktrace);
 //  //if fState=ess_CLOSED then begin // IGNORE DOUBLE CLOSE REQUEST
 //  //  writeln('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DOUBLE CLOSE !!!! ',GetHandleKey);
 //  //  exit;
 //  //end;
 //  if fOffloadWriting then begin
 //    if fWantCloseAfterW then begin
 //      writeln('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DOUBLE CLOSE !!!! ',GetHandleKey);
 //    end;
 //    fWantCloseAfterW := true;
 //    do_close         := false;
 //  end else begin
 //    do_close     := true;
 //    fWriteClosed := true;
 //    fState       := ess_CLOSED;
 //  end;
 //Leave_Sockstate(MLK);
 //if do_close then begin
 //  Shutdown(fst_RDWR);
 //  result:=_CheckSocketResult(fcom_close(fHandle));
 //end;
  Shutdown(fst_RDWR);
end;

function TFCOM_SOCK.CloseEnqueue(const hint: integer): EFOS_OS_ERROR;
begin
  if FSockWriteState <> esws_CONNECTED then exit(EFOS_OS_NOT_CONNECTED);//  raise EFRE_Exception.Create('socket is not connected');
  if FSockReadState  <> esrs_CONNECTED then exit(EFOS_OS_NOT_CONNECTED);//raise EFRE_Exception.Create('socket is not connected');
  Offload_Close(hint);
  result := EFOS_OS_OK;
end;

function TFCOM_SOCK.Connect(const AI: IFCOM_AI): EFOS_OS_ERROR;
begin
  _CheckSocketType(ft_INVALID,'cannot connect a socket of type '+CFCOM_SOCKET_TYPE[ft_INVALID]);
  fTyp := ft_CLIENT;
  result:=_CheckSocketResult(fcom_connect(fHandle,AI.GetAddress,AI.GetAddressLen));
  FSockWriteState := esws_CONNECTING;
  fMyAI:=AI;
end;

constructor TFCOM_SOCK.Create(const IP_Layer:FCOM_IP_LAYER;const PROTOCOL:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR);

  function NewHandle : fcom_int;
  var af,struc,proto:fcom_int;
  begin
     af:=0;struc:=0;proto:=0;
     case IP_Layer of
       fil_IPV4: af:=FCOM_AF_INET;
       fil_IPV6: af:=FCOM_AF_INET6;
     end;
     case PROTOCOL of
       fsp_UDP:  begin
                   struc:=FCOM_SOCK_DGRAM;
                   proto:=FCOM_IPPROTO_UDP;
                 end;
       fsp_TCP:  begin
                   struc:=FCOM_SOCK_STREAM;
                   proto:=FCOM_IPPROTO_TCP;
                 end;
       {$IFDEF FCOM_SCTP}
       fsp_SCTP: begin
                   struc:=FCOM_SOCK_STREAM;
                   proto:=FCOM_IPPROTO_SCTP;
                 end;
       fsp_SCTP_SEQ: begin
                   struc:=FCOM_SOCK_SEQPACKET;
                   proto:=IPPROTO_SCTP;
                 end;
       {$ENDIF}
     end;
     result:=fcom_socket(af,struc,proto);
    end;

begin
 inherited Create;
 _Initialize(IP_Layer,PROTOCOL);
 fHandle:=NewHandle;
 Error:=_CheckSocketResult(fHandle);
end;

destructor TFCOM_SOCK.Destroy;
begin
  EventSource.QuiesceEventSource;
  _SetReadClosed;
  _SetWriteClosed;
  if assigned(fMyAI) then fMyAI.Finalize;
  if assigned(_SSL) then begin
    SSL_free(_SSL);
  end;
  if assigned(FSSL_WRITEBUFFER) then begin
    Freemem(FSSL_WRITEBUFFER);
  end;
  if assigned(FSSL_READBUFFER) then begin
    Freemem(FSSL_READBUFFER);
  end;
  fcom_close(fHandle);
  FWriteQ.Finalize;
  inherited;
end;

function TFCOM_Handle.GetHandleKey :FCOM_HandleKey;
begin
 result:=fHandle;
end;

function TFCOM_SOCK.GetSocketType: FCOM_SOCKET_TYPE;
begin
 result:=fTyp;
end;


function TFCOM_SOCK.Datacount(out Count:fcom_int):EFOS_OS_ERROR;
begin
  count:=0;
  result:=_CheckSocketResult(fcom_Datacount(fHandle,count));
end;

function TFCOM_SOCK.Is_SSL_Enabled_Socket: boolean;
begin
  result := (FSSL_State=fss_SERVER_OK) or (FSSL_State=fss_CLIENT_OK);
end;

procedure TFCOM_SOCK._RequestWriteEV;
begin
  if FIL_IsClosing=1 then begin
    exit;
  end;
  EventSource.Enable_Write_Pending;
end;

procedure TFCOM_SOCK._RequestReadEV;
begin
  if FIL_IsClosing=1 then begin
    exit;
  end;
  EventSource.Enable_Read_Pending;
end;



function TFCOM_SOCK._Handle_SSL_Error(res: integer): EFOS_OS_ERROR;
var bres:integer;
begin
  res := SSL_get_error(_SSL,res);
  case res of
    SSL_ERROR_SSL             : begin
                                  FRE_SSL_ERR_Errorcheck(fSSL_Error,False);
                                  exit(EFOS_OS_SSL_ERROR);
                                end;
    SSL_ERROR_WANT_READ        :begin
                                  bres := BIO_ctrl_pending(Fnetwork_bio);
                                  if bres>0 then begin
                                    exit(EFOS_SSL_WANT_WRITE);
                                  end;
                                  exit(EFOS_SSL_WANT_READ);
                                end;
    SSL_ERROR_WANT_WRITE       : exit(EFOS_SSL_WANT_WRITE);
    SSL_ERROR_WANT_X509_LOOKUP : exit(EFOS_SSL_WANT_X509);
    else begin
      exit(EFOS_INTERNAL_ERROR);
    end;
 end;
end;

constructor TFCOM_SOCK.AcceptSocketCreate(const Socket: fcom_int; IPLayer: FCOM_IP_LAYER; Protocol: FCOM_SOCKET_PROTCOL; const AI: TFCOM_AI);
begin
 inherited Create;
 _Initialize(IPLayer,Protocol);
 fHandle := Socket;
 fMyAI   := AI;
 fTyp    := ft_SERVEDCONNECTION;
 _SetReadConnected;
 _SetWriteConnected;
end;


{ TFCOM_AI }

constructor TFCOM_AI.Create;
begin
 FillChar(Address,Sizeof(TFCOM_SOCKADDRSTORAGE),#0);
end;

function TFCOM_AI.GetAddress: PFCOM_SOCKADDRSTORAGE;
begin
 result:=@Address;
end;

function TFCOM_AI.GetAddressLen: fcom_socklen_t;
begin
 case Address.sin_family of
  FCOM_AF_INET :result:=SizeOf(TFCOM_SockAddrIn);
  FCOM_AF_INET6:result:=SizeOf(TFCOM_SockAddrIn6);
  else          result:=0;
 end;
end;

function TFCOM_AI.IPAsString: String;
begin
 case Address.sin_family of
  FCOM_AF_UNSPEC:result:='UNSPECIFIED';
  FCOM_AF_INET  :begin
                  result:=FCOM_Ip4ToStr(Address.sin_addr);
                 end;
  FCOM_AF_INET6 :begin
                  result:=FCOM_Ip6ToStr(Address.sin6_addr);
                 end;
  else           result:='INVALID';
 end;
end;

function TFCOM_AI.PortAsString: String;
begin
 case Address.sin_family of
  FCOM_AF_UNSPEC:result:='UNSPECIFIED';
  FCOM_AF_INET  :begin
                  result:=InttoStr(fcom_ntoh2(Address.sin_port));
                 end;
  FCOM_AF_INET6 :result:='IP6 NOT IMPLEMENTED'; //TBI
  else           result:='INVALID';
 end;
end;

function TFCOM_AI.SocketAsString: String;
begin
 case Address.sin_family of
  FCOM_AF_UNSPEC:result:='UNSPECIFIED';
  FCOM_AF_INET  :begin
                  result:=FCOM_Ip4ToStr(Address.sin_addr);
                 end;
  FCOM_AF_INET6 :begin
                  result:=FCOM_Ip6ToStr(Address.sin6_addr);
                 end;
  else           result:='INVALID';
 end;
 case Address.sin_family of
  FCOM_AF_UNSPEC:result:=result+':UNSPECIFIED';
  FCOM_AF_INET  :begin
                  result:=result+':'+InttoStr(fcom_ntoh2(Address.sin_port));
                 end;
  FCOM_AF_INET6 :result:=result+':IP6 NOT IMPLEMENTED'; //TBI
  else           result:=result+':'+'INVALID';
 end;
end;


function TFCOM_AI.GetAddressType: FCOM_ADDRESS_TYPE;
begin
 case Address.sin_family of
  FCOM_AF_UNSPEC:result:=fat_UNSPEC;
  FCOM_AF_INET  :result:=fat_IPV4;
  FCOM_AF_INET6 :result:=fat_IPV6;
  else           result:=fat_INVALID;
 end;
end;

function TFCOM_AI.ResolveandSet(addr: string;port:fcom_u_short): FCOM_ADDRESS_TYPE;
begin
 if (addr='*') or (addr='') then addr:='0.0.0.0';
 if FCOM_Test_IP4(addr,port,@Address,Address_Len) then begin
   result:=fat_IPV4;
 end else
 if FCOM_Test_IP6(addr,port,@Address,Address_Len) then begin
   result:=fat_IPV6;
 end else begin
   result:=fat_INVALID; // Todo -> implement getaddrinfo / self DNS Query
   GFRE_BT.CriticalAbort('NOT IMPLEMENTED');
 end;
end;



procedure TFCOM_AI.SetAddressLen(const len: fcom_socklen_t);
begin
 Address_Len:=len;
end;

function FCOM_AI_from_SOCKADDR(const sa:PFCOM_SOCKADDRSTORAGE):IFCOM_AI;
var AI:TFCOM_AI;
begin
 AI:=TFCOM_AI.Create;
 if sa<>nil then begin
  AI.Address:=sa^;
 end else begin
  FillChar(AI.Address,sizeof(TFCOM_SOCKADDRSTORAGE),#0);
 end;
 AI.Address_Len:=0;
 result:=AI;
end;


end.
