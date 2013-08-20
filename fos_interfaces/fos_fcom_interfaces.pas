unit fos_fcom_interfaces;

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
//{$IFDEF FPC}
  {$MODE objfpc}
  {$modeswitch nestedprocvars}
//{$ENDIF}
{$H+}

interface

uses classes,fos_fcom_types,fos_tool_interfaces;

type
  TFRE_FCOM_SSL_Type = (fssl_SSLv2,fssl_SSLv3,fssl_TLSv1,fssl_SSLv23,fssl_DTLS1);

  {$interfaces corba}
  IFCOM_BASE    = interface
    function   GetImplementor:TObject;
    procedure  Finalize(const finalize_key:integer=0);
  end;


  IFCOM_AI=interface(IFCOM_BASE) // Abstract Addressinfo
    ['FRE:FCOM:AI']
  //   function  DNSQuery       (Name: AnsiString; QType: TDNS_QTYPES; var Reply: TFOS_NPS;TargetHost:AnsiString;TargetPort:String='domain'): Boolean;
    function  ResolveandSet  (addr:string;port:fcom_u_short):FCOM_ADDRESS_TYPE; // Tries to set Numerical IP4 / IP6 or symbolic addresses
    function  GetAddressType :FCOM_ADDRESS_TYPE;
    function  GetAddress     :PFCOM_SOCKADDRSTORAGE; // Get in sockaddr_storage format
    function  GetAddressLen  :fcom_socklen_t; // Length of sockaddr_storage address
    procedure SetAddressLen  (const len:fcom_socklen_t); // Length of sockaddr_storage address
    function  IPAsString     :String;
    function  PortAsString   :String;
    function  SocketAsString :String;
  end;

  IFCOM_HANDLE=interface(IFCOM_BASE)
    ['FRE:FCOM:HANDLE']
    function    GetHandleKey     :FCOM_HandleKey;
    procedure   SetData          (const Data:Pointer);
    function    GetData          :Pointer;
    property    Data             :pointer read GetData write SetData;
  end;

  IFCOM_FILE=interface(IFCOM_HANDLE)
    ['FRE:FCOM:FILE']
  end;

  TFRE_SSL_Callback = function:string is nested;

  TFRE_SSL_INFO=record
    ssl_type              : TFRE_FCOM_SSL_Type;
    cerifificate_file     : string;
    private_key_file      : string;
    root_ca_file          : string;
    verify_peer           : boolean;
    fail_no_peer_cert     : boolean;
    verify_peer_cert_once : boolean;
    cipher_suites         : string;
    IsServer              : boolean;
    password_cb           : TFRE_SSL_Callback;
  end;
  PFRE_SSL_INFO=^TFRE_SSL_INFO;

  TFRE_FCOM_SSL_STATE=(fss_NO_SSL,fss_SERVER_NOT_SETUP,fss_SERVER_OK,fss_CLIENT_NOT_SETUP,fss_CLIENT_OK);
  TFRE_FCOM_SSL_WANTS=(fsw_BAD,fsw_NOTHING,fsw_READING,fsw_WRITING,fsw_x509);

  { IFCOM_SOCK }

  IFCOM_SOCK=interface(IFCOM_HANDLE)
    ['FRE:FCOM:SOCKET']
    function    GetSocketType       :FCOM_SOCKET_TYPE;
    //function    Connect             (const AI:IFCOM_AI):EFOS_OS_ERROR;
    function    Bind                (const AI:IFCOM_AI):EFOS_OS_ERROR;
    function    Listen              (const Backlog:longint):EFOS_OS_ERROR;
    function    Accept              (out   NewSock:IFCOM_SOCK):EFOS_OS_ERROR;
    function    Shutdown            (const How:FCOM_SHUTDOWN_TYPE):EFOS_OS_ERROR;
    function    SockClose           :EFOS_OS_ERROR;
    function    CloseEnqueue        (const hint:integer):EFOS_OS_ERROR; // "Offload Write a Shutdown"
    procedure   RequestClose        ;
    function    Get_AI              :IFCOM_AI;
    function    GetSocketErrorState (out   errorstate:EFOS_OS_ERROR):EFOS_OS_ERROR;
    function    GetVerboseDesc      : String;

    function    Send                (const SendData:Pointer; const Size: Integer;out SizeWritten:Integer;const AI:IFCOM_AI=nil):EFOS_OS_ERROR; // AI=nil, use bound AI, (connect,bind)
    function    SendString          (const SendData:AnsiString; out SizeWritten:Integer;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
    function    Receive             (const SendData:Pointer; const Size: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
    function    ReceiveString       (var   SendData:Ansistring; const Size: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
    function    ReceiveString_TO    (var   SendData:Ansistring; const Timeout: fcom_int;out SizeRead:fcom_int;const AI:IFCOM_AI=nil):EFOS_OS_ERROR;
    function    Datacount           (out   Count:fcom_int):EFOS_OS_ERROR;
    function    Readable            (const Timeout:fcom_int;out rdable:boolean):EFOS_OS_ERROR;

    procedure   Offload_Write       (const SendData:String);
    procedure   Offload_WriteBuf    (const data_ptr: Pointer; const size: integer; const ssl_encode: boolean ; const AI:IFCOM_AI=nil); // Send direct or prepare write completion via APS

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
    function    Get_SSL_ErrorString : string;
    function    SSL_ShutDown        : EFOS_OS_ERROR;
    function    SetBlocking         (const bON:Boolean):EFOS_OS_ERROR;
    function    SetNoDelay          (const bON:Boolean):EFOS_OS_ERROR;
    function    SetListenerReuse    (const bON:Boolean):EFOS_OS_ERROR;
  end;

  //  TFRE_FCOM_Sock_WriteEvent  = function  (const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean of object;
  TFRE_FCOM_APS_Event        = function (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean of object;
  TFRE_FCOM_TearDownEvent    = procedure (const Sock:IFCOM_SOCK) of object;
  TFRE_FCOM_InitEvent        = procedure (const SOCK:IFCOM_SOCK) of object;
  TFRE_FCOM_SocketError      = procedure (const sock:IFCOM_SOCK;const Error : EFOS_OS_ERROR) of object;


  IFCOM_Factory = interface
   ['FRE:FCOM:FACTORY']
   function New_FCOM_AI      :IFCOM_AI;
   function New_FCOM_NETSOCK (const IPL:FCOM_IP_LAYER;const Proto:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR):IFCOM_SOCK;
  end;

  var GFRE_FF:IFCOM_Factory;


implementation

end.
