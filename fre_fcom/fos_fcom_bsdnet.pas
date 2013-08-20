unit fos_fcom_bsdnet;

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

uses BaseUnix,FOS_FCOM_TYPES,FOS_FCOM_SOCKET,sockets,FOS_TOOL_FACTORY;

{ in.h -> FreeBSD 8.0 Beta 3

/*
 * Options for use with [gs]etsockopt at the IP level.
 * First word of comment is data type; bool is stored in int.
 */
#define	IP_OPTIONS		1    /* buf/ip_opts; set/get IP options */
#define	IP_HDRINCL		2    /* int; header is included with data */
#define	IP_TOS			3    /* int; IP type of service and preced. */
#define	IP_TTL			4    /* int; IP time to live */
#define	IP_RECVOPTS		5    /* bool; receive all IP opts w/dgram */
#define	IP_RECVRETOPTS		6    /* bool; receive IP opts for response */
#define	IP_RECVDSTADDR		7    /* bool; receive IP dst addr w/dgram */
#define	IP_SENDSRCADDR		IP_RECVDSTADDR /* cmsg_type to set src addr */
#define	IP_RETOPTS		8    /* ip_opts; set/get IP options */
#define	IP_MULTICAST_IF		9    /* struct in_addr *or* struct ip_mreqn;
				      * set/get IP multicast i/f  */
#define	IP_MULTICAST_TTL	10   /* u_char; set/get IP multicast ttl */
#define	IP_MULTICAST_LOOP	11   /* u_char; set/get IP multicast loopback */
#define	IP_ADD_MEMBERSHIP	12   /* ip_mreq; add an IP group membership */
#define	IP_DROP_MEMBERSHIP	13   /* ip_mreq; drop an IP group membership */
#define	IP_MULTICAST_VIF	14   /* set/get IP mcast virt. iface */
#define	IP_RSVP_ON		15   /* enable RSVP in kernel */
#define	IP_RSVP_OFF		16   /* disable RSVP in kernel */
#define	IP_RSVP_VIF_ON		17   /* set RSVP per-vif socket */
#define	IP_RSVP_VIF_OFF		18   /* unset RSVP per-vif socket */
#define	IP_PORTRANGE		19   /* int; range to choose for unspec port */
#define	IP_RECVIF		20   /* bool; receive reception if w/dgram */
/* for IPSEC */
#define	IP_IPSEC_POLICY		21   /* int; set/get security policy */
#define	IP_FAITH		22   /* bool; accept FAITH'ed connections */

#define	IP_ONESBCAST		23   /* bool: send all-ones broadcast */
#define	IP_BINDANY		24   /* bool: allow bind to any address */

#define	IP_FW_TABLE_ADD		40   /* add entry */
#define	IP_FW_TABLE_DEL		41   /* delete entry */
#define	IP_FW_TABLE_FLUSH	42   /* flush table */
#define	IP_FW_TABLE_GETSIZE	43   /* get table size */
#define	IP_FW_TABLE_LIST	44   /* list table contents */

#define	IP_FW_ADD		50   /* add a firewall rule to chain */
#define	IP_FW_DEL		51   /* delete a firewall rule from chain */
#define	IP_FW_FLUSH		52   /* flush firewall rule chain */
#define	IP_FW_ZERO		53   /* clear single/all firewall counter(s) */
#define	IP_FW_GET		54   /* get entire firewall rule chain */
#define	IP_FW_RESETLOG		55   /* reset logging counters */

#define IP_FW_NAT_CFG           56   /* add/config a nat rule */
#define IP_FW_NAT_DEL           57   /* delete a nat rule */
#define IP_FW_NAT_GET_CONFIG    58   /* get configuration of a nat rule */
#define IP_FW_NAT_GET_LOG       59   /* get log of a nat rule */

#define	IP_DUMMYNET_CONFIGURE	60   /* add/configure a dummynet pipe */
#define	IP_DUMMYNET_DEL		61   /* delete a dummynet pipe from chain */
#define	IP_DUMMYNET_FLUSH	62   /* flush dummynet */
#define	IP_DUMMYNET_GET		64   /* get entire dummynet pipes */

#define	IP_RECVTTL		65   /* bool; receive IP TTL w/dgram */
#define	IP_MINTTL		66   /* minimum TTL for packet or drop */
#define	IP_DONTFRAG		67   /* don't fragment packet */
}

type

   { TFCOM_IPFWCTRL }

   TFCOM_IPFWCTRL=class(TInterfacedObject)
    private
     csock: fcom_int;
     FError:EFOS_OS_ERROR;
    public
     constructor Create;
     function    Error:EFOS_OS_ERROR;
     destructor  Destroy;override;
     function    ZeroCounters(const rulenum:word=0):EFOS_OS_ERROR;
     procedure   Dump;
   end;


implementation

const	FCOMBSD_IP_FW_TABLE_ADD	=	40;   // add entry
const	FCOMBSD_IP_FW_TABLE_DEL	=	41;   // delete entry
const   FCOMBSD_IP_FW_TABLE_FLUSH=	42;   // flush table
const   FCOMBSD_IP_FW_TABLE_GETSIZE=	43;   // get table size
const	FCOMBSD_IP_FW_TABLE_LIST=	44;   // list table contents
const	FCOMBSD_IP_FW_ADD=		50;   // add a firewall rule to chain
const	FCOMBSD_IP_FW_DEL=		51;   // delete a firewall rule from chain
const	FCOMBSD_IP_FW_FLUSH=		52;   // flush firewall rule chain
const	FCOMBSD_IP_FW_ZERO=		53;   // clear single/all firewall counter(s)
const	FCOMBSD_IP_FW_GET=		54;   // get entire firewall rule chain
const	FCOMBSD_IP_FW_RESETLOG=		55;   // reset logging counters
const   FCOMBSD_IP_FW_NAT_CFG=          56;   // add/config a nat rule
const   FCOMBSD_IP_FW_NAT_DEL=          57;   // delete a nat rule
const   FCOMBSD_IP_FW_NAT_GET_CONFIG=   58;   // get configuration of a nat rule
const   FCOMBSD_IP_FW_NAT_GET_LOG=      59;   // get log of a nat rule
const	FCOMBSD_IP_DUMMYNET_CONFIGURE=	60;   // add/configure a dummynet pipe
const	FCOMBSD_IP_DUMMYNET_DEL=	61;   // delete a dummynet pipe from chain
const	FCOMBSD_IP_DUMMYNET_FLUSH=	62;   // flush dummynet
const	FCOMBSD_IP_DUMMYNET_GET=	64;   // get entire dummynet pipes
const	FCOMBSD_IP_RECVTTL=		65;   // bool; receive IP TTL w/dgram
const	FCOMBSD_IP_MINTTL=		66;   // minimum TTL for packet or drop
const	FCOMBSD_IP_DONTFRAG=		67;   // don't fragment packet


function _CheckSocketResult(const res: fcom_int): EFOS_OS_ERROR;
var x:integer;
begin
 if res<>FCOM_SOCKET_ERROR then begin
   result:=EFOS_OS_OK;
 end else begin
   x:=fcom_GetLastSockError;
   writeln('ERR = ',x,' ',res);
   result:=fcom_interpret_OS_Error(x);
   writeln('FOS_ERR = ',CFOS_OS_ERROR[result]);
 end;
end;

constructor TFCOM_IPFWCTRL.Create;
begin
 inherited;
 csock:=fcom_socket(FCOM_AF_INET, FCOM_SOCK_RAW, FCOM_IPPROTO_RAW);
 FError:=_CheckSocketResult(csock);
end;

function TFCOM_IPFWCTRL.Error: EFOS_OS_ERROR;
begin
  result:=FError;
end;

destructor TFCOM_IPFWCTRL.Destroy;
begin
  inherited Destroy;
end;

{ TFCOM_IPFWCTRL }
function TFCOM_IPFWCTRL.ZeroCounters(const rulenum:word=0):EFOS_OS_ERROR;
var res:fcom_int;
    opt:longint;
begin
 if rulenum=0 then begin
  res:=fpsetsockopt(csock, FCOM_IPPROTO_IP, FCOMBSD_IP_FW_ZERO, nil, 0);
 end else begin
  opt:=rulenum;
  res:=fpsetsockopt(csock, FCOM_IPPROTO_IP, FCOMBSD_IP_FW_ZERO, @opt, sizeof(opt));
 end;
 if res<>FCOM_SOCKET_ERROR then begin
  result:=EFOS_OS_OK;
 end else begin
  result:=fcom_interpret_OS_Error(fcom_GetLastSockError);
 end;
end;

procedure TFCOM_IPFWCTRL.Dump;
var s:String;
    cmd:integer;
    len:integer;
    poptlen:pSocklen;
    res:fcom_int;
    result:EFOS_OS_ERROR;

begin
 cmd:=FCOMBSD_IP_FW_GET; // IP_DUMMYNET_GET
 setlength(s,100000);
 len:=100000;
 res:=0;
 res:=fpgetsockopt(csock,FCOM_IPPROTO_IP,cmd,@s[1],@len);
 writeln('Dump ',res,' len ',len);
 setlength(s,len);
 writeln(GBS.Str2HexStr(s));
 if res<>FCOM_SOCKET_ERROR then begin
  result:=EFOS_OS_OK;
 end else begin
  result:=fcom_interpret_OS_Error(fcom_GetLastSockError);
 end;
 writeln('DUMP RESULT ',CFOS_OS_ERROR[result]);
end;

initialization

finalization

end.

