unit fos_fcom_pcap;

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

interface

uses
  {$ifdef UNIX}
  BaseUnix,
  {$endif}
  {$ifdef Windows}
   Windows,
  {$endif}
  FOS_FCOM_TYPES;

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$ifndef NO_SMART_LINK}
    {$smartlink on}
  {$endif}
{$ENDIF}

  const
     {$ifdef unix}
//     {$linklib pcapfos}
//     PCAP_LIB_NAME = 'pcapfos';
     PCAP_LIB_NAME = 'pcap';
     {$endif}
     {$ifdef WINDOWS}
     PCAP_LIB_NAME = 'wpcap';
     {$endif}
//     PCAP_VERSION_MAJOR = 2;
//     PCAP_VERSION_MINOR = 4;
     PCAP_ERRBUF_SIZE = 256;
     PCAP_IF_LOOPBACK = $00000001;

  type
     PPLongint = ^PLongint;
     PPcap_File_Header = ^TPcap_File_Header;
     TPcap_File_Header = record
       magic : DWord;
       version_major : Word;
       version_minor : Word;
       thiszone : Longint;
       sigfigs : DWord;
       snaplen : DWord;
       linktype : DWord;
     end;

     PBPF_Insn = ^TBPF_Insn;
     TBPF_Insn = record
       code: Word;
       jt: Byte;
       jf: Byte;
       k: DWord;
     end;

     PBPF_Program = ^TBPF_Program;
     TBPF_Program = record
       bf_len: PtrInt;
       bf_insns: PBPF_Insn;
     end;

     PDirection = ^TDirection;
     TDirection = (D_INOUT, D_IN, D_OUT);

     PPPcap_Pkthdr = ^PPcap_Pkthdr;

     PPcap_Pkthdr = ^TPcap_Pkthdr;
     TPcap_Pkthdr = record
       ts     : TFCOM_TimeVal;
       caplen : DWord;
       len    : DWord;
     end;

     PPcap_Stat = ^TPcap_Stat;
     TPcap_Stat = record
       ps_recv   : DWord;
       ps_drop   : DWord;
       ps_ifdrop : DWord;
       bs_capt   : DWord;
     end;

     PPcap_Addr = ^TPcap_Addr;
     TPcap_Addr = record
       next      : PPcap_Addr;
       addr      : PFCOM_SOCKADDRSTORAGE;
       netmask   : PFCOM_SOCKADDRSTORAGE;
       broadaddr : PFCOM_SOCKADDRSTORAGE;
       dstaddr   : PFCOM_SOCKADDRSTORAGE;
     end;

     PPPcap_If = ^PPcap_If;
     PPcap_If = ^TPcap_If;
     TPcap_If = record
       next : PPcap_If;
       name : PChar;
       description : PChar;
       addresses : PPcap_Addr;
       flags : DWord;
     end;
     
  { obfuscated C types }
     PPcap = ^TPcap;
     TPcap = record end;

     PPcapDumper = ^TPcapDumper;
     TPcapDumper = record end;

     TPcapHandler = procedure (para1: PChar; Header: PPcap_Pkthdr; Data: PChar); cdecl;

  function pcap_lookupdev       (ErrBuf: PChar): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_lookupnet       (Device: PChar; NetP: PDword; MaskP: PDword; ErrBuf: PChar): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_open_live       (Device: PChar; SnapLen: Longint; Promisc: Longint;to_ms: Longint; ebuf: PChar): PPcap; cdecl; external PCAP_LIB_NAME;
  function pcap_create          (Device: PChar; ebuf: PChar):PPCap; cdecl external PCAP_LIB_NAME;
  function pcap_set_snaplen     (p :PPcap ; SnapLen: Longint):longint; cdecl external PCAP_LIB_NAME;
  function pcap_set_promisc     (p :PPcap ; Promisc: Longint):longint; cdecl external PCAP_LIB_NAME;
  function pcap_set_rfmon       (p :PPcap ; RFMon  : Longint):longint; cdecl external PCAP_LIB_NAME;
  function pcap_set_timeout     (p :PPcap ; Timeout: Longint):longint; cdecl external PCAP_LIB_NAME;
  function pcap_set_buffer_size (p :PPcap ; Size   : Longint):longint; cdecl external PCAP_LIB_NAME;
  function pcap_can_set_rfmon   (p :PPcap ):longint; cdecl external PCAP_LIB_NAME;
  function pcap_activate        (p :PPcap ):longint; cdecl external PCAP_LIB_NAME;
  function pcap_open_dead(LinkType: Longint; SnapLen: Longint): PPcap; cdecl; external PCAP_LIB_NAME;
  function pcap_open_offline(FileName: PChar; ErrBuf: PChar): PPcap; cdecl; external PCAP_LIB_NAME;
  procedure pcap_close(p :PPcap); cdecl; external PCAP_LIB_NAME;
  function pcap_loop(p: PPcap; cnt: Longint; Callback: TPCapHandler; User: PChar): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_dispatch(p: PPcap; cnt: Longint; Callback: TPCapHandler; User: PChar): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_next(para1: PPcap; para2:PPcap_Pkthdr): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_next_ex(para1: PPcap; para2:PPPcap_Pkthdr; para3:PPChar): Longint; cdecl; external PCAP_LIB_NAME;
  procedure pcap_breakloop(para1:PPcap); cdecl; external PCAP_LIB_NAME;
  function pcap_stats(para1: PPcap; para2:PPcap_Stat): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_setfilter(para1: PPcap; para2:PBPF_Program): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_setdirection(para1: PPcap; para2:TDirection): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_getnonblock(para1: PPcap; para2:PChar): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_setnonblock(para1: PPcap; para2: Longint; para3:PChar): Longint; cdecl; external PCAP_LIB_NAME;
  procedure pcap_perror(para1: PPcap; para2:PChar); cdecl; external PCAP_LIB_NAME;
  function pcap_inject(para1: PPcap; para2:pointer; para3: TSize): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_sendpacket(para1: PPcap; para2: PChar; para3:Longint): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_strerror(para1:Longint): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_geterr(para1:PPcap): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_compile(para1: PPcap; para2:PBPF_Program; para3: PChar; para4: Longint; para5:DWord): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_compile_nopcap(para1: Longint; para2: Longint; para3:PBPF_Program; para4: PChar; para5: Longint; para6:DWord): Longint; cdecl; external PCAP_LIB_NAME;
  procedure pcap_freecode(para1:PBPF_Program); cdecl; external PCAP_LIB_NAME;
  function pcap_datalink(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_list_datalinks(para1: PPcap; para2:PPLongint): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_set_datalink(para1: PPcap; para2:Longint): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_datalink_name_to_val(para1:PChar): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_datalink_val_to_name(para1:Longint): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_datalink_val_to_description(para1:Longint): PChar; cdecl; external PCAP_LIB_NAME;
  function pcap_snapshot(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_is_swapped(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_major_version(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_minor_version(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_fileno(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_dump_open(para1: PPcap; para2:PChar):PPCapDumper; cdecl; external PCAP_LIB_NAME;
  function pcap_dump_ftell(para1:PPCapDumper): Longint; cdecl; external PCAP_LIB_NAME;
  function pcap_dump_flush(para1:PPCapDumper): Longint; cdecl; external PCAP_LIB_NAME;
  procedure pcap_dump_close(para1:PPCapDumper); cdecl; external PCAP_LIB_NAME;
  procedure pcap_dump(para1: PChar; para2:PPcap_Pkthdr; para3:PChar); cdecl; external PCAP_LIB_NAME;
  function pcap_findalldevs(para1:PPPcap_If; para2:PChar): Longint; cdecl; external PCAP_LIB_NAME;
  procedure pcap_freealldevs(para1:PPcap_If); cdecl; external PCAP_LIB_NAME;
  function pcap_lib_version: PChar; cdecl; external PCAP_LIB_NAME;
  function bpf_filter(para1:Pbpf_insn; para2: PChar; para3:PtrInt; para4:PtrInt):PtrInt; cdecl; external PCAP_LIB_NAME;
  function bpf_validate(f:Pbpf_insn; len:Longint): Longint; cdecl; external PCAP_LIB_NAME;
  function bpf_image(para1:Pbpf_insn; para2:Longint): PChar; cdecl; external PCAP_LIB_NAME;
  procedure bpf_dump(para1:PBPF_Program; para2:Longint); cdecl; external PCAP_LIB_NAME;

{$ifdef WINDOWS}
   function pcap_setbuff(p: PPcap; dim:Longint): Longint; cdecl; external PCAP_LIB_NAME;
   function pcap_setmode(p: PPcap; mode:Longint): Longint; cdecl; external PCAP_LIB_NAME;
   function pcap_setmintocopy(p: PPcap; size:Longint): Longint; cdecl; external PCAP_LIB_NAME;
{$endif}

{$ifdef unix}
  function pcap_get_selectable_fd(para1:PPcap): Longint; cdecl; external PCAP_LIB_NAME;
{$endif}

  const PCAPHDRLEN=sizeof(TPcap_Pkthdr);
implementation


end.
