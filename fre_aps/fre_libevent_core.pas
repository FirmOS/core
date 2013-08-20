unit fre_libevent_core;

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
{-$DEFINE FOS_LINK_STATIC}
{-$DEFINE FOS_DEBUG}


interface

uses
  Classes, SysUtils,FOS_FCOM_TYPES,cTypes,BaseUnix
  ;


{$IFDEF WINDOWS}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF FOS_LINK_STATIC}
    {$IFDEF DARWIN}
      {$IFDEF CPU64}
        {$IFDEF FOS_DEBUG}
          {$linklib libevent_core_fos64_darwin_deb.a}
          {$linklib libevent_pthreads_fos64_darwin_deb.a}
          {$linklib libevent_openssl_fos64_darwin_deb.a}
          {$linklib libevent_extra_fos64_darwin_deb.a}
        {$ELSE}
            {$linklib libevent_core_fos64_darwin_rel.a}
            {$linklib libevent_pthreads_fos64_darwin_rel.a}
            {$linklib libevent_openssl_fos64_darwin_rel.a}
            {$linklib libevent_extra_fos64_darwin_rel.a}
        {$ENDIF}
      {$ELSE}
        {$IFDEF FOS_DEBUG}
          {$linklib libevent_core_fos32_darwin_deb.a}
          {$linklib libevent_pthreads_fos32_darwin_deb.a}
          {$linklib libevent_openssl_fos32_darwin_deb.a}
          {$linklib libevent_extra_fos32_darwin_deb.a}
        {$ELSE}
          {$linklib libevent_core_fos32_darwin_rel.a}
          {$linklib libevent_pthreads_fos32_darwin_rel.a}
          {$linklib libevent_openssl_fos32_darwin_rel.a}
          {$linklib libevent_extra_fos32_darwin_rel.a}
        {$ENDIF}
      {$ENDIF}
    {$ELSE}
      {$IFDEF FREEBSD}
        {$IFDEF CPU64}
          {$IFDEF FOS_DEBUG}
            {$linklib libevent_core_fos64_freebsd_deb.a}
            {$linklib libevent_pthreads_fos64_freebsd_deb.a}
            {$linklib libevent_openssl_fos64_freebsd_deb.a}
            {$linklib libevent_extra_fos64_freebsd_deb.a}
          {$ELSE}
            {$linklib libevent_core_fos64_freebsd_rel.a}
            {$linklib libevent_pthreads_fos64_freebsd_rel.a}
            {$linklib libevent_openssl_fos64_freebsd_rel.a}
            {$linklib libevent_extra_fos64_freebsd_rel.a}
          {$ENDIF}
        {$ELSE}
          {$IFDEF FOS_DEBUG}
            {$linklib libevent_core_fos32_freebsd_deb.a}
            {$linklib libevent_pthreads_fos32_freebsd_deb.a}
            {$linklib libevent_openssl_fos32_freebsd_deb.a}
            {$linklib libevent_extra_fos32_freebsd_deb.a}
          {$ELSE}
            {$linklib libevent_core_fos32_freebsd_rel.a}
            {$linklib libevent_pthreads_fos32_freebsd_rel.a}
            {$linklib libevent_openssl_fos32_freebsd_rel.a}
            {$linklib libevent_extra_fos32_freebsd_rel.a}
          {$ENDIF}
        {$ENDIF}
      {$ELSE}
        {$IFDEF SOLARIS}
          {$IFDEF CPU64}
            {$IFDEF FOS_DEBUG}
              {$linklib libevent_core_fos64_solaris_deb.a}
              {$linklib libevent_pthreads_fos64_solaris_deb.a}
              {$linklib libevent_openssl_fos64_solaris_deb.a}
              {$linklib libevent_extra_fos64_solaris_deb.a}
            {$ELSE}
              {$linklib libevent_core_fos64_solaris_rel.a}
              {$linklib libevent_pthreads_fos64_solaris_rel.a}
              {$linklib libevent_openssl_fos64_solaris_rel.a}
              {$linklib libevent_extra_fos64_solaris_rel.a}
            {$ENDIF}
          {$ELSE}
            {$IFDEF FOS_DEBUG}
              {$linklib libevent_core_fos32_solaris_deb.a}
              {$linklib libevent_pthreads_fos32_solaris_deb.a}
              {$linklib libevent_openssl_fos32_solaris_deb.a}
              {$linklib libevent_extra_fos32_solaris_deb.a}
            {$ELSE}
              {$linklib libevent_core_fos32_solaris_rel.a}
              {$linklib libevent_pthreads_fos32_solaris_rel.a}
              {$linklib libevent_openssl_fos32_solaris_rel.a}
              {$linklib libevent_extra_fos32_solaris_rel.a}
            {$ENDIF}
          {$ENDIF}
        {$ELSE}
          {$IFDEF LINUX}
            {$IFDEF CPU64}
              {$IFDEF FOS_DEBUG}
                {$linklib libevent_core_fos64_linux_deb.a}
                {$linklib libevent_pthreads_fos64_linux_deb.a}
                {$linklib libevent_openssl_fos64_linux_deb.a}
                {$linklib libevent_extra_fos64_linux_deb.a}
              {$ELSE}
                {$linklib libevent_core_fos64_linux_rel.a}
                {$linklib libevent_pthreads_fos64_linux_rel.a}
                {$linklib libevent_openssl_fos64_linux_rel.a}
                {$linklib libevent_extra_fos64_linux_rel.a}
              {$ENDIF}
            {$ELSE}
              {$IFDEF FOS_DEBUG}
                {$linklib libevent_core_fos32_linux_deb.a}
                {$linklib libevent_pthreads_fos32_linux_deb.a}
                {$linklib libevent_openssl_fos32_linux_deb.a}
                {$linklib libevent_extra_fos32_linux_deb.a}
              {$ELSE}
                {$linklib libevent_core_fos32_linux_rel.a}
                {$linklib libevent_pthreads_fos32_linux_rel.a}
                {$linklib libevent_openssl_fos32_linux_rel.a}
                {$linklib libevent_extra_fos32_linux_rel.a}
              {$ENDIF}
            {$ENDIF}
            {$linklib librt.a}
          {$ELSE}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ELSE}
    {$linklib libevent-2.0.5}
    {$linklib libevent_pthreads-2.0.5}
  {$ENDIF FOS_LINK_STATIC}
{$ENDIF}


 const
    EV_FEATURE_ET  = $01;
    EV_FEATURE_O1  = $02;
    EV_FEATURE_FDS = $04;

    EVENT_BASE_FLAG_NOLOCK               = $01;
    EVENT_BASE_FLAG_IGNORE_ENV           = $02;
    EVENT_BASE_FLAG_STARTUP_IOCP         = $04;
    EVENT_BASE_FLAG_NO_CACHE_TIME        = $08;
    EVENT_BASE_FLAG_EPOLL_USE_CHANGELIST = $10;

    EVLOOP_ONCE                          = $01;
    EVLOOP_NONBLOCK                      = $02;
    EVLOOP_NO_EXIT_ON_EMPTY              = $04;

    EV_TIMEOUT                           = $01;
    EV_READ                              = $02;
    EV_WRITE                             = $04;
    EV_SIGNAL                            = $08;
    EV_PERSIST                           = $10;
    EV_ET                                = $20;
    // method strings = select,poll,epoll,kqueue,devpoll,evport,win32

 type
  event_base   = record
  end;

  event_config = record
  end;

  event        = record
  end;


  evutil_socket_t = cInt;
  PEvent_base     = ^event_base;
  Pevent_config   = ^event_config;
  PEvent          = ^event;

  event_callback_fn           = procedure (fd : evutil_socket_t ; short: cshort ; data:pointer) ; cdecl ;
  event_base_foreach_event_cb = function  (const base : PEvent_base ; const event : PEvent ; const data : Pointer):cInt; cdecl ;

  function  evthread_use_pthreads                                                       : cInt          ; cdecl ; external; // only U*xes / posix
  function  event_get_version                                                           : PChar         ; cdecl ; external;
  function  event_base_new                                                              : Pevent_base   ; cdecl ; external;
  function  event_config_new                                                            : Pevent_config ; cdecl ; external;
  function  event_config_require_features  (cfg : Pevent_config ; feature : cInt)       : cInt          ; cdecl ; external;
  function  event_config_set_flag          (cfg : Pevent_config ; evb_flag : cInt)      : cInt          ; cdecl ; external;
  function  event_base_new_with_config     (cfg : Pevent_config)                        : Pevent_base   ; cdecl ; external;
  function  event_config_avoid_method      (cfg : Pevent_config ; const method : PChar) : cInt          ; cdecl ; external;
  procedure event_config_free              (cfg : Pevent_config)                                        ; cdecl ; external;
  function  event_get_supported_methods                                                : PPChar         ; cdecl ; external;
  function  event_base_get_method          (const base:PEvent_base)                     : PChar         ; cdecl ; external;
  function  event_base_get_features        (const base:PEvent_base)                     : cInt          ; cdecl ; external;
  procedure event_base_free                (const base:PEvent_base)                                     ; cdecl ; external;
  function  event_base_priority_init       (const base:PEvent_base ; n_prios : cInt)    : cInt          ; cdecl ; external;
  function  event_base_get_npriorities     (const base:PEvent_base )                    : cInt          ; cdecl ; external;
  function  event_base_loop                (const base:PEvent_base ; flags : cInt)      : cInt          ; cdecl ; external;
  function  event_base_loopexit            (const base:PEvent_base ;
                                            const tv : PFCOM_TimeVal)                  : cInt           ; cdecl ; external;
  function  event_base_loopbreak           (const base:PEvent_base)                    : cInt           ; cdecl ; external;
  procedure event_base_gettimeofday_cached (const base:PEvent_base ;
                                            var   tv : TFCOM_TimeVal)                                   ; cdecl ; external;

  function  event_base_foreach_event        (const base:PEvent_base ;
                                               fn  : event_base_foreach_event_cb ;
                                               arg : Pointer):cInt                                      ; cdecl ; external; //2.1 alpha

  function  event_new                      (const base:PEvent_base ; fd : evutil_socket_t ;
                                            what: cshort ; cb : event_callback_fn ;
                                            arg : Pointer)                              : PEvent        ; cdecl ; external;
  function  event_get_callback_arg         (const ev:PEvent)                            : Pointer       ; cdecl ; external;
  procedure event_free                     (event :Pevent)                                              ; cdecl ; external;
  function  event_add                      (event :PEvent ; tv : PFCOM_TimeVal)         : cInt          ; cdecl ; external;
  function  event_del                      (event :PEvent )                             : cInt          ; cdecl ; external;
  function  event_priority_set             (event :PEvent; priority : cInt)             : cInt          ; cdecl ; external;
  procedure event_active                   (event :PEvent; what     : cInt ; ncalls:cshort)             ; cdecl ; external;




  // RFC3493
  function  evutil_inet_ntop               (af : cInt ; src:Pointer ; dst : PChar ;
                                            dst_len : size_t)                           : PChar         ; cdecl ; external;
  function  evutil_inet_pton               (af : cInt ; src:Pchar   ; dst : Pointer)    : cInt          ; cdecl ; external;
  function  evutil_parse_sockaddr_port     (str : PChar ; out_sa : PFCOM_SOCKADDRSTORAGE ;
                                            var outlen : cInt)                          : cInt          ; cdecl ; external;

  procedure Test_LE;

implementation

  var cfg   : Pevent_config;
      res   : integer;
      base  : PEvent_base;

    procedure EventCB (fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
    var tvw    : TFCOM_TimeVal = (tv_sec : 5 ; tv_usec : 0);
    begin
      writeln('EVENT CALLBACK ',fd,' ',short,' ',integer(data));
      writeln(event_base_loopbreak(base));
      tvw.tv_sec:=0;
      tvw.tv_usec:=0;
      writeln(event_base_loopexit(base,@tvw));
    end;

procedure TesT_LE;
var meths : PPchar;
    i     : Integer;
    tv    : TFCOM_TimeVal = (tv_sec : 5 ; tv_usec : 0);
    ev1,
    ev2   : PEvent;

  procedure _setupUnix;
    procedure _SetupSignals;
    var ign,dummy: SigactionRec;
    begin
      ign.sa_handler:=SigActionHandler(SIG_IGN);
      ign.sa_flags:=0;
      fpsigemptyset(ign.sa_mask);
      FPsigaction(SIGINT,  @ign, @dummy);
      FPSigaction(SIGUSR1, @ign, @dummy);
      FPSigaction(SIGHUP,  @ign, @dummy);
      FPSigaction(SIGTERM, @ign, @dummy);
      FPSigaction(SIGPIPE, @ign, @dummy); // IGNORE BROKEN PIPE
    end;
  begin
    _SetupSignals;
  end;

  var sa  : TFCOM_SOCKADDRSTORAGE;
      //sa  : sockaddr;
      len : cInt;
      s   : string;
      res_string  : string;

begin
  writeln('Libevent Test');
//  res := evutil_parse_sockaddr_port('fe80::ca2a:14ff:fe14:2764',@sa,len);
  len := sizeof(TFCOM_SOCKADDRSTORAGE);
  writeln('sizeof sockaddr ',len);
  FillByte(sa,sizeof(sa),0);
  res := evutil_parse_sockaddr_port('[fe80::ca2a:14ff:fe14:2764]:44000',@sa,len);
  res := evutil_parse_sockaddr_port('10.1.0.133:44000',@sa,len);
  writeln('PARSE ',res,' ',len);
  setlength(res_string,128);
  writeln(sa.sin_family);
  case sa.sin_family of
    FCOM_AF_INET : evutil_inet_ntop(FCOM_AF_INET,@sa.sin_addr,@res_String[1],Length(res_string));
    FCOM_AF_INET6: evutil_inet_ntop(FCOM_AF_INET6,@sa.sin6_addr,@res_String[1],Length(res_string));
  end;
  writeln('FORMAT : ',pchar(@res_string[1]),' ',BEtoN(sa.sin_port),' ',BEtoN(sa.sin6_port));
  exit;
  cfg := event_config_new;
  writeln('cfg ',integer(cfg));
  res := event_config_avoid_method(cfg, 'xx-kqueue');
  writeln('avoid : ',res);
  res := event_config_require_features(cfg, EV_FEATURE_ET or EV_FEATURE_O1 or EV_FEATURE_FDS);
  writeln('features : ',res);
  base := event_base_new_with_config(cfg);
  event_config_free(cfg);
  writeln('base : ',integer(base));
  _setupUnix;
  meths := event_get_supported_methods;
  for i:=0 to 100 do begin
    if assigned(meths[i]) then begin
      writeln('All Method : ',i,' ',string(meths[i]));
    end else break;
  end;
  writeln('Chosen Method : ',string(event_base_get_method(base)),' Features : ',event_base_get_features(base));
  ev1 := event_new(base, SIGINT, EV_SIGNAL or  EV_PERSIST, @EventCB, nil);

  writeln('EV1 ',integer(ev1));
  //event_base_loopexit(base, @tv);
  writeln('LOOP');
  event_add(ev1,@tv);
//  event_base_loop(base,EVLOOP_NO_EXIT_ON_EMPTY);

  event_base_loop(base,0);

  writeln('DONE');
  event_base_free(base);
end;



end.

