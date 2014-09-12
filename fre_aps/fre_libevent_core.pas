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

{-$UNDEF FOS_LINK_STATIC}


interface

uses
  Classes, SysUtils,FOS_FCOM_TYPES,cTypes,BaseUnix,Sockets,unixtype
  ;

{$IFDEF WINDOWS}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF DARWIN}
    {$IFDEF CPU64}
      {$IFDEF FOS_DEBUG}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libevent_core_fos64_darwin_deb.a}
          {$linklib libevent_pthreads_fos64_darwin_deb.a}
          {$linklib libevent_openssl_fos64_darwin_deb.a}
          {$linklib libevent_extra_fos64_darwin_deb.a}
        {$ELSE}
          {$linklib libevent_core_fos64_darwin_deb-fosdev}
          {$linklib libevent_pthreads_fos64_darwin_deb-fosdev}
          {$linklib libevent_openssl_fos64_darwin_deb-fosdev}
          {$linklib libevent_extra_fos64_darwin_deb-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ELSE}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libevent_core_fos64_darwin_rel.a}
          {$linklib libevent_pthreads_fos64_darwin_rel.a}
          {$linklib libevent_openssl_fos64_darwin_rel.a}
          {$linklib libevent_extra_fos64_darwin_rel.a}
        {$ELSE}
          {$linklib libevent_core_fos64_darwin_rel-fosdev}
          {$linklib libevent_pthreads_fos64_darwin_rel-fosdev}
          {$linklib libevent_openssl_fos64_darwin_rel-fosdev}
          {$linklib libevent_extra_fos64_darwin_rel-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ENDIF}
    {$ELSE CPU32}
      {$IFDEF FOS_DEBUG}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libevent_core_fos32_darwin_deb.a}
          {$linklib libevent_pthreads_fos32_darwin_deb.a}
          {$linklib libevent_openssl_fos32_darwin_deb.a}
          {$linklib libevent_extra_fos32_darwin_deb.a}
        {$ELSE}
          {$linklib libevent_core_fos32_darwin_deb-fosdev}
          {$linklib libevent_pthreads_fos32_darwin_deb-fosdev}
          {$linklib libevent_openssl_fos32_darwin_deb-fosdev}
          {$linklib libevent_extra_fos32_darwin_deb-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ELSE}
        {$IFDEF FOS_LINK_STATIC}
          {$linklib libevent_core_fos32_darwin_rel.a}
          {$linklib libevent_pthreads_fos32_darwin_rel.a}
          {$linklib libevent_openssl_fos32_darwin_rel.a}
          {$linklib libevent_extra_fos32_darwin_rel.a}
        {$ELSE}
          {$linklib libevent_core_fos32_darwin_rel-fosdev}
          {$linklib libevent_pthreads_fos32_darwin_rel-fosdev}
          {$linklib libevent_openssl_fos32_darwin_rel-fosdev}
          {$linklib libevent_extra_fos32_darwin_rel-fosdev}
        {$ENDIF FOS_LINK_STATIC}
      {$ENDIF}
    {$ENDIF CPU32/64}
  {$ELSE}
    {$IFDEF FREEBSD}
      {$IFDEF CPU64}
        {$IFDEF FOS_DEBUG}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libevent_core_fos64_freebsd_deb.a}
            {$linklib libevent_pthreads_fos64_freebsd_deb.a}
            {$linklib libevent_openssl_fos64_freebsd_deb.a}
            {$linklib libevent_extra_fos64_freebsd_deb.a}
          {$ELSE}
            {$linklib libevent_core_fos64_freebsd_deb-fosdev}
            {$linklib libevent_pthreads_fos64_freebsd_deb-fosdev}
            {$linklib libevent_openssl_fos64_freebsd_deb-fosdev}
            {$linklib libevent_extra_fos64_freebsd_deb-fosdev}
          {$ENDIF FOS_LINK_STATIC}
        {$ELSE NODEBUG}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libevent_core_fos64_freebsd_rel.a}
            {$linklib libevent_pthreads_fos64_freebsd_rel.a}
            {$linklib libevent_openssl_fos64_freebsd_rel.a}
            {$linklib libevent_extra_fos64_freebsd_rel.a}
          {$ELSE}
            {$linklib libevent_core_fos64_freebsd_rel-fosdev}
            {$linklib libevent_pthreads_fos64_freebsd_rel-fosdev}
            {$linklib libevent_openssl_fos64_freebsd_rel-fosdev}
            {$linklib libevent_extra_fos64_freebsd_rel-fosdev}
          {$ENDIF FOS_LINK_STATIC}
        {$ENDIF DEBUG/NODEBUG}
      {$ELSE CPU32}
        {$IFDEF FOS_DEBUG}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libevent_core_fos32_freebsd_deb.a}
            {$linklib libevent_pthreads_fos32_freebsd_deb.a}
            {$linklib libevent_openssl_fos32_freebsd_deb.a}
            {$linklib libevent_extra_fos32_freebsd_deb.a}
          {$ELSE}
            {$linklib libevent_core_fos32_freebsd_deb-fosdev}
            {$linklib libevent_pthreads_fos32_freebsd_deb-fosdev}
            {$linklib libevent_openssl_fos32_freebsd_deb-fosdev}
            {$linklib libevent_extra_fos32_freebsd_deb-fosdev}
          {$ENDIF FOS_LINK_STATIC}
        {$ELSE}
          {$IFDEF FOS_LINK_STATIC}
            {$linklib libevent_core_fos32_freebsd_rel.a}
            {$linklib libevent_pthreads_fos32_freebsd_rel.a}
            {$linklib libevent_openssl_fos32_freebsd_rel.a}
            {$linklib libevent_extra_fos32_freebsd_rel.a}
          {$ELSE}
            {$linklib libevent_core_fos32_freebsd_rel-fosdev}
            {$linklib libevent_pthreads_fos32_freebsd_rel-fosdev}
            {$linklib libevent_openssl_fos32_freebsd_rel-fosdev}
            {$linklib libevent_extra_fos32_freebsd_rel-fosdev}
          {$ENDIF FOS_LINK_STATIC}
        {$ENDIF}
      {$ENDIF CPU64/32}
    {$ELSE}
      {$IFDEF SOLARIS}
        {$IFDEF CPU64}
          {$IFDEF FOS_DEBUG}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libevent_core_fos64_solaris_deb.a}
              {$linklib libevent_pthreads_fos64_solaris_deb.a}
              {$linklib libevent_openssl_fos64_solaris_deb.a}
              {$linklib libevent_extra_fos64_solaris_deb.a}
            {$ELSE}
              {$linklib libevent_core_fos64_solaris_deb-fosdev}
              {$linklib libevent_pthreads_fos64_solaris_deb-fosdev}
              {$linklib libevent_openssl_fos64_solaris_deb-fosdev}
              {$linklib libevent_extra_fos64_solaris_deb-fosdev}
            {$ENDIF FOS_LINK_STATIC}
          {$ELSE}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libevent_core_fos64_solaris_rel.a}
              {$linklib libevent_pthreads_fos64_solaris_rel.a}
              {$linklib libevent_openssl_fos64_solaris_rel.a}
              {$linklib libevent_extra_fos64_solaris_rel.a}
            {$ELSE}
              {$linklib libevent_core_fos64_solaris_rel-fosdev}
              {$linklib libevent_pthreads_fos64_solaris_rel-fosdev}
              {$linklib libevent_openssl_fos64_solaris_rel-fosdev}
              {$linklib libevent_extra_fos64_solaris_rel-fosdev}
            {$ENDIF FOS_LINK_STATIC}
          {$ENDIF}
        {$ELSE}
          {$IFDEF FOS_DEBUG}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libevent_core_fos32_solaris_deb.a}
              {$linklib libevent_pthreads_fos32_solaris_deb.a}
              {$linklib libevent_openssl_fos32_solaris_deb.a}
              {$linklib libevent_extra_fos32_solaris_deb.a}
            {$ELSE}
              {$linklib libevent_core_fos32_solaris_deb-fosdev}
              {$linklib libevent_pthreads_fos32_solaris_deb-fosdev}
              {$linklib libevent_openssl_fos32_solaris_deb-fosdev}
              {$linklib libevent_extra_fos32_solaris_deb-fosdev}
            {$ENDIF FOS_LINK_STATIC}
          {$ELSE}
            {$IFDEF FOS_LINK_STATIC}
              {$linklib libevent_core_fos32_solaris_rel.a}
              {$linklib libevent_pthreads_fos32_solaris_rel.a}
              {$linklib libevent_openssl_fos32_solaris_rel.a}
              {$linklib libevent_extra_fos32_solaris_rel.a}
            {$ELSE}
              {$linklib libevent_core_fos32_solaris_rel-fosdev}
              {$linklib libevent_pthreads_fos32_solaris_rel-fosdev}
              {$linklib libevent_openssl_fos32_solaris_rel-fosdev}
              {$linklib libevent_extra_fos32_solaris_rel-fosdev}
            {$ENDIF FOS_LINK_STATIC}
          {$ENDIF}
        {$ENDIF}
      {$ELSE}
        {$IFDEF LINUX}
          {$IFDEF CPU64}
            {$IFDEF FOS_DEBUG}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libevent_core_fos64_linux_deb.a}
                {$linklib libevent_pthreads_fos64_linux_deb.a}
                {$linklib libevent_openssl_fos64_linux_deb.a}
                {$linklib libevent_extra_fos64_linux_deb.a}
              {$ELSE}
                {$linklib libevent_core_fos64_linux_deb-fosdev}
                {$linklib libevent_pthreads_fos64_linux_deb-fosdev}
                {$linklib libevent_openssl_fos64_linux_deb-fosdev}
                {$linklib libevent_extra_fos64_linux_deb-fosdev}
              {$ENDIF FOS_LINK_STATIC}
            {$ELSE}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libevent_core_fos64_linux_rel.a}
                {$linklib libevent_pthreads_fos64_linux_rel.a}
                {$linklib libevent_openssl_fos64_linux_rel.a}
                {$linklib libevent_extra_fos64_linux_rel.a}
              {$ELSE}
                {$linklib libevent_core_fos64_linux_rel-fosdev}
                {$linklib libevent_pthreads_fos64_linux_rel-fosdev}
                {$linklib libevent_openssl_fos64_linux_rel-fosdev}
                {$linklib libevent_extra_fos64_linux_rel-fosdev}
              {$ENDIF FOS_LINK_STATIC}
            {$ENDIF}
          {$ELSE CPU32}
            {$IFDEF FOS_DEBUG}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libevent_core_fos32_linux_deb.a}
                {$linklib libevent_pthreads_fos32_linux_deb.a}
                {$linklib libevent_openssl_fos32_linux_deb.a}
                {$linklib libevent_extra_fos32_linux_deb.a}
              {$ELSE}
                {$linklib libevent_core_fos32_linux_deb-fosdev}
                {$linklib libevent_pthreads_fos32_linux_deb-fosdev}
                {$linklib libevent_openssl_fos32_linux_deb-fosdev}
                {$linklib libevent_extra_fos32_linux_deb-fosdev}
              {$ENDIF FOS_LINK_STATIC}
            {$ELSE}
              {$IFDEF FOS_LINK_STATIC}
                {$linklib libevent_core_fos32_linux_rel.a}
                {$linklib libevent_pthreads_fos32_linux_rel.a}
                {$linklib libevent_openssl_fos32_linux_rel.a}
                {$linklib libevent_extra_fos32_linux_rel.a}
              {$ELSE}
                {$IFDEF CPUARM}
                  {$linklib libevent_core_fosarmhf32_linux_rel-fosdev}
                  {$linklib libevent_pthreads_fosarmhf32_linux_rel-fosdev}
                  {$linklib libevent_openssl_fosarmhf32_linux_rel-fosdev}
                  {$linklib libevent_extra_fosarmhf32_linux_rel-fosdev}
                {$ELSE}
                  {$linklib libevent_core_fos32_linux_rel-fosdev}
                  {$linklib libevent_pthreads_fos32_linux_rel-fosdev}
                  {$linklib libevent_openssl_fos32_linux_rel-fosdev}
                  {$linklib libevent_extra_fos32_linux_rel-fosdev}
                {$ENDIF}
              {$ENDIF FOS_LINK_STATIC}
            {$ENDIF}
          {$ENDIF CPU63/32}
          {$linklib librt.a}
        {$ELSE}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$PACKRECORDS C}
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

    BEV_EVENT_READING	                 = $01;	//**< error encountered while reading */
    BEV_EVENT_WRITING	                 = $02;	//**< error encountered while writing */
    BEV_EVENT_EOF		         = $10;	//**< eof file reached */
    BEV_EVENT_ERROR		         = $20;	//**< unrecoverable error encountered */
    BEV_EVENT_TIMEOUT	                 = $40;	//**< user-specified timeout reached */
    BEV_EVENT_CONNECTED	                 = $80;

    BEV_NORMAL                           = 0;
    BEV_FLUSH                            = 1;
    BEV_FINISHED                         = 2;

    BEV_OPT_CLOSE_ON_FREE                =1 SHL 0;
    BEV_OPT_THREADSAFE                   =1 SHL 1;
    BEV_OPT_DEFER_CALLBACKS              =1 SHL 2;
    BEV_OPT_UNLOCK_CALLBACKS             =1 SHL 3;

    BUFFEREVENT_SSL_OPEN                 = 0;
    BUFFEREVENT_SSL_CONNECTING           = 1;
    BUFFEREVENT_SSL_ACCEPTING            = 2;

    // method strings = select,poll,epoll,kqueue,devpoll,evport,win32

 type

  evdns_result              = (DNS_IPv4_A=1,DNS_PTR=2,DNS_IPv6_AAAA=3);
  bufferevent_filter_result = (BEV_OK = 0, BEV_NEED_MORE = 1, BEV_ERROR = 2 );
  evbuffer_eol_style        = (EVBUFFER_EOL_ANY, EVBUFFER_EOL_CRLF, EVBUFFER_EOL_CRLF_STRICT, EVBUFFER_EOL_LF );

  event_base                   = record end;
  event_config                 = record end;
  event                        = record end;
  bufferevent                  = record end;
  evbuffer                     = record end;
  evdns_base                   = record end;
  ev_token_bucket_cfg          = record end;
  bufferevent_rate_limit_group = record end;
  evdns_getaddrinfo_request    = record end;
  evbuffer_cb_entry            = record end;
  ssl_st                       = record end;


  ev_off_t                   = off_t;
  ev_ssize_t                 = ssize_t;
  evutil_socket_t            = cInt;
  Pssl_st                    = ^ssl_st;
  PEvent_base                = ^event_base;
  Pevent_config              = ^event_config;
  PEvent                     = ^event;
  PBufferevent               = ^bufferevent;
  PEvbuffer                  = ^evbuffer;
  Pevdns_base                = ^evdns_base;
  Pevdns_getaddrinfo_request = ^evdns_getaddrinfo_request;
  Pevbuffer_cb_entry         = ^evbuffer_cb_entry;

  Pev_token_bucket_cfg          = ^ev_token_bucket_cfg;
  Pbufferevent_rate_limit_group = ^bufferevent_rate_limit_group;

  tbuffereventpair      = array [0..1] of PBufferevent;
  tevutil_socket_t_pair = array [0..1] of evutil_socket_t;
  Pevutil_addrinfo      = ^evutil_addrinfo;

  evbuffer_cb_info      = record
    orig_size : size_t;
    n_added   : size_t;
    n_deleted : size_t;
  end;

  Pevbuffer_cb_info =^evbuffer_cb_info;

  event_callback_fn           = procedure (fd : evutil_socket_t      ; short: cshort ; data:pointer) ; cdecl ;
  event_base_foreach_event_cb = function  (const base : PEvent_base  ; const event : PEvent ; const data : Pointer):cInt; cdecl ;
  bufferevent_data_cb         = procedure (const bev  : PBufferevent ; const ctx : Pointer); cdecl;
  bufferevent_event_cb        = procedure (const bev  : PBufferevent ; what: cshort ; const ctx : Pointer); cdecl;
  bufferevent_filter_cb       = function  (const srv,dst : PEvbuffer ; dst_limit : ev_ssize_t ; mode : cint ; ctx : Pointer):bufferevent_filter_result; cdecl;
  free_context                = procedure (const ctx : Pointer); cdecl;
  evdns_callback_type         = procedure (dns_result : cint ; typ : cchar ; count : cint ; ttl : cint ;  addresses : pointer ; arg : pointer); cdecl;
  evdns_getaddrinfo_cb        = procedure (dns_result : cint ; res : Pevutil_addrinfo ; arg : Pointer); cdecl ;
  evbuffer_cb_func            = procedure (buffer : PEvbuffer ; info : Pevbuffer_cb_info ; arg : Pointer); cdecl;


  evutil_addrinfo = record
      ai_flags : cInt; {* AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST *}
      ai_family: cInt; {* PF_xxx *}
      ai_socktype: cInt; {* SOCK_xxx *}
      ai_protocol: cInt; {* 0 or IPPROTO_xxx for IPv4 and IPv6 *}
      ai_addrlen: size_t; {* length of ai_addr *}
      ai_canonname: PChar; {* canonical name for hostname *}
      ai_addr: psockaddr;	 {* binary address *}
      ai_next: Pevutil_AddrInfo;	 {* next structure in linked list *}
    end;

  evbuffer_ptr = record
    pos :ev_ssize_t;
    _internal: record
       chain        : Pointer;
       pos_in_chain : size_t;
    end;
  end;

  Pevbuffer_ptr = ^evbuffer_ptr;

  evbuffer_iovec = record
    iov_base : Pointer;
    iov_len  : size_t;
  end;

  Pevbuffer_iovec =  ^evbuffer_iovec;


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

  function  bufferevent_socket_new               (const base : PEvent_base  ; fd   : evutil_socket_t ; options : cInt):PBufferevent                           ; cdecl ; external;
  function  bufferevent_socket_connect           (const bev  : PBufferevent ; addr : PFCOM_SOCKADDRSTORAGE ;  const socklen : cint) :cint                     ; cdecl ; external;
  function  bufferevent_socket_connect_hostname  (const bev  : PBufferevent ; evdns_base : Pevdns_base ; family : cInt ; hostname : PChar ; port : cInt):cint ; cdecl ; external;
  function  bufferevent_socket_get_dns_error     (const bev  : PBufferevent):cInt                        ; cdecl ; external;
  function  bufferevent_base_set                 (const base : PEvent_base ; bufev : PBufferevent) :cInt ; cdecl ; external;
  function  bufferevent_get_base                 (const bev  : PBufferevent) : Pevent_base               ; cdecl ; external;
  function  bufferevent_priority_set             (const bev  : PBufferevent ; pri : cInt ) : cInt        ; cdecl ; external;
  procedure bufferevent_free                     (const bev  : PBufferevent)                             ; cdecl ; external;
  procedure bufferevent_setcb                    (const bev  : PBufferevent; readcb,writecb : bufferevent_data_cb ; eventcb : bufferevent_event_cb ; cbarg : Pointer); cdecl ; external ;
  function  bufferevent_setfd                    (const bev  : PBufferevent; fd : evutil_socket_t):cInt  ; cdecl ; external;
  function  bufferevent_getfd                    (const bev  : PBufferevent):evutil_socket_t; cdecl ; external;
  function  bufferevent_get_underlying           (const bev  : PBufferevent):PBufferevent;  cdecl ; external;
  function  bufferevent_write                    (const bev  : PBufferevent; const data : Pointer ; const size : size_t):cInt; cdecl ; external;
  function  bufferevent_write_buffer             (const bev  : PBufferevent; const buf  : PEvbuffer):cint; cdecl ; external;
  function  bufferevent_read                     (const bev  : PBufferevent; const data : Pointer ; const  size : size_t):size_t cdecl ; external;
  function  bufferevent_read_buffer              (const bev  : PBufferevent; const buf  : PEvbuffer):cint; cdecl ; external;
  function  bufferevent_get_input                (const bev  : PBufferevent):PEvbuffer; cdecl ; external;
  function  bufferevent_get_output               (const bev  : PBufferevent):PEvbuffer; cdecl ; external;
  function  bufferevent_disable                  (const bev  : PBufferevent; event :cshort):cint; cdecl ; external;
  function  bufferevent_enable                   (const bev  : PBufferevent; event :cshort):cint; cdecl ; external;
  function  bufferevent_get_enabled              (const bev  : PBufferevent):cshort; cdecl ; external;
  function  bufferevent_set_timeouts             (const bev  : PBufferevent; const timeout_read,timeout_write : PFCOM_TimeVal):cInt; cdecl ; external;
  procedure bufferevent_setwatermark             (const bev  : PBufferevent; events : cshort ; lowmark, highmark : size_t); cdecl ; external;
  procedure bufferevent_lock                     (const bev  : PBufferevent) ;cdecl ; external;
  procedure bufferevent_unlock                   (const bev  : PBufferevent) ;cdecl ; external;
  function  bufferevent_flush                    (const bev  : PBufferevent; iotype : cshort ; mode : cint):cint ;cdecl ; external;
  function  bufferevent_filter_new               (const underlying : PBufferevent; input_filter,output_filter : bufferevent_filter_cb ; options : cInt ; ftx : free_context ; ctx : Pointer):PBufferevent;cdecl ; external;
  function  bufferevent_pair_new                 (const bas  : PEvent_base ; options : cInt ; pair : tbuffereventpair):cInt ;cdecl ; external;
  function  bufferevent_pair_get_partner         (const bev  : PBufferevent):PBufferevent;cdecl;external;

  function  ev_token_bucket_cfg_new                      (const read_rate, read_burst, write_rate, write_burst : size_t ; const tick_len : PFCOM_TimeVal):Pev_token_bucket_cfg ; cdecl ; external;
  procedure ev_token_bucket_cfg_free                     (const cfg  : Pev_token_bucket_cfg ) ;cdecl ; external;
  function  bufferevent_set_rate_limit                   (const bev  : PBufferevent ; cfg : Pev_token_bucket_cfg):cInt ;cdecl ; external;
  function  bufferevent_rate_limit_group_new             (const base : PEvent_base  ; cfg : Pev_token_bucket_cfg):Pbufferevent_rate_limit_group;cdecl ; external;
  function  bufferevent_rate_limit_group_set_cfg         (const grp  : Pbufferevent_rate_limit_group ; cfg : Pev_token_bucket_cfg):cint ;cdecl ; external;
  function  bufferevent_rate_limit_group_set_min_share   (const grp  : Pbufferevent_rate_limit_group ; const val : size_t):cint ;cdecl ; external;
  procedure bufferevent_rate_limit_group_free            (const grp  : Pbufferevent_rate_limit_group) ; cdecl ; external;
  function  bufferevent_add_to_rate_limit_group          (const bev  : PBufferevent ;const g : Pbufferevent_rate_limit_group):cint; cdecl ; external;
  function  bufferevent_remove_from_rate_limit_group     (const bev  : PBufferevent) : cint ;cdecl ; external;
  function  bufferevent_get_read_limit                   (const bev  : PBufferevent):ev_ssize_t; cdecl ; external;
  function  bufferevent_get_write_limit                  (const bev  : PBufferevent):ev_ssize_t; cdecl ; external;
  function  bufferevent_get_max_to_read                  (const bev  : PBufferevent):ev_ssize_t; cdecl ; external;
  function  bufferevent_get_max_to_write                 (const bev  : PBufferevent):ev_ssize_t; cdecl ; external;
  function  bufferevent_rate_limit_group_get_read_limit  (const grp  : Pbufferevent_rate_limit_group):ev_ssize_t;cdecl ; external;
  function  bufferevent_rate_limit_group_get_write_limit (const grp  : Pbufferevent_rate_limit_group):ev_ssize_t;cdecl ; external;

  function  evdns_getaddrinfo                            (dns_base : Pevdns_base ; const nodename : PChar ; const servname : PChar ; const hints_in : Pevutil_addrinfo ; cb : evdns_getaddrinfo_cb ; arg : Pointer):Pevdns_getaddrinfo_request ; cdecl ; external;
  procedure evdns_getaddrinfo_cancel                     (req : Pevdns_getaddrinfo_request); cdecl ; external ;

  //int bufferevent_decrement_read_limit(struct bufferevent *bev, ev_ssize_t decr);
  //int bufferevent_decrement_write_limit(struct bufferevent *bev, ev_ssize_t decr);
  //int bufferevent_rate_limit_group_decrement_read(struct bufferevent_rate_limit_group *, ev_ssize_t);
  //int bufferevent_rate_limit_group_decrement_write(struct bufferevent_rate_limit_group *, ev_ssize_t);
  //void bufferevent_rate_limit_group_get_totals(struct bufferevent_rate_limit_group *grp,ev_uint64_t *total_read_out, ev_uint64_t *total_written_out);
  //void bufferevent_rate_limit_group_reset_totals(struct bufferevent_rate_limit_group *grp);

  //;cdecl ; external;
  //;cdecl ; external;

  //SSL
  function bufferevent_get_openssl_error                  (const bev : PBufferevent):culong; cdecl; external;
  function bufferevent_openssl_filter_new                 (const base : PEvent_base ; underlying : Pbufferevent ;  ssl : Pssl_st ;  ssl_state : cInt ; options : cint):Pbufferevent;cdecl ; external;
  function bufferevent_openssl_socket_new                 (const base : PEvent_base ;  fd : evutil_socket_t ; ssl : Pssl_st ; ssl_state : cInt ; options : cint):PBufferevent ; cdecl ; external;
  function bufferevent_openssl_get_ssl                    (const bev : PBufferevent):Pssl_st; cdecl ; external;
  function bufferevent_ssl_renegotiate                    (const bev : Pbufferevent):cint; cdecl ; external;

  //dns
  function  evdns_base_new                                (event_base:PEvent_base ; initialize_nameservers : cint):Pevdns_base ; cdecl ; external ;

  //util

  // RFC3493
  function  evutil_inet_ntop                    (af : cInt ; src:Pointer ; dst : PChar ; dst_len : size_t) : PChar          ; cdecl ; external;
  function  evutil_inet_pton                    (af : cInt ; src:Pchar   ; dst : Pointer)    : cInt                         ; cdecl ; external;
  function  evutil_parse_sockaddr_port          (str : PChar ; out_sa : PFCOM_SOCKADDRSTORAGE ; var outlen : cInt) : cInt   ; cdecl ; external;
  function  evutil_gai_strerror                 (err : cInt):PChar                                                          ; cdecl ; external;
  function  evutil_socketpair                   (const d, typ, protocol :cInt ;  sv : tevutil_socket_t_pair):cInt           ; cdecl ; external;
  function  evutil_make_socket_nonblocking      (sock : evutil_socket_t):cint                                               ; cdecl ; external;
  function  evutil_make_listen_socket_reuseable (sock : evutil_socket_t):cint                                               ; cdecl ; external;
  function  evutil_make_socket_closeonexec      (sock : evutil_socket_t):cint                                               ; cdecl ; external;
  function  evutil_closesocket                  (sock : evutil_socket_t):cint                                               ; cdecl ; external;
  function  evutil_socket_geterror              (sock : evutil_socket_t):cint                                               ; cdecl ; external;
  function  evutil_socket_error_to_string       (errcode:cint):Pchar                                                        ; cdecl ; external;

  function  evbuffer_new                        : PEvbuffer                                                                 ; cdecl ; external;
  procedure evbuffer_free                       (const evbuffer:PEvbuffer)                                                  ; cdecl ; external;
  function  evbuffer_get_length                 (const evbuffer:PEvbuffer):size_t                                           ; cdecl ; external;
  function  evbuffer_get_contiguous_space       (const evbuffer:PEvbuffer):size_t                                           ; cdecl ; external;
  function  evbuffer_expand                     (const evbuffer:PEvbuffer; datlen : size_t):cInt                            ; cdecl ; external;
  function  evbuffer_add                        (const evbuffer:PEvbuffer; const data: Pointer ; const datlen :size_t):cInt ; cdecl ; external;
  function  evbuffer_remove                     (const evbuffer:PEvbuffer; const data: Pointer ; const datlen :size_t):cInt ; cdecl ; external;
  function  evbuffer_copyout                    (const evbuffer:PEvbuffer; const data: Pointer ; const datlen :size_t):ev_ssize_t ; cdecl ; external;
  function  evbuffer_remove_buffer              (const src, dst:PEvbuffer; const datlen : size_t):cint                      ; cdecl ; external;
  function  evbuffer_readln                     (const buffer:PEvbuffer; var n_read_out : size_t ; eol_style :evbuffer_eol_style):PChar; cdecl ; external;
  function  evbuffer_add_buffer                 (const outbuffer,inbuffer:PEvbuffer):cInt                                   ; cdecl ; external;
  function  evbuffer_add_file                   (const outbuffer:PEvbuffer ; fd :cInt ;  offset,len : ev_off_t):cInt        ; cdecl ; external;
  function  evbuffer_drain                      (const buffer:PEvbuffer ; const len : size_t):cint                          ; cdecl ; external;
  function  evbuffer_write                      (const buffer:PEvbuffer ; fd : evutil_socket_t):cint                        ; cdecl ; external;
  function  evbuffer_write_atmost               (const buffer:PEvbuffer ; fd : evutil_socket_t ; howmuch : ev_ssize_t):cint ; cdecl ; external;
  function  evbuffer_read                       (const buffer:PEvbuffer ; fd : evutil_socket_t ; howmuch : cint):cint       ; cdecl ; external;
  function  evbuffer_peek                       (const buffer:PEvbuffer ; len : ev_ssize_t ; start_at : Pevbuffer_ptr ; vec_out : Pevbuffer_iovec ; n_vec : cInt):cInt ; cdecl ; external;
  function  evbuffer_pullup                     (const buffer:PEvbuffer ; size : ev_ssize_t):Pbyte                          ; cdecl ; external;
  function  evbuffer_prepend                    (const evbuffer:PEvbuffer; const data: Pointer ; const datlen :size_t):cInt ; cdecl ; external;
  function  evbuffer_prepend_buffer             (const outbuffer,inbuffer:PEvbuffer):cInt                                   ; cdecl ; external;
  function  evbuffer_reserve_space              (const buffer:PEvbuffer ; size : ev_ssize_t ; vec: Pevbuffer_iovec ; n_vec : cInt):cInt ; cdecl ; external;
  function  evbuffer_commit_space               (const buffer:PEvbuffer ; vec: Pevbuffer_iovec ; n_vec : cInt):cInt         ; cdecl ; external;

  function evbuffer_add_cb                      (const buffer:PEvbuffer ;  cb : evbuffer_cb_func ;  cbarg : Pointer ):Pevbuffer_cb_entry; cdecl ; external;
  function evbuffer_remove_cb_entry             (const buffer:PEvbuffer ; ent : Pevbuffer_cb_entry):cInt                    ; cdecl ; external;


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
  writeln('Libevent Test ',PChar(event_get_version));
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
  cfg := event_config_new;
  writeln('cfg ',integer(cfg));
//  res := event_config_avoid_method(cfg, 'xx-kqueue');
//  writeln('avoid : ',res);
//  res := event_config_require_features(cfg, EV_FEATURE_ET or EV_FEATURE_O1 or EV_FEATURE_FDS);
  res := event_config_require_features(cfg, EV_FEATURE_O1);
  writeln('features : ',res);
//  base := event_base_new_with_config(cfg);
  base := event_base_new;
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

