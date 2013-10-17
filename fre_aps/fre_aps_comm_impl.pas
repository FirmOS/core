unit fre_aps_comm_impl;

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

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_INTERLOCKED,FRE_FCOM_SSL,fos_fcom_handles,
  FRE_LIBEVENT_CORE,sockets,errors
  {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  {$IFDEF windows}
  ,windows
  {$ENDIF}
  ;

const C_EVENT_RUNNER_THREADS = 4;

procedure SetupAPS_Comm;

implementation

type

  TAPSC_EV_TYPES = (acev_READ,acev_WRITE,acev_TIMEOUT,acev_SIGNAL);
  TAPSC_EV_TYP   = set of TAPSC_EV_TYPES;
  TAPSC_CtrCMD   = procedure  (const cmd : byte ; const data : PShortString) of object;

  { TFRE_APS_LL_EvBaseController }
  {$PACKRECORDS 1}
  TFRE_APS_LL_EvBaseController=class
  private
  type
    TCommreadstate = (cr_BAD,cr_WAIT_LEN,cr_WAITDATA,cr_DISPATCH);
  var
    state    : TCommreadstate;
    cmd      : Byte;
    rest_len : Integer;
    data     : Shortstring;
    data_pos : NativeInt;
    data_len : NativeInt;
    FControlEvent  : PEvent;
    FTimeoutE      : PEvent;
    FCommSink      : evutil_socket_t;
    FEventBase     : PEvent_base;
    FonDispatch    : TAPSC_CtrCMD;
    procedure  _CommEventfired    (what : TAPSC_EV_TYP);
    procedure  _TimeoutEventfired (what : TAPSC_EV_TYP);
  public
    constructor Create(const commsink : cInt ; const mydispatch : TAPSC_CtrCMD);
    procedure   Loop;
  end;

  { TFRE_APS_COMM_LISTENERS_THREADS }

  TFRE_APS_COMM_LISTENERS_THREADS=class(TThread)
  private
    FCB    : TFRE_APS_LL_EvBaseController;
    procedure GotCtrCMD (const cmd : byte ; const data : PShortString);
  public
    constructor create(comm_sink : evutil_socket_t);
    procedure   Execute; override;
  end;

  { TFRE_APS_COMM }
  TFRE_APS_COMM=class(Tobject,IFRE_APS_COMM)
  private
    FListenPair      : tevutil_socket_t_pair;
    FListenersThread : TFRE_APS_COMM_LISTENERS_THREADS;
  public
    constructor create;
    procedure   AddListener_TCP (const Bind_IP:String;const Bind_Port:integer);
  end;

procedure SetupAPS_Comm;
begin
  GFRE_SC := TFRE_APS_COMM.Create;
end;

procedure   LogInfo(const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Info,'APSCOMM');
end;

procedure   LogWarning           (const s:String;const Args : Array of const);
begin
  GFRE_LOG.Log(s,args,'',fll_Warning,'APSCOMM');
end;

function TranslateOsError(const os_error: cint; const prefix: string; postfix: string): string;
begin
  result := prefix+StrError(os_error)+postfix;
end;

procedure CheckRaiseOSCall(const os_result: cint; const prefix: string; postfix: string; const crit_abort: boolean);
var err : cInt;
begin
  if os_result<>0 then
    begin
      err := fpgeterrno;
      if crit_abort then
        GFRE_BT.CriticalAbort(TranslateOsError(err,prefix,postfix))
      else
        raise exception.Create(TranslateOsError(err,prefix,postfix));
    end;
end;

procedure CheckRaise(const os_result: cint; const crit_abort: boolean; const file_nr: cInt=-1);
var postfix :string;
begin
  if file_nr<>-1 then
    postfix := ' ('+inttostr(file_nr)+')';
  CheckRaiseOSCall(os_result,'APS/FCOMM CRITICAL: ',postfix,crit_abort);
end;


function le_evtyp2TAPSC_EV_TYP(const what : cshort):TAPSC_EV_TYP;
begin
  result := [];
  if (what and EV_READ) >0 then
    Include(result,acev_READ);
  if (what and EV_WRITE) >0 then
    Include(result,acev_WRITE);
  if (what and EV_TIMEOUT) >0 then
    Include(result,acev_TIMEOUT);
  if (what and EV_SIGNAL) >0 then
    Include(result,acev_SIGNAL);
end;

procedure APSC_SetupTimeout(const time_ms : NativeInt ; var time : TFCOM_TimeVal);
begin
  Time.tv_usec  := (time_ms mod 1000) * 1000;
  Time.tv_sec   := time_ms div 1000;
end;

function APSC_typ2string(const typ:TAPSC_EV_TYP):ShortString;
begin
  result := '[';
  if acev_READ in typ
    then result := result +'R';
  if acev_WRITE in typ
    then result := result +'W';
  if acev_TIMEOUT in typ
    then result := result +'T';
  if acev_SIGNAL in typ
    then result := result +'S';
  result := result+']';
end;

function  GetANewEventBase : PEvent_base;
var cfg  : Pevent_config;
    res  : Integer;
    feat : Integer;
begin
  cfg      := event_config_new;
  res      := event_config_require_features(cfg, EV_FEATURE_O1);
  result  := event_base_new_with_config(cfg);
  event_config_free(cfg);
  if not assigned(result) then begin
    LogWarning('Cannot get features O1, using fallback',[]);
    result:= event_base_new;
    if not assigned(result) then
      GFRE_BT.CriticalAbort('COULD NOT AQUIRE ANY EVENTBASE!');
  end;
  //feat := event_base_get_features(FLE_Base);
  //if (feat and EV_FEATURE_ET )>0 then  Include(FLE_Features,evf_EDGE_TRIGGERED);
  //if (feat and EV_FEATURE_O1 )>0 then  Include(FLE_Features,evf_O1);
  //if (feat and EV_FEATURE_FDS)>0 then  Include(FLE_Features,evf_FILE_DESCRIPTORS);
  //writeln('');LogInfo('Eventbase Method : [%s %s  %s  %s ]',[string(event_base_get_method(FLE_Base)),BoolToStr(evf_EDGE_TRIGGERED in FLE_Features,'EDGE TRIGGERED','NOT EDGE TRIGGERED'),
end;

{ TFRE_APS_LL_EvBaseController }

procedure TFRE_APS_LL_EvBaseController._CommEventfired(what: TAPSC_EV_TYP);
var s         : string;
    rb        : NativeInt;
    size_read : NativeInt;
begin
  case state of
    cr_BAD: GFRE_BT.CriticalAbort('invalid state APSC basecontroller');
    cr_WAIT_LEN:
      begin
        rb := FpRead(FCommSink,cmd,5);
        if rb<>5 then
          GFRE_BT.CriticalAbort('only read '+inttostr(rb)+' bytes but expected 4');
        state    := cr_WAITDATA;
        data_len := rest_len;
        data_pos := 0;
      end;
    cr_WAITDATA:
      begin
        size_read := FpRead(FCommSink,data[data_pos],rest_len);
        dec(resT_len,size_read);
        inc(data_pos,size_read);
        if rest_len=0 then
          begin
            FonDispatch(cmd,@data[1]);
            state := cr_WAIT_LEN;
          end
        else
         state := cr_WAIT_LEN;
      end;
  end;
  //event_add(FControlEvent,nil);
end;

procedure TFRE_APS_LL_EvBaseController._TimeoutEventfired(what: TAPSC_EV_TYP);
begin
  writeln('TIMEOUT FIRED ',APSC_typ2string(what));
end;

var g_cnt : qword = 0;

constructor TFRE_APS_LL_EvBaseController.Create(const commsink: cInt; const mydispatch: TAPSC_CtrCMD);
begin
  FCommSink   := commsink;
  FonDispatch := mydispatch;
end;

procedure EventCB_TFRE_APS_LL_EvBaseController(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._CommEventfired(le_evtyp2TAPSC_EV_TYP(short));
end;

procedure EventCB_TFRE_APS_LL_EvBaseController_TO(fd : evutil_socket_t ; short: cshort ; data:pointer); cdecl;
begin
  TFRE_APS_LL_EvBaseController(data)._TimeoutEventfired(le_evtyp2TAPSC_EV_TYP(short));
end;

procedure TFRE_APS_LL_EvBaseController.Loop;
var res     : cInt;
    timeout : TFCOM_TimeVal;
begin
  FEventBase    := GetANewEventBase;
  FControlEvent := event_new (FEventBase,FCommSink,EV_READ+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController,self);
  FTimeoutE     := event_new (FEventBase,-1,EV_READ+EV_WRITE+EV_TIMEOUT+EV_PERSIST,@EventCB_TFRE_APS_LL_EvBaseController_TO,self);
  res := event_add(FControlEvent,nil);
  if not assigned(FControlEvent) then
    GFRE_BT.CriticalAbort('APSCL - cannot init control event');
  APSC_SetupTimeout(1000,timeout);
  res := event_add(FTimeoutE,@timeout);
  if not assigned(FTimeoutE) then
    GFRE_BT.CriticalAbort('APSCL - cannot init timeout event');
  state := cr_WAIT_LEN;
  res := event_base_loop(FEventBase,0);
  if res<>0 then
    GFRE_BT.CriticalAbort('APSCL EVENT LOOP FAILED ('+inttostr(res)+')');
end;


{ TFRE_APS_COMM_LISTENERS_THREADS }

procedure TFRE_APS_COMM_LISTENERS_THREADS.GotCtrCMD(const cmd: byte; const data: pshortstring);
begin
  inc(g_cnt);
  if g_cnt mod 1000000=0 then
    writeln('GOT FULL CMD ',data^);
end;

constructor TFRE_APS_COMM_LISTENERS_THREADS.create(comm_sink: evutil_socket_t);
begin
  FCB := TFRE_APS_LL_EvBaseController.Create(comm_sink,@GotCtrCMD);
  inherited create(false);
end;


procedure TFRE_APS_COMM_LISTENERS_THREADS.Execute;
begin
  try
    FCB.Loop;
  except on E:Exception do begin
    try
      GFRE_LOG.Log('APSCOMM M ['+e.Message+']','',fll_Emergency,'APSW',true);
      writeln('APS THREAD EXCEPTION ::: ',e.Message);
      writeln(GFRE_BT.DumpExceptionsBacktrace);
    except on e:exception do begin
      GFRE_LOG.LogEmergency('LOGGING EXCEPTION : APSCOMM M : '+e.Message);
    end;end;
  end;end;
end;

{ TFRE_APS_COMM }

constructor TFRE_APS_COMM.create;
 begin
  FListenPair[0] := -1;
  FListenPair[1] := -1;
  CheckRaise(evutil_socketpair(PF_LOCAL,SOCK_STREAM,0, FListenPair),true);
  //CheckRaise(evutil_make_socket_nonblocking(FListenPair[0]),true,FListenPair[0]);
  //CheckRaise(evutil_make_socket_nonblocking(FListenPair[1]),true,FListenPair[1]);
  //CheckRaise(evutil_make_socket_closeonexec(FListenPair[0]),true,FListenPair[0]);
  //CheckRaise(evutil_make_socket_closeonexec(FListenPair[1]),true,FListenPair[1]);
  FListenersThread:=TFRE_APS_COMM_LISTENERS_THREADS.Create(FListenPair[1]);
  sleep(100);
  AddListener_TCP('*',44000);
end;

procedure APSC_WriteCommpacket(const cmd : byte ; const  data:ShortString ; const fd : cint);
var pack : ShortString;
    len  : integer;
begin
  pack                 := Char(cmd)+#0#0#0#0+data;
  PCardinal(@pack[2])^ := Length(data);
  len := FpWrite(fd,pack[1],Length(pack));
  if len<>Length(pack) then
    raise exception.Create('failed to send comm packet')
end;

procedure TFRE_APS_COMM.AddListener_TCP(const Bind_IP: String; const Bind_Port: integer);
var Packet:ShortString;
    i:integer;
begin
  if length(Bind_IP)>128 then
    raise exception.Create('the bind ip has to be shorten then 128 characters');
  i:=0;
  repeat
    inc(i);
    APSC_WriteCommpacket(1,Bind_IP+'|'+inttostr(i),FListenPair[0]);
  until false;
end;


end.
