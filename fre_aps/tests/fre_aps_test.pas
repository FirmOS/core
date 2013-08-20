unit fre_aps_test;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_TOOL_INTERFACES;

type

     TTestClientState=(cs_CONNECTTRY,cs_CONNECTED,cs_SENDHELLO,cs_WAITHELLO,cs_FINISHED);
     TTestServerState=(ss_CONNECTED,ss_WAITHELLO,ss_SENDECHO,ss_WAITDISCO);
const
     CTestclientState:Array [low(TTestClientState)..high(TTestClientState)] of string= ('TRY CONNECT','CONNECTED','SEND HELLO','WAIT HELLO','FINISHED');
     CTestServerState:Array [low(TTestServerState)..high(TTestServerState)] of string= ('CONNECTED','WAIT HELLO','SENT ECHO','FINISHED');

var G_GoodClientReads:integer=0;
    G_TotalCreatedClients : integer = 0;
    G_SHOW_SRV_DBG   : boolean=false;
    G_SHOW_CLT_DBG   : boolean=false;

type
     { TClientStateHandler }

     TClientStateHandler=class(TObject)
       state:TTestClientState;
       constructor Create      ;
       function    Handler     (const sock:IFCOM_SOCK;const Event:EFOS_FCOM_MULTIEVENT;const Datacount:Integer):boolean;
       procedure   HandleError (const sock:IFCOM_SOCK;const err:String);
     end;

     { TServerStateHandler }

     TServerStateHandler=class(TObject)
       state:TTestServerState;
       constructor Create      (const ns:IFCOM_SOCK);
       function    Handler     (const sock:IFCOM_SOCK;const Event:EFOS_FCOM_MULTIEVENT;const Datacount:Integer):boolean;
       procedure   HandleError (const sock:IFCOM_SOCK;const err:String);
       function    WriteEvent  (const WSOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
     end;


     { TFRE_APS_Test }

     TFRE_APS_Test=class(TObject,IFRE_APS_PROCESS)
     private
       flast_time : int64;
       fshots     : int64;
       fshot_last : int64;
       fperiodic  : integer;
       ptim       : IFRE_APS_TIMER;
       fbreaker   : boolean;
     public
       destructor Destroy; override;
       procedure TimerLost;
       procedure Setup;
       procedure Terminate;
       procedure ReInit;
       procedure Interrupt;
       function  GetName:String;
       procedure TimeFunc           (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
       procedure TimeFuncAlertable  (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
       procedure OneShot            (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
       procedure Finalize;
     end;

     { TFRE_APS_SockTest }

     TFRE_APS_SockTest=class(TObject,IFRE_APS_PROCESS)
       cont:boolean;
       lsock : IFRE_APS_SOCKET_EVENTSOURCE;
       FQFUC : IFRE_APS_TIMER;
       function  Get_IR_Server : IR_FRE_APS_FCOM_SERVER_HANDLER;
       function  Get_IR_Client : IR_FRE_APS_FCOM_CLIENT_HANDLER;
       procedure Setup;
       procedure Terminate;
       procedure ReInit;
       procedure Interrupt;
       function  GetName:String;
       procedure QuitFunc       (const ES: IFRE_APS_EVENTSOURCE; const TID:integer;const Data:Pointer;const cp:integer=0);
       procedure TimeFunc       (const TID:integer;const Data:Pointer;const cp:integer=0);
       function  ClientHandler  (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
       function  ServerHandler  (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
       procedure InitServerSock (const SOCK:IFCOM_SOCK);
       procedure InitClientSock (const SOCK:IFCOM_SOCK);
       procedure TearDownServerSock (const Sock:IFCOM_SOCK);
       procedure TearDownClientSock (const Sock:IFCOM_SOCK);
       procedure ListenerError  (const sock:IFCOM_SOCK;const err:EFOS_OS_ERROR);
       procedure Finalize;
     end;

implementation

{ TFRE_APS_Test }

function TID:String;
begin
//TODO  result:='W'+inttostr(GFRE_S.CURRENT_THREAD)+' ';
end;

destructor TFRE_APS_Test.Destroy;
begin
  inherited Destroy;
end;

procedure TFRE_APS_Test.TimerLost;
begin
  writeln('Timer LOST!');
end;

procedure TFRE_APS_Test.Setup;
begin
  WriteLn('APS TEST SETUP');
  if pos('TIMER',uppercase(paramstr(1)))>0 then begin
    writeln('ADDING TIMER !');
    PTIM := GFRE_S.AddPeriodicTimer(21,@TimeFunc,Pointer(4713),dm_OneWorker,@TimerLost);
    //GFRE_S.AddOneShotTimer(1,@OneShot,pointer(1));
    fshot_last:=GFRE_BT.Get_Ticks_us;
  end;
  if pos('BREAKER',uppercase(paramstr(1)))>0 then begin
    writeln('ADDING ALERTABLE TIMER !');
    PTIM := GFRE_S.AddPeriodicSignalTimer(10000,@TimeFuncAlertable,Pointer(4713),dm_OneWorker);
    fshot_last:=GFRE_BT.Get_Ticks_us;
    fbreaker:=true;
  end;
end;

procedure TFRE_APS_Test.Terminate;
begin
  WriteLn('TERMINATE RECEIVED');
end;

procedure TFRE_APS_Test.ReInit;
begin
  if fbreaker then begin
    //writeln('SHOOTING ALERT');
    ptim.FireEventManual(true);
  end;
  //WriteLn('REINIT RECEIVED');
end;

procedure TFRE_APS_Test.Interrupt;
begin
 Writeln('INTERRUPT RECEiVED');
 GFRE_S.Quit;
end;

function TFRE_APS_Test.GetName: String;
begin
  result:='APS Test Process';
end;

procedure TFRE_APS_Test.TimeFunc(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var fnew:int64;
begin
 fnew:=GFRE_BT.Get_Ticks_us;
 GFRE_LOG.Log('Timer %d  Diff: %d us / %2.2f ms',[integer(data),fnew-flast_time,(fnew-flast_time) / 1000],'TIME');
 writeln(format('TID: %d  Timer %d  Diff: %d  / %2.2f ms',[TID, integer(data),fnew-flast_time,(fnew-flast_time) / 1000]));
 flast_time:=fnew;
 sleep(40);
 //sleep(2000); //TODO TIMER ERROR HANDLING (timer lost)
 inc(fperiodic);
 if fperiodic>10 then begin
  // GFRE_S.
  // TODO Reenable onseshot timer
 end;
end;

procedure TFRE_APS_Test.TimeFuncAlertable(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var fnew:int64;
begin
  if cp=2 then begin
    writeln(format('>DIRECT HIT WRITE Timer %d  Diff: %d us / %2.2f ms   >>>>> %d <<<<<   (%d)',[integer(data),fnew-flast_time,(fnew-flast_time) / 1000,cp,TID]));
  end;
  fnew:=GFRE_BT.Get_Ticks_us;
  if cp=0 then begin
    writeln(format('>Timer %d  Diff: %d us / %2.2f ms   >>>>> %d <<<<<',[integer(data),fnew-flast_time,(fnew-flast_time) / 1000,cp]));
  end;
  sleep(1000);
  if cp=0 then begin
    writeln(format('<Timer %d  Diff: %d us / %2.2f ms   >>>>> %d <<<<<',[integer(data),fnew-flast_time,(fnew-flast_time) / 1000,cp]));
  end;
  flast_time:=fnew;
end;

procedure TFRE_APS_Test.OneShot(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var fnew:int64;
begin
  ES.MarkFinalize;
  if fshots=100 then  exit;
  inc(fshots);
//  GFRE_LOG.Log('ONESHOT  %d  TIME : %d',[fshots,fshots*100],'ONETIME');
 // writeln(format('>TID %d ONESHOT  %d  TIME : %d',[TID,fshots,fshots*100]));
  GFRE_S.AddOneShotTimer(10,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //GFRE_S.AddOneShotTimer(100,@OneShot,nil);
  //sleep(1000);
  writeln(format('<TID %d ONESHOT  %d  TIME : %d',[TID,fshots,fshots*100]));
end;

procedure TFRE_APS_Test.Finalize;
begin
  if assigned(ptim) then begin
    PTIM.FinalizeIt;
  end;
  Free;
end;

{ TFRE_APS_SockTest }

function TFRE_APS_SockTest.Get_IR_Server: IR_FRE_APS_FCOM_SERVER_HANDLER;
begin
  result.InitServerSock:=@InitServerSock;
  result.ListenerError :=@ListenerError;
  result.ServerHandler :=@ServerHandler;
  result.TearDownServerSock:=@TearDownServerSock;
end;

function TFRE_APS_SockTest.Get_IR_Client: IR_FRE_APS_FCOM_CLIENT_HANDLER;
begin
  result.InitClientSock     := @InitClientSock;
  result.ClientHandler      := @ClientHandler;
  result.TearDownClientSock := @TearDownClientSock;
end;

procedure TFRE_APS_SockTest.Setup;
var me:EFOS_FCOM_MULTIERROR;
    cs:IFCOM_SOCK;
    s:string;
    i,cnt,socket_cnt:integer;
begin
  cont:=false;
  s:=uppercase(paramstr(1)+paramstr(2)+paramstr(3));
  WriteLn('APS SOCKET TEST SETUP USE [SERVER CLIENT CONT]');
  if pos('SERVER',s)>0 then begin
    me:=GFRE_S.AddSocketListener('*',44000,fil_IPV4,fsp_TCP,Get_IR_Server,true,lsock);
    if me<>ese_OK then begin
      GFRE_BT.CriticalAbort('Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
    end;
    writeln('SOCKTEST> listening socket created');
  end;
  if pos('CONT',s)>0 then begin
   cont:=true;
  end;
  if pos('CLIENT',s)>0 then begin
   cnt := StrToIntDef(paramstr(2),1);
   socket_cnt:=0;
   for i:=1 to cnt do begin
      me:=GFRE_S.AddSocketClient('127.0.0.1',44000,fil_IPV4,fsp_TCP,Get_IR_Client);
      if me<>ese_OK then begin
        writeln('>> CANNOT CREATE CLIENT SOCKET NR(',i,')',' ',CFOS_FCOM_MULTIERROR[me]);
      end else begin
        inc(socket_cnt);
        inc(G_TotalCreatedClients);
      end;
      //GFRE_S.AddPeriodicTimer(100,@TimeFunc,Pointer(4713));
   end;
   writeln(socket_cnt,'/',i,' client socket(s) created');
   FQFUC := GFRE_S.AddOneShotTimer(1000,@QuitFunc);
  end;
end;

procedure TFRE_APS_SockTest.Terminate;
begin
  if assigned(lsock) then begin
    lsock.FinalizeIt;
  end;
  writeln('(TERM SOCK)');
end;

procedure TFRE_APS_SockTest.ReInit;
begin
  //writeln('(HUP)');
end;

procedure TFRE_APS_SockTest.Interrupt;
begin
  writeln('INTERRUPT (CTRL-C)');
  GFRE_S.Quit;
end;

function TFRE_APS_SockTest.GetName: String;
begin

end;

procedure TFRE_APS_SockTest.QuitFunc(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
begin
  writeln('TIMED QUIT');
  GFRE_S.Quit;
end;

procedure TFRE_APS_SockTest.TimeFunc(const TID: integer; const Data: Pointer; const cp: integer);
var i:integer;
    me:EFOS_FCOM_MULTIERROR;
    c_handler : IR_FRE_APS_FCOM_CLIENT_HANDLER;
begin
  if not cont then exit;
  for i:=1 to 50 do begin
   c_handler.InitClientSock     := @InitClientSock;
   c_handler.ClientHandler      := @ClientHandler;
   c_handler.TearDownClientSock := @TearDownClientSock;
   me:=GFRE_S.AddSocketClient('127.0.0.1',44000,fil_IPV4,fsp_TCP,c_handler);
   //if me<>ese_OK then begin
   //  GFRE_BT.CriticalAbort('Cant create client socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
   // end;
  end;
end;

function  TFRE_APS_SockTest.ClientHandler(const Event: EFOS_FCOM_MULTIEVENT; const SOCK: IFCOM_SOCK; const Datacount: Integer):boolean;
begin
  result := TClientStateHandler(Sock.Data).Handler(sock,Event,Datacount);
end;

function TFRE_APS_SockTest.ServerHandler(const Event: EFOS_FCOM_MULTIEVENT; const SOCK: IFCOM_SOCK; const Datacount: Integer):boolean;
begin
  result := TServerStateHandler(Sock.Data).Handler(sock,Event,Datacount);
end;

procedure TFRE_APS_SockTest.InitServerSock(const SOCK:IFCOM_SOCK);
begin
  SOCK.Data:=TServerStateHandler.Create(sock);
  //writeln('SOCKTEST> GOT new connected sock ',sock.GetHandleKey);
end;

procedure TFRE_APS_SockTest.InitClientSock(const SOCK:IFCOM_SOCK);
begin
  //writeln('CS>> INIT CLIENT SOCK');
  Sock.Data:=TClientStateHandler.Create;
end;

procedure TFRE_APS_SockTest.TearDownServerSock(const Sock: IFCOM_SOCK);
begin
   //writeln('****************** TEAR DOWN SERVER SOCK ***');
  TServerStateHandler(SOCK.Data).free;
end;

procedure TFRE_APS_SockTest.TearDownClientSock(const Sock: IFCOM_SOCK);
begin
  //writeln('****************** TEAR DOWN CLIENT SOCK *** ',sock.GetHandleKey,' ',integer(sock.Data));
  TClientStateHandler(SOCK.Data).free;
end;

procedure TFRE_APS_SockTest.ListenerError(const sock: IFCOM_SOCK; const err: EFOS_OS_ERROR);

  procedure ReenableListener(const input:TObject);
  var me:EFOS_FCOM_MULTIERROR;
      lsock:IFRE_APS_SOCKET_EVENTSOURCE;
  begin
    me:=GFRE_S.AddSocketListener('*',44000,fil_IPV4,fsp_TCP,TFRE_APS_SockTest(input).Get_IR_Server,true,lsock);
    if me<>ese_OK then begin
      GFRE_BT.CriticalAbort('Cant create listening socket <%s>',[CFOS_FCOM_MULTIERROR[me]]);
    end;
    writeln('SOCKTEST> listening socket created');
  end;

begin
  writeln('LISTENER ERROR ',err,' ON ',sock.Get_AI.SocketAsString);
  writeln('REENABLING LISTENER in 1 second');
  GFRE_S.Schedule_Timed_LNM(1000,@ReenableListener,self);
end;

procedure TFRE_APS_SockTest.Finalize;
begin
  if assigned(lsock) then begin
    lsock.FinalizeIt;
  end;
  if assigned(FQFUC) then begin
   FQFUC.FinalizeIt;
  end;
  free;
end;

{ TClientStateHandler }

constructor TClientStateHandler.Create;
begin
  state:=cs_CONNECTTRY;
end;

function TClientStateHandler.Handler(const sock: IFCOM_SOCK; const Event: EFOS_FCOM_MULTIEVENT; const Datacount: Integer): boolean;
var sw,sr:integer;
     s:String;
   eos:EFOS_OS_ERROR;
begin
// writeln('CLIENTHANDLER> ',Event);
 result := true;
 case event of
    esv_SOCKERROR: begin
      HandleError(sock,'received error state');
    end;
    esv_SOCKCONNECTED: begin
      case state of
        cs_CONNECTTRY: begin
          if G_SHOW_CLT_DBG then writeln(TID,'C>  CONNECTED <',sock.GetHandleKey,'> -> SEND HELLO');
          state:=cs_CONNECTED;
        end;
        else HandleError(sock,'invalid state for SOCKCONNECTED event');
      end;
    end;
    esv_SOCKREAD: begin
      case state of
        cs_CONNECTED: begin
          //writeln(TID,'SOCKREAD/CONNECTED <',sock.GetMonitorHandleKey,'>');
          sock.ReceiveString(s,Datacount,sr);
          if G_SHOW_CLT_DBG then writeln(TID,'C>  CLIENT RECEIVED [',s,']');
          if G_SHOW_CLT_DBG then writeln(TID,'C>  SEND HELLO <',sock.GetHandleKey,'> -> WAITHELLO');
          s:=format('HELLO:%d',[sock.GetHandleKey]);
          //s:=StringOfChar('*',512);
          state:=cs_WAITHELLO;
          sock.Offload_Write(s);
          if G_SHOW_CLT_DBG then WriteLn(TID,'C< <CLIENT SCEDULED WRITE [',s,']');
        end;
        cs_WAITHELLO: begin
          if G_SHOW_CLT_DBG then writeln(TID,'C> WAITHELLO <',sock.GetHandleKey,'> -> FINISH');
          sock.ReceiveString(s,Datacount,sr);
          if G_SHOW_CLT_DBG then writeln(TID,'C> CLIENT RECEIVED [',s,']');
          sock.CloseEnqueue(100);
          result := false;
          //write('.');
          inc(G_GoodClientReads);
          //GFRE_S.Quit;
        end;
        else HandleError(sock,'invalid state for SOCKCONNECTED event');
      end;
    end;
    esv_SOCKCLOSED:begin
      sock.CloseEnqueue(4711);
      HandleError(sock,'!!!!!!!!!!!!!!   ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
    end;
    esv_SOCKWRITE,
    esv_SOCKEXCEPT,
    esv_SOCKCANTCONNECT,
    esv_SOCKCONNREFUSED,
    esv_SOCKCONNTIMEDOUT:
    begin
      HandleError(sock,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
    end;
  end;
end;

procedure TClientStateHandler.HandleError(const sock: IFCOM_SOCK; const err: String);
begin
  writeln(format('CLIENT ERROR FOR SOCKET <%d>, IN STATE <%s> : [%s]',[sock.GetHandleKey,CTestclientState[state],err]));
end;

//function TClientStateHandler.WriteEvent(const WSOCK: IFCOM_SOCK; const Datacount: Integer): boolean;
//var  s:String;
//   eos:EFOS_OS_ERROR;
//   sw:integer;
//begin
//  s:=format('HELLO:%d',[wsock.GetHandleKey]);
//  eos:=wsock.SendString(s,sw);
//  WriteLn(TID,'C< <CLIENT WROTE [',s,'] ',eos,' ',sw);
//end;

{ TServerStateHandler }

constructor TServerStateHandler.Create(const ns:IFCOM_SOCK);
begin
  state:=ss_CONNECTED;
//  sock:=ns;
end;

function TServerStateHandler.Handler(const sock:IFCOM_SOCK;const Event: EFOS_FCOM_MULTIEVENT;const Datacount: Integer):boolean;
var s:String;
    sr:integer;
    sw:integer;
    ose:EFOS_OS_ERROR;
begin
  if G_SHOW_SRV_DBG  then writeln('ENTER HANDLER ',Event,' ',Datacount,' in ',state);
  result := true;
  case event of
    esv_SOCKERROR: begin
      HandleError(sock,'received error state');
    end;
    esv_SOCKCONNECTED: begin
      case state of
        ss_CONNECTED: begin
          s := 'HELLO <'+sock.Get_AI.IPAsString+':'+sock.Get_AI.PortAsString+'>';
          sock.Offload_Write(s);
          state:=ss_WAITHELLO;
          result:=true;
        end;
        else HandleError(sock,'invalid state for SOCKCONNECTED event');
      end;
    end;
    esv_SOCKREAD: begin
      case state of
        ss_WAITHELLO: begin
          if G_SHOW_SRV_DBG then writeln(TID,'SERVER WAITHELLO <',sock.GetHandleKey,'-> SENDECHO');
          sock.ReceiveString(s,Datacount,sr);
          if G_SHOW_SRV_DBG then writeln(TID,'>SERVER RECEIVED [',s,']');
          state:=ss_SENDECHO;
          if G_SHOW_SRV_DBG then writeln(TID,'SEND ECHO <',sock.GetHandleKey,'-> WAITDISCO');
          state:=ss_WAITDISCO;
          s:='THANKYOU!';//+StringOfChar('*',4069);
          sock.Offload_Write(s);
          sock.CloseEnqueue(1);
          if G_SHOW_SRV_DBG then writeln(TID,'<SERVER SCEDuLED SENT [',s,'] ');
          result:=true;
        end;
        else HandleError(sock,'invalid state for SOCKREAD event');
      end;
    end;
    esv_SOCKWRITE: begin
        HandleError(sock,'invalid state for SOCKWRITE event');
    end;
    esv_SOCKCLOSED: begin
        case state of
          ss_WAITDISCO: begin
            //writeln(TID,'<>CLIENT DONE');
          end;
          else HandleError(sock,'invalid state for SOCKCLOSED event');
        end;
    end;
    esv_SOCKEXCEPT,
    esv_SOCKCANTCONNECT,
    esv_SOCKCONNREFUSED,
    esv_SOCKCONNTIMEDOUT: begin
      HandleError(sock,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
    end;
    else GFRE_BT.CriticalAbort('UNKNOWN STATE IN SERVER HANDLER');
  end;
  //GFRE_BT.CriticalAbort('DUNK');
  result:=true;
end;

procedure TServerStateHandler.HandleError(const sock: IFCOM_SOCK; const err: String);
begin
  writeln(format('SERVER ERROR FOR SOCKET <%s>, IN STATE <%s> : [%s]',[sock.GetVerboseDesc,CTestServerState[state],err]));
end;

function TServerStateHandler.WriteEvent(const WSOCK: IFCOM_SOCK; const Datacount: Integer): boolean;
var     ose : EFOS_OS_ERROR;
        s   : string;
        sw  : integer;
begin
  Result := false;
//  writeln('Entered Writeevent ',state,' DC=',Datacount);
  case state of
    ss_CONNECTED: ;
    ss_WAITHELLO: ;
    ss_SENDECHO: begin
      writeln(TID,'SEND ECHO <',wsock.GetHandleKey,'-> WAITDISCO');
      state:=ss_WAITDISCO;
      s:='THANKYOU';
      ose:=wsock.SendString(s,sw);
      writeln(TID,'<SERVER SENT [',s,'] ',ose);
      result:=true;
    end;
    ss_WAITDISCO: ;
  end;

end;

end.

