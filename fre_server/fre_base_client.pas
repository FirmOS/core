unit fre_base_client;

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
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FRE_SYS_BASE_CS,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,FOS_INTERLOCKED;


type

  { TFRE_BASE_CLIENT }

  TFRE_BASE_CLIENT=class(TObject,IFRE_APS_PROCESS)
  private
    type
      TBC_ClientState=(csUnknown,csWaitConnect,csConnected,csTimeoutWait,csSETUPSESSION);
      TDispatch_Continuation = record
        CID        : Qword;
        Contmethod : TFRE_DB_CONT_HANDLER;
        Answer     : IFRE_DB_COMMAND;
        ExpiredAt  : TFRE_DB_DateTime64;
      end;
    var
      FApps                    : IFRE_DB_Object;
      FTimeout                 : integer;
      fMySessionID             : String;
      FClientState             : TBC_ClientState;
      FCMD_Signal              : IFRE_APS_TIMER;
      FContinuationArray       : Array [0..cFRE_DB_MAX_PENDING_CLIENT_REQUESTS] of TDispatch_Continuation;
      FContinnuationCount      : Nativeint;
      FContinuationLock        : IFOS_LOCK;
      FConnectionLock          : IFOS_LOCK;
      FBaseconnection          : TFRE_CLIENT_BASE_CONNECTION;
      FClientSock              : IFCOM_SOCK;
      fsendcmd_id              : QWord;

    procedure CCB_SessionSetup(const DATA : IFRE_DB_Object ; const status:TFRE_DB_COMMAND_STATUS ; const error_txt:string);

    function  _GetClientHandler       : IR_FRE_APS_FCOM_CLIENT_HANDLER;
    procedure ConnectionInit          (const SOCK:IFCOM_SOCK);
    function  ConnectionHandler       (const Event:EFOS_FCOM_MULTIEVENT;const SOCK:IFCOM_SOCK;const Datacount:Integer):boolean;
    procedure ConnectionTearDown      (const Sock:IFCOM_SOCK);
    procedure SendServerCommand       (const base_connection: TFRE_CLIENT_BASE_CONNECTION; const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER=nil; const timeout: integer=5000);
    procedure MyConnectionStateChange (sender:Tobject);
    procedure MyStateCheckTimer       (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0); // Timout & CMD Arrived & Answer Arrived
    procedure MyCommandAnswerArrived  (const sender:TObject;const cmd:IFRE_DB_COMMAND); // from socket
    procedure DispatchAnswers         ;
  protected
    function  GetName: String;
    procedure Terminate;
    procedure ReInit; virtual;
    procedure Finalize;
    procedure Interrupt; virtual;
    procedure Setup;
  public
    function  Get_AppClassAndUid   (const appkey : string ; out app_classname : TFRE_DB_String ; out uid : TGuid) : boolean;
    function  SendServerCommand    (const InvokeClass,InvokeMethod : String;const uidpath:TFRE_DB_GUIDArray;const DATA: IFRE_DB_Object;const ContinuationCB : TFRE_DB_CONT_HANDLER=nil;const timeout:integer=5000) : boolean;
    procedure MySessionEstablished ; virtual;
    procedure MySessionDisconnected; virtual;
    procedure MyInitialize         ; virtual;
    procedure MyFinalize           ; virtual;
    procedure QueryUserPass        (out user,pass:string);virtual;
  end;


implementation

{ TFRE_BASE_CLIENT }

procedure TFRE_BASE_CLIENT.CCB_SessionSetup(const DATA: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const error_txt: string);
begin
  case status of
    cdcs_OK: begin
      if FClientState = csSETUPSESSION then begin;
        GFRE_DBI.LogInfo(dblc_FLEXCOM,'SESSION SEUP OK : SESSION [%s]',[fMySessionID]);
        FClientState := csConnected;
        FApps        := data.Field('APPS').AsObject.CloneToNewObject();
        writeln('GOT APPS : ',FApps.DumpToString());
        MySessionEstablished;
      end else begin
        //Connection failed due to an intermediate failure
        writeln('CCB_SetupSession FAILED');
        exit;
      end;
    end;
    cdcs_TIMEOUT: ;
    cdcs_ERROR: begin
      writeln('Session Setup Error : ',error_txt);
      writeln(' ' ,FTimeout,' ',FClientState);
      FTimeout     := 5;
      FClientState := csTimeoutWait;
    end;
  end;
end;

function TFRE_BASE_CLIENT._GetClientHandler: IR_FRE_APS_FCOM_CLIENT_HANDLER;
begin
  //TFRE_CLIENT_BASE_CONNECTION
  result.ClientHandler      := @ConnectionHandler;
  result.InitClientSock     := @ConnectionInit;
  result.TearDownClientSock := @ConnectionTeardown;
end;

procedure TFRE_BASE_CLIENT.ConnectionInit(const SOCK: IFCOM_SOCK);
begin
  FConnectionLock.Acquire;
    FBaseconnection                         := TFRE_CLIENT_BASE_CONNECTION.Create(sock);
    sock.Data                               := FBaseconnection;
    FBaseconnection.OnConnectionStateChange := @MyConnectionStateChange;
    FBaseconnection.OnNewCommandAnswerHere  := @MyCommandAnswerArrived;
    FClientSock                             := Sock;
  FConnectionLock.Release;
end;

function TFRE_BASE_CLIENT.ConnectionHandler(const Event: EFOS_FCOM_MULTIEVENT; const SOCK: IFCOM_SOCK; const Datacount: Integer): boolean;
begin
  FConnectionLock.Acquire;
  try
    result := TFRE_CLIENT_BASE_CONNECTION(sock.Data).Handler(Event,Datacount);
  finally
    FConnectionLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.ConnectionTearDown(const Sock: IFCOM_SOCK);
begin
  MySessionDisconnected;
  FConnectionLock.Acquire;
   TFRE_CLIENT_BASE_CONNECTION(sock.Data).Finalize;
   FBaseconnection := nil;
   if FClientState=csConnected then begin
     FClientState := csUnknown;
   end;
   FClientSock := nil;
   FConnectionLock.Release;
end;

procedure TFRE_BASE_CLIENT.SendServerCommand(const base_connection: TFRE_CLIENT_BASE_CONNECTION; const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER; const timeout: integer);
var i       : integer;
    send_ok : boolean;
begin
  if assigned(ContinuationCB) then begin
    send_ok := false;
    FContinuationLock.Acquire;
      for i := 0 to high(FContinuationArray) do begin
        if FContinuationArray[i].CID=0 then begin
          FContinuationArray[i].CID        := fsendcmd_id;
          FContinuationArray[i].Contmethod := ContinuationCB;
          FContinuationArray[i].ExpiredAt  := GFRE_BT.Get_DBTimeNow+timeout;
          send_ok := true;
          break;
        end;
      end;
    FContinuationLock.Release;
  end else begin
    send_ok := true;
  end;
  if send_ok then begin
    base_connection.InvokeServerCommand(InvokeClass,InvokeMethod,uidpath,data,fsendcmd_id,ContinuationCB=nil);
    inc(fsendcmd_id);
  end else begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'TOO MUCH PENDING C-S Commands !');
  end;
end;

procedure TFRE_BASE_CLIENT.MyConnectionStateChange(sender: Tobject);
var hc             : TCONNECTION_STATE;
    base_connction : TFRE_CLIENT_BASE_CONNECTION;
    data           : IFRE_DB_Object;
    fuser          : string;
    fpass          : string;

begin
  base_connction := TFRE_CLIENT_BASE_CONNECTION(sender);
  hc := base_connction.ConnectionState;
  //writeln('state changed ',hc);
  case  hc of
    cls_TIMEOUT: begin;
                 end;
    cls_REFUSED: begin;
                   //writeln('CONNECTION REFUSED');
                   FTimeout     := 10;
                   FClientState := csTimeoutwait;
                 end;
    cls_ERROR: ;
    cls_CONNECTED: begin
                     FClientState := csSETUPSESSION;
                     data := GFRE_DBI.NewObject;
                     data.Field('SESSION_ID').AsString:=fMySessionID;
                     QueryUserPass(fuser,fpass);
                     if fuser<>'' then begin
                       data.Field('USER').AsString:=fuser;
                       data.Field('PASS').AsString:=fpass;
                     end;
                     SendServerCommand(base_connction,'FIRMOS','INIT',Nil,data,@CCB_SessionSetup,5000); // Pending Q
                   end;
    cls_OK:      begin
                 end;
    cls_CLOSED: ;
  end;
end;

procedure TFRE_BASE_CLIENT.MyStateCheckTimer(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var  me : EFOS_FCOM_MULTIERROR;
begin
  //writeln('STATECHECK ',FClientState,' ',FTimeout,' ',integer(Data),' ',cp,' ',TID);
  case cp of
    0: begin  // Timeout
          case FClientState of
            csUnknown: begin
                         //writeln('LAUNCHING NEW CLIENT');
                         FClientState:=csWaitConnect;
                         GFRE_S.AddSocketClient('*',44001,fil_IPV4,fsp_TCP,_GetClientHandler);
                       end;
            csWaitConnect: begin

                       end;
            csConnected: begin
                         end;
            csTimeoutWait:begin
                         dec(FTimeout);
                         if FTimeout<=0 then begin
                           FClientState:=csUnknown;
                         end;
                       end;
          end;
       end;
     1: begin // Answer Arrived
          DispatchAnswers;
        end;
     2: begin // Service Requests
          writeln('GOT REQUEST');
        end;
    end;
end;

procedure TFRE_BASE_CLIENT.MyCommandAnswerArrived(const sender: TObject; const cmd: IFRE_DB_COMMAND);
var i              : integer;
    answer_matched : boolean;
begin
  FContinuationLock.Acquire;
  try
    for i:=0 to high(FContinuationArray) do begin
      if cmd.CommandID = FContinuationArray[i].CID then begin
        FContinuationArray[i].Answer:= CMD;
        answer_matched := true;
        break;
      end;
    end;
  finally
    FContinuationLock.Release;
  end;
  if answer_matched then begin
    FCMD_Signal.FireEventManual(false);
  end else begin
    GFRE_LOG.Log('GOT ANSWER FOR UNKNOWN COMMAND CID=%d',[CMD.CommandID],catError);
    writeln('-->>>>>>>>>>   CHECK ---- ANSWER NOT MATCHED ',CMD.AsJSONString);
    CMD.Finalize;
  end;
end;

procedure TFRE_BASE_CLIENT.DispatchAnswers;
var i              : integer;
    answer_matched : boolean;
    answer         : IFRE_DB_COMMAND;
    contmethod     : TFRE_DB_CONT_HANDLER;
    status         : TFRE_DB_COMMAND_STATUS;
    data           : IFRE_DB_Object;
begin
  FContinuationLock.Acquire;
  try
    answer := nil;
    for i:=0 to high(FContinuationArray) do begin
      if assigned(FContinuationArray[i].Answer) then begin
        answer            := FContinuationArray[i].Answer;
        contmethod        := FContinuationArray[i].Contmethod;
        FContinuationArray[i].CID:=0;     // mark slot free
        FContinuationArray[i].Answer:=nil;
        FContinuationArray[i].Contmethod:=nil;
        FContinuationArray[i].ExpiredAt:=0;
        break;
      end;
    end;
  finally
    FContinuationLock.Release;
  end;
  if not assigned(answer) then begin
    writeln('**** STRANGE EVENT BUT NO ANSWERS ???');
  end else begin
    case answer.CommandType of
      fct_SyncRequest: GFRE_BT.CriticalAbort('LOGIC: ANSWER CANNOT BE A SYNC REQUEST');
      fct_SyncReply:   status := cdcs_OK;
      fct_Error:       status := cdcs_ERROR;
    end;
    if answer.ChangeSession<>'' then begin
      fMySessionID := answer.ChangeSession;
    end;
      data := answer.CheckoutData;
    try
      contmethod(data,status,answer.ErrorText);
    finally
      data.Finalize;
    end;
    answer.Finalize;
  end;
end;

function TFRE_BASE_CLIENT.GetName: String;
begin
  result := 'FRE BASE CLIENT'
end;

procedure TFRE_BASE_CLIENT.Terminate;
begin
  writeln('-QUIT-');
  GFRE_S.Quit;
end;

procedure TFRE_BASE_CLIENT.ReInit;
begin
  writeln('HUP');
end;

procedure TFRE_BASE_CLIENT.Finalize;
begin
  MyFinalize;
  if assigned(FClientSock) then begin
    FClientSock.Finalize;
  end;
  if assigned(FBaseconnection) then begin
    FBaseconnection.Finalize;
  end;
  if assigned(FApps) then begin
    FApps.Finalize;
  end;
  FContinuationLock.Finalize;
  FConnectionLock.Finalize;
  FCMD_Signal.FinalizeIt;
end;

procedure TFRE_BASE_CLIENT.Interrupt;
begin
  if G_NO_INTERRUPT_FLAG THEN exit;
  writeln('INTERRUPT');
  GFRE_S.Quit;
end;

procedure TFRE_BASE_CLIENT.Setup;
begin
  FClientState := csUnknown;
  fsendcmd_id  := 1;
  fMySessionID := 'NEW';
  GFRE_TF.Get_Lock(FContinuationLock);
  GFRE_TF.Get_Lock(FConnectionLock);
  MyInitialize;
  FCMD_Signal  := GFRE_S.AddPeriodicSignalTimer(1000,@MyStatecheckTimer);
end;

function TFRE_BASE_CLIENT.Get_AppClassAndUid(const appkey: string; out app_classname: TFRE_DB_String; out uid: TGuid): boolean;
begin
  if FApps.FieldExists(appkey) then begin
     app_classname := FApps.Field(appkey).AsObject.Field('CLASS').AsString;
     uid           := FApps.Field(appkey).AsObject.Field('UID').AsGUID;
     result        := true;
  end else begin
    result := false;
  end;
end;

procedure TFRE_BASE_CLIENT.MyInitialize;
begin

end;

procedure TFRE_BASE_CLIENT.MyFinalize;
begin

end;

procedure TFRE_BASE_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := '';
  pass := '';
end;

function TFRE_BASE_CLIENT.SendServerCommand(const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER; const timeout: integer): boolean;
begin
  FConnectionLock.Acquire; //TODO: ENQUEUE and send sync
  try
    if Assigned(FBaseconnection) then begin
      SendServerCommand(FBaseconnection,InvokeClass,InvokeMethod,uidpath,DATA,ContinuationCB,timeout);
      result := true;
    end else begin
      result := false;
    end;
  finally
    FConnectionLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.MySessionEstablished;
begin

end;

procedure TFRE_BASE_CLIENT.MySessionDisconnected;
begin

end;


end.

