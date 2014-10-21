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

//TODO: Expire pending commands

interface

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FRE_SYS_BASE_CS,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,
  FRE_SYSTEM,baseunix;

var G_CONNREFUSED_TO : integer = 5;
    G_RETRY_TO       : integer = 5;

type

  { TFRE_BASE_CLIENT }

  TFRE_BASE_CLIENT=class(TFRE_DB_Base)
  private
    type
      TBC_ClientState=(csUnknown,csWaitConnect,csConnected,csTimeoutWait,csSETUPSESSION);
      TDispatch_Continuation = record
        CID        : Qword;
        Contmethod : TFRE_DB_CONT_HANDLER;
        Answer     : IFRE_DB_COMMAND;
        ExpiredAt  : TFRE_DB_DateTime64;
      end;
      TSUB_FEED_CSTATE=(sfc_NOT_CONNECTED,sfc_TRYING,sfc_OK);
      TSUB_CMD_STATE  =(cs_READ_LEN,cs_READ_DATA);
      TSUB_FEED_STATE=class
        FId           : ShortString;
        FConnectState : TSUB_FEED_CSTATE;
        FCMDState     : TSUB_CMD_STATE;
        FLen          : Cardinal;
        FData         : Pointer;
        FSpecfile     : Shortstring;
        FIp           : Shortstring;
        FPort         : Shortstring;
      end;
    var
      FApps                    : IFRE_DB_Object;
      FTimeout                 : integer;
      fMySessionID             : String;
      fMyMachineUIDs           : TFRE_DB_GUIDArray;
      FClientState             : TBC_ClientState;
      FContinuationArray       : Array [0..cFRE_DB_MAX_PENDING_CLIENT_REQUESTS] of TDispatch_Continuation;
      FContinnuationCount      : Nativeint;
      FBaseconnection          : TFRE_CLIENT_BASE_CONNECTION;
      fsendcmd_id              : QWord;

      FChannel                 : IFRE_APSC_CHANNEL;
      FChannelTimer            : IFRE_APSC_TIMER;
      FClientStateLock         : IFOS_LOCK;
      FSubFeedLock             : IFOS_LOCK;
      FSubfeedlist             : TList;

    procedure  CCB_SessionSetup        (const DATA : IFRE_DB_Object ; const status:TFRE_DB_COMMAND_STATUS ; const error_txt:string);

    procedure  NewChannel              (const channel : IFRE_APSC_CHANNEL ; const event : TAPSC_ChannelState);
    procedure  ChannelDisco            (const channel : IFRE_APSC_CHANNEL);
    procedure  ChannelRead             (const channel : IFRE_APSC_CHANNEL);

    procedure SendServerCommand       (const base_connection: TFRE_CLIENT_BASE_CONNECTION; const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER=nil; const timeout: integer=5000);

    procedure MyStateCheckTimer       (const TIM : IFRE_APSC_TIMER ; const flag1,flag2 : boolean); // Timout & CMD Arrived & Answer Arrived
    procedure MySubFeederStateTimer   (const TIM : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
    procedure ChannelTimerCB          (const TIM : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);

    procedure MyCommandAnswerArrived  (const sender:TObject;const cmd:IFRE_DB_COMMAND);
    procedure MyRequestArrived        (const sender:TObject;const cmd:IFRE_DB_COMMAND);
    procedure DispatchAnswers         ;
    procedure MyHandleSignals         (const signal       : NativeUint);

    procedure   SubFeederNewSocket          (const channel  : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure   SubfeederReadClientChannel  (const channel  : IFRE_APSC_CHANNEL);
    procedure   SubfeederDiscoClientChannel (const channel  : IFRE_APSC_CHANNEL);

  protected
    procedure  Terminate; virtual;
    procedure  ReInit; virtual;
    procedure  User1; virtual;
    procedure  User2; virtual;
    procedure  Interrupt; virtual;
  public
    constructor Create                ;
    destructor  Destroy               ; override ;
    procedure   Setup                 ; virtual;
    procedure  MyRegisterClasses       ; virtual;
    function  Get_AppClassAndUid      (const appkey : string ; out app_classname : TFRE_DB_String ; out uid: TFRE_DB_Guid) : boolean;
    function  SendServerCommand       (const InvokeClass,InvokeMethod : String;const uidpath:TFRE_DB_GUIDArray;const DATA: IFRE_DB_Object;const ContinuationCB : TFRE_DB_CONT_HANDLER=nil;const timeout:integer=5000) : boolean;
    function  AnswerSyncCommand       (const command_id : QWord ; const data  : IFRE_DB_Object) : boolean;
    function  AnswerSyncError         (const command_id : QWord ; const error : TFRE_DB_String) : boolean;
    procedure MySessionEstablished    (const chanman : IFRE_APSC_CHANNEL_MANAGER); virtual;
    procedure MySessionDisconnected   (const chanman : IFRE_APSC_CHANNEL_MANAGER); virtual;
    procedure MySessionSetupFailed    (const reason : string); virtual;
    procedure MyInitialize            ; virtual;
    procedure MyFinalize              ; virtual;
    procedure MyConnectionTimer       ; virtual;
    procedure QueryUserPass           (out user,pass:string);virtual;
    procedure RegisterRemoteMethods   (var remote_method_array : TFRE_DB_RemoteReqSpecArray); virtual;
    procedure WorkRemoteMethods       (const rclassname,rmethodname : TFRE_DB_NameType ; const command_id : Qword ; const input : IFRE_DB_Object ; const cmd_type : TFRE_DB_COMMANDTYPE); virtual;
    function  GetCurrentChanManLocked : IFRE_APSC_CHANNEL_MANAGER;
    procedure UnlockCurrentChanMan    ;
    procedure AddSubFeederEventViaUX  (const special_id : Shortstring);
    procedure AddSubFeederEventViaTCP (const ip,port,special_id : Shortstring);
    procedure SubfeederEvent          (const id:string; const dbo:IFRE_DB_Object);virtual;
    function  GetMyMachineUIDs        : TFRE_DB_GUIDArray;
  end;


implementation

{ TFRE_BASE_CLIENT }

procedure TFRE_BASE_CLIENT.CCB_SessionSetup(const DATA: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const error_txt: string);
var arr     : TFRE_DB_RemoteReqSpecArray;
    i       : NativeInt;
    regdata : IFRE_DB_Object;
    fmuid   : IFRE_DB_Field;
begin
  FClientStateLock.Acquire;
  try
    case status of
      cdcs_OK: begin
        if FClientState = csSETUPSESSION then begin
          if (data.FieldExists('LOGIN_OK') and (data.Field('LOGIN_OK').AsBoolean=true)) then
            begin
              if data.FieldOnlyExisting('MACHINE_UID',fmuid) then
                fMyMachineUIDs := fmuid.AsGUIDArr;
              GFRE_DBI.LogInfo(dblc_FLEXCOM,'SESSION SETUP OK : SESSION [%s] MACHINE [%s/%s]',[fMySessionID,cFRE_MACHINE_NAME,FREDB_GuidArray2StringStream(fMyMachineUIDs)]);
              FClientState := csConnected;
              FApps        := data.Field('APPS').AsObject.CloneToNewObject();
              writeln('GOT APPS : ',FApps.DumpToString());
              RegisterRemoteMethods(arr);
              if Length(arr)>0 then
                begin
                  regdata := GFRE_DBI.NewObject;
                  regdata.Field('mc').AsUInt16 := Length(arr);
                  for i := 0 to length(arr)-1 do
                    begin
                      regdata.Field('cl'+IntToStr(i)).AsString := arr[i].classname;
                      regdata.Field('mn'+IntToStr(i)).AsString := arr[i].methodname;
                      regdata.Field('ir'+IntToStr(i)).AsString := arr[i].invokationright;
                    end;
                  SendServerCommand('FIRMOS','REG_REM_METH',nil,regdata);
                end;
              MySessionEstablished(FChannel.GetChannelManager);
            end
          else
            begin
              GFRE_DBI.LogInfo(dblc_FLEXCOM,'SESSION SETUP FAILED : SESSION [%s]',[fMySessionID]);
              if assigned(FApps) then
                try
                  FApps.Finalize;
                except
                  writeln('*** APP FINALIZE EXCEPTION');
                end;
              FApps:=nil;
              FTimeout     := G_RETRY_TO;
              FClientState := csTimeoutWait;
              fMySessionID := 'NEW';
              FChannel.Finalize;
              FChannel:=nil;
              try
                writeln('SETUP FAIL',data.DumpToString());
                MySessionSetupFailed(data.Field('LOGIN_TXT').AsString);
              except
              end;
            end;
        end else begin
          //Connection failed due to an intermediate failure
          writeln('CCB_SetupSession FAILED ????');
          FClientState := csTimeoutWait;
          fMySessionID := 'NEW';
          FChannel.Finalize;
          FChannel:=nil;
          exit;
        end;
      end;
      cdcs_TIMEOUT: ;
      cdcs_ERROR: begin
        writeln('Session Setup Error - Server Returned Error: ',error_txt);
        writeln(' ' ,FTimeout,' ',FClientState);
        FTimeout     := G_RETRY_TO;
        FClientState := csTimeoutWait;
        fMySessionID := 'NEW';
        FClientState := csTimeoutWait;
        FChannel:=nil;
      end;
    end;
  finally
    FClientStateLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.NewChannel(const channel: IFRE_APSC_CHANNEL; const event: TAPSC_ChannelState);
var data           : IFRE_DB_Object;
    fuser          : string;
    fpass          : string;

begin
  FClientStateLock.Acquire;
  try
    writeln('GOT A NEW CHANNEL ON CM_',channel.GetChannelManager.GetID,' ',channel.GetVerboseDesc,' ',event,' ',channel.GetHandleKey);
    if assigned(FChannel) then
      GFRE_BT.CriticalAbort('I SHOULD NOT HAVE A CHANNEL HERE (B)!');
    case event of
      ch_NEW_CS_CONNECTED:
        begin
          FClientState    := csSETUPSESSION;
          FBaseconnection := TFRE_CLIENT_BASE_CONNECTION.Create(Channel);
          FBaseconnection.OnNewCommandAnswerHere  := @MyCommandAnswerArrived;
          FBaseconnection.OnNewServerRequest      := @MyRequestArrived;
          data := GFRE_DBI.NewObject;
          data.Field('SESSION_ID').AsString:=fMySessionID;
          data.Field('MACHINENAME').AsString:=cFRE_MACHINE_NAME;
          data.Field('MACHINEMAC').AsString:=cFRE_MACHINE_MAC;
          QueryUserPass(fuser,fpass);
          if fuser<>'' then begin
            data.Field('USER').AsString:=fuser;
            data.Field('PASS').AsString:=fpass;
          end;
          FChannel := channel;
          SendServerCommand(FBaseconnection,'FIRMOS','INIT',Nil,data,@CCB_SessionSetup,5000); // Pending Q
          channel.CH_Enable_Reading;
        end;
      ch_NEW_CHANNEL_FAILED:
        begin
          writeln('NEW CHANNEL ',channel.GetVerboseDesc,' FAILED ',channel.CH_GetErrorString,' ',channel.CH_GetErrorCode);
          channel.Finalize;
        end
      else
        GFRE_BT.CriticalAbort('unexpected newchannel event' +inttostr(ord(event)));
    end;
  finally
    FClientStateLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.ChannelDisco(const channel: IFRE_APSC_CHANNEL);
begin
 FClientStateLock.Acquire;
 try
    //writeln('CHANNEL ',channel.GetVerboseDesc,' DISCONNECT CM_' ,channel.GetChannelManager.GetID);
    if channel.CH_GetState<>ch_EOF then
      begin
        FTimeout     := G_CONNREFUSED_TO;
        if channel.CH_GetErrorCode=ESysECONNREFUSED then
          FClientState := csTimeoutwait;
      end;
    if FClientState=csConnected then
      MySessionDisconnected(FChannel.GetChannelManager);
    try
      if assigned(FBaseconnection) then
        begin
           FBaseconnection.Finalize;
           FBaseconnection := nil;
        end;
      if FClientState=csConnected then
        FClientState := csUnknown
      else
        FClientState := csTimeoutWait
    finally
      FChannel := nil;
    end;
  finally
    FClientStateLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.ChannelRead(const channel: IFRE_APSC_CHANNEL);
begin
  FBaseconnection.Handler(channel,channel.CH_GetDataCount);
end;

procedure TFRE_BASE_CLIENT.SendServerCommand(const base_connection: TFRE_CLIENT_BASE_CONNECTION; const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER; const timeout: integer);
var i       : integer;
    send_ok : boolean;
begin
  if assigned(ContinuationCB) then begin
    send_ok := false;
      for i := 0 to high(FContinuationArray) do begin
        if FContinuationArray[i].CID=0 then begin
          FContinuationArray[i].CID        := fsendcmd_id;
          FContinuationArray[i].Contmethod := ContinuationCB;
          FContinuationArray[i].ExpiredAt  := GFRE_BT.Get_DBTimeNow+timeout;
          send_ok := true;
          break;
        end;
      end;
  end else begin
    send_ok := true;
  end;
  if send_ok then begin
    base_connection.InvokeServerCommand(InvokeClass,InvokeMethod,uidpath,data,fsendcmd_id,ContinuationCB=nil);
    inc(fsendcmd_id);
  end else begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'BASE/CLIENT TOO MUCH PENDING C-S Commands !');
  end;
end;

procedure TFRE_BASE_CLIENT.MyStateCheckTimer(const TIM: IFRE_APSC_TIMER; const flag1, flag2: boolean);
begin
  FClientStateLock.Acquire;
  try
    if (flag1=false) and (flag2=false) then
      begin
        case FClientState of
          csUnknown: begin
                       FClientState:=csWaitConnect;
                       if Assigned(FChannel) then
                         GFRE_BT.CriticalAbort('SHOULD NOT HAVE A CHANNEL HERE!');
                       if cFRE_MWS_IP<>'' then
                         GFRE_SC.AddClient_TCP(cFRE_MWS_IP,'44001','FEED',nil,@NewChannel,@ChannelRead,@ChannelDisco)
                       else
                         GFRE_SC.AddClient_TCP('0.0.0.0','44001','FEED',nil,@NewChannel,@ChannelRead,@ChannelDisco)
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
  finally
    FClientStateLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.MySubFeederStateTimer(const TIM: IFRE_APSC_TIMER; const flag1, flag2: boolean);
var i    : NativeInt;
    subs : TSUB_FEED_STATE;
begin
  FSubFeedLock.Acquire;
  try
    for i := 0 to FSubfeedlist.Count-1 do
      begin
        subs := TSUB_FEED_STATE(FSubfeedlist[i]);
        case subs.FConnectState of
          sfc_NOT_CONNECTED:
            begin // Start a client
              subs.FConnectState:=sfc_TRYING;
              if (subs.FSpecfile<>'') then
                begin
                  if FileExists(subs.FSpecfile) then
                    GFRE_SC.AddClient_UX(subs.FSpecfile,inttostr(i),nil,@SubFeederNewSocket,@SubfeederReadClientChannel,@SubfeederDiscoClientChannel)
                end
              else
                GFRE_SC.AddClient_TCP(subs.FIp,subs.FPort,inttostr(i),nil,@SubFeederNewSocket,@SubfeederReadClientChannel,@SubfeederDiscoClientChannel);
            end;
          sfc_TRYING: ; // do nothing
          sfc_OK: ; // do nothing
        end;
      end;
  finally
    FSubFeedLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.ChannelTimerCB(const TIM: IFRE_APSC_TIMER; const flag1, flag2: boolean);
begin
  MyConnectionTimer;
end;


procedure TFRE_BASE_CLIENT.MyCommandAnswerArrived(const sender: TObject; const cmd: IFRE_DB_COMMAND);
var i              : integer;
    answer_matched : boolean;
begin
  for i:=0 to high(FContinuationArray) do begin
    if cmd.CommandID = FContinuationArray[i].CID then begin
      FContinuationArray[i].Answer:= CMD;
      answer_matched := true;
      break;
    end;
  end;
  if answer_matched then begin
    DispatchAnswers;
  end else begin
    GFRE_LOG.Log('GOT ANSWER FOR UNKNOWN COMMAND CID=%d',[CMD.CommandID],catError);
    CMD.Finalize;
  end;
end;

procedure TFRE_BASE_CLIENT.MyRequestArrived(const sender: TObject; const cmd: IFRE_DB_COMMAND);
begin
  try
    WorkRemoteMethods(cmd.InvokeClass,cmd.InvokeMethod,cmd.CommandID,cmd.CheckoutData,cmd.CommandType);
  finally
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

procedure TFRE_BASE_CLIENT.MyHandleSignals(const signal: NativeUint);
begin
  case signal of
    SIGHUP  : ReInit;
    SIGTERM : Terminate;
    SIGINT  : Interrupt;
    SIGUSR1 : User1;
    SIGUSR2 : USer2;
  end;
end;

procedure TFRE_BASE_CLIENT.MyRegisterClasses;
begin

end;

procedure TFRE_BASE_CLIENT.SubFeederNewSocket(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
var subs : TSUB_FEED_STATE;
    id   : NativeInt;
begin
  if channel_event=ch_NEW_CS_CONNECTED then
    begin
      FSubFeedLock.Acquire;
      try
        id   := StrToInt(channel.CH_GetID);
        subs := TSUB_FEED_STATE(FSubfeedlist[id]);
        subs.FConnectState := sfc_OK;
        subs.FCMDState     := cs_READ_LEN;
        channel.CH_Enable_Reading;
      finally
        FSubFeedLock.Release;
      end;
    end
  else
    begin
      channel.Finalize;
    end;
end;

procedure TFRE_BASE_CLIENT.SubfeederReadClientChannel(const channel: IFRE_APSC_CHANNEL);
var subs  : TSUB_FEED_STATE;
    id    : NativeInt;
    len   : cardinal;
    fcont : boolean;
    dbo   : IFRE_DB_Object;
begin
  FSubFeedLock.Acquire;
  try
    id   := StrToInt(channel.CH_GetID);
    subs := TSUB_FEED_STATE(FSubfeedlist[id]);
    repeat
      fcont := false;
      case subs.FCMDState of
        cs_READ_LEN:
          begin
            if channel.CH_GetDataCount>=4 then
              begin
                channel.CH_ReadBuffer(@subs.FLen,4);
                fcont := true;
                getmem(subs.FData,subs.FLen);
                subs.FCMDState:=cs_READ_DATA;
              end;
          end;
        cs_READ_DATA:
          begin
            if channel.CH_GetDataCount>=subs.FLen then
              begin
                channel.CH_ReadBuffer(subs.FData,subs.FLen);
                fcont := true;
                try
                  try
                    dbo := GFRE_DBI.CreateFromMemory(subs.FData);
                    try
                      SubfeederEvent(subs.FId,dbo);
                    except on e:exception do
                      begin
                        writeln('Failure in Subfeeder event processing '+e.Message);
                      end;
                    end;
                  finally
                    Freemem(subs.FData);
                    subs.FData:=nil;
                    if assigned(dbo) then
                      dbo.Finalize;
                  end;
                except on e:exception do
                  begin
                    writeln('SUB CHANNEL READ FAILED ',e.Message);
                    channel.Finalize;
                    subs.FConnectState := sfc_NOT_CONNECTED;
                  end;
                end;
                subs.FCMDState := cs_READ_LEN;
              end;
          end;
      end;
    until fcont=false;
  finally
    FSubFeedLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.SubfeederDiscoClientChannel(const channel: IFRE_APSC_CHANNEL);
var subs : TSUB_FEED_STATE;
    id   : NativeInt;
begin
  FSubFeedLock.Acquire;
  try
    id   := StrToInt(channel.CH_GetID);
    subs := TSUB_FEED_STATE(FSubfeedlist[id]);
    if subs.FCMDState=cs_READ_DATA then
      begin
        if assigned(subs.FData) then
          begin
            writeln('****************** SUBSUBFEEDER : Disconnected while reading data!');
            Freemem(subs.FData);
          end;
        subs.FData := nil;
      end;
    subs.FConnectState := sfc_NOT_CONNECTED;
  finally
    FSubFeedLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.Terminate;
begin
  writeln('SIGNAL TERMINATE');
  GFRE_SC.RequestTerminate;
end;

procedure TFRE_BASE_CLIENT.ReInit;
begin
  writeln('SIGNAL HANGUP');
end;

procedure TFRE_BASE_CLIENT.User1;
begin
  writeln('SIGNAL USER1');
end;

procedure TFRE_BASE_CLIENT.User2;
begin
  writeln('SIGNAL USER2');
end;


procedure TFRE_BASE_CLIENT.Interrupt;
begin
  if G_NO_INTERRUPT_FLAG THEN
    exit;
  writeln('INTERRUPT');
  GFRE_SC.RequestTerminate;
end;

constructor TFRE_BASE_CLIENT.Create;
begin
  inherited;
  FClientState := csUnknown;
  fsendcmd_id  := 1;
  fMySessionID := 'NEW';
  FChannel     := nil;
  GFRE_TF.Get_Lock(FClientStateLock);
  GFRE_TF.Get_Lock(FSubFeedLock);
  FSubfeedlist:=TList.Create;
end;

destructor TFRE_BASE_CLIENT.Destroy;
var  i: NativeInt;
begin
  MyFinalize;
  if assigned(FBaseconnection) then begin
    FBaseconnection.Finalize;
  end;
  if assigned(FApps) then begin
    FApps.Finalize;
  end;
  FClientStateLock.Finalize;
  FSubFeedLock.Finalize;
  for i:=0 to FSubfeedlist.Count-1 do
    begin
      TSUB_FEED_STATE(FSubfeedlist[i]).free;
    end;
  FSubfeedlist.Free;
  inherited Destroy;
end;

procedure TFRE_BASE_CLIENT.Setup;
begin
  if cFRE_MACHINE_NAME='' then
    GFRE_BT.CriticalAbort('no MACHINE NAME set / NAME entry missing in subsection [MACHINE] in .ini File / or startparameter --machine=<> missing ');
  if cFRE_MACHINE_MAC='' then
    GFRE_BT.CriticalAbort('no MACHINE MAC set / MAC entry missing in subsection [MACHINE] in .ini File / or startparameter --mac=<> missing ');
  if not FREDB_CheckMacAddress(cFRE_MACHINE_MAC) then
    GFRE_BT.CriticalAbort('mac address format invalid use a contiguos hexstring or a colon seperated string invalid [%s]',[cFRE_MACHINE_MAC]);
  GFRE_SC.AddTimer('F_STATE',1000,@MyStateCheckTimer);
  GFRE_SC.AddTimer('F_SUB_STATE',1000,@MySubFeederStateTimer);
  GFRE_SC.SetSingnalCB(@MyHandleSignals);
  MyInitialize;
end;

function TFRE_BASE_CLIENT.Get_AppClassAndUid(const appkey: string; out app_classname: TFRE_DB_String; out uid: TFRE_DB_Guid): boolean;
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

procedure TFRE_BASE_CLIENT.MyConnectionTimer;
begin

end;

procedure TFRE_BASE_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := '';
  pass := '';
end;

procedure TFRE_BASE_CLIENT.RegisterRemoteMethods(var remote_method_array: TFRE_DB_RemoteReqSpecArray);
var rem_methods : TFRE_DB_StringArray;
    i           : nativeInt;
begin
  rem_methods := Get_DBI_RemoteMethods;
  SetLength(remote_method_array,Length(rem_methods));
  for i:=0 to high(rem_methods) do
    begin
      remote_method_array[i].classname       := uppercase(ClassName);
      remote_method_array[i].methodname      := UpperCase(rem_methods[i]);
      remote_method_array[i].invokationright := '$REMIC_'+remote_method_array[i].classname+'.'+remote_method_array[i].methodname;
    end;
end;

procedure TFRE_BASE_CLIENT.WorkRemoteMethods(const rclassname, rmethodname: TFRE_DB_NameType; const command_id: Qword; const input: IFRE_DB_Object; const cmd_type: TFRE_DB_COMMANDTYPE);
begin
  try
    if uppercase(rclassname)=uppercase(ClassName) then
      Invoke_DBREM_Method(rmethodname,command_id,input,cmd_type)
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'Classname mismatch Mine:[%s] <> Requested:[%s] Method: [%s] CommandType [%s]',[ClassName,rclassname,rmethodname, CFRE_DB_COMMANDTYPE[cmd_type]]);
  except on e:exception
    do
      begin
        AnswerSyncError(command_id,e.Message);
        input.Finalize;
      end;
  end;
end;

function TFRE_BASE_CLIENT.GetCurrentChanManLocked: IFRE_APSC_CHANNEL_MANAGER;
begin
  FClientStateLock.Acquire;
  result := FChannel.GetChannelManager;
end;

procedure TFRE_BASE_CLIENT.UnlockCurrentChanMan;
begin
  FClientStateLock.Release;
end;

procedure TFRE_BASE_CLIENT.AddSubFeederEventViaUX(const special_id: Shortstring);
var sub_state : TSUB_FEED_STATE;
begin
  FSubFeedLock.Acquire;
  try
    sub_state := TSUB_FEED_STATE.Create;
    sub_state.FConnectState := sfc_NOT_CONNECTED;
    sub_state.FId           := special_id;
    sub_state.FSpecfile     := cFRE_UX_SOCKS_DIR+special_id;
    FSubfeedlist.Add(sub_state);
  finally
    FSubFeedLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.AddSubFeederEventViaTCP(const ip, port, special_id: Shortstring);
var sub_state : TSUB_FEED_STATE;
begin
  FSubFeedLock.Acquire;
  try
    sub_state := TSUB_FEED_STATE.Create;
    sub_state.FConnectState := sfc_NOT_CONNECTED;
    sub_state.FId           := special_id;
    sub_state.FSpecfile     := '';
    sub_state.FPort         := port;
    sub_state.FIp           := ip;
    FSubfeedlist.Add(sub_state);
  finally
    FSubFeedLock.Release;
  end;
end;

procedure TFRE_BASE_CLIENT.SubfeederEvent(const id: string; const dbo: IFRE_DB_Object);
begin

end;

function TFRE_BASE_CLIENT.GetMyMachineUIDs: TFRE_DB_GUIDArray;
begin
  result := fMyMachineUIDs;
end;

function TFRE_BASE_CLIENT.SendServerCommand(const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const ContinuationCB: TFRE_DB_CONT_HANDLER; const timeout: integer): boolean;
begin
  if Assigned(FBaseconnection) then begin
    SendServerCommand(FBaseconnection,InvokeClass,InvokeMethod,uidpath,DATA,ContinuationCB,timeout);
    result := true;
  end else begin
    result := false;
  end;
end;

function TFRE_BASE_CLIENT.AnswerSyncCommand(const command_id: QWord; const data: IFRE_DB_Object):boolean;
begin
  if assigned(FBaseconnection) then
    begin
      FBaseconnection.SendSyncAnswer(command_id,data);
      result := true;
    end
  else
    result := false;
end;

function TFRE_BASE_CLIENT.AnswerSyncError(const command_id: QWord; const error: TFRE_DB_String): boolean;
begin
  if assigned(FBaseconnection) then
    begin
      FBaseconnection.SendSyncErrorAnswer(command_id,error);
      result := true;
    end
  else
    result := false;
end;

procedure TFRE_BASE_CLIENT.MySessionEstablished(const chanman: IFRE_APSC_CHANNEL_MANAGER);
begin
  FChannelTimer := chanman.AddTimer(1000);
  FChannelTimer.TIM_SetID('CT');
  FChannelTimer.TIM_SetCallback(@ChannelTimerCB);
  FChannelTimer.TIM_Start;
end;

procedure TFRE_BASE_CLIENT.MySessionDisconnected(const chanman: IFRE_APSC_CHANNEL_MANAGER);
begin
  if assigned(FChannelTimer) then
    begin
      FChannelTimer.Finalize;
      FChannelTimer := nil;
    end;
end;

procedure TFRE_BASE_CLIENT.MySessionSetupFailed(const reason: string);
begin
  writeln('SETUP FAILED REASON : ',reason);
end;


end.

