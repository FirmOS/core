unit fre_sys_base_cs;

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
{$interfaces CORBA}

interface

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FOS_INTERLOCKED;

const cCMD_MAXSize                        = 512*1024;
      cFRE_DB_MAX_PENDING_CLIENT_REQUESTS = 10;

var fSession_Counter : Integer=0;

type TFRE_FCOM_PROTO            = (cfp_TLS); //UDP,TLS,SSH,CLUSTER ...
     TFRE_CMD_TYPE              = (cfc_LOGIN,cfc_CMD,cfc_ERROR,cfc_OK);
     TFRE_CMD_ERROR_TYPE        = (cce_INVALID_PROTOVERSION,cce_LOGIN_WRONG);

     TINTERNAL_CONNECTION_STATE = (cs_TRYCONNECT,cs_READY,cs_CONTCMD,cs_LOGINFAIL,cs_TIMEOUT,cs_WAITCMD);
     //TCONNECTION_STATE          = (cls_TIMEOUT,cls_REFUSED,cls_ERROR,cls_CONNECTED,cls_OK,cls_CLOSED);

     TSERVED_CONNECTION_STATE   = (ss_CONNECTED,ss_CONTCMD,ss_READY,ss_WAITCMD);
     TCMD_READ_STATE            = (crs_OK,crs_WAIT_CMD_SZ,crs_PARTIAL_READ,crs_COMMAND_TOO_LONG,crs_FAULT);



  { TFRE_DB_SOCK_CONNECTION }


  TFRE_DB_SOCK_CONNECTION=class(TObject)
  protected
    readm              : TMemoryStream;
    sizehdr_pos        : cardinal;
    fCMD_SIZE          : QWord;
    readrest           : cardinal;
    FCHANNEL           :IFRE_APSC_CHANNEL;
    function           _ReadHeader  (var data_count:NativeInt):TCMD_READ_STATE;
    function           _ReadCommand (var data_count:NativeInt;out cmd:IFRE_DB_COMMAND):TCMD_READ_STATE;
    function           _ReadRest    (dc:cardinal;out cmd:IFRE_DB_COMMAND):TCMD_READ_STATE;
  public
    SessionID   :String;
    constructor Create;
    destructor  Destroy;override;
    procedure   SetChannel          (const channel : IFRE_APSC_CHANNEL);
    procedure   HandleError         (const err:String);
    Function    GetSessionID        :String;
  end;

  { TFRE_CLIENT_BASE_CONNECTION }
  TFRE_CLIENT_BASE_CONNECTION=class(TFRE_DB_SOCK_CONNECTION)
  private
    var
      FOnNewCommandAnswerHere  : TFRE_DB_CMD_Request;
      FOnNewServerRequest      : TFRE_DB_CMD_Request;
      state                    : TINTERNAL_CONNECTION_STATE;
      Ferror                   : string;

    procedure   SetOnNewCommandAnswerHere  (AValue: TFRE_DB_CMD_Request);
    procedure   SetOnNewServerRequest      (AValue: TFRE_DB_CMD_Request);
  protected
  public
    READ_CMD    : IFRE_DB_Object;
    procedure   InvokeServerCommand (const InvokeClass,InvokeMethod : String;const uidpath:TFRE_DB_GUIDArray;const DATA: IFRE_DB_Object;const CID : Qword;const async:boolean);
    procedure   SendSyncAnswer      (const CID : Qword ; const data  : IFRE_DB_Object);
    procedure   SendSyncErrorAnswer (const CID : Qword ; const error : TFRE_DB_String);
    constructor Create              (const channel:IFRE_APSC_CHANNEL);
    destructor  Destroy             ;override;
    function    Handler             (const channel : IFRE_APSC_CHANNEL ;const Datacount:Integer):boolean;
    procedure   Test;
    procedure   Finalize;
    property    OnNewCommandAnswerHere  : TFRE_DB_CMD_Request read FOnNewCommandAnswerHere write SetOnNewCommandAnswerHere; // Not Threadsafe
    property    OnNewServerRequest      : TFRE_DB_CMD_Request read FOnNewServerRequest write SetOnNewServerRequest; //
  end;


  { TFRE_SERVED_BASE_CONNECTION }
  //TFRE_BASE_S=class;

  TFRE_SERVED_BASE_CONNECTION=class(TFRE_DB_SOCK_CONNECTION,IFRE_DB_COMMAND_REQUEST_ANSWER_SC)
  private
    FUserSession          : TFRE_DB_UserSession;
    FOnBindDefaultSession : TFRE_DB_FetchSessionCB;
    FOnUnbindSession      : TFRE_DB_SessionCB;
    state                 : TSERVED_CONNECTION_STATE;
    procedure SetOnBindDefaultSession(AValue: TFRE_DB_FetchSessionCB);
    procedure SetOnUnbindSession(AValue: TFRE_DB_SessionCB);
  protected
    procedure   Finalize;
    function    Implementor:TObject;
    function    Implementor_HC:TObject;
    function    GetInfoForSessionDesc     : String;
  public
    procedure   ReadChannelData           (const channel:IFRE_APSC_CHANNEL);
    procedure   DisconnectChannel         (const channel:IFRE_APSC_CHANNEL);
    constructor Create                    ;
    destructor  Destroy                   ;override;
    procedure   Send_ServerClient         (const ECN:IFRE_DB_COMMAND);
    property    OnBindInitialSession      : TFRE_DB_FetchSessionCB read FOnBindDefaultSession write SetOnBindDefaultSession;
    property    OnUnbindSession           : TFRE_DB_SessionCB read FOnUnbindSession write SetOnUnbindSession;
    procedure   DeactivateSessionBinding  (const from_session : boolean=false);
    procedure   UpdateSessionBinding      (const new_session : TObject);
    function    GetChannel                : IFRE_APSC_CHANNEL;
  end;

  TFRE_CONNECTION_CLASS=class of TFRE_DB_SOCK_CONNECTION;


implementation

{ TFRE_CLIENT_BASE_CONNECTION }

procedure TFRE_CLIENT_BASE_CONNECTION.SetOnNewCommandAnswerHere(AValue: TFRE_DB_CMD_Request);
begin
  FOnNewCommandAnswerHere:=AValue;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.SetOnNewServerRequest(AValue: TFRE_DB_CMD_Request);
begin
  FOnNewServerRequest:=AValue;
end;

//function TFRE_CLIENT_BASE_CONNECTION._ReadACommand(const cmd: IFRE_DB_COMMAND):boolean;
//var  c    : IFRE_DB_COMMAND;
//     CID  : Qword;
//begin
//  cid    := Cmd.CommandID;
//  result := cmd.Answer;
//  if Result then begin
//    READ_CMD  := cmd.GetData;
//    //con_state := cls_OK;eventcondition.ConditionHasChanged;
//  end else begin
//    //CLIENT.Command(cid,cmd.Data);
//  end;
//  cmd.StripOwnedObjects;
//  cmd.finalize;
//end;

procedure TFRE_CLIENT_BASE_CONNECTION.InvokeServerCommand(const InvokeClass, InvokeMethod: String; const uidpath: TFRE_DB_GUIDArray; const DATA: IFRE_DB_Object; const CID: Qword; const async: boolean);
var ose : EFOS_OS_ERROR;
    mem : TMemoryStream;
    ECN : IFRE_DB_COMMAND;
    i   : NativeInt;
begin
  try
    mem:=TMemoryStream.Create;
    mem.Position:=4;
    ECN := GFRE_DBI.NewDBCommand;
    if async then begin
      ECN.CommandType  := fct_AsyncRequest;
    end else begin
      ECN.CommandType  := fct_SyncRequest;
    end;
    ECN.InvokeClass  := InvokeClass;
    ECN.InvokeMethod := InvokeMethod;
    ECN.Answer       := false;
    ECN.CommandID    := CID;
    ECN.UidPath      := uidpath ;
    ECN.Data         := Data;
    mem.Size:=ECN.NeededSize+sizeof(QWord);
    ECN.CopyToMemory(mem.Memory+sizeof(QWord));
    PQWord(mem.Memory)^:=mem.Size-sizeof(qword);
    mem.Position:=0;
    FCHANNEL.CH_WriteBuffer(mem.Memory,mem.Size);
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.SendSyncAnswer(const CID: Qword; const data: IFRE_DB_Object);
var mem : TMemoryStream;
    ECN : IFRE_DB_COMMAND;
begin
  try
    mem:=TMemoryStream.Create;
    mem.Position:=4;
    ECN := GFRE_DBI.NewDBCommand;
    ECN.CommandType   := fct_SyncReply;
    ECN.Answer        := true;
    ECN.CommandID     := CID;
    ECN.Data          := Data;
    ECN.ClientCommand := true;
    mem.Size:=ECN.NeededSize+sizeof(QWord);
    ECN.CopyToMemory(mem.Memory+sizeof(QWord));
    PQWord(mem.Memory)^:=mem.Size-sizeof(qword);
    mem.Position:=0;
    FCHANNEL.CH_WriteBuffer(mem.Memory,mem.Size);
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.SendSyncErrorAnswer(const CID: Qword; const error: TFRE_DB_String);
var mem : TMemoryStream;
    ECN : IFRE_DB_COMMAND;
begin
  try
    mem:=TMemoryStream.Create;
    mem.Position:=4;
    ECN := GFRE_DBI.NewDBCommand;
    ECN.CommandType   := fct_Error;
    ECN.Answer        := true;
    ECN.CommandID     := CID;
    ECN.Data          := nil;
    ECN.ErrorText     := error;
    ECN.ClientCommand := true;
    mem.Size:=ECN.NeededSize+sizeof(QWord);
    ECN.CopyToMemory(mem.Memory+sizeof(QWord));
    PQWord(mem.Memory)^:=mem.Size-sizeof(qword);
    mem.Position:=0;
    FCHANNEL.CH_WriteBuffer(mem.Memory,mem.Size);
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

constructor TFRE_CLIENT_BASE_CONNECTION.Create(const channel: IFRE_APSC_CHANNEL);
begin
  inherited Create;
  state       := cs_READY;
  SessionID   := '-';
  FCHANNEL    := channel;
end;

destructor TFRE_CLIENT_BASE_CONNECTION.Destroy;
begin
  //WriteLn('!CLIENT BASE CONNECTION FREE!');
  inherited;
end;


function TFRE_CLIENT_BASE_CONNECTION.Handler(const channel: IFRE_APSC_CHANNEL; const Datacount: Integer): boolean;
var s           : String;
    sr          : NativeInt;
    sw          : NativeInt;
    dispatch_it : boolean;
    cmd         : IFRE_DB_COMMAND;
    readres     : TCMD_READ_STATE;
    myDatacount : NativeInt;

    procedure _Dispatch;
    begin
      try
        state          := cs_READY;
        fCMD_SIZE      := 0;
        sizehdr_pos    := 0;
        if cmd.Answer then begin
          FOnNewCommandAnswerHere(self,cmd);
        end else begin // Request
          FOnNewServerRequest(self,cmd);
        end;
      except on e:exception do begin
        writeln('DISPATCHING ERROR ',e.Message);
      end;end;
    end;

    procedure _Read_DB_Command;
    begin
      case _ReadCommand(myDataCount,cmd) of
        crs_OK: begin
          _Dispatch;
        end;
        crs_PARTIAL_READ: begin
          state:=cs_CONTCMD;
          exit;
        end;
        else begin
          GFRE_BT.CriticalAbort('FATAL COMMUNICATION ERROR : GOING DOWN');
          FCHANNEL.Finalize;
        end;
      end;
    end;

begin
//  writeln(GFRE_S.CURRENT_THREAD,' ENTER CONN HANDLER <',Event,' in ',state,':',con_state,' >');
//  try
  //result := true;
  //myDatacount := Datacount;
  //case event of
  //  esv_SOCKERROR: begin
  //    con_state:=cls_ERROR;
  //    HandleError(SOCK,'received error state');
  //  end;
  //  esv_SOCKCONNECTED: begin
  //    case state of
  //      cs_TRYCONNECT: begin
  //        state:=cs_READY;
  //        con_state:=cls_CONNECTED;//eventcondition.ConditionHasChanged;
  //        _Statechanged;
  //        exit;
  //      end;
  //      else HandleError(SOCK,'invalid state for SOCKCONNECTED event');
  //    end;
  //  end;
  //  esv_SOCKREAD: begin
      myDatacount:=Datacount;
      case state of
        cs_READY: begin
          case _ReadHeader(myDatacount) of
            crs_OK:          ;
            crs_WAIT_CMD_SZ: exit;
            crs_FAULT:       channel.Finalize;
          end;
          state := cs_WAITCMD;
          if myDataCount>0 then begin
            _Read_DB_Command;
          end;
          exit;
        end;
        cs_WAITCMD: _Read_DB_Command;
        cs_CONTCMD: begin
          //writeln('->>>>>>>>> SERVER CONTiNuATION READ ');
          case _ReadRest(myDatacount,cmd) of
            crs_OK: begin
              state:=cs_READY;
              _Dispatch;
              exit;
            end;
            crs_PARTIAL_READ: exit; // Stay in state
          end;
        end;
        else HandleError('Read Failed');
      end;
  //  end;
  //  esv_SOCKWRITE: begin
  //      HandleError(SOCK,'invalid state for SOCKWRITE event');
  //  end;
  //  esv_SOCKCLOSED: begin
  //      con_state:=cls_CLOSED;
  //      case state of
  //        cs_READY: begin
  //          writeln('<>CLIENT DONE');
  //          _Statechanged;
  //          //SOCK.UserData:=nil;
  //          //writeln('<>CLIENT DONE USERDATA=NIL');
  //          //writeln('A');
  //          //eventcondition.ConditionHasChanged;
  //          //writeln('B');
  //          exit;
  //        end;
  //        else HandleError(SOCK,'invalid state for SOCKCLOSED event');
  //      end;
  //  end;
  //  esv_SOCKEXCEPT: begin
  //    con_state:=cls_ERROR;
  //    HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
  //    exit;
  //  end;
  //  esv_SOCKCANTCONNECT: begin
  //    con_state:=cls_REFUSED;
  //    HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
  //    exit;
  //  end;
  //  esv_SOCKCONNREFUSED: begin
  //    con_state:=cls_REFUSED;
  //    HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
  //    exit;
  //  end;
  //  esv_SOCKCONNTIMEDOUT: begin
  //    con_state:=cls_TIMEOUT;
  //    HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
  //    exit;
  //  end;
  //  else GFRE_BT.CriticalAbort('UNKNOWN STATE IN SERVER HANDLER');
  //end;
  //writeln('GOOD LEAVE  CONN HANDLER <',Event,' -> ',state,'>',' ',result);
end;

procedure TFRE_CLIENT_BASE_CONNECTION.Test;
begin
  writeln('YES');
end;

procedure TFRE_CLIENT_BASE_CONNECTION.Finalize;
begin
  FRee;
end;


procedure TFRE_SERVED_BASE_CONNECTION.SetOnBindDefaultSession(AValue: TFRE_DB_FetchSessionCB);
begin
  FOnBindDefaultSession:=AValue;
end;

procedure TFRE_SERVED_BASE_CONNECTION.SetOnUnbindSession( AValue: TFRE_DB_SessionCB);
begin
  FOnUnbindSession:=AValue;
end;

procedure TFRE_SERVED_BASE_CONNECTION.Finalize;
begin
  free;
end;

function TFRE_SERVED_BASE_CONNECTION.Implementor: TObject;
begin
  result := self;
end;

function TFRE_SERVED_BASE_CONNECTION.Implementor_HC: TObject;
begin
  raise EFRE_DB_Exception.Create(edb_ERROR,'logic');
end;

function TFRE_SERVED_BASE_CONNECTION.GetInfoForSessionDesc: String;
begin
  result := 'FLEX:'+FCHANNEL.GetVerboseDesc;
end;

procedure TFRE_SERVED_BASE_CONNECTION.ReadChannelData(const channel: IFRE_APSC_CHANNEL);
var myDataCount : NativeInt;

    //s           : String;
    //sr          : integer;
    //sw          : integer;
    //dispatch_it : boolean;
    CMD         : IFRE_DB_COMMAND;
    //myDataCount : Integer;


    procedure _Dispatch;
    var id       : Uint64;
        apps     : IFRE_DB_APPLICATION_ARRAY;
        i        : integer;
        prom_res : TFRE_DB_PromoteResult;
        prom_err : TFRE_DB_String;
        //dummy    : TFRE_DB_CONTENT_DESC;
        app      : IFRE_DB_Object;
        //ex_sess  : TFRE_DB_UserSession;
        sessid   : String;
    begin
      try
        state := ss_READY;
        fCMD_SIZE:=0;
        sizehdr_pos:=0;
        GFRE_DBI.LogDebug(dblc_FLEX_IO,'> '+FCHANNEL.GetVerboseDesc+LineEnding+CMD.AsDBODump);
        if not assigned(FUserSession) then begin
          if (CMD.InvokeClass='FIRMOS') and (CMD.InvokeMethod='INIT') and (CMD.Answer=false) then begin
            try
              sessid := CMD.Data.Field('SESSION_ID').AsString;
              if not FOnBindDefaultSession(self,FUserSession,sessid,false) then
                begin
                  CMD.Data.ClearAllFields;   {Try a new session}
                  CMD.Data.Field('LOGIN_OK').AsBoolean := false;
                  CMD.Data.Field('LOGIN_TXT').AsString := 'Session ID '+sessid+' is not existing';
                  CMD.ChangeSession := 'NEW';
                end
              else
                begin
                  if cmd.Data.Field('USER').AsString<>'' then begin
                    prom_res:=FUserSession.Promote(cmd.Data.Field('USER').AsString,cmd.Data.Field('PASS').AsString,prom_err,true,sessid<>'NEW',true);
                  end;
                  if (prom_res=pr_OK) or (prom_res=pr_Takeover) then
                    begin
                      apps := FUserSession.GetSessionAppArray;
                      CMD.Data.ClearAllFields;
                      for i:=0 to high(apps) do begin
                        app                         := GFRE_DBI.NewObject;
                        app.Field('CLASS').AsString := apps[i].AsObject.SchemeClass;
                        app.Field('UID').AsGUID     := apps[i].UID;
                        CMD.Data.Field('APPS').AsObject.Field(apps[i].AppClassName).AsObject:=app;
                        CMD.Data.Field('LOGIN_OK').AsBoolean:=true;
                        CMD.Data.Field('LOGIN_TXT').AsString:='SESSION : '+FUserSession.GetSessionID;
                        CMD.ChangeSession := FUserSession.GetSessionID;
                      end;
                    end
                  else
                    begin
                      CMD.Data.ClearAllFields;
                      CMD.Data.Field('LOGIN_OK').AsBoolean := false;
                      CMD.Data.Field('LOGIN_TXT').AsString := prom_err;
                      CMD.ChangeSession := 'NEW';
                    end;
                end;
              CMD.Answer        := true;
              CMD.CommandType   := fct_SyncReply;
              CMD.ErrorText     := '';
              CMD.FatalClose    := false;
              Send_ServerClient(CMD);
            except on e:exception do begin
                CMD.Answer      := true;
                CMD.CommandType := fct_Error;
                CMD.ErrorText   := 'FAIL: '+e.Message;
                CMD.CheckoutData.Finalize;
                CMD.FatalClose  := true;
                Send_ServerClient(CMD);
            end;end;
          end else begin
            CMD.Answer      := true;
            CMD.CommandType := fct_Error;
            CMD.ErrorText   := 'FAIL: MUST CALL FIRMOS.INIT';
            CMD.CheckoutData.Finalize;
            CMD.FatalClose  := true;
            Send_ServerClient(CMD);
          end;
        end else begin
          FUserSession.Input_FRE_DB_Command(cmd);
        end;
      except on e:exception do begin
        writeln('DISPATCHING ERROR ',e.Message);
      end;end;
    end;

    procedure _Read_DB_Command;
    begin
      case _ReadCommand(myDataCount,cmd) of
        crs_OK: begin
          _Dispatch;
        end;
        crs_PARTIAL_READ: begin
          state:=ss_CONTCMD;
          exit;
        end;
        else begin
          GFRE_BT.CriticalAbort('SOCK CLOSE');
          FCHANNEL.Finalize;
        end;
      end;
    end;


begin
  repeat
    myDataCount := FCHANNEL.CH_GetDataCount;
    case state of
      ss_READY: begin
        case _ReadHeader(myDataCount) of
          crs_OK:               ;
          crs_WAIT_CMD_SZ: exit ;  // stay in cmd read state
          crs_FAULT: begin
                       FCHANNEL.Finalize; // Drop connection
                     end;
        end;
        state := ss_WAITCMD;
        if myDataCount>0 then begin
          _Read_DB_Command;
        end;
      end;
      ss_WAITCMD: _Read_DB_Command;
      ss_CONTCMD: begin
        //writeln('->>>>>>>>> SERVER CONTiNuATION READ ');
        case _ReadRest(myDatacount,cmd) of
          crs_OK: begin
            state:=ss_READY;
            _Dispatch;
          end;
          crs_PARTIAL_READ: exit; // Stay in state
        end;
      end;
      else begin
        HandleError('invalid state for SOCKREAD event');
      end;
    end;
    if myDataCount<0 then
      GFRE_BT.CriticalAbort('du kanst nicht rechnen');
  until myDataCount=0;
end;

procedure TFRE_SERVED_BASE_CONNECTION.DisconnectChannel(const channel: IFRE_APSC_CHANNEL);
begin
  Free;
end;

constructor TFRE_SERVED_BASE_CONNECTION.Create;
begin
  inherited Create;
  state     := ss_READY;
end;

destructor TFRE_SERVED_BASE_CONNECTION.Destroy;
begin
  if assigned(FUserSession) then begin
    FUserSession.ClearServerClientInterface;
  end;
  inherited;
end;

procedure TFRE_SERVED_BASE_CONNECTION.Send_ServerClient(const ECN: IFRE_DB_COMMAND);
var mem:TMemoryStream;
begin
  GFRE_DBI.LogDebug(dblc_FLEXCOM,'< '+FCHANNEL.GetVerboseDesc+' '+ECN.GetInvokeClass+'.'+ECN.GetInvokeMethod+' '+BoolToStr(ECN.Answer,'ANSWER','REQUEST')+' RID='+INTTOSTR(ECN.CommandID));
  GFRE_DBI.LogDebug(dblc_FLEX_IO,'<< '+ECN.AsDBODump);
  try
    mem:=TMemoryStream.Create;
    mem.Position:=4;
    mem.Size:=ECN.NeededSize+sizeof(QWord);
    ECN.CopyToMemory(mem.Memory+sizeof(QWord));
    PQWord(mem.Memory)^:=mem.Size-sizeof(qword);
    mem.Position:=0;
    FCHANNEL.CH_WriteBuffer(mem.Memory,mem.Size);
    if ecn.FatalClose then begin
      FCHANNEL.Finalize;
    end;
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

procedure TFRE_SERVED_BASE_CONNECTION.DeactivateSessionBinding(const from_session: boolean);
begin
  GFRE_DBI.LogInfo(dblc_SESSION,' DEACTIVATE SESSION BINDING FOR '+FCHANNEL.GetVerboseDesc+' '+FUserSession.GetSessionID);
  FUserSession := nil;
end;

procedure TFRE_SERVED_BASE_CONNECTION.UpdateSessionBinding(const new_session: TObject);
begin
  GFRE_DBI.LogInfo(dblc_SESSION,' UPDATE SESSION BINDING FOR '+FCHANNEL.GetVerboseDesc+' TO '+(new_session as TFRE_DB_UserSession).GetSessionID);
  FUserSession := new_session as TFRE_DB_UserSession;
end;

function TFRE_SERVED_BASE_CONNECTION.GetChannel: IFRE_APSC_CHANNEL;
begin
  result := FChannel;
end;





function TFRE_DB_SOCK_CONNECTION._ReadHeader(var data_count: NativeInt): TCMD_READ_STATE;
var rec_buffer  : Array[0..7] of Byte;
    size_read,i : integer;
    cmd_ptr     : PByte;
begin
  FillByte(rec_buffer,8,0);
  if data_count < (8-sizehdr_pos) then begin
    size_read := FCHANNEL.CH_ReadBuffer(@rec_buffer,data_count);
    if size_read=-1 then begin
      GFRE_LOG.Log('READHEADER: RECV PROBLEM A - %s',[FCHANNEL.CH_GetErrorString],catError);
      exit(crs_FAULT);
    end;
    if size_read<>data_count then begin
      GFRE_LOG.Log('READHEADER: DATACOUNT SIZE READ PROBLEM B - Sizeread %d <> datacount %s ',[size_read,data_count],catError);
      exit(crs_FAULT);
    end;
  end else begin
    size_read:=FCHANNEL.CH_ReadBuffer(@rec_buffer,8-sizehdr_pos);
    if size_read=-1 then begin
      GFRE_LOG.Log('READHEADER: RECV PROBLEM C - %s ',[FCHANNEL.CH_GetErrorString],catError);
      exit(crs_FAULT);
    end;
  end;
  dec(data_count,size_read);
  cmd_ptr := @fCMD_SIZE;
  for i:=0 to size_read-1 do begin
    cmd_ptr[i+sizehdr_pos] := rec_buffer[i];
  end;
  inc(sizehdr_pos,size_read);
  if (sizehdr_pos<0) or (sizehdr_pos>8) then GfRE_BT.CriticalAbort('INTERNAL FAILURE / read size hdr');
  if sizehdr_pos=8 then begin
    exit(crs_OK);
  end else begin
    exit(crs_WAIT_CMD_SZ);
  end;
end;

{ TFRE_DB_SOCK_CONNECTION }
function TFRE_DB_SOCK_CONNECTION._ReadCommand(var data_count:NativeInt;out cmd:IFRE_DB_COMMAND): TCMD_READ_STATE;
var avail_data : integer;
    size_read,toread:integer;
    dbo    : IFRE_DB_Object;
begin
  result:=crs_OK;
  cmd:=nil;
  readm.Clear;
  readm.Position:=0;
  readm.SetSize(fCMD_SIZE);
  toread:=gfre_bt.Min(fCMD_SIZE,data_count);
  try
    size_read := FCHANNEL.CH_ReadBuffer(readm.Memory,toread);
    dec(data_count,size_read);
  except on e:Exception do begin
    GFRE_LOG.Log('ERROR _READCOMMAND sock.receive ? -> %s',[e.Message],catError);
  end;end;
  //writeln('RECEIVE GOT ',res);
  if size_read=-1 then begin
    GFRE_LOG.Log('OS FAILURE RECIEVE/READCOMMAND? -> %s',[FCHANNEL.CH_GetErrorString],catError);
    writeln('OS FAILURE RECIEVE/READCOMMAND? -> '+FCHANNEL.CH_GetErrorString);
    FCHANNEL.Finalize;
    exit;
  end;
  readm.Position:=size_read;
  if toread<>size_read then begin
    GFRE_LOG.Log('COMMAND ERROR data_count=%d  fCMD_SIZE=%d Size_read=%d',[data_count,fCMD_SIZE,size_read],catError);
    FCHANNEL.Finalize;
    exit(crs_FAULT);
  end;
  if size_read<fCMD_SIZE then begin
    readrest:=fCMD_SIZE-size_read;
    exit(crs_PARTIAL_READ);
  end;
  try
    readm.Position:=0;
    dbo := nil;
    dbo := GFRE_DBI.CreateFromMemory(readm.Memory);
    if dbo.Supports(IFRE_DB_COMMAND,cmd) then begin
      exit(crs_OK);
    end else begin
      raise Exception.Create('IFRE_DB_COMMAND INTERFACE NOT SUPPORTED? - INTERNAL FAULT');
    end;
  except on e:EXCEPTION do begin
    GFRE_LOG.Log('COMMAND IS INVALID ERROR <%s>',[e.Message],catError);
    writeln('COMMAND IS INVALID ERROR <'+e.Message+'>');
    if assigned(dbo) then begin
      dbo.Finalize;
    end;
  end;end;
  FCHANNEL.Finalize;
  exit(crs_FAULT);
end;

function TFRE_DB_SOCK_CONNECTION._ReadRest(dc: cardinal;out cmd:IFRE_DB_COMMAND): TCMD_READ_STATE;
var  sr   : integer;
   toread : integer;
   dbo    : IFRE_DB_Object;
begin
//writeln('READREST ',readrest,' DATACOUNT ',dc,' MPOS ',readm.Position,' MSIZE ',readM.Size,' Stream Rest =',readm.size-readm.Position);
//try
  cmd:=nil;
  if dc>readrest then begin
    writeln('GOT MORE DATA TO READ, CLIPPING');
    dc:=readrest;
  end;
  toread:=GFRE_BT.Min(dc,readrest);
  if (readm.Size-readm.Position)<>readrest then GFRE_BT.CriticalAbort('INVALID STREAM LENGHT/READREST');
  try
    sr := FCHANNEL.CH_ReadBuffer(readM.Memory+readM.Position,toread);
  except on e:Exception do begin
    writeln('ERROR ',e.Message);
  end;end;
  if dc<>sr then begin
    GFRE_BT.CriticalAbort('COMMAND ERROR DC=%d  READREST=%d SR=%d',[dc,readrest,sr]);
    FCHANNEL.Finalize;
    exit(crs_FAULT);
  end;
  readrest:=readrest-dc;
  //WriteLn('READ ',readm.Position,' ',readm.Size);
  if readrest=0 then begin
    try
      readm.Position:=0;
      dbo := GFRE_DBI.CreateFromMemory(readm.Memory);
      if dbo.Supports(IFRE_DB_COMMAND,cmd) then begin
        exit(crs_OK);
      end else begin
        raise Exception.Create('IFRE_DB_COMMAND INTERFACE NOT SUPPORTED? - INTERNAL FAULT');
      end;
    except on e:EXCEPTION do begin
      GFRE_LOG.Log('COMMAND IS INVALID ERROR <%s>',[e.Message],catError);
      writeln('COMMAND IS INVALID ERROR <'+e.Message+'>');
      dbo.Finalize;
      exit(crs_FAULT);
    end;end;
  end else
  if readrest>0 then begin
    Result:=crs_PARTIAL_READ;
    readm.Position:=readm.Position+sr;
  end else begin
    GFRE_BT.CriticalAbort('READREST SIZE < 0 readrest=%d',[readrest]);
  end;
//finally
//  writeln('LEFT READREST ',result);
//end;
end;




constructor TFRE_DB_SOCK_CONNECTION.Create;
begin
  inherited;
  readm:=TMemoryStream.Create;
  //GFRE_Tf.Get_LFQ(offloadlist);
  sizehdr_pos:=0;
end;

destructor TFRE_DB_SOCK_CONNECTION.Destroy;
begin
  //if assigned(eventcondition) then begin
  //  eventcondition.Finalize; // Non refcounted IF
  //  eventcondition:=nil;;
  //end;
  readm.free;
  inherited Destroy;
end;

procedure TFRE_DB_SOCK_CONNECTION.SetChannel(const channel: IFRE_APSC_CHANNEL);
begin
  FCHANNEL := channel;
end;

procedure TFRE_DB_SOCK_CONNECTION.HandleError(const err: String);
begin
  writeln(classname,' CHANNEL ERROR ',FCHANNEL.GetVerboseDesc,' ',err,' ',FCHANNEL.CH_GetErrorString);
  FCHANNEL.Finalize;
end;

function TFRE_DB_SOCK_CONNECTION.GetSessionID: String;
begin
  result:=SessionID;
end;


end.

