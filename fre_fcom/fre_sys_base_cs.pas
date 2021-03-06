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
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_FCOM_INTERFACES,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FOS_INTERLOCKED;

const cCMD_MAXSize                        = 512*1024;
      cFRE_DB_MAX_PENDING_CLIENT_REQUESTS = 10;

var fSession_Counter : Integer=0;

type TFRE_FCOM_PROTO            = (cfp_TLS); //UDP,TLS,SSH,CLUSTER ...
     TFRE_CMD_TYPE              = (cfc_LOGIN,cfc_CMD,cfc_ERROR,cfc_OK);
     TFRE_CMD_ERROR_TYPE        = (cce_INVALID_PROTOVERSION,cce_LOGIN_WRONG);

     TINTERNAL_CONNECTION_STATE = (cs_TRYCONNECT,cs_READY,cs_CONTCMD,cs_LOGINFAIL,cs_TIMEOUT,cs_WAITCMD);
     TCONNECTION_STATE          = (cls_TIMEOUT,cls_REFUSED,cls_ERROR,cls_CONNECTED,cls_OK,cls_CLOSED);

     TSERVED_CONNECTION_STATE   = (ss_CONNECTED,ss_CONTCMD,ss_READY,ss_WAITCMD);
     TCMD_READ_STATE            = (crs_OK,crs_WAIT_CMD_SZ,crs_PARTIAL_READ,crs_COMMAND_TOO_LONG,crs_FAULT);



  { TFRE_DB_SOCK_CONNECTION }


  TFRE_DB_SOCK_CONNECTION=class(TObject)
  protected
    readm              : TMemoryStream;
    sizehdr_pos        : cardinal;
    fCMD_SIZE          : QWord;
    readrest           : cardinal;
    function           _ReadHeader (var data_count:integer):TCMD_READ_STATE;
    function           _ReadCommand (var data_count:integer;out cmd:IFRE_DB_COMMAND):TCMD_READ_STATE;
    function           _ReadRest    (dc:cardinal;out cmd:IFRE_DB_COMMAND):TCMD_READ_STATE;
  public
    SessionID   :String;
    SOCK        :IFCOM_SOCK;
    constructor Create;
    destructor  Destroy;override;
    function    Handler             (const Event:EFOS_FCOM_MULTIEVENT;const Datacount:Integer):boolean;virtual;abstract;
    procedure   HandleError         (const cs:IFCOM_SOCK;const err:String);virtual;abstract;
    Function    GetSessionID        :String;
  end;

  { TFRE_CLIENT_BASE_CONNECTION }
  TFRE_CLIENT_BASE_CONNECTION=class(TFRE_DB_SOCK_CONNECTION)
  private
    var
      FOnConnectionStateChange : TNotifyEvent;
      FOnNewCommandAnswerHere  : TFRE_DB_CMD_Request;
      FOnNewServerRequest      : TFRE_DB_CMD_Request;
      state                    : TINTERNAL_CONNECTION_STATE;
      con_state                : TCONNECTION_STATE;
      Ferror                   : string;

    procedure   SetOnConnectionStateChange (AValue: TNotifyEvent);
    procedure   SetOnNewCommandAnswerHere  (AValue: TFRE_DB_CMD_Request);
    procedure   SetOnNewServerRequest      (AValue: TFRE_DB_CMD_Request);
    procedure   _Statechanged;
  protected
    //function   _ReadACommand(const cmd:IFRE_DB_COMMAND):boolean; // True = Answer = State change
  public
    READ_CMD    : IFRE_DB_Object;
    procedure   InvokeServerCommand (const InvokeClass,InvokeMethod : String;const uidpath:TFRE_DB_GUIDArray;const DATA: IFRE_DB_Object;const CID : Qword;const async:boolean);
    constructor Create              (const socket:IFCOM_SOCK);
    destructor  Destroy             ;override;
    function    ConnectionState     :TCONNECTION_STATE;
    function    Handler             (const Event:EFOS_FCOM_MULTIEVENT;const Datacount:Integer):boolean;override;
    procedure   HandleError         (const cs:IFCOM_SOCK;const err:String);override;
    procedure   Test;
    procedure   Finalize;
    property    OnConnectionStateChange : TNotifyEvent read FOnConnectionStateChange write SetOnConnectionStateChange; // Not Threadsafe
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
    constructor Create                    ;
    destructor  Destroy                   ;override;
    function    Handler                   (const Event:EFOS_FCOM_MULTIEVENT;const Datacount:Integer):boolean;override;
    procedure   HandleError               (const cs:IFCOM_SOCK;const err:String);override;
    procedure   Send_ServerClient         (const ECN:IFRE_DB_COMMAND);
    property    OnBindInitialSession      : TFRE_DB_FetchSessionCB read FOnBindDefaultSession write SetOnBindDefaultSession;
    property    OnUnbindSession           : TFRE_DB_SessionCB read FOnUnbindSession write SetOnUnbindSession;
    procedure   DeactivateSessionBinding  ;
    procedure   UpdateSessionBinding      (const new_session : TObject);
  end;

  TFRE_CONNECTION_CLASS=class of TFRE_DB_SOCK_CONNECTION;


implementation

{ TFRE_CLIENT_BASE_CONNECTION }

procedure TFRE_CLIENT_BASE_CONNECTION.SetOnConnectionStateChange(AValue: TNotifyEvent);
begin
  FOnConnectionStateChange:=AValue;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.SetOnNewCommandAnswerHere(AValue: TFRE_DB_CMD_Request);
begin
  FOnNewCommandAnswerHere:=AValue;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.SetOnNewServerRequest(AValue: TFRE_DB_CMD_Request);
begin
  FOnNewServerRequest:=AValue;
end;

procedure TFRE_CLIENT_BASE_CONNECTION._Statechanged;
begin
  if assigned(FOnConnectionStateChange) then begin
    FOnConnectionStateChange(Self);
  end;
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
    SOCK.Offload_WriteBuf(mem.Memory,mem.Size,false);
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

constructor TFRE_CLIENT_BASE_CONNECTION.Create(const socket: IFCOM_SOCK);
begin
  inherited Create;
  state       := cs_TRYCONNECT;
  SessionID   := '-';
  SOCK        := socket;
end;

destructor TFRE_CLIENT_BASE_CONNECTION.Destroy;
begin
  //WriteLn('!CLIENT BASE CONNECTION FREE!');
  inherited;
end;


function TFRE_CLIENT_BASE_CONNECTION.Handler(const Event: EFOS_FCOM_MULTIEVENT; const Datacount: Integer):boolean;
var s:String;
    sr:integer;
    sw:integer;
    dispatch_it:boolean;
    cmd:IFRE_DB_COMMAND;
    readres:TCMD_READ_STATE;
    myDatacount:integer;

    procedure _Dispatch;
    begin
      try
        state          := cs_READY;
        fCMD_SIZE      := 0;
        sizehdr_pos    := 0;
        if cmd.Answer then begin
          writeln('>GOT ANSWER');
          FOnNewCommandAnswerHere(self,cmd);
          writeln('<GOT ANSWER');
        end else begin // Request
          writeln('******** CMD : GOT REQUEST HANDLE IT');
          writeln(cmd.AsDBODump);
          GFRE_BT.CriticalAbort('IMPLEMENT');
          //FOnNewServerRequest(self,cmd);
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
          GFRE_BT.CriticalAbort('SOCK CLOSE');
          sock.CloseEnqueue(99);
        end;
      end;
    end;

begin
//  writeln(GFRE_S.CURRENT_THREAD,' ENTER CONN HANDLER <',Event,' in ',state,':',con_state,' >');
//  try
  result := true;
  myDatacount := Datacount;
  case event of
    esv_SOCKERROR: begin
      con_state:=cls_ERROR;
      HandleError(SOCK,'received error state');
    end;
    esv_SOCKCONNECTED: begin
      case state of
        cs_TRYCONNECT: begin
          state:=cs_READY;
          con_state:=cls_CONNECTED;//eventcondition.ConditionHasChanged;
          _Statechanged;
          exit;
        end;
        else HandleError(SOCK,'invalid state for SOCKCONNECTED event');
      end;
    end;
    esv_SOCKREAD: begin
      case state of
        cs_READY: begin
          case _ReadHeader(myDatacount) of
            crs_OK:          ;
            crs_WAIT_CMD_SZ: exit;
            crs_FAULT:       SOCK.CloseEnqueue(99); // Drop connection
          end;
          state := cs_WAITCMD;
          if myDataCount>0 then begin
            _Read_DB_Command;
          end;
          exit;
        end;
        cs_WAITCMD: _Read_DB_Command;
        cs_CONTCMD: begin
          writeln('->>>>>>>>> SERVER CONTiNuATION READ ');
          case _ReadRest(myDatacount,cmd) of
            crs_OK: begin
              state:=cs_READY;
              _Dispatch;
              exit;
            end;
            crs_PARTIAL_READ: exit; // Stay in state
          end;
        end;
        else HandleError(SOCK,'invalid state for SOCKREAD event');
      end;
    end;
    esv_SOCKWRITE: begin
        HandleError(SOCK,'invalid state for SOCKWRITE event');
    end;
    esv_SOCKCLOSED: begin
        con_state:=cls_CLOSED;
        case state of
          cs_READY: begin
            writeln('<>CLIENT DONE');
            _Statechanged;
            //SOCK.UserData:=nil;
            writeln('<>CLIENT DONE USERDATA=NIL');
            writeln('A');
            //eventcondition.ConditionHasChanged;
            writeln('B');
            exit;
          end;
          else HandleError(SOCK,'invalid state for SOCKCLOSED event');
        end;
    end;
    esv_SOCKEXCEPT: begin
      con_state:=cls_ERROR;
      HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
      exit;
    end;
    esv_SOCKCANTCONNECT: begin
      con_state:=cls_REFUSED;
      HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
      exit;
    end;
    esv_SOCKCONNREFUSED: begin
      con_state:=cls_REFUSED;
      HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
      exit;
    end;
    esv_SOCKCONNTIMEDOUT: begin
      con_state:=cls_TIMEOUT;
      HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
      exit;
    end;
    else GFRE_BT.CriticalAbort('UNKNOWN STATE IN SERVER HANDLER');
  end;
  writeln('GOOD LEAVE  CONN HANDLER <',Event,' -> ',state,'>',' ',result);
end;

procedure TFRE_CLIENT_BASE_CONNECTION.HandleError(const cs: IFCOM_SOCK;  const err: String);
begin
  FError := Err;
  _Statechanged;
end;

procedure TFRE_CLIENT_BASE_CONNECTION.Test;
begin
  writeln('YES');
end;

procedure TFRE_CLIENT_BASE_CONNECTION.Finalize;
begin
  FRee;
end;


function TFRE_CLIENT_BASE_CONNECTION.ConnectionState: TCONNECTION_STATE;
begin
  result:=con_state;
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
  result := 'FLEX:'+SOCK.GetVerboseDesc;
end;

constructor TFRE_SERVED_BASE_CONNECTION.Create;
begin
  inherited Create;
  state     := ss_CONNECTED;
end;

destructor TFRE_SERVED_BASE_CONNECTION.Destroy;
begin
  if assigned(FUserSession) then begin
    FUserSession.ClearServerClientInterface;
  end;
  inherited;
end;

function TFRE_SERVED_BASE_CONNECTION.Handler(const Event: EFOS_FCOM_MULTIEVENT; const Datacount: Integer):boolean;
var s           : String;
    sr          : integer;
    sw          : integer;
    dispatch_it : boolean;
    CMD         : IFRE_DB_COMMAND;
    myDataCount : Integer;


    //BindInitialSession(bc,bc.UserSession,'');
    //InterLockedIncrement(fSession_Counter);
    //bc.SessionID :='S|'+inttostr(fSession_Counter)+'|'+inttostr(sock.GetHandleKey)+'|'+sock.Get_AI.SocketAsString; // Not the DB SessionId ? Obsolete ?
    //writeln('INIT FC/SESSION ',bc.SessionID);

    procedure _Dispatch;
    var id       : Uint64;
        apps     : IFRE_DB_APPLICATION_ARRAY;
        i        : integer;
        prom_res : TFRE_DB_PromoteResult;
        prom_err : TFRE_DB_String;
        dummy    : TFRE_DB_CONTENT_DESC;
        app      : IFRE_DB_Object;
    begin
      try
        state := ss_READY;
        fCMD_SIZE:=0;
        sizehdr_pos:=0;
        GFRE_DBI.LogDebug(dblc_FLEXCOM,'>>INPUT FROM : '+SOCK.GetVerboseDesc);
        GFRE_DBI.LogDebug(dblc_FLEXCOM,CMD.AsDBODump);
        GFRE_DBI.LogDebug(dblc_FLEXCOM,'********************************************************');
        if not assigned(FUserSession) then begin
          if (CMD.InvokeClass='FIRMOS') and (CMD.InvokeMethod='INIT') and (CMD.Answer=false) then begin
            try
              FOnBindDefaultSession(self,FUserSession,CMD.Data.Field('SESSION_ID').AsString,false);
              if cmd.Data.Field('USER').AsString<>'' then begin
                prom_res:=FUserSession.Promote(cmd.Data.Field('USER').AsString,cmd.Data.Field('PASS').AsString,prom_err,true,true,dummy);
                writeln('PROMOTION RESULT ',prom_res);
              end;
              apps := FUserSession.GetSessionAppArray;
              CMD.Data.ClearAllFields;
              writeln('ANWSERING FOR ',Length(apps),' APPS');
              for i:=0 to high(apps) do begin
                app                         := GFRE_DBI.NewObject;
                app.Field('CLASS').AsString := apps[i].AsObject.SchemeClass;
                app.Field('UID').AsGUID     := apps[i].UID;
                CMD.Data.Field('APPS').AsObject.Field(apps[i].ObjectName).AsObject:=app;
                writeln('  ',apps[i].ObjectName);
              end;
              CMD.Answer        := true;
              CMD.CommandType   := fct_SyncReply;
              CMD.ErrorText     := '';
              CMD.ChangeSession := FUserSession.GetSessionID;
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
          sock.CloseEnqueue(99);
        end;
      end;
    end;

begin
  //writeln('ENTER CONN HANDLER <',Event,' (in)-> ',state,'>',' ',Datacount);
  result := true;
  myDataCount := Datacount;
  case event of
    esv_SOCKERROR: begin
      HandleError(SOCK,'received error state');
    end;
    esv_SOCKCONNECTED: begin
      case state of
        ss_CONNECTED: begin
          state:=ss_READY;
        end;
        else HandleError(SOCK,'invalid state for SOCKCONNECTED event');
      end;
    end;
    esv_SOCKREAD: begin
      case state of
        ss_READY: begin
          case _ReadHeader(myDataCount) of
            crs_OK:               ;
            crs_WAIT_CMD_SZ: exit ;  // stay in cmd read state
            crs_FAULT: begin
                         SOCK.CloseEnqueue(99); // Drop connection
                       end;
          end;
          state := ss_WAITCMD;
          if myDataCount>0 then begin
            _Read_DB_Command;
          end;
        end;
        ss_WAITCMD: _Read_DB_Command;
        ss_CONTCMD: begin
          writeln('->>>>>>>>> SERVER CONTiNuATION READ ');
          case _ReadRest(myDatacount,cmd) of
            crs_OK: begin
              state:=ss_READY;
              _Dispatch;
              exit;
            end;
            crs_PARTIAL_READ: exit; // Stay in state
          end;
        end;
        else HandleError(SOCK,'invalid state for SOCKREAD event');
      end;
    end;
    esv_SOCKWRITE: begin
      HandleError(SOCK,'invalid state for SOCKWRITE event');
    end;
    esv_SOCKCLOSED: begin
        case state of
          ss_READY: begin
            //if assigned(base_S.SRV) then begin
            //  base_s.SRV.ConnectionDropped(SessionID);
            //end;
          end;
          else HandleError(SOCK,'invalid state for SOCKCLOSED event');
        end;
    end;
    esv_SOCKEXCEPT,
    esv_SOCKCANTCONNECT,
    esv_SOCKCONNREFUSED,
    esv_SOCKCONNTIMEDOUT: begin
      HandleError(SOCK,'ERROR EVENT <'+CFOS_FCOM_MULTIEVENT[Event]+'>');
    end;
    else GFRE_BT.CriticalAbort('UNKNOWN STATE IN SERVER HANDLER');
  end;
  //GFRE_BT.CriticalAbort('DUNK');
end;

procedure TFRE_SERVED_BASE_CONNECTION.HandleError(const cs: IFCOM_SOCK; const err: String);
begin
  writeln('--HANDLE ERROR ',err);
  //if assigned(base_S.SRV) then begin
  //  base_s.SRV.ConnectionDropped(SessionID);
  //end;
  cs.CloseEnqueue(99);
end;

procedure TFRE_SERVED_BASE_CONNECTION.Send_ServerClient(const ECN: IFRE_DB_COMMAND);
var mem:TMemoryStream;
begin
  GFRE_DBI.LogDebug(dblc_FLEXCOM,'<<OUTPUT TO '+Sock.GetVerboseDesc);
  GFRE_DBI.LogDebug(dblc_FLEXCOM,ECN.AsDBODump);
  GFRE_DBI.LogDebug(dblc_FLEXCOM,'**************************************************************************');
  try
    mem:=TMemoryStream.Create;
    mem.Position:=4;
    mem.Size:=ECN.NeededSize+sizeof(QWord);
    ECN.CopyToMemory(mem.Memory+sizeof(QWord));
    PQWord(mem.Memory)^:=mem.Size-sizeof(qword);
    mem.Position:=0;
    SOCK.Offload_WriteBuf(mem.Memory,mem.Size,false);
    if ecn.FatalClose then begin
      SOCK.CloseEnqueue(13);
    end;
  finally
    mem.Free;
    ECN.Finalize;
  end;
end;

procedure TFRE_SERVED_BASE_CONNECTION.DeactivateSessionBinding;
begin
  FUserSession := nil;
end;

procedure TFRE_SERVED_BASE_CONNECTION.UpdateSessionBinding(const new_session: TObject);
begin
  FUserSession := new_session as TFRE_DB_UserSession;
end;





function TFRE_DB_SOCK_CONNECTION._ReadHeader(var data_count: integer): TCMD_READ_STATE;
var rec_buffer  : Array[0..7] of Byte;
    size_read,i : integer;
    res         : EFOS_OS_ERROR;
    cmd_ptr     : PByte;
begin
  FillByte(rec_buffer,8,0);
  if data_count < (8-sizehdr_pos) then begin
    res:=SOCK.Receive(@rec_buffer,data_count,size_read);
    if res<>EFOS_OS_OK then begin
      GFRE_LOG.Log('READHEADER: RECV PROBLEM A - %s ',[CFOS_OS_ERROR[res]],catError);
      exit(crs_FAULT);
    end;
    if size_read<>data_count then begin
      GFRE_LOG.Log('READHEADER: DATACOUNT SIZE READ PROBLEM B - Sizeread %d <> datacount %s ',[size_read,data_count],catError);
      exit(crs_FAULT);
    end;
  end else begin
    res:=SOCK.Receive(@rec_buffer,8-sizehdr_pos,size_read);
    if res<>EFOS_OS_OK then begin
      GFRE_LOG.Log('READHEADER: RECV PROBLEM C - %s ',[CFOS_OS_ERROR[res]],catError);
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
function TFRE_DB_SOCK_CONNECTION._ReadCommand(var data_count:integer;out cmd:IFRE_DB_COMMAND): TCMD_READ_STATE;
var avail_data : integer;
    res:EFOS_OS_ERROR;
    size_read,toread:integer;
    dbo    : IFRE_DB_Object;
begin
//writeln('>READCOMMAD');
//try
  result:=crs_OK;
  cmd:=nil;
  //if fCMD_SIZE>cCMD_MAXSize then begin
  //  GFRE_LOG.Log('COMMAND SIZE VIOLATION %d is > %d',[fCMD_SIZE,cCMD_MAXSize],catError);
  //  GFRE_BT.CriticalAbort('SOCK CLOSE');
  //  SOCK.Close;
  //  exit(crs_COMMAND_TOO_LONG);
  //end;
  readm.Clear;
  readm.Position:=0;
  readm.SetSize(fCMD_SIZE);
  toread:=gfre_bt.Min(fCMD_SIZE,data_count);
  try
    res:=SOCK.Receive(readm.Memory,toread,size_read);
  except on e:Exception do begin
    writeln('ERROR _READCOMMAND sock.receive',e.Message);
  end;end;
  //writeln('RECEIVE GOT ',res);
  if res<>EFOS_OS_OK then begin
    GFRE_LOG.Log('OS FAILURE RECIEVE/READCOMMAND? -> %s',[CFOS_OS_ERROR[res]],catError);
    writeln('OS FAILURE RECIEVE/READCOMMAND? -> '+CFOS_OS_ERROR[res]);
    sock.CloseEnqueue(99);
    exit;
  end;
  readm.Position:=size_read;
  if toread<>size_read then begin
    GFRE_LOG.Log('COMMAND ERROR data_count=%d  fCMD_SIZE=%d Size_read=%d',[data_count,fCMD_SIZE,size_read],catError);
    SOCK.CloseEnqueue(99);
    exit(crs_FAULT);
  end;
  if size_read<fCMD_SIZE then begin
    readrest:=fCMD_SIZE-size_read;
    writeln(format('COMMAND SIZE NOT FULLY RECEIVED data_count=%d  fCMD_SIZE=%d REST=%d',[data_count,fCMD_SIZE,readrest]));
    exit(crs_PARTIAL_READ);
    GFRE_BT.CriticalAbort('COMMAND SIZE NOT FULLY RECEIVED = IMPL.BUG data_count=%d  fCMD_SIZE=%d',[data_count,fCMD_SIZE]);
  end;
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
    if assigned(dbo) then begin
      dbo.Finalize;
    end;
  end;end;
  SOCK.CloseEnqueue(99);
  exit(crs_FAULT);
end;

function TFRE_DB_SOCK_CONNECTION._ReadRest(dc: cardinal;out cmd:IFRE_DB_COMMAND): TCMD_READ_STATE;
var res   : EFOS_OS_ERROR;
     sr   : integer;
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
    res:=SOCK.Receive(readM.Memory+readM.Position,toread,sr);
  except on e:Exception do begin
    writeln('ERROR ',e.Message);
  end;end;
  //writeln('RECEIVE GOT ',res);
  if res<>EFOS_OS_OK then GFRE_BT.CriticalAbort(' OS FAILURE RECIEVE/READCOMMAND? -> %s',[CFOS_OS_ERROR[res]]);
  if dc<>sr then begin
    GFRE_BT.CriticalAbort('COMMAND ERROR DC=%d  READREST=%d SR=%d',[dc,readrest,sr]);
    SOCK.CloseEnqueue(99);
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

function TFRE_DB_SOCK_CONNECTION.GetSessionID: String;
begin
  result:=SessionID;
end;


end.

