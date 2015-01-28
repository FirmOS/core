unit fre_webssocket_baseserver;

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

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,FRE_HTTP_TOOLS,FRE_HTTP_SRVHANDLER,math,
  FRE_DB_INTERFACE,FRE_DB_COMMON,FRE_DB_CORE,FRE_SYSTEM,fre_binary_buffer,
  fpjson,jsonparser,
  FRE_WAPP_DOJO
    {$IFDEF UNIX}
  ,BASEUNIX
  {$ENDIF}
  ;

//TODO: SecurityType #1#1 Force/Bug
//TODO: SendServer Init -> not clean

//var next: boolean=false;
//    hin : boolean=false;
//    precond:boolean=falsE;

var cnt:integer=0;
    G_PROXY_ADDRESS : String;
    G_PROXY_PORT    : integer;

const
    {$IFDEF CPUARM}
      C_USE_LIBEVENT_FILEBUFFER=FALSE;
    {$ELSE}
      C_USE_LIBEVENT_FILEBUFFER=TRUE;
    {$ENDIF}
type
  TFRE_VNC_DECODE_State         = (fbu_IDLE,fbu_READ_MSG_TYPE,fbu_READ_MSG_FULL,fbu_CheckRectCntFull);
  TFRE_VNC_SUB_DECODE_State     = (fbuss_READ_RECT_DEF,fbuss_READ_RECT,fbuss_DO_CHECK_HEXTILE_COUNT);
  TFRE_VNC_SUB_HEX_DECODE_State = (fbussh_HEX_READ_MASK,fbussh_HEX_READ_TILE,fbussh_HEX_READ_TILE_RAW,fbussh_HEX_READ_TILE_BG_SPECIFIED,
                                   fbussh_HEX_READ_TILE_FG_SPECIFIED,fbussh_HEX_READ_TILE_ANY,fbussh_HEX_READ_TILE_READ_ANYRECTS);

{.$DEFINE WS_LOG_DEBUG}
{$DEFINE WS_LOG_ERROR}
{.$DEFINE WS_LOG_WARNING}
{.$DEFINE WS_LOG_INFO}
{.$DEFINE HTTP_LOG_DEBUG}


type

  { TFRE_WEBSOCKET_SERVERHANDLER_BASE }

  TFRE_WEBSOCKET_SERVERHANDLER_BASE=class(TFRE_HTTP_CONNECTION_HANDLER)
  private
    type TFRE_WEBSOCKET_MODE = (fsm_RFC6455,fsm_HIXIE);
  var
    FHeaderShort   : Boolean;
    FWSHeader      : ShortString;
    FShortData     : ShortString;
    FData          : String;
    FUpgraded      : Boolean;
    FMakey         : Array [0..3] of Byte;
    FFrameNotDone  : boolean;
    FDecodeFrame   : String;
    FByte          : PByte;
    FFIN_Flag      : Boolean;
    FMask          : Boolean;
    FLen           : QWord;
    FReceiveRest   : Qword;
    FGotLen        : QWord;
    FWSStartOpcode : Byte;
    FWSDataframe   : RawByteString;

    FWebSocket_Protocol      : String;
    FWSSockModeProtoVersion  : TFRE_WEBSOCKET_MODE;
    procedure   _ClearHeader;
    procedure   _EncodeData(data:TFRE_DB_RawByteString;const binary:boolean=false);
    procedure   _SendCloseFrame;
    procedure   _SendHttpResponseStream          (const code: integer; const answer: string; const content:TMemoryStream;const contenttype:string='';const is_attachment:boolean=false;const attachment_filename:string='');
    procedure   _SendHttpResponse                (const code: integer; const answer: string; const answer_params: array of const;const content:string='';const contenttype:string='';const is_attachment:boolean=false;const attachment_filename:string='');
    procedure   _SendHttpFileWithRangeCheck      (lfilename: string; const isAttachment:boolean=false;const use_dynamic_wwwroot : boolean=false ; const accept_range_requests : boolean = false);
    procedure   __SendHttpFile                   (const lfilename: string; const ispartial:boolean ; const isAttachment:boolean=false ; const offset:NativeUint=0; const len : NativeUint=0 ; const accept_range_request : boolean = false);
    procedure   _SendHttpFileMetaData            (const metae : TFRE_HTTP_METAENTRY ; const send_zipped : boolean);
    procedure   ClientConnected;virtual;
  protected
    procedure   Handle_WS_Upgrade  (const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
    procedure   ReadChannelData    (const channel:IFRE_APSC_CHANNEL); override;
  public
    procedure   SendToClient(data:TFRE_DB_RawByteString;const binary:boolean);
    procedure   ReceivedFromClient(const opcode:byte ; const dataframe : RawByteString);virtual;
  end;

  TFRE_VNC_PROXY_STATE=(vps_NOT_CONNECTED,vps_READ_RFB_VERSION_VNC2WC,vps_RFB_VERSION_SENT_WC2VNC,vps_RFB_VERSION_SENT_AUTH_WC2VNC,vps_RFB_VERSION_SENT_CLIENT_INIT_WC2VNC,vps_SHUFFELING);

  { TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY }

  TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY=class(TFRE_WEBSOCKET_SERVERHANDLER_BASE,IFRE_DB_COMMAND_REQUEST_ANSWER_SC)
  private
    FOnBindDefaultSession : TFRE_DB_FetchSessionCB;
    type
      TVNC_FBU_RECT=record
        xpos    : Word;
        ypos    : Word;
        width   : Word;
        height  : Word;
        enctype : Integer; // 32 Bit
      end;
      PVNC_FBU_RECT = ^TVNC_FBU_RECT;
      TFWS_Mode = (wsm_INVALID,wsm_VNCPROXY,wsm_FREDB,wsm_FREDB_DEACTIVATED);
   var
    FProxyState  : TFRE_VNC_PROXY_STATE;
    FWebsocketMode: TFWS_Mode;
    FBinaryData  : TFRE_BINARYDATA;

    //FBU  - TODO ENCAP STATE and create only if needed
    FFBU_State               : TFRE_VNC_DECODE_State;
    FFBU_SUB_State           : TFRE_VNC_SUB_DECODE_State;
    FFBU_SUB_HEX_STATE       : TFRE_VNC_SUB_HEX_DECODE_State;
    FFBU_NumRects            : NativeInt;
    FFBU_ReceivedRectCnt     : NativeInt;
    FLastSubrectCount        : NativeInt;
    FCurrentBytesPerPixel    : NativeInt;
    FCurrentRectCoding       : TVNC_FBU_RECT;
    FCurrentHextile_X_Tiles  : NativeInt;
    FCurrentHextile_Y_Tiles  : NativeInt;
    FCurrentHextile_X_TilesR : NativeInt;
    FCurrentHextile_Y_TilesR : NativeInt;
    FCurrentHexTile_RCount   : NativeInt;
    FCurrentHextile_RAW_SIZE : NativeInt;
    FCurrentHexTile_X_Short  : Boolean;
    FCurrentHexTile_Y_Short  : Boolean;
    FCurrentHexTile_Mask     : Byte;
    FCurrentHexTile_AnyCount : Byte;
    FNo_Base64               : boolean;
    FForceIgnoreFurtherInput : boolean;

    FCurrentSession          : TFRE_DB_UserSession;
    TransFormFunc            : TFRE_DB_TRANSFORM_FUNCTION;
    FVNCProxyChannel         : IFRE_APSC_CHANNEL;

    procedure   SetOnBindDefaultSession(AValue: TFRE_DB_FetchSessionCB);
    procedure   SendProxyFrame     (const data:TFRE_DB_RawByteString);
    procedure   WebReceived        (const dataframe :  string);
    procedure   NewProxyChannel    (const proxychan      : IFRE_APSC_CHANNEL ; const event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt);
    procedure   VNC_ProxyChannelDisconnect (const channel : IFRE_APSC_CHANNEL);
    procedure   VNC_ProxyChannelReadData   (const channel : IFRE_APSC_CHANNEL);
  protected
    procedure   BailoutException     (const message:string);
    procedure   BailoutShort         ;
    procedure   Handle_VNC_Framing   (const channel : IFRE_APSC_CHANNEL);
    procedure   Setup_VNC_Base64_ProxyMode;
    procedure   Setup_FirmOS_VC      (const host:string; const port:integer);
    procedure   Setup_ChristmasMode  ;
    procedure   Setup_FirmOS_FREDB   (const sessionkey:string);
    destructor  Destroy              ; override;
    procedure   Finalize             ;
    function    Implementor          : TObject;
    function    Implementor_HC       : TObject;
    function    GetInfoForSessionDesc     : String;
  public
    procedure   ClientConnected      ; override;
    procedure   DisconnectChannel    (const channel:IFRE_APSC_CHANNEL); override;
    property    OnBindInitialSession : TFRE_DB_FetchSessionCB read FOnBindDefaultSession write SetOnBindDefaultSession;

    procedure   ReceivedFromClient       (const opcode:byte;const dataframe : RawByteString);override; // Receive a CMD Frame from WEB Browser !
    procedure   Send_ServerClient        (const CMD_Answer: IFRE_DB_COMMAND);
    procedure   DeactivateSessionBinding (const from_session : boolean=false);
    procedure   UpdateSessionBinding     (const new_session : TObject);

    procedure   Default_Provider           (uri:TFRE_HTTP_URI); // default WWW Provider (not WS)
    function    GetChannel                 :IFRE_APSC_CHANNEL;
  end;


implementation

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.SendProxyFrame(const data: TFRE_DB_RawByteString);
var enc:string;
begin
  if FNo_Base64 then begin
    SendToClient(data,true);
  end else begin
    enc := GFRE_BT.Base64Encode(data);
    SendToClient(enc,false);
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.ReceivedFromClient(const opcode: byte; const dataframe: RawByteString);
var
  in_params,obj,res_obj         : IFRE_DB_Object;
  lmethod,lContent,lContentType : String;
  cmd                           : IFRE_DB_COMMAND;
  class_name                    : TFRE_DB_String;
  method_name                   : TFRE_DB_String;
  request_id                    : TFRE_DB_String;
  request_typ                   : TFRE_DB_String;
  input                         : IFRE_DB_Object;
  output                        : IFRE_DB_Object;
  uidp                          : TFRE_DB_GUIDArray;
  session                       : TFRE_DB_String;
  sw                            : integer;
  binary_text                   : RawByteString;

  procedure _ProcessCloseFrame;
  begin
    _SendCloseFrame;
    {$IFDEF WS_LOG_DEBUG}
    GFRE_DBI.LogWarning(dblc_WEBSOCK,'(!) WEBSOCK REQUESTED CLOSE '+FChannel.GetVerboseDesc);
    {$ENDIF}
    DeactivateSessionBinding;
    FChannel.cs_Finalize;
    FChannel:=nil;
    exit;
  end;

  procedure _SetupInput;
  begin
    try
      cmd        := GFRE_DBI.NewDBCommand;
      request_typ := in_params.Field('RTYPE').AsString;
      with cmd do begin
        case request_typ of
          'S'  : begin CommandType := fct_SyncRequest  ; cmd.SetIsClient(true); end;
          'SR' : begin CommandType := fct_SyncReply    ; end; // Answer to a Server Command
          'E'  : begin CommandType := fct_Error        ; end; // Answer to a Server Command
        end;
        InvokeClass  := uppercase(in_params.Field('CN').AsString);
        InvokeMethod := uppercase(in_params.Field('FN').AsString);
        CommandID    := StrToInt64Def(in_params.Field('RID').AsString,-1);
        UidPath      := in_params.Field('UIDPATH').AsGUIDArr;
        Data         := in_params.Field('PARAMS').AsObject.CloneToNewObject;
        if binary_text<>'' then
          Data.Field('BINCHUNK').AsStream.SetFromRawByteString(binary_text);
        BinaryDataKey:= in_params.Field('bdk').AsString;
        ChangeSession:= '';
      end;
    finally
      in_params.Finalize;
    end;
  end;


  procedure _ProcessTextFrame;
  begin
    {$IFDEF WS_LOG_DEBUG}
    GFRE_DBI.LogDebug(dblc_WS_JSON,'-> '+FChannel.GetVerboseDesc+LineEnding+dataframe);
    {$ENDIF}
    in_params  := GFRE_DBI.JSONObject2Object(dataframe);
    _SetupInput;
    if assigned(FCurrentSession) then
      FCurrentSession.Input_FRE_DB_Command(cmd)
    else
      begin
        FForceIgnoreFurtherInput := true;
        TFRE_DB_UserSession.CLS_ForceInvalidSessionReload(self,cmd);
      end;
  end;

  procedure _ProcessBinaryFrame;
  var len_binary,len_json : Integer;
      jsontext            : RawByteString;
  begin
    len_json    := PInteger(@dataframe[1])^; // Hostbyte order ?
    SetLength(jsontext,len_json);
    Move(dataframe[5],jsontext[1],len_json);
    len_binary  := Length(dataframe)-4-len_json;
    SetLength(binary_text,len_binary);
    Move(dataframe[1+4+len_json],binary_text[1],len_binary);
    {$IFDEF WS_LOG_DEBUG}
    GFRE_DBI.LogDebug(dblc_WS_JSON,'-> '+FChannel.GetVerboseDesc+LineEnding+dataframe);
    {$ENDIF}
    in_params  := GFRE_DBI.JSONObject2Object(jsontext);
    _SetupInput;
    FCurrentSession.Input_FRE_DB_Command(cmd);
  end;

begin
  if FForceIgnoreFurtherInput then
    begin
      if Opcode<>8 then
        begin
         _SendCloseFrame;
        end
      else
        begin
          FChannel.cs_Finalize;
          FChannel:=nil;
        end;
      exit;
    end;
  try
    case FWebsocketMode of
      wsm_INVALID: inherited ReceivedFromClient(opcode,dataframe);
      wsm_VNCPROXY: begin
        if FNo_Base64 then begin
          if Opcode=8 then begin
              {$IFDEF WS_LOG_DEBUG}
              GFRE_DBI.LogDebug(dblc_WEBSOCK,' VNC WEBSOCK CLOSE REQUESTED'+FChannel.GetVerboseDesc);
              {$ENDIF}
              _SendCloseFrame;
              if assigned(FVNCProxyChannel) then
                begin
                 FVNCProxyChannel.cs_Finalize;
                 FVNCProxyChannel:=nil;
                end;
              FChannel.cs_Finalize;
              FChannel:=nil;
            exit;
          end else begin
            WebReceived(dataframe);
          end;
        end else begin
          WebReceived(GFRE_BT.Base64Decode(dataframe));
        end;
      end;
      wsm_FREDB: begin
        case Opcode of
          1:  _ProcessTextFrame;
          2:  _ProcessBinaryFrame;
          8:  _ProcessCloseFrame;
          else
            begin
              {$IFDEF WS_LOG_ERROR}
              GFRE_DBI.LogError(dblc_WS_JSON,'-> '+FChannel.ch_GetVerboseDesc+LineEnding+'Ignoring unsupported WS Frame : '+inttostr(Opcode));
              {$ENDIF}
            end;
            end;
          end;
      wsm_FREDB_DEACTIVATED : begin
      end else begin
        raise EFRE_DB_Exception.Create(edb_ERROR,'BAD WEBSOCKETMODE');
      end;
    end;
  except on e:exception do
   begin
     writeln('*** ERROR PROCESSING WS INPUT : '+e.Message);
     {$IFDEF WS_LOG_ERROR}
     GFRE_DBI.LogError(dblc_WEBSOCK,'ERROR PROCESSING WS INPUT : '+e.Message);
     {$ENDIF}
   end;
  end;
end;



procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.WebReceived(const dataframe: string);
begin
//  writeln('WEB RECEIVED> ',dataframe);
  case FProxyState of
    vps_NOT_CONNECTED: ;
    vps_READ_RFB_VERSION_VNC2WC: begin
      FProxyState:=vps_RFB_VERSION_SENT_WC2VNC;
      FVNCProxyChannel.CH_WriteString(dataframe);
    end;
    vps_RFB_VERSION_SENT_WC2VNC: begin
      FProxyState:=vps_RFB_VERSION_SENT_AUTH_WC2VNC;
      FVNCProxyChannel.CH_WriteString(dataframe);
    end;
    vps_RFB_VERSION_SENT_AUTH_WC2VNC : begin
      writeln('CLIENT INIT ',dataframe);
      FProxyState:=vps_RFB_VERSION_SENT_CLIENT_INIT_WC2VNC;
      FVNCProxyChannel.CH_WriteString(dataframe);
    end;
    vps_SHUFFELING: begin
      FVNCProxyChannel.CH_WriteString(dataframe);
    end;
    else begin
       writeln('RECEIVED FROM WS -> UNHANDLED STATE ',FProxyState);
    end;
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.NewProxyChannel(const proxychan: IFRE_APSC_CHANNEL; const event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  if event = ch_NEW_CS_CONNECTED then
    begin
     FVNCProxyChannel := proxychan;
     FProxyState      := vps_READ_RFB_VERSION_VNC2WC;
     FVNCProxyChannel.ch_SetOnDisconnnect(@VNC_ProxyChannelDisconnect);
     FVNCProxyChannel.ch_SetOnReadData(@VNC_ProxyChannelReadData);
     FVNCProxyChannel.CH_Enable_Reading;
     writeln('PART 2 - SETUP VNC PROXY - CHANMANGER ',proxychan.cs_GetChannelManager.GetID);
    end
  else
    GFRE_BT.CriticalAbort('new vnc proxy connect / error handle this '+inttostr(ord(event)));
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.VNC_ProxyChannelDisconnect(const channel: IFRE_APSC_CHANNEL);
begin
  writeln('VNC <--> DOWN PROXY CHANNEL '+channel.ch_GetVerboseDesc);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.VNC_ProxyChannelReadData(const channel: IFRE_APSC_CHANNEL);
begin
  Handle_VNC_Framing(channel);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.SetOnBindDefaultSession(AValue: TFRE_DB_FetchSessionCB);
begin
  FOnBindDefaultSession:=AValue;
end;


procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.BailoutException(const message: string);
begin
  writeln('BAiLOUT ; ',message);
  raise Exception.Create('VNC2WC PROXY FAIL: '+message);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.BailoutShort;
begin
  GFRE_BT.CriticalAbort('logic data short');
end;



//var Test : specialize TFRE_GetBit<integer>;


procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Handle_VNC_Framing(const channel: IFRE_APSC_CHANNEL);
var data          : String;
    sr            : integer;
    done          : boolean;
    dummy_w       : Word;

    procedure SendProtocolVersion;
    var proto_version : string;
    begin
      if not FBinaryData.ReadAnsiString(12,proto_version) then begin
        writeln('PROTOCOL VERSION SHORT');
        exit;
      end;
      SendProxyFrame(proto_version);
    end;

    procedure SendSecurityResult;
    var res:string;
    begin
      if not FBinaryData.ReadAnsiString(4,res) then begin
        writeln('SECURITY RESULT SHORT');
        exit;
      end;
      SendProxyFrame(res);
    end;

    procedure SendServerInit;
    var rest : String;
    begin
      FProxyState :=  vps_SHUFFELING;
      FBinaryData.ReadAnsiString(FBinaryData.TailLength,rest); //TODO: Hope all is here
      SendProxyFrame(rest);
    end;

    procedure DecodeAndSendFullVNCFrames;
    var FServerToClientmessage : Byte;
        rest                   : string;

    function GetHextileRawSize:integer;
    var w,h : integer;
    begin
      w := 16;
      h := 16;
      if FCurrentHexTile_Y_Short or FCurrentHexTile_X_Short then begin
        if FCurrentHextile_Y_TilesR = FCurrentHextile_Y_Tiles-1 then begin
          h := 16 - ((FCurrentHextile_Y_Tiles * 16) - FCurrentRectCoding.height);
        end;
        if FCurrentHextile_X_TilesR = FCurrentHextile_X_Tiles-1 then begin
          w := 16 - ((FCurrentHextile_X_Tiles * 16) - FCurrentRectCoding.width);
        end;
        result := w*h;
        if result<>256 then begin
          //write('SHRT(',FCurrentHextile_Y_TilesR,' ',FCurrentHextile_X_TilesR,'  ',result,')');
        end;
      end else begin
        result := 256;
      end;
      //writeln('GRS(',Result,')');
    end;

    procedure SendAllRectsCheck;
    var pc : integer;
    begin
      FFBU_SUB_State := fbuss_READ_RECT_DEF;
      if FFBU_ReceivedRectCnt>=FFBU_NumRects then begin // Blast
        pc := FBinaryData.PeekedCount;
        if not FBinaryData.ReadAnsiString(FBinaryData.PeekedCount,rest) then BailoutShort;
        //writeln('****************** >>> SEND RECTS DATA=',pc,' rec=',FFBU_ReceivedRectCnt,' h=',FCurrentRectCoding.height,'x w=',FCurrentRectCoding.width,'@(',FCurrentRectCoding.xpos,',',FCurrentRectCoding.ypos,') T:',FCurrentRectCoding.enctype);
        assert(FFBU_ReceivedRectCnt=FFBU_NumRects);
        SendProxyFrame(rest);
        FFBU_State := fbu_IDLE;
        FFBU_ReceivedRectCnt:=0;
        FBinaryData.StartPeek;
      end;
    end;

    procedure CheckHexTilesDone;inline;
    var
      i: Integer;
    begin
      FFBU_SUB_HEX_STATE := fbussh_HEX_READ_MASK;
      inc(FCurrentHextile_X_TilesR);
      //write('>'+special+'++ ');
      inc(FCurrentHexTile_RCount);
      if FCurrentHextile_X_TilesR=FCurrentHextile_X_Tiles then begin
        inc(FCurrentHextile_Y_TilesR);
        FCurrentHextile_X_TilesR := 0;
      end;
      if (FCurrentHextile_X_TilesR=0) and (FCurrentHextile_Y_TilesR=FCurrentHextile_Y_Tiles) then begin
        //writeln(' || ',FCurrentHexTile_RCount,' - ',FCurrentHextile_X_Tiles*FCurrentHextile_Y_Tiles,' RL=', FBinaryData.TailLength(true));
        //writeln('--');
        FCurrentHexTile_RCount := 0;
        FFBU_State := fbu_CheckRectCntFull;
        SendAllRectsCheck;
      end;
    end;

    procedure _DumpDebugstate(const msg:string='');
    begin
       //writeln(FFBU_SUB_HEX_STATE:40,' ',FFBU_NumRects,'/',FFBU_ReceivedRectCnt,' ',' X/Y Tiles: ',FCurrentHextile_X_Tiles,'/',FCurrentHextile_Y_Tiles,' RX/Y=',FCurrentHextile_X_TilesR,'/',FCurrentHextile_Y_TilesR,' Any=',FCurrentHexTile_AnyCount,' MASK ',FCurrentHexTile_Mask,'    CRWS = ',FCurrentHextile_RAW_SIZE);
    end;

    begin
      while FBinaryData.TailLength(true)>0 do begin
        case FFBU_State of
          fbu_IDLE: begin
            FFBU_State := fbu_READ_MSG_TYPE;
          end;
          fbu_READ_MSG_TYPE: begin
            if not FBinaryData.ReadU8(FServerToClientmessage,true) then BailoutShort;
            FFBU_State := fbu_READ_MSG_FULL;
          end;
          fbu_READ_MSG_FULL: begin
            case FServerToClientmessage of
              0: begin  // FrameBuffer Update
                 if FBinaryData.TailLength(true)>=3 then begin
                   if not FBinaryData.SkipDataBytes(1,True) then BailoutShort;; // padding
                   if not FBinaryData.ReadU16(dummy_w,true,true) then BailoutShort;; // num_rects
                   FFBU_NumRects        := dummy_w;
                   FFBU_State           := fbu_CheckRectCntFull;
                   FFBU_ReceivedRectCnt := 0;
                   FFBU_SUB_State       := fbuss_READ_RECT_DEF;
                   SendAllRectsCheck;
                   //writeln('>>>>>>> NEW FBU RECTS  ',FFBU_NumRects,'  NOW READ THE RECTS ',FBinaryData.TailLength(true));
                 end else begin
                   break; // Need more data
                 end;
              end else begin
                writeln('INVALID SERVER TO CLIENT MESSAGE ',FServerToClientmessage,' ',FBinaryData.TailLength(true));
                writeln(GFRE_BT.Dump_Binary(FBinaryData.GetCurrentPosition(false)-3,32));
                GFRE_BT.CriticalAbort('bad');
              end;
            end;
          end;
          fbu_CheckRectCntFull: begin
              case FFBU_SUB_State of
                fbuss_READ_RECT_DEF: begin
                  if FBinaryData.TailLength(true)>=12 then begin
                    with FCurrentRectCoding do begin
                      if not FBinaryData.ReadU16(xpos,true,true)    then BailoutShort;
                      if not FBinaryData.ReadU16(ypos,true,true)    then abort;
                      if not FBinaryData.ReadU16(width,true,true)  then abort;
                      if not FBinaryData.ReadU16(height,true,true)   then abort;
                      if not FBinaryData.ReadI32(enctype,true,true) then abort;
                    end;
                    FFBU_SUB_State := fbuss_READ_RECT;
                    //with FCurrentRectCoding do
                    //  writeln('>>>>  GOT NEW RECT DEF ',enctype,' xpos=',xpos,' ypos=',ypos,' w=',width,' h=',height,' n=',FFBU_NumRects);
                  end else break; // need more data
                end;
                fbuss_READ_RECT: begin
                  case FCurrentRectCoding.enctype of
                    0: begin //Raw
                         with FCurrentRectCoding do begin
                           //writeln('READ RECT -',width*height*FCurrentBytesPerPixel,' ',FBinaryData.TailLength(true));
                           if FBinaryData.TailLength(true) >= (width*height*FCurrentBytesPerPixel) then begin
                             FBinaryData.SkipDataBytes(width*height*FCurrentBytesPerPixel,true);
                             //writeln('DATA AVAIL ',FBinaryData.TailLength(true),' ',FBinaryData.PeekedCount);
                             inc(FFBU_ReceivedRectCnt);
                             FFBU_SUB_State := fbuss_READ_RECT_DEF;
                             SendAllRectsCheck;
                           end else break; // need more data
                         end;
                       end;
                    1: begin // copyrect
                         if FBinaryData.TailLength(true)>=4 then begin
                           FBinaryData.SkipDataBytes(4,true);
                           inc(FFBU_ReceivedRectCnt);
                           FFBU_SUB_State := fbuss_READ_RECT_DEF;
                           SendAllRectsCheck;
                         end else break;
                       end;
                    5: begin
                          FCurrentHextile_X_Tiles  := Ceil(FCurrentRectCoding.width / 16);
                          FCurrentHextile_Y_Tiles  := Ceil(FCurrentRectCoding.height / 16);
                          FCurrentHexTile_X_Short  := (FCurrentRectCoding.width mod 16) <> 0;
                          FCurrentHexTile_Y_Short  := (FCurrentRectCoding.height mod 16) <> 0;
                          FCurrentHextile_Y_TilesR := 0;
                          FCurrentHextile_X_TilesR := 0;
                          //writeln('HEXTILE x=',FCurrentHextile_x_Tiles,' y=',FCurrentHextile_Y_Tiles,' x 16x16 ',FCurrentHexTile_X_Short,' ',FCurrentHexTile_Y_Short,' w x h=',FCurrentRectCoding.width,'x',FCurrentRectCoding.height);
                          FFBU_SUB_State     := fbuss_DO_CHECK_HEXTILE_COUNT;
                          FFBU_SUB_HEX_STATE := fbussh_HEX_READ_MASK;
                          inc(FFBU_ReceivedRectCnt);

                          if (FCurrentHextile_x_Tiles=2) and (FCurrentHextile_Y_Tiles=2) then begin
                            FCurrentHextile_Y_Tiles:=2;
                          end;
                       end;
                    else begin
                       writeln('UNKNOWN ENCODING TYPE ',FCurrentRectCoding.enctype);
                       BailoutException('UNKNOWN ENCODING TYPE '+inttostr(FCurrentRectCoding.enctype));
                    end;
                  end;
                end;
                fbuss_DO_CHECK_HEXTILE_COUNT: begin
                   case FFBU_SUB_HEX_STATE of
                     fbussh_HEX_READ_MASK: begin
                        if FBinaryData.TailLength(true)>=1 then begin
                          FBinaryData.ReadU8(FCurrentHexTile_Mask,true);
                          //writeln('M',FCurrentHexTile_Mask,'(',GetHextileRawSize,')');
                          //writeln(GFRE_BT.Dump_Binary(FBinaryData.GetCurrentPosition(true)-65,128,false,true));
                          if not (FCurrentHexTile_Mask in [0,1,2,4,6,8,10,12,14,24,26]) then begin
                            writeln('>>>>>>>>>* ');
                            writeln('>>>>>>>>>* ');
                            writeln('>>>>>>>>>* ');
                            writeln('>>>>>>>>>* UNKNOWN MASK ',FCurrentHexTile_Mask);
                            writeln('>>>>>>>>>* ');
                            writeln('>>>>>>>>>* ');
                            writeln('>>>>>>>>>* ');
                            writeln('BADBAD');
                            writeln(GFRE_BT.Dump_Binary(FBinaryData.GetCurrentPosition(true)-257,2048,false,true));
                            abort;
                          end;
                          FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE;
                          if FRE_GetBit(FCurrentHexTile_Mask,0) then begin // Raw
                            //if GetHextileRawSize<>256 then begin
                            //  writeln(GFRE_BT.Dump_Binary(FBinaryData.GetCurrentPosition(true)-257,2048,false,true));
                            //end;
                            FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_RAW;
                            FCurrentHextile_RAW_SIZE := GetHextileRawSize;
                          end else begin
                            if FRE_GetBit(FCurrentHexTile_Mask,1) then begin
                              FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_BG_SPECIFIED;
                            end else
                            if FRE_GetBit(FCurrentHexTile_Mask,2) then begin
                              FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_FG_SPECIFIED;
                            end else
                            if FRE_GetBit(FCurrentHexTile_Mask,3) then begin
                              FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_ANY;
                            end else
                            if FCurrentHexTile_Mask=0 then begin
                              //write(' Z ');
                              CheckHexTilesDone;
                            end else begin
                               writeln('INVALID MASK ',FCurrentHexTile_Mask);
                              abort;
                            end;
                          end;
                        end else break;
                     end;
                     fbussh_HEX_READ_TILE_BG_SPECIFIED: begin
                        if FBinaryData.TailLength(true) >= FCurrentBytesPerPixel then begin
                          //write(' BS');
                          FBinaryData.SkipDataBytes(FCurrentBytesPerPixel,true);
                          if FRE_GetBit(FCurrentHexTile_Mask,2) then begin
                            FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_FG_SPECIFIED;
                          end else
                          if FRE_GetBit(FCurrentHexTile_Mask,3) then begin
                            FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_ANY;
                          end else CheckHexTilesDone;
                        end else break;
                     end;
                     fbussh_HEX_READ_TILE_FG_SPECIFIED: begin
                       if FBinaryData.TailLength(true) >= FCurrentBytesPerPixel then begin
                          //write(' FS');
                          FBinaryData.SkipDataBytes(FCurrentBytesPerPixel,true);
                          if FRE_GetBit(FCurrentHexTile_Mask,3) then begin
                            FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_ANY;
                          end else CheckHexTilesDone;
                        end else break;
                     end;
                     fbussh_HEX_READ_TILE_ANY: begin
                       if FBinaryData.TailLength(true) >= 1 then begin
                          FBinaryData.ReadU8(FCurrentHexTile_AnyCount,true);
                          //write(' AC(',FCurrentHexTile_AnyCount);
                          FFBU_SUB_HEX_STATE := fbussh_HEX_READ_TILE_READ_ANYRECTS;
                        end else break;
                     end;
                     fbussh_HEX_READ_TILE_RAW: begin
                        if FBinaryData.TailLength(true) >= FCurrentHextile_RAW_SIZE*FCurrentBytesPerPixel then begin
                          //write(' R');
                          FBinaryData.SkipDataBytes(FCurrentHextile_RAW_SIZE*FCurrentBytesPerPixel,true);
                          CheckHexTilesDone;
                        end else break;
                     end;
                     fbussh_HEX_READ_TILE_READ_ANYRECTS: begin
                        if FCurrentHexTile_AnyCount=0 then abort;
                        if FRE_GetBit(FCurrentHexTile_Mask,4) then begin // subrects coloured
                          dummy_w := FCurrentBytesPerPixel;
                        end else begin
                          dummy_w := 0;
                        end;
                        if FBinaryData.TailLength(True) >= dummy_w + 2 then begin
                          //write('.',dummy_w+2,'');
                          FBinaryData.SkipDataBytes(dummy_w+2,true);
                          dec(FCurrentHexTile_AnyCount);
                          if FCurrentHexTile_AnyCount=0 then begin
                            FFBU_SUB_HEX_STATE := fbussh_HEX_READ_MASK;
                            CheckHexTilesDone;
                          end;
                        end else begin
                          //writeln('NOT ENOUGH DATA FOR HEXTILE ANYRECT');
                          break;
                        end;
                     end;
                   end;
                end;
              end;
            end;
          end;
      end;
    end;

begin
  //SetLength(data,datacount);
  //sock.ReceiveString(data,datacount,sr);
  //if datacount<>sr then begin
  // // gfre_Bt.CriticalAbort('DC <> SR ??? BAD %d<>%d',[datacount,sr]);
  //  writeln(format('WARNING DC <> SR ??? BAD %d<>%d',[datacount,sr]));
  //end;

  data := channel.CH_ReadString;
  sr   := Length(data);

  FBinaryData.ReadDataCopy(@data[1],sr);
  repeat
    done := true;
    case FProxyState of
      vps_NOT_CONNECTED: ;
      vps_READ_RFB_VERSION_VNC2WC: begin
                                     SendProtocolVersion;
                                   end;
      vps_RFB_VERSION_SENT_WC2VNC: begin
                                     FBinaryData.ReadU16(dummy_w,true);
                                     if dummy_w<>$101 then BailOutException('ONLY SECUREITYTYPE #1#1 SUPORTED');
                                     SendProxyFrame(#1#1);
                                   end;
      vps_RFB_VERSION_SENT_AUTH_WC2VNC: begin
                                     SendSecurityResult;
                                   end;
      vps_RFB_VERSION_SENT_CLIENT_INIT_WC2VNC: begin
                                     SendServerInit;
                                     FCurrentBytesPerPixel := 4;
                                     FBinaryData.StartPeek;
                                   end;
      vps_SHUFFELING: begin
         DecodeAndSendFullVNCFrames;
      end;
      else begin
        writeln('VNC2WC UNHANDLED CASE ',FProxyState);
      end;
    end;
  until done;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Setup_VNC_Base64_ProxyMode;
//var res : EFOS_FCOM_MULTIERROR;
begin
  abort;
  //writeln('BINARY CREATE');
  //FBinaryData := TFRE_BINARYDATA.Create(1024*1024*10,1024*1024*10); // 10 MB Initial binary block / 10 MB Max Block
  ////FBinaryData := TFRE_BINARYDATA.Create(1024*1024*500,1024*1024*500); // 10 MB Initial binary block / 10 MB Max Block
  //FProxyState := vps_NOT_CONNECTED;
  //writeln('ADD A WEBSOCKET VNC CLIENT');
  //res := GFRE_S.AddSocketClient(G_PROXY_ADDRESS,G_PROXY_PORT,fil_IPV4,fsp_TCP,Get_Client_IF);
  //if res <> ese_OK then begin
  //  GFRE_BT.CriticalAbort('cant proxy connect to %s:%d',[G_PROXY_ADDRESS,G_PROXY_PORT]);
  //end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Setup_FirmOS_VC(const host: string; const port: integer);
var res : EFOS_FCOM_MULTIERROR;
begin
  FWebsocketMode := wsm_VNCPROXY;
  FBinaryData := TFRE_BINARYDATA.Create(1024*1024*30,1024*1024*30); // 10 MB Initial binary block / 100 MB Max Block
  //FBinaryData := TFRE_BINARYDATA.Create(1024*1024*100,1024*1024*200); // 10 MB Initial binary block / 10 MB Max Block
  FProxyState := vps_NOT_CONNECTED;
  writeln('SETUP VNC PROXY - CHANMANGER ',FChannel.cs_GetChannelManager.GetID);
  GFRE_SC.AddClient_TCP(host,inttostr(port),'PROX',true,FChannel.cs_GetChannelManager,@NewProxyChannel);
  FNo_Base64 := true;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Setup_ChristmasMode;
begin
  writeln('SEND AA');
  SendToClient('AA',true);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Setup_FirmOS_FREDB(const sessionkey: string);
begin
  FWebsocketMode := wsm_FREDB;
  if FOnBindDefaultSession(self,FCurrentSession,sessionkey,true) then
    begin
      TransFormFunc := @FRE_WAPP_DOJO.TransformInvocation;
      FChannel.ch_SetVerboseDesc('FREDB WS ['+inttostr(FChannel.ch_GetHandleKey)+'] ('+sessionkey+') '+FChannel.ch_GetConnSocketAddr);
    end
  else
    begin
      TransFormFunc := @FRE_WAPP_DOJO.TransformInvocation;
      FCurrentSession:= nil;
    end;
end;

destructor TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Destroy;
begin
  case   FWebsocketMode of
    wsm_INVALID: ;
    wsm_VNCPROXY: begin
      if assigned(FVNCProxyChannel) then
        begin
          FVNCProxyChannel.cs_Finalize;
          FVNCProxyChannel:=nil;
        end;
      if assigned(FChannel) then
        begin
          FChannel.cs_Finalize;
          FChannel:=nil;
        end;
    end;
    wsm_FREDB: begin
                 writeln('WEBSOCKET/SESSIONSOCKET ',FChannel.ch_GetVerboseDesc,' HANDLER DISCONNECTED/FREED -> Clearing Session/Channel Binding');
                   try
                     if assigned(FCurrentSession) then
                       begin
                         FCurrentSession.LockSession;
                         try
                           FCurrentSession.ClearServerClientInterface;
                         finally
                           FCurrentSession.UnlockSession;
                         end;
                       end;
                 except on e:exception do
                   writeln('>> ERROR SENDING WEBSOCK_CLOSE TO CURRENT SESSION ',FCurrentSession.GetSessionID,' : ',e.Message);
                 end;
               end;
    wsm_FREDB_DEACTIVATED: begin
                 writeln('DEACTIVATED SOCKET DESTROYED');
               end;
  end;
  inherited Destroy;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Finalize;
begin
  Free;
end;

function TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Implementor: TObject;
begin
  result := self;
end;

function TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Implementor_HC: TObject;
begin
  raise EFRE_DB_Exception.Create(edb_ERROR,'LOGIC, NO HC Implementor usage allowed');
end;

function TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.GetInfoForSessionDesc: String;
begin
  result := 'WEB:'+FChannel.ch_GetVerboseDesc;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.ClientConnected;
var proto    : string;
    host     : string;
    port     : integer;
    encoding : string;
    session  : string;
begin
  {$IFDEF WS_LOG_DEBUG}
  GFRE_DBI.LogDebug(dblc_WEBSOCK,'WEBSOCK CONNECT / WANT PROTOCOL [%s] from [%s] Sock [%d]',[FWebSocket_Protocol,FChannel.GetVerboseDesc,FChannel.GetHandleKey]);
  {$ENDIF}
  proto := FWebSocket_Protocol;
  if Pos('FirmOS-FREDB',proto)=1 then begin
    session := Copy(proto,14,maxint);
    proto   := 'FirmOS-FREDB';
  end;
  if pos('|',proto)>0 then begin
    proto := GFRE_BT.SepLeft(proto,'|');
    host  := GFRE_BT.SepRight(FWebSocket_Protocol,'|');
    port  := StrToIntDef(GFRE_BT.SepRight(host,'|'),5900);
    host  := GFRE_BT.SepLeft(host,'|');
  end;
  case proto of
    'FirmOS-FREDB'     : Setup_FirmOS_FREDB(session);
    'FirmOS-VC'        : Setup_FirmOS_VC(host,port);
    'base64'           : Setup_VNC_Base64_ProxyMode;
    'X-TEST.firmos.org': Setup_ChristmasMode;
    else begin
      {$IFDEF WS_LOG_DEBUG}
      GFRE_DBI.LogError(dblc_WEBSOCK,'WEBSOCKET - PROTOCOL NOT SUPPORTED!',[]);
      {$ENDIF}
      FChannel.cs_Finalize;
      FChannel:=nil;
    end;
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.DisconnectChannel(const channel: IFRE_APSC_CHANNEL);
var Channeldesc : string;
begin
  try
    Channeldesc := channel.ch_GetVerboseDesc;
    Free;
  except on e:exception do
    {$IFDEF WS_LOG_ERROR}
    GFRE_DBI.LogError(dblc_WEBSOCK,'DISCONNECT CHANNEL '+Channeldesc+'  EXCEPTION '+e.Message);
    {$ENDIF}
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Send_ServerClient(const CMD_Answer: IFRE_DB_COMMAND);
var SC_CMD       : IFRE_DB_Object;
    lContent     : TFRE_DB_RawByteString;
    lContentType : String;
begin
  case CMD_Answer.CommandType of
    fct_SyncRequest,
    fct_SyncReply,
    fct_AsyncRequest: TransFormFunc(FCurrentSession,CMD_Answer.CommandType,CMD_Answer.Data,lContent,lContentType,false,fdbtt_WebSocket);
    fct_Error:        TransFormFunc(FCurrentSession,CMD_Answer.CommandType,TFRE_DB_MESSAGE_DESC.create.Describe('DISPATCH ERROR',CMD_Answer.ErrorText,fdbmt_error),lContent,lContentType,false,fdbtt_WebSocket);
    else              raise EFRE_DB_Exception.Create(edb_INTERNAL,'WRONG COMMAND TYPE');
  end;
  SC_CMD := GFRE_DBI.NewObject;
  case CMD_Answer.CommandType of
    fct_AsyncRequest :  SC_CMD.Field('RTYPE').AsString := 'A'  ;
    fct_SyncRequest  :  SC_CMD.Field('RTYPE').AsString := 'S'  ;
    fct_SyncReply    :  SC_CMD.Field('RTYPE').AsString := 'SR' ;
    fct_Error        :  SC_CMD.Field('RTYPE').AsString := 'E'  ;
  end;
  SC_CMD.Field('CN').AsString       := CMD_Answer.InvokeClass;
  SC_CMD.Field('FN').AsString       := CMD_Answer.InvokeMethod;
  SC_CMD.Field('RID').AsString      := IntToStr(CMD_Answer.CommandID);
  SC_CMD.Field('UIDPATH').AsGUIDArr := CMD_Answer.UidPath;
  SC_CMD.Field('PARAMS').AsObject   := CMD_Answer.CheckoutData;// Data.CloneToNewObject();
  if CMD_Answer.ChangeSession<>'' then begin // Only if The Session is to be Changed
    //writeln('>>*******************************        REQUEST A SESSION CHANGE                ************** FROM ',FCurrentSession.GetSessionID,' TO ',CMD_Answer.ChangeSession);
    SC_CMD.Field('SESSION').AsString  := CMD_Answer.ChangeSession;
  end;
  SC_CMD.Field('OUTPUT').Clear;
  SC_CMD.Field('OUTPUT').AsString   := lContent;
  lContent := SC_CMD.GetAsJSONString(false,false);//  CMD_Answer.AsJSONString;
  SendToClient(lContent,false);

  SC_CMD.Finalize;
  CMD_Answer.Finalize;
  {$IFDEF WS_LOG_DEBUG}
  GFRE_DBI.LogDebug(dblc_WS_JSON,'<- '+FChannel.GetVerboseDesc+LineEnding+lContent);
  {$ENDIF}
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.DeactivateSessionBinding(const from_session: boolean);
var loc : TFRE_DB_UserSession;
    sid : String;
begin
  if from_session then
    begin
      FCurrentSession := nil;
      FWebsocketMode  := wsm_FREDB_DEACTIVATED;
      FChannel.cs_Finalize;
      FChannel:=nil;
    end
  else
    begin
      loc             := FCurrentSession;
      FCurrentSession := nil;
      FWebsocketMode  := wsm_FREDB_DEACTIVATED;
      //GFRE_DBI.LogDebug(dblc_LOCKING,'(!)> '+loc.GetSessionID+ );
      if assigned(loc) then
        begin
          loc.LockSession;
          try
            sid:=loc.GetSessionID;
            loc.ClearServerClientInterface;
          finally
            loc.UnlockSession;
          end;
          {$IFDEF WS_LOG_WARNING}
          GFRE_DBI.LogWarning(dblc_WEBSOCK,'(!) CLEARED THE Session Channel Interface / Binding '+FChannel.GetVerboseDesc+ ' <-> '+sid);
          {$ENDIF}
        end;
    end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.UpdateSessionBinding(const new_session: TObject);
begin
  {$IFDEF WS_LOG_INFO}
  GFRE_DBI.LogInfo(dblc_SESSION,' WS UPDATE SESSION BINDING FOR '+FCHANNEL.GetVerboseDesc+' '+(new_session as TFRE_DB_UserSession).GetSessionID);
  {$ENDIF}
  FCurrentSession := new_session as TFRE_DB_UserSession;
end;


procedure TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.Default_Provider(uri: TFRE_HTTP_URI);
var lContent        : TFRE_DB_RawByteString;
    lFilename       : string;
    i               : Integer;
    class_guid      : TGUID;
    lObject         : String;
    lContentType    : TFRE_DB_String;
    lEtag           : TFRE_DB_String;
    urlEtag         : TFRE_DB_String;
    res_obj         : IFRE_DB_Object;
    in_params       : IFRE_DB_Object;
    rcode           : integer;
    enc_field       : TFRE_DB_NameTypeRL;
    is_attachment   : boolean;
    attachment_filename : string;
    if_none_match   : string;

  procedure StripCacheVersionTag;//inline;
  var idx : NativeInt;
  begin
    idx := POS_FOS_CACHE_TAG(uri.Document);
    if idx>0 then
      uri.Document:=Copy(uri.Document,1,idx-1);
  end;

begin
    ResponseHeader[rh_contentDisposition]     := '';
    ResponseHeader[rh_ETag]                   := '';
    ResponseEntityHeader[reh_LastModified]    := '';
    ResponseEntityHeader[reh_ContentRange]    := '';
    ResponseEntityHeader[reh_Expires]         := '';
    ResponseEntityHeader[reh_ContentEncoding] := '';
    ResponseHeader[rh_AcceptRanges]           := '';

    StripCacheVersionTag;

    if (uri.Document='') or ((length(uri.SplitPath)=0) and (ExtractFileExt(uri.Document)='')) then
      begin //root = application domain, except the stuff some bloke has already put into root, but at least with an extension
        if uri.Document='FirmOS_FRE_WS' then
          begin
            Handle_WS_Upgrade(uri.Document,uri.Method);
          end
        else
          begin
            _SendHttpFileWithRangeCheck('/');
          end;
        exit;
      end
    else
      begin
        if (length(uri.SplitPath)>0) then
          begin
            lFilename:=GFRE_BT.CombineString(uri.SplitPath,DirectorySeparator)+DirectorySeparator+uri.Document;
            if (uri.SplitPath[0]='download') then
              begin
                _SendHttpFileWithRangeCheck(lFilename,true,true,true);
              end
            else
            if (uri.SplitPath[0]='FDBOSF') then
              begin
                enc_field := uri.Document;
                if (Length(uri.SplitPath)=7)  { /FDBOSF/FSessionID/obj_uid/is_attachment(A|N)/mime_type/file_name/force_url_etag/fieldname }
                    and HttpBaseServer.FetchStreamDBO(uri.SplitPath[1],uri.SplitPath[2],enc_field,lContent,lContentType,lEtag) then
                     begin
                       is_attachment       := uri.SplitPath[3]='A';
                       lContentType        := GFRE_BT.HexStr2Str(uri.SplitPath[4]);
                       attachment_filename := GFRE_BT.HexStr2Str(uri.SplitPath[5]);
                       urlEtag             := GFRE_BT.HexStr2Str(uri.SplitPath[6]);
                       if_none_match       := GetHeaderField('If-None-Match');
                       if (lEtag<>'') and (if_none_match=lEtag) then { for "proper" Etag processing the etag force etag must be '-' from generation side (Firefox fix=set the etag) }
                         begin
                           ResponseHeader[rh_ETag] := lEtag;
                           if (if_none_match=lEtag) and (lEtag<>'') then
                             begin
                               _SendHttpResponse(304,'NOT CHANGED',[]);
                             end
                           else
                             begin
                               _SendHttpResponse(200,'OK',[],lContent,lContentType,is_attachment,attachment_filename);
                             end;
                         end
                       else
                         begin
                           ResponseHeader[rh_ETag] := lEtag;
                           _SendHttpResponse(200,'OK',[],lContent,lContentType,is_attachment,attachment_filename);
                         end;
                     end
                else
                  begin
                     _SendHttpResponse(420,'DBO STREAM FETCH FAILED',[]);
                  end;
              end
            else
              begin
                _SendHttpFileWithRangeCheck(lFilename);
              end;
          end
        else
          begin
             lFilename:=uri.Document;
             _SendHttpFileWithRangeCheck(lFilename);
          end;
      end;
end;

function TFRE_WEBSOCKET_SERVERHANDLER_FIRMOS_VNC_PROXY.GetChannel: IFRE_APSC_CHANNEL;
begin
  result := FChannel;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._ClearHeader;
begin
  FillByte(FWSHeader[1],SizeOf(FWSHeader),0);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._EncodeData(data: TFRE_DB_RawByteString; const binary: boolean);
var lLen : Qword;
    npos : integer;
begin
  case FWSSockModeProtoVersion of
    fsm_RFC6455: begin
       _ClearHeader;
       lLen:=Length(data);
       SetLength(FWSHeader,12);
       if binary then begin
         FWSHeader[1]:=chr($82);
       end else begin
         FWSHeader[1]:=chr($81);
       end;
       if lLen<126 then begin
          PByte(@FWSHeader[2])^ := Byte(lLen);
          npos:=2;
       end else
       if llen<=65535 then begin
         PByte(@FWSHeader[2])^ := 126;
         PWord(@FWSHeader[3])^ := NtoBE(Word(lLen));
         npos:=4;
       end else begin
         PByte(@FWSHeader[2])^ := 127;
         PQWord(@FWSHeader[3])^ := NtoBE(QWord(lLen));
         npos:=10;
       end;
       data := Copy(FWSHeader,1,npos)+data;
       FChannel.CH_WriteString(data);
    end;
    fsm_HIXIE: begin
       FChannel.CH_WriteString(#0+data+#255);
    end;
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._SendCloseFrame;
var lLen : Qword;
    npos : integer;
    data : TFRE_DB_RawByteString;
    sw   : integer;
begin
  data:=#3#0;
  case FWSSockModeProtoVersion of
    fsm_RFC6455: begin
       _ClearHeader;
       lLen:=0;
       SetLength(FWSHeader,12);
       FWSHeader[1]:=chr($88);
       PByte(@FWSHeader[2])^ := Byte(lLen);
       npos:=2;
       data := Copy(FWSHeader,1,npos)+data;
       FChannel.CH_WriteString(data);
    end;
    fsm_HIXIE: begin
       FChannel.CH_WriteString(#0#255);
    end;
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._SendHttpResponseStream(const code: integer; const answer: string; const content: TMemoryStream; const contenttype: string; const is_attachment: boolean; const attachment_filename: string);
var att_header : string;
begin
  SetResponseStatusLine(code,answer);
  ResponseEntityHeader[reh_ContentLength]:=inttostr(content.Size);
  if contenttype<>'' then begin
    ResponseEntityHeader[reh_ContentType]:=contenttype;
  end else begin
    ResponseEntityHeader[reh_ContentType]:='';
  end;
  ResponseEntityHeader[reh_Allow]      := 'GET, POST, PUT, DELETE, OPTIONS, TRACE';
  ResponseEntityHeader[reh_Connection] := 'keep-alive';
  if is_attachment then
    begin
      if attachment_filename='' then
        att_header := 'attachment;'
      else
        att_header := 'attachment; filename="'+attachment_filename+'"';
      ResponseHeader[rh_contentDisposition] := att_header;
    end;
  ResponseHeader      [rh_Location]       :='';
  SetResponseHeaders;
  SetEntityHeaders;
  SetBody('');
  SendResponse;
  FChannel.CH_WriteBuffer(content.Memory,content.Size);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._SendHttpResponse(const code: integer; const answer: string; const answer_params: array of const; const content: string; const contenttype: string; const is_attachment: boolean; const attachment_filename: string);
var att_header : string;
begin
  SetResponseStatusLine(code,format(answer,answer_params));
  ResponseEntityHeader[reh_ContentLength]:=inttostr(Length(content));
  if contenttype<>'' then begin
    ResponseEntityHeader[reh_ContentType]:=contenttype;
  end else begin
    ResponseEntityHeader[reh_ContentType]:='';
  end;
  ResponseEntityHeader[reh_Allow]      := 'GET, POST, PUT, DELETE, OPTIONS, TRACE';
  ResponseEntityHeader[reh_Connection] := 'keep-alive';
  //ResponseEntityHeader[reh_Expires]    := GFRE_DT.ToStrHTTP(GFRE_DT.Now_UTC+cFRE_DBT_1_YEAR);
  //if assigned(FSessionCookie) then begin
  //  ResponseEntityHeader[reh_SetCookie]  := FSessionCookie.CookieString;
  //end;
  if is_attachment then
    begin
      if attachment_filename='' then
        att_header := 'attachment;'
      else
        att_header := 'attachment; filename="'+attachment_filename+'"';
      ResponseHeader[rh_contentDisposition] := att_header;
    end;
  ResponseHeader      [rh_Location]       :='';
  SetResponseHeaders;
  SetEntityHeaders;
  SetBody(content);
  SendResponse;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._SendHttpFileWithRangeCheck(lfilename: string; const isAttachment: boolean; const use_dynamic_wwwroot: boolean; const accept_range_requests: boolean);
var
  fn              : string;
  gzip_allowed    : Boolean;
  metae           : TFRE_HTTP_METAENTRY;
  range           : string;
  accept_encoding : string;
  if_none_match   : string;

  procedure _ProcessRangeRequestAndSendFile;
  var
    info            : stat;
    flength         : NativeInt;
    frangetype      : string;
    frangestart     : string;
    frangeend       : string;
    foffset         : NativeInt;
    fend            : NativeInt;

  begin
      if fpstat(fn,info)<>0 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'Stat for file '+fn+' failed!');
      flength := info.st_size;
      fend    := flength;
      foffset := 0;
      {$IFDEF HTTP_LOG_DEBUG}
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Range: %s',[range]);
      {$ENDIF}
      frangetype  := GFRE_BT.SplitString(range,'=');
      if frangetype<>'bytes' then
        begin
          _SendHttpResponse(416,'REQUEST RANGE OTHER THAN BYTES NOT SUPPORTED',[]);
          exit;
        end;
      if Pos(',',range)>0 then
        begin
          _SendHttpResponse(416,'MULTIPLE REQUEST RANGES NOT SUPPORTED',[]);
          exit;
        end;
      frangestart := GFRE_BT.SplitString(range,'-');
      {$IFDEF HTTP_LOG_DEBUG}
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Rangestart: %s',[frangestart]);
      {$ENDIF}
      if length(range)>0 then
        frangeend := range;
      {$IFDEF HTTP_LOG_DEBUG}
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Rangeend: %s',[frangeend]);
      {$ENDIF}
      if (length(frangestart)=0) and (length(frangeend)=0) then
        begin
          _SendHttpResponse(416,'NO START OR END RANGE DEFINED',[]);
          exit;
        end;
      if length(frangestart)>0 then
        foffset := StrToInt64Def(frangestart,-1);
      if length(frangeend)>0 then
        fend    := StrToInt64Def(frangeend,-1);
      {$IFDEF HTTP_LOG_DEBUG}
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Rangeoffset: %d',[foffset]);
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Totallength: %d',[flength]);
      {$ENDIF}
      if (foffset=-1) or (fend=-1) then
        begin
          _SendHttpResponse(416,'COULD NOT PARSE RANGE',[]);
          exit;
        end;
      if foffset>flength then
        begin
          _SendHttpResponse(416,'RANGE START IS LARGER THAN FILESIZE',[]);
          exit;
        end;
      if fend>flength then
        begin
          _SendHttpResponse(416,'RANGE END IS LARGER THAN FILESIZE',[]);
          exit;
        end;

      flength:= fend-foffset;
      {$IFDEF HTTP_LOG_DEBUG}
      GFRE_DBI.LogDebug(dblc_HTTP_REQ,'Rangelength: %d',[flength]);
      {$ENDIF}
      if flength<=0 then
        begin
          _SendHttpResponse(416,'RANGE LENGTH IS EQUAL OR BELOW ZERO',[]);
          exit;
        end;
      ResponseEntityHeader[reh_ContentRange] :=' bytes '+IntToStr(foffset)+'-'+IntToStr(foffset+flength)+'/'+inttostr(info.st_size);
      SetETagandLastModified(fn,info.st_size,GFRE_DT.DateTimeToDBDateTime64(FileDateToDateTime(info.st_mtime))); { Setting eTag here, to prevent 2nd stat in __SendHttpFile}
      __SendHttpFile(fn,true,isAttachment,foffset,flength,accept_range_requests);
  end;

begin
  accept_encoding := GetHeaderField('Accept-Encoding');
  range           := GetHeaderField('Range');
  if_none_match   := GetHeaderField('If-None-Match');

  gzip_allowed := pos('gzip',accept_encoding)>0;
  if lfilename<>'/' then
    begin
      if not use_dynamic_wwwroot then
        begin
          fn := cFRE_SERVER_WWW_ROOT_DIR+DirectorySeparator+lfilename
        end
      else
        begin
          fn := cFRE_SERVER_WWW_ROOT_DYNAMIC+DirectorySeparator+lfilename
        end;
    end
  else
    begin
      fn:=cFRE_SERVER_WWW_ROOT_DIR+DirectorySeparator+cFRE_SERVER_WWW_ROOT_FILENAME;
      lfilename:=cFRE_SERVER_WWW_ROOT_FILENAME;
    end;

  if cFRE_USE_STATIC_CACHE=true then { using the cache, so use metadata - if not metaentry is possibly (while debugging, most certainly, wrong) }
    HttpBaseServer.FetchMetaEntry(fn,metae)
  else
    metae:=nil;

  if assigned(metae) then
    begin { When metadataexist, no range query is implemented by now }
      if if_none_match=metae.ETag then
        begin
          _SendHttpResponse(304,'NOT CHANGED',[]);
          exit;
        end;
      if metae.Cached then
        begin { cached }
          if gzip_allowed and metae.HasZippedCache then
            begin
              ResponseEntityHeader[reh_ContentEncoding] := 'gzip';
              ResponseEntityHeader[reh_LastModified]    := GFRE_DT.ToStrHTTP(metae.ModificationDate);
              ResponseHeader[rh_ETag]                   := metae.ETag;
              ResponseEntityHeader[reh_Expires]         := GFRE_DT.ToStrHTTP(GFRE_DT.Now_UTC+cFRE_DBT_1_YEAR);
              _SendHttpResponseStream(200,'OK',metae.ContentZipped,metae.MimeType);
              exit;
            end
          else
            begin
               ResponseEntityHeader[reh_LastModified]    := GFRE_DT.ToStrHTTP(metae.ModificationDate);
               ResponseHeader[rh_ETag]                   := metae.ETag;
               ResponseEntityHeader[reh_Expires]         := GFRE_DT.ToStrHTTP(GFRE_DT.Now_UTC+cFRE_DBT_1_YEAR);
               _SendHttpResponseStream(200,'OK',metae.ContentUnZipped,metae.MimeType);
               exit;
            end
        end
      else
        begin { not cached }
          _SendHttpFileMetadata(metae,gzip_allowed and metae.ZippedExist);
          exit;
        end;
    end
  else
    begin {No metadata availlable}
       if (use_dynamic_wwwroot) or (not cFRE_USE_STATIC_CACHE) then
         begin
           if length(range)<>0 then
              _ProcessRangeRequestAndSendFile
           else
             __SendHttpFile(fn,false,isAttachment,0,0,accept_range_requests);
         end
       else
         begin
           _SendHttpResponse(404,'NOT FOUND (NO META)',[]);
         end;
    end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.__SendHttpFile(const lfilename: string; const ispartial: boolean; const isAttachment: boolean; const offset: NativeUint; const len: NativeUint ; const accept_range_request : boolean = false);
var fh : THandle;
    m  : TMemoryStream;

  procedure _OpenFile;
  begin
    fh := FileOpen(lfilename,fmOpenRead+fmShareDenyNone);
    if fh=-1 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not get filehandle for '+lfilename+' !');
  end;

  procedure SetCommonFields(const data_len : NativeUint);
  begin
    if isAttachment then
       ResponseHeader[rh_contentDisposition] := 'attachment; filename="'+ExtractFileName(lfilename)+'"';
    if accept_range_request then
       ResponseHeader[rh_AcceptRanges]  := 'bytes';
    ResponseEntityHeader[reh_ContentLength] := inttostr(data_len);
    ResponseEntityHeader[reh_ContentType]   := FREDB_Filename2MimeType(lFilename);
    ResponseEntityHeader[reh_Allow]         := 'GET, POST, PUT, DELETE, OPTIONS, TRACE';
    ResponseEntityHeader[reh_Connection]    := 'keep-alive';
    ResponseEntityHeader[reh_Expires]       := GFRE_DT.ToStrHTTP(GFRE_DT.Now_UTC+cFRE_DBT_1_YEAR);
    ResponseHeader      [rh_Location]       :='';
    SetResponseHeaders;
    SetEntityHeaders;
    SetBody(''); // no data here
    SendResponse;
  end;

  procedure _SendPartial;
  begin
    _OpenFile;
    SetResponseStatusLine(206,format('Partial Content',[]));
    SetCommonFields(len);
    if C_USE_LIBEVENT_FILEBUFFER then
      begin
        FChannel.CH_WriteOpenedFile(fh,offset,len);
      end
    else
      begin
        FileClose(fh);
        m:=TMemoryStream.Create;
        m.LoadFromFile(lfilename);
        FChannel.CH_WriteBuffer(m.Memory+offset,len);
        m.free;
      end;
  end;

  procedure _SendFull;
  var info     : stat;
      flength  : NativeUint;
  begin
    if fpstat(lfilename,info)<>0 then
      begin
        _SendHttpResponse(404,'NOT FOUND (STAT)',[]);
        exit;
      end;
    flength := info.st_size;
    _OpenFile;
    SetResponseStatusLine(200,format('OK',[]));
    SetETagandLastModified(lfilename,info.st_size,GFRE_DT.DateTimeToDBDateTime64(FileDateToDateTime(info.st_mtime)));
    SetCommonFields(flength);
    if C_USE_LIBEVENT_FILEBUFFER then
      begin
       FChannel.CH_WriteOpenedFile(fh,0,flength);
      end
    else
      begin
        FileClose(fh);
        m:=TMemoryStream.Create;
        m.LoadFromFile(lfilename);
        FChannel.CH_WriteBuffer(m.Memory,m.Size);
        m.free;
      end;
  end;

begin
  if ispartial then
    _SendPartial
  else
    _SendFull;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE._SendHttpFileMetaData(const metae: TFRE_HTTP_METAENTRY; const send_zipped: boolean);
var fn          : string;
    fh          : THandle;
    info        : Stat;
    flength     : NativeUint;

begin
 if send_zipped then
   begin
     fn := metae.Filename+'.gzip';
     ResponseEntityHeader[reh_ContentEncoding] := 'gzip';
     flength := metae.ZippedSize;
   end
 else
   begin
     fn := metae.Filename;
     flength := metae.Size;
   end;

 if FileExists(fn) then
   begin
     ResponseEntityHeader[reh_LastModified]    := GFRE_DT.ToStrHTTP(metae.ModificationDate);
     ResponseHeader[rh_ETag]                   := metae.ETag;
     ResponseEntityHeader[reh_Expires]         := GFRE_DT.ToStrHTTP(GFRE_DT.Now_UTC+cFRE_DBT_1_YEAR);
     ResponseEntityHeader[reh_ContentLength]   := inttostr(flength);

     fh := FileOpen(fn,fmOpenRead+fmShareDenyNone);
     if fh=-1 then
       raise EFRE_DB_Exception.Create(edb_INTERNAL,'could not get filehandle for '+fn+' !');
     SetResponseStatusLine(200,'OK');
     //if isAttachment then
     //  ResponseHeader[rh_contentDisposition] := 'attachment; filename="'+ExtractFileName(lfilename)+'"';

     ResponseEntityHeader[reh_ContentType]   := metae.MimeType;
     ResponseEntityHeader[reh_Allow]         := 'GET, POST, PUT, DELETE, OPTIONS, TRACE';
     ResponseEntityHeader[reh_Connection]    := 'keep-alive';
     ResponseHeader      [rh_Location]       :='';
     SetResponseHeaders;
     SetEntityHeaders;
     SetBody(''); // no data here
     SendResponse;
     FChannel.CH_WriteOpenedFile(fh,0,flength);
   end
 else
   begin
     _SendHttpResponse(404,'NOT FOUND',[]);
   end;
end;


procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.Handle_WS_Upgrade(const uri: string; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
var FHost       : String;
    Fkey        : string;
    FDecKey     : string;
    field       : string;
    FVersion    : integer;
    FhixieAuth  : string;

  procedure BadProto(const s:String);
  begin
    writeln('>400 : ',s);
    SetResponseStatusLine(400,'FOS WS SRV BAD PROTO | '+s);
    ResponseEntityHeader[reh_SecWebSocketVersion]:='13, 8';
    ResponseEntityHeader[reh_ContentLength]:='0';
    ResponseEntityHeader[reh_ContentType]:='';
    SetResponseHeaders;
    SetEntityHeaders;
    SetBody('');
    SendResponse;
  end;

  procedure UpgradeOK;
  begin
    //writeln('>101 : UPGRADING ');
    SetResponseStatusLine(101,'Switching Protocols');
    ResponseEntityHeader[reh_Upgrade]:='websocket';
    ResponseEntityHeader[reh_Connection]:='Upgrade';
    ResponseEntityHeader[reh_SecWebSocketAccept]:=GFRE_BT.Base64Encode(GFRE_BT.SHA1String(Fkey+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
    //ResponseEntityHeader[reh_SecWebSocketProtocol]:='base64';
    ResponseEntityHeader[reh_SecWebSocketProtocol]:=FWebSocket_Protocol;
    ResponseEntityHeader[reh_SecWebSocketVersion]:='13, 8';
    //writeln('reh_SecWebSocketAccept: ',ResponseEntityHeader[reh_SecWebSocketAccept]);
    ResponseEntityHeader[reh_ContentLength]:='0';
    ResponseEntityHeader[reh_ContentType]:='';
    SetResponseHeaders;
    SetEntityHeaders;
    SetBody('');
    SendResponse;
  end;

  procedure BadVersion;
  begin
    writeln('>400 : BAD VERSION');
    SetResponseStatusLine(400,'FOS WS SRV BAD VERSION');
    ResponseEntityHeader[reh_ContentLength]:='0';
    ResponseEntityHeader[reh_ContentType]:='';
    ResponseEntityHeader[reh_SecWebSocketVersion]:='13, 8';
    SetResponseHeaders;
    SetEntityHeaders;
    SetBody('');
    SendResponse;
  end;

  procedure  ProcessOldWSHandshake;
  var key1,key2,key3  : string;
      key1n,key2n,j,i : integer;
      challengestring : string;
      p               : PByte;
      cauth          : String;
      origin         : string;

      function CalcScaled(const key:string):integer;
      var spaces : integer;
          key_nu : int64;
          key_n  : string;
          i      : integer;
      begin
        spaces := 0;
        for i:=1 to Length(key) do begin
           if key[i]=' ' then begin
             inc(spaces);
           end else
           if key[i] in ['0','1','2','3','4','5','6','7','8','9'] then begin
             key_n := key_n+key[i];
           end;
        end;
        key_nu := StrToInt64(key_n);
        result := key_nu div spaces;
        if (key_nu div spaces)*spaces <> key_nu then raise EFRE_DB_Exception.Create('old WS protocol fail A');
      end;

      procedure UpgradeHixieOK;
      begin
        SetResponseStatusLine(101,'WebSocket Protocol Handshake');
        ResponseEntityHeader[reh_Upgrade]:='WebSocket';
        ResponseEntityHeader[reh_Connection]:='Upgrade';
        //ResponseEntityHeader[reh_SecWebSocketAccept]:=GFRE_BT.Base64Encode(GFRE_BT.SHA1String(Fkey+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11'));
        ResponseEntityHeader[reh_SecWebSocketProtocol]     := FWebSocket_Protocol;
        ResponseEntityHeader[reh_old_SecWebSocketOrigin]   := origin;
        ResponseEntityHeader[reh_old_SecWebSocketLocation] := 'ws://'+cFRE_WebServerLocation_HixiedWS+'/FirmOS_FRE_WS';
        ResponseEntityHeader[reh_ContentLength]:='16';
        ResponseEntityHeader[reh_ContentType]:='';
        SetResponseHeaders;
        SetEntityHeaders;
        SetBody(cauth);
        SendResponse;
      end;

  begin
    field := trim(GetHeaderField('Upgrade'));
    if lowercase(field)<>'websocket' then begin BadProto('no websocket in Upgrade header:'+field);exit; end;
    key1 := trim(GetHeaderField('Sec-WebSocket-Key1'));
    key2 := trim(GetHeaderField('Sec-WebSocket-Key2'));
    origin := trim(GetHeaderField('Origin'));
    FWebSocket_Protocol := GetHeaderField('Sec-WebSocket-Protocol');
    //key1 := '3e6b263  4 17 80';
    //key2 := '17  9 G`ZD9   2 2b 7X 3 /r90';
    key3 := FindOldWS_HickseyContent;
    //key3 := #$57#$6A#$4E#$7D#$7C#$4D#$28#$36;
    if Length(key3)<>8 then raise Exception.Create('old WS protocol fail B');
    key1n := CalcScaled(key1);
    key2n := CalcScaled(key2);
    SetLength(challengestring,16);
    j:=1;
    p := @key1n;
    for i:=4 downto 1 do begin
       challengestring[j] := Char(p[i-1]);
       inc(j);
    end;
    p := @key2n;
    for i:=4 downto 1 do begin
       challengestring[j] := Char(p[i-1]);
       inc(j);
    end;
    for i:=1 to 8 do begin
       challengestring[j] := key3[i];
       inc(j);
    end;
     cauth := GFRE_BT.HashString_MD5(challengestring);
     UpgradeHixieOK;
     FWSSockModeProtoVersion := fsm_Hixie;
  end;

begin
  //writeln('DISPATCH TEST ',uri,' ',method);
  if method<>rprm_GET then begin   // RFC 6455 - 4.2.1 - 1
    Unsupported;
  end else begin
    FHost          :=  GetHeaderField('Host');
    FVersion       := StrToIntDef(trim(GetHeaderField('Sec-WebSocket-Version')),0);
    FWSStartOpcode := 255;
    FWSDataframe   := '';
    if FVersion=0 then begin // Try Safari / Hicksy
       ProcessOldWSHandshake;
       FUpgraded   := true;
       ClientConnected;
    end else begin
      //writeln('HOST: ',FHost);  // RFC 6455 - 4.2.1 - 2
      field := trim(GetHeaderField('Upgrade'));
      if lowercase(field)<>'websocket' then begin BadProto('no websocket in Upgrade header:'+field);exit; end;
      if pos('upgrade',lowercase(GetHeaderField('Connection')))=0 then begin BadProto('no upgrade in connection');exit; end;
      FKey  := trim(GetHeaderField('Sec-WebSocket-Key'));
      //writeln('FKEY: <',fkey,'>');
      try
        FDecKey := GFRE_BT.Base64Decode(Fkey);
      except
        FDecKey:='';
      end;
      if length(FDecKey)<>16 then begin BadProto('sec ws key len not 16');exit; end;
      //writeln('Sec-WebSocket-Key: ',Fkey); // RFC 6455 - 4.2.1 - 5  must be 16 byte len
      //writeln('Sec-WebSocket-Version: ',FVersion); // RFC 6455 - 4.2.1 - 6  -> 13 for RFC
      if ((FVersion<13) and (FVersion<>8)) then begin
        BadVersion;
        exit;
      end;
      //opt
      //writeln('|Origin|: ',GetHeaderField('Origin')); // RFC 6455 - 4.2.1 - 7
      //writeln('|Sec-WebSocket-Protocol|: ',GetHeaderField('Sec-WebSocket-Protocol')); // RFC 6455 - 4.2.1 - 8
      FWebSocket_Protocol := GetHeaderField('Sec-WebSocket-Protocol');
      //writeln('|Sec-WebSocket-Extensions|: ',GetHeaderField('Sec-WebSocket-Extensions')); // RFC 6455 - 4.2.1 - 9
      UpgradeOK;
      FUpgraded:=true;
      FWSSockModeProtoVersion := fsm_RFC6455;
      ClientConnected;
    end;
  end;
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.ReadChannelData(const channel: IFRE_APSC_CHANNEL);
var FOpcode      : Byte;

  procedure DecodeWS_Proto;
  var i                     : Integer;
      lRecLen               : integer;
      FEnd                  : PByte;
      lfull_len             : integer;
      continue_short_decode : boolean;
      ws_data               : string;
      bailout               : NativeInt;

      label    again;

    procedure UnMaskDecode;
    var i : NativeInt;
    begin
      if FMask then begin
        for i:=1 to Length(FDecodeFrame) do begin
          FDecodeFrame[i] := char(BYTE(FDecodeFrame[i]) XOR byte(FMakey[(i-1) mod 4]));
        end;
      end;
      FWSDataframe:=FWSDataframe+FDecodeFrame;
      if FFIN_Flag then
        begin
          try
            ReceivedFromClient(FWSStartOpcode,FWSDataframe);
          finally
            FWSStartOpcode:=255;
            FWSDataframe:=''
          end;
        end
    end;

    function QueryRestLen:integer;
    begin
      result := FEnd+1-FByte;
    end;

    procedure ReadByteAdvance(const cnt:integer=1);
    begin
      inc(FByte,cnt);
      if FByte>(FEnd+1) then begin
        writeln('RESTLEN ',QueryRestLen);
        GFRE_BT.CriticalAbort('READ TOO MUCH ON '+inttostr(cnt)+' '+inttostr(length(ws_data)));
      end;
    end;

    function TryDecodeShortFrame(const short_len:integer):boolean;
    begin
      FFIN_Flag := (FByte[0] and $80) = $80;
      FOpcode   := FByte[0] and $7F;
      FMask     := (FByte[1] and $80) = $80;
      FLen      := (FByte[1] and $7F);
      if not FMask then gfre_bt.CriticalAbort('GOT A UNMASKED SHORTFRAME');
      if not FFIN_Flag then gfre_bt.CriticalAbort('GOT A SHORTFRAME WITHOUT FIN');
      if FLen=(short_len-6) then begin // 4 byte mask + 2 byte prot
        exit(true);
      end else begin
        // not a shortframe;
      end;
      result := false;
    end;

  begin
    ws_data := channel.CH_ReadString;
    if Length(ws_data)<1 then GFRE_BT.CriticalAbort('YOU MUST PROVIDE DATA LEN='+inttostr(Length(ws_data)));
    if FHeaderShort then begin
      FHeaderShort:=false;
      ws_data := FShortData+ws_data;
      FShortData:='';
      FFrameNotDone:=false;
    end;
    FEnd      := PByte(@ws_data[length(ws_data)]);
    if not FFrameNotDone then begin
      FFrameNotDone:=true;
      FByte     := PByte(@ws_data[1]);
      bailout   := 10;
     again:
      dec(bailout);
      if bailout=0 then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'bailout reached in ws decode');
      lfull_len := QueryRestLen;
      if lfull_len<12 then begin // TODO Hang - Check for only 1 single packet and no stuffer behind
        //writeln('-- REQUESTLEN SHORT -- possible STALL ',lfull_len,' ',ClassName,'  - ',FWSSockModeProtoVersion);
        if lfull_len>=6 then begin // try decode shortframe;
          continue_short_decode := TryDecodeShortFrame(lfull_len);
        end;
        if not continue_short_decode then begin // bailout
          FHeaderShort := True;
          lfull_len:=FEnd-FByte+1;
          SetLength(FShortData,lfull_len);
          Move(FByte^,FShortData[1],lfull_len);
          exit;
        end;
      end;
      FFIN_Flag := (FByte^ and $80) = $80;
      FOpcode   := FByte^ and $7F;
      ReadByteAdvance;
      FMask     := (FByte^ and $80) = $80;
      FLen      := FByte^ and $7F;
      if (FWSStartOpcode=255) and (FOpcode<>0) then
        FWSStartOpcode := FOpcode; { store starting opcode }
      ReadByteAdvance;
      if FLen=127 then begin     // self.FLen
        FLen := BEtoN(PQWord(FByte)^);
        ReadByteAdvance(8);
      end else
      if FLen=126 then begin
        FLen := BEtoN(PWord(FByte)^);
        ReadByteAdvance(2);
      end;
      if FMask then begin
        FMakey[0] := FByte^; ReadByteAdvance;
        FMakey[1] := FByte^; ReadByteAdvance;
        FMakey[2] := FByte^; ReadByteAdvance;
        FMakey[3] := FByte^; ReadByteAdvance;
      end;
      SetLength(FDecodeFrame,FLen); // self.flen  self.FFin_flag self.fmask
      //FillChar(FDecodeFrame[1],Flen,$EA);
      FGotLen   :=  Fend - FByte + 1;
      if FGotLen>FLen then begin
        Move(FByte^,FDecodeFrame[1],FLen); // ==
        UnMaskDecode;
        ReadByteAdvance(FLen);
        goto again;
      end else
      if FGotLen<Flen then begin
        Move(FByte^,FDecodeFrame[1],FGotLen); // ==
        FReceiveRest := FLen-FGotLen;
      end else begin
        Move(FByte^,FDecodeFrame[1],FLen); // ==
        FFrameNotDone:=false;
        UnMaskDecode;
      end;
    end else begin
      lRecLen := Length(ws_data);
      FByte   := @ws_data[1];
      if FGotLen+lRecLen>FLen then begin
        Move(FByte^,FDecodeFrame[FGotLen+1],FLen-FGotLen); // ==
        ReadByteAdvance(FLen-FGotLen);
        UnMaskDecode;
        goto again;
      end else
      if FGotLen+lRecLen<FLen then begin
        Move(FByte^,FDecodeFrame[FGotLen+1],lRecLen); // == self.fgotlen self.freceiverest
        dec(FReceiveRest,lRecLen);
        inc(FGotLen,lRecLen);
        ReadByteAdvance(lRecLen);
      end else begin
        Move(FByte^,FDecodeFrame[FGotLen+1],lRecLen); // ==
        FReceiveRest:=0;
        FFrameNotDone:=false;
        UnMaskDecode;
      end;
    end;
  end;

  procedure DecodeWS_Proto_Old;
  var ws_data:string;
  begin
    writeln('>> WARNING OLD WS PROTOCOL RECEIVED ...');
    if Length(ws_data)>0 then begin
      if (ws_data[1]=#0) and (ws_data[length(ws_data)]=#255) then begin
        FDecodeFrame:=Copy(ws_data,2,Length(ws_data)-2);
        ReceivedFromClient(FOpcode,FDecodeFrame);
      end else begin
         raise EFRE_DB_Exception.Create('half-frame not handled, or bad data received ...');
      end;
    end;
  end;

begin
  if FUpgraded then begin
    case FWSSockModeProtoVersion of
      fsm_RFC6455: DecodeWS_Proto;
      fsm_HIXIE:   DecodeWS_Proto_Old;
    end;
  end else begin
    inherited ReadChannelData(channel); // Parse Handle HTTP - WS Request;
  end;
end;


procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.SendToClient(data: TFRE_DB_RawByteString; const binary: boolean);
begin
  _EncodeData(data,binary);
end;

procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.ClientConnected;
begin
  abort;
end;


procedure TFRE_WEBSOCKET_SERVERHANDLER_BASE.ReceivedFromClient(const opcode: byte; const dataframe: RawByteString);
var s:String;
begin
  inc(cnt);
//  if cnt>1180 then begin
    //if cnt mod 1000 = 0 then write(' ',cnt,' ');
    //s:=StringOfChar('A',Length(FDecodeFrame));
    //if s<>FDecodeFrame then begin
      writeln(' ',cnt:5,':',length(FDecodeFrame):3,'> '+FDecodeFrame);
    //end;
 // end;

//  end;
end;


end.

