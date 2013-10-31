unit fre_http_srvhandler;

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

//TODO: CASE SENSITIVE !! not rfc compliant

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils,strutils,FRE_DB_INTERFACE,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FOS_TOOL_INTERFACES,FRE_HTTP_TOOLS;

type
  IFRE_HTTP_BASESERVER=interface
    procedure  DispatchHttpRequest   (const connection_object:TObject;const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
    procedure  FetchHullHTML         (var lContent:TFRE_DB_RawByteString;var lContentType:string);
    function   FetchFileCached       (file_path:String;var data:TFRE_DB_RawByteString):boolean;
  end;

  { TFRE_HTTP_CONNECTION_HANDLER }
  TFRE_HTTP_PARSER_REQ_STATE      = (rprs_PARSEREQ_LINE,rprs_PARSE_HEADERS,rprs_PARSEBODY,rprs_DISPATCHREQUEST);
  TFRE_HTTP_PARSER_ERROR          = (rpe_OK,rpe_CONTINUE,rpe_INVALID_REQ_LINE,rpe_INVALID_REQUEST);

  TFRE_HTTP_ResponseHeaders        = (rh_AcceptRanges,rh_Age,rhETag,rh_Location,rh_ProxyAuth,rh_ReryAfter,rh_Server,rh_Vary,rh_WWWAuth,rh_contentDisposition);
  TFRE_HTTP_ResponseEntityHeaders  = (reh_Allow,reh_ContentEncoding,reh_ContentLanguage,reh_ContentLength,reh_ContentMD5,reh_ContentRange,reh_ContentType,reh_Origin,
                                      reh_Expires,reh_LastModified,reh_Connection,reh_CacheControl,reh_SetCookie,reh_Upgrade,reh_SecWebSocketAccept,reh_SecWebSocketProtocol,reh_SecWebSocketVersion,
                                      reh_old_SecWebSocketOrigin,reh_old_SecWebSocketLocation);
const
  CFRE_HTTP_ResponseHeaders : Array [TFRE_HTTP_ResponseHeaders] of String =
                              ('Accept-Ranges','Age','ETag','Location','Proxy-Authenticate','Retry-After','Server','Vary','WWW-Authenticate','Content-Disposition');
  CFRE_HTTP_ResponseEntityHeaders : Array [TFRE_HTTP_ResponseEntityHEaders] of String =
                              ('Allow','Content-Encoding','Content-Language','Content-Length','Content-MD5',
                               'Content-Range','Content-Type','Origin','Expires','Last-Modified','Connection','Cache-Control','Set-Cookie','Upgrade','Sec-WebSocket-Accept','Sec-WebSocket-Protocol','Sec-WebSocket-Version',
                               'Sec-WebSocket-Origin','Sec-WebSocket-Location');
type


  TFRE_HTTP_CONNECTION_HANDLER=class
  private
     FInternalState                       : TFRE_HTTP_PARSER_REQ_STATE;
     FRequest                             : String;
     FRequestURIPos                       : integer;
     FRequestURILen                       : integer;
     FRequestVersionPos                   : integer;
     FRequestVersionLen                   : integer;
     FHeaderPos                           : integer;
     FHeaderLen                           : integer;
     FContentLenght                       : integer;
     FHost                                : string;
     FContentStart                        : integer;
     FHasContent                          : boolean;
     FResponse                            : String;
     //FWriteQ                              : IFOS_LFQ;
     FResponseHeaders                     : Array[TFRE_HTTP_ResponseHeaders] of String;
     FResponseEntityHeaders               : Array[TFRE_HTTP_ResponseEntityHEaders] of String;
     FHttpBase                            : IFRE_HTTP_BASESERVER;
     FReqErrorState                       : TFRE_HTTP_PARSER_ERROR;
     function  GetResponseEntityHeader(idx: TFRE_HTTP_ResponseEntityHeaders): String;
     function  GetResponseHeader(idx: TFRE_HTTP_ResponseHeaders): String;
     procedure SetResponseEntityHeader(idx: TFRE_HTTP_ResponseEntityHeaders; const AValue: String);
     procedure SetResponseHeader(idx: TFRE_HTTP_ResponseHeaders; const AValue: String);
  protected
     FChannel                             : IFRE_APSC_CHANNEL;
     function  GetContent                 : String;
     function  GetRequestHeaders          : string;
     function  GetRequestUri              : string;
     function  GetRequestVersion          : string;
     function  GetHeaderField             (const fieldname:String):string;
     procedure NotFound                   ;
     procedure Unsupported                ;
     procedure InitForNewRequest          ;
     procedure SetResponseStatusLine      (const StatusCode:integer; const ReasonPhrase:String);
     procedure SetResponseHeaders         ;
     procedure SetEntityHeaders           ;
     procedure SetBody                    (const data:string);
     procedure SendResponse               ;
     procedure HttpRequest                (const method:TFRE_HTTP_PARSER_REQUEST_METHOD);
     procedure DispatchRequest   (const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD); virtual;
  public
     FRequestMethod  : TFRE_HTTP_PARSER_REQUEST_METHOD;
  public
     function    HttpBaseServer     :IFRE_HTTP_BASESERVER;
     function    Response           : string;
     constructor Create             (const channel:IFRE_APSC_CHANNEL ; const http_server : IFRE_HTTP_BASESERVER);
     procedure   ReadChannelData    (const channel:IFRE_APSC_CHANNEL);virtual;
     procedure   DisconnectChannel  (const channel:IFRE_APSC_CHANNEL);virtual;
     function    RawRequest         :String;
     function    FindOldWS_HickseyContent : String;
     property    RequestUri         :string read GetRequestUri;
     property    RequestVersion     :string read GetRequestVersion;
     property    RequestHeaders     :string read GetRequestHeaders;
     property    RequestContent     :String read GetContent;
     property    RequestHost        :string read FHost;
     property    ResponseHeader       [idx:TFRE_HTTP_ResponseHeaders]       : String read GetResponseHeader write SetResponseHeader;
     property    ResponseEntityHeader [idx:TFRE_HTTP_ResponseEntityHeaders] : String read GetResponseEntityHeader write SetResponseEntityHeader;
  end;

implementation

{ TOffloadWriteObject }


{ TFRE_HTTP_CONNECTION_HANDLER }

function TFRE_HTTP_CONNECTION_HANDLER.GetRequestUri: string;
begin
  result := Copy(FRequest,FRequestURIPos,FRequestURILen);
end;



function TFRE_HTTP_CONNECTION_HANDLER.GetResponseHeader(idx: TFRE_HTTP_ResponseHeaders): String;
begin
  result := FResponseHeaders[idx];
end;

function TFRE_HTTP_CONNECTION_HANDLER.GetResponseEntityHeader(idx: TFRE_HTTP_ResponseEntityHeaders): String;
begin
  result := FResponseEntityHeaders[idx];
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetResponseEntityHeader(idx: TFRE_HTTP_ResponseEntityHeaders; const AValue: String);
begin
  FResponseEntityHeaders[idx] := AValue;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetResponseHeader(idx: TFRE_HTTP_ResponseHeaders; const AValue: String);
begin
  FResponseHeaders[idx] := AValue;
end;


function TFRE_HTTP_CONNECTION_HANDLER.GetContent: String;
begin
  if FHasContent then begin
    result := Copy(FRequest,FContentStart,FContentLenght);
  end;
end;

function TFRE_HTTP_CONNECTION_HANDLER.GetRequestHeaders: string;
begin
  result := Copy(FRequest,FHeaderPos,FHeaderLen);
end;

function TFRE_HTTP_CONNECTION_HANDLER.GetRequestVersion: string;
begin
  result := Copy(FRequest,FRequestVersionPos,FRequestVersionLen);
end;

function TFRE_HTTP_CONNECTION_HANDLER.GetHeaderField(const fieldname: String): string;
var lStart,lEnd,lNamelen:integer;
begin
  result:='';
  lStart := PosEx(fieldname+':',FRequest,FHeaderPos);
  if lStart>0 then begin
    lNamelen := Length(fieldname)+1;
    lStart   := lStart+lNamelen;
    lEnd     := PosEx(#13#10,FRequest,lStart); //TODO -> FOLDED HEADER FIELDS (LWS)
    result   := trim(Copy(FRequest,lStart,lEnd-lStart));
  end;
end;


procedure TFRE_HTTP_CONNECTION_HANDLER.Unsupported;
begin
  SetResponseStatusLine(500,'FOS SRV UNSUPPORTED REQUEST');
  ResponseEntityHeader[reh_ContentLength]:='0';
  ResponseEntityHeader[reh_ContentType]:='';
  SetResponseHeaders;
  SetEntityHeaders;
  SetBody('');
  SendResponse;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.NotFound;
begin
  SetResponseStatusLine(404,'FOS SRV OBJECT NOT FOUND');
  ResponseEntityHeader[reh_ContentLength]:='0';
  ResponseEntityHeader[reh_ContentType]:='';
  SetResponseHeaders;
  SetEntityHeaders;
  SetBody('');
  SendResponse;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.InitForNewRequest;
begin
  FInternalState := rprs_PARSEREQ_LINE;
  FContentLenght := -1;
  FHeaderPos     := -1;
  FHeaderLen     := -1;
  FContentStart  := -1;
  FRequest       := '';
  FHasContent    := false;
  FRequestURIPos := -1;
  FRequestURILen := -1;
  FResponse      := '';
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetResponseStatusLine(const StatusCode: integer; const ReasonPhrase: String);
begin
  FResponse:='HTTP/1.1 '+Inttostr(StatusCode)+' '+ReasonPhrase+#13#10;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetResponseHeaders;
var i : TFRE_HTTP_ResponseHeaders;
begin
  for i in TFRE_HTTP_ResponseHeaders do begin
    if FResponseHeaders[i]<>'' then begin
      FResponse:=Fresponse+CFRE_HTTP_ResponseHeaders[i]+': '+FResponseHeaders[i]+#13#10;
    end;
  end;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetEntityHeaders;
var i : TFRE_HTTP_ResponseEntityHeaders;
begin
  for i in TFRE_HTTP_ResponseEntityHeaders do begin
    if FResponseEntityHeaders[i]<>'' then begin
      if i=reh_ContentType then begin
        FResponse:=Fresponse+CFRE_HTTP_ResponseEntityHeaders[i]+': '+FResponseEntityHeaders[i]+'; charset=utf-8'+#13#10;
      end else begin
        FResponse:=Fresponse+CFRE_HTTP_ResponseEntityHeaders[i]+': '+FResponseEntityHeaders[i]+#13#10;
      end;
    end;
  end;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.SetBody(const data: string);
begin
  FResponse:=FResponse+#13#10+data;
end;


procedure TFRE_HTTP_CONNECTION_HANDLER.SendResponse;
var i : TFRE_HTTP_ResponseEntityHeaders;
    lResponse:string;
    lPos:integer;
begin
  //GFRE_DBI.LogInfo(dblc_HTTPSRV,'< [%s] [%s]',[GFRE_BT.SepLeft(FResponse,#13#10),FChannel.GetVerboseDesc]);
  //GFRE_DBI.LogDebug(dblc_HTTPSRV,'%s',[FResponse]);
  FChannel.CH_WriteString(FResponse);
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.HttpRequest(const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
begin
  try
    if not assigned(HttpBaseServer) then begin
      DispatchRequest(RequestHost+RequestUri,method);
    end else begin
      HttpBaseServer.DispatchHttpRequest(self, RequestHost+RequestUri,method);
    end;
  except
    on E:Exception do begin
      writeln('HTTP: ERROR : 500 ERROR ON DISPATCH REQUEST '+e.Message);
      SetResponseStatusLine(500,'ERROR ON DISPATCH REQUEST '+e.Message);
      ResponseEntityHeader[reh_ContentLength]:='0';
      ResponseEntityHeader[reh_ContentType]:='';
      SetResponseHeaders;
      SetEntityHeaders;
      SetBody('');
      SendResponse;
    end;
  end;
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.DispatchRequest(const uri: string; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
begin
  abort;
end;

function TFRE_HTTP_CONNECTION_HANDLER.HttpBaseServer: IFRE_HTTP_BASESERVER;
begin
  result := FHttpBase;
end;

function TFRE_HTTP_CONNECTION_HANDLER.Response: string;
begin
  result := FResponse;
end;

constructor TFRE_HTTP_CONNECTION_HANDLER.Create(const channel: IFRE_APSC_CHANNEL; const http_server: IFRE_HTTP_BASESERVER);
begin
  FChannel := channel;
  InitForNewRequest;
  FResponseHeaders        [rh_Server]:='FirmOS FRE HTTP Engine';
  FResponseEntityHeaders  [reh_ContentLength]:='0';
  FHttpBase := http_server;
end;


procedure TFRE_HTTP_CONNECTION_HANDLER.ReadChannelData(const channel: IFRE_APSC_CHANNEL);
var
    data          : string;
    lContinue     : boolean;
    lActualPos    : integer;
    lReqLineLen   : integer;

    label         cont;


  procedure _ParseReqLine;
  var lLineend      : integer;
      lSpacePos     : integer;

      function  _RestCheck(const position,needed : integer):boolean;
      begin
        result := position+needed <= lReqLineLen;
      end;

  begin
    lLineend    := pos(#13#10,FRequest);
    if lLineend=0 then exit; //stay in state
    lActualPos   := 1 ;
    if PosEx('GET',FRequest,lActualPos)=1 then begin
       FRequestMethod := rprm_GET;
       inc(lActualPos,4);
    end else
    if PosEx('POST',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_POST;
      inc(lActualPos,5);
    end else
    if PosEx('PUT',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_PUT;
      inc(lActualPos,4);
    end else
    if PosEx('OPTIONS',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_OPTIONS;
      inc(lActualPos,8);
    end else
    if PosEx('DELETE',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_DELETE;
      inc(lActualPos,7);
    end else
    if PosEx('HEAD',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_HEAD;
      inc(lActualPos,5);
    end else
    if PosEx('TRACE',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_TRACE;
      inc(lActualPos,6);
    end else
    if PosEx('CONNECT',FRequest,lActualPos)=1 then begin
      FRequestMethod := rprm_CONNECT;
      inc(lActualPos,8);
    end else begin
      FReqErrorState := rpe_INVALID_REQUEST;
      exit;
    end;
    //writeln('REQUEST MODE - ',FRequestMethod);
    lSpacePos:=PosEx(' ',FRequest,lActualPos);
    if lSpacePos>1 then begin
       FRequestURIPos := lActualPos;
       FRequestURILen := lSpacePos-lActualPos;
       //writeln('REQUEST URI : ',RequestUri);
       inc(lActualPos,FRequestURILen+1);
    end else begin
      FReqErrorState := rpe_INVALID_REQUEST;
      exit;
    end;
    lSpacePos:=PosEx(#13#10,FRequest,lActualPos);
    if lSpacePos>1 then begin
       FRequestVersionPos := lActualPos;
       FRequestVersionLen := lSpacePos-lActualPos;
       //writeln('REQUEST VERSION  : ',RequestVersion);
       inc(lActualPos,FRequestVersionLen+2);
    end else begin
      FReqErrorState := rpe_INVALID_REQUEST;
      exit;
    end;
    //writeln(lActualPos,'/',lReqLineLen);
    FInternalState  := rprs_PARSE_HEADERS;
    FHeaderPos      := lActualPos;
    lContinue       := lActualPos<lReqLineLen;
  end;

  procedure _ParseHeaders;
  var lHeaderEndPos:integer;

      procedure _EvaluateHeaders;
      begin
        FContentLenght := StrToIntDef(GetHeaderField('Content-Length'),-1);
        FHost          := GetHeaderField('Host');
      end;

  begin
   // writeln('>>PARSEHEADERS ',FHeaderPos);
    if FHeaderPos=-1 then begin
      FHeaderPos:=Posex(#13#10,FRequest)+2;
      if FHeaderPos<=1 then GFRE_BT.CriticalAbort('LOGIC FAILURE');
    end;
    lActualPos := FHeaderPos;
    lHeaderEndPos := PosEx(#13#10#13#10,FRequest,lActualPos);
    if lHeaderEndPos > 1 then begin
      FHeaderLen    := lHeaderEndPos-lActualPos;
      FContentStart := lHeaderEndPos+4; // Theoretical Content Start
      //writeln('HEADERS');
      //writeln(RequestHeaders);
      _EvaluateHeaders;
      //writeln('CLEN ',FContentLenght);
    end;
    lContinue      := lReqLineLen>=(lHeaderEndPos+3);
    FInternalState := rprs_PARSEBODY;
    //writeln('>>PARSEHEADERS ',lContinue,' ',lReqLineLen,' ',lHeaderEndPos,' ',lHeaderEndPos+3);
  end;

  procedure _ParseBody;
  begin
    //writeln('--- PARSEBODY ---');
    if FContentLenght<>-1 then begin
      if lReqLineLen+1-(FContentStart+FContentLenght)>=0 then begin
         FHasContent:=true;
      end else begin
        FReqErrorState := rpe_CONTINUE;
        exit;
      end;
    end else begin
       FHasContent := false;
    end;
    FInternalState := rprs_DISPATCHREQUEST;
    lContinue      := True;
  end;

  procedure _DispatchRequest;
  begin
    //FMyCookie.CookieString:=GetHeaderField('Cookie');
    //GFRE_DBI.LogInfo(dblc_HTTPSRV,'> %s [%s]',[GFRE_BT.SepLeft(FRequest,#13#10),FChannel.GetVerboseDesc]);
    //GFRE_DBI.LogDebug(dblc_HTTPSRV,'%s',[GFRE_BT.SepLeft(FRequest,#13#10#13#10)]);
    HttpRequest(FRequestMethod);
    InitForNewRequest;
    FInternalState:=rprs_PARSEREQ_LINE;
  end;

begin
  FReqErrorState := rpe_OK;
  FRequest    := FRequest + channel.CH_ReadString;
  lReqLineLen := Length(FRequest);
  //writeln('REQ::: ');
  //writeln(FRequest);
  //if datalen=0 then GFRE_BT.CriticalAbort('DATA LEN 0');
  cont:
    lContinue := false;
    case FInternalState of
      rprs_PARSEREQ_LINE     : _ParseReqline;
      rprs_PARSE_HEADERS     : _ParseHeaders;
      rprs_PARSEBODY         : _ParseBody;
      rprs_DISPATCHREQUEST   : _DispatchRequest;
      else begin
        writeln('LOGIC STATE FAILURE ',FInternalState,' ',NativeUint(self));
        abort;
      end;
    end;
    if lContinue then begin
     // writeln('Continuing... ',FInternalState);
      goto cont;
    end;
  //writeln('ERRORSTATE : ',Result);
end;

procedure TFRE_HTTP_CONNECTION_HANDLER.DisconnectChannel(const channel: IFRE_APSC_CHANNEL);
begin

end;

function TFRE_HTTP_CONNECTION_HANDLER.RawRequest: String;
begin
  result := FRequest;
end;

function TFRE_HTTP_CONNECTION_HANDLER.FindOldWS_HickseyContent: String;
var lpos:integer;
begin
  lpos := Pos(#13#10#13#10,FRequest);
  if lpos >0 then begin
    result := Copy(FRequest,lpos+4,maxint);
  end;
end;


end.

