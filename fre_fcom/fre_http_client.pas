unit fre_http_client;

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
  Classes, SysUtils,strutils,FRE_APS_INTERFACE,fre_http_tools;

type
    TFRE_SIMPLE_HTTP_CLIENT = class;
    TFRE_SIMPLE_HTTP_CONTENT_CB = procedure(const sender : TFRE_SIMPLE_HTTP_CLIENT ; const http_status,content_len : NativeInt ;  const contenttyp : string ; content : PByte) is nested;

    { TFRE_SIMPLE_HTTP_CLIENT }

    TFRE_SIMPLE_HTTP_CLIENT=class
    private
    const
      cHTTPLE = #13#10;
    type
      httpstate=(hs_WaitResponse,hs_ParseHeaders);
    var
      FState        : httpstate;
      FHttpProtocol : String[20];
      FUserAgent    : String[128];
      FHost         : String[128];
      Fport         : String[8];
      FIsDNSMode    : Boolean;
      FHttpRequest  : string;
      FHttpResponse : string;
      FContentlen   : NativeInt;
      FContenttyp   : String;
      FContentstart : NativeInt;
      FHostIPPort   : String;
      FHttpResponseCode : NativeInt;
      FContentCB    : TFRE_SIMPLE_HTTP_CONTENT_CB;
      Fchannel      : IFRE_APSC_CHANNEL;
      FError        : string;

      procedure   localNewChannel (const channel: IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt);
      procedure   localRead       (const channel: IFRE_APSC_CHANNEL);
      procedure   localDisco      (const channel: IFRE_APSC_CHANNEL);
      procedure   DoCallBack      ;
    public
      constructor create;
      destructor  Destroy         ; override;
      procedure   SetHost         (const host:string);
      procedure   SetUA           (const useragent:string);
      procedure   SetPort         (const port:string);
      procedure   SetIP_Mode      (const ip_mode : boolean);
      procedure   SetHttpProtocol (const proto:string);
      procedure   GetMethod       (const url:string ; const contentcallback : TFRE_SIMPLE_HTTP_CONTENT_CB);
    end;


implementation

{ TFRE_SIMPLE_HTTP_CLIENT }

procedure TFRE_SIMPLE_HTTP_CLIENT.localNewChannel(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
begin
  case channel_event of
    ch_ErrorOccured:
      begin
        FError := errorstring;
      end;
    ch_NEW_CS_CONNECTED:
      begin
        Fchannel    := Channel;
        FHostIPPort := channel.CH_GetConnSocketAddr;
        channel.CH_WriteString(FHttpRequest);
        channel.CH_Enable_Reading;
      end;
    ch_NEW_CHANNEL_FAILED:
      begin
       if assigned(FContentCB) then
         FContentCB(self,500,0,FError,nil);
      end
    else
      raise Exception.Create('TFRE_SIMPLE_HTTP_CLIENT - localnewchannel unhandled channel event');
  end;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.localRead(const channel: IFRE_APSC_CHANNEL);
var lContinue : boolean;

  function FetchTag(const tag:string; var val:string):boolean;
  var sp,ep:NativeInt;
  begin
    sp := pos(tag,FHttpResponse);
    if sp>0 then
      ep:=PosEx(cHTTPLE,FHttpResponse,sp+1);
    if ep>0 then
      begin
        result := true;
        sp     := sp+length(tag)+1;
        val    := trim(Copy(FHttpResponse,sp,ep-sp));
      end
    else
      result:=false;
  end;

  procedure TrySetContentLen;
  var tagval:string;
  begin
    if FetchTag('Content-Length:',tagval) then
      begin
        FContentlen:=StrToIntDef(tagval,-1);
        FHttpResponseCode := StrToIntDef(copy(FHttpResponse,9,4),500);
      end;
  end;

  procedure TrySetContentTyp;
  var tagval:string;
  begin
    if FetchTag('Content-Type:',tagval) then
      FContenttyp := tagval;
  end;

  function CheckLenAvail:boolean;
  var vl : NativeInt;
  begin
    result := false;
    FContentstart := pos(cHTTPLE+cHTTPLE,FHttpResponse);
    if FContentstart>0 then
      begin
        FContentstart:=FContentstart+4;
        vl := Length(FHttpResponse)-FContentstart+1;
        if FContentlen<=(vl) then
          exit(true);
      end;
  end;

begin
  FHttpResponse := FHttpResponse+channel.CH_ReadString;
  repeat
    lContinue:=false;
    case FState of
      hs_WaitResponse: begin
                         if pos(cHTTPLE+cHTTPLE,FHttpResponse)>1 then
                           begin
                             FState       := hs_ParseHeaders;
                             lContinue    := true;
                             FContentlen  := -1;
                           end;
                       end;
      hs_ParseHeaders: begin
                         if FContentlen=-1 then
                           TrySetContentLen;
                         if FContenttyp='' then
                           TrySetContentTyp;
                         if (FContentlen<>-1) and
                            (FContenttyp<>'') then
                              begin
                                if CheckLenAvail then
                                  begin
                                    channel.cs_Finalize;
                                    DoCallback;
                                  end;
                              end;
                       end;
    end;
  until lContinue=false;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.localDisco(const channel: IFRE_APSC_CHANNEL);
begin
  writeln('Cannel '+channel.CH_GetVerboseDesc,' DISCO ');
  if assigned(FContentCB) then
    FContentCB(self,500,0,FError,nil);
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.DoCallBack;
begin
  if assigned(FContentCB) then
    FContentCB(self,FHttpResponseCode,FContentlen,FContenttyp,@FHttpResponse[FContentstart]);
end;

constructor TFRE_SIMPLE_HTTP_CLIENT.create;
begin
  FHttpProtocol:= 'HTTP/1.1';
  FUserAgent   := 'FirmOS/httpc';
  FPort        := '80';
  FIsDNSMode   := true;
end;

destructor TFRE_SIMPLE_HTTP_CLIENT.Destroy;
begin
  if assigned(Fchannel) then
    Fchannel.cs_Finalize;
  inherited Destroy;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.SetHost(const host: string);
begin
  FHost := host;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.SetUA(const useragent: string);
begin
  FUserAgent := useragent;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.SetPort(const port: string);
begin
  Fport:=port;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.SetIP_Mode(const ip_mode: boolean);
begin
  FIsDNSMode := not ip_mode;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.SetHttpProtocol(const proto: string);
begin
  FHttpProtocol := proto;
end;

procedure TFRE_SIMPLE_HTTP_CLIENT.GetMethod(const url: string; const contentcallback: TFRE_SIMPLE_HTTP_CONTENT_CB);
begin
  FHttpRequest:=              'GET '+FRE_URI_Escape(url)+' '+FHttpProtocol+FHttpRequest+cHTTPLE;
  FHttpRequest:=FHttpRequest+ 'User-Agent: '+FUserAgent+cHTTPLE;
  FHttpRequest:=FHttpRequest+ 'Host: '+FHost+cHTTPLE;
  FHttpRequest:=FHttpRequest+ 'Accept: */*'+cHTTPLE;
  FHttpRequest:=FHttpRequest+ cHTTPLE;
  FContentCB  :=contentcallback;
  if FIsDNSMode then
    GFRE_SC.AddClient_TCP_DNS(FHost,Fport,'FHT',false,nil,@localNewChannel,@localRead,@localDisco)
  else
    GFRE_SC.AddClient_TCP(FHost,Fport,'FHT',false,nil,@localNewChannel,@localRead,@localDisco)
end;

end.

