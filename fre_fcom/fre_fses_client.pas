unit fre_fses_client;

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
  Classes, SysUtils,fos_strutils,FRE_APS_INTERFACE,fre_http_tools,fos_tool_interfaces;

type
    TFRE_FS_ES_CLIENT = class;
    TFRE_FS_ES_CONTENT_CB = procedure(const sender : TFRE_FS_ES_CLIENT ; const http_status,content_len : NativeInt ;  const contenttyp : string ; content : PByte) is nested;

    { TFRE_FS_ES_CLIENT }

    TFRE_FS_ES_CLIENT=class
    private
    const
      cFSLE  = #10;
      cFSLES = #13#10;
    type
      fs_read_state  = (hs_NotConn,hs_ConnTry,hs_ReconnectTry,hs_WaitAuth,hs_ParseAuth,hs_CheckAuthResponse,hs_Online);
      fs_write_state = (ws_NotConn,ws_Rdy,ws_Progress);
    var
      FState        : fs_read_state;
      FWrite        : fs_write_state;
      FHost         : String[128];
      Fport         : String[8];
      Flock         : IFOS_Lock;
      FFsautpw      : String;
      FIsDNSMode    : Boolean;
      FFSResponse   : string;

      FContentlen   : NativeInt;
      FContenttyp   : String;
      FContentstart : NativeInt;
      FHostIPPort   : String;
      FHttpResponseCode : NativeInt;
      FContentCB    : TFRE_FS_ES_CONTENT_CB;
      Fchannel      : IFRE_APSC_CHANNEL;
      FChanTimer    : IFRE_APSC_TIMER;

      procedure   localNewChannel (const channel: IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
      procedure   localRead       (const channel: IFRE_APSC_CHANNEL);
      procedure   localDisco      (const channel: IFRE_APSC_CHANNEL);
      procedure   DoInvalidLogin  ;
      procedure   ChannelTimer    (const timer        : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
      procedure   Bailout         (const error:string);
    public
      constructor create;
      destructor  Destroy         ; override;
      procedure   SetHost         (const host:string);
      procedure   SetPort         (const port:string);
      procedure   SetIP_Mode      (const ip_mode : boolean);
      procedure   SetFS_Password  (const authpw:string);
      procedure   Connect         ;
      function    SendSMS         (const number : string ; const msg :string ; out err:string):boolean;
    end;


implementation

{ TFRE_FS_ES_CLIENT }

procedure TFRE_FS_ES_CLIENT.localNewChannel(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);  // self.FHostIPPort
begin
  Flock.Acquire;
  try
    case channel_event of
      ch_NEW_CS_CONNECTED:
        begin
          Fchannel := Channel;
          channel.CH_Enable_Reading;
          FHostIPPort := channel.GetConnSocketAddr;
          //writeln('CONNECTED CHANNEL ',channel.CH_GetID);
          FState := hs_WaitAuth;
        end;
      ch_NEW_CHANNEL_FAILED:
        begin
         if assigned(FContentCB) then
           FContentCB(self,500,0,channel.CH_GetErrorString,nil);
        end
      else
        raise Exception.Create('TFRE_FS_ES_CLIENT - localnewchannel unhandled channel event');
    end;
  finally
    Flock.Release;
  end;
end;

procedure TFRE_FS_ES_CLIENT.localRead(const channel: IFRE_APSC_CHANNEL);
var lContinue     : boolean;
    cmd_reply     : string;


  function FetchTag(const tag:string; var val:string):boolean;
  var sp,ep:NativeInt;
  begin
    sp := pos(tag,FFSResponse);
    if sp>0 then
      ep:=FOS_PosEx(cFSLE,FFSResponse,sp+1);
    if ep>0 then
      begin
        result := true;
        sp     := sp+length(tag)+1;
        val    := trim(Copy(FFSResponse,sp,ep-sp));
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
        FHttpResponseCode := StrToIntDef(copy(FFSResponse,9,4),500);
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
    FContentstart := pos(cFSLE+cFSLE,FFSResponse);
    if FContentstart>0 then
      begin
        FContentstart:=FContentstart+4;
        vl := Length(FFSResponse)-FContentstart+1;
        if FContentlen<=(vl) then
          exit(true);
      end;
  end;

  procedure TryAuth;
  var tagval        : string;
  begin
    if FetchTag('Content-Type:',tagval) then
      begin
        if tagval='auth/request' then
          begin
            channel.CH_WriteString('auth '+FFsautpw+cFSLE+cFSLE);
            FState      := hs_CheckAuthResponse;
            FFSResponse := '';
            exit;
          end;
      end;
    Bailout('auth/state fail : read ['+Copy(FFSResponse,1,30)+']...');
  end;

  function GetCMDReply(out reply:string):boolean;
  var tagval : string;
  begin
    result := false;
    if FetchTag('Content-Type:',tagval) then
      if tagval='command/reply' then
        if FetchTag('Reply-Text:',reply) then
          begin
            FFSResponse := '';
            exit(true);
          end;
    Bailout('read cmd/reply fail : read ['+Copy(FFSResponse,1,30)+']...');
    FFSResponse:='';
  end;

begin
  FFSResponse := FFSResponse+channel.CH_ReadString;
  //writeln('LOC READ ',FState,' ',FWrite,'  ',FFSResponse);
  GFRE_LOG.Log('FSES READ '+FFSResponse,'FS_CMD',fll_Debug);
  repeat
    lContinue:=false;
    case FState of
      hs_WaitAuth:
        begin
          if pos(cFSLE+cFSLE,FFSResponse)>1 then      // self.FFSResponse;
            begin
              FState       := hs_ParseAuth;
              lContinue    := true;
              FContentlen  := -1;
            end;
        end;
      hs_ParseAuth:
        TryAuth;
      hs_CheckAuthResponse:
        begin
          if GetCMDReply(cmd_reply) then
            begin
              if cmd_reply='-ERR invalid' then
                begin
                  FState := hs_NotConn;
                  FWrite := ws_NotConn;
                  FFSResponse:='';
                  GFRE_BT.CriticalAbort('authentication failure on FSES');
                end
              else
              if cmd_reply='+OK accepted' then
                begin
                  FState := hs_Online;
                  FFSResponse:='';
                  FWrite := ws_Rdy;
                end
              else
                Bailout('auth/reply unexpected response');
            end;
        end;
      hs_Online: begin
                   case FWrite of
                     ws_NotConn: ;// ignore;
                     ws_Rdy: ;     // ignore;
                     ws_Progress:
                       begin
                         GFRE_LOG.Log('FSES COMMAND '+FFSResponse,'FS_CMD',fll_Info);
                         FFSResponse:='';
                         FWrite:=ws_Rdy;
                       end;
                   end;
                   //if FContentlen=-1 then
                   //  TrySetContentLen;
                   //if FContenttyp='' then
                   //  TrySetContentTyp;
                   //if (FContentlen<>-1) and
                   //   (FContenttyp<>'') then
                   //     begin
                   //       if CheckLenAvail then
                   //         begin
                   //           channel.Finalize;
                   //         end;
                   //     end;
                  end;
    end;
  until lContinue=false;
end;

procedure TFRE_FS_ES_CLIENT.localDisco(const channel: IFRE_APSC_CHANNEL);
begin
  Flock.Acquire;
  try
    GFRE_LOG.Log('FSES CHANNEL DISCONNECT','FS_TIM');
    FFSResponse := '';
    //writeln('FSES DISCONNECT ',channel.CH_GetID);
    channel.Finalize;
    Fchannel := nil;
    FState  := hs_ReconnectTry;
    FWrite  := ws_NotConn;
  finally
    Flock.Release;
  end;
end;


procedure TFRE_FS_ES_CLIENT.DoInvalidLogin;
begin
  writeln('LOGIN invalid');
end;

procedure TFRE_FS_ES_CLIENT.ChannelTimer(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
var msg : String;
  procedure StartClient;
  begin
    if FIsDNSMode then
      GFRE_SC.AddClient_TCP_DNS(FHost,Fport,'FHT',nil,@localNewChannel,@localRead,@localDisco)
    else
      GFRE_SC.AddClient_TCP(FHost,Fport,'FHT',nil,@localNewChannel,@localRead,@localDisco)
  end;

begin
  Flock.Acquire;
  try
    if flag1 then
      begin
        if FState=hs_ConnTry then
          begin
            StartClient;
          end;
      end
    else
      begin
        if FState<>hs_Online then
          begin
            writestr(msg,'TIMEOUT / STATES ',FState,'  ',FWrite, ' ',timer.TIM_GetID,' ');
            GFRE_LOG.Log(msg,'FS_TIM');
          end;
        if FState=hs_ReconnectTry then
          begin
            StartClient;
          end;
      end;
  finally
    Flock.Release;
  end;
end;

procedure TFRE_FS_ES_CLIENT.Bailout(const error: string);
begin
  try
    GFRE_LOG.Log('BAILOUT: '+error,'FS_TIM');
    Fchannel.Finalize;
    Fchannel:=nil;
  except
  end;
end;

constructor TFRE_FS_ES_CLIENT.create;
begin
  FPort          := '8021';
  FIsDNSMode     := true;
  FFsautpw       := '';
  FWrite         := ws_NotConn;
  GFRE_TF.Get_Lock(Flock);
  FChanTimer     := GFRE_SC.AddTimer('fsest',5000,@ChannelTimer);
end;

destructor TFRE_FS_ES_CLIENT.Destroy;
begin
  if assigned(Fchannel) then
    Fchannel.Finalize;
  inherited Destroy;
end;

procedure TFRE_FS_ES_CLIENT.SetHost(const host: string);
begin
  FHost := host;
end;

procedure TFRE_FS_ES_CLIENT.SetPort(const port: string);
begin
  Fport:=port;
end;

procedure TFRE_FS_ES_CLIENT.SetIP_Mode(const ip_mode: boolean);
begin
  FIsDNSMode := not ip_mode;
end;

procedure TFRE_FS_ES_CLIENT.SetFS_Password(const authpw: string);
begin
  FFsautpw := authpw;
end;

procedure TFRE_FS_ES_CLIENT.Connect;
begin
  Flock.Acquire;
  try
    if FState=hs_NotConn then
      begin
        FState := hs_ConnTry;
        FChanTimer.TIM_Trigger(true);
      end;
  finally
    Flock.Release;
  end;
end;

function TFRE_FS_ES_CLIENT.SendSMS(const number: string; const msg: string; out err: string): boolean;
begin
  Flock.Acquire;
  try
    case FWrite of
      ws_NotConn:
        begin
          err := 'fses not connected';
          exit(false);
        end;
      ws_Rdy:
        begin
          err:='';
          result := true;
          FWrite := ws_Progress;
          Fchannel.CH_SAFE_WriteString('api gsmopen_sendsms gsm01 '+number+' '+msg+cFSLES+cFSLES);
        end;
      ws_Progress:
        begin
          err := 'send in progress';
          result:=false;
        end;
    end;
  finally
    Flock.Release;
  end;
end;

end.

