unit fre_mail;

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
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils,
  asn1util, blcksock, clamsend, dnssend, ftpsend, ftptsend, httpsend,
  imapsend, ldapsend, mimeinln, mimemess, mimepart, nntpsend, pingsend,
  pop3send, slogsend, smtpsend, snmpsend, sntpsend, synachar, synacode,
  synacrypt, synadbg, synafpc, synaicnv, synaip, synamisc, synautil,
  synsock, tlntsend,ssl_openssl,
  Classes,
  FOS_TOOL_INTERFACES;

type
  EFRE_SMTP = class (Exception);

  { TFRE_Mail_Send }

  TFRE_Mail_Send = class
  private
    fHost        : string;
    fPort        : string;
    fUser        : string;
    fPasswd      : string;

    FRaiseExceptionOnError: boolean;

    procedure LogInfo        (const s: string);
    procedure LogError       (const s: string);
    procedure LogDebug       (const s: string);

    procedure SetRaiseExceptionOnError(AValue: boolean);
    function  GetMessageID   :string;

  public
    constructor Create; virtual;
    procedure SetSMTPHost    (const host: string; const port : string; const user: string; const passwd: string);
    function  SendText       (const mailfrom: string; const mailto: string; const subject: string; const body : string) : boolean;
    function  SendFiles      (const mailfrom: string; const mailto: string; const subject: string; const body : string; const files:TFOSStringArray) : boolean;
    function  Send           (const mailfrom: string; const mailto: string; const msg:string) : boolean;
    property  RaiseExceptionOnError : boolean read FRaiseExceptionOnError write SetRaiseExceptionOnError;
  end;



implementation



{ TFRE_Mail_Send }

procedure TFRE_Mail_Send.LogInfo(const s: string);
begin
  GFRE_LOG.Log(s,'MAIL',fll_Info);
end;

procedure TFRE_Mail_Send.LogError(const s: string);
begin
  GFRE_LOG.Log(s,'MAIL',fll_Error);
  if FRaiseExceptionOnError then begin
    raise EFRE_SMTP.Create(s);
  end;
end;

procedure TFRE_Mail_Send.LogDebug(const s: string);
begin
  GFRE_LOG.Log(s,'MAIL',fll_Debug);
end;

procedure TFRE_Mail_Send.SetRaiseExceptionOnError(AValue: boolean);
begin
  FRaiseExceptionOnError:=AValue;
end;

function TFRE_Mail_Send.GetMessageID: string;
begin
  result:=IntToHex(Random(maxint),8)+IntToHex(Random(maxint),8)+IntToHex(Random(maxint),8)+IntToHex(Random(maxint),8)+'@fremail';
end;

constructor TFRE_Mail_Send.Create;
begin
  RaiseExceptionOnError:=false;
end;

procedure TFRE_Mail_Send.SetSMTPHost(const host: string; const port: string; const user: string; const passwd: string);
begin
 fHost   := host;
 fport   := port;
 fuser   := user;
 fpasswd := passwd;
end;

function TFRE_Mail_Send.SendText(const mailfrom: string; const mailto: string; const subject: string; const body: string): boolean;
var files : TFOSStringArray;
begin
  setlength(files,0);
  result := SendFiles(mailfrom,mailto,subject,body,files);
end;

function TFRE_Mail_Send.SendFiles(const mailfrom: string; const mailto: string; const subject: string; const body: string; const files: TFOSStringArray): boolean;
var ifiles : integer;
    mess   : TMimeMess;
    part   : TMimePart;
    mtext  : TStringList;
    msg    : string;
begin
  mess    := TMimeMess.Create;
  mtext   := TStringList.Create;
  try
    part       := mess.AddPartMultipart('mixed',nil);
    mtext.Text := body;
    mess.AddPartText(mtext,part);
    for ifiles := low(files) to high(files) do begin
      try
        mess.AddPartBinaryFromFile(files[ifiles],part);
      except on E: Exception do begin
        LogError(E.Message);
        if RaiseExceptionOnError then begin
          raise;
        end;
        exit(false);
      end; end;
    end;
    mess.header.from      := mailfrom;
    mess.header.tolist.add(mailto);
    mess.header.subject   := subject;
    mess.Header.MessageID := GetMessageID;
    mess.EncodeMessage;
    msg := mess.Lines.Text;
  finally
    mtext.Free;
    mess.Free;
  end;
  result := Send(mailfrom,mailto,msg);
end;

function TFRE_Mail_Send.Send(const mailfrom: string; const mailto: string; const msg: string): boolean;
var
  smtp      : TSMTPSend;
  msg_lines : TStringList;

  procedure _Error(const s: string);
  begin
    LogError('SMTP ERROR :'+ s +':'+ smtp.EnhCodeString+' # '+smtp.ResultString);
  end;

begin
  msg_lines := TStringList.Create;
  smtp      := TSMTPSend.Create;
  try
    msg_lines.Text := msg;

    smtp.UserName   := fUser;
    smtp.Password   := fPasswd;
    smtp.TargetHost := fHost;
    smtp.TargetPort := fPort;
    smtp.AutoTLS    := true;

    LogDebug('SMTP Login');
    if not smtp.Login() then begin
      _Error('Login');exit(false);
    end;

    LogDebug('SMTP Mail');
//    if not smtp.MailFrom(sFrom, Length(sFrom)) then
    if not smtp.MailFrom(mailFrom, 0) then begin
      _Error('MailFrom');
      exit(false);
    end;

    if not smtp.MailTo(mailTo) then begin
      _Error('MailTo');
      exit(false);
    end;

    if not smtp.MailData(msg_lines) then begin
      _Error('MailData');
      exit(false);
    end;

    LogDebug('SMTP Logout');
    if not smtp.Logout() then begin
      _Error('Logout');
    end;
    LogInfo('SMTP MAIL SEND OK:'+mailfrom+' -> '+mailto);
    exit(true);
  finally
    msg_lines.Free;
    smtp.Free;
  end;
end;



end.

