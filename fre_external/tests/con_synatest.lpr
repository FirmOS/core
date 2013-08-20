program con_synatest;

{$mode objfpc}{$H+}

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
  Classes;


type
  ESMTP = class (Exception);

// e-mail template (subject and other headers+CRLF+CRLF+e-mail body)
// in file sFileName
procedure AddToLog(const s:string);
begin
  writeln('LOG: ' +s);
end;

procedure MailSend(const sSmtpHost, sSmtpPort, sSmtpUser, sSmtpPasswd, sFrom, sTo, sMailText: AnsiString);
var
  smtp: TSMTPSend;
  msg_lines: TStringList;
begin
  msg_lines := TStringList.Create;
  smtp := TSMTPSend.Create;
  try
    //msg_lines.LoadFromFile(sFileName);
    msg_lines.Add(sMailText);
    msg_lines.Insert(0, 'From: ' + sFrom);
    msg_lines.Insert(1, 'To: ' + sTo);

    smtp.UserName := sSmtpUser;
    smtp.Password := sSmtpPasswd;

    smtp.TargetHost := sSmtpHost;
    smtp.TargetPort := sSmtpPort;
    smtp.AutoTLS    := true;

    AddToLog('SMTP Login');
    if not smtp.Login() then
      raise ESMTP.Create('SMTP ERROR: Login:' + smtp.EnhCodeString+' # '+smtp.ResultString);
//    AddToLog('SMTP StartTLS');
//    if not smtp.StartTLS() then
//      raise ESMTP.Create('SMTP ERROR: StartTLS:' + smtp.EnhCodeString+' # '+smtp.ResultString);
    AddToLog('SMTP Mail');
//    if not smtp.MailFrom(sFrom, Length(sFrom)) then
    if not smtp.MailFrom(sFrom, 0) then
      raise ESMTP.Create('SMTP ERROR: MailFrom:' + smtp.EnhCodeString+' # '+smtp.ResultString);
    if not smtp.MailTo(sTo) then
      raise ESMTP.Create('SMTP ERROR: MailTo:' + smtp.EnhCodeString+' # '+smtp.ResultString);
    if not smtp.MailData(msg_lines) then
      raise ESMTP.Create('SMTP ERROR: MailData:' + smtp.EnhCodeString+' # '+smtp.ResultString);

    AddToLog('SMTP Logout');
    if not smtp.Logout() then
      raise ESMTP.Create('SMTP ERROR: Logout:' + smtp.EnhCodeString+' # '+smtp.ResultString);
    AddToLog('OK!');
  finally
    msg_lines.Free;
    smtp.Free;
  end;
end;

begin
  MailSend('mail.firmos.at','25','mailer','45mfj345l2094pc1','erwin.sedlacek@firmos.at','franz_schober@gmx.at','SUBJECT: TESTMAIL ...'+CRLF+CRLF+'This is the BODY');
end.

