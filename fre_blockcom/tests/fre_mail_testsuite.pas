unit fre_mail_testsuite;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,fpcunit,testregistry,testdecorator, FOS_TOOL_INTERFACES,FRE_SYSTEM,
  fre_mail;

type

  { TFRE_Mail_Tests }

  TFRE_Mail_Tests = class (TTestCase)
  published
    procedure PlainMail;
    procedure OtherDomain;
    procedure AttachmentMail;
    procedure MissingFileMail;
  end;


implementation

{ TFRE_Mail_Tests }

procedure TFRE_Mail_Tests.PlainMail;
var mail : TFRE_Mail_Send;
    res  : boolean;
begin
  mail   := TFRE_MAIL_Send.Create;
  mail.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
  res    := mail.SendText('erwin.sedlacek@firmos.at','franz.schober@firmos.at','My Subject','This is the BODY');
  AssertEquals('Send Failed',true,res);
end;

procedure TFRE_Mail_Tests.OtherDomain;
var mail : TFRE_Mail_Send;
    res  : boolean;
begin
  mail   := TFRE_MAIL_Send.Create;
  mail.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
  res    := mail.SendText('erwin.sedlacek@firmos.at','franz_schober@gmx.at','My Subject','This is the BODY');
  AssertEquals('Send Failed',true,res);
end;


procedure TFRE_Mail_Tests.AttachmentMail;
var mail  : TFRE_Mail_Send;
    files : TFOSStringArray;
    res   : boolean;
begin
  setlength(files,2);
  files[0]:='attachment1.jpg';
  files[1]:='attachment2.pdf';

  mail   := TFRE_MAIL_Send.Create;
  mail.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
  res    := mail.SendFiles('erwin.sedlacek@firmos.at','franz.schober@firmos.at','My Subject','This is the BODY',files);
  AssertEquals('Send Failed',true,res);
end;

procedure TFRE_Mail_Tests.MissingFileMail;
var mail  : TFRE_Mail_Send;
    files : TFOSStringArray;
    res   : boolean;
begin
  setlength(files,1);
  files[0]:='attachmentxxx.jpg';

  mail   := TFRE_MAIL_Send.Create;
  mail.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
  res    := mail.SendFiles('erwin.sedlacek@firmos.at','franz.schober@firmos.at','My Subject','This is the BODY',files);
  AssertEquals('Send Did not Fail',false,res);

end;

initialization
  RegisterTest(TFRE_Mail_Tests);
end.

