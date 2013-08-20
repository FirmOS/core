program test_mail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  consoletestrunner,fre_configuration,FRE_DB_INTERFACE,FOS_DEFAULT_IMPLEMENTATION,
  FRE_DB_CORE,
  FOS_TOOL_INTERFACES,  Classes, fre_mail_testsuite, fre_mail;

var App: TTestRunner;

begin
  InitMinimal;
  Initialize_Read_FRE_CFG_Parameter;
  GFRE_DBI.LocalZone := 'Europe/Vienna';
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE Mail Testsuite';
  App.Run;
  App.Free;
end.
