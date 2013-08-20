program test_process;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  consoletestrunner,fre_configuration,FRE_DB_INTERFACE,FOS_DEFAULT_IMPLEMENTATION,
  FRE_DB_CORE,
  FOS_TOOL_INTERFACES,
  Classes, FRE_PROCESS, fre_process_testsuite;

var App: TTestRunner;

begin
  InitMinimal;
  Initialize_Read_FRE_CFG_Parameter;
  GFRE_DBI.LocalZone := 'Europe/Vienna';
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE Process Testsuite';
  App.Run;
  App.Free;
end.
