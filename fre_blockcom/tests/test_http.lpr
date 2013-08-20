program test_http;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  consoletestrunner,FOS_DEFAULT_IMPLEMENTATION,
  FOS_TOOL_INTERFACES,  Classes, fre_http_testsuite, fre_http;

var App : TTestRunner;
begin
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE HTTP Request Testsuite';
  App.Run;
  App.Free;
end.
