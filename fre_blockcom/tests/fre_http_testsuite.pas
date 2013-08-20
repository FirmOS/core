unit fre_http_testsuite;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,fpcunit,testregistry,testdecorator, FOS_TOOL_INTERFACES,FRE_SYSTEM,
  fre_http;

type

  { TFRE_HTTP_Tests }

  TFRE_HTTP_Tests = class (TTestCase)
  published
    procedure FetchURL_HtmlTest;
  end;


implementation


{ TFRE_HTTP_Tests }

procedure TFRE_HTTP_Tests.FetchURL_HtmlTest;
var r:string;
    res:boolean;
begin
  res := FRE_HTTP_GET_Req_String('www.kernel.org',r);
  writeln(res);
  writeln('TXT',r);
end;

initialization
  RegisterTest(TFRE_HTTP_Tests);
end.

