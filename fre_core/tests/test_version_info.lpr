program test_version_info;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  FRE_SYSTEM;

{$I fos_version_helper.inc}

type

  { TTestApp }

  TTestApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure  WriteHelp; virtual;
    procedure  WriteVersion; virtual;
  end;

{ TTestApp }

procedure TTestApp.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hv',['help','version']);
  if ErrorMsg<>'' then begin
    writeln(ErrorMsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('v','version') then begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TTestApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestApp.Destroy;
begin
  inherited Destroy;
end;

procedure TTestApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;

procedure TTestApp.WriteVersion;
begin
  writeln(GFOS_VHELP_GET_VERSION_STRING);
end;

var
  Application: TTestApp;
begin
  Application:=TTestApp.Create(nil);
  Application.Title:='Version TestApp';
  Application.Run;
  Application.Free;
end.

