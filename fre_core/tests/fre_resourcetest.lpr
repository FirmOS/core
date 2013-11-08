program fre_resourcetest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TFREResourcetestApplication }

  TFREResourcetestApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure TestMemory                      (const blocksize:integer=1024);
    procedure TestFileHandles;
    procedure TestProcessFork;
  end;

{ TMyApplication }

procedure TFREResourcetestApplication.DoRun;
var
  ErrorMsg: String;
  s       : string;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hfpm:',['help','file','process','memory']);
  if ErrorMsg<>'' then begin
    Writeln('Invalid option: ',Errormsg);
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('m','memory') then begin
    s:=GetOptionValue('m','memory');
    TestMemory(strtoint(s));
    Terminate;
    Exit;
  end;

  if HasOption('f','file') then begin
    TestFileHandles;
    Terminate;
    Exit;
  end;

  if HasOption('p','process') then begin
    TestProcessFork;
    Terminate;
    Exit;
  end;

  { add your program here }

  // stop program loop
  Terminate;
end;

constructor TFREResourcetestApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFREResourcetestApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TFREResourcetestApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName);
  writeln(' OPTIONS');
  writeln('  -h            | --help                 : print this help');
  writeln('  -m <size>     | --memory <size>          : getmem allocation in <size> blocks');
  writeln('  -f            | --file                 : file handles');
  writeln('  -p            | --process              : process fork');
end;

procedure TFREResourcetestApplication.TestMemory(const blocksize: integer);
var total_alloc     : QWord;
    success_getmems : QWord;
    p               : pointer;
begin
  total_alloc     := 0;
  success_getmems := 0;

  while true do
    begin
      p:=GetMem(blocksize);
      if p=nil then
        begin
          writeln('GetMem resultet nil');
          writeln('Total Allocated:',total_alloc,' Successfull Getmem Calls:',success_getmems);
        end
      else
        begin
          inc(total_alloc,blocksize);
          inc(success_getmems);
          if (success_getmems mod 1000)=0 then
            writeln('Total Allocated:',total_alloc,' Successfull Getmem Calls:',success_getmems);
        end;
  end;
end;

procedure TFREResourcetestApplication.TestFileHandles;
var total_fh : QWord;
    sl       : TStringlist;
begin
  sl := TstringList.Create;
  try
    sl.Add('TEST');
    sl.SaveToFile('testfile');
  finally
    sl.Free;
  end;
  while true do
    begin
      if FileOpen('testfile',fmOpenRead+fmShareDenyNone)=-1 then
        begin
          writeln('Filehandle -1');
          writeln('Total fh:',total_fh);
        end
      else
        begin
          inc(total_fh);
          if (total_fh mod 1000)=0 then
            writeln('Total fh:',total_fh);
        end;
    end;
end;

procedure TFREResourcetestApplication.TestProcessFork;
begin

end;

var
  Application: TFREResourcetestApplication;
begin
  Application:=TFREResourcetestApplication.Create(nil);
  Application.Title:='Resourcetest';
  Application.Run;
  Application.Free;
end.

