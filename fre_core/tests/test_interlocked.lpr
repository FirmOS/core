program test_interlocked;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Sysutils, FOS_INTERLOCKED
  { you can add units after this };


type

  { TAddThread }

  TAddThread = class (TThread)
   protected
     procedure Execute; override;
  end;

var
    g:int64;

{ TAddThread }

procedure TAddThread.Execute;
var j:integer;
begin
 for j:=0 to 100000-1 do begin
  FOS_IL_ExchangeAdd64(g,5);
 end;
end;

var
 tx:Array[0..3] of TAddThread;
  i:integer;

begin
 g:=0;
 for i:=0 to 3 do begin
  tx[i]:=TAddThread.Create(false);
 end;

 for i:=0 to 3 do begin
  tx[i].Waitfor;
 end;
 writeln(g);

end.

