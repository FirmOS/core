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
  {$IFDEF CPU64}
    g:int64;
  {$ELSE}
    g:longword;
  {$ENDIF}

{ TAddThread }

procedure TAddThread.Execute;
var j:integer;
begin
 for j:=0 to 100000-1 do begin
  {$IFDEF CPU64}
    FOS_IL_ExchangeAdd64(g,5);
 {$ELSE}
    FOS_IL_ExchangeAdd(g,5);
 {$ENDIF}
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

