program CON_LFQTEST;

{$mode objfpc}{$H+}

uses
  cmem,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Sysutils,Classes,cpu,FOS_LFQ,FOS_LOCKING;


{$IFDEF CPU32}
const cpuver='CPU32';
{$ELSE}
{$IFDEF CPU64}
const cpuver='CPU64';
{$ENDIF}
{$ENDIF}

var LFQ:TFOS_LFQ;

   // Mega Inneficient Gauss Sum Test
   // sum = (n*(n+1) /2
   // Calculated by concurrent Threads

type

   TPushRec=class
     PID  : String;
     Item : QWord;
   end;

   { TProducer }
   TProducer=class(TThread)
     PID  : String;
     L    : TFOS_E;
     Fupto : QWord;
     procedure   Execute;override;
     constructor Create (const id:string;const upto:QWord);
   end;

   { TConsumer }

   TConsumer=class(TThread)
     L         : TFOS_E;
     PID       : String;
     sum       : array of qword;
     cnt       : array of qword;
     Fupto     : QWord;
     Done      : boolean;
     FPCount   : integer;
     procedure   Execute;override;
     constructor Create (const id:string;const upto:QWord;const pcount:integer);
     procedure   DumpData;
   end;


function TotalCheck:boolean;forward;

{ TConsumer }

procedure TConsumer.Execute;
var pr:TPushRec;
    id:integer;
begin
  L.WaitFor;
  repeat
    pr:=TPushRec(LFQ.Pop);
    if assigned(pr) then begin
     id:=StrToInt(pr.PID);
     sum[id]:=sum[id] + pr.Item;
     inc(cnt[id]);
  //   write('<C',id,'>(',pr.PID,' ',pr.Item,')');
     pr.Free;
    end else begin
     sleep(0);
    end;
    if totalcheck then break;
  until Terminated;
  L.Free;
  done:=true;
end;

constructor TConsumer.Create(const id: string; const upto: QWord;const pcount:integer);
begin
  inherited Create(false);
  L:=TFOS_E.Create;
  PID:=id;
  Fupto:=upto;
  done:=false;
  FPCount:=pcount;
  SetLength(sum,pcount);
  setlength(cnt,pcount);
end;

procedure TConsumer.DumpData;
var
  i: Integer;
begin
 write(pid,'  :');
 for i:=0 to FPCount-1 do begin
   write(format('C=%1.1d(SUM=%8.8d CNT=%8.8d) ',[i,sum[i],cnt[i]]));
 end;
 writeln;
end;


{ TProducer }

procedure TProducer.Execute;
var pr : TPushRec;
    cnt: QWord;
begin
  writeln('Producer ',PID,' ready.');
  L.WaitFor;
  writeln('Producer ',PID,' started.');
  cnt := 0;
  repeat
    inc(cnt);
    pr := TPushRec.Create;
    pr.PID  := Pid;
    pr.Item := cnt;
//    write('<P> ',pr.PID,' ',pr.Item,' ');
    LFQ.Push(pr);
  until (cnt=Fupto) or Terminated;
  writeln('Producer ',PID,' done producing. (',cnt,'/',Fupto,')');
  L.Free;
end;

constructor TProducer.Create(const id: string; const upto: QWord);
begin
  inherited Create(false);
  L:=TFOS_E.Create;
  PID:=id;
  Fupto:=upto;
end;


const  PCOUNT:integer = 1;
       CCOUNT:integer = 1;
       UPTO  :QWord   = 2;
var
    P : array of TProducer;
    C : array of TConsumer;
    i : integer;
    alldone : boolean;

function TotalCheck:boolean;
var i:integer;
     tcnt,tsum:array [0..10] of qword;
     j: Integer;

begin
  result:=true;
  for i:=0 to pcount-1 do begin
    tcnt[i]:=0;
    tsum[i]:=0;
  end;
  for i:=0 to ccount-1 do begin
    for j:=0 to pcount-1 do begin;
      tcnt[j] := tcnt[j] + C[i].cnt[j];
    end;
  end;
  for i:=0 to ccount-1 do begin
    for j:=0 to pcount-1 do begin;
      if tcnt[j]<>(UPTO) then begin
  //     writeln('false ',i,' ',j,' ',tcnt[j]);
       exit(false);
      end;
    end;
  end;
  writeln('true');
end;

procedure Total(const pcount,ccount:integer);
var i:integer;
    tcnt,tsum:array of qword;
    j: Integer;
begin
 SetLength(tcnt,ccount);
 SetLength(tsum,ccount);
 for i:=0 to ccount-1 do begin
   for j:=0 to pcount-1 do begin;
     tcnt[j] := tcnt[j] + C[i].cnt[j];
     tsum[j] := tsum[j] + C[i].sum[j];
   end;
 end;
 writeln('TOTAL');
 for i:=0 to ccount-1 do begin
   write('C ',i,' CNT=',tcnt[i],' SUM=',tsum[i]);
 end;
 writeln;
 writeln('Expected is ',(UPTO*(upto+1)) div 2);
 writeln;
end;

  procedure SimpleTest;
  var val:Pointer;
  begin
    writeln('Start SimpleTest');
    LFQ:=TFOS_LFQ.Create;
    lfq.Push(pointer(4711));
    val:=lfq.Pop;
    lfq.free;
    if qword(val)=4711 then begin
      writeln('SimpleTest Passed');
    end else begin
      writeln('Simpletest Failed ',qword(val));
    end;
  end;

begin


 PCOUNT:=StrToIntDef(paramstr(1),4);
 CCOUNT:=StrToIntDef(paramstr(2),2);
 UPTO  :=StrToIntDef(paramstr(3),10000);

 writeln('FOS LFQ Test / ',cpuver,' Producers=',PCOUNT,' Consumers=',CCOUNT,' from 1 to ',UPTO);
 {$IFDEF CPU32}
   writeln('CAS 128 Support = False');
 {$ELSE}
 if  InterlockedCompareExchange128Support then begin
   writeln('CAS 128 Support = True');
 end else begin
   writeln('CAS 128 Support = False');
 end;
 {$ENDIF}
 LFQ:=TFOS_LFQ.Create;
 SetLength(p,pcount);
 SetLength(c,pcount);
 for i:=0 to PCOUNT-1 do begin
  P[i] := TProducer.Create(inttostr(i),upto);
 end;
 for i:=0 to CCOUNT-1 do begin
   C[i] := TConsumer.Create(inttostr(i),upto,PCOUNT);
 end;
 writeln('Start Consuming/Producing');
 for i:=0 to PCOUNT-1 do begin
   P[i].L.SetEvent;
 end;
 for i:=0 to CCOUNT-1 do begin
   C[i].L.SetEvent;
 end;
 repeat
   for i:=0 to CCOUNT-1 do begin
     alldone:=true;
     if not c[i].done then alldone:=false;
   end;
   sleep(10);
 until alldone;
// sleep(5000);
 writeln('all done');
 for i:=0 to PCOUNT-1 do begin
   p[i].Terminate;
 end;
 for i:=0 to CCOUNT-1 do begin
   c[i].Terminate;
 end;
 for i:=0 to CCOUNT-1 do begin
   c[i].WaitFor;
   c[i].DumpData;
 end;
 total(pcount,CCOUNT);
 for i:=0 to PCOUNT-1 do begin
   p[i].Free;
 end;
 for i:=0 to CCOUNT-1 do begin
   c[i].Free;
 end;
 LFQ.Free;
end.

