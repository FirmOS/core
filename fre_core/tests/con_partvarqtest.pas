program CON_PARTVARQTEST;

{$mode objfpc}{$H+}

uses  Cmem,{$IFDEF UNIX}CThreads,{$ENDIF}
      Classes, SysUtils,FOS_PARTVARQ,FOS_TOOL_INTERFACES,FOS_NPS,FOS_LFQ,FOS_LOCKING,FOS_CPU_TOOLS;

type TPRODUCER=class;
     TConsumer=class;

var C       : TFOS_PARTVARQ;
    PCQ     : TFOS_PARTVARQ;
    MUL     : TFOS_PARTVARQ;
    PRO     : TProducer;
    CON     : TConsumer;
    TestAtt : Array of String;

    RDLN    : Boolean=false;
    NOWRITE : Boolean=false;
    LCK     : boolean=false;
    CS      : TFOS_LOCK;

  procedure PushString(const s:string);
  var res:TFOS_COMPACT_STATUS;
  begin
    res:=C.Push(PChar(s),length(s));
    if not NOWRITE then write('PUSH <',s,'> S=',C.Space);
    if res<>cs_OK then begin
      writeln(' => ',res,' SP=',c.Space);
      abort;
    end else begin
      if not NOWRITE then WriteLn(' OK');
    end;
    if RDLN then begin writeln('-----');readln;end;
  end;

  function Popstring(const qkey:integer;out data1:pointer;var mfp:pointer):string; overload;forward;

  function Popstring(var mfp:pointer;const qk:integer=0):string;
  var data1:pointer;
  begin
    result:=Popstring(qk,data1,mfp);
  end;

  function Popstring(const qkey:integer;out data1:pointer;var mfp:pointer):string; overload;
  var l1,l2: integer;
      p1,p2: Pointer;
      res  : TFOS_COMPACT_STATUS;
  begin
   data1:=nil;
   res:=C.Pop(qkey,p1,p2,l1,l2);
   if res<>cs_OK then begin
     writeln(res,' SP=',c.Space,' CT=',c.Count(qkey),' ');
     exit;
   end;
   data1:=p1;
   if mfp=nil then begin
     C.MarkFree(qkey,data1);
   end else begin
     mfp:=data1;
   end;
   SetLength(result,l1+l2);
   Move(p1^,result[1],l1);
   if l2>0 then Move(p2^,result[l1+1],l2);
   if not NOWRITE then writeln('Got <',result,'> C=',C.Count(qkey),' S=',C.Space);
   if RDLN then begin writeln('-----');readln;end;
  end;

  procedure MassTest;
  var i:integer;
      s:string;
      res:TFOS_COMPACT_STATUS;
      old,new:int64;
  begin
   s:='Aloha from Hawaii';
   old:=GFRE_BT.Get_Ticks_ms;
   C:=TFOS_PARTVARQ.Create(1024*1024*500);
   writeln('ALLOC');
   new:=GFRE_BT.Get_Ticks_ms;
   writeln('ALLOC TIME ',new-old);
   old:=GFRE_BT.Get_Ticks_ms;
   for i:=1 to 10000000 do begin
    //C.Lock;
    res:=C.Push(PChar(s),length(s));
    //C.Unlock;
    if res<>cs_OK then begin
     writeln('PUSH ERROR QFULL ?',ord(res),' ',res);
     c._Dumpstate('QFULL?',false);
     abort;
    end;
   end;
//TODO   WriteLn('Queue Count ',C.Count);
   new:=GFRE_BT.Get_Ticks_ms;
   writeln('PUSH QUEUE TIME ',new-old);
   old:=GFRE_BT.Get_Ticks_ms;
   for i:=1 to 10000000 do begin
    //C.Lock;
    res:=C.PopAsString(0,s);
    if i mod 1000000 = 0 then begin
     writeln(i);
    end;
    //C.Unlock;
    if res<>cs_OK then begin
     writeln('ERROR ',ord(res),' ',res);
     abort;
    end;
    if s<>'Aloha from Hawaii' then begin
      writeln('ERROR INVLID STRING');
      abort;
    end;
   end;
//TODO   WriteLn('Queue Count ',C.Count);
   new:=GFRE_BT.Get_Ticks_ms;
   writeln('POP QUEUE TIME ',new-old);
   //readln;
  end;

  procedure LFQ_Test;
  var LQ:TFOS_LFQ;
       s:string;
       i,ls:integer;
       p:pchar;
       old,new:int64;
  begin
   LQ:=TFOS_LFQ.Create;
   s:='Aloha from Hawaii'+#0;
   ls:=Length(s);
   old:=GFRE_BT.Get_Ticks_ms;
   for i:=1 to 10000000 do begin
    p:=Getmem(ls);
    Move(s[1],p^,ls);
    lq.Push(p);
   end;
   new:=GFRE_BT.Get_Ticks_ms;
   writeln('LQ PUSH QUEUE TIME ',new-old);
   old:=GFRE_BT.Get_Ticks_ms;
   for i:=1 to 10000000 do begin
    p:=lq.Pop;
    if pchar(p)<>'Aloha from Hawaii' then begin
      writeln('ERROR INVLID STRING ',pchar(p));
      abort;
    end;
    Freemem(p);
   end;
   new:=GFRE_BT.Get_Ticks_ms;
   writeln('LQ POP QUEUE TIME ',new-old);
   p:=lq.Pop;
   writeln(pchar(p));
  end;


 type
   { TProducer }
   TProducer=class(Tthread)
     procedure Execute;override;
   end;

   { TConsumer }

   TConsumer=clasS(TThread)
     procedure Execute;override;
   end;

   { TMultiProd }

   TMultiProd=class(TThread)
     procedure Execute;override;
   end;

   { TMultiConsumer }

   TMultiConsumer=class(TThread)
     cpu:integer;
     accounted:cardinal;
     procedure Execute;override;
   end;

 { TMultiConsumer }

  procedure TMultiConsumer.Execute;
   var i,k,ii,j,fail:cardinal;
       s:string;
       d1,d2:pointer;
       l1,l2:cardinal;
       res:TFOS_COMPACT_STATUS;
       err:string;
   begin
    i:=0;
    j:=1;
    accounted:=0;
    fail:=0;
    if not GFRE_CPU.Bind_to_logical(cpu,err) then GFRE_BT.CriticalAbort('multi consumer not set cpuaffinity <%s> CPU=%d',[err,cpu]);
    writeln('MULTI CONSUMER START (',cpu,')');
    try
    repeat
     inc(i);
     if LCK then CS.Acquire;
     res:=MUL.PopAsString(cpu-1,s);
     if LCK then CS.Release;
     if res=cs_OK then begin
      inc(j);
     end else begin
      inc(fail);
     end;
     //sleep(0);
    until (Terminated);
    writeln('MC (',cpu,') Sucessfully consumed => ',j,' Fail: ',fail,' Count ',MUL.Count(cpu-1));
    accounted:=j;
    except on e:exception do begin
      GFRE_BT.CriticalAbort(e.Message,true);
    end;end;
  end;

 { TMultiProd }

 procedure TMultiProd.Execute;
 var i,j,w,fail,len:Integer;
      s:string[20];
      res:TFOS_COMPACT_STATUS;
      err:string;
 begin
   j:=1;
   fail:=0;
   w:=0;
   if not GFRE_CPU.Bind_to_logical(0,err) then GFRE_BT.CriticalAbort('multi producer not set cpuaffinity <%s>',[err]);
   try
    writeln('PRODUCER START (MULTITEST)');
    repeat
//     writeln('PRODUCER WRAP');
     inc(w);
     for i:=0 to Length(TestAtt)-1 do begin
       if LCK then CS.Acquire;
       res:=MUL.Push(@TestAtt[i][1],Length(TestAtt[i]));
       if LCK then CS.Release;
       if res<>cs_OK then begin
        inc(fail);
       end else begin
        //write('P',i,' ');
        inc(j);
        //if i=100000 then exit;
       end;
     end;
   until terminated;
   writeln('Sucessfully produced => ',j,' Fail:',fail,' Wrapped ',w);
   except on e:exception do begin
     writeln(GFRE_BT.DumpExceptionsBacktrace);
     writeln('M PRO Ex ',e.Message);
   end;end;
 end;

 { TConsumer }

 procedure TConsumer.Execute;
 var i,j,fail:cardinal;
     s:string;
     d1,d2:pointer;
     l1,l2:cardinal;
     res:TFOS_COMPACT_STATUS;
     err:string;

 begin
  i:=0;
  j:=1;
  fail:=0;
  if not GFRE_CPU.Bind_to_logical(1,err) then GFRE_BT.CriticalAbort('producer could not set cpuaffinity<%s>',[err]);
  writeln('CONSUMER START');
  try
  repeat
   inc(i);
   if LCK then CS.Acquire;
   res:=PCQ.PopAsString(0,s);
   if LCK then CS.Release;
   if res=cs_OK then begin
    //write('C ',s,' ');
    if StrToInt(s)<>j then begin
      PRO.Terminate;
      pro.WaitFor;
      writeln;
      writeln('i=',i,' j=',j,' fail=',fail);
      PCQ._Dumpstate('FISH');
      GFRE_BT.CriticalAbort('Snorkel Popped %s <> Expected %d  CNT = %d   SPACE = %d',[s,j,PCQ.Count(1),PCQ.Space]);
    end;
    inc(j);
   end else begin
    inc(fail);
   end;
   //sleep(0);
  until (Terminated) and (PCQ.Count(0)=0);
  writeln('Sucessfully consumed => ',j,' Fail: ',fail,' Count ',PCQ.Count(0));
  except on e:exception do begin
    GFRE_BT.CriticalAbort(e.Message,true);
  end;end;
 end;

 { TProducer }

 procedure TProducer.Execute;
 var i,fail,len:Integer;
     s:string[20];
     res:TFOS_COMPACT_STATUS;
     err:string;
 begin
  i:=1;
  fail:=0;
  if not GFRE_CPU.Bind_to_logical(0,err) then GFRE_BT.CriticalAbort('producer could not set cpuaffinity  <%s>',[err]);
  try
   writeln('PRODUCER START');
   len:=Length(s);
  repeat
    s:=IntToStr(i);
    if LCK then CS.Acquire;
    res:=PCQ.Push(@s[1],Length(s));
    if LCK then CS.Release;
    if res<>cs_OK then begin
     inc(fail);
    end else begin
     //write('P ',i,' ');
     inc(i);
     //if i=100000 then exit;
    end;
  until terminated;
  writeln('Sucessfully produced => ',i,' Fail:',fail);
  except on e:exception do begin
    writeln(GFRE_BT.DumpExceptionsBacktrace);
    writeln('PRO Ex ',e.Message);
  end;end;
 end;





procedure CallLocalProc(AProc, Frame: Pointer; Param1: PtrInt;Param2:PtrInt); inline;
type PointerLocal = procedure(_EBP: Pointer; Param1: PtrInt; Param2:PtrInt);
begin
  PointerLocal(AProc)(Frame, Param1,Param2);
end;

procedure ExecuteLocalCallBack(const locproc:pointer;const localparm:integer;const iterations:integer);
var Frame:Pointer;
    i:integer;
begin
 for i:=1 to iterations do begin
   Frame:=get_caller_frame(get_frame);
   CallLocalProc(locproc,Frame,localparm,i);
 end;
end;

procedure LocalCallBacktest;
var k:integer;

  procedure LocalTest(index:integer;param:integer);
  begin
    writeln('K = ',k,' Index=',index,' Param=',param);
  end;

begin
  writeln('LOCALCALLBACKTEST');
  k:=33;
  ExecuteLocalCallback(@Localtest,27,5);
  writeln('DONE');
end;

 procedure LocalPop();
 begin

 end;

 procedure Basic;
 var  s:string;
      i:integer;
      markfreea : array [1..5] of Pointer;
      markfreec : integer;
      dummymfp  : pointer;
      rmp       : integer;
 begin
  writeln('BASIC TEST');
  NOWRITE:=false;
//  NOWRITE:=true;
  RDLN:=false;
//  RDLN:=true;
  C:=TFOS_PARTVARQ.Create(257);
  dummymfp:=nil;
  writeln('BASIC INIT');
  PushString('Hall1');
  Popstring(dummymfp);
  PushString('Elvi1');
  Popstring(dummymfp);
  PushString('Hall2');
  Popstring(dummymfp);
  PushString('Elvi2');
  Popstring(dummymfp);
  PushString('Hall3');
  Popstring(dummymfp);
  PushString('Elvi3');
  Popstring(dummymfp);
  markfreec :=1;
  for i:=0 to 2560 do begin
    writeln('I====',i);
    C.StartPush(13);
    s:='Hallo';
    C.MultiPush(@s[1],5);
    s:='du';
    C.MultiPush(@s[1],2);
    s:=Format('%6.6s',[inttostr(i)]);//   'Pferd!';
    C.MultiPush(@s[1],6);
    C.EndPush;
    dummymfp:=pointer(1);
    Popstring(dummymfp);
    markfreea[markfreec] := dummymfp;
    inc(markfreec);
    if markfreec=6 then begin
     repeat
       rmp := Random(5)+1;
       if markfreea[rmp]<>nil then begin
         C.MarkFree(0,markfreea[rmp]);
         markfreea[rmp]:=nil;
         dec(markfreec);
       end;
       if markfreec=1 then break;
       write('fuck..',markfreec);
     until false;
    end;
  end;

 end;

procedure Markfree;
var  s:string;
     i:integer;
     markfreea : array [1..50] of Pointer;
     markfreec : integer;
     dummymfp  : pointer;
     rmp       : integer;
     to_free   : integer;
     fi        : integer;
     l         : integer;
begin
 writeln('MARK FREE TEST');
 NOWRITE:=true;
//  NOWRITE:=true;
 RDLN:=false;
//  RDLN:=true;
 C:=TFOS_PARTVARQ.Create(64*1024);
 dummymfp:=nil;
 markfreec :=1;
 for fi:=1 to high(markfreea) do begin
  markfreea[fi]:=nil;
 end;
 for i:=0 to 25600 do begin
   if (i mod 1000)=0 then writeln('I====',i);
   l:=random(25)+10;
   if C.StartPush(l+7)=cs_OK then begin
     s:='Hallo';
     C.MultiPush(@s[1],5);
     s:='du';
     C.MultiPush(@s[1],2);
     s:=Format('%6.6s',[inttostr(i)])+'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx';
     C.MultiPush(@s[1],l);
     C.EndPush;
     dummymfp:=pointer(1);
   end else GFRE_BT.CriticalAbort('eierbär draussen');
   Popstring(dummymfp);
   for fi:=0 to high(markfreea) do begin
    if markfreea[fi]=nil then begin
     markfreea[fi]:=dummymfp;
     //writeln('set ',fi);
     break;
    end;
   end;
//   markfreea[markfreec] := dummymfp;
   inc(markfreec);
//   writeln('markfreec ',markfreec);
   if markfreec=51 then begin
    to_free:=random(20)+1;
    repeat
      rmp := Random(50)+1;
      if markfreea[rmp]<>nil then begin
//        writeln('free ',rmp);
        C.MarkFree(0,markfreea[rmp]);
        markfreea[rmp]:=nil;
        dec(markfreec);
        //write('to free ',to_free);
        //write('fuck..',markfreec);
      end;
      if markfreec=to_free then break;
    until false;
   end;
 end;
 writeln('clear the rest');
 for fi:=low(markfreea) to high(markfreea) do begin
  if markfreea[fi]<>nil then begin
   write('free ',fi);
   C.MarkFree(0,markfreea[fi]);
   write(' SP= ',c.Space,' ');
   markfreea[fi]:=nil;
  end;
 end;
 if C.Space=C.QSize then begin
  writeln(' MARK FREE TEST DONE !');
 end else begin
  writeln(' MARK FREE TEST FAILED !');
  abort;
 end;

end;

procedure MarkfreeMulti;
type
  TMF = record
   p    :pointer;
   qkey :integer;
  end;

var  s:string;
     i:integer;
     markfreea : array [1..50] of TMF;
     markfreec : integer;
     dummymfp  : pointer;
     rmp       : integer;
     to_free   : integer;
     fi        : integer;
     rqkey     : integer;
     qcount    : integer;
begin
 writeln('MARK FREE MULTI TEST');
 NOWRITE:=false;
//  NOWRITE:=true;
 RDLN:=false;
//  RDLN:=true;
 qcount:=GFRE_CPU.Logical;
 C:=TFOS_PARTVARQ.Create(64*1024,qcount);
 dummymfp:=nil;
 markfreec :=1;
 for fi:=1 to high(markfreea) do begin
  markfreea[fi].p:=nil;
  markfreea[fi].qkey:=-1;
 end;

 for i:=0 to 2560 do begin
   writeln('I====',i);
   C.StartPush(13);
   s:='Hallo';
   C.MultiPush(@s[1],5);
   s:='du';
   C.MultiPush(@s[1],2);
   s:=Format('%6.6s',[inttostr(i)]);//   'Pferd!';
   C.MultiPush(@s[1],6);
   C.EndPush;
   dummymfp:=pointer(1);
   rqkey:=-1;
   while rqkey=-1 do begin
    rmp:=random(qcount);
    if C.Count(rmp)>0 then begin
     rqkey:=rmp;
     break;
    end;
   end;
   Popstring(dummymfp,rqkey);
   for fi:=0 to high(markfreea) do begin
    if markfreea[fi].p=nil then begin
     markfreea[fi].p:=dummymfp;
     markfreea[fi].qkey:=rqkey;
     writeln('set ',fi);
     break;
    end;
   end;
//   markfreea[markfreec] := dummymfp;
   inc(markfreec);
//   writeln('markfreec ',markfreec);
   if markfreec=51 then begin
    to_free:=random(20)+1;
    repeat
      rmp := Random(50)+1;
      if markfreea[rmp].p<>nil then begin
//        writeln('free ',rmp);
        C.MarkFree(markfreea[rmp].qkey,markfreea[rmp].p);
        markfreea[rmp].p:=nil;
        dec(markfreec);
        write('to free ',to_free);
        write('fuck..',markfreec);
      end;
      if markfreec=to_free then break;
    until false;
   end;
 end;
 writeln('clear the rest');
 for fi:=low(markfreea) to high(markfreea) do begin
  if markfreea[fi].p<>nil then begin
   write('free ',fi);
   C.MarkFree(markfreea[fi].qkey,markfreea[fi].p);
   write(' SP= ',c.Space,' ');
   markfreea[fi].p:=nil;
  end;
 end;
 if C.Space=C.QSize then begin
  writeln(' MARK FREE MULTI TEST DONE !');
 end else begin
  writeln(' MARK FREE MULTI TEST FAILED !');
  abort;
 end;

end;

procedure InterLeavedPopStatic;
var cnt,cntv,j,fpush,fpop,i:integer;
    LA:Array[1..12] of pointer;
    s:string;
    l1,l2:Cardinal;
    p1,p2:Pointer;
    res  :TFOS_COMPACT_STATUS;
    lcnt:cardinal;
    qkey:cardinal;
    dummymfp:pointer;

begin
 NOWRITE:=false;
 C:=TFOS_PARTVARQ.Create(64,4);
 dummymfp:=nil;
 PushString('Hallo');
 PushString('Du');
 PushString('Super');
 PushString('Nuss');
 Popstring(dummymfp,1);
 Popstring(dummymfp,2);
 Popstring(dummymfp,3);
 Popstring(dummymfp,4);
end;


 procedure Interleave;
 var s:string;
     j:integer;
     dummymfp:pointer;
 begin
  writeln('INTERLEAVE TEST A');
  RDLN:=false;
  NOWRITE:=true;
  C:=TFOS_PARTVARQ.Create(26);
  j:=0;
  dummymfp:=nil;
  repeat
   inc(j);
   PushString('Eins1');
   PushString('Z2');
   PushString('Dr3');
   s:=Popstring(dummymfp);
   if s<>'Eins1' then GFRE_BT.CriticalAbort('Eins1 <> %s',[s]);
   PushString('V');
   s:=Popstring(dummymfp);
   if s<>'Z2' then GFRE_BT.CriticalAbort('2');
   s:=Popstring(dummymfp);
   if s<>'Dr3' then GFRE_BT.CriticalAbort('3');
   s:=Popstring(dummymfp);
   if s<>'V' then GFRE_BT.CriticalAbort('4');
  until j=100000;
  writeln('INTERLEAVE TEST A DONE');
 end;

 procedure ProConTest;
 begin

 end;


procedure dump_binary(p:pointer;const len:integer);
var i:integer;
    b:char;
begin
 for i:=1 to len do begin
  B:=PChar(p)^;
  if not (b in [' ','0'..'9','A'..'Z','a'..'z']) then b:=' ';
  write(format('%2.2x(%1.1s) ',[byte(pbyte(p)^),string(char(b))]));
  if i mod 16=0 then writeln;
  inc(p);
 end;
 writeln;
end;

 var i,j:cardinal;
     s:string;
     cpu_cnt:Integer;
     MPRO:TMultiProd;
     MCON:Array [1..32] of TMultiConsumer;
     msum:cardinal;
     err:TFRE_OS_Result;
     label bypass;

begin
 randomize;
 LCK:= paramstr(1)<>'';
 if lck then CS:=TFOS_LOCK.Create;

 writeln('LOCKING IS ',BoolToStr(LCK,'ON','OFF'));
 cpu_cnt := GFRE_CPU.Logical;
writeln('CPU COUNT = ', cpu_cnt);
 //goto bypass;
// Basic;
// InterLeavedPopStatic;
// Interleave;
RDLN    := true;
NOWRITE := true;
 Basic;//exit;
 MarkFree;//exit;
 MarkFreeMulti;//exit;
// writeln('----');
 writeln('COMPACT Q Test');
 writeln('CPU (threads) =',cpu_cnt);
 writeln('Do mass test ? (y/N)');
 ReadLn(s);
 if s='y' then begin
  writeln('MASS GENERATION TEST');
  MassTest;
  writeln('MASS GENERATION TEST (LFQ)');
  LFQ_Test;
 end;
 writeln('Do PRO/CON test ? (y/N)');
 Readln(s);
 if s='y' then begin
   writeln('PRESS PRODUCER/CONSUMER TEST');
//   LCK:=false;
   PCQ:=TFOS_PARTVARQ.Create(64);
   for i:=1 to 5 do begin
    PRO:=TProducer.Create(true);
    CON:=TConsumer.Create(true);
    Pro.Resume;con.Resume;
    sleep(2000);
    pro.Terminate;
    pro.WaitFor;
    con.Terminate;
    con.WaitFor;
    PRO.free;
    CON.free;
   end;
 end;
bypass:
 writeln('PRODUCER / MULTICONSUMER Test');
 SetLength(TestAtt,100000);
 writeln('FILLING TEST ARRAY');
 for i:=0 to Length(TestAtt)-1 do begin
   TestAtt[i]:=inttostr(i)+';'+DateTimeToStr(now);
   if i mod 100000=0 then write(' ',i,' ');
 end;
 writeln;
 writeln('FILLING TEST ARRAY - DONE');
 MUL:=TFOS_PARTVARQ.Create(1024,cpu_cnt-1); // smalles possible q
 repeat
  MPRO:=TMultiProd.Create(true);
  for j:=1 to cpu_cnt-1 do begin
   MCON[j]:=TMultiConsumer.Create(true);
   MCON[j].cpu:=j;
  end;
  MPRO.Resume;
  for j:=1 to cpu_cnt-1 do begin
   MCON[j].Resume;
  end;
  sleep(1000);
  mpro.Terminate;
  mpro.WaitFor;
  mpro.Free;
  msum:=0;
  for j:=1 to cpu_cnt-1 do begin
   MCON[j].Terminate;
   MCON[j].WaitFor;
   msum:=msum+MCON[j].accounted;
   MCON[j].Free;
  end;
  writeln(format('------------------     ---->>> DONE >> Consumed in sum = %10.0n',[single(msum)]));
 until false;
end.

