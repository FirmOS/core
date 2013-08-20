unit test_genrb_tree;

{$IFDEF FPC}
  {$MODE objfpc} {$H+}
{$ENDIF}

interface

{$ASSERTIONS ON}

uses  Classes, SysUtils,FOS_REDBLACKTREE_GEN;


procedure test_rb_ss(const count:cardinal;const rand:boolean);
procedure test_rb_ii(const count:cardinal;const rand:boolean);
procedure test_rb_irec(const count:cardinal;const rand:boolean);
procedure test_rb_interface(const count:cardinal;const rand:boolean);
procedure test_rb_objects(const count:cardinal;const rand:boolean);
procedure test_rb_localproc;


implementation
//very simple timing functions

{$IFDEF UNIX}
 uses UnixType,Unix,BaseUnix,Dateutils,Initc;
{$ENDIF}

{$IFDEF UNIX}
function FOS_GET_TICKS_MS:Int64; register;inline;
var tz:timeval;
    intern:int64;
begin
 fpgettimeofday(@tz,nil);
 intern:=int64(tz.tv_sec)*1000*1000;
 intern:=intern+tz.tv_usec;
 result:=intern div 1000;
end;
{$ELSE}
function FOS_GET_TICKS_MS:Int64; register;inline;
var Stamp: TTimeStamp;
begin
  Stamp  := DateTimeToTimeStamp(Now);
  Result := Stamp.Time;
end;
{$ENDIF}

var _randrange:int64;



procedure test_rb_ss(const count:cardinal;const rand:boolean);
var TSS:TFOS_RB_Tree_SS;
    i:integer;
    key,val:string;
    colls:cardinal;
    ov,nv:int64;

    function randkey:string;inline;
    begin
     result:=inttostr(Random(_randrange));
    end;

begin
 writeln('STRING/STRING Test');
 _randrange:=int64(count)*int64(count)*1000;
 TSS:=TFOS_RB_Tree_SS.Create(@Default_RB_String_Compare);
 TSS.Add('K0','V0');
 val:='VAL WRONG';
 assert(TSS.AddCheck('K0',val)=false);
 TSS.Add('K0',val);
 assert(val='V0');
 writeln('EXPECTED ',val,' OK');
 for i:=1 to 5 do begin
  TSS.Add('K'+inttostr(i),'V'+inttostr(i));
 end;
 if TSS.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TSS.FindNext(key,val);
 end;
 TSS.Clear(nil);
 //TSS.free;

 writeln;
 if TSS.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=',val,' ');
   until not TSS.FindPrev(key,val);
 end;

 writeln;
 randomize;colls:=0;
 TSS.Clear(nil);
 //Insert Test
 ov:=FOS_GET_TICKS_MS;
 for i:=count downto 1 do begin
  if i mod 100000=0 then write(i div 100000,' ');
  if rand then begin
   if not TSS.Add(randkey,inttostr(i)) then begin
    inc(colls);
   end;
  end else begin
   if not TSS.Add(inttostr(i),inttostr(i)) then begin
    writeln('unexpected collision');
    halt;
   end;
  end;
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 if rand then begin
  writeln('Added randomized ',count,' count (',tss.Count,') / ',colls,' collisions ',nv,' ms');
 end else begin
  writeln('Added linear backward ',count,' count (',tss.count,') / ',colls,' collisions in ',nv,' ms');
 end;

 //Linear Scan Test
 ov:=FOS_GET_TICKS_MS;
 if TSS.FirstNode(key,val) then begin  // Scan it forward
   repeat
    i:=strtoint(val);
    if i mod 100000=0 then write(i div 100000,' '); // Funny output in random mode !
   until not TSS.FindNext(key,val);
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 writeln('Linear scan in ',nv,' ms');

 //Access Test
 for i:=1 to 10 do begin
  key:=inttostr(random(count));
  ov:=FOS_GET_TICKS_MS;
  TSS.Find(key,val);
  nv:=FOS_GET_TICKS_MS;
  nv:=nv-ov;
  writeln('Access ',val,' in ',nv,' ms');
 end;

 ov:=FOS_GET_TICKS_MS;
 TSS.Clear(nil);
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln('Cleared in ',nv,' ms');
 TSS.Free;
end;

procedure test_rb_ii(const count:cardinal;const rand:boolean);
var TII:TFOS_RB_Tree_II;
    i:integer;
    key,val:integer;
    colls:cardinal;
    ov,nv:int64;
    lw:integer;

    function randkey:integer;inline;
    begin
     result:=Random(_randrange);
    end;

    procedure NextPrevTesting(fkey:integer);
    var
     found:boolean;
     key:integer;
    begin

     writeln('---------------------------------');
     writeln('Testing NextPrev '+inttostr(fkey));
     key:=fkey;
     found:=TII.FindNext(key,val);
     if found then begin
      writeln('FOUND NEXT',key);
     end else begin
      writeln('NOT FOUND NEXT');
     end;
     key:=fkey;
     found:=TII.FindNextOrEQ(key,val);
     if found then begin
      writeln('FOUND NEQ',key);
     end else begin
      writeln('NOT FOUND NEQ');
     end;
     key:=fkey;
     found:=TII.FindPrev(key,val);
     if found then begin
      writeln('FOUND PREV',key);
     end else begin
      writeln('NOT FOUND PREV');
     end;
     key:=fkey;
     found:=TII.FindPrevOrEQ(key,val);
     if found then begin
      writeln('FOUND PEQ',key);
     end else begin
      writeln('NOT FOUND PEQ');
     end;
    end;

begin
 //writeln('Integer/Integer Test');
 _randrange:=int64(count)*int64(count)*1000;
 TII:=TFOS_RB_Tree_II.Create(@Default_RB_Integer_Compare);
 TII.Add(0,0);
 val:=-99;
 assert(TII.AddCheck(0,val)=false);
 assert(val=0);
 //writeln('EXPECTED ',val,' OK');
 for i:=1 to 5 do begin
  TII.Add(i,i);
 end;
 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;
 writeln;
 if TII.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=',val,' ');
   until not TII.FindPrev(key,val);
 end;
 writeln;

 TII.Clear(nil);

 for i:=1 to 12 do begin
  TII.Add(i,i*i);
 end;
 TII.Delete(11,i);

 TII.SwitchMode(rbStatic);

 for i:=1 to 12 do begin
  if TII.Find(i,val) then begin
   write('Found:',i,'=',val,' ');
  end else begin
   write('Not found:',i,' ');
  end;
 end;

 assert(TII.Exists(10)=true);
 assert(TII.Exists(11)=false);

 writeln('Updateing 8 to 88 ');
 TII.Update(8,88);
 if TII.Find(8,val) then begin
  write('Found:',8,'=',val,' ');
  assert(val=88);
 end;
 writeln;
 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;
 writeln;
 if TII.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=',val,' ');
   until not TII.FindPrev(key,val);
 end;
 writeln;

 for key:=0 to 20 do begin
  if TII.GetIndex(key,lw) then begin
   writeln('GetIndex:',key,' idx=',lw);
  end else begin
   writeln('No Index for :',key,' ');
  end;
 end;

 for i:=-1 to 20 do begin
  if TII.GetDirect(i,key,val) then begin
   writeln('Get Direct:',i,' key',key,'=',val,' ');
  end else begin
   writeln('Not in Array:',i,' ');
  end;
 end;

 TII.Clear(nil);

 TII.Add(1,1);
 TII.Add(5,25);
 TII.Add(6,36);
 TII.Add(7,49);
 TII.Add(11,11*11);

 writeln('DYNAMIC');

 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;

 NextPrevTesting(7);
 NextPrevTesting(8);
 NextPrevTesting(11);
 NextPrevTesting(12);
 NextPrevTesting(1);
 NextPrevTesting(0);

 writeln('STATIC');

 TII.SwitchMode(rbStatic);

 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;

 NextPrevTesting(7);
 NextPrevTesting(8);
 NextPrevTesting(11);
 NextPrevTesting(12);
 NextPrevTesting(1);
 NextPrevTesting(0);

 writeln;

 TII.Clear(nil);

 TII.Add(1,1);

 writeln('DYNAMIC');

 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;

 NextPrevTesting(1);

 TII.Clear(nil);

 TII.Add(1,1);
 TII.Add(5,25);

 writeln('DYNAMIC');

 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val,' ');
   until not TII.FindNext(key,val);
 end;

 NextPrevTesting(1);

 exit;

 randomize;colls:=0;
 TII.Clear(nil);
 //Insert Test
 ov:=FOS_GET_TICKS_MS;
 for i:=count downto 1 do begin
  if i mod 100000=0 then write(i div 100000,' ');
  if rand then begin
   if not TII.Add(randkey,i) then begin
    inc(colls);
   end;
  end else begin
   if not TII.Add(i,i) then begin
    writeln('unexpected collision');
    halt;
   end;
  end;
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 if rand then begin
  writeln('Added randomized ',count,' count (',TII.Count,') / ',colls,' collisions ',nv,' ms');
 end else begin
  writeln('Added linear backward ',count,' count (',TII.count,') / ',colls,' collisions in ',nv,' ms');
 end;

 //Linear Scan Test
 ov:=FOS_GET_TICKS_MS;
 if TII.FirstNode(key,val) then begin  // Scan it forward
   repeat
    i:=val;
    if i mod 100000=0 then write(i div 100000,' '); // Funny output in random mode !
   until not TII.FindNext(key,val);
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 writeln('Linear scan in ',nv,' ms');

 //Access Test
 for i:=1 to 10 do begin
  key:=random(count);
  ov:=FOS_GET_TICKS_MS;
  TII.Find(key,val);
  nv:=FOS_GET_TICKS_MS;
  nv:=nv-ov;
  writeln('Access ',val,' in ',nv,' ms');
 end;

 ov:=FOS_GET_TICKS_MS;
 TII.Clear(nil);
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln('Cleared in ',nv,' ms');
 TII.Free;
end;

type
   RRecord=packed record
    val:string;
    extra:integer;
   end;

   TREC_Tree=specialize TGFOS_RBTree<integer,RRecord>;

   function DefaultRecord:RRecord;
   begin
    result.val:='RED';
    result.extra:=4711;
   end;


procedure test_rb_irec(const count:cardinal;const rand:boolean);
var TIR:TREC_Tree;
    i:integer;
    key:integer;
    val:RRecord;
    colls:cardinal;
    ov,nv:int64;

    function randkey:integer;inline;
    begin
     result:=Random(_randrange);
    end;


begin
 writeln('Integer/Custom Record Test');
 _randrange:=int64(count)*int64(count)*1000;
 TIR:=TREC_Tree.Create(@Default_RB_Integer_Compare);
 val.extra:=123;
 val.val:='SUPER';
 TIR.Add(0,val);
 val.val:='BAD';
 val.extra:=123;
 assert(TIR.AddCheck(0,val)=false);
 assert(val.val='SUPER');
 writeln('EXPECTED ',val.val,' OK');
 for i:=1 to 5 do begin
  val.val:='REC '+inttostr(i);
  val.extra:=i*1000;
  TIR.Add(i,val);
 end;
 if TIR.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=(',val.val,',',val.extra,') ');
   until not TIR.FindNext(key,val);
 end;
 writeln;
 if TIR.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=(',val.val,',',val.extra,') ');
   until not TIR.FindPrev(key,val);
 end;
 writeln;
 randomize;colls:=0;
 TIR.Clear(nil);
 //Insert Test
 ov:=FOS_GET_TICKS_MS;
 for i:=count downto 1 do begin
  if i mod 100000=0 then write(i div 100000,' ');
  if rand then begin
   val.val:=inttostr(i);
   val.extra:=i*1000;
   if not TIR.Add(randkey,val) then begin
    inc(colls);
   end;
  end else begin
   val.val:=inttostr(i);
   val.extra:=i*1000;
   if not TIR.Add(i,val) then begin
    writeln('unexpected collision');
    halt;
   end;
  end;
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 if rand then begin
  writeln('Added randomized ',count,' count (',TIR.Count,') / ',colls,' collisions ',nv,' ms');
 end else begin
  writeln('Added linear backward ',count,' count (',TIR.count,') / ',colls,' collisions in ',nv,' ms');
 end;

 //Linear Scan Test
 ov:=FOS_GET_TICKS_MS;
 if TIR.FirstNode(key,val) then begin  // Scan it forward
   repeat
    i:=strtoint(val.val);
    if i mod 100000=0 then write(i div 100000,' '); // Funny output in random mode !
   until not TIR.FindNext(key,val);
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 writeln('Linear scan in ',nv,' ms');

 //Access Test
 for i:=1 to 10 do begin
  key:=random(count);
  ov:=FOS_GET_TICKS_MS;
  TIR.Find(key,val);
  nv:=FOS_GET_TICKS_MS;
  nv:=nv-ov;
  writeln('Access (',val.val,',',val.extra,') in ',nv,' ms');
 end;

 ov:=FOS_GET_TICKS_MS;
 TIR.Clear(nil);
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln('Cleared in ',nv,' ms');
 writeln('Default Test');
 TIR.Find(-1234,val);
 writeln('Val: (',val.val,',',val.extra,')');
 TIR.Free;
end;

type
  { TStoreObject }
  IStoreIntf=interface
    procedure SetVal(val:string);
    function  GetVal:String;
  end;

  TStoreObject=class(TInterfacedObject,IStoreIntf)
  private
   _val:string;
  public
   procedure SetVal(val:string);
   function  GetVal:String;
  end;

{ TStoreObject }
  procedure TStoreObject.SetVal(val: string);
  begin
   _val:=val;
  end;

  function TStoreObject.GetVal: String;
  begin
   result:=_val;
  end;

type TIntf_Tree=specialize TGFOS_RBTree<integer,IStoreIntf>;

function DefaultIntf:IStoreIntf;
var o:TStoreObject;
begin
 o:=TStoreObject.Create;
 o._val:='undef';
 result:=o;
end;

procedure test_rb_interface(const count:cardinal;const rand:boolean);
var TIF:TIntf_Tree;
    i:integer;
    key:integer;
    val:IStoreIntf;
    colls:cardinal;
    ov,nv:int64;

    function randkey:integer;inline;
    begin
     result:=Random(_randrange);
    end;

    function NewVal:IStoreIntf;
    begin
     result:=TStoreObject.Create;
    end;

begin
 writeln('Integer/Interface Test');
 _randrange:=int64(count)*int64(count)*1000;
 TIF:=TIntf_Tree.Create(@Default_RB_Integer_Compare);
 val:=NewVal;val.SetVal('123');
 TIF.Add(0,val);
 val:=NewVal;val.SetVal('BAD');
 assert(TIF.AddCheck(0,val)=false);
 assert(val.GetVal='123');
 writeln('EXPECTED ',val.GetVal,' OK');
 for i:=1 to 5 do begin
  val:=NewVal;
  val.SetVal('REC '+inttostr(i));
  TIF.Add(i,val);
 end;
 if TIF.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val.GetVal,' ');
   until not TIF.FindNext(key,val);
 end;
 writeln;
 if TIF.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=',val.GetVal,' ');
   until not TIF.FindPrev(key,val);
 end;
 writeln;
 randomize;colls:=0;
 TIF.Clear(nil);
 //Insert Test
 ov:=FOS_GET_TICKS_MS;
 for i:=count downto 1 do begin
  val:=NewVal;
  if i mod 100000=0 then write(i div 100000,' ');
  if rand then begin
   val.SetVal(inttostr(i));
   if not TIF.Add(randkey,val) then begin
    inc(colls);
   end;
  end else begin
   val.SetVal(inttostr(i));
   if not TIF.Add(i,val) then begin
    writeln('unexpected collision');
    halt;
   end;
  end;
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 if rand then begin
  writeln('Added randomized ',count,' count (',TIF.Count,') / ',colls,' collisions ',nv,' ms');
 end else begin
  writeln('Added linear backward ',count,' count (',TIF.count,') / ',colls,' collisions in ',nv,' ms');
 end;

 //Linear Scan Test
 ov:=FOS_GET_TICKS_MS;
 if TIF.FirstNode(key,val) then begin  // Scan it forward
   repeat
    i:=strtoint(val.GetVal);
    if i mod 100000=0 then write(i div 100000,' '); // Funny output in random mode !
   until not TIF.FindNext(key,val);
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 writeln('Linear scan in ',nv,' ms');

 //Access Test
 for i:=1 to 10 do begin
  key:=random(count);
  ov:=FOS_GET_TICKS_MS;
  TIF.Find(key,val);
  nv:=FOS_GET_TICKS_MS;
  nv:=nv-ov;
  if assigned(val) then begin
   writeln('Access ',val.GetVal,' in ',nv,' ms');
  end else begin
   writeln('not found');
  end;
 end;

 ov:=FOS_GET_TICKS_MS;
 TIF.Clear(nil);
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln('Cleared in ',nv,' ms');
 writeln('Default Test');
 TIF.Find(-1234,val);
 if assigned(val) then begin
  writeln('Val: ',val.GetVal);
 end;
 TIF.Free;
end;

type TObj_Tree=specialize TGFOS_RBTree<integer,TStoreObject>;

function DefaultObj:TStoreObject;
var o:TStoreObject;
begin
 o:=TStoreObject.Create;
 o._val:='undef';
 result:=o;
end;

procedure FreeObject(const Obj:TStoreObject);
begin
 obj.free;
end;

procedure test_rb_objects(const count:cardinal;const rand:boolean);
var TOF:TObj_Tree;
    i:integer;
    key:integer;
    val,nval:TStoreObject;
    colls:cardinal;
    ov,nv:int64;
    nf:boolean;

    function randkey:integer;inline;
    begin
     result:=Random(_randrange);
    end;

    function NewVal:TStoreObject;
    begin
     result:=TStoreObject.Create;
    end;

begin
 writeln('Integer/Object Test');
 _randrange:=int64(count)*int64(count)*1000;
 TOF:=TObj_Tree.Create(@Default_RB_Integer_Compare);
 val:=NewVal;val.SetVal('123');
 TOF.Add(0,val);
 val:=NewVal;val.SetVal('BAD');
 nval:=nil;
 assert(TOF.AddCheck(0,nval)=false);
 val.free;
 assert(nval.GetVal='123');
 writeln('EXPECTED ',nval.GetVal,' OK');
 for i:=1 to 5 do begin
  val:=NewVal;
  val.SetVal('REC '+inttostr(i));
  TOF.Add(i,val);
 end;
 if TOF.FirstNode(key,val) then begin  // Scan it forward
   repeat
     write(key,'=',val.GetVal,' ');
   until not TOF.FindNext(key,val);
 end;
 val.free; // WARNING -> THE LAST FIND NEXT DELIVERS THE DEFAULT TYPE = TOBJECT = MEMLEAK
 writeln;
 if TOF.LastNode(key,val) then begin // Scan it backward
   repeat
     write(key,'=',val.GetVal,' ');
   until not TOF.FindPrev(key,val);
 end;
 val.free; // WARNING -> THE LAST FIND NEXT DELIVERS THE DEFAULT TYPE = TOBJECT = MEMLEAK
 writeln;
 randomize;colls:=0;
 TOF.ClearN(@FreeObject);
 //Insert Test
 ov:=FOS_GET_TICKS_MS;
 for i:=count downto 1 do begin
  val:=NewVal;
  if i mod 100000=0 then write(i div 100000,' ');
  if rand then begin
   val.SetVal(inttostr(i));
   if not TOF.Add(randkey,val) then begin
    val.free;
    val:=nil;
    inc(colls);
   end;
  end else begin
   val.SetVal(inttostr(i));
   if not TOF.Add(i,val) then begin
    writeln('unexpected collision');
    halt;
   end;
  end;
 end;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 if rand then begin
  writeln('Added randomized ',count,' count (',TOF.Count,') / ',colls,' collisions ',nv,' ms');
 end else begin
  writeln('Added linear backward ',count,' count (',TOF.count,') / ',colls,' collisions in ',nv,' ms');
 end;

 //Linear Scan Test
 ov:=FOS_GET_TICKS_MS;
 if TOF.FirstNode(key,val) then begin  // Scan it forward
   repeat
    i:=strtoint(val.GetVal);
    if i mod 100000=0 then write(i div 100000,' '); // Funny output in random mode !
   until not TOF.FindNext(key,val);
 end;
 val.free;
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln;
 writeln('Linear scan in ',nv,' ms');

 //Access Test
 for i:=1 to 10 do begin
  key:=random(count);
  ov:=FOS_GET_TICKS_MS;
  nf:=not TOF.Find(key,val);
  nv:=FOS_GET_TICKS_MS;
  nv:=nv-ov;
  if assigned(val) then begin
   writeln('Access ',val.GetVal,' in ',nv,' ms');
  end else begin
   writeln('not found');
  end;
  if nf then val.free; // free undef object !
 end;


 ov:=FOS_GET_TICKS_MS;
 TOF.ClearN(@FreeObject);
 nv:=FOS_GET_TICKS_MS;
 nv:=nv-ov;
 writeln('Cleared in ',nv,' ms');
 writeln('Default Test');
 TOF.Find(-1234,val);
 if assigned(val) then writeln('Val: ',val.GetVal);
 val.Free;
 TOF.Free;
end;


procedure test_rb_localproc;
var x:integer;
    TIR:TREC_Tree;
    val:RRecord;
    i: Integer;

  procedure LocalTest(key:PInteger;val:RRecord);
  begin
    writeln(key^,' ',val.val,' ',val.extra,' ',x);
  end;

begin
 writeln('Integer/Custom Record Test');
 _randrange:=10000000;

 TIR:=TREC_Tree.Create(@Default_RB_Integer_Compare);

 for i:=1 to 10 do begin
  val.val:='REC '+inttostr(i);
  val.extra:=i*1000;
  TIR.Add(i,val);
 end;
 x:=33;
 TIR.ForAllItems_LM(@LocalTest);

 WriteLn;

 for i:=20 downto 11 do begin
  val.val:='REC '+inttostr(i);
  val.extra:=i*1000;
  TIR.Add(i,val);
 end;

 x:=44;
 TIR.ForAllItems_LM(@LocalTest);

 TIR.Clear(nil);

 writeln('static local procedure');

 for i:=1 to 10 do begin
  val.val:='REC '+inttostr(i);
  val.extra:=i*1000;
  TIR.Add(i,val);
 end;
 x:=33;
 WriteLn;

 for i:=20 downto 11 do begin
  val.val:='REC '+inttostr(i);
  val.extra:=i*1000;
  TIR.Add(i,val);
 end;

 x:=44;
 TIR.SwitchMode(rbStatic);
 TIR.ForAllItems_LM(@LocalTest);

 TIR.Free;
end;




end.

