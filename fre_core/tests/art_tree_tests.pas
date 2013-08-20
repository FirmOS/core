program art_tree_tests;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

uses
  //{$IFDEF UNIX}{$IFDEF UseCThreads}
  //cthreads,
  //{$ENDIF}{$ENDIF}
  //cmem,
  Classes, cpu, sysutils, fos_art_tree,unix;

function gettime : double;
var now_tv : timeval;
begin
  fpgettimeofday(@now_tv,nil);
  result := Double(now_tv.tv_sec) + Double(now_tv.tv_usec)/1000000.0;
end;

//type
//   TFOS_RB_Tree_TQ      = specialize TGFOS_RBTree<QWord,Qword>;

var start : double;
    n,rep : NativeUint;
    keys  : Array of UInt64;
    i,r   : NativeInt;
    key   : Array [0..7] of Byte;
    keyn  : Qword;
    //testt : TFOS_RB_Tree_TQ;
    art_tree : TFRE_ART_TREE;
    value : NativeUint;

  procedure art_scan(const mode:byte);

    procedure MyInc(var val:NativeUint);
    begin
      val := val;
    end;

    function MyBreak2(var val:NativeUint):boolean;
    begin
      val := val;
      result:=false;
    end;


    function MyBreak(var val:NativeUint):boolean;
    begin
      result:=false;
      //write(val,' ');
      if val =  10 then
          result:=true
        else
          result:=false;
    end;

    var mykey:array[0..7] of byte;
        lokey:array[0..7] of byte;
        hikey:array[0..7] of byte;
        mylen,val:NativeUint;

    procedure SetKey (var val:NativeUint;const key:PByte ; const key_len:NativeUint);
    begin
      mylen := key_len;
      move(key^,mykey,key_len);
    end;

    procedure RangeDump(var val:NativeUint ; const key:PByte ; const key_len:NativeUint ; var error : boolean);
    begin
      val := val;
      inc(mylen);
    end;

  begin
    case mode of
      0 : art_tree.LinearScan(@MyInc);
      1 : writeln('Break Scan : '+LineEnding,art_tree.LinearScanBreak(@MyBreak));
      2 : art_tree.LinearScanBreak(@MyBreak2);
      4 : begin
            mylen := Length(keys) div 4;
            Move(keys[mylen]         ,lokey[0],8);
            Move(keys[mylen+2*mylen] ,hikey[0],8);
            mylen := 0;
            art_tree.RangeScan(@lokey[0],@hikey[0],8,8,@RangeDump);
            writeln('Range Scanned ',mylen);
          end;
      9 : begin
            //art_tree.DumpTree(false,true);
            for i:=1 to art_tree.GetValueCount do
              begin
                art_tree.FirstKeyVal(@setkey);
                if not art_tree.RemoveBinaryKey(mykey,mylen,val) then
                  begin
                    writeln('BAD REMOVE ',mylen,' ',PQWord(@mykey)^,' ',SwapEndian(PQWord(@mykey)^));
                    abort;
                  end;
              end;
          end;
      end;
  end;


begin
   Randomize;
   raw_test;

   if Paramcount<>2 then begin
     writeln(format('usage: %s n 0|1|2'+LineEnding+'n: number of keys'+LineEnding+'0: sorted key'+LineEnding+'1: dense keys'+LineEnding+'2: sparse keys'+LineEnding,[paramstr(0)]));
     exit;
   end;
   start := gettime ;
   n := StrToInt64(paramstr(1));
   setlength(keys,n);
   for i:=0 to n-1 do begin
     keys[i] := i;
   end;
   if ParamStr(2)='2' then begin
     for i:=0 to n-1 do begin
       keys[i] := keys[i] or (Random($7fffffff) shl 32);
     end;
   end;
   if ParamStr(2)='3' then begin
     for i:=0 to n-1 do begin
       keys[i] := (Random($8fffffffffffffff));
     end;
   end;
   for i:=0 to n-1 do begin
     keys[i] := SwapEndian(keys[i]);
   end;
   writeln(format('buildup,%d,%2.5f',[n,gettime-start]));

   art_tree := TFRE_ART_TREE.Create;
   start := gettime ;
   for i := 0 to n-1 do begin
     art_tree.InsertBinaryKey(@keys[i],-8,i);
   end;
   writeln(format('insert,%d,%2.5f',[n,gettime-start]));

   start := gettime;
   for i:=0 to n-1 do
     begin
       if not art_tree.ExistsBinaryKey(@keys[i],8,value) then
         begin
           writeln('Failure Lookup ',i);
         end
       else
         begin
         end;
     end;
  writeln(format('lookup all,%d,%f',[n,gettime-start]));
  start := gettime;
  art_scan(0);
  writeln(format('linear scan,%d,%2.5f',[n,gettime-start]));
  start := gettime;
  art_scan(2);
  writeln(format('linear break scan,no hit,%d,%2.5f',[n,gettime-start]));
  art_scan(1);
  start := gettime;
  art_scan(4);
  writeln(format('range  scan ,%d,%2.5f',[n,gettime-start]));
  start := gettime;
  art_scan(9);
  writeln(format('linear delete first key scan ,%d,%2.5f',[n,gettime-start]));
  art_tree.Destroy;
end.

