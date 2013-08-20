program test_sort;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

uses
  {$IFDEF UNIX}
  cthreads,BaseUnix,Unix,
  {$ENDIF}
  Sysutils,Classes, FOS_GENERIC_SORT,FOS_REDBLACKTREE_GEN;


const
  SEED = 123;
  SIZE = 10000000;
  RUNS = 1;
type
  TSortType   = NativeInt;
  PSortType   = ^TSortType;
  TSortArray  = array of TSortType;

  function SORT_CMP(const a,b:TSortType):NativeInt;
  begin
    result := a-b;
  end;

  function quick_sort_partition(var dst:TSortArray; const left, right, pivot : NativeInt ):NativeInt; inline;
  var value : TSortType;
      swap  : TSortType;
      index : NativeInt;
      i     : NativeInt;
  begin
    value := dst[pivot]; dst[pivot] := dst[right]; dst[right] := value;
    index := left;
    i     := left;
    while(i < right) do begin
      if SORT_CMP(dst[i],value) <= 0 then begin
        swap := dst[i]; dst[i] := dst[index]; dst[index] := swap;
        inc(index);
      end;
      inc(i);
    end;
    swap   := dst[right]; dst[right] := dst[index]; dst[index] := swap;
    result := index;
  end;

  function binary_insertion_find(dst : PSortType; const  x : TSortType ; const size : NativeInt):nativeint; inline;
  var l,c,r,i,val  : NativeInt;
      lx,cx,rx     : TSortType;
  begin
    l  := 0;
    r  := size - 1;
    c  := SarLongint(r,1);
    lx := dst[l];
    if SORT_CMP(x, lx) < 0 then begin
      exit(0)
    end else
    if SORT_CMP(x, lx) = 0 then begin
      i := 1;
      while (SORT_CMP(x, dst[i]) = 0) do inc(i);
      exit(i);
    end;
    rx := dst[r];
    cx := dst[c];
    while true do begin
      val := SORT_CMP(x, cx);
      if val < 0 then begin
        if (c - l) <= 1 then exit(c);
        r  := c;
        rx := cx;
      end else
      if val > 0 then begin
        if (r - c) <= 1 then exit(c + 1);
        l  := c;
        lx  := cx;
      end else begin
        repeat
          inc(c);
          cx := dst[c];
        until SORT_CMP(x, cx) <> 0;
        exit(c);
      end;
      c  := l + sarLongint((r - l),1);
      cx := dst[c];
    end;
  end;

///* Binary insertion sort, but knowing that the first "start" entries are sorted.  Used in timsort. */
  procedure binary_insertion_sort_start(dst:PSortType; const start, size : NativeInt);inline;
  var i,j,location : NativeInt;
      x            : TSortType;
  begin
    for i := start to  size-1 do begin
      // If this entry is already correct, just move along
      if (SORT_CMP(dst[i-1], dst[i]) <= 0) then continue;
      // Else we need to find the right place, shift everything over, and squeeze in
      x        := dst[i];
      location := binary_insertion_find(dst, x, i);
      j := i - 1;
      while (j >= location) do begin
        dst[j + 1] := dst[j];
        dec(j);
      end;
      dst[location] := x;
    end;
  end;

///* Binary insertion sort */
 procedure BINARY_INSERTION_SORT(dst : PSortType ; const size:NativeInt);inline;
 begin
   binary_insertion_sort_start(dst, 1, size);
 end;

  procedure quick_sort_recurse(var dst:TSortArray ; const left, right : NativeInt);
  var pivot,new_pivot : NativeInt;
  begin
    if right <= left then exit;
    if (right - left +1 ) < 16 then begin
      BINARY_INSERTION_SORT(@dst[left], right - left + 1);
      exit;
    end;
    pivot     := left + SarLongint((right - left),1);
    new_pivot := quick_sort_partition(dst,left,right,pivot);
    quick_sort_recurse(dst,left,new_pivot-1);
    quick_sort_recurse(dst,new_pivot+1,right);
  end;

  procedure quick_sort(var dst:TSortArray;const size:NativeInt);
  begin
    quick_sort_recurse(dst,0,size-1)
  end;


  procedure MERGE_SORT(dst:PSortType; const size:NativeInt);
  var middle,lout,i,j : NativeInt;
      newdst          : PSortType;
  begin
    if size < 4 then begin
      BINARY_INSERTION_SORT(dst, size);
      exit;
    end;
    middle := size div 2;
    MERGE_SORT(dst, middle);
    MERGE_SORT(@dst[middle], size - middle);

    Getmem(newdst,size * sizeof(TSortType));
    lout  := 0;
    i     := 0;
    j     := middle;
    while lout <> size do begin
      if i < middle then begin
        if j < size then begin
          if SORT_CMP(dst[i], dst[j]) <= 0 then begin
            newdst[lout] := dst[i]; inc(i);
          end  else begin
            newdst[lout] := dst[j]; inc(j);
          end;
        end else begin
          newdst[lout] := dst[i]; inc(i);
        end;
      end else begin
        newdst[lout] := dst[j]; inc(j);
      end;
      inc(lout);
    end;
    move(newdst[0] , dst^,  size * sizeof(TSortType) );
  end;

  procedure verify(const dst : PSortType);
  var i,j:NativeInt;
  begin
    for i := 1 to size-1 do begin
      if dst[i-1]>dst[i] then begin
        writeln('verify failed at ',i,' (',dst[i-1],' > ',dst[i],')');
        exit;
      end;
    end;
  end;



procedure heap_sift_down(const dst : PSortType; const sstart,send : NativeInt);
var root  : NativeInt;
    child : NativeInt;
    tswap : TSortType;
begin
  root := sstart;
  while ( (root SHL 1) <= send) do begin
    child := root SHL 1;
    if ((child < send) AND (SORT_CMP(dst[child], dst[child + 1]) < 0)) then begin
      inc(child);
    end;
    if (SORT_CMP(dst[root], dst[child]) < 0) then begin
      tSwap      := dst[root];
      dst[root]  := dst[child];
      dst[child] := tSwap;
      root := child;
    end else begin
      exit;
    end;
  end;
end;

procedure heapify(const dst : PSortType ; const size : NativeInt);
var start : NativeInt;
begin
  start := SarLongint(size);
  while start >= 0 do begin
    heap_sift_down(dst, start, size - 1);
    dec(start);
  end;
end;

procedure HEAP_SORT(const dst : PSortType ; const size:NativeInt);
var send  : NativeInt;
    Tswap : TSortType;
begin
  heapify(dst, size);
  send := size - 1;
  while send > 0 do begin
    tSwap     := dst[send];
    dst[send] := dst[0];
    dst[0]    := tSwap;
    heap_sift_down(dst, 0, send - 1);
    dec(send);
  end;
end;

procedure dump(const dst : TSortArray);
var i: NativeInt;
begin
  writeln('');
  for i:=0 to high(dst) do begin
    write(' ',dst[i]);
  end;
  writeln('');
end;

function utime : double; inline;
var tz:timeval;
begin
  //result := GFRE_BT.Get_Ticks_us;
  fpgettimeofday(@tz,nil);
  result:= double(tz.tv_sec)*1000000.0 + double(tz.tv_usec);
end;

procedure fill(var dst : TSortArray);
var i:NativeInt;
begin
  RandSeed := SEED;
  for i:=0 to high(dst) do begin
    //dst[i] := Random(MaxInt);
    dst[i] := high(dst)-i;
  end;
end;

type

  TSortRec= record
    key  : Pstring;
    data : Pointer;
  end;
  PSortRec = ^TSortRec;

  TSortRec2= record
    key0 : NativeInt;
    key1 : Pstring;
  end;
  PSortRec2 = ^TSortRec2;
  TGenSort     = specialize OFRE_GenericSorter<TSortRec>;
  TRB_SortTree = specialize TGFOS_RBTree<TSortRec2,PString>;



function xSort_Cmp(const a,b:TSortRec):Nativeint;inline;
begin
//  result := strtoint(a.key^) - strtoint(b.key^);
   result := CompareStr(a.key^,b.key^);
end;

function TreeCompare(const a,b : TSortRec2):NativeInt;
begin

end;


procedure RBDump(const mt:TRB_SortTree);
var key:TSortRec2;
    val:PString;

  procedure DumpItems(const Item:PString);
  begin
    writeln(Item^);
  end;

var
  i: Integer;
begin
    mt.ForAllItemsRange(mt.count-6,5,@DumpItems);
end;

procedure run_test;
var start_time,end_time,total_time: Double;
    dst,arr : TSortArray;
    i       : NativeInt;
    ms      : OIntSorter;
    mx      : TGenSort;
    mt      : TRB_SortTree;
    test    : ^NativeInt;
    tst     : PSortRec;
    dat     : Array of String;
    dit     : Array of NativeInt;
    ik      : TSortRec2;
begin
  writeln('Sizeof ',sizeof(TSortRec));
  SetLength(dst,SIZE);
  SetLength(arr,SIZE);
  fill(arr);
  for i:=1 to RUNS do begin
    Move(arr[0],dst[0],Length(arr)*sizeof(arr[0]));
    start_time := utime;
      quick_sort(dst,length(dst));
    end_time   := utime;
    total_time := total_time + (end_time-start_time);
  end;
  verify(@dst[0]);

  writeln('qsort ',total_time/RUNS:2:2,' runs ',RUNS,' size ',SIZE);
  total_time := 0;
  for i:=1 to RUNS do begin
    Move(arr[0],dst[0],Length(arr)*sizeof(arr[0]));
    start_time := utime;
      MERGE_SORT(@dst[0],length(dst));
    end_time   := utime;
    total_time := total_time + (end_time-start_time);
  end;
  verify(@dst[0]);
  writeln('mergesort ',total_time/RUNS:2:2,' runs ',RUNS,' size ',SIZE);

  total_time := 0;
  for i:=1 to RUNS do begin
    Move(arr[0],dst[0],Length(arr)*sizeof(arr[0]));
    start_time := utime;
      HEAP_SORT(@dst[0],length(dst));
    end_time   := utime;
    total_time := total_time + (end_time-start_time);
  end;
  verify(@dst[0]);
  writeln('heapsort ',total_time/RUNS:2:2,' runs ',RUNS,' size ',SIZE);

  writeln('');
  writeln('---');
  writeln('');

  total_time := 0;
  test := Getmem(sizeof(NativeInt)*SIZE);
  for i:=1 to RUNS do begin
    Move(arr[0],test[0],Length(arr)*sizeof(arr[0]));
    ms.InitData(@SORT_CMP,@test[0],size);
    start_time := utime;
      ms.quick_sort;
    end_time   := utime;
    total_time := total_time + (end_time-start_time);
  end;
  verify(test);
  writeln('gen quicksort ',total_time/RUNS:2:2,' runs ',RUNS,' size ',SIZE);

  total_time := 0;
  test := Getmem(sizeof(NativeInt)*SIZE);
  for i:=1 to RUNS do begin
    Move(arr[0],test[0],Length(arr)*sizeof(arr[0]));
    ms.InitData(@SORT_CMP,@test[0],size);
    start_time := utime;
      ms.merge_sort;
    end_time   := utime;
    total_time := total_time + (end_time-start_time);
  end;
  verify(test);
  writeln('gen mergesort ',total_time/RUNS:2:2,' runs ',RUNS,' size ',SIZE);

  total_time := 0;
  tst := Getmem(sizeof(TSortRec)*SIZE);
  writeln('init');
  setlength(dat,SIZE);
  setlength(dit,SIZE);
  for i:=0 to size-1 do begin
     dit[i]     := size-i;//  Random(MaxInt);
     dat[i]     := inttostr(dit[i]);
     tst[i].key := @dat[i];
//     tst[i].data:= IntToStr(tst[i].key);
  end;
  writeln('done');
  //total_time:=0;
  //start_time := utime;
  //  mx.initdata(tst,size);
  //  mx.heap_sort;
  //end_time   := utime;
  //total_time := total_time + (end_time-start_time);
  //writeln('gen tsort sort ',total_time:2:2,' size ',SIZE);


  writeln('start tree sort');
  mt := TRB_SortTree.Create(@Treecompare);
  total_time:=0;
  start_time := utime;
  for i:=0 to length(dat)-1 do begin
    ik.key0:=dit[i];
    ik.key1:=@dat[i];
    mt.Add(ik,@dat[i]);
  end;
  end_time   := utime;
  total_time := total_time + (end_time-start_time);
  writeln('gen tree sort ',total_time:2:2,' size ',SIZE);

  total_time:=0;
  start_time := utime;
  RBDump(mt);
  end_time   := utime;
  total_time := total_time + (end_time-start_time);
  writeln('gen tree scan ',total_time:2:2,' size ',SIZE);
 // mt.switchmode(rbStatic);
//  writeln('gen tree sort ',total_time:2:2,' size ',SIZE);

//  verify(arr);
 // dump(dst);
end;

begin
  run_test;
end.

