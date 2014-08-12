unit fos_generic_sort;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
} 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

   { OFRE_GenericSorter }

   generic OFRE_GenericSorter<TSortType>=object
   private
     type
       TSortProc=function(const a,b:TSortType):NativeInt;
       PSortType=^TSortType;
     var
       FData : PSortType;
       FSort : TSortProc;
       FSize : NativeInt;
    procedure  binary_insertion_sort_start  (dst : PSortType; const start, size : NativeInt);inline;
    function   binary_insertion_find        (dst : PSortType; const  x : TSortType ; const size : NativeInt):nativeint; inline;
    function   quick_sort_partition         (var   dst:PSortType   ; const left, right, pivot : NativeInt ):NativeInt; inline;
    procedure  quick_sort_recurse           (var   dst:PSortType ; const left, right : NativeInt); inline;
    procedure  _merge_sort                  (const dst:PSortType; const size:NativeInt);
    procedure  heapify                      (const dst : PSortType ; const size : NativeInt);
    procedure  heap_sift_down               (const dst : PSortType; const sstart,send : NativeInt);
  public
    //procedure   xxx;virtual;abstract;
    procedure   InitData                     (const sort_function : TSortProc;const data:PSortType;const size:NativeInt);
    function    Data                         : PSortType;
    procedure   quick_sort                   ;
    procedure   binary_insertion_sort        ;
    function    binary_find                  (const x: TSortType):nativeint;
    procedure   merge_sort                   ;
    procedure   heap_sort                    ;
  end;

  OIntSorter=specialize OFRE_GenericSorter<NativeInt>;


implementation


function SORT_CMP(const a,b:NativeInt):integer; inline;
begin
  result := a-b;
end;


//procedure MERGE_SORT(dst:PSortType; const size:NativeInt);
//

procedure OFRE_GenericSorter.binary_insertion_sort_start(dst: PSortType; const start, size: NativeInt);
var i,j,location : NativeInt;
    x            : TSortType;
begin
  for i := start to  size-1 do begin
    if (FSORT(dst[i-1], dst[i]) <= 0) then continue;
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

function OFRE_GenericSorter.binary_insertion_find(dst: PSortType; const x: TSortType; const size: NativeInt): nativeint;
var l,c,r,i,val  : NativeInt;
    lx,cx,rx     : TSortType;
begin
  l  := 0;
  r  := size - 1;
  c  := SarLongint(r,1);
  lx := dst[l];
  if FSort(x, lx) < 0 then begin
    exit(0)
  end else
  if FSort(x, lx) = 0 then begin
    i := 1;
    while (FSort(x, dst[i]) = 0) do inc(i);
    exit(i);
  end;
  rx := dst[r];
  cx := dst[c];
  while true do begin
    val := FSort(x, cx);
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
      until FSort(x, cx) <> 0;
      exit(c);
    end;
    c  := l + sarLongint((r - l),1);
    cx := dst[c];
  end;
end;



procedure OFRE_GenericSorter.BINARY_INSERTION_SORT;
begin
   binary_insertion_sort_start(FData, 1, Fsize);
end;

function OFRE_GenericSorter.binary_find(const x: TSortType): nativeint;
begin
   result := binary_insertion_find(FData, x, FSize);
end;

procedure OFRE_GenericSorter.merge_sort;
begin
  _merge_sort(FData,FSize);
end;

procedure OFRE_GenericSorter.heap_sort;
var send  : NativeInt;
    Tswap : TSortType;
begin
  heapify(FData, Fsize);
  send := Fsize - 1;
  while send > 0 do begin
    tSwap       := FData[send];
    FData[send] := FData[0];
    Fdata[0]    := tSwap;
    heap_sift_down(Fdata, 0, send - 1);
    dec(send);
  end;
end;

procedure OFRE_GenericSorter._merge_sort(const dst: PSortType; const size: NativeInt);
var middle,lout,i,j : NativeInt;
    newdst          : PSortType;
begin
  if size < 16 then begin
    binary_insertion_sort_start(dst, 1,  size);
    exit;
  end;
  middle := size div 2;
  _MERGE_SORT(dst, middle);
  _MERGE_SORT(@dst[middle], size - middle);

  Getmem(newdst,size * sizeof(TSortType));
  lout  := 0;
  i     := 0;
  j     := middle;
  while lout <> size do begin
    if i < middle then begin
      if j < size then begin
        if FSort(dst[i], dst[j]) <= 0 then begin
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
  move(newdst[0] , dst^,  size * sizeof(TSortType));
end;

procedure OFRE_GenericSorter.heapify(const dst: PSortType; const size: NativeInt);
var start : NativeInt;
begin
  start := SarLongint(size);
  while start >= 0 do begin
    heap_sift_down(dst, start, size - 1);
    dec(start);
  end;
end;

procedure OFRE_GenericSorter.heap_sift_down(const dst: PSortType; const sstart, send: NativeInt);
var root  : NativeInt;
    child : NativeInt;
    tswap : TSortType;
begin
  root := sstart;
  while ( (root SHL 1) <= send) do begin
    child := root SHL 1;
    if ((child < send) AND (FSort(dst[child], dst[child + 1]) < 0)) then begin
      inc(child);
    end;
    if (FSort(dst[root], dst[child]) < 0) then begin
      tSwap      := dst[root];
      dst[root]  := dst[child];
      dst[child] := tSwap;
      root := child;
    end else begin
      exit;
    end;
  end;
end;



procedure OFRE_GenericSorter.InitData(const sort_function : TSortProc ; const data: PSortType; const size: NativeInt);
var i:integer;
begin
  FSort := sort_function;
  FData := data;
  FSize := size;
end;

function OFRE_GenericSorter.Data: PSortType;
begin
  result := @FData;
end;

function OFRE_GenericSorter.quick_sort_partition(var dst:PSortType; const left, right, pivot: NativeInt): NativeInt;
var value : TSortType;
    swap  : TSortType;
    index : NativeInt;
    i     : NativeInt;
begin
  value := dst[pivot]; dst[pivot] := dst[right]; dst[right] := value;
  index := left;
  i     := left;
  while(i < right) do begin
    if FSORT(dst[i],value) <= 0 then begin
      swap := dst[i]; dst[i] := dst[index]; dst[index] := swap;
      inc(index);
    end;
    inc(i);
  end;
  swap   := dst[right]; dst[right] := dst[index]; dst[index] := swap;
  result := index;
end;

procedure OFRE_GenericSorter.quick_sort_recurse(var dst:PSortType ; const left, right: NativeInt);
var pivot,new_pivot : NativeInt;
begin
  if right <= left then exit;
  if (right - left +1 ) < 16 then begin
    binary_insertion_sort_start(@dst[left],1,right - left + 1);
    exit;
  end;
  pivot     := left + SarLongint((right - left),1);
  new_pivot := quick_sort_partition(dst, left,right,pivot);
  quick_sort_recurse(dst, left,new_pivot-1);
  quick_sort_recurse(dst, new_pivot+1,right);
end;

procedure OFRE_GenericSorter.quick_sort;
var fd:PSortType;
begin
  fd := @FData[0];
  quick_sort_recurse(Fd,0,Fsize-1)
end;




end.

