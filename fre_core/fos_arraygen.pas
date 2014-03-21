unit fos_arraygen;

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
{$modeswitch nestedprocvars}

interface

uses FOS_GENERIC_SORT,Sysutils;

type

   { OGFOS_Array }

   generic
     OGFOS_Array<_TType> = object
    private
     type
       TGFOS_ArrayType          = array of _TType;
       TGFOS_ArrayBreakIterator = procedure (const element : _TType ; var halt:boolean) is nested;
       TGFOS_ArrayBrkIterator   = function  (const element : _TType):boolean is nested;
       TGFOS_ArrayIterator      = procedure (const element : _TType) is nested;
       TGFOS_SortProc           = function  (const a,b : _TType):NativeInt;
       TGFOS_Sorter             = specialize OFRE_GenericSorter<_TType>;
    var private
       FArray     : TGFOS_ArrayType;
       Flength    : NativeInt;
       function   GetItem(idx: Nativeint): _TType;
       procedure  Setitem(idx: Nativeint; AValue: _TType);
    public
     //procedure   x            ; virtual;abstract;
     procedure   Reserve      (const CapExtension:NativeInt);inline;
     procedure   Init         (const initial_capacity : NativeInt = 0);
     procedure   Add2Array    (const el:_TType;const cap_extension:NativeInt=1);
     function    Add2ArrayChk (const el:_TType;const cap_extension:NativeInt=1):boolean; // check existing
     function    Remove       (const el:_TType):Boolean;
     function    Count        : NativeInt; inline;
     function    Capacity     : NativeInt; inline;
     function    HighArray    : NativeInt;
     function    ForAllBreak  (const iterator: TGFOS_ArrayBreakIterator): boolean;
     function    ForAllBrk    (const iterator:TGFOS_ArrayBrkIterator):boolean;
     procedure   ForAll       (const iterator:TGFOS_ArrayIterator);
     procedure   Sort         (sort_proc : TGFOS_SortProc);
     function    Find         (const el:_TType;sort_proc : TGFOS_SortProc):NativeInt; //-1 smaller then lowest element -2= higher then highest element
     property    Items        [idx:Nativeint]:_TType read GetItem Write Setitem; default;
   end;

   operator=(g1, g2: TGUID) b : boolean;

implementation

operator=(g1, g2: TGUID)b: boolean;
begin
  result := CompareDWord(g1,g2,4)=0;
end;


function OGFOS_Array.GetItem(idx: Nativeint): _TType;
begin
  result := FArray[idx];
end;

procedure OGFOS_Array.Setitem(idx: Nativeint; AValue: _TType);
begin
 FArray[idx] := AValue;
end;


procedure OGFOS_Array.Reserve(const CapExtension: NativeInt);
var len_array : NativeInt;
begin
  //writeln('RESERVE CALL',CapExtension);
  len_array := Length(FArray);
  SetLength(FArray,len_array + CapExtension);
end;

procedure OGFOS_Array.Init(const initial_capacity: NativeInt);
begin
  SetLength  (FArray,initial_capacity);
end;

procedure OGFOS_Array.Add2Array(const el: _TType; const cap_extension: NativeInt);
begin
  if Count=Capacity then begin
    Reserve(cap_extension);
  end;
  Farray[Flength] := el;
  inc(Flength);
  exit;
end;

function OGFOS_Array.Add2ArrayChk(const el: _TType; const cap_extension: NativeInt): boolean;
var  i: NativeInt;
begin
  for i:=0 to HighArray do begin
    if FArray[i]=el then exit(false);
  end;
  Add2Array(el,cap_extension);
  result:=true;
end;


function OGFOS_Array.Remove(const el: _TType): Boolean; // Naive Implementation //TODO: ?FUNCTION
var idx,i   : NativeInt;
begin
  if Length(FArray)>0 then begin
    idx:=0;
    for i:=0 to High(FArray) do begin
      if FArray[i]=el then begin
        FArray[i] := FArray[HighArray];
        FArray[HighArray] := nil;
        dec(Flength);
        exit(true);
      end;
    end;
  end;
  result := false;
end;

function OGFOS_Array.Count: NativeInt;
begin
  result := Flength;
end;

function OGFOS_Array.Capacity: NativeInt;
begin
  result := Length(Farray);
end;

function OGFOS_Array.HighArray: NativeInt;
begin
  result := Flength-1;
end;

function OGFOS_Array.ForAllBreak(const iterator: TGFOS_ArrayBreakIterator): boolean;
var i    : NativeInt;
    halt : boolean;
begin
  halt := false;
  for i:=0 to HighArray do
    begin
       iterator(Farray[i],halt);
       if halt then
         exit(true);
    end;
  result := false;
end;

function OGFOS_Array.ForAllBrk(const iterator: TGFOS_ArrayBrkIterator):boolean;
var i:NativeInt;
begin
  result:=false;
  for i:=0 to HighArray do begin
    if iterator(Farray[i]) then exit(true);
  end;
end;

procedure OGFOS_Array.ForAll(const iterator: TGFOS_ArrayIterator);
var i: NativeInt;
begin
  for i:=0 to HighArray do begin
    iterator(Farray[i]);
  end;
end;

procedure OGFOS_Array.Sort(sort_proc: TGFOS_SortProc);
var GenSort : TGFOS_Sorter;
begin
  GenSort.InitData(sort_proc,@FArray[0],Flength);
  GenSort.quick_sort;
end;

function OGFOS_Array.Find(const el: _TType; sort_proc: TGFOS_SortProc): NativeInt;
var GenSort : TGFOS_Sorter;
begin
  GenSort.InitData(sort_proc,@FArray[0],Flength+1);
  result := GenSort.binary_find(el)-1;
  //writeln('HERE:',result,' ',Flength);
  if result=HighArray then begin
    //writeln('HERE ---');
    if FArray[result]<>el then result := -2;
  end;
end;



end.

