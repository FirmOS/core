unit fos_sparelistgen;

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

uses sysutils;

type
  { OFOS_SpareList }
  generic
     OFOS_SpareList<_TType> = object
     private type
         _PTType                      = ^_TType;
         TGFOS_SpareListType          = array of _TType;
         TGFOS_ElemProcHalt           = procedure (var x:_TType;const idx:NativeInt ; var halt_flag:boolean) is nested; { complexer cases }
         TGFOS_ElemProc               = procedure (var x:_TType) is nested; { simple short form }
         TGFOS_ExtCompareElemProc     = function  (const x1,x2 : _PTType) : boolean;
         TGFOS_ExtCompareNullElemProc = function  (const x1:_PTType) : boolean;
     private
       FArray        : TGFOS_SpareListType;
       FCnt          : NativeInt;
       FCurrSpares   : NativeInt;
       FReservSpares : NativeInt;
       FNullElement  : _TType;
       FCompareFunc  : TGFOS_ExtCompareElemProc;
       FNullCompare  : TGFOS_ExtCompareNullElemProc;
       function  MyNullCompare (const x1:_PTType) : boolean;
       function  MyExtCompare  (const x1,x2 : _PTType) : boolean;
     public
       function  Reserve              : NativeInt;
       procedure InitSparseList       (const NullElement:_TType ; const NullCompare : TGFOS_ExtCompareNullElemProc ; const Compare : TGFOS_ExtCompareElemProc ;  const numberofemptyslots:NativeUint=25);
       procedure InitSparseListPtrCmp (const numberofemptyslots:NativeUint=25);
       function  Add                  (const elem: _TType):NativeInt; // new result = insert index or -1 on exists
       function  Exists               (const elem: _TType):NativeInt;
       function  Delete               (const elem: _TType):Boolean;
       function  GetElement           (idx : NativeInt): _TType;
       procedure SetElement           (idx : NativeInt; AValue: _TType);
       function  Count                : NativeInt;
       procedure ForAll               (const elem_func : TGFOS_ElemProc);
       function  ForAllBreak          (const elem_func : TGFOS_ElemProcHalt):Boolean;
       function  ForAllBreak2         (const elem_func : TGFOS_ElemProcHalt ; var halt : boolean):boolean;
       procedure ClearIndex           (const idx : NativeInt);
       function  GetLastNotNull       (var elem : _TType):NativeInt;
       function  GetFirstNotNull      (var elem : _TType):NativeInt;
       property  Element              [idx : NativeInt]:_TType read GetElement write SetElement; default;
       //procedure _DummyForceFPC_Recompile ; virtual ; abstract;
  end;


implementation

{ OFOS_SpareList }

function OFOS_SpareList.MyNullCompare(const x1: _PTType): boolean;
begin
  if FNullCompare=nil then
    begin
      result := CompareMem(x1,@FNullElement,sizeof(_TType));
    end
  else
    begin
      result := FNullCompare(x1);
    end;
end;

function OFOS_SpareList.MyExtCompare(const x1, x2: _PTType): boolean;
begin
  if FCompareFunc=nil then
    result := CompareMem(x1,x2,sizeof(_TType))
  else
    result := FCompareFunc(x1,x2);
end;

function OFOS_SpareList.Reserve: NativeInt;
var diff : NativeInt;
begin
  result := high(FArray)+1;
  diff   := FReservSpares-FCurrSpares;
  assert(diff>0);
  SetLength(FArray,Length(FArray)+diff);
end;

procedure OFOS_SpareList.InitSparseList(const NullElement: _TType; const NullCompare: TGFOS_ExtCompareNullElemProc; const Compare: TGFOS_ExtCompareElemProc; const numberofemptyslots: NativeUint);
var i : NAtiveInt;
begin
  FCnt          := 0;
  SetLength     (FArray,numberofemptyslots);
  FCurrSpares   := numberofemptyslots;
  FReservSpares := numberofemptyslots;
  FNullElement  := NullElement;
  FNullCompare  := NullCompare;
  FCompareFunc  := Compare;
  for i := 0 to High(FArray) do
    FArray[i] := NullElement;
end;

procedure OFOS_SpareList.InitSparseListPtrCmp(const numberofemptyslots: NativeUint);
var nullelem : _TType;
begin
  FillByte(nullelem,sizeof(FNullElement),0);
  InitSparseList(nullelem,nil,nil);
end;



function OFOS_SpareList.Add(const elem: _TType): NativeInt;
var firstspare : NativeInt;
    i          : NAtiveInt;
begin
  if Exists(elem)<>-1 then
    exit(-1);
  firstspare:=-1;
  for i:=0 to high(FArray) do
    if MyNullCompare(@FArray[i]) then
      begin
        firstspare := i;
        break;
      end;
  if firstspare>=0 then
    begin
      FArray[firstspare] := elem;
    end
  else
    begin
      firstspare := Reserve;
      FArray[firstspare] := elem;
    end;
  result := firstspare;
  dec(FCurrSpares);
  inc(FCnt);
end;

function OFOS_SpareList.Exists(const elem: _TType): NativeInt;
var i          : NativeInt;
    isnull     : boolean;
    cmpcnt     : NativeInt;
begin
  result  := -1;
  cmpcnt  :=  0;
  if FCnt = 0 then
    exit(-1);
  for i:=0 to high(FArray) do
    if (not MyNullCompare(@FArray[i])) then
      begin
        if MyExtCompare(@FArray[i],@elem) then
          exit(i);
        inc(cmpcnt);
        if cmpcnt=FCnt then
          exit(-1);
      end;
  exit(-1);
end;


function OFOS_SpareList.Delete(const elem: _TType): Boolean;
var idx : NativeInt;
begin
  idx := Exists(elem);
  if idx<0 then
    exit(false);
  FArray[idx] := FNullElement;
  inc(FCurrSpares);
  dec(FCnt);
  result := true;
end;

function OFOS_SpareList.GetElement(idx: NativeInt): _TType;
begin
  result := FArray[idx];
end;

procedure OFOS_SpareList.SetElement(idx: NativeInt; AValue: _TType);
begin
  if MyNullCompare(@FArray[idx]) then
    begin
      FArray[idx] := AValue; // Add
      dec(FCurrSpares);
      inc(FCnt);
    end
  else
    begin
      FArray[idx] := AValue; // Overwrite
    end
end;

function OFOS_SpareList.Count: NativeInt;
begin
  result := FCnt;
end;

procedure OFOS_SpareList.ForAll(const elem_func: TGFOS_ElemProc);
var i       : NativeInt;
    cnt,
    maxarr  : NativeInt;
begin
  cnt     := FCnt;
  maxarr  := Length(FArray);
  i       := 0;
  while (i <  maxarr) and (cnt>0)  do
    begin
      if not MyNullCompare(@FArray[i]) then
        begin
          elem_func(FArray[i]);
          dec(cnt);            { decrement saved max item cnt for every non null element}
        end;
      inc(i);
    end;
end;

function OFOS_SpareList.ForAllBreak(const elem_func: TGFOS_ElemProcHalt): Boolean;
var   haltf : boolean;
begin
  haltf := false;
  result := ForAllBreak2(elem_func,haltf);
end;

function OFOS_SpareList.ForAllBreak2(const elem_func: TGFOS_ElemProcHalt; var halt: boolean): boolean;
var cnt,savecnt,
    maxarr,i    : NativeInt;
begin
  result  := true;
  cnt     := 0;
  maxarr  := Length(FArray);
  i       := 0;
  savecnt := FCnt; { a clearindex (which is allowed) would modify the count -> save it for local usage }
  while (i < maxarr) and (savecnt>0) do
    begin
      if not MyNullCompare(@FArray[i]) then
        begin
          elem_func(FArray[i],i,halt);
          inc(cnt);
          if cnt=savecnt then { search the whole array, but break if we have reached the element count, there cant be further non null elements}
             exit(halt);
          if halt then
             exit(true);
        end;
      inc(i);
    end;
    exit(false);
end;

procedure OFOS_SpareList.ClearIndex(const idx: NativeInt);
begin
  if (idx>=0) and (idx<=high(FArray)) then
    begin
      FArray[idx] := FNullElement;
      Dec(FCnt);
      Inc(FCurrSpares);
    end
  else
    raise Exception.create('SPARELIST - CLEARINDEX OUT OF BOUNDS');
end;

function OFOS_SpareList.GetLastNotNull(var elem: _TType): NativeInt;
var i          : NativeInt;
    isnull     : boolean;
begin
  result  := -1;
  elem    :=  FNullElement;
  if FCnt = 0 then
    exit(-1);
  for i:= high(FArray) downto 0 do
    if (not MyNullCompare(@FArray[i])) then
      begin
        elem := FArray[i];
        exit(i);
      end;
  exit(-1);
end;

function OFOS_SpareList.GetFirstNotNull(var elem: _TType): NativeInt;
var i          : NativeInt;
    isnull     : boolean;
begin
  result  := -1;
  elem    := FNullElement;
  if FCnt = 0 then
    exit(-1);
  for i:= 0 to high(FArray) do
    if (not MyNullCompare(@FArray[i])) then
      begin
        elem := FArray[i];
        exit(i);
      end;
  exit(-1);
end;


end.

