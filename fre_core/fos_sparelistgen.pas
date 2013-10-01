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
         TGFOS_ElemProc               = procedure (var x:_TType;const idx:NativeInt ; var halt_flag:boolean) is nested;
         TGFOS_ExtCompareElemProc     = function  (const x1,x2 : _PTType) : boolean; // is nested; //of object;
         TGFOS_ExtCompareNullElemProc = function  (const x1:_PTType) : boolean; //  is nested; //     ; of object;
     private
       FArray        : TGFOS_SpareListType;
       FCnt          : NativeInt;
       FCurrSpares   : NativeInt;
       FReservSpares : NativeInt;
       FNullElement  : _TType;
       FCompareFunc  : TGFOS_ExtCompareElemProc;
       FNullCompare  : TGFOS_ExtCompareNullElemProc;
     public
       function  Reserve        : NativeInt;
       procedure InitSparseList (const NullElement:_TType ; const NullCompare : TGFOS_ExtCompareNullElemProc ; const Compare : TGFOS_ExtCompareElemProc ;  const numberofemptyslots:NativeUint=25);
       function  Add            (const elem: _TType):boolean;
       function  Exists         (const elem: _TType): NativeInt;
       function  Delete         (const elem: _TType):Boolean;
       function  GetElement     (idx : NativeInt): _TType;
       procedure SetElement     (idx : NativeInt; AValue: _TType);
       property  Element        [idx : NativeInt]:_TType read GetElement write SetElement; default;
       function  Count          : NativeInt;
       function  ForAllBreak    (const elem_func : TGFOS_ElemProc):Boolean;
       procedure ClearIndex     (const idx : NativeInt);
       //procedure x; virtual; abstract;
  end;

  OFOS_SL_TObject  = specialize OFOS_SpareList<TObject>;

  //OFOS_SL_TGuid    = specialize OFOS_SpareList<TGuid>;

implementation

function OFOS_SL_EmptyTestObj(const obj: TObject): boolean;
begin
  result := obj=nil;
end;

{ OFOS_SpareList }


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


function OFOS_SpareList.Add(const elem: _TType): boolean;
var firstspare : NativeInt;
    i          : NAtiveInt;
begin
  if Exists(elem)<>-1 then
    exit(false);
  firstspare:=-1;
  result := true;
  for i:=0 to high(FArray) do
    begin
      if FNullCompare(@FArray[i]) then
        begin
          firstspare := i;
          break;
        end;
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
  dec(FCurrSpares);
  inc(FCnt);
end;

function OFOS_SpareList.Exists(const elem: _TType): NativeInt;
var i          : NativeInt;
    isnull     : boolean;
    cmpcnt     : NativeInt;
begin
  result     := -1;
  cmpcnt     :=  0;
  if FCnt = 0 then
    exit(-1);
  for i:=0 to high(FArray) do
    begin
      if (not FNullCompare(@FArray[i])) then
        begin
          if FCompareFunc(@FArray[i],@elem) then
            exit(i);
          inc(cmpcnt);
          if cmpcnt=FCnt then
            exit(-1);
        end;
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
end;

function OFOS_SpareList.GetElement(idx: NativeInt): _TType;
begin
  result := FArray[idx];
end;

procedure OFOS_SpareList.SetElement(idx: NativeInt; AValue: _TType);
begin
  if FNullCompare(@FArray[idx]) then
    begin
      FArray[idx] := AValue; // Add
      dec(FCurrSpares);
      inc(FCnt);
    end
  else
    begin
      FArray[idx] := AValue; // Overwrite
    end;
end;

function OFOS_SpareList.Count: NativeInt;
begin
  result := FCnt;
end;

function OFOS_SpareList.ForAllBreak(const elem_func: TGFOS_ElemProc): Boolean;
var i       : NativeInt;
    haltf   : boolean;
    cnt,
    savecnt : NativeInt;
begin
  result  := true;
  haltf   := false;
  cnt     := 0;
  savecnt := FCnt;
  for i := 0 to High(FArray) do
    begin
      if not FNullCompare(@FArray[i]) then
        begin
          elem_func(FArray[i],i,haltf);
          inc(cnt);
          if cnt=savecnt then
             exit(haltf);
          if haltf then
             exit(true);
        end;
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


end.

