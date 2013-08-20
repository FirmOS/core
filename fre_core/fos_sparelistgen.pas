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
         TGFOS_SpareListType      = array of _TType;
         TGFOS_ElemProc           = procedure (var x:_TType;const idx:NativeInt ; var halt_flag:boolean) is nested;
         TGFOS_ExtCompareElemProc = function  (const x1,x2 : _TType) : boolean is nested;
     private
       FArray        : TGFOS_SpareListType;
       FCnt          : NativeInt;
       FCurrSpares   : NativeInt;
       FReservSpares : NativeInt;
     public
       function  Reserve : NativeInt;
       procedure Init          (const numberofemptyslots:NativeUint=25);
       procedure InitFromArray (const densearray : TGFOS_SpareListType ; const numberofemptyslots:NativeUint=25);
       function  Add           (const elem: _TType):boolean;
       function  Exists        (const elem: _TType ; out firstspare : NativeInt): NativeInt;
       function  Exists        (const elem: _TType): NativeInt;
       function  Exists        (const elem: _TType ; const external_compare : TGFOS_ExtCompareElemProc): NativeInt;
       function  Delete        (const elem: _TType):Boolean;
       function  Count         : NativeInt;
       function  ForAllBreak   (const elem_func : TGFOS_ElemProc):Boolean;
       procedure ClearIndex    (const idx : NativeInt);
       procedure x; virtual; abstract;
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

procedure OFOS_SpareList.Init(const numberofemptyslots: NativeUint);
begin
  FCnt          := 0;
  SetLength(FArray,numberofemptyslots);
  FCurrSpares   := numberofemptyslots;
  FReservSpares := numberofemptyslots;
end;

procedure OFOS_SpareList.InitFromArray(const densearray: TGFOS_SpareListType; const numberofemptyslots: NativeUint);
begin
  FArray        := copy(densearray);
  FCurrSpares   := 0;
  FCnt          := Length(FArray);
  FReservSpares := numberofemptyslots;
  Reserve;
end;

function OFOS_SpareList.Add(const elem: _TType): boolean;
var firstspare : NativeInt;
begin
  if Exists(elem,firstspare)<>-1 then
    exit(false);
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

function OFOS_SpareList.Exists(const elem: _TType ; out firstspare : NativeInt): NativeInt;
var i          : NativeInt;
begin
  firstspare := -1;
  result     := -1;
  for i:=0 to high(FArray) do
    begin
      if (firstspare=-1)
         and (FArray[i]=nil) then
           firstspare:=i;
      if FArray[i]=elem then
           exit(i);
    end;
  exit(-1);
end;

function OFOS_SpareList.Exists(const elem: _TType): NativeInt;
var dummy : NativeInt;
begin
  result := Exists(elem,dummy);
end;

function OFOS_SpareList.Exists(const elem: _TType; const external_compare: TGFOS_ExtCompareElemProc): NativeInt;
var i          : NativeInt;
begin
  result     := -1;
  for i:=0 to high(FArray) do
    if assigned(FArray[i]) and
       external_compare(FArray[i],elem) then
        exit(i);
  exit(-1);
end;

function OFOS_SpareList.Delete(const elem: _TType): Boolean;
var idx : NativeInt;
begin
  idx := Exists(elem);
  if idx<0 then
    exit(false);
  FArray[idx] := nil;
  inc(FCurrSpares);
  dec(FCnt);
end;

function OFOS_SpareList.Count: NativeInt;
begin
  result := FCnt;
end;

function OFOS_SpareList.ForAllBreak(const elem_func: TGFOS_ElemProc): Boolean;
var i      : NativeInt;
    haltf  : boolean;
begin
  result := true;
  haltf  := false;
  for i := 0 to High(FArray) do
    begin
      if FArray[i]<>nil then
        begin
          elem_func(FArray[i],i,haltf);
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
      FArray[idx] := nil;
      Dec(FCnt);
      Inc(FCurrSpares);
    end
  else
    raise Exception.create('SPARELIST - CLEARINDEX OUT OF BOUNDS');
end;


end.

