unit fos_arraysetgen;

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
   { TGFOS_Arrayset }
   generic
     TGFOS_Arrayset<_TType> = class(TInterfacedObject)
    public
     type
       TGFOS_ArraysetType = array of _TType;
       TForAll            = procedure(var el:_TType) of object;
       TForAllData        = procedure(var el:_TType;const Data:Pointer) of object;
    var private
       Farray          : TGFOS_ArraysetType;
       FUnassignedType : _TType;
       Fleaks          : integer;
    public
     constructor Create     (const Unassigned:_TType);
     procedure   Add        (const el:_TType);
     procedure   Remove     (const el:_TType);
     function    IsIn       (const el:_TType): Boolean;
     function    AsArray    :TGFOS_ArraysetType;
     procedure   ForAll     (const proc:TForAll);
     procedure   ForAllData (const proc:TForAllData;const Data:Pointer);
   end;

   generic
     TGFOS_Array<_TType> = object
    public
     type
       TGFOS_ArrayType   = array of _TType;
    var private
       Farray:TGFOS_ArrayType;
    public
     procedure   Add        (const el:_TType);
     function    Length     : integer;
     function    High       : integer;
     //function    AsArray    :TGFOS_ArraysetType;
     //procedure   ForAll     (const proc:TForAll);
     //procedure   ForAllData (const proc:TForAllData;const Data:Pointer);
   end;

implementation

procedure TGFOS_Array.Add(const el: _TType);
begin
  SetLength(Farray,Length+1);
  Farray[high] := el;
end;


function TGFOS_Array.Length: integer;
begin
  result := System.Length(Farray);
end;

function TGFOS_Array.High: integer;
begin
  result := System.High(Farray);
end;

constructor TGFOS_Arrayset.Create(const Unassigned: _TType);
begin
  FUnassignedType:=Unassigned;
  FLeaks:=0;
end;

{ TGFOS_Arrayset }

procedure TGFOS_Arrayset.Add(const el: _TType);
var l,i:integer;
begin
  if el=FUnassignedType then raise Exception.Create('you cannot add no element');
  l:=Length(Farray);
  for i:=0 to l-1 do begin
    if Farray[i]=el then exit; // Already in set
  end;
  for i:=0 to l-1 do begin // reuse slot
   if Farray[i]=FUnassignedType then begin
     Farray[i]:=el;
     dec(Fleaks);
     exit;
   end;
  end;
  SetLength(Farray,l+1);
  Farray[l]:=el;
end;

procedure TGFOS_Arrayset.Remove(const el: _TType);
var i: Integer;
begin
  for i:=0 to length(Farray)-1 do begin
    if Farray[i]=el then begin
      Farray[i]:=FUnassignedType;
      inc(fleaks);
    end;
  end;
end;

function TGFOS_Arrayset.IsIn(const el: _TType): Boolean;
var i: Integer;
begin
  Result:=false;
  for i:=0 to length(Farray)-1 do begin
    if Farray[i]=el then begin
      Result:=true;
      exit;
    end;
  end;
end;

function TGFOS_Arrayset.AsArray: TGFOS_ArraysetType;
var i,j:integer;
begin
  SetLength(Result,Length(Farray)-Fleaks);
  j:=0;
  for i:=0 to Length(Farray)-1 do begin
    if Farray[i]<>FUnassignedType then begin
      result[j]:=Farray[i];
      inc(j);
    end;
  end;
end;

procedure TGFOS_Arrayset.ForAll(const proc: TForAll);
var i: Integer;
begin
  for i:=0 to length(Farray)-1 do begin
    if Farray[i]<>FUnassignedType then proc(Farray[i]);
  end;
end;

procedure TGFOS_Arrayset.ForAllData(const proc: TForAllData; const Data: Pointer);
var i: Integer;
begin
  for i:=0 to length(Farray)-1 do begin
    if Farray[i]<>FUnassignedType then proc(Farray[i],Data);
  end;
end;


end.

