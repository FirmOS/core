unit fos_listarray;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FOS_INTERLOCKED;

const c_array_length = 100000;

type

  { TFOS_LISTARRAY }

  TListIndex = integer;

  generic TFOS_LISTARRAY <_TStore> = class
   private type
     _LR=record
       data            : _TStore;
       used            : longword;
       next            : TListIndex;
       prev            : TListIndex;
     end;

   var private
     arr               : Array[0..c_array_length] of _LR;
     last_store        : TListindex;
     _count            : integer;

  public

   constructor Create;
   function    Store   (const data  : _TStore   ; const prev:TListIndex): TListIndex;
   function    Get     (const index : TListIndex; out data :_TStore; out prev:TListindex;const clear:boolean)   : TListIndex;
   function    Count   : integer;
  end;

  TFOS_LISTIntegerArray = specialize TFOS_LISTARRAY <integer>;

implementation

{ TFOS_LISTARRAY }

constructor TFOS_LISTARRAY.Create;
var i:TListIndex;

begin
 for i:=low(arr) to high(arr) do begin
  arr[i].next:=-1;
  arr[i].prev:=-1;
  arr[i].used:=0;
 end;
 last_store:=-1;
 _count:=0;
end;

function TFOS_LISTARRAY.Store(const data: _TStore; const prev: TListIndex): TListIndex;
var i         :TListIndex;
    old_store :TListIndex;

begin
 result:=-1;
 if _count=length(arr) then begin
  exit;   // array full, exit with -1
 end;
 i:=last_store;
 old_store:=i;
 repeat
  inc(i);
//  writeln(i);
  if i>=c_array_length then begin
   i:=0;
  end;
  if arr[i].used=0 then begin
   arr[i].used:=1;
   arr[i].data:=data;
   arr[i].next:=-1;
   if prev<>-1 then begin
    if arr[prev].used=0 then begin
     raise Exception.Create('LISTARRAY STORE FAILED, PREVIOUS RECORD IS NOT USED !'+inttostr(prev));
    end;
    arr[prev].next:=i;
   end;
   arr[i].prev:=prev;
   result:=i;
   last_store:=i;
   inc(_count);
   break;
  end;
 until i=old_store;
 // result -1 means no free space
end;

function TFOS_LISTARRAY.Get(const index: TListIndex; out data: _TStore; out prev: TListindex;const clear: boolean ): TListIndex;
begin
 if arr[index].used=0 then begin
  raise Exception.Create('LISTARRAY GET FAILED, RECORD IS NOT USED !'+inttostr(index));
 end;
 data   := arr[index].data;
 prev   := arr[index].prev;
 result := arr[index].next;
 if clear then begin
  if prev<>-1 then begin
   if arr[prev].used=0 then begin
    raise Exception.Create('LISTARRAY STORE FAILED, PREVIOUS RECORD IS NOT USED !'+inttostr(prev));
   end;
   arr[prev].next:=arr[index].next;
  end;
  if result<>-1 then begin
   if arr[result].used=0 then begin
    raise Exception.Create('LISTARRAY STORE FAILED, NEXT RECORD IS NOT USED !'+inttostr(result));
   end;
   arr[result].prev:=prev;
  end;
  arr[index].used:=0;
  arr[index].next:=-1;
  arr[index].prev:=-1;
  dec(_count);
 end;
end;


function TFOS_LISTARRAY.Count: integer;
begin
 result:=_count;
end;

end.

