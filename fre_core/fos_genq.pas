unit FOS_GENQ;

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
{.$DEFINE DEBUG_Q}

interface
uses  Classes, SysUtils;
  type
    TFOS_Q_STATUS=(cs_OK,cs_NOSPACE,cs_ZEROPOP);
    { TFOS_Q }
    generic TFOS_Q<T>=object
    private
      type
        TFOS_Q_Iterator=procedure(const item:T) is nested;
      var
      Q:Array of T;
      fendpos,fstartpos:cardinal;
      fsize,fspace:cardinal;
      fcount:Cardinal;
     {$IFDEF DEBUG_Q }procedure _Dumpstate(const s:string); {$ENDIF}
    public
      procedure Initialize  (const size:cardinal);
      function  Push        (const Item:T):TFOS_Q_STATUS;
      function  Pop         (out Item:T):TFOS_Q_STATUS;
      function  Peek        (out Item:T):TFOS_Q_STATUS;
      function  Count       :Cardinal;
      function  Space       :Cardinal;
      function  QSize       :Cardinal;
      procedure SearchForAllItems(const iterator:TFOS_Q_Iterator);
      procedure  x;
    end;

implementation

{ FOS_Q }
{$IFDEF DEBUG_Q}
procedure TFOS_Q._Dumpstate(const s: string);
var i:integer;
begin
 for i:=0 to length(q)-1 do begin
  write(i,'->(',integer(q[i]),') ');
 end;
 writeln(format('%s> Size(%d) Space(%d) FStartpos(%d) FEndpos(%d) Count (%d)',[s,fsize,fspace,fstartpos,fendpos,fcount]));
end;
{$ENDIF}

procedure TFOS_Q.Initialize(const size: cardinal);
begin
 SetLength (q,size);
 fsize     := size;
 fspace    := size;
 fendpos   := 0;
 fstartpos := 0;
 fcount    := 0;
end;

function TFOS_Q.Push(const Item: T):TFOS_Q_STATUS;
begin
  if fspace=0 then exit(cs_NOSPACE);
  Q[fendpos]:=Item;
  Inc(fendpos);
  if fendpos=fsize then begin
    {$IFDEF DEBUG_Q} writeln('WRAP AROUND');{$ENDIF}
    fendpos:=0;
  end;
  Inc(fcount);
  Dec(fspace);
  result:=cs_OK;
  {$IFDEF DEBUG_Q} _Dumpstate('PUSH'); {$ENDIF}
end;


function TFOS_Q.Pop(out Item:T):TFOS_Q_STATUS;
begin
 result:=Peek(Item);
 if result<>cs_OK then begin
   Fillchar(Item,SizeOf(Item),#0);
   exit;
 end;
 Inc(fspace);
 Dec(fcount);
 Inc(fstartpos);
 if fstartpos=fsize then begin
  fstartpos:=0;
  {$IFDEF DEBUG_Q} WriteLn('WRAPAROUND START POS');{$ENDIF}
 end;
{$IFDEF DEBUG_Q} _Dumpstate('POP'); {$ENDIF}
end;

function TFOS_Q.Peek(out Item: T): TFOS_Q_STATUS;
begin
 if fcount=0 then exit(cs_ZEROPOP);
 result:=cs_OK;
 Item := Q[fstartpos];
end;

function TFOS_Q.Count: Cardinal;
begin
  result:=fcount;
end;

function TFOS_Q.Space: Cardinal;
begin
  result:=fspace;
end;

function TFOS_Q.QSize: Cardinal;
begin
  result:=fsize;
end;

procedure TFOS_Q.SearchForAllItems(const iterator: TFOS_Q_Iterator);
var i       : integer;
    mystart : integer;
begin
 mystart := fstartpos;
 for i:=0 to Count-1 do begin
   iterator(Q[mystart]);
   inc(mystart);
   if mystart=fsize then begin
     writeln(i);
     mystart:=0;
   end;
 end;
end;

procedure TFOS_Q.x;
begin

end;

end.

