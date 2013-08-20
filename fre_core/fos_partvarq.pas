unit fos_partvarq;

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

{$DEFINE DEBUG_CQ}

{$mode objfpc}{$H+}

interface

uses
  SysUtils,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,Classes;

  {TFOS_PARTVARQ }

  // Single Producer / Multi ConsumerQ / Consumers Pop in their partition, work Inplace and have to FinishPop then ...
 const MaxThreads = 24;
 type
  FOS_QArray=Array of Byte;

  TFOS_COMPACT_STATUS=(cs_OK,cs_NOSPACE,cs_ZEROPUSH,cs_ZEROPOP,cs_SIZE_EXCEEDED,cs_AGAIN);

  TFOS_PARTVARQ=class
  private
    FConsumerCount : integer;
    fdata          : Array [0..MaxThreads-1] of  FOS_QArray;
    pushroller     : integer;
    chkval         : integer;
    ffree_space    : Array [0..MaxThreads-1] of integer;
    fpushed_count  : Array [0..MaxThreads-1] of integer;
    fflast_free_pos: Array [0..MaxThreads-1] of Pointer;
    fopen_popo_cnt : Array [0..MaxThreads-1] of integer;
    fendpos,
    fstartpos      : Array [0..MaxThreads-1] of integer;
    fpartition_size     : integer; // Size is constant and Equal
    mul_pd_sz      : integer;
    mul_pdsz_b     : integer;
    mul_sz         : integer;
    push_marker    : Longword;
    pop_marker     : Longword;
    function   _Padlen(const len:integer):integer;
    function   _FindPushQ(const padded_sz:integer):Boolean;
  public
    procedure   _Dumpstate(const s:string;const full:boolean=true);
    constructor Create (part_size:integer;const consumer_count:integer=1);
    destructor  Destroy; override;

    function    Push        (const Data:Pointer;const Size:integer)            :TFOS_COMPACT_STATUS;
    function    StartPush   (const WholeSize:integer)                          :TFOS_COMPACT_STATUS; // Start a Multiple to same Q Push
    procedure   MultiPush   (const Data:Pointer;const Size:integer);           // Continue
    procedure   EndPush     ; // Finish Multpush

    function    PopAsString (const   qkey:integer;var   s:string)             :TFOS_COMPACT_STATUS;
    function    Pop         (const   qkey:integer; out Data1,Data2 : Pointer ; out Size1,Size2:integer):TFOS_COMPACT_STATUS;
    procedure   MarkFree    (const   qkey:integer; const Data1:pointer); // I am done with that chunk of data // free largest conected block
    function    Count       (const   qkey:integer):integer;
    function    Space       :integer;
    function    QSize       :integer;
  end;

implementation

{ TFOS_PARTVARQ }
//{$IFDEF DEBUG_CQ}
procedure dump_binary(p:pointer;const len:integer);
var i:integer;
    b:char;
begin
 for i:=1 to len do begin
  B:=PChar(p)^;
  if not (b in [' ','0'..'9','A'..'Z','a'..'z']) then b:=' ';
  write(format('%2.2x(%1.1s) ',[byte(pbyte(p)^),string(char(b))]));
  if i mod 16=0 then writeln;
  inc(p);
 end;
 writeln;
end;
//{$ENDIF}

//{$IFDEF DEBUG_CQ}
procedure TFOS_PARTVARQ._Dumpstate(const s:string;const full:boolean=true);
var i:integer;
begin
  exit;
  for i:=0 to FConsumerCount-1 do begin
  writeln('DUMP <',s,'>');
  writeln(format('Q[%d] > Size(%d) Space(%d) FStartpos(%d) FEndpos(%d) Count (%d)',[i,fpartition_size,ffree_space[i],fstartpos[i],fendpos[i],fpushed_count[i]]));
  if full then dump_binary(@fdata[i][0],fpartition_size);
 end;
end;

constructor TFOS_PARTVARQ.Create(part_size: integer;const consumer_count: integer);
var i:integer;
begin
 inherited Create;

 if part_size<16 then part_size:=16;
 fpartition_size := _Padlen(part_size);
 FConsumerCount:=consumer_count;
 for i:=0 to FConsumerCount-1 do begin
   SetLength(fdata[i],part_size);
   ffree_space[i]    := part_size;
   fendpos[i]        := 0;
   fstartpos[i]      := 0;
   fpushed_count[i]  := 0;
   fopen_popo_cnt[i] := 0;
   fflast_free_pos[i]:= @fdata[i][0];
 end;
 chkval:=0;
 pushroller    := -1;
 push_marker   := $DEADBEEF;
 pop_marker    := $C5C5C5C5;
end;

destructor TFOS_PARTVARQ.Destroy;
var i:integer;
begin
  for i:= 0 to FConsumerCount-1 do begin;
    SetLength (fdata[i],0);
  end;
  inherited Destroy;
end;

//{$ENDIF}

function TFOS_PARTVARQ._Padlen(const len: integer): integer;
//var pad_len:integer;
begin
  //pad_len:=len mod sizeof(integer);
  //if pad_len>0 then begin
  // result:=len+(sizeof(integer)-pad_len);
  //end else begin
  // result:=len;
  //end;
  //TODO SPeedcheck
  result  := (len-1) + (4-((len-1) mod 4));
  //if result<>pad_len2 then GFRE_BT.CriticalAbort('haha looser %d  --- %d --- %d',[len,result,pad_len2]);
end;

function TFOS_PARTVARQ._FindPushQ(const padded_sz:integer): Boolean;
var cnt:integer;
    fnd:boolean;
begin
  fnd:=false;
  cnt:=0;
  repeat
   inc(pushroller);if pushroller>=FConsumerCount then pushroller:=0;
   if ffree_space[pushroller]-padded_sz-(2*sizeof(integer))>=0 then begin // SIZE / FINISHMARKER / DATA
     fnd:=true;
     break;
   end;
   inc(cnt);
  until (fnd or (cnt=FConsumerCount));
//  writeln('FINWPUSH Q ',padded_sz,' ',ffree_space[pushroller]-padded_sz-(2*sizeof(integer)),' ',fnd);
  result:=fnd;
end;




function TFOS_PARTVARQ.Push(const Data: Pointer; const Size: integer):TFOS_COMPACT_STATUS;
var padded_sz,sz1,sz2:integer;
begin
  if size=0 then exit(cs_ZEROPUSH);
  padded_sz:=_Padlen(size);

  if not _FindPushQ(padded_sz) then exit(cs_NOSPACE);
  Move(Size,fdata[pushroller][fendpos[pushroller]],sizeof(integer));Inc(fendpos[pushroller],sizeof(integer));
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;
  Move(push_marker,fdata[pushroller][fendpos[pushroller]],sizeof(integer));Inc(fendpos[pushroller],sizeof(integer));
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;

  if fendpos[pushroller]+padded_sz<=fpartition_size then begin
    Move(Data^,fdata[pushroller][fendpos[pushroller]],size);
    Inc(fendpos[pushroller],padded_sz);
  end else begin
    sz1:=fpartition_size-fendpos[pushroller];
    sz2:=padded_sz-sz1;
    Move(Pointer(Data)^,fdata[pushroller][fendpos[pushroller]],sz1);
    Move(Pointer(Data+sz1)^,fdata[pushroller][0],sz2);
    FEndpos[pushroller]:=sz2;
  end;
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;
  InterLockedExchangeAdd(ffree_space[pushroller],-1*integer((padded_sz+(2*sizeof(integer)))));
  InterLockedIncrement(fpushed_count[pushroller]);
  result:=cs_OK;
  {$IFDEF DEBUG_CQ} _Dumpstate('PUSH'); {$ENDIF}
end;

function TFOS_PARTVARQ.StartPush(const WholeSize: integer): TFOS_COMPACT_STATUS;
var padded_sz:integer;
begin
  if WholeSize=0 then exit(cs_ZEROPUSH);
  padded_sz:=_Padlen(WholeSize);
  if not _FindPushQ(padded_sz) then begin
    mul_sz     := -1;
    exit(cs_NOSPACE);
  end;
  mul_pd_sz  := padded_sz;
  mul_pdsz_b := padded_sz;
  mul_sz     := WholeSize;

//  Move(PtrUInt(WholeSize),fdata[pushroller][fendpos[pushroller]],sizeof(integer));
  Move(WholeSize,fdata[pushroller][fendpos[pushroller]],sizeof(integer));
  Inc(fendpos[pushroller],sizeof(integer));
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;
  Move(push_marker,fdata[pushroller][fendpos[pushroller]],sizeof(integer));
  Inc(fendpos[pushroller],sizeof(integer));
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;
  exit(cs_OK);
end;

procedure TFOS_PARTVARQ.MultiPush(const Data: Pointer; const Size: integer);
var sz1,sz2:integer;
begin
  {$IFDEF DEBUG_CQ}
    if (mul_sz=-1) then GFRE_BT.CriticalAbort(' eierbär ');  // dont multipush without a valid start
  {$ENDIF}
  mul_sz    := mul_sz-Size;
  mul_pd_sz := mul_pd_sz-Size;
  if fendpos[pushroller]+Size<=fpartition_size then begin
    Move(Pointer(Data)^,fdata[pushroller][fendpos[pushroller]],size);
    Inc(fendpos[pushroller],size);
  end else begin
    writeln('MOVE ON MOVE ON WRAP');
    sz1:=fpartition_size-fendpos[pushroller];
    sz2:=Size-sz1;
    Move(Pointer(Data)^,fdata[pushroller][fendpos[pushroller]],sz1);
    Move(Pointer(Data+sz1)^,fdata[pushroller][0],sz2);
    FEndpos[pushroller]:=sz2;
  end;
  if mul_sz=0 then begin // final push ...
    inc(fendpos[pushroller],mul_pd_sz); // Rest size;
  end;
  if fendpos[pushroller]=fpartition_size then fendpos[pushroller]:=0;
end;

procedure TFOS_PARTVARQ.EndPush;
begin
  InterLockedExchangeAdd(ffree_space[pushroller],-1*integer((mul_pdsz_b+(2*sizeof(integer)))));
  InterLockedIncrement(fpushed_count[pushroller]);
  {$IFDEF DEBUG_CQ} _Dumpstate('PUSH'); {$ENDIF}
end;



function TFOS_PARTVARQ.PopAsString(const qkey: integer; var s: string): TFOS_COMPACT_STATUS;
var l1,l2:integer;
    p1,p2:Pointer;
begin
  result:=Pop(qkey,p1,p2,l1,l2);
 if result<>cs_OK then begin
  exit;
 end;
 try
  SetLength(s,l1+l2);
  Move(p1^,s[1],l1);
  if l2>0 then Move(p2^,s[l1+1],l2);
 except on e:exception do begin
   // _Dumpstate(e.Message+' MOVE ERROR');
   GFRE_BT.CriticalAbort('FCUK');
 end;end;
 try
   MarkFree(qkey,p1);
 except on e:exception do begin
  //  _Dumpstate(e.Message+' POP ERROR');
   GFRE_BT.CriticalAbort('FCUK');
 end;end;
end;


function TFOS_PARTVARQ.Pop(const qkey:integer;out Data1, Data2: Pointer; out Size1, Size2: integer): TFOS_COMPACT_STATUS;
var   size:integer;
    pad_sz:integer;
begin
 {$IFDEF DEBUG_CQ}
   if (qkey<0) or (qkey>=FConsumerCount) then GFRE_BT.CriticalAbort('your qkey is for the bucket ...');
 {$ENDIF}
 if fpushed_count[qkey]=0 then exit(cs_ZEROPOP);
 InterLockedDecrement(fpushed_count[qkey]);

 size:=PLongWord(@fdata[qkey][fstartpos[qkey]])^; // Read size
 pad_sz:=_Padlen(size);
// fstore_sz[qkey]:=pad_sz+(2*SizeOf(integer));  // ???

 inc(fstartpos[qkey],sizeof(integer)); // skip
 if fstartpos[qkey]=fpartition_size then fstartpos[qkey]:=0;
 if PLongword(@fdata[qkey][fstartpos[qkey]])^<>push_marker then GFRE_BT.CriticalAbort('PUSH MARKER NOT SET ! %d %d',[PLongWord(@fdata[qkey][fstartpos[qkey]])^,integer(push_marker)]);
 PlongWord(@fdata[qkey][fstartpos[qkey]])^:=pop_marker;

 inc(fstartpos[qkey],sizeof(integer)); // skip
 if fstartpos[qkey]=fpartition_size then fstartpos[qkey]:=0;

// if fstartpos[qkey]+sizeof(integer)+size<=fpartition_size then begin //TODO WARUM NUR WARUM ?not wrapped
 if fstartpos[qkey]+size<=fpartition_size then begin // not wrapped
  // writeln('HERE 1 -> ',fstartpos[qkey]);
   Data1:=@(fdata[qkey][fstartpos[qkey]]);
   Data2:=nil;
   Size1:=Size;
   Size2:=0;
 end else begin //wrapped
  // writeln('HERE 2 -> ',fstartpos[qkey]);
   Data1:=@(fdata[qkey][fstartpos[qkey]]);
   Data2:=@(Fdata[qkey][0]);
   Size1:=fpartition_size-fstartpos[qkey];
   Size2:=Size-Size1;
 end;
 inc(fstartpos[qkey],pad_sz);
 if fstartpos[qkey]>=fpartition_size then fstartpos[qkey]:=fstartpos[qkey]-fpartition_size;
 inc(fopen_popo_cnt[qkey]);
 {$IFDEF DEBUG_CQ} _Dumpstate('POP'); {$ENDIF}
 exit(cs_OK);
end;

procedure TFOS_PARTVARQ.MarkFree(const qkey: integer; const Data1: pointer);
var Markerptr   : PCardinal;
    TmpFreePos  : PCardinal;
    padded_size : integer;
    whole_sum   : integer;
//    sl          : TStringList;
begin
  whole_sum  := 0;
 {$IFDEF DEBUG_CQ}
   if (qkey<0) or (qkey>=FConsumerCount) then GFRE_BT.CriticalAbort('your qkey is for the bucket ...');
   if not ((data1>=@fdata[qkey][0]) and (data1<=@fdata[qkey][fpartition_size-sizeof(cardinal)])) then gfre_bt.CriticalAbort('data1 ptr out of range');
   if fopen_popo_cnt[qkey]<=0 then gfre_bt.CriticalAbort('Popo Count not positive %d qkey %d',[fopen_popo_cnt[qkey],qkey]);
 {$ENDIF}
  Markerptr:=Data1;
  if Markerptr<>@fdata[qkey][0] then begin
    Dec(Markerptr);
  end else begin
    Markerptr  := @fdata[qkey][fpartition_size-sizeof(cardinal)];
  end;
  {$IFDEF DEBUG_CQ}
    if not ((Markerptr>=@fdata[qkey][0]) and (Markerptr<=@fdata[qkey][fpartition_size-sizeof(cardinal)])) then gfre_bt.CriticalAbort('Markerptr ptr out of range');
  {$ENDIF}
  TmpFreePos := Markerptr;
  if TmpFreePos<>@fdata[qkey][0] then begin
    Dec(TmpFreePos);
  end else begin
    TmpFreePos  := @fdata[qkey][fpartition_size-SizeOf(Cardinal)];
  end;
  {$IFDEF DEBUG_CQ}
    if not ((TmpFreePos>=@fdata[qkey][0]) and (TmpFreePos<=@fdata[qkey][fpartition_size-sizeof(cardinal)])) then gfre_bt.CriticalAbort('TmpFreePos ptr out of range');
  {$ENDIF}
  if Markerptr^<>pop_marker then gfre_bt.CriticalAbort('du kannst nach hause fahrn (%x) -- %x --- %x',[Markerptr^,cardinal(Markerptr)-cardinal(@fdata[qkey][0]),cardinal(data1)]); //entweder beaf oder doppelfree ?
  Markerptr^ := $FEEDBEEF;
//  sl:=TStringlist.Create;
//  sl.add('Error Case:');
  if TmpFreePos=fflast_free_pos[qkey] then begin
    repeat
      padded_size             := _Padlen(PCardinal(fflast_free_pos[qkey])^);
//      sl.add(Format('OPenPopo=%d ps=%d flastfreepos=%x ',[fopen_popo_cnt[qkey],padded_size,ptruint(fflast_free_pos[qkey])]));
      fflast_free_pos[qkey]   := PCardinal(PByte(fflast_free_pos[qkey]) + padded_size + 2 * SizeOf(integer));
//      sl.add(Format('1 flastfreepos=%x ',[ptruint(fflast_free_pos[qkey])]));
      if fflast_free_pos[qkey] > @fdata[qkey][fpartition_size-sizeof(cardinal)] then fflast_free_pos[qkey] := fflast_free_pos[qkey] - fpartition_size;
//      sl.add(Format('2 flastfreepos=%x ',[ptruint(fflast_free_pos[qkey])]));
      {$IFDEF DEBUG_CQ}
       if not ((fflast_free_pos[qkey]>=@fdata[qkey][0]) and (fflast_free_pos[qkey]<=@fdata[qkey][fpartition_size-sizeof(cardinal)])) then gfre_bt.CriticalAbort('fflast_free_pos[qkey] ptr out of range');
      {$ENDIF}
      whole_sum := whole_sum + padded_size + 2 * sizeof(integer);
//      sl.add(Format('new wholesum %d',[whole_sum]));
      dec(fopen_popo_cnt[qkey]);
//      writeln('POPO ',fopen_popo_cnt[qkey],' KEY ',qkey);
      if (fopen_popo_cnt[qkey]=0) then break;
      Markerptr := fflast_free_pos[qkey];
      inc(Markerptr,1);
      if Markerptr > @fdata[qkey][fpartition_size-sizeof(cardinal)] then begin
//        sl.add('Markerpos Fixxed');
        Markerptr := Markerptr - (fpartition_size div 4);
      end;
//      sl.add(Format('END: Markerpos=%x start=%x end=%x',[Ptruint(Markerptr),Ptruint(@fdata[qkey][0]),PtrUint(@fdata[qkey][fpartition_size-sizeof(cardinal)])]));
      if Markerptr^=$FEEDBEEF then continue;
      if Markerptr^=pop_marker then break;
      writeln(GFRE_BT.Dump_Binary(Markerptr,16));
//      writeln(sl.Text);
      if not ((Markerptr>=@fdata[qkey][0]) and (Markerptr<=@fdata[qkey][fpartition_size-sizeof(cardinal)])) then gfre_bt.CriticalAbort('Markerptr ptr out of range');
      GFRE_BT.CriticalAbort('?? %x MP=%x  QS=%x',[Markerptr^,PtrUint(Markerptr)]);
    until false;
  end;
//  writeln('WHOLE SUM:'+inttostr(whole_sum)+' QKEY:'+inttostr(qkey));
  if whole_sum>0 then begin
    {$IFDEF DEBUG_CQ}
//      writeln('FREESPACE ',ffree_space[qkey]);
//      if (whole_sum>fpartition_size) then GFRE_BT.CriticalAbort('eierbär22 frei qkey '+inttostr(qkey)+' WS:'+inttostr(whole_sum)+' PS:'+inttostr(fpartition_size) );
      whole_sum:=InterLockedExchangeAdd(ffree_space[qkey],whole_sum);
      if (whole_sum>fpartition_size) then begin
//       writeln(sl.Text);
       GFRE_BT.CriticalAbort('eierbär frei qkey '+inttostr(qkey)+' WS:'+inttostr(whole_sum)+' PS:'+inttostr(fpartition_size) );
      end;
     // writeln('FREESPACE ',ffree_space[qkey],' ',qkey);
    {$ELSE}
      InterLockedExchangeAdd(ffree_space[qkey],whole_sum);
    {$ENDIF}
  end;
//  sl.free;
// fpopping[qkey]:=0;
//// old:=InterlockedCompareExchange(fpopping[qkey],0,mk);
//// if old <> mk then dbt.CriticalAbort('FCUK2');
// {$IFDEF DEBUG_CQ} _Dumpstate('FIPO'); {$ENDIF}
{$IFDEF DEBUG_CQ}
  _Dumpstate('MARKFREE');
{$ENDIF}
end;



function TFOS_PARTVARQ.Count(const qkey: integer): integer; //COunt Approximation
begin
 {$IFDEF DEBUG_CQ}
   if (qkey<0) or (qkey>=FConsumerCount) then GFRE_BT.CriticalAbort('your qkey is for the bucket ...');
 {$ENDIF}
  result := fpushed_count[qkey];
end;

function TFOS_PARTVARQ.Space: integer; //Space approximation
var i:integer;
begin
  result:=0;
  for i:=0 to FConsumerCount-1 do begin
   result:=result+ffree_space[i];
  end;
end;

function TFOS_PARTVARQ.QSize: integer;
begin
  result:=fpartition_size*FConsumerCount;
end;

initialization

end.

