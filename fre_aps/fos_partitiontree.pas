unit fos_partitiontree;

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
  Classes, SysUtils,FOS_REDBLACKTREE_GEN,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FOS_INTERLOCKED;

type

  { TFOS_PARTITION_TREE }

  generic TFOS_PARTITION_TREE<_TKey,_TStore,_TPassType> = class (TObject)
  private type
    _IT=specialize TGFOS_RBTree<_TKEY,_TStore>;
     TCompareKeys     = function  (const Item1, Item2: _TKey): Integer;
    _RTT=record
      key             : _TKey;
      part            : integer;
      forall_counter  : integer;
      pt              : _TPassType;
    end;
    _PRTT=^_RTT;
  var private
    PAR : ARRAY OF _IT;
    basetick : int64;
    c_partitions:integer;
  private
     FMaximumEntries: QWord;
     procedure  _SetMaximumEntries  (const AValue: QWord);
     procedure  _AddCont           (const TID:integer;const CMD:Pointer;const cp:integer);
     procedure  _DelCont           (const TID:integer;const CMD:Pointer;const cp:integer);
     procedure  _OperateCont       (const TID:integer;const CMD:Pointer;const cp:integer);
     procedure  _ForallCont        (const TID:integer;const CMD:Pointer;const cp:integer);
     function   _KeyModulo         (const k:_TKey):integer;inline;
  public
    constructor Create        (const Compare:TCompareKeys);
    destructor  Destroy       ;override;
    procedure   FindAdd       (const key:_TKey;const passtype:_TPasstype);  // Ignores  new value if old value exists
    procedure   FindAddDirect (const key:_TKey;const passtype:_TPasstype;const part:integer);  // Ignores  new value if old value exists
    function    PartitionKey  (const key:_TKey):integer;
    procedure   Operate       (const key:_TKey;const passtype:_TPasstype);
    procedure   Delete        (const key:_TKey;const passtype:_TPasstype);
    procedure   ForAll        (const passtype:_TPasstype);
    function    Count         :QWord;
    procedure   DumpCounters  ;
    procedure   Dumptrees     (const passtype: _TPasstype);
    property    MaximumEntries: QWord read FMaximumEntries write _SetMaximumEntries;
  end;

implementation

{ TFOS_PARTITION_TREE }

procedure TFOS_PARTITION_TREE._SetMaximumEntries(const AValue: QWord);
begin
 if FMaximumEntries=AValue then exit;
 FMaximumEntries:=AValue;
end;

procedure TFOS_PARTITION_TREE._AddCont(const TID:integer;const CMD: Pointer; const cp: integer);
var found_store:_TStore;
    new_store:_Tstore;

begin
  if par[_PRTT(CMD)^.part].Find(_PRTT(CMD)^.key,found_store) then begin
//    writeln(GFRE_BT.Get_Ticks_us-basetick,':  SEARCH (',_PRTT(CMD)^.key,') IN ',_PRTT(CMD)^.part,' FOUND KEY= ',_PRTT(CMD)^.key);
    _PRTT(cmd)^.pt.found(tid,_PRTT(CMD)^.key,found_store,false);
    _PRTT(cmd)^.pt.free;
    _PRTT(cmd)^.pt:=nil;
    dispose(_PRTT(cmd));
    exit;
  end else begin
//    writeln(GFRE_BT.Get_Ticks_us-basetick,':  SEARCH (',_PRTT(CMD)^.key,') IN ',GFRE_S.Current_Thread,' NOT FOUND');
    if FMaximumEntries>0 then begin
     if Count>=FMaximumEntries then begin
      GFRE_LOG.Log('MAXIMUM ENTRIES '+inttostr(FMaximumEntries)+' IN PARTITION TREE REACHED',catInfo);
      _PRTT(CMD)^.pt.addfailed(tid,_PRTT(CMD)^.key);
      _PRTT(cmd)^.pt.free;
      _PRTT(cmd)^.pt:=nil;
      dispose(_PRTT(cmd));
//      GFRE_BT.CriticalAbort('MAXIMUM ENTRIES '+inttostr(FMaximumEntries)+' IN PARTITION TREE REACHED');
      exit;
     end;
    end;
    new_store:=_TStore.create;
    if not PAR[TID].add(_PRTT(CMD)^.key,new_store) then begin
      writeln(GFRE_BT.Get_Ticks_us-basetick,'  ABORTING 1');
      GFRE_BT.CriticalAbort('shit happens / Thread=(%d)  Would like to add Key',[TID]);
    end;
//    writeln(GFRE_BT.Get_Ticks_us-basetick,':  ADDED (',_PRTT(CMD)^.key,') IN ',GFRE_S.Current_Thread);
    _PRTT(CMD)^.pt.found(tid,_PRTT(CMD)^.key,_TStore(new_store),true);
    _PRTT(CMD)^.pt.free;
    dispose(_PRTT(cmd));
    exit;
  end;
end;

procedure TFOS_PARTITION_TREE._DelCont(const TID:integer;const CMD: Pointer; const cp: integer);
var found_store:_TStore;
begin
 if par[_PRTT(CMD)^.part].Delete(_PRTT(CMD)^.key,found_store) then begin
   _PRTT(cmd)^.pt.delete(tid,_PRTT(CMD)^.key,found_store);
   found_store.free;
 end;
 _PRTT(cmd)^.pt.free;
 _PRTT(cmd)^.pt:=nil;
 dispose(_PRTT(cmd));
end;

procedure TFOS_PARTITION_TREE._OperateCont(const TID:integer;const CMD: Pointer; const cp: integer);
var found_store:_TStore;
begin
 if par[_PRTT(CMD)^.part].Find(_PRTT(CMD)^.key,found_store) then begin
   _PRTT(cmd)^.pt.operate(tid,_PRTT(CMD)^.key,found_store);
 end;
 _PRTT(cmd)^.pt.free;
 _PRTT(cmd)^.pt:=nil;
 dispose(_PRTT(cmd));
end;

procedure TFOS_PARTITION_TREE._ForallCont(const TID:integer;const CMD: Pointer; const cp: integer);
var ilr:integer;
begin
 try
    par[TID].ForAllItems(@_PRTT(cmd)^.pt.forall,_PRTT(cmd)^.pt);
    ilr:=InterLockedDecrement(_PRTT(cmd)^.forall_counter);
//    writeln('FORALL_COUNTER ',ilr);
    if ilr=0 then begin
//      writeln ('FREE PASSTYPE',integer(cmd));
//      writeln(GFRE_S.CURRENT_THREAD,integer(_PRTT(cmd)^.pt));
      _PRTT(cmd)^.pt.free;
      _PRTT(cmd)^.pt:=nil;
 //    GFRE_BT.CriticalAbort('DEBUG FORALL FREE DONE');
      dispose(_PRTT(cmd));
    end;
 except on e:Exception do begin
  writeln('EXCEPTION IN FOR ALL CONT :'+E.Message);
  writeln('THREAD :',TID);
  writeln('PT :',integer(_PRTT(cmd)^.pt));
  raise;
 end; end;
end;

function TFOS_PARTITION_TREE._KeyModulo(const k: _TKey): integer; inline;
var len,i:integer;
    kv:Cardinal;
    pk:PByte;
begin
  kv:=0;
  len:=SizeOf(_TKey);pk:=@k;
  for i:=0 to len-1 do begin
    kv:=kv+pk^;inc(pk);
  end;
  result:=kv mod c_partitions;
//  writeln(result);
end;

constructor TFOS_PARTITION_TREE.Create(const Compare:TCompareKeys);
var i    : Integer;
begin
  inherited Create;
  c_partitions:=GFRE_CPU.Logical;
  SetLength(PAR,c_partitions);
//  WriteLn('SET TO ',length(PAR),' PARTITIONS');
  for i:=0 to c_partitions-1 do begin
    par[i]:=_IT.Create(Compare);
  end;
  basetick:=GFRE_BT.Get_Ticks_us;
  FMaximumEntries:=0;
end;

destructor TFOS_PARTITION_TREE.Destroy;
var
  i: Integer;
begin
  for i:=0 to high(PAR) do begin
    par[i].Free;
  end;
  inherited Destroy;
end;

procedure TFOS_PARTITION_TREE.FindAdd(const key: _TKey;const passtype:_TPasstype);
var i:Integer;
    rttp:_PRTT;
begin
  new(rttp);
  rttp^.key              := key;
  rttp^.part             := _KeyModulo(key);
  rttp^.pt               := passtype;
  GFRE_S.AddGenericCommandForSpecificP(rttp,rttp^.part,@_AddCont,1);
end;

procedure TFOS_PARTITION_TREE.FindAddDirect(const key: _TKey; const passtype: _TPasstype; const part: integer);
var rttp:_RTT;
begin
  rttp.key  := key;
  rttp.part := part;
  rttp.pt   := passtype;
  _AddCont(part,@rttp,1);
end;

function TFOS_PARTITION_TREE.PartitionKey(const key: _TKey): integer;
begin
 result:=_KeyModulo(key);
end;

procedure TFOS_PARTITION_TREE.Operate(const key: _TKey;const passtype: _TPasstype);
var rttp:_PRTT;
begin
  new(rttp);
  rttp^.key              := key;
  rttp^.part             := _KeyModulo(key);
  rttp^.pt               := passtype;
  GFRE_S.AddGenericCommandForSpecificP(rttp,rttp^.part,@_OperateCont,1);
end;

procedure TFOS_PARTITION_TREE.Delete(const key: _TKey;const passtype: _TPasstype);
var rttp:_PRTT;
begin
  new(rttp);
  rttp^.key              := key;
  rttp^.part             := _KeyModulo(key);
  rttp^.pt               := passtype;
  GFRE_S.AddGenericCommandForSpecificP(rttp,rttp^.part,@_DelCont,1);
end;

procedure TFOS_PARTITION_TREE.ForAll(const passtype: _TPasstype);
var i:Integer;
    rttp:_PRTT;
begin
  new(rttp);
  rttp^.pt               := passtype;
  rttp^.forall_counter   := c_partitions;
  GFRE_S.AddGenericCommandForAllP(rttp,@_ForAllCont,1);
end;

function TFOS_PARTITION_TREE.Count: QWord;
var i:integer;
begin
 result:=0;
 for i:=0 to high(PAR) do begin
  result:=result+par[i].Count;
 end;
end;

procedure TFOS_PARTITION_TREE.DumpCounters;
var
  i,k: Integer;
begin
  k:=0;
  write('Counters [');
  for i:=0 to high(PAR) do begin
    k:=k+par[i].Count;
    write(' ',par[i].Count:2,' ');
  end;
  writeln('  ',k,' ] ');
end;

procedure TFOS_PARTITION_TREE.Dumptrees(const passtype: _TPasstype);
var
  i: Integer;
begin
  for i:=0 to high(par) do begin
    writeln('TREE DUMP PARTITION (',i,') THREAD=',i);
    par[i].ForAllItems(@passtype.DumpText,pointer(i));
  end;
end;

end.

