unit fos_redblacktree_gen;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

uses
  Classes,sysutils,math;

type
  TRB_NodeColor=(R,B);

  TRB_Mode=(rbDynamic,rbStatic,rbHybrid);


 // {$B-}
  { TGFOS_RBTree }
  generic TGFOS_RBTree<_TKey,_TStore> = class(TObject)
    public type
      PFOS_RB_NodeG=^TFOS_RB_NodeG;
      _PStore       =^_TStore;
      _PKey         =^_TKey;
      TFOS_RB_NodeG = packed record
          k:  _TKey;
          left, right, parent: PFOS_RB_NodeG;
          col: TRB_NodeColor;
          val:_TStore;
       end;
      TFOS_ArrayNodeG = packed record
          k:  _TKey;
          val:_TStore;
       end;
      TFOS_TStore_Array = array of _TStore;
      TFOS_BuildArray   = record
        idx : NativeInt;
        arr : TFOS_TStore_Array;
      end;
      PFOS_BuildArray = ^TFOS_BuildArray;

       TCompareKeys           = function  (const Item1, Item2: _TKey): NativeInt;
       TCompareKeysEx         = function  (const Item1, Item2: _TKey;const DP:Pointer): NativeInt;
       TGUndefinedKey         = function  :_TKey;
       TGFOS_RB_OnItem        = procedure (const Item:_TStore) of object;
       TGFOS_RB_OnItemN       = procedure (const Item:_TStore);
       TGFOS_RB_OnItemP       = procedure (const Item:_TStore;const P:Pointer) of object;
       TGFOS_RB_OnItemPN      = procedure (const Item:_TStore;const P:pointer);
       TGFOS_RB_OnKey         = procedure (const Key :_TKey;const P:Pointer) of Object;
       TGFOS_RB_OnKeyN        = procedure (const Key :_TKey;const P:Pointer);
       TGFOS_RB_OnNode        = procedure (const Key :_TKey;const Item:_TStore;const P:Pointer) of Object;
       TGFOS_RB_OnNodeN       = procedure (const Key :_TKey;const Item:_TStore;const P:Pointer);
       TGFOS_RB_OnNodeLocal   = procedure (const Key:Pointer;const Item:Pointer);
       TGFOS_RB_OnNodeNested  = procedure (const Key :_TKey;const Item:_TStore) is nested;
       TGFOS_RB_OnItemNested  = procedure (const Item:_TStore) is nested;
       TGFOS_RB_OnNodeNestedB = function  (const Key :_TKey;const Item:_TStore):boolean is nested;
       TGFOS_RB_OnItemNestedB = function  (const Item:_TStore):boolean is nested;
    var private
      emptyval     : _TStore;
      _Count       : QWord;
      _CompareSi   : TCompareKeys;
      _CompareEx   : TCompareKeysEx;
      FDataPointer : Pointer;
      root         : PFOS_RB_NodeG;
      leftmost     : PFOS_RB_NodeG;
      rightmost    : PFOS_RB_NodeG;
      _rbmode      : TRB_Mode;
      FTreeChanged : Boolean;
      nodearray    : Array of TFOS_ArrayNodeG;
      procedure  RotLeft               (var x: PFOS_RB_NodeG);inline;
      procedure  RotRight              (var x: PFOS_RB_NodeG);inline;
      function   Min                   (var x: PFOS_RB_NodeG): PFOS_RB_NodeG;
      function   Max                   (var x: PFOS_RB_NodeG): PFOS_RB_NodeG;
      procedure  _Delete               (z: PFOS_RB_NodeG);
      function   _Find                 (key:_TKey):PFOS_RB_NodeG;
      function   _FindNextPrev         (key:_TKey;const next:boolean;const equalsearch:boolean=false):PFOS_RB_NodeG;
      procedure  _fast_erase           (x: PFOS_RB_NodeG;const DoFreeItem:TGFOS_RB_OnItem);
      procedure  _fast_eraseN          (x: PFOS_RB_NodeG;const DoFreeItem:TGFOS_RB_OnItemN);
      procedure  _fast_eraseIt         (x: PFOS_RB_NodeG;const DoFreeItem:TGFOS_RB_OnItemNested);
      procedure  _LocalProcedure_Hack  (Proc,FrameP,p1,p2: Pointer); inline;
      function   _AFind                (key:_TKey):Integer;
      function   _AFindNextPrev        (key:_TKey;const next:boolean;const equalsearch:boolean=false):Integer;
      procedure  _SetLocalStaticArray  (const Key :_TKey;const Item:_TStore;const P:Pointer);
      function   _InternalAddCheck     (const key:_TKey;var   Store:_TStore;const replace:boolean; out KeyPointer:_Pkey;out StorePointer:_PStore):Boolean;
      function   InternalCompare       (const Item1, Item2: _TKey):integer;inline;
      procedure  _Zero                 ;
      procedure  _BuildArrayFromNodes  (const Key :_TKey;const Item:_TStore;const P:Pointer);
    protected
      procedure  RBInc                 (var x: PFOS_RB_NodeG);inline;
      procedure  RBDec                 (var x: PFOS_RB_NodeG);inline;
    public
      constructor Create               (const Compare:TCompareKeys);
      constructor Create               (const Compare:TCompareKeysEx;const DataPointer:Pointer);
      destructor  Destroy              ; override;
      procedure   Clear                (const DoFreeItem: TGFOS_RB_OnItem=nil);
      procedure   ClearN               (const DoFreeItem: TGFOS_RB_OnItemN=nil);
      procedure   ClearItems           (const DoFreeItem: TGFOS_RB_OnItemNested=nil);
      function    Find                 (const key:_TKey;out   store:_TStore):Boolean;  // Delivers True on Find
      function    FindEx               (const key:_TKey;out   store:_PStore):Boolean;  // Delivers True on Find
      function    Exists               (const key:_TKey):Boolean;
      function    Update               (const key:_TKey;const Store:_TStore):Boolean;  // Delivers True on Find
      function    Delete               (const key:_TKey;var   Store:_TStore):Boolean;  // Deletes the Item from the Directory, returns true on found
      function    AddCheck             (const key:_TKey;var   Store:_TStore;const replace:boolean=false):Boolean; // Delivers Old Value on Find
      function    Add                  (const key:_TKey;const Store:_TStore):Boolean;  // Ignores  new value if old value exists
      function    AddCheckEx           (const key:_TKey;var   Store:_TStore;const replace:boolean; var KeyPointer:_Pkey;var StorePointer:_PStore):Boolean;
      function    FindNext             (var   key:_TKey;var   Store:_TStore):Boolean;  // Delivers false if no next element
      function    FindPrev             (var   key:_TKey;var   Store:_TStore):Boolean;  // Delivers false  if no prev element
      function    FindNextOrEQ         (var   key:_TKey;var   Store:_TStore):Boolean;  // Delivers false if no next element
      function    FindPrevOrEQ         (var   key:_TKey;var   Store:_TStore):Boolean;  // Delivers true  if no prev element
      function    FirstNode            (var   key:_TKey;var   Store:_TStore):boolean;
      function    LastNode             (var   key:_TKey;var   Store:_TStore):boolean;
      function    Count                :QWord;
      procedure   ForAllValues         (const ValProc: TGFOS_RB_OnItemP ;const P:Pointer); // You Have to manually signal a TREECHANGE !
      procedure   ForAllValuesN        (const ValProc: TGFOS_RB_OnItemPN;const P:Pointer);
      procedure   ForAllKeys           (const KeyProc: TGFOS_RB_OnKey ;const P:Pointer);
      procedure   ForAllKeysN          (const KeyProc: TGFOS_RB_OnKeyN;const P:Pointer);
      procedure   ForAllItems          (const NodProc: TGFOS_RB_OnNode;const P:Pointer);
      procedure   ForAllItemsN         (const NodProc: TGFOS_RB_OnNodeN;const P:Pointer);
      procedure   ForAllItems_LM       (const Local_NodeProc:Pointer); // Local Procedure of Type Local(key:Pointer;node:Pointer)
      procedure   ForAllItems          (const nested_item_proc: TGFOS_RB_OnItemNested);
      procedure   ForAllNodes          (const nested_node_proc: TGFOS_RB_OnNodeNested);
      function    ForAllItemsBrk       (const nested_item_proc: TGFOS_RB_OnItemNestedB):boolean;
      function    ForAllNodesBrk       (const nested_node_proc: TGFOS_RB_OnNodeNestedB):boolean;
      procedure   SwitchMode           (const mode:TRB_Mode);
      procedure   SaveToStream         (const st:TStream);
      procedure   LoadFromStream       (const st:TStream);
      function    GetDirect            (const a_index:integer; var key:_TKey; var Store:_TStore):boolean;  // Direct Access on static Tree, True on in Range
      function    GetIndex             (const key:_Tkey;var a_index:integer):boolean;
      procedure   ForAllItemsRange     (const idx:integer ; item_count : integer ; const nested_item_proc: TGFOS_RB_OnItemNested);
      procedure   ForAllNodesRange     (const idx:integer ; item_count : integer ; const nested_node_proc: TGFOS_RB_OnNodeNested);
      function    GetAllItemsAsArray   : TFOS_TStore_Array;
      procedure   SignalTreeChange     ;
      function    QueryTreeChange      : Boolean;
   end;

   TFOS_RB_Tree_SS      = specialize TGFOS_RBTree<string,string>;
   TFOS_RB_Tree_II      = specialize TGFOS_RBTree<integer,integer>;
//   IFOS_RB_Tree_IIf     = specialize IFOS_RB_Tree<integer,integer>;
//   TFOS_RB_Tree_IIf     = class(TFOS_RB_Tree_II,IFOS_RB_Tree_IIf);


//Default Sorting & Value functions
function Default_RB_String_Compare   (const S1, S2: string): NativeInt;
function Default_RB_Integer_Compare  (const d1, d2: integer): NativeInt;
function Default_RB_Cardinal_Compare (const d1, d2: cardinal): NativeInt;
function Default_RB_Int64_Compare    (const d1, d2: int64): NativeInt;
function Default_RB_Qword_Compare    (const d1, d2: qword): NativeInt;

implementation



function Default_RB_String_Compare(const S1, S2: string): NativeInt;
var count1, count2,i: integer;
    p1,p2:pointer;
begin
  Count1 := Length(S1);Count2 := Length(S2);
  if count1=count2 then begin
    i := 0;
    result := 0;
    p1:=pointer(s1);
    p2:=pointer(s2);
    while (result=0) and (I<count1) do begin
      result:=byte(P1^)-byte(P2^);
      P1:=pchar(P1)+1;P2:=pchar(P2)+1;
      inc(i);
    end;
  end else
  if Count1>Count2 then begin
   result:=1;
   exit;
  end else begin
   result:=-1;
   exit;
  end;
end;



function Default_RB_Integer_Compare(const d1, d2: integer): NativeInt;
begin
  if d1=d2 then begin
   result := 0;
  end else
  if d1>d2 then begin
   result:=1;
  end else begin
   result:=-1;
  end;
end;

function Default_RB_Cardinal_Compare(const d1, d2: cardinal): NativeInt;
begin
  if d1=d2 then begin
   result := 0;
  end else
  if d1>d2 then begin
   result:=1;
  end else begin
   result:=-1;
  end;
end;


function Default_RB_Int64_Compare(const d1, d2: int64): NativeInt;
begin
  if d1=d2 then begin
   result := 0;
  end else
  if d1>d2 then begin
   result:=1;
  end else begin
   result:=-1;
  end;
end;

function Default_RB_Qword_Compare(const d1, d2: qword): NativeInt;
begin
  if d1=d2 then begin
    result := 0;
  end else
  if d1>d2 then begin
    result:=1;
  end else begin
    result:=-1;
  end;
end;


constructor TGFOS_RBTree.Create(const Compare:TCompareKeys);
begin
  inherited Create;
  _Count:=0;
  _CompareSi:=Compare;
  _rbmode:=rbDynamic;
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

constructor TGFOS_RBTree.Create(const Compare: TCompareKeysEx; const DataPointer: Pointer);
begin
  inherited Create;
  _Count       :=0;
  _CompareEx   := Compare;
  _rbmode      := rbDynamic;
  FDataPointer := DataPointer;
  root         := nil;
  leftmost     := nil;
  rightmost    := nil;
end;


function TGFOS_RBTree.Delete(const key: _TKey; var Store: _TStore): Boolean;
var n:PFOS_RB_NodeG;
begin
  case _rbMode of
    rbDynamic: begin
      n:=_Find(key);
      if not assigned(n) then begin
        {$HINTS OFF}
        Fillchar(store,sizeof(store),#0);
        {$HINTS ON}
        result:=false;
      end else begin
        store:=n^.val;
        _Delete(n);
        dec(_count);
        result:=true;
        FTreeChanged := true;
      end;
    end;
    rbStatic: begin
      raise Exception.Create('DELETE NOT ALLOWED IN STATIC TREE');
    end;
    rbHybrid: begin
      raise Exception.Create('NOT IMPLEMENTED');
    end;
  end;
end;

destructor TGFOS_RBTree.Destroy;
begin
  Clear;
  inherited Destroy;
end;


function TGFOS_RBTree.FirstNode(var key: _TKey; var Store: _TStore): boolean;
begin
 case _rbMode of
  rbDynamic: begin
   if Assigned(leftmost) then begin
    result := true;
    key    := leftmost^.k;
    Store  := leftmost^.val;
   end else begin
    result := false;
    Fillchar(store,sizeof(store),#0);
    Fillchar(key,sizeof(key),#0);
   end;
  end;
  rbStatic: begin
   if length(nodearray)>0 then begin
    result:=true;
    key    := nodearray[0].k;
    Store  := nodearray[0].val;
   end else begin
    result := false;
    Fillchar(store,sizeof(store),#0);
    Fillchar(key,sizeof(key),#0);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

function TGFOS_RBTree.LastNode(var key: _TKey; var Store: _TStore): boolean;
begin
 case _rbMode of
  rbDynamic: begin
   if Assigned(rightmost) then begin
    result := true;
    key    := rightmost^.k;
    Store  := rightmost^.val;
   end else begin
    result := false;
    Fillchar(store,sizeof(store),#0);
    Fillchar(key,sizeof(key),#0);
   end;
  end;
  rbStatic: begin
   if length(nodearray)>0 then begin
    result:=true;
    key    := nodearray[high(nodearray)].k;
    Store  := nodearray[high(nodearray)].val;
   end else begin
    result := false;
    Fillchar(store,sizeof(store),#0);
    Fillchar(key,sizeof(key),#0);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

function TGFOS_RBTree.Count: QWord;
begin
 result:=_Count;
end;

procedure TGFOS_RBTree.ForAllKeys(const KeyProc: TGFOS_RB_OnKey;const P:Pointer);
var n:PFOS_RB_NodeG;
    ky:_TKey;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    ky :=n^.k;
    KeyProc(ky,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ky :=nodearray[a_index].k;
    KeyProc(ky,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

procedure TGFOS_RBTree.ForAllKeysN(const KeyProc: TGFOS_RB_OnKeyN;const P:Pointer);
var n:PFOS_RB_NodeG;
    ky:_TKey;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    ky :=n^.k;
    KeyProc(ky,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ky :=nodearray[a_index].k;
    KeyProc(ky,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;


procedure TGFOS_RBTree.ForAllValues(const ValProc: TGFOS_RB_OnItemP;const P:Pointer);
var n:PFOS_RB_NodeG;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    VAlProc(n^.val,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ValProc(nodearray[a_index].val,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

procedure TGFOS_RBTree.ForAllValuesN(const ValProc: TGFOS_RB_OnItemPN;const P:Pointer);
var n:PFOS_RB_NodeG;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    VAlProc(n^.val,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ValProc(nodearray[a_index].val,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;



procedure TGFOS_RBTree.ForAllItems(const NodProc: TGFOS_RB_OnNode;const P: Pointer);
var n:PFOS_RB_NodeG;
    ky:_Tkey;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    ky :=n^.k;
    NodProc(ky,n^.val,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ky :=nodearray[a_index].k;
    NodProc(ky,nodearray[a_index].val,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
end;
end;

procedure TGFOS_RBTree.ForAllItemsN(const NodProc: TGFOS_RB_OnNodeN;const P: Pointer);
var n:PFOS_RB_NodeG;
    ky:_TKey;
    a_index:integer;
begin
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    ky :=n^.k;
    NodProc(ky,n^.val,p);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    ky :=nodearray[a_index].k;
    NodProc(ky,nodearray[a_index].val,p);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

procedure TGFOS_RBTree.ForAllItems_LM(const Local_NodeProc: Pointer);
var n:PFOS_RB_NodeG;
    frame:Pointer;
    a_index:integer;
begin
 Frame:=get_caller_frame(get_frame);
 case _rbMode of
  rbDynamic: begin
   n:=leftmost;
   while Assigned(n) do begin
    _LocalProcedure_Hack(Local_NodeProc,frame,@n^.k,@n^.val);
    if n<>rightmost then begin
     RBInc(n);
    end else begin
     exit;
    end;
   end;
  end;
  rbStatic: begin
   for a_index:=low(nodearray) to high(nodearray) do begin
    _LocalProcedure_Hack(Local_NodeProc,frame,@nodearray[a_index].k,@nodearray[a_index].val);
   end;
  end;
  rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

procedure TGFOS_RBTree.ForAllItems(const nested_item_proc: TGFOS_RB_OnItemNested);
var n:PFOS_RB_NodeG;
    ky:_Tkey;
    a_index:integer;
begin
  case _rbMode of
    rbDynamic: begin
     n:=leftmost;
     while Assigned(n) do begin
      ky :=n^.k;
      nested_item_proc(n^.val);
      if n<>rightmost then begin
       RBInc(n);
      end else begin
       exit;
      end;
     end;
    end;
    rbStatic: begin
     for a_index:=low(nodearray) to high(nodearray) do begin
      ky :=nodearray[a_index].k;
      nested_item_proc(nodearray[a_index].val);
     end;
    end;
    rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
  end;
end;

procedure TGFOS_RBTree.ForAllNodes(const nested_node_proc: TGFOS_RB_OnNodeNested);
var n:PFOS_RB_NodeG;
    ky:_Tkey;
    a_index:integer;
begin
  case _rbMode of
    rbDynamic: begin
     n:=leftmost;
     while Assigned(n) do begin
      ky :=n^.k;
      nested_node_proc(ky,n^.val);
      if n<>rightmost then begin
       RBInc(n);
      end else begin
       exit;
      end;
     end;
    end;
    rbStatic: begin
     for a_index:=low(nodearray) to high(nodearray) do begin
      ky :=nodearray[a_index].k;
      nested_node_proc(ky,nodearray[a_index].val);
     end;
    end;
    rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
  end;
end;

function TGFOS_RBTree.ForAllItemsBrk(const nested_item_proc: TGFOS_RB_OnItemNestedB): boolean;
var n:PFOS_RB_NodeG;
    a_index:integer;
begin
  result:=false;
  case _rbMode of
    rbDynamic: begin
     n:=leftmost;
     while Assigned(n) do begin
      if nested_item_proc(n^.val) then exit(true);
      if n<>rightmost then begin
       RBInc(n);
      end else begin
       exit;
      end;
     end;
    end;
    rbStatic: begin
     for a_index:=low(nodearray) to high(nodearray) do begin
      if nested_item_proc(nodearray[a_index].val) then exit(true);
     end;
    end;
    rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
  end;
end;

function TGFOS_RBTree.ForAllNodesBrk(const nested_node_proc: TGFOS_RB_OnNodeNestedB): boolean;
var n:PFOS_RB_NodeG;
    ky:_Tkey;
    a_index:integer;
begin
  result := false;
  case _rbMode of
    rbDynamic: begin
     n:=leftmost;
     while Assigned(n) do begin
      ky :=n^.k;
      if nested_node_proc(ky,n^.val) then exit(true);
      if n<>rightmost then begin
       RBInc(n);
      end else begin
       exit;
      end;
     end;
    end;
    rbStatic: begin
     for a_index:=low(nodearray) to high(nodearray) do begin
      ky :=nodearray[a_index].k;
      if nested_node_proc(ky,nodearray[a_index].val) then exit(true);
     end;
    end;
    rbHybrid: raise Exception.Create('NOT IMPLEMENTED');
  end;
end;

procedure TGFOS_RBTree.SwitchMode(const mode: TRB_Mode);
var a_index:LongWord;
begin
 if mode=_rbmode then exit;
 case mode of
  rbDynamic:begin
   raise Exception.Create('NOT IMPLEMENTED');
  end;
  rbStatic:begin
   SetLength(nodearray,_Count);
   a_index:=0;
   ForAllItems(@_SetLocalStaticArray,@a_index);
   if (root <> nil) then _fast_erase(root,nil);
   root := nil;
   leftmost := nil;
   rightmost := nil;
   _rbmode:=rbStatic;
  end;
  rbHybrid:begin
   raise Exception.Create('NOT IMPLEMENTED');
  end;
 end;
end;

procedure TGFOS_RBTree.SaveToStream(const st: TStream);
var sze :LongWord;
    cnt :LongWord;
      i :Integer;
begin
 case _rbmode of
  rbDynamic: raise Exception.Create('NOT IMPLEMENTED');
  rbStatic : begin
   cnt:=Length(nodearray);
   sze:=sizeof(TFOS_ArrayNodeG);
   st.WriteDWord(cnt);
   for i:=0 to cnt-1 do begin
    st.Write(nodearray[i],sze);
   end;
  end;
  rbHybrid : raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

procedure TGFOS_RBTree.LoadFromStream(const st: TStream);
var sze :LongWord;
    cnt :LongWord;
      i :Integer;
begin
 case _rbmode of
  rbDynamic: raise Exception.Create('NOT IMPLEMENTED');
  rbStatic : begin
   sze:=sizeof(TFOS_ArrayNodeG);
   cnt:=st.ReadDWord;
   SetLength(nodearray,cnt);
   _Count:=cnt;
   for i:=0 to cnt-1 do begin
    st.Read(nodearray[i],sze);
   end;
  end;
  rbHybrid : raise Exception.Create('NOT IMPLEMENTED');
 end;
end;

function TGFOS_RBTree.GetDirect(const a_index: integer;var key: _TKey; var Store: _TStore): boolean;
var i   : integer;
    nd  : PFOS_RB_NodeG;
begin
 case _rbmode of
  rbDynamic:begin
   //raise Exception.Create('GET NOT POSSIBLE ON DYNAMIC TREE');
   if a_index<count then begin
     nd := leftmost;
     for i:= 1 to a_index do begin
       RBInc(nd);
     end;
     key    := nd^.k;
     store  := nd^.val;
     result := true;
   end else begin
    result  :=false;
    Fillchar(store,sizeof(store),#0);
   end;
  end;
  rbStatic:begin
   if (a_index>=low(nodearray)) and (a_index<=high(nodearray)) then begin
    key:=nodearray[a_index].k;
    store:=nodearray[a_index].val;
    result:=true;
   end else begin
    result  :=false;
    Fillchar(store,sizeof(store),#0);
   end;
  end;
  rbHybrid:begin
   raise Exception.Create('NOT IMPLEMENTED');
  end;
 end;
end;

function TGFOS_RBTree.GetIndex(const key: _Tkey; var a_index: integer): boolean;
var a_ix:integer;
begin
 case _rbmode of
  rbDynamic:begin
   raise Exception.Create('GET NOT POSSIBLE ON DYNAMIC TREE');
  end;
  rbStatic:begin
   a_ix:=_AFind(key);
   if a_ix>=0 then begin
    a_index:=a_ix;
    result:=true;
   end else begin
    result:=false;
   end;
  end;
  rbHybrid:begin
   raise Exception.Create('NOT IMPLEMENTED');
  end;
 end;
end;

procedure TGFOS_RBTree.ForAllItemsRange(const idx: integer; item_count: integer; const nested_item_proc: TGFOS_RB_OnItemNested);
var nd:PFOS_RB_NodeG;
     i:integer;
begin
  case _rbmode of
   rbDynamic:begin
    if (idx<count) and (item_count>0) then begin
      nd := leftmost;
      for i:= 1 to idx do begin
        RBInc(nd);
      end;
      if idx+item_count>=count then begin
        item_count := count-idx-1;
      end;
      for i:=1 to item_count do begin
        nested_item_proc(nd^.val);
        RBInc(nd);
      end;
     end;
    end;
   else raise Exception.Create('NOT IMPLEMENTED');
  end;
end;

procedure TGFOS_RBTree.ForAllNodesRange(const idx: integer; item_count: integer; const nested_node_proc: TGFOS_RB_OnNodeNested);
var nd:PFOS_RB_NodeG;
     i:integer;
begin
  case _rbmode of
   rbDynamic:begin
    if (idx<count) and (item_count>0) then begin
      nd := leftmost;
      for i:= 1 to idx do begin
        RBInc(nd);
      end;
      if idx+item_count>count then begin
        item_count := count-idx;
      end;
      for i:=1 to item_count do begin
        nested_node_proc(nd^.k,nd^.val);
        if i<item_count then begin
          RBInc(nd);
        end;
      end;
     end;
    end;
   else raise Exception.Create('NOT IMPLEMENTED');
  end;
end;


function TGFOS_RBTree.GetAllItemsAsArray: TFOS_TStore_Array;
var build_struct : TFOS_BuildArray;
begin
  SetLength    (result,count);
  build_struct.idx := 0;
  build_struct.arr := result;
  ForAllItems  (@_BuildArrayFromNodes,@build_struct);
end;

procedure TGFOS_RBTree.SignalTreeChange;
begin
  FTreeChanged := true;
end;

function TGFOS_RBTree.QueryTreeChange: Boolean;
begin
  result       := FTreeChanged;
  FTreeChanged := false;
end;



function TGFOS_RBTree.FindNext(var key: _TKey;var Store:_TStore):Boolean;
var n:PFOS_RB_NodeG;
    a_index:Integer;
begin
 result:=false;
 if (_rbmode=rbDynamic) or (_rbMode=rbHybrid) then begin
  n:=_FindNextPrev(key,true);
  if assigned(n)then begin
    result  :=true;
    Store   :=n^.val;
    key     :=n^.k;
  end else begin
   case _rbmode of
    rbDynamic: begin
     result  :=false;
     Fillchar(store,sizeof(store),#0);
    end;
    rbHybrid: begin
     raise Exception.Create('NOT IMPLEMENTED');
    end;
   end;
  end;
 end else begin
  a_index:=_AFindNextPrev(key,true);
  if a_index>=0 then begin
   key:=nodearray[a_index].k;
   Store:=nodearray[a_index].val;
   result:=true;
  end else begin
   result  :=false;
   Fillchar(store,sizeof(store),#0);
  end;
 end;
end;

function TGFOS_RBTree.FindPrev(var key: _TKey;var Store:_TStore):Boolean;
var n:PFOS_RB_NodeG;
    a_index:Integer;
begin
 result:=false;
 if (_rbmode=rbDynamic) or (_rbMode=rbHybrid) then begin
  n:=_FindNextPrev(key,false);
  if assigned(n)then begin
    result  :=true;
    Store   :=n^.val;
    key     :=n^.k;
  end else begin
   case _rbmode of
    rbDynamic: begin
     result  :=false;
     Fillchar(store,sizeof(store),#0);
    end;
    rbHybrid: begin
     raise Exception.Create('NOT IMPLEMENTED');
    end;
   end;
  end;
 end else begin
  a_index:=_AFindNextPrev(key,false);
  if a_index>=0 then begin
   key:=nodearray[a_index].k;
   Store:=nodearray[a_index].val;
   result:=true;
  end else begin
   result  :=false;
   Fillchar(store,sizeof(store),#0);
  end;
 end;
end;

function TGFOS_RBTree.FindNextOrEQ(var key: _TKey; var Store: _TStore): Boolean;
var n:PFOS_RB_NodeG;
    a_index:Integer;
begin
 result := false;
 if (_rbmode=rbDynamic) or (_rbMode=rbHybrid) then begin
  n:=_FindNextPrev(key,true,true);
  if assigned(n)then begin
    result  :=true;
    Store   :=n^.val;
    key     :=n^.k;
  end else begin
   case _rbmode of
    rbDynamic: begin
     result  :=false;
     Fillchar(store,sizeof(store),#0);
    end;
    rbHybrid: begin
     raise Exception.Create('NOT IMPLEMENTED');
    end;
   end;
  end;
 end else begin
  a_index:=_AFindNextPrev(key,true,true);
  if a_index>=0 then begin
   key:=nodearray[a_index].k;
   Store:=nodearray[a_index].val;
   result:=true;
  end else begin
   result  :=false;
   Fillchar(store,sizeof(store),#0);
  end;
 end;
end;

function TGFOS_RBTree.FindPrevOrEQ(var key: _TKey; var Store: _TStore): Boolean;
var n:PFOS_RB_NodeG;
    a_index:Integer;
begin
 result := false;
 if (_rbmode=rbDynamic) or (_rbMode=rbHybrid) then begin
  n:=_FindNextPrev(key,false,true);
  if assigned(n)then begin
    result  :=true;
    Store   :=n^.val;
    key     :=n^.k;
  end else begin
   case _rbmode of
    rbDynamic: begin
     result  :=false;
     Fillchar(store,sizeof(store),#0);
    end;
    rbHybrid: begin
     raise Exception.Create('NOT IMPLEMENTED');
    end;
   end;
  end;
 end else begin
  a_index:=_AFindNextPrev(key,false,true);
  if a_index>=0 then begin
   key:=nodearray[a_index].k;
   Store:=nodearray[a_index].val;
   result:=true;
  end else begin
   result  :=false;
   Fillchar(store,sizeof(store),#0);
  end;
 end;
end;

procedure TGFOS_RBTree.Clear(const DoFreeItem:TGFOS_RB_OnItem);
begin
  FTreeChanged := true;
  if (root <> nil) then _fast_erase(root,DoFreeItem);
  _Zero;
end;

procedure TGFOS_RBTree.ClearN(const DoFreeItem: TGFOS_RB_OnItemN);
begin
  FTreeChanged := true;
  if (root <> nil) then _fast_eraseN(root,DoFreeItem);
  _Zero;
end;

procedure TGFOS_RBTree.ClearItems(const DoFreeItem: TGFOS_RB_OnItemNested);
begin
  FTreeChanged := true;
  if (root <> nil) then _fast_eraseIt(root,DoFreeItem);
  _Zero;
end;

function TGFOS_RBTree.Find(const key: _TKey;out store:_TStore): Boolean;
var nd  :PFOS_RB_NodeG;
    a_index:Integer;
begin
  if (_rbmode=rbDynamic) or (_rbmode=rbHybrid) then begin
    nd:=_Find(key);
    if assigned(nd) then begin
     store:=nd^.val;
     result:=true;
    end else begin
     case _rbmode of
      rbDynamic: begin
       Fillchar(store,sizeof(store),#0);
       result:=false;
      end;
      rbHybrid: begin
       raise Exception.create('NOT IMPLEMENTED');
      end;
     end;
    end;
  end else begin
    a_index:=_AFind(key);
    if a_index>=0 then begin
     store:=nodearray[a_index].val;
     result:=true;
    end else begin
     Fillchar(store,sizeof(store),#0);
     result:=false;
    end;
  end;
end;

function TGFOS_RBTree.FindEx(const key: _TKey; out store: _PStore): Boolean;
var nd:PFOS_RB_NodeG;
begin
  nd := _Find(key);
  result := assigned(nd);
  if result then begin
    store := @nd^.val;
  end;
end;

function TGFOS_RBTree.Exists(const key: _TKey): Boolean;
var val:_TStore;
begin
  val:=emptyval;
  result:=Find(key,val);
end;


function TGFOS_RBTree.Update(const key: _TKey; const Store: _TStore): Boolean;
var nd:PFOS_RB_NodeG;
    a_index:Integer;
begin
 nd:=_Find(key);
 if assigned(nd) then begin
   nd^.val:=Store;
   result:=true;
   FTreeChanged := true;
 end else begin
  case _rbmode of
   rbDynamic: begin
     result:=false;
   end;
   rbStatic: begin
     a_index:=_AFind(key);
     if a_index>=0 then begin
       nodearray[a_index].val:=Store;
       FTreeChanged := true;
     end else begin
       result:=false;
     end;
   end;
   rbHybrid: begin
    raise Exception.create('NOT IMPLEMENTED');
   end;
  end;
 end;
end;

procedure TGFOS_RBTree.RotLeft(var x: PFOS_RB_NodeG);
var
  y: PFOS_RB_NodeG;
begin
  y := x^.right;
  x^.right := y^.left;
  if (y^.left <> nil) then begin
    y^.left^.parent := x;
  end;
  y^.parent := x^.parent;
  if (x = root) then begin
    root := y;
  end else if (x = x^.parent^.left) then begin
    x^.parent^.left := y;
  end else begin
    x^.parent^.right := y;
  end;
  y^.left := x;
  x^.parent := y;
end;

procedure TGFOS_RBTree.RotRight(var x: PFOS_RB_NodeG);
var
  y: PFOS_RB_NodeG;
begin
  y := x^.left;
  x^.left := y^.right;
  if (y^.right <> nil) then begin
    y^.right^.parent := x;
  end;
  y^.parent := x^.parent;
  if (x = root) then begin
    root := y;
  end else if (x = x^.parent^.right) then begin
    x^.parent^.right := y;
  end else begin
    x^.parent^.left := y;
  end;
  y^.right := x;
  x^.parent := y;
end;

function TGFOS_RBTree.Min(var x: PFOS_RB_NodeG): PFOS_RB_NodeG;
begin
  Result := x;
  while (Result^.left <> nil) do Result := Result^.left;
end;

function TGFOS_RBTree.Max(var x: PFOS_RB_NodeG): PFOS_RB_NodeG;
begin
  Result := x;
  while (Result^.right <> nil) do Result := Result^.right;
end;

function TGFOS_RBTree.AddCheckEx(const key:_TKey;var   Store:_TStore;const replace:boolean; var KeyPointer:_Pkey;var StorePointer:_PStore):Boolean;
begin
  result := _InternalAddCheck(key,store,replace,KeyPointer,StorePointer);
end;

function TGFOS_RBTree._InternalAddCheck (const key:_TKey;var   Store:_TStore;const replace:boolean; out KeyPointer:_Pkey;out StorePointer:_PStore):Boolean;
var x, y, z, zpp: PFOS_RB_NodeG;
    cmp: Integer;
    oldval:_TStore;
begin
  case _rbmode of
   rbDynamic: begin
    z := New(PFOS_RB_NodeG);
    { Initialize fields in new node z }
    z^.k := key;
    z^.left := nil;
    z^.right := nil;
    z^.col := R;
    z^.val:=Store;
    KeyPointer  := @z^.k;
    StorePointer := @z^.val;
    { Maintain leftmost and rightmost nodes }
    if (leftmost = nil) then begin
      leftmost := z;
    end else
    if (InternalCompare(key, leftmost^.k) < 0) then begin
      leftmost := z;
    end;
    if (rightmost = nil) then begin
      rightmost := z;
    end else
    if (InternalCompare(key, rightmost^.k) > 0) then begin
      rightmost := z;
    end;
    { Insert node z }
    y := nil;
    x := root;
    while (x <> nil) do begin
      y := x;
      cmp := InternalCompare(key, x^.k);
      if (cmp < 0) then begin
        x := x^.left;
      end else if (cmp > 0) then begin
        x := x^.right;
      end else begin
        { val already exists in tree. }
        KeyPointer  := @x^.k;
        StorePointer := @x^.val;
        Dispose(z);
        oldval:=x^.val;
        if replace then begin
         x^.val:=Store;
        end;
        Store:=oldval;
        result:=false; //Return old Value
        exit;
      end;
    end;
    inc(_Count);
    FTreeChanged := true;
    z^.parent := y;
    if (y = nil) then begin
      root := z;
    end else if (InternalCompare(key, y^.k) < 0) then begin
      y^.left := z;
    end else begin
      y^.right := z;
    end;
    store:=z^.val;
    result:=true;
    { Rebalance tree }
    repeat
      if (z=root) then break;
      if not (z^.parent^.col = R) then break;
      zpp := z^.parent^.parent;
      if (z^.parent = zpp^.left) then begin
        y := zpp^.right;
        if ((y <> nil) and (y^.col = R)) then begin
          z^.parent^.col := B;
          y^.col := B;
          zpp^.col := R;
          z := zpp;
        end else begin
          if (z = z^.parent^.right) then begin
            z := z^.parent;
            RotLeft(z);
          end;
          z^.parent^.col := B;
          zpp^.col := R;
          RotRight(zpp);
        end;
      end else begin
        y := zpp^.left;
        if ((y <> nil) and (y^.col = R)) then begin
          z^.parent^.col := B;
          y^.col := B;
          zpp^.col := R;
          z := zpp;
        end else begin
          if (z = z^.parent^.left) then begin
            z := z^.parent;
            RotRight(z);
          end;
          z^.parent^.col := B;
          zpp^.col := R;
          RotLeft(zpp);
        end;
      end;
    until false;
    root^.col := B;
   end;
   rbStatic: begin
    raise Exception.Create('ADD NOT ALLOWED IN STATIC TREE');
   end;
   rbHybrid: begin
    raise Exception.Create('NOT IMPLEMENTED');
   end;
  end;
end;

function TGFOS_RBTree.InternalCompare(const Item1, Item2: _TKey): integer;
begin
  if assigned(_CompareSi) then begin
    result := _CompareSi(item1,item2);
  end else begin
    result := _CompareEx(item1,item2,FDataPointer);
  end;
end;

procedure TGFOS_RBTree._Zero;
begin
  root := nil;
  leftmost := nil;
  rightmost := nil;
  _count:=0;
  case _rbmode of
    rbStatic: begin
      SetLength(nodearray,0);
    end;
    rbHybrid: begin
      raise Exception.create('NOT IMPLEMENTED');
    end;
  end;
  _rbmode:=rbDynamic;
end;

procedure TGFOS_RBTree._BuildArrayFromNodes(const Key: _TKey; const Item: _TStore; const P: Pointer);
var build_struct : PFOS_BuildArray;
begin
  with PFOS_BuildArray(p)^ do begin
    arr[idx] := Item;
    inc(idx);
  end;
end;


function TGFOS_RBTree.AddCheck(const key: _TKey;var Store:_TStore;const replace:boolean=false):Boolean;
var keyp : _PKey;
   nodep : _PStore;
begin
  result := _InternalAddCheck(key,store,replace,keyp,nodep);
end;

function TGFOS_RBTree.Add(const key: _TKey; const Store: _TStore): Boolean;
var temp:_TStore;
begin
 temp:=Store;
 result:=AddCheck(key,temp);
end;


procedure TGFOS_RBTree._Delete(z: PFOS_RB_NodeG);
var  w, x, y, x_parent: PFOS_RB_NodeG;
     tmpcol: TRB_NodeColor;

begin
  y := z;
  x := nil;
  x_parent := nil;

  if (y^.left = nil) then begin    { z has at most one non-null child. y = z. }
    x := y^.right;     { x might be null. }
  end else begin
    if (y^.right = nil) then begin { z has exactly one non-null child. y = z. }
      x := y^.left;    { x is not null. }
    end else begin
      { z has two non-null children.  Set y to }
      y := y^.right;   {   z's successor.  x might be null. }
      while (y^.left <> nil) do begin
        y := y^.left;
      end;
      x := y^.right;
    end;
  end;

  if (y <> z) then begin
    { "copy y's sattelite data into z" }
    { relink y in place of z.  y is z's successor }
    z^.left^.parent := y;
    y^.left := z^.left;
    if (y <> z^.right) then begin
      x_parent := y^.parent;
      if (x <> nil) then begin
        x^.parent := y^.parent;
      end;
      y^.parent^.left := x;   { y must be a child of left }
      y^.right := z^.right;
      z^.right^.parent := y;
    end else begin
      x_parent := y;
    end;
    if (root = z) then begin
      root := y;
    end else if (z^.parent^.left = z) then begin
      z^.parent^.left := y;
    end else begin
      z^.parent^.right := y;
    end;
    y^.parent := z^.parent;
    tmpcol := y^.col;
    y^.col := z^.col;
    z^.col := tmpcol;
    y := z;  { y now points to node to be actually deleted }
  end else begin                        { y = z }
    x_parent := y^.parent;
    if (x <> nil)  then begin
      x^.parent := y^.parent;
    end;
    if (root = z) then begin
      root := x;
    end else begin
      if (z^.parent^.left = z) then begin
        z^.parent^.left := x;
      end else begin
        z^.parent^.right := x;
      end;
    end;
	  if (leftmost = z) then begin
	    if (z^.right = nil) then begin      { z^.left must be null also }
	      leftmost := z^.parent;
	    end else begin
	      leftmost := Min(x);
      end;
    end;
	  if (rightmost = z) then begin
	    if (z^.left = nil) then begin       { z^.right must be null also }
	      rightmost := z^.parent;
	    end else begin                     { x == z^.left }
	      rightmost := Max(x);
      end;
    end;
  end;

  { Rebalance tree }
  if (y^.col = B)  then begin
    repeat
      if (x=root) then break;
      if  x<>nil  then begin
       if (x^.col<>B) then break;
      end;
      if (x = x_parent^.left)  then begin
          w := x_parent^.right;
          if (w^.col = R)  then begin
            w^.col := B;
            x_parent^.col := R;
            RotLeft(x_parent);
            w := x_parent^.right;
          end;
          if (((w^.left = nil) or
               (w^.left^.col = B)) and
              ((w^.right = nil) or
               (w^.right^.col = B)))  then begin
            w^.col := R;
            x := x_parent;
            x_parent := x_parent^.parent;
          end else begin
            if ((w^.right = nil) or (w^.right^.col = B)) then begin
              w^.left^.col := B;
              w^.col := R;
              RotRight(w);
              w := x_parent^.right;
            end;
            w^.col := x_parent^.col;
            x_parent^.col := B;
            if (w^.right <> nil)  then begin
              w^.right^.col := B;
            end;
            RotLeft(x_parent);
            x := root;
         end
      end else begin
        w := x_parent^.left;
        if (w^.col = R)  then begin
          w^.col := B;
          x_parent^.col := R;
          RotRight(x_parent);
          w := x_parent^.left;
        end;
        if (((w^.right = nil) or
             (w^.right^.col = B)) and
            ((w^.left = nil) or
             (w^.left^.col = B)))  then begin
          w^.col := R;
          x := x_parent;
          x_parent := x_parent^.parent;
        end else begin
          if (w^.left = nil) or (w^.left^.col = B) then begin
            w^.right^.col := B;
            w^.col := R;
            RotLeft(w);
            w := x_parent^.left;
          end;
          w^.col := x_parent^.col;
          x_parent^.col := B;
          if (w^.left <> nil) then begin
            w^.left^.col := B;
          end;
          RotRight(x_parent);
          x := root;
        end;
      end;
    until  false;
    if (x <> nil) then begin
      x^.col := B;
    end;
  end;
  dispose(y);
end;

function TGFOS_RBTree._Find(key: _TKey): PFOS_RB_NodeG;
var cmp: integer;
    node: PFOS_RB_NodeG;
begin
  result:=nil;
  node := root;
  while (node <> nil) do begin
    cmp := InternalCompare(node^.k, key);
    if cmp < 0 then begin
      node := node^.right;
    end else if cmp > 0 then begin
      node := node^.left;
    end else begin
      result:=node;
      break;
    end;
  end;
end;

function TGFOS_RBTree._FindNextPrev(key: _TKey;const next:boolean;const equalsearch:boolean=false): PFOS_RB_NodeG;
var cmp: integer;
    node: PFOS_RB_NodeG;
begin
  result:=nil;
  node := root;
  while true do begin
    if node=nil then exit;
    cmp := InternalCompare(node^.k, key);
    if cmp < 0 then begin
      if node^.right<>nil then begin
       node := node^.right;
      end else begin
       result:=node;
       if not next then exit;
       break;
      end;
    end else if cmp > 0 then begin
      if node^.left<>nil then begin
       node := node^.left;
      end else begin
       result:=node;
       if next then exit;
       break;
      end;
    end else begin
      if (node^.left=nil) and (node^.right=nil) and (node^.parent=nil) and (equalsearch=false) then begin
       result:=nil;
       exit;
      end else begin
       result:=node;
       break;
      end;
    end;
  end;
  if equalsearch and (InternalCompare(key,node^.k)=0) then exit;
  if next then begin
   if result=rightmost then begin
    result:=nil;
   end else begin
    RBInc(result);
   end;
  end else begin
   if result=leftmost then begin
    result:=nil;
   end else begin
    RBDec(result);
   end;
  end;
end;


procedure TGFOS_RBTree._fast_erase(x: PFOS_RB_NodeG;const DoFreeItem:TGFOS_RB_OnItem);
begin
   if (x^.left <> nil) then  _fast_erase(x^.left,DoFreeItem);
   if (x^.right <> nil) then _fast_erase(x^.right,DoFreeItem);
   if assigned(DoFreeItem) then DoFreeItem(x^.val);
   dispose(x);
end;

procedure TGFOS_RBTree._fast_eraseN(x: PFOS_RB_NodeG; const DoFreeItem: TGFOS_RB_OnItemN);
begin
   if (x^.left <> nil) then  _fast_eraseN(x^.left,DoFreeItem);
   if (x^.right <> nil) then _fast_eraseN(x^.right,DoFreeItem);
   if assigned(DoFreeItem) then DoFreeItem(x^.val);
   dispose(x);
end;

procedure TGFOS_RBTree._fast_eraseIt(x: PFOS_RB_NodeG; const DoFreeItem: TGFOS_RB_OnItemNested);
begin
   if (x^.left <> nil) then  _fast_eraseIT(x^.left,DoFreeItem);
   if (x^.right <> nil) then _fast_eraseIT(x^.right,DoFreeItem);
   if assigned(DoFreeItem) then DoFreeItem(x^.val);
   dispose(x);
end;

procedure TGFOS_RBTree._LocalProcedure_Hack(Proc, FrameP, p1, p2: Pointer);inline;
type PL = procedure(_EBP,p1,p2: Pointer);
begin
  PL(Proc)(FrameP,p1,p2);
end;

function TGFOS_RBTree._AFind(key: _TKey): Integer;
var cmp: integer;
    l,r,m:integer;
begin
 result:=-1;
 l:=0;
 r:=High(nodearray);
 while (r>=l) do begin
  m:=l+((r-l) div 2);
  cmp := InternalCompare(nodearray[m].k, key);
  if cmp < 0 then begin
   l:=m+1;
  end else if cmp > 0 then begin
   r:=m-1;
  end else begin
   result:=m;
   break;
  end;
 end;
end;

function TGFOS_RBTree._AFindNextPrev(key: _TKey; const next: boolean;const equalsearch: boolean): Integer;
var cmp:integer;
    l,r,m:integer;
begin
 result:=-1;
 l:=0;
 r:=High(nodearray);
 if r=0 then begin
  exit;
 end;
 while (r>=l) do begin
  m:=l+((r-l) div 2);
  cmp := InternalCompare(nodearray[m].k, key);
  if cmp < 0 then begin
   l:=m+1;
  end else if cmp > 0 then begin
   r:=m-1;
  end else begin
   break;
  end;
 end;
 if (equalsearch) and (cmp=0) then begin
   result:=m;
   exit;
 end;
 if next then begin
  if cmp>0 then begin
   result:=m;
  end else begin
   if m<high(nodearray) then begin
    inc(m);
    result:=m;
   end else begin
    result:=-1;
   end;
  end;
 end else begin
  if cmp<0 then begin
   result:=m;
  end else begin
   if m>low(nodearray) then begin
    dec(m);
    result:=m;
   end else begin
    result:=-1;
   end;
  end;
 end;
end;

procedure TGFOS_RBTree._SetLocalStaticArray(const Key :_TKey;const Item:_TStore;const P:Pointer);
var a_index:PLongword;
begin
  a_index:=p;
  nodearray[a_index^].k:=Key;
  nodearray[a_index^].val:=Item;
  inc(a_index^);
end;



procedure TGFOS_RBTree.RBInc(var x: PFOS_RB_NodeG);
var y: PFOS_RB_NodeG;
begin
  if (x^.right <> nil) then begin
    x := x^.right;
    while (x^.left <> nil) do begin
      x := x^.left;
    end;
  end else begin
    y := x^.parent;
    while (x = y^.right) do begin
      x := y;
      y := y^.parent;
    end;
    if (x^.right <> y) then
      x := y;
  end
end;


procedure TGFOS_RBTree.RBDec(var x: PFOS_RB_NodeG);
var  y: PFOS_RB_NodeG;
begin
  if (x^.left <> nil)  then begin
    y := x^.left;
    while (y^.right <> nil) do begin
      y := y^.right;
    end;
    x := y;
  end else begin
    y := x^.parent;
    while (x = y^.left) do begin
      x := y;
      y := y^.parent;
    end;
    x := y;
  end
end;

end.

