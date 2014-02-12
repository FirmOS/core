unit fos_art_tree;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
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
{$DEFINE ART_PURE_PASCAL}

//TODO
// Implement SSE Versions

interface

uses SysUtils;

const CFREA_max_compressed_prefix =  9;
      CFREA_slot_is_empty         = $30;
      CFREA_emptyMarkerQW         = $3030303030303030;
      CFREA_maxKeyLen             = 512;

type
   {$PACKENUM 1}
   TFRE_ART_NodeType=(artNodeType4,artNodeType16,artNodeType48,artNodeType256,artNodeTypeLeaf);

   PNativeUint = ^NativeUint;
   PNativeInt  = ^NativeInt;
   TExConvert = record
     case byte of
      0 : (Ptr     : PNativeUint);
      1 : (PtrUInt : PtrUInt);
   end;
   PExConvert = ^TExConvert;

   PFRE_ART_Node     = ^TFRE_ART_Node;
   PFRE_ART_LeafNode = ^TFRE_ART_LeafNode;
   PPFRE_ART_Node    = ^PFRE_ART_Node;
   PFRE_ART_Node4    = ^TFRE_ART_Node4;
   PFRE_ART_Node16   = ^TFRE_ART_Node16;
   PFRE_ART_Node48   = ^TFRE_ART_Node48;
   PFRE_ART_Node256  = ^TFRE_ART_Node256;

   { TFRE_ART_Node }

   TFRE_ART_Node = object
      count        : Word; // number of non-null children
      prefixLength : SmallInt; // length of the compressed path (prefix)
      prefix       : Array [0..CFREA_max_compressed_prefix] of Byte;
      typ          : TFRE_ART_NodeType;
      procedure InitNode(const node_typ : TFRE_ART_NodeType);
   end;

   { TFRE_ART_LeafNode }
   TFRE_ART_LeafNode = packed object(TFRE_ART_Node)
      stored_key     : PtrUInt;
      stored_key_len : NativeInt;
      stored_value   : NativeUint;
      class function NewAndInitLeaf (value : NativeUint; key:PByte ; key_len:Nativeint): PFRE_ART_LeafNode; static; // negative keylens mean that the pointer to the key is stored, must not be volatile or moving!
      function       GetStoredKey     : PByte; inline;
      function       GetStoredKeyLen  : NativeInt; inline;
      function       GetStoredValue   : PNativeUint; inline;
      procedure      Finalize;
   end;

   { TFRE_ART_Node4 }
   TFRE_ART_Node4 =  object(TFRE_ART_Node)
      key         : array [0..3] of Byte;
      child       : array [0..3] of PFRE_ART_Node;
      procedure       InitNode4;
      function        ChildIndex(const searchchild : PFRE_ART_Node) : NativeInt;
      class function  NewAndInitNode4 : PFRE_ART_Node4; static;
   end;

   { TFRE_ART_Node16 }
   TFRE_ART_Node16 =  object(TFRE_ART_Node)
      key         : array [0..15] of Byte;
      child       : array [0..15] of PFRE_ART_Node;
      procedure       InitNode16;
      function        ChildIndex(const searchchild : PFRE_ART_Node) : NativeInt;
      class function  NewAndInitNode16 : PFRE_ART_Node16; static;
   end;

   { TFRE_ART_Node48 }
   TFRE_ART_Node48 =  object(TFRE_ART_Node)
      childIndex  : array [0..255] of Byte;
      child       : array [0..47] of PFRE_ART_Node;
      procedure       InitNode48;
      class function  NewAndInitNode48 : PFRE_ART_Node48; static;
   end;

   { TFRE_ART_Node256 }
   TFRE_ART_Node256 =  object(TFRE_ART_Node)
      child        : array [0..255] of PFRE_ART_Node;
      procedure       InitNode256;
      class function  NewAndInitNode256 : PFRE_ART_Node256; static;
   end;

var
   GFRE_ART_nullNode:PFRE_ART_Node=NIL;

type

     { TFRE_ART_TREE }
     TFRE_ART_TREE=class
     private type
       TFRE_ART_NODE_DUMP_PROC              = procedure (const node : PFRE_ART_Node ; const level : NativeUint) is nested;
       TFRE_ART_NODE_PROC                   = procedure (const node : PFRE_ART_Node) is nested;
     public type
       TFRE_ART_NodeValueProc               = procedure (var value : NativeUint) is nested;
       TFRE_ART_NodeValueBreakProc          = procedure (var value : NativeUint ; var break : boolean) is nested;
       TFRE_ART_NodeCallback                = procedure (var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint) is nested;
       TFRE_ART_NodeBreakCallback           = procedure (var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint ; var break : boolean) is nested;
       TFRE_ART_NodeBreakCallbackCountDown  = procedure (var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint ; var break : boolean ; var downcounter,upcounter : NativeInt ; const abscntr : NativeInt) is nested;
     private var
       FArtTree    : PFRE_ART_Node;
       FValueCount : NativeUint;
       procedure   FinalizeLeaf            (node : PFRE_ART_Node);
       {$IFNDEF ART_PURE_PASCAL}
       function    flipSign                (keyByte : Byte) : Byte; inline; // for SSE
       {$ENDIF}
       function    findChild               (n    : PFRE_ART_Node; keyByte : Byte):PPFRE_ART_Node;
       function    findChildOrRightSibling (n    : PFRE_ART_Node; keyByte : Byte):PPFRE_ART_Node;
       function    minimum_inner           (node : PFRE_ART_Node; keyByte : Byte):PFRE_ART_Node; // Find the inner node  with smallest key, greater equal then keybyte
       function    minimum                 (node : PFRE_ART_Node):PFRE_ART_Node; // Find the leaf with smallest key
       function    maximum                 (node : PFRE_ART_Node):PFRE_ART_Node; // Find the leaf with largest key
       procedure   InsertChildIntoNode4    (node : PFRE_ART_Node4   ; nodeRef : PPFRE_ART_Node ; keyByte   : Byte ; child : PFRE_ART_Node);
       procedure   InsertChildIntoNode16   (node : PFRE_ART_Node16  ; nodeRef : PPFRE_ART_Node ; keyByte   : Byte ; child : PFRE_ART_Node);
       procedure   InsertChildIntoNode48   (node : PFRE_ART_Node48  ; nodeRef : PPFRE_ART_Node ; keyByte   : Byte ; child : PFRE_ART_Node);
       procedure   InsertchildIntoNode256  (node : PFRE_ART_Node256 ;                            keyByte   : Byte ; child : PFRE_ART_Node);
       function    erase                   (node : PFRE_ART_Node    ; nodeRef : PPFRE_ART_Node ; key       : PByte ; keyLength,depth : NativeInt;var value: PtrUInt):boolean;

       function    artmin                  (a,b : Nativeint):NativeUint; inline;
       procedure   CopyPrefix              (src,dst : PFRE_ART_Node); inline; // Helper function that copies the prefix from the source to the destination node
       function    LeafMatches             (leaf: PFRE_ART_Node ; key : PByte; keyLength,depth : Nativeint):boolean; inline ;  // Check if the key of the leaf is equal to the searched key
       function    PrefixMismatchAtByte    (node: PFRE_ART_Node ; key : PByte; keyLength,depth : Nativeint):Nativeint; // inline ;// Compare the key with the prefix of the node, return the number matching bytes
       function    InsertInto              (node: PFRE_ART_Node; nodeRef: PPFRE_ART_Node; key: PByte; key_len, depth: NativeInt; var value: NativeUint): boolean;
       function    LookupKey               (node: PFRE_ART_Node ; key : PByte;keyLength,depth:NativeInt):PFRE_ART_Node; // Find the node with a matching key
       function    LookupKeyPessimistic    (node: PFRE_ART_Node ; key : PByte;keyLength,depth:NativeInt):PFRE_ART_Node; // Find the node with a matching key
       function    LookupKeyOrPrefix       (node: PFRE_ART_Node ; key : PByte;keyLength,depth:NativeInt):PFRE_ART_Node;

       function    IsALeafNode             (node: PFRE_ART_Node):boolean; inline;

       procedure   ForAllLvlLeafs          (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_DUMP_PROC;const level:Nativeint);
       procedure   ForAllLvlLeafsAndInner  (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_DUMP_PROC;const level:Nativeint); // Head Recursion

       procedure   ForAllNodeLeafs         (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_PROC ; var halt : boolean);
       procedure   ForAllLeafsAndInner     (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_PROC);
       procedure   ForAllNodeLeafsRange    (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_PROC ; var halt : boolean ; lmismatch, hmismatch : boolean ; const lo_key,hi_key: PByte ; depth : Nativeint);
       procedure   ForAllNodeLeafsRangeReverse (node: PFRE_ART_Node; const node_proc : TFRE_ART_NODE_PROC ; var halt : boolean ; lmismatch, hmismatch : boolean ; const lo_key,hi_key: PByte ; depth : Nativeint);


       procedure   ForAllValue             (node: PFRE_ART_Node; const node_proc : TFRE_ART_NodeValueProc);
       procedure   ForAllValueReverse      (node: PFRE_ART_Node; const node_proc : TFRE_ART_NodeValueProc);
       procedure   ForAllValueBreak        (node: PFRE_ART_Node; const node_proc : TFRE_ART_NodeValueBreakProc;var break:boolean);
       procedure   _InnerCheck             ;
     public
       destructor  Destroy                 ; override;
       procedure   Clear                   ;
       procedure   DumpTree                (const key_as_string : boolean = false ; const with_inner_nodes:boolean=false);
       function    InsertBinaryKey         (const key: PByte  ; const keylen : Nativeint ; const value: PtrUInt):boolean;
       function    InsertBinaryKeyOrFetch  (const key: PByte  ; const keylen : Nativeint ; var   value: PtrUInt):boolean;
       function    ExistsBinaryKey         (const key: PByte  ; const keylen : Nativeint ; var   value: PtrUInt):boolean;
       function    RemoveBinaryKey         (const key: PByte  ; const keylen : Nativeint ; var   value: PtrUInt):boolean;
       function    InsertStringKey         (const key: string ; const value  : PtrUInt):boolean;
       function    InsertStringKeyOrFetch  (const key: string ; var   value  : PtrUInt):boolean;
       function    ExistsStringKey         (const key: String ; var   value: PtrUInt):boolean;
       function    RemoveStringKey         (const key: String ; var   value: PtrUInt):boolean;

       function    InsertUInt64Key         (const key: Uint64 ; var value : PtrUInt):boolean;

       procedure   LinearScan              (const nested_node_proc : TFRE_ART_NodeValueProc;const desc : boolean=false);
       function    LinearScanBreak         (const nested_node_proc: TFRE_ART_NodeValueBreakProc; var break: boolean): Boolean;
       procedure   LinearScanKeyVals       (const nested_node_proc : TFRE_ART_NodeCallback);
       function    FirstKeyVal             (const callback : TFRE_ART_NodeCallback):boolean;
       function    LastKeyVal              (const callback : TFRE_ART_NodeCallback):boolean;

       function    FirstKeyValString       (var key_string: String ; var out_val : PtrUInt) : Boolean;
       function    LastKeyValString        (var key_string: String ; var out_val : PtrUInt) : Boolean;

       function    PrefixScan              (const prefix_key : PByte ; key_len : NativeUint ; const nested_node_proc : TFRE_ART_NodeBreakCallback):boolean;

       function    RangeScan               (const lo_key,hi_key: PByte; const lo_keylen,hi_keylen : NativeUint ; const nested_node_proc : TFRE_ART_NodeBreakCallbackCountDown ; const max_count : NativeInt=0 ; skip : NativeInt=0 ; const asc : boolean=true):boolean;
       function    GetValueCount           : NativeUint;
     end;

procedure  raw_test;

implementation

function DumpBinary(p: pointer; const len: cardinal): string;
var i:integer;
begin
   result := '';
   for i:=1 to len do
     begin
       result:=result+format('%2.2x ',[byte(pbyte(p)^)]);
       inc(p);
     end;
end;

var buf:array[0..100] of byte;

procedure  raw_test;
var i,j     : integer;
    key     : string;
    tree    : TFRE_ART_TREE;
    tree2   : TFRE_ART_TREE;
    val     : PtrUInt;
    val2    : PtrUInt;
    val3    : PtrUInt;
    cntval  : NativeUint;
    testkey : string[10];
    inserts : array [0..12] of string = ('a','aa','b','aaa','aaaa','bar','baz','bazaaar','bazaaaroni','f','foolrunner','TFRE_DB_RESOURCE_CONTAINER','TFRE_DB_RESOURCE');
    test2   : array [0..17] of string =
                                        (
                                        'TFRE_DB_Object',
                                        'TFRE_DB_TEXT',
                                        'TFRE_DB_NAMED_OBJECT',
                                        'TFRE_DB_USER',
                                        'TFRE_DB_COMMAND',
                                        'TFRE_DB_SchemeObject',
                                        'TFRE_DB_FieldSchemeDefinition',
                                        'TFRE_DB_COLLECTION',
                                        'TFRE_DB_DERIVED_COLLECTION',
                                        'TFRE_DB_SCHEME_COLLECTION',
                                        'TFRE_DB_WORKFLOW',
                                        'TFRE_DB_WORKFLOW_STEP',
                                        'TFRE_DB_APPDATA',
                                        'TFRE_DB_RIGHT',
                                        'TFRE_DB_GROUP',
                                        'TFRE_DB_ROLE',
                                        'TFRE_DB_RESOURCE_CONTAINER',
                                        'TFRE_DB_RESOURCE');


    procedure Dump;
    begin
      writeln('---');
      tree.DumpTree(False);
      writeln('---');
    end;



  procedure RealTestCase;
  var i,len  : NativeInt;
           k : NativeUint;
  begin
    for i:=0 to high(test2) do
      begin
        len := length(test2[i]);
        Move(test2[i][1],buf,len);
        if not tree.InsertBinaryKey(@buf[0],len,i) then
          abort;
        if not tree.ExistsBinaryKey(@buf[0],len,k) then
          begin
            k:=0;
            abort;
          end;
      end;
    for i:=0 to high(test2) do
      if not tree.ExistsBinaryKey(@test2[i][1],Length(test2[i]),k) then
        begin
          k:=0;
          abort;
        end;
  end;

  procedure PrefixTest;
    procedure DumpPrefixScan(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint ; var halt:boolean);
    var s:String;
    begin
      SetLength(s,KeyLen);
      move(key^,s[1],KeyLen);
      writeln('KEY : ',s,' Val ',value);
    end;
  begin
     tree.InsertStringKey('aaaaaaa',3);
     tree.InsertStringKey('aaaaaa',4);
     tree.InsertStringKey('aaaaa',5);
     tree.InsertStringKey('aaaa',6);
     tree.InsertStringKey('aaa',7);
     tree.InsertStringKey('aa',8);
     tree.InsertStringKey('a',9);
     tree.InsertStringKey('ab',10);
     tree.InsertStringKey('aba',10);
     tree.InsertStringKey('aaacaaaa',14);
     tree.InsertStringKey('abb',10);
     //tree.InsertStringKey('aac',10);
     tree.InsertStringKey('aaaaaaaaa',1);
     tree.InsertStringKey('ac',10);
     tree.InsertStringKey('ad',10);
     tree.InsertStringKey('aaaaaaaa',2);
     tree.InsertStringKey('ae',10);
     tree.InsertStringKey('aab',11);
     tree.InsertStringKey('aaab',12);
     tree.InsertStringKey('baabaaa',13);
     tree.InsertStringKey('baaaaaa',3);
     tree.InsertStringKey('baaaaa',4);
     tree.InsertStringKey('baaaa',5);
     tree.InsertStringKey('baaa',6);
     tree.InsertStringKey('baa',7);
     tree.InsertStringKey('ba',8);
     tree.InsertStringKey('b',9);
     tree.InsertStringKey('bb',10);
     tree.InsertStringKey('bba',10);
     tree.InsertStringKey('baacaaaa',14);
     tree.InsertStringKey('bbb',10);
     tree.InsertStringKey('baaaaaaaa',1);
     tree.InsertStringKey('bc',10);
     tree.InsertStringKey('bd',10);
     tree.InsertStringKey('baaaaaaa',2);
     tree.InsertStringKey('be',10);
     tree.InsertStringKey('bab',11);
     tree.InsertStringKey('baab',12);
     tree.InsertStringKey('baabaaa',13);
     testkey:='bb';
     tree.PrefixScan(@testkey[1],Length(testkey),@DumpPrefixScan);
  end;

  procedure RangeTest;
  var i    : NativeInt;
      key  : string;
      keyh : string;
      val  : integer;

    procedure NodeBreak(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint ; var break : boolean ; var dcounter,upcounter : NativeInt ; const abscntr : NativeInt);
    begin
      writeln('Val ',value,' ',Copy(Pchar(key),1,KeyLen));
    end;

  begin
   for i:= 1 to 50000 do begin
     val := 100+(i);
     //val := 100+(i);
     key := Format('%4.4d',[val]);
     //writeln(key);
     tree.InsertStringKey(key,val);
   end;
   //tree.DumpTree(true,false);
   //halt;

   key  := '0397';
   keyh := '0411';
   tree.RangeScan(@key[1],@keyh[1],length(key),Length(keyh),@NodeBreak,0);
   //tree.RangeScan(nil,@keyh[1],0,Length(keyh),@NodeBreak,0);
   //tree.RangeScan(@key[1],nil,length(key),Length(keyh),@NodeBreak,0);
   //halt;
   //for i:= 0 to 1000 do
   //  begin
   //    key := Format('%4.4d',[i]);
   //    write(key,' :');
   //    tree.RangeScan(@key[1],@keyh[1],length(key),Length(keyh),nil,1);
   //    writeln;
   //  end;
   //tree.DumpTree(true,false);
  end;

begin
  cntval:=1000;
  val  := 0;
  val2 := 0;
  tree := TFRE_ART_TREE.Create;
  RealTestCase;
  PrefixTest;
  RangeTest;
  //halt;
  tree.Clear;
  for i:=0 to high(inserts) do
    begin
      key := inserts[i];
      assert(tree.InsertStringKey(key,(i*2))=true);
      assert(tree.InsertStringKey(key,(i*2+1)-1)=false);
    end;
  tree.DumpTree(true,false);
  //tree.
  //halt;
  assert(tree.RemoveStringKey('a',val)=true);
  assert(tree.RemoveStringKey('aaaa',val)=true);
  assert(tree.RemoveStringKey('aa',val)=true);
  assert(tree.RemoveStringKey('b',val)=true);
  assert(tree.RemoveStringKey('baz',val)=true);
  assert(tree.RemoveStringKey('f',val)=true);
  //tree.DumpTree(true,false);
  //tree.FirstKeyVal(@NodeCallback);
  //tree.LastKeyVal(@NodeCallback);
  //tree.DumpTree(true,false);
  tree.Destroy;
  tree := TFRE_ART_TREE.Create;
  for i:=0 to high(inserts) do
    for j:=0 to 66 do
      begin
        key := inserts[i]+'--'+Char(j+64);
        assert(tree.InsertStringKey(key,cntval)=true);
        assert(tree.InsertStringKey(key,0)=false);
        tree._InnerCheck;
        inc(cntval);
        writeln('Insert ',cntval);
      end;
  //writeln('---');
  tree._InnerCheck;
  tree.DumpTree(true,true);
  //writeln('---');
  //writeln('Values : ',tree.GetValueCount);
  val := tree.GetValueCount;
  val3:=0;
  for i:=1 to val do
    begin
      tree._InnerCheck;
      assert(tree.LastKeyValString(key,val2)=true);
      assert(tree.RemoveStringKey(key,val3)=true);
      tree._InnerCheck;
      assert(val2=val3);
      //writeln(tree.GetValueCount);
    end;
  assert(tree.FirstKeyValString(key,val2)=false);
  assert(tree.GetValueCount=0);
  tree.destroy;

  tree := TFRE_ART_TREE.Create;
  for i:=0 to high(inserts) do begin
    key := inserts[i];
    assert(tree.InsertStringKey(key,(i*2))=true);
    assert(tree.InsertStringKey(key,(i*2)+1)=false);
  end;

  tree2 := TFRE_ART_TREE.Create;
  for i:=high(inserts) downto 0 do begin
    key := inserts[i];
    assert(tree2.InsertStringKey(key,(i*2))=true);
    assert(tree2.InsertStringKey(key,(i*2)+1)=false);
  end;
  for i:=high(inserts) downto 0 do begin
    key := inserts[i];
    assert(tree.ExistsStringKey(key,val)=true);
    assert(tree2.ExistsStringKey(key,val2)=true);
    assert(val=val2);
  end;
  tree.Destroy;
  tree2.Destroy;
end;

procedure TFRE_ART_TREE.ForAllLvlLeafs(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_DUMP_PROC; const level: Nativeint);
var i:NativeInt;
begin
  if node=nil then exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLvlLeafs(child[i],node_proc,level+1);
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLvlLeafs(child[i],node_proc,level+1);
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          ForAllLvlLeafs(child[childIndex[i]],node_proc,level+1);
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          ForAllLvlLeafs(PFRE_ART_Node256(node)^.child[i],node_proc,level+1);
    artNodeTypeLeaf:
        node_proc(node,level+1);
  end;
end;

procedure TFRE_ART_TREE.ForAllLvlLeafsAndInner(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_DUMP_PROC; const level: Nativeint);
var i:NativeInt;
begin
  if node=nil then exit;
  node_proc(node,level);
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLvlLeafsAndInner(child[i],node_proc,level+1);
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLvlLeafsAndInner(child[i],node_proc,level+1);
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          ForAllLvlLeafsAndInner(child[childIndex[i]],node_proc,level+1);
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          ForAllLvlLeafsAndInner(PFRE_ART_Node256(node)^.child[i],node_proc,level+1);
    artNodeTypeLeaf: ;
  end;
end;

procedure TFRE_ART_TREE.ForAllNodeLeafs(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_PROC; var halt: boolean);
var i:NativeInt;
begin
  if node=nil then
    exit;
  if halt then
    exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          ForAllNodeLeafs(child[i],node_proc,halt);
                          if halt then
                            exit;
                        end;
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          ForAllNodeLeafs(child[i],node_proc,halt);
                          if halt then
                            exit;
                        end;
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          begin
                            ForAllNodeLeafs(child[childIndex[i]],node_proc,halt);
                            if halt then
                              exit;
                          end;
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          begin
                            ForAllNodeLeafs(PFRE_ART_Node256(node)^.child[i],node_proc,halt);
                            if halt then
                              exit;
                          end;
    artNodeTypeLeaf:
        begin
          node_proc(node);
          if halt then
            exit;
        end;
  end;
end;


procedure TFRE_ART_TREE.ForAllLeafsAndInner(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_PROC);
var i   : NativeInt;
begin
  if node=nil then exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLeafsAndInner(child[i],node_proc);
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllLeafsAndInner(child[i],node_proc);
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          ForAllLeafsAndInner(child[childIndex[i]],node_proc);
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          ForAllLeafsAndInner(PFRE_ART_Node256(node)^.child[i],node_proc);
    artNodeTypeLeaf: ;
  end;
  node_proc(node);
end;

//l,hmismatch the depth-1 key is different so dont compare this low, high value as it must be lower/higher ....(radix compare)
procedure TFRE_ART_TREE.ForAllNodeLeafsRange(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_PROC; var halt: boolean ; lmismatch, hmismatch : boolean ; const lo_key, hi_key: PByte; depth: Nativeint);
var i:NativeInt;
begin
  if halt then
    exit;
  if (not lmismatch) and (PrefixMismatchAtByte(node,lo_key,CFREA_max_compressed_prefix,depth) <> node^.prefixLength) then
    exit
  else
    depth := depth + node^.prefixLength;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          if (lmismatch or (key[i]>=lo_key[depth])) and
                             (hmismatch or (key[i]<=hi_key[depth])) then
                               begin
                                 ForAllNodeLeafsRange(child[i],node_proc,halt,lmismatch or (key[i] <> lo_key[depth]),hmismatch or (key[i] <> hi_key[depth]),lo_key,hi_key,depth+1);
                               end;
                          if halt then
                            exit;
                        end;
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          if (lmismatch or (key[i]>=lo_key[depth])) and
                             (hmismatch or (key[i]<=hi_key[depth])) then
                               begin
                                 ForAllNodeLeafsRange(child[i],node_proc,halt,lmismatch or (key[i] <> lo_key[depth]),hmismatch or (key[i]<>hi_key[depth]),lo_key,hi_key,depth+1);
                               end;
                          if halt then
                            exit;
                        end;
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      begin
                        for i:=0 to 255 do
                          if childIndex[i]<>CFREA_slot_is_empty then
                            begin
                              if (lmismatch or (childIndex[i]>=lo_key[depth])) and
                                 (hmismatch or (childIndex[i]<=hi_key[depth])) then
                                   ForAllNodeLeafsRange(child[childIndex[i]],node_proc,halt,lmismatch or (childIndex[i]<>lo_key[depth]),hmismatch or (childIndex[i]<>hi_key[depth]),lo_key,hi_key,depth+1);
                              if halt then
                                exit;
                            end;
                      end;
    artNodeType256: with PFRE_ART_Node256(node)^ do
                        for i:=0 to 255 do
                          if assigned(child[i]) then
                            begin
                              if (lmismatch or (i>=lo_key[depth])) and
                                 (hmismatch or (i<=hi_key[depth])) then
                                   ForAllNodeLeafsRange(child[i],node_proc,halt,lmismatch or (i<>lo_key[depth]),hmismatch or (i<>hi_key[depth]),lo_key,hi_key,depth+1);
                              if halt then
                                exit;
                            end;
    artNodeTypeLeaf:
        node_proc(node);
  end;
end;

procedure TFRE_ART_TREE.ForAllNodeLeafsRangeReverse(node: PFRE_ART_Node; const node_proc: TFRE_ART_NODE_PROC; var halt: boolean; lmismatch, hmismatch: boolean; const lo_key, hi_key: PByte; depth: Nativeint);
var i:NativeInt;
begin
  if halt then
    exit;
  if (not lmismatch) and (PrefixMismatchAtByte(node,lo_key,CFREA_max_compressed_prefix,depth) <> node^.prefixLength) then
    exit
  else
    depth := depth + node^.prefixLength;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=node^.count-1 downto 0 do
                        begin
                          if (lmismatch or (key[i]>=lo_key[depth])) and
                             (hmismatch or (key[i]<=hi_key[depth])) then
                               begin
                                 ForAllNodeLeafsRangeReverse(child[i],node_proc,halt,lmismatch or (key[i] <> lo_key[depth]),hmismatch or (key[i] <> hi_key[depth]),lo_key,hi_key,depth+1);
                               end;
                          if halt then
                            exit;
                        end;
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i := node^.count-1 downto 0 do
                        begin
                          if (lmismatch or (key[i]>=lo_key[depth])) and
                             (hmismatch or (key[i]<=hi_key[depth])) then
                               begin
                                 ForAllNodeLeafsRangeReverse(child[i],node_proc,halt,lmismatch or (key[i] <> lo_key[depth]),hmismatch or (key[i]<>hi_key[depth]),lo_key,hi_key,depth+1);
                               end;
                          if halt then
                            exit;
                        end;
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      begin
                        for i:=255 downto 0 do
                          if childIndex[i]<>CFREA_slot_is_empty then
                            begin
                              if (lmismatch or (childIndex[i]>=lo_key[depth])) and
                                 (hmismatch or (childIndex[i]<=hi_key[depth])) then
                                   ForAllNodeLeafsRangeReverse(child[childIndex[i]],node_proc,halt,lmismatch or (childIndex[i]<>lo_key[depth]),hmismatch or (childIndex[i]<>hi_key[depth]),lo_key,hi_key,depth+1);
                              if halt then
                                exit;
                            end;
                      end;
    artNodeType256: with PFRE_ART_Node256(node)^ do
                        for i:=255 downto 0 do
                          if assigned(child[i]) then
                            begin
                              if (lmismatch or (i>=lo_key[depth])) and
                                 (hmismatch or (i<=hi_key[depth])) then
                                   ForAllNodeLeafsRangeReverse(child[i],node_proc,halt,lmismatch or (i<>lo_key[depth]),hmismatch or (i<>hi_key[depth]),lo_key,hi_key,depth+1);
                              if halt then
                                exit;
                            end;
    artNodeTypeLeaf:
        node_proc(node);
  end;
end;

procedure TFRE_ART_TREE.ForAllValue(node: PFRE_ART_Node; const node_proc: TFRE_ART_NodeValueProc);
var i:NativeInt;
begin
  if node=nil then exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllValue(child[i],node_proc);
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        ForAllValue(child[i],node_proc);
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          ForAllValue(child[childIndex[i]],node_proc);
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          ForAllValue(PFRE_ART_Node256(node)^.child[i],node_proc);
    artNodeTypeLeaf:
        node_proc(PFRE_ART_LeafNode(node)^.GetStoredValue^);
  end;
end;

procedure TFRE_ART_TREE.ForAllValueReverse(node: PFRE_ART_Node; const node_proc: TFRE_ART_NodeValueProc);
var i:NativeInt;
begin
  if node=nil then exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=node^.count-1 downto 0 do
                        ForAllValueReverse(child[i],node_proc);
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=node^.count-1 downto 0 do
                        ForAllValueReverse(child[i],node_proc);
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=255 downto 0 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          ForAllValueReverse(child[childIndex[i]],node_proc);
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=255 downto 0 do
                        if assigned(child[i]) then
                          ForAllValueReverse(PFRE_ART_Node256(node)^.child[i],node_proc);
    artNodeTypeLeaf:
        node_proc(PFRE_ART_LeafNode(node)^.GetStoredValue^);
  end;
end;

procedure TFRE_ART_TREE.ForAllValueBreak(node: PFRE_ART_Node; const node_proc: TFRE_ART_NodeValueBreakProc; var break: boolean);
var i:NativeInt;
begin
  if node=nil then exit;
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          ForAllValueBreak(child[i],node_proc,break);
                          if break then exit;
                        end;
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      for i:=0 to node^.count-1 do
                        begin
                          ForAllValueBreak(child[i],node_proc,break);
                          if break then exit;
                        end;
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      for i:=0 to 255 do
                        if childIndex[i]<>CFREA_slot_is_empty then
                          begin
                            ForAllValueBreak(child[childIndex[i]],node_proc,break);
                            if break then exit;
                          end;
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      for i:=0 to 255 do
                        if assigned(child[i]) then
                          begin
                            ForAllValueBreak(PFRE_ART_Node256(node)^.child[i],node_proc,break);
                            if break then exit;
                          end;
    artNodeTypeLeaf:
        begin
          node_proc(PFRE_ART_LeafNode(node)^.GetStoredValue^,break);
          if break then
            exit;
        end;
  end;
end;

procedure TFRE_ART_TREE._InnerCheck;
var cnt  : integer;
    TEST : Array of Array of Byte;

    procedure TestProc(const node : PFRE_ART_Node);
    var  p4        : PFRE_ART_Node4;
         p16       : PFRE_ART_Node16;
         p48       : PFRE_ART_Node48;
         i         : NativeInt;
         child_cnt : NativeInt;
         k1,k2     : Byte;
    begin
      //assert(node^.typ=artNodeTypeLeaf);
      case node^.typ of
        artNodeType4:
            begin
               p4 := PFRE_ART_Node4(node);
               for i := 0 to p4^.count-2 do
                 begin
                   if p4^.key[i]>p4^.key[i+1] then
                     begin
                       writeln('BAD NODE 4 ');
                       abort;
                     end;
                 end;
            end;
        artNodeType16:
            begin
              p16 := PFRE_ART_Node16(node);
              for i := 0 to p16^.count-2 do
                begin
                  //k1 := flipSign(p16^.key[i]);
                  //k2 := flipsign(p16^.key[i+1]);
                  k1 := p16^.key[i];
                  k2 := p16^.key[i+1];
                  if k1>k2 then
                    begin
                      writeln('BAD NODE 16 at ',i);
                      abort;
                    end;
                end;
            end;
        artNodeType48:
            begin
              p48 := PFRE_ART_Node48(node);
              child_cnt:=0;
              for i := 0 to 255 do
                begin
                  if p48^.childIndex[i]>48 then
                    begin
                      writeln('BAD NODE 48 at ',i,' childidx = ',p48^.childIndex[i]);
                      abort;
                    end;
                  if p48^.childIndex[i]<>CFREA_slot_is_empty then
                    inc(child_cnt);
                end;
              if p48^.count<>child_cnt then
                begin
                  writeln('BAD NODE 48 ChildCount mismatch ',child_cnt,' <> ',p48^.count);
                  abort;
                end;
            end;
        artNodeType256: ;
        artNodeTypeLeaf:
            begin
              with PFRE_ART_LeafNode(node)^ do
                begin
                  SetLength(TEST[cnt],GetStoredKeyLen);
                  Move(GetStoredKey^,TEST[cnt][0],GetStoredKeyLen);
                  //writeln('Building ',cnt,' : len =',length(TEST[cnt]),' ',DumpBinary(@test[cnt][0],Length(test[cnt])),' ',Copy(PChar(@test[cnt][0]),1,Length(test[cnt])));
                end;
              inc(cnt);
            end;
      end;
    end;

    function CompareMemRanges(const r1,r2 ; const l1,l2 : NativeInt):NativeInt;
    begin
      if l1<l2 then
        begin
          result := CompareByte(r1,r2,l1);
          if result=0 then
              exit(1);
        end
      else
      if l1>l2 then
        begin
          result := CompareByte(r1,r2,l2);
          if result=0 then
            exit(-1);
        end;
      result := CompareByte(r1,r2,l1);
    end;

begin
  cnt:=0;
  SetLength(TEST,FValueCount);
  ForAllLeafsAndInner(FArtTree,@TestProc);
  for cnt:= 0 to high(TEST)-1 do
    begin
      if CompareMemRanges(test[cnt][0],test[cnt+1][0],length(test[cnt]),length(test[cnt+1]))<>-1 then
        begin
          writeln('BAD SORT ORDER AT ',cnt,' ',DumpBinary(@test[cnt][0],Length(test[cnt])),' and ',DumpBinary(@test[cnt+1][0],Length(test[cnt+1])));
          abort;
        end;
    end;
end;

destructor TFRE_ART_TREE.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TFRE_ART_TREE.Clear;
  procedure FreeIt(const node:PFRE_ART_Node);
  begin
    case node^.typ of
      artNodeType4,
      artNodeType16,
      artNodeType48,
      artNodeType256:  Dispose(node);
      artNodeTypeLeaf: FinalizeLeaf(node);
    end;
  end;
begin
  ForAllLeafsAndInner(FArtTree,@FreeIt);
  FArtTree:=nil;
end;

function TFRE_ART_TREE.IsALeafNode(node : PFRE_ART_Node):boolean; inline;
begin
  result := node^.typ=artNodeTypeLeaf;
end;

procedure TFRE_ART_TREE.FinalizeLeaf(node: PFRE_ART_Node);
begin
  PFRE_ART_LeafNode(node)^.Finalize;
  Dispose(node);
end;

{$IFNDEF ART_PURE_PASCAL}
function TFRE_ART_TREE.flipSign(keyByte : Byte) : Byte;
begin
  result := keyByte xor 128;
end;
{$ENDIF}


function ctz(x : word):NativeUint; inline;
var n : NativeUint;
begin
  n := 1;
  if (x and $ff)=0 then begin
    n:=n+8;
    x:=x shr 8;
  end;
  if (x and $0f)=0 then begin
    n:=n+4;
    x:=x shr 4;
  end;
  if (x and $03)=0 then begin
    n:=n+2;
    x:=x shr 2;
  end;
  result := n-(x and 1);
end;

function TFRE_ART_TREE.findChild(n : PFRE_ART_Node; keyByte : Byte):PPFRE_ART_Node; // Find the next child for the keyByte
var i              : NativeUint;
    {$IFNDEF ART_PURE_PASCAL}
    flippedKeyByte : Byte;
    {$ENDIF}

    procedure bin_search(const A:PByte); inline;
    var imid      : NativeInt;
        imin,imax : NativeInt;
    begin
      imin := 0;
      imax := PFRE_ART_Node16(n)^.count-1;
      while (imin < imax) do
        begin
          imid := (imax - imin) div 2 + imin;
          if (A[imid]) < KeyByte then
            imin := imid + 1
          else
            imax := imid;
        end;
      if (imax=imin) and (A[imin]=KeyByte) then
        result := @PFRE_ART_Node16(n)^.child[imin]
      else
        result := @GFRE_ART_nullNode;
    end;

begin
  case n^.typ of
    artNodeType4:
        with PFRE_ART_Node4(n)^ do
          begin
            for i:=0 to count-1 do
              if key[i] = keyByte then
                exit(@child[i]);
             exit(@GFRE_ART_nullNode);
          end;
    artNodeType16:
        {$IFDEF ART_PURE_PASCAL}
          begin
            bin_search(PFRE_ART_Node16(n)^.key);
            exit;
          end;
        {$ELSE ART_PURE_PASCAL}
            //Node16* node=static_cast<Node16*>(n);
            //__m128i cmp=_mm_cmpeq_epi8(_mm_set1_epi8(flipSign(keyByte)),_mm_loadu_si128(reinterpret_cast<__m128i*>(node->key)));
            //unsigned bitfield=_mm_movemask_epi8(cmp)&((1<<node->count)-1);
            //if (bitfield)
            //    return &node->child[ctz(bitfield)]; else
            //        return &nullNode;
        {$ENDIF}
    artNodeType48:
        with PFRE_ART_Node48(n)^ do
          begin
            if childIndex[keyByte] <> CFREA_slot_is_empty then
              exit(@child[childIndex[keyByte]])
            else
              exit(@GFRE_ART_nullNode)
          end;
    artNodeType256:
          with PFRE_ART_Node256(n)^ do
           exit(@child[keyByte]);
  end;
end;

function TFRE_ART_TREE.findChildOrRightSibling(n: PFRE_ART_Node; keyByte: Byte): PPFRE_ART_Node;
var i              : NativeUint;

begin
  case n^.typ of
    artNodeType4:
        with PFRE_ART_Node4(n)^ do
          begin
            for i:=0 to count-1 do
              if key[i] >= keyByte then
                exit(@child[i]);
             exit(@GFRE_ART_nullNode);
          end;
    artNodeType16:
        with PFRE_ART_Node16(n)^ do
          begin
            for i:=0 to count-1 do
              if key[i] >= keyByte then
                exit(@child[i]);
             exit(@GFRE_ART_nullNode);
            exit;
          end;
    artNodeType48:
        with PFRE_ART_Node48(n)^ do
          begin
            for i := keyByte to 255 do
              if childIndex[i] <> CFREA_slot_is_empty then
                exit(@child[childIndex[i]]);
            exit(@GFRE_ART_nullNode);
          end;
    artNodeType256:
        with PFRE_ART_Node256(n)^ do
          begin
            for i := keyByte to 255 do
              if child[i]<>nil then
                exit(@child[i]);
            exit(@GFRE_ART_nullNode);
          end;
  end;
end;

function TFRE_ART_TREE.minimum(node : PFRE_ART_Node):PFRE_ART_Node; // Find the leaf with smallest key
var pos : NativeInt;
begin
  if node=nil then
    exit(nil);
  if IsALeafNode(node) then
    exit(node);
  case node^.typ of
    artNodeType4:     with PFRE_ART_Node4(node)^ do
                        exit(minimum(child[0]));
    artNodeType16:    with PFRE_ART_Node16(node)^ do
                        exit(minimum(child[0]));
    artNodeType48:    with PFRE_ART_Node48(node)^ do
                        begin
                          pos := 0;
                          while childIndex[pos]=CFREA_slot_is_empty do
                            pos:=pos+1;
                          exit(minimum(child[childIndex[pos]]));
                        end;
    artNodeType256:   with PFRE_ART_Node256(node)^ do
                      begin
                        pos := 0;
                        while child[pos]=nil do
                          pos:=pos+1;
                        exit(minimum(child[pos]));
                      end;
  end;
end;

function TFRE_ART_TREE.minimum_inner(node: PFRE_ART_Node; keyByte: Byte): PFRE_ART_Node;
var pos : NativeInt;
begin
  if node=nil then
    exit(nil);
  if IsALeafNode(node) then
    exit(node);
  case node^.typ of
    artNodeType4:     with PFRE_ART_Node4(node)^ do
                        exit(child[0]);
    artNodeType16:    with PFRE_ART_Node16(node)^ do
                        exit(child[0]);
    artNodeType48:    with PFRE_ART_Node48(node)^ do
                        begin
                          pos := 0;
                          while childIndex[pos]=CFREA_slot_is_empty do
                            pos:=pos+1;
                          exit(child[childIndex[pos]]);
                        end;
    artNodeType256:   with PFRE_ART_Node256(node)^ do
                      begin
                        pos := 0;
                        while child[pos]=nil do
                          pos:=pos+1;
                        exit(child[pos]);
                      end;
  end;
end;

function TFRE_ART_TREE.maximum(node : PFRE_ART_Node):PFRE_ART_Node; // Find the leaf with largest key
var pos : NativeInt;
begin
  if node=nil then
    exit(nil);
  if IsALeafNode(node) then
    exit(node);
  case node^.typ of
    artNodeType4:   with PFRE_ART_Node4(node)^ do
                      exit(maximum(child[count-1]));
    artNodeType16:  with PFRE_ART_Node16(node)^ do
                      exit(maximum(child[count-1]));
    artNodeType48:  with PFRE_ART_Node48(node)^ do
                      begin
                        pos := 255;
                        while childIndex[pos]=CFREA_slot_is_empty do
                          pos:=pos-1;
                        exit(maximum(child[childIndex[pos]]));
                      end;
    artNodeType256: with PFRE_ART_Node256(node)^ do
                      begin
                        pos := 255;
                        while child[pos]=nil do
                          pos:=pos-1;
                        exit(maximum(child[pos]));
                      end;
  end;
end;

function TFRE_ART_TREE.LeafMatches(leaf: PFRE_ART_Node; key: PByte; keyLength, depth: Nativeint): boolean;  // Check if the key of the leaf is equal to the searched key
var leafKey : PByte;
    i       : NativeUint;
begin
  if depth<>keyLength then
    begin
      leafKey := PFRE_ART_LeafNode(leaf)^.GetStoredKey;
      for i:=depth to keyLength-1 do
        if leafKey[i]<>key[i] then
          exit(false);
    end;
  result := true;
end;

function TFRE_ART_TREE.PrefixMismatchAtByte(node : PFRE_ART_Node ; key : PByte ; keylength,depth : NativeInt):Nativeint; // Compare the key with the prefix of the node, return the number matching bytes
var pos     : Nativeint;
    minKey  : PByte;
    max_len : Nativeint;
begin
  if node^.prefixLength>CFREA_max_compressed_prefix then
    begin
      pos := 0;
      while (pos<CFREA_max_compressed_prefix) do
        begin
          if not((depth+pos)<keylength) then
            exit(pos);
          assert((depth+pos)<keylength);
          if key[depth+pos] <> node^.prefix[pos] then
            exit(pos);
          inc(pos);
        end;
      minKey := PFRE_ART_LeafNode(minimum(node))^.GetStoredKey;
      pos := 0;
      while pos < node^.prefixLength do
        begin
          if (depth+pos)>=keylength then
              exit(pos);
          if not ((depth+pos)<keylength) then
            depth:=depth;
          assert((depth+pos)<keylength);
          if (key[depth+pos] <> minKey[depth+pos]) then
            exit(pos);
          inc(pos);
        end;
    end
  else
    begin
      pos := 0;
      while pos < node^.prefixLength do
        begin
          if (depth+pos)>=(keylength) then
            exit(pos);
          if (key[depth+pos] <> node^.prefix[pos]) then
            exit(pos);
          inc(pos);
        end;
    end;
  exit(pos);
end;


function TFRE_ART_TREE.LookupKey(node: PFRE_ART_Node; key: PByte; keyLength, depth: NativeInt): PFRE_ART_Node;
var skippedPrefix : boolean=false;
    leafKey       : PByte;
    i             : NativeInt;
begin
  while (node<>Nil) do
    begin
      if IsALeafNode(node) then
        begin
        if (skippedPrefix=false) and (depth=keyLength) then
           exit(node);
        //if (depth<>keyLength) then
          begin // Check leaf
            with PFRE_ART_LeafNode(node)^ do
              leafKey := GetStoredKey;
            if skippedPrefix then
              begin
                i:=0;
              end
            else
              begin
                i:=depth;
              end;
            while(i<keyLength) do
              begin
                if (leafKey[i]<>key[i]) then
                  exit(nil);
                inc(i);
              end;
          end;
          exit(node);
        end;
      if (node^.prefixLength>0) then
        begin
          if (node^.prefixLength<CFREA_max_compressed_prefix)  then
            begin
              for i:=0 to node^.prefixLength-1 do
                begin
                  if (depth+i)>=keyLength then
                    exit(nil);
                  if not (depth+i<keyLength) then
                    depth:=depth;
                  assert(depth+i<keyLength);
                  if (key[depth+i]<>node^.prefix[i]) then
                    exit(nil)
                end;
            end
          else
            begin
              skippedPrefix:=true;
            end;
          depth := depth + node^.prefixLength;
        end;
      //assert(depth<keyLength);
      if depth<keyLength then
        node := findChild(node,key[depth])^
      else
        node := findChild(node,0)^;
      inc(depth);
    end;
  exit(nil);
end;

function TFRE_ART_TREE.LookupKeyPessimistic(node: PFRE_ART_Node; key: PByte; keyLength, depth: NativeInt): PFRE_ART_Node;
begin // Find the node with a matching key, alternative pessimistic version
  while assigned(node) do
    begin
      if IsALeafNode(node) then
        begin
          if LeafMatches(node,key,keyLength,depth) then
            exit(node);
          exit(nil);
        end;
      if PrefixMismatchAtByte(node,key,keyLength,depth) <> node^.prefixLength then
        exit(nil)
      else
        depth := depth + node^.prefixLength;
      if depth<keyLength then
        node := findChild(node,key[depth])^
      else
        node := findChild(node,0)^;
      inc(depth);
    end;
  exit(nil);
end;

function TFRE_ART_TREE.LookupKeyOrPrefix(node: PFRE_ART_Node; key: PByte; keyLength, depth: NativeInt): PFRE_ART_Node;
var mm_pos : NativeInt;
begin // Find the node with a matching key, alternative pessimistic version
  while assigned(node) do
    begin
      if IsALeafNode(node) then
        begin
          if LeafMatches(node,key,keyLength,depth) then
            exit(node);
          exit(nil);
        end;
      mm_pos := PrefixMismatchAtByte(node,key,keyLength,depth);
      if (mm_pos+depth)>=keyLength then
        exit(node);
      if mm_pos <> node^.prefixLength then
        exit(nil)
      else
        depth := depth + node^.prefixLength;
      if depth<keyLength then
        node := findChild(node,key[depth])^
      else
        node := findChild(node,0)^;
      inc(depth);
    end;
  exit(nil);
end;


function TFRE_ART_TREE.artmin(a,b : Nativeint):NativeUint;
begin
  if a<b then
    exit(a)
  else
    exit(b);
end;


procedure TFRE_ART_TREE.CopyPrefix(src,dst : PFRE_ART_Node); // Helper function that copies the prefix from the source to the destination node
begin
  dst^.prefixLength := src^.prefixLength;
  move(src^.prefix,dst^.prefix,artmin(src^.prefixLength,CFREA_max_compressed_prefix));
end;

function TFRE_ART_TREE.InsertInto(node : PFRE_ART_Node ; nodeRef : PPFRE_ART_Node ; key : PByte ; key_len,depth : Nativeint ; var value : NativeUint) : boolean;
var
    minKey          : PByte;
    newPrefixLength : Nativeint;
    newNode         : PFRE_ART_Node;
    mismatchPos     : NativeInt;
    child           : PPFRE_ART_Node;
    abs_key_len     : NativeInt;
    keybyte         : Byte;

begin
  if node=nil then
    begin
      nodeRef^ := TFRE_ART_LeafNode.NewAndInitLeaf(value,key,key_len);
      exit(true);
    end;
  abs_key_len := abs(key_len);
  if IsALeafNode(node) then
    begin // Replace leaf with a Node4 and store both leaves in it
      newPrefixLength:=0;
      while      (PFRE_ART_LeafNode(node)^.GetStoredKeyLen>depth+newPrefixLength)
             and (abs_key_len>depth+newPrefixLength)
             and (PFRE_ART_LeafNode(node)^.GetStoredKey[depth+newPrefixLength]=key[depth+newPrefixLength])
               do begin
                 inc(newPrefixLength);
               end;
      if (depth+newPrefixLength >= abs_key_len) and (PFRE_ART_LeafNode(node)^.GetStoredKeyLen<=abs_key_len) then
        begin
          value := PFRE_ART_LeafNode(node)^.GetStoredValue^;
          exit(false);
        end;
      newNode  := TFRE_ART_Node4.NewAndInitNode4;
      newNode^.prefixLength:=newPrefixLength;
      move(key[depth],newNode^.prefix,artmin(newPrefixLength,CFREA_max_compressed_prefix));
      nodeRef^ := newNode;
      if (PFRE_ART_LeafNode(node)^.GetStoredKeyLen<=depth+newPrefixLength) then //Stored Keylength exhausted ?
        InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,0,node) // Leaf,Key Exhausted
      else
        InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,PFRE_ART_LeafNode(node)^.GetStoredKey[depth+newPrefixLength],node);
      if abs_key_len <= (depth+newPrefixLength) then
        InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,0,TFRE_ART_LeafNode.NewAndInitLeaf(value,key,key_len))
      else
        InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,key[depth+newPrefixLength],TFRE_ART_LeafNode.NewAndInitLeaf(value,key,key_len));
      exit(true);
    end;
  // Handle prefix of inner node
  if node^.prefixLength > 0  then
    begin
      mismatchPos := PrefixMismatchAtByte(node,key,abs_key_len,depth);
      if mismatchPos <> node^.prefixLength then
        begin
          // Prefix differs, create new node
          newNode  := TFRE_ART_Node4.NewAndInitNode4;
          nodeRef^ := newNode;
          newNode^.prefixLength := mismatchPos;
          if newNode^.prefixLength>0 then
            move(node^.prefix,newNode^.prefix,artmin(mismatchPos,CFREA_max_compressed_prefix));
          if (node^.prefixLength<CFREA_max_compressed_prefix) then
            begin // Break up prefix
              InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,node^.prefix[mismatchPos],node);
              node^.prefixLength := node^.prefixLength  - (mismatchPos+1);
              if node^.prefixLength > 0 then
                move(node^.prefix[mismatchPos+1],node^.prefix,artmin(node^.prefixLength,CFREA_max_compressed_prefix));
            end
          else
            begin
              node^.prefixLength := node^.prefixLength - (mismatchPos+1);
              with PFRE_ART_LeafNode(minimum(node))^ do
                minKey    := GetStoredKey;
              InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,minKey[depth+mismatchPos],node);
              move(minKey[depth+mismatchPos+1],node^.prefix,artmin(node^.prefixLength,CFREA_max_compressed_prefix));
            end;
          if depth+mismatchPos<abs_key_len then
            keybyte := key[depth+mismatchPos]
          else
            begin
              if depth+mismatchPos<>abs_key_len then
                begin
                  raise Exception.Create('logic error art tree, glowing in the memory');
                end;
              keybyte := 0;
            end;
          InsertChildIntoNode4(PFRE_ART_Node4(newNode),nodeRef,keybyte,TFRE_ART_LeafNode.NewAndInitLeaf(value,key,key_len));
          exit(true);
      end;
      depth := depth + node^.prefixLength;
  end;
  // Recurse
  if depth<abs_key_len then
    child := findChild(node,key[depth])
  else
    child := findChild(node,0);
  if assigned(child^) then
    begin
      if depth<abs_key_len then
        begin
          result := InsertInto(child^,child,key,key_len,depth+1,value);
          exit(result);
        end
      else
        begin
          value  :=  PFRE_ART_LeafNode(child^)^.GetStoredValue^;
          result := false;
          exit(false);
        end;
    end;
  // insert leaf into inner node
  if depth<abs_key_len then
    keybyte := key[depth]
  else
    keybyte := 0;
  newNode := TFRE_ART_LeafNode.NewAndInitLeaf(value,key,key_len);
  case (node^.typ) of
    artNodeType4  : InsertChildIntoNode4  (  PFRE_ART_Node4(node),nodeRef,keybyte,newNode);
    artNodeType16 : InsertChildIntoNode16 ( PFRE_ART_Node16(node),nodeRef,keybyte,newNode);
    artNodeType48 : InsertChildIntoNode48 ( PFRE_ART_Node48(node),nodeRef,keybyte,newNode);
    artNodeType256: InsertchildIntoNode256(PFRE_ART_Node256(node),        keybyte,newNode);
  end;
  result := true;
end;

procedure TFRE_ART_TREE.InsertChildIntoNode4(node : PFRE_ART_Node4 ; nodeRef : PPFRE_ART_Node ; keyByte : Byte ; child : PFRE_ART_Node);
var pos     : NativeUint;
    newNode : PFRE_ART_Node16;
    i       : NativeInt;
begin
  if node^.count<4 then
    begin  // insert leaf into inner node
      pos := 0;
      while (pos<node^.count) and (node^.key[pos]<keyByte) do
        inc(pos);
      if (node^.count-pos>0) then
        begin
          move(node^.key[pos],node^.key[pos+1],node^.count-pos);
          move(node^.child[pos],node^.child[pos+1],(node^.count-pos)*sizeof(NativeUint));
        end;
      node^.key[pos]   := keyByte;
      node^.child[pos] := child;
      inc(node^.count);
    end
  else
    begin
      newNode  := TFRE_ART_Node16.NewAndInitNode16;// Grow to Node16
      nodeRef^ := newNode;
      newNode^.count := 4;
      CopyPrefix(node,newNode);
      {$IFDEF ART_PURE_PASCAL}
      for i := 0 to 3 do
        newNode^.key[i] := node^.key[i];
      {$ELSE ART_PURE_PASCAL}
      for i := 0 to 3 do
        newNode^.key[i] := flipSign(node^.key[i]);
      {$ENDIF ART_PURE_PASCAL}
      move(node^.child,newNode^.child,node^.count*sizeof(NativeUint));
      Dispose(node);
      InsertChildIntoNode16(newNode,nodeRef,keyByte,child);
    end;
end;

procedure TFRE_ART_TREE.InsertChildIntoNode16 (node : PFRE_ART_Node16 ; nodeRef : PPFRE_ART_Node ; keyByte : Byte ; child : PFRE_ART_Node);
var pos            : NativeUint;
    newNode        : PFRE_ART_Node48;
    i              : NativeInt;
    {$IFNDEF ART_PURE_PASCAL}
    keyByteFlipped : Byte;
    {$ENDIF ART_PURE_PASCAL}
begin
  if node^.count<16 then
    begin // insert leaf into inner node
    // insert element
    {$IFDEF ART_PURE_PASCAL}
      pos := 0;
      while (pos<node^.count) and (node^.key[pos]<keyByte) do inc(pos);
      if node^.count-pos>0 then
        begin
          move(node^.key[pos],node^.key[pos+1],node^.count-pos);
          move(node^.child[pos],node^.child[pos+1],(node^.count-pos)*sizeof(NativeUint));
        end;
      node^.key[pos]   := keyByte;
      node^.child[pos] := child;
      inc(node^.count);
    {$ELSE ART_PURE_PASCAL}
      //uint8_t keyByteFlipped=flipSign(keyByte);
      //__m128i cmp=_mm_cmplt_epi8(_mm_set1_epi8(keyByteFlipped),_mm_loadu_si128(reinterpret_cast<__m128i*>(node->key)));
      //uint16_t bitfield=_mm_movemask_epi8(cmp)&(0xFFFF>>(16-node->count));
      //unsigned pos=bitfield?ctz(bitfield):node->count;
      //memmove(node->key+pos+1,node->key+pos,node->count-pos);
      //memmove(node->child+pos+1,node->child+pos,(node->count-pos)*sizeof(uintptr_t));
      //node->key[pos]=keyByteFlipped;
      //node->child[pos]=child;
      //node->count++;
    {$ENDIF}
    end
  else
    begin // Grow to Node48
      newNode  := TFRE_ART_Node48.NewAndInitNode48;
      nodeRef^ := newNode;
      move(node^.child,newNode^.child,node^.count*sizeof(NativeUint));
      for i:=0 to node^.count-1 do
        begin
          {$IFDEF ART_PURE_PASCAL}
          newNode^.childIndex[node^.key[i]]:=i;
          {$ELSE ART_PURE_PASCAL}
          newNode^.childIndex[flipSign(node^.key[i])]:=i;
          {$ENDIF}
        end;
      CopyPrefix(node,newNode);
      newNode^.count := node^.count;
      Dispose(node);
      InsertChildIntoNode48(newNode,nodeRef,keyByte,child);
    end;
end;



procedure TFRE_ART_TREE.InsertChildIntoNode48 (node : PFRE_ART_Node48; nodeRef : PPFRE_ART_Node ; keyByte : Byte ; child : PFRE_ART_Node);
var pos     : NativeUint;
    newNode : PFRE_ART_Node256;
    i       : NativeInt;
begin
  if node^.count<48 then // insert leaf into inner node
    begin
      pos := node^.count;
      if assigned(node^.child[pos]) then begin
        pos := 0;
        while node^.child[pos]<>Nil do begin
          inc(pos);
        end;
      end;
      node^.child[pos] := child;
      node^.childIndex[keyByte] := pos;
      inc(node^.count);
    end
  else
    begin // Grow to Node256
      newNode := TFRE_ART_Node256.NewAndInitNode256;
      for i :=0 to 255 do
        if node^.childIndex[i]<>CFREA_slot_is_empty then
          newNode^.child[i] := node^.child[node^.childIndex[i]];
      newNode^.count := node^.count;
      CopyPrefix(node,newNode);
      nodeRef^ := newNode;
      Dispose(node);
      InsertchildIntoNode256(newNode,keyByte,child);
    end;
end;

procedure TFRE_ART_TREE.InsertchildIntoNode256(node: PFRE_ART_Node256; keyByte: Byte; child: PFRE_ART_Node);
begin
  inc(node^.count);  // insert leaf into inner node
  node^.child[keyByte] := child;
end;



function TFRE_ART_TREE.erase(node: PFRE_ART_Node; nodeRef: PPFRE_ART_Node; key: PByte; keyLength, depth: NativeInt; var value: PtrUInt): boolean;
var child          : PPFRE_ART_Node;

  procedure eraseNode4   (node : PFRE_ART_Node4;noderef : PPFRE_ART_Node;leafplace : PPFRE_ART_Node); //inline;
  var pos   : NativeInt;
      child : PFRE_ART_Node;
      l1,l2 : NativeUint;
  begin
    pos := leafPlace-PPFRE_ART_Node(@node^.child); // Delete leaf from inner node
    assert((pos>=0) and (pos<4));
    move(node^.key[pos+1],node^.key[pos],node^.count-pos-1);
    move(node^.child[pos+1],node^.child[pos],(node^.count-pos-1)*sizeof(NativeUint));
    dec(node^.count);
    if (node^.count=1) then
      begin
        child := node^.child[0]; // Get rid of one-way node
        if not IsALeafNode(child) then
          begin
            l1 := node^.prefixLength; // Concantenate prefixes
            if l1<CFREA_max_compressed_prefix then
              begin
                node^.prefix[l1] := node^.key[0];
                inc(l1);
              end;
            if l1<CFREA_max_compressed_prefix then
              begin
                l2 := artmin(child^.prefixLength,CFREA_max_compressed_prefix-l1);
                move(child^.prefix,node^.prefix[l1],l2);
                l1 := l1 + l2;
              end;
            move(node^.prefix,child^.prefix,artmin(l1,CFREA_max_compressed_prefix)); // Store concantenated prefix
            child^.prefixLength := child^.prefixLength  + node^.prefixLength + 1;
          end;
        nodeRef^ := child;
        dispose(node);
      end;
  end;
  procedure eraseNode16  (node : PFRE_ART_Node16;noderef : PPFRE_ART_Node;leafplace : PPFRE_ART_Node);// inline;
  var pos     : NativeUint;
      newNode : PFRE_ART_Node4;
      i       : NativeInt;
  begin
    pos := leafPlace - PPFRE_ART_NODE(@node^.child); // Delete leaf from inner node
    move(node^.key[pos+1],node^.key[pos],node^.count-pos-1);
    move(node^.child[pos+1],node^.child[pos],(node^.count-pos-1)*sizeof(NativeUint));
    dec(node^.count);
    if (node^.count=3) then
      begin
        newNode := TFRE_ART_Node4.NewAndInitNode4; // Shrink to Node4
        newNode^.count := 3;
        CopyPrefix(node,newNode);
        for i:=0 to 2 do
          {$IFDEF ART_PURE_PASCAL}
          newNode^.key[i] := node^.key[i];
          {$ELSE ART_PURE_PASCAL}
          newNode^.key[i] := flipSign(node^.key[i]);
          {$ENDIF ART_PURE_PASCAL}
        move(node^.child,newNode^.child,sizeof(NativeUint)*3);
        nodeRef^ := newNode;
        dispose(node);
      end;
  end;
  procedure eraseNode48  (node : PFRE_ART_Node48  ; noderef : PPFRE_ART_Node ; keyByte   : Byte); //inline;
  var newNode : PFRE_ART_Node16;
      i       : NativeUint;
  begin
    node^.child[node^.childIndex[keyByte]] := nil; // Delete leaf from inner node
    node^.childIndex[keyByte]              := CFREA_slot_is_empty;
    dec(node^.count);
    if (node^.count=12) then
      begin
        // Shrink to Node16
        newNode  := TFRE_ART_Node16.NewAndInitNode16;
        nodeRef^ := newNode;
        CopyPrefix(node,newNode);
        for i := 0 to 255 do
          begin
            if node^.childIndex[i] <> CFREA_slot_is_empty then
              begin
                {$IFDEF ART_PURE_PASCAL}
                newNode^.key[newNode^.count]   := i;
                {$ELSE ART_PURE_PASCAL}
                newNode^.key[newNode^.count]   := flipSign(i);
                {$ENDIF ART_PURE_PASCAL}
                newNode^.child[newNode^.count] := node^.child[node^.childIndex[i]];
                inc(newNode^.count);
              end;
          end;
        dispose(node);
      end;
  end;


  procedure eraseNode256 (node : PFRE_ART_Node256 ; noderef : PPFRE_ART_Node ; keyByte : Byte); //inline;
  var newNode : PFRE_ART_Node48;
      b       : NativeUint;
  begin
    node^.child[keyByte] := Nil; // Delete leaf from inner node
    dec(node^.count);
    if (node^.count=37) then
      begin // Shrink to Node48
        newNode  := TFRE_ART_Node48.NewAndInitNode48;
        nodeRef^ := newNode;
        CopyPrefix(node,newNode);
        for b := 0 to 255 do
          begin
            if assigned(node^.child[b]) then
              begin
                newNode^.childIndex[b] :=newNode^.count;
                newNode^.child[newNode^.count] := node^.child[b];
                inc(newNode^.count);
              end;
           end;
        dispose(node);
      end;
  end;

begin
  result := false;
  if not assigned(node) then // Delete a leaf from a tree
    exit;
  if IsALeafNode(node) then
    begin // Make sure we have the right leaf
      if (LeafMatches(node,key,keyLength,depth)) then
        begin
          value := PFRE_ART_LeafNode(node)^.stored_value;
          FinalizeLeaf(node);
          nodeRef^ := nil;
          result   := true;
        end;
      exit;
    end;
  if node^.prefixLength<>0 then begin // Handle prefix
      if (PrefixMismatchAtByte(node,key,keyLength,depth)<>node^.prefixLength) then exit;
      depth := depth + node^.prefixLength;
  end;

  if depth<keyLength then
    child := findChild(node,key[depth])
  else
    child := findChild(node,0);

  if assigned(child^) then
    begin
      if IsALeafNode(child^) and LeafMatches(child^,key,keyLength,depth) then
        begin
          value := PFRE_ART_LeafNode(child^)^.stored_value;
          FinalizeLeaf(child^);
          case node^.typ of   // Leaf found, delete it in inner node
            artNodeType4:   eraseNode4  (PFRE_ART_Node4  (node),nodeRef,child);
            artNodeType16:  eraseNode16 (PFRE_ART_Node16 (node),nodeRef,child);
            artNodeType48:  eraseNode48 (PFRE_ART_Node48 (node),nodeRef,key[depth]);
            artNodeType256: eraseNode256(PFRE_ART_Node256(node),nodeRef,key[depth]);
          end;
          //FinalizeLeaf(remember_child);
          result := true;
        end
      else
        result := erase(child^,child,key,keyLength,depth+1,value);
    end
  else
    exit(false);
end;


{ TFRE_ART_LeafNode }

class function TFRE_ART_LeafNode.NewAndInitLeaf(value: NativeUint; key: PByte; key_len: Nativeint): PFRE_ART_LeafNode;
begin
  if key_len=0 then raise Exception.Create('art tree keylen must be > 0');
  result         := New(PFRE_ART_LeafNode);
  result^.InitNode(artNodeTypeLeaf);
  if key_len <= sizeof(NativeUint) then
    begin
      if key_len<0 then // store key pointer
        begin
          PExConvert(@result^.stored_key)^.Ptr := PNativeUInt(key);
        end
      else
        begin // copy short key
          move(key^,result^.stored_key,key_len);
        end
    end else begin
      PExConvert(@result^.stored_key)^.Ptr := Getmem(key_len);
      move(key^,TExConvert(Result^.stored_key).Ptr^,key_len);
    end;
  result^.stored_key_len := key_len;
  result^.stored_value   := value;
end;

function TFRE_ART_LeafNode.GetStoredKey: PByte;
begin
  if (stored_key_len<=SizeOf(NativeUint)) and (stored_key_len>=0) then
    begin
      result := @stored_key;
    end
  else
    begin
      result := PByte(PExConvert(@stored_key)^.Ptr);
    end;
end;

function TFRE_ART_LeafNode.GetStoredKeyLen: NativeInt;
begin
  Result := abs(stored_key_len);
end;

function TFRE_ART_LeafNode.GetStoredValue: PNativeUint;
begin
  result := @stored_value;
end;

procedure TFRE_ART_LeafNode.Finalize;
begin
  if stored_key_len<=SizeOf(NativeUint) then // don't free direct keys or pointers to keys
    exit
  else
    Freemem(PExConvert(@stored_key)^.Ptr,stored_key_len);
end;

{ TFRE_ART_TREE }


function TFRE_ART_TREE.InsertBinaryKey(const key: PByte; const keylen: Nativeint; const value: PtrUInt): boolean;
var val : PtrUInt;
begin
  val := value;
  result := InsertInto(FArtTree,@FArtTree,key,keylen,0,val);
  if result then
    inc(FValueCount);
  //if not assigned(LookupKey(FArtTree,key,keylen,0)) then
  //  raise Exception.Create('DEBUG FAIL LOOKUP');
end;

function TFRE_ART_TREE.InsertBinaryKeyOrFetch(const key: PByte; const keylen: Nativeint; var value: PtrUInt): boolean;
begin
  result := InsertInto(FArtTree,@FArtTree,key,keylen,0,value);
  if result then
    inc(FValueCount);
end;

function TFRE_ART_TREE.ExistsBinaryKey(const key: PByte; const keylen: Nativeint; var value: PtrUInt): boolean;
var node : PFRE_ART_Node;
begin
  node   := LookupKey(FArtTree,key,keylen,0);
  //node   := LookupKeyPessimistic(FArtTree,key,keylen,0);
  result := assigned(node)
    and (PFRE_ART_LeafNode(node)^.GetStoredKeyLen=keylen);
  if result then
    value := PFRE_ART_LeafNode(node)^.GetStoredValue^
  else
    value := 0;
end;

function TFRE_ART_TREE.RemoveBinaryKey(const key: PByte; const keylen: Nativeint; var value: PtrUInt): boolean;
begin
  result :=erase(FArtTree,@FArtTree,key,keylen,0,value);
  if result then
    dec(FValueCount);
end;

function TFRE_ART_TREE.InsertUInt64Key(const key: Uint64; var value: PtrUInt): boolean;
var ukey : UInt64;
begin
  {$IFDEF ENDIAN_LITTLE}
    ukey := SwapEndian(key);
  {$ELSE}
    ukey := key;
  {$ENDIF}
  result := InsertBinaryKey(@ukey,8,value);
end;

function TFRE_ART_TREE.InsertStringKey(const key: string; const value: PtrUInt): boolean;
begin
  result := InsertBinaryKey(@key[1],length(key),value);
end;

function TFRE_ART_TREE.InsertStringKeyOrFetch(const key: string; var value: PtrUInt): boolean;
begin
  result := InsertBinaryKeyOrFetch(@key[1],length(key),value);
end;

function TFRE_ART_TREE.ExistsStringKey(const key: String; var value: PtrUInt): boolean;
begin
  result := ExistsBinaryKey(@key[1],Length(key),value);
end;

function TFRE_ART_TREE.RemoveStringKey(const key: String; var value: PtrUInt): boolean;
begin
  result := RemoveBinaryKey(@key[1],Length(key),value);
end;

procedure TFRE_ART_TREE.DumpTree(const key_as_string: boolean; const with_inner_nodes: boolean);

  procedure LocalDump(const node:PFRE_ART_Node ; const level:NativeUint);
  var i      : NativeInt;
      s      : string;
      indent : string;

      function  N4Keys(const node : PFRE_ART_Node):string;
      var i:integer;
      begin
        result := '';
        with PFRE_ART_Node4(node)^ do
          for i:=0 to 3 do
            if assigned(child[i]) then
              result := result + ' ('+IntToHex(key[i],2)+')'
            else
              result := result + ' '+IntToHex(key[i],2);
      end;

  begin
    if with_inner_nodes then
      indent := StringOfChar(' ',level*2);
    case node^.typ of
      artNodeType4:
          with PFRE_ART_Node4(node)^ do
            begin
              writeln(indent+format('NODE4 COUNT(%d) KEYS[ %s ] PREFIXLEN(%d)',[count,N4Keys(node),prefixLength]));
            end;
      artNodeType16:
          with PFRE_ART_Node16(node)^ do
            begin
              write(indent+format('NODE 16 COUNT(%d) KEYS[',[count]));
              for i :=0 to count-1 do
                write(' ',key[i],' ');
              writeln(format('] PREFIXLEN(%d)',[prefixLength]));
            end;
      artNodeType48:
          with PFRE_ART_Node48(node)^ do
            begin
              write(indent+format('NODE 48 COUNT(%d) KEYS[',[count]));
              for i :=0 to 255 do
                if childIndex[i]<>CFREA_slot_is_empty then
                  write(' ',IntToHex(i,2),' ');
              writeln(format('] PREFIXLEN(%d)',[prefixLength]));
            end;
      artNodeType256:
          with PFRE_ART_Node256(node)^ do
            begin
              writeln(indent+format('NODE 256 COUNT(%d)',[count]));
            end;
      artNodeTypeLeaf:
          with PFRE_ART_LeafNode(node)^ do
              if key_as_string then
                begin
                  SetLength(s,stored_key_len);
                  Move(GetStoredKey[0],s[1],stored_key_len);
                  writeln(indent+format('LEAF: KEY[%s]=[%u]',[s,GetStoredValue^]));
                end
              else
                begin
                  writeln(indent+format('LEAF: KEY[%s]=[%u]',[DumpBinary(GetStoredKey,stored_key_len),GetStoredValue^]));
                end;
  end;
 end;

begin
  if with_inner_nodes then
    begin
      ForAllLvlLeafsAndInner(FArtTree,@LocalDump,0);
    end else begin
      ForAllLvlLeafs(FArtTree,@LocalDump,0);
    end;
end;

procedure TFRE_ART_TREE.LinearScan(const nested_node_proc: TFRE_ART_NodeValueProc; const desc: boolean);
begin
  if not desc then
    ForAllValue(FArtTree,nested_node_proc)
  else
    ForAllValueReverse(FArtTree,nested_node_proc);
end;



function TFRE_ART_TREE.LinearScanBreak(const nested_node_proc: TFRE_ART_NodeValueBreakProc ; var break : boolean): Boolean;
begin
  ForAllValueBreak(FArtTree,nested_node_proc,break);
end;

procedure TFRE_ART_TREE.LinearScanKeyVals(const nested_node_proc: TFRE_ART_NodeCallback);
var halt:boolean;

  procedure node_proc(const node : PFRE_ART_Node);
  begin
    with PFRE_ART_LeafNode(node)^ do
      nested_node_proc(GetStoredValue^,GetStoredKey,GetStoredKeyLen);
  end;

begin
  halt := false;
  ForAllNodeLeafs(FArtTree,@node_proc,halt);
end;

function TFRE_ART_TREE.FirstKeyVal(const callback: TFRE_ART_NodeCallback): boolean;
var node : PFRE_ART_Node;
begin
  node   := minimum(FArtTree);
  result := assigned(node);
  if result then
    with PFRE_ART_LeafNode(node)^ do
      callback(GetStoredValue^,GetStoredKey,GetStoredKeyLen);
end;

function TFRE_ART_TREE.LastKeyVal(const callback: TFRE_ART_NodeCallback): boolean;
var node : PFRE_ART_Node;
begin
  node   := maximum(FArtTree);
  result := assigned(node);
  if result then
    with PFRE_ART_LeafNode(node)^ do
      callback(GetStoredValue^,GetStoredKey,GetStoredKeyLen);
end;

function TFRE_ART_TREE.FirstKeyValString(var key_string: String; var out_val: PtrUInt): Boolean;
var node : PFRE_ART_Node;
begin
  node := minimum(FArtTree);
  result := assigned(node);
  if result then
    with PFRE_ART_LeafNode(node)^ do
      begin
        SetLength(key_string,GetStoredKeyLen);
        Move(GetStoredKey^,key_string[1],GetStoredKeyLen);
        out_val := GetStoredValue^;
      end
end;

function TFRE_ART_TREE.LastKeyValString(var key_string: String; var out_val: PtrUInt): Boolean;
var node : PFRE_ART_Node;
begin
  node := minimum(FArtTree);
  result := assigned(node);
  if result then
    with PFRE_ART_LeafNode(node)^ do
      begin
        SetLength(key_string,GetStoredKeyLen);
        Move(GetStoredKey^,key_string[1],GetStoredKeyLen);
        out_val := stored_value;
      end
end;

function TFRE_ART_TREE.PrefixScan(const prefix_key: PByte; key_len: NativeUint; const nested_node_proc: TFRE_ART_NodeBreakCallback): boolean;
var node  : PFRE_ART_Node;
  procedure CheckNode(const node :PFRE_ART_Node);
  begin
    with PFRE_ART_LeafNode(node)^ do
      nested_node_proc(GetStoredValue^,GetStoredKey,GetStoredKeyLen,result);
  end;

begin
  result := false;
  node := LookupKeyOrPrefix(FArtTree,prefix_key,key_len,0);
  if not assigned(node) then
    exit;
  if node^.typ=artNodeTypeLeaf then
    begin
      with PFRE_ART_LeafNode(node)^ do
        nested_node_proc(stored_value,GetStoredKey,GetStoredKeyLen,result);
      if Result then
        exit;
    end
  else
    begin
      ForAllNodeLeafs(node,@CheckNode,result);
    end;
end;

function TFRE_ART_TREE.RangeScan(const lo_key, hi_key: PByte; const lo_keylen, hi_keylen: NativeUint; const nested_node_proc: TFRE_ART_NodeBreakCallbackCountDown; const max_count: NativeInt; skip: NativeInt; const asc: boolean): boolean;
var
    halt      : boolean;
    abscnt    : NativeInt;
    abshalt   : NativeInt;
    hi_key_ex : array [0..CFREA_maxKeyLen-1] of Byte;
    lo_key_ex : array [0..CFREA_maxKeyLen-1] of Byte;
    last_node : PFRE_ART_Node;

  procedure Iterate(const node :PFRE_ART_Node);

    function CheckKeyInRange:boolean;
    var keylen : NativeInt;
        i      : NAtiveInt;
        pskey  : PByte;
    begin
      result := true;
      keylen := PFRE_ART_LeafNode(node)^.GetStoredKeyLen;
      pskey  := PFRE_ART_LeafNode(node)^.GetStoredKey;
      if keylen<=lo_keylen then
        for i := 0 to lo_keylen-1 do
          begin
            if pskey[i]>lo_key_ex[i] then
              break;
            if pskey[i]<lo_key_ex[i] then
              begin
                writeln('WARNING ART TREE - RANGE SCAN FAIL LOWER');
                exit(false);
              end;
          end;
      if keylen>=hi_keylen then
        for i := 0 to hi_keylen-1 do
          begin
            if pskey[i]<hi_key_ex[i] then
              break;
            if pskey[i]>hi_key_ex[i] then
              begin
                writeln('WARNING ART TREE - RANGE SCAN FAIL HIGHER');
                exit(false);
              end;
          end;

    end;

  begin
    if not CheckKeyInRange then
      exit;
    //if skip>0 then
    //  dec(skip)
    //else
      with PFRE_ART_LeafNode(node)^ do
        begin
          nested_node_proc(GetStoredValue^,GetStoredKey,GetStoredKeyLen,halt,skip,abscnt,abshalt);
          last_node := node;
        end;
    if (abshalt>0) and (abscnt>=abshalt) then
      begin
        halt := true;
        exit;
      end;
  end;

begin
  if not assigned(FArtTree) then
    exit;
  halt    := false;
  abscnt  := 0;
  abshalt := max_count;
  FillByte(lo_key_ex[0],SizeOf(lo_key_ex),0);
  if lo_key<>nil then
    Move(lo_key^,lo_key_ex[0],lo_keylen);
  FillByte(hi_key_ex[0],SizeOf(hi_key_ex),0);
  if hi_key<>nil then
    Move(hi_key^,hi_key_ex[0],hi_keylen)
  else
    FillByte(hi_key_ex[0],SizeOf(hi_key_ex),255);
  if asc then
    ForAllNodeLeafsRange(FArtTree,@Iterate,halt,false,false,@lo_key_ex[0],@hi_key_ex[0],0)
  else
    ForAllNodeLeafsRangeReverse(FArtTree,@Iterate,halt,false,false,@lo_key_ex[0],@hi_key_ex[0],0);
  result := halt;
end;



function TFRE_ART_TREE.GetValueCount: NativeUint;
begin
  result := FValueCount;
end;

{ TFRE_ART_Node256 }
procedure TFRE_ART_Node256.InitNode256;
begin
  InitNode(artNodeType256);
  {$ifdef CPU32}
    FillDWord(child,256,0);
  {$else}
    FillQWord(child,256,0);
  {$endif}
end;

class function TFRE_ART_Node256.NewAndInitNode256: PFRE_ART_Node256;
begin
  result := New(PFRE_ART_Node256);
  result^.InitNode256;
end;

{ TFRE_ART_Node48 }

procedure TFRE_ART_Node48.InitNode48;
begin
  InitNode(artNodeType48);
  FillQWord(childIndex,sizeof(childIndex) div 8, CFREA_emptyMarkerQW);
  {$ifdef CPU32}
    FillDWord(child,48,0);
  {$else}
    FillQWord(child,48,0);
  {$endif}
end;

class function TFRE_ART_Node48.NewAndInitNode48: PFRE_ART_Node48;
begin
  result := New(PFRE_ART_Node48);
  result^.InitNode48;
end;


{ TFRE_ART_Node16 }

procedure TFRE_ART_Node16.InitNode16;
begin
  InitNode(artNodeType16);
  FillQWord(key,sizeof(key) div 8,0);
  {$ifdef CPU32}
    FillQWord(child,8,0);
  {$else}
    FillQWord(child,16,0);
  {$endif}
end;

function TFRE_ART_Node16.ChildIndex(const searchchild: PFRE_ART_Node): NativeInt;
var i:NativeInt;
begin
  for i:=0 to count-1 do
    if child[i] = searchchild then
      exit(i);
  exit(-1);
end;

class function TFRE_ART_Node16.NewAndInitNode16: PFRE_ART_Node16;
begin
  result := New(PFRE_ART_Node16);
  result^.InitNode16;
end;

{ TFRE_ART_Node4 }

procedure TFRE_ART_Node4.InitNode4;
begin
  InitNode(artNodeType4);
  FillDWord(key,1,0);
  {$ifdef CPU32}
    FillQWord(child,2,0);
  {$else}
    FillQWord(child,4,0);
  {$endif}
end;

function TFRE_ART_Node4.ChildIndex(const searchchild: PFRE_ART_Node): NativeInt;
var i:nativeint;
begin
  for i:=0 to count-1 do
    if child[i] = searchchild then
      exit(i);
  exit(-1);
end;

class function TFRE_ART_Node4.NewAndInitNode4:PFRE_ART_Node4;
begin
  result := New(PFRE_ART_Node4);
  result^.InitNode4;
end;

{ TFRE_ART_Node }

procedure TFRE_ART_Node.InitNode(const node_typ: TFRE_ART_NodeType);
begin
  prefixLength := 0;
  count        := 0;
  typ          := node_typ;
end;

end.

