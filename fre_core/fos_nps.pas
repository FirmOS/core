unit fos_nps;

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

{$MODE objfpc} {$H+}

//TODO : Unicode ? / Ugly Streaming with bad size assumptions ! (=64 Bit Support)

//Fully Interfaced Edition - EXPERIMENTAL !

interface

uses SysUtils,Classes,zstream,FOS_TOOL_INTERFACES;

function Get_IFOS_NPS:IFOS_NPS;


implementation

const
    cMagic      :String[6]='FOSNPS';
    cNPSVersion :Byte=1;
type
  TFOS_NPS=class;

  //RFOS_String=packed record
  // str_len:Cardinal;
  // str_ptr:Array [0..0] of AnsiChar;
  //end;
  //PFOS_String=^RFOS_String;

  //TFOS_RB_OnItem=procedure(const Item:Pointer) of object;
  TFOS_RB_NodeColor = (R,B);

  RPropStoreFileHeader=packed record
    Magic      :Array [0..5] of char;
    Version    :Byte;
    Comp       :TFOS_NPS_Compression;
  end;



  TFOS_PROP_RB_NodeP = ^TFOS_PROP_RB_NODE;
  TFOS_PROP_RB_NODE = packed record
    k: AnsiString;
    left, right, parent: TFOS_PROP_RB_NodeP;
    col: TFOS_RB_NodeColor;
    val:RFOS_PROPERTY;
  end;


  TFOS_PropTree = class
    private
      root:      TFOS_PROP_RB_NodeP;
      leftmost:  TFOS_PROP_RB_NodeP;
      rightmost: TFOS_PROP_RB_NodeP;
      cnt      : LongInt;
      procedure  RotLeft(const x: TFOS_PROP_RB_NodeP);
      procedure  RotRight(const x: TFOS_PROP_RB_NodeP);
      function   Min(const x: TFOS_PROP_RB_NodeP): TFOS_PROP_RB_NodeP;
      function   Max(const x: TFOS_PROP_RB_NodeP): TFOS_PROP_RB_NodeP;
      procedure  _Delete(z: TFOS_PROP_RB_NodeP);
      function   _Find(key:ansistring):TFOS_PROP_RB_NodeP;
      procedure  DoFreeItem(const val:RFOS_PROPERTY);
      function   _FindNextPrev(key:ansistring;const next:boolean):TFOS_PROP_RB_NodeP;
    protected
      procedure   RBInc(var x: TFOS_PROP_RB_NodeP);
      procedure   RBDec(var x: TFOS_PROP_RB_NodeP);
    public
      constructor Create;
      destructor  Destroy; override;
      procedure   Clear;
      function    Count:LongInt;

      function    Find(key:ansistring;out node:TFOS_PROP_RB_NodeP):Boolean;overload; // Delivers True on Find
      function    Find(key:ansistring): RFOS_PROPERTY;overload;


      function    Delete(key: ansistring): RFOS_PROPERTY; // Deletes the Item from the Directory, returns Pointer
      function    Add(key: ansistring):TFOS_PROP_RB_NodeP;
   end;

  TLList=class(TFOS_PropTree);


  TPLPList=class(TList) // TLazy Sort List with ownage
   private
    OwnsObjects : Boolean;
   protected
    procedure Notify(Ptr: Pointer; Action: TListNotification);override;
  end;

  TMList=class(TInterfaceList) // List of Propstores
   private
    // DontFree:Boolean;
   protected
    //procedure Notify(Ptr: Pointer; Action: TListNotification);override;
  end;


  PMRepObj = ^MRepObj;

  MRepObj = record
   Name   : AnsiString;
   PList  : TMList;
  end;

  { TFOS_NPS }

  TFOS_NPS=class(TInterfacedObject,IFOS_NPS)
  private
    _PList   : TLList;
    _PLPList : TPLPList;
    comp     : TFOS_NPS_Compression;

    function    GetCompression: TFOS_NPS_Compression;
    procedure   SetCompression(const AValue: TFOS_NPS_Compression);
    procedure   _CleanUp;
    function    GetPList: TLList;
    function    GetPLPList: TPLPList;

    function    Find(const PL:TLList;const S: ansistring;out Node:TFOS_PROP_RB_NodeP): Boolean;
    function    _FindOrInit(const name:ansistring):TFOS_PROP_RB_NodeP;
    function    _FindGet(const name:ansistring;out Node:TFOS_PROP_RB_NodeP;const ShouldBe:TFOS_PropType):TFOS_PropType;

    function    PL_Find(const PL:TPLPList;const S: ansistring; out Index: Integer): Boolean;

    property    PList  :TLList   read GetPList;
    property    PLPList:TPLPList read GetPLPList;

  public
    constructor Create;
    destructor  Destroy;override;

    procedure   Clear;
    function    GetProp(const name:ansistring;out value:RFOS_PROPERTY):boolean;overload;
    function    FindProp(const name:ansistring):TFOS_PropType;

    function    FindNxtPrv(var search_found_name:ansistring;out value:RFOS_PROPERTY;const next:boolean):boolean;

    function    FindFirst(out found_name:ansistring;out value:RFOS_PROPERTY):boolean;overload;
    function    FindLast (out found_name:ansistring;out value:RFOS_PROPERTY):boolean;overload;

    function    GetProp(const name:ansistring;out value:ansistring):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:boolean):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:Byte):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:ShortInt):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:Smallint):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:Word):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:LongInt):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:LongWord):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:Int64):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:QWord):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:Single):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSBoolArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSByteArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSShortIntArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSSmallintArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSWordArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSLongwordArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSLongintArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSInt64Array):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSQWordArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSSingleArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSDoubleArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSCurrencyArray):TFOS_PropType;overload;
    function    GetProp(const name:ansistring;out value:TFOSStringArray):TFOS_PropType;overload;

    function    GetPropDouble  (const name:ansistring;out value:Double):TFOS_PropType;overload;
    function    GetPropCurrency(const name:ansistring;out value:Currency):TFOS_PropType;overload;
    function    GetPropDateTime(const name:ansistring;out value:TDateTime):TFOS_PropType;overload;

    function    GetPropBool    (const name:ansistring):TFOS_BoolType;
    function    GetPropString  (const name:ansistring):string;
    function    GetPropByte    (const name:ansistring):Byte;
    function    GetPropShortInt(const name:ansistring):ShortInt;
    function    GetPropSmallInt(const name:ansistring):SmallInt;
    function    GetPropWord    (const name:ansistring):Word;
    function    GetPropLongint (const name:ansistring):Longint;
    function    GetPropLongword(const name:ansistring):LongWord;
    function    GetPropInt64   (const name:ansistring):Int64;
    function    GetPropQWord   (const name:ansistring):QWord;
    function    GetPropSingle  (const name:ansistring):Single;
    function    GetPropDouble  (const name:ansistring):Double;
    function    GetPropCurrency(const name:ansistring):Currency;
    function    GetPropDateTime(const name:ansistring):TDateTime;
    function    GetPropNPS     (const name:ansistring):IFOS_NPS;

    function    GetPropStream(const name:ansistring;out str:TStream;const new_copy:boolean=false):TFOS_PropType; // new_copy=Makes a new Copy of The Stream else Reference
    function    AccessStream(const name:ansistring):TMemorystream; // gives access to a internal stream (creates if nonexistent)

    function    GetPropNPS(const name:ansistring;var PS:IFOS_NPS):TFOS_PropType;
    function    GetPropObj(const name:ansistring;out Obj:TObject):TFOS_PropType;
    function    CheckOutObj(const name:ansistring;out Obj:TObject):TFOS_PropType;

    function    CheckOutNPS(const name:ansistring;out PS:IFOS_NPS):TFOS_PropType;
    function    DelProp(const name:ansistring):TFOS_PropType;

    procedure   SetPropNull(const name:ansistring);
    procedure   SetProp(const name:ansistring;const value:ansistring);overload;
    procedure   SetProp(const name:ansistring;const value:boolean);overload;
    procedure   SetProp(const name:ansistring;const value:Byte);overload;
    procedure   SetProp(const name:ansistring;const value:ShortInt);overload;
    procedure   SetProp(const name:ansistring;const value:Smallint);overload;
    procedure   SetProp(const name:ansistring;const value:Word);overload;
    procedure   SetProp(const name:ansistring;const value:LongInt);overload;
    procedure   SetProp(const name:ansistring;const value:LongWord);overload;
    procedure   SetProp(const name:ansistring;const value:Int64);overload;
    procedure   SetProp(const name:ansistring;const value:QWord);overload;

    procedure   SetProp(const name:ansistring;const value:TFOSBoolArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSByteArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSShortIntArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSSmallintArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSWordArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSLongWordArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSLongintArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSInt64Array);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSQWordArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSSingleArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSDoubleArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSCurrencyArray);overload;
    procedure   SetProp(const name:ansistring;const value:TFOSStringArray);overload;

    procedure   SetPropStream(const name:ansistring;const str:TStream);
    procedure   SetPropNPS(const name:ansistring;const PS:IFOS_NPS);
    procedure   SetPropObj(const name:ansistring;const Obj:TObject); // Speichert keine Objekte persistent !!! (FOSTEXT) = integer

    procedure   SetPropDateTime(const name:ansistring;const value:TDateTime);overload;

    procedure   SetProp(const name:ansistring;const value:Single);overload;
    procedure   SetPropDouble(const name:ansistring;const value:Double);overload;
    procedure   SetPropCurrency(const name:ansistring;const value:Currency);overload;

    function    AddPropListPS(const name:ansistring;const PS:IFOS_NPS):integer;
    procedure   RemoveProplistEntry(const name:ansistring;const idx: integer);
    procedure   ChangePropListPositons(const name:ansistring;const idx1,idx2: integer);
    procedure   InsertAtPosition(const name:ansistring;const idx:integer;const before:boolean;const PS:IFOS_NPS); // inserts after that Position
    procedure   GetPropListEntryPL(const name: ansistring; const idx: integer;out PL:IFOS_NPS);
    procedure   CheckOutPropListEntryPL(const name: ansistring; const idx: integer;out PL:IFOS_NPS); // Removes From List but does not Free it !!!!
    procedure   SetPropListEntryPL(const name: ansistring; const idx: integer;const PL:IFOS_NPS);
    function    ClearPropList(const name:ansistring):boolean;

    function    PropListCount(const name:ansistring):integer; // Get Number of PL entrys !
    function    PropCount:integer; // Get Real Properties Count without lists Lists;
    function    PropListsCount:integer; // Get Number of Proplists
    function    PropListName(const i:integer):ansistring;
    procedure   Dump(const sl:TStringlist;const indent: integer = 0);
    function    PropValToString(const val:RFOS_PROPERTY):ansistring;
    function    DumpText(const indent: integer = 0):string;
    procedure   _LoadfromStream(const str: TStream);
    procedure   LoadfromStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
    procedure   LoadfromFile(const name:ansistring;const cached:boolean=false;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
    procedure   _SavetoStream(const str: TStream);
    procedure   _SaveToPS(const ps:IFOS_NPS);
    procedure   SavetoStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
    procedure   SavetoFile(const name:ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
    function    AsAnsiString(const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring=''):Ansistring;
    procedure   SetFromAnsiString(const s:Ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
    procedure   SaveToPS(const target:IFOS_NPS);
    property    Compression:TFOS_NPS_Compression read GetCompression write SetCompression;
  end;


function _CompareStr(const S1, S2: ansistring): LongInt; forward;

// ansistring Basis
{$IFDEF FPC}
function Compare(const O1, O2: ansistring): LongInt; inline;
{$ELSE}
function Compare(const O1, O2: ansistring): LongInt;
{$ENDIF}
begin
  Result:=_CompareStr(O1,O2);
end;

function _CompareMemRange(P1, P2: Pointer; Length: Longword): LongInt;forward;

{$IFDEF FPC}

function Get_IFOS_NPS: IFOS_NPS;
begin
  result:=TFOS_NPS.Create;
end;

function _CompareStr(const S1, S2: ansistring): LongInt; inline;
{$ELSE}
function _CompareStr(const S1, S2: ansistring): LongInt;
{$ENDIF}
var count1, count2: integer;
begin
  Count1 := Length(S1);
  Count2 := Length(S2);
  if count1=count2 then begin
   result := _CompareMemRange(Pointer(S1),Pointer(S2), Count1);
  end else
  if Count1>Count2 then begin
   result:=1;
   exit;
  end else begin
   result:=-1;
   exit;
  end;
end;

{   CompareMemRange returns the result of comparison of Length bytes at P1 and P2
    case       result
    P1 < P2    < 0
    P1 > P2    > 0
    P1 = P2    = 0    }

{$IFDEF FPC}
function _CompareMemRange(P1, P2: Pointer; Length: Longword): LongInt; inline;
{$ELSE}
function _CompareMemRange(P1, P2: Pointer; Length: Longword): LongInt;
{$ENDIF}
var  i: Longword;
begin
  i := 0;
  result := 0;
  while (result=0) and (I<length) do
    begin
    result:=byte(P1^)-byte(P2^);
    P1:=pansichar(P1)+1;            // VP compat.
    P2:=pansichar(P2)+1;
    i := i + 1;
   end ;
end ;

procedure _CleanOldProp(val:RFOS_PROPERTY);
begin
   case val.PropType of
     ptStream: begin
       val.T_Stream.Free;
     end;
     ptString,
     ptBoolArray,ptByteArray,ptShortintArray,ptSmallintArray,ptWordArray,ptLongWordArray,
     ptLongintArray,ptInt64Array,ptQWordArray,ptSingleArray,ptDoubleArray,ptCurrencyArray:
     begin
       Dispose(val.T_PString); // Pointer to a Disposable type
     end;
     ptStringArray: begin
       Dispose(val.T_PStringArray);
     end;
     ptPS: begin
      IFOS_NPS(val.T_PS):=nil; // 250110 IF Change
     end;
     ptMemoryObject: begin
      // val.T_Object.Free;
      // Object is not OWNED (change 24.03.2009)
     end;
   end;
end;


{ TFOS_NPS }

function TFOS_NPS.GetProp(const name: ansistring; out value: ShortInt):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptShortInt);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: Smallint):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptSmallInt);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: Byte):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptByte);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: ansistring):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptString);
  if result=ptNotFound then begin
   value:='';
   exit;
  end else begin
   value:=node^.val.T_PString^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: boolean):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptBool);
  if result=ptNotFound then begin
   value:=false;
  end else begin
   value := node^.val.T_Bool;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: Int64):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptInt64);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: QWord):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptQWord);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: LongWord):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptLongword);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: Word):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptWord);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: LongInt):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptLongInt);
  if result=ptNotFound then begin
   value:=0;
  end else begin
   value := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: Single):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptSingle);
  if result=ptNotFound then begin
   value:=0;
   exit;
  end else begin
   value := node^.val.T_Single;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSBoolArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptBoolArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PBoolArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSByteArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptByteArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PByteArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSShortIntArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptShortintArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PShortIntArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSSmallintArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptSmallintArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PSmallintArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSWordArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptWordArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PWordArray^;
  end;
end;


function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSLongwordArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptLongWordArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PLongWordArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSLongintArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptLongintArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PLongintArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSInt64Array): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptInt64Array);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PInt64Array^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSQWordArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptQWordArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PQWordArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSSingleArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptSingleArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PSingleArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSDoubleArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptDoubleArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PDoubleArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSCurrencyArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptCurrencyArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PCurrencyArray^;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring; out value: TFOSStringArray): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptStringArray);
  if result=ptNotFound then begin
   value:=nil;
   exit;
  end else begin
   value:=node^.val.T_PStringArray^;
  end;
end;

function TFOS_NPS.GetPropBool(const name: ansistring): TFOS_BoolType;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptBool)=ptNotFound then begin
   Result:=fbtNotSet;
  end else begin
   if node^.val.T_Bool then begin
    result:=fbtTrue;
   end else begin
    result:=fbtFalse;
   end;
  end;
end;

function TFOS_NPS.GetPropString(const name: ansistring): string;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptString)=ptNotFound then begin
   result:='';
  end else begin
   result:=node^.val.T_PString^;
  end;
end;

function TFOS_NPS.GetPropByte(const name: ansistring): Byte;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptByte)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetPropShortInt(const name: ansistring): ShortInt;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptShortInt)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetPropSmallInt(const name: ansistring): SmallInt;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptSmallInt)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetPropWord(const name: ansistring): Word;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptWord)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetPropLongint(const name: ansistring): Longint;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptLongInt)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetPropLongword(const name: ansistring): LongWord;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptLongword)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetPropInt64(const name: ansistring): Int64;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptInt64)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Int;
  end;
end;

function TFOS_NPS.GetPropQWord(const name: ansistring): QWord;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptQWord)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_UInt;
  end;
end;

function TFOS_NPS.GetPropSingle(const name: ansistring): Single;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptSingle)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Single;
  end;
end;

function TFOS_NPS.GetPropDouble(const name: ansistring): Double;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptDouble)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Double;
  end;
end;

function TFOS_NPS.GetPropCurrency(const name: ansistring): Currency;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptCurrency)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Currency;
  end;
end;

function TFOS_NPS.GetPropDateTime(const name: ansistring): TDateTime;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptDateTime)=ptNotFound then begin
   result := 0;
  end else begin
   result := node^.val.T_Date;
  end;
end;

function TFOS_NPS.GetPropNPS(const name: ansistring): IFOS_NPS;
var node:TFOS_PROP_RB_NodeP;
begin
  if _FindGet(name,node,ptPS)=ptNotFound then begin
    Result:=nil;
    exit;
  end else begin
    result := IFOS_NPS(node^.val.T_PS);
  end;
end;


function TFOS_NPS.GetPropDouble(const name: ansistring; out value: Double):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptDouble);
  if result=ptNotFound then begin
   value:=0;
   exit;
  end else begin
   value := node^.val.T_Double;
  end;
end;

procedure TFOS_NPS.GetPropListEntryPL(const name: ansistring; const idx: integer; out PL: IFOS_NPS);
var i,last:integer;
begin
 if PL_Find(PLPList,name,i) then begin // Find the list for the list
  last:=PMRepObj(_PLPList[i])^.PList.Count;
  if idx>=last then begin
   raise Exception.Create('proplist index out of bounds ('+inttostr(idx)+') from ('+inttostr(last)+')');
  end;
  //PL:=IFOS_NPS(PMRepObj(_PLPList[i])^.PList[idx]);
  abort;
 end else begin
   raise Exception.Create('proplist doesnt exist');
 end;
end;

function TFOS_NPS.GetPropNPS(const name: ansistring;var PS: IFOS_NPS): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptPS);
  if result=ptNotFound then begin
   PS:=nil;
   exit;
  end else begin
   PS := IFOS_NPS(node^.val.T_PS);
  end;
end;

function TFOS_NPS.GetPropObj(const name: ansistring;out Obj: TObject): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptMemoryObject);
  if result=ptNotFound then begin
   obj:=nil;
   exit;
  end else begin
   obj := node^.val.T_Object;
  end;
end;

function TFOS_NPS.GetPropStream(const name: ansistring;out str: TStream;const new_copy:boolean=false): TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptStream);
  if result=ptNotFound then begin
   str:=nil;
   exit;
  end else begin
   if new_copy then begin
    str:=TMemoryStream.Create;
   end;
   str.CopyFrom(node^.val.T_Stream,0);
   str.Position:=0;
  end;
end;

function TFOS_NPS.AccessStream(const name: ansistring): TMemorystream;
var node:TFOS_PROP_RB_NodeP;
    pt  :TFOS_PropType;
begin
  pt:=_FindGet(name,node,ptNotFound);
  if pt<>ptStream then begin
   node:=_FindOrInit(name);
   node^.val.PropType:=ptStream;node^.val.T_Stream:=TMemoryStream.Create;
  end;
  result:=node^.val.T_Stream;
end;

procedure TFOS_NPS.InsertAtPosition(const name: ansistring; const idx: integer; const before: boolean; const PS: IFOS_NPS);
var last,ix,i:integer;
begin
 if PL_Find(PLPList,name,i) then begin // Find the list for the list
  if before then begin
   ix:=idx;
  end else begin
   ix:=idx+1;
  end;
  last:=PMRepObj(_PLPList[i])^.PList.Count;
  if ix>last then begin
    raise Exception.Create('proplist index out of bounds ('+inttostr(ix)+') from ('+inttostr(last)+')');
  end;
  if ix=last then begin
   PMrepObj(_PLPList[i])^.PList.Add(PS);  // Add the PL to the end of the list (A Normal Unsorted LIST !);
  end else begin
   PMrepObj(_PLPList[i])^.PList.Insert(ix,PS);
  end;
 end else begin // create a List and add to the Beginning
   raise Exception.Create('proplist doesnt exist');
 end;
end;

function TFOS_NPS.PL_Find(const PL: TPLPList; const S: ansistring;  out Index: Integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False; // Fast Binary Search
  L := 0;
  H := PL.Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := Compare(PMRepObj(PL[i])^.Name, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TFOS_NPS.PropCount: integer;
begin
 if not assigned(_PList) then begin
  result:=0;
 end else begin
  result:=PList.Count;
 end;
end;

function TFOS_NPS.PropListCount(const name: ansistring): integer;
var idx:integer;
begin
 if not assigned(_PLPList) then begin
  result:=0;exit;
 end;
 if PL_Find(PLPList,name,idx) then begin // Find the list for the list
  result:=PMRepObj(_PLPList[idx])^.PList.Count;
 end else begin // create a List and add to the Beginning
  result:=0; // List does not exist
 end;
end;


function TFOS_NPS.PropListsCount: integer;
begin
  if assigned(_PLPList) then begin
   result:=PLPList.Count;
  end else begin
   result:=0;
  end;
end;

function TFOS_NPS.PropListName(const i: integer): ansistring;
begin
 if i<PLPList.count then begin
  result:=PMRepObj(_PLPList[i])^.Name;
 end else begin
   raise Exception.Create('proplist index out of bounds');
 end;
end;

procedure TFOS_NPS.Dump(const sl: TStringlist; const indent: integer);
var  s, sv : ansistring;
     i, j  : integer;
     PSS   : IFOS_NPS;
     ssm   : TStringStream;
     Name  : ansistring;
     val   : RFOS_PROPERTY;

  procedure LF;
  begin
    sl.add(StringOfChar(' ', indent) + s);
    s := '';
  end;

  function Hd(const alf: boolean = False): ansistring;
  begin
    s      := s + '<' + Name + '> [' + CFOS_Proptype[val.PropType] + ']';
    Result := s;
    if alf then LF;
  end;

begin
  s := '';
  if FindFirst(Name, val) then begin
    repeat
      case val.PropType of
        ptNotFound: HD(True);
        ptNull: HD(True);
        ptBool:
        begin
          HD;
          if val.T_Bool then
            s := s + '=TRUE'
          else
            s := s + '=FALSE';
          LF;
        end;
        ptByte,
        ptShortInt,
        ptSmallInt,
        ptWord,
        ptLongInt,
        ptLongword,
        ptInt64:
        begin
          HD;
          s := s + '=' + IntToStr(val.T_Int);
          LF;
        end;
        ptQWord:
        begin
          HD;
          s := s + '=' + IntToStr(val.T_UInt);
          LF;
        end;
        ptSingle:
        begin
          HD;
          s := s + '=' + FloatToStr(val.T_Single);
          LF;
        end;
        ptDouble:
        begin
          HD;
          s := s + '=' + FloatToStr(val.T_Double);
          LF;
        end;
        ptCurrency:
        begin
          HD;
          s := s + '=' + FloatToStr(val.T_Currency);
          LF;
        end;
        ptDateTime:
        begin
          HD;
          s := s + '=' + DateTimeToStr(val.T_Single);
          LF;
        end;
        ptString:
        begin
          HD;
          s := s + '=' + Copy(val.T_PString^,1,50);
          LF;
        end;
        ptStream:
        begin
          ssm := TStringStream.Create('');
          try
            val.T_Stream.Position := 0;
            ssm.CopyFrom(val.T_Stream, GFRE_BT.min(20, val.T_Stream.Size));
            ssm.Position := 0;
            sv := ssm.DataString;
            sl.add(StringOfChar(' ', indent) + HD(False) + ' = ' +
              (GFRE_BT.Str2HexStr(sv)) + ' / ' + sv);
            s := '';
          finally
            ssm.Free;
          end;
        end;
        ptPS:
        begin
          sl.add(StringOfChar(' ', indent) + HD(False));
          IFOS_NPS(val.T_PS).Dump(sl, indent + 2);
          s := '';
        end;
        ptMemoryObject:
        begin
          HD;
          s := s + '=*Object';
          LF;
        end;
        ptBoolArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PBoolArray^)-1 do begin
            s:=s+BoolToStr(val.T_PBoolArray^[i],'T','F');
            if i<(Length(val.T_PBoolArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptByteArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PByteArray^)-1 do begin
            s:=s+IntToStr(val.T_PByteArray^[i]);
            if i<(Length(val.T_PByteArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptShortintArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PShortIntArray^)-1 do begin
            s:=s+IntToStr(val.T_PShortIntArray^[i]);
            if i<(Length(val.T_PShortIntArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+',..]'; break;end;
          end;
          LF;
        end;
        ptSmallintArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PSmallintArray^)-1 do begin
            s:=s+IntToStr(val.T_PSmallintArray^[i]);
            if i<(Length(val.T_PSmallintArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptWordArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PWordArray^)-1 do begin
            s:=s+IntToStr(val.T_PWordArray^[i]);
            if i<(Length(val.T_PWordArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptLongWordArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PLongWordArray^)-1 do begin
            s:=s+IntToStr(val.T_PLongWordArray^[i]);
            if i<(Length(val.T_PLongWordArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptLongintArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PLongintArray^)-1 do begin
            s:=s+IntToStr(val.T_PLongintArray^[i]);
            if i<(Length(val.T_PLongintArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptInt64Array:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PInt64Array^)-1 do begin
            s:=s+IntToStr(val.T_PInt64Array^[i]);
            if i<(Length(val.T_PInt64Array^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptQWordArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PQWordArray^)-1 do begin
            s:=s+IntToStr(val.T_PQWordArray^[i]);
            if i<(Length(val.T_PQWordArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptSingleArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PSingleArray^)-1 do begin
            s:=s+FloatToStr(val.T_PSingleArray^[i]);
            if i<(Length(val.T_PSingleArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptDoubleArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PDoubleArray^)-1 do begin
            s:=s+FloatToStr(val.T_PDoubleArray^[i]);
            if i<(Length(val.T_PDoubleArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptCurrencyArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PCurrencyArray^)-1 do begin
            s:=s+CurrToStr(val.T_PCurrencyArray^[i]);
            if i<(Length(val.T_PCurrencyArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        ptStringArray:
        begin
          HD;s:=s+' [';
          for i:=0 to length(val.T_PStringArray^)-1 do begin
            s:=s+'"'+val.T_PStringArray^[i]+'"';
            if i<(Length(val.T_PStringArray^)-1) then s:=s+',' else s:=s+']';
            if (i=10) then begin s:=s+'..]'; break;end;
          end;
          LF;
        end;
        else GFRE_BT.CriticalAbort('unhandled ptoptype %d',[val.PropType]);
      end;
    until FindNxtPrv(Name, val, True) = False;
  end;
  for i := 0 to PropListsCount - 1 do
  begin
    s := StringOfChar(' ', indent);
    s := s + s;
    sl.add(s + PropListName(i) + '>');
    for j := 0 to PropListCount(PropListname(i)) - 1 do begin
      s := StringOfChar(' ', indent + 1);
      s := s + s;
      sl.add(s + IntToStr(j) + ':');
      GetPropListEntryPL(PropListname(i), j, pss);
      pss.Dump(sl, indent + 2);
    end;
  end;
end;

function TFOS_NPS.PropValtoString(const val: RFOS_PROPERTY): ansistring;
var ssm:TStringStream;
begin
  case val.PropType of
    ptNotFound: result:='NOT FOUND';
    ptNull:     result:='NULL';
    ptBool:     if val.T_Bool then result:='TRUE' else result:='FALSE';
    ptByte,
    ptShortInt,
    ptSmallInt,
    ptWord,
    ptLongInt,
    ptLongword,
    ptInt64:    result := IntToStr(val.T_Int);
    ptQWord:    result := IntToStr(val.T_UInt);
    ptSingle:   result := FloatToStr(val.T_Single);
    ptDouble:   result := FloatToStr(val.T_Double);
    ptCurrency: result := FloatToStr(val.T_Currency);
    ptDateTime: result := DateTimeToStr(val.T_Single);
    ptString:   result := val.T_PString^;
    ptStream:  begin
      ssm := TStringStream.Create('');
      try
        val.T_Stream.Position := 0;
        ssm.CopyFrom(val.T_Stream, GFRE_BT.min(20, val.T_Stream.Size));
        ssm.Position := 0;
        result := ssm.DataString;
      finally
        ssm.Free;
      end;
    end;
    ptPS: result:='(PS)';
    ptMemoryObject: result:='*Object';
  end;
end;

function TFOS_NPS.DumpText(const indent: integer): string;
var sl:Tstringlist;
begin
 sl:=TStringlist.create;
 try
  Dump(sl,indent);
  result:=sl.Text;
 finally
  sl.free;
 end;
end;

procedure TFOS_NPS._SavetoStream(const str: TStream);
var  i, j,
     len,leni: integer;
     PSS:  IFOS_NPS;
     Name: ansistring;
     val:  RFOS_PROPERTY;
     b  :  Byte;

  procedure WriteProptype;
  begin
   if ord(val.PropType)=0 then GFRE_BT.CriticalAbort('PROPTYPE=0');
   if val.PropType=ptBool then begin
    if val.T_Bool then begin
     b:=ord(_str_BoolTrue);
    end else begin
     b:=ord(_str_BoolFalse);
    end;
    str.Write(b,1); // Don't waste bytes on Boolean
   end else begin
    str.Write(val.PropType,1);
   end;
   len:=length(Name);
   str.Write(len, 4);
   str.Write(Pointer(Name)^,len);
  end;

  procedure WritePListType;
  begin
   len:=length(Name);
   str.Write(len, 4);
   str.Write(Pointer(Name)^,len);
   str.Write(leni, 4);
  end;

begin
  if FindFirst(Name, val) then repeat
      WriteProptype;
      case val.PropType of
        ptNotFound : ;
        ptNull     : ;
        ptBool     : ;
        ptByte     : str.Write(val.T_UInt,1);
        ptShortInt : str.Write(val.T_Int,1);
        ptSmallInt : str.Write(val.T_Int,2);
        ptWord     : str.Write(val.T_UInt,2);
        ptLongInt  : str.Write(val.T_Int,4);
        ptLongword : str.Write(val.T_UInt,4);
        ptInt64    : str.Write(val.T_Int,8);
        ptQWord    : str.Write(val.T_UInt,8);
        ptSingle   : str.Write(val.T_Single,4);
        ptDouble   : str.Write(val.T_Double,8);
        ptCurrency : str.Write(val.T_Currency,8);
        ptDateTime : str.Write(val.T_Date,8);
        ptString   : begin
                      len:=length(val.T_PString^);
                      str.Write(len, 4);
                      str.Write(val.T_PString^[1],len);
                     end;
        ptStream   : begin
                      len:=val.T_Stream.Size;
                      str.Write(len, 4);
                      str.Write(val.T_Stream.Memory^,len);
                     end;
        ptPS       : begin
                      IFOS_NPS(val.T_PS)._SavetoStream(str);
                     end;
        ptMemoryObject: ; // Skip - dont stream
        ptBoolArray: begin
                       len:=Length(val.T_PBoolArray^);str.Write(len,4);
                       for i:=0 to High(val.T_PBoolArray^) do str.Write(val.T_PBoolArray^[i],sizeof(val.T_PBoolArray^[0]));
                     end;
        ptByteArray: begin
                       len:=Length(val.T_PByteArray^);str.Write(len,4);
                       for i:=0 to High(val.T_PByteArray^) do str.Write(val.T_PByteArray^[i],sizeof(val.T_PByteArray^[0]));
                     end;
        ptShortintArray:begin
                          len:=Length(val.T_PShortIntArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PShortIntArray^) do str.Write(val.T_PShortIntArray^[i],sizeof(val.T_PShortIntArray^[0]));
                        end;
        ptSmallintArray:begin
                          len:=Length(val.T_PSmallintArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PSmallintArray^) do str.Write(val.T_PSmallintArray^[i],sizeof(val.T_PSmallintArray^[0]));
                        end;
        ptWordArray:    begin
                          len:=Length(val.T_PWordArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PWordArray^) do str.Write(val.T_PWordArray^[i],sizeof(val.T_PWordArray^[0]));
                        end;
        ptLongWordArray:begin
                          len:=Length(val.T_PLongWordArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PLongWordArray^) do str.Write(val.T_PLongWordArray^[i],sizeof(val.T_PLongWordArray^[0]));
                        end;
        ptLongintArray:begin
                          len:=Length(val.T_PLongintArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PLongintArray^) do str.Write(val.T_PLongintArray^[i],sizeof(val.T_PLongintArray^[0]));
                        end;
        ptInt64Array:begin
                          len:=Length(val.T_PInt64Array^);str.Write(len,4);
                          for i:=0 to High(val.T_PInt64Array^) do str.Write(val.T_PInt64Array^[i],sizeof(val.T_PInt64Array^[0]));
                        end;
        ptQWordArray:begin
                          len:=Length(val.T_PQWordArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PQWordArray^) do str.Write(val.T_PQWordArray^[i],sizeof(val.T_PQWordArray^[0]));
                        end;
        ptSingleArray:begin
                          len:=Length(val.T_PSingleArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PSingleArray^) do str.Write(val.T_PSingleArray^[i],sizeof(val.T_PSingleArray^[0]));
                      end;
        ptDoubleArray:begin
                          len:=Length(val.T_PDoubleArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PDoubleArray^) do str.Write(val.T_PDoubleArray^[i],sizeof(val.T_PDoubleArray^[0]));
                      end;
        ptCurrencyArray:begin
                          len:=Length(val.T_PCurrencyArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PCurrencyArray^) do str.Write(val.T_PCurrencyArray^[i],sizeof(val.T_PCurrencyArray^[0]));
                        end;
        ptStringArray:begin
                          len:=Length(val.T_PStringArray^);str.Write(len,4);
                          for i:=0 to High(val.T_PStringArray^) do begin
                            len:=length(val.T_PStringArray^[i]);str.Write(len, 4);
                            str.Write(val.T_PStringArray^[i][1],len);
                          end;
                      end;
        else GFRE_BT.CriticalAbort('save to stream unknown propertytype <%d>',[ord(val.PropType)]);
      end;
  until FindNxtPrv(Name, val, True) = False;
  len:=PropListsCount;
  if len>0 then begin
    b:=ord(_str_PL_Mark);
    str.Write(b,1);
    str.Write(len, 4);
    for i := 0 to len-1 do begin
      name:=PropListName(i);
      leni:=PropListCount(name);
      WritePListType;
      for j := 0 to leni-1 do begin
        GetPropListEntryPL(name, j, pss);
        pss._SavetoStream(str);
      end;
    end;
  end;
  b:=ord(_str_StreamEnd);
  str.Write(b,1);
end;

procedure TFOS_NPS._SaveToPS(const ps: IFOS_NPS);
var  i, j,
     len,leni: integer;
     PSS,
     new  :  IFOS_NPS;
     Name :  ansistring;
     val  :  RFOS_PROPERTY;

begin
  ps.Clear; // save version, copy to existing (merge) should be tested better
  if FindFirst(Name, val) then repeat
      case val.PropType of
        ptNotFound : ;
        ptNull     : ps.SetPropNull(name) ;
        ptBool     : ps.SetProp(name,val.T_Bool);
        ptByte     : ps.SetProp(name,Byte(val.T_UInt));
        ptShortInt : ps.SetProp(name,ShortInt(val.T_Int));
        ptSmallInt : ps.SetProp(name,SmallInt(val.T_Int));
        ptWord     : ps.SetProp(name,Word(val.T_UInt));
        ptLongInt  : ps.SetProp(name,LongInt(val.T_Int));
        ptLongword : ps.SetProp(name,LongWord(val.T_UInt));
        ptInt64    : ps.SetProp(name,int64(val.T_Int));
        ptQWord    : ps.SetProp(name,QWord(val.T_UInt));
        ptSingle   : ps.SetProp(name,single(val.T_Single));
        ptDouble   : ps.SetProp(name,double(val.T_Double));
        ptCurrency : ps.SetProp(name,Currency(val.T_Currency));
        ptDateTime : ps.SetProp(name,TDateTime(val.T_Date));
        ptString   : begin
                      ps.SetProp(name,val.T_PString^);
                     end;
        ptStream   : begin
                      len:=val.T_Stream.Size;
                      //ps.DelProp(name); think merge
                      ps.AccessStream(name).Write(val.T_Stream.Memory^,len);
                     end;
        ptPS       : begin
                      //ps.DelProp(name); think merge
                      GFRE_TF.Get_NPS(new);
                      ps.SetPropNPS(name,new);
                      IFOS_NPS(val.T_PS)._SavetoPS(new);
                     end;
        ptMemoryObject: begin
                       //ps.DelProp(Name); think merge
                       ps.SetPropObj(Name,val.T_Object);
                     end;
        ptBoolArray: begin
                       ps.SetProp(name,val.T_PBoolArray^);
                     end;
        ptByteArray: begin
                       ps.SetProp(name,val.T_PByteArray^);
                     end;
        ptShortintArray:begin
                       ps.SetProp(name,val.T_PShortIntArray^);
                        end;
        ptSmallintArray:begin
                       ps.SetProp(name,val.T_PSmallintArray^);
                        end;
        ptWordArray:    begin
                       ps.SetProp(name,val.T_PWordArray^);
                        end;
        ptLongWordArray:begin
                       ps.SetProp(name,val.T_PLongWordArray^);
                        end;
        ptLongintArray:begin
                       ps.SetProp(name,val.T_PLongintArray^);
                        end;
        ptInt64Array:begin
                       ps.SetProp(name,val.T_PInt64Array^);
                        end;
        ptQWordArray:begin
                       ps.SetProp(name,val.T_PQWordArray^);
                        end;
        ptSingleArray:begin
                       ps.SetProp(name,val.T_PSingleArray^);
                      end;
        ptDoubleArray:begin
                       ps.SetProp(name,val.T_PDoubleArray^);
                      end;
        ptCurrencyArray:begin
                       ps.SetProp(name,val.T_PCurrencyArray^);
                        end;
        ptStringArray:begin
                       ps.SetProp(name,val.T_PStringArray^);
        end;
        else GFRE_BT.CriticalAbort('save to ps unknown propertytype <%d>',[ord(val.PropType)]);
      end;
  until FindNxtPrv(Name, val, True) = False;
  len:=PropListsCount;
  if len>0 then begin
    for i := 0 to len-1 do begin
      name:=PropListName(i);
      leni:=PropListCount(name);
      for j := 0 to leni-1 do begin
        GetPropListEntryPL(name, j, pss);
        GFRE_TF.Get_NPS(new);
        pss._SaveToPS(new);
        ps.AddPropListPS(name,new);
      end;
    end;
  end;
end;

procedure TFOS_NPS.SavetoStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
var  hd     : RPropStoreFileHeader;
     mystr  : TMemoryStream;
     cstr   : Tcompressionstream;
begin
  with hd do begin
    Magic       := cMagic;
    Version     := cNPSVersion;
    Comp        := Compression;
  end;
  mystr:=TMemoryStream.Create;
  try
    mystr.Write(hd,sizeof(hd));
    case Compression of
      ncl_NONE: begin
                  cstr:=Tcompressionstream.create(clnone,mystr);
                end;
      ncl_FAST: begin
                  cstr:=Tcompressionstream.create(clfastest,mystr);
                end;
      ncl_MAX: begin
                  cstr:=Tcompressionstream.create(clmax,mystr);
               end;
    end;
    try
      _SavetoStream(cstr);
    finally
      cstr.free;
    end;
    mystr.Position:=0;
    if EncryptionKey<>'' then begin
      GFRE_BT.EncryptAESStreamCBC256(mystr,EncryptionKey,EncryptionVector,str);
    end else begin
      str.CopyFrom(mystr,0);
    end;
  finally
     mystr.Free;
  end;
end;

procedure TFOS_NPS._LoadfromStream(const str: TStream);
var  s   : ansistring;
     i, j,len,leni: integer;
     PSS:  TFOS_NPS;
     pt:   TFOS_PropType;
     nsm:  TMemoryStream;
     Name: ansistring;
     val:  RFOS_PROPERTY;
     bool_array:TFOSBoolArray;
     byte_array:TFOSByteArray;
     shortint_array :TFOSShortIntArray;
     smallint_array :TFOSSmallintArray;
     word_array:TFOSWordArray;
     longint_array:TFOSLongintArray;
     longword_array:TFOSLongwordArray;
     int64_array:TFOSInt64Array;
     qword_array:TFOSQWordArray;
     single_array:TFOSSingleArray;
     double_array:TFOSDoubleArray;
     currency_array:TFOSCurrencyArray;
     string_array:TFOSStringArray;

  procedure ReadProptype;
  begin
   pt:=ptNotFound;
   str.Read(pt,1);
   if pt=_str_StreamEnd then begin
    exit;
   end;
   if pt=_str_PL_Mark then begin
    exit;
   end;;
   str.Read(len,4);
   setlength(name,len);
   str.Read(Pointer(Name)^,len);
  end;


  procedure RaiseIllegal;
  begin
   GFRE_BT.CriticalAbort('illegal nps stream format byte <%d>',[pt]);
  end;

begin
  s := '';
  repeat
        val.PropType:=ptBool; // suppress compiler warning
        Fillchar(val,Sizeof(val),0);
        ReadProptype; // and name
        case pt of
          ptNotFound      : RaiseIllegal;
          ptNull          : SetPropNull(name);
          _str_BoolFalse  : Setprop(name,false);
          _str_BoolTrue   : setprop(name,true);
          ptByte          : begin str.Read(val.T_UInt,1)    ; setprop(name,Byte(val.T_UInt));end;
          ptShortInt      : begin str.Read(val.T_Int,1)     ; setprop(name,ShortInt(val.T_Int));end;
          ptSmallInt      : begin str.Read(val.T_Int,2)     ; setprop(name,SmallInt(val.T_Int));end;
          ptWord          : begin str.Read(val.T_UInt,2)    ; setprop(name,Word(val.T_UInt));end;
          ptLongInt       : begin str.Read(val.T_Int,4)     ; setprop(name,Longint(val.T_Int));end;
          ptLongword      : begin str.Read(val.T_UInt,4)    ; setprop(name,Longword(val.T_UInt));end;
          ptInt64         : begin str.Read(val.T_Int,8)     ; setprop(name,Int64(val.T_Int));end;
          ptQWord         : begin str.Read(val.T_UInt,8)    ; SetProp(name,QWord(val.T_UInt));end;
          ptSingle        : begin str.Read(val.T_Single,4)  ; setprop(name,val.T_Single);end;
          ptDouble        : begin str.Read(val.T_Double,8)  ; SetPropDouble(name,val.T_Double);end;
          ptCurrency      : begin str.Read(val.T_Currency,8); SetPropCurrency(name,val.T_Currency);end;
          ptDateTime      : begin str.Read(val.T_Date,8)    ; SetPropDateTime(name,val.T_Date);end;
          ptString        : begin
                              str.Read(len,4);setlength(s,len);str.Read(Pointer(s)^,len);
                              SetProp(name,s);
                            end;
          ptStream   : begin
                        str.Read(len,4);
                        nsm:=AccessStream(name);
                        nsm.Size:=len;
                        str.Read(nsm.Memory^,len);
                       end;
          ptPS       : begin
                        pss:=TFOS_NPS.Create;
                        PSS._LoadfromStream(str);
                        SetPropNPS(name,pss); // set deeper ps
                       end;
          ptMemoryObject: ; // Skip - dont stream
          ptBoolArray: begin
                            str.Read(len,4);setlength(bool_array,len);
                            for i:=0 to high(bool_array) do str.Read(bool_array[i],sizeof(bool_array[0]));
                            SetProp(name,bool_array);
                       end;
          ptByteArray: begin
                            str.Read(len,4);setlength(byte_array,len);
                            for i:=0 to high(byte_array) do str.Read(byte_array[i],sizeof(byte_array[0]));
                            SetProp(name,byte_array);
                       end;
          ptShortintArray:begin
                            str.Read(len,4);setlength(shortint_array,len);
                            for i:=0 to high(shortint_array) do str.Read(shortint_array[i],sizeof(shortint_array[0]));
                            SetProp(name,shortint_array);
                          end;
          ptSmallintArray:begin
                            str.Read(len,4);setlength(smallint_array,len);
                            for i:=0 to high(smallint_array) do str.Read(smallint_array[i],sizeof(smallint_array[0]));
                            SetProp(name,smallint_array);
                          end;
          ptWordArray:begin
                            str.Read(len,4);setlength(word_array,len);
                            for i:=0 to high(word_array) do str.Read(word_array[i],sizeof(word_array[0]));
                            SetProp(name,word_array);
                          end;
          ptLongWordArray:begin
                            str.Read(len,4);setlength(longword_array,len);
                            for i:=0 to high(longword_array) do str.Read(longword_array[i],sizeof(longword_array[0]));
                            SetProp(name,longword_array);
                          end;
          ptLongintArray:begin
                            str.Read(len,4);setlength(longint_array,len);
                            for i:=0 to high(longint_array) do str.Read(longint_array[i],sizeof(longint_array[0]));
                            SetProp(name,longint_array);
                          end;
          ptInt64Array:begin
                            str.Read(len,4);setlength(int64_array,len);
                            for i:=0 to high(int64_array) do str.Read(int64_array[i],sizeof(int64_array[0]));
                            SetProp(name,int64_array);
                          end;
          ptQWordArray:begin
                            str.Read(len,4);setlength(qword_array,len);
                            for i:=0 to high(qword_array) do str.Read(qword_array[i],sizeof(qword_array[0]));
                            SetProp(name,qword_array);
                          end;
          ptSingleArray:begin
                            str.Read(len,4);setlength(single_array,len);
                            for i:=0 to high(single_array) do str.Read(single_array[i],sizeof(single_array[0]));
                            SetProp(name,single_array);
                          end;
          ptDoubleArray:begin
                            str.Read(len,4);setlength(double_array,len);
                            for i:=0 to high(double_array) do str.Read(double_array[i],sizeof(double_array[0]));
                            SetProp(name,double_array);
                          end;
          ptCurrencyArray:begin
                            str.Read(len,4);setlength(currency_array,len);
                            for i:=0 to high(currency_array) do str.Read(currency_array[i],sizeof(currency_array[0]));
                            SetProp(name,currency_array);
                          end;
          ptStringArray:begin
                            str.Read(len,4);setlength(string_array,len);
                            for i:=0 to high(string_array) do begin
                              str.Read(len,4);setlength(string_array[i],len);str.Read(Pointer(string_array[i])^,len);
                            end;
                            SetProp(name,string_array);
                          end;
          _str_PL_Mark: begin
                          str.Read(len,4);
                          for i := 0 to len-1 do begin
                            leni:=0;
                            str.Read(leni, 4);setlength(s,leni);
                            str.Read(Pointer(s)^,leni);
                            str.Read(leni, 4);
                            for j := 0 to leni-1 do begin
                              pss:=TFOS_NPS.Create;
                              PSS._LoadfromStream(str);
                              AddPropListPS(s,pss); // set deeper ps
                            end;
                          end;
                        end;
          _str_StreamEnd: break;
          else RaiseIllegal;
      end;
  until False;
end;

procedure TFOS_NPS.LoadfromStream(const str: TStream;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
var  hd     : RPropStoreFileHeader;
     mystr  : TStream;
     cstr   : Tdecompressionstream;

  procedure _Read;
  begin
    mystr.Read(hd,sizeof(hd));
    if hd.Magic<>cMagic then raise Exception.Create('Invalid NPS Signature/Magic');
    if hd.Version<>cNPSVersion then raise Exception.Create('Invalid NPS Fileversion got '+inttostr(hd.Version)+' expected '+inttostr(cNPSVersion));
    cstr:=Tdecompressionstream.create(mystr,false);
    try
      _LoadfromStream(cstr);
    finally
      cstr.Free;
    end;
  end;

begin
  if EncryptionKey<>'' then begin
    mystr:=TMemoryStream.Create;
    try
      GFRE_BT.DecryptAESStreamCBC256(str,EncryptionKey,EncryptionVector,mystr);
      mystr.Position:=0;
      _Read;
    finally
      mystr.free;
    end;
  end else begin
    mystr:=str;
    _Read;
  end;
end;

procedure TFOS_NPS.SavetoFile(const name: ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
var f:TFileStream;
begin
 f:=TFileStream.Create(name,fmCreate);
 try
  SavetoStream(f,EncryptionKey,EncryptionVector);
 finally
  f.free;
 end;
end;

function TFOS_NPS.AsAnsiString(const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring=''): Ansistring;
var f:TStringStream;
begin
 f:=TStringStream.Create('');
 try
  SavetoStream(f,EncryptionKey,EncryptionVector);
  result:=f.DataString;
 finally
  f.free;
 end;
end;

procedure TFOS_NPS.SetFromAnsiString(const s: Ansistring;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
var f:TStringStream;
begin
 f:=TStringStream.Create(s);
 try
  LoadfromStream(f,EncryptionKey,EncryptionVector);
 finally
  f.free;
 end;
end;

procedure TFOS_NPS.SaveToPS(const target: IFOS_NPS);
begin
  _SaveToPS(target);
end;


procedure TFOS_NPS.LoadfromFile(const name: ansistring;const cached:boolean=false;const EncryptionKey:AnsiString='';const EncryptionVector:Ansistring='');
var f:TStream;
begin
 if cached then begin
   f:=TMemoryStream.Create;
   try
     TMemoryStream(f).LoadFromFile(name);
     LoadfromStream(f,EncryptionKey,EncryptionVector);
   finally
     f.free;
   end;
 end else begin
   f:=TFileStream.Create(name,fmOpenRead);
   try
    LoadfromStream(f);
   finally
    f.free;
   end;
 end;
end;

procedure TFOS_NPS.RemoveProplistEntry(const name: ansistring; const idx: integer);
var i,last:integer;
begin
 if PL_Find(PLPList,name,i) then begin // Find the list for the list
  last:=PMRepObj(_PLPList[i])^.PList.Count;
  if idx>=last then begin
   raise Exception.Create('proplist index out of bounds ('+inttostr(idx)+') from ('+inttostr(last)+')');
  end;
  PMRepObj(_PLPList[i])^.PList.Delete(idx); // Drop and Free it if necessary
 end else begin
   raise Exception.Create('proplist doesnt exist');
 end;
end;

function TFOS_NPS.GetPropCurrency(const name: ansistring; out value: Currency):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptCurrency);
  if result=ptNotFound then begin
   value:=0;
   exit;
  end else begin
   value := node^.val.T_Currency;
  end;
end;

function TFOS_NPS.GetPropDateTime(const name: ansistring; out value: TDateTime):TFOS_PropType;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptDateTime);
  if result=ptNotFound then begin
   value:=0;
   exit;
  end else begin
   value := node^.val.T_Date;
  end;
end;

function TFOS_NPS.GetProp(const name: ansistring;  out value: RFOS_PROPERTY): boolean;
var node:TFOS_PROP_RB_NodeP;
begin
  result:=false;
  if not assigned(_PList) then exit;
  if not Find(PList,name,node) then exit;
  value:=node^.val;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: ShortInt);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptShortInt;node^.val.T_Int:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: Smallint);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptSmallInt;node^.val.T_Int:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: Byte);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptByte;node^.val.T_UInt:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: ansistring);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptString;
 node^.val.T_PString:=new(PString);
 node^.val.T_PString^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: boolean);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptBool;node^.val.T_Bool:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: Int64);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptInt64;node^.val.T_Int:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: QWord);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptQWord;node^.val.T_UInt:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSBoolArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptBoolArray;
 node^.val.T_PBoolArray:=new(PFOSBoolArray);
 node^.val.T_PBoolArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSByteArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptByteArray;
 node^.val.T_PByteArray:=new(PFOSByteArray);
 node^.val.T_PByteArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSShortIntArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptShortintArray;
 node^.val.T_PShortIntArray:=new(PFOSShortIntArray);
 node^.val.T_PShortIntArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSSmallintArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptSmallintArray;
 node^.val.T_PSmallintArray:=new(PFOSSmallintArray);
 node^.val.T_PSmallintArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSWordArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptWordArray;
 node^.val.T_PWordArray:=new(PFOSWordArray);
 node^.val.T_PWordArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSLongwordArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptLongWordArray;
 node^.val.T_PLongWordArray:=new(PFOSLongWordArray);
 node^.val.T_PLongWordArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSLongintArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptLongintArray;
 node^.val.T_PLongintArray:=new(PFOSLongintArray);
 node^.val.T_PLongintArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSInt64Array);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptInt64Array;
 node^.val.T_PInt64Array:=new(PFOSInt64Array);
 node^.val.T_PInt64Array^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSQWordArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptQWordArray;
 node^.val.T_PQWordArray:=new(PFOSQWordArray);
 node^.val.T_PQWordArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSSingleArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptSingleArray;
 node^.val.T_PSingleArray:=new(PFOSSingleArray);
 node^.val.T_PSingleArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSDoubleArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptDoubleArray;
 node^.val.T_PDoubleArray:=new(PFOSDoubleArray);
 node^.val.T_PDoubleArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSCurrencyArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptCurrencyArray;
 node^.val.T_PCurrencyArray:=new(PFOSCurrencyArray);
 node^.val.T_PCurrencyArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: TFOSStringArray);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptStringArray;
 node^.val.T_PStringArray:=new(PFOSStringArray);
 node^.val.T_PStringArray^:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: Single);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptSingle;node^.val.T_Single:=value;
end;

procedure TFOS_NPS.SetPropDouble(const name: ansistring; const value: Double);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptDouble;node^.val.T_Double:=value;
end;

procedure TFOS_NPS.SetPropListEntryPL(const name: ansistring; const idx: integer; const PL: IFOS_NPS);
var i,last:integer;
begin
 if PL_Find(PLPList,name,i) then begin // Find the list for the list
  last:=PMRepObj(_PLPList[i])^.PList.Count;
  if idx>=last then begin
   raise Exception.Create('proplist index out of bounds ('+inttostr(idx)+') from ('+inttostr(last)+')');
  end;
  abort;
  //if PL<>IFOS_NPS(PMRepObj(_PLPList[i])^.PList[idx]) then begin
  // PMRepObj(_PLPList[i])^.PList.Delete(idx); // Drop and Free it if necessary
  // PMRepObj(_PLPList[i])^.PList.Insert(Idx,PL); // Insert at Position
  //end;
 end else begin
   raise Exception.Create('proplist doesnt exist');
 end;
end;

procedure TFOS_NPS.SetPropDateTime(const name: ansistring; const value: TDateTime);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptDateTime;node^.val.T_Date:=value;
end;

procedure TFOS_NPS.SetPropCurrency(const name: ansistring; const value: Currency);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptCurrency;node^.val.T_Currency:=value;
end;

procedure TFOS_NPS.SetPropNPS(const name: ansistring; const PS: IFOS_NPS);
var node:TFOS_PROP_RB_NodeP;
begin
 if PS=nil then begin
   raise Exception.Create('dont add nil pointer to nps !');
 end;
 if Find(PList,name,node) then begin
  if node^.val.PropType=ptPS then begin
   if IFOS_NPS(node^.val.T_PS)=ps then exit; // This Propstore is alread Set;
  end;
  _CleanOldProp(node^.val);
 end else begin
  node:=_PList.Add(name);
 end;
 node^.val.T_PS:=nil;
 IFOS_NPS(node^.val.T_PS):=ps;
 node^.val.PropType:=ptPS;
end;

procedure TFOS_NPS.SetPropNull(const name: ansistring);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptNull;node^.val.T_UInt:=0;
end;

procedure TFOS_NPS.SetPropObj(const name: ansistring; const Obj: TObject);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptMemoryObject;node^.val.T_Object:=Obj;
end;


procedure TFOS_NPS.SetPropStream(const name: ansistring; const str: TStream);
var node:TFOS_PROP_RB_NodeP;
       m:TMemorystream;
begin
 m:=TMemoryStream.Create;
 m.CopyFrom(str,0);
 node:=_FindOrInit(name);
 node^.val.PropType:=ptStream;node^.val.T_Stream:=m;
end;

procedure TFOS_NPS._CleanUp;
begin
  if assigned(_PList) then _PList.Clear;
  if assigned(_PLPList) then _PLPList.Clear;
end;

function TFOS_NPS.GetCompression: TFOS_NPS_Compression;
begin
  result:=comp;
end;


procedure TFOS_NPS.SetCompression(const AValue: TFOS_NPS_Compression);
begin
  comp:=AValue;
end;



function TFOS_NPS._FindGet(const name: ansistring; out Node: TFOS_PROP_RB_NodeP;const ShouldBe:TFOS_PropType): TFOS_PropType;
begin
  result:=ptNotFound;
  if not assigned(_PList) then exit;
  if not Find(PList,name,node) then exit;
  result:=node^.val.PropType;
  if (result<>ShouldBe) and (Shouldbe<>ptNotFound) then raise Exception.Create('TFOSPropstore: type mismatch on get / should be '+CFOS_Proptype[shouldbe]+' but is '+CFOS_Proptype[result]);
end;

function TFOS_NPS.FindNxtPrv(var search_found_name:ansistring;out value: RFOS_PROPERTY;const next:boolean): boolean;
var Node: TFOS_PROP_RB_NodeP;
begin
 node:=PList._FindNextPrev(search_found_name,next);
 if not assigned(node) then begin
  value.PropType:=ptNotFound;
  value.T_Int:=0;
  search_found_name:='';
  result:=false;
 end else begin
  result:=true;
  value:=node^.val;
  search_found_name:=node^.k;
 end;
end;



function TFOS_NPS._FindOrInit(const name: ansistring): TFOS_PROP_RB_NodeP;
begin
 if Find(PList,name,result) then begin
  _CleanOldProp(result^.val);
 end else begin
  result:=_PList.Add(name);
 end;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: LongWord);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptLongword;node^.val.T_UInt:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: Word);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptWord;node^.val.T_UInt:=value;
end;

procedure TFOS_NPS.SetProp(const name: ansistring; const value: LongInt);
var node:TFOS_PROP_RB_NodeP;
begin
 node:=_FindOrInit(name);
 node^.val.PropType:=ptLongInt;node^.val.T_Int:=value;
end;


function TFOS_NPS.AddPropListPS(const name: ansistring;  const PS: IFOS_NPS): integer;
var idx:integer;
    L:TMLIST;
    MRepObj:PMRepObj;
begin
// if PS=self then begin
//   raise Exception.Create('dont add ps to same ps !');
// end;
 if PS=nil then begin
   raise Exception.Create('Nil Propstore ? !!!');
 end;
 if PL_Find(PLPList,name,idx) then begin // Find the list for the list
   result:=PMrepObj(_PLPList[idx])^.PList.Add(PS);  // Add the PS to the end of the list (A Normal Unsorted LIST !);
 end else begin // create a List and add to the Beginning
  L:=TMList.Create;
  result:=L.Add(PS);
  New(MRepObj);
  MRepObj^.Name:=Name;
  MRepObj^.PList:=L;
  _PLPList.Insert(idx,MRepObj);
 end;
end;


procedure TFOS_NPS.ChangePropListPositons(const name: ansistring; const idx1,  idx2: integer);
var i,last:integer;
begin
 if PL_Find(PLPList,name,i) then begin // Find the list for the list
  last:=PMRepObj(_PLPList[i])^.PList.Count;
  if idx1>=last then begin
   raise Exception.Create('proplist index1 out of bounds ('+inttostr(idx1)+') from ('+inttostr(last)+')');
  end;
  if idx2>=last then begin
   raise Exception.Create('proplist index2 out of bounds ('+inttostr(idx1)+') from ('+inttostr(last)+')');
  end;
  PMRepObj(_PLPList[i])^.PList.Exchange(idx1,idx2); // Exchange it
 end else begin
   raise Exception.Create('proplist doesnt exist');
 end;
end;


function TFOS_NPS.CheckOutNPS(const name: ansistring;  out PS: IFOS_NPS): TFOS_PropType;
var Node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptPS);
  if result=ptNotFound then begin
   PS:=nil;
   exit;
  end else begin
   _PList.Delete(node^.k);
   PS := IFOS_NPS(node^.val.T_PS);
  end;
end;


function TFOS_NPS.CheckOutObj(const name: ansistring; out Obj: TObject): TFOS_PropType;
var Node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptPS);
  if result=ptNotFound then begin
   Obj:=nil;
   exit;
  end else begin
   _PList.Delete(node^.k);
   Obj := node^.val.T_Object;
  end;
end;

procedure TFOS_NPS.CheckOutPropListEntryPL(const name: ansistring; const idx: integer; out PL: IFOS_NPS);
begin
 GetPropListEntryPL(name,idx,PL);
 PMRepObj(_PLPList[idx])^.PList.Delete(idx); // Drop and Free it if necessary
end;

procedure TFOS_NPS.Clear;
begin
 _CleanUp;
end;

function TFOS_NPS.ClearPropList(const name: ansistring): boolean;
var idx:integer;
begin
 if PL_Find(PLPList,name,idx) then begin // Find the list for the list
  _PLPList.Delete(idx);
  result:=true;
 end else begin // create a List and add to the Beginning
  result:=false;
 end;
end;

constructor TFOS_NPS.Create;
begin
 inherited Create;
 Compression:=ncl_NONE;
end;

function TFOS_NPS.DelProp(const name: ansistring): TFOS_PropType;
var val:RFOS_PROPERTY;
begin
 result:=ptNotFound;
 if not assigned(_PList) then begin
  exit;
 end;
 val:=_PList.Delete(name);
 result:=val.PropType;
 if result<>ptNotFound then begin
   _CleanOldProp(val);
 end;
end;

destructor TFOS_NPS.Destroy;
begin
 _CleanUp;
 _PList.Free;_PList:=nil;
 _PLPList.Free;_PLPList:=nil;
 inherited Destroy;
end;


function TFOS_NPS.Find(const PL: TLList; const S: ansistring;  out Node: TFOS_PROP_RB_NodeP): Boolean;
begin
 result:=PL.Find(s,Node);
end;

{
function TFOS_NPS.FindFirst(out found_name: string;  out value: variant;out proptype:TFOS_PropType): boolean;
out val:RFOS_PROPERTY;
begin
 result:=FindFirst(found_name,val);
 proptype:=val.PropType;
 value:=_ValToVariant(val);
end;
}

function TFOS_NPS.FindFirst(out found_name: ansistring;  out value: RFOS_PROPERTY): boolean;
begin
 result:=false;
 if not assigned(PList) then exit;
 if assigned(_PList.leftmost) then begin
  result:=true;
  found_name:=_PList.leftmost^.k;
  value:=_PList.leftmost^.val;
 end else begin
  result:=false;
  found_name:='';
  value.PropType:=ptNotFound;
  value.T_Int:=0;
 end;
end;

function TFOS_NPS.FindLast(out found_name: ansistring;  out value: RFOS_PROPERTY): boolean;
begin
 result:=false;
 if not assigned(PList) then exit;
 if assigned(_PList.rightmost) then begin
  result:=true;
  found_name:=_PList.rightmost^.k;
  value:=_PList.rightmost^.val;
 end else begin
  result:=false;
  found_name:='';
  value.PropType:=ptNotFound;
  value.T_Int:=0;
 end;
end;

{
function TFOS_NPS.FindNxt(const name: String;out found_name:string;out value: variant;out proptype:TFOS_PropType): boolean;
out val:RFOS_PROPERTY;
begin
 result:=FindNxtPrv(name,found_name,val,true);
 proptype:=val.PropType;
 value:=_ValToVariant(val);
end;
}

{
function TFOS_NPS.FindPrv(const name: String;out found_name:string; out value: variant;out proptype:TFOS_PropType): boolean;
out val:RFOS_PROPERTY;
begin
 result:=FindNxtPrv(name,found_name,val,false);
 proptype:=val.PropType;
 value:=_ValToVariant(val);
end;
}

function TFOS_NPS.FindProp(const name: ansistring):TFOS_PropType;
var Node:TFOS_PROP_RB_NodeP;
begin
  result:=_FindGet(name,node,ptNotFound);
end;



function TFOS_NPS.GetPList: TLList;
begin
 if not assigned(_PList) then begin
  _PList:=TLList.Create;
 end;
 result:=_PList;
end;

function TFOS_NPS.GetPLPList: TPLPList;
begin
 if not assigned(_PLPList) then begin
  _PLPList:=TPLPList.Create;
  _PLPList.OwnsObjects:=true;
 end;
 result:=_PLPList;
end;


{
function TFOS_NPS.FindLast(out found_name: string; out value: variant;out proptype:TFOS_PropType): boolean;
out val:RFOS_PROPERTY;
begin
 result:=FindLast(found_name,val);
 proptype:=val.PropType;
 value:=_ValToVariant(val);
end;
}

{ TPLPList }

procedure TPLPList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if OwnsObjects then begin
    case Action of
      lnDeleted:begin
                 PMRepObj(Ptr)^.Name:='';
                 PMRepObj(Ptr)^.PList.Free;
                 Dispose(PMRepObj(Ptr));
                end;
     end;
  end;
  inherited Notify(Ptr, Action);
end;

{ TMList }

{
procedure TMList.Notify(Ptr: Pointer; Action: TListNotification);
begin
    case Action of
      lnDeleted:begin
                 if not DontFree then begin
                  TFOS_NPS(Ptr).Free;
                 end;
                end;
     end;
  inherited;
end;
}

{ TFOS_PropTree }

function TFOS_PropTree.Add(key: ansistring): TFOS_PROP_RB_NodeP;
var x, y, z, zpp: TFOS_PROP_RB_NodeP;
    cmp: LongInt;

 function DoLoop:Boolean;
 begin
  result:=false;
  if (z <> root) then
   if (z^.parent^.col = R) then result:=true;
 end;

 function FuncRight:Boolean;
 begin
  result:=false;
  if (y <> nil) then
   if (y^.col = R) then result:=true;
 end;

begin
  z := New(TFOS_PROP_RB_NodeP);
  { Initialize fields in new node z }
  z^.k := key;
  z^.left := nil;
  z^.right := nil;
  z^.col := R;

  { Maintain leftmost and rightmost nodes }
  if (leftmost = nil) then begin
    leftmost := z;
  end else
  if (_CompareStr(key, leftmost^.k) < 0) then begin
    leftmost := z;
  end;
  if (rightmost = nil) then begin
    rightmost := z;
  end else
  if (_CompareStr(key, rightmost^.k) > 0) then begin
    rightmost := z;
  end;
  { Insert node z }
  y := nil;
  x := root;
  while (x <> nil) do begin
    y := x;
    cmp := _CompareStr(key, x^.k);
    if (cmp < 0) then begin
      x := x^.left;
    end else if (cmp > 0) then begin
      x := x^.right;
    end else begin
      { val already exists in tree. }
      // This case does not occur in Propstore usage
      // Please do a check - If Leftmost and Rightmost are safe
      Dispose(z);
      result:=x; //Return Old Value
      exit;
    end;
  end;
  z^.parent := y;
  if (y = nil) then begin
    root := z;
  end else if (_CompareStr(key, y^.k) < 0) then begin
    y^.left := z;
  end else begin
    y^.right := z;
  end;
  result:=z;
  { Rebalance tree }
  while DoLoop do begin
    zpp := z^.parent^.parent;
    if (z^.parent = zpp^.left) then begin
      y := zpp^.right;
      if FuncRight then begin
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
      if FuncRight then begin
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
  end;
  root^.col := B;
  inc(cnt); // Only inc Count on addition
end;

procedure TFOS_PropTree.Clear;
 procedure fast_erase(x: TFOS_PROP_RB_NodeP);
 begin
   if (x^.left <> nil) then  fast_erase(x^.left);
   if (x^.right <> nil) then fast_erase(x^.right);
   DoFreeItem(x^.val);
   dispose(x);
   dec(cnt);
 end;
begin
  if (root <> nil) then fast_erase(root);
  root := nil;
  leftmost := nil;
  rightmost := nil;
end;

function TFOS_PropTree.Count: LongInt;
begin
 result:=cnt;
end;

constructor TFOS_PropTree.Create;
begin
 inherited;
 root := nil;
 leftmost := nil;
 rightmost := nil;
 cnt:=0;
end;

function TFOS_PropTree.Delete(key: ansistring): RFOS_PROPERTY;
var n:TFOS_PROP_RB_NodeP;
begin
 n:=_Find(key);
 if not assigned(n) then begin
  result.PropType:=ptNotFound;
  result.T_Int:=0;
 end else begin
  result:=n^.val;
  _Delete(n);
 end;
end;

destructor TFOS_PropTree.Destroy;
begin

  inherited;
end;

procedure TFOS_PropTree.DoFreeItem(const val: RFOS_PROPERTY);
begin
 _CleanOldProp(val);
end;

function TFOS_PropTree.Find(key: ansistring): RFOS_PROPERTY;
var node:TFOS_PROP_RB_NodeP;
begin
 result.PropType:=ptNotFound;
 Find(key,node);
 if assigned(node) then begin
  result:=node^.val;
 end;
end;


function TFOS_PropTree.Find(key: ansistring; out node: TFOS_PROP_RB_NodeP): Boolean;
begin
 node:=_Find(key);
 if assigned(node) then begin
  result:=true;
 end else begin
  result:=false;
 end;
end;

function TFOS_PropTree.Max(const x: TFOS_PROP_RB_NodeP): TFOS_PROP_RB_NodeP;
begin
  Result := x;
  while (Result^.right <> nil) do Result := Result^.right;
end;

function TFOS_PropTree.Min(const x: TFOS_PROP_RB_NodeP): TFOS_PROP_RB_NodeP;
begin
  Result := x;
  while (Result^.left <> nil) do Result := Result^.left;
end;

procedure TFOS_PropTree.RBDec(var x: TFOS_PROP_RB_NodeP);
var
  y: TFOS_PROP_RB_NodeP;
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

procedure TFOS_PropTree.RBInc(var x: TFOS_PROP_RB_NodeP);
var
  y: TFOS_PROP_RB_NodeP;
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

procedure TFOS_PropTree.RotLeft(const x: TFOS_PROP_RB_NodeP);
var
  y: TFOS_PROP_RB_NodeP;
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

procedure TFOS_PropTree.RotRight(const x: TFOS_PROP_RB_NodeP);
var
  y: TFOS_PROP_RB_NodeP;
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



procedure TFOS_PropTree._Delete(z: TFOS_PROP_RB_NodeP);
var
  w, x, y, x_parent: TFOS_PROP_RB_NodeP;
  tmpcol: TFOS_RB_NodeColor;

  function DoLoop:Boolean;
  begin
   result:=false;
   if (x <> root) then begin
    if (x = nil) then result:=true
    else if (x^.col = B) then result:=true;
   end;
  end;

  function FuncRight:Boolean;
  begin
   result:=false;
   if (w^.right = nil) then result:=true
   else if (w^.right^.col = B) then result:=true;
  end;

  function FuncLeft:Boolean;
  begin
   result:=false;
   if (w^.left = nil) then result:=true
   else if (w^.left^.col = B) then result:=true;
  end;

begin
  y := z;
  x := nil;
//  x_parent := nil;

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
    while DoLoop do begin
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
            if FuncRight then begin
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
            x := root; { break; }
         end
      end else begin
        { same as above, with right <^. left. }
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
          if FuncLeft then begin
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
          x := root; { break; }
        end;
      end;
    end;
    if (x <> nil) then begin
      x^.col := B;
    end;
  end;
  dispose(y);
  dec(cnt)
end;

function TFOS_PropTree._Find(key: ansistring): TFOS_PROP_RB_NodeP;
var cmp: LongInt;
    node: TFOS_PROP_RB_NodeP;
begin
  result:=nil;
  node := root;
  while (node <> nil) do begin
    cmp := _CompareStr(node^.k, key);
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

function TFOS_PropTree._FindNextPrev(key: ansistring; const next: boolean): TFOS_PROP_RB_NodeP;
var cmp: integer;
    node: TFOS_PROP_RB_NodeP;
begin
  result:=nil;
  node := root;
  while true do begin
    if node=nil then exit;
    cmp := _CompareStr(node^.k, key);
    if cmp < 0 then begin
      if node^.right<>nil then begin
       node := node^.right;
      end else begin
       result:=node;
       if not next then exit;
       break;
      end;
    end else if cmp > 0 then begin
      if node^.left <>nil then begin
       node := node^.left;
      end else begin
       result:=node;
       if next then exit;
       break;
      end;
    end else begin
      if (node^.left=nil) and (node^.right=nil) and (node^.parent=nil) then begin
       result:=nil;
       exit;
      end else begin
       result:=node;
       break;
      end;
    end;
  end;
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


initialization
 assert(sizeof(RFOS_PROPERTY)=12,'RFOS_PROPERTY_SIZE 12<>'+inttostr(sizeof(RFOS_PROPERTY)));
end.
