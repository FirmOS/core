unit fre_db_persistance_common;

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

//BIG TODO : Remove Fillbytes and verify ART TREE FUNCTION

interface

uses
  Classes, SysUtils,FRE_DB_COMMON,FRE_DB_INTERFACE,FRE_DB_CORE,FOS_ARRAYGEN,FOS_GENERIC_SORT,FOS_TOOL_INTERFACES,FOS_AlignedArray,FOS_REDBLACKTREE_GEN,
  fos_art_tree,fos_sparelistgen;

type

  { TFRE_DB_IndexValueStore }

  TFRE_DB_IndexValueStore=class
  private
    FOBJArray : Array of TFRE_DB_Object;
    procedure InternalCheck;
  public
    function  Exists          (const guid : TGUID) : boolean;
    function  Add             (const  obj : TFRE_DB_Object) : boolean;
    function  IndexedObjects  : TFRE_DB_ObjectArray;
    procedure StreamToThis    (const stream:TStream);
    procedure LoadFromThis    (const stream:TStream ; const coll: IFRE_DB_PERSISTANCE_COLLECTION);
  end;

  { TFRE_DB_MM_Index }

  TFRE_DB_MM_Index=class
  protected
    FIndex           : TFRE_ART_TREE;
    FIndexName       : TFRE_DB_NameType;
    FUniqueName      : TFRE_DB_NameType;
    FFieldname       : TFRE_DB_NameType;
    FUniqueFieldname : TFRE_DB_NameType;
    FFieldType       : TFRE_DB_FIELDTYPE;
    FUnique          : Boolean;
    FCollection      : IFRE_DB_PERSISTANCE_COLLECTION;
    //transient data
    transkey        : Array [0..CFREA_maxKeyLen] of Byte;
    transkeylen     : NativeInt;
    updtranskey     : Array [0..CFREA_maxKeyLen] of Byte;
    updtranskeylen  : NativeInt;
    FUpdateKeySame  : boolean;
    //transient data end
    function       GetStringRepresentationOfTransientKey : String;
    procedure      SetTranformedKeyDBS               (const value : TFRE_DB_String ; const update_key : boolean); virtual ;
    function       FetchIndexedValsTransformedKey    (var obj : TFRE_DB_ObjectArray):boolean;
    procedure      TransformToBinaryComparable       (fld:TFRE_DB_FIELD ; const update_key : boolean); virtual; abstract;
    function       CompareTransformedKeyAndUpdateKey : boolean;
    procedure      StreamToThis                      (const stream: TStream);virtual;
    procedure      StreamIndex                       (const stream: TStream);virtual;
    procedure      LoadIndex                         (const stream: TStream ; const coll : IFRE_DB_PERSISTANCE_COLLECTION);virtual;
    class function CreateFromStream                  (const stream: TStream ; const coll : IFRE_DB_PERSISTANCE_COLLECTION):TFRE_DB_MM_Index;
  public
    constructor Create                               (const idx_name,fieldname: TFRE_DB_NameType ; const fieldtype : TFRE_DB_FIELDTYPE ; const unique : boolean ; const collection : IFRE_DB_PERSISTANCE_COLLECTION);
    function    Indexname                            : TFRE_DB_NameType;
    function    Uniquename                           : PFRE_DB_NameType;
    function    FieldTypeIndexCompatCheck            (fld:TFRE_DB_FIELD ; const raise_ex : boolean) : TFRE_DB_Errortype; virtual; abstract;
    function    IndexAddCheck                        (const obj             : TFRE_DB_Object; const check_only : boolean ; const raise_ex: boolean ; const use_already_transformed_key:boolean): TFRE_DB_Errortype; virtual;
    function    IndexUpdCheck                        (const new_obj,old_obj : TFRE_DB_Object; const check_only : boolean ; const raise_ex: boolean ; const use_already_transformed_key:boolean): TFRE_DB_Errortype; virtual;
    function    IndexDelCheck                        (const obj             : TFRE_DB_Object; const check_only : boolean ; const raise_ex: boolean ; const use_already_transformed_key:boolean): TFRE_DB_Errortype; virtual;
    function    SupportsDataType                     (const typ : TFRE_DB_FIELDTYPE):boolean; virtual ; abstract;
    function    IsUnique                             : Boolean;
  end;


  { TFRE_DB_UnsignedIndex }

  TFRE_DB_UnsignedIndex=class(TFRE_DB_MM_Index)
    constructor CreateStreamed              (const stream : TStream ; const idx_name, fieldname: TFRE_DB_NameType ; const fieldtype : TFRE_DB_FIELDTYPE ; const unique : boolean ; const collection : IFRE_DB_PERSISTANCE_COLLECTION);
  end;

  { TFRE_DB_SignedIndex }

  TFRE_DB_SignedIndex=class(TFRE_DB_MM_Index)
    constructor CreateStreamed              (const stream : TStream ; const idx_name, fieldname: TFRE_DB_NameType ; const fieldtype : TFRE_DB_FIELDTYPE ; const unique : boolean ; const collection : IFRE_DB_PERSISTANCE_COLLECTION);
  end;

  { TFRE_DB_TextIndex }

  TFRE_DB_TextIndex=class(TFRE_DB_MM_Index) //TODO Unicode Key Conversion
  private
    FCaseInsensitive : Boolean;
  protected
    procedure   SetTranformedKeyDBS         (const value : TFRE_DB_String ; const update_key : boolean); override ;
    procedure   StreamToThis                (const stream: TStream);override;
  public
    constructor Create                      (const idx_name,fieldname: TFRE_DB_NameType ; const fieldtype : TFRE_DB_FIELDTYPE ; const unique, case_insensitive : boolean ; const collection : IFRE_DB_PERSISTANCE_COLLECTION);
    constructor CreateStreamed              (const stream : TStream ; const idx_name, fieldname: TFRE_DB_NameType ; const fieldtype : TFRE_DB_FIELDTYPE ; const unique : boolean ; const collection : IFRE_DB_PERSISTANCE_COLLECTION);
    function    FieldTypeIndexCompatCheck   (fld:TFRE_DB_FIELD ; const raise_ex : boolean) : TFRE_DB_Errortype; override;
    procedure   TransformToBinaryComparable (fld:TFRE_DB_FIELD ; const update_key : boolean); override;
    function    SupportsDataType            (const typ: TFRE_DB_FIELDTYPE): boolean; override;
  end;

  { TFRE_DB_Persistance_Collection }

  TFRE_DB_Persistance_Collection=class(TObject,IFRE_DB_PERSISTANCE_COLLECTION,IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER)
  private
    FName         : TFRE_DB_NameType;
    FUpperName    : TFRE_DB_NameType;
    FLayer        : IFRE_DB_PERSISTANCE_LAYER;
    FVolatile     : Boolean;
    FGuidObjStore : TFRE_ART_TREE;
    FIndexStore   : array of TFRE_DB_MM_INDEX;
    dummy         : PtrUInt;
    FPrepared     : Boolean;

    function      IsVolatile         : boolean;

    function      IndexExists      (const idx_name : TFRE_DB_NameType):NativeInt;
    procedure     AddIndex         (const idx : TFRE_DB_MM_Index);

    function      IndexAddCheck    (const obj              : TFRE_DB_Object;const check_only : boolean ; const raise_ex : boolean ; const use_already_transformed_key:boolean) : TFRE_DB_Errortype;
    function      IndexUpdCheck    (const new_obj, old_obj : TFRE_DB_Object;const check_only : boolean ; const raise_ex : boolean ; const use_already_transformed_key:boolean) : TFRE_DB_Errortype;
    function      IndexDelCheck    (const del_obj          : TFRE_DB_Object;const check_only : boolean ; const raise_ex : boolean ; const use_already_transformed_key:boolean) : TFRE_DB_Errortype;

    function      StoreInThisColl     (const new_obj         : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;
    function      UpdateInThisColl    (const new_obj,old_obj : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;
    function      DeleteFromThisColl  (const del_obj : TFRE_DB_Object ; const raise_ex : boolean ; const checkphase : boolean):TFRE_DB_Errortype;

    function      CloneOutObject   (const inobj:TFRE_DB_Object):TFRE_DB_Object;
    function      CloneOutArray    (const objarr : TFRE_DB_ObjectArray):TFRE_DB_ObjectArray;

    procedure     StreamToThis     (const stream : TStream);
    procedure     LoadFromThis     (const stream : TStream);

    function      _GetIndexedObj   (const query_value : TFRE_DB_String   ; out   obj       : TFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false ; const clone_out : boolean=true):boolean; overload ;
    procedure     InternalUnprepare;
    function      FetchIntFromColl    (const uid:TGuid ; var obj : TFRE_DB_Object):boolean;
  public
    function    CollectionName     (const unique:boolean):TFRE_DB_NameType;
    function    GetPersLayerIntf   : IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
    function    UniqueName         : PFRE_DB_NameType;
    constructor Create             (const coll_name: TFRE_DB_NameType; Volatile: Boolean; const pers_layer: IFRE_DB_PERSISTANCE_LAYER);
    function    Count              : int64;
    function    Exists             (const ouid: TGUID): boolean;
    //function    Delete             (const ouid: TGUID): boolean;
    //function    Remove             (const ouid : TGUID ; out old_obj : TFRE_DB_Object) : boolean;
    //function    AddCheck           (const new_guid :TGuid ; const obj :TFRE_DB_Object) : boolean;
    procedure   Clear              ; // Clear Store but dont free
    procedure   ForAllItems        (const iter    : TFRE_DB_Obj_Iterator); // must allow modification of collection // clones out
    function    ForAllitemsBreak   (const func    : TFRE_DB_Obj_IteratorBreak):boolean;

    function    Store              (var   new_obj : TFRE_DB_Object ; const raise_ex : boolean=true ; var ncolls : TFRE_DB_StringArray=nil):TFRE_DB_Errortype;
    function    Delete             (const ouid    : TGUID          ; const raise_ex : boolean=true ; var ncolls : TFRE_DB_StringArray=nil):TFRE_DB_Errortype;

    function    Fetch              (const uid:TGUID ; var obj : TFRE_DB_Object) : boolean;
    function    LinearScan         (const fieldname: TFRE_DB_NameType;  const field_expr: TFRE_DB_FIELD_EXPRESSION): TFRE_DB_Object;
    function    First              : TFRE_DB_Object;
    function    Last               : TFRE_DB_Object;
    function    GetItem            (const num:uint64) : TFRE_DB_Object;
    function    DefineIndexOnField (const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType): TFRE_DB_Errortype;
    function    GetIndexedObj      (const query_value : TFRE_DB_String   ; out   obj       : TFRE_DB_Object      ; const index_name : TFRE_DB_NameType='def'):boolean; // for the string fieldtype
    function    GetIndexedObj      (const query_value : TFRE_DB_String   ; out   obj       : TFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean; overload ;
    function    GetIndexedUID      (const query_value: TFRE_DB_String    ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def'): boolean;
    function    GetIndexedUID      (const query_value: TFRE_DB_String    ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean; overload ;

    function    CheckFieldChangeAgainstIndex (const oldfield,newfield : TFRE_DB_FIELD ; const change_type : TFRE_DB_ObjCompareEventType ; const check : boolean ; const raise_ex: boolean) :TFRE_DB_Errortype;
  end;

  { TFRE_DB_CollectionTree }

  { TFRE_DB_CollectionManageTree }

  TFRE_DB_PersColl_Iterator = function(const coll:IFRE_DB_PERSISTANCE_COLLECTION):boolean is nested;


  TFRE_DB_CollectionManageTree = class
  private
    FCollTree : TFRE_ART_TREE;
    dummy     : PtrUInt;
  public
    constructor Create;
    function    NewCollection     (const coll_name : TFRE_DB_NameType ; out Collection:IFRE_DB_PERSISTANCE_COLLECTION ; const volatile_in_memory:boolean ; const pers_layer:IFRE_DB_PERSISTANCE_LAYER) : TFRE_DB_Errortype;
    function    DeleteCollection  (const coll_name : TFRE_DB_NameType):TFRE_DB_Errortype;
    function    GetCollection     (const coll_name : TFRE_DB_NameType ; out Collection:IFRE_DB_PERSISTANCE_COLLECTION) : boolean;
    procedure   ForAllCollections (const iter : TFRE_DB_PersColl_Iterator);
  end;

  //TFRE_DB_REF_TYPE=(fredb_REFOUTBOUND,fredb_REFINBOUND);

  RFRE_DB_GUID_RefLink_Out_Key = packed record
    GUID      : Array [0..15] of Byte;
    RefTyp    : Byte;  // 17 Bytes // Outlink = $99 // Inlink= $AA
    FieldName : Array [0..62] of Byte;
  end;
  PFRE_DB_GUID_RefLink_Out_Key = ^RFRE_DB_GUID_RefLink_Out_Key;

  RFRE_DB_GUID_RefLink_In_Key = packed record
    GUID            : Array [0..15] of Byte;
    RefTyp          : Byte;
    FromGuid        : Array [0..15] of Byte; // 25 Bytes // Outlink = $99 // Inlink= $AA
    FromFieldScheme : Array [0..127] of Byte;
  end;
  PFRE_DB_GUID_RefLink_In_Key = ^RFRE_DB_GUID_RefLink_In_Key;


  { TREF_LinkEncapsulation }

  TREF_LinkEncapsulation=class(Tobject)
  private
    FLinks : TFRE_DB_GUIDArray;
  public
    constructor Create (const links : TFRE_DB_GUIDArray);
    function    Links  : TFRE_DB_GUIDArray;
  end;

  { TFRE_DB_Master_Data }

  TFRE_DB_Master_Data=class(TObject)
  private
    FMasterPersistantObjStore  : TFRE_ART_TREE;
    FMasterVolatileObjStore    : TFRE_ART_TREE;
    FMasterRefLinks            : TFRE_ART_TREE;
    FMasterCollectionStore     : TFRE_DB_CollectionManageTree;

    function     GetOutBoundRefLinks        (const from_obj : TGUID): TFRE_DB_ObjectReferences;
    function     GetInboundLinks            (const to_obj   : TGUID): TFRE_DB_ObjectReferences;
    function     GetOutBoundRefLinksCount   (const from_obj : TGUID): NativeInt;
    function     GetInboundLinksCount       (const to_obj   : TGUID): NativeInt;


    function     __CheckReferenceToList     (const obj: TFRE_DB_Object; fieldname: TFRE_DB_NameType; list: TFRE_DB_GUIDArray; const raise_ex: boolean): TFRE_DB_Errortype;
    function     _SetupInitialRefLink       (const from_key : TFRE_DB_Object ; const fieldname: TFRE_DB_NameType ; const references_to_list : TFRE_DB_GUIDArray ; const raise_ex : boolean):TFRE_DB_Errortype;
    function     _ChangeRefLink             (const from_obj : TFRE_DB_Object ; const fieldname: TFRE_DB_NameType ; const references_to_list : TFRE_DB_GUIDArray ; const raise_ex : boolean):TFRE_DB_Errortype;
    function     _SetupInitialRefLinks      (const from_key : TFRE_DB_Object ; const references_to_list : TFRE_DB_ObjectReferences ; const raise_ex : boolean):TFRE_DB_Errortype;
    function     _UpdateRefLinks            (const key : TGUID ; const references_to_list : TFRE_DB_ObjectReferences ; const raise_ex : boolean):TFRE_DB_Errortype;
    // Check full referential integrity, check if to objects exist
    function     _CheckRefIntegrityToLink   (const obj:TFRE_DB_Object ; var ref_array : TFRE_DB_ObjectReferences ; const raise_ex : boolean):TFRE_DB_Errortype;

    // Remove a reflinkfield, delete refence to and from indexes
    function     _RemoveRefLinkFieldDelRefs (const obj:TFRE_DB_Object ; field : TFRE_DB_FIELD ; const raise_ex : boolean ; const check_only : boolean):TFRE_DB_Errortype;

  public
    function     InternalStoreObjectFromStable (const obj : TFRE_DB_Object) : TFRE_DB_Errortype;
    function     InternalRebuildRefindex                                    : TFRE_DB_Errortype;
    procedure    InternalStoreLock                                          ;

    constructor Create;
    function    GetReferenceCount     (const obj_uid: TGuid; const from: boolean): NativeInt;
    function    GetReferences         (const obj_uid: TGuid ; const from: boolean): TFRE_DB_ObjectReferences;
    function    ExistsObject          (const obj_uid : TGuid ) : Boolean;
    function    FetchObject           (const obj_uid : TGuid ; var obj : TFRE_DB_Object ; const internal_obj : boolean) : boolean;
    function    StoreObject           (const obj     : TFRE_DB_Object ; const raise_ex : boolean ; const check_only : boolean) : TFRE_DB_Errortype;
    function    DeleteObject          (const obj_uid : TGuid ; const raise_ex : boolean ; const check_only : boolean )         : TFRE_DB_Errortype;
    procedure   ForAllObjectsInternal (const pers,volatile:boolean ; const iter:TFRE_DB_Obj_Iterator); // No Clone
    function    MasterColls           : TFRE_DB_CollectionManageTree;
  end;

  { TFRE_DB_ChangeStep }

  TFRE_DB_ChangeStep=class
  protected
    FIsInsert : Boolean;
  public
    function   IsInsert               : Boolean;
    function   DescribeText           : String ; virtual; abstract;
    function   CheckExistence          (const master : TFRE_DB_Master_Data ; const raise_ex:boolean): TFRE_DB_Errortype; virtual;
    procedure  WriteToWAL             (const m:TMemoryStream); virtual ; abstract;
    procedure  UnprepareCollection    ; virtual;
    function   StoreInCollectionCheck (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; virtual ; abstract;
    function   MasterStore            (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; virtual ; abstract;
  end;

  { TFRE_DB_InsertStep }

  TFRE_DB_InsertStep=class(TFRE_DB_ChangeStep)
  private
    FNewObj   : TFRE_DB_Object;
    FColl     : IFRE_DB_PERSISTANCE_COLLECTION;
  public
    constructor Create                  (new_obj : TFRE_DB_Object ; const coll:IFRE_DB_PERSISTANCE_COLLECTION ; const is_insert : boolean);
    destructor  Destroy                 ; override;
    function    DescribeText            : String; override;
    function    IsARootInsert           : Boolean;
    function    CheckExistence          (const master : TFRE_DB_Master_Data ; const raise_ex:boolean): TFRE_DB_Errortype;
    function    StoreInCollectionCheck  (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; override;
    function    MasterStore             (const master : TFRE_DB_Master_Data ; const raise_ex:boolean ; const check : boolean): TFRE_DB_Errortype; override;
    procedure   WriteToWAL              (const m:TMemoryStream);override;
    procedure   UnprepareCollection     ; override;
  end;

  { TFRE_DB_DeleteStep }

  TFRE_DB_DeleteStep=class(TFRE_DB_ChangeStep)
  private
    FDelObj  : TFRE_DB_Object;
    CollName : TFRE_DB_NameType;
    FObjPtr  : ^TFRE_DB_Object;
  public
    constructor Create                   (var del_obj : TFRE_DB_Object ; const is_insert : boolean);
    function    DescribeText             : String; override;
    procedure   WriteToWAL               (const m:TMemoryStream) ; override;
    function    StoreInCollectionCheck   (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; override;
    function    MasterStore              (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; override;
  end;

  TFRE_DB_UpdateStep=class;
  { TFRE_DB_UpdateStep }

  RFRE_DB_UpdateSubStep=record
    updtyp   : TFRE_DB_ObjCompareEventType;
    colllist : Array of TFRE_DB_NameType;
    newfield : TFRE_DB_FIELD;
    oldfield : TFRE_DB_FIELD;
    Step     : TFRE_DB_UpdateStep;
  end;

  TFRE_DB_UpdateStep=class(TFRE_DB_ChangeStep)
  public
    FSublist    : Array of RFRE_DB_UpdateSubStep;
    FCnt        : NativeInt;
    upobj       : TFRE_DB_Object;
    to_upd_obj  : TFRE_DB_Object;
  public
    constructor Create                   (obj,to_update_obj : TFRE_DB_Object ; const is_insert : boolean);
    procedure   AddSubStep               (const uptyp : TFRE_DB_ObjCompareEventType ; const new,old : TFRE_DB_FIELD);
    function    DescribeText             : String; override;
    function    HasNoChanges             : Boolean;
    procedure   WriteToWAL               (const m:TMemoryStream);override;
    function    StoreInCollectionCheck   (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; override;
    function    MasterStore              (const master : TFRE_DB_Master_Data ; const raise_ex: boolean ; const check : boolean): TFRE_DB_Errortype; override;
  end;

  OFRE_SL_TFRE_DB_ChangeStep  = specialize OFOS_SpareList<TFRE_DB_ChangeStep>;

  { TFRE_DB_TransactionalUpdateList }

  TFRE_DB_TransactionalUpdateList = class(TObject)
  private
    FChangeList  : OFRE_SL_TFRE_DB_ChangeStep;
    FChangeCount : NativeInt;
    FTransId     : TFRE_DB_NameType;
    FMaster      : TFRE_DB_Master_Data;
    FLayer       : IFRE_DB_PERSISTANCE_LAYER;
    FWalMem      : TMemoryStream;
    function     ProcessCheck          (const raise_ex:boolean):TFRE_DB_Errortype;
  public
    constructor  Create                (const TransID : TFRE_DB_NameType ; const master_data : TFRE_DB_Master_Data ; const layer : IFRE_DB_PERSISTANCE_LAYER);
    procedure    AddChangeStep         (const step:TFRE_DB_ChangeStep);
    procedure    PrintTextLog          ;
    //function     ProcessForInsert      (const raise_ex:boolean):TFRE_DB_Errortype;
    //function     ProcessForUpdate      (const raise_ex:boolean):TFRE_DB_Errortype;
    procedure    Write_WAL             ;
    function     Commit                (const raise_ex:boolean):TFRE_DB_Errortype;
    function     Rollback              (const raise_ex:boolean):TFRE_DB_Errortype;
  end;




  //var
  //    GSYS_COLLS  : TFRE_DB_CollectionManageTree;

implementation

{ TFRE_DB_SignedIndex }

constructor TFRE_DB_SignedIndex.CreateStreamed(const stream: TStream; const idx_name, fieldname: TFRE_DB_NameType; const fieldtype: TFRE_DB_FIELDTYPE; const unique: boolean; const collection: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  abort;
end;

{ TFRE_DB_UnsignedIndex }

constructor TFRE_DB_UnsignedIndex.CreateStreamed(const stream: TStream; const idx_name, fieldname: TFRE_DB_NameType; const fieldtype: TFRE_DB_FIELDTYPE; const unique: boolean; const collection: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  abort;
end;

{ TFRE_DB_ChangeStep }

function TFRE_DB_ChangeStep.IsInsert: Boolean;
begin
  result := FIsInsert;
end;

function TFRE_DB_ChangeStep.CheckExistence(const master: TFRE_DB_Master_Data; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  result := edb_OK;
end;

procedure TFRE_DB_ChangeStep.UnprepareCollection;
begin

end;

{ TREF_LinkEncapsulation }

constructor TREF_LinkEncapsulation.Create(const links: TFRE_DB_GUIDArray);
begin
  FLinks := Copy(links);
end;

function TREF_LinkEncapsulation.Links: TFRE_DB_GUIDArray;
begin
  result := Copy(FLinks);
end;


{ TFRE_DB_UpdateStep }

constructor TFRE_DB_UpdateStep.Create(obj, to_update_obj: TFRE_DB_Object; const is_insert: boolean);
begin
  SetLength(FSublist,25);
  FCnt          := 0;
  upobj         := obj;
  to_upd_obj    := to_update_obj;
  FIsInsert     := is_insert;
end;

procedure TFRE_DB_UpdateStep.AddSubStep(const uptyp: TFRE_DB_ObjCompareEventType; const new, old: TFRE_DB_FIELD);
begin
  if FCnt>=Length(FSublist) then
   SetLength(FSublist,Length(FSublist)+25);
  with FSublist[fcnt] do
    begin
      updtyp   := uptyp;
      newfield := new;
      oldfield := old;
      Step     := self;
    end;
  inc(fcnt);
end;

function TFRE_DB_UpdateStep.DescribeText: String;
var i : NativeInt;
    s : string;
begin
  result := ' UPDATE OBJECT ['+upobj.SchemeClass+' | '+upobj.UID_String+'] '+LineEnding;
  for i:=0 to FCNT-1 do
    with FSublist[i] do
      case updtyp of
        cev_FieldDeleted:
          begin
            WriteStr(s,'  ',i:4,'>DELETE FIELD : ',newfield.FieldName,' ',newfield.AsString,LineEnding);
            result := result +s;
          end;
        cev_FieldAdded:
          begin
            WriteStr(s,'  ',i:4,'>ADDED FIELD : ',newfield.FieldName,' : ',newfield.AsString,LineEnding);
            result := result +s;
          end;
        cev_FieldChanged:
          begin
            WriteStr(s,'  ',i:4,'>FIELD CHANGE : ',oldfield.FieldName,' ',oldfield.AsString,' => ',newfield.AsString,LineEnding);
            result := result +s;
          end;
      end;
end;

function TFRE_DB_UpdateStep.HasNoChanges: Boolean;
begin
  result := FCnt=0;
end;

procedure TFRE_DB_UpdateStep.WriteToWAL(const m: TMemoryStream);
var nsize,usize,wsize : NativeInt;
begin
  if upobj.IsVolatile then
    exit;
  m.WriteAnsiString('SU'+upobj.UID_String+to_upd_obj.UID_String);
  nsize := upobj.NeededSize;
  usize := to_upd_obj.NeededSize;
  wsize := nsize + usize;
  if (m.Size-m.Position)<(wsize) then
      m.SetSize(m.Size + wsize + 4096);
  upobj.CopyToMemory(m.Memory+m.Position);
  m.Position:=m.Position+nsize;
  to_upd_obj.CopyToMemory(m.Memory+m.Position);
  m.Position:=m.Position+usize;
  m.WriteAnsiString('SU'+upobj.UID_String+to_upd_obj.UID_String);
end;

//Check what has to be done at master level, (reflinks)
function TFRE_DB_UpdateStep.StoreInCollectionCheck(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;
var i,j       : NativeInt;
    collarray : IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
begin
  result := edb_OK;
  for i:=0 to FCnt-1 do
    with FSublist[i] do
      begin
        collarray := to_upd_obj.__InternalGetCollectionList;
        for j := 0 to high(collarray) do
          begin
            result := collarray[j].CheckFieldChangeAgainstIndex(oldfield,newfield,updtyp,check,raise_ex);
            if result<>edb_OK then
              exit;
         end;
      end
end;

function TFRE_DB_UpdateStep.MasterStore(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;
var i,j       : NativeInt;
    collarray : IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;

    procedure _DeletedField;
    begin
      with FSublist[i] do
        case newfield.FieldType of
          fdbft_Object:
            begin
              result := master.DeleteObject(newfield.AsObject.UID,raise_ex,check);
              if result<>edb_OK then
                exit;
            end;
          fdbft_ObjLink:
            begin
               result := master._RemoveRefLinkFieldDelRefs(step.to_upd_obj,newfield,raise_ex,check);
               if result <> edb_OK then
                 exit;
            end
          else begin
            if not check then
              to_upd_obj.DeleteField(newfield.FieldName);
          end; // ok
        end;
    end;

    procedure _AddedField;
    begin
      with FSublist[i] do
        case newfield.FieldType of
          fdbft_NotFound,fdbft_GUID,fdbft_Byte,fdbft_Int16,fdbft_UInt16,fdbft_Int32,fdbft_UInt32,fdbft_Int64,fdbft_UInt64,
          fdbft_Real32,fdbft_Real64,fdbft_Currency,fdbft_String,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream :
            begin
             // Just add the Field
              if check then
                exit;
              to_upd_obj.Field(newfield.FieldName).CloneFromFieldFull(newfield);
            end;
          fdbft_Object:
            begin
              to_upd_obj.Field(newfield.FieldName).AsObject := newfield.AsObject;
            end;
          fdbft_ObjLink:
            begin
               if check then
                 begin
                   result := master.__CheckReferenceToList(to_upd_obj,newfield.FieldName,newfield.AsObjectLinkArray,raise_ex);
                   if result<>edb_OK then
                     exit;
                 end
               else
                 begin
                   result := master._SetupInitialRefLink(to_upd_obj,newfield.FieldName,newfield.AsObjectLinkArray,raise_ex);
                   if result<> edb_OK then
                     exit;
                   to_upd_obj.Field(newfield.FieldName).AsObjectLinkArray:=newfield.AsObjectLinkArray;
                 end;
            end;
          fdbft_CalcField:
            begin
              abort;
            end;
        end;
    end;

    procedure _ChangedField;
    begin
      with FSublist[i] do
        case newfield.FieldType of
          fdbft_NotFound,fdbft_GUID,fdbft_Byte,fdbft_Int16,fdbft_UInt16,fdbft_Int32,fdbft_UInt32,fdbft_Int64,fdbft_UInt64,
          fdbft_Real32,fdbft_Real64,fdbft_Currency,fdbft_String,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream :
            begin
              if check then
                begin
                  result := edb_OK;
                  exit;
                end;
              to_upd_obj.Field(newfield.FieldName).CloneFromFieldFull(newfield);
            end;
          fdbft_Object:
            begin
              if check then
                begin
                  result := edb_OK;
                  exit;
                end
              else
                begin
                  writeln('CHANGE OBJECT - (FIELD) ',check,' ',oldfield.ValueCount,'  ',newfield.ValueCount);
                  to_upd_obj.Field(newfield.FieldName).AsObjectArr := newfield.AsObjectArr;
                end;
            end;
          fdbft_ObjLink:
            begin
               if check then
                 begin
                   result := master.__CheckReferenceToList(to_upd_obj,newfield.FieldName,newfield.AsObjectLinkArray,raise_ex);
                   if result<>edb_OK then
                     exit;
                 end
               else
                 begin
                   result := master._ChangeRefLink(to_upd_obj,newfield.FieldName,newfield.AsObjectLinkArray,raise_ex);
                   if result<> edb_OK then
                     exit;
                   to_upd_obj.Field(newfield.FieldName).AsObjectLinkArray:=newfield.AsObjectLinkArray;
                 end;
            end;
          fdbft_CalcField:
            begin
              writeln('CHANGE (CALCFIELD) ABORT');
              abort;
            end;
        end;
    end;

begin
  result := edb_OK;
  if to_upd_obj.IsObjectRoot then
    if length(to_upd_obj.__InternalGetCollectionList)=0 then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'must have internal collections to store into');
  for i:=0 to FCnt-1 do
    begin
      with FSublist[i] do
        case updtyp of
          cev_FieldDeleted:
            _DeletedField;
          cev_FieldAdded:
            _AddedField;
          cev_FieldChanged:
            _ChangedField;
        end;
    end;
end;

{ TFRE_DB_DeleteStep }

constructor TFRE_DB_DeleteStep.Create(var del_obj: TFRE_DB_Object; const is_insert: boolean);
begin
  FDelObj   := del_obj;
  FIsInsert := is_insert;
  FObjPtr   := @del_obj;
end;

function TFRE_DB_DeleteStep.DescribeText: String;
begin
  if assigned(FDelObj.Parent) then
      WriteStr(result,' DELETE CHILD OBJECT ',FDelObj.UID_String,' IN PARENT ',FDelObj.Parent.UID_String)
  else
      WriteStr(result,' DELETE ROOT OBJECT ',FDelObj.UID_String);
end;

procedure TFRE_DB_DeleteStep.WriteToWAL(const m: TMemoryStream);
begin
  if FDelObj.IsObjectRoot then
    begin
      if FDelObj.IsVolatile then
        exit;
      m.WriteAnsiString('SD'+FDelObj.UID_String);
      m.WriteAnsiString(CollName);
      m.WriteAnsiString('ED'+FDelObj.UID_String);
    end;
end;

function TFRE_DB_DeleteStep.StoreInCollectionCheck(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;
var arr : IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
      i : NativeInt;
begin
  assert(IsInsert=false);
  if check then
    result := edb_OK
  else
    begin
      if FDelObj.IsObjectRoot then // TDOD -> CHECK, make it working
        begin
          arr := FDelObj.__InternalGetCollectionList;
          for i := 0 to high(arr) do
            begin
              result := arr[i].GetPersLayerIntf.DeleteFromThisColl(FDelObj,raise_ex,check);
              if result<>edb_OK then
                exit;
            end;
        end
      else
        begin
          result := edb_OK;
        end;
    end;
end;

function TFRE_DB_DeleteStep.MasterStore(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;
begin
  writeln('*****DELETE STEP .... ReMOVINg : ',FDelObj.UID_String);
  assert(IsInsert=false);
  result := master.DeleteObject(FDelObj.UID,raise_ex,check);
  if result=edb_OK then
    FObjPtr^:=nil;
end;

{ TFRE_DB_TransactionalUpdateList }

constructor TFRE_DB_TransactionalUpdateList.Create(const TransID: TFRE_DB_NameType; const master_data: TFRE_DB_Master_Data; const layer: IFRE_DB_PERSISTANCE_LAYER);
begin
  FTransId := TransID;
  FChangeList.Init(10);
  FMaster  := master_data;
  FLayer   := layer;
  FWalMem  := TMemoryStream.Create;
end;

procedure TFRE_DB_TransactionalUpdateList.AddChangeStep(const step: TFRE_DB_ChangeStep);
begin
  FChangeList.Add(step);
end;

procedure TFRE_DB_TransactionalUpdateList.PrintTextLog;
var count : NativeInt = 0;

  procedure Dump(var x:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    inc(count);
    writeln(count:5,x.DescribeText);
  end;

begin
  FChangeList.ForAllBreak(@Dump);
end;

function TFRE_DB_TransactionalUpdateList.ProcessCheck(const raise_ex: boolean): TFRE_DB_Errortype;
var failure : boolean;

  procedure CheckForExistence(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    with step do
      result:=CheckExistence(FMaster,raise_ex);
    if result<>edb_OK then
      halt_flag := true;
  end;

  procedure StoreInCollectionCheck(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    with step do
      result:=StoreInCollectionCheck(FMaster,raise_ex,true);
    if result<>edb_OK then
      halt_flag := true;
  end;

  procedure MasterStoreCheck(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    with step do
      result:=MasterStore(FMaster,raise_ex,true);
    if result<>edb_OK then
      halt_flag := true;
  end;

  procedure UnprepareCollections(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    step.UnprepareCollection;
  end;


begin
  try
    try
      failure := false;
      result := edb_OK;
      FChangeList.ForAllBreak(@CheckForExistence);
      if Result<>edb_OK then
        exit;
      FChangeList.ForAllBreak(@StoreInCollectionCheck);
      if Result<>edb_OK then
        exit;
      FChangeList.ForAllBreak(@MasterStoreCheck);
      if Result<>edb_OK then
        exit;
    except
      failure := true;
      raise;
    end;
  finally
    if failure then
      begin
        FChangeList.ForAllBreak(@UnprepareCollections);
      end;
  end;
end;

procedure TFRE_DB_TransactionalUpdateList.Write_WAL;

  procedure WriteWAL(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    step.WriteToWal(FWalMem);
  end;

begin
  FWalMem.Position:=0;
  FChangeList.ForAllBreak(@WriteWal);
  if FWalMem.Position<>0 then
    begin
      FLayer.SyncWriteWAL(FWalMem);
    end;
end;

function TFRE_DB_TransactionalUpdateList.Commit(const raise_ex: boolean): TFRE_DB_Errortype;

  procedure StoreInCollection(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    result := step.StoreInCollectionCheck(FMaster,raise_ex,false);
    if result<>edb_OK then
      halt_flag:=true;
    if step is TFRE_DB_InsertStep then
      halt_flag:=true;
  end;

  //Store objects and sub objects
  procedure MasterStore(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    result := step.MasterStore(FMaster,raise_ex,false);
    if result<>edb_OK then
      halt_flag:=true;
  end;

  procedure CleanUp(var step:TFRE_DB_ChangeStep;const idx:NativeInt ; var halt_flag:boolean);
  begin
    step.Free;
  end;

begin
  try
    result := ProcessCheck(raise_ex);
    if result <>edb_OK then
      exit(result);
    Write_WAL;
    result := edb_OK;
    FChangeList.ForAllBreak(@StoreInCollection);
    if result<>edb_OK then
      abort;
    FChangeList.ForAllBreak(@MasterStore);
    if result<>edb_OK then
      abort;
  finally
    FChangeList.ForAllBreak(@Cleanup);
  end;
end;

function TFRE_DB_TransactionalUpdateList.Rollback(const raise_ex: boolean): TFRE_DB_Errortype;
begin
  abort;
end;

{ TFRE_DB_InsertStep }

constructor TFRE_DB_InsertStep.Create(new_obj: TFRE_DB_Object; const coll: IFRE_DB_PERSISTANCE_COLLECTION; const is_insert: boolean);
var cn:string;
begin
  FNewObj   := new_obj;
  FColl     := coll;
  FIsInsert := is_insert;
end;

destructor TFRE_DB_InsertStep.Destroy;
begin
  inherited Destroy;
end;

function TFRE_DB_InsertStep.DescribeText: String;
begin
  if assigned(FNewObj.Parent) then
    begin
      WriteStr(result,' INSERT NEW CHILD OBJECT ',FNewObj.UID_String,' IN PARENT ',FNewObj.Parent.UID_String,' FIELD ',FNewObj.ParentField.FieldName,' OBJROOT : ',FNewObj.ObjectRoot.UID_String);
    end
  else
    begin
      WriteStr(result,' INSERT NEW ROOT OBJECT ',FNewObj.UID_String,' INTO ',fcoll.CollectionName(false));
    end;
end;

function TFRE_DB_InsertStep.IsARootInsert: Boolean;
begin
  result := not assigned(FNewObj.Parent);
end;

function TFRE_DB_InsertStep.CheckExistence(const master: TFRE_DB_Master_Data; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  if master.ExistsObject(FNewObj.UID) then
    if raise_ex then
      raise EFRE_DB_Exception.Create(edb_EXISTS,'the obj [%s] does already exist in master data.')
    else
      exit(edb_EXISTS);
  result := edb_OK;
end;

function TFRE_DB_InsertStep.StoreInCollectionCheck(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;
begin
  //writeln('********** INSERT CHECK ',FNewObj.UID_String,' ',FNewObj.ClassName,'  ',IsARootInsert);
  //writeln(FNewObj.DumpToString());
  //writeln('********** INSERT CHECK ',FNewObj.UID_String,' ',FNewObj.ClassName,'  ',IsARootInsert);
  if IsARootInsert then
    result := FColl.GetPersLayerIntf.StoreInThisColl(FNewObj,raise_ex,check)
  else
    begin
       //TODO think about child objects storing in other collections
      if IsInsert then
        begin
          result := edb_INTERNAL;
          if raise_ex then
            raise EFRE_DB_Exception.Create(result,'ONLY ROOT OBJECTS CAN BE INSERTED CURRENTLY');
        end
      else
        begin
          result := edb_OK; // already checked for existence
        end;
    end;
end;

function TFRE_DB_InsertStep.MasterStore(const master: TFRE_DB_Master_Data; const raise_ex: boolean; const check: boolean): TFRE_DB_Errortype;

  procedure MasterStoreAndSubObjects(const obj:TFRE_DB_Object; var halt:boolean);
  begin
    //writeln('ROOT OBJECT - MASTER STORE :: ',obj.InternalUniqueDebugKey,' IS INSERT ',IsInsert);
    result := master.StoreObject(obj,raise_ex,check);
    if result<>edb_OK then
      halt:=true;
  end;

begin
  if FNewObj.IsObjectRoot then
    begin
      assert((check=true) or (length(FNewObj.__InternalGetCollectionList)>0));
      FNewObj.ForAllObjectsBreak(@MasterStoreAndSubObjects)
    end
  else
    begin
      //writeln('CHILD OBJECT (alone) - MASTER STORE :: ',FNewObj.InternalUniqueDebugKey,' IS INSERT ',IsInsert);
      result := master.StoreObject(FNewObj,raise_ex,check);
    end;
end;


procedure TFRE_DB_InsertStep.WriteToWAL(const m: TMemoryStream);
var nsize : NativeInt;
begin
  if FNewObj.IsVolatile then
    exit;
  if not IsARootInsert then // Only root objects need to be in WAL
    exit;
  m.WriteAnsiString('SI'+FNewObj.UID_String+FColl.CollectionName);
  nsize := FNewObj.NeededSize;
  if (m.Size-m.Position)<nsize then
    m.SetSize(m.Size + nsize + 4096);
  FNewObj.CopyToMemory(m.Memory+m.Position);
  m.Position:=m.Position+nsize;
  m.WriteAnsiString('EI'+FNewObj.UID_String);
end;

procedure TFRE_DB_InsertStep.UnprepareCollection;
begin
  if assigned(FColl) then
    FColl.InternalUnprepare;
end;

{ TFRE_DB_IndexValueStore }

procedure TFRE_DB_IndexValueStore.InternalCheck;
var i:NativeInt;
begin
  try
    for i:=0 to high(FOBJArray) do
      FOBJArray[i].Assert_CheckStoreLocked;
  except on e:Exception do
   begin
    writeln('E ',e.Message);
    writeln('LEN ARRAY ',Length(FOBJArray));
    for i:=0 to high(FOBJArray) do
      begin
        writeln('--',i,' ',FOBJArray[i].InternalUniqueDebugKey);
        writeln(FOBJArray[i].DumpToString());
        writeln('--');
      end;
    raise;
   end;
  end;
end;


function TFRE_DB_IndexValueStore.Exists(const guid: TGUID): boolean;
var i : NativeInt;
begin
  for i := 0 to High(FOBJArray) do
    if FREDB_Guids_Compare(FOBJArray[i].UID,guid)=0 then
      exit(true);
  result := false;
end;

function TFRE_DB_IndexValueStore.Add(const obj: TFRE_DB_Object): boolean;
begin
  if Exists(obj.UID) then
    exit(false);
  SetLength(FOBJArray,Length(FOBJArray)+1);
  FOBJArray[high(FOBJArray)] := obj;
  result := true;
end;


function TFRE_DB_IndexValueStore.IndexedObjects: TFRE_DB_ObjectArray;
begin
  result := FOBJArray;
  InternalCheck;
end;

procedure TFRE_DB_IndexValueStore.StreamToThis(const stream: TStream);
var i : NativeInt;
begin
  stream.WriteQWord(Length(FOBJArray));
  for i:=0 to high(FOBJArray) do
    stream.WriteBuffer(FOBJArray[i].UID,SizeOf(TGuid));
end;

procedure TFRE_DB_IndexValueStore.LoadFromThis(const stream: TStream; const coll: IFRE_DB_PERSISTANCE_COLLECTION);
var i,cnt : NativeInt;
    uid   : TGUID;
begin
  cnt := stream.ReadQWord;
  SetLength(FOBJArray,cnt);
  for i:=0 to high(FOBJArray) do
    begin
      stream.ReadBuffer(uid,SizeOf(TGuid));
      if not coll.GetPersLayerIntf.FetchIntFromColl(uid,FOBJArray[i]) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'STREAM LOAD INDEX ERROR CANT FIND [%s] IN COLLECTION',[GFRE_BT.GUID_2_HexString(uid)]);
    end;
end;

{ TFRE_DB_Master_Data }


function TFRE_DB_Master_Data.GetOutBoundRefLinks(const from_obj: TGUID): TFRE_DB_ObjectReferences;
var key : RFRE_DB_GUID_RefLink_Out_Key;
    cnt : NativeInt;

   procedure Iterate(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
   var encap   : TREF_LinkEncapsulation;
       namelen : NativeInt;
   begin
     if cnt=Length(result) then
       SetLength(result,Length(result)+10);
     encap := FREDB_PtrUIntToObject(value) as TREF_LinkEncapsulation;
     namelen := KeyLen-17;
     assert(namelen>0);
     SetLength(result[cnt].fieldname,namelen);
     move(PFRE_DB_GUID_RefLink_Out_Key(key)^.FieldName,result[cnt].fieldname[1],namelen); // copy name
     result[cnt].linklist :=encap.Links;
     //writeln('FIELD : ',result[cnt].fieldname,' ',FREDB_GuidArray2String(result[cnt].linklist));
     inc(cnt);
   end;

begin
  cnt := 0;
  move(from_obj,key.GUID,16);
  key.RefTyp:=$99;
  FMasterRefLinks.PrefixScan(@key,17,@Iterate);
  SetLength(result,cnt);
end;

function TFRE_DB_Master_Data.GetInboundLinks(const to_obj: TGUID): TFRE_DB_ObjectReferences;
var key : RFRE_DB_GUID_RefLink_In_Key;
    cnt : NativeInt;

   procedure Iterate(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
   var namelen : NativeInt;
   begin
     if cnt=Length(result) then
       SetLength(result,Length(result)+10);
     assert(value=$BEEF0BAD);
     namelen := KeyLen-33;
     Assert(namelen>0);
     SetLength(result[cnt].fieldname,namelen);
     SetLength(result[cnt].linklist,1);
     move(PFRE_DB_GUID_RefLink_In_Key(key)^.FromFieldScheme,result[cnt].fieldname[1],namelen); // copy name
     move(PFRE_DB_GUID_RefLink_In_Key(key)^.FromGuid,result[cnt].linklist[0],16); // copy guid
     //writeln('  > INL FROM FIELD : ',result[cnt].fieldname,' ',FREDB_GuidArray2String(result[cnt].linklist));
     inc(cnt);
   end;

begin
  cnt := 0;
  move(to_obj,key.GUID,16);
  key.RefTyp:=$AA;
  FMasterRefLinks.PrefixScan(@key,17,@Iterate);
  SetLength(result,cnt);
end;

function TFRE_DB_Master_Data.GetOutBoundRefLinksCount(const from_obj: TGUID): NativeInt;
var key : RFRE_DB_GUID_RefLink_Out_Key;

   procedure Iterate(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
   begin
     inc(result);
   end;

begin
  result := 0;
  move(from_obj,key.GUID,16);
  key.RefTyp:=$99;
  FMasterRefLinks.PrefixScan(@key,17,@Iterate);
end;

function TFRE_DB_Master_Data.GetInboundLinksCount(const to_obj: TGUID): NativeInt;
var key : RFRE_DB_GUID_RefLink_In_Key;

   procedure Iterate(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
   begin
     inc(result);
   end;

begin
  result := 0;
  move(to_obj,key.GUID,16);
  key.RefTyp:=$AA;
  FMasterRefLinks.PrefixScan(@key,17,@Iterate);
end;

function TFRE_DB_Master_Data.__CheckReferenceToList(const obj:TFRE_DB_Object ; fieldname: TFRE_DB_NameType; list: TFRE_DB_GUIDArray ; const raise_ex : boolean): TFRE_DB_Errortype;
var j       : NativeInt;
    ref_obj : TFRE_DB_Object;
begin
  //writeln('TODO _ PARALLEL CHECK OF REFLINK INDEX TREE');
  if not FREDB_CheckGuidsUnique(list) then
    if raise_ex then
       raise EFRE_DB_Exception.Create(edb_ERROR,'referential link check: links not unique from obj(%s:%s)',[obj.UID_String,fieldname])
    else
      exit(edb_ERROR);
  for j:=0 to high(list) do
    begin
      if not FetchObject(list[j],ref_obj,true) then
        begin
          if raise_ex then
            raise EFRE_DB_Exception.Create(edb_ERROR,'referential link check: link from obj(%s:%s) to obj(%s) : the to object does not exist!',[obj.UID_String,fieldname,GFRE_BT.GUID_2_HexString(list[j])])
          else
            exit(edb_NOT_FOUND);
        end;
      if obj.IsVolatile or obj.IsSystem then
        begin
          if raise_ex then
            raise EFRE_DB_Exception.Create(edb_ERROR,'referential link check: link from obj(%s) to obj(%s) : the linked object is volatile or system!',[obj.UID_String,fieldname,GFRE_BT.GUID_2_HexString(list[j])])
          else
            exit(edb_NOT_FOUND);
        end;
    end;
  result:=edb_OK;
end;

// Setup the "to_list" for KEY-UID,Field,(Subkeys)
// For every in the "to_list" referenced object set an inbound link, from KEY-UID

function TFRE_DB_Master_Data._SetupInitialRefLink(const from_key: TFRE_DB_Object; const fieldname: TFRE_DB_NameType; const references_to_list: TFRE_DB_GUIDArray; const raise_ex: boolean): TFRE_DB_Errortype;
var refkey   : RFRE_DB_GUID_RefLink_Out_Key;
    refinkey : RFRE_DB_GUID_RefLink_In_Key;
    reenc    : TREF_LinkEncapsulation;
    i        : NativeInt;
    refin_fn : TFRE_DB_NameTypeRL;

    //j,k      : NativeInt;
    //dumlist  : TFRE_DB_ObjectReferences;
    //duminlist: TFRE_DB_ObjectReferences;


begin
  reenc := TREF_LinkEncapsulation.Create(references_to_list);
  move(from_key.UID,refkey.GUID,16);
  move(fieldname[1],refkey.FieldName,Length(fieldname));
  refkey.RefTyp := $99;
  if not FMasterRefLinks.InsertBinaryKey(@refkey,17+Length(fieldname),FREDB_ObjectToPtrUInt(reenc)) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'although prechecked the reflink key exists. :-(');

  refin_fn := from_key.SchemeClass+'|'+fieldname;
  refinkey.RefTyp := $AA;
  move(from_key.UID,refinkey.FromGuid,16);
  move(refin_fn[1],refinkey.FromFieldScheme,length(refin_fn));
  for i := 0 to high(references_to_list) do begin
    move(references_to_list[i],refinkey.GUID,16);
    if not FMasterRefLinks.InsertBinaryKey(@refinkey,33+Length(refin_fn),$BEEF0BAD) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'although prechecked the reflink key exists. :-(');
  end;

  //writeln('------------------------');
  //writeln('SETUP_INITIAL_REF_LINK');
  //writeln('------------------------');
  //dumlist := GetOutBoundRefLinks(from_key.UID);
  //for i:=0 to high(dumlist) do
  //  begin
  //    writeln(from_key.UID_String,' ',dumlist[i].fieldname,' -> ',FREDB_GuidArray2String(dumlist[i].linklist));
  //    for j:=0 to high(dumlist[i].linklist) do
  //      begin;
  //        duminlist := GetInboundLinks(dumlist[i].linklist[j]);
  //        for k := 0 to high(duminlist) do
  //          writeln('  Pointed to obj ',GFRE_BT.GUID_2_HexString(dumlist[i].linklist[j]),' <- by ',duminlist[k].fieldname,' ',FREDB_GuidArray2String(duminlist[k].linklist));
  //      end;
  //  end;
  //writeln('------------------------');
  result := edb_OK;
end;

function TFRE_DB_Master_Data._ChangeRefLink(const from_obj: TFRE_DB_Object; const fieldname: TFRE_DB_NameType; const references_to_list: TFRE_DB_GUIDArray; const raise_ex: boolean): TFRE_DB_Errortype;
var refkey    : RFRE_DB_GUID_RefLink_Out_Key;
    refinkey  : RFRE_DB_GUID_RefLink_In_Key;
    reenc     : TREF_LinkEncapsulation;
    i         : NativeInt;
    dummy     : NativeUint;
    refin_fn  : TFRE_DB_NameTypeRL;

    //j,k       : NativeInt;
    //dumlist   : TFRE_DB_ObjectReferences;
    //duminlist : TFRE_DB_ObjectReferences;
    //dummyold  : TFRE_DB_GUIDArray;

begin
  move(from_obj.UID,refkey.GUID,16);
  move(fieldname[1],refkey.FieldName,Length(fieldname));
  refkey.RefTyp := $99;
  if not FMasterRefLinks.ExistsBinaryKey(@refkey,17+Length(fieldname),dummy) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'although prechecked the reflink key to change does not exists. :-(');
  reenc := FREDB_PtrUIntToObject(dummy) as TREF_LinkEncapsulation;

  //dummyold := reenc.Links;

  //writeln(FREDB_GuidArray2String (reenc.Links));
  // RemoveOldLinks
  refin_fn        := from_obj.SchemeClass+'|'+fieldname;
  refinkey.RefTyp := $AA;
  move(from_obj.UID,refinkey.FromGuid,16);
  move(refin_fn[1],refinkey.FromFieldScheme,length(refin_fn));
  for i := 0 to high(reenc.FLinks) do begin
    move(reenc.FLinks[i],refinkey.GUID,16);
    if not FMasterRefLinks.RemoveBinaryKey(@refinkey,33+Length(refin_fn),dummy) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'although prechecked the to delete reflink key does not exists. :-(');
  end;
  // RemoveOldLinks

  reenc.FLinks := Copy(references_to_list); // new links
  //Update Inbound links
  for i := 0 to high(references_to_list) do begin
    move(references_to_list[i],refinkey.GUID,16);
    if not FMasterRefLinks.InsertBinaryKey(@refinkey,33+Length(refin_fn),$BEEF0BAD) then
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'although prechecked the inbound reflink key exists. :-(');
  end;
  //Update Inbound links

  //writeln('------------------------');
  //writeln('UPDATED_REF_LINK');
  //writeln('------------------------');
  //dumlist := GetOutBoundRefLinks(from_obj.UID);
  //for i:=0 to high(dumlist) do
  //  begin
  //    writeln(GFRE_BT.GUID_2_HexString(from_obj.UID),' ',dumlist[i].fieldname,' -> ',FREDB_GuidArray2String(dumlist[i].linklist));
  //    for j:=0 to high(dumlist[i].linklist) do
  //      begin;
  //        duminlist := GetInboundLinks(dumlist[i].linklist[j]);
  //        for k := 0 to high(duminlist) do
  //          writeln('  Pointed to obj ',GFRE_BT.GUID_2_HexString(dumlist[i].linklist[j]),' <- by ',duminlist[k].fieldname,' ',FREDB_GuidArray2String(duminlist[k].linklist));
  //      end;
  //  end;
  //writeln('------------------------OLD_______');
  //for j:=0 to high(dummyold) do
  //  begin;
  //    duminlist := GetInboundLinks(dummyold[j]);
  //    for k := 0 to high(duminlist) do
  //      writeln('  Pointed to obj ',GFRE_BT.GUID_2_HexString(dummyold[j]),' <- by ',duminlist[k].fieldname,' ',FREDB_GuidArray2String(duminlist[k].linklist));
  //  end;
  //writeln('------------------------');

  result := edb_OK;
end;

function TFRE_DB_Master_Data._SetupInitialRefLinks(const from_key: TFRE_DB_Object; const references_to_list: TFRE_DB_ObjectReferences; const raise_ex: boolean): TFRE_DB_Errortype;
var
  i: NativeInt;
begin
  result := edb_OK;
  for i:=0 to high(references_to_list) do
    begin
      result := _SetupInitialRefLink(from_key,references_to_list[i].fieldname,references_to_list[i].linklist,raise_ex);
      if result<>edb_OK then
        exit;
    end;
end;

function TFRE_DB_Master_Data._UpdateRefLinks(const key: TGUID; const references_to_list: TFRE_DB_ObjectReferences; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  writeln('UPDATE_REF_LINKS FULL STOP');
  halt;
  result := edb_OK;
end;

function TFRE_DB_Master_Data._CheckRefIntegrityToLink(const obj: TFRE_DB_Object; var ref_array: TFRE_DB_ObjectReferences ; const raise_ex : boolean): TFRE_DB_Errortype;
var new_references_to_list : TFRE_DB_ObjectReferences;
    i                      : NativeInt;
begin
  new_references_to_list := obj.ReferencesFromData;
  for i:=0 to high(new_references_to_list) do
    begin
      result := __CheckReferenceToList(obj,new_references_to_list[i].fieldname,new_references_to_list[i].linklist,raise_ex);
      if result<>edb_OK then
        exit;
    end;
  ref_array := new_references_to_list;
  result    := edb_OK;
end;

function TFRE_DB_Master_Data._RemoveRefLinkFieldDelRefs(const obj: TFRE_DB_Object; field: TFRE_DB_FIELD; const raise_ex: boolean; const check_only: boolean): TFRE_DB_Errortype;
begin
  writeln('_RemoveRefLinFieldefs FULL STOP');
  halt;
  result := edb_OK;
end;

function TFRE_DB_Master_Data.InternalStoreObjectFromStable(const obj: TFRE_DB_Object): TFRE_DB_Errortype;
var
   key    : TGuid;
   dummy  : PtrUInt;

   procedure Store(const obj:TFRE_DB_Object; var halt:boolean);
   begin
     dummy := FREDB_ObjectToPtrUInt(obj);
     key   := obj.UID;
     //writeln('RELOAD STORE : ',obj.UID_String,' ',obj.IsObjectRoot);
     if not FMasterPersistantObjStore.InsertBinaryKeyOrFetch(@key,sizeof(tguid),dummy) then
       result := edb_EXISTS;
     if result<>edb_OK then
       halt := true
   end;

begin
  Result := edb_OK;
  obj.ForAllObjectsBreak(@Store);
end;

function TFRE_DB_Master_Data.InternalRebuildRefindex: TFRE_DB_Errortype;

  procedure BuildRef(const obj:TFRE_DB_Object);
  var references_to_list : TFRE_DB_ObjectReferences;
  begin
    result := _CheckRefIntegrityToLink(obj,references_to_list,true); // Todo Check inbound From Links (unique?)
    if Length(references_to_list)>0 then
      begin
        //writeln('SETUP REFLINKS ',obj.UID_String,' ',Length(references_to_list));
        _SetupInitialRefLinks(obj,references_to_list,true);
      end;
  end;

begin
  ForAllObjectsInternal(true,false,@BuildRef);
  result := edb_OK;
end;

procedure TFRE_DB_Master_Data.InternalStoreLock;

  procedure BuildRef(const obj:TFRE_DB_Object);
  begin
    if obj.IsObjectRoot then
      obj.Set_Store_Locked(true);
  end;

begin
  ForAllObjectsInternal(true,false,@BuildRef);
end;

//procedure TFRE_DB_Master_Data._AddRefLink(const from_obj, to_obj: TGuid; const rebuild: boolean);
//var
//    from_key,to_key   : TGUID_RefLink_Key;
//    //from_o,to_o       : TFRE_DB_Object;
//    from_field        : TFRE_DB_FIELD;
//    to_field          : TFRE_DB_FIELD;
//    max,current       : integer;
//    guidarray         : TFRE_DB_GUIDArray;
//
//    //procedure _SetupInitial(const set_guid:TGuid);
//    //begin
//    //  SetLength(guidarray,c_REFLINK_BLOCK_SIZE);
//    //  guidarray[0] := set_guid;
//    //end;
//
//    procedure _SetupNext(const set_guid:TGuid);
//    var null_fnd,i : integer;
//        len        : integer;
//    begin
//      for i:= 0 to high(guidarray) do begin
//        if FREDB_Guids_Same(guidarray[i],set_guid) then begin
//          if not rebuild then begin
//            raise EFRE_DB_Exception.Create(edb_EXISTS,'guid %s already set in linklist',[GFRE_BT.GUID_2_HexString(set_guid)]);
//          end else begin
//            GFRE_DB.LogWarning(dblc_REFERENCES,'rebuild : guid %s already set in linklist',[GFRE_BT.GUID_2_HexString(set_guid)]);
//          end;
//        end;
//      end;
//      null_fnd:=-1;
//      for i:= 0 to high(guidarray) do begin
//        if FREDB_Guids_Same(guidarray[i],CFRE_DB_NullGUID) then begin
//          null_fnd:=i;
//          break;
//        end;
//      end;
//      if null_fnd=-1 then begin
//        len := length(guidarray);
//        SetLength(guidarray,length(guidarray)+c_REFLINK_BLOCK_SIZE);
//        guidarray[len]      := set_guid;
//      end else begin
//        guidarray[null_fnd] := set_guid;
//      end;
//    end;
//
//begin
//  //if not Exists(from_obj) then raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'while updating reflinks, the  from object uid=%s was not found',[GFRE_BT.GUID_2_HexString(from_obj)]);
//  //if not Exists(to_obj) then raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'while updating reflinks, the to object uid=%s was not found',[GFRE_BT.GUID_2_HexString(to_obj)]);
//  //from_field_name := GFRE_BT.GUID_2_HexString(from_obj);
//  //to_field_name   := GFRE_BT.GUID_2_HexString(to_obj);
//  assert(from_obj<>to_obj);
//  from_key.Part1 := TGUID_Access(from_obj).Part1;
//  from_key.Part2 := TGUID_Access(from_obj).Part2;
//  from_key.Rtyp  := $aa;
//
//  to_key.Part1 := TGUID_Access(to_obj).Part1;
//  to_key.Part2 := TGUID_Access(to_obj).Part2;
//  to_key.Rtyp  := $bb;
//
//  FMasterRefLinks.InsertBinaryKey(@from_key.Part1,sizeof(from_key),dummy);
//
//  from_field := FReferentialLinks._Field('F').AsObject._Field(from_field_name);
//  to_field   := FReferentialLinks._Field('T').AsObject._Field(to_field_name);
//  guidarray := from_field.AsGUIDArr;
//  _SetupNext(to_obj);
//  from_field.AsGUIDArr:=guidarray;
//  guidarray := to_field.AsGUIDArr;
//  _SetupNext(from_obj);
//  to_field.AsGUIDArr:=guidarray;
//end;
//

constructor TFRE_DB_Master_Data.Create;
begin
  FMasterPersistantObjStore := TFRE_ART_TREE.Create;
  FMasterVolatileObjStore   := TFRE_ART_TREE.Create;
  FMasterRefLinks           := TFRE_ART_TREE.Create;
  FMasterCollectionStore    := TFRE_DB_CollectionManageTree.Create;
end;

function TFRE_DB_Master_Data.GetReferenceCount(const obj_uid: TGuid; const from: boolean): NativeInt;
begin
  if from then
    result := GetOutBoundRefLinksCount(obj_uid)
  else
    result := GetInboundLinksCount(obj_uid);
end;

function TFRE_DB_Master_Data.GetReferences(const obj_uid: TGuid; const from: boolean): TFRE_DB_ObjectReferences;
begin
  if from then
    result := GetOutBoundRefLinks(obj_uid)
  else
    result := GetInboundLinks(obj_uid);
end;

function TFRE_DB_Master_Data.ExistsObject(const obj_uid: TGuid): Boolean;
var dummy : NativeUint;
begin
  if FMasterVolatileObjStore.ExistsBinaryKey(@obj_uid,SizeOf(TGuid),dummy) then
    exit(true);
  if FMasterPersistantObjStore.ExistsBinaryKey(@obj_uid,SizeOf(TGuid),dummy) then
    exit(true);
  exit(false);
end;

function TFRE_DB_Master_Data.FetchObject(const obj_uid: TGuid; var obj: TFRE_DB_Object ; const internal_obj : boolean): boolean;
var dummy : NativeUint;
    clobj : TFRE_DB_Object;
begin
  obj := nil;
  //write('TRY MASTER FETCH ',gfre_bt.GUID_2_HexString(obj_uid));
  result := FMasterVolatileObjStore.ExistsBinaryKey(@obj_uid,SizeOf(TGuid),dummy);
  if result then
    begin
      obj := FREDB_PtrUIntToObject(dummy) as TFRE_DB_Object;
      obj.Assert_CheckStoreLocked;
    end
  else
    begin
     result := FMasterPersistantObjStore.ExistsBinaryKey(@obj_uid,SizeOf(TGuid),dummy);
     if result then
       begin
         obj := FREDB_PtrUIntToObject(dummy) as TFRE_DB_Object;
         obj.__InternalClearManageInfo;
         //if Length(obj.__InternalGetCollectionList)<1 then
         //  begin
         //    writeln('OBJ FCUKED UP');
         //    writeln(obj.DumpToString());
         //    halt;
         //    abort;
         //  end;
         //writeln(' IS OK ! ',obj.InternalUniqueDebugKey);
       end
     else
       //writeln(' FAILED !!!!!!');
    end;
  if result and
     not internal_obj then
       begin
         obj.Assert_CheckStoreLocked;
         obj.Set_Store_Locked(false);
         try
          if Length(obj.__InternalGetCollectionList)<1 then
            abort;
          clobj := obj.CloneToNewObject;
         finally
           obj.Set_Store_Locked(true);
         end;
         obj := clobj;
         obj.__InternalClearManageInfo;
       end;
end;

function TFRE_DB_Master_Data.StoreObject(const obj: TFRE_DB_Object; const raise_ex: boolean; const check_only: boolean): TFRE_DB_Errortype;
var references_to_list : TFRE_DB_ObjectReferences;
    key                : TGuid;
    dummy              : PtrUInt;
begin
  key := obj.UID;
  result := _CheckRefIntegrityToLink(obj,references_to_list,raise_ex); // Todo Check inbound From Links (unique?)
  if result<>edb_OK then
    exit;
  if (obj.IsVolatile
     or obj.IsSystem)
     and (Length(references_to_list)>0) then
       begin
         if raise_ex then
           raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'a volatile or system object is not allowed to reference other objects')
         else
           exit(edb_INVALID_PARAMS);
       end;
  if obj.IsVolatile then
    begin
      if check_only then
        begin
          if FMasterVolatileObjStore.ExistsBinaryKey(@key,SizeOf(TGuid),dummy) then // TODO:Remove DEBUG CHECK
            begin
              if raise_ex then
                raise EFRE_DB_Exception.Create(edb_EXISTS,'cannot store volatile object')
              else
                result := edb_EXISTS
            end;
          result := edb_OK;
        end
      else
        begin
          if FMasterVolatileObjStore.InsertBinaryKey(@key,SizeOf(TGuid),FREDB_ObjectToPtrUInt(obj)) then
            result := edb_OK
          else
            if raise_ex then
              raise EFRE_DB_Exception.Create(edb_EXISTS,'cannot store volatile object')
            else
              result := edb_EXISTS;
          result := edb_OK;
        end;
    end
  else
    begin // Not Volatile
      Result := edb_OK;
      dummy := FREDB_ObjectToPtrUInt(obj);
      if check_only then
        begin
          if FMasterPersistantObjStore.ExistsBinaryKey(@key,SizeOf(TGuid),dummy) then
            exit(edb_EXISTS)
        end
      else
        begin
          if FMasterPersistantObjStore.InsertBinaryKeyOrFetch(@key,sizeof(tguid),dummy) then
            begin
              //writeln('>> REAL MASTER STORE OF ',obj.InternalUniqueDebugKey);
              if not FMasterPersistantObjStore.ExistsBinaryKey(@key,sizeof(tguid),dummy) then
                abort;
              result := edb_OK
            end
          else
            if raise_ex then
              raise EFRE_DB_Exception.Create(edb_EXISTS,'cannot store persistent object [%s]',[obj.InternalUniqueDebugKey])
            else
              exit(edb_EXISTS);
          if Length(references_to_list)>0 then
            begin
              result := _SetupInitialRefLinks(obj,references_to_list,raise_ex);
              if result<> edb_OK then
                exit;
            end;
        end;
    end;
end;

function TFRE_DB_Master_Data.DeleteObject(const obj_uid: TGuid; const raise_ex: boolean; const check_only: boolean): TFRE_DB_Errortype;
var dummy : PtrUInt;
begin
  if check_only then
    begin
      if GetReferenceCount(obj_uid,false) > 0 then
        if raise_ex then
          raise EFRE_DB_Exception.Create(edb_OBJECT_REFERENCED,'DELETE OF OBJECT [%s] FAILED, OBJECT IS REFERENCED',[GFRE_BT.GUID_2_HexString(obj_uid)])
        else
          exit(edb_OBJECT_REFERENCED);
      exit(edb_OK);
    end;
  if not FMasterVolatileObjStore.RemoveBinaryKey(@obj_uid,SizeOf(TGuid),dummy) then
    if not FMasterPersistantObjStore.RemoveBinaryKey(@obj_uid,SizeOf(TGuid),dummy) then
      if raise_ex then
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'DELETE OF OBJECT [%s] FAILED, OBJECT NOT FOUND',[GFRE_BT.GUID_2_HexString(obj_uid)])
      else
        exit(edb_NOT_FOUND);
  result := edb_OK;
end;

procedure TFRE_DB_Master_Data.ForAllObjectsInternal(const pers, volatile: boolean; const iter: TFRE_DB_Obj_Iterator);

  procedure ObjCallBack(var val:NativeUint);
  begin
    iter(FREDB_PtrUIntToObject(val) as TFRE_DB_Object);
  end;

begin
  if pers then
    FMasterPersistantObjStore.LinearScan(@ObjCallback);
  if volatile then
    FMasterPersistantObjStore.LinearScan(@ObjCallback);
end;

function TFRE_DB_Master_Data.MasterColls: TFRE_DB_CollectionManageTree;
begin
  result := FMasterCollectionStore;
end;

{ TFRE_DB_TextIndex }

procedure TFRE_DB_TextIndex.SetTranformedKeyDBS(const value: TFRE_DB_String; const update_key: boolean);
begin
  if FCaseInsensitive then
    inherited SetTranformedKeyDBS(UpperCase(value),update_key)
  else
    inherited SetTranformedKeyDBS(value,update_key);
end;

procedure TFRE_DB_TextIndex.StreamToThis(const stream: TStream);
begin
  Inherited StreamToThis(stream);
  if FCaseInsensitive then
    stream.WriteByte(1)
  else
    stream.WriteByte(0);
  StreamIndex(stream);
end;

constructor TFRE_DB_TextIndex.Create(const idx_name, fieldname: TFRE_DB_NameType; const fieldtype: TFRE_DB_FIELDTYPE; const unique, case_insensitive: boolean; const collection: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  inherited Create(idx_name,fieldname,fieldtype,unique,collection);
  FCaseInsensitive := case_insensitive;
end;

constructor TFRE_DB_TextIndex.CreateStreamed(const stream: TStream; const idx_name, fieldname: TFRE_DB_NameType; const fieldtype: TFRE_DB_FIELDTYPE; const unique: boolean; const collection: IFRE_DB_PERSISTANCE_COLLECTION);
var ci : Boolean;
begin
  ci := stream.ReadByte=1;
  Create(idx_name,fieldname,fieldtype,unique,ci,collection);
  LoadIndex(stream,collection);
end;


function TFRE_DB_TextIndex.FieldTypeIndexCompatCheck(fld: TFRE_DB_FIELD; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  if fld.FieldType<>fdbft_String then
    if raise_ex then
      raise EFRE_DB_Exception.Create(edb_ILLEGALCONVERSION,'the text index can only be used to index a string field, not a [%s] field. Maybe use a calculated field with results a string field',[fld.FieldTypeAsString])
    else
      exit(edb_ILLEGALCONVERSION);
  result := edb_OK;
end;

procedure TFRE_DB_TextIndex.TransformToBinaryComparable(fld: TFRE_DB_FIELD; const update_key: boolean);
begin
  SetTranformedKeyDBS(fld.AsString,update_key);
end;

function TFRE_DB_TextIndex.SupportsDataType(const typ: TFRE_DB_FIELDTYPE): boolean;
begin
  if typ=fdbft_String then
    exit(true)
  else
    exit(false)
end;


{ TFRE_DB_MM_Index }

constructor TFRE_DB_MM_Index.Create(const idx_name, fieldname: TFRE_DB_NameType; const fieldtype: TFRE_DB_FIELDTYPE; const unique: boolean; const collection: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  FIndex           := TFRE_ART_TREE.Create;
  FIndexName       := idx_name;
  FUniqueName      := UpperCase(FIndexName);
  FUnique          := unique;
  FFieldname       := fieldname;
  FUniqueFieldname := uppercase(fieldname);
  FFieldType       := fieldtype;
  FUnique          := unique;
  FCollection      := collection;
end;

function TFRE_DB_MM_Index.Indexname: TFRE_DB_NameType;
begin
  result := FIndexName;
end;

function TFRE_DB_MM_Index.Uniquename: PFRE_DB_NameType;
begin
  result := @FUniqueName;
end;

function TFRE_DB_MM_Index.IndexAddCheck(const obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean;const use_already_transformed_key:boolean): TFRE_DB_Errortype;
var fld          : TFRE_DB_FIELD;
    dummy        : NativeUint;
    values       : TFRE_DB_IndexValueStore;
begin
  if not use_already_transformed_key then
    begin
      if not obj.FieldOnlyExisting(FFieldname,fld) then
        if raise_ex then
          raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the field [%s] which should be indexed by index [%s] could not be found in object [%s]',[FFieldname,FIndexName,GFRE_BT.GUID_2_HexString(obj.UID)])
        else
          exit(edb_NOT_FOUND);
      result := FieldTypeIndexCompatCheck(fld,raise_ex);
      if result <> edb_OK then
        exit;
      TransformtoBinaryComparable(fld,false);
    end;
  if check_only then
      if FIndex.ExistsBinaryKey(@transkey,transkeylen,dummy) then
        begin
          if FUnique then
            if raise_ex then
              begin
               FIndex.ExistsBinaryKey(@transkey,transkeylen,dummy);
               raise EFRE_DB_Exception.Create(edb_EXISTS,'for the unique index [%s/%s/%s] the key already exists [%s]',[FCollection.CollectionName(false),FIndexName,FFieldname,GetStringRepresentationOfTransientKey])
              end
            else
              exit(edb_EXISTS)
          else
            begin
              values := FREDB_PtrUIntToObject(dummy) as TFRE_DB_IndexValueStore;
              if values.Exists(obj.UID) then
                if raise_ex then
                  raise EFRE_DB_Exception.Create(edb_EXISTS,'for the non unique index [%s/%s] the value(=obj) already exists',[FIndexName,FFieldname])
                else
                  exit(edb_EXISTS);
            end;
          exit(edb_OK);
        end
      else
        exit(edb_OK)
  else
    begin
      values := TFRE_DB_IndexValueStore.Create;
      dummy    := FREDB_ObjectToPtrUInt(values);
      if FIndex.InsertBinaryKey(@transkey,transkeylen,dummy) then
        begin
          if not FIndex.ExistsBinaryKey(@transkey,transkeylen,dummy) then
            begin
              abort;
              FIndex.InsertBinaryKey(@transkey,transkeylen,dummy);
            end;
          if not values.Add(obj) then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal index unique/empty/add failure');
          result := edb_OK;
        end
      else
        begin
          values.free;
          values := FREDB_PtrUIntToObject(dummy) as TFRE_DB_IndexValueStore;
          if FUnique then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal unique index add/exists failure')
          else
            if not values.Add(obj) then
              raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal index non unique add failure');
          result := edb_OK;
        end;
    end;
end;

function TFRE_DB_MM_Index.IndexUpdCheck(const new_obj, old_obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean; const use_already_transformed_key: boolean): TFRE_DB_Errortype;
var oldfld,newfld : TFRE_DB_FIELD;
    dummy         : NativeUint;
    values        : TFRE_DB_IndexValueStore;
begin
  if not use_already_transformed_key then
    begin
      old_obj.FieldOnlyExisting(FFieldname,oldfld);
      if not new_obj.FieldOnlyExisting(FFieldname,newfld) then
        if raise_ex then
          raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the field [%s] which should be indexed by index [%s] could not be found in object [%s]',[FFieldname,FIndexName,GFRE_BT.GUID_2_HexString(new_obj.UID)])
        else
          exit(edb_NOT_FOUND);
      result := FieldTypeIndexCompatCheck(newfld,raise_ex);
      if result <> edb_OK then
        exit;
      TransformtoBinaryComparable(oldfld,false);
      TransformtoBinaryComparable(newfld,true);
      FUpdateKeySame := CompareTransformedKeyAndUpdateKey;
    end;
  if check_only then
    begin
      if FUpdateKeySame then
        exit(edb_OK); // Keyfield has not changed
      if FIndex.ExistsBinaryKey(@updtranskey,updtranskeylen,dummy) then
        begin
          if FUnique then
            if raise_ex then
               raise EFRE_DB_Exception.Create(edb_EXISTS,'cant update / for the unique index [%s/%s] the key already exists',[FIndexName,FFieldname])
            else
              exit(edb_EXISTS)
          else
            begin
              values := FREDB_PtrUIntToObject(dummy) as TFRE_DB_IndexValueStore;
              if values.Exists(new_obj.UID) then
                if raise_ex then
                  raise EFRE_DB_Exception.Create(edb_EXISTS,'cant update / for the non unique index [%s/%s] the value(=obj) already exists',[FIndexName,FFieldname])
                else
                  exit(edb_EXISTS);
            end;
          exit(edb_OK);
        end
      else
        exit(edb_OK)
    end
  else
    begin
      abort;
      values := TFRE_DB_IndexValueStore.Create;
      dummy    := FREDB_ObjectToPtrUInt(values);
      if FIndex.InsertBinaryKey(@transkey,transkeylen,dummy) then
        begin
          if not FIndex.ExistsBinaryKey(@transkey,transkeylen,dummy) then
            abort;
          if not values.Add(new_obj) then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal index unique/empty/add failure');
          result := edb_OK;
        end
      else
        begin
          values.free;
          values := FREDB_PtrUIntToObject(dummy) as TFRE_DB_IndexValueStore;
          if FUnique then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal unique index add/exists failure')
          else
            if not values.Add(new_obj) then
              raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected internal index non unique add failure');
          result := edb_OK;
        end;
    end;
end;

function TFRE_DB_MM_Index.IndexDelCheck(const obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean; const use_already_transformed_key: boolean): TFRE_DB_Errortype;
begin

end;

function TFRE_DB_MM_Index.IsUnique: Boolean;
begin
  result := FUnique;
end;

function TFRE_DB_MM_Index.GetStringRepresentationOfTransientKey: String;
begin
  SetLength(result,transkeylen);
  move(transkey,result[1],transkeylen);
end;

procedure TFRE_DB_MM_Index.SetTranformedKeyDBS(const value: TFRE_DB_String; const update_key: boolean);
begin
  if not update_key then
    begin
      FillByte(transkey,Length(transkey),0);
      transkeylen := Length(value);
      Move(value[1],transkey[0],transkeylen);
    end
  else
    begin
      FillByte(transkey,Length(transkey),0);
      updtranskeylen := Length(value);
      Move(value[1],updtranskey[0],updtranskeylen);
    end
end;

function TFRE_DB_MM_Index.FetchIndexedValsTransformedKey(var obj: TFRE_DB_ObjectArray): boolean;
var dummy : NativeUint;
begin
  result := FIndex.ExistsBinaryKey(@transkey,transkeylen,dummy);
  if result then
    obj := (FREDB_PtrUIntToObject(dummy) as TFRE_DB_IndexValueStore).IndexedObjects
  else
    obj := nil;
end;

function TFRE_DB_MM_Index.CompareTransformedKeyAndUpdateKey: boolean;
begin
  if updtranskeylen=transkeylen then
    if CompareMemRange(@transkey[0],@updtranskey[0],transkeylen)=0 then
      exit(true);
  exit(false);
end;

procedure TFRE_DB_MM_Index.StreamToThis(const stream: TStream);
begin
  stream.WriteAnsiString(ClassName);
  stream.WriteAnsiString(FIndexName);
  stream.WriteAnsiString(FFieldname);
  stream.WriteAnsiString(CFRE_DB_FIELDTYPE_SHORT[FFieldType]);
  if FUnique then
    stream.WriteByte(1)
  else
    stream.WriteByte(0)
end;

procedure TFRE_DB_MM_Index.StreamIndex(const stream: TStream);
var i:NativeInt;

  procedure StreamKeyVal(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
  var ixs : TFRE_DB_IndexValueStore;
  begin
    stream.WriteQWord(KeyLen);
    stream.WriteBuffer(Key^,KeyLen);
    ixs := FREDB_PtrUIntToObject(value) as TFRE_DB_IndexValueStore;
    ixs.StreamToThis(stream);
  end;

begin
  i := FIndex.GetValueCount;
  stream.WriteQWord(i);
  FIndex.LinearScanKeyVals(@StreamKeyVal);
end;

procedure TFRE_DB_MM_Index.LoadIndex(const stream: TStream ; const coll: IFRE_DB_PERSISTANCE_COLLECTION);
var i,cnt      : NativeInt;
    keylen     : NativeUint;
    key        : RawByteString;
    ixs        : TFRE_DB_IndexValueStore;

begin
  cnt := stream.ReadQWord;
  for i := 1 to cnt do
    begin
      keylen := stream.ReadQWord;
      SetLength(key,keylen);
      stream.ReadBuffer(Key[1],keylen);
      ixs := TFRE_DB_IndexValueStore.Create;
      ixs.LoadFromThis(stream,coll);
      if not FIndex.InsertBinaryKey(@key[1],keylen,FREDB_ObjectToPtrUInt(ixs)) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'stream load : index add failure [%s]',[key]);
    end;
end;

class function TFRE_DB_MM_Index.CreateFromStream(const stream: TStream ; const coll : IFRE_DB_PERSISTANCE_COLLECTION): TFRE_DB_MM_Index;
var
    cn,idxn,fieldn : String;
    ft             : TFRE_DB_FIELDTYPE;
    unique         : boolean;

begin
  cn     := stream.ReadAnsiString;
  idxn   := stream.ReadAnsiString;
  fieldn := stream.ReadAnsiString;
  ft     := FREDB_FieldtypeShortString2Fieldtype(stream.ReadAnsiString);
  unique := stream.ReadByte=1;
  case cn of
    'TFRE_DB_TextIndex'     : result := TFRE_DB_TextIndex.CreateStreamed(stream,idxn,fieldn,ft,unique,coll);
    'TFRE_DB_SignedIndex'   : result := TFRE_DB_SignedIndex.CreateStreamed(stream,idxn,fieldn,ft,unique,coll);
    'TFRE_DB_UnsignedIndex' : result := TFRE_DB_UnsignedIndex.CreateStreamed(stream,idxn,fieldn,ft,unique,coll);
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'Unsupported streaming index class [%s]',[cn]);
  end;
end;

{ TFRE_DB_CollectionTree }

constructor TFRE_DB_CollectionManageTree.Create;
begin
  FCollTree := TFRE_ART_TREE.Create;
end;

function TFRE_DB_CollectionManageTree.NewCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean; const pers_layer: IFRE_DB_PERSISTANCE_LAYER): TFRE_DB_Errortype;
var coll     : TFRE_DB_Persistance_Collection;
    safename : TFRE_DB_NameType;
begin
  safename := UpperCase(coll_name);
  if FCollTree.ExistsBinaryKey(@safename[1],Length(safename),dummy) then
    begin
      Collection := TFRE_DB_Persistance_Collection(dummy);
      result     := edb_EXISTS;
    end
  else
    begin
      coll := TFRE_DB_Persistance_Collection.Create(coll_name,volatile_in_memory,pers_layer);
      if FCollTree.InsertBinaryKey(@coll.UniqueName^[1],length(coll.UniqueName^),FREDB_ObjectToPtrUInt(coll)) then
        begin
          Collection := coll;
          exit(edb_OK);
        end
      else
        begin
          coll.Free;
          exit(edb_INTERNAL);
        end;
    end;
end;

function TFRE_DB_CollectionManageTree.DeleteCollection(const coll_name: TFRE_DB_NameType): TFRE_DB_Errortype;
var coll     : TFRE_DB_Persistance_Collection;
    safename : TFRE_DB_NameType;
begin
  safename := UpperCase(coll_name);
  if FCollTree.RemoveBinaryKey(@safename[1],Length(safename),dummy) then
    begin
      Coll := TFRE_DB_Persistance_Collection(dummy);
      result     := edb_OK;
      Coll.Free;
    end
  else
    begin
      result := edb_NOT_FOUND;
    end;
end;

function TFRE_DB_CollectionManageTree.GetCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): boolean;
var coll     : TFRE_DB_Persistance_Collection;
    safename : TFRE_DB_NameType;
begin
  safename:=uppercase(coll_name);
  if FCollTree.ExistsBinaryKey(@safename[1],length(safename),dummy) then
    begin
      Collection := TFRE_DB_Persistance_Collection(dummy);
      result     := true;
    end
  else
    begin
      Result := false;
    end;
end;

procedure TFRE_DB_CollectionManageTree.ForAllCollections(const iter: TFRE_DB_PersColl_Iterator);

  function IterateColls(var dummy:NativeUInt):boolean;
  begin
    result := iter(FREDB_PtrUIntToObject(dummy) as TFRE_DB_Persistance_Collection)
  end;

begin
  FCollTree.LinearScanBreak(@IterateColls);
end;

{ TFRE_DB_Persistance_Collection }

function TFRE_DB_Persistance_Collection.IsVolatile: boolean;
begin
  result := FVolatile;
end;

function TFRE_DB_Persistance_Collection.IndexExists(const idx_name: TFRE_DB_NameType): NativeInt;
var
  i           : Integer;
  FUniqueName : TFRE_DB_NameType;
begin
  result := -1;
  FUniqueName := UpperCase(idx_name);
  for i := 0 to high(FIndexStore) do
    if FIndexStore[i].Uniquename^=FUniqueName then
      exit(i);
end;

procedure TFRE_DB_Persistance_Collection.AddIndex(const idx: TFRE_DB_MM_Index);
var high : NativeInt;
begin
  high := Length(FIndexStore);
  SetLength(FIndexStore,high+1);
  FIndexStore[high] := idx;
end;

function TFRE_DB_Persistance_Collection.IndexAddCheck(const obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean; const use_already_transformed_key: boolean): TFRE_DB_Errortype;
var i : NativeInt;
begin
  for i:= 0 to high(FIndexStore) do
    begin
      result := FIndexStore[i].IndexAddCheck(obj,check_only,raise_ex,use_already_transformed_key);
      if result <> edb_OK then
        exit;
    end;
end;

function TFRE_DB_Persistance_Collection.IndexUpdCheck(const new_obj, old_obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean; const use_already_transformed_key: boolean): TFRE_DB_Errortype;
var i : NativeInt;
begin
  for i:= 0 to high(FIndexStore) do
    begin
      result := FIndexStore[i].IndexUpdCheck(new_obj, old_obj,check_only,raise_ex,use_already_transformed_key);
      if result <> edb_OK then
        exit;
    end;
end;

function TFRE_DB_Persistance_Collection.IndexDelCheck(const del_obj: TFRE_DB_Object; const check_only: boolean; const raise_ex: boolean; const use_already_transformed_key: boolean): TFRE_DB_Errortype;
var i : NativeInt;
begin
  for i:= 0 to high(FIndexStore) do
    begin
      result := FIndexStore[i].IndexDelCheck(del_obj,check_only,raise_ex,use_already_transformed_key);
      if result <> edb_OK then
        exit;
    end;
end;

constructor TFRE_DB_Persistance_Collection.Create(const coll_name: TFRE_DB_NameType; Volatile: Boolean; const pers_layer: IFRE_DB_PERSISTANCE_LAYER);
begin
 FGuidObjStore := TFRE_ART_TREE.Create;
 FName         := coll_name;
 FVolatile     := Volatile;
 FLayer        := pers_layer;
 FUpperName    := UpperCase(FName);
end;

function TFRE_DB_Persistance_Collection.Count: int64;
begin
  result := FGuidObjStore.GetValueCount;
end;

function TFRE_DB_Persistance_Collection.Exists(const ouid: TGUID): boolean;
begin
  result := FGuidObjStore.ExistsBinaryKey(@ouid,SizeOf(ouid),dummy);
end;

function TFRE_DB_Persistance_Collection.Delete(const ouid: TGUID; const raise_ex: boolean; var ncolls: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
  result := FLayer.DeleteObject(ouid,raise_ex,CollectionName(true),ncolls);
  //result := FGuidObjStore.RemoveBinaryKey(@ouid,SizeOf(ouid),dummy);
end;

//function TFRE_DB_Persistance_Collection.AddCheck(const new_guid: TGuid; const obj: TFRE_DB_Object): boolean;
//begin
//  result := FGuidObjStore.InsertBinaryKey(@new_guid,SizeOf(new_guid),NativeUint(obj));
//end;

procedure TFRE_DB_Persistance_Collection.Clear;
begin
  FGuidObjStore.Clear;
end;

procedure TFRE_DB_Persistance_Collection.ForAllItems(const iter: TFRE_DB_Obj_Iterator);
  procedure ForAll(var val:PtrUInt);
  var newobj : TFRE_DB_Object;
  begin
    newobj := CloneOutObject(FREDB_PtrUIntToObject(val) as TFRE_DB_Object);
    iter(newobj);
  end;
begin
  FGuidObjStore.LinearScan(@ForAll);
end;

function TFRE_DB_Persistance_Collection.ForAllitemsBreak(const func: TFRE_DB_Obj_IteratorBreak): boolean;
  function ForAll(var val:PtrUInt):boolean;
  var newobj : TFRE_DB_Object;
  begin
    newobj := CloneOutObject(FREDB_PtrUIntToObject(val) as TFRE_DB_Object);
    result := func(newobj);
  end;
begin
  result := FGuidObjStore.LinearScanBreak(@ForAll);
end;

function TFRE_DB_Persistance_Collection.Store(var new_obj: TFRE_DB_Object; const raise_ex: boolean; var ncolls: TFRE_DB_StringArray): TFRE_DB_Errortype;
begin
   if FVolatile then
    new_obj.Set_Volatile;
  result := FLayer.StoreOrUpdateObject(new_obj,FName,true,raise_ex,ncolls);
end;

// An object is allowed only once in a collection, but can be stored in multiple collections
// An object is always at least in one collection, dangling objects (without beeing in a collection) are errors
// All subobjects are stored and fetchable in the "Master" store too
// Subobjects can only be parented once (can only be part of one object), thus need to be unique

function TFRE_DB_Persistance_Collection.StoreInThisColl(const  new_obj: TFRE_DB_Object; const raise_ex: boolean ; const checkphase : boolean): TFRE_DB_Errortype;
begin
  // Check existance in this collection
  if checkphase then
    begin
      if FPrepared then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic failure, should not be prepared [%s]',[FName]);
      if FGuidObjStore.ExistsBinaryKey(new_obj.UIDP,SizeOf(TGuid),dummy) then
        if raise_ex then
          raise EFRE_DB_Exception.Create(edb_EXISTS,'object [%s] already exists on store in collection [%s]',[new_obj.UID_String,FName])
        else
          exit(edb_EXISTS);
      result  := IndexAddCheck(new_obj,true,raise_ex,false);
      if result<>edb_OK then
        exit;
      FPrepared := True;
    end
  else
    begin
      if not FPrepared then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic failure, should be prepared');
      try
        result := IndexAddCheck(new_obj,false,raise_ex,true);
        if result<>edb_OK then
          exit;
        if not FGuidObjStore.InsertBinaryKey(new_obj.UIDP,SizeOf(TGUID),FREDB_ObjectToPtrUInt(new_obj)) then
          if raise_ex then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'store of object [%s] in collection [%s] failed -> already exists on store after exist check ?',[new_obj.UID_String,FName])
          else
            exit(edb_EXISTS);
        new_obj.__InternalCollectionAdd(self); // Add The Colection Reference to a directly stored master or child object
        //if Assigned(FBaseConn) then
        //  FBaseConn.AssociateObject(new_obj);
      finally
        FPrepared := false;
        assert(length(new_obj.__InternalGetCollectionList)>0);
      end;
    end;
end;

function TFRE_DB_Persistance_Collection.UpdateInThisColl(const new_obj, old_obj: TFRE_DB_Object; const raise_ex: boolean; const checkphase: boolean): TFRE_DB_Errortype;
begin
  abort;
  // Check existance in this collection
  if checkphase then
    begin
      result  := IndexUpdCheck(new_obj,old_obj,true,raise_ex,false);
      if result<>edb_OK then
        exit;
    end
  else
    begin
     abort;
      //result := IndexAddCheck(upd_obj,false,raise_ex,true);
      //if result<>edb_OK then
      //  exit;
      //if not FGuidObjStore.InsertBinaryKey(upd_obj.UIDP,SizeOf(TGUID),FREDB_ObjectToPtrUInt(upd_obj)) then
      //  if raise_ex then
      //    raise EFRE_DB_Exception.Create(edb_INTERNAL,'store of object [%s] in collection [%s] failed -> already exists on store after exist check ?',[upd_obj.UID_String,FName])
      //  else
      //    exit(edb_EXISTS);
      //upd_obj.__InternalCollectionAdd(self);
    end;
end;

function TFRE_DB_Persistance_Collection.DeleteFromThisColl(const del_obj: TFRE_DB_Object; const raise_ex: boolean; const checkphase: boolean): TFRE_DB_Errortype;
begin
  if checkphase then
    begin
      writeln('MISSING: CHECK INDICES');
      exit(edb_OK)
    end
  else
    begin
     writeln('MISSING : UPDATE INDICES!!');
     exit(edb_OK);
    end;
end;

function TFRE_DB_Persistance_Collection.CloneOutObject(const inobj: TFRE_DB_Object): TFRE_DB_Object;
begin
  inobj.Assert_CheckStoreLocked;
  inobj.Set_Store_Locked(false);
  try
   if Length(inobj.__InternalGetCollectionList)<1 then
     abort;;
   result := inobj.CloneToNewObject;
   if result = inobj then
     abort;
  finally
    inobj.Set_Store_Locked(true);
  end;
end;

function TFRE_DB_Persistance_Collection.CloneOutArray(const objarr: TFRE_DB_ObjectArray): TFRE_DB_ObjectArray;
var i:NativeInt;
begin
  SetLength(result,length(objarr));
  for i:=0 to high(objarr) do
    result[i] := CloneOutObject(objarr[i]);
end;

procedure TFRE_DB_Persistance_Collection.StreamToThis(const stream: TStream);
var i,cnt,vcnt : nativeint;

   procedure AllGuids(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
   var s:string[16];
   begin
     assert(KeyLen=16);
     SetLength(s,16);
     move(key^,s[1],16);
     stream.WriteAnsiString(s); // guid;
     inc(vcnt);
   end;

begin
  if FVolatile then
    abort;
  stream.Position:=0;
  stream.WriteAnsiString('FDBC');
  stream.WriteAnsiString(FName);
  cnt  := FGuidObjStore.GetValueCount;
  vcnt := 0;
  stream.WriteQWord(cnt);
  FGuidObjStore.LinearScanKeyVals(@AllGuids);
  assert(vcnt=cnt);
  stream.WriteQWord(length(FIndexStore));
  for i:=0 to high(FIndexStore) do
    FIndexStore[i].StreamToThis(stream);
end;

procedure TFRE_DB_Persistance_Collection.LoadFromThis(const stream: TStream);
var in_txt : String;
    cnt,i  : NativeInt;
    uid    : TGuid;
    dbo    : TFRE_DB_Object;
begin
  in_txt := stream.ReadAnsiString;
  if in_txt<>'FDBC' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'COLLECTION STREAM INVALID : signature bad');
  in_txt := stream.ReadAnsiString;
  if in_txt<>FName then
    raise EFRE_DB_Exception.Create(edb_ERROR,'COLLECTION STREAM INVALID NAME DIFFERS: [%s <> %s]',[]);
  cnt := stream.ReadQWord;
  //writeln('RELOADING COLLECTION ',in_txt,' / ',cnt);
  for i := 1 to cnt do
    begin
      in_txt := stream.ReadAnsiString; // guid;
      assert(Length(in_txt)=16);
      move(in_txt[1],uid,16);
      if not FLayer.Fetch(uid,dbo,true) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'COLLECTION LOAD / FETCH FAILED [%s]',[GFRE_BT.GUID_2_HexString(uid)]);
      if not FGuidObjStore.InsertBinaryKey(dbo.UIDP,SizeOf(TGUID),FREDB_ObjectToPtrUInt(dbo)) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'COLLECTION LOAD / INSERT FAILED [%s] EXISTS',[GFRE_BT.GUID_2_HexString(uid)]);
      dbo.__InternalCollectionAdd(self);
    end;
  cnt := stream.ReadQWord;
  SetLength(FIndexStore,cnt);
  for i := 0 to high(FIndexStore) do
    FIndexStore[i] := TFRE_DB_MM_Index.CreateFromStream(stream,self);
end;

function TFRE_DB_Persistance_Collection.CollectionName(const unique: boolean): TFRE_DB_NameType;
begin
  if unique then
    result := UniqueName^
  else
    result := FName;
end;

function TFRE_DB_Persistance_Collection.GetPersLayerIntf: IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
begin
  result := self;
end;

function TFRE_DB_Persistance_Collection.Fetch(const uid: TGUID; var obj: TFRE_DB_Object): boolean;
begin
  result := FGuidObjStore.ExistsBinaryKey(@uid,SizeOf(TGuid),dummy);
  if result then
    obj := CloneOutObject(FREDB_PtrUIntToObject(dummy) as TFRE_DB_Object)
  else
    obj := nil;
end;

function TFRE_DB_Persistance_Collection.LinearScan(const fieldname: TFRE_DB_NameType; const field_expr: TFRE_DB_FIELD_EXPRESSION): TFRE_DB_Object;
var obj : TFRE_DB_Object;

  function ForAll(var val:PtrUInt):boolean;
  begin
    obj := TFRE_DB_Object(val);
    obj.Assert_CheckStoreLocked;
    obj.Set_Store_Locked(false);
    try
      result := field_expr(obj.Field(fieldname));
    finally
      obj.Set_Store_Locked(true);
    end;
  end;

begin
 if FGuidObjStore.LinearScanBreak(@ForAll) then
   result := CloneOutObject(obj)
 else
   result := nil;
end;

function TFRE_DB_Persistance_Collection.First: TFRE_DB_Object;
  procedure SetIt(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
  begin
    result := TFRE_DB_Object(value);
  end;
begin
  abort;
  FGuidObjStore.FirstKeyVal(@SetIt);
end;

function TFRE_DB_Persistance_Collection.Last: TFRE_DB_Object;
  procedure SetIt(var value : NativeUInt ; const Key : PByte ; const KeyLen : NativeUint);
  begin
    result := TFRE_DB_Object(value);
  end;
begin
  abort;
  FGuidObjStore.LastKeyVal(@SetIt);
end;

function TFRE_DB_Persistance_Collection.GetItem(const num: uint64): TFRE_DB_Object;
begin
  abort;
end;

function TFRE_DB_Persistance_Collection.DefineIndexOnField(const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType): TFRE_DB_Errortype;
var index    : TFRE_DB_MM_Index;
begin
  if IndexExists(index_name)>=0 then
    exit(edb_EXISTS);
  case FieldType of
    fdbft_GUID,
    fdbft_Boolean,
    fdbft_Byte,
    fdbft_UInt16,
    fdbft_UInt32,
    fdbft_UInt64 :
      begin
        index := TFRE_DB_UnsignedIndex.Create(index_name,fieldname,fieldtype,unique,self);
      end;
    fdbft_Int16,    // invert Sign bit by xor (1 shl (bits-1)), then swap endian
    fdbft_Int32,
    fdbft_Int64,
    fdbft_Currency, // = int64*10000;
    fdbft_DateTimeUTC:
      begin
        index := TFRE_DB_SignedIndex.Create(index_name,fieldname,fieldtype,unique,self);
      end;
    //fdbft_Real32: ;
    //fdbft_Real64: ;
    fdbft_String:
      begin
        index := TFRE_DB_TextIndex.Create(index_name,FieldName,FieldType,unique,ignore_content_case,self);
      end;
    //fdbft_Stream: ;
    //fdbft_Object: ;
    //fdbft_ObjLink: ;
    //fdbft_CalcField: ;
    else exit(edb_UNSUPPORTED);
  end;
  AddIndex(index);
end;

// Check if a field can be removed safely from an object stored in this collection, or if an index exists on that field
//TODO -> handle indexed field change
function TFRE_DB_Persistance_Collection.CheckFieldChangeAgainstIndex(const oldfield, newfield: TFRE_DB_FIELD; const change_type: TFRE_DB_ObjCompareEventType; const check: boolean; const raise_ex: boolean): TFRE_DB_Errortype;
var i : NativeInt;
begin
  //raise EFRE_DB_Exception.Create(edb_EXISTS,'an update to the object [%s] would delete field [%s], which is against an index of collection [%s]',[newfield.ParentObject.UID_String,newfield.FieldName,collarray[j].CollectionName(false)])
  //raise EFRE_DB_Exception.Create(edb_INTERNAL,'an update to the object [%s] would add field [%s], which is an index field of collection [%s], this cant be a valid update ',[newfield.ParentObject.UID_String,newfield.FieldName,collarray[j].CollectionName(false)])
  case change_type of
    cev_FieldDeleted: ;
    cev_FieldAdded: ;
    cev_FieldChanged: ;
  end;
  for i := 0 to high(FIndexStore) do
    if FIndexStore[i].FUniqueFieldname=uppercase(newfield.FieldName) then
      abort;
  //for i := 0 to high(FIndexStore) do
  //  if FIndexStore[i].FUniqueFieldname=uppercase(newfield.FieldName) then
  //      exit(edb_EXISTS);
  result := edb_OK;
end;

function TFRE_DB_Persistance_Collection.GetIndexedObj(const query_value: TFRE_DB_String; out obj: TFRE_DB_Object; const index_name: TFRE_DB_NameType): boolean;
var arr   : TFRE_DB_ObjectArray;
begin
  result := GetIndexedObj(query_value,arr,index_name,true);
  if result then
    begin
      if Length(arr)<>1 then
        raise EFRE_DB_Exception.create(edb_INTERNAL,'a unique index internal store contains [%d] elements!',[length(arr)]);
      obj := arr[0];
    end;
end;

function TFRE_DB_Persistance_Collection.GetIndexedObj(const query_value: TFRE_DB_String; out obj: TFRE_DB_ObjectArray; const index_name: TFRE_DB_NameType; const check_is_unique: boolean): boolean;
begin
  result := _GetIndexedObj(query_value,obj,index_name,check_is_unique,true);
end;

function TFRE_DB_Persistance_Collection._GetIndexedObj(const query_value: TFRE_DB_String; out obj: TFRE_DB_ObjectArray; const index_name: TFRE_DB_NameType; const check_is_unique: boolean; const clone_out: boolean): boolean;
var idx   : NativeInt;
    index : TFRE_DB_MM_Index;
    arr   : TFRE_DB_ObjectArray;
begin
  idx := IndexExists(index_name);
  if idx=-1 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the requested index named [%s] does not exist on collection [%s]',[index_name,FName]);
  index := FIndexStore[idx];
  if check_is_unique and
     not index.IsUnique then
       raise EFRE_DB_Exception.Create(edb_ERROR,'the requested index named [%s] is not unique you must not use a point query',[index_name]);
  if not index.SupportsDataType(fdbft_String) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'the requested index named [%s] does not support a query of [%s]',[index_name,CFRE_DB_FIELDTYPE[fdbft_String]]);
  index.SetTranformedKeyDBS(query_value,false);
  result := index.FetchIndexedValsTransformedKey(arr);
  if clone_out then
    obj := CloneOutArray(arr)
  else
    obj := arr; // no copy
end;

procedure TFRE_DB_Persistance_Collection.InternalUnprepare;
begin
  FPrepared:=false;
end;

function TFRE_DB_Persistance_Collection.FetchIntFromColl(const uid: TGuid; var obj: TFRE_DB_Object): boolean;
begin
  result := FGuidObjStore.ExistsBinaryKey(@uid,SizeOf(TGuid),dummy);
  if result then
    obj := FREDB_PtrUIntToObject(dummy) as TFRE_DB_Object
  else
    obj := nil;
end;

function TFRE_DB_Persistance_Collection.GetIndexedUID(const query_value: TFRE_DB_String; out obj_uid: TGUID; const index_name: TFRE_DB_NameType): boolean;
var ouidarr : TFRE_DB_GUIDArray;
begin
  result:=GetIndexedUID(query_value,ouidarr,index_name,true);
  if result then
    obj_uid := ouidarr[0];
end;

function TFRE_DB_Persistance_Collection.GetIndexedUID(const query_value: TFRE_DB_String; out obj_uid: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const check_is_unique: boolean): boolean;
var objarr : TFRE_DB_ObjectArray;
         i : NativeInt;
begin
  result:=_GetIndexedObj(query_value,objarr,index_name,check_is_unique,False);
  SetLength(obj_uid,Length(objarr));
  for i:=0 to high(objarr) do
    obj_uid[i] := objarr[i].UID;
end;

function TFRE_DB_Persistance_Collection.UniqueName: PFRE_DB_NameType;
begin
  UniqueName := @FUpperName;
end;

initialization

end.

