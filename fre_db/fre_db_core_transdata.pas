unit fre_db_core_transdata;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2012, FirmOS Business Solutions GmbH
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
     Classes, SysUtils,fos_sparelistgen,fre_db_interface,fre_db_core,fos_art_tree,fos_basis_tools,fos_tool_interfaces,fos_arraygen,fre_db_persistance_common,crc;
var
    cFRE_INT_TUNE_SYSFILTEXTENSION_SZ : NativeUint = 128;

type
  TFRE_DB_TRANS_COLL_DATA = class;

  TFRE_DB_DC_ORDER = record
    order_field      : TFRE_DB_NameType;
    //order_field_type : TFRE_DB_FIELDTYPE;
    ascending        : boolean;
  end;

  TFRE_DB_DC_ORDER_LIST = array of TFRE_DB_DC_ORDER;

  TFRE_DB_DC_ORDER_ITERATOR = procedure(const order:TFRE_DB_DC_ORDER) is nested;

  { TFRE_DB_DC_ORDER_DEFINITION }

  TFRE_DB_DC_ORDER_DEFINITION = class { defines a globally stored ordered and transformed set of dbo's}
  private
    FOrderList     : array of TFRE_DB_DC_ORDER;
    FKey           : TFRE_DB_TRANS_COLL_DATA_KEY;
    FKeyPartMaj    : TFRE_DB_NameType; { may be a Parentcollectionname or an starting uid }
    FKeyPartMin    : TFRE_DB_NameType; { may be a dc name or a ReflinkDefinition }
    FRefLinkCh     : TFRE_DB_NameTypeRLArray;
    FIsRefLinkM    : Boolean;
    function    IsSealed : Boolean;
  public
    procedure   MustNotBeSealed;
    procedure   MustBeSealed;
    procedure   SetDataKeyColl    (const parent_collectionname,derivedcollname       : TFRE_DB_NameType);
    procedure   SetDataKeyReflink (const startuid : TFRE_DB_GUID ; const RLChainSpec : TFRE_DB_NameTypeRLArray);
    procedure   ClearOrders       ;
    procedure   AddOrderDef       (const orderfield_name : TFRE_DB_NameType ; const asc : boolean);
    procedure   Seal              ;
    function    GetBaseKeyPart    : TFRE_DB_TRANS_COLL_DATA_KEY;
    function    GetFullKeyDef     : TFRE_DB_TRANS_COLL_DATA_KEY;
    procedure   ForAllOrders      (const orderiterator : TFRE_DB_DC_ORDER_ITERATOR);
  end;

  RDC_Filter_Entry=record
    key          : TFRE_DB_NameType;
    fldname      : TFRE_DB_NameType;
    flttype      : TFRE_DB_FILTERTYPE;
    filtervalcnt : NativeUInt;
    filtervalues : Pointer; {points to a mem block of filter values}
  end;

  PRDC_Filter_Entry = ^RDC_Filter_Entry;

  OFRE_SL_RDC_Filter_Def  = specialize OFOS_SpareList<RDC_Filter_Entry>;

  { TFRE_DB_DC_FILTER_DEFINITION }

  TFRE_DB_DC_FILTER_DEFINITION = class(TFRE_DB_DC_FILTER_DEFINITION_BASE)
  private
    FFilterKey : TFRE_DB_TRANS_COLL_DATA_KEY;
    FKeyList   : OFRE_SL_RDC_Filter_Def; { for a stable hash it may be necessary to order the filter list (add/delete)}
    function   IsSealed : Boolean;
  public
    constructor Create               ;
    destructor  Destroy              ;override;
    procedure   AddFilters           (const source : TFRE_DB_DC_FILTER_DEFINITION_BASE);
    procedure   MustNotBeSealed      ;
    procedure   MustBeSealed         ;
    procedure   Seal                 ;
    function    GetFilterKey         : TFRE_DB_TRANS_COLL_DATA_KEY;
  end;

  { TFRE_DB_QUERY }

  TFRE_DB_QUERY=class(TFRE_DB_QUERY_BASE)
  private
    function GetFilterDefinition: TFRE_DB_DC_FILTER_DEFINITION;
    function GetOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION;
  protected
     FBaseData               : TFRE_DB_TRANS_COLL_DATA;      { assigned transformed and ordered data }
     FConnection             : IFRE_DB_CONNECTION;           { used for right profile check (implicit righfilter) }
     FQueryId                : TFRE_DB_NameType;             { ID of this specific Query }
     //FCollTransfromKey      : TFRE_DB_NameTypeRL;          { an order is created and diversificated by the transformation(dc name) and the parent collection }
     FReferenceMode          : Boolean;                      { true if reference query, false if collection query }
     FParentChldLinkFldSpec  : TFRE_DB_NameTypeRL;
     FQueryFilters           : TFRE_DB_DC_FILTER_DEFINITION; { managed here, cleanup here}
     FOrderDef               : TFRE_DB_DC_ORDER_DEFINITION;  { linked to order definition of base ordered data, dont cleanup here}
     FDependcyUids           : Array of TFRE_DB_GUIDArray;   { this is a special filter type, the GUID array may at some later time be extended to support other field types }
     FParentIds              : TFRE_DB_GUIDArray;            { path to root}
     FUserKey                : TFRE_DB_String;               { Login@Domain | GUID ?}
     FFullTextFilter         : TFRE_DB_String;               { use this on all string fields, if set}
     FStartIdx               : NativeInt;                    { }
     FToDeliverCount         : NativeInt;                    { }
     FResultDBOs             : TFRE_DB_GUIDArray;            { }

     FQueryRunning           : boolean;
     FQueryStartTime         : NativeInt;
     FQueryEndTime           : NativeInt;
     FQueryCurrIdx           : NativeInt;
     FQueryDeliveredCount    : NativeInt;
     FQueryPotentialCount    : NativeInt;

     FQCreationTime : TFRE_DB_DateTime64;

     procedure               StartQueryRun;
     procedure               EndQueryRun;
     //function                TestObjAgainstQueryFilterAndIncIdx (const obj : IFRE_DB_Object): Boolean; { filter must match and start condition must match, true=add to result }
  public
     constructor Create;
     destructor  Destroy              ; override;
     function    GetQueryID           : TFRE_DB_NameType; override;
     function    HasOrderDefinition   : boolean;
     property    Orderdef             : TFRE_DB_DC_ORDER_DEFINITION  read GetOrderDefinition; { lazy create on access}
     property    Filterdef            : TFRE_DB_DC_FILTER_DEFINITION read GetFilterDefinition; { lazy create on access}
     function    GetBaseTransDataKey  : TFRE_DB_TRANS_COLL_DATA_KEY;
     function    GetFullQueryOrderKey : TFRE_DB_TRANS_COLL_DATA_KEY;
     procedure   SetBaseOrderedData   (const basedata   : TFRE_DB_TRANS_RESULT_BASE);override;
     procedure   SetMaxResultDBOLen   ;
     procedure   SetResultUID         (const idx: NativeInt; const uid: TFRE_DB_GUID);
     procedure   AddjustResultDBOLen  ;
     function    Execute              (const iterator   : IFRE_DB_Obj_Iterator):NativeInt;override; { execute the query, determine count and array of result dbo's }
  end;

  { TFRE_DB_TRANFORMED_DATA }

  TFRE_DB_TRANFORMED_DATA=class
  private
    FBaseKey         : TFRE_DB_NameTypeRL;
    FTransformeddata : IFRE_DB_ObjectArray;
    FTDCreationTime  : TFRE_DB_DateTime64;
  public
    function    GetTransFormKey : TFRE_DB_NameTypeRL;
    function    GetDataArray    : PFRE_DB_ObjectArray;
    constructor Create(const base_key : TFRE_DB_NameTypeRL);
  end;

  { TFRE_DB_OrderContainer }

  TFRE_DB_OrderContainer=class
  private
    FOBJArray : OFRE_SL_TFRE_DB_Object;
    function AddObject(const obj : TFRE_DB_Object):boolean;
    function Exists   (const obj : TFRE_DB_Object):boolean;
  public
    procedure   ForAllBreak(const iter : IFRE_DB_ObjectIteratorBrk ; var halt : boolean);
    constructor Create;
  end;

  { TFRE_DB_FilterContainer }

  TFRE_DB_FilterContainer=class
  private
    FOBJArray : Array of IFRE_DB_Object;
    FCnt      : NativeUint;
    FFilled   : Boolean;
    FFCCreationTime  : TFRE_DB_DateTime64;
    procedure SetFilled(AValue: boolean);
  public
    property    IsFilled         : boolean read FFilled write SetFilled;
    procedure   Execute          (const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY);
    procedure   CheckFilteredAdd (const obj : IFRE_DB_Object);
    procedure   AdjustLength     ;
    constructor Create;
  end;


  { TFRE_DB_TRANS_COLL_DATA }

  TFRE_DB_TRANS_COLL_DATA=class(TFRE_DB_TRANS_RESULT_BASE)
  protected
    FOrderDef          : TFRE_DB_DC_ORDER_DEFINITION;
    FTransCollDataLock : IFOS_LOCK;
    FBaseTransData     : TFRE_DB_TRANFORMED_DATA;
    FArtTreeKeyToObj   : TFRE_ART_TREE; { store the Pointer to the Transformed Data Entry}
    FArtTreeFilterKey  : TFRE_ART_TREE; { store a filtering based on the order }
    FTOCreationTime    : TFRE_DB_DateTime64;
  public
    procedure    LockBase      ; override;
    procedure    UnlockBase    ; override;
    constructor  Create        (const orderdef : TFRE_DB_DC_ORDER_DEFINITION ; base_trans_data : TFRE_DB_TRANFORMED_DATA);
    procedure    OrderTheData  ;
    destructor   Destroy       ; override;
    function     GetFullKeyDef : TFRE_DB_TRANS_COLL_DATA_KEY;
    procedure    Execute       (const iter : IFRE_DB_Obj_Iterator ; const qry_context : TFRE_DB_QUERY);
  end;

  OFRE_DB_TransCollTransformedDataList = specialize OGFOS_Array<TFRE_DB_TRANFORMED_DATA>; { list of transformed collections }
  OFRE_DB_TransCollOrderedDataList     = specialize OGFOS_Array<TFRE_DB_TRANS_COLL_DATA>; { orders upon the data, referencing to the collection }

  { TFRE_DB_TRANSDATA_MANAGER }

  TFRE_DB_TRANSDATA_MANAGER=class(TFRE_DB_TRANSDATA_MANAGER_BASE)
  private
    FArtQueryStore : TFRE_ART_TREE;
    FTransLock     : IFOS_LOCK;
    FTransList     : OFRE_DB_TransCollTransformedDataList; { List of base transformed data}
    FOrders        : OFRE_DB_TransCollOrderedDataList;     { List of orderedings of base transforms}
    function    GetBaseTransformedData (base_key : TFRE_DB_NameTypeRL ; out base_data : TFRE_DB_TRANFORMED_DATA) : boolean;
    procedure   AddBaseTransformedData (const base_data : TFRE_DB_TRANFORMED_DATA);
  public
    constructor Create        ;
    destructor  Destroy       ; override;
    procedure   LockManager   ; override;
    procedure   UnlockManager ; override;
    function    GetNewFilterDefinition    : TFRE_DB_DC_FILTER_DEFINITION_BASE; override;
    function    GetTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; var cd   : TFRE_DB_TRANS_RESULT_BASE):boolean; override;
    procedure   NewTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; const dc : IFRE_DB_DERIVED_COLLECTION ; var cd : TFRE_DB_TRANS_RESULT_BASE);override;
    {
     Generate the query spec from the JSON Webinput object
     dependency_reference_ids : this are the dependency keys that be considered to use from the JSON (usually one, input dependency)
     collection_transform_key : unique specifier of the DATA TRANSFORMATION defined by this collection, ORDERS derive from them
    }
    function    GenerateQueryFromRawInput (const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray ; const dc_name,parent_name : TFRE_DB_NameTypeRL ;
                                           const dc_static_filters : TFRE_DB_DC_FILTER_DEFINITION_BASE ; const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean;
                                           const DefaultOrderFieldtype: TFRE_DB_FIELDTYPE ; const session : IFRE_DB_UserSession): TFRE_DB_QUERY_BASE;override;
    { remember the query as open for the session }
    procedure   StoreQuery                (const qry: TFRE_DB_QUERY_BASE); override;
    { forget the query as open for the session }
    procedure   RemoveQuery               (const qry_id : TFRE_DB_NameType);override;
    { forget all querys for the session/dc }
    procedure   DropAllQuerys             (const session : IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL); override; { can be dc wide, or session wide dc_name='' }
    function    FormQueryID               (const session : IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL ; const client_part : shortstring):TFRE_DB_NameType;override;
  end;


procedure  InitTransfromManager;
procedure  FinalizeTransformManager;

implementation

procedure InitTransfromManager;
begin
  if not assigned(GFRE_DB_TCDM) then
    GFRE_DB_TCDM := TFRE_DB_TRANSDATA_MANAGER.Create;
end;

procedure FinalizeTransformManager;
begin
  if assigned(GFRE_DB_TCDM) then
    begin
      GFRE_DB_TCDM.Free;
      GFRE_DB_TCDM:=nil;
    end;
end;

{ TFRE_DB_FilterContainer }

procedure TFRE_DB_FilterContainer.SetFilled(AValue: boolean);
begin
  FFilled:=AValue;
end;

procedure TFRE_DB_FilterContainer.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY);
var i   : NativeInt;
    obj : IFRE_DB_Object;
begin
  if qry_context.FStartIdx > High(FOBJArray) then
    raise EFRE_DB_Exception.Create(edb_MISMATCH,'query start point too high');
  qry_context.FQueryPotentialCount := FCnt;
  qry_context.SetMaxResultDBOLen;
  for i:=qry_context.FStartIdx to High(FOBJArray) do
    begin
      obj := FOBJArray[i];
      iter(obj.CloneToNewObject);
      qry_context.SetResultUID(qry_context.FQueryCurrIdx,obj.UID);
      inc(qry_context.FQueryCurrIdx);
      inc(qry_context.FQueryDeliveredCount);
      if qry_context.FQueryDeliveredCount=qry_context.FToDeliverCount then
        break;
    end;
  qry_context.AddjustResultDBOLen;
end;

procedure TFRE_DB_FilterContainer.CheckFilteredAdd(const obj: IFRE_DB_Object);
begin
  if FCnt=Length(FOBJArray) then
    SetLength(FOBJArray,Length(FOBJArray)+cFRE_INT_TUNE_SYSFILTEXTENSION_SZ);
  //Test Filter Code ...
  if true then
    begin
      FOBJArray[FCnt]:=obj;
      inc(FCnt);
    end;
end;

procedure TFRE_DB_FilterContainer.AdjustLength;
begin
  SetLength(FOBJArray,FCnt);
end;

constructor TFRE_DB_FilterContainer.Create;
begin
  inherited Create;
  FCnt := 0;
  FFCCreationTime := GFRE_DT.Now_UTC;
end;


{ TFRE_DB_DC_FILTER_DEFINITION }

function TFRE_DB_DC_FILTER_DEFINITION.IsSealed: Boolean;
begin
  result := FFilterKey<>'';
end;

{ TFRE_DB_DC_Filter_Def }

function RDC_NullCompare(const filt : PRDC_Filter_Entry):boolean;
begin
  result := filt^.key='';
end;

function RDC_ElemCompare(const a,b : PRDC_Filter_Entry):boolean;
begin
  result := a^.key = b^.key;
end;

constructor TFRE_DB_DC_FILTER_DEFINITION.Create;
begin
  inherited;
  FKeyList.InitSparseList(default(RDC_Filter_Entry),@RDC_NullCompare,@RDC_ElemCompare,10);
end;

destructor TFRE_DB_DC_FILTER_DEFINITION.Destroy;
begin
  inherited Destroy;
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddFilters(const source: TFRE_DB_DC_FILTER_DEFINITION_BASE);
var src : TFRE_DB_DC_FILTER_DEFINITION;

   procedure add(var x:RDC_Filter_Entry;const idx:NativeInt ; var halt_flag:boolean);
   begin
     abort;
   end;

begin
  src := source as TFRE_DB_DC_FILTER_DEFINITION;
  src.FKeyList.ForAllBreak(@add);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.MustNotBeSealed;
begin
 if IsSealed then
   raise EFRE_DB_Exception.Create(edb_ERROR,'filter definition is already sealed');
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.MustBeSealed;
begin
 if not IsSealed then
   raise EFRE_DB_Exception.Create(edb_ERROR,'filter definition is not done');
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.Seal;
var key : string;
    i   : NativeInt;

   procedure add(var x:RDC_Filter_Entry;const idx:NativeInt ; var halt_flag:boolean);
   begin
     key:=key+x.key;
   end;

begin
  MustNotBeSealed;
  key := '';
  FKeyList.ForAllBreak(@Add);
  FFilterKey := 'FK|'+GFRE_BT.HashFast32_Hex(key);
end;

function TFRE_DB_DC_FILTER_DEFINITION.GetFilterKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  MustBeSealed;
  result := FFilterKey;
end;

{ TFRE_DB_DC_ORDER_DEFINITION }

function TFRE_DB_DC_ORDER_DEFINITION.IsSealed: Boolean;
begin
  result := FKey<>'';
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.MustNotBeSealed;
begin
 if IsSealed then
   raise EFRE_DB_Exception.Create(edb_ERROR,'order definition is already sealed');
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.MustBeSealed;
begin
 if not IsSealed then
   raise EFRE_DB_Exception.Create(edb_ERROR,'order definition is not done');
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.SetDataKeyColl(const parent_collectionname, derivedcollname: TFRE_DB_NameType);
begin
  FKeyPartMaj := parent_collectionname;
  FKeyPartMin := derivedcollname;
  FIsRefLinkM := false;
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.SetDataKeyReflink(const startuid: TFRE_DB_GUID; const RLChainSpec: TFRE_DB_NameTypeRLArray);
begin
  FKeyPartMaj := 'U:'+FREDB_G2H(startuid);
  FRefLinkCh  := RLChainSpec;
  FIsRefLinkM := false;
end;

function TFRE_DB_DC_ORDER_DEFINITION.GetFullKeyDef: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if FKey='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'key definition is not done')
  else
    result := FKey;
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.ForAllOrders(const orderiterator: TFRE_DB_DC_ORDER_ITERATOR);
var order : TFRE_DB_DC_ORDER;
begin
  for order in FOrderList do
    orderiterator(order);
end;


procedure TFRE_DB_DC_ORDER_DEFINITION.ClearOrders;
begin
  SetLength(FOrderList,0);
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.AddOrderDef(const orderfield_name: TFRE_DB_NameType; const asc: boolean);
begin
  MustNotBeSealed;
  SetLength(FOrderList,Length(FOrderList)+1);
  with FOrderList[high(FOrderList)] do
    begin
      ascending   := asc;
      order_field := orderfield_name;
    end;
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.Seal;
var key : string;
    i   : NativeInt;
begin
  MustNotBeSealed;
  if FIsRefLinkM then
    begin
      for i := 0 to high(FRefLinkCh) do
          key := key+FRefLinkCh[i];
      FKeyPartMin := 'R|'+GFRE_BT.HashFast32_Hex(key);
    end;
  key := '';
  for i := 0 to high(FOrderList) do
    with FOrderList[i] do
      key := key +order_field+BoolToStr(ascending,'A','D');
  FKey := FKeyPartMaj+'/'+FKeyPartMin+'/'+GFRE_BT.HashFast32_Hex(key);
end;

function TFRE_DB_DC_ORDER_DEFINITION.GetBaseKeyPart: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FKeyPartMaj+'/'+FKeyPartMin;
end;


{ TFRE_DB_OrderContainer }

function TFRE_DB_OrderContainer.AddObject(const obj: TFRE_DB_Object): boolean;
var idx : NativeInt;
begin
  idx    := FOBJArray.Add(obj);
  result := idx=-1;
end;

function TFRE_DB_OrderContainer.Exists(const obj: TFRE_DB_Object): boolean;
begin
  result := FOBJArray.Exists(obj)<>-1;
end;

procedure TFRE_DB_OrderContainer.ForAllBreak(const iter: IFRE_DB_ObjectIteratorBrk; var halt: boolean);

  procedure MyIter(var obj : TFRE_DB_Object ; const idx:Int64 ; var halt:boolean);
  begin
    iter(obj,halt);
  end;

begin
  FOBJArray.ForAllBreak2(@MyIter,halt);
end;

constructor TFRE_DB_OrderContainer.Create;
begin
  inherited Create;
  FOBJArray.InitSparseListPtrCmp;
end;

{ TFRE_DB_QUERY }

function TFRE_DB_QUERY.GetFilterDefinition: TFRE_DB_DC_FILTER_DEFINITION;
begin
  if not assigned(FQueryFilters) then
    FQueryFilters := TFRE_DB_DC_FILTER_DEFINITION.Create;
  result := FQueryFilters;
end;

function TFRE_DB_QUERY.GetOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION;
begin
  if not assigned(FOrderDef) then
    FOrderDef := TFRE_DB_DC_ORDER_DEFINITION.Create;
  result := FOrderDef;
end;

procedure TFRE_DB_QUERY.StartQueryRun;
begin
  FQueryRunning   := true;
  FQueryCurrIdx   := 0;
  FQueryStartTime := GFRE_BT.Get_Ticks_ms;
  GFRE_DBI.LogDebug(dblc_QUERY,'QUERY ID [%s] start',[FQueryId]);
end;

procedure TFRE_DB_QUERY.EndQueryRun;
begin
  FQueryEndTime := GFRE_BT.Get_Ticks_ms;
  FQueryRunning := False;
  GFRE_DBI.LogInfo(dblc_QUERY,'QUERY ID [%s] finished in %d ms',[FQueryId,FQueryEndTime-FQueryStartTime]);
end;

constructor TFRE_DB_QUERY.Create;
begin
  inherited;
  FQCreationTime := GFRE_DT.Now_UTC;
end;

destructor TFRE_DB_QUERY.Destroy;
begin
  FQueryFilters.free;
  inherited Destroy;
end;

function TFRE_DB_QUERY.GetQueryID: TFRE_DB_NameType;
begin
  result := FQueryId;
end;

function TFRE_DB_QUERY.HasOrderDefinition: boolean;
begin
  result := assigned(FOrderDef);
end;

function TFRE_DB_QUERY.GetBaseTransDataKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if not assigned(FOrderDef) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no orderdef');
  FOrderDef.MustBeSealed;
  result := FOrderDef.GetBaseKeyPart;
end;

function TFRE_DB_QUERY.GetFullQueryOrderKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if not assigned(FOrderDef) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no orderdef');
  FOrderDef.MustBeSealed;
  result := FOrderDef.GetFullKeyDef;
end;

procedure TFRE_DB_QUERY.SetBaseOrderedData(const basedata: TFRE_DB_TRANS_RESULT_BASE);
begin
  FBaseData := basedata as TFRE_DB_TRANS_COLL_DATA;
end;

procedure TFRE_DB_QUERY.SetMaxResultDBOLen;
begin
  SetLength(FResultDBOs,FToDeliverCount);
end;

procedure TFRE_DB_QUERY.SetResultUID(const idx: NativeInt; const uid: TFRE_DB_GUID);
begin
  FResultDBOs[idx] := uid;
end;

procedure TFRE_DB_QUERY.AddjustResultDBOLen;
begin
  SetLength(FResultDBOs,FQueryDeliveredCount);
end;

function TFRE_DB_QUERY.Execute(const iterator: IFRE_DB_Obj_Iterator): NativeInt;
begin
  if not assigned(FBaseData) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no base data available');
  StartQueryRun;
  FBaseData.Execute(iterator,self);
  EndQueryRun;
  result := FQueryPotentialCount;
end;


{ TFRE_DB_TRANFORMED_DATA }

function TFRE_DB_TRANFORMED_DATA.GetTransFormKey: TFRE_DB_NameTypeRL;
begin
  result := FBaseKey;
end;

function TFRE_DB_TRANFORMED_DATA.GetDataArray: PFRE_DB_ObjectArray;
begin
  result := @FTransformeddata;
end;

constructor TFRE_DB_TRANFORMED_DATA.Create(const base_key: TFRE_DB_NameTypeRL);
begin
  FBaseKey        := uppercase(base_key);
  FTDCreationTime := GFRE_DT.Now_UTC;
end;

{ TFRE_DB_TRANSDATA_MANAGER }

procedure TFRE_DB_TRANSDATA_MANAGER.UnlockManager;
begin
  FTransLock.Release;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetNewFilterDefinition: TFRE_DB_DC_FILTER_DEFINITION_BASE;
begin
  result := TFRE_DB_DC_FILTER_DEFINITION.Create;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetBaseTransformedData(base_key: TFRE_DB_NameTypeRL; out base_data: TFRE_DB_TRANFORMED_DATA): boolean;
var fnd : boolean;
  procedure Search(const bd : TFRE_DB_TRANFORMED_DATA ; var halt :boolean);
  begin
    if bd.GetTransFormKey = base_key then
      begin
        halt      := true;
        base_data := bd;
      end;
  end;

begin
  base_data := nil;
  base_key  := UpperCase(base_key);
  fnd       := false;
  result    := FTransList.ForAllBreak(@Search,fnd);
  GFRE_DBI.LogDebug(dblc_DBTDM,'BASE TRANSFORMED DATA FOR [%s] %s',[base_key,BoolToStr(fnd,'FOUND','NOT FOUND')]);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.AddBaseTransformedData(const base_data: TFRE_DB_TRANFORMED_DATA);
begin
  FTransList.Add2Array(base_data);
end;

constructor TFRE_DB_TRANSDATA_MANAGER.Create;
begin
  GFRE_TF.Get_Lock(FTransLock,true);
  FOrders.Init(10);
  FTransList.Init(10);
  FArtQueryStore := TFRE_ART_TREE.Create;
end;

destructor TFRE_DB_TRANSDATA_MANAGER.Destroy;
begin
  FTransLock.Finalize;
  { TODO : Free Transformations, Free Orders}
  FArtQueryStore.Free;   { TODO: clean queries }
  inherited Destroy;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.LockManager;
begin
  FTransLock.Acquire;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetTransformedDataLocked(const qry: TFRE_DB_QUERY_BASE; var cd: TFRE_DB_TRANS_RESULT_BASE): boolean;
var fkd : TFRE_DB_TRANS_COLL_DATA_KEY;
    fnd : boolean;

  procedure Search(const tcd : TFRE_DB_TRANS_COLL_DATA ; var halt : boolean);
  begin
    if tcd.FOrderDef.GetFullKeyDef=fkd then
      begin
        halt := true;
        cd   := tcd;
        tcd.LockBase;
      end;
  end;

begin
  LockManager;
  fnd    := false;
  fkd    := TFRE_DB_QUERY(qry).Orderdef.GetFullKeyDef;
  result := FOrders.ForAllBreak(@Search,fnd);
  result := fnd;
  GFRE_DBI.LogDebug(dblc_DBTDM,'>GET ORDERING FOR TRANSFORMED DATA FOR [%s] %s',[fkd,BoolToStr(fnd,'FOUND','NOT FOUND')]);
  if not fnd then
    result := FOrders.ForAllBreak(@Search,fnd);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.NewTransformedDataLocked(const qry: TFRE_DB_QUERY_BASE; const dc: IFRE_DB_DERIVED_COLLECTION; var cd: TFRE_DB_TRANS_RESULT_BASE);
var transdata         : TFRE_DB_TRANFORMED_DATA;
    basekey           : TFRE_DB_TRANS_COLL_DATA_KEY;
    st,et             : NativeInt;
    rcnt              : NativeInt;
begin
  with (qry) as TFRE_DB_QUERY do
    begin
      basekey := GetBaseTransDataKey;
      if not GetBaseTransformedData(basekey,transdata) then      { 1st search for Transformeddata }
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,'>BASE TRANSFORMING DATA FOR [%s]',[basekey]);
          st        := GFRE_BT.Get_Ticks_ms;
          transdata := TFRE_DB_TRANFORMED_DATA.Create(basekey);
          dc.TransformAllTo(transdata.GetDataArray^,rcnt);       { need base transformation layer}
          AddBaseTransformedData(transdata);
          et        := GFRE_BT.Get_Ticks_ms;
          GFRE_DBI.LogInfo(dblc_DBTDM,'<BASE TRANSFORMING DATA FOR [%s] DONE - Transformed %d records in %d ms',[basekey,rcnt,et-st]);
        end;
      cd := TFRE_DB_TRANS_COLL_DATA.Create(FOrderDef,transdata); { generate the ordered, transformed data (next layer) }
      FOrders.Add2Array(TFRE_DB_TRANS_COLL_DATA(cd));            { internal add the data }
      TFRE_DB_TRANS_COLL_DATA(cd).OrderTheData;                  { order it }
      TFRE_DB_TRANS_COLL_DATA(cd).LockBase;                      { return the locked result }
    end;
end;

function TFRE_DB_TRANSDATA_MANAGER.GenerateQueryFromRawInput(const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray; const dc_name, parent_name: TFRE_DB_NameTypeRL; const dc_static_filters: TFRE_DB_DC_FILTER_DEFINITION_BASE; const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean; const DefaultOrderFieldtype: TFRE_DB_FIELDTYPE; const session: IFRE_DB_UserSession): TFRE_DB_QUERY_BASE;
var fld : IFRE_DB_FIELD;
      i : NativeInt;
    qry : TFRE_DB_QUERY;

   procedure ProcessDependencies;
   var fld   : IFRE_DB_FIELD;
         i,j : NativeInt;
   begin
     SetLength(qry.FDependcyUids,Length(dependecy_reference_id));
     for i:=0 to high(dependecy_reference_id) do
       begin
         fld := input.FieldPath('DEPENDENCY.'+dependecy_reference_id[i]+'_REF.FILTERVALUES',true);
         if assigned(fld) then
           with qry do
             begin
               SetLength(FDependcyUids[i],fld.ValueCount);
               for j := 0 to High(FDependcyUids) do
                 FDependcyUids[i][j] := FREDB_H2G(fld.AsStringArr[j]);
               exit;
             end
       end;
   end;

   procedure ProcessOrderDefinition;
   var cnt,i : integer;
       sort  : IFRE_DB_Object;
   begin //nl
     if input.FieldExists('sort') then
       begin
         cnt := input.Field('sort').ValueCount;
         for i:=0 to cnt-1 do begin
           sort := input.Field('SORT').AsObjectItem[i];
           qry.OrderDef.AddOrderDef(sort.Field('PROPERTY').AsString,sort.Field('ASCENDING').AsBoolean);
         end;
       end
     else
       qry.Orderdef.AddOrderDef(DefaultOrderField,DefaultOrderAsc);

     qry.Orderdef.SetDataKeyColl(dc_name,parent_name);
     qry.OrderDef.Seal;
   end;

   procedure ProcessCheckChildQuery;
   begin
     with qry do begin
       if input.FieldExists('parentid') then
         begin { this is a child query }
           FReferenceMode :=  true;
           FParentIds :=FREDB_String2GuidArray(input.Field('parentid').AsString);
           if Length(FParentIds)>0 then
             begin
               FParentIds[0] := FParentIds[high(FParentIds)];
               SetLength(FParentIds,1);
             end;
         end
       else
         begin
           SetLength(FParentIds,0);
           FReferenceMode :=  false;
         end;
     end;
   end;

   procedure ProcessRange;
   begin
     with qry do
       begin
         FStartIdx        := input.Field('start').AsInt32;
         FToDeliverCount  := input.Field('count').AsInt32;
       end;
   end;

   procedure ProcessCheckFulltextFilter;
   var ftx : TFRE_DB_String;
   begin
     ftx := input.Field('FULLTEXT').AsString;
     if ftx<>'' then begin
       qry.FFullTextFilter := ftx;
       //SetLength(result,1);
       //with result[0] do begin
       //  filter_key      := '*D_FTX';
       //  field_name      := 'FTX_SEARCH';
       //  value           := ftx;
       //  filtertype      := dbft_PART;
       //  on_transform    := false;
       //  on_filter_field := true;
       //end;
     end;
   end;

   procedure Processfilters;
   begin
     qry.Filterdef.AddFilters(dc_static_filters);
     qry.Filterdef.Seal;
   end;

   procedure SetQueryID;
   begin
     qry.FQueryId := FormQueryID(session,dc_name,input.Field('QUERYID').AsString);
   end;

begin
  //writeln('QUERY_DEF');
  //writeln(input.DumpToString());
  //writeln('---');
  qry := TFRE_DB_QUERY.Create;
  SetQueryID;
  ProcessCheckChildQuery;
  ProcessDependencies;
  ProcessOrderDefinition;
  ProcessRange;
  ProcessCheckFulltextFilter;
  ProcessFilters;
  Result := qry;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.StoreQuery(const qry : TFRE_DB_QUERY_BASE);
var qptr : NativeUint;
begin
  LockManager;
  try
    qptr := FREDB_ObjectToPtrUInt(qry);
    if not FArtQueryStore.InsertStringKeyOrFetch(qry.GetQueryID,qptr) then
      begin
        qry.free;
        raise EFRE_DB_Exception.Create(edb_ERROR,'double add query try, query id''s are not unique');
      end;
    GFRE_DBI.LogDebug(dblc_DBTDM,'STORE QUERY ID [%s] TOTAL Q COUNT %d',[qry.GetQueryID,FArtQueryStore.GetValueCount]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.RemoveQuery(const qry_id: TFRE_DB_NameType);
var qptr : NativeUint;
    qry  : TFRE_DB_QUERY;
begin
  LockManager;
  try
    if FArtQueryStore.RemoveStringKey(qry_id,qptr) then
      begin
        qry := FREDB_PtrUIntToObject(qptr) as TFRE_DB_QUERY;
        qry.Free;
        GFRE_DBI.LogDebug(dblc_DBTDM,'REMOVED QUERY ID [%s] TOTAL Q COUNT %d',[qry_id,FArtQueryStore.GetValueCount]);
      end
    else
      GFRE_DBI.LogDebug(dblc_DBTDM,'FAIL/NOT FOUND : REMOVE QUERY ID [%s] TOTAL Q COUNT %d',[qry_id,FArtQueryStore.GetValueCount]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.DropAllQuerys(const session: IFRE_DB_UserSession; const dc_name: TFRE_DB_NameTypeRL);
var qryid:TFRE_DB_NameType;

  procedure ClearIt(var val : PtrUInt);
  var qry:TFRE_DB_QUERY;
  begin
    qry := FREDB_PtrUIntToObject(val) as TFRE_DB_QUERY;
    GFRE_DBI.LogDebug(dblc_DBTDM,'REMOVING CLIENT QUERY ID [%s]',[qry.GetQueryID]);
    qry.free;
  end;

  procedure DC_Clear;
  begin
    GFRE_DBI.LogDebug(dblc_DBTDM,'>START REMOVING DERIVED COLLECTION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
    FArtQueryStore.PrefixStringScanClear(qryid,@clearit);
    GFRE_DBI.LogDebug(dblc_DBTDM,'<REMOVED DC QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
  end;

  procedure Session_Clear;
  begin
    GFRE_DBI.LogDebug(dblc_DBTDM,'>START REMOVING SESSION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
    FArtQueryStore.PrefixStringScanClear(qryid,@clearit);
    GFRE_DBI.LogDebug(dblc_DBTDM,'<REMOVED SESSION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
  end;

begin
  LockManager;
  try
    qryid := FormQueryID(session,dc_name,'');
    if dc_name<>'' then
      DC_Clear
    else
      Session_Clear;
  finally
    UnlockManager;
  end;
end;

function TFRE_DB_TRANSDATA_MANAGER.FormQueryID(const session: IFRE_DB_UserSession; const dc_name: TFRE_DB_NameTypeRL; const client_part: shortstring): TFRE_DB_NameType;
begin
  result := session.GetSessionID;
  if dc_name<>'' then
    result := result + '/'+GFRE_BT.HashFast32_Hex(dc_name);
  if client_part<>'' then
    result := result+'#'+client_part;
end;




{ TFRE_DB_TRANS_COLL_DATA }

procedure TFRE_DB_TRANS_COLL_DATA.LockBase;
begin
  FTransCollDataLock.Acquire;
end;

procedure TFRE_DB_TRANS_COLL_DATA.UnlockBase;
begin
  FTransCollDataLock.Release;
end;

constructor TFRE_DB_TRANS_COLL_DATA.Create(const orderdef: TFRE_DB_DC_ORDER_DEFINITION; base_trans_data: TFRE_DB_TRANFORMED_DATA);
begin
  FOrderDef         := orderdef;
  FArtTreeKeyToObj  := TFRE_ART_TREE.Create;
  FArtTreeFilterKey := TFRE_ART_TREE.Create;
  FTOCreationTime   := GFRE_DT.Now_UTC;
  GFRE_TF.Get_Lock(FTransCollDataLock);
  FBaseTransData    := base_trans_data;
end;

procedure TFRE_DB_TRANS_COLL_DATA.OrderTheData;
var i       : NativeInt;
    Key     : Array [0..512] of Byte;
    KeyLen  : NativeInt;
    currpos : NativeInt;
    oc      : TFRE_DB_OrderContainer;
    dummy   : PtrUInt;
    obj     : IFRE_DB_Object;
    tobj    : TFRE_DB_Object;

    procedure SetBinaryKey(const obj : IFRE_DB_Object);
    var fld : IFRE_DB_FIELD;

      procedure  Iter(const order : TFRE_DB_DC_ORDER);
      var fld    : IFRE_DB_Field;
          idx    : TFRE_DB_MM_IndexClass;
      begin
        if obj.FieldOnlyExisting(order.order_field,fld) then
          begin
            if TFRE_DB_MM_Index.GetIndexClassForFieldtype(fld.FieldType,idx)=edb_UNSUPPORTED then
              begin
                TFRE_DB_TextIndex.TransformToBinaryComparable(nil,@key[currpos],KeyLen,false,not order.ascending); { fallback transform the nil/unknown/unsupported fieldtype to a text null value }
              end
            else
              if idx=TFRE_DB_UnsignedIndex then
                TFRE_DB_UnsignedIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,@Key[currpos],keylen,false,not order.ascending)
              else
              if idx=TFRE_DB_SignedIndex then
                TFRE_DB_SignedIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,@Key[currpos],keylen,false,not order.ascending)
              else
              if idx=TFRE_DB_TextIndex then
                TFRE_DB_TextIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,@Key[currpos],keylen,false,not order.ascending)
              else
                raise EFRE_DB_Exception.Create(edb_INTERNAL,' unknonw idx typed must be reported as unsupported!');
          end
        else
          begin
            TFRE_DB_TextIndex.TransformToBinaryComparable(nil,@key[currpos],KeyLen,false); { fallback transform the nil/unknown/unsupported fieldtype to a text null value }
          end;
        currpos := currpos+KeyLen;
      end;

    begin
      currpos := 0;
      FOrderDef.ForAllOrders(@iter);
    end;

begin
  FArtTreeKeyToObj.Clear;
  for i := 0 to High(FBaseTransData.GetDataArray^) do
    begin
      obj  := FBaseTransData.GetDataArray^[i];
      tobj := obj.Implementor as TFRE_DB_Object;
      SetBinaryKey(obj);
      oc    := TFRE_DB_OrderContainer.Create;
      dummy := FREDB_ObjectToPtrUInt(oc);
      if not FArtTreeKeyToObj.InsertBinaryKeyOrFetch(key,currpos,dummy) then
        begin
          oc := FREDB_PtrUIntToObject(dummy) as TFRE_DB_OrderContainer;
        end;
      oc.AddObject(tobj);
      tobj.Field(cFRE_DB_SYS_ORDER_REF_KEY).AsObject.Field(GetFullKeyDef).AsBoolean:=true;
    end;
end;

destructor TFRE_DB_TRANS_COLL_DATA.Destroy;
begin
  FArtTreeKeyToObj.Destroy;
  FTransCollDataLock.Finalize;
  inherited Destroy;
end;

function TFRE_DB_TRANS_COLL_DATA.GetFullKeyDef: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FOrderDef.GetFullKeyDef;
end;

procedure TFRE_DB_TRANS_COLL_DATA.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY);
var brk        : boolean;
    filtkey    : TFRE_DB_TRANS_COLL_DATA_KEY;
    filtercont : TFRE_DB_FilterContainer;
    dummy      : PNativeUint;
    st,et      : NativeInt;

  procedure IteratorBreak(var dummy : PtrUInt ; var halt : boolean);
  var oc : TFRE_DB_OrderContainer;

      procedure MyIter(const obj : IFRE_DB_Object; var myhalt : boolean);
      begin
        filtercont.CheckFilteredAdd(obj);
      end;

  begin
    oc := FREDB_PtrUIntToObject(dummy) as TFRE_DB_OrderContainer;
    oc.ForAllBreak(@MyIter,halt);
  end;

begin
  brk := false;
  filtkey    := qry_context.FQueryFilters.GetFilterKey;
  dummy      := nil;
  if FArtTreeFilterKey.InsertStringKeyOrFetchR(filtkey,dummy) then
    begin
      filtercont := TFRE_DB_FilterContainer.Create;
      dummy^     := FREDB_ObjectToPtrUInt(filtercont);
    end
  else
    filtercont := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_FilterContainer;
  if not filtercont.IsFilled then
    begin
      GFRE_DBI.LogInfo(dblc_DBTDM,'>NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s]',[GetFullKeyDef,filtkey]);
      st := GFRE_BT.Get_Ticks_ms;
      FArtTreeKeyToObj.LinearScanBreak(@IteratorBreak,brk,false);
      filtercont.IsFilled := true;
      filtercont.AdjustLength;
      et := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogInfo(dblc_DBTDM,'<NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] DONE in %d ms',[GetFullKeyDef,filtkey,et-st]);
    end;
  filtercont.Execute(iter,qry_context);
end;

{ TFRE_DB_TRANSDATA_MANAGER }


end.

