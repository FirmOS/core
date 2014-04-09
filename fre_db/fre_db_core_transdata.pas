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
     Classes, SysUtils,fre_db_interface,fre_db_core,fos_art_tree,fos_basis_tools,fos_tool_interfaces,fos_arraygen,fre_db_persistance_common;

type

  TFRE_DB_QUERY=class(TFRE_DB_QUERY_BASE)
  protected
     FQueryId               : TFRE_DB_NameType;
     FCollTransfromKey      : TFRE_DB_NameTypeRL; { an order is created and dversificated by the transformation(dc name) and the parent collection }
     FReferenceMode         : Boolean;            { true if reference query, false if collection query }
     FParentChldLinkFldSpec : TFRE_DB_NameTypeRL;
     FOrderDef              : TFRE_DB_DC_ORDER_DEFINITION;
     FFilterDef             : TFRE_DB_DC_FILTER_DEFINITION;
     //FExpendedRefs  : TFRE_DB_GUIDArray; {                     }
     FDependcyUids           : TFRE_DB_GUIDArray; { this is a filter type }
     FParentIds              : TFRE_DB_GUIDArray; { path to root}
     FUserKey                : TFRE_DB_String;    { Login@Domain | GUID ?}
     FFullTextFilter         : TFRE_DB_String;    { use this on all string fields, if set}
     FStartIdx               : NativeInt;
     FCount                  : NativeInt;
     FResultDBOs             : TFRE_DB_GUIDArray;
  public

  end;

  { TFRE_DB_TRANFORMED_DATA }

  TFRE_DB_TRANFORMED_DATA=class
  private
    FBaseKey         : TFRE_DB_NameTypeRL;
    FTransformeddata : IFRE_DB_ObjectArray;
  public
    function    GetTransFormKey : TFRE_DB_NameTypeRL;
    function    GetDataArray    : PFRE_DB_ObjectArray;
    constructor Create(const base_key : TFRE_DB_NameTypeRL);
  end;


  { TFRE_DB_TRANS_COLL_DATA }

  TFRE_DB_TRANS_COLL_DATA=class(TFRE_DB_TRANS_RESULT_BASE)
  private
    FOrderDef          : TFRE_DB_DC_ORDER_DEFINITION;
    FTransCollDataLock : IFOS_LOCK;
    FBaseTransData     : TFRE_DB_TRANFORMED_DATA;
    FArtTree           : TFRE_ART_TREE;
  public
    procedure    LockBase     ; override;
    procedure    UnlockBase   ; override;
    constructor  Create       (const orderdef : TFRE_DB_DC_ORDER_DEFINITION ; base_trans_data : TFRE_DB_TRANFORMED_DATA);
    procedure    OrderTheData ;
    destructor   Destroy      ; override;
  end;

  TFRE_DB_TRANS_RESULT = class(TFRE_DB_TRANS_COLL_DATA)
  public
  end;


  OFRE_DB_TransCollTransformedDataList = specialize OGFOS_Array<TFRE_DB_TRANFORMED_DATA>; { list of transformed collections }
  OFRE_DB_TransCollOrderedDataList     = specialize OGFOS_Array<TFRE_DB_TRANS_COLL_DATA>; { orders upon the data, referencing to the collection }

  { TFRE_DB_TRANSDATA_MANAGER }

  TFRE_DB_TRANSDATA_MANAGER=class(TFRE_DB_TRANSDATA_MANAGER_BASE)
  private
    FTransLock : IFOS_LOCK;
    FTransList : OFRE_DB_TransCollTransformedDataList;
    FOrderList : OFRE_DB_TransCollOrderedDataList;
    function    GetBaseTransformedData (base_key : TFRE_DB_NameTypeRL ; out base_data : TFRE_DB_TRANFORMED_DATA) : boolean;
    procedure   AddBaseTransformedData (const base_data : TFRE_DB_TRANFORMED_DATA);
  public
    constructor Create        ;
    destructor  Destroy       ; override;
    procedure   LockManager   ; override;
    procedure   UnlockManager ; override;
    function    GetTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; var cd   : TFRE_DB_TRANS_RESULT_BASE):boolean; override;
    procedure   NewTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; const dc : IFRE_DB_DERIVED_COLLECTION ; var cd : TFRE_DB_TRANS_RESULT_BASE);override;
    {
     Generate the query spec from the JSON Webinput object
     dependency_reference_ids : this are the dependency keys that be considered to use from the JSON (usually one, input dependency)
     collection_transform_key : unique specifier of the DATA TRANSFORMATION defined by this collection, ORDERS derive from them
    }
    function    GenerateQueryFromRawInput (const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray ; const collection_transform_key : TFRE_DB_NameTypeRL ; const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean; const DefaultOrderFieldtype: TFRE_DB_FIELDTYPE; const replace_default_order: boolean=true): TFRE_DB_QUERY_BASE;override;
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
  FBaseKey := uppercase(base_key);
end;

{ TFRE_DB_TRANSDATA_MANAGER }

procedure TFRE_DB_TRANSDATA_MANAGER.UnlockManager;
begin
  FTransLock.Release;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetBaseTransformedData(base_key: TFRE_DB_NameTypeRL; out base_data: TFRE_DB_TRANFORMED_DATA): boolean;

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
  result    := FTransList.ForAllBreak(@Search);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.AddBaseTransformedData(const base_data: TFRE_DB_TRANFORMED_DATA);
begin
  FTransList.Add2Array(base_data);
end;

constructor TFRE_DB_TRANSDATA_MANAGER.Create;
begin
  GFRE_TF.Get_Lock(FTransLock,true);
  FOrderList.Init(10);
  FTransList.Init(10);
end;

destructor TFRE_DB_TRANSDATA_MANAGER.Destroy;
begin
  FTransLock.Finalize;
  { TODO : Free Transformations, Free Orders}
  inherited Destroy;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.LockManager;
begin
  FTransLock.Acquire;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetTransformedDataLocked(const qry: TFRE_DB_QUERY_BASE; var cd: TFRE_DB_TRANS_RESULT_BASE): boolean;

  procedure Search(const tcd : TFRE_DB_TRANS_COLL_DATA ; var halt : boolean);
  begin
    if tcd.FOrderDef.GetKeyDefinition=TFRE_DB_QUERY(qry).FOrderDef.GetKeyDefinition then
      begin
        halt := true;
        tcd.LockBase;
      end;
  end;

begin
  LockManager;
  cd     := nil;
  result := FOrderList.ForAllBreak(@Search);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.NewTransformedDataLocked(const qry: TFRE_DB_QUERY_BASE; const dc: IFRE_DB_DERIVED_COLLECTION; var cd: TFRE_DB_TRANS_RESULT_BASE);
var transdata         : TFRE_DB_TRANFORMED_DATA;
begin
  with (qry) as TFRE_DB_QUERY do
    begin
      if not GetBaseTransformedData(FCollTransfromKey,transdata) then
        begin
          transdata := TFRE_DB_TRANFORMED_DATA.Create(FCollTransfromKey); { create base transformed data}
          dc.TransformAllTo(transdata.GetDataArray^);
          AddBaseTransformedData(transdata);
        end;
      cd := TFRE_DB_TRANS_RESULT.Create(FOrderDef,transdata);
      FOrderList.Add2Array(TFRE_DB_TRANS_COLL_DATA(cd));
      TFRE_DB_TRANS_COLL_DATA(cd).OrderTheData;
      TFRE_DB_TRANS_COLL_DATA(cd).LockBase;
    end;
end;

function TFRE_DB_TRANSDATA_MANAGER.GenerateQueryFromRawInput(const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray; const collection_transform_key: TFRE_DB_NameTypeRL; const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean; const DefaultOrderFieldtype: TFRE_DB_FIELDTYPE; const replace_default_order: boolean): TFRE_DB_QUERY_BASE;
var fld : IFRE_DB_FIELD;
      i : NativeInt;
    qry : TFRE_DB_QUERY;

   procedure ProcessDependencies;
   var fld   : IFRE_DB_FIELD;
         i,j : NativeInt;
   begin
     for i:=0 to high(dependecy_reference_id) do
       begin
         fld := input.FieldPath('DEPENDENCY.'+dependecy_reference_id[i]+'_REF.FILTERVALUES',true);
         if assigned(fld) then
           with qry do
             begin
               SetLength(FDependcyUids,fld.ValueCount);
               for j := 0 to High(FDependcyUids) do
                 FDependcyUids[j] := FREDB_H2G(fld.AsStringArr[j]);
               exit;
             end
       end;
   end;

   procedure ProcessOrderDefinition;
   var cnt,i : integer;
       sort  : IFRE_DB_Object;
   begin //nl
     if replace_default_order then
       qry.FOrderDef.ClearOrders;
     if input.FieldExists('sort') then
       begin
         cnt := input.Field('sort').ValueCount;
         for i:=0 to cnt-1 do begin
           sort := input.Field('SORT').AsObjectItem[i];
           qry.FOrderDef.AddOrderDef(sort.Field('PROPERTY').AsString,sort.Field('ASCENDING').AsBoolean);
         end;
       end;
     qry.FOrderDef.Seal;
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
         FStartIdx := input.Field('start').AsInt32;
         FCount    := input.Field('count').AsInt32;
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

begin
  qry := TFRE_DB_QUERY.Create;
  ProcessCheckChildQuery;
  ProcessDependencies;
  ProcessOrderDefinition;
  ProcessRange;
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
  FOrderDef := orderdef;
  FArtTree  := TFRE_ART_TREE.Create;
  GFRE_TF.Get_Lock(FTransCollDataLock);
  FBaseTransData := base_trans_data;
end;

procedure TFRE_DB_TRANS_COLL_DATA.OrderTheData;
var i      : NativeInt;
    Key    : Array [0..512] of Byte;
    KeyLen : NativeInt;
begin
  FArtTree.Clear;
  for i := 0 to High(FBaseTransData.GetDataArray^) do
    begin
      //FOrderDef.CalcBinaryKey(FBaseTransData[i],@Key,KeyLen);
    end;
end;

destructor TFRE_DB_TRANS_COLL_DATA.Destroy;
begin
  FArtTree.Destroy;
  FTransCollDataLock.Finalize;
  inherited Destroy;
end;

{ TFRE_DB_TRANSDATA_MANAGER }


end.

