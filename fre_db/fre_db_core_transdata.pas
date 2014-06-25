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

{
  TODO:
  *) Repeat check Filterings for long not used ones, prune them
  *) CRITICAL: (!) Update ResultDBO Array of Query on Notification !
}

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
     Classes,contnrs, SysUtils,fos_sparelistgen,fre_db_interface,fre_db_core,fos_art_tree,fos_basis_tools,fos_tool_interfaces,fos_arraygen,fre_db_common,fre_db_persistance_common,strutils,fre_aps_interface,math;
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
    FOrderList          : array of TFRE_DB_DC_ORDER;
    FKey                : TFRE_DB_TRANS_COLL_DATA_KEY;
    function    IsSealed : Boolean;
  public
    procedure   MustNotBeSealed;
    procedure   MustBeSealed;
    procedure   SetDataKeyColl    (const parent_collectionname,derivedcollname : TFRE_DB_NameType ; const ParentChildspec : TFRE_DB_NameTypeRL);
    procedure   ClearOrders       ;
    procedure   AddOrderDef       (const orderfield_name : TFRE_DB_NameType ; const asc : boolean);
    procedure   Seal              ;
    function    GetBaseKeyPart    : TFRE_DB_TRANS_COLL_DATA_KEY;
    function    GetFullKey        : TFRE_DB_TRANS_COLL_DATA_KEY;
    procedure   ForAllOrders      (const orderiterator : TFRE_DB_DC_ORDER_ITERATOR);
    procedure   SetupBinaryKey    (const obj : TFRE_DB_Object ; const Key : PByteArray ; var max_key_len : NativeInt ; const tag_object : boolean); { setup a binary key according to this order }
  end;

  { TFRE_DB_FILTER_STRING }

  TFRE_DB_FILTER_STRING=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : TFRE_DB_StringArray;
    FFilterType : TFRE_DB_STR_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterHit   (const obj : IFRE_DB_Object; var flt_errors : Int64):boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_String ; const stringfiltertype : TFRE_DB_STR_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_SIGNED }

  TFRE_DB_FILTER_SIGNED=class(TFRE_DB_FILTER_BASE)
  protected
    FValues       : TFRE_DB_Int64Array;
    FFilterType   : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of Int64 ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;


  { TFRE_DB_FILTER_UNSIGNED }

  TFRE_DB_FILTER_UNSIGNED=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : TFRE_DB_UInt64Array;
    FFilterType : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of UInt64 ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_CURRENCY }

  TFRE_DB_FILTER_CURRENCY=class(TFRE_DB_FILTER_BASE)
  protected
    FValues       : TFRE_DB_CurrencyArray;
    FFilterType   : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of Currency ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_DATETIME }

  TFRE_DB_FILTER_DATETIME=class(TFRE_DB_FILTER_BASE)
  protected
    FValues       : TFRE_DB_DateTimeArray;
    FFilterType   : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_DateTime64 ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_REAL64 }

  TFRE_DB_FILTER_REAL64=class(TFRE_DB_FILTER_BASE)
  protected
    FValues       : TFRE_DB_Real64Array;
    FFilterType   : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of Double ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;


  { TFRE_DB_FILTER_BOOLEAN }

  TFRE_DB_FILTER_BOOLEAN=class(TFRE_DB_FILTER_BASE)
  protected
    FValue      : Boolean;
    FFilterType : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone           : TFRE_DB_FILTER_BASE;override;
    function  CheckFilterHit  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey: TFRE_DB_NameType; override;
    procedure InitFilter      (const fieldname : TFRE_DB_NameType ; const value: boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_UID }

  TFRE_DB_FILTER_UID=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : TFRE_DB_GUIDArray;
    FFilterType : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_GUID ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;

  { TFRE_DB_FILTER_AUTO_DEPENDENCY }

  TFRE_DB_FILTER_AUTO_DEPENDENCY=class(TFRE_DB_FILTER_BASE)
  protected
    FValues       : TFRE_DB_GUIDArray; { expanded uid values }
    FRL_Spec      : TFRE_DB_NameTypeRLArray;
    FStartValues  : TFRE_DB_GUIDArray;
  public
    function  Clone                   : TFRE_DB_FILTER_BASE;override;
    function  CheckFilterHit          (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey        : TFRE_DB_NameType; override;
    procedure ExpandReferences        (const dbc : IFRE_DB_CONNECTION);
    procedure InitFilter              (const RL_Spec: TFRE_DB_NameTypeRLArray; const StartDependecyValues: TFRE_DB_GUIDArray; const negate: boolean; const include_null_values: boolean ;  const dbname: TFRE_DB_NameType);
    procedure ReEvaluateFilter        ; override;
    function  CheckReflinkUpdateEvent (const key_descr: TFRE_DB_NameTypeRL) : boolean; override;
  end;


  { TFRE_DB_FILTER_SCHEME }

  TFRE_DB_FILTER_SCHEME=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : Array of TFRE_DB_NameType;
  public
    function  Clone            : TFRE_DB_FILTER_BASE ; override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    procedure InitFilter       (filtervalues : Array of TFRE_DB_String ; const negate:boolean);
  end;

  { TFRE_DB_FILTER_RIGHT }

  TFRE_DB_FILTER_RIGHT=class(TFRE_DB_FILTER_BASE)
  protected
    FRight     : TFRE_DB_STANDARD_RIGHT_SET;
    FUserToken : IFRE_DB_USER_RIGHT_TOKEN;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (stdrightset  : TFRE_DB_STANDARD_RIGHT_SET ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN ; const negate:boolean);
  end;

  { TFRE_DB_FILTER_PARENT }

  TFRE_DB_FILTER_PARENT=class(TFRE_DB_FILTER_BASE)
  protected
    FAllowedParent : String;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const allowed_parent_path: TFRE_DB_GUIDArray);
  end;

  { TFRE_DB_FILTER_CHILD }

  TFRE_DB_FILTER_CHILD=class(TFRE_DB_FILTER_BASE)
  protected
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterHit   (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       ;
  end;



  { TFRE_DB_DC_FILTER_DEFINITION }

  TFRE_DB_DC_FILTER_DEFINITION = class(TFRE_DB_DC_FILTER_DEFINITION_BASE)
  private
    FFilterKey  : TFRE_DB_TRANS_COLL_FILTER_KEY; { summed unique filter key }
    FKeyList    : TFPHashObjectList;
    FFilterMiss : boolean;
    FFilterErr  : int64;
    function   IsSealed : Boolean;
    procedure  _ForAllAdd    (obj:TObject ; arg:Pointer);
    procedure  _ForAllKey    (obj:TObject ; arg:Pointer);
    procedure  _ForAllFilter (obj:TObject ; arg:Pointer);
  public
    constructor Create                       ;
    destructor  Destroy                      ;override;
    procedure   AddFilters                   (const source : TFRE_DB_DC_FILTER_DEFINITION_BASE; const clone : boolean=true); { add filters,take ownership }
    procedure   AddFilter                    (const source : TFRE_DB_FILTER_BASE; const clone : boolean=false);              { add filter,take ownership }
    procedure   AddStringFieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalue  : TFRE_DB_String              ; const stringfiltertype : TFRE_DB_STR_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddSignedFieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Int64              ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddUnsignedFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Uint64             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddCurrencyFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Currency           ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddReal64FieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Double             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddDatetimeFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_DateTime64 ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddBooleanFieldFilter        (const key,fieldname:TFRE_DB_NameType ; filtervalue  : boolean                                                                       ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddUIDFieldFilter            (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_GUID       ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=false ; const include_null_values : boolean=false);override;
    procedure   AddSchemeObjectFilter        (const key:          TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_String                                                       ; const negate:boolean=false);override;
    procedure   AddStdRightObjectFilter      (const key:          TFRE_DB_NameType ; stdrightset  : TFRE_DB_STANDARD_RIGHT_SET  ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN      ; const negate:boolean=false);override;
    procedure   AddChildFilter               (const key:          TFRE_DB_NameType); override ;
    procedure   AddParentFilter              (const key:          TFRE_DB_NameType ; const allowed_parent_path : TFRE_DB_GUIDArray); override ;
    procedure   AddAutoDependencyFilter      (const key:          TFRE_DB_NameType ; const RL_Spec : TFRE_DB_NameTypeRLArray ;  const StartDependecyValues : TFRE_DB_GUIDArray  ; const negate:boolean=false ; const include_null_values : boolean=false ; const dbname : TFRE_DB_NameType='');override;

    function    RemoveFilter                 (const key:          TFRE_DB_NameType):boolean;override;
    function    FilterExists                 (const key:          TFRE_DB_NameType):boolean;override;
    procedure   MustNotBeSealed              ;
    procedure   MustBeSealed                 ;
    procedure   Seal                         ;
    function    GetFilterKey                 : TFRE_DB_TRANS_COLL_FILTER_KEY;
    function    DoesObjectPassFilters        (const obj : IFRE_DB_Object) : boolean;
    procedure   CheckDBReevaluationFilters   ;
    function    CheckAutoDependencyFilter    (const key_description : TFRE_DB_NameTypeRL):boolean;
    //procedure
  end;

  { TFRE_DB_QUERY }

  TFRE_DB_QUERY=class(TFRE_DB_QUERY_BASE)
  private
    function  GetFilterDefinition: TFRE_DB_DC_FILTER_DEFINITION;
    function  GetOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION;
  protected
     FBaseData               : TFRE_DB_TRANS_COLL_DATA;      { assigned transformed and ordered data }
     FConnection             : IFRE_DB_CONNECTION;           { used for right profile check (implicit righfilter) }
     FQueryId                : TFRE_DB_NameType;             { ID of this specific Query }
     FQueryClientID          : TFRE_DB_NameType;
     FQueryDescr             : string;
     //FCollTransfromKey      : TFRE_DB_NameTypeRL;          { an order is created and diversificated by the transformation(dc name) and the parent collection }
     //FReferenceMode          : Boolean;                      { true if reference query, false if collection query }
     FParentChildLinkFldSpec : TFRE_DB_NameTypeRL;           { rl spec of the parent child relation }
     FParentChildSkipschemes : TFRE_DB_NameTypeRLArray;      { skip this schemes in a parent child query }
     FQueryFilters           : TFRE_DB_DC_FILTER_DEFINITION; { managed here, cleanup here}
     FOrderDef               : TFRE_DB_DC_ORDER_DEFINITION;  { linked to order definition of base ordered data, dont cleanup here}
     FDependencyIds          : TFRE_DB_StringArray;          { dependency id's of this querys DC, in same order as }
     FDepRefConstraints      : TFRE_DB_NameTypeRLArrayArray; { this reference link constraints, there must be an extra }
     FDependcyFilterUids     : Array of TFRE_DB_GUIDArray;   { this is a special filter type, the GUID array may, at some later time, be extended to support other field types }
     FParentIds              : TFRE_DB_GUIDArray;            { parentid / path to root / of this query (filter) }
     FUserKey                : TFRE_DB_String;               { Login@Domain | GUID ?}
     FFullTextFilter         : TFRE_DB_String;               { use this on all string fields, if set}
     FStartIdx               : NativeInt;                    { }
     FToDeliverCount         : NativeInt;                    { }
     FResultDBOs             : IFRE_DB_ObjectArray;          { remember all objs of this query, need to update that list by notify updates, need to hold that list to check against changes in filterchanges }
     FResultDBOsCompare      : IFRE_DB_ObjectArray;          { due to a filter update rerun the query - store in compare array, compare, send updates, switch the arrays }

     FQueryRunning           : boolean;
     FQueryStartTime         : NativeInt;
     FQueryEndTime           : NativeInt;
     FQueryCurrIdx           : NativeInt;
     FQueryDeliveredCount    : NativeInt;
     FQueryPotentialCount    : NativeInt;

     FQCreationTime          : TFRE_DB_DateTime64;
     FSessionID              : String;

     function                GetReflinkSpec        (const upper_refid : TFRE_DB_NameType):TFRE_DB_NameTypeRLArray;
     function                GetReflinkStartValues (const upper_refid : TFRE_DB_NameType):TFRE_DB_GUIDArray; { start values for the RL expansion }
  public
     procedure   SetResultObject                   (const idx: NativeInt; const obj: IFRE_DB_Object ; const compare_run : boolean);
     procedure   AddjustResultDBOLen               (const compare_run : boolean);
     procedure   SetMaxResultDBOLen                (const compare_run : boolean);
     procedure   StartQueryRun                     (const compare_run : boolean);
     procedure   EndQueryRun                       (const compare_run : boolean);

     constructor Create;
     destructor  Destroy                           ; override;
     function    GetQueryID                        : TFRE_DB_NameType; override;
     function    HasOrderDefinition                : boolean;
     property    Orderdef                          : TFRE_DB_DC_ORDER_DEFINITION  read GetOrderDefinition;  { lazy create on access}
     property    Filterdef                         : TFRE_DB_DC_FILTER_DEFINITION read GetFilterDefinition; { lazy create on access}
     function    GetBaseTransDataKey               : TFRE_DB_TRANS_COLL_DATA_KEY;
     function    GetFullQueryOrderKey              : TFRE_DB_TRANS_COLL_DATA_KEY;
     procedure   SetBaseOrderedData                (const basedata   : TFRE_DB_TRANS_RESULT_BASE ; const session_id : TFRE_DB_String);override;
     function    ExecuteQuery                      (const iterator   : IFRE_DB_Obj_Iterator):NativeInt;override; { execute the query, determine count and array of result dbo's }
     procedure   ProcessFilterChangeBasedUpdates   ;
     function    GetStoreID                        : TFRE_DB_NameType;
  end;


  { TFRE_DB_SESSION_UPO }

  TFRE_DB_SESSION_UPO = class
  private
     FStoreList : TFPHashObjectList;
     FSessid    : TFRE_DB_NameType;
  public
     constructor Create                   (const session_id : TFRE_DB_NameType);
     destructor  Destroy                  ; override;
     procedure   AddStoreUpdate           (const store_id,qid: TFRE_DB_NameType; const upo: IFRE_DB_Object);
     procedure   AddStoreInsert           (const store_id,qid: TFRE_DB_NameType; const upo: IFRE_DB_Object; const parent_id, reference_id: TFRE_DB_String; const before: boolean);
     procedure   AddStoreDelete           (const store_id,qid: TFRE_DB_NameType; const id: TFRE_DB_String);
     procedure   DispatchAllNotifications ;
  end;

  { TFRE_DB_TRANSDATA_CHANGE_NOTIFIER }

  TFRE_DB_TRANSDATA_CHANGE_NOTIFIER=class(IFRE_DB_TRANSDATA_CHANGE_NOTIFIER)
  private
     FSessionUpdateList : TFPHashObjectList;
     function    GetSessionUPO                (const sessionid: TFRE_DB_NameType): TFRE_DB_SESSION_UPO;
  public
     constructor Create                       ;
     destructor  Destroy                      ;override;
     procedure   AddDirectSessionUpdateEntry  (const update_dbo : IFRE_DB_Object); { add a dbo update for sessions dbo's (forms) }
     procedure   AddGridInplaceUpdate         (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const query_id : TFRE_DB_NameType ; const upo: IFRE_DB_Object); { inplace update entry for the store }
     procedure   AddGridInsertUpdate          (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const upo: IFRE_DB_Object; const parent_id, reference_id: TFRE_DB_String; const before: boolean);
     procedure   AddGridRemoveUpdate          (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const del_id: TFRE_DB_String);
     procedure   NotifyAll;
  end;

  { TFRE_DB_TRANFORMED_DATA }

  TFRE_DB_TRANFORMED_DATA=class(TFRE_DB_TRANSFORMED_ARRAY_BASE)
  private
    FKey                  : TFRE_DB_TRANS_COLL_DATA_KEY;   { CN/DCN/CHILD RL SPEC }
    FTransformKey         : QWord;
    FTransformedData      : TFPHashObjectList;
    FOrderings            : TFPObjectList;
    FTDCreationTime       : TFRE_DB_DateTime64;
    FChildDataIsLazy      : Boolean; { the child data is lazy : UNSUPPORTED }
    FDC                   : IFRE_DB_DERIVED_COLLECTION;
    function              IncludesChildData      : Boolean; { the query is a tree query, thus the objects contain extension pointers to TFRE_DB_TRANFORMED_DATA objects }
    function              HasReflinksInTransform : Boolean; { the query has reflinks in the transform, so on RL change the Transform has to be revaluated ...}
  public
    procedure   Cleanup                       ; override;

    procedure   UpdateObjectByNotify          (const obj : IFRE_DB_Object);                                                                                             { notify step 1 }
    procedure   InsertObjectByNotify          (const coll_name : TFRE_DB_NameType ; const obj : IFRE_DB_Object ; const rl_ins : boolean ; const parent : TFRE_DB_GUID); { notify step 1 }
    procedure   RemoveObjectByNotify          (const coll_name : TFRE_DB_NameType ; const obj : IFRE_DB_Object ; const rl_rem : boolean ; const parent : TFRE_DB_GUID); { notify step 1 }

    procedure   SetupOutboundRefLink          (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL); { notify step 1 }
    procedure   SetupInboundRefLink           (const from_obj : IFRE_DB_Object   ; const to_obj: TGUID          ; const key_description : TFRE_DB_NameTypeRL); { notify step 1 }
    procedure   InboundReflinkDropped         (const from_obj: IFRE_DB_Object    ; const to_obj   : TGUID       ; const key_description : TFRE_DB_NameTypeRL); { notify step 1 }
    procedure   OutboundReflinkDropped        (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL); { notify step 1 }


    function    ExistsObj                     (const uid:TFRE_DB_GUID):NativeInt;
    procedure   SetTransformedObject          (const tr_obj : IFRE_DB_Object);override;                           { inital fill, from initial transform }
    procedure   SetTransObjectSingleInsert    (const tr_obj : IFRE_DB_Object);override;                           { single fill from notify }

    procedure   HandleUpdateTransformedObject (const tr_obj : IFRE_DB_Object; const upd_idx: NativeInt); override;{ notify update, step 2 }
    procedure   HandleInsertTransformedObject (const tr_obj : IFRE_DB_Object);override;                           { notify update, step 2 }
    procedure   HandleDeleteTransformedObject (const del_idx : NativeInt);                                        { notify update, step 2 }

    procedure   TransformAll                  (var rcnt : NativeInt);                                             { transform all objects of the parent collection }
    constructor Create                        (const key : TFRE_DB_TRANS_COLL_DATA_KEY ; const dc : IFRE_DB_DERIVED_COLLECTION);
    destructor  Destroy                       ; override;
    procedure   ForAllObjs                    (const forall : TFRE_DB_Obj_Iterator);

    procedure   AddOrdering                   (const ordering : TFRE_DB_TRANS_COLL_DATA);
    procedure   RemoveOrdering                (const ordering : TFRE_DB_TRANS_COLL_DATA);
    function    GetTransFormKey               : TFRE_DB_TRANS_COLL_DATA_KEY;
  end;

  { TFRE_DB_OrderContainer }

  TFRE_DB_OrderContainer=class
  private
    FOBJArray : OFRE_SL_TFRE_DB_Object;
  public
    function    AddObject     (const obj : TFRE_DB_Object):boolean;
    function    Exists        (const obj : TFRE_DB_Object):boolean;
    procedure   ReplaceObject (const old_obj,new_obj : TFRE_DB_Object);
    function    RemoveObject  (const old_obj : TFRE_DB_Object):boolean; { true if this was the last value in the container }
    procedure   ForAllBreak   (const iter : IFRE_DB_ObjectIteratorBrk ; var halt : boolean);
    constructor Create        ;
  end;

  { TFRE_DB_FilterContainer }

  TFRE_DB_FilterContainer=class
  private
    FOBJArray        : Array of IFRE_DB_Object;
    FCnt             : NativeUint;
    FFilled          : Boolean;
    FFCCreationTime  : TFRE_DB_DateTime64;
    FFilters         : TFRE_DB_DC_FILTER_DEFINITION;
    ForderKey        : TFRE_DB_NameType;
    procedure SetFilled(AValue: boolean);
  public
    property    IsFilled                      : boolean read FFilled write SetFilled;
    procedure   CheckDBReevaluation           ;
    procedure   Execute                       (const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY ; const compare_run : boolean); { compare run = second run on filter updates }
    procedure   CheckFilteredAdd              (const obj : IFRE_DB_Object);
    procedure   Notify_CheckFilteredUpdate    (const td  : TFRE_DB_TRANS_COLL_DATA ; const old_obj,new_obj : IFRE_DB_Object ; const order_changed : boolean); { invoke session update }
    function    Notify_CheckFilteredDelete    (const td  : TFRE_DB_TRANS_COLL_DATA ; const old_obj         : IFRE_DB_Object ; const do_qry_proc : boolean=true) : NativeInt; { invoke session update }
    function    Notify_CheckFilteredInsert    (const td  : TFRE_DB_TRANS_COLL_DATA ; const new_obj         : IFRE_DB_Object ; const do_qry_proc : boolean=true) : NativeInt; { invoke session update }
    function    DoesObjectPassFilterContainer (const obj : IFRE_DB_Object):boolean;
    procedure   AdjustLength                  ;
    constructor Create                        (const order_key : TFRE_DB_NameType);
    destructor  Destroy                       ; override;
    function    Filters                       : TFRE_DB_DC_FILTER_DEFINITION;
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
    procedure          InsertIntoTree (const insert_obj : TFRE_DB_Object);

    procedure          Notify_UpdateIntoTree (const old_obj,new_obj : TFRE_DB_Object);
    procedure          Notify_InsertIntoTree (const new_obj : TFRE_DB_Object);
    procedure          Notify_InsertIntoTree (const key: PByte; const keylen: NativeInt; const new_obj: TFRE_DB_Object ; const propagate_up : boolean = true);
    procedure          Notify_DeleteFromTree (const old_obj : TFRE_DB_Object);
    procedure          Notify_DeleteFromTree (const key: PByte; const keylen: NativeInt; const old_obj: TFRE_DB_Object ; const propagate_up : boolean = true);
  public
    procedure    LockBase                ; override;
    procedure    UnlockBase              ; override;
    constructor  Create                  (const orderdef : TFRE_DB_DC_ORDER_DEFINITION ; base_trans_data : TFRE_DB_TRANFORMED_DATA);
    destructor   Destroy                 ; override;
    procedure    Execute                 (const iter : IFRE_DB_Obj_Iterator ; const qry_context : TFRE_DB_QUERY ; const compare_run : boolean);
    procedure    OrderTheData            ;
    function     GetFullKey              : TFRE_DB_TRANS_COLL_DATA_KEY; override;
    procedure    UpdateTransformedobject (const old_obj,new_object : IFRE_DB_Object);
    procedure    InsertTransformedobject (const new_object : IFRE_DB_Object);
    procedure    DeleteTransformedobject (const del_object : IFRE_DB_Object);
  end;

  OFRE_DB_TransCollTransformedDataList = specialize OGFOS_Array<TFRE_DB_TRANFORMED_DATA>; { list of transformed collections }
  OFRE_DB_TransCollOrderedDataList     = specialize OGFOS_Array<TFRE_DB_TRANS_COLL_DATA>; { orders upon the data, referencing to the collection }


  TFRE_DB_TDM_STATS_CLEANER=class;

  TFRE_DB_INBOUND_N_BLOCK=class
    FBlock : IFRE_DB_Object;
  end;

  { TFRE_DB_TRANSDATA_MANAGER }

  TFRE_DB_TRANSDATA_MANAGER=class(TFRE_DB_TRANSDATA_MANAGER_BASE,IFRE_DB_DBChangedNotification)
  private
  type
       TFRE_DB_QueryIterator = procedure(const query : TFRE_DB_QUERY) is nested;
  var
    FTransformKey  : QWord;
    FStatCleaner   : TFRE_DB_TDM_STATS_CLEANER;
    FArtQueryStore : TFRE_ART_TREE;
    FTransLock     : IFOS_LOCK;
    FTransList     : OFRE_DB_TransCollTransformedDataList; { List of base transformed data}
    FOrders        : OFRE_DB_TransCollOrderedDataList;     { List of orderings of base transforms}
    FCurrentNotify : TFRE_DB_TRANSDATA_CHANGE_NOTIFIER;    { gather list of notifications for a notification block (transaction) }
    FCurrentNLayer : TFRE_DB_NameType;

    procedure   ForAllQueries          (const query_iter : TFRE_DB_QueryIterator);
    function    GetBaseTransformedData (base_key : TFRE_DB_NameTypeRL ; out base_data : TFRE_DB_TRANFORMED_DATA) : boolean;
    procedure   AddBaseTransformedData (const base_data : TFRE_DB_TRANFORMED_DATA);
    procedure   TL_StatsTimer;

    {NOFIF BLOCK INTERFACE}
    procedure  StartNotificationBlock (const key : TFRE_DB_TransStepId);
    procedure  FinishNotificationBlock(out block : IFRE_DB_Object);
    procedure  SendNotificationBlock  (const block : IFRE_DB_Object);
    procedure  CollectionCreated      (const coll_name: TFRE_DB_NameType) ;
    procedure  CollectionDeleted      (const coll_name: TFRE_DB_NameType) ;
    procedure  IndexDefinedOnField    (const coll_name: TFRE_DB_NameType  ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean);
    procedure  IndexDroppedOnField    (const coll_name: TFRE_DB_NameType  ; const index_name: TFRE_DB_NameType);
    procedure  ObjectStored           (const coll_name: TFRE_DB_NameType  ; const obj : IFRE_DB_Object); { FULL STATE }
    procedure  ObjectDeleted          (const obj : IFRE_DB_Object);                                      { FULL STATE }
    procedure  ObjectRemoved          (const coll_name: TFRE_DB_NameType ; const obj : IFRE_DB_Object);  { FULL STATE }
    procedure  ObjectUpdated          (const obj : IFRE_DB_Object ; const colls:TFRE_DB_StringArray);    { FULL STATE }
    procedure  SubObjectStored        (const obj : IFRE_DB_Object ; const parent_field_name : TFRE_DB_NameType ; const ParentObjectUIDPath : TFRE_DB_GUIDArray);
    procedure  SubObjectDeleted       (const obj : IFRE_DB_Object ; const parent_field_name : TFRE_DB_NameType ; const ParentObjectUIDPath : TFRE_DB_GUIDArray);
    procedure  DifferentiallUpdStarts (const obj_uid   : IFRE_DB_Object);           { DIFFERENTIAL STATE}
    procedure  FieldDelete            (const old_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldAdd               (const new_field : IFRE_DB_Field);            { DIFFERENTIAL STATE}
    procedure  FieldChange            (const old_field,new_field : IFRE_DB_Field);  { DIFFERENTIAL STATE}
    procedure  DifferentiallUpdEnds   (const obj_uid   : TFRE_DB_GUID);             { DIFFERENTIAL STATE}
    procedure  SetupOutboundRefLink   (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL);
    procedure  SetupInboundRefLink    (const from_obj : IFRE_DB_Object   ; const to_obj: TGUID          ; const key_description : TFRE_DB_NameTypeRL);
    procedure  InboundReflinkDropped  (const from_obj: IFRE_DB_Object    ; const to_obj   : TGUID       ; const key_description : TFRE_DB_NameTypeRL);
    procedure  OutboundReflinkDropped (const from_obj : TGUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL);
    procedure  FinalizeNotif          ;
    {NOFIF BLOCK INTERFACE - END}
    function   DBC                    (const dblname : TFRE_DB_NameType) : IFRE_DB_CONNECTION;

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
    function   GenerateQueryFromRawInput (const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray ; const dependency_reference_constraint : TFRE_DB_NameTypeRLArrayArray;
                                          const dependency_negate : boolean ; const parent_child_spec : TFRE_DB_NameTypeRL ; const parent_child_skip_schemes : TFRE_DB_NameTypeRLArray ;
                                          const dc_name,parent_name : TFRE_DB_NameTypeRL ; const dc_static_filters : TFRE_DB_DC_FILTER_DEFINITION_BASE ;
                                          const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean;
                                          const session : IFRE_DB_UserSession): TFRE_DB_QUERY_BASE; override ;
    { remember the query as open for the session }
    procedure   StoreQuery                (const qry: TFRE_DB_QUERY_BASE); override;
    { forget the query as open for the session }
    procedure   RemoveQuery               (const qry_id : TFRE_DB_NameType);override;
    { forget all querys for the session/dc }
    procedure   DropAllQuerys             (const session : IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL); override; { can be dc wide, or session wide dc_name='' }
    function    FormQueryID               (const session : IFRE_DB_UserSession ; const dc_name : TFRE_DB_NameTypeRL ; const client_part : shortstring):TFRE_DB_NameType;override;
    procedure   ApplyInboundNotificationBlock  (const dbname: TFRE_DB_NameType ; const block : IFRE_DB_Object);
    procedure   InboundNotificationBlock  (const dbname: TFRE_DB_NameType ; const block : IFRE_DB_Object); override;

    { --- Notify gathering }
    procedure   CN_AddDirectSessionUpdateEntry       (const update_dbo : IFRE_DB_Object); { add a dbo update for sessions dbo's (forms) }
    procedure   CN_AddGridInplaceUpdate              (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : TFRE_DB_NameType ; const upo : IFRE_DB_Object);
    procedure   CN_AddGridInplaceDelete              (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : TFRE_DB_NameType ; const del_id: TFRE_DB_String);
    procedure   CN_AddGridInsertUpdate               (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : TFRE_DB_NameType ; const upo : IFRE_DB_Object ;  const parent_id,reference_id: TFRE_DB_String ; const before : boolean);
    procedure   UpdateObjectInFilterKey              (const td : TFRE_DB_TRANS_COLL_DATA ; const filtercont : TFRE_DB_FilterContainer ; const new_obj : IFRE_DB_Object ; const idx : NativeInt); { in place update }
    procedure   RemoveUpdateObjectInFilterKey        (const td : TFRE_DB_TRANS_COLL_DATA ; const filtercont : TFRE_DB_FilterContainer ; const old_obj : IFRE_DB_Object ; const idx : NativeInt);
    procedure   InsertUpdateObjectInFilterKey        (const td : TFRE_DB_TRANS_COLL_DATA ; const filtercont : TFRE_DB_FilterContainer ; const new_obj : IFRE_DB_Object ; const idx : NativeInt ; const parent_id : string);
  end;

  { TFRE_DB_TDM_STATS_CLEANER }

  TFRE_DB_TDM_STATS_CLEANER=class(TThread)
  private
    FStatEvent : IFOS_TE;
    FQ         : IFOS_LFQ;
    FTM        : TFRE_DB_TRANSDATA_MANAGER;
  public
    constructor Create(const tm:TFRE_DB_TRANSDATA_MANAGER);
    destructor  Destroy;override;
    procedure   Terminate;
    procedure   Execute ;override;
  end;


procedure  InitTransfromManager;
procedure  FinalizeTransformManager;

implementation

function G_TCDM : TFRE_DB_TRANSDATA_MANAGER;
begin
  result := GFRE_DB_TCDM as TFRE_DB_TRANSDATA_MANAGER;
end;


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

procedure FREDB_DumpArray(const object_array : IFRE_DB_ObjectArray ; const start, cnt : NativeInt ; const fn : string);
var i : NativeInt;
begin
  for i := 0 to high(object_array) do
    begin
      //mkey := object_array[i].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
      writeln(i,' :: ',object_array[i].field(fn).AsString,' -- ',object_array[i].UID_String);
    end;
end;

function  FREDB_BinaryFindIndexInSorted(const search_key : TFRE_DB_ByteArray ; const order_Key : TFRE_DB_NameType ; var before : NativeInt ; const object_array : IFRE_DB_ObjectArray ; var exists : boolean ; const exact_uid : PGuid=nil):NativeInt;
var midx,leftx,rightx : NativeInt;
    mkey              : TFRE_DB_ByteArray;
    res               : NativeInt;

    function Compare(key1,key2 : TFRE_DB_ByteArray):NativeInt;
    var k1l,k2l,km,i : NativeInt;
        v1,v2        : byte;
    begin
      k1l := Length(key1);
      k2l := Length(key2);
      km  := max(k1l,k2l)-1;
      for i :=0 to km do
       begin
         if i<k1l then
           v1 := key1[i]
         else
           v1 := 0;
         if i<k2l then
           v2 := key2[i]
         else
           v2 := 0;
         result := v1 - v2;
         if result<>0 then
           break;
       end;
      //if Length(key1)=Length(key2) then
      //  begin
      //    Result := CompareByte(key1[0],key2[0],Length(key1));
      //  end
      //else
      //  if Length(key1)<Length(key2) then
      //    result := -1
      //  else
      //    result := 1;
    end;

    //procedure CheckResult;
    //var i :integer;
    //
    //    function Dumpb(const a: TFRE_DB_ByteArray):string;
    //    var i : integer;
    //    begin
    //      result := '';
    //      for i := 0 to high(a) do
    //       result := result+'.'+inttostr(a[i]);
    //    end;
    //
    //begin
    //  writeln('>---BINARY INSERT DUMP--- ',order_Key);
    //  for i := 0 to high(object_array) do
    //    begin
    //      mkey := object_array[i].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
    //      writeln(i,' :: ',Dumpb(mkey),' /||/  ',Dumpb(search_key),' ix=',midx,' ',before,'  -- ',Compare(mkey,search_key));
    //    end;
    //  writeln('<---BINARY INSERT DUMP---',order_Key);
    //end;

    function FindExactKey : NativeInt;
    begin
      {first go to beginning }
      if object_array[midx].UID=exact_uid^ then { quick bailout }
        exit(midx);
      repeat { go back }
        if (midx=0) then
          break;
        mkey := object_array[midx-1].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
        if Compare(search_key,mkey)=-1 then
          dec(midx)
        else
          break;
      until false;
      repeat
        if object_array[midx].UID=exact_uid^ then { quick bailout }
          exit(midx);
        if Compare(search_key,mkey)=0 then
          inc(midx)
        else
          break;
      until false;
      raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the exact uid was not found in the array');
    end;

    function FindLastKey : NativeInt;
    begin
      repeat { go back }
        if (midx=high(object_array)) then
          break;
        mkey := object_array[midx+1].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
        if Compare(search_key,mkey)=0 then
          inc(midx)
        else
          break;
      until false;
    end;


begin
  leftx  := 0;
  before := 0;
  midx   := 0;
  res    := 0;
  rightx := high(object_array);
  while  leftx<=rightx do
    begin
       midx := leftx + ((rightx - leftx) div 2);
       mkey := object_array[midx].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
       if Length(mkey)=0 then
         raise EFRE_DB_Exception.Create(edb_ERROR,'invalid order key len 0  ');
       res := Compare(mkey,search_key);
       if res=0 then
         begin
           exists := true;
           result := midx;
           if assigned(exact_uid) then
             res := FindExactKey
           else
             res := FindLastKey;
           exit;
         end;
       if res>0 then
         rightx := midx-1
       else
         leftx  := midx+1;
    end;
  exists := false;
  before := res;  { 1 = before}
  result := midx;
  //CheckResult;
end;

{ TFRE_DB_FILTER_AUTO_DEPENDENCY }

function TFRE_DB_FILTER_AUTO_DEPENDENCY.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_AUTO_DEPENDENCY;
begin
  fClone                  := TFRE_DB_FILTER_AUTO_DEPENDENCY.Create(FKey);
  fClone.FNegate          := FNegate;
  fClone.FFieldname       := FFieldname;
  fClone.FAllowNull       := FAllowNull;
  fClone.FStartValues     := Copy(FStartValues);
  fClone.FValues          := Copy(FValues);
  fClone.FRL_Spec         := Copy(FRL_Spec);
  fClone.FDBName          := FDBName;
  fClone.FNeedsReEvaluate := FNeedsReEvaluate;
  result                  := fClone;
end;

function TFRE_DB_FILTER_AUTO_DEPENDENCY.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fieldvals      : TFRE_DB_GUIDArray;
     fld           : IFRE_DB_Field;
     error_fld     : boolean;
     i,j           : NativeInt;
begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      fieldvals     := fld.AsGUIDArr;
      try
        if not fnegate then
          begin
            result := false;
            for i:=0 to high(fieldvals) do
             for j:=0 to high(FValues) do
               if FREDB_Guids_Same(fieldvals[i],FValues[j]) then
                 begin
                   result := true;
                   break;
                 end;
          end
        else
          begin
            if length(FValues)=0 then
              exit;
            result := true;
            for i:=0 to high(fieldvals) do
             for j:=0 to high(FValues) do
               if FREDB_Guids_Same(fieldvals[i],FValues[j]) then
                 begin
                   result := false;
                   break;
                 end;
          end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := result or error_fld; { invert result, or filter error results }
end;
function TFRE_DB_FILTER_AUTO_DEPENDENCY.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String;
begin
  hsh := GFRE_BT.HashFast32(nil,0,0);
  scr := FDBName+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FRL_Spec) do
    hsh := GFRE_BT.HashFast32(@FRL_Spec[i][1],Length(FRL_Spec[i]),hsh);
  for i:= 0 to high(FStartValues) do
    hsh := GFRE_BT.HashFast32(@FStartValues[i],SizeOf(TFRE_DB_GUID),hsh);
  result := 'A:'+ GFRE_BT.Mem2HexStr(@hsh,4);
end;

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.ExpandReferences(const dbc: IFRE_DB_CONNECTION);
begin
  (dbc.Implementor as TFRE_DB_CONNECTION).ExpandReferencesNoRightCheck(FStartValues,FRL_Spec,FValues);
  //(dbc.Implementor as TFRE_DB_CONNECTION).ExpandReferences(FStartValues,FRL_Spec,FValues);
end;

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.InitFilter(const RL_Spec: TFRE_DB_NameTypeRLArray; const StartDependecyValues: TFRE_DB_GUIDArray; const negate: boolean; const include_null_values: boolean; const dbname: TFRE_DB_NameType);
begin
  FRL_Spec         := RL_Spec;
  FStartValues     := StartDependecyValues;
  FNegate          := negate;
  FAllowNull       := include_null_values;
  FDBName          := dbname;
  FNeedsReEvaluate := true;
  FFieldname       := 'UID';
end;

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.ReEvaluateFilter;
var db : IFRE_DB_CONNECTION;
begin
  db := G_TCDM.DBC(FDBName);
  ExpandReferences(db);
end;

function TFRE_DB_FILTER_AUTO_DEPENDENCY.CheckReflinkUpdateEvent(const key_descr: TFRE_DB_NameTypeRL): boolean;
var
  i: NativeInt;
begin
  result := false;
  for i := 0 to high(FRL_Spec) do
     begin
       if key_descr=FRL_Spec[i] then
         begin
           result := true;
           break;
         end;
     end;
  if result then
    ReEvaluateFilter;
end;


{ TFRE_DB_SESSION_UPO }

constructor TFRE_DB_SESSION_UPO.Create(const session_id: TFRE_DB_NameType);
begin
  FStoreList := TFPHashObjectList.Create(false);
  FSessid    := session_id;
end;

destructor TFRE_DB_SESSION_UPO.Destroy;
begin
  FStoreList.Free;
  inherited Destroy;
end;

procedure TFRE_DB_SESSION_UPO.AddStoreUpdate(const store_id, qid: TFRE_DB_NameType; const upo: IFRE_DB_Object);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st :=  FStoreList.Find(store_id) as TFRE_DB_UPDATE_STORE_DESC;
  if not Assigned(update_st) then
    begin
      update_st := TFRE_DB_UPDATE_STORE_DESC.create.Describe(store_id);
      FStoreList.Add(store_id,update_st);
    end;
  update_st.addUpdatedEntry(upo);
end;

procedure TFRE_DB_SESSION_UPO.AddStoreInsert(const store_id, qid: TFRE_DB_NameType; const upo: IFRE_DB_Object; const parent_id, reference_id: TFRE_DB_String; const before: boolean);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st :=  FStoreList.Find(store_id) as TFRE_DB_UPDATE_STORE_DESC;
  if not Assigned(update_st) then
    begin
      update_st := TFRE_DB_UPDATE_STORE_DESC.create.Describe(store_id);
      FStoreList.Add(store_id,update_st);
    end;
  update_st.addNewEntry(upo,reference_id,parent_id,not before);
end;

procedure TFRE_DB_SESSION_UPO.AddStoreDelete(const store_id, qid: TFRE_DB_NameType; const id: TFRE_DB_String);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st :=  FStoreList.Find(store_id) as TFRE_DB_UPDATE_STORE_DESC;
  if not Assigned(update_st) then
    begin
      update_st := TFRE_DB_UPDATE_STORE_DESC.create.Describe(store_id);
      FStoreList.Add(store_id,update_st);
    end;
  update_st.addDeletedEntry(id);
end;

procedure TFRE_DB_SESSION_UPO.DispatchAllNotifications;
var i  : NativeInt;
    ct : TFRE_DB_CONTENT_DESC;
begin
  for i := 0 to FStoreList.Count-1 do
     begin
       ct := FStoreList.Items[i] as TFRE_DB_CONTENT_DESC;
       GFRE_DBI.NetServ.SendDelegatedContentToClient(FSessid,ct);
     end;
  //ses.DispatchCoroutine(@ses.COR_SendContentOnBehalf,update_st);
    //end;
end;

{ TFRE_DB_TRANSDATA_CHANGE_NOTIFIER }

function TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.GetSessionUPO(const sessionid : TFRE_DB_NameType): TFRE_DB_SESSION_UPO;
begin
  result := FSessionUpdateList.Find(sessionid) as TFRE_DB_SESSION_UPO;
  if not assigned(result) then
    begin
      result := TFRE_DB_SESSION_UPO.Create(sessionid);
      FSessionUpdateList.Add(sessionid,result);
    end;
end;

constructor TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.Create;
begin
  FSessionUpdateList := TFPHashObjectList.Create(true);
end;

destructor TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.Destroy;
begin
  FSessionUpdateList.free;
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddDirectSessionUpdateEntry(const update_dbo: IFRE_DB_Object);
var halt : boolean;
  //procedure AllSessions
begin
  halt := false;
  { todo session should register update dbo's here ...}
  //GFRE_DBI.NetServ.ForAllSessionsLocked(@AllSessions,halt);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridInplaceUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const query_id: TFRE_DB_NameType; const upo: IFRE_DB_Object);
begin
  GetSessionUPO(sessionid).AddStoreUpdate(store_id,query_id,upo);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridInsertUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const upo: IFRE_DB_Object; const parent_id,reference_id: TFRE_DB_String; const before: boolean);
begin
  GetSessionUPO(sessionid).AddStoreInsert(store_id,qry_id,upo,parent_id,reference_id,before);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridRemoveUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const del_id: TFRE_DB_String);
begin
  GetSessionUPO(sessionid).AddStoreDelete(store_id,qry_id,del_id);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.NotifyAll;
var i    : NativeInt;
    supo : TFRE_DB_SESSION_UPO;
begin
  for i  := 0 to FSessionUpdateList.Count-1 do
   begin
     supo := FSessionUpdateList.Items[i] as TFRE_DB_SESSION_UPO;
     supo.DispatchAllNotifications;
   end;
end;


{ TFRE_DB_TDM_STATS_CLEANER }

constructor TFRE_DB_TDM_STATS_CLEANER.Create(const tm: TFRE_DB_TRANSDATA_MANAGER);
begin
  FTM := tm;
  GFRE_TF.Get_TimedEvent(FStatEvent);
  GFRE_TF.Get_LFQ(FQ);
  inherited Create(false);
end;

destructor TFRE_DB_TDM_STATS_CLEANER.Destroy;
begin
  Terminate;
  WaitFor;
  FStatEvent.Finalize;
  inherited Destroy;
end;

procedure TFRE_DB_TDM_STATS_CLEANER.Terminate;
begin
  inherited Terminate;
  FStatEvent.SetEvent;
end;

procedure TFRE_DB_TDM_STATS_CLEANER.Execute;
var shot  : Boolean;
    block : TFRE_DB_INBOUND_N_BLOCK;
begin
  repeat
    FStatEvent.WaitFor(1000);
    if not Terminated then
      begin
        repeat
          block := TFRE_DB_INBOUND_N_BLOCK(FQ.Pop);
          if block = nil then
            break;
          try
            TFRE_DB_TRANSDATA_MANAGER(GFRE_DB_TCDM).ApplyInboundNotificationBlock('',block.FBlock);
            block.FBlock.Finalize;
            block.FBlock:=nil;
            block.Free;
            block:=nil;
          except
            on e:exception do
              begin

              end;
          end;
        until false;
        FTM.TL_StatsTimer;
      end
    else
      begin
        writeln('FINAL ROUND');
      end;
  until Terminated;
end;

{ TFRE_DB_FILTER_CHILD }

function TFRE_DB_FILTER_CHILD.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_CHILD;
begin
  fClone                := TFRE_DB_FILTER_CHILD.Create(FKey);
  result                := fClone;
end;

function TFRE_DB_FILTER_CHILD.GetDefinitionKey: TFRE_DB_NameType;
begin
  result := 'CF';
end;

function TFRE_DB_FILTER_CHILD.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fld  : IFRE_DB_Field;
    fp   : string;
begin
  if obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_FULL,fld) then
    fp := fld.AsString
  else
    fp := '';
  result :=  fp='';// not obj.FieldExists(cFRE_DB_SYS_PARENT_PATH_FULL);
end;

procedure TFRE_DB_FILTER_CHILD.InitFilter;
begin

end;

{ TFRE_DB_FILTER_PARENT }

function TFRE_DB_FILTER_PARENT.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_PARENT;
begin
  fClone                := TFRE_DB_FILTER_PARENT.Create(FKey);
  fClone.FAllowedParent := FAllowedParent;
  result                := fClone;
end;


function TFRE_DB_FILTER_PARENT.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : NativeInt;
begin
  hsh := GFRE_BT.HashFast32(@FAllowedParent[1],length(FAllowedParent),0);
  result := 'PF:'+ GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_PARENT.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fld  : IFRE_DB_FIELD;
    pid  : String;
begin
  result := false;
  if obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_PART,fld) then
    begin
      pid    :=  fld.AsString;
      result :=  FAllowedParent=pid; { FAllowed Parent (part) }
    end
end;

procedure TFRE_DB_FILTER_PARENT.InitFilter(const allowed_parent_path: TFRE_DB_GUIDArray);
var
  i: NativeInt;
begin
  for i:=0 to high(allowed_parent_path)-1 do
    FAllowedParent := FAllowedParent+'-'+FREDB_G2H(allowed_parent_path[i]);
  FAllowedParent:=FAllowedParent+FREDB_G2H(allowed_parent_path[i]);
end;

{ TFRE_DB_FILTER_REAL64 }

function TFRE_DB_FILTER_REAL64.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_REAL64;
begin
  fClone             := TFRE_DB_FILTER_REAL64.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_REAL64.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(QWord),hsh);
  result := 'R:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_REAL64.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var multivalfield : boolean;
    fieldval      : Double;
    fieldisnull   : boolean;
    fielmismatch  : boolean;
    error_fld     : boolean;
    fld           : IFRE_DB_Field;

  procedure DoInBounds;
  var lbnd,ubnd : Double;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>lbnd) and (fieldval<ubnd);
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : Double;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>=lbnd) and (fieldval<=ubnd);
  end;

  procedure AllValues;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure OneValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure NoValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsReal64;
        case FFilterType of
          dbnf_EXACT:                result := fieldval= FValues[0];
          dbnf_LESSER:               result := fieldval< FValues[0];
          dbnf_LESSER_EQ:            result := fieldval<=FValues[0];
          dbnf_GREATER:              result := fieldval> FValues[0];
          dbnf_GREATER_EQ:           result := fieldval>=FValues[0];
          dbnf_IN_RANGE_EX_BOUNDS:   DoInBounds;
          dbnf_IN_RANGE_WITH_BOUNDS: DoWithBounds;
          dbnf_AllValuesFromFilter:  AllValues;
          dbnf_OneValueFromFilter:   OneValue;
          dbnf_NoValueInFilter:      NoValue;
        end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

procedure TFRE_DB_FILTER_REAL64.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of Double; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT,
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the real64 filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
      if Length(filtervalues)<>2 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the real64 filter with numfiltertype %s, needs exactly two bounding values',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_NoValueInFilter,
    dbnf_OneValueFromFilter:
      if Length(filtervalues)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the real64 filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_DATETIME }

function TFRE_DB_FILTER_DATETIME.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_DATETIME;
begin
  fClone             := TFRE_DB_FILTER_DATETIME.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_DATETIME.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(QWord),hsh);
  result := 'D:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_DATETIME.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var multivalfield : boolean;
    fieldval      : TFRE_DB_DateTime64;
    fieldisnull   : boolean;
    fielmismatch  : boolean;
    error_fld     : boolean;
    fld           : IFRE_DB_Field;

  procedure DoInBounds;
  var lbnd,ubnd : TFRE_DB_DateTime64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>lbnd) and (fieldval<ubnd);
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : TFRE_DB_DateTime64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>=lbnd) and (fieldval<=ubnd);
  end;

  procedure AllValues;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure OneValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure NoValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;


begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsDateTimeUTC;
        case FFilterType of
          dbnf_EXACT:                result := fieldval= FValues[0];
          dbnf_LESSER:               result := fieldval< FValues[0];
          dbnf_LESSER_EQ:            result := fieldval<=FValues[0];
          dbnf_GREATER:              result := fieldval> FValues[0];
          dbnf_GREATER_EQ:           result := fieldval>=FValues[0];
          dbnf_IN_RANGE_EX_BOUNDS:   DoInBounds;
          dbnf_IN_RANGE_WITH_BOUNDS: DoWithBounds;
          dbnf_AllValuesFromFilter:  AllValues;
          dbnf_OneValueFromFilter:   OneValue;
          dbnf_NoValueInFilter:      NoValue;
        end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

procedure TFRE_DB_FILTER_DATETIME.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of Int64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT,
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the datetime filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
      if Length(filtervalues)<>2 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the datetime filter with numfiltertype %s, needs exactly two bounding values',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_NoValueInFilter,
    dbnf_OneValueFromFilter:
      if Length(filtervalues)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the datetime filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_CURRENCY }

function TFRE_DB_FILTER_CURRENCY.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_CURRENCY;
begin
  fClone             := TFRE_DB_FILTER_CURRENCY.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_CURRENCY.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(QWord),hsh);
  result := 'C:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_CURRENCY.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var multivalfield : boolean;
    fieldval      : Currency;
    fieldisnull   : boolean;
    fielmismatch  : boolean;
    error_fld     : boolean;
    fld           : IFRE_DB_Field;

  procedure DoInBounds;
  var lbnd,ubnd : Currency;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>lbnd) and (fieldval<ubnd);
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : Currency;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>=lbnd) and (fieldval<=ubnd);
  end;

  procedure AllValues;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure OneValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure NoValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsCurrency;
        case FFilterType of
          dbnf_EXACT:                result := fieldval= FValues[0];
          dbnf_LESSER:               result := fieldval< FValues[0];
          dbnf_LESSER_EQ:            result := fieldval<=FValues[0];
          dbnf_GREATER:              result := fieldval> FValues[0];
          dbnf_GREATER_EQ:           result := fieldval>=FValues[0];
          dbnf_IN_RANGE_EX_BOUNDS:   DoInBounds;
          dbnf_IN_RANGE_WITH_BOUNDS: DoWithBounds;
          dbnf_AllValuesFromFilter:  AllValues;
          dbnf_OneValueFromFilter:   OneValue;
          dbnf_NoValueInFilter:      NoValue;
      end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

procedure TFRE_DB_FILTER_CURRENCY.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of Currency ; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT,
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the currency filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
      if Length(filtervalues)<>2 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the currency filter with numfiltertype %s, needs exactly two bounding values',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_NoValueInFilter,
    dbnf_OneValueFromFilter:
      if Length(filtervalues)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the currency filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_RIGHT }

function TFRE_DB_FILTER_RIGHT.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_RIGHT;
begin
  fClone             := TFRE_DB_FILTER_RIGHT.Create(FKey);
  fClone.FRight      := FRight;
  fClone.FNegate     := FNegate;
  fClone.FUserToken  := FUserToken;
  result             := fClone;
end;

function TFRE_DB_FILTER_RIGHT.GetDefinitionKey: TFRE_DB_NameType;
begin
  result := 'Z:';
  if sr_STORE in FRight then
    result:=result+'S';
  if sr_UPDATE in FRight then
    result:=result+'U';
  if sr_FETCH in FRight then
    result:=result+'F';
  if sr_DELETE in FRight then
    result:=result+'D';
  if FNegate then
    result:=result+'1'
  else
    result:=result+'0';
  result:=result+FUserToken.GetUniqueTokenKey;
end;

function TFRE_DB_FILTER_RIGHT.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var cn:ShortString;
begin
  cn := obj.PreTransformedScheme;
  result := FUserToken.CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,cn,FRight)=edb_OK;
end;

procedure TFRE_DB_FILTER_RIGHT.InitFilter(stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean);
begin
  if stdrightset=[] then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one right must be specified for the filter');
  FRight     := stdrightset;
  FUserToken := usertoken;
  FNegate    := negate;
end;

{ TFRE_DB_FILTER_SCHEME }

function TFRE_DB_FILTER_SCHEME.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_SCHEME;
begin
  fClone             := TFRE_DB_FILTER_SCHEME.Create(FKey);
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := False;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_SCHEME.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
begin
  result := false;
  inc(flt_errors); // Scheme Filter not implemented
end;

function TFRE_DB_FILTER_SCHEME.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(nil,0,0);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i][1],Length(FValues[i]),hsh);
  result := 'X:'+GFRE_BT.Mem2HexStr(@hsh,4)+BoolToStr(FNegate,'1','0');
end;

procedure TFRE_DB_FILTER_SCHEME.InitFilter(filtervalues: array of TFRE_DB_String; const negate: boolean);
var i:integer;
begin
  if Length(filtervalues)=0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one scheme must be specified for the filter');
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FNegate     := negate;
end;

{ TFRE_DB_FILTER_UID }

function TFRE_DB_FILTER_UID.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_UID;
begin
  fClone             := TFRE_DB_FILTER_UID.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_UID.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fieldvals      : TFRE_DB_GUIDArray;
     fld           : IFRE_DB_Field;
     error_fld     : boolean;
     i,j           : NativeInt;
begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      fieldvals     := fld.AsGUIDArr;
      try
        case FFilterType of
          dbnf_EXACT:               { all fieldvalues and filtervalues must be the same in the same order }
            begin
              result := true;
              if Length(fieldvals) <> Length(FValues) then
                begin
                  result:=false;
                  exit;
                end
              else
                for i:=0 to high(FValues) do
                  if not FREDB_Guids_Same(fieldvals[i],FValues[i]) then
                    begin
                      result:=false;
                      exit;
                    end;
            end;
          dbnf_OneValueFromFilter:  {}
            begin
              result := false;
              for i:=0 to high(fieldvals) do
               for j:=0 to high(FValues) do
                 if FREDB_Guids_Same(fieldvals[i],FValues[j]) then
                   begin
                     result := true;
                     break;
                   end;
            end;
          dbnf_NoValueInFilter:
            begin
              if length(FValues)=0 then
                  exit;
                result := true;
                for i:=0 to high(fieldvals) do
                 for j:=0 to high(FValues) do
                   if FREDB_Guids_Same(fieldvals[i],FValues[j]) then
                     begin
                       result := false;
                       break;
                     end;
            end;

          //dbnf_NoValueInFilter:    begin
          //                           add := true;
          //                           if length(guid_filt_vals)=0 then
          //                             exit;
          //                           for i:=0 to high(guid_field_vals) do begin
          //                             for j:=0 to high(guid_filt_vals) do begin
          //                               if FREDB_Guids_Same(guid_field_vals[i],guid_filt_vals[j]) then begin
          //                                 add := false;
          //                                 exit;
          //                               end;
          //                             end;
          //                           end;
          //                         end;

          dbnf_AllValuesFromFilter: { all fieldvalues must be in filter}
            begin
              error_fld:=true;
              inc(flt_errors);
            end;
        end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor fnegate) or error_fld; { invert result, or filter error results }
end;

function TFRE_DB_FILTER_UID.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(nil,0,0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(TFRE_DB_GUID),hsh);
  result := 'G:'+ GFRE_BT.Mem2HexStr(@hsh,4);
end;

procedure TFRE_DB_FILTER_UID.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of TFRE_DB_GUID; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFieldname  := fieldname;
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the uid filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_OneValueFromFilter: ; { empty array is allowed }
    dbnf_NoValueInFilter: ; { empty array is allowed }
      //if Length(filtervalues)=0 then
      //  raise EFRE_DB_Exception.Create(edb_ERROR,'the uid filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ,
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
        raise EFRE_DB_Exception.Create(edb_ERROR,'the uid filter does not support numfiltertype %s',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_BOOLEAN }

function TFRE_DB_FILTER_BOOLEAN.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_BOOLEAN;
begin
  fClone             := TFRE_DB_FILTER_BOOLEAN.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValue      := FValue;
  result             := fClone;
end;

function TFRE_DB_FILTER_BOOLEAN.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fieldval       : boolean;
     fld           : IFRE_DB_Field;
     multivalfield : boolean;
     error_fld     : boolean;
begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsBoolean;
        result        := fieldval=FValue;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := not FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

function TFRE_DB_FILTER_BOOLEAN.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[8];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FValue,'1','0')+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  result := 'B:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

procedure TFRE_DB_FILTER_BOOLEAN.InitFilter(const fieldname: TFRE_DB_NameType; const value: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  FValue      := value;
  FNegate     := false;
  FAllowNull  := include_null_values;
end;

{ TFRE_DB_FILTER_UNSIGNED }

function TFRE_DB_FILTER_UNSIGNED.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_UNSIGNED;
begin
  fClone             := TFRE_DB_FILTER_UNSIGNED.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_UNSIGNED.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(QWord),hsh);
  result := 'U:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_UNSIGNED.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
begin
  result := false;
  inc(flt_errors); // Unsigned Filter not implemented
end;

procedure TFRE_DB_FILTER_UNSIGNED.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of UInt64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT,
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the unsigned filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
      if Length(filtervalues)<>2 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the unsigned filter with numfiltertype %s, needs exactly two bounding values',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_NoValueInFilter,
    dbnf_OneValueFromFilter:
      if Length(filtervalues)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the unsigned filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_SIGNED }

function TFRE_DB_FILTER_SIGNED.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_SIGNED;
begin
  fClone             := TFRE_DB_FILTER_SIGNED.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_SIGNED.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[6];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_NUM_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i],SizeOf(int64),hsh);
  result := 'S:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_SIGNED.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var multivalfield : boolean;
    fieldval      : int64;
    fieldisnull   : boolean;
    fielmismatch  : boolean;
    error_fld     : boolean;
    fld           : IFRE_DB_Field;

  procedure DoInBounds;
  var lbnd,ubnd : int64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>lbnd) and (fieldval<ubnd);
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : int64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := (fieldval>=lbnd) and (fieldval<=ubnd);
  end;

  procedure AllValues;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure NoValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

  procedure OneValue;
  begin
    error_fld:=true;
    inc(flt_errors); { not implemented }
  end;

begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsInt64;
        case FFilterType of
          dbnf_EXACT:                result := fieldval= FValues[0];
          dbnf_LESSER:               result := fieldval< FValues[0];
          dbnf_LESSER_EQ:            result := fieldval<=FValues[0];
          dbnf_GREATER:              result := fieldval> FValues[0];
          dbnf_GREATER_EQ:           result := fieldval>=FValues[0];
          dbnf_IN_RANGE_EX_BOUNDS:   DoInBounds;
          dbnf_IN_RANGE_WITH_BOUNDS: DoWithBounds;
          dbnf_AllValuesFromFilter:  AllValues;
          dbnf_OneValueFromFilter:   OneValue;
          dbnf_NoValueInFilter:      NoValue;
        end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

procedure TFRE_DB_FILTER_SIGNED.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of Int64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := numfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
  case numfiltertype of
    dbnf_EXACT,
    dbnf_LESSER,
    dbnf_LESSER_EQ,
    dbnf_GREATER,
    dbnf_GREATER_EQ:
      if Length(filtervalues)<>1 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the signed filter with numfiltertype %s, needs exactly one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_IN_RANGE_EX_BOUNDS,
    dbnf_IN_RANGE_WITH_BOUNDS:
      if Length(filtervalues)<>2 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the signed filter with numfiltertype %s, needs exactly two bounding values',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
    dbnf_AllValuesFromFilter,
    dbnf_NoValueInFilter,
    dbnf_OneValueFromFilter:
      if Length(filtervalues)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'the signed filter with numfiltertype %s, needs at least one value',[CFRE_DB_NUM_FILTERTYPE[numfiltertype]]);
  end;
end;

{ TFRE_DB_FILTER_STRING }

function TFRE_DB_FILTER_STRING.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_STRING;
begin
  fClone             := TFRE_DB_FILTER_STRING.Create(FKey);
  fClone.FFieldname  := FFieldname;
  fClone.FNegate     := FNegate;
  fClone.FAllowNull  := FAllowNull;
  fClone.FFilterType := FFilterType;
  fClone.FValues     := Copy(FValues);
  result             := fClone;
end;

function TFRE_DB_FILTER_STRING.GetDefinitionKey: TFRE_DB_NameType;
var  hsh : cardinal;
     i   : Integer;
     scr : String[4];
begin
  hsh := GFRE_BT.HashFast32(@FFieldname[1],Length(FFieldname),0);
  scr := CFRE_DB_STR_FILTERTYPE[FFilterType]+BoolToStr(FNegate,'1','0')+BoolToStr(FAllowNull,'1','0');
  hsh := GFRE_BT.HashFast32(@scr[1],Length(scr),hsh);
  for i:= 0 to high(FValues) do
    hsh := GFRE_BT.HashFast32(@FValues[i][1],Length(FValues[i]),hsh);
  result := 'T:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_STRING.CheckFilterHit(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fieldval       : TFRE_DB_String;
     fld           : IFRE_DB_Field;
     multivalfield : boolean;
     error_fld     : boolean;
begin
  error_fld := false;
  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsString;
        case FFilterType of
          dbft_EXACT:      result := AnsiContainsText(fieldval,FValues[0]) and (length(fieldval)=length(FValues[0]));
          dbft_PART:       result := AnsiContainsText(fieldval,FValues[0]);
          dbft_STARTPART:  result := AnsiStartsText  (FValues[0],fieldval);
          dbft_ENDPART:    result := AnsiEndsText    (FValues[0],fieldval);
        end;
      except { invalid conversion }
        error_fld := true;
        inc(flt_errors);
      end;
    end
  else { fld is null }
    result := FAllowNull;
  result := (result xor FNegate) or error_fld; { invert result, or filter error results }
end;

procedure TFRE_DB_FILTER_STRING.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of TFRE_DB_String; const stringfiltertype: TFRE_DB_STR_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var i:integer;
begin
  FFieldname  := fieldname;
  SetLength(FValues,length(filtervalues));
  for i:=0 to high(filtervalues) do
    FValues[i] := filtervalues[i];
  FFilterType := stringfiltertype;
  FNegate     := negate;
  FAllowNull  := include_null_values;
end;

{ TFRE_DB_FilterContainer }

procedure TFRE_DB_FilterContainer.SetFilled(AValue: boolean);
begin
  FFilled:=AValue;
end;

procedure TFRE_DB_FilterContainer.CheckDBReevaluation;
begin
  FFilters.CheckDBReevaluationFilters;
end;

function TFRE_DB_FilterContainer.DoesObjectPassFilterContainer(const obj: IFRE_DB_Object): boolean;
begin
  result := FFilters.DoesObjectPassFilters(obj);
end;

procedure TFRE_DB_FilterContainer.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY; const compare_run: boolean);
var i   : NativeInt;
    obj : IFRE_DB_Object;
begin
  //if qry_context.FStartIdx > High(FOBJArray) then
  //  raise EFRE_DB_Exception.Create(edb_MISMATCH,'query start point too high');
  qry_context.FQueryPotentialCount := FCnt;
  qry_context.SetMaxResultDBOLen(compare_run);
  for i:=qry_context.FStartIdx to High(FOBJArray) do
    begin
      obj := FOBJArray[i].CloneToNewObject();
      if assigned(iter) then { compare run does not need the objects }
        iter(obj);
      qry_context.SetResultObject(qry_context.FQueryCurrIdx,obj,compare_run);
      inc(qry_context.FQueryCurrIdx);
      inc(qry_context.FQueryDeliveredCount);
      if qry_context.FQueryDeliveredCount=qry_context.FToDeliverCount then
        break;
    end;
  qry_context.AddjustResultDBOLen(compare_run);
end;

procedure TFRE_DB_FilterContainer.CheckFilteredAdd(const obj: IFRE_DB_Object);
begin
  if FCnt=Length(FOBJArray) then
    SetLength(FOBJArray,Length(FOBJArray)+cFRE_INT_TUNE_SYSFILTEXTENSION_SZ);
  //Test Filter Code ...
  if DoesObjectPassFilterContainer(obj) then
    begin
      FOBJArray[FCnt] := obj;
      inc(FCnt);
    end;
end;

procedure TFRE_DB_FilterContainer.Notify_CheckFilteredUpdate(const td: TFRE_DB_TRANS_COLL_DATA; const old_obj, new_obj: IFRE_DB_Object; const order_changed: boolean);
var old_idx   : NativeInt;
    new_idx   : NativeInt;
    before    : NativeInt;
    pid       : string;
    fld       : IFRE_DB_Field;
    exists    : boolean;
begin
  if DoesObjectPassFilterContainer(new_obj) then
    begin { Update object does pass the filters}
      if order_changed then { order has potentially changed, key has changed / len value / but order may not have been changed in filtering }
        begin
          old_idx := Notify_CheckFilteredDelete(td,old_obj,false);
          new_idx := Notify_CheckFilteredInsert(td,new_obj,false);
          if old_idx=new_idx then
            begin
              if old_idx<>-1 then
                begin
                  GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH UPDATE OBJECT ORDER NOT CHANGED (WAS A POTENTIAL CHANGE) [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
                  G_TCDM.UpdateObjectInFilterKey(td,self,new_obj,old_idx);           { in place update }
                end;
            end
          else
            begin
              if old_idx<>-1 then
                begin
                  GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH UPDATE/REMOVE OBJECT ORDER HAS CHANGED [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
                  G_TCDM.RemoveUpdateObjectInFilterKey(td,self,old_obj,old_idx);     { remove an object }
                end;
              if new_idx<>-1 then
                begin
                  GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH UPDATE/INSERT OBJECT ORDER HAS CHANGED [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
                  if new_obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_PART,fld) then
                    pid := fld.AsString
                  else
                    pid := '';
                  G_TCDM.InsertUpdateObjectInFilterKey(td,self,new_obj,new_idx,pid);  { insert an object in filtering, now check queries }
                end;
            end;
        end
      else
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH UPDATE OBJECT ORDER NOT CHANGED [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
          old_idx := FREDB_BinaryFindIndexInSorted(old_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr,Forderkey,before,FOBJArray,exists);
          if exists=false then
            raise EFRE_DB_Exception.Create(edb_ERROR,'could not find the object notify_checkfiltered update');
          FOBJArray[old_idx] := new_obj;
          G_TCDM.UpdateObjectInFilterKey(td,self,new_obj,old_idx);  { in place update }
        end;
    end
  else
    begin { Update object does not pass the filters anymore -> remove it}
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER REJECT UPDATE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
    end;
end;

function TFRE_DB_FilterContainer.Notify_CheckFilteredDelete(const td: TFRE_DB_TRANS_COLL_DATA; const old_obj: IFRE_DB_Object; const do_qry_proc: boolean): NativeInt;

  procedure RemoveOldObject;
  var old_idx   : NativeInt;
      before    : NativeInt;
      old_key   : TFRE_DB_ByteArray;
      exists    : boolean;
  begin
    old_key  := old_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr;
    old_idx  := FREDB_BinaryFindIndexInSorted(old_key,ForderKey,before,FOBJArray,exists,old_obj.PUID);
    result   := old_idx;
    if exists=false then
      raise EFRE_DB_Exception.Create(edb_ERROR,'notify filtered delete / not found');
    FOBJArray := FREDB_RemoveIdxFomObjectArray(FOBJArray,old_idx);
    dec(FCnt);
    AdjustLength;
    if do_qry_proc then
      G_TCDM.RemoveUpdateObjectInFilterKey(td,self,old_obj,old_idx);  { remove an object }
  end;

begin
  result := -1;
  if DoesObjectPassFilterContainer(old_obj) then
    begin { old object is potentially in the client query present }
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH DELETE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
      RemoveOldObject;
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER REJECT DELETE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
    end;
end;


function TFRE_DB_FilterContainer.Notify_CheckFilteredInsert(const td: TFRE_DB_TRANS_COLL_DATA; const new_obj: IFRE_DB_Object; const do_qry_proc: boolean): NativeInt;
var new_key : TFRE_DB_ByteArray;

  procedure InsertNewObject;
  var ia_idx   : NativeInt;
      before   : NativeInt;
      exists   : boolean;
      fld      : IFRE_DB_FIELD;
      pid      : String;

  begin
    new_key  := new_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr;
    if Length(new_key)=0 then
      raise EFRE_DB_Exception.Create(edb_ERROR,'total order key / binary key not found in insert');
    ia_idx := FREDB_BinaryFindIndexInSorted(new_key,ForderKey,before,FOBJArray,exists);
    if exists then
      raise EFRE_DB_Exception.Create(edb_ERROR,'notify_checkfiltered insert -> exists');
    FOBJArray := FREDB_InsertAtIdxToObjectArray(FOBJArray,ia_idx,new_obj,before>0); { ia_idx gets adjusted }
    result := ia_idx;
    inc(FCnt);
    if new_obj.FieldOnlyExisting(cFRE_DB_SYS_PARENT_PATH_PART,fld) then
      pid := fld.AsString;
    if do_qry_proc then
      G_TCDM.InsertUpdateObjectInFilterKey(td,self,new_obj,ia_idx,pid);  { insert an object in filtering, now check queries }
  end;

begin
  result := -1;
  if DoesObjectPassFilterContainer(new_obj) then
    begin { old object is potentially in the client query present }
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH INSERT OBJECT [%s] IN ORDER/FILTER [%s / %s]',[new_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
      InsertNewObject;
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER REJECT INSERT OBJECT [%s] IN ORDERFILTER [%s / %s]',[new_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
    end;
end;

procedure TFRE_DB_FilterContainer.AdjustLength;
begin
  SetLength(FOBJArray,FCnt);
end;

constructor TFRE_DB_FilterContainer.Create(const order_key: TFRE_DB_NameType);
begin
  inherited Create;
  FCnt := 0;
  FFCCreationTime := GFRE_DT.Now_UTC;
  FFilters        := TFRE_DB_DC_FILTER_DEFINITION.Create;
  ForderKey       := order_key;
end;

destructor TFRE_DB_FilterContainer.Destroy;
begin
  FFilters.Free;
  inherited Destroy;
end;

function TFRE_DB_FilterContainer.Filters: TFRE_DB_DC_FILTER_DEFINITION;
begin
  result := FFilters;
end;


{ TFRE_DB_DC_FILTER_DEFINITION }

function TFRE_DB_DC_FILTER_DEFINITION.IsSealed: Boolean;
begin
  result := FFilterKey<>'';
end;

procedure TFRE_DB_DC_FILTER_DEFINITION._ForAllAdd(obj: TObject; arg: Pointer);
var clone  : boolean;
begin
  clone := NativeUint(arg)=1;
  AddFilter(obj as TFRE_DB_FILTER_BASE,clone);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION._ForAllKey(obj: TObject; arg: Pointer);
begin
  TStringList(arg).Add((obj as TFRE_DB_FILTER_BASE).GetDefinitionKey);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION._ForAllFilter(obj: TObject; arg: Pointer);
var tob : TFRE_DB_Object;
    flt : TFRE_DB_FILTER_BASE;
begin
  if FFilterMiss=true then
    exit;
  tob         := TFRE_DB_Object(arg);
  flt         := obj as TFRE_DB_FILTER_BASE;
  FFilterMiss := not flt.CheckFilterHit(tob,FFilterErr);
end;

constructor TFRE_DB_DC_FILTER_DEFINITION.Create;
begin
  inherited;
  FKeyList := TFPHashObjectList.Create(true);
end;

destructor TFRE_DB_DC_FILTER_DEFINITION.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddFilters(const source: TFRE_DB_DC_FILTER_DEFINITION_BASE; const clone: boolean);
var src : TFRE_DB_DC_FILTER_DEFINITION;
begin
  src := source as TFRE_DB_DC_FILTER_DEFINITION;
  if clone then
    src.FKeyList.ForEachCall(@_ForAllAdd,Pointer(1))
  else
    src.FKeyList.ForEachCall(@_ForAllAdd,Pointer(0));
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddFilter(const source: TFRE_DB_FILTER_BASE; const clone: boolean);
var filt : TFRE_DB_FILTER_BASE;
    key  : TFRE_DB_NameType;
begin
  key := source.GetKeyName;
  if FKeyList.FindIndexOf(key)<>-1 then
    begin
      try
        if not clone then
          source.Free;
      except
      end;
      raise EFRE_DB_Exception.Create(edb_ERROR,'FILTER WITH KEY ALREADY EXISTS IN THIS DEFINITION');
    end;
  if clone then
    filt := source.Clone
  else
    filt := source;
  FKeyList.Add(source.GetKeyName,filt)
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddStringFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalue: TFRE_DB_String; const stringfiltertype: TFRE_DB_STR_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_STRING;
begin
  filt := TFRE_DB_FILTER_STRING.Create(key);
  filt.InitFilter(fieldname,filtervalue,stringfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddSignedFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of Int64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_SIGNED;
begin
  filt := TFRE_DB_FILTER_SIGNED.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddUnsignedFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of Uint64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_UNSIGNED;
begin
  filt := TFRE_DB_FILTER_UNSIGNED.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddCurrencyFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of Currency; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_CURRENCY;
begin
  filt := TFRE_DB_FILTER_CURRENCY.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddReal64FieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of Double; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_REAL64;
begin
  filt := TFRE_DB_FILTER_REAL64.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddDatetimeFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of TFRE_DB_DateTime64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_DATETIME;
begin
  filt := TFRE_DB_FILTER_DATETIME.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddBooleanFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalue: boolean; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_BOOLEAN;
begin
  filt := TFRE_DB_FILTER_BOOLEAN.Create(key);
  if negate then
    filtervalue := not filtervalue;
  filt.InitFilter(fieldname,filtervalue,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddUIDFieldFilter(const key, fieldname: TFRE_DB_NameType; filtervalues: array of TFRE_DB_GUID; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_UID;
begin
  filt := TFRE_DB_FILTER_UID.Create(key);
  filt.InitFilter(fieldname,filtervalues,numfiltertype,negate,include_null_values);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddSchemeObjectFilter(const key: TFRE_DB_NameType; filtervalues: array of TFRE_DB_String; const negate: boolean);
var filt : TFRE_DB_FILTER_SCHEME;
begin
  filt := TFRE_DB_FILTER_SCHEME.Create(key);
  filt.InitFilter(filtervalues,negate);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddStdRightObjectFilter(const key: TFRE_DB_NameType; stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean);
var filt : TFRE_DB_FILTER_RIGHT;
begin
  filt := TFRE_DB_FILTER_RIGHT.Create(key);
  filt.InitFilter(stdrightset,usertoken,negate);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddChildFilter(const key: TFRE_DB_NameType);
var filt : TFRE_DB_FILTER_CHILD;
begin
  filt := TFRE_DB_FILTER_CHILD.Create(key);
  filt.InitFilter;
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddParentFilter(const key: TFRE_DB_NameType; const allowed_parent_path: TFRE_DB_GUIDArray);
var filt : TFRE_DB_FILTER_PARENT;
begin
  filt := TFRE_DB_FILTER_PARENT.Create(key);
  filt.InitFilter(allowed_parent_path);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddAutoDependencyFilter(const key: TFRE_DB_NameType; const RL_Spec: TFRE_DB_NameTypeRLArray; const StartDependecyValues: TFRE_DB_GUIDArray; const negate: boolean; const include_null_values: boolean; const dbname: TFRE_DB_NameType);
var filt : TFRE_DB_FILTER_AUTO_DEPENDENCY;
begin
  filt := TFRE_DB_FILTER_AUTO_DEPENDENCY.Create(key);
  filt.InitFilter(RL_Spec,StartDependecyValues,negate,include_null_values,dbname);
  AddFilter(filt,false);
end;

function TFRE_DB_DC_FILTER_DEFINITION.RemoveFilter(const key: TFRE_DB_NameType): boolean;
var idx : Integer;
begin
  idx    := FKeyList.FindIndexOf(key);
  result := idx<>-1;
  if result then
    FKeyList.Delete(idx);
end;

function TFRE_DB_DC_FILTER_DEFINITION.FilterExists(const key: TFRE_DB_NameType): boolean;
var idx : Integer;
begin
  idx    := FKeyList.FindIndexOf(key);
  result := idx<>-1;
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
var sl : TStringList;
begin
  MustNotBeSealed;
  FFilterKey:='';
  sl := TStringList.Create;
  try
    FKeyList.ForEachCall(@_ForAllKey,sl);
    sl.Sort;
    FFilterKey := sl.CommaText;
    FFilterKey := 'FK|'+GFRE_BT.HashFast32_Hex(FFilterKey);
  finally
    sl.free;
  end;
end;

function TFRE_DB_DC_FILTER_DEFINITION.GetFilterKey: TFRE_DB_TRANS_COLL_FILTER_KEY;
begin
  MustBeSealed;
  result := FFilterKey;
end;

function TFRE_DB_DC_FILTER_DEFINITION.DoesObjectPassFilters(const obj: IFRE_DB_Object): boolean;
begin
  FFilterMiss := false;
  FFilterErr  := 0;
  FKeyList.ForEachCall(@_ForAllFilter,obj.Implementor);
  result := not FFilterMiss;
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.CheckDBReevaluationFilters;
var i : NativeInt;
    f : TFRE_DB_FILTER_BASE;
begin
  for i := 0 to FKeyList.Count-1 do
    begin
      f := FKeyList.Items[i] as TFRE_DB_FILTER_BASE;
      if f.FilterNeedsDbUpdate then
        f.ReEvaluateFilter;
    end;
end;

function TFRE_DB_DC_FILTER_DEFINITION.CheckAutoDependencyFilter(const key_description: TFRE_DB_NameTypeRL): boolean;
var i    : NativeInt;
    f    : TFRE_DB_FILTER_BASE;
    need : boolean;
begin
 result := false;
  for i := 0 to FKeyList.Count-1 do
    begin
      f      := FKeyList.Items[i] as TFRE_DB_FILTER_BASE;
      need   := f.CheckReflinkUpdateEvent(key_description);
      result := result or need;
    end;
end;

{ TFRE_DB_DC_ORDER_DEFINITION }

function TFRE_DB_DC_ORDER_DEFINITION.IsSealed: Boolean;
begin
  result := FKey.key<>'';
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

procedure TFRE_DB_DC_ORDER_DEFINITION.SetDataKeyColl(const parent_collectionname, derivedcollname: TFRE_DB_NameType; const ParentChildspec: TFRE_DB_NameTypeRL);
begin
  FKey.Collname  := parent_collectionname;
  FKey.DC_Name   := derivedcollname;
  FKey.RL_Spec   := ParentChildspec;
  FKey.RL_SpecUC := ParentChildspec;
  if FKey.RL_Spec='' then
    FKey.RL_Spec := '#'
  else
    FKey.RL_Spec := GFRE_BT.HashFast32_Hex(ParentChildspec);
end;


function TFRE_DB_DC_ORDER_DEFINITION.GetFullKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if FKey.key='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'key definition is not done');
  result := Fkey;
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.ForAllOrders(const orderiterator: TFRE_DB_DC_ORDER_ITERATOR);
var order : TFRE_DB_DC_ORDER;
begin
  for order in FOrderList do
    orderiterator(order);
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.SetupBinaryKey(const obj: TFRE_DB_Object; const Key: PByteArray; var max_key_len: NativeInt; const tag_object: boolean);
var fld    : IFRE_DB_FIELD;
    KeyLen : NativeInt;

  procedure  Iter(const order : TFRE_DB_DC_ORDER);
  var fld    : IFRE_DB_Field;
      idx    : TFRE_DB_MM_IndexClass;
  begin
    if obj.FieldOnlyExistingI(order.order_field,fld) then
      begin
        if TFRE_DB_MM_Index.GetIndexClassForFieldtype(fld.FieldType,idx)=edb_UNSUPPORTED then
          begin
            TFRE_DB_TextIndex.TransformToBinaryComparable(nil,key[max_key_len],KeyLen,false,not order.ascending); { fallback transform the nil/unknown/unsupported fieldtype to a text null value }
          end
        else
          if idx=TFRE_DB_UnsignedIndex then
            TFRE_DB_UnsignedIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,Key[max_key_len],keylen,false,not order.ascending)
          else
          if idx=TFRE_DB_SignedIndex then
            TFRE_DB_SignedIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,Key[max_key_len],keylen,false,not order.ascending)
          else
          if idx=TFRE_DB_TextIndex then
            TFRE_DB_TextIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,Key[max_key_len],keylen,false,not order.ascending)
          else
          if idx=TFRE_DB_RealIndex then
            TFRE_DB_RealIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,Key[max_key_len],keylen,false,not order.ascending)
          else
            raise EFRE_DB_Exception.Create(edb_INTERNAL,' unknonw idx typed must be reported as unsupported ! idx='+idx.classname);
      end
    else
      begin
        TFRE_DB_TextIndex.TransformToBinaryComparable(nil,@key[max_key_len],KeyLen,false); { fallback transform the nil/unknown/unsupported fieldtype to a text null value }
      end;
    max_key_len := max_key_len+KeyLen;
  end;

  procedure TagObject;
  var byte_arr : TFRE_DB_ByteArray;
  begin
    FREDB_BinaryKey2ByteArray(key[0],max_key_len,byte_arr);
    obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(FKey.orderkey).AsByteArr := byte_arr;
  end;

begin
  max_key_len := 0;
  ForAllOrders(@iter);
  if tag_object then
    TagObject;
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
  key := '';
  for i := 0 to high(FOrderList) do
    with FOrderList[i] do
      key  := key +order_field+BoolToStr(ascending,'A','D');
  FKey.OrderKey := GFRE_BT.HashFast32_Hex(key);
  FKey.key      := Fkey.Collname+'/'+FKey.DC_Name+'/'+FKey.RL_Spec+'/'+FKey.OrderKey;
end;

function TFRE_DB_DC_ORDER_DEFINITION.GetBaseKeyPart: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if FKey.key='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'key definition is not done');
  result          := FKey; { copy whole key return requested part }
  result.key      := FKey.Collname+'/'+FKey.DC_Name+'/'+FKey.RL_Spec; { shorten string key }
  result.orderkey := '';
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

procedure TFRE_DB_OrderContainer.ReplaceObject(const old_obj, new_obj: TFRE_DB_Object);
var idx : NativeInt;
begin
  idx := FOBJArray.Exists(old_obj);
  if idx=-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'ordercontainer/replace old object not found');
  FOBJArray.Element[idx] := new_obj;
end;

function TFRE_DB_OrderContainer.RemoveObject(const old_obj: TFRE_DB_Object): boolean;
var idx : NativeInt;
begin
  if not FOBJArray.Delete(old_obj) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic / remove from ordercontainer / value not found');
  result := FOBJArray.Count=0;
end;

procedure TFRE_DB_OrderContainer.ForAllBreak(const iter: IFRE_DB_ObjectIteratorBrk; var halt: boolean);

  procedure MyIter(var obj : TFRE_DB_Object ; const idx:NativeInt ; var halt:boolean);
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

procedure TFRE_DB_QUERY.StartQueryRun(const compare_run: boolean);
begin
  FQueryRunning   := true;
  FQueryCurrIdx   := 0;
  FQueryStartTime := GFRE_BT.Get_Ticks_ms;
  if not compare_run then
    GFRE_DBI.LogDebug(dblc_QUERY,'QUERY ID [%s] start',[FQueryId])
  else
    GFRE_DBI.LogDebug(dblc_QUERY,'COMPARE/QUERY ID [%s] start',[FQueryId])
end;

procedure TFRE_DB_QUERY.EndQueryRun(const compare_run: boolean);
begin
  FQueryEndTime := GFRE_BT.Get_Ticks_ms;
  FQueryRunning := False;
  if not compare_run then
    GFRE_DBI.LogInfo(dblc_QUERY,'QUERY ID [%s] finished in %d ms',[FQueryId,FQueryEndTime-FQueryStartTime])
  else
    GFRE_DBI.LogInfo(dblc_QUERY,'COMPARE/QUERY ID [%s] finished in %d ms',[FQueryId,FQueryEndTime-FQueryStartTime]);
end;

function TFRE_DB_QUERY.GetReflinkSpec(const upper_refid: TFRE_DB_NameType): TFRE_DB_NameTypeRLArray;
var i:integer;
begin
  for i:=0 to high(FDependencyIds) do
    if FDependencyIds[i]=upper_refid then
      exit(FDepRefConstraints[i]);
  raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find a reflink spec for reference id '+upper_refid);
end;

function TFRE_DB_QUERY.GetReflinkStartValues(const upper_refid: TFRE_DB_NameType): TFRE_DB_GUIDArray;
var i:integer;
begin
  for i:=0 to high(FDependencyIds) do
    if FDependencyIds[i]=upper_refid then
      exit(FDependcyFilterUids[i]);
  raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find the filter values for reference id '+upper_refid);
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
  result.orderkey:='';
  result.filterkey:='';
end;

function TFRE_DB_QUERY.GetFullQueryOrderKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  if not assigned(FOrderDef) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no orderdef');
  FOrderDef.MustBeSealed;
  result           := FOrderDef.GetFullKey;
  result.filterkey := Filterdef.GetFilterKey;
end;

procedure TFRE_DB_QUERY.SetBaseOrderedData(const basedata: TFRE_DB_TRANS_RESULT_BASE ; const session_id : TFRE_DB_String);
begin
  FBaseData  := basedata as TFRE_DB_TRANS_COLL_DATA;
  FSessionId := session_id;
end;

procedure TFRE_DB_QUERY.SetMaxResultDBOLen(const compare_run: boolean);
begin
  if not compare_run then
    SetLength(FResultDBOs,FToDeliverCount)
  else
    SetLength(FResultDBOsCompare,FToDeliverCount);
end;

procedure TFRE_DB_QUERY.SetResultObject(const idx: NativeInt; const obj: IFRE_DB_Object; const compare_run: boolean);
begin
  if not compare_run then
    FResultDBOs[idx] := obj
  else
    FResultDBOsCompare[idx] := obj;
end;

procedure TFRE_DB_QUERY.AddjustResultDBOLen(const compare_run: boolean);
begin
  if not compare_run then
    SetLength(FResultDBOs,FQueryDeliveredCount)
  else
    SetLength(FResultDBOsCompare,FQueryDeliveredCount);
end;

function TFRE_DB_QUERY.ExecuteQuery(const iterator: IFRE_DB_Obj_Iterator): NativeInt;
begin
  if not assigned(FBaseData) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no base data available');
  StartQueryRun(false);
  FBaseData.Execute(iterator,self,false);
  EndQueryRun(false);
  result := FQueryPotentialCount;
end;

procedure TFRE_DB_QUERY.ProcessFilterChangeBasedUpdates;
var diff,d1,d2 : NativeInt;
begin
  if not assigned(FBaseData) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'compare run - no base data available');
  StartQueryRun(true);
  FBaseData.Execute(nil,self,true);
  EndQueryRun(true);
  d1 := Length(FResultDBOsCompare);
  d2 := Length(FResultDBOs);
  diff := d1 - d2;
  //result := FQueryPotentialCount;
end;

function TFRE_DB_QUERY.GetStoreID: TFRE_DB_NameType;
begin
  result := FOrderDef.FKey.DC_Name;
end;


{ TFRE_DB_TRANFORMED_DATA }

function TFRE_DB_TRANFORMED_DATA.IncludesChildData: Boolean;
begin
  result := FDC.HasParentChildRefRelationDefined;
end;

function TFRE_DB_TRANFORMED_DATA.HasReflinksInTransform: Boolean;
begin
  result := FDC.HasReflinksInTransformation;
end;

procedure TFRE_DB_TRANFORMED_DATA.Cleanup;
var cnt : NativeInt;
begin
  FTransformedData.Clear;
  //for cnt :=0 to High(FTransformeddata) do
  //  FTransformeddata[cnt].Finalize;
  //Setlength(FTransformeddata,0);
end;

procedure TFRE_DB_TRANFORMED_DATA.UpdateObjectByNotify(const obj: IFRE_DB_Object);
var idx  : NativeInt;
    tupo : TFRE_DB_Object;
    ppf  : string;
begin
  idx := ExistsObj(obj.UID); { Check if the Object Exists in the current Transformed Data}
  if idx>=0 then
    begin
      tupo := FTransformedData.Items[idx] as TFRE_DB_Object;
      ppf  := tupo.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsString;
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >NOTIFY UPDATE OBJECT [%s] AGAINST TRANSDATA [%s] PARENTPATH [%s]',[obj.GetDescriptionID,FKey.key,ppf]);
      FDC.TransformSingleUpdate(obj.CloneToNewObject(),self,FChildDataIsLazy,idx,ppf);
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >NOTIFY SKIP UPDATE OBJECT [%s] AGAINST TRANSDATA [%s] PARENTPATH [%s] OBJECT NOT FOUND',[obj.GetDescriptionID,FKey.key,ppf]);
      { not needed - dont clone }
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.InsertObjectByNotify(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object; const rl_ins: boolean; const parent: TFRE_DB_GUID);
var idx : NativeInt;
    par : IFRE_DB_Object;
    ppf : TFRE_DB_String;
begin
  if rl_ins or (uppercase(coll_name) =  FKey.Collname) then
    begin
      if rl_ins then { Reflink update (tree), fetch parent,  set parentpath}
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,' >NOTIFY INSERT OBJECT [%s] AGAINST TRANSDATA [%s] COLL [%s] IS A REFLINK UPDATE',[obj.GetDescriptionID,FKey.key,coll_name]);
          idx := ExistsObj(parent);
          par := FTransformedData.Items[idx] as TFRE_DB_Object;
          ppf := par.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsString;
          if ppf='' then
            ppf := par.UID_String
          else
            ppf := ppf+','+par.UID_String;
          FDC.TransformSingleInsert(obj.CloneToNewObject(),self,FChildDataIsLazy,true,ppf); { Frees the object }
        end
      else
        begin { root insert }
          GFRE_DBI.LogDebug(dblc_DBTDM,' >NOTIFY INSERT OBJECT [%s] AGAINST TRANSDATA [%s] COLL [%s] IS A COLLECTION UPDATE',[obj.GetDescriptionID,FKey.key,coll_name]);
          idx := ExistsObj(obj.UID);
          if idx>=0 then
            raise EFRE_DB_Exception.Create(edb_ERROR,'td updateobjectbynotify failed - not found')
          else
            FDC.TransformSingleInsert(obj.CloneToNewObject(),self,FChildDataIsLazy,false,''); { Frees the object }
        end;
    end
  else
    begin { skip,  dont clone }
      {
        not needed, this is no direct collection update
        RL Updates are processed with the RL Events
      }
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.RemoveObjectByNotify(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object; const rl_rem: boolean; const parent: TFRE_DB_GUID);
var idx : NativeInt;
begin
  if rl_rem or (uppercase(coll_name) =  FKey.Collname) then
    begin
      idx := ExistsObj(obj.UID);
      if idx>=0 then
        HandleDeleteTransformedObject(idx)
      else
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'td removeobjectbynotify failed - not found');
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.SetupOutboundRefLink(const from_obj: TGUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL);
var idx : NativeInt;
begin
  if IncludesChildData and
     (FKey.RL_SpecUC=key_description) then
       begin
         idx := ExistsObj(to_obj.UID);
         if idx>=0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td setupoutbound rl - exists')
         else
           InsertObjectByNotify('',to_obj,true,from_obj)
       end;
  if HasReflinksInTransform and
      true then
        begin
          abort; { handle that right}
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.SetupInboundRefLink(const from_obj: IFRE_DB_Object; const to_obj: TGUID; const key_description: TFRE_DB_NameTypeRL);
var idx : NativeInt;
begin
  if IncludesChildData and
     (FKey.RL_SpecUC=key_description) then
       begin
         idx := ExistsObj(from_obj.UID);
         if idx>=0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td setupinbound rl - exists')
         else
           InsertObjectByNotify('',from_obj,true,to_obj)
       end;
  if HasReflinksInTransform and
      true then
        begin
          abort; { handle that right}
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.InboundReflinkDropped(const from_obj: IFRE_DB_Object; const to_obj: TGUID; const key_description: TFRE_DB_NameTypeRL);
var idx : NativeInt;

    function DependcyMatchesKey : boolean;
    begin
      FDC.IsDependencyFilteredCollection;
    end;

begin
  if IncludesChildData and
     (FKey.RL_SpecUC=key_description) then
       begin
         idx := ExistsObj(from_obj.UID);
         if idx<0 then
           raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'td inboundrldropped - not found')
         else
           RemoveObjectByNotify('',from_obj,true,to_obj)
       end;
  if HasReflinksInTransform and
      true then
        begin
          abort; { handle that right}
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.OutboundReflinkDropped(const from_obj: TGUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL);
var idx : NativeInt;
begin
  if IncludesChildData and
     (FKey.RL_SpecUC=key_description) then
       begin
         idx := ExistsObj(to_obj.UID);
         if idx<0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td outboundrldropped - not found')
         else
           RemoveObjectByNotify('',to_obj,true,from_obj)
       end;
  if HasReflinksInTransform and
      true then
        begin
          abort; { handle that right}
        end;
end;

function TFRE_DB_TRANFORMED_DATA.ExistsObj(const uid: TFRE_DB_GUID): NativeInt;
var us : ShortString;
    ob : TFRE_DB_Object;
begin
  us := FREDB_G2SB(UID);
  result := FTransformedData.FindIndexOf(us);
  if result>=0 then
    begin
      ob := FTransformedData.Items[result] as TFRE_DB_Object;
      if ob.UID<>FREDB_SB2G(us) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'uid mismatch / hash collsion ?');
    end;
end;


procedure TFRE_DB_TRANFORMED_DATA.SetTransformedObject(const tr_obj: IFRE_DB_Object);
var us : ShortString;
    ob : TFRE_DB_Object;
begin
  us := FREDB_G2SB(tr_obj.UID);
  ob := FTransformedData.Find(us) as TFRE_DB_Object;
  if assigned(ob) then
    if ob.UID=tr_obj.UID then
      raise EFRE_DB_Exception.Create(edb_ERROR,'double add / transformed data')
    else
      raise EFRE_DB_Exception.Create(edb_ERROR,'hash collision / double add / transformed data');
  FTransformedData.Add(us,tr_obj.Implementor);
end;

procedure TFRE_DB_TRANFORMED_DATA.SetTransObjectSingleInsert(const tr_obj: IFRE_DB_Object);
begin
  SetTransformedObject(tr_obj); { currently no need to diversificate }
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleUpdateTransformedObject(const tr_obj: IFRE_DB_Object ; const upd_idx : NativeInt);
var old_o : TFRE_DB_Object;
    i     : NativeInt;
    od    : TFRE_DB_TRANS_COLL_DATA;
begin
  GFRE_DBI.LogDebug(dblc_DBTDM,'   TRANSFORMED OBJECT UPDATE [%s] AGAINST ORDERING [%s]',[tr_obj.UID_String,FKey.key]);
  old_o := FTransformeddata.Items[upd_idx] as TFRE_DB_Object;
  try
    FTransformedData.Delete(upd_idx);                           { all orderings now point to the dangling removed object}
    SetTransformedObject(tr_obj);                               { new object in hashlist }
    for i := 0 to FOrderings.Count-1 do
      begin
        od := FOrderings.Items[i] as TFRE_DB_TRANS_COLL_DATA;
        GFRE_DBI.LogDebug(dblc_DBTDM,'   >MATCH UPDATE OBJECT [%s] IN ORDERING [%s]',[tr_obj.UID_String,od.FOrderDef.GetFullKey.key]);
        od.UpdateTransformedobject(old_o,tr_obj); { propagate old object up}
      end;
  finally
    old_o.Finalize;
  end;
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleInsertTransformedObject(const tr_obj: IFRE_DB_Object);
var  i : NativeInt;
    od : TFRE_DB_TRANS_COLL_DATA;
begin
  GFRE_DBI.LogDebug(dblc_DBTDM,'   TRANSFORMED OBJECT INSERT [%s] AGAINST ORDERING [%s]',[tr_obj.UID_String,FKey.key]);
  SetTransObjectSingleInsert(tr_obj);                                                 { new object in hashlist }
  for i := 0 to FOrderings.Count-1 do
    begin
      od := FOrderings.Items[i] as TFRE_DB_TRANS_COLL_DATA;
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >MATCH INSERT OBJECT [%s] IN ORDERING [%s]',[tr_obj.UID_String,od.FOrderDef.GetFullKey.key]);
      od.InsertTransformedobject(tr_obj); { propagate new object up}
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleDeleteTransformedObject(const del_idx: NativeInt);
var del_o : TFRE_DB_Object;
    i     : NativeInt;
begin
  del_o := FTransformeddata.Items[del_idx] as TFRE_DB_Object;
  try
    FTransformedData.Delete(del_idx);                           { all orderings now point to the dangling removed object}
    for i := 0 to FOrderings.Count-1 do
      (FOrderings.Items[i] as TFRE_DB_TRANS_COLL_DATA).DeleteTransformedobject(del_o); { propagate old object up}
  finally
    del_o.Finalize;
  end;
end;

function TFRE_DB_TRANFORMED_DATA.GetTransFormKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FKey;
end;

procedure TFRE_DB_TRANFORMED_DATA.TransformAll(var rcnt: NativeInt);
begin
  FDC.TransformAllTo(self,FChildDataIsLazy,rcnt);
end;


constructor TFRE_DB_TRANFORMED_DATA.Create(const key: TFRE_DB_TRANS_COLL_DATA_KEY; const dc: IFRE_DB_DERIVED_COLLECTION);
begin
  FKey                    := key;
  Fkey.orderkey           := ''; { this is unordered, the orderkey comes from the frist query generating the data }
  FChildDataIsLazy        := True;
  FDC                     := dc;
  FTDCreationTime         := GFRE_DT.Now_UTC;
  FTransformedData        := TFPHashObjectList.Create(false);
  FOrderings              := TFPObjectList.Create(false);
end;

destructor TFRE_DB_TRANFORMED_DATA.Destroy;
begin
  FOrderings.Clear;
  inherited Destroy;
end;

procedure TFRE_DB_TRANFORMED_DATA.ForAllObjs(const forall: TFRE_DB_Obj_Iterator);
var i : NativeInt;
begin
  for i:=0 to FTransformedData.Count-1 do
    forall(FTransformedData.Items[i] as TFRE_DB_Object);
end;

procedure TFRE_DB_TRANFORMED_DATA.AddOrdering(const ordering: TFRE_DB_TRANS_COLL_DATA);
begin
  if FOrderings.IndexOf(ordering)<>-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'double add ordering');
  FOrderings.Add(ordering);
end;

procedure TFRE_DB_TRANFORMED_DATA.RemoveOrdering(const ordering: TFRE_DB_TRANS_COLL_DATA);
var idx : NativeInt;
begin
  idx := FOrderings.IndexOf(ordering);
  if idx=-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'ordering not found on remove');
  FOrderings.Delete(idx);
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

procedure TFRE_DB_TRANSDATA_MANAGER.ForAllQueries(const query_iter: TFRE_DB_QueryIterator);
  procedure Iterate(var qptr : PtrUInt);
  begin
    query_iter(FREDB_PtrUIntToObject(qptr) as TFRE_DB_QUERY);
  end;
begin
  FArtQueryStore.LinearScan(@Iterate);
end;

function TFRE_DB_TRANSDATA_MANAGER.GetBaseTransformedData(base_key: TFRE_DB_NameTypeRL; out base_data: TFRE_DB_TRANFORMED_DATA): boolean;
var fnd : boolean;
  procedure Search(const bd : TFRE_DB_TRANFORMED_DATA ; var halt :boolean);
  begin
    if bd.GetTransFormKey.key = base_key then
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

procedure TFRE_DB_TRANSDATA_MANAGER.TL_StatsTimer;
begin
  LockManager;
  try
    exit;
    writeln('GDBTM Stats:  QueryCount ',FArtQueryStore.GetValueCount,' Transforms: ',FTransList.Count,' Orders: ',FOrders.Count);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.StartNotificationBlock(const key: TFRE_DB_TransStepId);
begin
  if assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic/a notify block is currently assigned');
  FCurrentNotify := TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.Create;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FinishNotificationBlock(out block: IFRE_DB_Object);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic/no notify block is currently assigned');
  FCurrentNotify.NotifyAll;
  FreeAndNil(FCurrentNotify);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SendNotificationBlock(const block: IFRE_DB_Object);
begin
  abort;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CollectionCreated(const coll_name: TFRE_DB_NameType);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CollectionDeleted(const coll_name: TFRE_DB_NameType);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.IndexDefinedOnField(const coll_name: TFRE_DB_NameType; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.IndexDroppedOnField(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectStored(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.InsertObjectByNotify(coll_name,obj.CloneToNewObject(),false,CFRE_DB_NullGUID);
  end;

begin
  if coll_name='' then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'collname not set on ObjectStored Notification');
  LockManager;
  try
    GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT STORED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END  OBJECT STORED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectDeleted(const obj: IFRE_DB_Object);
begin
  { trigger on removed, not deleted }
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectUpdated(const obj: IFRE_DB_Object; const colls: TFRE_DB_StringArray);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.UpdateObjectByNotify(obj);
  end;

begin
  LockManager;
  try
    GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT UPDATED [%s] in Collections [%s]',[obj.GetDescriptionID,FREDB_CombineString(colls,',')]);
    FCurrentNotify.AddDirectSessionUpdateEntry(obj.CloneToNewObject);
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END   OBJECT UPDATED [%s] in Collections [%s]',[obj.GetDescriptionID,FREDB_CombineString(colls,',')]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SubObjectStored(const obj: IFRE_DB_Object; const parent_field_name: TFRE_DB_NameType; const ParentObjectUIDPath: TFRE_DB_GUIDArray);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SubObjectDeleted(const obj: IFRE_DB_Object; const parent_field_name: TFRE_DB_NameType; const ParentObjectUIDPath: TFRE_DB_GUIDArray);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.DifferentiallUpdStarts(const obj_uid: IFRE_DB_Object);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldDelete(const old_field: IFRE_DB_Field);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldAdd(const new_field: IFRE_DB_Field);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldChange(const old_field, new_field: IFRE_DB_Field);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.DifferentiallUpdEnds(const obj_uid: TFRE_DB_GUID);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectRemoved(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.RemoveObjectByNotify(coll_name,obj,false,CFRE_DB_NullGUID);
  end;

begin
  LockManager;
  try
    GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT REMOVED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END   OBJECT REMOVED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SetupOutboundRefLink(const from_obj: TGUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.SetupOutboundReflink(from_obj,to_obj.CloneToNewObject,key_description);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.GetFilterDefinition.CheckAutoDependencyFilter(key_description) then
        qry.ProcessFilterChangeBasedUpdates;
  end;

begin
  LockManager;
  try
    FTransList.ForAll(@CheckIfNeeded);      { search all base transforms for needed updates ... }
    ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SetupInboundRefLink(const from_obj: IFRE_DB_Object; const to_obj: TGUID; const key_description: TFRE_DB_NameTypeRL);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.SetupInboundRefLink(from_obj.CloneToNewObject,to_obj,key_description);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.GetFilterDefinition.CheckAutoDependencyFilter(key_description) then
        qry.ProcessFilterChangeBasedUpdates;
  end;


begin
  LockManager;
  try
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.InboundReflinkDropped(const from_obj: IFRE_DB_Object; const to_obj: TGUID; const key_description: TFRE_DB_NameTypeRL);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.InboundReflinkDropped(from_obj.CloneToNewObject,to_obj,key_description);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.GetFilterDefinition.CheckAutoDependencyFilter(key_description) then
        qry.ProcessFilterChangeBasedUpdates;
  end;


begin
  LockManager;
  try
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.OutboundReflinkDropped(const from_obj: TGUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.OutboundReflinkDropped(from_obj,to_obj.CloneToNewObject,key_description);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.GetFilterDefinition.CheckAutoDependencyFilter(key_description) then
        qry.ProcessFilterChangeBasedUpdates;
  end;


begin
  LockManager;
  try
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FinalizeNotif;
begin

end;

function TFRE_DB_TRANSDATA_MANAGER.DBC(const dblname: TFRE_DB_NameType): IFRE_DB_CONNECTION;
begin
  CheckDbResult(GFRE_DB.NetServer.GetDBWithServerRights(dblname,result),'TCDM - could not fetch admin dbc '+dblname);
end;

constructor TFRE_DB_TRANSDATA_MANAGER.Create;
begin
  GFRE_TF.Get_Lock(FTransLock,true);
  FOrders.Init(10);
  FTransList.Init(10);
  FArtQueryStore := TFRE_ART_TREE.Create;
  FStatCleaner   := TFRE_DB_TDM_STATS_CLEANER.Create(self);
end;

destructor TFRE_DB_TRANSDATA_MANAGER.Destroy;
begin
  FStatCleaner.Free;
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
var fkd : shortstring;
    fnd : boolean;

  procedure Search(const tcd : TFRE_DB_TRANS_COLL_DATA ; var halt : boolean);
  begin
    if tcd.FOrderDef.GetFullKey.key=fkd then
      begin
        halt := true;
        cd   := tcd;
        tcd.LockBase;
      end;
  end;

begin
  LockManager;
  fnd    := false;
  fkd    := TFRE_DB_QUERY(qry).Orderdef.GetFullKey.key;
  result := FOrders.ForAllBreak(@Search,fnd);
  result := fnd;
  GFRE_DBI.LogDebug(dblc_DBTDM,'>GET ORDERING FOR TRANSFORMED DATA FOR [%s] %s',[fkd,BoolToStr(fnd,'FOUND','NOT FOUND')]);
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
      if not GetBaseTransformedData(basekey.key,transdata) then      { 1st search for Transformeddata }
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,'>BASE TRANSFORMING DATA FOR [%s]',[basekey.key]);
          st        := GFRE_BT.Get_Ticks_ms;
          inc(FTransformKey);
          transdata := TFRE_DB_TRANFORMED_DATA.Create(basekey,dc);  { the transform data is identified by the basekey }
          transdata.TransFormAll(rcnt);
          AddBaseTransformedData(transdata);
          et        := GFRE_BT.Get_Ticks_ms;
          GFRE_DBI.LogInfo(dblc_DBTDM,'<BASE TRANSFORMING DATA FOR [%s] DONE - Transformed %d records in %d ms',[basekey.key,rcnt,et-st]);
        end;
      cd := TFRE_DB_TRANS_COLL_DATA.Create(FOrderDef,transdata); { generate the ordered, transformed data (next layer) }
      FOrders.Add2Array(TFRE_DB_TRANS_COLL_DATA(cd));            { internal add the data }
      TFRE_DB_TRANS_COLL_DATA(cd).OrderTheData;                  { order it }
      TFRE_DB_TRANS_COLL_DATA(cd).LockBase;                      { return the locked result }
    end;
end;

function TFRE_DB_TRANSDATA_MANAGER.GenerateQueryFromRawInput(const input: IFRE_DB_Object; const dependecy_reference_id: TFRE_DB_StringArray; const dependency_reference_constraint: TFRE_DB_NameTypeRLArrayArray; const dependency_negate: boolean; const parent_child_spec: TFRE_DB_NameTypeRL; const parent_child_skip_schemes: TFRE_DB_NameTypeRLArray; const dc_name, parent_name: TFRE_DB_NameTypeRL; const dc_static_filters: TFRE_DB_DC_FILTER_DEFINITION_BASE; const DefaultOrderField: TFRE_DB_NameType; DefaultOrderAsc: Boolean; const session: IFRE_DB_UserSession): TFRE_DB_QUERY_BASE;
var fld : IFRE_DB_FIELD;
      i : NativeInt;
    qry : TFRE_DB_QUERY;

   procedure ProcessDependencies;
   var fld   : IFRE_DB_FIELD;
         i,j : NativeInt;
   begin
     SetLength(qry.FDependencyIds,Length(dependecy_reference_id));
     for i:=0 to high(qry.FDependencyIds) do
      qry.FDependencyIds[i] := uppercase(dependecy_reference_id[i]);
     SetLength(qry.FDependcyFilterUids,Length(dependecy_reference_id));
     qry.FDepRefConstraints := dependency_reference_constraint;
     for i:=0 to high(dependecy_reference_id) do
       begin
         fld := input.FieldPath('DEPENDENCY.'+dependecy_reference_id[i]+'_REF.FILTERVALUES',true);
         if assigned(fld) then
           with qry do
             begin
               SetLength(FDependcyFilterUids[i],fld.ValueCount);
               for j := 0 to High(FDependcyFilterUids) do
                 FDependcyFilterUids[i][j] := FREDB_H2G(fld.AsStringArr[j]);
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
     qry.Orderdef.SetDataKeyColl(parent_name,dc_name,qry.FParentChildLinkFldSpec);
     qry.OrderDef.Seal;
   end;

   procedure ProcessCheckChildQuery;
   begin
     with qry do begin
       FParentChildLinkFldSpec := parent_child_spec;         { comes from dc }
       FParentChildSkipschemes := parent_child_skip_schemes; { comes from dc }
       if input.FieldExists('parentid') then
         begin { this is a child query }
           FParentIds := FREDB_String2GuidArray(input.Field('parentid').AsString);
           //if Length(FParentIds)>0 then
           //  begin
           //    FParentIds[0] := FParentIds[high(FParentIds)];
           //    SetLength(FParentIds,1);
           //  end;
           Filterdef.AddParentFilter('*SPCF*',FParentIds); { add a parent field filter }
         end
       else
         Filterdef.AddChildFilter('*SPCF*'); { add a child filter }
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
   var dop : IFRE_DB_Object;

       procedure AddFilter(const filter_key : TFRE_DB_NameType ; const filterdef : IFRE_DB_Object);
       var ftstr       : shortstring;
           ft          : TFRE_DB_FILTERTYPE;
           ftfrom_vals : TFRE_DB_FILTERTYPE;
           ffn         : TFRE_DB_NameType;
           sft         : TFRE_DB_STR_FILTERTYPE;
           fld         : IFRE_DB_FIELD;
           fallownull  : boolean;
           fnegate     : boolean;
           nft         : TFRE_DB_NUM_FILTERTYPE;
           nfts        : TFRE_DB_String;


           procedure ProcessDateTimeFilter;
           var ffld : IFRE_DB_Field;
               i    : NativeInt;
               vals : TFRE_DB_DateTimeArray;
           begin {}
             ffld :=  filterdef.field('FILTERVALUES');
             vals := ffld.AsInt64Arr;
             case ftfrom_vals of
               dbf_SIGNED: ;
               else
                 raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested datetime field filter must use signed filter values');
             end;
             qry.Filterdef.AddDatetimeFieldFilter(filter_key,ffn,vals,nft,fnegate,fallownull);
           end;

           function _C2S : TFRE_DB_Int64Array;inline;
           begin
             result := filterdef.field('FILTERVALUES').ConvAsSignedArray;
           end;

           function _C2US : TFRE_DB_UInt64Array;inline;
           begin
             result := filterdef.field('FILTERVALUES').ConvAsUnsignedArray;
           end;

           function _C2CURR : TFRE_DB_CurrencyArray;
           begin
             result := filterdef.field('FILTERVALUES').ConvAsCurrencyArray;
           end;

           function _C2REAL : TFRE_DB_Real64Array;
           begin
             result := filterdef.field('FILTERVALUES').ConvAsReal64Array;
           end;

           procedure ProcessUidFilter;
           var vals         : TFRE_DB_GUIDArray;
               sa           : TFRE_DB_StringArray;
               i            : NativeInt;
               refl_filter  : boolean;
               refl_spec    : TFRE_DB_NameTypeRLArray;
               refl_vals    : TFRE_DB_GUIDArray;
               expanded_uid : TFRE_DB_GUIDArray;
               //dep_f_param  : TFRE_DB_DependencyFilterParameter;
           begin
             if FREDB_StringInArray(uppercase(ffn),qry.FDependencyIds) then
               begin { dependency ref UID Filter}
                 refl_spec := qry.GetReflinkSpec(ffn);
                 refl_vals := qry.GetReflinkStartValues(ffn);
                 session.GetDBConnection.ExpandReferences(refl_vals,refl_spec,expanded_uid);
                 if not dependency_negate then  here - make auto dependency filter working ! like no value}
                   qry.Filterdef.AddUIDFieldFilter(filter_key,'UID',expanded_uid,dbnf_OneValueFromFilter,dependency_negate,fallownull)
                 else
                   qry.Filterdef.AddUIDFieldFilter(filter_key,'UID',expanded_uid,dbnf_NoValueInFilter,dependency_negate,fallownull);
                 //qry.Filterdef.AddAutoDependencyFilter(filter_key,refl_spec,refl_vals,dependency_negate,fallownull,session.GetDBConnection.GetDatabaseName);
               end
             else
               begin { "normal" UID Filter}
                 raise EFRE_DB_Exception.Create(edb_internal,'uid filter type not implemented ');
                 //if nfts='' then
                 //  nft := dbnf_OneValueFromFilter;
                 //sa  := filterdef.field('FILTERVALUES').AsStringArr;
                 //SetLength(vals,Length(sa));
                 //for i := 0 to high(vals) do
                 //  vals[i] := FREDB_H2G(sa[i]);
                 //refl_filter := Length(qry.FDepRefConstraints)>0;
                 //if refl_filter then
                 //
                 //qry.Filterdef.AddUIDFieldFilter(filter_key,ffn,vals,nft,fnegate,fallownull);
               end
           end;

       //var tst : string;
       begin
         //writeln('FILTER : ',filter_key);
         //tst  := filterdef.DumpToString;
         //writeln(filterdef.DumpToString);

         nfts := filterdef.field('NUMFILTERTYPE').AsString;
         if nfts<>'' then
           nft := FREDB_String2NumfilterType(nfts)
         else
           nft := dbnf_EXACT;

         ft         := FREDB_FilterTypeString2Filtertype(filterdef.Field('FILTERTYPE').AsString);
         ffn        := uppercase(filterdef.Field('FILTERFIELDNAME').AsString);
         case filterdef.field('FILTERVALUES').FieldType of { value type does not propagate exact through json->dbo}
           fdbft_Byte:        ftfrom_vals := dbf_SIGNED;
           fdbft_Int16:       ftfrom_vals := dbf_SIGNED;
           fdbft_UInt16:      ftfrom_vals := dbf_SIGNED;
           fdbft_Int32:       ftfrom_vals := dbf_SIGNED;
           fdbft_UInt32:      ftfrom_vals := dbf_SIGNED;
           fdbft_Int64:       ftfrom_vals := dbf_SIGNED;
           fdbft_UInt64:      ftfrom_vals := dbf_UNSIGNED;
           fdbft_Real32:      ftfrom_vals := dbf_REAL64;
           fdbft_Real64:      ftfrom_vals := dbf_REAL64;
           fdbft_Currency:    ftfrom_vals := dbf_CURRENCY;
           fdbft_String:      ftfrom_vals := dbf_TEXT;
           fdbft_Boolean:     ftfrom_vals := dbf_BOOLEAN;
           fdbft_DateTimeUTC: ftfrom_vals := dbf_DATETIME;
           fdbft_GUID:        ftfrom_vals := dbf_UNSIGNED;
           fdbft_NotFound:    ftfrom_vals := dbf_EMPTY;
           else
             raise EFRE_DB_Exception.Create(edb_ERROR,'cannot determine filtertype from input filtervalues fieldtype=%s',[CFRE_DB_FIELDTYPE[filterdef.field('FILTERVALUES').FieldType]]);
         end;
         if filterdef.FieldOnlyExisting('ALLOWNULL',fld) then
           fallownull := fld.AsBoolean
         else
           fallownull := false;
         if filterdef.FieldOnlyExisting('NEG',fld) then
           fnegate    := fld.AsBoolean
         else
           fnegate    := false;
         case ft of
           dbf_TEXT:
             begin
               if ftfrom_vals<>dbf_TEXT then
                 raise EFRE_DB_Exception.Create(edb_MISMATCH,'a string field filter must use string filter values');
               ftstr := filterdef.field('STRFILTERTYPE').AsString;
               if ftstr<>'' then
                 sft := FREDB_String2StrFilterType(filterdef.field('STRFILTERTYPE').AsString)
               else
                 sft := dbft_PART;
               qry.Filterdef.AddStringFieldFilter(filter_key,ffn,filterdef.field('FILTERVALUES').AsStringArr[0],sft,fallownull);
             end;
           dbf_SIGNED:
             begin
               case ftfrom_vals of
                 dbf_SIGNED: qry.Filterdef.AddSignedFieldFilter(filter_key,ffn,_C2S,FREDB_String2NumfilterType(filterdef.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
                 dbf_REAL64: qry.Filterdef.AddReal64FieldFilter(filter_key,ffn,_C2REAL,FREDB_String2NumfilterType(filterdef.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
                 else
                   raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested signed field filter must use signed or real64 filter values');
               end;
             end;
           dbf_UNSIGNED:
               raise EFRE_DB_Exception.Create(edb_INTERNAL,'a web requested unsigned field filter is not implemented');
           dbf_CURRENCY:
             begin
               case ftfrom_vals of
                 dbf_SIGNED,
                 dbf_CURRENCY,
                 dbf_REAL64 : ;
                 else
                   raise EFRE_DB_Exception.Create(edb_MISMATCH,'a web requested currency field filter must use signed or real64 filter values');
               end;
               qry.Filterdef.AddCurrencyFieldFilter(filter_key,ffn,_C2CURR,FREDB_String2NumfilterType(filterdef.field('NUMFILTERTYPE').AsString),fnegate,fallownull);
             end;
           dbf_DATETIME:  ProcessDateTimeFilter;
           dbf_BOOLEAN:   qry.Filterdef.AddBooleanFieldFilter(filter_key,ffn,filterdef.field('FILTERVALUES').AsBoolean,fnegate,fallownull);
           dbf_GUID:      ProcessUIDFilter;
           dbf_SCHEME:    qry.Filterdef.AddSchemeObjectFilter(filter_key,filterdef.field('FILTERVALUES').AsStringArr,fnegate);
           dbf_RIGHT:     qry.Filterdef.AddStdRightObjectFilter(filter_key,FREDB_RightSetString2RightSet(filterdef.field('FILTERVALUES').AsString),session.GetDBConnection.SYS.GetCurrentUserToken,fnegate);
           else raise EFRE_DB_Exception.Create(edb_ERROR,'unhandled filter type');
         end;
       end;

   begin
     qry.Filterdef.AddStdRightObjectFilter('*SRF*',[sr_FETCH],session.GetDBConnection.SYS.GetCurrentUserToken,false);
     qry.Filterdef.AddFilters(dc_static_filters,true);
     if input.FieldOnlyExistingObject('DEPENDENCY',dop) then
       begin
         dop.ForAllObjectsFieldName(@AddFilter);
       end;
     qry.Filterdef.Seal;
   end;

   procedure SetQueryID;
   begin
     qry.FQueryClientID := input.Field('QUERYID').AsString;
     qry.FQueryId       := FormQueryID(session,dc_name,qry.FQueryClientID);
     qry.FQueryDescr    := Format('QRY(%s) DC(%s) CLID(%s)',[qry.FQueryId,dc_name,qry.FQueryClientID]);
   end;

begin
  //writeln('QUERY_DEF');
  //writeln('DEPENDENCY REFERENCE : ',FREDB_CombineString(dependecy_reference_id,','));
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

procedure TFRE_DB_TRANSDATA_MANAGER.ApplyInboundNotificationBlock(const dbname: TFRE_DB_NameType; const block: IFRE_DB_Object);
var dummy : IFRE_DB_Object;
begin
  self.StartNotificationBlock(Block.Field('KEY').AsString);
  try
    FREDB_ApplyNotificationBlockToNotifIF(block,self,FCurrentNLayer);
  finally
    self.FinishNotificationBlock(dummy);
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.InboundNotificationBlock(const dbname: TFRE_DB_NameType; const block: IFRE_DB_Object);
var bl : TFRE_DB_INBOUND_N_BLOCK;
begin
  bl        := TFRE_DB_INBOUND_N_BLOCK.Create;
  bl.FBlock := block.CloneToNewObject(); { TODO REMOVE CLONEING ONLY FOR TEST}
  FStatCleaner.FQ.Push(bl);
  FStatCleaner.FStatEvent.SetEvent;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddDirectSessionUpdateEntry(const update_dbo: IFRE_DB_Object);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / direct session update entry');
  FCurrentNotify.AddDirectSessionUpdateEntry(update_dbo);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInplaceUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const upo: IFRE_DB_Object);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid inplace update');
  FCurrentNotify.AddGridInplaceUpdate(sessionid,store_id,qry_id,upo);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInplaceDelete(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType ; const del_id: TFRE_DB_String);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid delete');
  FCurrentNotify.AddGridRemoveUpdate(sessionid,store_id,qry_id,del_id);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInsertUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: TFRE_DB_NameType; const upo: IFRE_DB_Object; const parent_id , reference_id: TFRE_DB_String ; const before: boolean);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid insert update');
  FCurrentNotify.AddGridInsertUpdate(sessionid,store_id,qry_id,upo,parent_id,reference_id,before);
end;

{
 QSize = 3
 Q1 U1     Case Update      CASE 2 (DELETE)            CASE 3 (INS)               CASE 4 (INS/DEL (SPLIT UPD)
    U2      U7  - NOP       Delete U5                  INS U2.5                   DO Case 3 and 4, but dont update
    U3      U13 - UP Q3U13                                                        Total Query Count
 Q2 U4                      Q1 - NOP                   Q1 REM U3,INS U2.5
    U5	                    Q2 - REM U5 from Client    Q2 INS Q3 (Head), REM U6
    U6                           INS U7 into Query     Q3 INS U11, REM U14
 Q3 U12                     Q3 - REM U12
    U13	                         INS U15 (if avail)
    U14						DEC TOTAL QRY CNT	       INC TOTAL QRY CNT
}

procedure TFRE_DB_TRANSDATA_MANAGER.UpdateObjectInFilterKey(const td: TFRE_DB_TRANS_COLL_DATA; const filtercont: TFRE_DB_FilterContainer; const new_obj: IFRE_DB_Object; const idx: NativeInt);

  procedure HandleQueryUpdate(const qry : TFRE_DB_QUERY);
  var
      full_data_key : TFRE_DB_TRANS_COLL_DATA_KEY;
      ses           : TFRE_DB_UserSession;
      update_st     : TFRE_DB_UPDATE_STORE_DESC;
      upo           : IFRE_DB_Object;
  begin
    full_data_key           := td.GetFullKey;
    full_data_key.filterkey := filtercont.Filters.GetFilterKey;
    if not (FREDB_CompareTransCollDataKeys(full_data_key,qry.GetFullQueryOrderKey)) then
         exit; { query does not match the delivered update spec }
    GFRE_DBI.LogDebug(dblc_DBTDM,'       >QRY MATCH UPDATE OBJECT [%s] IN QRY [%s]',[new_obj.UID_String,qry.GetFullQueryOrderKey.key]);
    if (idx >= qry.FStartIdx) and (idx < qry.FStartIdx+qry.FQueryDeliveredCount) then
      begin { update is in the open query page }
        if not GFRE_DBI.NetServ.FetchSessionByIdLocked(qry.FSessionID,ses) then
          begin
            GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] NOT FOUND ON UPDATE QRY(?)',[qry.FSessionID]);
            exit;
          end
        else
          begin
            try
              upo := new_obj.CloneToNewObject;
              td.FBaseTransData.FDC.FinalRightTransform(ses,upo);
            finally
              ses.UnlockSession;
            end;
            CN_AddGridInplaceUpdate(qry.FSessionID,qry.GetStoreID,qry.GetQueryID,upo);
          end;
      end;
  end;

begin
  ForAllQueries(@HandleQueryUpdate);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.RemoveUpdateObjectInFilterKey(const td: TFRE_DB_TRANS_COLL_DATA; const filtercont: TFRE_DB_FilterContainer; const old_obj: IFRE_DB_Object; const idx: NativeInt);

  procedure HandleQueryRemove(const qry : TFRE_DB_QUERY);
  var
      full_data_key : TFRE_DB_TRANS_COLL_DATA_KEY;
      ses           : TFRE_DB_UserSession;
      update_st     : TFRE_DB_UPDATE_STORE_DESC;
      upo           : IFRE_DB_Object;
  begin
    full_data_key           := td.GetFullKey;
    full_data_key.filterkey := filtercont.Filters.GetFilterKey;
    if not (FREDB_CompareTransCollDataKeys(full_data_key,qry.GetFullQueryOrderKey)) then
         exit; { query does not match the delivered update spec }
    GFRE_DBI.LogDebug(dblc_DBTDM,'       >QRY MATCH REMOVE OBJECT [%s] IN QRY [%s]',[old_obj.UID_String,qry.GetFullQueryOrderKey.key]);
    if (idx >= qry.FStartIdx) and (idx < qry.FStartIdx+qry.FQueryDeliveredCount) then
      begin { delete is in the query page }
        //if FREDB_GuidInArray(old_obj.UID,qry.FResultDBOs)=-1 then { qry.FResultDBOs not neccessary (check)}
        //  raise EFRE_DB_Exception.Create(edb_ERROR,'logic/error update not found in result set');
        dec(qry.FQueryDeliveredCount);
        dec(qry.FQueryPotentialCount);
        CN_AddGridInplaceDelete(qry.FSessionID,qry.GetStoreID,qry.GetQueryID,old_obj.UID_String);
      end;
  end;

begin
  ForAllQueries(@HandleQueryRemove);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.InsertUpdateObjectInFilterKey(const td: TFRE_DB_TRANS_COLL_DATA; const filtercont: TFRE_DB_FilterContainer; const new_obj: IFRE_DB_Object; const idx: NativeInt; const parent_id: string);

  procedure HandleQueryInsert(const qry : TFRE_DB_QUERY);
  var full_data_key : TFRE_DB_TRANS_COLL_DATA_KEY;
      ses           : TFRE_DB_UserSession;
      update_st     : TFRE_DB_UPDATE_STORE_DESC;
      upo           : IFRE_DB_Object;
      ref_uid       : TFRE_DB_Guid;

      procedure CN_Insert(const ref_id : string ; const before : boolean);
      begin
        inc(qry.FQueryDeliveredCount);
        inc(qry.FQueryPotentialCount);
        CN_AddGridInsertUpdate(qry.FSessionID,qry.GetStoreID,qry.GetQueryID,upo,parent_id,'',before); { add the first object }
      end;

      procedure InsertFirst;
      begin
        if not GFRE_DBI.NetServ.FetchSessionByIdLocked(qry.FSessionID,ses) then
          begin
            GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] NOT FOUND ON UPDATE/INSERT QRY(?)',[qry.FSessionID]);
            exit;
          end
        else
          begin
            try
              upo := new_obj.CloneToNewObject;
              td.FBaseTransData.FDC.FinalRightTransform(ses,upo);
            finally
              ses.UnlockSession;
            end;
            CN_Insert('',false);
          end;
      end;

  begin
    full_data_key           := td.GetFullKey;
    full_data_key.filterkey := filtercont.Filters.GetFilterKey;
    if not (FREDB_CompareTransCollDataKeys(full_data_key,qry.GetFullQueryOrderKey)) then
         exit; { query does not match the delivered update spec }
    GFRE_DBI.LogDebug(dblc_DBTDM,'       >QRY MATCH INSERT OBJECT [%s] IN QRY [%s]',[new_obj.UID_String,qry.GetFullQueryOrderKey.key]);
    if qry.FQueryDeliveredCount=0 then
      begin
        InsertFirst;
        exit;
      end;
    if (idx >= qry.FStartIdx) and (idx < qry.FStartIdx+qry.FToDeliverCount) then
      begin { update is in the open query page }
        //if FREDB_GuidInArray(new_obj.UID,qry.FResultDBOs)<>-1 then { qry.FResultDBOs not neccessary (check)}
        //  raise EFRE_DB_Exception.Create(edb_ERROR,'logic/error update already found in result set');
        if not GFRE_DBI.NetServ.FetchSessionByIdLocked(qry.FSessionID,ses) then
          begin
            GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] NOT FOUND ON UPDATE/INSERT QRY(?)',[qry.FSessionID]);
            exit;
          end
        else
          begin
            try
              upo := new_obj.CloneToNewObject;
              td.FBaseTransData.FDC.FinalRightTransform(ses,upo);
            finally
              ses.UnlockSession;
            end;
            if (idx=qry.FStartIdx) then { = first }
              begin
                ref_uid := filtercont.FOBJArray[idx+1].UID;
                CN_Insert(FREDB_G2H(ref_uid),true);
              end
            else
            if (idx<qry.FStartIdx+qry.FQueryDeliveredCount) then { smaller then last deliverd, but not first}
              begin
                ref_uid := filtercont.FOBJArray[idx-1].UID;
                CN_Insert(FREDB_G2H(ref_uid),false); { add the new object, idx-1 = the element the client had at this position }
              end
            else
            if (qry.FQueryDeliveredCount < qry.FToDeliverCount) then { qry has room to deliver additional }
              begin
                ref_uid := filtercont.FOBJArray[idx-1].UID;
                CN_Insert(FREDB_G2H(ref_uid),false); { add the new object, idx-1 = the element the client had at this position }
                //writeln('DUMP START');
                //FREDB_DumpArray(filtercont.FOBJArray,0,high(filtercont.FOBJArray),'key');
                //writeln('DUMP END');
                //writeln('REF UID ',FREDB_G2H(ref_uid),' AFTER');
              end
            else
              begin { qry is full ? }
                abort;
              end;
              { OK , delivered one additional entry };
            //CN_AddGridInplaceUpdate();
          end;
      end;
  end;

begin
  ForAllQueries(@HandleQueryInsert);
end;


procedure TFRE_DB_TRANS_COLL_DATA.InsertIntoTree(const insert_obj: TFRE_DB_Object);
var
    Key      : Array [0..512] of Byte;
    k_len    : NativeInt;
    oc       : TFRE_DB_OrderContainer;
    dummy    : PPtrUInt;
    byte_arr : TFRE_DB_ByteArray;
begin
  FOrderDef.SetupBinaryKey(insert_obj,@key,k_len,True);
  dummy := nil;
  if FArtTreeKeyToObj.InsertBinaryKeyorFetchR(key,k_len,dummy) then
    begin
      oc := TFRE_DB_OrderContainer.Create;
      dummy^ := FREDB_ObjectToPtrUInt(oc);
    end
  else
    oc := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_OrderContainer;
  oc.AddObject(insert_obj);
end;


procedure TFRE_DB_TRANS_COLL_DATA.Notify_InsertIntoTree(const new_obj: TFRE_DB_Object);
var
   keynew       : Array [0..512] of Byte;
   keynewlen    : NativeInt;
begin
  FOrderDef.SetupBinaryKey(new_obj,@Keynew[0],keynewlen,true);
  Notify_InsertIntoTree(@keynew[0],keynewlen,new_obj);
end;

procedure TFRE_DB_TRANS_COLL_DATA.Notify_InsertIntoTree(const key: PByte; const keylen: NativeInt; const new_obj: TFRE_DB_Object; const propagate_up: boolean);
var
    oc       : TFRE_DB_OrderContainer;
    dummy    : PPtrUInt;
    byte_arr,
    byte_arr2: TFRE_DB_ByteArray;

  procedure CheckAllOpenFiltersInsert(var dummy : PtrUInt);
  var filtercont : TFRE_DB_FilterContainer;
  begin
    filtercont := FREDB_PtrUIntToObject(dummy) as TFRE_DB_FilterContainer;
    filtercont.Notify_CheckFilteredInsert(self,new_obj);
  end;

begin
  dummy := nil;
  if FArtTreeKeyToObj.InsertBinaryKeyorFetchR(key,keylen,dummy) then
    begin
      oc := TFRE_DB_OrderContainer.Create;
      dummy^ := FREDB_ObjectToPtrUInt(oc);
    end
  else
    oc := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_OrderContainer;
  oc.AddObject(new_obj);

  FREDB_BinaryKey2ByteArray(key,keylen,byte_arr);
  byte_arr2 := new_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(FOrderDef.FKey.orderkey).AsByteArr;
  if (Length(byte_arr)<>Length(byte_arr2)) or (CompareByte(byte_arr,byte_arr2,Length(byte_arr))=0) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal wrong tagged');
  if propagate_up then
    FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersInsert);
end;

procedure TFRE_DB_TRANS_COLL_DATA.Notify_DeleteFromTree(const old_obj: TFRE_DB_Object);
var
   keyold       : Array [0..512] of Byte;
   keyoldlen    : NativeInt;
begin
  FOrderDef.SetupBinaryKey(old_obj,@KeyOld[0],keyoldlen,false);
  Notify_DeleteFromTree(@keyold,keyoldlen,old_obj);
end;

procedure TFRE_DB_TRANS_COLL_DATA.Notify_DeleteFromTree(const key: PByte; const keylen: NativeInt; const old_obj: TFRE_DB_Object; const propagate_up: boolean);
var valueptr     : PtrUInt;
    oc           : TFRE_DB_OrderContainer;

  procedure CheckAllOpenFiltersRemoveOrderChanged(var dummy : PtrUInt);
  var filtercont : TFRE_DB_FilterContainer;
  begin
    filtercont := FREDB_PtrUIntToObject(dummy) as TFRE_DB_FilterContainer;
    filtercont.Notify_CheckFilteredDelete(self,old_obj);
  end;

begin
  if not FArtTreeKeyToObj.ExistsBinaryKey(key,keylen,valueptr) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'notify tree update internal / value not found');
  oc := FREDB_PtrUIntToObject(valueptr) as TFRE_DB_OrderContainer;
  if oc.RemoveObject(old_obj) then
    begin
      FArtTreeKeyToObj.RemoveBinaryKey(key,keylen,valueptr);
      assert(TFRE_DB_OrderContainer(valueptr)=oc,'internal/logic remove failed');
      oc.free;
    end;
  if propagate_up then
    FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersRemoveOrderChanged);
end;


procedure TFRE_DB_TRANS_COLL_DATA.Notify_UpdateIntoTree(const old_obj, new_obj: TFRE_DB_Object);
var
   keyold,
   keynew       : Array [0..512] of Byte;
   keyoldlen,
   keynewlen    : NativeInt;
   orderchanged : boolean;
   valueptr     : PtrUInt;
   oc           : TFRE_DB_OrderContainer;

   procedure CheckAllOpenFiltersNoOrderChanged(var dummy : PtrUInt);
   var filtercont : TFRE_DB_FilterContainer;
   begin
     filtercont := FREDB_PtrUIntToObject(dummy) as TFRE_DB_FilterContainer;
     filtercont.Notify_CheckFilteredUpdate(self,old_obj,new_obj,false);
   end;

   procedure CheckAllOpenFiltersOrderChanged(var dummy : PtrUInt);
   var filtercont : TFRE_DB_FilterContainer;
   begin
     filtercont := FREDB_PtrUIntToObject(dummy) as TFRE_DB_FilterContainer;
     filtercont.Notify_CheckFilteredUpdate(self,old_obj,new_obj,true);
   end;

begin
  FOrderDef.SetupBinaryKey(old_obj,@KeyOld[0],keyoldlen,false);
  FOrderDef.SetupBinaryKey(new_obj,@keynew[0],keynewlen,true);
  orderchanged := (keyoldlen <> keynewlen) or
                  (CompareByte(keynew,keyold,keyoldlen)<>0);
  if orderchanged then { Key has changed, thus order has possibly changed -> issue an remove, insert cycle ....}
    begin { todo - check if order realy changed, neighbors ?}
      GFRE_DBI.LogDebug(dblc_DBTDM,'    >POTENTIAL ORDER CHANGED / UPDATE OBJECT [%s] IN ORDERING [%s] DELETE/INSERT CYCLE',[new_obj.UID_String,FOrderDef.GetFullKey.key]);
      Notify_DeleteFromTree(@keyold[0],keyoldlen,old_obj,false);
      Notify_InsertIntoTree(@keynew[0],keynewlen,new_obj,false);
      FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersOrderChanged);
    end
  else
    begin
      if not FArtTreeKeyToObj.ExistsBinaryKey(keyold,keyoldlen,valueptr) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,'notify tree update internal / value not found');
      GFRE_DBI.LogDebug(dblc_DBTDM,'    >ORDER NOT CHANGED / UPDATE OBJECT [%s] IN ORDERING [%s]',[new_obj.UID_String,FOrderDef.GetFullKey.key]);
      oc := FREDB_PtrUIntToObject(valueptr) as TFRE_DB_OrderContainer;
      oc.ReplaceObject(old_obj,new_obj);
      FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersNoOrderChanged);
    end;
end;

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
  base_trans_data.AddOrdering(self);
end;

procedure TFRE_DB_TRANS_COLL_DATA.OrderTheData;
var i       : NativeInt;

    procedure OrderArray(const tobj : TFRE_DB_Object);
    begin
      InsertIntoTree(tobj);
      //tobj.Field(cFRE_DB_SYS_ORDER_REF_KEY).AsObject.Field(GetFullKeyDef).AsBoolean:=true;
    end;

begin
  FArtTreeKeyToObj.Clear;
  FBaseTransData.ForAllObjs(@OrderArray);
  //OrderArray(FBaseTransData.GetDataArray);
  //for i := 0 to High(FBaseTransData.GetDataArray^) do
  //  begin
  //    tobj := FBaseTransData.GetDataArray^[i].Implementor as TFRE_DB_Object;
  //    //tobj.Field(cFRE_DB_SYS_ORDER_REF_KEY).AsObject.Field(GetFullKeyDef).AsBoolean:=true;
  //    FChildTransdata := TFRE_DB_TRANFORMED_DATA(tobj.ExtensionTag);
  //    if assigned(FChildTransdata) then
  //      begin
  //      end;
  //  end;
end;

function TFRE_DB_TRANS_COLL_DATA.GetFullKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FOrderDef.GetFullKey;
end;

destructor TFRE_DB_TRANS_COLL_DATA.Destroy;
begin
  FArtTreeKeyToObj.Destroy;
  FTransCollDataLock.Finalize;
  inherited Destroy;
end;


procedure TFRE_DB_TRANS_COLL_DATA.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY; const compare_run: boolean);
var brk        : boolean;
    filtkey    : TFRE_DB_TRANS_COLL_FILTER_KEY;
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
      filtercont := TFRE_DB_FilterContainer.Create(GetFullKey.orderkey);
      filtercont.Filters.AddFilters(qry_context.FQueryFilters);
      filtercont.Filters.Seal;
      dummy^     := FREDB_ObjectToPtrUInt(filtercont);
    end
  else
    begin
      GFRE_DBI.LogInfo(dblc_DBTDM,'>REUSING FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] [%s]',[GetFullKey.key,filtkey,BoolToStr(filtercont.IsFilled,'FILLED','NOT FILLED')]);
      filtercont := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_FilterContainer;
    end;
  filtercont.CheckDBReevaluation; { check if the filter was updated and needs db, reevaluation }
  if not filtercont.IsFilled then
    begin
      GFRE_DBI.LogInfo(dblc_DBTDM,'>NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s]',[GetFullKey.key,filtkey]);
      st := GFRE_BT.Get_Ticks_ms;
      filtercont.Filters.MustBeSealed;
      FArtTreeKeyToObj.LinearScanBreak(@IteratorBreak,brk,false);
      filtercont.IsFilled := true;
      filtercont.AdjustLength;
      et := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogInfo(dblc_DBTDM,'<NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] DONE in %d ms',[GetFullKey.key,filtkey,et-st]);
    end;
  filtercont.Execute(iter,qry_context,compare_run);
end;

procedure TFRE_DB_TRANS_COLL_DATA.UpdateTransformedobject(const old_obj, new_object: IFRE_DB_Object);
begin
  Notify_UpdateIntoTree(old_obj.Implementor as TFRE_DB_Object,new_object.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_TRANS_COLL_DATA.InsertTransformedobject(const new_object: IFRE_DB_Object);
begin
  Notify_InsertIntoTree(new_object.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_TRANS_COLL_DATA.DeleteTransformedobject(const del_object: IFRE_DB_Object);
begin
  Notify_DeleteFromTree(del_object.Implementor as TFRE_DB_Object);
end;

{ TFRE_DB_TRANSDATA_MANAGER }


end.

