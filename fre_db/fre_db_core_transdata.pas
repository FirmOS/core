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
  *) Improve Locking !
}

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
     Classes,contnrs, SysUtils,fre_db_interface,fre_db_core,fos_art_tree,fos_tool_interfaces,fos_arraygen,fre_db_common,fre_db_persistance_common,fos_strutils,fre_aps_interface,math,fre_system;
var
    cFRE_INT_TUNE_SYSFILTEXTENSION_SZ : NativeUint =  128;
    cFRE_INT_TUNE_FILTER_PURGE_TO     : NativeUint =  0; // 5*1000; // 0 DONT PURGE


{
  TDM - Transformed Data Manager
  Notify Strategy
  1) Apply Changes from Notification Block (Transaction ID), mark QRYs with TID
  2) On Block End Check all Qry's for TID mark -> run compare querys
}

type
  TFRE_DB_TRANSFORMED_ORDERED_DATA = class;

  TFRE_DB_DC_ORDER = record
    order_field      : TFRE_DB_NameType;
    ascending        : boolean;
    case_insensitive : boolean;
  end;

  TFRE_DB_DC_ORDER_LIST = array of TFRE_DB_DC_ORDER;

  TFRE_DB_DC_ORDER_ITERATOR = procedure(const order:TFRE_DB_DC_ORDER) is nested;

  { TFRE_DB_DC_ORDER_DEFINITION }

  TFRE_DB_DC_ORDER_DEFINITION = class(TFRE_DB_DC_ORDER_DEFINITION_BASE) { defines a globally stored ordered and transformed set of dbo's}
  private
    FOrderList          : array of TFRE_DB_DC_ORDER;
    FKey                : TFRE_DB_TRANS_COLL_DATA_KEY;
    function    IsSealed : Boolean;
  public
    procedure   MustNotBeSealed;
    procedure   MustBeSealed;
    procedure   SetDataKeyColl    (const parent_collectionname,derivedcollname : TFRE_DB_NameType ; const ParentChildspec : TFRE_DB_NameTypeRL);
    procedure   ClearOrders       ; override;
    procedure   AddOrderDef       (const orderfield_name : TFRE_DB_NameType ; const asc : boolean ; const case_insens : boolean); override;
    procedure   Seal              ;
    function    GetBaseKeyPart    : TFRE_DB_TRANS_COLL_DATA_KEY;
    function    GetFullKey        : TFRE_DB_TRANS_COLL_DATA_KEY;
    procedure   ForAllOrders      (const orderiterator : TFRE_DB_DC_ORDER_ITERATOR);
    procedure   AssignFrom        (const orderdef : TFRE_DB_DC_ORDER_DEFINITION_BASE); override;
    procedure   SetupBinaryKey    (const obj : TFRE_DB_Object ; const Key : PByteArray ; var max_key_len : NativeInt ; const tag_object : boolean); { setup a binary key according to this order }
    function    OrderCount        : NativeInt; override;
  end;

  { TFRE_DB_FILTER_STRING }

  TFRE_DB_FILTER_STRING=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : TFRE_DB_StringArray;
    FFilterType : TFRE_DB_STR_FILTERTYPE;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType;override;
    function  CheckFilterMiss  (const obj : IFRE_DB_Object; var flt_errors : Int64):boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const fieldname : TFRE_DB_NameType ; filtervalues : Array of Double ; const numfiltertype : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean ; const include_null_values : boolean);
  end;


  { TFRE_DB_FILTER_BOOLEAN }

  TFRE_DB_FILTER_BOOLEAN=class(TFRE_DB_FILTER_BASE)
  protected
    FValue      : Boolean;
    FFilterType : TFRE_DB_NUM_FILTERTYPE;
  public
    function  Clone           : TFRE_DB_FILTER_BASE;override;
    function  CheckFilterMiss (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
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
    function  CheckFilterMiss         (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey        : TFRE_DB_NameType; override;
    procedure FilterExpandRefs        (const dbc : IFRE_DB_CONNECTION);
    procedure InitFilter              (const RL_Spec: TFRE_DB_NameTypeRLArray; const StartDependecyValues: TFRE_DB_GUIDArray; const negate: boolean; const include_null_values: boolean ;  const dbname: TFRE_DB_NameType);
    procedure ReEvalFilterStartVals   ; override;
    function  CheckReflinkUpdateEvent (const key_descr: TFRE_DB_NameTypeRL) : boolean; override;
  end;


  { TFRE_DB_FILTER_SCHEME }

  TFRE_DB_FILTER_SCHEME=class(TFRE_DB_FILTER_BASE)
  protected
    FValues     : Array of TFRE_DB_NameType;
  public
    function  Clone            : TFRE_DB_FILTER_BASE ; override;
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    procedure InitFilter       (filtervalues : Array of TFRE_DB_String ; const negate:boolean);
  end;

  { TFRE_DB_FILTER_RIGHT }

  TFRE_DB_FILTER_RIGHT=class(TFRE_DB_FILTER_BASE)
  private
    type
      TFRE_DB_FILTER_RIGHT_FILTER_MODE=(fm_ObjectRightFilter,fm_ReferedRightFilter,fm_ObjectRightFilterGeneric,fm_ReferedRightFilterGeneric);
  protected
    FRight            : TFRE_DB_STANDARD_RIGHT_SET;
    FMode             : TFRE_DB_FILTER_RIGHT_FILTER_MODE;
    FUserTokenClone   : IFRE_DB_USER_RIGHT_TOKEN;
    FSchemeclass      : TFRE_DB_Nametype;
    FDomIDField       : TFRE_DB_Nametype;
    FObjUidField      : TFRE_DB_NameType;
    FSchemeClassField : TFRE_DB_NameType;
    FRightSet         : TFRE_DB_StringArray;
    FIgnoreField      : TFRE_DB_NameType;   { specify a fieldname }
    FIgnoreValue      : TFRE_DB_NameType;   { and a value that ignore field contains, the filter should NOT Filter values that are to be ignored (ignorefield.asstring=value)}
  public
    function  Clone                    : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey         : TFRE_DB_NameType; override;
    function  CheckFilterMiss          (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter               (stdrightset  : TFRE_DB_STANDARD_RIGHT_SET ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN ; const negate:boolean ; const ignoreFieldname , ignoreFieldValue : TFRE_DB_String);
    procedure InitFilterRefered        (domainidfield,objuidfield,schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType ; stdrightset  : TFRE_DB_STANDARD_RIGHT_SET ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN ; const negate:boolean; const ignoreFieldname , ignoreFieldValue : TFRE_DB_String);
    procedure InitFilterGenRights      (const stdrightset  : array of TFRE_DB_String ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN ; const negate:boolean; const ignoreFieldname , ignoreFieldValue : TFRE_DB_String);
    procedure InitFilterGenRightsRefrd (const domainidfield,objuidfield,schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType ; stdrightset  : array of TFRE_DB_String ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN ; const negate:boolean; const ignoreFieldname , ignoreFieldValue : TFRE_DB_String);
  end;

  { TFRE_DB_FILTER_PARENT }

  TFRE_DB_FILTER_PARENT=class(TFRE_DB_FILTER_BASE)
  protected
    FAllowedParent : String;
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override;
    procedure InitFilter       (const allowed_parent_path: TFRE_DB_GUIDArray);
  end;

  { TFRE_DB_FILTER_CHILD }

  TFRE_DB_FILTER_CHILD=class(TFRE_DB_FILTER_BASE)
  protected
  public
    function  Clone            : TFRE_DB_FILTER_BASE;override;
    function  GetDefinitionKey : TFRE_DB_NameType; override;
    function  CheckFilterMiss  (const obj: IFRE_DB_Object ; var flt_errors : Int64): boolean; override; { definition OK ? (miss) }
    procedure InitFilter       ;
  end;



  { TFRE_DB_DC_FILTER_DEFINITION }

  TFRE_DB_DC_FILTER_DEFINITION = class(TFRE_DB_DC_FILTER_DEFINITION_BASE)
  private
    FFilterKey     : TFRE_DB_TRANS_COLL_FILTER_KEY; { summed unique filter key }
    FKeyList       : TFPHashObjectList;
    FFilterMiss    : boolean;
    FFilterErr     : int64;
    FFiltDefDBname : TFRE_DB_NameType;
    function   IsSealed : Boolean;
    procedure  _ForAllAdd    (obj:TObject ; arg:Pointer);
    procedure  _ForAllKey    (obj:TObject ; arg:Pointer);
    procedure  _ForAllFilter (obj:TObject ; arg:Pointer);
  public
    constructor Create                       (const filter_dbname : TFRE_DB_NameType);
    destructor  Destroy                      ;override;
    procedure   AddFilters                   (const source : TFRE_DB_DC_FILTER_DEFINITION_BASE; const clone : boolean=true); { add filters,take ownership }
    procedure   AddFilter                    (const source : TFRE_DB_FILTER_BASE; const clone : boolean=false);              { add filter,take ownership }
    procedure   AddStringFieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalue  : TFRE_DB_String              ; const stringfiltertype : TFRE_DB_STR_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddSignedFieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Int64              ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddUnsignedFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Uint64             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddCurrencyFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Currency           ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddReal64FieldFilter         (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of Double             ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddDatetimeFieldFilter       (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_DateTime64 ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddBooleanFieldFilter        (const key,fieldname:TFRE_DB_NameType ; filtervalue  : boolean                                                                       ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddUIDFieldFilter            (const key,fieldname:TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_GUID       ; const numfiltertype    : TFRE_DB_NUM_FILTERTYPE ; const negate:boolean=true  ; const include_null_values : boolean=false);override;
    procedure   AddSchemeObjectFilter        (const key:          TFRE_DB_NameType ; filtervalues : Array of TFRE_DB_String                                                       ; const negate:boolean=true );override;
    procedure   AddStdRightObjectFilter      (const key:          TFRE_DB_NameType ; stdrightset  : TFRE_DB_STANDARD_RIGHT_SET  ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN      ; const negate:boolean=true ; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');override;
    procedure   AddStdClassRightFilter       (const key:          TFRE_DB_NameType ; domainidfield, objuidfield, schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean=true; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String=''); override;
    procedure   AddObjectRightFilter         (const key:          TFRE_DB_NameType ; rightset  : Array of TFRE_DB_String  ; const usertoken : IFRE_DB_USER_RIGHT_TOKEN      ; const negate:boolean=true ; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');override;
    procedure   AddClassRightFilter          (const key:          TFRE_DB_NameType ; domainidfield, objuidfield, schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; rightset: Array of TFRE_DB_String; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean=true; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');override;
    procedure   AddChildFilter               (const key:          TFRE_DB_NameType); override ;
    procedure   AddParentFilter              (const key:          TFRE_DB_NameType ; const allowed_parent_path : TFRE_DB_GUIDArray); override ;
    procedure   AddAutoDependencyFilter      (const key:          TFRE_DB_NameType ; const RL_Spec : Array of TFRE_DB_NameTypeRL ;  const StartDependecyValues : Array of TFRE_DB_GUID  ; const one_value:boolean=true ; const include_null_values : boolean=false);override;

    function    RemoveFilter                 (const key:          TFRE_DB_NameType):boolean;override;
    function    FilterExists                 (const key:          TFRE_DB_NameType):boolean;override;
    procedure   RemoveAllFilters             ;override;
    procedure   RemoveAllFiltersPrefix       (const key_prefix:   TFRE_DB_NameType);override;

    procedure   MustNotBeSealed              ;
    procedure   MustBeSealed                 ;
    procedure   Seal                         ;
    function    GetFilterKey                 : TFRE_DB_TRANS_COLL_FILTER_KEY;
    function    DoesObjectPassFilters        (const obj : IFRE_DB_Object) : boolean;
    function    CheckDBReevaluationFilters   : boolean;
    function    CheckAutoDependencyFilter    (const key_description : TFRE_DB_NameTypeRL):boolean;
    function    FilterDBName                 : TFRE_DB_NameType;
    //procedure
  end;

  TFRE_DB_FilterContainer=class;

  { TFRE_DB_QUERY }

  TFRE_DB_QUERY=class(TFRE_DB_QUERY_BASE)
  private
    function  GetFilterDefinition: TFRE_DB_DC_FILTER_DEFINITION;
    function  GetOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION;
  protected
     FBaseData               : TFRE_DB_TRANSFORMED_ORDERED_DATA; { assigned transformed and ordered data }
     FFilterContainer        : TFRE_DB_FilterContainer;          { the filtercontainer of the ordering   }
     FQryDBName              : TFRE_DB_NameType;                 { used for reeval filters, etc  }
     FQueryId                : TFRE_DB_NameType;                 { ID of this specific Query }
     FQueryClientID          : Int64;
     FQueryDescr             : string;
     FParentChildLinkFldSpec : TFRE_DB_NameTypeRL;               { rl spec of the parent child relation }
     FParentChildSkipschemes : TFRE_DB_NameTypeRLArray;          { skip this schemes in a parent child query }
     FQueryFilters           : TFRE_DB_DC_FILTER_DEFINITION;     { managed here, cleanup here}
     FOrderDef               : TFRE_DB_DC_ORDER_DEFINITION;      { linked to order definition of base ordered data, dont cleanup here}
     FDependencyIds          : TFRE_DB_StringArray;              { dependency id's of this querys DC, in same order as }
     FDepRefConstraints      : TFRE_DB_NameTypeRLArrayArray;     { this reference link constraints, there must be an extra }
     FDependcyFilterUids     : Array of TFRE_DB_GUIDArray;       { this is a special filter type, the GUID array may, at some later time, be extended to support other field types }
     FParentIds              : TFRE_DB_GUIDArray;                { parentid / path to root / of this query (filter) }
     FIsChildQuery           : Boolean;                          { this is a request for childs }
     FUserKey                : TFRE_DB_String;                   { Login@Domain | GUID ?}
     FStartIdx               : NativeInt;                        { }
     FToDeliverCount         : NativeInt;                        { }
     FResultDBOs             : IFRE_DB_ObjectArray;              { remember all objs of this query, need to update that list by notify updates, need to hold that list to check against changes in filterchanges }
     FResultDBOsCompare      : IFRE_DB_ObjectArray;              { due to a filter update rerun the query - store in compare array, compare, send updates, switch the arrays }

     FQueryRunning           : boolean;
     FQueryRunningCmp        : boolean;
     FQueryStartTime         : NativeInt;
     FQueryEndTime           : NativeInt;
     FQueryStartTimeCmp      : NativeInt;
     FQueryEndTimeCmp        : NativeInt;
     FQueryCurrIdx           : NativeInt;
     FQueryCurrIdxCmp        : NativeInt;
     FQueryDeliveredCount    : NativeInt;
     FQueryDeliveredCountCmp : NativeInt;
     FQueryPotentialCount    : NativeInt;
     FQueryPotentialCountCmp : NativeInt;
     FQueryIsLastPage        : boolean;   { Flag to indicate that this query includes the last page of the resultset (the last element) }

     FQCreationTime          : TFRE_DB_DateTime64;
     FSessionID              : String;
     FQryTransTag            : TFRE_DB_TransStepId;
     FOnlyOneUID             : TFRE_DB_GUID;
     FUidPointQry            : Boolean;

     function                GetReflinkSpec        (const upper_refid : TFRE_DB_NameType):TFRE_DB_NameTypeRLArray;
     function                GetReflinkStartValues (const upper_refid : TFRE_DB_NameType):TFRE_DB_GUIDArray; { start values for the RL expansion }
     procedure               SwapCompareQueryQry   ;
  public
     procedure   SetResultObject                   (const idx: NativeInt; const obj: IFRE_DB_Object ; const compare_run : boolean);
     procedure   AddjustResultDBOLen               (const compare_run : boolean);
     procedure   SetMaxResultDBOLen                (const compare_run : boolean);
     procedure   StartQueryRun                     (const compare_run : boolean);
     procedure   EndQueryRun                       (const compare_run : boolean);

     constructor Create                            (const qry_dbname : TFRE_DB_NameType);
     destructor  Destroy                           ; override;
     function    GetQueryID                        : TFRE_DB_NameType; override;
     function    GetQueryID_ClientPart             : Int64;
     function    HasOrderDefinition                : boolean;
     property    Orderdef                          : TFRE_DB_DC_ORDER_DEFINITION  read GetOrderDefinition;
     property    Filterdef                         : TFRE_DB_DC_FILTER_DEFINITION read GetFilterDefinition;
     function    GetBaseTransDataKey               : TFRE_DB_TRANS_COLL_DATA_KEY;
     function    GetFullQueryOrderKey              : TFRE_DB_TRANS_COLL_DATA_KEY;
     procedure   SetBaseOrderedData                (const basedata   : TFRE_DB_TRANS_RESULT_BASE ; const session_id : TFRE_DB_String);override;
     procedure   UnlockBaseData                    ; override;
     function    ExecuteQuery                      (const iterator   : IFRE_DB_Obj_Iterator):NativeInt;override; { execute the query, determine count and array of result dbo's }
     procedure   ExecutePointQuery                 (const iterator   : IFRE_DB_Obj_Iterator);override;
     function    CheckAutoDependencyFilterChanges  (const key_description : TFRE_DB_NameTypeRL):boolean;
     procedure   ProcessFilterChangeBasedUpdates   ; { compare query invokation }
     procedure   ProcessChildObjCountChange        (const obj : IFRE_DB_Object);
     function    GetStoreID                        : TFRE_DB_NameType;
     procedure   TagForUpInsDelRLC                 (const TransID : TFRE_DB_TransStepId);
     procedure   ClearTransactionTag               ;
     function    GetTransactionStepID              : TFRE_DB_TransStepId;
     property    QryDBName                         : TFRE_DB_NameType read FQryDBName;
  end;


  { TFRE_DB_SESSION_UPO }

  TFRE_DB_SESSION_UPO = class
  private
     FStoreList : TFPHashObjectList;
     FSessid    : TFRE_DB_NameType;
     function   GetUpdateStore            (const store_id: shortstring): TFRE_DB_UPDATE_STORE_DESC;
  public
     constructor Create                   (const session_id : TFRE_DB_NameType);
     destructor  Destroy                  ; override;
     procedure   AddStoreUpdate           (const store_id: TFRE_DB_NameType; const qid: Int64; const upo: IFRE_DB_Object);
     procedure   AddStoreInsert           (const store_id: TFRE_DB_NameType; const qid: Int64; const upo: IFRE_DB_Object; const reference_id: TFRE_DB_String);
     procedure   AddStoreDelete           (const store_id: TFRE_DB_NameType; const qid: Int64; const id: TFRE_DB_String);
     procedure   DispatchAllNotifications ;
  end;

  { TFRE_DB_TRANSDATA_CHANGE_NOTIFIER }

  TFRE_DB_TRANSDATA_CHANGE_NOTIFIER=class(IFRE_DB_TRANSDATA_CHANGE_NOTIFIER)
  private
     FSessionUpdateList : TFPHashObjectList;
     FKey               : TFRE_DB_TransStepId;
     function    GetSessionUPO                (const sessionid: TFRE_DB_NameType): TFRE_DB_SESSION_UPO;
  public
     property    TransKey                     : TFRE_DB_TransStepId read FKey;
     constructor Create                       (const key: TFRE_DB_TransStepId);
     destructor  Destroy                      ;override;
     procedure   AddDirectSessionUpdateEntry  (const update_dbo : IFRE_DB_Object); { add a dbo update for sessions dbo's (forms) }
     procedure   AddGridInplaceUpdate         (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64 ; const upo: IFRE_DB_Object); { inplace update entry for the store }
     procedure   AddGridInsertUpdate          (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const upo: IFRE_DB_Object; const reference_id: TFRE_DB_String);
     procedure   AddGridRemoveUpdate          (const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const del_id: TFRE_DB_String);
     procedure   NotifyAll;
  end;

  { TFRE_DB_TRANFORMED_DATA }

  TFRE_DB_TRANFORMED_DATA=class(TFRE_DB_TRANSFORMED_ARRAY_BASE)
  private
    FTDDBName             : TFRE_DB_NameType;
    FKey                  : TFRE_DB_TRANS_COLL_DATA_KEY;   { CN/DCN/CHILD RL SPEC }
    FTransformKey         : QWord;
    FTransformedData      : TFPHashObjectList;
    FOrderings            : TFPObjectList;
    FTDCreationTime       : TFRE_DB_DateTime64;
    FChildDataIsLazy      : Boolean; { the child data is lazy : UNSUPPORTED }
    FDC                   : IFRE_DB_DERIVED_COLLECTION;
    function              IncludesChildData      : Boolean; { the query is a tree query, thus the objects contain extension pointers to TFRE_DB_TRANFORMED_DATA objects }
    function              HasReflinksInTransform : Boolean; { the query has reflinks in the transform, so on RL change the Transform has to be revaluated ...}
    procedure             _AssertCheckTransid    (const obj : IFRE_DB_Object ; const transkey : TFRE_DB_TransStepId);
  public
    procedure   Cleanup                       ; override;

    procedure   UpdateObjectByNotify          (const obj : IFRE_DB_Object ; const transkey : TFRE_DB_TransStepId);                                                                                             { notify step 1 }
    procedure   InsertObjectByNotify          (const coll_name : TFRE_DB_NameType ; const obj : IFRE_DB_Object ; const rl_ins : boolean ; const parent : TFRE_DB_GUID ; const transkey : TFRE_DB_TransStepId); { notify step 1 }
    procedure   RemoveObjectByNotify          (const coll_name : TFRE_DB_NameType ; const obj : IFRE_DB_Object ; const rl_rem : boolean ; const parent : TFRE_DB_GUID ; const transkey : TFRE_DB_TransStepId); { notify step 1 }

    { The reflink operations are ordered to occur before the update,insert,delete operations (remove should not do a reflink change(!) }
    procedure   SetupOutboundRefLink          (const from_obj : TFRE_DB_GUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL ; const transkey : TFRE_DB_TransStepId);          { notify step 1 }
    procedure   SetupInboundRefLink           (const from_obj : IFRE_DB_Object   ; const to_obj: TFRE_DB_GUID          ; const key_description : TFRE_DB_NameTypeRL ; const transkey : TFRE_DB_TransStepId);          { notify step 1 }
    procedure   InboundReflinkDropped         (const from_obj : IFRE_DB_Object   ; const to_obj: TFRE_DB_GUID          ; const key_description : TFRE_DB_NameTypeRL ; const transkey : TFRE_DB_TransStepId);          { notify step 1 }
    procedure   OutboundReflinkDropped        (const from_obj : TFRE_DB_GUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL ; const transkey : TFRE_DB_TransStepId);          { notify step 1 }


    function    ExistsObj                     (const uid:TFRE_DB_GUID):NativeInt;
    procedure   SetTransformedObject          (const tr_obj : IFRE_DB_Object);override;                           { inital fill, from initial transform }
    procedure   SetTransObjectSingleInsert    (const tr_obj : IFRE_DB_Object);override;                           { single fill from notify }

    procedure   HandleUpdateTransformedObject (const tr_obj : IFRE_DB_Object ; const upd_idx: NativeInt); override ;            { notify update, step 2 }
    procedure   HandleInsertTransformedObject (const tr_obj : IFRE_DB_Object ; const parent_object : IFRE_DB_Object);override;  { notify update, step 2 }
    procedure   HandleDeleteTransformedObject (const del_idx : NativeInt     ; const parent_object : IFRE_DB_Object ; transtag : TFRE_DB_TransStepId); { notify update, step 2 }

    procedure   TransformAll                  (var rcnt : NativeInt);                                             { transform all objects of the parent collection }
    constructor Create                        (const key : TFRE_DB_TRANS_COLL_DATA_KEY ; const dc : IFRE_DB_DERIVED_COLLECTION ; const db_name : TFRE_DB_NameType);
    destructor  Destroy                       ; override;
    procedure   ForAllObjs                    (const forall : TFRE_DB_Obj_Iterator);

    procedure   AddOrdering                   (const ordering : TFRE_DB_TRANSFORMED_ORDERED_DATA);
    procedure   RemoveOrdering                (const ordering : TFRE_DB_TRANSFORMED_ORDERED_DATA);
    function    GetTransFormKey               : TFRE_DB_TRANS_COLL_DATA_KEY;
    procedure   CheckIntegrity                ;
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
    procedure        SetFilled(AValue: boolean);
    procedure        ClearDataInFilter;
    function         UnconditionalRemoveOldObject (const td: TFRE_DB_TRANSFORMED_ORDERED_DATA ; const old_obj: IFRE_DB_Object ; const ignore_non_existing: boolean):NativeInt;
    function         UnconditionalInsertNewObject (const td: TFRE_DB_TRANSFORMED_ORDERED_DATA ; const new_obj: IFRE_DB_Object):NativeInt;

  public
    property    IsFilled                      : boolean read FFilled write SetFilled;
    procedure   CheckDBReevaluation           ;
    procedure   Execute                       (const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY ; const compare_run : boolean); { compare run = second run on filter updates }
    procedure   ExecutePointQuery             (const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY);
    procedure   CheckFilteredAdd              (const obj : IFRE_DB_Object);
    procedure   Notify_CheckFilteredUpdate    (const td  : TFRE_DB_TRANSFORMED_ORDERED_DATA ; const old_obj,new_obj : IFRE_DB_Object ; const order_changed : boolean);                { invoke session update }
    function    Notify_CheckFilteredDelete    (const td  : TFRE_DB_TRANSFORMED_ORDERED_DATA ; const old_obj         : IFRE_DB_Object) : NativeInt; { invoke session update }
    function    Notify_CheckFilteredInsert    (const td  : TFRE_DB_TRANSFORMED_ORDERED_DATA ; const new_obj         : IFRE_DB_Object) : NativeInt; { invoke session update }
    function    DoesObjectPassFilterContainer (const obj : IFRE_DB_Object):boolean;
    procedure   AdjustLength                  ;
    constructor Create                        (const order_key : TFRE_DB_NameType ; const filter_cont_dbname : TFRE_DB_NameType);
    destructor  Destroy                       ; override;
    function    Filters                       : TFRE_DB_DC_FILTER_DEFINITION;
    function    PurgeFilterDataDueToTimeout   : boolean;
    procedure   Checkintegrity                ;
  end;


  { TFRE_DB_TRANSFORMED_ORDERED_DATA }

  TFRE_DB_TRANSFORMED_ORDERED_DATA=class(TFRE_DB_TRANS_RESULT_BASE)
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
    procedure          Notify_DeleteFromTree (const old_obj : TFRE_DB_Object ; transtag : TFRE_DB_TransStepId);
    procedure          Notify_DeleteFromTree (const key: PByte; const keylen: NativeInt; const old_obj: TFRE_DB_Object ; const propagate_up : boolean = true ; const transtag : TFRE_DB_TransStepId = '');

    procedure          CheckUpdateFilterContainer(const qry_context : TFRE_DB_QUERY ; const compare_run : boolean);
  public
    procedure    LockBase                ; override;
    procedure    UnlockBase              ; override;
    constructor  Create                  (const orderdef : TFRE_DB_DC_ORDER_DEFINITION ; base_trans_data : TFRE_DB_TRANFORMED_DATA);
    destructor   Destroy                 ; override;
    procedure    Execute                 (const iter : IFRE_DB_Obj_Iterator ; const qry_context : TFRE_DB_QUERY ; const compare_run : boolean ; const point_qry : boolean=false);
    procedure    OrderTheData            ;
    function     GetFullKey              : TFRE_DB_TRANS_COLL_DATA_KEY; override;
    procedure    UpdateTransformedobject (const old_obj,new_object : IFRE_DB_Object);
    procedure    InsertTransformedobject (const new_object : IFRE_DB_Object);
    procedure    DeleteTransformedobject (const del_object : IFRE_DB_Object ; transtag : TFRE_DB_TransStepId);
    function     GetFiltersCount         : NativeInt;
    function     GetFilledFiltersCount   : NativeInt;
    procedure    CheckDoFilterPurge      ;
    procedure    DebugCheckintegrity     ;
  end;

  OFRE_DB_TransCollTransformedDataList = specialize OGFOS_Array<TFRE_DB_TRANFORMED_DATA>; { list of transformed collections }
  OFRE_DB_TransCollOrderedDataList     = specialize OGFOS_Array<TFRE_DB_TRANSFORMED_ORDERED_DATA>; { orders upon the data, referencing to the collection }


  TFRE_DB_TDM_STATS_CLEANER = class;
  TFRE_DB_TDM_NOTIFY_APPLY  = class;

  TFRE_DB_INBOUND_N_BLOCK = class
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
    FNotifyProc    : TFRE_DB_TDM_NOTIFY_APPLY;
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
    procedure   AssertCheckTransactionID (const obj : IFRE_DB_Object ; const transid : TFRE_DB_TransStepId);

    {NOFIF BLOCK INTERFACE}
    procedure  StartNotificationBlock (const key : TFRE_DB_TransStepId);
    procedure  FinishNotificationBlock(out block : IFRE_DB_Object);
    procedure  SendNotificationBlock  (const block : IFRE_DB_Object);
    procedure  CollectionCreated      (const coll_name: TFRE_DB_NameType  ; const in_memory_only : boolean ; const tsid : TFRE_DB_TransStepId);
    procedure  CollectionDeleted      (const coll_name: TFRE_DB_NameType  ; const tsid : TFRE_DB_TransStepId) ;
    procedure  IndexDefinedOnField    (const coll_name: TFRE_DB_NameType  ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean;
                                       const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean;
                                       const unique_null_values: boolean ; const tsid : TFRE_DB_TransStepId);
    procedure  IndexDroppedOnField    (const coll_name: TFRE_DB_NameType  ; const index_name: TFRE_DB_NameType ; const tsid : TFRE_DB_TransStepId);
    procedure  ObjectStored           (const coll_name: TFRE_DB_NameType  ; const obj : IFRE_DB_Object ; const tsid : TFRE_DB_TransStepId); { FULL STATE }
    procedure  ObjectDeleted          (const coll_names: TFRE_DB_NameTypeArray ; const obj : IFRE_DB_Object ; const tsid : TFRE_DB_TransStepId);                                      { FULL STATE }
    procedure  ObjectRemoved          (const coll_names: TFRE_DB_NameTypeArray ; const obj : IFRE_DB_Object ; const is_a_full_delete : boolean ; const tsid : TFRE_DB_TransStepId);  { FULL STATE }
    procedure  ObjectUpdated          (const obj : IFRE_DB_Object ; const colls:TFRE_DB_StringArray ; const tsid : TFRE_DB_TransStepId);    { FULL STATE }
    procedure  DifferentiallUpdStarts (const obj_uid   : IFRE_DB_Object ; const tsid : TFRE_DB_TransStepId);            { DIFFERENTIAL STATE}
    procedure  FieldDelete            (const old_field : IFRE_DB_Field  ; const tsid : TFRE_DB_TransStepId);            { DIFFERENTIAL STATE}
    procedure  FieldAdd               (const new_field : IFRE_DB_Field  ; const tsid : TFRE_DB_TransStepId);            { DIFFERENTIAL STATE}
    procedure  FieldChange            (const old_field,new_field : IFRE_DB_Field ; const tsid : TFRE_DB_TransStepId);   { DIFFERENTIAL STATE}
    procedure  DifferentiallUpdEnds   (const obj_uid   : TFRE_DB_GUID ; const tsid : TFRE_DB_TransStepId);              { DIFFERENTIAL STATE}
    procedure  SetupOutboundRefLink   (const from_obj : TFRE_DB_GUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL ; const tsid : TFRE_DB_TransStepId);
    procedure  SetupInboundRefLink    (const from_obj : IFRE_DB_Object   ; const to_obj: TFRE_DB_GUID          ; const key_description : TFRE_DB_NameTypeRL ; const tsid : TFRE_DB_TransStepId);
    procedure  InboundReflinkDropped  (const from_obj : IFRE_DB_Object   ; const to_obj: TFRE_DB_GUID          ; const key_description : TFRE_DB_NameTypeRL ; const tsid : TFRE_DB_TransStepId);
    procedure  OutboundReflinkDropped (const from_obj : TFRE_DB_GUID            ; const to_obj: IFRE_DB_Object ; const key_description : TFRE_DB_NameTypeRL ; const tsid : TFRE_DB_TransStepId);
    procedure  FinalizeNotif          ;
    {NOFIF BLOCK INTERFACE - END}
    function   DBC                    (const dblname : TFRE_DB_NameType) : IFRE_DB_CONNECTION;
    procedure  ChildObjCountChange    (const parent_obj : IFRE_DB_Object); { the child object count has changed, send an update on queries with P-C relation, where this object is in }
  public
    constructor Create        ;
    destructor  Destroy       ; override;
    procedure   LockManager   ; override;
    procedure   UnlockManager ; override;
    function    GetNewOrderDefinition     : TFRE_DB_DC_ORDER_DEFINITION_BASE; override ;
    function    GetNewFilterDefinition    (const filter_db_name : TFRE_DB_NameType)  : TFRE_DB_DC_FILTER_DEFINITION_BASE ; override;
    function    GetTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; var cd   : TFRE_DB_TRANS_RESULT_BASE):boolean; override;
    procedure   NewTransformedDataLocked  (const qry : TFRE_DB_QUERY_BASE ; const dc : IFRE_DB_DERIVED_COLLECTION ; var cd : TFRE_DB_TRANS_RESULT_BASE);override;
    {
     Generate the query spec from the JSON Webinput object
     dependency_reference_ids : this are the dependency keys that be considered to use from the JSON (usually one, input dependency)
     collection_transform_key : unique specifier of the DATA TRANSFORMATION defined by this collection, ORDERS derive from them
    }
    function   GenerateQueryFromQryDef    (const qry_def : TFRE_DB_QUERY_DEF):TFRE_DB_QUERY_BASE; override;

    { remember the query as open for the session }
    procedure   StoreQuery                (const qry: TFRE_DB_QUERY_BASE); override;
    { forget the query as open for the session }
    procedure   RemoveQuery               (const qry_id : TFRE_DB_NameType);override;
    { forget all querys for the session/dc }
    procedure   DropAllQuerys                        (const session_id : TFRE_DB_String ; const dc_name : TFRE_DB_NameTypeRL); override; { can be dc wide, or session wide dc_name='' }
    function    FormQueryID                          (const session_id : TFRE_DB_String ; const dc_name : TFRE_DB_NameTypeRL ; const client_part : int64):TFRE_DB_NameType;override;
    procedure   ApplyInboundNotificationBlock        (const dbname: TFRE_DB_NameType ; const block : IFRE_DB_Object);
    procedure   InboundNotificationBlock             (const dbname: TFRE_DB_NameType ; const block : IFRE_DB_Object); override;

    { --- Notify gathering }
    procedure   CN_AddDirectSessionUpdateEntry       (const update_dbo : IFRE_DB_Object); { add a dbo update for sessions dbo's (forms) }
    procedure   CN_AddGridInplaceUpdate              (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : Int64 ; const upo : IFRE_DB_Object);
    procedure   CN_AddGridInplaceDelete              (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : Int64 ; const del_id: TFRE_DB_String);
    procedure   CN_AddGridInsertUpdate               (const sessionid : TFRE_DB_NameType ; const store_id   : TFRE_DB_NameType ; const qry_id : Int64 ; const upo : IFRE_DB_Object ;  const reference_id: TFRE_DB_String);

    procedure   TagQueries4UpInsDel                  (const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const TransActionTag: TFRE_DB_TransStepId);
    procedure   UpdateChildCount_QRY                 (const qry : TFRE_DB_QUERY ; const idx : NativeInt);
    procedure   UpdateLiveStatistics                 (const stats : IFRE_DB_Object);override;
  end;

  { TFRE_DB_TDM_STATS_CLEANER }

  TFRE_DB_TDM_STATS_CLEANER=class(TThread)
  private
    FStatEvent : IFOS_TE;
    FTM        : TFRE_DB_TRANSDATA_MANAGER;
  public
    constructor Create    (const tm:TFRE_DB_TRANSDATA_MANAGER);
    destructor  Destroy   ;override;
    procedure   Terminate ;
    procedure   Execute   ;override;
  end;

  { TFRE_DB_TDM_NOTIFY_APPLY }

  TFRE_DB_TDM_NOTIFY_APPLY=class(TThread)
  private
    FNotifyEvent : IFOS_TE;
    FQ           : IFOS_LFQ;
    FTM          : TFRE_DB_TRANSDATA_MANAGER;
  public
    procedure    EnqueueBlock (const bl : TObject);
    constructor  Create       (const tm:TFRE_DB_TRANSDATA_MANAGER);
    destructor   Destroy      ;override;
    procedure    Terminate    ;
    procedure    Execute      ;override;
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

function  FREDB_BinaryFindIndexInSorted(const search_key : TFRE_DB_ByteArray ; const order_Key : TFRE_DB_NameType ; var before : NativeInt ; const object_array : IFRE_DB_ObjectArray ; var exists : boolean ; const exact_uid : PFRE_DB_Guid=nil ; const ignore_non_existing : boolean=false):NativeInt;
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
    end;

    function FindExactKey : NativeInt;
    begin
      {first go to beginning }
      if object_array[midx].UID=exact_uid^ then { quick bailout }
        exit(midx);
      repeat { go back }
        if (midx=0) then
          break;
        mkey := object_array[midx-1].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
        if Compare(search_key,mkey)=-0 then
          dec(midx)
        else
          break;
      until false;
      repeat
        if midx>high(object_array) then
          break;
        if object_array[midx].UID=exact_uid^ then { quick bailout }
          exit(midx);
        if Compare(search_key,mkey)=0 then
          inc(midx)
        else
          break;
      until false;
      if not ignore_non_existing then
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the exact uid was not found in the array')
      else
        exists := false;
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
  exists := false;
  while  leftx<=rightx do
    begin
       midx := leftx + ((rightx - leftx) div 2);
       mkey := object_array[midx].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_Key).AsByteArr;
       if Length(mkey)=0 then
         raise EFRE_DB_Exception.Create(edb_ERROR,'invalid order key len 0');
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
end;

{ TFRE_DB_TDM_NOTIFY_APPLY }

procedure TFRE_DB_TDM_NOTIFY_APPLY.EnqueueBlock(const bl: TObject);
begin
  FQ.Push(bl);
  FNotifyEvent.SetEvent;
end;

constructor TFRE_DB_TDM_NOTIFY_APPLY.Create(const tm: TFRE_DB_TRANSDATA_MANAGER);
begin
  FTM := tm;
  GFRE_TF.Get_TimedEvent(FNotifyEvent);
  GFRE_TF.Get_LFQ(FQ);
  Inherited create(false);
end;

destructor TFRE_DB_TDM_NOTIFY_APPLY.Destroy;
begin
  Terminate;
  WaitFor;
  FNotifyEvent.Finalize;
  inherited Destroy;
end;

procedure TFRE_DB_TDM_NOTIFY_APPLY.Terminate;
begin
  inherited Terminate;
  FNotifyEvent.SetEvent;
end;

procedure TFRE_DB_TDM_NOTIFY_APPLY.Execute;
var shot  : Boolean;
    block : TFRE_DB_INBOUND_N_BLOCK;
begin
  GFRE_LOG.RegisterThread('TDM_NT');
  repeat
    FNotifyEvent.WaitFor(1000);
    if not Terminated then
      begin
        repeat
          block := TFRE_DB_INBOUND_N_BLOCK(FQ.Pop);
          if block = nil then
            break;
          try
            FTM.ApplyInboundNotificationBlock('',block.FBlock);
            block.FBlock.Finalize;
            block.FBlock:=nil;
            block.Free;
            block:=nil;
          except
            on e:exception do
              begin
                GFRE_DBI.LogError(dblc_DBTDM,'APPLY INBOUND BLOCK/EVENT ERROR [%s]',[e.Message]);
                //exit; { DEBUG EXIT & STOP THREAD }
                break;
              end;
          end;
        until false;
      end
    else
      begin
        //writeln('FINAL ROUND');
      end;
  until Terminated;
end;

{ TFRE_DB_FILTER_AUTO_DEPENDENCY }

function TFRE_DB_FILTER_AUTO_DEPENDENCY.Clone: TFRE_DB_FILTER_BASE;
var fClone : TFRE_DB_FILTER_AUTO_DEPENDENCY;
begin
  fClone                    := TFRE_DB_FILTER_AUTO_DEPENDENCY.Create(FKey);
  fClone.FNegate            := FNegate;
  fClone.FFieldname         := FFieldname;
  fClone.FAllowNull         := FAllowNull;
  fClone.FStartValues       := Copy(FStartValues);
  fClone.FValues            := Copy(FValues);
  fClone.FRL_Spec           := Copy(FRL_Spec);
  fClone.FDBName            := FDBName;
  fClone.FNeedsDBReEvaluate := FNeedsDBReEvaluate;
  result                    := fClone;
end;

function TFRE_DB_FILTER_AUTO_DEPENDENCY.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
            if length(FStartValues)=0 then
              begin
                result := false;
              end
            else
              begin
                result := true;
                for i:=0 to high(fieldvals) do
                 for j:=0 to high(FValues) do
                   if FREDB_Guids_Same(fieldvals[i],FValues[j]) then
                     begin
                       result := false;
                       break;
                     end;
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

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.FilterExpandRefs(const dbc: IFRE_DB_CONNECTION);
begin
  SetLength(FValues,0);
  (dbc.Implementor as TFRE_DB_CONNECTION).ExpandReferencesNoRightCheck(FStartValues,FRL_Spec,FValues);
end;

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.InitFilter(const RL_Spec: TFRE_DB_NameTypeRLArray; const StartDependecyValues: TFRE_DB_GUIDArray; const negate: boolean; const include_null_values: boolean; const dbname: TFRE_DB_NameType);
begin
  if DBName='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'AutoFilter dbname not set !');
  FRL_Spec           := RL_Spec;
  FStartValues       := StartDependecyValues;
  FNegate            := negate;
  FAllowNull         := include_null_values;
  FDBName            := dbname;
  FNeedsDBReEvaluate := true;
  FFieldname         := 'UID';
end;

procedure TFRE_DB_FILTER_AUTO_DEPENDENCY.ReEvalFilterStartVals;
var db : IFRE_DB_CONNECTION;
begin
  db := G_TCDM.DBC(FDBName);
  FilterExpandRefs(db);
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
    FNeedsDBReEvaluate := true;
    //ReEvalFilterStartVals;
end;


{ TFRE_DB_SESSION_UPO }

function TFRE_DB_SESSION_UPO.GetUpdateStore(const store_id: shortstring): TFRE_DB_UPDATE_STORE_DESC;
begin
  result :=  FStoreList.Find(store_id) as TFRE_DB_UPDATE_STORE_DESC;
  if not Assigned(result) then
    begin
      result := TFRE_DB_UPDATE_STORE_DESC.create.Describe(store_id);
      FStoreList.Add(store_id,result);
    end;
end;

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

procedure TFRE_DB_SESSION_UPO.AddStoreUpdate(const store_id: TFRE_DB_NameType; const qid: Int64; const upo: IFRE_DB_Object);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st := GetUpdateStore(store_id);
  update_st.addUpdatedEntry(upo,qid);
end;

procedure TFRE_DB_SESSION_UPO.AddStoreInsert(const store_id: TFRE_DB_NameType; const qid: Int64; const upo: IFRE_DB_Object; const reference_id: TFRE_DB_String);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st := GetUpdateStore(store_id);
  update_st.addNewEntry(upo,qid,reference_id);
end;

procedure TFRE_DB_SESSION_UPO.AddStoreDelete(const store_id: TFRE_DB_NameType; const qid: Int64; const id: TFRE_DB_String);
var update_st : TFRE_DB_UPDATE_STORE_DESC;
begin
  update_st := GetUpdateStore(store_id);
  update_st.addDeletedEntry(id,qid);
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

constructor TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.Create(const key: TFRE_DB_TransStepId);
begin
  FSessionUpdateList := TFPHashObjectList.Create(true);
  FKey               := key;
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

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridInplaceUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const upo: IFRE_DB_Object);
begin
  GetSessionUPO(sessionid).AddStoreUpdate(store_id,qry_id,upo);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridInsertUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const upo: IFRE_DB_Object; const reference_id: TFRE_DB_String);
begin
  GetSessionUPO(sessionid).AddStoreInsert(store_id,qry_id,upo,reference_id);
end;

procedure TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.AddGridRemoveUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const del_id: TFRE_DB_String);
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
  GFRE_LOG.RegisterThread('TDM_S');
  repeat
    FStatEvent.WaitFor(1000);
    if not Terminated then
      FTM.TL_StatsTimer
    else
      begin
        //writeln('FINAL ROUND');
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

function TFRE_DB_FILTER_CHILD.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
begin
  result := FREDB_PP_ObjectInParentPath(obj,''); { is root (no parent) in array ?}
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

function TFRE_DB_FILTER_PARENT.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
begin
  result := FREDB_PP_ObjectInParentPathLastParent(obj,FAllowedParent);
end;

procedure TFRE_DB_FILTER_PARENT.InitFilter(const allowed_parent_path: TFRE_DB_GUIDArray);
begin
  FAllowedParent := FREDB_G2H(allowed_parent_path[0]); { currently only the immediate parent is used (client restriction) }
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

function TFRE_DB_FILTER_REAL64.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
    result := not((fieldval>lbnd) and (fieldval<ubnd));
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : Double;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := not((fieldval>=lbnd) and (fieldval<=ubnd));
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
          dbnf_EXACT:                result := not(fieldval= FValues[0]);
          dbnf_LESSER:               result := not(fieldval< FValues[0]);
          dbnf_LESSER_EQ:            result := not(fieldval<=FValues[0]);
          dbnf_GREATER:              result := not(fieldval> FValues[0]);
          dbnf_GREATER_EQ:           result := not(fieldval>=FValues[0]);
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

function TFRE_DB_FILTER_DATETIME.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
    result := not((fieldval>lbnd) and (fieldval<ubnd));
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : TFRE_DB_DateTime64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := not((fieldval>=lbnd) and (fieldval<=ubnd));
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
          dbnf_EXACT:                result := not(fieldval= FValues[0]);
          dbnf_LESSER:               result := not(fieldval< FValues[0]);
          dbnf_LESSER_EQ:            result := not(fieldval<=FValues[0]);
          dbnf_GREATER:              result := not(fieldval> FValues[0]);
          dbnf_GREATER_EQ:           result := not(fieldval>=FValues[0]);
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

procedure TFRE_DB_FILTER_DATETIME.InitFilter(const fieldname: TFRE_DB_NameType; filtervalues: array of TFRE_DB_DateTime64; const numfiltertype: TFRE_DB_NUM_FILTERTYPE; const negate: boolean; const include_null_values: boolean);
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

function TFRE_DB_FILTER_CURRENCY.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
    result := not((fieldval>lbnd) and (fieldval<ubnd));
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : Currency;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := not((fieldval>=lbnd) and (fieldval<=ubnd));
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
          dbnf_EXACT:                result := not(fieldval= FValues[0]);
          dbnf_LESSER:               result := not(fieldval< FValues[0]);
          dbnf_LESSER_EQ:            result := not(fieldval<=FValues[0]);
          dbnf_GREATER:              result := not(fieldval> FValues[0]);
          dbnf_GREATER_EQ:           result := not(fieldval>=FValues[0]);
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
  fClone                   := TFRE_DB_FILTER_RIGHT.Create(FKey);
  fClone.FRight            := FRight;
  fClone.FNegate           := FNegate;
  fClone.FUserTokenClone   := FUserTokenClone;
  fClone.FDomIDField       := FDomIDField;
  fClone.FSchemeclass      := FSchemeClass;
  fClone.FMode             := FMode;
  fClone.FSchemeClassField := FSchemeClassField;
  fClone.FObjUidField      := FObjUidField;
  fClone.FDomIDField       := FDomIDField;
  fClone.FRightSet         := copy(FRightSet);
  fClone.FIgnoreField      := FIgnoreField;
  fClone.FIgnoreValue      := FIgnoreValue;
  result                   := fClone;
end;

function TFRE_DB_FILTER_RIGHT.GetDefinitionKey: TFRE_DB_NameType;
var hsh : cardinal;
      i : NativeInt;
begin
  result :='';
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
  result := result+inttostr(ord(FMode))+':'+FSchemeclass+'-'+FDomIDField+'-'+FObjUidField+'-'+FSchemeClassField+':'+FUserTokenClone.GetUniqueTokenKey+'.'+FIgnoreField+'.'+FIgnoreValue;
  hsh := GFRE_BT.HashFast32(@result[1],length(result),0);
  for i:= 0 to high(FRightSet) do
    hsh := GFRE_BT.HashFast32(@FRightSet[i][1],Length(FRightSet[i]),hsh);
  result := 'Z:'+GFRE_BT.Mem2HexStr(@hsh,4);
end;

function TFRE_DB_FILTER_RIGHT.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var cn     : ShortString;
    fld    : IFRE_DB_Field;
    ivalue : TFRE_DB_String;

    function ReferedDomainCheck(const std : boolean):boolean;
    var domid  : TFRE_DB_GUID;
        objuid : TFRE_DB_GUID;
    begin
      try
        if (FDomIDField<>'') and obj.FieldOnlyExisting(FDomIDField,fld) then
          domid :=  fld.AsGUID
        else
          domid := CFRE_DB_NullGUID; { skip domain right tests to false (no right)}
        if (FObjUidField<>'') and obj.FieldOnlyExisting(FObjUidField,fld) then
          objuid := fld.AsGUID
        else
          objuid := CFRE_DB_NullGUID; { skip object right tests to false (no right)}
        if (FSchemeClassField<>'') then
          FSchemeclass := uppercase(obj.Field(FSchemeClassField).AsString);
        if std then
          result := FUserTokenClone.CheckStdRightSetUIDAndClass(objuid,domid,FSchemeclass,FRight)<>edb_OK { use negative (!) check in filters }
        else
          result := FUserTokenClone.CheckGenRightSetUIDAndClass(objuid,domid,FSchemeclass,FRightSet)<>edb_OK; { use negative (!) check in filters }
      except
        inc(flt_errors);
        result := true;
      end;
    end;

    function ObjectRightcheck:boolean;
    begin
      cn := obj.PreTransformedScheme;
      result := FUserTokenClone.CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,cn,FRight)<>edb_OK;
    end;

    function ObjectRightcheckGeneric:boolean;
    begin
      cn := obj.PreTransformedScheme;
      result := FUserTokenClone.CheckGenRightSetUIDAndClass(obj.UID,obj.DomainID,cn,FRightSet)<>edb_OK;
    end;


begin
  if (FIgnoreValue<>'') and obj.FieldOnlyExisting(FIgnoreField,fld) then
    begin
      try
        if FIgnoreValue=fld.AsString then
          exit(true); { do not respect negate }
      except
        inc(flt_errors);
        result := true; { get's negated }
      end;
    end;
  case FMode of
    fm_ObjectRightFilter:
      result := ObjectRightcheck;
    fm_ReferedRightFilter:
      result := ReferedDomainCheck(true);
    fm_ObjectRightFilterGeneric:
      result := ObjectRightcheckGeneric;
    fm_ReferedRightFilterGeneric:
      result := ReferedDomainCheck(false);
  end;
  result := (result xor FNegate); { invert result }
end;

procedure TFRE_DB_FILTER_RIGHT.InitFilter(stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreFieldname, ignoreFieldValue: TFRE_DB_String);
begin
  if stdrightset=[] then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one right must be specified for the filter');
  if not assigned(usertoken) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'a usertoken must be specified for the filter');
  FRight          := stdrightset;
  FUserTokenClone := usertoken;
  FNegate         := negate;
  FMode           := fm_ObjectRightFilter;
  FIgnoreField    := ignoreFieldname;
  FIgnoreValue    := ignoreFieldValue;
end;

procedure TFRE_DB_FILTER_RIGHT.InitFilterRefered(domainidfield, objuidfield, schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreFieldname, ignoreFieldValue: TFRE_DB_String);
begin
  if stdrightset=[] then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one right must be specified for the filter');
  if not assigned(usertoken) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'a usertoken must be specified for the filter');
  FRight            := stdrightset;
  FUserTokenClone   := usertoken;
  FNegate           := negate;
  FMode             := fm_ReferedRightFilter;
  FSchemeclass      := schemeclass;
  FDomIDField       := domainidfield;
  FObjUidField      := objuidfield;
  FSchemeClassField := schemeclassfield;
  FIgnoreField      := ignoreFieldname;
  FIgnoreValue      := ignoreFieldValue;
end;

procedure TFRE_DB_FILTER_RIGHT.InitFilterGenRights(const stdrightset: array of TFRE_DB_String; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreFieldname, ignoreFieldValue: TFRE_DB_String);
var i : NativeInt;
begin
  if Length(stdrightset)=0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one right must be specified for the filter');
  SetLength(FRightSet,Length(stdrightset));
  for i := 0 to high(stdrightset) do
    FRightSet[i] := stdrightset[i];
  if not assigned(usertoken) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'a usertoken must be specified for the filter');
  FRight          := [];
  FUserTokenClone := usertoken;
  FNegate         := negate;
  FMode           := fm_ObjectRightFilterGeneric;
  FIgnoreField    := ignoreFieldname;
  FIgnoreValue    := ignoreFieldValue;
end;

procedure TFRE_DB_FILTER_RIGHT.InitFilterGenRightsRefrd(const domainidfield, objuidfield, schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; stdrightset: array of TFRE_DB_String; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreFieldname, ignoreFieldValue: TFRE_DB_String);
var i : NativeInt;
begin
  if Length(stdrightset)=0 then
    raise EFRE_DB_Exception.Create(edb_ERROR,'at least one right must be specified for the filter');
  SetLength(FRightSet,Length(stdrightset));
  for i := 0 to high(stdrightset) do
    FRightSet[i] := stdrightset[i];
  if not assigned(usertoken) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'a usertoken must be specified for the filter');
  FRight            := [];
  FUserTokenClone   := usertoken;
  FNegate           := negate;
  FMode             := fm_ReferedRightFilterGeneric;
  FSchemeclass      := schemeclass;
  FDomIDField       := domainidfield;
  FObjUidField      := objuidfield;
  FSchemeClassField := schemeclassfield;
  FIgnoreField      := ignoreFieldname;
  FIgnoreValue      := ignoreFieldValue;
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

function TFRE_DB_FILTER_SCHEME.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var
  cn: ShortString;
  i : NativeInt;
begin
  cn := obj.PreTransformedScheme;
  result := true;
  for i:= 0 to high(FValues) do begin //self
    if cn=FValues[i] then begin
      Result:=false;
      break;
    end;
  end;
  result := (result xor FNegate); { invert result }
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
    FValues[i] := UpperCase(filtervalues[i]);
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

function TFRE_DB_FILTER_UID.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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

function TFRE_DB_FILTER_BOOLEAN.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
        result        := not(fieldval=FValue);
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

function TFRE_DB_FILTER_UNSIGNED.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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

function TFRE_DB_FILTER_SIGNED.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
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
    result := not ((fieldval>lbnd) and (fieldval<ubnd));
  end;

  procedure DoWithBounds;
  var lbnd,ubnd : int64;
  begin
    lbnd   := FValues[0];
    ubnd   := FValues[1];
    result := not ((fieldval>=lbnd) and (fieldval<=ubnd));
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
          dbnf_EXACT:                result := not (fieldval= FValues[0]);
          dbnf_LESSER:               result := not (fieldval< FValues[0]);
          dbnf_LESSER_EQ:            result := not (fieldval<=FValues[0]);
          dbnf_GREATER:              result := not (fieldval> FValues[0]);
          dbnf_GREATER_EQ:           result := not (fieldval>=FValues[0]);
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

function TFRE_DB_FILTER_STRING.CheckFilterMiss(const obj: IFRE_DB_Object; var flt_errors: Int64): boolean;
var fieldval       : TFRE_DB_String;
     fld           : IFRE_DB_Field;
     multivalfield : boolean;
     error_fld     : boolean;
     filterVal     : TFRE_DB_String;
begin
  error_fld := false;
  filterVal := FValues[0];

  if obj.FieldOnlyExisting(FFieldname,fld) then
    begin
      multivalfield := fld.ValueCount>1;
      try
        fieldval      := fld.AsString;
        case FFilterType of
          dbft_EXACT:      result := not (((length(fieldval)=0) or FOS_AnsiContainsText(fieldval,filterVal)) and (length(fieldval)=length(filterVal)));
          dbft_PART:       result := not FOS_AnsiContainsText(fieldval,filterVal);
          dbft_STARTPART:  result := not FOS_AnsiStartsText  (filterVal,fieldval);
          dbft_ENDPART:    result := not FOS_AnsiEndsText    (filterVal,fieldval);
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

procedure TFRE_DB_FilterContainer.ClearDataInFilter;
begin
  SetLength(FOBJArray,0); { only references ...}
  FFilled := false;
  FCnt    := 0;
end;

function TFRE_DB_FilterContainer.UnconditionalRemoveOldObject(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const old_obj: IFRE_DB_Object; const ignore_non_existing: boolean): NativeInt;
var old_idx   : NativeInt;
    before    : NativeInt;
    old_key   : TFRE_DB_ByteArray;
    exists    : boolean;
begin
  if IsFilled then
    begin
      old_key  := old_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr;
      old_idx := -1;
      //for k := 0 to high(FOBJArray) do
      // begin
      //   if FOBJArray[k].UID=old_obj.UID then
      //     old_idx := k;
      // end;
      //uids := old_obj.UID_String;
      old_idx  := FREDB_BinaryFindIndexInSorted(old_key,ForderKey,before,FOBJArray,exists,old_obj.PUID,true);
      result   := old_idx;
      if exists then
        begin
          FOBJArray := FREDB_RemoveIdxFomObjectArray(FOBJArray,old_idx);
          dec(FCnt);
          AdjustLength;
        end
      else
        if ignore_non_existing=false then
          raise EFRE_DB_Exception.Create(edb_ERROR,'notify filtered delete / not found');
    end
end;

function TFRE_DB_FilterContainer.UnconditionalInsertNewObject(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const new_obj: IFRE_DB_Object): NativeInt;
var ia_idx   : NativeInt;
    before   : NativeInt;
    exists   : boolean;
    i        : NativeInt;
    new_key  : TFRE_DB_ByteArray;

begin
  if IsFilled then
    begin
      new_key  := new_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr;
      if Length(new_key)=0 then
        raise EFRE_DB_Exception.Create(edb_ERROR,'total order key / binary key not found in insert');
      ia_idx := FREDB_BinaryFindIndexInSorted(new_key,ForderKey,before,FOBJArray,exists);
      FOBJArray := FREDB_InsertAtIdxToObjectArray(FOBJArray,ia_idx,new_obj,before>0); { ia_idx gets adjusted }
      result := ia_idx;
      inc(FCnt);
    end;
end;

procedure TFRE_DB_FilterContainer.CheckDBReevaluation;
begin
  if FFilters.CheckDBReevaluationFilters then
    ClearDataInFilter;
end;

function TFRE_DB_FilterContainer.DoesObjectPassFilterContainer(const obj: IFRE_DB_Object): boolean;
begin
  result := FFilters.DoesObjectPassFilters(obj);
end;

procedure TFRE_DB_FilterContainer.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY; const compare_run: boolean);
var i   : NativeInt;
    obj : IFRE_DB_Object;
    hio : NativeInt;
begin
  if not compare_run then
    begin
      qry_context.FQueryPotentialCount := FCnt;
      qry_context.SetMaxResultDBOLen(false);
      if assigned(iter) then
        begin
          hio := High(FOBJArray);
          for i:=qry_context.FStartIdx to hio do
            begin
              obj := FOBJArray[i].CloneToNewObject();
              iter(obj);
              qry_context.SetResultObject(qry_context.FQueryCurrIdx,obj,false);
              inc(qry_context.FQueryCurrIdx);
              inc(qry_context.FQueryDeliveredCount);
              if qry_context.FQueryDeliveredCount=qry_context.FToDeliverCount then
                break;
            end;
          if (qry_context.FStartIdx+qry_context.FQueryCurrIdx)=Length(FOBJArray) then
            qry_context.FQueryIsLastPage := true
          else
            qry_context.FQueryIsLastPage := false;
          qry_context.AddjustResultDBOLen(false);
        end;
    end
  else
    begin
      qry_context.FQueryPotentialCountCmp := FCnt; // self.FCnt
      qry_context.SetMaxResultDBOLen(true);
      hio := High(FOBJArray);
      for i:=qry_context.FStartIdx to hio do
        begin
          obj := FOBJArray[i];
          qry_context.SetResultObject(qry_context.FQueryCurrIdxCmp,obj,true);
          inc(qry_context.FQueryCurrIdxCmp);
          inc(qry_context.FQueryDeliveredCountCmp);
          if (qry_context.FQueryDeliveredCountCmp=qry_context.FToDeliverCount) then
            if (qry_context.FQueryIsLastPage) then
              begin
                inc(qry_context.FToDeliverCount);     { Direspect "todelivercount" on last page, to gather items inserted in the last page }
                qry_context.SetMaxResultDBOLen(true); { expand resultset }
              end
            else
              break;
        end;
      qry_context.AddjustResultDBOLen(true);
    end;
end;

procedure TFRE_DB_FilterContainer.ExecutePointQuery(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY);
var i   : NativeInt;
    obj : IFRE_DB_Object;
    hio : NativeInt;
begin
  qry_context.FQueryPotentialCount := 1;
  qry_context.FToDeliverCount      := 1;
  qry_context.SetMaxResultDBOLen(false);
  hio := High(FOBJArray);
  if qry_context.FUidPointQry then
    begin
      for i := 0 to high(FOBJArray) do
       begin
         if FOBJArray[i].UID=qry_context.FOnlyOneUID then
           begin
             obj := FOBJArray[i].CloneToNewObject;
             qry_context.SetResultObject(0,obj,false);
             qry_context.FQueryDeliveredCount := 1;
             if assigned(iter) then
               iter(obj);
             break;
           end;
       end;
    end
  else
    case qry_context.FStartIdx of
      -1 : begin
             if Length(FOBJArray)>0 then
               begin
                 obj := FOBJArray[0].CloneToNewObject;
                 qry_context.SetResultObject(0,obj,false);
                 qry_context.FQueryDeliveredCount := 1;
                 if assigned(iter) then
                   iter(obj);
               end;
           end;
      -2 : begin
             if (Length(FOBJArray)>0) then
               begin
                 obj := FOBJArray[high(FOBJArray)].CloneToNewObject;
                 qry_context.SetResultObject(0,obj,false);
                 qry_context.FQueryDeliveredCount := 1;
                 if assigned(iter) then
                   iter(obj);
               end;
           end
      else
        begin
          if (Length(FOBJArray)>0) and (qry_context.FStartIdx<Length(FOBJArray)) then
            begin
              obj := FOBJArray[qry_context.FStartIdx].CloneToNewObject;
              qry_context.SetResultObject(0,obj,false);
              qry_context.FQueryDeliveredCount := 1;
              if assigned(iter) then
                iter(obj);
            end;
        end;
    end;
end;


procedure TFRE_DB_FilterContainer.CheckFilteredAdd(const obj: IFRE_DB_Object);
begin
  if FCnt=Length(FOBJArray) then
    SetLength(FOBJArray,Length(FOBJArray)+cFRE_INT_TUNE_SYSFILTEXTENSION_SZ);
  if DoesObjectPassFilterContainer(obj) then
    begin
      FOBJArray[FCnt] := obj;
      inc(FCnt);
    end;
end;

procedure TFRE_DB_FilterContainer.Notify_CheckFilteredUpdate(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const old_obj, new_obj: IFRE_DB_Object; const order_changed: boolean);
var old_idx   : NativeInt;
    new_idx   : NativeInt;
    before    : NativeInt;
    exists    : boolean;
    i         : NativeInt;
begin
  if not IsFilled then
    exit;
  if DoesObjectPassFilterContainer(new_obj) then
    begin { Update object does pass the filters}
      if order_changed then { order has potentially changed, key has changed / len value / but order may not have been changed in filtering }
        begin
          old_idx := Notify_CheckFilteredDelete(td,old_obj);
          new_idx := Notify_CheckFilteredInsert(td,new_obj);
        end
      else
        begin
          //GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH UPDATE OBJECT ORDER NOT CHANGED [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
          old_idx := FREDB_BinaryFindIndexInSorted(old_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(ForderKey).AsByteArr,Forderkey,before,FOBJArray,exists);
          if exists=false then
            begin
              { the object passes the filter but is not in the filter, add it }
              //UnconditionalInsertNewObject(td,new_obj,false);
            end
          else
            begin
              FOBJArray[old_idx] := new_obj;
              //G_TCDM.UpdateObjectInFilterKey(td,self,new_obj,old_idx);  { in place update }
            end;
        end;
    end
  else
    begin { Update object does not pass the filters anymore -> remove it}
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER REJECT UPDATE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
      old_idx := UnconditionalRemoveOldObject(td,old_obj,true); { ignore non existing objects ...}
    end;
end;

function TFRE_DB_FilterContainer.Notify_CheckFilteredDelete(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const old_obj: IFRE_DB_Object): NativeInt;
begin
  if not IsFilled then
    exit;
  result := -1;
  if DoesObjectPassFilterContainer(old_obj) then
    begin { old object is potentially in the client query present }
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH DELETE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
      result := UnconditionalRemoveOldObject(td,old_obj,false);
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER REJECT DELETE OBJECT [%s] IN ORDER/FILTER [%s / %s]',[old_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
    end;
end;


function TFRE_DB_FilterContainer.Notify_CheckFilteredInsert(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA; const new_obj: IFRE_DB_Object): NativeInt;
begin
  if not IsFilled then
    exit;
  result := -1;
  if DoesObjectPassFilterContainer(new_obj) then
    begin { old object is potentially in the client query present }
      GFRE_DBI.LogDebug(dblc_DBTDM,'     >FILTER MATCH INSERT OBJECT [%s] IN ORDER/FILTER [%s / %s]',[new_obj.UID_String,td.GetFullKey.orderkey,Filters.GetFilterKey]);
      result := UnconditionalInsertNewObject(td,new_obj);
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

constructor TFRE_DB_FilterContainer.Create(const order_key: TFRE_DB_NameType; const filter_cont_dbname: TFRE_DB_NameType);
begin
  inherited Create;
  FCnt := 0;
  FFCCreationTime := GFRE_DT.Now_UTC;
  FFilters        := TFRE_DB_DC_FILTER_DEFINITION.Create(filter_cont_dbname);
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

function TFRE_DB_FilterContainer.PurgeFilterDataDueToTimeout: boolean;
var ntime : TFRE_DB_DateTime64;
begin
  if not FFilled then
    exit;
  ntime := GFRE_DT.Now_UTC;
  if (cFRE_INT_TUNE_FILTER_PURGE_TO<>0) and ((ntime-FFCCreationTime) > cFRE_INT_TUNE_FILTER_PURGE_TO) then
    begin
      ClearDataInFilter;
      GFRE_DB.LogDebug(dblc_DBTDM,' ORDER/FILTER [%s/%s] PURGED / TIMEOUT (%d ms) ',[ForderKey,FFilters.GetFilterKey,cFRE_INT_TUNE_FILTER_PURGE_TO]);
      exit(true);
    end;
  result := false;
end;

procedure TFRE_DB_FilterContainer.Checkintegrity;
var  i   : NativeInt;
     obj : IFRE_DB_Object;
begin
  try
    for i := 0 to high(FOBJArray) do
      begin
        obj := FOBJArray[i].CloneToNewObject();
        obj.Finalize;
        obj := FOBJArray[i].CloneToNewObject();
        obj.Finalize;
        obj := FOBJArray[i].CloneToNewObject();
        obj.Finalize;
      end;
  except
    on E:Exception do
      begin
        writeln('--- FILTER INTEGRITY CHECK FAILED ',e.Message,' ',ForderKey,' ',FFilters.GetFilterKey);
      end;
  end;
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
  FFilterMiss := not flt.CheckFilterMiss(tob,FFilterErr);
end;

constructor TFRE_DB_DC_FILTER_DEFINITION.Create(const filter_dbname: TFRE_DB_NameType);
begin
  FKeyList       := TFPHashObjectList.Create(true);
  FFiltDefDBname := filter_dbname;
  if FFiltDefDBname='' then
    raise EFRE_DB_Exception.Create(edb_ERROR,'dbname not set in create filter def');
end;

destructor TFRE_DB_DC_FILTER_DEFINITION.Destroy;
begin
  FKeyList.Free;
  inherited Destroy;
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddFilters(const source: TFRE_DB_DC_FILTER_DEFINITION_BASE; const clone: boolean);
var src : TFRE_DB_DC_FILTER_DEFINITION;
begin
  if not assigned(source) then
    exit;
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

procedure TFRE_DB_DC_FILTER_DEFINITION.AddStdRightObjectFilter(const key: TFRE_DB_NameType; stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');
var filt : TFRE_DB_FILTER_RIGHT;
begin
  filt := TFRE_DB_FILTER_RIGHT.Create(key);
  filt.InitFilter(stdrightset,usertoken,negate,ignoreField,ignoreValue);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddStdClassRightFilter(const key: TFRE_DB_NameType; domainidfield,objuidfield,schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; stdrightset: TFRE_DB_STANDARD_RIGHT_SET; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');
var filt : TFRE_DB_FILTER_RIGHT;
begin
  filt := TFRE_DB_FILTER_RIGHT.Create(key);
  filt.InitFilterRefered(domainidfield,objuidfield,schemeclassfield,schemeclass,stdrightset,usertoken,negate,ignoreField,ignoreValue);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddObjectRightFilter(const key: TFRE_DB_NameType; rightset: array of TFRE_DB_String; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');
var filt : TFRE_DB_FILTER_RIGHT;
begin
  filt := TFRE_DB_FILTER_RIGHT.Create(key);
  filt.InitFilterGenRights(rightset,usertoken,negate,ignoreField,ignoreValue);
  AddFilter(filt,false);
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.AddClassRightFilter(const key: TFRE_DB_NameType; domainidfield, objuidfield, schemeclassfield: TFRE_DB_NameType; schemeclass: TFRE_DB_NameType; rightset: array of TFRE_DB_String; const usertoken: IFRE_DB_USER_RIGHT_TOKEN; const negate: boolean; const ignoreField:TFRE_DB_NameType=''; const ignoreValue:TFRE_DB_String='');
var filt : TFRE_DB_FILTER_RIGHT;
begin
  filt := TFRE_DB_FILTER_RIGHT.Create(key);
  filt.InitFilterGenRightsRefrd(domainidfield,objuidfield,schemeclassfield,schemeclass,rightset,usertoken,negate,ignoreField,ignoreValue);
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

procedure TFRE_DB_DC_FILTER_DEFINITION.AddAutoDependencyFilter(const key: TFRE_DB_NameType; const RL_Spec: array of TFRE_DB_NameTypeRL; const StartDependecyValues: array of TFRE_DB_GUID; const one_value: boolean; const include_null_values: boolean);
var filt : TFRE_DB_FILTER_AUTO_DEPENDENCY;
    rla  : TFRE_DB_NameTypeRLArray;
    ga   : TFRE_DB_GUIDArray;
    i    : NativeInt;
begin
  filt := TFRE_DB_FILTER_AUTO_DEPENDENCY.Create(key);
  SetLength(rla,Length(RL_Spec));
  for i := 0 to high(RL_Spec) do
    rla[i] := RL_Spec[i];
  SetLength(ga,Length(StartDependecyValues));
  for i := 0 to high(StartDependecyValues) do
    ga[i] := StartDependecyValues[i];
  filt.InitFilter(rla,ga,not one_value,include_null_values,FFiltDefDBname);
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

procedure TFRE_DB_DC_FILTER_DEFINITION.RemoveAllFilters;
begin
  FKeyList.Clear;
end;

procedure TFRE_DB_DC_FILTER_DEFINITION.RemoveAllFiltersPrefix(const key_prefix: TFRE_DB_NameType);
var idx : NativeInt;

    function findprefix:boolean;
    var i    : NativeInt;
        filt : TFRE_DB_FILTER_BASE;
        key  : TFRE_DB_NameType;
    begin
      for i:= 0 to FKeyList.Count-1 do
        begin
          filt := FKeyList.Items[i] as TFRE_DB_FILTER_BASE;
          key  := filt.GetKeyName;
          if pos(key_prefix,key)=1 then
            begin
              idx:=i;
              exit(true);
            end;
        end;
      idx:=-1;
      result := false;
    end;

begin
  while findprefix do
    FKeyList.Delete(idx);
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

function TFRE_DB_DC_FILTER_DEFINITION.CheckDBReevaluationFilters: boolean;
var i : NativeInt;
    f : TFRE_DB_FILTER_BASE;
begin
  result := false;
  for i := 0 to FKeyList.Count-1 do
    begin
      f := FKeyList.Items[i] as TFRE_DB_FILTER_BASE;
      if f.FilterNeedsDbUpdate then
        begin
          f.ReEvalFilterStartVals;
          result := true;
        end;
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
  if Result then
    begin

    end;
end;

function TFRE_DB_DC_FILTER_DEFINITION.FilterDBName: TFRE_DB_NameType;
begin
  result := FFiltDefDBname;
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

procedure TFRE_DB_DC_ORDER_DEFINITION.AssignFrom(const orderdef: TFRE_DB_DC_ORDER_DEFINITION_BASE);

  procedure Copy(const order:TFRE_DB_DC_ORDER);
  begin
    with order do
      AddOrderDef(order_field,ascending,case_insensitive);
  end;

begin
  ClearOrders;
  (orderdef as TFRE_DB_DC_ORDER_DEFINITION).ForAllOrders(@Copy);
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
            TFRE_DB_TextIndex.TransformToBinaryComparable(fld.Implementor as TFRE_DB_FIELD,Key[max_key_len],keylen,order.case_insensitive,not order.ascending)
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

function TFRE_DB_DC_ORDER_DEFINITION.OrderCount: NativeInt;
begin
  result := Length(FOrderList);
end;


procedure TFRE_DB_DC_ORDER_DEFINITION.ClearOrders;
begin
  SetLength(FOrderList,0);
end;

procedure TFRE_DB_DC_ORDER_DEFINITION.AddOrderDef(const orderfield_name: TFRE_DB_NameType; const asc: boolean; const case_insens: boolean);
begin
  MustNotBeSealed;
  SetLength(FOrderList,Length(FOrderList)+1);
  with FOrderList[high(FOrderList)] do
    begin
      ascending        := asc;
      order_field      := orderfield_name;
      case_insensitive := case_insens;
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
  result := FQueryFilters;
end;

function TFRE_DB_QUERY.GetOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION;
begin
  result := FOrderDef;
end;

procedure TFRE_DB_QUERY.StartQueryRun(const compare_run: boolean);
begin
  if not compare_run then
    begin
      FQueryRunning        := true;
      FQueryCurrIdx        := 0;
      FQueryDeliveredCount := 0;
      FQueryStartTime      := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogDebug(dblc_QUERY,'QUERY ID [%s] start',[FQueryId])
    end
  else
    begin
      FQueryRunningCmp        := true;
      FQueryCurrIdxCmp        := 0;
      FQueryDeliveredCountCmp := 0;
      FQueryStartTimeCmp      := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogDebug(dblc_QUERY,'COMPARE/QUERY ID [%s] start',[FQueryId])
    end;
end;

procedure TFRE_DB_QUERY.EndQueryRun(const compare_run: boolean);
begin
  if not compare_run then
    begin
      FQueryEndTime := GFRE_BT.Get_Ticks_ms;
      FQueryRunning := False;
      GFRE_DBI.LogInfo(dblc_QUERY,'QUERY ID [%s] finished in %d ms',[FQueryId,FQueryEndTime-FQueryStartTime])
    end
  else
    begin
      FQueryEndTimeCmp := GFRE_BT.Get_Ticks_ms;
      FQueryRunningCmp := False;
      GFRE_DBI.LogInfo(dblc_QUERY,'COMPARE/QUERY ID [%s] finished in %d ms',[FQueryId,FQueryEndTimeCmp-FQueryStartTimeCmp]);
    end;
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

procedure TFRE_DB_QUERY.SwapCompareQueryQry;
var i : NativeInt;
begin
  for i := 0 to High(FResultDBOs) do
   begin
     FResultDBOs[i].Finalize;
   end;
  FResultDBOs             := copy(FResultDBOsCompare);
  FQueryCurrIdx           := FQueryCurrIdxCmp;
  FQueryDeliveredCount    := FQueryDeliveredCountCmp;
  FQueryPotentialCount    := FQueryPotentialCountCmp;
  SetLength(FResultDBOsCompare,0);
end;

constructor TFRE_DB_QUERY.Create(const qry_dbname: TFRE_DB_NameType);
begin
  FQCreationTime := GFRE_DT.Now_UTC;
  FQryDBname     := qry_dbname;
  FOrderDef      := TFRE_DB_DC_ORDER_DEFINITION.Create;
  FQueryFilters  := TFRE_DB_DC_FILTER_DEFINITION.Create(qry_dbname);
end;

destructor TFRE_DB_QUERY.Destroy;
begin
  FQueryFilters.free;
  {FOrderDef.Free; dont free orderdef it's used in the orders TODO: CHECK}
  GFRE_DBI.LogDebug(dblc_DBTDM,'  <> FINALIZE QRY [%s]',[GetQueryID]);
  inherited Destroy;
end;

function TFRE_DB_QUERY.GetQueryID: TFRE_DB_NameType;
begin
  result := FQueryId;
end;

function TFRE_DB_QUERY.GetQueryID_ClientPart: Int64;
begin
  result := FQueryClientID;
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
  FBaseData  := basedata as TFRE_DB_TRANSFORMED_ORDERED_DATA;
  FSessionId := session_id;
end;

procedure TFRE_DB_QUERY.UnlockBaseData;
begin
  FBaseData.UnlockBase;
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
  try
    if not compare_run then
      FResultDBOs[idx] := obj.CloneToNewObject { objects will be freed on updated/inser/deleted -> references are invalid then }
    else
      FResultDBOsCompare[idx] := obj.CloneToNewObject
  except
    on e:exception do
      begin
        GFRE_DBI.LogError(dblc_DBTDM,'INTERNAL - SetResultObject Failed '+e.Message);
        raise;
      end;
  end;
end;

procedure TFRE_DB_QUERY.AddjustResultDBOLen(const compare_run: boolean);
begin
  if not compare_run then
    SetLength(FResultDBOs,FQueryDeliveredCount)
  else
    SetLength(FResultDBOsCompare,FQueryDeliveredCountCmp);
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

procedure TFRE_DB_QUERY.ExecutePointQuery(const iterator: IFRE_DB_Obj_Iterator);
begin
  if not assigned(FBaseData) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'no base data available');
  StartQueryRun(false);
  FBaseData.Execute(iterator,self,false,true);
  EndQueryRun(false);
end;

function TFRE_DB_QUERY.CheckAutoDependencyFilterChanges(const key_description: TFRE_DB_NameTypeRL): boolean;
begin
  result := FFilterContainer.Filters.CheckAutoDependencyFilter(key_description);
end;

procedure TFRE_DB_QUERY.ProcessFilterChangeBasedUpdates;
var diff,d1,d2 : NativeInt;
    upo        : IFRE_DB_Object;

  function FinalTransFormUPO(const new_obj : IFRE_DB_Object):boolean;
  var ses : TFRE_DB_UserSession;
  begin
    result := false;
    if not GFRE_DBI.NetServ.FetchSessionByIdLocked(FSessionID,ses) then
      begin
        GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] NOT FOUND ON UPDATE/INSERT QRY(?)',[FSessionID]);
        exit;
      end
    else
      begin
        try
          upo := new_obj.CloneToNewObject;
          FBaseData.FBaseTransData.FDC.FinalRightTransform(ses,upo);
          result := true;
        finally
          ses.UnlockSession;
        end;
      end;
  end;

  procedure FullUpdate;
  var WorkArray   : IFRE_DB_ObjectArray;
      UpdateArray : IFRE_DB_ObjectArray;
      upcount     : NativeInt;
      i,j         : NativeInt;
      key         : TFRE_DB_ByteArray;
      order_key   : TFRE_DB_NameType;
      before      : NativeInt;
      exists      : Boolean;
      cnt         : Nativeint;
      idx         : NativeInt;
      refid       : string;
      otag,
      ntag        : TFRE_DB_TransStepId;
      okey        : TFRE_DB_ByteArray;
      nkey        : TFRE_DB_ByteArray;
      comp        : SizeInt;
      tsid        : TFRE_DB_TransStepId;
      tsid2       : TFRE_DB_TransStepId;

      procedure AddToUpdateArray(const obj : IFRE_DB_Object);
      begin
        if Length(UpdateArray)=upcount then
          SetLength(UpdateArray,upcount+25);
        UpdateArray[upcount]:=obj;
        inc(upcount);
      end;

  begin
     { Delete  all Missing }
     cnt := 0;
     SetLength(WorkArray,Length(FResultDBOs));   { The work array has the same size as the original result set }
     upcount:=0;
     for i := 0 to high(FResultDBOs) do          { Do for every Object that was in the Result set of the query }
       begin
         exists := false;
         for j:=0 to High(FResultDBOsCompare) do { Check if the object still exists in the new result set, or if the order has changed (!) }
           begin
             if FResultDBOs[i].UID=FResultDBOsCompare[j].uid then
               begin
                 otag  := FResultDBOs[i].Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
                 ntag  := FResultDBOsCompare[j].Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
                 if ntag<>otag then { object has a new transaction tag, thus was modified }
                   begin
                     order_key := FFilterContainer.ForderKey;
                     nkey      := FResultDBOsCompare[j].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_key).AsByteArr;
                     okey      := FResultDBOs       [i].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_key).AsByteArr;
                     comp      := CompareByte(nkey[0],okey[0],Length(nkey));
                     if (Length(nkey)<>Length(okey)) or
                         (comp<>0) then
                       begin
                         exists := false;                                                     { Fake that the object does not exist -> Remove the object (possible order change), }
                         FResultDBOs[i].Field(cFRE_DB_SYS_TAG_ORDER_CHANGED).AsBoolean:=true; { but make a TAG, to remebber that fact }
                       end
                     else
                       begin
                         exists := true;
                         AddToUpdateArray(FResultDBOsCompare[j]);                             { only add to update array if the object has no delete/insert cycle }
                       end;
                   end
                 else
                   exists := true; { object not updated by a transaction, so it exists and the order has not changed }
                 break;
               end;
           end;
         if exists then
           begin
             WorkArray[cnt] := FResultDBOs[i]; { object is still in the result set }
             inc(cnt);
           end
         else
           begin
             G_TCDM.CN_AddGridInplaceDelete(FSessionID,GetStoreID,GetQueryID_ClientPart,FResultDBOs[i].UID_String); { send delete for non existing object, or to change the order }
           end;
       end;
     SetLength(WorkArray,cnt); { objects not in the result are now deleted }
     SetLength(UpdateArray,upcount);
     { Next send Updates }
     for i := 0 to high(UpdateArray) do
      begin
        if FinalTransFormUPO(UpdateArray[i]) then
          G_TCDM.CN_AddGridInplaceUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo);
      end;
     { Next insert New Objects }
     for i := 0 to high(FResultDBOsCompare) do
       begin
         exists := false;
         for j:=0 to High(FResultDBOs) do
           begin
             if  FResultDBOs[j].UID=FResultDBOsCompare[i].uid then
               begin
                 if FResultDBOs[j].FieldExists(cFRE_DB_SYS_TAG_ORDER_CHANGED) then
                   exists := false
                 else
                   exists :=true;
                 break;
               end;
           end;
         if not exists then
           begin { insert }
             order_key := FFilterContainer.ForderKey;
             key := FResultDBOsCompare[i].Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(order_key).AsByteArr;
             idx := FREDB_BinaryFindIndexInSorted(key,order_key,before,WorkArray,exists,nil);
             if idx<=0 then
               begin
                 if FinalTransFormUPO(FResultDBOsCompare[i]) then
                   begin
                     if (idx<0) then
                       begin
                         if Length(WorkArray)>0 then
                           refid := WorkArray[0].UID_String
                         else
                           refid := '';
                         G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert last }
                       end
                     else { idx=0 }
                       begin
                         if before>0 then
                           begin
                             if Length(WorkArray)>0 then
                               refid := WorkArray[0].UID_String
                             else
                               refid := '';
                             G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert last }
                           end
                         else
                           begin
                             if Length(WorkArray)>1 then
                               refid := WorkArray[1].UID_String
                             else
                               refid := '';
                             G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert last }
                           end;
                       end;
                   end;
               end
             else
             if idx>=High(WorkArray) then
               begin
                 if FinalTransFormUPO(FResultDBOsCompare[i]) then
                   begin
                     if (idx=high(WorkArray)) and (before>0) then
                       begin { must be before }
                         refid := WorkArray[idx].UID_String;
                         G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert }
                       end
                     else
                       begin
                         G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,''); { send insert last }
                       end;
                   end;
               end
             else
               begin  { idx < high}
                 if FinalTransFormUPO(FResultDBOsCompare[i]) then
                   begin
                     if before<0 then
                       begin { insert after }
                         refid := WorkArray[idx+1].UID_String;
                         G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert at position }
                       end
                     else
                       begin { insert at  }
                         refid := WorkArray[idx].UID_String;
                         G_TCDM.CN_AddGridInsertUpdate(FSessionID,GetStoreID,GetQueryID_ClientPart,upo,refid); { send insert at position }
                       end
                   end;
               end;
             WorkArray := FREDB_InsertAtIdxToObjectArray(WorkArray,idx,FResultDBOsCompare[i],before>0);
           end
       end;
  end;

  var i : NativeInt;

begin
  { the query is executed and the filter gets populated with 'new' dbos  }
  //d2 := Length(FResultDBOs);
  //d1 := Length(FResultDBOsCompare);
  if not assigned(FBaseData) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'compare run - no base data available');
  StartQueryRun(true);
  FBaseData.Execute(nil,self,true); { a compare run implies a filter data update -> rerun filter}
  EndQueryRun(true);
  d1 := Length(FResultDBOsCompare);
  d2 := Length(FResultDBOs);

  //writeln('DEBUG___ ',diff);
  //for i:=0 to high(FResultDBOsCompare) do
  // begin
  //   writeln('COMPARE POS ',i);
  //   writeln(FResultDBOsCompare[i].DumpToString());
  //   writeln('------');
  // end;
  //for i:=0 to high(FResultDBOs) do
  // begin
  //   writeln('Original POS ',i);
  //   writeln(FResultDBOs[i].DumpToString());
  //   writeln('------');
  // end;

  diff := d1 - d2;
  //if diff>0 then
  //  Inserted;
  //if diff<0 then
  //  Removed;
  //if diff=0 then
  //  Updated;

  FullUpdate;

  { update query to comparequery }
  SwapCompareQueryQry;
end;

procedure TFRE_DB_QUERY.ProcessChildObjCountChange(const obj: IFRE_DB_Object);
var i : NativeInt;
begin
  for i:=0 to high(FResultDBOs) do
    begin
      if FResultDBOs[i].UID=obj.UID then
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,'TAG/QRY UPDATE OBJECT DUE TO CHILD OBJECT COUNT CHANGE IN QRY [%s], UID [%s]',[GetQueryID,obj.UID_String]);
          TagForUpInsDelRLC(obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString);
          //FResultDBOs[i].Finalize;
          //FResultDBOs[i] := obj.CloneToNewObject;
          //G_TCDM.UpdateChildCount_QRY(self,i);
        end;
    end;
end;

function TFRE_DB_QUERY.GetStoreID: TFRE_DB_NameType;
begin
  result := FOrderDef.FKey.DC_Name;
end;

procedure TFRE_DB_QUERY.TagForUpInsDelRLC(const TransID: TFRE_DB_TransStepId);
var ok : String;
begin
  if not (pos('/',TransID)>0)  then
    raise EFRE_DB_Exception.Create(edb_ERROR,'must provide full tag');
  ok := GetFullQueryOrderKey.key;
  GFRE_DBI.LogDebug(dblc_DBTDM,'       >QRY MATCH UP/INS/DEL TAG IN ORDERFILTER [%s]',[ok]);
  FQryTransTag := TransID;
end;

procedure TFRE_DB_QUERY.ClearTransactionTag;
begin
  FQryTransTag := '';
end;

function TFRE_DB_QUERY.GetTransactionStepID: TFRE_DB_TransStepId;
begin
  result := FQryTransTag;
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

procedure TFRE_DB_TRANFORMED_DATA._AssertCheckTransid(const obj: IFRE_DB_Object; const transkey: TFRE_DB_TransStepId);
var otid : TFRE_DB_TransStepId;
begin
  otid := obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
  if otid<>transkey then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'Modification Transid not matching : [%s <> %s]',[otid,transkey]);
end;

procedure TFRE_DB_TRANFORMED_DATA.Cleanup;
var cnt : NativeInt;
begin
  FTransformedData.Clear;
end;

procedure TFRE_DB_TRANFORMED_DATA.UpdateObjectByNotify(const obj: IFRE_DB_Object; const transkey: TFRE_DB_TransStepId);
var idx  : NativeInt;
    tupo : TFRE_DB_Object;
    ppf  : string;
begin
  _AssertCheckTransid(obj,transkey);
  idx := ExistsObj(obj.UID); { Check if the Object Exists in the current Transformed Data}
  if idx>=0 then
    begin
      tupo := FTransformedData.Items[idx] as TFRE_DB_Object;
      ppf  := tupo.Field(cFRE_DB_SYS_PARENT_PATH_FULL).AsString;
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >NOTIFY UPDATE OBJECT [%s] AGAINST TRANSDATA [%s] PARENTPATH [%s]',[obj.GetDescriptionID,FKey.key,ppf]);
      FDC.TransformSingleUpdate(G_TCDM.DBC(FTDDBName),obj.CloneToNewObject(),self,FChildDataIsLazy,idx,ppf,transkey);
    end
  else
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >NOTIFY SKIP UPDATE OBJECT [%s] AGAINST TRANSDATA [%s] PARENTPATH [%s] OBJECT NOT FOUND',[obj.GetDescriptionID,FKey.key,ppf]);
      { not needed - dont clone }
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.InsertObjectByNotify(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object; const rl_ins: boolean; const parent: TFRE_DB_GUID; const transkey: TFRE_DB_TransStepId);
var idx : NativeInt;
    par : IFRE_DB_Object;
    ppf : TFRE_DB_String;
begin
  _AssertCheckTransid(obj,transkey);
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
          FDC.TransformSingleInsert(G_TCDM.DBC(FTDDBName),obj.CloneToNewObject(),self,FChildDataIsLazy,true,ppf,par,transkey); { Frees the object }
        end
      else
        begin { root insert }
          //GFRE_DBI.LogDebug(dblc_DBTDM,' >NOTIFY INSERT OBJECT [%s] AGAINST TRANSDATA [%s] COLL [%s] IS A COLLECTION UPDATE',[obj.GetDescriptionID,FKey.key,coll_name]);
          idx := ExistsObj(obj.UID);
          if idx>=0 then
            begin
              if IncludesChildData then
                GFRE_DBI.LogDebug(dblc_DBTDM,' >SKIP EXISTING NOTIFY INSERT OBJECT [%s] AGAINST TRANSDATA [%s] COLL [%s]',[obj.GetDescriptionID,FKey.key,coll_name])
                 { the object got highly probably updated by an rl insert, via reflinks, so skip double transform}
              else
                raise EFRE_DB_Exception.Create(edb_ERROR,'td updateobjectbynotify failed - already found, but not a parent child relation')
            end
          else
            begin
              GFRE_DBI.LogDebug(dblc_DBTDM,' >NOTIFY INSERT OBJECT [%s] AGAINST TRANSDATA [%s] COLL [%s] IS A COLLECTION UPDATE',[obj.GetDescriptionID,FKey.key,coll_name]);
              FDC.TransformSingleInsert(G_TCDM.DBC(FTDDBName),obj.CloneToNewObject(),self,FChildDataIsLazy,false,'',nil,transkey); { Frees the object }
            end;
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


procedure TFRE_DB_TRANFORMED_DATA.RemoveObjectByNotify(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object; const rl_rem: boolean; const parent: TFRE_DB_GUID; const transkey: TFRE_DB_TransStepId);
var idx,pidx : NativeInt;
    par      : TFRE_DB_Object;
begin
  if rl_rem or (uppercase(coll_name) =  FKey.Collname) then
    begin
      idx := ExistsObj(obj.UID);
      if idx>=0 then
        begin
          if rl_rem then
            begin
              pidx := ExistsObj(parent);
              par  := FTransformedData.Items[pidx] as TFRE_DB_Object;
            end
          else
            begin
              par := nil;
            end;
          HandleDeleteTransformedObject(idx,par,transkey)
        end
      else
        begin
          if IncludesChildData then
            { the object got highly probably removed by an rl drop, via reflinks, so skip double delete try }
          else
            raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'td removeobjectbynotify failed - not found / but not a childquery data');
        end;
    end;
end;

function FREDB_CompareReflinkSpecs(const filter_spec,notify_spec : TFRE_DB_NameTypeRL):boolean;
var fdir,ndir:boolean;
    ffield,fscheme:TFRE_DB_NameTypeRL;
    nfield,nscheme:TFRE_DB_NameTypeRL;
begin
  fdir   := FREDB_SplitRefLinkDescription(filter_spec,ffield,fscheme);
  ndir   := FREDB_SplitRefLinkDescription(notify_spec,nfield,nscheme);
  result := false;
  if fdir<>ndir then
    exit;
  if nfield=ffield then
    if (fscheme='') or (fscheme=nscheme) then
      exit(true)
end;

procedure TFRE_DB_TRANFORMED_DATA.SetupOutboundRefLink(const from_obj: TFRE_DB_GUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL; const transkey: TFRE_DB_TransStepId);
var idx : NativeInt;
begin
  if IncludesChildData and
     FREDB_CompareReflinkSpecs(FKey.RL_SpecUC,key_description) then
       begin
         idx := ExistsObj(to_obj.UID);
         if idx>=0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td setupoutbound rl - exists')
         else
           InsertObjectByNotify('',to_obj,true,from_obj,transkey)
       end;
  if HasReflinksInTransform and
      true then
        begin
          { TODO - handle that right }
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.SetupInboundRefLink(const from_obj: IFRE_DB_Object; const to_obj: TFRE_DB_GUID; const key_description: TFRE_DB_NameTypeRL; const transkey: TFRE_DB_TransStepId);
var idx : NativeInt;
begin
  if IncludesChildData and
     FREDB_CompareReflinkSpecs(FKey.RL_SpecUC,key_description) then
       begin
         idx := ExistsObj(from_obj.UID);
         if idx>=0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td setupinbound rl - exists')
         else
           InsertObjectByNotify('',from_obj,true,to_obj,transkey)
       end;
  if HasReflinksInTransform and
      true then
        begin
          { TODO - handle that right }
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.InboundReflinkDropped(const from_obj: IFRE_DB_Object; const to_obj: TFRE_DB_GUID; const key_description: TFRE_DB_NameTypeRL; const transkey: TFRE_DB_TransStepId);
var idx : NativeInt;
begin
  if IncludesChildData and
     FREDB_CompareReflinkSpecs(FKey.RL_SpecUC,key_description) then
       begin
         idx := ExistsObj(from_obj.UID);
         if idx<0 then
           raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'td inboundrldropped - not found')
         else
           RemoveObjectByNotify('',from_obj,true,to_obj,transkey)
       end;
  if HasReflinksInTransform and
      true then
        begin
          { TODO - handle that right }
        end;
end;

procedure TFRE_DB_TRANFORMED_DATA.OutboundReflinkDropped(const from_obj: TFRE_DB_GUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL; const transkey: TFRE_DB_TransStepId);
var idx : NativeInt;
begin
  if IncludesChildData and
     FREDB_CompareReflinkSpecs(FKey.RL_SpecUC,key_description) then
       begin
         idx := ExistsObj(to_obj.UID);
         if idx<0 then
           raise EFRE_DB_Exception.Create(edb_EXISTS,'td outboundrldropped - not found')
         else
           RemoveObjectByNotify('',to_obj,true,from_obj,transkey)
       end;
  if HasReflinksInTransform and
      true then
        begin
          { TODO - handle that right }
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
var us  : ShortString;
    ob  : TFRE_DB_Object;
    i   : NativeInt;
    ppa : TFRE_DB_StringArray;
begin
  us := FREDB_G2SB(tr_obj.UID);
  ob := FTransformedData.Find(us) as TFRE_DB_Object;
  if assigned(ob) then
    begin
      if ob.UID=tr_obj.UID then
        begin
          ppa := FREDB_PP_GetParentPaths(tr_obj);
          if length(ppa)<>1 then
            raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected double add / there should be exactly one parentpath');
          FREDB_PP_AddParentPathToObj(ob,ppa[0]);
        end
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'hash collision / double add / transformed data');
    end
  else
    FTransformedData.Add(us,tr_obj.Implementor);
end;

procedure TFRE_DB_TRANFORMED_DATA.SetTransObjectSingleInsert(const tr_obj: IFRE_DB_Object);
begin
  SetTransformedObject(tr_obj); { currently no need to diversificate }
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleUpdateTransformedObject(const tr_obj: IFRE_DB_Object ; const upd_idx : NativeInt);
var old_o : TFRE_DB_Object;
    i     : NativeInt;
    od    : TFRE_DB_TRANSFORMED_ORDERED_DATA;
begin
  GFRE_DBI.LogDebug(dblc_DBTDM,'   TRANSFORMED OBJECT UPDATE [%s] AGAINST ORDERING [%s]',[tr_obj.UID_String,FKey.key]);
  old_o := FTransformeddata.Items[upd_idx] as TFRE_DB_Object;
  try
    FTransformedData.Delete(upd_idx);                           { all orderings now point to the dangling removed object}
    SetTransformedObject(tr_obj);                               { new object in hashlist }
    for i := 0 to FOrderings.Count-1 do
      begin
        od := FOrderings.Items[i] as TFRE_DB_TRANSFORMED_ORDERED_DATA;
        GFRE_DBI.LogDebug(dblc_DBTDM,'   >MATCH UPDATE OBJECT [%s] IN ORDERING [%s]',[tr_obj.UID_String,od.FOrderDef.GetFullKey.key]);
        od.UpdateTransformedobject(old_o,tr_obj); { propagate old object up}
      end;
  finally
    old_o.Finalize;
  end;
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleInsertTransformedObject(const tr_obj: IFRE_DB_Object; const parent_object: IFRE_DB_Object);
var  i : NativeInt;
    od : TFRE_DB_TRANSFORMED_ORDERED_DATA;

  procedure CheckParentObjectChildcountChange;
  var pcc        : NativeInt;
      pflag      : string;
      tid        : TFRE_DB_TransStepId;
      tid2       : TFRE_DB_TransStepId;
  begin
   if assigned(parent_object) then
     begin
       pflag := parent_object.Field(cFRE_DB_CLN_CHILD_FLD).AsString;
       if pflag='' then
         pcc := 0
       else
         pcc := parent_object.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32;
       parent_object.Field(cFRE_DB_CLN_CHILD_FLD).AsString     := cFRE_DB_CLN_CHILD_FLG;
       parent_object.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32      := pcc+1;
       tid2 := tr_obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
       //tid  := parent_object.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
       parent_object.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString := tid2;
       G_TCDM.ChildObjCountChange(parent_object);
     end;
  end;

begin
  GFRE_DBI.LogDebug(dblc_DBTDM,'   TRANSFORMED OBJECT INSERT [%s] AGAINST ORDERING [%s]',[tr_obj.UID_String,FKey.key]);
  SetTransObjectSingleInsert(tr_obj); { new object in hashlist }
  CheckParentObjectChildcountChange;
  for i := 0 to FOrderings.Count-1 do
    begin
      od := FOrderings.Items[i] as TFRE_DB_TRANSFORMED_ORDERED_DATA;
      GFRE_DBI.LogDebug(dblc_DBTDM,'   >MATCH INSERT OBJECT [%s] IN ORDERING [%s]',[tr_obj.UID_String,od.FOrderDef.GetFullKey.key]);
      od.InsertTransformedobject(tr_obj); { propagate new object up}
    end;
end;

procedure TFRE_DB_TRANFORMED_DATA.HandleDeleteTransformedObject(const del_idx: NativeInt; const parent_object: IFRE_DB_Object; transtag: TFRE_DB_TransStepId);
var del_o : TFRE_DB_Object;
    i     : NativeInt;

  procedure CheckParentObjectChildcountChange;
  var pcc        : NativeInt;
      pflag      : string;
  begin
   if assigned(parent_object) then
     begin
       pcc   := parent_object.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32;
       pflag := parent_object.Field(cFRE_DB_CLN_CHILD_FLD).AsString;
       if pcc=0 then
         raise EFRE_DB_Exception.Create(edb_ERROR,'unexpected CheckParentObjectChildCountChange '+inttostr(pcc));
       if pcc>1 then
         parent_object.Field(cFRE_DB_CLN_CHILD_FLD).AsString := cFRE_DB_CLN_CHILD_FLG
       else
         parent_object.DeleteField(cFRE_DB_CLN_CHILD_FLD);
       parent_object.Field(cFRE_DB_CLN_CHILD_CNT).AsInt32  := pcc-1;
       parent_object.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString := transtag;
       G_TCDM.ChildObjCountChange(parent_object);
     end;
  end;

begin
  del_o := FTransformeddata.Items[del_idx] as TFRE_DB_Object;
  try
    CheckParentObjectChildcountChange;
    FTransformedData.Delete(del_idx);                           { all orderings now point to the dangling removed object}
    for i := 0 to FOrderings.Count-1 do
      (FOrderings.Items[i] as TFRE_DB_TRANSFORMED_ORDERED_DATA).DeleteTransformedobject(del_o,transtag); { propagate old object up}
  finally
    del_o.Finalize;
  end;
end;

function TFRE_DB_TRANFORMED_DATA.GetTransFormKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FKey;
end;

procedure TFRE_DB_TRANFORMED_DATA.CheckIntegrity;

  procedure CheckInt(const ino : TFRE_DB_Object);
  var obj : IFRE_DB_Object;
  begin
   try
    obj := ino.CloneToNewObject();
    obj.Finalize;
    obj := ino.CloneToNewObject();
    obj.Finalize;
    obj := ino.CloneToNewObject();
    obj.Finalize;
   except
     on e : exception do
      begin
        writeln('TD Integrity Check Failed ',e.Message,' ',FKey.key);
        halt;
      end;
   end;
  end;

begin
  ForAllObjs(@CheckInt);
end;

procedure TFRE_DB_TRANFORMED_DATA.TransformAll(var rcnt: NativeInt);
begin
  FDC.TransformAllTo(G_TCDM.DBC(FTDDBName),self,FChildDataIsLazy,rcnt);
end;


constructor TFRE_DB_TRANFORMED_DATA.Create(const key: TFRE_DB_TRANS_COLL_DATA_KEY; const dc: IFRE_DB_DERIVED_COLLECTION; const db_name: TFRE_DB_NameType);
begin
  FKey                    := key;
  Fkey.orderkey           := ''; { this is unordered, the orderkey comes from the frist query generating the data }
  FChildDataIsLazy        := True;
  FDC                     := dc;
  FTDCreationTime         := GFRE_DT.Now_UTC;
  FTransformedData        := TFPHashObjectList.Create(false);
  FOrderings              := TFPObjectList.Create(false);
  FTDDBName               := db_name;
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

procedure TFRE_DB_TRANFORMED_DATA.AddOrdering(const ordering: TFRE_DB_TRANSFORMED_ORDERED_DATA);
begin
  if FOrderings.IndexOf(ordering)<>-1 then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'double add ordering');
  FOrderings.Add(ordering);
end;

procedure TFRE_DB_TRANFORMED_DATA.RemoveOrdering(const ordering: TFRE_DB_TRANSFORMED_ORDERED_DATA);
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

function TFRE_DB_TRANSDATA_MANAGER.GetNewOrderDefinition: TFRE_DB_DC_ORDER_DEFINITION_BASE;
begin
  result := TFRE_DB_DC_ORDER_DEFINITION.Create;
end;

function TFRE_DB_TRANSDATA_MANAGER.GetNewFilterDefinition(const filter_db_name: TFRE_DB_NameType): TFRE_DB_DC_FILTER_DEFINITION_BASE;
begin
  result := TFRE_DB_DC_FILTER_DEFINITION.Create(filter_db_name);
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
var cnt  : NativeInt;
    fcnt : NativeInt;

  procedure CountFilters(const order : TFRE_DB_TRANSFORMED_ORDERED_DATA);
  begin
    cnt  := cnt  + order.GetFiltersCount;
    fcnt := fcnt + order.GetFilledFiltersCount;
    order.CheckDoFilterPurge;
  end;

  procedure Debug_Checkup(const order : TFRE_DB_TRANSFORMED_ORDERED_DATA);
  begin
    order.DebugCheckintegrity;
  end;

begin
  LockManager;
  try
    cnt  := 0;
    fcnt := 0;
    FOrders.ForAll(@CountFilters);
    //FOrders.ForAll(@Debug_Checkup);
    //GFRE_DB.LogDebug(dblc_DBTDM,'> TM STATS Queries (%d) Transforms (%d) Orders (%d) Filters/FilledFilters (%d/%d)',[FArtQueryStore.GetValueCount,FTransList.Count,FOrders.Count,cnt,fcnt]);
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.AssertCheckTransactionID(const obj: IFRE_DB_Object; const transid: TFRE_DB_TransStepId);
var ttag : TFRE_DB_TransStepId;
begin
  ttag := obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString;
  if ttag <>transid then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'transaction id mismatch OBJ=[%s] NOTIFY=[%s]',[ttag,transid]);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.StartNotificationBlock(const key: TFRE_DB_TransStepId);
begin
  if assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic/a notify block is currently assigned');
  FCurrentNotify := TFRE_DB_TRANSDATA_CHANGE_NOTIFIER.Create(key);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FinishNotificationBlock(out block: IFRE_DB_Object);

  procedure HandleTaggedQueries(const qry :TFRE_DB_QUERY);
  var orderkey : shortstring;
  begin
    orderkey := qry.GetFullQueryOrderKey.key;
    try
      if qry.GetTransactionStepID<>'' then
        begin
          GFRE_DBI.LogDebug(dblc_DBTDM,'  > PROCESSING TAGGED QRY QID[%S] OK[%s] TRANSID [%s]',[qry.GetQueryID,orderkey,qry.GetTransactionStepID]);
          qry.ProcessFilterChangeBasedUpdates;
          GFRE_DBI.LogDebug(dblc_DBTDM,'  < PROCESSING TAGGED QRY QID[%S] OK[%s] TRANSID [%s] DONE',[qry.GetQueryID,orderkey,qry.GetTransactionStepID]);
        end;
    finally
      qry.ClearTransactionTag;
    end;
  end;

begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'logic/no notify block is currently assigned');

  GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START QRY TAG SCAN TID [%s]',[FCurrentNotify.TransKey]);
  ForAllQueries(@HandleTaggedQueries);
  GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END QRY TAG SCAN [%s]',[FCurrentNotify.TransKey]);

  FCurrentNotify.NotifyAll;
  FreeAndNil(FCurrentNotify);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SendNotificationBlock(const block: IFRE_DB_Object);
begin
  abort;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CollectionCreated(const coll_name: TFRE_DB_NameType; const in_memory_only: boolean; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CollectionDeleted(const coll_name: TFRE_DB_NameType; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.IndexDefinedOnField(const coll_name: TFRE_DB_NameType; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.IndexDroppedOnField(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectStored(const coll_name: TFRE_DB_NameType; const obj: IFRE_DB_Object; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.InsertObjectByNotify(coll_name,obj.CloneToNewObject(),false,CFRE_DB_NullGUID,tsid);
  end;

begin
  AssertCheckTransactionID(obj,tsid);
  if coll_name='' then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'collname not set on ObjectStored Notification');
    GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT STORED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
    FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
    GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END  OBJECT STORED [%s] in Collection [%s]',[obj.GetDescriptionID,coll_name]);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectDeleted(const coll_names: TFRE_DB_NameTypeArray; const obj: IFRE_DB_Object; const tsid: TFRE_DB_TransStepId);
var logcolls : string;
  { if an object is finally deleted the reflinks from the objects are removed (if possible) }

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  var i : NativeInt;
  begin
    for i:=0 to high(coll_names) do
      tcd.RemoveObjectByNotify(coll_names[i],obj,false,CFRE_DB_NullGUID,tsid);
  end;

  procedure LogEntry;
  begin
    logcolls := FREDB_CombineString(FREDB_NametypeArray2StringArray(coll_names),',');
    GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT DELETED [%s] in Collections [%s]',[obj.GetDescriptionID,logcolls]);
  end;

  procedure LogLeave;
  begin
    GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END   OBJECT DELETED [%s] in Collections [%s]',[obj.GetDescriptionID,logcolls]);
  end;

begin
  AssertCheckTransactionID(obj,tsid);
  GFRE_DBI.LogDebugIf(dblc_DBTDM,@LogEntry);
  FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
  GFRE_DBI.LogDebugIf(dblc_DBTDM,@LogLeave);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectUpdated(const obj: IFRE_DB_Object; const colls: TFRE_DB_StringArray; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.UpdateObjectByNotify(obj,tsid);
  end;

begin
  AssertCheckTransactionID(obj,tsid);
  GFRE_DBI.LogDebug(dblc_DBTDM,'-> NOTIFY : START OBJECT UPDATED [%s] in Collections [%s]',[obj.GetDescriptionID+' '+obj.Field(cFRE_DB_SYS_TRANS_IN_OBJ_WAS_A).AsString,FREDB_CombineString(colls,',')]);
  FCurrentNotify.AddDirectSessionUpdateEntry(obj.CloneToNewObject);
  FTransList.ForAll(@CheckIfNeeded); { search all base transforms for needed updates ... }
  GFRE_DBI.LogDebug(dblc_DBTDM,'<- NOTIFY : END   OBJECT UPDATED [%s] in Collections [%s]',[obj.GetDescriptionID,FREDB_CombineString(colls,',')]);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.DifferentiallUpdStarts(const obj_uid: IFRE_DB_Object; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldDelete(const old_field: IFRE_DB_Field; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldAdd(const new_field: IFRE_DB_Field; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.FieldChange(const old_field, new_field: IFRE_DB_Field; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.DifferentiallUpdEnds(const obj_uid: TFRE_DB_GUID; const tsid: TFRE_DB_TransStepId);
begin
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ObjectRemoved(const coll_names: TFRE_DB_NameTypeArray; const obj: IFRE_DB_Object; const is_a_full_delete: boolean; const tsid: TFRE_DB_TransStepId);
begin
  { if an object is only removed from a collection the reflinks from the objects stay the same }
  if not is_a_full_delete then
    begin
      GFRE_DBI.LogDebug(dblc_DBTDM,'--->>>>>>>>> COLLECTION ONLY REMOVE NOT HANDLED BY NOTIFY ? / TEST IT // NOTIFY : START OBJECT REMOVED [%s] in Collection [%s]',[obj.GetDescriptionID,FREDB_CombineString(FREDB_NametypeArray2StringArray(coll_names),',')]);
    end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SetupOutboundRefLink(const from_obj: TFRE_DB_GUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.SetupOutboundReflink(from_obj,to_obj.CloneToNewObject,key_description,tsid);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.CheckAutoDependencyFilterChanges(key_description) then
        qry.TagForUpInsDelRLC(tsid);
  end;

begin
  FTransList.ForAll(@CheckIfNeeded);      { search all base transforms for needed updates ... }
  ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
end;

procedure TFRE_DB_TRANSDATA_MANAGER.SetupInboundRefLink(const from_obj: IFRE_DB_Object; const to_obj: TFRE_DB_GUID; const key_description: TFRE_DB_NameTypeRL; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.SetupInboundRefLink(from_obj.CloneToNewObject,to_obj,key_description,tsid);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.CheckAutoDependencyFilterChanges(key_description) then
      qry.TagForUpInsDelRLC(tsid);
  end;


begin
  FTransList.ForAll(@CheckIfNeeded);      { search all base transforms for needed updates ... }
  ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
end;

procedure TFRE_DB_TRANSDATA_MANAGER.InboundReflinkDropped(const from_obj: IFRE_DB_Object; const to_obj: TFRE_DB_GUID; const key_description: TFRE_DB_NameTypeRL; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.InboundReflinkDropped(from_obj.CloneToNewObject,to_obj,key_description,tsid);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.CheckAutoDependencyFilterChanges(key_description) then
      qry.TagForUpInsDelRLC(tsid);
  end;


begin
  FTransList.ForAll(@CheckIfNeeded);      { search all base transforms for needed updates ... }
  ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
end;

procedure TFRE_DB_TRANSDATA_MANAGER.OutboundReflinkDropped(const from_obj: TFRE_DB_GUID; const to_obj: IFRE_DB_Object; const key_description: TFRE_DB_NameTypeRL; const tsid: TFRE_DB_TransStepId);

  procedure CheckIfNeeded(const tcd : TFRE_DB_TRANFORMED_DATA);
  begin
    tcd.OutboundReflinkDropped(from_obj,to_obj.CloneToNewObject,key_description,tsid);
  end;

  procedure CheckAutoFilterUpdates(const qry : TFRE_DB_QUERY);
  begin
    if qry.CheckAutoDependencyFilterChanges(key_description) then
      qry.TagForUpInsDelRLC(tsid);
  end;


begin
  FTransList.ForAll(@CheckIfNeeded);      { search all base transforms for needed updates ... }
  ForAllQueries(@CheckAutoFilterUpdates); { check if a filter has changed, due to reflink changes }
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ChildObjCountChange(const parent_obj: IFRE_DB_Object);

  procedure CheckUpdates(const qry : TFRE_DB_QUERY);
  begin
    qry.ProcessChildObjCountChange(parent_obj);
  end;

begin
  ForAllQueries(@CheckUpdates); { check if a filter has changed, due to reflink changes }
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
  FNotifyProc    := TFRE_DB_TDM_NOTIFY_APPLY.Create(self);
end;

destructor TFRE_DB_TRANSDATA_MANAGER.Destroy;
begin
  FStatCleaner.Free;
  FNotifyProc.Free;
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

  procedure Search(const tcd : TFRE_DB_TRANSFORMED_ORDERED_DATA ; var halt : boolean);
  begin
    try
      if tcd.FOrderDef.GetFullKey.key=fkd then
        begin
          halt := true;
          cd   := tcd;
          tcd.LockBase;
        end;
    except
      tcd.FOrderDef.GetFullKey;
    end;
  end;

begin
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
          transdata := TFRE_DB_TRANFORMED_DATA.Create(basekey,dc,QryDBName);  { the transform data is identified by the basekey }
          transdata.TransFormAll(rcnt);
          AddBaseTransformedData(transdata);
          et        := GFRE_BT.Get_Ticks_ms;
          GFRE_DBI.LogInfo(dblc_DBTDM,'<BASE TRANSFORMING DATA FOR [%s] DONE - Transformed %d records in %d ms',[basekey.key,rcnt,et-st]);
        end;
      cd := TFRE_DB_TRANSFORMED_ORDERED_DATA.Create(FOrderDef,transdata); { generate the ordered, transformed data (next layer) }
      FOrders.Add2Array(TFRE_DB_TRANSFORMED_ORDERED_DATA(cd));            { internal add the data }
      TFRE_DB_TRANSFORMED_ORDERED_DATA(cd).OrderTheData;                  { order it }
      TFRE_DB_TRANSFORMED_ORDERED_DATA(cd).LockBase;                      { return the locked result }
    end;
end;

function TFRE_DB_TRANSDATA_MANAGER.GenerateQueryFromQryDef(const qry_def: TFRE_DB_QUERY_DEF): TFRE_DB_QUERY_BASE;
var qry : TFRE_DB_QUERY;

   procedure ProcessDependencies;
   var i : NativeInt;
   begin
     if qry.FIsChildQuery then { do not process dependencies for child queries ( check with client ) }
       exit;
     SetLength(qry.FDependencyIds,Length(qry_def.DependencyRefIds));
     for i:=0 to high(qry.FDependencyIds) do
      qry.FDependencyIds[i] := uppercase(qry_def.DependencyRefIds[i]);
     qry.FDepRefConstraints  := qry_def.DepRefConstraints;
     qry.FDependcyFilterUids := qry_def.DepFilterUids;
   end;

   procedure ProcessOrderDefinition;
   begin
     qry.Orderdef.AssignFrom(qry_def.OrderDefRef);
     qry.Orderdef.SetDataKeyColl(qry_def.ParentName,qry_def.DerivedCollName,qry_def.ParentChildSpec);
     qry.OrderDef.Seal;
   end;

   procedure ProcessCheckChildQuery;
   begin
     with qry do begin
       FParentChildLinkFldSpec := qry_def.ParentChildSpec;        { comes from dc }
       FParentChildSkipschemes := qry_def.ParentChildSkipSchemes; { comes from dc }
       if length(qry_def.ParentIds)>0 then
         begin { this is a child query }
           FParentIds := qry_def.ParentIds;
           Filterdef.AddParentFilter('*SPCF*',FParentIds); { add a parent field filter }
           FIsChildQuery := true;
         end
       else
         begin
           Filterdef.AddChildFilter('*SPCF*'); { add a child filter }
           FIsChildQuery := false;
         end;
     end;
   end;

   procedure ProcessRange;
   begin
     with qry do
       begin
         FStartIdx        := qry_def.StartIdx;
         FToDeliverCount  := qry_def.ToDeliverCount;
       end;
   end;

   procedure ProcessCheckFulltextFilter;
   begin
     if qry_def.FullTextFilter<>'' then
       begin
         qry.Filterdef.AddStringFieldFilter('*FTX*','FTX_SEARCH',qry_def.FullTextFilter,dbft_PART);
       end
     else
       begin
         qry.Filterdef.RemoveFilter('*FTX*');
       end;
   end;

   procedure Processfilters;
   begin
     qry.Filterdef.AddStdRightObjectFilter('*SRF*',[sr_FETCH],qry_def.UserTokenRef.CloneToNewUserToken);
     qry.Filterdef.AddFilters(qry_def.FilterDefStaticRef,true);
     qry.Filterdef.AddFilters(qry_def.FilterDefDynamicRef,true);
     qry.Filterdef.Seal;
   end;

   procedure SetQueryID;
   begin
     qry.FQueryClientID := qry_def.ClientQueryID;
     qry.FQueryId       := FormQueryID(qry_def.SessionID,qry_def.DerivedCollName,qry_def.ClientQueryID);
     qry.FQueryDescr    := Format('QRY(%s) DC(%s) CLID(%d)',[qry.FQueryId,qry_def.DerivedCollName,qry.FQueryClientID]);
   end;

begin
  qry := TFRE_DB_QUERY.Create(qry_def.DBName);
  qry.FOnlyOneUID := qry_def.OnlyOneUID;
  if qry.FOnlyOneUID<>CFRE_DB_NullGUID then
    qry.FUidPointQry:=true;
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
    id   : TFRE_DB_NameType;
begin
  LockManager;
  try
    qptr := FREDB_ObjectToPtrUInt(qry);
    if not FArtQueryStore.InsertStringKeyOrFetch(qry.GetQueryID,qptr) then
      begin
        id := qry.GetQueryID;
        qry.free;
        raise EFRE_DB_Exception.Create(edb_ERROR,'double add query try, query id''s are not unique [%s]',[id]);
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

procedure TFRE_DB_TRANSDATA_MANAGER.DropAllQuerys(const session_id: TFRE_DB_String; const dc_name: TFRE_DB_NameTypeRL);
var qryid:TFRE_DB_NameType;

  procedure ClearIt(var val : PtrUInt);
  var qry:TFRE_DB_QUERY;
  begin
    qry := FREDB_PtrUIntToObject(val) as TFRE_DB_QUERY;
    GFRE_DBI.LogDebug(dblc_DBTDM,'  REMOVING CLIENT QUERY ID [%s]',[qry.GetQueryID]);
    qry.free;
  end;

  procedure DC_Clear;
  begin
    GFRE_DBI.LogDebug(dblc_DBTDM,'>START REMOVING DERIVED COLLECTION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
    FArtQueryStore.PrefixStringScanClear(qryid,@clearit);
    GFRE_DBI.LogDebug(dblc_DBTDM,'<REMOVED DC QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
  end;

  procedure Session_Clear;  { TODO : HACKY / UNDERSTAND }
  begin
    GFRE_DBI.LogDebug(dblc_DBTDM,'>START REMOVING SESSION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
    FArtQueryStore.PrefixStringScanClear(qryid,@clearit);
    GFRE_DBI.LogDebug(dblc_DBTDM,'<REMOVED SESSION QUERYS ID [%s] TOTAL Q COUNT=%d',[qryid,FArtQueryStore.GetValueCount]);
  end;

begin
  LockManager;
  try
    qryid := FormQueryID(session_id,dc_name,0);
    if dc_name<>'' then
      DC_Clear
    else
      Session_Clear;
  finally
    UnlockManager;
  end;
end;

function TFRE_DB_TRANSDATA_MANAGER.FormQueryID(const session_id: TFRE_DB_String; const dc_name: TFRE_DB_NameTypeRL; const client_part: int64): TFRE_DB_NameType;
begin
  result := session_id;
  if dc_name<>'' then
    result := result + '/'+GFRE_BT.HashFast32_Hex(dc_name);
  if client_part<>0 then
    result := result+'#'+inttostr(client_part);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.ApplyInboundNotificationBlock(const dbname: TFRE_DB_NameType; const block: IFRE_DB_Object);
var dummy : IFRE_DB_Object;
begin
  LockManager;
  try
    self.StartNotificationBlock(Block.Field('KEY').AsString);
    try
      FREDB_ApplyNotificationBlockToNotifIF(block,self,FCurrentNLayer);
    finally
      self.FinishNotificationBlock(dummy);
    end;
  finally
    UnlockManager;
  end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.InboundNotificationBlock(const dbname: TFRE_DB_NameType; const block: IFRE_DB_Object);
var bl : TFRE_DB_INBOUND_N_BLOCK;
begin
  bl        := TFRE_DB_INBOUND_N_BLOCK.Create;
  bl.FBlock := block.CloneToNewObject(); { TODO REMOVE CLONING ONLY FOR TEST}
  FNotifyProc.EnqueueBlock(bl);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddDirectSessionUpdateEntry(const update_dbo: IFRE_DB_Object);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / direct session update entry');
  GFRE_DBI.LogDebug(dblc_DBTDM,'         >CN_DIRECT SESSION UPDATE',[]);
  GFRE_DBI.LogDebug(dblc_DBTDM,'           >%s',[update_dbo.DumpToString(15)]);
  FCurrentNotify.AddDirectSessionUpdateEntry(update_dbo);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInplaceUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const upo: IFRE_DB_Object);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid inplace update');
  GFRE_DBI.LogDebug(dblc_DBTDM,'         >CN_INPLACE UPDATE SES[%s]  STORE[%s] CQID [%d]',[sessionid,store_id,qry_id]);
  GFRE_DBI.LogDebug(dblc_DBTDM,'           >%s',[upo.DumpToString(15)]);
  FCurrentNotify.AddGridInplaceUpdate(sessionid,store_id,qry_id,upo);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInplaceDelete(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64 ; const del_id: TFRE_DB_String);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid delete');
  GFRE_DBI.LogDebug(dblc_DBTDM,'         >CN_DELETE UPDATE SES[%s]  STORE[%s] CQID [%d] DEL ID [%s]',[sessionid,store_id,qry_id,del_id]);
  FCurrentNotify.AddGridRemoveUpdate(sessionid,store_id,qry_id,del_id);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.CN_AddGridInsertUpdate(const sessionid: TFRE_DB_NameType; const store_id: TFRE_DB_NameType; const qry_id: Int64; const upo: IFRE_DB_Object; const reference_id: TFRE_DB_String);
begin
  if not assigned(FCurrentNotify) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal/current notify gatherer not assigned / grid insert update');
  GFRE_DBI.LogDebug(dblc_DBTDM,'         >CN_INSERT UPDATE SES[%s]  STORE[%s] CQID [%d] REF ID [%s]',[sessionid,store_id,qry_id,reference_id]);
  GFRE_DBI.LogDebug(dblc_DBTDM,'           >%s',[upo.DumpToString(15)]);
  FCurrentNotify.AddGridInsertUpdate(sessionid,store_id,qry_id,upo,reference_id);
end;

procedure TFRE_DB_TRANSDATA_MANAGER.TagQueries4UpInsDel(const td: TFRE_DB_TRANSFORMED_ORDERED_DATA ; const TransActionTag : TFRE_DB_TransStepId);

  procedure HandleQueryUpdate(const qry : TFRE_DB_QUERY);
  var
      full_data_key : TFRE_DB_TRANS_COLL_DATA_KEY;
      ses           : TFRE_DB_UserSession;
      update_st     : TFRE_DB_UPDATE_STORE_DESC;
      upo           : IFRE_DB_Object;
  begin
    full_data_key           := td.GetFullKey;
    //full_data_key.filterkey := filtercont.Filters.GetFilterKey;
    if not (FREDB_CompareTransCollDataKeysOrderOnly(full_data_key,qry.GetFullQueryOrderKey)) then
      exit; { query does not match the delivered update spec }
    qry.TagForUpInsDelRLC(TransActionTag);
  end;

begin
  assert(TransActionTag<>'',' transaction tag must be set');
  ForAllQueries(@HandleQueryUpdate);
end;


procedure TFRE_DB_TRANSDATA_MANAGER.UpdateChildCount_QRY(const qry: TFRE_DB_QUERY; const idx: NativeInt);
var ses : TFRE_DB_UserSession;
    upo : IFRE_DB_Object;
begin
  if not GFRE_DBI.NetServ.FetchSessionByIdLocked(qry.FSessionID,ses) then
    begin
      GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] UPDATE/CHILDCOUNT NOT FOUND ON UPDATE QRY(?)',[qry.FSessionID]);
      exit;
    end
  else
    begin
      try
        upo := qry.FResultDBOs[idx].CloneToNewObject;
        qry.FBaseData.FBaseTransData.FDC.FinalRightTransform(ses,upo);
      finally
        ses.UnlockSession;
      end;
      CN_AddGridInplaceUpdate(qry.FSessionID,qry.GetStoreID,qry.GetQueryID_ClientPart,upo);
    end;
end;

procedure TFRE_DB_TRANSDATA_MANAGER.UpdateLiveStatistics(const stats: IFRE_DB_Object);
var stat_entry : IFRE_DB_Object;

  procedure SearchStatQuery(const qry : TFRE_DB_QUERY);
  var storeid : TFRE_DB_NameType;
      ct      : TFRE_DB_UPDATE_STORE_DESC;
      entry   : IFRE_DB_Object;
      change  : IFRE_DB_Object;
      i       : Integer;

      function FinalTransFormUPO:boolean;
      var ses : TFRE_DB_UserSession;
      begin
        result := false;
        if not GFRE_DBI.NetServ.FetchSessionByIdLocked(qry.FSessionID,ses) then
          begin
            GFRE_DBI.LogWarning(dblc_DBTDM,'> SESSION [%s] NOT FOUND ON UPDATE/INSERT QRY(?)',[qry.FSessionID]);
            exit;
          end
        else
          begin
            try
              change := entry.CloneToNewObject;
              qry.FBaseData.FBaseTransData.FDC.FinalRightTransform(ses,change);
              result := true;
            finally
              ses.UnlockSession;
            end;
          end;
      end;

      var statmethod : TMethod;
          statmfld   : IFRE_DB_Field;

      type
        tstatclassm = procedure(const transformed_output : IFRE_DB_Object ; const stat_data : IFRE_DB_Object ; const statfieldname : TFRE_DB_Nametype) of object;

  begin
    storeid := qry.GetStoreID;
    if qry.FBaseData.FBaseTransData.FDC.HasStatTransforms then
      begin
        for i:=0 to high(qry.FResultDBOs) do
          if qry.FResultDBOs[i].UID=stat_entry.Field('statuid').AsGUID then
              begin
                if qry.FResultDBOs[i].FieldOnlyExisting(cFRE_DB_SYS_STAT_METHODPOINTER,statmfld) then
                  begin
                    //writeln('>>>>>>>>>>>>>>>>>>>>> FOUND A STATUSUPDATE ENTRY UID  ---');
                    //writeln(qry.FResultDBOs[i].DumpToString());
                    //writeln('--------- CORRESPONDING STATISTIC OBJECT ');
                    //writeln(stat_entry.DumpToString());
                    //writeln('---------------------');
                    statmethod.Code := Pointer(statmfld.AsUint64);
                    entry := qry.FResultDBOs[i];
                    if FinalTransFormUPO then
                      begin
                        tstatclassm(statmethod)(change,stat_entry,'status'); { after final right transform, do finally the stat transform }
                        ct := TFRE_DB_UPDATE_STORE_DESC.create.Describe(storeid);
                        ct.addUpdatedEntry(change,qry.GetQueryID_ClientPart);
                        GFRE_DBI.NetServ.SendDelegatedContentToClient(qry.FSessionID,ct);
                      end;
                  end
                else
                  begin
                    writeln('>>>>>>>>>>>>>>>>>>>>> FOUND A STATUSUPDATE ENTRY UID BUT NO STATISTIC METHOD ---');
                    writeln(qry.FResultDBOs[i].DumpToString());
                    writeln('--------- CORRESPONDING STATISTIC OBJECT ');
                    writeln(stat_entry.DumpToString());
                    writeln('---------------------');
                  end;
              end;
      end;
  end;

  procedure MyStatsUpdate(const stat_obj:IFRE_DB_Object);
  begin
    stat_entry := stat_obj; { make it available through stack frame }
    //writeln('>>>  SEARCH FOR STATUS OBJECT : ');
    //writeln(stat_entry.DumpToString());
    //writeln('-----------------------');
    ForAllQueries(@SearchStatQuery);
  end;

begin
  LockManager;
  try
    stats.ForAllObjects(@MyStatsUpdate);
  finally
    UnlockManager;
  end;
end;


procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.InsertIntoTree(const insert_obj: TFRE_DB_Object);
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


procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Notify_InsertIntoTree(const new_obj: TFRE_DB_Object);
var
   keynew       : Array [0..512] of Byte;
   keynewlen    : NativeInt;
begin
  FOrderDef.SetupBinaryKey(new_obj,@Keynew[0],keynewlen,true);
  Notify_InsertIntoTree(@keynew[0],keynewlen,new_obj);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Notify_InsertIntoTree(const key: PByte; const keylen: NativeInt; const new_obj: TFRE_DB_Object; const propagate_up: boolean);
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
  if oc.Exists(new_obj) then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'Notify Inset KEY Failed');
  oc.AddObject(new_obj);

  FREDB_BinaryKey2ByteArray(key,keylen,byte_arr);
  byte_arr2 := new_obj.Field(cFRE_DB_SYS_T_OBJ_TOTAL_ORDER).AsObject.Field(FOrderDef.FKey.orderkey).AsByteArr;
  if (Length(byte_arr)<>Length(byte_arr2)) or (CompareByte(byte_arr[0],byte_arr2[0],Length(byte_arr))<>0) then
    raise EFRE_DB_Exception.Create(edb_ERROR,'internal wrong tagged');
  if propagate_up then
    begin
      FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersInsert);
      G_TCDM.TagQueries4UpInsDel(self,new_obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString);
    end;
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Notify_DeleteFromTree(const old_obj: TFRE_DB_Object; transtag: TFRE_DB_TransStepId);
var
   keyold       : Array [0..512] of Byte;
   keyoldlen    : NativeInt;
begin
  FOrderDef.SetupBinaryKey(old_obj,@KeyOld[0],keyoldlen,false);
  Notify_DeleteFromTree(@keyold,keyoldlen,old_obj,true,transtag);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Notify_DeleteFromTree(const key: PByte; const keylen: NativeInt; const old_obj: TFRE_DB_Object; const propagate_up: boolean; const transtag: TFRE_DB_TransStepId);
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
  if oc.RemoveObject(old_obj) then { true = remove the (empty) ordercontaienr now }
    begin
      FArtTreeKeyToObj.RemoveBinaryKey(key,keylen,valueptr);
      assert(TFRE_DB_OrderContainer(valueptr)=oc,'internal/logic remove failed');
      oc.free;
    end;
  if propagate_up then
    begin
      FArtTreeFilterKey.LinearScan(@CheckAllOpenFiltersRemoveOrderChanged);
      G_TCDM.TagQueries4UpInsDel(self,transtag);
    end;
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.CheckUpdateFilterContainer(const qry_context: TFRE_DB_QUERY; const compare_run: boolean);
var brk        : boolean;
    filtkey    : TFRE_DB_TRANS_COLL_FILTER_KEY;
    dummy      : PNativeUint;
    st,et      : NativeInt;
    s          : string;

  procedure IteratorBreak(var dummy : PtrUInt ; var halt : boolean);
  var oc : TFRE_DB_OrderContainer;

      procedure MyIter(const obj : IFRE_DB_Object; var myhalt : boolean);
      begin
        qry_context.FFilterContainer.CheckFilteredAdd(obj);
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
      qry_context.FFilterContainer := TFRE_DB_FilterContainer.Create(GetFullKey.orderkey,qry_context.GetFilterDefinition.FilterDBName);
      qry_context.FFilterContainer.Filters.AddFilters(qry_context.FQueryFilters);          { clone filters into filtercontainer spec }
      qry_context.FFilterContainer.Filters.Seal;                                           { store the filtercontainer reference in the query }
      dummy^     := FREDB_ObjectToPtrUInt(qry_context.FFilterContainer);                   { but manage it in the art tree }
    end
  else
    begin
      qry_context.FFilterContainer := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_FilterContainer;
      s := GetFullKey.key;
      s := filtkey;
      s := BoolToStr(qry_context.FFilterContainer.IsFilled,'FILLED','NOT FILLED');
      GFRE_DBI.LogInfo(dblc_DBTDM,'>REUSING FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] [%s]',[GetFullKey.key,filtkey,BoolToStr(qry_context.FFilterContainer.IsFilled,'FILLED','NOT FILLED')]);
    end;
  qry_context.FFilterContainer.CheckDBReevaluation; { check if the filter was updated and needs db reevaluation }
  if not qry_context.FFilterContainer.IsFilled then
    begin
      GFRE_DBI.LogInfo(dblc_DBTDM,'>NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s]',[GetFullKey.key,filtkey]);
      st := GFRE_BT.Get_Ticks_ms;
      qry_context.FFilterContainer.Filters.MustBeSealed;
      FArtTreeKeyToObj.LinearScanBreak(@IteratorBreak,brk,false);
      qry_context.FFilterContainer.IsFilled := true;
      qry_context.FFilterContainer.AdjustLength;
      et := GFRE_BT.Get_Ticks_ms;
      GFRE_DBI.LogInfo(dblc_DBTDM,'<NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] DONE in %d ms',[GetFullKey.key,filtkey,et-st]);
    end;
end;


procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Notify_UpdateIntoTree(const old_obj, new_obj: TFRE_DB_Object);
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
                  (CompareByte(keynew[0],keyold[0],keyoldlen)<>0);
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
    G_TCDM.TagQueries4UpInsDel(self,new_obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.LockBase;
begin
  FTransCollDataLock.Acquire;
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.UnlockBase;
begin
  FTransCollDataLock.Release;
end;

constructor TFRE_DB_TRANSFORMED_ORDERED_DATA.Create(const orderdef: TFRE_DB_DC_ORDER_DEFINITION; base_trans_data: TFRE_DB_TRANFORMED_DATA);
begin
  FOrderDef         := orderdef;
  FArtTreeKeyToObj  := TFRE_ART_TREE.Create;
  FArtTreeFilterKey := TFRE_ART_TREE.Create;
  FTOCreationTime   := GFRE_DT.Now_UTC;
  GFRE_TF.Get_Lock(FTransCollDataLock);
  FBaseTransData    := base_trans_data;
  base_trans_data.AddOrdering(self);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.OrderTheData;
var i       : NativeInt;

    procedure OrderArray(const tobj : TFRE_DB_Object);
    begin
      InsertIntoTree(tobj);
    end;

begin
  FArtTreeKeyToObj.Clear;
  FBaseTransData.ForAllObjs(@OrderArray);
end;

function TFRE_DB_TRANSFORMED_ORDERED_DATA.GetFullKey: TFRE_DB_TRANS_COLL_DATA_KEY;
begin
  result := FOrderDef.GetFullKey;
end;

destructor TFRE_DB_TRANSFORMED_ORDERED_DATA.Destroy;
begin
  FArtTreeKeyToObj.Destroy;
  FTransCollDataLock.Finalize;
  inherited Destroy;
end;


procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.Execute(const iter: IFRE_DB_Obj_Iterator; const qry_context: TFRE_DB_QUERY; const compare_run: boolean; const point_qry: boolean);
//var brk        : boolean;
//    filtkey    : TFRE_DB_TRANS_COLL_FILTER_KEY;
//    dummy      : PNativeUint;
//    st,et      : NativeInt;
//    s          : string;
//
//  procedure IteratorBreak(var dummy : PtrUInt ; var halt : boolean);
//  var oc : TFRE_DB_OrderContainer;
//
//      procedure MyIter(const obj : IFRE_DB_Object; var myhalt : boolean);
//      begin
//        qry_context.FFilterContainer.CheckFilteredAdd(obj);
//      end;
//
//  begin
//    oc := FREDB_PtrUIntToObject(dummy) as TFRE_DB_OrderContainer;
//    oc.ForAllBreak(@MyIter,halt);
//  end;

begin
  //brk := false;
  //filtkey    := qry_context.FQueryFilters.GetFilterKey;
  //dummy      := nil;
  //if FArtTreeFilterKey.InsertStringKeyOrFetchR(filtkey,dummy) then
  //  begin
  //    qry_context.FFilterContainer := TFRE_DB_FilterContainer.Create(GetFullKey.orderkey,qry_context.GetFilterDefinition.FilterDBName);
  //    qry_context.FFilterContainer.Filters.AddFilters(qry_context.FQueryFilters);          { clone filters into filtercontainer spec }
  //    qry_context.FFilterContainer.Filters.Seal;                                           { store the filtercontainer reference in the query }
  //    dummy^     := FREDB_ObjectToPtrUInt(qry_context.FFilterContainer);                   { but manage it in the art tree }
  //  end
  //else
  //  begin
  //    qry_context.FFilterContainer := FREDB_PtrUIntToObject(dummy^) as TFRE_DB_FilterContainer;
  //    s := GetFullKey.key;
  //    s := filtkey;
  //    s := BoolToStr(qry_context.FFilterContainer.IsFilled,'FILLED','NOT FILLED');
  //    GFRE_DBI.LogInfo(dblc_DBTDM,'>REUSING FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] [%s]',[GetFullKey.key,filtkey,BoolToStr(qry_context.FFilterContainer.IsFilled,'FILLED','NOT FILLED')]);
  //  end;
  //qry_context.FFilterContainer.CheckDBReevaluation; { check if the filter was updated and needs db reevaluation }
  ////if compare_run then
  //// TODO REMOVE THIS, UPDATE SHOULD hAVE BEEN APPLIED IN FiLTER ALREADY
  ////  begin
  ////    GFRE_DBI.LogInfo(dblc_DBTDM,'>CLEAR FILTERING DUE TO COMPARE RUN FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s]',[GetFullKey.key,filtkey]);
  ////    qry_context.FFilterContainer.ClearDataInFilter;
  ////  end;
  //if not qry_context.FFilterContainer.IsFilled then
  //  begin
  //    GFRE_DBI.LogInfo(dblc_DBTDM,'>NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s]',[GetFullKey.key,filtkey]);
  //    st := GFRE_BT.Get_Ticks_ms;
  //    qry_context.FFilterContainer.Filters.MustBeSealed;
  //    FArtTreeKeyToObj.LinearScanBreak(@IteratorBreak,brk,false);
  //    qry_context.FFilterContainer.IsFilled := true;
  //    qry_context.FFilterContainer.AdjustLength;
  //    et := GFRE_BT.Get_Ticks_ms;
  //    GFRE_DBI.LogInfo(dblc_DBTDM,'<NEW FILTERING FOR BASEDATA FOR [%s] FILTERKEY[%s] DONE in %d ms',[GetFullKey.key,filtkey,et-st]);
  //  end;
  CheckUpdateFilterContainer(qry_context,compare_run);
  if point_qry then
    qry_context.FFilterContainer.ExecutePointQuery(iter,qry_context)
  else
    qry_context.FFilterContainer.Execute(iter,qry_context,compare_run);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.UpdateTransformedobject(const old_obj, new_object: IFRE_DB_Object);
begin
  Notify_UpdateIntoTree(old_obj.Implementor as TFRE_DB_Object,new_object.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.InsertTransformedobject(const new_object: IFRE_DB_Object);
begin
  Notify_InsertIntoTree(new_object.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.DeleteTransformedobject(const del_object: IFRE_DB_Object; transtag: TFRE_DB_TransStepId);
begin
  Notify_DeleteFromTree(del_object.Implementor as TFRE_DB_Object,transtag);
end;

function TFRE_DB_TRANSFORMED_ORDERED_DATA.GetFiltersCount: NativeInt;
begin
  result := FArtTreeFilterKey.GetValueCount;
end;

function TFRE_DB_TRANSFORMED_ORDERED_DATA.GetFilledFiltersCount: NativeInt;

   procedure GetCount(var f : NativeUint);
   var filter : TFRE_DB_FilterContainer;
   begin
     filter := FREDB_PtrUIntToObject(f) as TFRE_DB_FilterContainer;
     if filter.IsFilled then
       inc(result);
   end;

 begin
   result := 0;
   FArtTreeFilterKey.LinearScan(@GetCount);
 end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.CheckDoFilterPurge;

  procedure CheckPurge(var f : NativeUint);
  var filter : TFRE_DB_FilterContainer;
  begin
    filter := FREDB_PtrUIntToObject(f) as TFRE_DB_FilterContainer;
    filter.PurgeFilterDataDueToTimeout;
  end;

begin
  FArtTreeFilterKey.LinearScan(@CheckPurge);
end;

procedure TFRE_DB_TRANSFORMED_ORDERED_DATA.DebugCheckintegrity;

  procedure CheckIntegrity(var f : NativeUint);
  var filter : TFRE_DB_FilterContainer;
  begin
    filter := FREDB_PtrUIntToObject(f) as TFRE_DB_FilterContainer;
    filter.Checkintegrity;
  end;

begin
  FBaseTransData.CheckIntegrity;
  FArtTreeFilterKey.LinearScan(@CheckIntegrity);
end;

{ TFRE_DB_TRANSDATA_MANAGER }


end.

