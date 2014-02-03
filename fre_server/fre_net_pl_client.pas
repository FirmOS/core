unit fre_net_pl_client;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,fre_system,fos_tool_interfaces,fre_aps_interface,fre_db_interface,fre_db_core;

  {TODO : Disallow Databases named "GLOBAL"}
type
  { TFRE_DB_PL_NET_CLIENT }

  TFRE_DB_PL_NET_CLIENT=class(TObject)
  private
  type
    TPL_CONN_CSTATE  = (sfc_NOT_CONNECTED,sfc_TRYING,sfc_NEGOTIATE_LAYER,sfc_OK,sfc_Failed);
    TPL_CMD_STATE    = (cs_READ_LEN,cs_READ_DATA);

    { TPLNet_PersistanceCollection }
    TPLNet_PersistanceCollection=class(IFRE_DB_PERSISTANCE_COLLECTION)
    private
      FName      : TFRE_DB_NameType;
      FUppername : TFRE_DB_NameType;
    public
      constructor     Create                     (const collname : TFRE_DB_NameType ; const isVolatile : Boolean);
      function        GetPersLayerIntf           : IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
      function        GetPersLayer               : IFRE_DB_PERSISTANCE_LAYER;

      function        IsVolatile                 : boolean;
      function        CollectionName             (const unique:boolean=true):TFRE_DB_NameType;
      function        Count                      : int64;
      function        Exists                     (const ouid: TGUID): boolean;
      procedure       GetAllUIDS                 (var uids : TFRE_DB_GUIDArray);

      function        Fetch                      (const uid:TGUID ; var obj : IFRE_DB_Object) : boolean;
      function        First                      : IFRE_DB_Object;
      function        Last                       : IFRE_DB_Object;
      function        GetItem                    (const num:uint64) : IFRE_DB_Object;
      function        DefineIndexOnField         (const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false): TFRE_DB_Errortype;

      function        GetIndexedObj              (const query_value : TFRE_DB_String ; out   obj       : IFRE_DB_Object;const index_name:TFRE_DB_NameType='def'):boolean; // for the string fieldtype
      function        GetIndexedObj              (const query_value : TFRE_DB_String ; out   obj       : IFRE_DB_ObjectArray ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean;
      function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TGUID               ; const index_name : TFRE_DB_NameType='def'): boolean;
      function        GetIndexedUID              (const query_value : TFRE_DB_String ; out obj_uid     : TFRE_DB_GUIDArray   ; const index_name : TFRE_DB_NameType='def' ; const check_is_unique : boolean=false):boolean; overload ;

      procedure       ForAllIndexed              (var guids : TFRE_DB_GUIDArray ; const index_name:TFRE_DB_NameType='def'; const ascending:boolean=true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const only_count_unique_vals : boolean=false);

      procedure       ForAllIndexedSignedRange   (const min_value,max_value : int64          ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const only_count_unique_vals : boolean=false);
      procedure       ForAllIndexedUnsignedRange (const min_value,max_value : QWord          ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const only_count_unique_vals : boolean=false);
      procedure       ForAllIndexedStringRange   (const min_value,max_value : TFRE_DB_String ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const min_is_null : boolean = false ; const max_is_max : boolean = false ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const only_count_unique_vals : boolean=false);
      procedure       ForAllIndexPrefixString    (const prefix              : TFRE_DB_String ; var   guids    : TFRE_DB_GUIDArray ; const index_name : TFRE_DB_NameType ; const ascending: boolean = true ; const max_count : NativeInt=0 ; skipfirst : NativeInt=0 ; const only_count_unique_vals : boolean=false);
    end;

    { TPLNet_Layer }
    TPLNet_Layer = class(IFRE_DB_PERSISTANCE_LAYER)
      FNETPL          : TFRE_DB_PL_NET_CLIENT;
      FId             : NativeInt;
      FConnectState   : TPL_CONN_CSTATE;
      FCMDState       : TPL_CMD_STATE;
      FLen            : Cardinal;
      FData           : Pointer;
      FSpecfile       : Shortstring;
      FLayername      : TFRE_DB_NameType;
      FIp             : Shortstring;
      FPort           : Shortstring;
      FChannel        : IFRE_APSC_CHANNEL;
      FConnLock       : IFOS_Lock;
      FLayerWait      : IFOS_E;
      FLasterror      : String;
      FLastErrorCode  : TFRE_DB_Errortype;
      FCommandPending : boolean;
      FGlobal         : boolean;
      FAnswer         : IFRE_DB_Object;
      FCollections    : TList;
      constructor Create                     (nclient:TFRE_DB_PL_NET_CLIENT ; id:NativeInt ; ip,port,specfile,layername : Shortstring);
      destructor  Destroy                    ;override;
      procedure   LockLayer                  ;
      procedure   UnLockLayer                ;
      procedure   WaitForConnectStart        ;

      function    GetDboAsBufferLen          (const dbo: IFRE_DB_Object ; var mem : Pointer):UInt32;

      function    NewPersistenceLayerCommand (const cmdid: string) : IFRE_DB_Object;
      procedure   COR_SendDBO                (const Data : Pointer);
      function    SendCycle                  (const cmd : IFRE_DB_Object ; out answer : IFRE_DB_Object) : boolean;
      procedure   CheckRaiseAnswerError      (const answer : IFRE_DB_Object;const dont_raise : boolean=false);

      function    _CollectionExists          (const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION):Boolean;
      procedure   _AddCollection(const coll_name: TFRE_DB_NameType; const isVolatile: boolean);

      {PS Layer Interface }

      procedure DEBUG_DisconnectLayer         (const db:TFRE_DB_String;const clean_master_data :boolean = false);

      function  GetConnectedDB                : TFRE_DB_NameType;
      function  GetLastError                  : TFRE_DB_String;
      function  ExistCollection               (const coll_name : TFRE_DB_NameType) : Boolean;
      function  GetCollection                 (const coll_name : TFRE_DB_NameType ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION) : Boolean;
      function  NewCollection                 (const coll_name : TFRE_DB_NameType ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
      function  DeleteCollection              (const coll_name : TFRE_DB_NameType ) : TFRE_DB_TransStepId;

      function  Connect                       (const db_name:TFRE_DB_String ; out database_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false) : TFRE_DB_Errortype;
      function  DatabaseList                  : IFOS_STRINGS;
      function  DatabaseExists                (const dbname:TFRE_DB_String):Boolean;
      function  CreateDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
      function  DeleteDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
      procedure Finalize                      ;

      function  GetReferences                 (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
      function  GetReferencesCount            (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
      function  GetReferencesDetailed         (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;

      function  StartTransaction              (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType) : TFRE_DB_Errortype;
      function  Commit                        : boolean;
      procedure RollBack                      ;

      function  ObjectExists                  (const obj_uid : TGUID) : boolean;
      function  DeleteObject                  (const obj_uid : TGUID  ; const collection_name: TFRE_DB_NameType = ''):TFRE_DB_TransStepId;
      function  Fetch                         (const ouid   :  TGUID  ; out   dbo:IFRE_DB_Object ; const internal_object : boolean=false): boolean;
      function  StoreOrUpdateObject           (const obj : IFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean) : TFRE_DB_TransStepId;
      procedure SyncWriteWAL                  (const WALMem : TMemoryStream);
      procedure SyncSnapshot                  (const final : boolean=false);
      procedure SetNotificationStreamCallback (const change_if : IFRE_DB_DBChangedNotification);
      function  GetNotificationStreamCallback : IFRE_DB_DBChangedNotification;
    end;
  var
    FLayers            : TList;
    FLayerLock         : IFOS_LOCK;
    FStateTimer        : IFRE_APSC_TIMER;
    FGlobalConnectIP   : string;
    FGlobalConnectPort : string;
    FGlobalUnixSocket  : string;

    procedure   MyStateCheckTimer  (const TIM : IFRE_APSC_TIMER ; const flag1,flag2 : boolean); // Timout & CMD Arrived & Answer Arrived
    procedure   NewSocket          (const channel  : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure   ReadClientChannel  (const channel  : IFRE_APSC_CHANNEL);
    procedure   DiscoClientChannel (const channel  : IFRE_APSC_CHANNEL);
    procedure   NewDBOFromServer_Locked   (const pls : TPLNet_Layer ; const dbo : IFRE_DB_Object);
  public
    constructor Create;
    destructor  Destroy;override;
    procedure   SetConnectionDetails (const ip,port:string;const uxs:string);
    function    ConnectPLServer      (const name:TFRE_DB_NameType ; out conn_layer : IFRE_DB_PERSISTANCE_LAYER):TFRE_DB_Errortype;
    function    SearchForLayer       (const db_name : TFRE_DB_NameType ; out database_layer : IFRE_DB_PERSISTANCE_LAYER):boolean;
  end;

  function Get_PersistanceLayer_PS_Net(const ip,port:string) : IFRE_DB_PERSISTANCE_LAYER;

implementation

var GNET : TFRE_DB_PL_NET_CLIENT;
    GLAY : IFRE_DB_PERSISTANCE_LAYER;

function Get_PersistanceLayer_PS_Net(const ip, port: string): IFRE_DB_PERSISTANCE_LAYER;
begin
  GNET := TFRE_DB_PL_NET_CLIENT.Create;
  GNET.SetConnectionDetails(ip,port,'plsrv');
  CheckDbResult(GNET.ConnectPLServer('GLOBAL',GLAY),'',true,false,GLAY);
  result := GLAY;
end;

{ TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection }

constructor TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Create(const collname: TFRE_DB_NameType; const isVolatile: Boolean);
begin
  FName      := collname;
  FUppername := uppercase(FName);
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetPersLayerIntf: IFRE_DB_PERSISTANCE_COLLECTION_4_PERISTANCE_LAYER;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetPersLayer: IFRE_DB_PERSISTANCE_LAYER;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.IsVolatile: boolean;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.CollectionName(const unique: boolean): TFRE_DB_NameType;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Count: int64;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Exists(const ouid: TGUID): boolean;
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetAllUIDS(var uids: TFRE_DB_GUIDArray);
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Fetch(const uid: TGUID; var obj: IFRE_DB_Object): boolean;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.First: IFRE_DB_Object;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Last: IFRE_DB_Object;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetItem(const num: uint64): IFRE_DB_Object;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.DefineIndexOnField(const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean): TFRE_DB_Errortype;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetIndexedObj(const query_value: TFRE_DB_String; out obj: IFRE_DB_Object; const index_name: TFRE_DB_NameType): boolean;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetIndexedObj(const query_value: TFRE_DB_String; out obj: IFRE_DB_ObjectArray; const index_name: TFRE_DB_NameType; const check_is_unique: boolean): boolean;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetIndexedUID(const query_value: TFRE_DB_String; out obj_uid: TGUID; const index_name: TFRE_DB_NameType): boolean;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.GetIndexedUID(const query_value: TFRE_DB_String; out obj_uid: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const check_is_unique: boolean): boolean;
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.ForAllIndexed(var guids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const ascending: boolean; const max_count: NativeInt; skipfirst: NativeInt; const only_count_unique_vals: boolean);
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.ForAllIndexedSignedRange(const min_value, max_value: int64; var guids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const only_count_unique_vals: boolean);
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.ForAllIndexedUnsignedRange(const min_value, max_value: QWord; var guids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const only_count_unique_vals: boolean);
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.ForAllIndexedStringRange(const min_value, max_value: TFRE_DB_String; var guids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const ascending: boolean; const min_is_null: boolean; const max_is_max: boolean; const max_count: NativeInt; skipfirst: NativeInt; const only_count_unique_vals: boolean);
begin
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.ForAllIndexPrefixString(const prefix: TFRE_DB_String; var guids: TFRE_DB_GUIDArray; const index_name: TFRE_DB_NameType; const ascending: boolean; const max_count: NativeInt; skipfirst: NativeInt; const only_count_unique_vals: boolean);
begin
  abort;
end;

constructor TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Create(nclient: TFRE_DB_PL_NET_CLIENT; id: NativeInt; ip, port, specfile, layername: Shortstring);
begin
  Fid          := id;
  FIp          := ip;
  FPort        := port;
  FSpecfile    := specfile;
  FLayername   := uppercase(layername);
  FGlobal      := FLayername='GLOBAL';
  FNETPL       := nclient;
  FCollections := TList.Create;
  GFRE_TF.Get_Lock(FConnLock);
  GFRE_TF.Get_Event(FLayerWait);
end;

destructor TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Destroy;
begin
  FConnLock.Finalize;
  FLayerWait.Finalize;
  FCollections.free;
  inherited Destroy;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.LockLayer;
begin
  FConnLock.Acquire;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.UnLockLayer;
begin
  FConnLock.Release;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.WaitForConnectStart;
begin
  FLayerWait.WaitFor;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetDboAsBufferLen(const dbo: IFRE_DB_Object; var mem: Pointer): UInt32;
var len : UInt32;
    ns  : UInt32;
begin
  ns := dbo.NeededSize;
  Getmem(mem,ns+4);
  dbo.CopyToMemory(mem+4);
  PCardinal(mem)^:=ns;
  result := ns+4;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.NewPersistenceLayerCommand(const cmdid: string): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field('CID').AsString:=cmdid;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.COR_SendDBO(const Data: Pointer);
var cmd : IFRE_DB_Object;
    mem : pointer;
    siz : Cardinal;
begin
  cmd := TFRE_DB_Object(data);
  siz := GetDboAsBufferLen(cmd,mem);
  try
    cmd.Finalize;
    FChannel.CH_WriteBuffer(mem,siz);
  finally
    Freemem(mem);
  end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.SendCycle(const cmd: IFRE_DB_Object; out answer: IFRE_DB_Object): boolean;
begin
  assert(FCommandPending=false,'double send cycle!');
  FCommandPending := true;
  FChannel.GetChannelManager.ScheduleCoRoutine(@self.COR_SendDBO,cmd.Implementor);
  FLayerWait.WaitFor;
  answer := FAnswer;
  FAnswer:=nil;
  FCommandPending:=false;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.CheckRaiseAnswerError(const answer: IFRE_DB_Object; const dont_raise: boolean);
var ec : TFRE_DB_Errortype;
    es : String;
begin
  try
    ec := TFRE_DB_Errortype(answer.Field('EC').AsInt16);
    es := answer.Field('ES').AsString;
    if (ord(ec)<ord(low(TFRE_DB_Errortype))) or
        (ord(ec)>ord(High(TFRE_DB_Errortype))) then
          begin
            FLastErrorCode := edb_INTERNAL;
            FLasterror     := es+' and the errorcode is out of bounds(!!)'+inttostr(ord(ec));
          end
  except
    on e:Exception do
      begin
        ec := edb_INTERNAL;
        es := 'CHECKERROR EXCEPTION '+e.Message+' '+es;
      end;
  end;
  FLasterror     := es;
  FLastErrorCode := ec;
  if (ec<>edb_OK) and
     (dont_raise=false) then
       raise EFRE_DB_Exception.Create(ec,es);
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer._CollectionExists(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): Boolean;
var i   : NativeInt;
    cnu : TFRE_DB_NameType;
begin
  cnu := UpperCase(coll_name);
  for i := 0 to FCollections.Count-1 do
    if  TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection(FCollections[i]).CollectionName(true)=cnu then
      begin
        Collection := TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection(FCollections[i]);
        exit(true);
      end;
  exit(false);
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer._AddCollection(const coll_name: TFRE_DB_NameType;const isVolatile : boolean);
var ci : IFRE_DB_PERSISTANCE_COLLECTION;
begin
 if not _CollectionExists(coll_name,ci) then
   FCollections.Add(TFRE_DB_PL_NET_CLIENT.TPLNet_PersistanceCollection.Create(coll_name,isVolatile));
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DEBUG_DisconnectLayer(const db: TFRE_DB_String; const clean_master_data: boolean);
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetConnectedDB: TFRE_DB_NameType;
begin
  result := FLayername;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetLastError: TFRE_DB_String;
//var cmd,answer : IFRE_DB_Object;
//    s          : string;
begin
  result := FLasterror;   {Get's set on every cmd execution as answer from partner}
  //cmd := NewPersistenceLayerCommand('GLE');
  //SendCycle(cmd,answer);
  //try
  //  CheckRaiseAnswerError(answer);
  //  FLasterror := answer.Field('A').AsString;
  //  result     := FLasterror;
  //finally
  //  answer.Finalize;
  //end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.ExistCollection(const coll_name: TFRE_DB_NameType): Boolean;
var cmd,answer : IFRE_DB_Object;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer')
  else
    begin
      cmd := NewPersistenceLayerCommand('EC');
      cmd.Field('CN').AsString:=coll_name;
      SendCycle(cmd,answer);
      try
        CheckRaiseAnswerError(answer);
        result := answer.Field('A').AsBoolean;
        FLasterror     := '';
        FLastErrorCode := edb_OK;
      finally
        answer.Finalize;
      end;
    end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): Boolean;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer')
  else
    result := _CollectionExists(coll_name,Collection);
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.NewCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
var cmd,answer : IFRE_DB_Object;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  begin
    cmd := NewPersistenceLayerCommand('NC');
    cmd.Field('cn').AsString:=coll_name;
    cmd.Field('v').AsBoolean:=volatile_in_memory;
    SendCycle(cmd,answer);
    try
      CheckRaiseAnswerError(answer);
      result := answer.Field('TSID').AsString;
      FLasterror     := '';
      FLastErrorCode := edb_OK;
      _AddCollection(coll_name,volatile_in_memory);
    finally
      answer.Finalize;
    end;
  end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DeleteCollection(const coll_name: TFRE_DB_NameType): TFRE_DB_TransStepId;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Connect(const db_name: TFRE_DB_String; out database_layer: IFRE_DB_PERSISTANCE_LAYER; const drop_wal: boolean): TFRE_DB_Errortype;
begin
  if not FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is only allowed in then global layer');
  if FNETPL.SearchForLayer(db_name,database_layer) then
    exit(edb_OK);
  result := FNETPL.ConnectPLServer(db_name,database_layer);
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DatabaseList: IFOS_STRINGS;
var cmd,answer : IFRE_DB_Object;
    dba        : TFRE_DB_StringArray;
    i          : NativeInt;
begin
  cmd := NewPersistenceLayerCommand('DL');
  SendCycle(cmd,answer);
  try
    CheckRaiseAnswerError(answer);
    dba := answer.Field('A').AsStringArr;
    result := GFRE_TF.Get_FOS_Strings;
    for i:=0 to high(dba) do
      begin
        result.Add(dba[i]);
      end;
    FLasterror     := '';
    FLastErrorCode := edb_OK;
  finally
    answer.Finalize;
  end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DatabaseExists(const dbname: TFRE_DB_String): Boolean;
var cmd,answer : IFRE_DB_Object;
begin
  cmd := NewPersistenceLayerCommand('DE');
  cmd.Field('DB').AsString:=dbname;
  SendCycle(cmd,answer);
  try
    CheckRaiseAnswerError(answer);
    result := answer.Field('A').AsBoolean;
    FLasterror     := '';
    FLastErrorCode := edb_OK;
  finally
    answer.Finalize;
  end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.CreateDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
var cmd,answer : IFRE_DB_Object;
begin
  cmd := NewPersistenceLayerCommand('CD');
  cmd.Field('DB').AsString:=dbname;
  SendCycle(cmd,answer);
  try
    CheckRaiseAnswerError(answer,true);
    result := FLastErrorCode;
  finally
    answer.Finalize;
  end;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DeleteDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
var cmd,answer : IFRE_DB_Object;
begin
  cmd := NewPersistenceLayerCommand('DD');
  cmd.Field('DB').AsString:=dbname;
  SendCycle(cmd,answer);
  try
    CheckRaiseAnswerError(answer,true);
    result := FLastErrorCode;
  finally
    answer.Finalize;
  end;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Finalize;
begin
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetReferences(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetReferencesCount(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetReferencesDetailed(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_ObjectReferences;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Commit: boolean;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.RollBack;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.ObjectExists(const obj_uid: TGUID): boolean;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.DeleteObject(const obj_uid: TGUID; const collection_name: TFRE_DB_NameType): TFRE_DB_TransStepId;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.Fetch(const ouid: TGUID; out dbo: IFRE_DB_Object; const internal_object: boolean): boolean;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.StoreOrUpdateObject(const obj: IFRE_DB_Object; const collection_name: TFRE_DB_NameType; const store: boolean): TFRE_DB_TransStepId;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.SyncWriteWAL(const WALMem: TMemoryStream);
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.SyncSnapshot(const final: boolean);
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

procedure TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.SetNotificationStreamCallback(const change_if: IFRE_DB_DBChangedNotification);
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

function TFRE_DB_PL_NET_CLIENT.TPLNet_Layer.GetNotificationStreamCallback: IFRE_DB_DBChangedNotification;
begin
  if FGlobal then
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'operation is not allowed in then global layer');
  abort;
end;

{ TFRE_DB_PL_NET_CLIENT.TPLNet_Layer }

procedure TFRE_DB_PL_NET_CLIENT.NewSocket(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
var layr : TPLNet_Layer;
    id   : NativeInt;
begin
  if channel_event=ch_NEW_CS_CONNECTED then
    begin
      FLayerlock.Acquire;
      try
        id   := StrToInt(channel.CH_GetID);
        layr := TPLNet_Layer(FLayers[id]);
        layr.FConnectState := sfc_NEGOTIATE_LAYER;
        layr.FCMDState     := cs_READ_LEN;
        channel.CH_AssociateData(FREDB_ObjectToPtrUInt(layr));
        channel.CH_WriteString(layr.FLayername);
        channel.CH_Enable_Reading;
      finally
        FLayerLock.Release;
      end;
    end
  else
    begin
      channel.Finalize;
    end;
end;

procedure TFRE_DB_PL_NET_CLIENT.ReadClientChannel(const channel: IFRE_APSC_CHANNEL);
var layr  : TPLNet_Layer;
    id    : NativeInt;

    procedure _FetchDBO;
    var
        len   : cardinal;
        fcont : boolean;
        dbo   : IFRE_DB_Object;
    begin
      repeat
        fcont := false;
        case layr.FCMDState of
          cs_READ_LEN:
            begin
              if channel.CH_GetDataCount>=4 then
                begin
                  channel.CH_ReadBuffer(@layr.FLen,4);
                  fcont := true;
                  getmem(layr.FData,layr.FLen);
                  layr.FCMDState:=cs_READ_DATA;
                end;
            end;
          cs_READ_DATA:
            begin
              if channel.CH_GetDataCount>=layr.FLen then
                begin
                  channel.CH_ReadBuffer(layr.FData,layr.FLen);
                  fcont := true;
                  try
                    try
                      dbo := GFRE_DBI.CreateFromMemory(layr.FData);
                      try
                        NewDBOFromServer_Locked(layr,dbo);
                      except on e:exception do
                        begin
                          GFRE_DBI.LogError(dblc_PERSITANCE,'FAILURE INBOUND EVENT PROCESSING [%s]',[e.Message]);
                        end;
                      end;
                    finally
                      Freemem(layr.FData);
                      layr.FData:=nil;
                    end;
                  except on e:exception do
                    begin
                      writeln('SUB CHANNEL READ FAILED ',e.Message);
                      channel.Finalize;
                      layr.FConnectState := sfc_NOT_CONNECTED;
                    end;
                  end;
                  layr.FCMDState := cs_READ_LEN;
                end;
            end;
        end;
      until fcont=false;
    end;

    procedure _NegotiateLayerAnswer;
    var answer:string;
    begin
      answer := channel.CH_ReadString;
      if answer='OK' then
        begin
          layr.FConnectState := sfc_OK;
          layr.FChannel:=channel;
          layr.FLayerWait.SetEvent;
        end
      else
        begin
          channel.Finalize;
          layr.FConnectState := sfc_Failed;
          layr.FLayerWait.SetEvent;
        end;
    end;

begin
  layr := FREDB_PtrUIntToObject(channel.CH_GetAssociateData) as TPLNet_Layer;
  layr.LockLayer;
  try
    case layr.FConnectState of
      sfc_NOT_CONNECTED,sfc_TRYING:
        GFRE_BT.CriticalAbort('invalid state, read clientchannel '+IntToStr(ord(layr.FConnectState)));
      sfc_NEGOTIATE_LAYER:
        _NegotiateLayerAnswer;
      sfc_OK:
        _FetchDBO;
    end;
  finally
    layr.UnLockLayer;
  end;
end;

procedure TFRE_DB_PL_NET_CLIENT.DiscoClientChannel(const channel: IFRE_APSC_CHANNEL);
var layr : TPLNet_Layer;
begin
  layr := TPLNet_Layer(FLayers[strtoint(channel.CH_GetID)]);
  if assigned(layr) then
    begin
      layr.LockLayer;
      try
        case layr.FConnectState of
          sfc_TRYING:
            begin {Disconnect in trying = connection refused}
              layr.FLasterror := 'CONNECTION REFUSED';
              layr.FConnectState:=sfc_Failed;
              layr.FLayerWait.SetEvent;
            end;
          sfc_NOT_CONNECTED: ;
            //GFRE_BT.CriticalAbort('invalid state, read clientchannel '+IntToStr(ord(layr.FConnectState)));
          sfc_NEGOTIATE_LAYER: ;
            //_NegotiateLayerAnswer;
          sfc_OK: ;
        end;
      finally
        layr.UnLockLayer;
      end;
    end;
end;

procedure TFRE_DB_PL_NET_CLIENT.NewDBOFromServer_Locked(const pls: TPLNet_Layer; const dbo: IFRE_DB_Object);
begin
  if dbo.Field('CID').AsString='EVENT' then {Process Event}
    begin

    end
  else
    begin {It's an answer}
      assert(pls.FCommandPending=true,'sequencing error cmd not pending!');
      pls.FAnswer := dbo;
      pls.FLayerWait.SetEvent;
    end;
end;

{ TFRE_DB_PL_NET_CLIENT }

procedure TFRE_DB_PL_NET_CLIENT.MyStateCheckTimer(const TIM: IFRE_APSC_TIMER; const flag1, flag2: boolean);
var i : NativeInt;
begin
  FLayerLock.Acquire;
  try
    if (flag1=false) and (flag2=false) then
      for i := 0 to FLayers.Count-1 do
        with TPLNet_Layer(FLayers[i]) do
          case FConnectState of
            sfc_NOT_CONNECTED:
              begin // Start a client
                FConnectState:=sfc_TRYING;
                if FSpecfile<>'' then
                  GFRE_SC.AddClient_UX(FSpecfile,inttostr(i),nil,@NewSocket,@ReadClientChannel,@DiscoClientChannel)
                else
                  GFRE_SC.AddClient_TCP(FIp,FPort,inttostr(i),nil,@NewSocket,@ReadClientChannel,@DiscoClientChannel);
              end;
              sfc_TRYING: ; // do nothing
              sfc_OK: ; // do nothing
            end;
  finally
    FLayerLock.Release;
  end;
end;

constructor TFRE_DB_PL_NET_CLIENT.Create;
begin
  FLayers     := TList.Create;
  GFRE_TF.Get_Lock(FLayerLock);
  FStateTimer := GFRE_SC.AddTimer('CS',1000,@MyStateCheckTimer);
end;

destructor TFRE_DB_PL_NET_CLIENT.Destroy;
begin
  FLayers.free;
  FLayerLock.Finalize;
  inherited Destroy;
end;

procedure TFRE_DB_PL_NET_CLIENT.SetConnectionDetails(const ip, port: string; const uxs: string);
begin
  FGlobalConnectIP   := Ip;
  FGlobalConnectPort := port;
  FGlobalUnixSocket  := cFRE_UX_SOCKS_DIR+uxs;
end;

function TFRE_DB_PL_NET_CLIENT.ConnectPLServer(const name: TFRE_DB_NameType; out conn_layer: IFRE_DB_PERSISTANCE_LAYER): TFRE_DB_Errortype;
var lay : TPLNet_Layer;
begin
  FLayerLock.Acquire;
  try
    lay := TPLNet_Layer.Create(self,FLayers.Count,FGlobalConnectIP,FGlobalConnectPort,FGlobalUnixSocket,name);
    FLayers.Add(lay);
  finally
    FLayerLock.Release;
  end;
  conn_layer := lay;
  FStateTimer.TIM_Trigger;
  lay.WaitForConnectStart;
  case lay.FConnectState of
    sfc_OK:
      begin
        result     := edb_OK;
      end;
    sfc_Failed:
      begin
        result := edb_ERROR;
      end
    else
      result := edb_INTERNAL;
  end;
end;

function TFRE_DB_PL_NET_CLIENT.SearchForLayer(const db_name: TFRE_DB_NameType; out database_layer: IFRE_DB_PERSISTANCE_LAYER): boolean;
var i : NativeInt;
begin
  result := false;
  FLayerLock.Acquire;
  try
    for i := 0 to FLayers.count-1 do
      if uppercase(TPLNet_Layer(FLayers[i]).FLayername)=uppercase(db_name) then
        begin
          database_layer := TPLNet_Layer(FLayers[i]);
          result         := true;
        end;
    database_layer := nil;
    result := false;
  finally
    FLayerLock.Release;
  end;
end;


end.

