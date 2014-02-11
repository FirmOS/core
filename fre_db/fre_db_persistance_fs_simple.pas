unit fre_db_persistance_fs_simple;

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

interface

uses  Classes, SysUtils,FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,fre_db_persistance_common,fos_sparelistgen,baseunix,FRE_SYSTEM;

function Get_PersistanceLayer_PS_Simple(const basedir:TFRE_DB_String):IFRE_DB_PERSISTANCE_LAYER;

//{$IFDEF DARWIN}
function  fredbps_fsync(filedes : cint): cint; cdecl; external 'c' name 'fsync';
//{$ENDIF}

   { TFRE_DB_PS_FILE }

   // The global layer is used to initial build and connect the specialized database layers (clone per db)
   // there exists one system database, and n user databases
   // masterdata is global volatile shared, system persistent shared, and per db shared

  var  FTransaction         : TFRE_DB_TransactionalUpdateList;

  type

   TFRE_DB_PS_FILE = class(TObject,IFRE_DB_PERSISTANCE_LAYER)
   private
     FMaster               : TFRE_DB_Master_Data; // Global, Volatile, Per DB, at least one for system
     FBasedirectory        : TFRE_DB_String;
     FMasterCollDir        : TFRE_DB_String;
     FLocalConnDir         : TFRE_DB_String;
     FCollectionsDir       : TFRE_DB_String;
     FMetaDir              : TFRE_DB_String;
     FWalDir               : TFRE_DB_String;
     FWalFilename          : TFRE_DB_String;
     FWalStream            : THandleStream;
     FConnected            : Boolean;
     FGlobalLayer          : Boolean;
     FBlockWALWrites       : Boolean;
     FConnectedLayers      : Array of TFRE_DB_PS_FILE;
     FConnectedDB          : TFRE_DB_String;
     FLastError            : TFRE_DB_String;
     FLastErrorCode        : TFRE_DB_Errortype;
     FChangeNotificationIF : IFRE_DB_DBChangedNotification;
     FChangeNotificationProxy : TFRE_DB_DBChangedNotificationProxy;

     procedure    _ConnectCheck             ;
     procedure    _SetupDirs                (const db_name:TFRE_DB_String);
     function     EscapeDBName              (const name:string):string;
     function     UnEsacpeDBName            (const name:string):string;
     constructor  InternalCreate            (const basedir,name:TFRE_DB_String ; out result:TFRE_DB_Errortype);

     function     GetCollection             (const coll_name : TFRE_DB_NameType ; out Collection:IFRE_DB_PERSISTANCE_COLLECTION) : Boolean;
     function     ExistCollection           (const coll_name : TFRE_DB_NameType) : Boolean;


     procedure   _OpenWAL                   ;
     procedure   _CloseWAL                  ;
     procedure   _ClearWAL                  ;
     procedure   _StoreCollectionPersistent (const coll:IFRE_DB_PERSISTANCE_COLLECTION ; const no_storelocking : boolean=false);
     procedure   _StoreObjectPersistent     (const obj:TFRE_DB_Object ; const no_storelocking : boolean=false);

     procedure   WT_StoreCollectionPersistent  (const coll:IFRE_DB_PERSISTANCE_COLLECTION);
     procedure   WT_DeleteCollectionPersistent (const coll:IFRE_DB_PERSISTANCE_COLLECTION);
     procedure   WT_StoreObjectPersistent      (const obj: IFRE_DB_Object; const no_store_locking: boolean=true);
     procedure   WT_DeleteObjectPersistent     (const iobj:IFRE_DB_Object);


     {< Backup Functionality}
     function    FDB_GetObjectCount            (const coll:boolean): Integer;
     procedure   FDB_ForAllObjects             (const cb:IFRE_DB_Obj_Iterator);
     procedure   FDB_ForAllColls               (const cb:IFRE_DB_Obj_Iterator);
     procedure   FDB_PrepareDBRestore          (const phase:integer);
     procedure   FDB_SendObject                (const obj:IFRE_DB_Object);
     procedure   FDB_SendCollection            (const obj:IFRE_DB_Object);
     { Backup Functionality >}


     procedure   _LoadCollectionPersistent  (const file_name : string);
     procedure   _LoadObjectPersistent      (const UID: TGuid; var obj: TFRE_DB_Object);
     procedure   _SyncDBInternal            (const final:boolean=false);

     function   _InternalFetchConnectedLayer(db_name:TFRE_DB_String;var idx :NativeInt): TFRE_DB_PS_FILE;

     procedure  DEBUG_DisconnectLayer (const db:TFRE_DB_String;const clean_master_data :boolean = false);
   public
     function    GetConnectedDB               : TFRE_DB_NameType;

     constructor Create                     (const basedir,name:TFRE_DB_String);
     destructor  Destroy                    ; override;
     procedure   Finalize                   ;



     function    DatabaseList       : IFOS_STRINGS;
     function    DatabaseExists     (const dbname:TFRE_DB_String):Boolean;
     function    CreateDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeleteDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;


     function    GetReferences         (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
     function    GetReferencesCount    (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
     function    GetReferencesDetailed (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;


     function    Connect             (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false) : TFRE_DB_Errortype;
     function    ObjectExists        (const obj_uid : TGUID) : boolean;
     function    Fetch               (const ouid:TGUID;out dbo:IFRE_DB_Object;const internal_object:boolean): boolean;
     function    FetchO              (const ouid:TGUID;out dbo:TFRE_DB_Object;const internal_object:boolean): boolean;

     { Transactional Operations / These operations report the last transaction step id generated, there may be more then one generated }

     function    NewCollection       (const coll_name : TFRE_DB_NameType ; const CollectionClassname : Shortstring ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
     function    DeleteCollection    (const coll_name : TFRE_DB_NameType) : TFRE_DB_TransStepId; // todo transaction context
     function    StoreOrUpdateObject (const   iobj:IFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean) : TFRE_DB_TransStepId;
     function    DefineIndexOnField  (const coll_name: TFRE_DB_NameType ; const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean ; const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false): TFRE_DB_TransStepId;


     { Delete Operation :  collection_name = '' delete from all ->  collectionname<>'' only remove from collection }
     function    DeleteObject        (const obj_uid : TGUID    ;  const collection_name: TFRE_DB_NameType = '') : TFRE_DB_TransStepId;
     function    StartTransaction    (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType='T') : TFRE_DB_Errortype;
     function    Commit              : boolean;
     procedure   RollBack            ;
     // Transactional Operations Done

     procedure   SyncWriteWAL        (const WALMem : TMemoryStream);
     procedure   SyncSnapshot        (const final : boolean=false);
     function    GetLastError        : TFRE_DB_String;
     function    GetLastErrorCode    : TFRE_DB_Errortype;
     procedure   SetNotificationStreamCallback (const change_if : IFRE_DB_DBChangedNotification ; const create_proxy : boolean=true);
     function    GetNotificationStreamCallback : IFRE_DB_DBChangedNotification;
   end;

implementation

var GNOTIF_LOG : TFRE_DB_DBChangedNotificationBase;

function Get_PersistanceLayer_PS_Simple(const basedir: TFRE_DB_String): IFRE_DB_PERSISTANCE_LAYER;
var l_Persistance_Layer : TFRE_DB_PS_FILE;
begin
  if not assigned(GNOTIF_LOG) then
    GNOTIF_LOG := TFRE_DB_DBChangedNotificationBase.Create;
  l_Persistance_Layer := TFRE_DB_PS_FILE.Create(basedir,'BASE');
  result              := l_Persistance_Layer;
end;


procedure TFRE_DB_PS_FILE._ConnectCheck;
begin
  if not(FConnected) then
    GFRE_BT.CriticalAbort('operation is not allowed on a unconnected persistance layer');
end;

procedure TFRE_DB_PS_FILE._SetupDirs(const db_name: TFRE_DB_String);
begin
  FLocalConnDir   := SetDirSeparators(FBaseDirectory+'/'+EscapeDBName(db_name)+'/');
  FMasterCollDir  := SetDirSeparators(FLocalConnDir+'/MASTER/');
  FCollectionsDir := SetDirSeparators(FLocalConnDir+'/Colls/');
  FMetaDir        := SetDirSeparators(FLocalConnDir+'/Meta/');
  FWalDir         := SetDirSeparators(FLocalConnDir+'/WAL/');
  FWalFilename    := SetDirSeparators(FWalDir+'/'+'FREDB_WAL.log');
end;


function TFRE_DB_PS_FILE.EscapeDBName(const name: string): string;
begin
  result := GFRE_BT.Str2HexStr(uppercase(name));
end;

function TFRE_DB_PS_FILE.UnEsacpeDBName(const name: string): string;
begin
  result := GFRE_BT.HexStr2Str(name);
end;

constructor TFRE_DB_PS_FILE.InternalCreate(const basedir, name: TFRE_DB_String; out result: TFRE_DB_Errortype);

    procedure _BuildMasterCollection;
      procedure add_guid(file_name:AnsiString);
      var g   : TGUID;
          obj : TFRE_DB_Object;
      begin
        g := GFRE_BT.HexString_2_GUID(Copy(file_name,1,32));
        _LoadObjectPersistent(g,obj);
        result := FMaster.InternalStoreObjectFromStable(obj);
        if result<>edb_OK then
          raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RECREATE MEMORY FROM STABLE at [%s]',[GFRE_BT.GUID_2_HexString(g)]);
      end;
    begin
      GFRE_BT.List_Files(FMasterCollDir,@add_guid);
      result := FMaster.InternalRebuildRefindex;
      if result<>edb_OK then
        raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RECREATE REFERENTIAL INTEGRITY FROM STABLE');
      FMaster.InternalStoreLock;
    end;

    procedure _BuildCollections;
      procedure add_collection(file_name:AnsiString);
      begin
        _LoadCollectionPersistent(file_name);
      end;
    begin
      GFRE_BT.List_Files(FCollectionsDir,@add_collection);
    end;

    procedure _BuildMetaData;
    var m        : TMemorystream;
        filename : TFRE_DB_String;
        obj      : TFRE_DB_Object;
    begin
      filename   := 'reflinks.fdbo';
      if FileExists(FMetaDir+filename) then begin
        m:=TMemoryStream.Create;
        try
          m.LoadFromFile(FMetaDir+filename);
          obj:= TFRE_DB_Object.CreateFromMemory(m.Memory);
        finally
          m.free;
        end;
        //meta_cb(fdbmt_Reflinks,obj);
      end;
    end;

    function File_Size (const File_Name : String) : Int64;
    var F : File;
    begin
       Assign (F, File_Name);
       try
          Reset (F, 1);
          try
            exit (FileSize (F));
          finally
             Close (F);
          end
       except
       end {try};
       exit (-1);
    end;

    procedure _RepairWithWAL;
    var fs : TFileStream;
    begin
      fs := TFileStream.Create(FWalFilename,fmOpenRead);
      try
        FMaster.ApplyWAL(fs);
      finally
         fs.Free;
      end;
    end;

begin
  FBasedirectory := basedir;
  if not DirectoryExists(FBaseDirectory) then begin
    if not ForceDirectories(FBaseDirectory) then begin
      GFRE_BT.CriticalAbort('cannot setup basedirectory');
    end;
  end;
  _SetupDirs(name);
  if not DirectoryExists(FLocalConnDir)   then
    begin
      result := edb_NOT_FOUND;
      exit;
    end;
  if not DirectoryExists(FMasterCollDir)  then
    begin
      result := edb_NOT_FOUND;
      exit;
    end;
  if not DirectoryExists(FCollectionsDir) then
    begin
      result := edb_NOT_FOUND;
      exit;
    end;
  if not DirectoryExists(FMetaDir)        then
    begin
      result := edb_NOT_FOUND;
      exit;
    end;
  FConnectedDB := name;
  if GDROP_WAL then
    _ClearWAL;
  if FileExists(FWalFilename)
     and (File_Size(FWalFilename)=0) then
       _ClearWAL;
  FMaster := TFRE_DB_Master_Data.Create(FConnectedDB,self);
  FBlockWALWrites:=true;
  _BuildMasterCollection;
  _BuildCollections;
  FBlockWALWrites:=false;
  if FileExists(FWalFilename) then
    begin
      _ClearWAL;
      //HACK
      //Autorepair
      //_RepairWithWAL;
      //raise EFRE_DB_PL_Exception.Create(edb_ERROR,'SERVER SHUTDOWN WAS UNCLEAN, MUST REAPPLY WAL FOR [%s]',[FConnectedDB]);
    end;
  result := edb_OK;
  _OpenWAL;
  FConnected   := true;
  FGlobalLayer := false;
end;

function TFRE_DB_PS_FILE.GetCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): Boolean;
begin
  result := FMaster.MasterColls.GetCollection(coll_name,Collection);
  if (not result)
     and (FGlobalLayer=false) then // Fetch from Global
      result := GFRE_DB_PS_LAYER.GetCollection(coll_name,Collection);
end;

function TFRE_DB_PS_FILE.ExistCollection(const coll_name: TFRE_DB_NameType): Boolean;
var dummy : IFRE_DB_PERSISTANCE_COLLECTION;
begin
  result := GetCollection(coll_name,dummy);
end;

procedure TFRE_DB_PS_FILE._OpenWAL;
var handle : THandle;
    res    : longint;
begin
  FWalStream := nil;
  if FileExists(FWalFilename) then
    begin
      repeat
        handle := FpOpen(pointer(FWalFilename),O_WRONLY or O_APPEND); // O_SYNC
        res    := fpgeterrno;
      until (handle<>-1) or (res<>ESysEINTR);
      if (handle<0)  then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
      FWalStream := THandleStream.Create(handle);
    end
  else
    begin
      if GDISABLE_WAL then
        exit; // do not create a new WAL File
      repeat
        handle := FpOpen(pointer(FWalFilename),O_CREAT or O_WRONLY or O_APPEND); // O_SYNC
        res    := fpgeterrno;
      until (handle<>-1) or (res<>ESysEINTR);
      if (handle<0)  then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
      FWalStream := THandleStream.Create(handle);
    end;
end;

procedure TFRE_DB_PS_FILE._CloseWAL;
begin
  FpClose(FWalStream.Handle);
  FWalStream.Free;
  FWalStream:=nil;
end;

procedure TFRE_DB_PS_FILE._ClearWAL;
begin
  if GDISABLE_WAL then
    exit; // do not delete a  WAL File
  if FileExists(FWalFilename) then
    if not DeleteFile(FWalFilename) then
      raise EFRE_DB_PL_Exception.Create(edb_ERROR,'WAL DELETE FAILED ['+FConnectedDB+']');
end;

procedure TFRE_DB_PS_FILE._StoreCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION; const no_storelocking: boolean);
var f : TFileStream;
begin
  if not coll.IsVolatile then
    begin
      //writeln('  ->> SYNCING COLL ',coll.CollectionName(false));
      GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
      f :=  TFileStream.Create(FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName(false))+'.col',fmCreate+fmOpenReadWrite);
      try
        coll.GetPersLayerIntf.StreamToThis(f);
      finally
        f.free;
      end;
    end;
end;

procedure TFRE_DB_PS_FILE._LoadCollectionPersistent(const file_name: string);
var f    : TFileStream;
    res  : TFRE_DB_Errortype;
    coll : IFRE_DB_PERSISTANCE_COLLECTION;
    name : TFRE_DB_NameType;
begin
  name := GFRE_BT.HexStr2Str(Copy(file_name,1,Length(file_name)-4));
  GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>LOAD COLLECTION [%s]',[name]);
  f :=  TFileStream.Create(FCollectionsDir+file_name,fmOpenRead);
  try
    res := FMaster.MasterColls.NewCollection(name,'*',coll,false,self);
    if res <> edb_OK then
      raise EFRE_DB_PL_Exception.Create(res,'LOAD COLLECTION FROM STABLE FAILED FOR [%s]',[name]);
    coll.GetPersLayerIntf.LoadFromThis(f);
  finally
    f.free;
  end;
end;

procedure TFRE_DB_PS_FILE._StoreObjectPersistent(const obj: TFRE_DB_Object; const no_storelocking: boolean);
var  FileName : string[60];
     m        : TMemoryStream;

begin
  if obj.IsObjectRoot then
    begin
      if not no_storelocking then
        begin
          obj.Assert_CheckStoreLocked;
          obj.Set_Store_Locked(false);
        end;
      try
        obj._InternalGuidNullCheck;
        filename    := GFRE_BT.GUID_2_HexString(obj.UID)+'.fdbo';
        m:=TMemoryStream.Create;
        try
          m.Size:=obj.NeededSize;
          obj.CopyToMemory(m.Memory);
          m.SaveToFile(FMasterCollDir+filename);
        finally
          m.free;
        end;
        GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<STORE OBJECT : '+obj.UID_String+' DONE');
      finally
        if not no_storelocking then
          begin
            obj.Set_Store_Locked(true);
          end;
      end;
    end;
end;

procedure TFRE_DB_PS_FILE.WT_StoreCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  _StoreCollectionPersistent(coll);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  if not coll.IsVolatile then
    begin
      GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>DELETE COLLECTION [%s]',[coll.CollectionName]);
      if not DeleteFile(FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName(false))+'.col') then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete collection '+coll.CollectionName());
    end;
end;

procedure TFRE_DB_PS_FILE.WT_StoreObjectPersistent(const obj: IFRE_DB_Object ; const no_store_locking : boolean=true);
begin
  _StoreObjectPersistent(obj.Implementor as TFRE_DB_Object,no_store_locking);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteObjectPersistent(const iobj: IFRE_DB_Object);
var  FileName : string[60];
     m        : TMemoryStream;
     obj      : TFRE_DB_Object;

begin
  if iobj.IsObjectRoot then
    begin
      obj := iobj.Implementor as TFRE_DB_Object;
      filename    := GFRE_BT.GUID_2_HexString(obj.UID)+'.fdbo';
      if not DeleteFile(FMasterCollDir+FileName) then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete file '+FMasterCollDir+FileName);
      GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<FINAL DELETE  OBJECT : '+obj.UID_String+' DONE');
    end;
end;

function TFRE_DB_PS_FILE.FDB_GetObjectCount(const coll: boolean): Integer;
begin
  if coll then
    result := FMaster.MasterColls.GetCollectionCount
  else
    result := FMaster.GetPersistantRootObjectCount;
end;

procedure TFRE_DB_PS_FILE.FDB_ForAllObjects(const cb: IFRE_DB_Obj_Iterator);

  procedure ForAll(const obj : TFRE_DB_Object);
  var lock : boolean;
  begin
    if obj.IsObjectRoot then
      begin
        obj.Set_Store_LockedUnLockedIf(false,lock);
        try
          cb(obj);
        finally
          obj.Set_Store_LockedUnLockedIf(true,lock);
        end;
      end;
  end;

begin
  FMaster.ForAllObjectsInternal(true,false,@ForAll);
end;

procedure TFRE_DB_PS_FILE.FDB_ForAllColls(const cb: IFRE_DB_Obj_Iterator);

  procedure CollCB(const pcoll : TFRE_DB_Persistance_Collection);
  var obj : IFRE_DB_Object;
  begin
    obj := pcoll.BackupToObject;
    try
      cb(obj);
    finally
      obj.Finalize;
    end;
  end;

begin
  FMaster.MasterColls.ForAllCollections(@CollCB);
end;

procedure TFRE_DB_PS_FILE.FDB_PrepareDBRestore(const phase: integer);
var result : TFRE_DB_Errortype;
begin
  case phase of
    0 :
      begin
        DeleteDatabase(FConnectedDB);
        CreateDatabase(FConnectedDB);
        FMaster.FDB_CleanUpMasterData;
      end;
    1 :
      begin { Objects transferred, rebuild refindex}
        result := FMaster.InternalRebuildRefindex;
        if result<>edb_OK then
          raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RECREATE REFERENTIAL INTEGRITY FROM STABLE');
        FMaster.InternalStoreLock;
      end;
    100 :
      begin

      end;
    else
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'unexpected restore db phase %d',[phase]);
  end;
end;

procedure TFRE_DB_PS_FILE.FDB_SendObject(const obj: IFRE_DB_Object);
var result : TFRE_DB_Errortype;
begin
  result := FMaster.InternalStoreObjectFromStable(obj.Implementor as TFRE_DB_Object);
  WT_StoreObjectPersistent(obj,true);
  if result<>edb_OK then
    raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RESTORE OBJECT FROM BACKUP at [%s]',[obj.GetDescriptionID]);
end;

procedure TFRE_DB_PS_FILE.FDB_SendCollection(const obj: IFRE_DB_Object);
var res  : TFRE_DB_Errortype;
    coll : IFRE_DB_PERSISTANCE_COLLECTION;
    name : TFRE_DB_NameType;
begin
  name := obj.Field('CollectionName').AsString;
  res := FMaster.MasterColls.NewCollection(name,'*',coll,false,self);
  if res <> edb_OK then
    raise EFRE_DB_PL_Exception.Create(res,'LOAD COLLECTION FROM BACKUP FAILED FOR [%s]',[name]);
  coll.GetPersLayerIntf.RestoreFromObject(obj);
  WT_StoreCollectionPersistent(coll);
end;

procedure TFRE_DB_PS_FILE._LoadObjectPersistent(const UID: TGuid; var obj: TFRE_DB_Object);
var m          : TMemorystream;
    filename   : TFRE_DB_String;
    uid_string : TFRE_DB_String;
begin
  uid_string:=GFRE_BT.GUID_2_HexString(uid);
  //GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>RETRIEVE OBJECT [%s]',[uid_string]);
  filename   := uid_string+'.fdbo';
  m:=TMemoryStream.Create;
  try
    m.LoadFromFile(FMasterCollDir+filename);
    obj:= TFRE_DB_Object.CreateFromMemory(m.Memory);
  finally
    m.free;
  end;
  //GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<RETRIEVE OBJECT [%s] DONE',[uid_string]);
end;

procedure TFRE_DB_PS_FILE._SyncDBInternal(const final:boolean=false);

   procedure WriteColls(const coll:TFRE_DB_PERSISTANCE_COLLECTION);
   begin
     _StoreCollectionPersistent(coll);
   end;

   procedure StoreObjects(const obj : TFRE_DB_Object);
   begin
     _StoreObjectPersistent(obj);
   end;

begin
  if GDISABLE_SYNC then
    begin
      GFRE_DBI.LogNotice(dblc_PERSITANCE,'<<SKIPPING SYNC OF DB [%s] / WRITE THROUGH MODE',[FConnectedDB]);
      exit;
    end;

  FMaster.MasterColls.ForAllCollections(@WriteColls);
  FMaster.ForAllObjectsInternal(true,false,@StoreObjects);
  if assigned(FWalStream) then
    begin
      _CloseWAL;
      _ClearWAL;
      if not final then
        _OpenWAL;
    end;
end;

function TFRE_DB_PS_FILE._InternalFetchConnectedLayer(db_name:TFRE_DB_String;var idx :NativeInt): TFRE_DB_PS_FILE;
var i : Nativeint;
begin
  result  := nil;
  for i := 0 to high(FConnectedLayers) do
    if assigned(FConnectedLayers[i])
       and (FConnectedLayers[i].FConnectedDB=db_name) then
        begin
          idx := i;
          exit(FConnectedLayers[i]);
        end;
  idx :=-1;
end;

procedure TFRE_DB_PS_FILE.DEBUG_DisconnectLayer(const db: TFRE_DB_String; const clean_master_data: boolean);
var idx : NativeInt;
     l  : TFRE_DB_PS_FILE;
begin
  if not FGlobalLayer then
    raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'fail');
  if clean_master_data then
    FMaster.FDB_CleanUpMasterData;
  l := _InternalFetchConnectedLayer(db,idx);
  l.Free;
  FConnectedLayers[idx] := nil;
end;

function TFRE_DB_PS_FILE.GetConnectedDB: TFRE_DB_NameType;
begin
  result := FConnectedDB;
end;

function TFRE_DB_PS_FILE.NewCollection(const coll_name: TFRE_DB_NameType; const CollectionClassname: Shortstring; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
var coll                : TFRE_DB_Persistance_Collection;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    step                : TFRE_DB_NewCollectionStep;

begin
  CleanApply := false;
  try
    try
      if not assigned(FTransaction) then
        begin
          FTransaction        := TFRE_DB_TransactionalUpdateList.Create('C',Fmaster,FChangeNotificationIF);
          ImplicitTransaction := True;
        end;
      step := TFRE_DB_NewCollectionStep.Create(self,coll_name,CollectionClassname,volatile_in_memory);
      FTransaction.AddChangeStep(step);
      if ImplicitTransaction then
        FTransaction.Commit(self);
      CleanApply := true;
      Collection := step.GetNewCollection;
      result     := step.GetTransActionStepID;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
          raise;
        end;
    end;
  finally
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
        if CleanApply then
          result := '';
      end;
  end;
end;

function TFRE_DB_PS_FILE.DeleteCollection(const coll_name: TFRE_DB_NameType): TFRE_DB_TransStepId;
var coll                : TFRE_DB_Persistance_Collection;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    step                : TFRE_DB_DeleteCollectionStep;
begin
  CleanApply := false;
  try
    try
      if not assigned(FTransaction) then
        begin
          FTransaction        := TFRE_DB_TransactionalUpdateList.Create('DC',Fmaster,FChangeNotificationIF);
          ImplicitTransaction := True;
        end;
      step := TFRE_DB_DeleteCollectionStep.Create(self,coll_name);
      FTransaction.AddChangeStep(step);
      if ImplicitTransaction then
        FTransaction.Commit(self);
      CleanApply := true;
      result     := step.GetTransActionStepID;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
    end;
  finally
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
        if CleanApply then
          result := '';
      end;
  end;
end;


constructor TFRE_DB_PS_FILE.Create(const basedir,name: TFRE_DB_String);
var GSTRING   : String;
    MAX_GUID  : TGuid;
begin
  FBasedirectory := basedir;
  if not DirectoryExists(FBaseDirectory) then begin
    if not ForceDirectories(FBaseDirectory) then begin
      GFRE_BT.CriticalAbort('cannot setup basedirectory');
    end;
  end;
  FMaster       := TFRE_DB_Master_Data.Create('GLOBAL',self);
  FConnectedDB  := 'GLOBAL';
  FGlobalLayer  := True;
end;


destructor TFRE_DB_PS_FILE.Destroy;
var
  i: NativeInt;
begin
  if FGlobalLayer then
    begin
      for i:=0 to high(FConnectedLayers) do
        begin
          FConnectedLayers[i].Free;
        end;
      GNOTIF_LOG.Free;
    end
  else
    begin
      if Length(FConnectedLayers)>0 then
        raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'Connected Layer in non global layer is non empty!');
    end;
  FMaster.Free;

  inherited destroy;
end;

procedure TFRE_DB_PS_FILE.Finalize;
begin
  free;
end;


//function TFRE_DB_PS_FILE.StoreRefLinks(const obj: TFRE_DB_Object): TFRE_DB_Errortype;
//var needed_size : int64;
//    filename    : TFRE_DB_String;
//    m           : TMemorystream;
//begin
//  _ConnectCheck;
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>STORE REFERENCE LINKS');
//  result:=edb_OK;
//  needed_size := obj.NeededSize;
//  filename    := 'reflinks.fdbo';
//  m:=TMemoryStream.Create;
//  try
//    try
//      m.Size:=needed_size;
//      obj.CopyToMemory(m.Memory);
//      m.SaveToFile(FMetaDir+filename);
//    except on e:exception do begin
//      result := edb_ERROR;
//      exit;
//    end;end;
//  finally
//    m.free;
//  end;
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<STORE REFERENCE LINKS DONE');
//end;
//
//function TFRE_DB_PS_FILE.StoreCollection(const coll: TFRE_DB_COLLECTION): TFRE_DB_Errortype;
//var m:TMemoryStream;
//begin
//  _ConnectCheck;
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
//  result:=edb_OK;
//  m:=TMemoryStream.Create;
//  try
//    try
//    m.Size:=coll.NeededSize;
//    coll.CopyToMemory(m.Memory);
//    m.SaveToFile(FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName)+'.fdbo');
//    except on e:exception do begin
//      result := edb_ERROR;
//      exit;
//    end;end;
//  finally
//    m.free;
//  end;
//  inc(FLayerStats.StoreColls);
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<STORE COLLECTION [%s] DONE',[coll.CollectionName]);
//end;

//function TFRE_DB_PS_FILE.RetrieveCollection(const collname: TFRE_DB_NameType; var coll: TFRE_DB_COLLECTION; const manage_info: TFRE_DB_Collection_ManageInfo): TFRE_DB_Errortype;
//var m        : TMemorystream;
//    filename : TFRE_DB_String;
//begin
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>RETRIEVE COLLECTION [%s]',[collname]);
//  filename := GFRE_BT.Str2HexStr(collname)+'.fdbo';
//  m:=TMemoryStream.Create;
//  try
//    m.LoadFromFile(FCollectionsDir+filename);
//    coll:= TFRE_DB_Object.CreateFromMemory(m.Memory,manage_info.FConnection) as TFRE_DB_COLLECTION;
//  finally
//    m.free;
//  end;
//  result := edb_OK;
//  inc(FLayerStats.Retrieve);
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<RETRIEVE COLLECTION [%s] DONE',[collname]);
//end;

//function TFRE_DB_PS_FILE.DeleteCollection(const coll: TFRE_DB_COLLECTION): TFRE_DB_Errortype;
//var cname:TFRE_DB_String;
//begin
//  _ConnectCheck;
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>DELETE COLLECTION [%s]',[coll.CollectionName]);
//  cname:=FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName)+'.fdbo';
//  if not FileExists(cname) then exit(edb_NOT_FOUND);
//  try
//    if not DeleteFile(cname) then exit(edb_ERROR);
//  except
//    exit(edb_ERROR);
//  end;
//  result:=edb_OK;
//  inc(FLayerStats.DeleteColls);
//  GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<DELETE COLLECTION [%s] DONE',[coll.CollectionName]);
//end;


//function TFRE_DB_PS_FILE.DeleteObject(const UID: TGuid): TFRE_DB_Errortype;
//var filename  :TFRE_DB_String;
//    uid_string:TFRE_DB_String;
//_ConnectCheck;
//uid_string := GFRE_BT.GUID_2_HexString(uid);
//GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>DELETE OBJECT [%s]',[uid_string]);
//result := edb_OK;
//filename := uid_string+'.fdbo';
//if not DeleteFile(FMasterCollDir+filename) then begin
//  result := edb_ERROR;
//  exit;
//end;
//GFRE_DBI.LogDebug(dblc_PERSITANCE,'<<DELETE OBJECT [%s]',[uid_string]);
//begin
//end;

function TFRE_DB_PS_FILE.Fetch(const ouid: TGUID; out dbo: IFRE_DB_Object; const internal_object: boolean): boolean;
var dboo : TFRE_DB_Object;
begin
  result := FMaster.FetchObject(ouid,dboo,internal_object);
  if result then
    dbo := dboo
  else
    dbo := nil;
end;

function TFRE_DB_PS_FILE.FetchO(const ouid: TGUID; out dbo: TFRE_DB_Object; const internal_object: boolean): boolean;
begin
  result := FMaster.FetchObject(ouid,dbo,internal_object);
end;

function TFRE_DB_PS_FILE.DeleteObject(const obj_uid: TGUID; const collection_name: TFRE_DB_NameType): TFRE_DB_TransStepId;
var
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    delete_object       : TFRE_DB_Object;
    i                   : NativeInt;
    step                : TFRE_DB_ChangeStep;

begin
  CleanApply := false;
  try
    try
      if not assigned(FTransaction) then
        begin
          FTransaction        := TFRE_DB_TransactionalUpdateList.Create('ID',Fmaster,FChangeNotificationIF);
          ImplicitTransaction := True;
        end;
        //if collection_name='' then
        if not FetchO(obj_uid,delete_object,true) then
          raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be deleted but was not found [%s]',[GFRE_BT.GUID_2_HexString(obj_uid)]);

        //SetLength(notify_collections,Length(delete_object.__InternalGetCollectionList));
        //for i := 0 to high(notify_collections) do
        //  notify_collections[i] := delete_object.__InternalGetCollectionList[i].CollectionName();

        delete_object.Set_Store_Locked(false);
        if delete_object.IsObjectRoot then
           step := TFRE_DB_DeleteObjectStep.Create(self,delete_object,collection_name,false)
        else
           step := TFRE_DB_DeleteSubObjectStep.Create(self,delete_object,collection_name,false);
        FTransaction.AddChangeStep(step);
        result := step.GetTransActionStepID;
        if ImplicitTransaction then
          Commit;
        CleanApply := true;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
          raise;
        end;
    end;
  finally
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
        if CleanApply then
          result := '';
      end;
  end;
end;

// This is always the first entry into the store and update chain
function TFRE_DB_PS_FILE.StoreOrUpdateObject(const iobj: IFRE_DB_Object; const collection_name: TFRE_DB_NameType; const store: boolean): TFRE_DB_TransStepId;
var coll                : IFRE_DB_PERSISTANCE_COLLECTION;
    error               : TFRE_DB_Errortype;
    to_update_obj       : TFRE_DB_Object;
    change_list         : TFRE_DB_Object;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    i                   : NativeInt;
    changes             : Boolean;
    obj                 : TFRE_DB_Object;
    updatestep          : TFRE_DB_UpdateStep;

  procedure GeneralChecks;
  begin
    if obj.DomainID=CFRE_DB_NullGUID then
      raise EFRE_DB_PL_Exception.Create(edb_ERROR,'persistance failure, an object without a domainid cannot be stored');
    obj._InternalGuidNullCheck;
  end;

  procedure GenInsert(const insert_obj : IFRE_DB_Object);
  begin
    if insert_obj.IsObjectRoot then
      FTransaction.AddChangeStep(TFRE_DB_InsertStep.Create(self,insert_obj.Implementor as TFRE_DB_Object,coll,store))
    else
      FTransaction.AddChangeStep(TFRE_DB_InsertSubStep.Create(self,insert_obj.Implementor as TFRE_DB_Object,coll,store));
  end;

  procedure GenDelete(const del_obj : IFRE_DB_Object);
  begin
    assert(not del_obj.IsObjectRoot); { this must be a subobject delete }
    FTransaction.AddChangeStep(TFRE_DB_DeleteSubObjectStep.Create(self,del_obj.Implementor as TFRE_DB_Object,collection_name,store));
  end;

  procedure GenUpdate(const is_child_update : boolean ; const up_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_ifield, old_ifield: IFRE_DB_Field);
  var child      : TFRE_DB_Object;
      new_object : TFRE_DB_Object;
      old_fld,
      new_fld    : TFRE_DB_FIELD;
      s          : string;

  begin
    if assigned(old_ifield) then
      begin
        old_fld := old_ifield.Implementor as TFRE_DB_FIELD;
        s:=old_fld.FieldName;
      end
    else
      old_fld := nil;
    if assigned(new_ifield) then
      begin
        new_fld := new_ifield.Implementor as TFRE_DB_FIELD;
        s:=new_fld.FieldName;
      end
    else
      new_fld := nil;

    case update_type of
      cev_FieldDeleted:
          updatestep.addsubstep(cev_FieldDeleted,nil,old_fld,is_child_update,up_obj.Implementor as TFRE_DB_Object);
      cev_FieldAdded:
          updatestep.addsubstep(cev_FieldAdded,new_fld,nil,is_child_update,up_obj.Implementor as TFRE_DB_Object);
      cev_FieldChanged :
          updatestep.addsubstep(cev_FieldChanged,new_fld,old_fld,is_child_update,up_obj.Implementor as TFRE_DB_Object);
    end;
  end;


begin
  obj        := iobj.Implementor as TFRE_DB_Object;
  CleanApply := false;
  coll       := nil;
  try
    try
      GeneralChecks;
      if store then
        begin
          if not obj.IsObjectRoot then
            raise EFRE_DB_PL_Exception.Create(edb_UNSUPPORTED,'store of non root objects in a collection is not allowed');
          if collection_name='' then
            raise EFRE_DB_PL_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request');
          if not GetCollection(collection_name,coll) then
            raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the specified collection [%s] was not found',[collection_name]);
          if not assigned(FTransaction) then
            begin
              FTransaction        := TFRE_DB_TransactionalUpdateList.Create('S',Fmaster,FChangeNotificationIF);
              ImplicitTransaction := True;
            end;
          to_update_obj := nil;
          if coll.IsVolatile then
            obj.Set_Volatile;
          TFRE_DB_Object.GenerateAnObjChangeList(obj,nil,@GenInsert,@GenDelete,@GenUpdate);
          result := FTransaction.GetTransLastStepTransId;
          //writeln('>TRANSACTION INSERT LOG');
          //FTransaction.PrintTextLog;
          //writeln('<TRANSACTION INSERT LOG');
          //result := FTransaction.ProcessCheck(raise_ex);
          //if result <>edb_OK then
          //  exit(result);
          if ImplicitTransaction then
            FTransaction.Commit(self);
          obj.Set_Store_Locked(true);
          CleanApply := true;
        end
      else
        begin
          if not obj.IsObjectRoot then
            raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the object [%s] is a child object, only root objects updates are allowed',[obj.UID_String]);
          if not FetchO(obj.UID,to_update_obj,true) then
            raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be updated but was not found [%s]',[obj.UID_String]);
          if collection_name<>'' then
            if not GetCollection(collection_name,coll) then
              raise EFRE_DB_PL_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request');
          try
            if not assigned(FTransaction) then
              begin
                FTransaction        := TFRE_DB_TransactionalUpdateList.Create('U',Fmaster,FChangeNotificationIF);
                ImplicitTransaction := True;
              end else
                ImplicitTransaction := false;
              try
                 to_update_obj.Set_Store_Locked(false);
                 updatestep := TFRE_DB_UpdateStep.Create(self,obj,to_update_obj,false);
                 FTransaction.AddChangeStep(updatestep);
                 TFRE_DB_Object.GenerateAnObjChangeList(obj,to_update_obj,@GenInsert,@GenDelete,@GenUpdate);
                 result := FTransaction.GetTransLastStepTransId;
              finally
                to_update_obj.Set_Store_Locked(true);
              end;
              result := FTransaction.GetTransLastStepTransId;
              if ImplicitTransaction then
                changes := Commit;
            CleanApply := true;
            if not changes then
              result := '';
          finally
            obj.Finalize;
            obj:=nil;
          end;
        end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
          raise;
        end;
    end;
  finally
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
        if CleanApply then
          result := '';
      end;
  end;
end;

function TFRE_DB_PS_FILE.DefineIndexOnField(const coll_name: TFRE_DB_NameType ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    step                : TFRE_DB_DefineIndexOnFieldStep;

begin
  CleanApply := false;
  try
    try
      if not assigned(FTransaction) then
        begin
          FTransaction        := TFRE_DB_TransactionalUpdateList.Create('DIOF',Fmaster,FChangeNotificationIF);
          ImplicitTransaction := True;
        end;
      step := TFRE_DB_DefineIndexOnFieldStep.Create(self,coll_name,FieldName,FieldType,unique,ignore_content_case,index_name,allow_null_value,unique_null_values);
      FTransaction.AddChangeStep(step);
      if ImplicitTransaction then
        FTransaction.Commit(self);
      CleanApply := true;
      result     := step.GetTransActionStepID;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
          raise;
        end;
    end;
  finally
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
        if CleanApply then
          result := '';
      end;
  end;
end;

function TFRE_DB_PS_FILE.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  try
    if Assigned(FTransaction) then
     exit(edb_EXISTS);
    FTransaction := TFRE_DB_TransactionalUpdateList.Create(id,FMaster,FChangeNotificationIF);
    result := edb_OK;
  except
    on e:EFRE_DB_PL_Exception do
      begin
        FLastErrorCode := E.ErrorType;
        FLastError     := E.Message;
        GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
        raise;
      end;
    on e:EFRE_DB_Exception do
      begin
        FLastErrorCode := E.ErrorType;
        FLastError     := E.Message;
        GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
        raise;
      end;
    on e:Exception do
      begin
        FLastErrorCode := edb_INTERNAL;
        FLastError     := E.Message;
        GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
        raise;
      end;
  end;
end;

function TFRE_DB_PS_FILE.Commit: boolean;
begin
  try
    try
      result := FTransaction.Commit(self);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
          raise;
        end;
    end;
  finally
    FTransaction.Free;
    FTransaction := nil;
  end;
end;

procedure TFRE_DB_PS_FILE.RollBack;
begin
  try
    try
      FTransaction.Rollback;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSITANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSITANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSITANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
          raise;
        end;
    end;
  finally
    FTransaction.Free;
    FTransaction := nil;
  end;
end;

procedure TFRE_DB_PS_FILE.SyncWriteWAL(const WALMem: TMemoryStream); // TODO Check Performance impace of reopening WAL
var res:cint;
begin
  if GDISABLE_WAL then
    exit;
  if FBlockWALWrites then
    raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'DOING WAL WRITES WHILE BLOCKED !!');
  //writeln('####  ',FLayerStats.Name);
  if not assigned(FWalStream) then
    _OpenWAL;
  FWalStream.Write(WALMem.Memory^,WALMem.Position);
  res := fredbps_fsync(FWalStream.Handle);
  //writeln('WAL SYNC ',res);
end;

procedure TFRE_DB_PS_FILE.SyncSnapshot(const final: boolean);
var i : NativeInt;
begin
  if FGlobalLayer then
    begin
      for i := 0 to high(FConnectedLayers) do
        if assigned(FConnectedLayers[i]) then
          FConnectedLayers[i].SyncSnapshot(final)
    end
  else
    begin
      _SyncDBInternal(final);
    end;
end;

function TFRE_DB_PS_FILE.GetLastError: TFRE_DB_String;
begin
  result := FLastError;
end;

function TFRE_DB_PS_FILE.GetLastErrorCode: TFRE_DB_Errortype;
begin
  result := FLastErrorCode;
end;

procedure TFRE_DB_PS_FILE.SetNotificationStreamCallback(const change_if: IFRE_DB_DBChangedNotification; const create_proxy: boolean);
begin
  if create_proxy then
    begin
      FChangeNotificationProxy := TFRE_DB_DBChangedNotificationProxy.Create(change_if);
      FChangeNotificationIF    := FChangeNotificationProxy;
    end
  else
    FChangeNotificationIF := change_if;
end;

function TFRE_DB_PS_FILE.GetNotificationStreamCallback: IFRE_DB_DBChangedNotification;
begin
  if assigned(FChangeNotificationIF) then
    result := FChangeNotificationIF
  else
    result := GNOTIF_LOG;
end;

function TFRE_DB_PS_FILE.Connect(const db_name: TFRE_DB_String; out db_layer: IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false): TFRE_DB_Errortype;
var up_dbname : TFRE_DB_String;
    idx       : NativeInt;
begin
  if db_name='' then exit(edb_INVALID_PARAMS);
  up_dbname := uppercase(db_name);
  db_layer  := _InternalFetchConnectedLayer(db_name,idx);
  if not assigned(db_layer) then
    begin
      SetLength(FConnectedLayers,Length(FConnectedLayers)+1);
      FConnectedLayers[high(FConnectedLayers)] := TFRE_DB_PS_FILE.InternalCreate(FBasedirectory,up_dbname,result);
      db_layer := FConnectedLayers[high(FConnectedLayers)];
      db_layer.SetNotificationStreamCallback(GetNotificationStreamCallback,false);
    end
  else
    begin
      result := edb_OK;
    end;
end;

function TFRE_DB_PS_FILE.DatabaseList: IFOS_STRINGS;
var i:integer;
begin
  result := GFRE_TF.Get_FOS_Strings;
  GFRE_BT.List_Directorys(FBasedirectory,result,1,false);
  for i:=0 to result.Count-1 do begin
    result[i] := UnEsacpeDBName(result[i]);
  end;
end;

//function TFRE_DB_PS_FILE.FlushObjects: TFRE_DB_Errortype;
//begin
//  inc(FLayerStats.FlushObj);
//  result := edb_OK;
//end;
//
//function TFRE_DB_PS_FILE.FlushCollections: TFRE_DB_Errortype;
//begin
//  inc(FLayerStats.FlushColl);
//  result := edb_OK;
//end;

function TFRE_DB_PS_FILE.DatabaseExists(const dbname: TFRE_DB_String): Boolean;
begin
  _SetupDirs(dbname);
  result :=DirectoryExists(FLocalConnDir);
end;

function TFRE_DB_PS_FILE.CreateDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
begin
  if dbname = '' then
    exit(edb_NOT_FOUND);
  if UpperCase(dbname)='GLOBAL' then
    exit(edb_RESERVED);
  _SetupDirs(dbname);
  if DirectoryExists(FLocalConnDir)   then exit(edb_EXISTS);
  if not ForceDirectories(FLocalConnDir) then exit(edb_ERROR);
  if not ForceDirectories(FMasterCollDir) then exit(edb_ERROR);
  if not ForceDirectories(FCollectionsDir) then exit(edb_ERROR);
  if not ForceDirectories(FMetaDir) then exit(edb_ERROR);
  if not ForceDirectories(FWalDir) then exit(edb_ERROR);
  result := edb_OK;
end;

function TFRE_DB_PS_FILE.DeleteDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
var dir:TFRE_DB_String;
begin
  if dbname='' then
    exit(edb_NOT_FOUND);
  if UpperCase(dbname)='GLOBAL' then
    exit(edb_RESERVED);
  dir := SetDirSeparators(FBasedirectory+'/'+EscapeDBName(dbname));
  if not DirectoryExists(dir) then exit(edb_NOT_FOUND);
  if GFRE_BT.Delete_Directory(dir) then begin
    result := edb_OK;
  end else begin
    Result := edb_ERROR;
  end;
end;

function TFRE_DB_PS_FILE.GetReferences(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
begin
  result := FMaster.GetReferences(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_PS_FILE.GetReferencesCount(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  result := FMaster.GetReferencesCount(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_PS_FILE.GetReferencesDetailed(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_ObjectReferences;
begin
  result := FMaster.GetReferencesDetailed(obj_uid,from,scheme_prefix_filter,field_exact_filter);
end;

function TFRE_DB_PS_FILE.ObjectExists(const obj_uid: TGUID): boolean;
begin
  result := FMaster.ExistsObject(obj_uid);
end;


end.

