unit fre_db_persistance_fs_simple;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2014, FirmOS Business Solutions GmbH
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

uses  Classes, SysUtils,Contnrs,FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,fre_db_persistance_common,baseunix,fos_interlocked,FRE_SYSTEM;

function Get_PersistanceLayer_PS_Simple(const basedir:TFRE_DB_String):IFRE_DB_PERSISTANCE_LAYER;

//{$IFDEF DARWIN}
function  fredbps_fsync(filedes : cint): cint; cdecl; external 'c' name 'fsync';
//{$ENDIF}

   { TFRE_DB_PS_FILE }

   // The global layer is used to initial build and connect the specialized database layers (clone per db)
   // there exists one system database, and n user databases
   // masterdata is global volatile shared, system persistent shared, and per db shared

  var  G_GlobalLayerLock : IFOS_LOCK;

  type

   TFRE_DB_PS_FILE = class(TObject,IFRE_DB_PERSISTANCE_LAYER)
   private
     FMaster               : TFRE_DB_Master_Data; // Global, Volatile, Per DB, at least one for system, NIL in GLOBAL LAYER
     FBasedirectory        : TFRE_DB_String;
     FMasterCollDir        : TFRE_DB_String;
     FLocalConnDir         : TFRE_DB_String;
     FCollectionsDir       : TFRE_DB_String;
     FMetaDir              : TFRE_DB_String;
     FWalDir               : TFRE_DB_String;
     FConnected            : Boolean;
     FIsGlobalLayer        : Boolean;
     FDontFinalizeNotif    : Boolean;
     FConnectedLayers      : Array of TFRE_DB_PS_FILE;
     FConnectedDB          : TFRE_DB_String;
     FChangeNotificationIF : IFRE_DB_DBChangedNotification;

     procedure    _ConnectCheck                ;
     procedure    _SetupDirs                   (const db_name:TFRE_DB_String);
     function     EscapeDBName                 (const name:string):string;
     function     UnEsacpeDBName               (const name:string):string;

     function     _GetCollection               (const coll_name : TFRE_DB_NameType ; out Collection:TFRE_DB_PERSISTANCE_COLLECTION) : Boolean;

     procedure   _StoreCollectionPersistent    (const coll:TFRE_DB_PERSISTANCE_COLLECTION);
     procedure   _StoreObjectPersistent        (const obj:TFRE_DB_Object);

     procedure   WT_TransactionID              (const number:qword);
     procedure   WT_StoreCollectionPersistent  (const coll:TFRE_DB_PERSISTANCE_COLLECTION_BASE);
     procedure   WT_DeleteCollectionPersistent (const collname : TFRE_DB_NameType);
     procedure   WT_StoreObjectPersistent      (const obj: IFRE_DB_Object);
     procedure   WT_DeleteObjectPersistent     (const iobj:IFRE_DB_Object);
     function    INT_Fetch                     (const ouid    :  TFRE_DB_GUID  ; out   dbi:IFRE_DB_Object):boolean; { unlocked internal fetch }

     {< Backup Functionality}
     function    FDB_GetObjectCount            (const coll:boolean; const SchemesFilter:TFRE_DB_StringArray=nil): Integer;
     procedure   FDB_ForAllObjects             (const cb:IFRE_DB_ObjectIteratorBrk; const SchemesFilter:TFRE_DB_StringArray=nil);
     procedure   FDB_ForAllColls               (const cb:IFRE_DB_Obj_Iterator);
     function    FDB_GetAllCollsNames          :TFRE_DB_NameTypeArray;
     function    FDB_TryGetIndexStream         (const collname : TFRE_DB_NameType ; const ix_name : TFRE_DB_Nametype ; out stream : TStream):boolean;
     procedure   FDB_PrepareDBRestore          (const phase:integer);     { used for various preparations and checks }
     procedure   FDB_SendObject                (const obj:IFRE_DB_Object);
     procedure   FDB_SendCollection            (const obj:IFRE_DB_Object);
     { Backup Functionality >}

     procedure   _SyncDBInternal               ;

     function   _InternalFetchConnectedLayer   (db_name:TFRE_DB_String;var idx :NativeInt): TFRE_DB_PS_FILE;

     procedure   DEBUG_DisconnectLayer         (const db:TFRE_DB_String);
     procedure   DEBUG_InternalFunction        (const func:NativeInt);

     function    _FetchO                       (const ouid:TFRE_DB_GUID ; out dbo:TFRE_DB_Object ; const internal_object:boolean): boolean;
     procedure   MustNotBeGlobalLayerCheck     ;
     procedure   MustBeGlobalLayer             ;
     function    LayerLock                          : IFOS_LOCK;
     function    GetConnectedDB                     : TFRE_DB_NameType;
     function    LoadScheme                         : TFRE_DB_Errortype;

   public
     constructor InternalCreate                     (const basedir, name: TFRE_DB_String; out result: TFRE_DB_Errortype);

     constructor Create                             (const basedir:TFRE_DB_String);
     destructor  Destroy                            ; override;
     procedure   Finalize                           ;

     function    DatabaseList                       : IFOS_STRINGS;
     function    DatabaseExists                     (const dbname:TFRE_DB_String):Boolean;
     function    CreateDatabase                     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeleteDatabase                     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeployDatabaseScheme               (const scheme:IFRE_DB_Object):TFRE_DB_Errortype;
     function    GetDatabaseScheme                  (out   scheme:IFRE_DB_Object):TFRE_DB_Errortype;

     function    GetReferences                      (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType='' ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_GUIDArray;
     function    GetReferencesCount                 (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType='' ; const user_context : PFRE_DB_GUID=nil):NativeInt;
     function    GetReferencesDetailed              (const obj_uid:TFRE_DB_GUID;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType='' ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_ObjectReferences;

     function    Connect                            (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const NotifIF : IFRE_DB_DBChangedNotificationBlock=nil) : TFRE_DB_Errortype;
     function    Disconnect                         : TFRE_DB_Errortype;
     function    ObjectExists                       (const obj_uid : TFRE_DB_GUID) : boolean;
     function    Fetch                              (const ouid    : TFRE_DB_GUID  ; out   dbo:IFRE_DB_Object ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_Errortype;
     function    BulkFetch                          (const obj_uids: TFRE_DB_GUIDArray ; out objects : IFRE_DB_ObjectArray ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_Errortype;
     function    RebuildUserToken                   (const user_uid: TFRE_DB_GUID):IFRE_DB_USER_RIGHT_TOKEN;

     { Transactional Operations / These operations report the last transaction step id generated, there may be more then one generated }

     function    StoreOrUpdateObject                 (const   iobj  : IFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean ; const user_context : PFRE_DB_GUID=nil) : TFRE_DB_TransStepId; { must free the iobj in every case !}
     function    DeleteObject                        (const obj_uid : TFRE_DB_GUID   ; const collection_name: TFRE_DB_NameType = ''  ; const user_context : PFRE_DB_GUID=nil) : TFRE_DB_TransStepId;

     { Delete Operation :  collection_name = '' delete from all ->  collectionname<>'' only remove from collection }
     function    StartTransaction                    (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType='T') : TFRE_DB_Errortype;
     function    Commit                              : boolean;
     procedure   RollBack                            ;
     // Transactional Operations Done

     procedure   SyncSnapshot                        ;
     function    GetNotificationRecordIF             : IFRE_DB_DBChangedNotification;

     { Collection Interface }
     function    CollectionExistCollection           (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : Boolean;
     function    CollectionNewCollection             (const coll_name: TFRE_DB_NameType ; const volatile_in_memory: boolean ; const user_context : PFRE_DB_GUID=nil): TFRE_DB_TransStepId;
     function    CollectionDeleteCollection          (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : TFRE_DB_TransStepId; // todo transaction context
     function    CollectionDefineIndexOnField        (const coll_name: TFRE_DB_NameType ; const FieldName   : TFRE_DB_NameType ; const FieldType : TFRE_DB_FIELDTYPE   ; const unique     : boolean;
                                                      const ignore_content_case: boolean ; const index_name : TFRE_DB_NameType ; const allow_null_value : boolean=true ; const unique_null_values: boolean=false ; const is_a_domain_index: boolean = false; const user_context : PFRE_DB_GUID=nil): TFRE_DB_TransStepId;
     function    CollectionGetIndexDefinition        (const coll_name: TFRE_DB_NameType ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_INDEX_DEF;
     function    CollectionDropIndex                 (const coll_name: TFRE_DB_NameType ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_TransStepId;
     function    CollectionGetAllIndexNames          (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : TFRE_DB_NameTypeArray;
     function    CollectionExistsInCollection        (const coll_name: TFRE_DB_NameType ; const check_uid: TFRE_DB_GUID ; const and_has_fetch_rights: boolean ; const user_context : PFRE_DB_GUID=nil): boolean;
     function    CollectionFetchInCollection         (const coll_name: TFRE_DB_NameType ; const check_uid: TFRE_DB_GUID ; out   dbo:IFRE_DB_Object ; const user_context : PFRE_DB_GUID=nil):TFRE_DB_Errortype;
     function    CollectionBulkFetch                 (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil): IFRE_DB_ObjectArray;
     function    CollectionBulkFetchUIDS             (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil): TFRE_DB_GUIDArray;
     procedure   CollectionClearCollection           (const coll_name: TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil); { TODO: Create implicit transaction }
     function    CollectionIndexExists               (const coll_name: TFRE_DB_NameType ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil):Boolean;
     function    CollectionGetIndexedValueCount      (const coll_name: TFRE_DB_NameType; const qry_val: IFRE_DB_Object; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID=nil): NativeInt;
     function    CollectionGetIndexedObjsFieldval    (const coll_name: TFRE_DB_NameType ; const qry_val : IFRE_DB_Object ; out objs : IFRE_DB_ObjectArray ; const index_must_be_full_unique : boolean ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : NativeInt;
     function    CollectionGetIndexedUidsFieldval    (const coll_name: TFRE_DB_NameType ; const qry_val : IFRE_DB_Object ; out objs : TFRE_DB_GUIDArray   ; const index_must_be_full_unique : boolean ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : NativeInt;
     function    CollectionRemoveIndexedUidsFieldval (const coll_name: TFRE_DB_NameType ; const qry_val : IFRE_DB_Object ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : NativeInt;
     function    CollectionGetIndexedObjsRange       (const coll_name: TFRE_DB_NameType ; const min,max : IFRE_DB_Object ; const ascending: boolean ; const max_count,skipfirst : NativeInt ; out objs : IFRE_DB_ObjectArray ; const min_val_is_a_prefix : boolean ; const index_name:TFRE_DB_NameType ; const user_context : PFRE_DB_GUID=nil) : NativeInt;
     function    CollectionGetFirstLastIdxCnt        (const coll_name: TFRE_DB_NameType ; const idx : Nativeint ; out obj : IFRE_DB_Object ; const user_context : PFRE_DB_GUID=nil) : NativeInt;
   end;

implementation

function Get_PersistanceLayer_PS_Simple(const basedir: TFRE_DB_String): IFRE_DB_PERSISTANCE_LAYER;
var l_Persistance_Layer : TFRE_DB_PS_FILE;
begin
  l_Persistance_Layer := TFRE_DB_PS_FILE.Create(basedir);
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
      var g          : TFRE_DB_GUID;
          obj        : TFRE_DB_Object;
          m          : TMemorystream;
          uid_string : TFRE_DB_String;
      begin
        g := FREDB_H2G(Copy(file_name,1,32));
        uid_string:=FREDB_G2H(g);
        GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>RETRIEVE OBJECT [%s]',[uid_string]);
        m:=TMemoryStream.Create;
        try
          m.LoadFromFile(FMasterCollDir+file_name);
          obj:= TFRE_DB_Object.CreateFromMemory(m.Memory);
          if not obj.FieldExists(cFRE_DB_SYS_T_LMO_TRANSID) then
            obj.Field(cFRE_DB_SYS_T_LMO_TRANSID).AsString:='RESTORE_'+inttostr(G_DB_TX_Number);
        finally
          m.free;
        end;
        GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<RETRIEVE OBJECT [%s] DONE',[uid_string]);
        result := FMaster.InternalStoreObjectFromStable(obj);
        if result<>edb_OK then
          raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RECREATE MEMORY FROM STABLE at [%s]',[FREDB_G2H(g)]);
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
      var f     : TFileStream;
          res   : TFRE_DB_Errortype;
          coll  : TFRE_DB_PERSISTANCE_COLLECTION;
          name  : TFRE_DB_String;
          ename : TFRE_DB_String;
          ext   : TFRE_DB_String;

          procedure LoadCollection;
          begin
            GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>LOAD COLLECTION [%s]',[name]);
            f :=  TFileStream.Create(FCollectionsDir+file_name,fmOpenRead);
            try
              res := FMaster.MasterColls.NewCollection(name,coll,false,self);
              if res <> edb_OK then
                raise EFRE_DB_PL_Exception.Create(res,'LOAD COLLECTION FROM STABLE FAILED FOR [%s]',[name]);
              (coll as TFRE_DB_Persistance_Collection).LoadFromThis(f);
            finally
              f.free;
            end;
          end;

          procedure LoadIndexDef;
          var obj : IFRE_DB_Object;
              fni : TFRE_DB_String;
          begin
            fni := FCollectionsDir+DirectorySeparator+ename+'.idd';
            if FileExists(fni) then
              begin
                GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>LOAD IDX DEF COLLECTION [%s - %s]',[name,name+'.idd']);
                obj := GFRE_DBI.CreateFromFile(fni);
                try
                  coll.CreateIndexDefsFromObj(obj);
                finally
                  obj.Finalize;
                end;
              end;
          end;

      begin
        ext   := uppercase(GFRE_BT.SepRight(file_name,'.'));
        ename := GFRE_BT.SepLeft(file_name,'.');
        name  := GFRE_BT.HexStr2Str(ename);
        case ext of
          'IDX' : { load after idd load };
          'IDD' : { load after collectionload };
          'COL' :
            begin
              LoadCollection;
              LoadIndexDef;
            end;
          else
            begin
              GFRE_DBI.LogError(dblc_PERSISTANCE,'>> COLLECTIONLOAD / IGNORE INVALID EXTENSION FILE [%s]',[file_name]);
              writeln('>> COLLECTIONLOAD / IGNORE INVALID EXTENSION FILE ',file_name);
            end;
        end;
      end;

    begin
      GFRE_BT.List_Files(FCollectionsDir,@add_collection);
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

  FMaster := TFRE_DB_Master_Data.Create(FConnectedDB,self);
  _BuildMasterCollection;
  _BuildCollections;
  if not Assigned(G_SysScheme) then
    try
      LoadScheme;
    except
      on E:EXCEPTION do
      begin
        GFRE_DB.LogEmergency(dblc_PERSISTANCE,'could not load databasescheme [%s]',[e.Message]);
      end;
    end;
  if not GDBPS_SKIP_STARTUP_CHECKS then
    CheckDbResult(FMaster.InternalCheckRestoredBackup,' Internal Consistency Check Failed');
  FConnected   := true;
  FIsGlobalLayer := false;
end;

function TFRE_DB_PS_FILE._GetCollection(const coll_name: TFRE_DB_NameType; out Collection: TFRE_DB_PERSISTANCE_COLLECTION): Boolean;
begin
  result := FMaster.MasterColls.GetCollection(coll_name,Collection);
end;

function TFRE_DB_PS_FILE.CollectionExistCollection(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): Boolean;
var dummy : TFRE_DB_PERSISTANCE_COLLECTION;
begin
  LayerLock.Acquire;
  try
    result := FMaster.MasterColls.GetCollection(coll_name,dummy);
  finally
     LayerLock.Release;
  end;
end;


procedure TFRE_DB_PS_FILE._StoreCollectionPersistent(const coll: TFRE_DB_PERSISTANCE_COLLECTION);
var f   : TFileStream;
    fnb : string;
    ixo : IFRE_DB_Object;
    nta : TFRE_DB_NameTypeArray;
    i   : NativeInt;
begin
  if not coll.IsVolatile then
    begin
      fnb := FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName(false));
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
      f :=  TFileStream.Create(fnb+'.col',fmCreate+fmOpenReadWrite);
      try
        coll.StreamToThis(f);
      finally
        f.free;
      end;
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE COLLECTION IDD [%s]',[coll.CollectionName]);
      try
        ixo := coll.GetIndexDefObject;
        ixo.SaveToFile(fnb+'.idd');
      finally
        ixo.Finalize;
      end;
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE COLLECTION IDX [%s]',[coll.CollectionName]);
      nta := coll.IndexNames;
      for i:=0 to high(nta) do
        begin
          f :=  TFileStream.Create(fnb+'-'+GFRE_BT.Str2HexStr(nta[i])+'.idx',fmCreate+fmOpenReadWrite);
          try
            coll.StreamIndexToThis(nta[i],f);
          finally
            f.free;
          end;
        end;
    end;
end;

procedure TFRE_DB_PS_FILE._StoreObjectPersistent(const obj: TFRE_DB_Object);
var  FileName : string;
begin
  if obj.IsVolatile then
    exit;
  if obj.IsObjectRoot then
    begin
      obj._InternalGuidNullCheck;
      filename    := FMasterCollDir+FREDB_G2H(obj.UID)+'.fdbo';
      obj.SaveToFile(FileName);
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<STORE OBJECT : '+obj.UID_String+' DONE');
    end
  else
    raise EFRE_DB_Exception.Create(edb_PERSISTANCE_ERROR,'invalid try to do a persistent subobject store');
end;

procedure TFRE_DB_PS_FILE.WT_TransactionID(const number: qword);
var filename:string;
begin
  filename := FBasedirectory+DirectorySeparator+'transaction';
  try
    GFRE_BT.StringToFile(filename,IntToStr(number));
  except
    raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot write current transaction number !!');
  end;
end;

procedure TFRE_DB_PS_FILE.WT_StoreCollectionPersistent(const coll: TFRE_DB_PERSISTANCE_COLLECTION_BASE);
begin
  _StoreCollectionPersistent(coll as TFRE_DB_PERSISTANCE_COLLECTION);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteCollectionPersistent(const collname: TFRE_DB_NameType);
var fn : String;
begin
  fn := FCollectionsDir+GFRE_BT.Str2HexStr(collname)+'.col';
  if FileExists(fn) then
    begin
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>DELETE COLLECTION [%s]',[collname]);
      if not DeleteFile(fn) then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete collection '+collname);
    end
  else
    begin
      GFRE_DBI.LogError(dblc_PERSISTANCE,'>>DELETE COLLECTION [%s] FAILED / FILE NOT FOUND (%s)',[collname,fn]);
    end;
end;

procedure TFRE_DB_PS_FILE.WT_StoreObjectPersistent(const obj: IFRE_DB_Object);
begin
  _StoreObjectPersistent(obj.Implementor as TFRE_DB_Object);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteObjectPersistent(const iobj: IFRE_DB_Object);
var  FileName : shortstring;
     obj      : TFRE_DB_Object;
begin
  if iobj.IsObjectRoot then
    begin
      obj := iobj.Implementor as TFRE_DB_Object;
      filename    := FMasterCollDir+FREDB_G2H(obj.UID)+'.fdbo';
      if not DeleteFile(FileName) then
        raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete file '+FileName);
      GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<FINAL DELETE  OBJECT : '+obj.UID_String+' DONE');
    end;
end;

function TFRE_DB_PS_FILE.INT_Fetch(const ouid: TFRE_DB_GUID; out dbi: IFRE_DB_Object): boolean;
var dbo : TFRE_DB_Object;
begin
  result := _FetchO(ouid,dbo,true);
  if result then
    dbi := dbo
  else
    dbi:=nil;
end;

function TFRE_DB_PS_FILE.FDB_GetObjectCount(const coll: boolean; const SchemesFilter: TFRE_DB_StringArray): Integer;
begin
  if coll then
    result := FMaster.MasterColls.GetCollectionCount
  else
    result := FMaster.GetPersistantRootObjectCount(FREDB_StringArray2Upper(SchemesFilter));
end;

procedure TFRE_DB_PS_FILE.FDB_ForAllObjects(const cb: IFRE_DB_ObjectIteratorBrk; const SchemesFilter: TFRE_DB_StringArray);
var filter : TFRE_DB_StringArray;
    guids  : TFRE_DB_GUIDArray;
    cnt    : NativeInt;
    obj    : TFRE_DB_Object;
    break  : boolean;

  procedure ForAll(const obj : TFRE_DB_Object;var break:boolean);
  var lock : boolean;
  begin
    break := false;
    if obj.IsObjectRoot then
        if  (Length(filter)=0) or
            (FREDB_StringInArray(uppercase(obj.SchemeClass),filter)) then
               begin
                 obj.Set_Store_LockedUnLockedIf(false,lock);
                try
                  if cnt=Length(guids) then
                    SetLength(guids,Length(guids)+1024);
                  guids[cnt] := obj.UID;
                  inc(cnt);
                finally
                  obj.Set_Store_LockedUnLockedIf(true,lock);
                end;
              end;
  end;

begin
  filter := FREDB_StringArray2Upper(SchemesFilter);
  cnt    := 0;
  FMaster.ForAllObjectsInternal(true,false,@ForAll);
  SetLength(guids,cnt);
  break  := false;
  for cnt  := 0 to high(guids) do
    if FMaster.FetchObject(guids[cnt],obj,false) then
      begin
        cb(obj,break);
        if break then
          exit;
      end
    else
      raise EFRE_DB_Exception.Create(edb_INTERNAL,'for all bulk/fetch error');
end;

procedure TFRE_DB_PS_FILE.FDB_ForAllColls(const cb: IFRE_DB_Obj_Iterator);

  procedure CollCB(const pcoll : TFRE_DB_Persistance_Collection);
  begin
    cb(pcoll.BackupToObject);
  end;

begin
  FMaster.MasterColls.ForAllCollections(@CollCB);
end;

function TFRE_DB_PS_FILE.FDB_GetAllCollsNames: TFRE_DB_NameTypeArray;
var cnt : NativeInt;

  procedure CollCB(const pcoll : TFRE_DB_Persistance_Collection);
  begin
    if Length(result)=cnt then
      SetLength(result,Length(result)+32);
    result[cnt]:=pcoll.CollectionName(false);
    inc(cnt);
  end;

begin
  cnt := 0;
  SetLength(Result,32);
  FMaster.MasterColls.ForAllCollections(@CollCB);
  SetLength(Result,cnt);
end;

function TFRE_DB_PS_FILE.FDB_TryGetIndexStream(const collname: TFRE_DB_NameType; const ix_name: TFRE_DB_Nametype; out stream: TStream): boolean;
var fn : string;
begin
  fn := GFRE_BT.Str2HexStr(collname)+'-'+GFRE_BT.Str2HexStr(ix_name)+'.idx';
  result := false;
  if FileExists(FCollectionsDir+fn) then
    begin
      try
        stream := TFileStream.Create(FCollectionsDir+fn,fmOpenRead);
        result := true;
      except
      end;
    end;
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
    2 :
      begin { Do a Checkphase of the restore }
        result := FMaster.InternalCheckRestoredBackup;
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
  WT_StoreObjectPersistent(obj);
  if result<>edb_OK then
    raise EFRE_DB_PL_Exception.Create(result,'FAILED TO RESTORE OBJECT FROM BACKUP at [%s]',[obj.GetDescriptionID]);
end;

procedure TFRE_DB_PS_FILE.FDB_SendCollection(const obj: IFRE_DB_Object);
var res  : TFRE_DB_Errortype;
    coll : TFRE_DB_PERSISTANCE_COLLECTION;
    name : TFRE_DB_NameType;
begin
  name := obj.Field('CollectionName').AsString;
  res := FMaster.MasterColls.NewCollection(name,coll,false,self);
  if res <> edb_OK then
    raise EFRE_DB_PL_Exception.Create(res,'LOAD COLLECTION FROM BACKUP FAILED FOR [%s]',[name]);
  coll.RestoreFromObject(obj);
  WT_StoreCollectionPersistent(coll);
end;

procedure TFRE_DB_PS_FILE._SyncDBInternal;

   procedure WriteColls(const coll:TFRE_DB_PERSISTANCE_COLLECTION);
   begin
     _StoreCollectionPersistent(coll);
   end;

   procedure StoreObjects(const obj : TFRE_DB_Object ; var break:boolean);
   begin
     _StoreObjectPersistent(obj);
   end;

begin
  if GDISABLE_SYNC then
    begin
      GFRE_DBI.LogNotice(dblc_PERSISTANCE,'<<SKIPPING SYNC OF DB [%s] / WRITE THROUGH MODE',[FConnectedDB]);
      exit;
    end;
  FMaster.MasterColls.ForAllCollections(@WriteColls);
  FMaster.ForAllObjectsInternal(true,false,@StoreObjects);
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

procedure TFRE_DB_PS_FILE.DEBUG_DisconnectLayer(const db: TFRE_DB_String);
var idx : NativeInt;
     l  : TFRE_DB_PS_FILE;
begin
  if not FIsGlobalLayer then
    raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'fail');
  idx := 0;
  l   := _InternalFetchConnectedLayer(db,idx);
  l.Free;
  FConnectedLayers[idx] := nil;
end;

procedure TFRE_DB_PS_FILE.DEBUG_InternalFunction(const func: NativeInt);

  procedure CheckAllStoreLocked;
  var
    i: NativeInt;
  begin
    G_SysMaster.InternalCheckStoreLocked;
    for i:=0 to high(G_AllNonsysMasters) do
      G_AllNonsysMasters[i].InternalCheckStoreLocked;
  end;

begin
  case func of
    1 : begin
          CheckAllStoreLocked;
        end;
  end;
end;

function TFRE_DB_PS_FILE.GetConnectedDB: TFRE_DB_NameType;
begin
  result := FConnectedDB;
end;

function TFRE_DB_PS_FILE.LoadScheme: TFRE_DB_Errortype;
var objdir : string;
begin
  result := edb_OK;
  objdir := FBasedirectory+DirectorySeparator+'fre_scheme.dbo';
  if FileExists(objdir) then
    try
      G_SysScheme := TFRE_DB_Object.CreateFromFile(objdir);
      result := edb_OK;
    except
      on e:exception do
        begin
          result.Code := edb_ERROR;
          result.Msg  := 'Failed to load db scheme Error:'+e.Message;
        end;
    end;
end;

function TFRE_DB_PS_FILE.CollectionNewCollection(const coll_name: TFRE_DB_NameType; const volatile_in_memory: boolean; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    step                : TFRE_DB_NewCollectionStep;

begin
  LayerLock.Acquire;
  try
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction       := TFRE_DB_TransactionalUpdateList.Create('C',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_NewCollectionStep.Create(self,FMaster,coll_name,volatile_in_memory,user_context);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        result         := step.GetTransActionStepID;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionDeleteCollection(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    step                : TFRE_DB_DeleteCollectionStep;

begin
  LayerLock.Acquire;
  try
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('DC',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_DeleteCollectionStep.Create(self,FMaster,coll_name,user_context);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        result         := step.GetTransActionStepID;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

constructor TFRE_DB_PS_FILE.Create(const basedir: TFRE_DB_String);
var GSTRING   : String;
begin
  GFRE_TF.Get_Lock(G_GlobalLayerLock,True);
  G_UserTokens := TFPHashList.Create;
  FBasedirectory := basedir;
  if not DirectoryExists(FBaseDirectory) then begin
    if not ForceDirectories(FBaseDirectory) then begin
      GFRE_BT.CriticalAbort('cannot setup basedirectory');
    end;
  end;
  FMaster               := nil; //TFRE_DB_Master_Data.Create('GLOBAL',self);
  FConnectedDB          := 'GLOBAL';
  FIsGlobalLayer        := True;

  FChangeNotificationIF := TFRE_DB_DBChangedNotificationBase.Create(FConnectedDB);
  FDontFinalizeNotif    := false;
  if FileExists(FBasedirectory+DirectorySeparator+'transaction') then
    begin
      GSTRING := GFRE_BT.StringFromFile(FBasedirectory+DirectorySeparator+'transaction');
      G_DB_TX_Number := StrToIntDef(GSTRING,0);
    end
  else
    writeln('>>NO GLOBAL TRANSACTION COUNTER FILE FOUND'); { todo: log WARNING}
  if G_DB_TX_Number=0 then
    writeln('>> GLOBAL TRANSACTION COUNTER IS ZERO - MAYBE BAD/NO FILE'); { todo: log WARNING}
end;


destructor TFRE_DB_PS_FILE.Destroy;
var
  i: NativeInt;
begin
  if FIsGlobalLayer then
    begin
      for i:=0 to high(FConnectedLayers) do
        FConnectedLayers[i].Free;
      G_GlobalLayerLock.Finalize;
    end
  else
    begin
      if Length(FConnectedLayers)>0 then
        raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'Connected Layer in non global layer is non empty!');
    end;
  FMaster.Free;

  if not FDontFinalizeNotif
     and assigned(FChangeNotificationIF) then
       FChangeNotificationIF.FinalizeNotif;

  FChangeNotificationIF:=nil;
  inherited destroy;
end;

procedure TFRE_DB_PS_FILE.Finalize;
begin
  free;
end;

function TFRE_DB_PS_FILE.Fetch(const ouid: TFRE_DB_GUID; out dbo: IFRE_DB_Object; const user_context: PFRE_DB_GUID): TFRE_DB_Errortype;
var dboo : TFRE_DB_Object;
begin
  LayerLock.Acquire;
  try
    try
      if _FetchO(ouid,dboo,false) then
        begin
          dbo := dboo;
          exit(edb_OK);
        end
      else
        begin
          dbo            := nil;
          exit(edb_NOT_FOUND);
        end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          dbo := nil;
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          dbo := nil;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          dbo := nil;
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.BulkFetch(const obj_uids: TFRE_DB_GUIDArray; out objects: IFRE_DB_ObjectArray; const user_context: PFRE_DB_GUID): TFRE_DB_Errortype;
var dboa  : TFRE_DB_ObjectArray;
    i     : NativeInt;
    all   : Boolean;
begin
  SetLength(dboa,length(obj_uids));
  LayerLock.Acquire;
  try
    try
      all := true;
      for i := 0 to high(dboa) do
         if not FMaster.FetchObject(obj_uids[i],dboa[i],false) then
           begin
             all := false;
             break;
           end;
      if all then
        begin
          SetLength(objects,Length(dboa));
          for i := 0 to high(objects) do
            objects[i] := dboa[i];
          exit(edb_OK);
        end
      else
        begin
          try
            for i := 0 to high(dboa) do
              dboa[i].Finalize;
          except
            on e:exception do
              GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/PL EXCEPTION BULKFETCHFAIL/FREE EXCEPTION :  %s',['Fetch',e.Message]);
          end;
          exit(edb_NOT_FOUND);
        end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          objects := nil;
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          objects := nil;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          objects := nil;
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.RebuildUserToken(const user_uid: TFRE_DB_GUID): IFRE_DB_USER_RIGHT_TOKEN;
var sys_admin   : boolean;
    domcnt      : NativeInt;
    idx         : NativeInt;
    domuids     : TFRE_DB_GUIDArray;
    domnames    : TFRE_DB_NameTypeArray;
    obj         : TFRE_DB_Object;
    myuser      : FRE_DB_CORE.TFRE_DB_USER;
    FSysDomains : TFRE_DB_PERSISTANCE_COLLECTION;
    FSysDomain  : IFRE_DB_DOMAIN;
    FSysDomainO : TFRE_DB_Object;
    iobj        : IFRE_DB_Object;
    tokeno      : TFRE_DB_USER_RIGHT_TOKEN;

  procedure IterateDomains(const domi : IFRE_DB_Object);
  var dom  : IFRE_DB_DOMAIN;
      domo : TFRE_DB_Object;
      lck  : Boolean;
  begin
    domo := domi.Implementor_HC as TFRE_DB_Object;
    try
      domo.Set_Store_LockedUnLockedIf(false,lck);
      domi.IntfCast(IFRE_DB_DOMAIN,dom);
      domuids[idx]  := dom.UID;
      domnames[idx] := dom.Domainname(false);
      inc(idx);
    finally
      domo.Set_Store_LockedUnLockedIf(true,lck);
    end;
  end;

  function _GetRightsArrayForUser(const user: IFRE_DB_USER): TFRE_DB_StringArray;

    function __OwnUserRights : TFRE_DB_StringArray;
    var uid:TFRE_DB_GUID;
    begin
      uid := (user.Implementor as TFRE_DB_Object).UID;
      SetLength(result,3);
      result[0] := TFRE_DB_Base.GetStdObjectRightName(sr_STORE,uid);
      result[1] := TFRE_DB_Base.GetStdObjectRightName(sr_UPDATE,uid);
      result[2] := TFRE_DB_Base.GetStdObjectRightName(sr_FETCH,uid);
    end;

    function __GetRoleIDArray(const usergroupids: TFRE_DB_GUIDArray ; const users_domainid : TFRE_DB_GUID): TFRE_DB_GUIDArray;
    var i            : integer;
        lUserGroup   : FRE_DB_CORE.TFRE_DB_GROUP;
        lRoleIDs     : TFRE_DB_ObjLinkArray;
    begin
      lUserGroup := nil;
      lRoleIDs   := nil;
      for i:=0 to high(UserGroupIDs) do begin
        if not _FetchO(UserGroupIDs[i],obj,true) then
          raise EFRE_DB_Exception.Create('Could not fetch group by id '+FREDB_G2H(UserGroupIDs[i]))
        else
          begin
            obj.Set_Store_Locked(false);
            try
              lUserGroup := obj as FRE_DB_CORE.TFRE_DB_GROUP;
              if not ((lUserGroup.isDisabled) and
                 (lUserGroup.DomainID=users_domainid)) then
                   FREDB_ConcatGUIDArrays(lRoleIDs,lUserGroup.RoleIDs);
            finally
              obj.Set_Store_Locked(true);
            end;
          end;
      end;
      result := lRoleIDs;
    end;

    function __GetRightsArrayForRoles(const roleids: TFRE_DB_GUIDArray ; const users_domainid : TFRE_DB_GUID): TFRE_DB_StringArray;
    var i            : integer;
        lRole        : FRE_DB_CORE.TFRE_DB_ROLE;
        lAllRights   : TFRE_DB_StringArray;
    begin
      lAllRights := nil;
      for i:=0 to high(roleids) do
        begin
          if not _FetchO(roleids[i],obj,true) then //FetchRolebyID(roleids[i],lRole,true)<>edb_OK then begin
            raise EFRE_DB_Exception.Create('Could not fetch role by id '+FREDB_G2H(roleids[i]))
          else
            begin
              obj.Set_Store_Locked(false);
              try
                lRole := obj as FRE_DB_CORE.TFRE_DB_ROLE;
                if not ((lRole.isDisabled) and
                        (lRole.DomainID=users_domainid)) then
                  FREDB_ConcatStringArrays(lAllRights,lRole.GetRightNames);
              finally
                obj.Set_Store_Locked(true);
              end;
            end;
        end;
      result := lAllRights;
    end;

  begin
    result := __GetRightsArrayForRoles(__GetRoleIDArray(user.GetUserGroupIDs,user.DomainID),user.DomainID);
    FREDB_ConcatStringArrays(result,__OwnUserRights);
  end;

begin
  result := nil;
  LayerLock.Acquire;
  try
    try
      MustNotBeGlobalLayerCheck;
      if not _FetchO(user_uid,obj,true) then
        raise EFRE_DB_Exception.Create(edb_MISMATCH,'the specified uid[%s] is not existing',[FREDB_G2H(user_uid)]);
      if not (obj is FRE_DB_CORE.TFRE_DB_USER) then
        raise EFRE_DB_Exception.Create(edb_MISMATCH,'the specified uid[%s] is not a user but a [%s]',[FREDB_G2H(user_uid),obj.SchemeClass]);
      myuser      := FRE_DB_CORE.TFRE_DB_USER(obj);
      try
        myuser.Set_Store_Locked(false);
        if not G_SysMaster.MasterColls.GetCollection('SysDomain',FSysDomains) then
          raise EFRE_DB_Exception.Create(edb_MISMATCH,'cannot fetch pl collection "SysDomain"');
        if not FSysDomains.GetIndexedObjInternal('SYSTEM',iobj,'def') then
          raise EFRE_DB_Exception.Create(edb_MISMATCH,'cannot fetch system domain object');
        if not iobj.Supports(IFRE_DB_DOMAIN,FSysDomain) then
          raise EFRE_DB_Exception.Create(edb_MISMATCH,'invalid system domain object');
        FSysDomainO := iobj.Implementor_HC as TFRE_DB_Object;
        try
          FSysDomainO.Set_Store_Locked(false);
          sys_admin := false;
          if (uppercase(myuser.Login)='ADMIN') and
             (myuser.DomainID=FSysDomain.UID) then
             sys_admin := true;
          domcnt := FSysDomains.Count;
          SetLength(domuids,domcnt);
          SetLength(domnames,domcnt);
          idx := 0;
          FSysDomains.ForAllInternalI(@IterateDomains);
          tokeno := TFRE_DB_USER_RIGHT_TOKEN.Create(myuser.UID,myuser.Login,myUser.Firstname,myUser.Lastname,'',myuser.Userclass,myuser.GetUserGroupIDS,_GetRightsArrayForUser(myUser),sys_admin,FSysDomain.UID,myuser.DomainID,domuids,domnames);
          G_UpdateUserToken(myuser.UID,tokeno);
          Result := tokeno.CloneToNewUserToken;
        finally
          FSysDomainO.Set_Store_Locked(true);
        end;
      finally
        myuser.Set_Store_Locked(true);
      end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;


function TFRE_DB_PS_FILE._FetchO(const ouid: TFRE_DB_GUID; out dbo: TFRE_DB_Object; const internal_object: boolean): boolean;
begin
  result := FMaster.FetchObject(ouid,dbo,internal_object);
end;

procedure TFRE_DB_PS_FILE.MustNotBeGlobalLayerCheck;
begin
  if FIsGlobalLayer then
    raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'-Operation only allowed on non global layer!-');
end;

procedure TFRE_DB_PS_FILE.MustBeGlobalLayer;
begin
  if not FIsGlobalLayer then
    raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'-Operation only allowed on global layer!-');
end;

function TFRE_DB_PS_FILE.DeleteObject(const obj_uid: TFRE_DB_GUID; const collection_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var
    ImplicitTransaction : Boolean;
    delete_object       : TFRE_DB_Object;
    step                : TFRE_DB_ChangeStep;
    ut                  : TFRE_DB_USER_RIGHT_TOKEN;

begin
  LayerLock.Acquire;
  try
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('ID',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
          if not _FetchO(obj_uid,delete_object,true) then
            raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be deleted but was not found [%s]',[FREDB_G2H(obj_uid)]);
          if (collection_name<>'') then
            if delete_object.__InternalCollectionExistsName(collection_name)=-1 then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the request to delete object [%s] from collection [%s] could not be completed, the object is not stored in the requested collection',[delete_object.UID_String,collection_name]);
          if assigned(user_context) then
            begin
              G_GetUserToken(user_context,ut,true);
              if ut.CheckStdRightSetUIDAndClass(delete_object.UID,delete_object.DomainID,delete_object.SchemeClass,[sr_DELETE])<>edb_OK then
                raise EFRE_DB_Exception.Create(edb_ACCESS,'no right to delete object [%s]',[FREDB_G2H(obj_uid)]);
            end;
          if delete_object.IsObjectRoot then
            step := TFRE_DB_DeleteObjectStep.Create(self,FMaster,delete_object,collection_name,false,user_context)
          else
            raise EFRE_DB_Exception.Create(edb_ERROR,'a delete of a subobject is only allowed via an update of an root object');
          G_Transaction.AddChangeStep(step);
          result := step.GetTransActionStepID;
          if ImplicitTransaction then
            Commit;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

// This is always the first entry into the store and update chain
function TFRE_DB_PS_FILE.StoreOrUpdateObject(const iobj: IFRE_DB_Object; const collection_name: TFRE_DB_NameType; const store: boolean; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var coll                : TFRE_DB_PERSISTANCE_COLLECTION;
    to_update_obj       : TFRE_DB_Object;
    ImplicitTransaction : Boolean;
    changes             : Boolean;
    obj                 : TFRE_DB_Object;
    updatestep          : TFRE_DB_UpdateStep;


  procedure GeneralChecks;
  begin
    if obj.DomainID=CFRE_DB_NullGUID then
      raise EFRE_DB_PL_Exception.Create(edb_ERROR,'persistance failure, an object without a domainid cannot be stored');
    obj._InternalGuidNullCheck;
  end;

begin
  LayerLock.Acquire;
  try
    obj        := iobj.Implementor as TFRE_DB_Object;
    coll       := nil;
    try
      try
        GeneralChecks;
        if store then
          begin
            if not obj.IsObjectRoot then
              raise EFRE_DB_PL_Exception.Create(edb_UNSUPPORTED,'store of non root objects is not allowed');
            if collection_name='' then
              raise EFRE_DB_PL_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request');
            if not _GetCollection(collection_name,coll) then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the specified collection [%s] was not found',[collection_name]);
            if not assigned(G_Transaction) then
              begin
                G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('S',Fmaster,FChangeNotificationIF);
                ImplicitTransaction := True;
              end;
            to_update_obj := nil;
            G_Transaction.AddChangeStep(TFRE_DB_InsertStep.Create(self,FMaster,iobj.Implementor as TFRE_DB_Object,coll,store,user_context));
            result := G_Transaction.GetTransLastStepTransId;
            if ImplicitTransaction then
              G_Transaction.Commit;
          end
        else
          begin { update }
            if not obj.IsObjectRoot then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the object [%s] is a child object, only root objects updates are allowed',[obj.UID_String]);
            if not _FetchO(obj.UID,to_update_obj,true) then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be updated but was not found [%s]',[obj.UID_String]);
            if length(to_update_obj.__InternalGetCollectionList)=0 then
              begin
                writeln('::: OFFENDING OBJECT ', to_update_obj.DumpToString());
                if not GDBPS_SKIP_STARTUP_CHECKS then
                  raise EFRE_DB_PL_Exception.Create(edb_INTERNAL,'fetched to update ubj must have internal collections(!)');
              end;
            if collection_name<>'' then
              if to_update_obj.__InternalCollectionExistsName(collection_name)=-1 then
                raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'a collectionname was given for updaterequest, but the dbo is not in that collection');
            if not assigned(G_Transaction) then
              begin
                G_Transaction       := TFRE_DB_TransactionalUpdateList.Create('U',Fmaster,FChangeNotificationIF);
                ImplicitTransaction := True;
              end else
                ImplicitTransaction := false;
            if G_DEBUG_TRIGGER_1 then
              begin
                G_DEBUG_TRIGGER_1:=true;
              end;
            updatestep := TFRE_DB_UpdateStep.Create(self,FMaster,obj,to_update_obj,false,user_context);
            if updatestep.HasNoChanges then
              updatestep.Free
            else
              G_Transaction.AddChangeStep(updatestep);
            result := G_Transaction.GetTransLastStepTransId;
            if ImplicitTransaction then
              changes := Commit;
            if not changes then
              result := '';
          end;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionDefineIndexOnField(const coll_name: TFRE_DB_NameType; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean; const is_a_domain_index: boolean; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    step                : TFRE_DB_DefineIndexOnFieldStep;

begin
  LayerLock.Acquire;
  try
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction       := TFRE_DB_TransactionalUpdateList.Create('DIOF',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_DefineIndexOnFieldStep.Create(self,FMaster,coll_name,FieldName,FieldType,unique,ignore_content_case,index_name,allow_null_value,unique_null_values,is_a_domain_index,user_context);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        result     := step.GetTransActionStepID;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetIndexDefinition(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_INDEX_DEF;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      Res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not Res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexDefinition(index_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionDropIndex(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    step                : TFRE_DB_DropIndexStep;

begin
  LayerLock.Acquire;
  try
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('DRPIX',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_DropIndexStep.Create(self,FMaster,coll_name,index_name,user_context);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        result         := step.GetTransActionStepID;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
      end;
    finally
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetAllIndexNames(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_NameTypeArray;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      Res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not Res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.IndexNames;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  LayerLock.Acquire;
  try
    try
      if Assigned(G_Transaction) then
       exit(edb_EXISTS);
      G_Transaction := TFRE_DB_TransactionalUpdateList.Create(id,FMaster,FChangeNotificationIF);
      result := edb_OK;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.Commit: boolean;
begin
  LayerLock.Acquire;
  try
    try
      try
        result := G_Transaction.Commit;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
      end;
    finally
      G_Transaction.Free;
      G_Transaction := nil;
    end;
  finally
    LayerLock.Release;
  end;
end;

procedure TFRE_DB_PS_FILE.RollBack;
begin
  LayerLock.Acquire;
  try
    try
      try
        G_Transaction.Rollback;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
      end;
    finally
      G_Transaction.Free;
      G_Transaction := nil;
    end;
  finally
    LayerLock.Release;
  end;
end;

procedure TFRE_DB_PS_FILE.SyncSnapshot;
var i : NativeInt;
begin
  if FIsGlobalLayer then
    begin
      for i := 0 to high(FConnectedLayers) do
        if assigned(FConnectedLayers[i]) then
          FConnectedLayers[i].SyncSnapshot;
    end
  else
    begin
      _SyncDBInternal;
    end;
end;

function TFRE_DB_PS_FILE.GetNotificationRecordIF: IFRE_DB_DBChangedNotification;
begin
  result := FChangeNotificationIF;
end;

function TFRE_DB_PS_FILE.LayerLock: IFOS_LOCK;
begin
  result := G_GlobalLayerLock;
end;

function TFRE_DB_PS_FILE.CollectionExistsInCollection(const coll_name: TFRE_DB_NameType; const check_uid: TFRE_DB_GUID; const and_has_fetch_rights: boolean; const user_context: PFRE_DB_GUID): boolean;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    ut         : TFRE_DB_USER_RIGHT_TOKEN;
    obj        : TFRE_DB_Object;
begin
  E_FOS_TestNosey;
  LayerLock.Acquire;
  try
    try
      result := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not result then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.FetchIntFromCollO(check_uid,obj);
      if and_has_fetch_rights then
        if assigned(user_context) then
          begin
            G_GetUserToken(user_context,ut,true);
            if ut.CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,obj.SchemeClass,[sr_FETCH])=edb_OK then
              exit(true)
            else
              exit(false);
          end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionExistsInCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionExistsInCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionExistsInCollection',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionFetchInCollection(const coll_name: TFRE_DB_NameType; const check_uid: TFRE_DB_GUID; out dbo: IFRE_DB_Object; const user_context: PFRE_DB_GUID): TFRE_DB_Errortype;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    ut         : TFRE_DB_USER_RIGHT_TOKEN;
    obj        : TFRE_DB_Object;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      dbo := nil;
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      res := collection.FetchIntFromCollO(check_uid,obj);
      if not res then
        exit(edb_NOT_FOUND);
      if assigned(user_context) then
        begin
          G_GetUserToken(user_context,ut,true);
          if ut.CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,obj.SchemeClass,[sr_FETCH])<>edb_OK then
            exit(edb_ACCESS)
        end;
      dbo    := collection.CloneOutObject(obj);
      result := edb_OK;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionFetchInCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionFetchInCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionFetchInCollection',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionBulkFetch(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): IFRE_DB_ObjectArray;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;

begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      collection.GetAllObjectsRC(result,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetch',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetch',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetch',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionBulkFetchUIDS(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_GUIDArray;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      collection.GetAllUIDsRC(result,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetchUIDS',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetchUIDS',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionBulkFetchUIDS',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

procedure TFRE_DB_PS_FILE.CollectionClearCollection(const coll_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID);
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    ut         : TFRE_DB_USER_RIGHT_TOKEN;
    res        : boolean;
    cnt        : NativeInt;
    uidlist    : TFRE_DB_GUIDArray;
    i          : NativeInt;

    procedure GatherWithRights(const obj : TFRE_DB_Object);
    begin
      if ut.CheckStdRightSetUIDAndClass(obj.UID,obj.DomainID,obj.SchemeClass,[sr_FETCH])=edb_OK then
        begin
          uidlist[cnt] := obj.UID;
          inc(cnt);
        end;
    end;

begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      if not assigned(user_context) then
        collection.GetAllUIDS(uidlist)
      else
        begin
          G_GetUserToken(user_context,ut,true);
          SetLength(uidlist,collection.Count);
          cnt := 0;
          collection.ForAllInternal(@GatherWithRights);
          SetLength(uidlist,cnt);
        end;
      for i:=0 to high(uidlist) do
        DeleteObject(uidlist[i],coll_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionClearCollection',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionClearCollection',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionClearCollection',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionIndexExists(const coll_name: TFRE_DB_NameType; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): Boolean; { no right for collections defined/implemented by now }
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
begin
  LayerLock.Acquire;
  try
    try
      Result := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not Result then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.IndexExists(index_name)<>-1;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionIndexExists',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetIndexedValueCount(const coll_name: TFRE_DB_NameType; const qry_val: IFRE_DB_Object; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexedValueCountRC(qry_val,index_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedValueCount',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedValueCount',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedValueCount',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetIndexedObjsFieldval(const coll_name: TFRE_DB_NameType; const qry_val: IFRE_DB_Object; out objs: IFRE_DB_ObjectArray; const index_must_be_full_unique: boolean; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexedObjsClonedRC(qry_val,objs,index_must_be_full_unique,index_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsFieldval',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsFieldval',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsFieldval',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetIndexedUidsFieldval(const coll_name: TFRE_DB_NameType; const qry_val: IFRE_DB_Object; out objs: TFRE_DB_GUIDArray; const index_must_be_full_unique: boolean; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexedUidsRC(qry_val,objs,index_must_be_full_unique,index_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedUidsFieldval',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedUidsFieldval',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedUidsFieldval',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionRemoveIndexedUidsFieldval(const coll_name: TFRE_DB_NameType; const qry_val: IFRE_DB_Object; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
    uidlist    : TFRE_DB_GUIDArray;
    i          : NativeInt;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexedUidsRC(qry_val,uidlist,false,index_name,user_context);
      for i:=0 to high(uidlist) do
         DeleteObject(uidlist[i],coll_name);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionRemoveIndexedUidsFieldval',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionRemoveIndexedUidsFieldval',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionRemoveIndexedUidsFieldval',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetIndexedObjsRange(const coll_name: TFRE_DB_NameType; const min, max: IFRE_DB_Object; const ascending: boolean; const max_count, skipfirst: NativeInt; out objs: IFRE_DB_ObjectArray; const min_val_is_a_prefix: boolean; const index_name: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetIndexedObjsClonedRCRange(min,max,ascending,max_count,skipfirst,objs,min_val_is_a_prefix,index_name,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsRange',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsRange',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetIndexedObjsRange',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CollectionGetFirstLastIdxCnt(const coll_name: TFRE_DB_NameType; const idx: Nativeint; out obj: IFRE_DB_Object; const user_context: PFRE_DB_GUID): NativeInt;
var collection : TFRE_DB_PERSISTANCE_COLLECTION;
    ut         : TFRE_DB_USER_RIGHT_TOKEN;
    res        : boolean;
begin
  LayerLock.Acquire;
  try
    try
      res := FMaster.MasterColls.GetCollectionInt(coll_name,Collection);
      if not res then
        raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'collection [%s] not found',[coll_name]);
      result := collection.GetFirstLastIdxCnt(idx,obj,user_context);
    except
      on e:EFRE_DB_PL_Exception do
        begin
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetFirstLastIdxCnt',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetFirstLastIdxCnt',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['CollectionGetFirstLastIdxCnt',e.Message]);
          raise;
        end;
    end;
  finally
    LayerLock.Release;
  end;
end;


function TFRE_DB_PS_FILE.Connect(const db_name: TFRE_DB_String ; out db_layer: IFRE_DB_PERSISTANCE_LAYER ; const NotifIF: IFRE_DB_DBChangedNotificationBlock): TFRE_DB_Errortype;
var up_dbname : TFRE_DB_String;
    idx       : NativeInt;
    dblayer_o : TFRE_DB_PS_FILE;

  procedure UpdateNotifyIF;
  begin
    if assigned(NotifIF) then
      begin
        if assigned(dblayer_o.FChangeNotificationIF)
           and (not dblayer_o.FDontFinalizeNotif) then
             dblayer_o.FChangeNotificationIF.FinalizeNotif;
        dblayer_o.FChangeNotificationIF := TFRE_DB_DBChangedNotificationProxy.Create(NotifIF,db_name);
        dblayer_o.FDontFinalizeNotif := false;
      end
    else
      if not assigned(dblayer_o.FChangeNotificationIF) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,' should not happen ');
  end;

begin
  LayerLock.Acquire; { self }
  try
    if db_name='' then
      begin
        result.Code := edb_INVALID_PARAMS;
        result.Msg  := 'dbname is empty';
      end;
    up_dbname := uppercase(db_name);
    if (up_dbname<>'SYSTEM')
       and not assigned(G_SysMaster)
         then raise EFRE_DB_Exception.Create(edb_ERROR,'the first connect must be made to SYSTEM');
    dblayer_o  := _InternalFetchConnectedLayer(db_name,idx);
    if not assigned(dblayer_o) then
      begin
        SetLength(FConnectedLayers,Length(FConnectedLayers)+1);
        FConnectedLayers[high(FConnectedLayers)] := TFRE_DB_PS_FILE.InternalCreate(FBasedirectory,up_dbname,result);
        if result<>edb_OK then
          begin
            SetLength(FConnectedLayers,Length(FConnectedLayers)-1);
            exit;
          end;
        dblayer_o := FConnectedLayers[high(FConnectedLayers)];
        if dblayer_o.FConnectedDB='SYSTEM' then
          begin
            G_SysMaster := dblayer_o.FMaster; { copy SYSTEM Masterdata reference to global layer}
          end
        else
          begin
            SetLength(G_AllNonsysMasters,Length(G_AllNonsysMasters)+1);
            G_AllNonsysMasters[high(G_AllNonsysMasters)] := dblayer_o.FMaster;
          end;
        db_layer       := dblayer_o;
        UpdateNotifyIF;
        result         := edb_OK;
      end
    else
      begin
        result  := edb_OK;
        db_layer:= dblayer_o;
        UpdateNotifyIF;
      end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.Disconnect: TFRE_DB_Errortype;
begin
  result := edb_OK;
  if FIsGlobalLayer then
    raise EFRE_DB_Exception.Create(edb_ERROR,'you must not disconnect the GLOBAL layer')
  else
    begin
      { Let the "cloned" Layer exists but finalize the bound notif - currently only one conn layer per db is allowed }
    end;
end;

function TFRE_DB_PS_FILE.DatabaseList: IFOS_STRINGS;
var i : integer;
begin
  LayerLock.Acquire;
  try
    result := GFRE_TF.Get_FOS_Strings;
    GFRE_BT.List_Directorys(FBasedirectory,result,1,false);
    for i:=0 to result.Count-1 do begin
      result[i] := UnEsacpeDBName(result[i]);
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DatabaseExists(const dbname: TFRE_DB_String): Boolean;
begin
  LayerLock.Acquire;
  try
    _SetupDirs(dbname);
    result :=DirectoryExists(FLocalConnDir);
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CreateDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
begin
  LayerLock.Acquire;
  try
    if dbname = '' then
      begin
        Result.Code:=edb_INVALID_PARAMS;
        result.Msg:='dbname is empty';
        exit;
      end;
    if UpperCase(dbname)='GLOBAL' then
      begin
        Result.Code:=edb_RESERVED;
        result.Msg:='GLOBAL is a reserved name';
        exit;
      end;
    _SetupDirs(dbname);
    result.Msg  := 'database '+dbname+'already exists';
    result.Code := edb_EXISTS;
    if DirectoryExists(FLocalConnDir) then
      exit;
    if not (ForceDirectories(FLocalConnDir)) then
      exit;
    if not (ForceDirectories(FMasterCollDir)) then
      exit;
    if not (ForceDirectories(FCollectionsDir)) then
      exit;
    if not (ForceDirectories(FMetaDir)) then
      exit;
    if not (ForceDirectories(FWalDir)) then
      exit;
    result     := edb_OK;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DeleteDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
var dir: TFRE_DB_String;
      i: Integer;
begin
  LayerLock.Acquire;
  try
    if dbname='' then
      begin
        result.Code  := edb_INVALID_PARAMS;
        result.Msg   := 'dbname is empty';
      end;
    if UpperCase(dbname)='GLOBAL' then
      begin
        result.Code  := edb_RESERVED;
        result.Msg   := 'GLOBAL is reserved';
      end;
    dir := SetDirSeparators(FBasedirectory+'/'+EscapeDBName(dbname));
    if not DirectoryExists(dir) then
      begin
        result.Code := edb_NOT_FOUND;
        result.Msg  := 'the db named ['+dbname+'] is not found';
        exit;
      end;
    if GFRE_BT.Delete_Directory(dir) then begin
      begin
        result         := edb_OK;
        for i:=0 to high(FConnectedLayers) do
          begin
            if uppercase(FConnectedLayers[i].FConnectedDB)=uppercase(dbname) then;
              FConnectedLayers[i].FMaster.FDB_CleanUpMasterData;
          end;
      end;
    end else begin
      result.SetIt(edb_ERROR,'could not delete the db named ['+dbname+']');
      exit;
    end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DeployDatabaseScheme(const scheme: IFRE_DB_Object): TFRE_DB_Errortype;
var objdir : string;
begin
  LayerLock.Acquire;
  try
    MustNotBeGlobalLayerCheck;
    objdir := FBasedirectory+DirectorySeparator+'fre_scheme.dbo';
    scheme.SaveToFile(objdir);
    G_SysScheme.Finalize;
    LoadScheme;
    result := edb_OK;
  finally
    LayerLock.Finalize;
  end;
end;

function TFRE_DB_PS_FILE.GetDatabaseScheme(out scheme: IFRE_DB_Object): TFRE_DB_Errortype;
begin
  LayerLock.Acquire;
  try
    if assigned(G_SysScheme) then
      begin
        scheme := G_SysScheme.CloneToNewObject;
        result := edb_OK;
      end
    else
      begin
        result     := edb_NOT_FOUND;
        result.Msg := 'scheme not set';
      end;
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferences(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_GUIDArray;
begin
  LayerLock.Acquire;
  try
    result := FMaster.GetReferencesRC(obj_uid,from,scheme_prefix_filter,field_exact_filter,user_context,false);
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferencesCount(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): NativeInt;
begin
  LayerLock.Acquire;
  try
    result := FMaster.GetReferencesCountRC(obj_uid,from,scheme_prefix_filter,field_exact_filter,user_context);
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferencesDetailed(const obj_uid: TFRE_DB_GUID; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType; const user_context: PFRE_DB_GUID): TFRE_DB_ObjectReferences;
begin
  LayerLock.Acquire;
  try
    result := FMaster.GetReferencesDetailedRC(obj_uid,from,scheme_prefix_filter,field_exact_filter,user_context);
  finally
    LayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.ObjectExists(const obj_uid: TFRE_DB_GUID): boolean;
begin
  LayerLock.Acquire;
  try
    result := FMaster.ExistsObject(obj_uid);
  finally
    LayerLock.Release;
  end;
end;

end.

