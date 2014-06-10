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

uses  Classes, SysUtils,FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,fre_db_persistance_common,fos_sparelistgen,baseunix,fos_interlocked,FRE_SYSTEM;

function Get_PersistanceLayer_PS_Simple(const basedir:TFRE_DB_String):IFRE_DB_PERSISTANCE_LAYER;

//{$IFDEF DARWIN}
function  fredbps_fsync(filedes : cint): cint; cdecl; external 'c' name 'fsync';
//{$ENDIF}

   { TFRE_DB_PS_FILE }

   // The global layer is used to initial build and connect the specialized database layers (clone per db)
   // there exists one system database, and n user databases
   // masterdata is global volatile shared, system persistent shared, and per db shared

  type TFRE_DB_ASYNC_WT_THREAD=class;

  var  G_Transaction : TFRE_DB_TransactionalUpdateList;
       GAsyncWT      : TFRE_DB_ASYNC_WT_THREAD;

  type

   { TFRE_DB_ASYNC_WRITE_BLOCK }
   TFRE_DB_ASYNC_CMD=class
     procedure DoOperation ;virtual;abstract;
   end;

   { TFRE_DB_ASYNC_DEL_CMD }

   TFRE_DB_ASYNC_DEL_CMD=class(TFRE_DB_ASYNC_CMD)
   private
     FFilename :  String;
   public
     constructor Create(const filename : String);
     procedure   DoOperation; override;
   end;

   TFRE_DB_ASYNC_WRITE_BLOCK=class(TFRE_DB_ASYNC_CMD)
   private
     FFilename :  String;
     m         : TMemoryStream;
   public
     constructor Create(const filename : String);
     function    Stream : TMemoryStream;
     procedure   DoOperation; override;
     destructor  Destroy;override;
   end;

   { TFRE_DB_ASYNC_WT_THREAD }

   TFRE_DB_ASYNC_WT_THREAD=class(TThread)
   private
     Flfq : IFOS_LFQ;
     FWte : IFOS_TE;
     QC   : NativeUInt;
     procedure EmptyTheQ;
   public
     constructor Create;
     destructor  Destroy;override;
     procedure   Execute;override;
     procedure   PushWrite(const wb : TFRE_DB_ASYNC_CMD);
     procedure   Terminate;
   end;



   TFRE_DB_PS_FILE = class(TObject,IFRE_DB_PERSISTANCE_LAYER)
   private
     FMaster               : TFRE_DB_Master_Data; // Global, Volatile, Per DB, at least one for system, NIL in GLOBAL LAYER
     FSystemMaster         : TFRE_DB_Master_Data; // The Masterdata of the System Connection, only (! )set in global layer
     FLayerLock            : IFOS_LOCK;
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
     FDontFinalizeNotif    : Boolean;
     FConnectedLayers      : Array of TFRE_DB_PS_FILE;
     FConnectedDB          : TFRE_DB_String;
     FLastError            : TFRE_DB_String;
     FLastErrorCode        : TFRE_DB_Errortype;
     FChangeNotificationIF : IFRE_DB_DBChangedNotification;
     //FChangeNotifBlockIF   : IFRE_DB_DBChangedNotificationBlock;

     procedure    _ConnectCheck             ;
     procedure    _SetupDirs                (const db_name:TFRE_DB_String);
     function     EscapeDBName              (const name:string):string;
     function     UnEsacpeDBName            (const name:string):string;
     constructor  InternalCreate             (const basedir, name: TFRE_DB_String; out result: TFRE_DB_Errortype; const sysmasterdata: TFRE_DB_Master_Data);

     function     GetCollection             (const coll_name : TFRE_DB_NameType ; out Collection:IFRE_DB_PERSISTANCE_COLLECTION) : Boolean;
     function     ExistCollection           (const coll_name : TFRE_DB_NameType) : Boolean;


     procedure   _OpenWAL                   ;
     procedure   _CloseWAL                  ;
     procedure   _ClearWAL                  ;
     procedure   _StoreCollectionPersistent (const coll:IFRE_DB_PERSISTANCE_COLLECTION ; const no_storelocking : boolean=false);
     procedure   _StoreObjectPersistent     (const obj:TFRE_DB_Object ; const no_storelocking : boolean=false);

     procedure   WT_TransactionID              (const number:qword);
     procedure   WT_StoreCollectionPersistent  (const coll:IFRE_DB_PERSISTANCE_COLLECTION);
     procedure   WT_DeleteCollectionPersistent (const collname : TFRE_DB_NameType);
     procedure   WT_StoreObjectPersistent      (const obj: IFRE_DB_Object; const no_store_locking: boolean=true);
     procedure   WT_DeleteObjectPersistent     (const iobj:IFRE_DB_Object);
     function    INT_Fetch                     (const ouid    :  TGUID  ; out   dbi:IFRE_DB_Object):boolean; { unlocked internal fetch }
     function    WT_GetSysLayer                : IFRE_DB_PERSISTANCE_LAYER;

     {< Backup Functionality}
     function    FDB_GetObjectCount            (const coll:boolean; const SchemesFilter:TFRE_DB_StringArray=nil): Integer;
     procedure   FDB_ForAllObjects             (const cb:IFRE_DB_ObjectIteratorBrk; const SchemesFilter:TFRE_DB_StringArray=nil);
     procedure   FDB_ForAllColls               (const cb:IFRE_DB_Obj_Iterator);
     function    FDB_GetAllCollsNames          :TFRE_DB_NameTypeArray;
     procedure   FDB_PrepareDBRestore          (const phase:integer);     { used for various preparations and checks }
     procedure   FDB_SendObject                (const obj:IFRE_DB_Object);
     procedure   FDB_SendCollection            (const obj:IFRE_DB_Object);
     { Backup Functionality >}

     procedure   _SyncDBInternal            (const final:boolean=false);

     function   _InternalFetchConnectedLayer   (db_name:TFRE_DB_String;var idx :NativeInt): TFRE_DB_PS_FILE;

     procedure   DEBUG_DisconnectLayer         (const db:TFRE_DB_String;const clean_master_data :boolean = false);
     function    _FetchO                       (const ouid:TGUID ; out dbo:TFRE_DB_Object ; const internal_object:boolean): boolean;

   public
     function    GetConnectedDB                : TFRE_DB_NameType;

     constructor Create                        (const basedir,name:TFRE_DB_String);
     destructor  Destroy                       ; override;
     procedure   Finalize                      ;

     function    DatabaseList                  : IFOS_STRINGS;
     function    DatabaseExists                (const dbname:TFRE_DB_String):Boolean;
     function    CreateDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeleteDatabase                (const dbname:TFRE_DB_String):TFRE_DB_Errortype;


     function    GetReferences                 (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_GUIDArray;
     function    GetReferencesCount            (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):NativeInt;
     function    GetReferencesDetailed         (const obj_uid:TGuid;const from:boolean ; const scheme_prefix_filter : TFRE_DB_NameType ='' ; const field_exact_filter : TFRE_DB_NameType=''):TFRE_DB_ObjectReferences;


     function    Connect             (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false ; const NotifIF : IFRE_DB_DBChangedNotificationBlock=nil) : TFRE_DB_Errortype;
     function    Disconnect          : TFRE_DB_Errortype;
     function    ObjectExists        (const obj_uid : TGUID) : boolean;
     function    Fetch               (const ouid    :  TGUID  ; out   dbo:IFRE_DB_Object):TFRE_DB_Errortype;
     function    BulkFetch           (const obj_uids: TFRE_DB_GUIDArray ; out objects : IFRE_DB_ObjectArray):TFRE_DB_Errortype;


     { Transactional Operations / These operations report the last transaction step id generated, there may be more then one generated }

     function    NewCollection       (const coll_name : TFRE_DB_NameType ; const CollectionClassname : Shortstring ; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_TransStepId;
     function    DeleteCollection    (const coll_name : TFRE_DB_NameType) : TFRE_DB_TransStepId; // todo transaction context
     function    StoreOrUpdateObject (const   iobj:IFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean) : TFRE_DB_TransStepId; { must free the iobj in every case !}
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
     function    GetNotificationRecordIF : IFRE_DB_DBChangedNotification;
   end;

implementation

function Get_PersistanceLayer_PS_Simple(const basedir: TFRE_DB_String): IFRE_DB_PERSISTANCE_LAYER;
var l_Persistance_Layer : TFRE_DB_PS_FILE;
begin
  l_Persistance_Layer := TFRE_DB_PS_FILE.Create(basedir,'BASE');
  result              := l_Persistance_Layer;
end;

{ TFRE_DB_ASYNC_DEL_CMD }

constructor TFRE_DB_ASYNC_DEL_CMD.Create(const filename: String);
begin
  FFilename := filename;
end;

procedure TFRE_DB_ASYNC_DEL_CMD.DoOperation;
begin
  if not DeleteFile(FFileName) then
    begin
      writeln('------------------------>>>>>>>>>>>>> DELETE EMERGENCY   ',FFilename);
      GFRE_DBI.LogEmergency(dblc_PERSISTANCE,'>>DELETE ASYNC [%s] FAILED!',[FFilename]);
    end
  else
    begin
       GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>DELETE ASYNC [%s]',[FFilename]);
    end;
end;

{ TFRE_DB_ASYNC_WT_THREAD }

procedure TFRE_DB_ASYNC_WT_THREAD.EmptyTheQ;
var WB : TFRE_DB_ASYNC_CMD;
begin
  repeat
    WB :=TFRE_DB_ASYNC_CMD(Flfq.Pop);
    if assigned(wb) then
      begin
        FOS_IL_DEC_NATIVE(QC);
        try
          wb.DoOperation;
        except
          on e:Exception do
            begin
              GFRE_DBI.LogEmergency(dblc_PERSISTANCE,'>>ASYNC WT OPERATION ASYNC FAILED / [%s]',[WB.ClassName]);
            end;
        end;
        wb.free;
        Sleep(0);
      end;
  until WB=nil;
end;

constructor TFRE_DB_ASYNC_WT_THREAD.Create;
begin
  GFRE_TF.Get_LFQ(Flfq);
  GFRE_TF.Get_TimedEvent(FWte);
  Inherited Create(False);
end;

destructor TFRE_DB_ASYNC_WT_THREAD.Destroy;
begin
  EmptyTheQ;
  Flfq.Finalize;
  FwTe.Finalize;
  inherited Destroy;
end;

procedure TFRE_DB_ASYNC_WT_THREAD.Execute;
begin
  try
    while not Terminated do
      begin
        FWte.WaitFor(1000);
        EmptyTheQ;
      end;
  except
    on e:exception do
    begin
    end;
  end;
end;

procedure TFRE_DB_ASYNC_WT_THREAD.PushWrite(const wb: TFRE_DB_ASYNC_CMD);
begin
  FOS_IL_INC_NATIVE(QC);
  Flfq.Push(wb);
  FWte.SetEvent;
end;

procedure TFRE_DB_ASYNC_WT_THREAD.Terminate;
begin
  inherited Terminate;
end;

{ TFRE_DB_ASYNC_WRITE_BLOCK }

constructor TFRE_DB_ASYNC_WRITE_BLOCK.Create(const filename: String);
begin
  FFilename := filename;
  m         := TMemoryStream.Create;
end;

function TFRE_DB_ASYNC_WRITE_BLOCK.Stream: TMemoryStream;
begin
  result := m;
end;

procedure TFRE_DB_ASYNC_WRITE_BLOCK.DoOperation;
var msg : string;
begin
     try
       m.SaveToFile(FFilename);
       GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<STORE ASYNC : '+FFilename+' DONE');
     except on e:exception do
       begin
         writeln('----------------------------------------->>>>> EXC - ASYNC WRITE ',FFilename,' ',e.Message);
         msg := '<<STORE ASYNC : '+FFilename+' FAILED -> NOT DONE (!!!) '+e.Message;
         GFRE_DBI.LogError(dblc_PERSISTANCE,msg);
         GFRE_DBI.LogEmergency(dblc_PERSISTANCE,msg);
       end;
     end;
end;

destructor TFRE_DB_ASYNC_WRITE_BLOCK.Destroy;
begin
  m.Free;
  inherited Destroy;
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

constructor TFRE_DB_PS_FILE.InternalCreate(const basedir, name: TFRE_DB_String; out result: TFRE_DB_Errortype ;  const sysmasterdata : TFRE_DB_Master_Data);

    procedure _BuildMasterCollection;

      procedure add_guid(file_name:AnsiString);
      var g          : TGUID;
          obj        : TFRE_DB_Object;
          m          : TMemorystream;
          uid_string : TFRE_DB_String;
      begin
        g := GFRE_BT.HexString_2_GUID(Copy(file_name,1,32));
        uid_string:=GFRE_BT.GUID_2_HexString(g);
        GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>RETRIEVE OBJECT [%s]',[uid_string]);
        m:=TMemoryStream.Create;
        try
          m.LoadFromFile(FMasterCollDir+file_name);
          obj:= TFRE_DB_Object.CreateFromMemory(m.Memory);
        finally
          m.free;
        end;
        GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<RETRIEVE OBJECT [%s] DONE',[uid_string]);
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
      var f    : TFileStream;
          res  : TFRE_DB_Errortype;
          coll : IFRE_DB_PERSISTANCE_COLLECTION;
          name : TFRE_DB_NameType;
      begin
        name := GFRE_BT.HexStr2Str(Copy(file_name,1,Length(file_name)-4));
        GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>LOAD COLLECTION [%s]',[name]);
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
  GFRE_TF.Get_Lock(FLayerLock,true);
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
  FMaster := TFRE_DB_Master_Data.Create(FConnectedDB,self,sysmasterdata);
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
  if not GDBPS_SKIP_STARTUP_CHECKS then
    CheckDbResult(FMaster.InternalCheckRestoredBackup,' Internal Consistency Check Failed');
  FConnected   := true;
  FGlobalLayer := false;
end;

function TFRE_DB_PS_FILE.GetCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): Boolean;
begin
  result := FMaster.MasterColls.GetCollection(coll_name,Collection);
  //if (not result)
  //   and (FGlobalLayer=false) then // Fetch from Global
  //    result := GFRE_DB_PS_LAYER.GetCollection(coll_name,Collection);
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
var f  : TFileStream;
    m  : TFRE_DB_ASYNC_WRITE_BLOCK;
    fn : string;
begin
  if not coll.IsVolatile then
    begin
      fn := FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName(false))+'.col';
      if GDBPS_TRANS_WRITE_ASYNC then
        begin
          m :=  TFRE_DB_ASYNC_WRITE_BLOCK.Create(fn);
          coll.GetPersLayerIntf.StreamToThis(m.Stream);
          GAsyncWT.PushWrite(m);
        end
      else
        begin
          GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
          f :=  TFileStream.Create(fn,fmCreate+fmOpenReadWrite);
          try
            coll.GetPersLayerIntf.StreamToThis(f);
          finally
            f.free;
          end;
        end;
    end;
end;

procedure TFRE_DB_PS_FILE._StoreObjectPersistent(const obj: TFRE_DB_Object; const no_storelocking: boolean);
var  FileName : string;
     m        : TMemoryStream;
     w        : TFRE_DB_ASYNC_WRITE_BLOCK;

begin
  if obj.IsVolatile then
    exit;
  if obj.IsObjectRoot then
    begin
      if not no_storelocking then
        begin
          obj.Assert_CheckStoreLocked;
          obj.Set_Store_Locked(false);
        end;
      try
        obj._InternalGuidNullCheck;
        filename    := FMasterCollDir+GFRE_BT.GUID_2_HexString(obj.UID)+'.fdbo';
        if GDBPS_TRANS_WRITE_ASYNC then
          begin
            w := TFRE_DB_ASYNC_WRITE_BLOCK.Create(FileName);
            w.Stream.Size:=obj.NeededSize;
            obj.CopyToMemory(w.Stream.Memory);
            GAsyncWT.PushWrite(w);
          end
        else
          begin
            m:=TMemoryStream.Create;
            try
              m.Size:=obj.NeededSize;
              obj.CopyToMemory(m.Memory);
              m.SaveToFile(filename);
            finally
              m.free;
            end;
            GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<STORE OBJECT : '+obj.UID_String+' DONE');
          end;
      finally
        if not no_storelocking then
          begin
            obj.Set_Store_Locked(true);
          end;
      end;
    end;
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

procedure TFRE_DB_PS_FILE.WT_StoreCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
begin
  _StoreCollectionPersistent(coll);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteCollectionPersistent(const collname: TFRE_DB_NameType);
var wd : TFRE_DB_ASYNC_DEL_CMD;
    fn : String;
begin
  //if not coll.IsVolatile then
    //begin
      fn := FCollectionsDir+GFRE_BT.Str2HexStr(collname)+'.col';
      if GDBPS_TRANS_WRITE_ASYNC then
        begin
          wd := TFRE_DB_ASYNC_DEL_CMD.Create(fn);
          GAsyncWT.PushWrite(wd);
        end
      else
        begin
          if FileExists(fn) then
            begin
              GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>DELETE COLLECTION [%s]',[collname]);
              if not DeleteFile(fn) then
                raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete collection '+collname);
            end;
        end;
    //end;
end;

procedure TFRE_DB_PS_FILE.WT_StoreObjectPersistent(const obj: IFRE_DB_Object ; const no_store_locking : boolean=true);
begin
  _StoreObjectPersistent(obj.Implementor as TFRE_DB_Object,no_store_locking);
end;

procedure TFRE_DB_PS_FILE.WT_DeleteObjectPersistent(const iobj: IFRE_DB_Object);
var  FileName : shortstring;
     m        : TMemoryStream;
     obj      : TFRE_DB_Object;
     wd       : TFRE_DB_ASYNC_DEL_CMD;

begin
  if iobj.IsObjectRoot then
    begin
      obj := iobj.Implementor as TFRE_DB_Object;
      filename    := FMasterCollDir+GFRE_BT.GUID_2_HexString(obj.UID)+'.fdbo';
      if GDBPS_TRANS_WRITE_ASYNC then
        begin
          wd := TFRE_DB_ASYNC_DEL_CMD.Create(FileName);
          GAsyncWT.PushWrite(wd);
        end
      else
        begin
          if not DeleteFile(FileName) then
            raise EFRE_DB_PL_Exception.Create(edb_ERROR,'cannot persistance delete file '+FileName);
          GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<FINAL DELETE  OBJECT : '+obj.UID_String+' DONE');
        end;
    end;
end;

function TFRE_DB_PS_FILE.INT_Fetch(const ouid: TGUID; out dbi: IFRE_DB_Object): boolean;
var dbo : TFRE_DB_Object;
begin
  result := _FetchO(ouid,dbo,true);
  if result then
    dbi := dbo
  else
    dbi:=nil;
end;

function TFRE_DB_PS_FILE.WT_GetSysLayer: IFRE_DB_PERSISTANCE_LAYER;
begin
  if FMaster.IsSystemMasterData then
    exit(self)
  else
    exit(FMaster.SysLayer);
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

procedure TFRE_DB_PS_FILE._SyncDBInternal(const final:boolean=false);

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
  //if clean_master_data then       { BAD 23.5.14 GLOBAL LAYER SHOULD NOT HAVE A MASTER...}
  //  FMaster.FDB_CleanUpMasterData;
  if uppercase(DB)='SYSTEM' then
    FSystemMaster := nil;
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
  FLayerLock.Acquire;
  try
    CleanApply := false;
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction       := TFRE_DB_TransactionalUpdateList.Create('C',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_NewCollectionStep.Create(self,FMaster,coll_name,CollectionClassname,volatile_in_memory);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        CleanApply     := true;
        Collection     := step.GetNewCollection;
        result         := step.GetTransActionStepID;
        FLastErrorCode := edb_OK;
        FLastError     := '';
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['NewCollection',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
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
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DeleteCollection(const coll_name: TFRE_DB_NameType): TFRE_DB_TransStepId;
var coll                : TFRE_DB_Persistance_Collection;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    step                : TFRE_DB_DeleteCollectionStep;
begin
  FLayerLock.Acquire;
  try
    CleanApply := false;
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('DC',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_DeleteCollectionStep.Create(self,FMaster,coll_name);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        CleanApply     := true;
        result         := step.GetTransActionStepID;
        FLastErrorCode := edb_OK;
        FLastError     := '';
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteCollection',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
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
    FLayerLock.Release;
  end;
end;


constructor TFRE_DB_PS_FILE.Create(const basedir,name: TFRE_DB_String);
var GSTRING   : String;
    //MAX_GUID  : TGuid;
begin
  GFRE_TF.Get_Lock(FLayerLock,True);
  FBasedirectory := basedir;
  if not DirectoryExists(FBaseDirectory) then begin
    if not ForceDirectories(FBaseDirectory) then begin
      GFRE_BT.CriticalAbort('cannot setup basedirectory');
    end;
  end;
  FMaster               := nil; //TFRE_DB_Master_Data.Create('GLOBAL',self);
  FConnectedDB          := 'GLOBAL';
  FGlobalLayer          := True;
  FChangeNotificationIF := TFRE_DB_DBChangedNotificationBase.Create(FConnectedDB);
  FDontFinalizeNotif    := false;
  if GDBPS_TRANS_WRITE_ASYNC then
    GAsyncWT := TFRE_DB_ASYNC_WT_THREAD.Create;
  if FileExists(FBasedirectory+DirectorySeparator+'transaction') then
    begin
      GSTRING := GFRE_BT.StringFromFile(FBasedirectory+DirectorySeparator+'transaction');
      G_DB_TX_Number := StrToIntDef(GSTRING,0);
    end
  else
    GFRE_DBI.LogWarning(dblc_PERSISTANCE,'>>NO GLOBAL TRANSACTION COUNTER FILE FOUND');
  if G_DB_TX_Number=0 then
    GFRE_DBI.LogWarning(dblc_PERSISTANCE,'>> GLOBAL TRANSACTION COUNTER IS ZERO - MAYBE BAD/NO FILE');
end;


destructor TFRE_DB_PS_FILE.Destroy;
var
  i: NativeInt;
begin
  if FGlobalLayer then
    begin
      if GDBPS_TRANS_WRITE_ASYNC then
        begin
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'>> TERMINATING ASYNC WT WAIT');
          GAsyncWT.Terminate;
          GAsyncWT.WaitFor;
          GAsyncWT.Free;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'>> TERMINATING ASYNC WT DONE');
        end;
      for i:=0 to high(FConnectedLayers) do
        begin
          FConnectedLayers[i].Free;
        end;
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
  FLayerLock.Finalize;
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
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE REFERENCE LINKS');
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
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<STORE REFERENCE LINKS DONE');
//end;
//
//function TFRE_DB_PS_FILE.StoreCollection(const coll: TFRE_DB_COLLECTION): TFRE_DB_Errortype;
//var m:TMemoryStream;
//begin
//  _ConnectCheck;
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
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
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<STORE COLLECTION [%s] DONE',[coll.CollectionName]);
//end;

//function TFRE_DB_PS_FILE.RetrieveCollection(const collname: TFRE_DB_NameType; var coll: TFRE_DB_COLLECTION; const manage_info: TFRE_DB_Collection_ManageInfo): TFRE_DB_Errortype;
//var m        : TMemorystream;
//    filename : TFRE_DB_String;
//begin
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>RETRIEVE COLLECTION [%s]',[collname]);
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
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<RETRIEVE COLLECTION [%s] DONE',[collname]);
//end;

//function TFRE_DB_PS_FILE.DeleteCollection(const coll: TFRE_DB_COLLECTION): TFRE_DB_Errortype;
//var cname:TFRE_DB_String;
//begin
//  _ConnectCheck;
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>DELETE COLLECTION [%s]',[coll.CollectionName]);
//  cname:=FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName)+'.fdbo';
//  if not FileExists(cname) then exit(edb_NOT_FOUND);
//  try
//    if not DeleteFile(cname) then exit(edb_ERROR);
//  except
//    exit(edb_ERROR);
//  end;
//  result:=edb_OK;
//  inc(FLayerStats.DeleteColls);
//  GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<DELETE COLLECTION [%s] DONE',[coll.CollectionName]);
//end;


//function TFRE_DB_PS_FILE.DeleteObject(const UID: TGuid): TFRE_DB_Errortype;
//var filename  :TFRE_DB_String;
//    uid_string:TFRE_DB_String;
//_ConnectCheck;
//uid_string := GFRE_BT.GUID_2_HexString(uid);
//GFRE_DBI.LogDebug(dblc_PERSISTANCE,'>>DELETE OBJECT [%s]',[uid_string]);
//result := edb_OK;
//filename := uid_string+'.fdbo';
//if not DeleteFile(FMasterCollDir+filename) then begin
//  result := edb_ERROR;
//  exit;
//end;
//GFRE_DBI.LogDebug(dblc_PERSISTANCE,'<<DELETE OBJECT [%s]',[uid_string]);
//begin
//end;

function TFRE_DB_PS_FILE.Fetch(const ouid: TGUID; out dbo: IFRE_DB_Object): TFRE_DB_Errortype;
var dboo : TFRE_DB_Object;
begin
  FLayerLock.Acquire;
  try
    try
      if _FetchO(ouid,dboo,false) then
        begin
          dbo := dboo;
          FLastErrorCode := edb_OK;
          FLastError     := '';
          exit(edb_OK);
        end
      else
        begin
          dbo            := nil;
          FLastErrorCode := edb_NOT_FOUND;
          FLastError     := '';
          exit(edb_NOT_FOUND);
        end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          dbo := nil;
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          dbo := nil;
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          dbo := nil;
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Fetch',e.Message]);
          raise;
        end;
    end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.BulkFetch(const obj_uids: TFRE_DB_GUIDArray; out objects: IFRE_DB_ObjectArray): TFRE_DB_Errortype;
var dboo  : TFRE_DB_Object;
    dboa  : TFRE_DB_ObjectArray;
    i     : NativeInt;
    all   : Boolean;
begin
  SetLength(dboa,length(obj_uids));
  FLayerLock.Acquire;
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
          FLastErrorCode := edb_OK;
          FLastError     := '';
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
          FLastErrorCode := edb_NOT_FOUND;
          FLastError     := '';
          exit(edb_NOT_FOUND);
        end;
    except
      on e:EFRE_DB_PL_Exception do
        begin
          objects := nil;
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          objects := nil;
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          objects := nil;
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['BulkFetch',e.Message]);
          raise;
        end;
    end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE._FetchO(const ouid: TGUID; out dbo: TFRE_DB_Object; const internal_object: boolean): boolean;
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
  FLayerLock.Acquire;
  try
    CleanApply := false;
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('ID',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
          //if collection_name='' then
          if not _FetchO(obj_uid,delete_object,true) then
            raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be deleted but was not found [%s]',[GFRE_BT.GUID_2_HexString(obj_uid)]);

          //SetLength(notify_collections,Length(delete_object.__InternalGetCollectionList));
          //for i := 0 to high(notify_collections) do
          //  notify_collections[i] := delete_object.__InternalGetCollectionList[i].CollectionName();

          delete_object.Set_Store_Locked(false);
          if delete_object.IsObjectRoot then
             step := TFRE_DB_DeleteObjectStep.Create(self,FMaster,delete_object,collection_name,false)
          else
             step := TFRE_DB_DeleteSubObjectStep.Create(self,FMaster,delete_object,collection_name,false);
          G_Transaction.AddChangeStep(step);
          result := step.GetTransActionStepID;
          if ImplicitTransaction then
            Commit;
          CleanApply := true;
          FLastErrorCode := edb_OK;
          FLastError     := '';
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DeleteObject',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
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
    FLayerLock.Release;
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
    if not store then
      begin
        exit;
        writeln('THIS IS NOT ALLOWED 1');
        halt;
      end;
    if insert_obj.IsObjectRoot then
      G_Transaction.AddChangeStep(TFRE_DB_InsertStep.Create(self,FMaster,insert_obj.Implementor as TFRE_DB_Object,coll,store))
    else
      G_Transaction.AddChangeStep(TFRE_DB_InsertSubStep.Create(self,FMaster,insert_obj.Implementor as TFRE_DB_Object,coll,store));
  end;

  procedure GenDelete(const del_obj : IFRE_DB_Object);
  begin
    if not store then
      begin
        writeln('THIS IS NOT ALLOWED 2');
        halt;
      end;
    assert(not del_obj.IsObjectRoot); { this must be a subobject delete }
    G_Transaction.AddChangeStep(TFRE_DB_DeleteSubObjectStep.Create(self,FMaster,del_obj.Implementor as TFRE_DB_Object,collection_name,store));
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
  FLayerLock.Acquire;
  try
    obj        := iobj.Implementor as TFRE_DB_Object;
    CleanApply := false;
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
            if not GetCollection(collection_name,coll) then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the specified collection [%s] was not found',[collection_name]);
            if not assigned(G_Transaction) then
              begin
                G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('S',Fmaster,FChangeNotificationIF);
                ImplicitTransaction := True;
              end;
            to_update_obj := nil;
            if coll.IsVolatile then
              obj.Set_Volatile;
            TFRE_DB_Object.GenerateAnObjChangeList(obj,nil,@GenInsert,nil,nil); { there must not be updates or delets in STORE case}
            result := G_Transaction.GetTransLastStepTransId;
            if ImplicitTransaction then
              G_Transaction.Commit;
            obj.Set_Store_Locked(true);
            obj:=nil;
            CleanApply     := true;
            FLastErrorCode := edb_OK;
            FLastError     := '';
          end
        else
          begin
            if not obj.IsObjectRoot then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'the object [%s] is a child object, only root objects updates are allowed',[obj.UID_String]);
            if not _FetchO(obj.UID,to_update_obj,true) then
              raise EFRE_DB_PL_Exception.Create(edb_NOT_FOUND,'an object should be updated but was not found [%s]',[obj.UID_String]);
            if length(to_update_obj.__InternalGetCollectionList)=0 then
              begin
                writeln('::: OFFENDING OBJECT ', to_update_obj.DumpToString());
                //halt;
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
              try
                 to_update_obj.Set_Store_Locked(false);
                 if G_DEBUG_TRIGGER_1 then
                   begin
                     G_DEBUG_TRIGGER_1:=true;
                     //writeln('------ TO UPDATE OBJ ',to_update_obj.DumpToString());
                     //writeln('------- IN Object    ',iobj.DumpToString());
                     //halt;
                   end;
                 updatestep := TFRE_DB_UpdateStep.Create(self,FMaster,obj,to_update_obj,false);
                 TFRE_DB_Object.GenerateAnObjChangeList(obj,to_update_obj,nil,nil,@GenUpdate);
                 if updatestep.HasNoChanges then
                   updatestep.Free
                 else
                   G_Transaction.AddChangeStep(updatestep);
                 result := G_Transaction.GetTransLastStepTransId;
              finally
                to_update_obj.Set_Store_Locked(true);
              end;
              result := G_Transaction.GetTransLastStepTransId;
              if ImplicitTransaction then
                changes := Commit;
            CleanApply := true;
            if not changes then
              result := '';
            FLastErrorCode := edb_OK;
            FLastError     := '';
          end;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['StoreOrUpdateObject',e.Message]);
            raise;
          end;
      end;
    finally
      try
        if assigned(obj) then
          begin
            obj.Finalize;
            //PInt64(Pointer(@obj))^ := 0;
          end;
      except
        on E:Exception do
          GFRE_DBI.LogError(dblc_PERSISTANCE,'cannot finalize dbo on StoreOrUpdate, invalid instance [%s]',[e.Message]);
      end;
      if ImplicitTransaction then
        begin
          G_Transaction.Free;
          G_Transaction := nil;
        end;
    end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DefineIndexOnField(const coll_name: TFRE_DB_NameType ; const FieldName: TFRE_DB_NameType; const FieldType: TFRE_DB_FIELDTYPE; const unique: boolean; const ignore_content_case: boolean; const index_name: TFRE_DB_NameType; const allow_null_value: boolean; const unique_null_values: boolean): TFRE_DB_TransStepId;
var ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    step                : TFRE_DB_DefineIndexOnFieldStep;

begin
  FLayerLock.Acquire;
  try
    CleanApply := false;
    try
      try
        if not assigned(G_Transaction) then
          begin
            G_Transaction        := TFRE_DB_TransactionalUpdateList.Create('DIOF',Fmaster,FChangeNotificationIF);
            ImplicitTransaction := True;
          end;
        step := TFRE_DB_DefineIndexOnFieldStep.Create(self,FMaster,coll_name,FieldName,FieldType,unique,ignore_content_case,index_name,allow_null_value,unique_null_values);
        G_Transaction.AddChangeStep(step);
        if ImplicitTransaction then
          G_Transaction.Commit;
        CleanApply := true;
        result     := step.GetTransActionStepID;
        FLastErrorCode := edb_OK;
        FLastError     := '';
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['DefineIndexOnField',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
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
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType): TFRE_DB_Errortype;
begin
  FLayerLock.Acquire;
  try
    try
      if Assigned(G_Transaction) then
       exit(edb_EXISTS);
      G_Transaction := TFRE_DB_TransactionalUpdateList.Create(id,FMaster,FChangeNotificationIF);
      result := edb_OK;
      FLastErrorCode := edb_OK;
      FLastError     := '';
    except
      on e:EFRE_DB_PL_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
      on e:EFRE_DB_Exception do
        begin
          FLastErrorCode := E.ErrorType;
          FLastError     := E.Message;
          GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
      on e:Exception do
        begin
          FLastErrorCode := edb_INTERNAL;
          FLastError     := E.Message;
          GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Starttransaction',e.Message]);
          raise;
        end;
    end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.Commit: boolean;
begin
  FLayerLock.Acquire;
  try
    try
      try
        result := G_Transaction.Commit;
        if result then
         begin
           FLastErrorCode := edb_OK;
           FLastError     := '';
         end;
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Commit',e.Message]);
            raise;
          end;
      end;
    finally
      G_Transaction.Free;
      G_Transaction := nil;
    end;
  finally
    FLayerLock.Release;
  end;
end;

procedure TFRE_DB_PS_FILE.RollBack;
begin
  FLayerLock.Acquire;
  try
    try
      try
        G_Transaction.Rollback;
        FLastErrorCode := edb_OK;
        FLastError     := '';
      except
        on e:EFRE_DB_PL_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogNotice(dblc_PERSISTANCE,'PL/PL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
        on e:EFRE_DB_Exception do
          begin
            FLastErrorCode := E.ErrorType;
            FLastError     := E.Message;
            GFRE_DBI.LogInfo(dblc_PERSISTANCE,'PL/DB EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
        on e:Exception do
          begin
            FLastErrorCode := edb_INTERNAL;
            FLastError     := E.Message;
            GFRE_DBI.LogError(dblc_PERSISTANCE,'PL/INTERNAL EXCEPTION ON [%s] - FAIL :  %s',['Rollback',e.Message]);
            raise;
          end;
      end;
    finally
      G_Transaction.Free;
      G_Transaction := nil;
    end;
  finally
    FLayerLock.Release;
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

//procedure TFRE_DB_PS_FILE.SetNotificationStreamCallback(const change_if: IFRE_DB_DBChangedNotification; const create_proxy: boolean);
//begin
//  if create_proxy then
//    begin
//      FChangeNotificationProxy := TFRE_DB_DBChangedNotificationProxy.Create(change_if);
//      FChangeNotificationIF    := FChangeNotificationProxy;
//    end
//  else
//    FChangeNotificationIF := change_if;
//end;

function TFRE_DB_PS_FILE.GetNotificationRecordIF: IFRE_DB_DBChangedNotification;
begin
  result := FChangeNotificationIF;
end;

function TFRE_DB_PS_FILE.Connect(const db_name: TFRE_DB_String; out db_layer: IFRE_DB_PERSISTANCE_LAYER; const drop_wal: boolean; const NotifIF: IFRE_DB_DBChangedNotificationBlock): TFRE_DB_Errortype;
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
        //if NotifIF.InterfaceNeedsAProxy then
        //  begin
            dblayer_o.FChangeNotificationIF := TFRE_DB_DBChangedNotificationProxy.Create(NotifIF,db_name);
            dblayer_o.FDontFinalizeNotif := false;
        //  end
        //else
        //  begin
        //    dblayer_o.FChangeNotificationIF := NotifIF;
        //    dblayer_o.FDontFinalizeNotif := true;
        //  end;
      end
    else
      if not assigned(dblayer_o.FChangeNotificationIF) then
        raise EFRE_DB_Exception.Create(edb_INTERNAL,' should not happen ');
        //dblayer_o.FChangeNotificationIF := TFRE_DB_DBChangedNotificationBase.Create(db_name);
  end;

begin
  FLayerLock.Acquire;
  try
    if db_name='' then
      begin
        FLastErrorCode :=edb_INVALID_PARAMS;
        exit(FLastErrorCode);
      end;
    up_dbname := uppercase(db_name);
    dblayer_o  := _InternalFetchConnectedLayer(db_name,idx);
    if not assigned(dblayer_o) then
      begin
        SetLength(FConnectedLayers,Length(FConnectedLayers)+1);
        FConnectedLayers[high(FConnectedLayers)] := TFRE_DB_PS_FILE.InternalCreate(FBasedirectory,up_dbname,result,FSystemMaster);
        if result<>edb_OK then
          exit;
        dblayer_o := FConnectedLayers[high(FConnectedLayers)];
        if dblayer_o.FConnectedDB='SYSTEM' then
          FSystemMaster := dblayer_o.FMaster; { copy SYSTEM Masterdata reference to global layer}
        db_layer  := dblayer_o;
        UpdateNotifyIF;
        result         := edb_OK;
        FLastErrorCode := edb_OK;
        FLastError     := '';
      end
    else
      begin
        result  := edb_OK;
        db_layer:= dblayer_o;
        UpdateNotifyIF;
      end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.Disconnect: TFRE_DB_Errortype;
begin
  if FGlobalLayer then
    raise EFRE_DB_Exception.Create(edb_ERROR,'you must not disconnect the GLOBAL layer')
  else
    begin
      //FChangeNotificationIF.FinalizeNotif; { Let the "cloned" Layer exists but finalize the bound notif - currently only one conn layer per db is allowed }
      //FChangeNotificationIF:=nil;
    end;
end;

function TFRE_DB_PS_FILE.DatabaseList: IFOS_STRINGS;
var i : integer;
begin
  FLayerLock.Acquire;
  try
    result := GFRE_TF.Get_FOS_Strings;
    GFRE_BT.List_Directorys(FBasedirectory,result,1,false);
    for i:=0 to result.Count-1 do begin
      result[i] := UnEsacpeDBName(result[i]);
    end;
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DatabaseExists(const dbname: TFRE_DB_String): Boolean;
begin
  FLayerLock.Acquire;
  try
    _SetupDirs(dbname);
    result :=DirectoryExists(FLocalConnDir);
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.CreateDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
begin
  FLayerLock.Acquire;
  try
    if dbname = '' then
      begin
        FLastErrorCode := edb_NOT_FOUND;
        exit(FLastErrorCode);
      end;
    if UpperCase(dbname)='GLOBAL' then
      begin
        FLastErrorCode := edb_RESERVED;
        exit(FLastErrorCode);
      end;
    _SetupDirs(dbname);
    FLastError := 'database '+dbname+'already exists';
    if DirectoryExists(FLocalConnDir)   then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    if not ForceDirectories(FLocalConnDir) then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    if not ForceDirectories(FMasterCollDir) then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    if not ForceDirectories(FCollectionsDir) then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    if not ForceDirectories(FMetaDir) then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    if not ForceDirectories(FWalDir) then
      begin
        FLastErrorCode:=edb_EXISTS;
        exit(FLastErrorCode);
      end;
    result     := edb_OK;
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.DeleteDatabase(const dbname: TFRE_DB_String): TFRE_DB_Errortype;
var dir: TFRE_DB_String;
      i: Integer;
begin
  FLayerLock.Acquire;
  try
    FLastError:='';
    if dbname='' then
      begin
        FLastErrorCode:=edb_INVALID_PARAMS;
        exit(FLastErrorCode);
      end;
    if UpperCase(dbname)='GLOBAL' then
      begin
        FLastErrorCode:=edb_RESERVED;
        exit(FLastErrorCode);
      end;
    dir := SetDirSeparators(FBasedirectory+'/'+EscapeDBName(dbname));
    if not DirectoryExists(dir) then
      begin
        FLastErrorCode:=edb_NOT_FOUND;
        exit(FLastErrorCode);
      end;
    if GFRE_BT.Delete_Directory(dir) then begin
      begin
        result         := edb_OK;
        FLastErrorCode := edb_OK;
        FLastError     := '';
        for i:=0 to high(FConnectedLayers) do
          begin
            if uppercase(FConnectedLayers[i].FConnectedDB)=uppercase(dbname) then;
              FConnectedLayers[i].FMaster.FDB_CleanUpMasterData;
          end;
      end;
    end else begin
      FLastErrorCode:=edb_ERROR;
      exit(FLastErrorCode);
    end;
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferences(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_GUIDArray;
begin
  FLayerLock.Acquire;
  try
    result := FMaster.GetReferences(obj_uid,from,scheme_prefix_filter,field_exact_filter);
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferencesCount(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): NativeInt;
begin
  FLayerLock.Acquire;
  try
    result := FMaster.GetReferencesCount(obj_uid,from,scheme_prefix_filter,field_exact_filter);
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.GetReferencesDetailed(const obj_uid: TGuid; const from: boolean; const scheme_prefix_filter: TFRE_DB_NameType; const field_exact_filter: TFRE_DB_NameType): TFRE_DB_ObjectReferences;
begin
  FLayerLock.Acquire;
  try
    result := FMaster.GetReferencesDetailed(obj_uid,from,scheme_prefix_filter,field_exact_filter);
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;

function TFRE_DB_PS_FILE.ObjectExists(const obj_uid: TGUID): boolean;
begin
  FLayerLock.Acquire;
  try
    result := FMaster.ExistsObject(obj_uid);
    FLastErrorCode := edb_OK;
    FLastError     := '';
  finally
    FLayerLock.Release;
  end;
end;


end.

