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
     FMaster              : TFRE_DB_Master_Data; // Global, Volatile, Per DB, at least one for system
     FBasedirectory       : TFRE_DB_String;
     FMasterCollDir       : TFRE_DB_String;
     FLocalConnDir        : TFRE_DB_String;
     FCollectionsDir      : TFRE_DB_String;
     FMetaDir             : TFRE_DB_String;
     FWalDir              : TFRE_DB_String;
     FWalFilename         : TFRE_DB_String;
     FWalStream           : THandleStream;
     FConnected           : Boolean;
     FGlobalLayer         : Boolean;
     FBlockWALWrites      : Boolean;
     FConnectedLayers     : Array of TFRE_DB_PS_FILE;
     FConnectedDB         : TFRE_DB_String;
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
     procedure   _StoreCollectionPersistent (const coll:IFRE_DB_PERSISTANCE_COLLECTION);
     procedure   _LoadCollectionPersistent  (const file_name : string);
     procedure   _StoreObjectPersistent     (const obj:TFRE_DB_Object);
     procedure   _LoadObjectPersistent      (const UID: TGuid; var obj: TFRE_DB_Object);
     procedure   _SyncDBInternal            (const final:boolean=false);

     function   _InternalFetchConnectedLayer(db_name:TFRE_DB_String;var idx :NativeInt): TFRE_DB_PS_FILE;

     procedure  DEBUG_DisconnectLayer (const db:TFRE_DB_String;const clean_master_data :boolean = false);
   public

     constructor Create                     (const basedir,name:TFRE_DB_String);
     destructor  Destroy                    ; override;
     procedure   Finalize                   ;



     function    DatabaseList       : IFOS_STRINGS;
     function    DatabaseExists     (const dbname:TFRE_DB_String):Boolean;
     function    CreateDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeleteDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;

     function    GetReferenceCount   (const obj_uid: TGuid; const from: boolean): NativeInt;
     function    GetReferences       (const obj_uid: TGuid ; const from: boolean): TFRE_DB_ObjectReferences;

     function    Connect             (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false) : TFRE_DB_Errortype;
     function    ObjectExists        (const obj_uid : TGUID) : boolean;
     function    Fetch               (const ouid:TGUID;out dbo:TFRE_DB_Object;const internal_object:boolean): boolean;

     // Transactional Operations
     function    NewCollection       (const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_Errortype; // todo transaction context
     function    DeleteCollection    (const coll_name : TFRE_DB_NameType ; const global_system_namespace:boolean) : TFRE_DB_Errortype; // todo transaction context

     function    StoreOrUpdateObject (var   obj:TFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean ; var notify_collections : TFRE_DB_StringArray ; var status_text : string) : TFRE_DB_Errortype;
     function    DeleteObject        (const obj_uid : TGUID    ;  const collection_name: TFRE_DB_NameType = '' ; var notify_collections: TFRE_DB_StringArray = nil):TFRE_DB_Errortype;
     function    StartTransaction    (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType ; const raise_ex : boolean=true) : TFRE_DB_Errortype;
     function    Commit              : boolean;
     procedure   RollBack            ;
     // Transactional Operations Done

     procedure   SyncWriteWAL        (const WALMem : TMemoryStream);
     procedure   SyncSnapshot        (const final : boolean=false);
   end;

implementation

function Get_PersistanceLayer_PS_Simple(const basedir: TFRE_DB_String): IFRE_DB_PERSISTANCE_LAYER;
var l_Persistance_Layer : TFRE_DB_PS_FILE;
begin
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
          raise EFRE_DB_Exception.Create(result,'FAILED TO RECREATE MEMORY FROM STABLE at [%s]',[GFRE_BT.GUID_2_HexString(g)]);
      end;
    begin
      GFRE_BT.List_Files(FMasterCollDir,@add_guid);
      result := FMaster.InternalRebuildRefindex;
      FMaster.InternalStoreLock;
      if result<>edb_OK then
        raise EFRE_DB_Exception.Create(result,'FAILED TO RECREATE REFERENTIAL INTEGRITY FROM STABLE');
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
      //Autorepair
      _RepairWithWAL;
      raise EFRE_DB_Exception.Create(edb_ERROR,'SERVER SHUTDOWN WAS UNCLEAN, MUST REAPPLY WAL FOR [%s]',[FConnectedDB]);
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
      result := GFRE_DB_DEFAULT_PS_LAYER.GetCollection(coll_name,Collection);
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
        raise EFRE_DB_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
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
        raise EFRE_DB_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
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
      raise EFRE_DB_Exception.Create(edb_ERROR,'WAL DELETE FAILED ['+FConnectedDB+']');
end;

procedure TFRE_DB_PS_FILE._StoreCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
var f : TFileStream;
begin
  if not coll.IsVolatile then
    begin
      writeln('  ->> SYNCING COLL ',coll.CollectionName(false));
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
    res := FMaster.MasterColls.NewCollection(name,coll,false,self);
    if res <> edb_OK then
      raise EFRE_DB_Exception.Create(res,'LOAD COLLECTION FROM STABLE FAILED FOR [%s]',[name]);
    coll.GetPersLayerIntf.LoadFromThis(f);
  finally
    f.free;
  end;
end;

procedure TFRE_DB_PS_FILE._StoreObjectPersistent(const obj: TFRE_DB_Object);
var  FileName : string[60];
     m        : TMemoryStream;

begin
  if obj.IsObjectRoot then
    begin
      obj.Assert_CheckStoreLocked;
      obj.Set_Store_Locked(false);
      try
        //GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>STORE OBJECT : '+obj.UID_String);
        //writeln(' >> Syncing ',obj.UID_String,' ',obj.ClassName);
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
        obj.Set_Store_Locked(true);
      end;
    end;
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

   function WriteColls(const coll:IFRE_DB_PERSISTANCE_COLLECTION):boolean;
   begin
      _StoreCollectionPersistent(coll);
      result := false;
   end;

   procedure StoreObjects(const obj : TFRE_DB_Object);
   begin
     _StoreObjectPersistent(obj);
   end;

begin
  if GDISABLE_SYNC then
    begin
      writeln('ABORT/IGNORE - SYNC FOR DB : ',FConnectedDB);
      exit;
    end;

  writeln('>WRITEING MEMORY TO DISK FOR DB : ',FConnectedDB);
  FMaster.MasterColls.ForAllCollections(@WriteColls);
  writeln('-Syncing Objects');
  FMaster.ForAllObjectsInternal(true,false,@StoreObjects);
  writeln('<WRITEING MEMORY TO DISK DONE   : ',FConnectedDB);
  if assigned(FWalStream) then
    begin
      writeln('DROPPING WAL');
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
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'fail');
  if clean_master_data then
    FMaster.DEBUG_CleanUpMasterData;
  l := _InternalFetchConnectedLayer(db,idx);
  l.Free;
  FConnectedLayers[idx] := nil;
end;

function TFRE_DB_PS_FILE.NewCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean): TFRE_DB_Errortype;
var coll                : TFRE_DB_Persistance_Collection;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    step                : TFRE_DB_NewCollectionStep;

begin
  CleanApply := false;
  if not assigned(FTransaction) then
    begin
      FTransaction        := TFRE_DB_TransactionalUpdateList.Create('C',Fmaster);
      ImplicitTransaction := True;
    end;
  try
    try
      step := TFRE_DB_NewCollectionStep.Create(coll_name,volatile_in_memory);
      FTransaction.AddChangeStep(step);
      if ImplicitTransaction then
        FTransaction.Commit(self);
      CleanApply := true;
      Collection := step.GetNewCollection;
      result := edb_OK;
    except on e:Exception do
      begin
        ex_message := e.Message;
        raise;
      end;
    end;
  finally
    if not CleanApply then
      begin
        writeln('*******************************');
        writeln('>>>>>> NEW COLLECTION  TRANSACTION ERROR !');
        writeln(ex_message);
        writeln('*******************************');
      end;
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
      end;
  end;
end;

function TFRE_DB_PS_FILE.DeleteCollection(const coll_name: TFRE_DB_NameType; const global_system_namespace: boolean): TFRE_DB_Errortype;
begin
  if global_system_namespace then
    result := FMaster.MasterColls.DeleteCollection(coll_name)
  else
    result := FMaster.MasterColls.DeleteCollection(coll_name);
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
  FGlobalLayer  := True;
end;


destructor TFRE_DB_PS_FILE.Destroy;
begin
  writeln('FINALIZING PERSISTENCE LAYER');
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

function TFRE_DB_PS_FILE.Fetch(const ouid: TGUID; out dbo: TFRE_DB_Object; const internal_object: boolean): boolean;
begin
  result := FMaster.FetchObject(ouid,dbo,internal_object);
end;

function TFRE_DB_PS_FILE.DeleteObject(const obj_uid: TGUID; const collection_name: TFRE_DB_NameType; var notify_collections: TFRE_DB_StringArray): TFRE_DB_Errortype;
var
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    delete_object       : TFRE_DB_Object;
    i                   : NativeInt;

begin
  CleanApply := false;
  if not assigned(FTransaction) then
    begin
      FTransaction        := TFRE_DB_TransactionalUpdateList.Create('ID',Fmaster);
      ImplicitTransaction := True;
    end;
  try
    try
      //if collection_name='' then
      if not Fetch(obj_uid,delete_object,true) then
        raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'an object should be deleted but was not found [%s]',[GFRE_BT.GUID_2_HexString(obj_uid)]);

    SetLength(notify_collections,Length(delete_object.__InternalGetCollectionList));
    for i := 0 to high(notify_collections) do
      notify_collections[i] := delete_object.__InternalGetCollectionList[i].CollectionName();

    delete_object.Set_Store_Locked(false);
      try
        FTransaction.AddChangeStep(TFRE_DB_DeleteStep.Create(delete_object,false));
        if ImplicitTransaction then
          Commit;
      finally
        if Assigned(delete_object) then
          delete_object.Set_Store_Locked(true);
      end;
      CleanApply := true;
      result := edb_OK;
    except on e:Exception do
      begin
        ex_message := e.Message;
        raise;
      end;
    end;
  finally
    if not CleanApply then
      begin
        writeln('*******************************');
        writeln('>>>>>> DELETE TRANSACTION ERROR !');
        writeln(ex_message);
        writeln('*******************************');
      end;
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
      end;
  end;
end;

// This is always the first entry into the store and update chain
function TFRE_DB_PS_FILE.StoreOrUpdateObject(var obj: TFRE_DB_Object; const collection_name: TFRE_DB_NameType; const store: boolean; var notify_collections: TFRE_DB_StringArray; var status_text: string): TFRE_DB_Errortype;
var coll                : IFRE_DB_PERSISTANCE_COLLECTION;
    error               : TFRE_DB_Errortype;
    to_update_obj       : TFRE_DB_Object;
    change_list         : TFRE_DB_Object;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    i                   : NativeInt;
    changes             : Boolean;

  procedure GeneralChecks;
  begin
    if obj.DomainID=CFRE_DB_NullGUID then
      raise EFRE_DB_Exception.Create(edb_ERROR,'persistance failure, an object without a domainid cannot be stored');
  end;

begin
  CleanApply := false;
  try
    try
      GeneralChecks;
      if store then
        begin
          //if collection_name='' then
          //  raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request');
          //if not GetCollection(collection_name,coll) then
          //  raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the specified collection [%s] was not found',[collection_name]);
          if not assigned(FTransaction) then
            begin
              FTransaction        := TFRE_DB_TransactionalUpdateList.Create('S',Fmaster);
              ImplicitTransaction := True;
            end;
          to_update_obj := nil;
          result := FTransaction.GenerateAnObjChangeList(store,obj,collection_name,notify_collections);
          if result <>edb_OK then
            exit(result);
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
          result := edb_OK;
        end
      else
        begin
          //if not Fetch(obj.UID,to_update_obj,true) then
          //  raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'an object should be updated but was not found [%s]',[obj.UID_String]);
          //coll := nil;
          //if collection_name<>'' then
          //  if not GetCollection(collection_name,coll) then
          //    raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request');
          if not assigned(FTransaction) then
            begin
              FTransaction        := TFRE_DB_TransactionalUpdateList.Create('U',Fmaster);
              ImplicitTransaction := True;
            end else
              ImplicitTransaction := false;

            result := FTransaction.GenerateAnObjChangeList(store,obj,collection_name,notify_collections);
            //writeln('>TRANSACTION UPDATE LOG');
            //FTransaction.PrintTextLog;
            //writeln('<TRANSACTION UPDATE LOG');
            //result := FTransaction.ProcessCheck(raise_ex);
            //if result <>edb_OK then
            //  exit(result);
            if ImplicitTransaction then
              changes := Commit;
          CleanApply := true;
          if changes then
            result := edb_OK
          else
            result := edb_NO_CHANGE;
          //CheckThatObjectAndAllSubsSatisfiyCollectionIndexes;
        //  if result<>edb_OK then
        //    exit;
        ////  CheckThatAllDeletedSubobjectsAreNotReferenced
        ////  DeleteTheSubobjects         WithRespect To Reflinsk
        ////  InsertTheNewSubobjects      WithRespect To Reflinks
        ////  UpdateAllExistingSubobjects WithRespect To Reflinks (overwrite)
          //writeln('UPDATE ++');
          //halt;
        end;
    except on e:Exception do
      begin
        result      := edb_ERROR;
        status_text := e.Message;
      end;
    end;
  finally
    if not CleanApply then
      begin
        writeln('*******************************');
        writeln('>>>>>> STORE OR UPDATE TRANSACTION ERROR !');
        writeln(status_text);
        writeln('*******************************');
      end;
    if ImplicitTransaction then
      begin
        FTransaction.Free;
        FTransaction := nil;
      end;
  end;
end;

function TFRE_DB_PS_FILE.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  if Assigned(FTransaction) then
   exit(edb_EXISTS);
  FTransaction := TFRE_DB_TransactionalUpdateList.Create(id,FMaster);
  result := edb_OK;
end;

function TFRE_DB_PS_FILE.Commit: boolean;
begin
  try
    result := FTransaction.Commit(self);
  finally
    FTransaction.Free;
    FTransaction := nil;
  end;
end;

procedure TFRE_DB_PS_FILE.RollBack;
begin
  try
    FTransaction.Rollback;
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
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'DOING WAL WRITES WHILE BLOCKED !!');
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
      db_layer := FConnectedLayers[high(FConnectedLayers)]
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
  if dbname = '' then exit(edb_NOT_FOUND);
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
  if dbname='' then exit(edb_NOT_FOUND);
  dir := SetDirSeparators(FBasedirectory+'/'+EscapeDBName(dbname));
  if not DirectoryExists(dir) then exit(edb_NOT_FOUND);
  if GFRE_BT.Delete_Directory(dir) then begin
    result := edb_OK;
  end else begin
    Result := edb_ERROR;
  end;
end;

function TFRE_DB_PS_FILE.GetReferenceCount(const obj_uid: TGuid; const from: boolean): NativeInt;
begin
  result := FMaster.GetReferenceCount(obj_uid,from);
end;

function TFRE_DB_PS_FILE.GetReferences(const obj_uid: TGuid; const from: boolean): TFRE_DB_ObjectReferences;
begin
  result := FMaster.GetReferences(obj_uid,from);
end;

function TFRE_DB_PS_FILE.ObjectExists(const obj_uid: TGUID): boolean;
begin
  result := FMaster.ExistsObject(obj_uid);
end;


end.

