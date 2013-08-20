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
     FConnectedLayers     : Array of TFRE_DB_PS_FILE;
     FConnectedDB         : TFRE_DB_String;
     FTransaction         : TFRE_DB_TransactionalUpdateList;
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

     procedure   DEBUG_DisconnectLayer (const db:TFRE_DB_String);
   public

     constructor Create                     (const basedir,name:TFRE_DB_String);
     destructor  Destroy                    ; override;
     procedure   Finalize                   ;

     function    NewCollection      (const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean; const global_system_namespace: boolean): TFRE_DB_Errortype; // todo transaction context
     function    DeleteCollection   (const coll_name : TFRE_DB_NameType ; const global_system_namespace:boolean) : TFRE_DB_Errortype; // todo transaction context


     function    DatabaseList       : IFOS_STRINGS;
     function    DatabaseExists     (const dbname:TFRE_DB_String):Boolean;
     function    CreateDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;
     function    DeleteDatabase     (const dbname:TFRE_DB_String):TFRE_DB_Errortype;

     function    GetReferenceCount   (const obj_uid: TGuid; const from: boolean): NativeInt;
     function    GetReferences       (const obj_uid: TGuid ; const from: boolean): TFRE_DB_ObjectReferences;

     function    Connect             (const db_name:TFRE_DB_String ; out db_layer : IFRE_DB_PERSISTANCE_LAYER ; const drop_wal : boolean=false) : TFRE_DB_Errortype;
     function    ObjectExists        (const obj_uid : TGUID) : boolean;
     function    Fetch               (const ouid:TGUID;out dbo:TFRE_DB_Object;const internal_object:boolean): boolean;
     function    StoreOrUpdateObject (var   obj:TFRE_DB_Object ; const collection_name : TFRE_DB_NameType ; const store : boolean ; const raise_ex : boolean ; var notify_collections : TFRE_DB_StringArray) : TFRE_DB_Errortype;
     function    DeleteObject        (const obj_uid : TGUID  ;  const raise_ex : boolean ; const collection_name: TFRE_DB_NameType = '' ; var notify_collections: TFRE_DB_StringArray = nil):TFRE_DB_Errortype;
     function    StartTransaction    (const typ:TFRE_DB_TRANSACTION_TYPE ; const ID:TFRE_DB_NameType ; const raise_ex : boolean=true) : TFRE_DB_Errortype;
     function    Commit              (const raise_ex : boolean=true): TFRE_DB_Errortype;
     function    RollBack            (const raise_ex : boolean=true): TFRE_DB_Errortype;
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
          obj:= TFRE_DB_Object.CreateFromMemory(m.Memory,nil);
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
  if FileExists(FWalFilename) then
    begin
      //Autorepair
      raise EFRE_DB_Exception.Create(edb_ERROR,'SERVER SHUTDOWN WAS UNCLEAN, MUST REAPPLY WAL FOR [%s]',[FConnectedDB]);
    end;
  FMaster := TFRE_DB_Master_Data.Create;
  _BuildMasterCollection;
  _BuildCollections;
  result := edb_OK;
  _OpenWAL;
  FConnected   := true;
  FGlobalLayer := false;
end;

function TFRE_DB_PS_FILE.GetCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION): Boolean;
begin
  result := FMaster.MasterColls.GetCollection(coll_name,Collection);
  if (not result)
     and (FGlobalLayer=false) then
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
  if FileExists(FWalFilename) then
    begin
      repeat
        handle := FpOpen(pointer(FWalFilename),O_WRONLY or O_APPEND); // O_SYNC
        res    := fpgeterrno;
      until (handle<>-1) or (res<>ESysEINTR);
      if (handle<0)  then
        raise EFRE_DB_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
    end
  else
    begin
      repeat
        handle := FpOpen(pointer(FWalFilename),O_CREAT or O_WRONLY or O_APPEND); // O_SYNC
        res    := fpgeterrno;
      until (handle<>-1) or (res<>ESysEINTR);
      if (handle<0)  then
        raise EFRE_DB_Exception.Create(edb_ERROR,'could not open the WAL [%d]',[res]);
    end;
  FWalStream := THandleStream.Create(handle);
end;

procedure TFRE_DB_PS_FILE._CloseWAL;
begin
  FpClose(FWalStream.Handle);
  FWalStream.Free;
  FWalStream:=nil;
end;

procedure TFRE_DB_PS_FILE._ClearWAL;
begin
  if FileExists(FWalFilename) then
    if not DeleteFile(FWalFilename) then
      raise EFRE_DB_Exception.Create(edb_ERROR,'WAL DELETE FAILED ['+FConnectedDB+']');
end;

procedure TFRE_DB_PS_FILE._StoreCollectionPersistent(const coll: IFRE_DB_PERSISTANCE_COLLECTION);
var f : TFileStream;
begin
  if not coll.IsVolatile then
    begin
      GFRE_DBI.LogDebug(dblc_PERSITANCE,'>>STORE COLLECTION [%s]',[coll.CollectionName]);
      f :=  TFileStream.Create(FCollectionsDir+GFRE_BT.Str2HexStr(coll.CollectionName)+'.col',fmCreate+fmOpenReadWrite);
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
      writeln('  ->> SYNCING COLL ',coll.CollectionName(false));
      _StoreCollectionPersistent(coll);
      result := false;
   end;

   procedure StoreObjects(const obj : TFRE_DB_Object);
   begin
     _StoreObjectPersistent(obj);
   end;

begin
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

procedure TFRE_DB_PS_FILE.DEBUG_DisconnectLayer(const db: TFRE_DB_String);
var idx : NativeInt;
     l  : TFRE_DB_PS_FILE;
begin
  if not FGlobalLayer then
    raise EFRE_DB_Exception.Create(edb_INTERNAL,'fail');
  l := _InternalFetchConnectedLayer(db,idx);
  l.Free;
  FConnectedLayers[idx] := nil;
end;

function TFRE_DB_PS_FILE.NewCollection(const coll_name: TFRE_DB_NameType; out Collection: IFRE_DB_PERSISTANCE_COLLECTION; const volatile_in_memory: boolean; const global_system_namespace: boolean): TFRE_DB_Errortype;
var coll : TFRE_DB_Persistance_Collection;
begin
  if global_system_namespace then
    result := FMaster.MasterColls.NewCollection(coll_name,Collection,true,self)
  else
    begin
      result := FMaster.MasterColls.NewCollection(coll_name,Collection,volatile_in_memory,self);
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
  //if _CheckUnclean then begin
  //  writeln('>UNCLEAN SHUTDOWN DETECTED -> CHECK');
  //  _SetUnclean(false);
  //end;
  //_SetUnclean(true);
  FMaster       := TFRE_DB_Master_Data.Create;
  FGlobalLayer  := True;
end;

//function TFRE_DB_PS_FILE.Clone(const name: TFRE_DB_String): IFRE_DB_PERSISTANCE_LAYER;
//var res : TFRE_DB_PS_FILE;
//begin
//  res := TFRE_DB_PS_File.InternalCreate(FBasedirectory,name);
//  res.Fcloned := true;
//  res.FMaster := FMaster;
//  result := res;
//end;

destructor TFRE_DB_PS_FILE.Destroy;
begin

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

function TFRE_DB_PS_FILE.DeleteObject(const obj_uid: TGUID; const raise_ex: boolean; const collection_name: TFRE_DB_NameType; var notify_collections: TFRE_DB_StringArray): TFRE_DB_Errortype;
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
      FTransaction        := TFRE_DB_TransactionalUpdateList.Create('IMPLICIT',Fmaster,self);
      ImplicitTransaction := True;
    end;
  try
    try
      //if collection_name='' then
      if not Fetch(obj_uid,delete_object,true) then
        if raise_ex then
          raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'an object should be deleted but was not found [%s]',[GFRE_BT.GUID_2_HexString(obj_uid)])
        else
          exit(edb_NOT_FOUND);

    SetLength(notify_collections,Length(delete_object.__InternalGetCollectionList));
    for i := 0 to high(notify_collections) do
      notify_collections[i] := delete_object.__InternalGetCollectionList[i].CollectionName();

    delete_object.Set_Store_Locked(false);
      try
        FTransaction.AddChangeStep(TFRE_DB_DeleteStep.Create(delete_object,false));
        if ImplicitTransaction then
            result := Commit(raise_ex);
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
      end
    else
    FTransaction := nil;
  end;
end;

// This is always the first entry into the store and update chain
function TFRE_DB_PS_FILE.StoreOrUpdateObject(var obj: TFRE_DB_Object; const collection_name: TFRE_DB_NameType; const store: boolean; const raise_ex: boolean; var notify_collections: TFRE_DB_StringArray): TFRE_DB_Errortype;
var coll                : IFRE_DB_PERSISTANCE_COLLECTION;
    error               : TFRE_DB_Errortype;
    to_update_obj       : TFRE_DB_Object;
    change_list         : TFRE_DB_Object;
    ImplicitTransaction : Boolean;
    CleanApply          : Boolean;
    ex_message          : string;
    i                   : NativeInt;

    //procedure CheckThatObjectAndAllSubsSatisfiyCollectionIndexes;
    //var colllist : IFRE_DB_PERSISTANCE_COLLECTION_ARRAY;
    //           i : NativeInt;
    //begin
    //  colllist := to_update_obj.__InternalGetCollectionList;
    //  for i := 0 to high(colllist) do
    //    begin
    //      result := colllist[i].GetPersLayerIntf.UpdateInThisColl(obj,to_update_obj,raise_ex,true);
    //      if result <> edb_OK then
    //        exit;
    //    end;
    //end;

    function  GenerateAnObjChangeList(const raise_ex:boolean):TFRE_DB_Errortype;
    var deleted_obj  : OFRE_SL_TFRE_DB_Object;
        inserted_obj : OFRE_SL_TFRE_DB_Object;
        updated_obj  : OFRE_SL_TFRE_DB_Object;

        //procedure WriteGuid(const o : TFRE_DB_Object ; const idx : NativeInt; var halt:boolean);
        //begin
        //  write(idx,' ',o.UID_String,',');
        //end;

        function ObjectGuidCompare(const o1,o2:TFRE_DB_Object):boolean;
        begin

          result := FREDB_Guids_Same(o1.UID,o2.UID);
        end;

        procedure SearchInOldAndRemoveExistingInNew(var o : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
        begin
          if deleted_obj.Exists(o,@ObjectGuidCompare)<>-1 then
            begin
              updated_obj.Add(o);
              inserted_obj.ClearIndex(idx);
            end
        end;

        procedure SearchInUpdatesAndRemoveExistingFromOld(var o : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
        var ex : NativeInt;
        begin
          if updated_obj.Exists(o,@ObjectGuidCompare)<>-1 then
            deleted_obj.ClearIndex(idx);
        end;

        procedure GenerateUpdates(var new_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
        var child      : TFRE_DB_Object;
            updatestep : TFRE_DB_UpdateStep;

            procedure CompareEvent (const obj:TFRE_DB_Object ; const compare_event : TFRE_DB_ObjCompareEventType ; const new_fld,old_field:TFRE_DB_FIELD);
            begin
              case compare_event of
                cev_FieldDeleted:
                    updatestep.addsubstep(cev_FieldDeleted,new_fld,nil);
                cev_FieldAdded:
                    updatestep.addsubstep(cev_FieldAdded,new_fld,nil);
                cev_FieldChanged :
                    updatestep.addsubstep(cev_FieldChanged,new_fld,old_field);
              end;
            end;

        begin
          if not FMaster.ExistsObject(new_object.UID) then
            begin
              writeln('DEBUG EXISTS CHECK UPDATE FAILED ',new_object.UID_String,' ',store);
              system.halt();
            end;
          if new_object.IsObjectRoot then
            begin
              updatestep := TFRE_DB_UpdateStep.Create(new_object,to_update_obj,store);
              new_object.__InternalCompareToObj(to_update_obj,@CompareEvent);
            end
          else
            begin
              child      := to_update_obj.FetchChildObj(new_object.UID);
              assert(assigned(child));
              updatestep := TFRE_DB_UpdateStep.Create(new_object,child,store);
              new_object.__InternalCompareToObj(child,@CompareEvent);
            end;
          if updatestep.HasNoChanges then
            updatestep.Free
          else
            begin
              FTransaction.AddChangeStep(updatestep);
              //writeln(updatestep.DescribeText);
            end;
             //FTransaction.PostProcessUpdateStep(updatestep);
        end;

        procedure GenerateInserts(var new_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
        begin
          FTransaction.AddChangeStep(TFRE_DB_InsertStep.Create(new_object,coll,store));
          if store then
            halt := true; // In insert case only generate an insert for the root object
        end;

        procedure GenerateDeletes(var del_object : TFRE_DB_Object ; const idx : NativeInt ; var halt: boolean);
        begin
          if not FMaster.ExistsObject(del_object.UID) then
            begin
              writeln('EXISTS CHECK DELETE FAILED ');
              system.halt;
            end;
          FTransaction.AddChangeStep(TFRE_DB_DeleteStep.Create(del_object,store));
        end;
    begin
      if G_DEBUG_TRIGGER_1=true then
        G_DEBUG_TRIGGER_1:=true;
      deleted_obj.Init(25);
      inserted_obj.Init(25);
      updated_obj.Init(25);
      //if assigneD(to_update_obj) then
        //to_update_obj.Field('pemper').AsString:='faker';
      //to_update_obj.Field('TEST').AsString:='fuuker';
      //to_update_obj.FieldPath('desc.txt').AsString:='ChangedChanged';
      //to_update_obj.DeleteField('desc');
      //obj.DeleteField('desc');

      //writeln('--- OLD OBJECT ----');
      //if assigned(to_update_obj) then
      //  writeln(to_update_obj.DumpToString());
      //writeln('--- NEW OBJECT -----');
      //writeln(obj.DumpToString());
      //writeln('------------');

      if assigned(to_update_obj) then // update case
        to_update_obj.__InternalGetFullObjectList(deleted_obj);
      obj.__InternalGetFullObjectList(inserted_obj);
//
//      writeln('------------------------');
//      writeln(' STEP A');
//      write('DELETED  LIST [');deleted_obj.ForAllBreak(@WriteGuid);writeln('] ',deleted_obj.Count);
//      write('INSERTED LIST [');inserted_obj.ForAllBreak(@WriteGuid);writeln('] ',inserted_obj.Count);
//      writeln('STEP B');
//      writeln('------------------------');

      // Yields the updated_obj in the updatelist and the inserts in the newlist, all objects come from the "new non persitent object copy"
      inserted_obj.ForAllBreak(@SearchInOldAndRemoveExistingInNew);
      // Yields the deletes in the oldlist, all objects in this are from the "old, stored persitent object"
      deleted_obj.ForAllBreak(@SearchInUpdatesAndRemoveExistingFromOld);

      //write('DELETED  LIST [');deleted_obj.ForAllBreak(@WriteGuid);writeln('] ',deleted_obj.Count);
      //write('INSERTED LIST [');inserted_obj.ForAllBreak(@WriteGuid);writeln('] ',inserted_obj.Count);
      //write('UPDATED  LIST [');updated_obj.ForAllBreak(@WriteGuid);writeln('] ',updated_obj.Count);

      if deleted_obj.Count>0 then
        deleted_obj.ForAllBreak(@GenerateDeletes);
      if inserted_obj.Count>0 then
        inserted_obj.ForAllBreak(@GenerateInserts);
      if updated_obj.Count>0 then
        updated_obj.ForAllBreak(@GenerateUpdates);
      result := edb_OK;
    end;


begin
  CleanApply := false;
  if not assigned(FTransaction) then
    begin
      FTransaction        := TFRE_DB_TransactionalUpdateList.Create('IMPLICIT',Fmaster,self);
      ImplicitTransaction := True;
    end;
  try
    try
      if store then
        begin
          if collection_name='' then
            if raise_ex then
              raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request')
            else
              exit(edb_INVALID_PARAMS);
          if not GetCollection(collection_name,coll) then
            if raise_ex then
              raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'the specified collection [%s] was not found',[collection_name])
            else
              exit(edb_NOT_FOUND);
          to_update_obj := nil;
          result := GenerateAnObjChangeList(raise_ex);
          if result <>edb_OK then
            exit(result);
          //writeln('>TRANSACTION INSERT LOG');
          //FTransaction.PrintTextLog;
          //writeln('<TRANSACTION INSERT LOG');
          //result := FTransaction.ProcessCheck(raise_ex);
          //if result <>edb_OK then
          //  exit(result);
          if ImplicitTransaction then
              result := Commit(raise_ex);
          obj.Set_Store_Locked(true);
          CleanApply := true;
          result := edb_OK;
        end
      else
        begin
          if not Fetch(obj.UID,to_update_obj,true) then
            if raise_ex then
              raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'an object should be updated but was not found [%s]',[obj.UID_String])
            else
              exit(edb_NOT_FOUND);
          coll := nil;
          if collection_name<>'' then
            if not GetCollection(collection_name,coll) then
              if raise_ex then
                raise EFRE_DB_Exception.Create(edb_INVALID_PARAMS,'a collectionname must be provided on store request')
              else
                exit(edb_INVALID_PARAMS);

          SetLength(notify_collections,Length(to_update_obj.__InternalGetCollectionList));
          for i := 0 to high(notify_collections) do
            notify_collections[i] := to_update_obj.__InternalGetCollectionList[i].CollectionName();
          to_update_obj.Set_Store_Locked(false);
          try
            GenerateAnObjChangeList(raise_ex);
            //writeln('>TRANSACTION UPDATE LOG');
            //FTransaction.PrintTextLog;
            //writeln('<TRANSACTION UPDATE LOG');
            //result := FTransaction.ProcessCheck(raise_ex);
            //if result <>edb_OK then
            //  exit(result);
            if ImplicitTransaction then
                result := Commit(raise_ex);
          finally
            to_update_obj.Set_Store_Locked(true);
          end;
          CleanApply := true;
          result := edb_OK;
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
        ex_message := e.Message;
        raise;
      end;
    end;
  finally
    if not CleanApply then
      begin
        writeln('*******************************');
        writeln('>>>>>> STORE OR UPDATE TRANSACTION ERROR !');
        writeln(ex_message);
        writeln('*******************************');
      end
    else
      FTransaction := nil;
  end;
end;

function TFRE_DB_PS_FILE.StartTransaction(const typ: TFRE_DB_TRANSACTION_TYPE; const ID: TFRE_DB_NameType; const raise_ex: boolean): TFRE_DB_Errortype;
begin
  if Assigned(FTransaction) then
    raise EFRE_DB_Exception.Create(edb_EXISTS,'a transaction context was already created');
  FTransaction := TFRE_DB_TransactionalUpdateList.Create(id,FMaster,self);
end;

function TFRE_DB_PS_FILE.Commit(const raise_ex: boolean): TFRE_DB_Errortype;
begin
  try
    result       := FTransaction.Commit(raise_ex);
  finally
    FTransaction.Free;
    FTransaction := nil;
  end;
end;

function TFRE_DB_PS_FILE.RollBack(const raise_ex: boolean): TFRE_DB_Errortype;
begin
  try
    result := FTransaction.Rollback(raise_ex);
  finally
    FTransaction.Free;
    FTransaction := nil;
  end;
end;

procedure TFRE_DB_PS_FILE.SyncWriteWAL(const WALMem: TMemoryStream); // TODO Check Performance impace of reopening WAL
var res:cint;
begin
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

