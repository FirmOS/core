unit fre_pl_dbo_server;

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

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,fre_db_core,fre_basedbo_server,fre_system,
  fre_db_persistance_common;

type


  { TFRE_PL_DBO_SERVER }

  TFRE_PL_DBO_SERVER = class;

  { TFRE_DB_Connected_Layer }

  TFRE_DB_Connected_Layer=class
  private
    FLayer   : IFRE_DB_PERSISTANCE_LAYER;
    FNotifIf : IFRE_DB_DBChangedNotification;
    FServer  : TFRE_PL_DBO_SERVER;
  public
    procedure   SendNotificationBlock (const block : IFRE_DB_Object);
    constructor Create(const server: TFRE_PL_DBO_SERVER; const layerdbname: TFRE_DB_NameType);
    destructor  Destroy;override;
  end;

  TFRE_PL_DBO_SERVER=class(TFRE_DBO_SERVER)
  private
    FDataTimer : IFRE_APSC_TIMER;
    FLayers    : Array of TFRE_DB_Connected_Layer;
    FLayerLock : IFOS_LOCK;
  protected
    procedure   SetupPersistanceLayers;
    procedure   WatchDog              (const timer : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
    procedure   NewChannel            (const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState); override;
    procedure   RemoveChannelFromList (const channel: IFRE_APSC_CHANNEL);
  public
    procedure   Setup                 ; override;
    destructor  Destroy               ; override;
    function    SearchLayer           (layerID:TFRE_DB_NameType ; out layer : IFRE_DB_PERSISTANCE_LAYER):boolean;
    procedure   NotifyAllConnectedLayers(layerID:TFRE_DB_NameType ; const block : IFRE_DB_Object);
  end;

  { TFRE_DB_SERVER_NET_LAYER }

  TFRE_DB_SERVER_NET_LAYER=class(TObject) { Channel Bound }
  private
  type
    TPL_CMD_STATE   = (cs_READ_LEN,cs_READ_DATA);
  var
    FChannel       : IFRE_APSC_CHANNEL;
    FlayerID       : TFRE_DB_NameType;
    Fserver        : TFRE_PL_DBO_SERVER;
    myLayer        : IFRE_DB_PERSISTANCE_LAYER;
    FCMDState      : TPL_CMD_STATE;
    FLen           : Cardinal;
    FData          : Pointer;
  protected
    procedure   ChannelRead       (const channel      : IFRE_APSC_CHANNEL);
    procedure   ChannelDisconnect (const channel      : IFRE_APSC_CHANNEL);
  public
    procedure   SendAnswer              (const dbo   : IFRE_DB_Object);
    procedure   COR_SendNotifyBlock     (const data : Pointer);
    procedure   NewDBOFromClient_Locked (const dbo   : IFRE_DB_Object);
    constructor Create(server : TFRE_PL_DBO_SERVER);
    destructor  Destroy;override;
  end;


implementation

{ TFRE_DB_Connected_Layer }

procedure TFRE_DB_Connected_Layer.SendNotificationBlock(const block: IFRE_DB_Object);
begin
  //writeln('BLOCK>> ',block.DumpToString());
  FServer.NotifyAllConnectedLayers(FLayer.GetConnectedDB,block);
  block.Finalize;
end;

constructor TFRE_DB_Connected_Layer.Create(const server: TFRE_PL_DBO_SERVER ; const layerdbname : TFRE_DB_NameType);
begin
  FServer  := server;
  FNotifIf := TFRE_DB_DBChangedNotificationProxy.Create(nil,layerdbname,@SendNotificationBlock);
end;

destructor TFRE_DB_Connected_Layer.Destroy;
begin
  FNotifIf.FinalizeNotif;
  inherited Destroy;
end;

{ TFRE_DB_SERVER_NET_LAYER }

procedure TFRE_DB_SERVER_NET_LAYER.ChannelRead(const channel: IFRE_APSC_CHANNEL);
var lLayerID : TFRE_DB_NameType;

  procedure _FetchDBO;
  var
      fcont : boolean;
      dbo   : IFRE_DB_Object;
  begin
    repeat
      fcont := false;
      case FCMDState of
        cs_READ_LEN:
          begin
            if channel.CH_GetDataCount>=4 then
              begin
                channel.CH_ReadBuffer(@FLen,4);
                fcont := true;
                getmem(FData,FLen);
                FCMDState:=cs_READ_DATA;
              end;
          end;
        cs_READ_DATA:
          begin
            if channel.CH_GetDataCount>=FLen then
              begin
                channel.CH_ReadBuffer(FData,FLen);
                fcont := true;
                try
                  try
                    dbo := GFRE_DBI.CreateFromMemory(FData);
                    try
                      NewDBOFromClient_Locked(dbo);
                    except on e:exception do
                      begin
                        GFRE_DBI.LogError(dblc_PERSISTANCE,'FAILURE INBOUND EVENT PROCESSING [%s]',[e.Message]);
                      end;
                    end;
                  finally
                    Freemem(FData);
                    FData:=nil;
                  end;
                except on e:exception do
                  begin
                    writeln('SUB CHANNEL READ FAILED ',e.Message);
                    channel.Finalize;
                    //FConnectState := sfc_NOT_CONNECTED;
                  end;
                end;
                FCMDState := cs_READ_LEN;
              end;
          end;
      end;
    until fcont=false;
  end;

begin
  if (FlayerID='') then
    begin { Layer not negotiated, read layername}
      lLayerID := channel.CH_ReadString;
      if not Fserver.SearchLayer(lLayerID,myLayer) then
        begin
          channel.CH_WriteString('NOT FOUND');
          channel.Finalize;
          Free;
        end
      else
        begin
          channel.CH_WriteString('OK');
          FlayerID := lLayerID;
        end;
    end
  else
    begin {Read PL Interface}
      _FetchDBO;
    end;
end;

procedure TFRE_DB_SERVER_NET_LAYER.ChannelDisconnect(const channel: IFRE_APSC_CHANNEL);
begin
  Fserver.RemoveChannelFromList(channel);
  Free;
end;

procedure TFRE_DB_SERVER_NET_LAYER.SendAnswer(const dbo: IFRE_DB_Object);
var mem : pointer;
    siz : Cardinal;
begin
  mem := nil;
  siz := FREDB_GetDboAsBufferLen(dbo,mem);
  try
    dbo.Finalize;
    FChannel.CH_WriteBuffer(mem,siz);
  finally
    Freemem(mem);
  end;
end;

procedure TFRE_DB_SERVER_NET_LAYER.COR_SendNotifyBlock(const data: Pointer);
var mem   : pointer;
    siz   : Cardinal;
    dbo   : IFRE_DB_Object;
    block : IFRE_DB_Object;
begin
  mem := nil;
  block := TFRE_DB_Object(data);
  dbo := GFRE_DBI.NewObject;
  dbo.Field('CID').AsString    := 'EVENT';
  dbo.Field('LAYER').AsString  := FlayerID;
  dbo.Field('BLOCK').AsObject  := block;
  siz := FREDB_GetDboAsBufferLen(dbo,mem);
  try
    dbo.Finalize;
    FChannel.CH_WriteBuffer(mem,siz);
  finally
    Freemem(mem);
  end;
end;

procedure TFRE_DB_SERVER_NET_LAYER.NewDBOFromClient_Locked(const dbo: IFRE_DB_Object);
var CID : ShortString;

    procedure __SendOK(const TSID:TFRE_DB_TransStepId='');
    begin
      dbo.Field('EC').AsInt16  := ord(edb_OK);
      if tsid<>'' then
        begin
          dbo.Field('TSID').AsString := tsid;
        end;
      dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure __SendResultCode(const ec : TFRE_DB_Errortype);
    begin
      dbo.Field('EC').AsInt16  := ord(ec);
      if ec<>edb_OK then
        dbo.Field('ES').AsString := myLayer.GetLastError
      else
        dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    function __GetColl(const cmd : IFRE_DB_Object):IFRE_DB_PERSISTANCE_COLLECTION;
    var cn : TFRE_DB_NameType;
    begin
      cn := cmd.Field('CN').AsString;
      if not myLayer.GetCollection(cn,result) then
        raise EFRE_DB_Exception.Create(edb_ERROR,'implicit net collection failure, collection [%s] not found',[cn]);
    end;

    procedure _GetLastError;
    begin
      dbo.ClearAllFields;
      dbo.Field('A').AsString  := myLayer.GetLastError;
      dbo.Field('EC').AsInt16  := ord(edb_OK);
      dbo.Field('ES').AsString := myLayer.GetLastError;
      SendAnswer(dbo);
    end;

    procedure _DatabaseList;
    var foss : IFOS_STRINGS;
           i : NativeInt;
    begin
      dbo.ClearAllFields;
      foss := myLayer.DatabaseList;
      for i := 0 to foss.Count-1 do
        dbo.Field('A').AddString(foss[i]);
      __SendOK;
    end;

    procedure _DatabaseExists;
    var dbname : TFRE_DB_NameType;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      dbo.Field('A').AsBoolean := myLayer.DatabaseExists(dbname);
      __SendOK;
    end;

    procedure _CreateDatabase;
    var dbname : TFRE_DB_NameType;
        ec     : TFRE_DB_Errortype;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      ec := myLayer.CreateDatabase(dbname);
      __SendResultCode(ec);
    end;

    procedure _DeleteDatabase;
    var dbname : TFRE_DB_NameType;
        ec     : TFRE_DB_Errortype;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      ec := myLayer.DeleteDatabase(dbname);
      __SendResultCode(ec);
    end;

    procedure _ExistsCollection;
    var collname : TFRE_DB_NameType;
        pcoll    : IFRE_DB_PERSISTANCE_COLLECTION;
    begin
      collname := dbo.Field('CN').AsString;
      dbo.ClearAllFields;
      dbo.Field('A').AsBoolean := myLayer.GetCollection(collname,pcoll);
      if assigned(pcoll) then
        begin
          dbo.Field('V').AsBoolean  := pcoll.IsVolatile;
          dbo.Field('CCN').AsString := pcoll.GetCollectionClassName;
        end;
      __SendOK;
    end;

    procedure _NewCollection;
    var collname : TFRE_DB_NameType;
        isVol    : Boolean;
        tsid     : TFRE_DB_TransStepId;
        col      : IFRE_DB_PERSISTANCE_COLLECTION;
        ccn      : ShortString;
    begin
      collname := dbo.Field('cn').AsString;
      ccn      := dbo.Field('ccn').AsString;
      isVol    := dbo.Field('v').AsBoolean;
      dbo.ClearAllFields;
      tsid := myLayer.NewCollection(collname,ccn,col,isVol);
      __SendOK(tsid);
    end;

    procedure _DeleteCollection;
    var collname : TFRE_DB_NameType;
    begin
      collname := dbo.Field('CN').AsString;
      dbo.ClearAllFields;
      dbo.Field('TSID').AsString := myLayer.DeleteCollection(collname);
      __SendOK;
    end;


    procedure _DefineIndexOnField;
    var coll_name   : TFRE_DB_NameType;
        field_name  : TFRE_DB_NameType;
        fieldtype   : TFRE_DB_FIELDTYPE;
        unique      : Boolean;
        igncase     : Boolean;
        idx_name    : TFRE_DB_NameType;
        allow_null  : Boolean;
        unique_null : Boolean;
        tsid        : TFRE_DB_TransStepId;
    begin
      coll_name   := dbo.Field('CN').AsString;
      field_name  := dbo.Field('FN').AsString;
      fieldtype   := FieldtypeShortString2Fieldtype(dbo.Field('FT').AsString);
      unique      := dbo.Field('U').AsBoolean;
      igncase     := dbo.Field('CC').AsBoolean;
      idx_name    := dbo.Field('IN').AsString;
      allow_null  := dbo.Field('AN').AsBoolean;
      unique_null := dbo.Field('UN').AsBoolean;
      dbo.ClearAllFields;
      tsid        := myLayer.DefineIndexOnField(coll_name,field_name,fieldtype,unique,igncase,idx_name,allow_null,unique_null);
      __SendOK(tsid);
    end;

    procedure _GetIndexedUid(const arr:boolean);
    var qv    : TFRE_DB_String;
        ixn   : TFRE_DB_NameType;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
        arra  : TFRE_DB_GUIDArray;
        gui   : TFRE_DB_GUID;
        ciu   : boolean;
    begin
      qv  := dbo.Field('Q').AsString;
      ixn := dbo.Field('IN').AsString;
      if arr then
        ciu := dbo.Field('CIU').AsBoolean;
      coll := __GetColl(dbo);
      dbo.ClearAllFields;
      if arr then
        begin
          dbo.Field('A').AsBoolean := coll.GetIndexedUID(qv,arra,ixn,ciu);
          dbo.Field('G').AsGUIDArr := arra;
        end
      else
        begin
          dbo.Field('A').AsBoolean := coll.GetIndexedUID(qv,gui,ixn);
          dbo.Field('G').AsGUID    := gui;
        end;
      __SendOK;
    end;

    procedure _GetAllGuids;
    var arra  : TFRE_DB_GUIDArray;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
    begin
      coll := __GetColl(dbo);
      dbo.ClearAllFields;
      arra:=nil;
      coll.GetAllUIDS(arra);
      dbo.Field('G').AsGUIDArr := arra;
      __SendOK;
    end;

    procedure _CollForallIndexed;
    var arra  : TFRE_DB_GUIDArray;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
        ixn   : TFRE_DB_NameType;
        asc   : boolean;
        skip  : NativeInt;
        mxc   : NativeInt;
    begin
      coll := __GetColl(dbo);
      ixn  := dbo.Field('IN').AsString;
      asc  := dbo.Field('A').AsBoolean;
      mxc  := dbo.Field('M').AsInt64;
      skip := dbo.Field('S').AsInt64;
      dbo.ClearAllFields;
      arra:=nil;
      coll.ForAllIndexed(arra,ixn,asc,mxc,skip);
      dbo.Field('G').AsGUIDArr := arra;
      __SendOK;
    end;

    procedure _CollForAllIndexedRange(const mode:NativeInt);
    var arra  : TFRE_DB_GUIDArray;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
        ixn   : TFRE_DB_NameType;
        asc   : boolean;
        misn  : boolean;
        maim  : boolean;
        skip  : NativeInt;
        mxc   : NativeInt;
        mivsi : Int64;
        mavsi : Int64;
        mivus : UInt64;
        mavus : UInt64;
        mivst : TFRE_DB_String;
        mavst : TFRE_DB_String;
    begin
      coll := __GetColl(dbo);
      ixn  := dbo.Field('IN').AsString;
      asc  := dbo.Field('A').AsBoolean;
      if (mode<3) then
        begin
          misn := dbo.Field('MN').AsBoolean;
          maim := dbo.Field('MM').AsBoolean;
        end;
      asc  := dbo.Field('A').AsBoolean;
      mxc  := dbo.Field('M').AsInt64;
      skip := dbo.Field('S').AsInt64;
      arra:=nil;
      case mode of
        0: { signed range }
          begin
            mivsi := dbo.Field('MIV').AsInt64;
            mavsi := dbo.Field('MAV').AsInt64;
            coll.ForAllIndexedSignedRange(mivsi,mavsi,arra,ixn,asc,misn,maim,mxc,skip);
          end;
        1: { unsigned range }
          begin
            mivus := dbo.Field('MIV').AsUInt64;
            mavus := dbo.Field('MAV').AsUInt64;
            coll.ForAllIndexedUnsignedRange(mivus,mavus,arra,ixn,asc,misn,maim,mxc,skip);
          end;
        2: { string range }
          begin
            mivst := dbo.Field('MIV').AsString;
            mavst := dbo.Field('MAV').AsString;
            coll.ForAllIndexedStringRange(mivst,mavst,arra,ixn,asc,misn,maim,mxc,skip);
          end;
        3: { prefix string }
          begin
            mivst := dbo.Field('MIV').AsString;
            coll.ForAllIndexPrefixString(mivst,arra,ixn,asc,mxc,skip);
          end;
      end;
      dbo.ClearAllFields;
      dbo.Field('G').AsGUIDArr := arra;
      __SendOK;
    end;

    procedure _GetIndexedObj(const arr:boolean);
    var qv    : TFRE_DB_String;
        ixn   : TFRE_DB_NameType;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
        orra  : IFRE_DB_ObjectArray;
        obj   : IFRE_DB_Object;
        ciu   : boolean;
        res   : boolean;
    begin
      qv  := dbo.Field('Q').AsString;
      ixn := dbo.Field('IN').AsString;
      if arr then
        ciu := dbo.Field('CIU').AsBoolean;
      coll := __GetColl(dbo);
      dbo.ClearAllFields;
      if arr then
        begin
          dbo.Field('A').AsBoolean   := coll.GetIndexedObj(qv,orra,ixn,ciu);
          dbo.Field('O').AsObjectArr := orra;
        end
      else
        begin
          res := coll.GetIndexedObj(qv,obj,ixn);
          dbo.Field('A').AsBoolean := res;
          if res then
            dbo.Field('O').AsObject  := obj;
        end;
      __SendOK;
    end;

    procedure _CollFetch(const arr:boolean);
    var qv    : TFRE_DB_GUID;
        obj   : IFRE_DB_Object;
        coll  : IFRE_DB_PERSISTANCE_COLLECTION;
        res   : boolean;
    begin
      qv  := dbo.Field('G').AsGUID;
      coll := __GetColl(dbo);
      dbo.ClearAllFields;
      if arr then
        begin
          raise EFRE_DB_Exception.Create(edb_UNSUPPORTED,'not implemented');
        end
      else
        begin
          obj:=nil;
          res := coll.Fetch(qv,obj);
          dbo.Field('A').AsBoolean := res;
          if res then
            dbo.Field('O').AsObject  := obj;
        end;
      __SendOK;
    end;


    procedure _StoreOrUpdate;
    var obj  : IFRE_DB_Object;
        cn   : TFRE_DB_NameType;
        s    : Boolean;
        tsid : TFRE_DB_TransStepId;
    begin
      obj := dbo.Field('O').CheckOutObject;
      cn  := dbo.Field('CN').AsString;
      s   := dbo.Field('S').AsBoolean;
      dbo.ClearAllFields;
      tsid  := myLayer.StoreOrUpdateObject(obj,cn,s);
      __SendOK(tsid);
    end;

    procedure _GlobFetch;
    var obj  : IFRE_DB_Object;
        g    : TFRE_DB_GUID;
        res  : TFRE_DB_Errortype;
    begin
      g   := dbo.Field('G').AsGUID;
      dbo.ClearAllFields;
      res := myLayer.Fetch(g,obj,false);
      dbo.Field('EC').AsInt16 := ord(res);
      if res=edb_OK then
        begin
          dbo.Field('ES').AsString := '';
          dbo.Field('O').AsObject  := obj;
        end
      else
        begin
          dbo.Field('ES').AsString := myLayer.GetLastError;
        end;
      SendAnswer(dbo);
    end;

    procedure _GlobDelete;
    var g    : TFRE_DB_GUID;
        cn   : TFRE_DB_NameType;
        tsid : TFRE_DB_TransStepId;
    begin
      g   := dbo.Field('G').AsGUID;
      cn   := dbo.Field('CN').AsString;
      dbo.ClearAllFields;
      tsid := myLayer.DeleteObject(g,cn);
      __SendOK(tsid);
    end;


    procedure _GetReferences(const how:NativeInt);
    var guid  : TFRE_DB_GUID;
        from  : boolean;
        spf   : TFRE_DB_NameType;
        fef   : TFRE_DB_NameType;
        arr   : TFRE_DB_GUIDArray;
        cnt   : NativeInt;
        orf   : TFRE_DB_ObjectReferences;
        sn,fn : TFRE_DB_StringArray;
        lu    : TFRE_DB_GUIDArray;
        i     : NativeInt;
    begin
      guid := dbo.Field('G').AsGUID;
      from := dbo.Field('F').AsBoolean;
      spf  := dbo.Field('SP').AsString;
      fef  := dbo.Field('FE').AsString;
      dbo.ClearAllFields;
      case how of
        0 :
          begin
            arr := myLayer.GetReferences(guid,from,spf,fef);
            dbo.Field('G').AsGUIDArr := arr;
          end;
        1 :
          begin
            cnt := myLayer.GetReferencesCount(guid,from,spf,fef);
            dbo.Field('C').AsInt64 := cnt;
          end;
        2 :
          begin
            orf := myLayer.GetReferencesDetailed(guid,from,spf,fef);
            SetLength(fn,Length(orf));
            SetLength(sn,Length(orf));
            SetLength(lu,Length(orf));
            for i:=0 to high(orf) do
              with orf[i] do
                begin
                  fn[i] := fieldname;
                  sn[i] := schemename;
                  lu[i] := linked_uid;
                end;
            dbo.Field('FN').AsStringArr := fn;
            dbo.Field('SN').AsStringArr := sn;
            dbo.Field('LU').AsGUIDArr   := lu;
          end;
      end;
      __SendOK;
    end;

begin
  try
    cid := dbo.Field('CID').AsString;
    case cid of
       'GLE'   : _GetLastError;
       'DL'    : _DatabaseList;
       'DE'    : _DatabaseExists;
       'CD'    : _CreateDatabase;
       'DD'    : _DeleteDatabase;
       'EC'    : _ExistsCollection;
       'GC'    : _ExistsCollection;
       'NC'    : _NewCollection;
       'DC'    : _DeleteCollection;
       'DIF'   : _DefineIndexOnField;
       'CGIUS' : _GetIndexedUid(true);
       'CGIU'  : _GetIndexedUid(false);
       'SOU'   : _StoreOrUpdate;
       'CGIO'  : _GetIndexedObj(false);
       'CHIOS' : _GetIndexedObj(true);
       'CGAU'  : _GetAllGuids;
       'CFAI'  : _CollForallIndexed;
       'CFAISR': _CollForAllIndexedRange(0);
       'CFAIUR': _CollForAllIndexedRange(1);
       'CFAISS': _CollForAllIndexedRange(2);
       'CFAIPS': _CollForAllIndexedRange(3);
       'CF'    : _CollFetch(false);
       'F'     : _GlobFetch;
       'D'     : _GlobDelete;
       'GR'    : _GetReferences(0);
       'GRC'   : _GetReferences(1);
       'GRD'   : _GetReferences(2);
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN PERSISTANCE NETCMD [%s] ON LAYER [%s]',[cid,FlayerID]);
    end;
  except
      on E:Exception do
        begin
          dbo.ClearAllFields;
          if myLayer.GetLastErrorCode<>edb_OK then
            begin
              dbo.Field('EC').AsInt16  := ord(myLayer.GetLastErrorCode);
              dbo.Field('ES').AsString := myLayer.GetLastError;
            end
          else
            begin { Fallback when an error occurs that did not set the Lasterror property of the layer (invalid field access in protocol) }
              dbo.Field('EC').AsInt16  := ord(edb_PERSISTANCE_ERROR);
              dbo.Field('ES').AsString := e.Message;
            end;
          SendAnswer(dbo);
        end;
  end;
end;


constructor TFRE_DB_SERVER_NET_LAYER.Create(server: TFRE_PL_DBO_SERVER);
begin
  Fserver := server;
end;

destructor TFRE_DB_SERVER_NET_LAYER.Destroy;
begin
  inherited Destroy;
end;

{ TFRE_PL_DBO_SERVER }

procedure TFRE_PL_DBO_SERVER.SetupPersistanceLayers;

  procedure _ConnectAllDatabases;
  var i       : Integer;
      dblist  : IFOS_STRINGS;
      dbname  : string;
      layer   : IFRE_DB_PERSISTANCE_LAYER;
      clayer  : TFRE_DB_Connected_Layer;
  begin
    dblist := GFRE_DB_PS_LAYER.DatabaseList;
    SetLength(FLayers,dblist.Count);
    GFRE_DB.LogInfo(dblc_SERVER,'START SERVING DATABASES [%s]',[dblist.Commatext]);
    for i:= 0 to dblist.Count-1 do
      begin
        dbname := dblist[i];
        clayer := TFRE_DB_Connected_Layer.Create(self,dbname);
        CheckDbResult(GFRE_DB_PS_LAYER.Connect(dbname,clayer.FLayer,false,clayer.FNotifIf),'FAIL STARTUP PLDB : '+dbname);
        FLayers[i] := clayer;
        GFRE_DB.LogNotice(dblc_PERSISTANCE,'STARTUP CONNECT [%s]',[clayer.FLayer.GetConnectedDB]);
      end;
  end;
begin
  _ConnectAllDatabases;
end;

procedure TFRE_PL_DBO_SERVER.Setup;
begin
  GFRE_TF.Get_Lock(FLayerLock);
  SetupPersistanceLayers;
  FDBO_Srv_Cfg.SpecialFile := cFRE_UX_SOCKS_DIR+cFRE_PS_LAYER_UXSOCK_NAME;
  FDBO_Srv_Cfg.Id          := 'FRE:PLServer';
  FDBO_Srv_Cfg.Port        := '44010';
  FDBO_Srv_Cfg.IP          := '0.0.0.0';
  FDBO_Srv_Cfg.FDontSendId := true;
  inherited Setup;
  GFRE_SC.AddTimer('WD',1000,@WatchDog);
end;

destructor TFRE_PL_DBO_SERVER.Destroy;
var layer : TFRE_DB_Connected_Layer;
begin
  for layer in FLayers do
    begin
      layer.free;
    end;
  FLayerLock.Finalize;
  inherited Destroy;
end;

procedure TFRE_PL_DBO_SERVER.WatchDog(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
//var obj : IFRE_DB_Object;
begin
   //obj := GFRE_DBI.NewObject;
   //obj.Field('MyData_TS').AsDateTimeUTC := GFRE_DT.Now_UTC;
   //obj.Field('MyData_Dta').AsUInt32 := random(10000);
   //PushDataToClients(obj);
end;

procedure TFRE_PL_DBO_SERVER.NewChannel(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
var new_pl_handler : TFRE_DB_SERVER_NET_LAYER;
begin
  inherited NewChannel(channel, channel_event);
  new_pl_handler := TFRE_DB_SERVER_NET_LAYER.Create(self);
  new_pl_handler.FChannel:=channel;
  channel.CH_AssociateData(FREDB_ObjectToPtrUInt(new_pl_handler));
  channel.SetOnReadData(@new_pl_handler.ChannelRead);
  channel.SetOnDisconnnect(@new_pl_handler.ChannelDisconnect);
end;

procedure TFRE_PL_DBO_SERVER.RemoveChannelFromList(const channel: IFRE_APSC_CHANNEL);
begin
  DiscoChannel(channel);
end;

function TFRE_PL_DBO_SERVER.SearchLayer(layerID: TFRE_DB_NameType; out layer: IFRE_DB_PERSISTANCE_LAYER): boolean;
var i : NativeInt;
begin
  layerID := UpperCase(layerID);
  if layerID='GLOBAL' then
    begin
      layer := GFRE_DB_PS_LAYER;
      exit(true);
    end
  else
    begin
      FLayerLock.Acquire;
      try
        for i := 0 to High(FLayers) do
          if uppercase(FLayers[i].FLayer.GetConnectedDB)=layerID then
            begin
              layer := FLayers[i].FLayer;
              exit(true);
            end;
      finally
        FLayerLock.Release;
      end;
    end;
  result := false;
end;

procedure TFRE_PL_DBO_SERVER.NotifyAllConnectedLayers(layerID: TFRE_DB_NameType; const block: IFRE_DB_Object);
var blocko : TFRE_DB_Object;
  procedure Search(const channel : IFRE_APSC_CHANNEL);
  var pl_handler : TFRE_DB_SERVER_NET_LAYER;
  begin
    pl_handler :=  TFRE_DB_SERVER_NET_LAYER(channel.CH_GetAssociateData);
    if pl_handler.FlayerID=layerID then
      begin
        blocko := block.CloneToNewObject.Implementor as TFRE_DB_Object;
        pl_handler.FChannel.GetChannelManager.ScheduleCoRoutine(@pl_handler.COR_SendNotifyBlock,blocko);
      end;
  end;

begin
  FLayerLock.Acquire;
  try
     ForAllChannels(@Search);
  finally
    FLayerLock.Release;
  end;
end;



initialization

end.
