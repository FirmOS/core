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

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,fre_db_core,fre_basedbo_server,fre_system;

type


  { TFRE_PL_DBO_SERVER }

  TFRE_PL_DBO_SERVER=class(TFRE_DBO_SERVER)
  private
    FDataTimer : IFRE_APSC_TIMER;
    FLayers    : Array of IFRE_DB_PERSISTANCE_LAYER;
    FLayerLock : IFOS_LOCK;
  protected
    procedure   SetupPersistanceLayer ;
    procedure   Setup                 ; override;
    destructor  Destroy               ; override;
    procedure   WatchDog              (const timer : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
    procedure   NewChannel            (const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState); override;
    procedure   RemoveChannelFromList (const channel: IFRE_APSC_CHANNEL);
  public
    function    SearchLayer           (layerID:TFRE_DB_NameType ; out layer : IFRE_DB_PERSISTANCE_LAYER):boolean;
  end;

  { TFRE_DB_SERVER_NET_LAYER }

  TFRE_DB_SERVER_NET_LAYER=class(TObject) {Channel Bound}
  private
  type
    TPL_CMD_STATE   = (cs_READ_LEN,cs_READ_DATA);
  var
    FChannel      : IFRE_APSC_CHANNEL;
    FlayerID      : TFRE_DB_NameType;
    Fserver       : TFRE_PL_DBO_SERVER;
    myLayer       : IFRE_DB_PERSISTANCE_LAYER;
    FCMDState     : TPL_CMD_STATE;
    FLen          : Cardinal;
    FData         : Pointer;
  protected
    procedure   ChannelRead       (const channel      : IFRE_APSC_CHANNEL);
    procedure   ChannelDisconnect (const channel      : IFRE_APSC_CHANNEL);
  public
    procedure   SendAnswer              (const dbo : IFRE_DB_Object);
    procedure   NewDBOFromClient_Locked (const dbo : IFRE_DB_Object);
    constructor Create(server : TFRE_PL_DBO_SERVER);
    destructor  Destroy;override;
  end;


implementation

{ TFRE_DB_SERVER_NET_LAYER }

procedure TFRE_DB_SERVER_NET_LAYER.ChannelRead(const channel: IFRE_APSC_CHANNEL);
var lLayerID : TFRE_DB_NameType;

  procedure _FetchDBO;
  var
      len   : cardinal;
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
var cmd : IFRE_DB_Object;
    mem : pointer;
    siz : Cardinal;
begin
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
      dbo.Field('EC').AsInt16  := ord(edb_OK);
      dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure _DatabaseExists;
    var dbname : TFRE_DB_NameType;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      dbo.Field('A').AsBoolean := myLayer.DatabaseExists(dbname);
      dbo.Field('EC').AsInt16  := ord(edb_OK);
      dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure _CreateDatabase;
    var dbname : TFRE_DB_NameType;
        ec     : TFRE_DB_Errortype;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      ec := myLayer.CreateDatabase(dbname);
      dbo.Field('EC').AsInt16  := ord(ec);
      if ec<>edb_OK then
        dbo.Field('ES').AsString := myLayer.GetLastError
      else
        dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure _DeleteDatabase;
    var dbname : TFRE_DB_NameType;
        ec     : TFRE_DB_Errortype;
    begin
      dbname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      ec := myLayer.DeleteDatabase(dbname);
      dbo.Field('EC').AsInt16  := ord(ec);
      if ec<>edb_OK then
        dbo.Field('ES').AsString := myLayer.GetLastError
      else
        dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure _ExistsCollection;
    var collname : TFRE_DB_NameType;
    begin
      collname := dbo.Field('db').AsString;
      dbo.ClearAllFields;
      dbo.Field('A').AsBoolean := myLayer.ExistCollection(collname);
      dbo.Field('EC').AsInt16  := ord(edb_OK);
      dbo.Field('ES').AsString := '';
      SendAnswer(dbo);
    end;

    procedure _NewCollection;
    var collname : TFRE_DB_NameType;
        isVol    : Boolean;
        tsid     : TFRE_DB_TransStepId;
        col      : IFRE_DB_PERSISTANCE_COLLECTION;
    begin
      collname := dbo.Field('cn').AsString;
      isVol    := dbo.Field('v').AsBoolean;
      dbo.ClearAllFields;
      tsid := myLayer.NewCollection(collname,col,isVol);
      dbo.Field('EC').AsInt16    := ord(edb_OK);
      dbo.Field('TSID').AsString := tsid;
      dbo.Field('ES').AsString   := '';
      SendAnswer(dbo);
    end;

begin
  try
    cid := dbo.Field('CID').AsString;
    case cid of
       'GLE' : _GetLastError;
       'DL'  : _DatabaseList;
       'DE'  : _DatabaseExists;
       'CD'  : _CreateDatabase;
       'DD'  : _DeleteDatabase;
       'EC'  : _ExistsCollection;
       'NC'  : _NewCollection;
      else
        raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN PERSISTANCE NETCMD [%s] ON LAYER [%s]',[cid,FlayerID]);
    end;
  except
    on e:exception do
      begin
        dbo.ClearAllFields;
        dbo.Field('EC').AsInt16  := ord(edb_PERSISTANCE_ERROR);
        dbo.Field('ES').AsString := e.Message;
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

procedure TFRE_PL_DBO_SERVER.SetupPersistanceLayer;

  procedure _ConnectAllDatabases;
  var i       : Integer;
      dblist  : IFOS_STRINGS;
      ndbc    : TFRE_DB_CONNECTION;
      dbname  : string;
      res     : TFRE_DB_Errortype;
      log_txt : string;
      app     : TFRE_DB_APPLICATION;
      layer   : IFRE_DB_PERSISTANCE_LAYER;
  begin
    dblist := GFRE_DB_PS_LAYER.DatabaseList;
    dblist.Add('SYSTEM');
    SetLength(FLayers,dblist.Count);
    GFRE_DB.LogInfo(dblc_SERVER,'START SERVING DATABASES [%s]',[dblist.Commatext]);
    for i:= 0 to dblist.Count-1 do
      begin
        dbname := dblist[i];
        CheckDbResult(GFRE_DB_PS_LAYER.Connect(dbname,layer,false),'FAIL STARTUP PLDB : '+dbname);
        FLayers[i] := layer;
        GFRE_DB.LogNotice(dblc_PERSISTANCE,'STARTUP CONNECT [%s]',[FLayers[i].GetConnectedDB]);
      end;
  end;
begin
  _ConnectAllDatabases;
end;

procedure TFRE_PL_DBO_SERVER.Setup;
begin
  GFRE_TF.Get_Lock(FLayerLock);
  SetupPersistanceLayer;
  FDBO_Srv_Cfg.SpecialFile := cFRE_UX_SOCKS_DIR+'plsrv';
  FDBO_Srv_Cfg.Id          := 'FRE:PLServer';
  FDBO_Srv_Cfg.Port        := '44010';
  FDBO_Srv_Cfg.IP          := '0.0.0.0';
  FDBO_Srv_Cfg.FDontSendId := true;
  inherited Setup;
  GFRE_SC.AddTimer('WD',1000,@WatchDog);
end;

destructor TFRE_PL_DBO_SERVER.Destroy;
begin
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
          if uppercase(FLayers[i].GetConnectedDB)=layerID then
            begin
              layer := FLayers[i];
              exit(true);
            end;
      finally
        FLayerLock.Release;
      end;
    end;
  result := false;
end;



initialization

end.
