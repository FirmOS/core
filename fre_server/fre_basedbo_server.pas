unit fre_basedbo_server;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FRE_SYS_BASE_CS,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,FOS_INTERLOCKED,FRE_SYSTEM,baseunix;

type
  RDBO_SRV_CFG=record
    Id          : Shortstring; // Hello ID OF Subfeeder;
    SpecialFile : Shortstring; // if set open an unix socket
    IP,Port     : String; //IF set open an TCP Listener;
    FDontSendId : Boolean;
  end;

  { TFRE_DBO_SERVER }

  TFRE_DBO_SERVER=class(TFRE_DB_Base)
  private
    FLock        : IFOS_LOCK;
    FListenerUX  : IFRE_APSC_LISTENER;
    FListenerTCP : IFRE_APSC_LISTENER;
    procedure   NewListener  (const new_listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
  protected
    FChannelList : TList;
    FDBO_Srv_Cfg : RDBO_SRV_CFG;
    procedure   NewChannel   (const channel      : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState ; const errorstring: string; const errorcode: NativeInt);virtual;
    procedure   DiscoChannel (const channel      : IFRE_APSC_CHANNEL); virtual;
    procedure   ReadChannel  (const channel      : IFRE_APSC_CHANNEL); virtual;
  public
    constructor Create;
    destructor  Destroy  ; override ;
    procedure   Setup; virtual;
    procedure   PushDataToClients(const data_object : IFRE_DB_Object);virtual;
    procedure   ForAllChannels   (const chan_iter : TFRE_APSC_CHANNEL_EVENT_NESTED);virtual;
  end;

implementation

{ TFRE_DBO_SERVER }

procedure TFRE_DBO_SERVER.NewListener(const new_listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  abort;
  err := new_listener.GetErrorString;
  if state =als_EVENT_NEW_LISTENER then
    begin
      if new_listener.cs_GetID='ux' then
        begin
          FListenerUX:=new_listener;
          new_listener.cs_Start;
        end
      else
      if new_listener.cs_GetID='tcp' then
        begin
          FListenerTCP:=new_listener;
          new_listener.cs_Start;
        end
      else
        GFRE_BT.CriticalAbort('unsupported/unknown listenerid '+new_listener.cs_GetID);
    end;
end;

procedure TFRE_DBO_SERVER.NewChannel(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState; const errorstring: string; const errorcode: NativeInt);
var dbo : IFRE_DB_Object;
    mem : Pointer;
    siz : Cardinal;
begin
  if channel.CH_IsClientChannel then
    begin
      GFRE_BT.CriticalAbort('unexpected client channel connect?');
    end
  else
    begin
      //writeln('CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID,' via LISTENR ',channel.GetListener.GetListeningAddress,' PARTNER=',channel.GetConnSocketAddr);
      if not FDBO_Srv_Cfg.FDontSendId then
        begin
          dbo := GFRE_DBI.NewObject;
          dbo.Field('PLID').AsString:=FDBO_Srv_Cfg.Id;
          siz :=  FREDB_GetDboAsBufferLen(dbo,mem);
          dbo.Finalize;
          channel.CH_WriteBuffer(mem,siz);
          Freemem(mem);
        end;
      channel.ch_SetOnDisconnnect(@DiscoChannel);
      channel.ch_SetOnReadData(@ReadChannel);
      FLock.Acquire;
      try
        if FChannelList.IndexOf(channel)<>-1 then
          GFRE_BT.CriticalAbort('channel double add?');
        FChannelList.Add(channel);
        channel.CH_Enable_Reading;
      finally
        FLock.Release;
      end;
    end;
end;

procedure TFRE_DBO_SERVER.DiscoChannel(const channel: IFRE_APSC_CHANNEL);
begin
  FLock.Acquire;
  try
    if FChannelList.Remove(channel)=-1 then
      GFRE_BT.CriticalAbort('unexpected : channel not found');
  finally
    FLock.Release;
  end;
end;

procedure TFRE_DBO_SERVER.ReadChannel(const channel: IFRE_APSC_CHANNEL);
begin
  writeln('!!READ UNSUPPORTED');
  GFRE_BT.CriticalAbort('DONT WRITE SOMETHING TO ME');
end;

constructor TFRE_DBO_SERVER.Create;
begin
  inherited;
  GFRE_TF.Get_Lock(FLock);
  FChannelList := TList.Create;
end;

destructor TFRE_DBO_SERVER.Destroy;
begin
  if assigned(FLock) then
    FLock.Finalize;
  FChannelList.Free;
  inherited Destroy;
end;

procedure TFRE_DBO_SERVER.Setup;
begin
  if cFRE_MACHINE_NAME='' then
    begin
      GFRE_BT.CriticalAbort('No NAME set in subsection [MACHINE] in .ini File');
    end;
  GFRE_SC.SetListenerCB(@NewListener);
  GFRE_SC.SetNewChannelCB(@NewChannel);
  if FDBO_Srv_Cfg.SpecialFile<>'' then
     FListenerUX := GFRE_SC.AddDefaultGroupListenerUX(FDBO_Srv_Cfg.SpecialFile,'ux');
  if FDBO_Srv_Cfg.IP<>'' then
    GFRE_SC.AddDefaultGroupListenerTCP(FDBO_Srv_Cfg.IP,FDBO_Srv_Cfg.Port,'tcp');
end;


procedure TFRE_DBO_SERVER.PushDataToClients(const data_object: IFRE_DB_Object);
var mem : pointer;
    siz : Cardinal;
    i : Nativeint;
begin
//  GFRE_DBI.LogDebug(dblc_APPLICATION,'PushDataToClients[%s]',[data_object.DumpToString()]);
  siz := FREDB_GetDboAsBufferLen(data_object,mem); {TODO LZ4 Compress}
  try
    data_object.Finalize;
    FLock.Acquire;
    try
      for i:=0 to FChannelList.Count-1 do
        IFRE_APSC_CHANNEL(FChannelList[i]).cs_WriteBuffer(mem,siz);
    finally
      FLock.Release;
    end;

  finally
    Freemem(mem);
  end;
end;

procedure TFRE_DBO_SERVER.ForAllChannels(const chan_iter: TFRE_APSC_CHANNEL_EVENT_NESTED);
var
  i: NativeInt;
begin
  FLock.Acquire;
  try
    for i:=0 to FChannelList.Count-1 do
      chan_iter(IFRE_APSC_CHANNEL(FChannelList[i]));
  finally
    FLock.Release;
  end;
end;

end.

