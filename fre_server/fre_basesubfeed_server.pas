unit fre_basesubfeed_server;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,FRE_APS_INTERFACE,FOS_FCOM_TYPES,FRE_SYS_BASE_CS,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,FOS_INTERLOCKED,baseunix;

type
  RSUB_FEEDER_CFG=record
    Id          : Shortstring; // Hello ID OF Subfeeder;
    SpecialFile : Shortstring; // if set open an unix socket
    IP,Port     : String; //IF set open an TCP Listener;
  end;

  { TFRE_BASESUBFEED_SERVER }

  TFRE_BASESUBFEED_SERVER=class(TFRE_DB_Base)
  private
    FLock        : IFOS_LOCK;
    FChannelList : TList;
    FListenerUX  : IFRE_APSC_LISTENER;
    FListenerTCP : IFRE_APSC_LISTENER;
    procedure NewListener  (const new_listener : IFRE_APSC_LISTENER ; const state : TAPSC_ListenerState);
    procedure NewChannel   (const channel      : IFRE_APSC_CHANNEL ; const channel_event : TAPSC_ChannelState);
    procedure DiscoChannel (const channel      : IFRE_APSC_CHANNEL);
    procedure ReadChannel  (const channel      : IFRE_APSC_CHANNEL);
    function  GetDboAsBufferLen(const dbo: IFRE_DB_Object; var mem: Pointer): UInt32;
  protected
    FCfg         : RSUB_FEEDER_CFG;
  public
    constructor Create;
    destructor  Destroy  ; override ;
    procedure   Setup; virtual;
    procedure   PushDataToClients(const data_object : IFRE_DB_Object);virtual;
  end;

implementation

{ TFRE_BASESUBFEED_SERVER }

procedure TFRE_BASESUBFEED_SERVER.NewListener(const new_listener: IFRE_APSC_LISTENER; const state: TAPSC_ListenerState);
var err :string;
begin
  err := new_listener.GetErrorString;
  writeln('LISTENER STATE ',new_listener.Getstate,' ',new_listener.GetListeningAddress,' ',state,' ',err);
  if state =als_EVENT_NEW_LISTENER then
    begin
      if new_listener.GetID='subux' then
        begin
          FListenerUX:=new_listener;
          new_listener.Start;
        end
      else
      if new_listener.GetID='subtcp' then
        begin
          FListenerTCP:=new_listener;
          new_listener.Start;
        end
      else
        GFRE_BT.CriticalAbort('unsupported/unknown listenerid '+new_listener.GetID);
    end;
end;

procedure TFRE_BASESUBFEED_SERVER.NewChannel(const channel: IFRE_APSC_CHANNEL; const channel_event: TAPSC_ChannelState);
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
      writeln('CHANNEL CONNECT ON MGR ',channel.GetChannelManager.GetID,' via LISTENR ',channel.GetListener.GetListeningAddress,' PARTNER=',channel.GetConnSocketAddr);
      dbo := GFRE_DBI.NewObject;
      dbo.Field('SUBFEEDER_ID').AsString:=FCfg.Id;
      siz := GetDboAsBufferLen(dbo,mem);
      dbo.Finalize;
      channel.CH_WriteBuffer(mem,siz);
      Freemem(mem);
      channel.CH_Enable_Reading;
      channel.SetOnDisconnnect(@DiscoChannel);
      channel.SetOnReadData(@ReadChannel);
      FLock.Acquire;
      try
        if FChannelList.IndexOf(channel)<>-1 then
          GFRE_BT.CriticalAbort('channel double add?');
        FChannelList.Add(channel);
      finally
        FLock.Release;
      end;
    end;
end;

procedure TFRE_BASESUBFEED_SERVER.DiscoChannel(const channel: IFRE_APSC_CHANNEL);
begin
  FLock.Acquire;
  try
    writeln('CHANNEL DISCO ',channel.GetVerboseDesc);
    if FChannelList.Remove(channel)=-1 then
      GFRE_BT.CriticalAbort('unexpected : channel not found');
  finally
    FLock.Release;
  end;
end;

procedure TFRE_BASESUBFEED_SERVER.ReadChannel(const channel: IFRE_APSC_CHANNEL);
begin
  writeln('!!READ UNSUPPORTED');
  GFRE_BT.CriticalAbort('DONT WRITE SOMETHING TO ME');
end;

function TFRE_BASESUBFEED_SERVER.GetDboAsBufferLen(const dbo: IFRE_DB_Object ; var mem : Pointer):UInt32;
var len : UInt32;
    ns  : UInt32;
begin
  ns := dbo.NeededSize;
  Getmem(mem,ns+4);
  dbo.CopyToMemory(mem+4);
  PCardinal(mem)^:=ns;
  result := ns+4;
end;

constructor TFRE_BASESUBFEED_SERVER.Create;
begin
  inherited;
  GFRE_TF.Get_Lock(FLock);
  FChannelList := TList.Create;
end;

destructor TFRE_BASESUBFEED_SERVER.Destroy;
begin
  if assigned(FLock) then
    FLock.Finalize;
  FChannelList.Free;
  inherited Destroy;
end;

procedure TFRE_BASESUBFEED_SERVER.Setup;
begin
  GFRE_SC.SetNewListenerCB(@NewListener);
  GFRE_SC.SetNewChannelCB(@NewChannel);
  if FCfg.SpecialFile<>'' then
    GFRE_SC.AddListener_UX(FCfg.SpecialFile,'subux');
  if FCfg.IP<>'' then
    GFRE_SC.AddListener_TCP(FCfg.IP,FCfg.Port,'subtcp');
end;


procedure TFRE_BASESUBFEED_SERVER.PushDataToClients(const data_object: IFRE_DB_Object);
var mem : pointer;
    siz : Cardinal;
    i : Nativeint;
begin
  siz := GetDboAsBufferLen(data_object,mem);
  data_object.Finalize;
  FLock.Acquire;
  try
    for i:=0 to FChannelList.Count-1 do
      IFRE_APSC_CHANNEL(FChannelList[i]).CH_SAFE_WriteBuffer(mem,siz);
  finally
    FLock.Release;
  end;
end;

end.

