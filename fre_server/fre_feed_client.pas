unit fre_feed_client;

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
  Classes, SysUtils,fre_base_client,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE; // FOS_VM_CONTROL_INTERFACE,fos_stats_control_interface;

var
  cVM_HostUser   : string = '';
  cVMHostMachine : string = '';
  cFEEDUser      : string = '';
  cFEEDPass      : string = '';
//  cVMHostMachine : string = '10.001.000.102';
  //cVMHostMachine :string = '10.220.251.10';

type


  { TFRE_FEED_CLIENT }

  TFRE_FEED_CLIENT=class(TFRE_BASE_CLIENT)
  private
    FEED_Timer            : IFRE_APS_TIMER;
    FEED_Timer30          : IFRE_APS_TIMER;
    FVM_Feeding           : Boolean;
    FDISK_Feeding         : Boolean;
    FAPP_Feeding          : Boolean;
    FVM_FeedAppClass      : TFRE_DB_String;
    FVM_FeedAppUid        : TGuid;
    FSTORAGE_FeedAppClass : TFRE_DB_String;
    FSTORAGE_FeedAppUid   : TGuid;
    FAPPL_FeedAppClass    : TFRE_DB_String;
    FAPPL_FeedAppUid      : TGuid;
    //vmc                   : IFOS_VM_HOST_CONTROL; // Todo Move MVStats to Statscontroller
    //statscontroller       : IFOS_STATS_CONTROL;
  public
    procedure  MySessionEstablished    ; override;
    procedure  MySessionDisconnected   ; override;
    procedure  QueryUserPass           (out user, pass: string); override;
    procedure  MyInitialize            ; override;
    procedure  MyFinalize              ; override;
    procedure  GenerateFeedDataTimer   (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
    procedure  GenerateFeedDataTimer30 (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
  end;


implementation

procedure TFRE_FEED_CLIENT.MySessionEstablished;
begin
  if Get_AppClassAndUid('corebox_storage',FSTORAGE_FeedAppClass,FSTORAGE_FeedAppUid) then begin
    FDISK_Feeding := True;
  end;
  if Get_AppClassAndUid('COREBOX_APPLIANCE',FAPPL_FeedAppClass,FAPPL_FeedAppUid) then begin
    FAPP_Feeding := True;
  end;
  if Get_AppClassAndUid('COREBOX_VM',FVM_FeedAppClass,FVM_FeedAppUid) then begin
    FVM_Feeding   := True;
  end else begin
    GFRE_DBI.LogError(dblc_FLEXCOM,'FEEDiNG NOT POSSIBLE, COREBOX_VM APP NOT FOUND!');
  end;
end;

procedure TFRE_FEED_CLIENT.MySessionDisconnected;
begin
  FVM_Feeding   := false;
  FDISK_Feeding := false;
  FAPP_Feeding  := false;
end;

procedure TFRE_FEED_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := cFEEDUser;
  pass := cFEEDPass;
end;

procedure TFRE_FEED_CLIENT.MyInitialize;
begin
  FEED_Timer      := GFRE_S.AddPeriodicTimer (1000,@GenerateFeedDataTimer);
  FEED_Timer30    := GFRE_S.AddPeriodicTimer (30000,@GenerateFeedDataTimer30);
  //vmc             := Get_VM_Host_Control     (cVM_HostUser,cVMHostMachine);
  //statscontroller := Get_Stats_Control       (cVM_HostUser,cVMHostMachine);
  //vmc.VM_EnableVMMonitor                     (true);
  //statscontroller.StartDiskPersistentMonitorParser(true);
  //statscontroller.StartCPUParser(true);
  //statscontroller.StartRAMParser(true);
  //statscontroller.StartNetworkParser(true);
  //statscontroller.StartCacheParser(true);
  //statscontroller.StartZFSParser(true);
  //statscontroller.StartZpoolIostatParser(true);
end;
procedure TFRE_FEED_CLIENT.MyFinalize;
begin
  FEED_Timer.FinalizeIt;
  //vmc.Finalize ;
  //statscontroller.Finalize;
end;

var g_disc_delay : integer=0;

procedure TFRE_FEED_CLIENT.GenerateFeedDataTimer(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var vmo : IFRE_DB_Object;
begin
  //if FAPP_Feeding then
  //  begin
  //    try
  //      vmo := GFRE_DBI.NewObject;
  //      vmo.Field('LIVE STATUS FEED').AsString:='LSF_0.0.1';
  //      vmo.Field('TIMESTAMP').AsDateTimeUTC := GFRE_DT.Now_UTC;
  //      vmo.Field('CPU').AsObject := statscontroller.Get_CPU_Data;
  //      vmo.Field('VMSTAT').AsObject := statscontroller.Get_Ram_Data;
  //      vmo.Field('NET').AsObject := statscontroller.Get_Network_Data;
  //      vmo.Field('CACHE').AsObject := statscontroller.Get_CacheData;
  //      vmo.Field('DISK').AsObject := statscontroller.Get_Disk_Data;
  //      SendServerCommand(FAPPL_FeedAppClass,'RAW_DATA_FEED',TFRE_DB_GUIDArray.Create(FAPPL_FeedAppUid),vmo);
  //      writeln('LIVEUPDATE SENT! ' , GFRE_DT.Now_UTC);
  //    except on e:exception do begin
  //      writeln('FEED EXCEPTION : ',e.Message);
  //    end;end;
  //  end;
  //if FDISK_Feeding then
  //  begin
  //    try
  //      vmo := GFRE_DBI.NewObject;
  //      vmo.Field('DISK').AsObject := statscontroller.Get_Disk_Data;
  //      vmo.Field('ZPOOLIO').AsObject := statscontroller.Get_ZpoolIostat_Data;
  //      SendServerCommand(FSTORAGE_FeedAppClass,'RAW_DISK_FEED',TFRE_DB_GUIDArray.Create(FSTORAGE_FeedAppUid),vmo);
  //      writeln('DISK LIVEUPDATE SENT!');
  //    except on e:exception do begin
  //      writeln('SEND DISK FEED EXCEPTION : ',e.Message);
  //    end;end;
  //  end;
  //if FVM_Feeding then
  //  begin
  //    try
  //      SendServerCommand(FVM_FeedAppClass,'VM_FEED_UPDATE',TFRE_DB_GUIDArray.Create(FVM_FeedAppUid),vmc.Get_VM_Data);
  //      writeln('VM LIVEUPDATE SENT!');
  //    except on e:exception do begin
  //      writeln('VM LIVEUPDATE FEED EXCEPTION : ',e.Message);
  //    end;end;
  //  end;
end;

procedure TFRE_FEED_CLIENT.GenerateFeedDataTimer30(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var vmo : IFRE_DB_Object;
begin
  //if FAPP_Feeding then
  //  begin
  //    try
  //      vmo := GFRE_DBI.NewObject;
  //      vmo.Field('LIVE STATUS FEED 30').AsString:='LSF30_0.0.1';
  //      vmo.Field('ZFS').AsObject := statscontroller.Get_ZFS_Data_Once;
  //      SendServerCommand(FAPPL_FeedAppClass,'RAW_DATA_FEED30',TFRE_DB_GUIDArray.Create(FAPPL_FeedAppUid),vmo);
  //      writeln('LIVEUPDATE (30) SENT! ' , GFRE_DT.Now_UTC);
  //    except on e:exception do begin
  //      writeln('FEED EXCEPTION : ',e.Message);
  //    end;end;
  //  end;
end;

end.

