unit fos_tool_factory;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

{$MODE objfpc} {$H+}

interface


uses Classes,SysUtils,FOS_TOOL_INTERFACES,FOS_NPS;

var    {$IFDEF WIN32}
        cIniFilename:string='fss.ini';
       {$ELSE}
        cIniFilename:string='fss.ini';
       {$ENDIF}
       {$IFDEF WIN32}
         cDefaultIniDir:string='v:\fossip\';
       {$ELSE}
         cDefaultIniDir:string='/etc/opt/firmos/';
       {$ENDIF}

type

 { TFOS_TOOL_FACTORY }
 TFOS_TOOL_FACTORY=class(TInterfacedObject,IFOS_TOOL_FACTORY)
 private
  public
    constructor  Create;
    destructor   Destroy;override;

    procedure    Get_Event(out E:IFOS_E);
    procedure    Get_TimedEvent(out TE:IFOS_TE);
    procedure    Get_TimedDataEvent(out TE:IFOS_DATA_TE);
    procedure    Get_Lock(out LOCK:IFOS_LOCK;const with_timing : boolean=false);
    procedure    Get_RW_Lock(out RWL:IFOS_RW_LOCK);
    procedure    Get_LFQ(out LFQ:IFOS_LFQ);
    procedure    Get_NPS(out NPS:IFOS_NPS);
    function     Get_FOS_Strings:IFOS_STRINGS;overload;
    function     GetDefaultIniFileName:String;
    function     ReadDefaultIniValue(const section,value,default:string):String;
 end;


implementation

uses FOS_LOCKING,FOS_LFQ,FOS_DEFAULT_STRINGS,
//     FOS_BASIS_TOOLS,// must be in INTERFACE section otherwise crash ...
//     FOS_JOBSHEDULER,
     inifiles;

{ TFOS_TOOL_FACTORY }


constructor TFOS_TOOL_FACTORY.Create;
begin
 inherited;
end;

destructor TFOS_TOOL_FACTORY.Destroy;
begin
 inherited;
end;

{
procedure TFOS_TOOL_FACTORY.Get_Application_Syncer(const DM: MDeliveryMethod;const Parent:TObject;
                                                         out APS: IFOS_Application_Syncer);
begin
 if not assigned(GFRE_AS) then raise exception.Create('no appsyncer plugin availlable');
 GFRE_AS(DM,Parent,APS);
end;
}

{
procedure TFOS_TOOL_FACTORY.Get_JobScheduler(out js: IFOS_Jobscheduler);
begin
 js:=TFOS_JobScheduler.Create;
end;
}

procedure TFOS_TOOL_FACTORY.Get_Event(out E: IFOS_E);
begin
 E:=TFOS_E.Create;
end;



function TFOS_TOOL_FACTORY.GetDefaultIniFileName: String;
begin
 if FileExists(cIniFileName) then begin
  result:=cIniFilename;
 end else begin
  result:=cDefaultIniDir+cIniFilename;
 end;
end;


procedure TFOS_TOOL_FACTORY.Get_LFQ(out LFQ: IFOS_LFQ);
begin
 LFQ:=TFOS_LFQ.Create;
end;

procedure TFOS_TOOL_FACTORY.Get_NPS(out NPS: IFOS_NPS);
begin
 NPS:=Get_IFOS_NPS;
end;


{
procedure TFOS_TOOL_FACTORY.Get_ThreadedWorker(const Threads: Byte;const ID: String; out FTW: IFOS_THREADED_WORKER);
begin
 FTW:=Get_FOS_NG_WORKER(Threads,Id);
end;
}
procedure TFOS_TOOL_FACTORY.Get_TimedDataEvent(out TE: IFOS_DATA_TE);
begin
 TE:=TFOS_DATA_TE.Create;
end;

procedure TFOS_TOOL_FACTORY.Get_Lock(out LOCK: IFOS_LOCK; const with_timing: boolean);
begin
 if with_timing then
   LOCK:=TFOS_TIMED_LOCK.Create
 else
   LOCK:=TFOS_LOCK.Create;
end;

procedure TFOS_TOOL_FACTORY.Get_RW_Lock(out RWL: IFOS_RW_LOCK);
begin
 RWL:=TFOS_RW_LOCK.Create;
end;

procedure TFOS_TOOL_FACTORY.Get_TimedEvent(out TE: IFOS_TE);
begin
 TE:=TFOS_TE.Create;
end;

function TFOS_TOOL_FACTORY.ReadDefaultIniValue(const section, value, default: string): String;
var fn:string;
    ini:TMemIniFile;
begin
 inherited;
 fn:=GetDefaultIniFileName;
 INI:=TMemIniFile.Create(fn);
 try
  result:=INI.ReadString(section,value,default);
 finally
  Ini.Free;
 end;
end;

function TFOS_TOOL_FACTORY.Get_FOS_Strings: IFOS_STRINGS;
begin
 result:=Get_FOS_DefaultStrings;
end;


initialization

finalization

end.
