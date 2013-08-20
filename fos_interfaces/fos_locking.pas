unit fos_locking;

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

{$IFDEF FPC}
 {$MODE DELPHI}{$H+}
{$ENDIF}

interface
uses Sysutils,SyncObjs,
     {$IFNDEF FPC}
      windows,
     {$ENDIF}
     FOS_TOOL_INTERFACES;

type
  TFOS_E=class;

  { TFOS_LOCK }

  TFOS_LOCK=class(TObject,IFOS_LOCK)
   private
    CS:SyncObjs.TCriticalSection;
   public
    constructor Create;
    destructor  Destroy;override;
    procedure   Acquire;
    procedure   Release;
    procedure   Finalize;
  end;

  TFOS_RW_LOCK=class(TInterfacedObject,IFOS_RW_LOCK) // If you lock multiple -> you are Dead locked
  private
   rc,wc:integer;
   x,y,z,wsem,rsem:TFOS_E; // Stallings (Reader/Writer Problem)
  public
   constructor Create;
   destructor  Destroy;override;
   procedure   AcquireRead;
   procedure   ReleaseRead;
   procedure   AcquireWrite;
   procedure   ReleaseWrite;
  end;

  TFOS_TE=class(TInterfacedObject,IFOS_TE) 
  private
 {$IFNDEF FPC}
   FSig:TEvent;
 {$ELSE}
   FSig:PRTLEvent;
 {$ENDIF}
  public
   constructor Create;
   destructor  Destroy; override;
   procedure   WaitFor(timeout:integer);
   function    SetEvent:boolean;
  end;

  TFOS_DATA_TE=class(TFOS_TE,IFOS_DATA_TE)
  private
   FData:String;
   FData2:String;
   FData3:String;
   FData4:String;
  public
   procedure   SetData(const data:string);
   procedure   SetData2(const data:string);
   procedure   SetData3(const data:string);
   procedure   SetData4(const data:string);
   function    GetData:string;
   function    GetData2:string;
   function    GetData3:string;
   function    GetData4:string;
  end;

 { TFOS_E }

 TFOS_E=class(TInterfacedObject,IFOS_E)
 private
 {$IFDEF FPC}
   FSig:PRTLEvent;
 {$ELSE}
   FHandle:THandle;
 {$ENDIF}
  public
 public
   constructor Create;
   destructor  Destroy;override;
   procedure   WaitFor;
   procedure   SetEvent;
   procedure   Finalize;
 end;


implementation

{ TFOS_TE }

constructor TFOS_TE.Create;
begin
 inherited;
 {$IFDEF FPC}
   FSig:=RTLEventCreate;
 {$ELSE}
   FSig:=TEvent.Create(nil,false,false,'');
 {$ENDIF}
end;


destructor TFOS_TE.Destroy;
begin
 {$IFDEF FPC}
   RTLeventdestroy(FSig);
 {$ELSE}
   FSIG.Free;
 {$ENDIF}
 inherited;
end;

function TFOS_TE.SetEvent: boolean;
begin
  result:=true;
 {$IFDEF FPC}
   RTLeventSetEvent(FSig);
 {$ELSE}
   FSig.SetEvent;
 {$ENDIF}
end;

procedure TFOS_TE.WaitFor(timeout: integer);
{$IFNDEF FPC}
 var wr:TWaitResult;
{$ENDIF}
begin
 {$IFDEF FPC}
  if timeout=0 then begin
    RTLeventWaitFor(FSig);
  end else begin
    RTLeventWaitFor(FSig,timeout);
  end;
 {$ELSE}
  wr:=FSig.WaitFor(timeout);
  if wr=wrError then begin
   raise Exception.Create('SIGNAL FAILED WITH CODE '+inttostr(FSig.LastError));
  end else begin
   raise Exception.Create('SIGNAL ABANDONED');
  end;
 {$ENDIF}
end;

{ TFOS_E }

constructor TFOS_E.Create;
begin
 {$IFDEF FPC}
  FSig := RTLEventCreate;
 {$ELSE}
  FHandle :=CreateEvent(nil,false,false,'');
 {$ENDIF}
end;

destructor TFOS_E.Destroy;
begin
 {$IFDEF FPC}
   RTLeventdestroy(FSig);
 {$ELSE}
   CloseHandle(FHandle);
 {$ENDIF}
  inherited Destroy;
end;


procedure TFOS_E.SetEvent;
begin
  {$IFDEF FPC}
   RTLeventSetEvent(FSig);
  {$ELSE}
    windows.setevent(fhandle);
  {$ENDIF}
end;

procedure TFOS_E.Finalize;
begin
  free;
end;

procedure TFOS_E.WaitFor;
begin
 {$IFDEF FPC}
   RTLeventWaitFor(FSig);
 {$ELSE}
   WaitForSingleObject(FHandle,INFINITE);
 {$ENDIF}
end;

{ TFOS_RW_LOCK }

procedure TFOS_RW_LOCK.AcquireRead;
begin
 z.WaitFor;
  rsem.WaitFor;
   x.WaitFor;
    inc(RC);
    if rc=1 then begin
     wsem.WaitFor;
    end;
   x.SetEvent;
  rsem.SetEvent;
 z.SetEvent;
end;

procedure TFOS_RW_LOCK.AcquireWrite;
begin
  y.WaitFor;
   inc(wc);
   if wc=1 then begin
    rsem.WaitFor;
   end;
  y.SetEvent;
  wsem.WaitFor;
end;

constructor TFOS_RW_LOCK.Create;
begin
 x:=TFOS_E.Create;
 y:=TFOS_E.Create;
 z:=TFOS_E.Create;
 RSEM:=TFOS_E.Create;
 WSEM:=TFOS_E.Create;
 WC:=0;
 RC:=0;
end;

destructor TFOS_RW_LOCK.Destroy;
begin
 x.Free;
 y.Free;
 z.free;
 rsem.Free;
 wsem.free;
 inherited;
end;

procedure TFOS_RW_LOCK.ReleaseRead;
begin
  x.WaitFor;
   dec(rc);
   if rc=0 then begin
     wsem.SetEvent;
   end;
  x.SetEvent;
end;

procedure TFOS_RW_LOCK.ReleaseWrite;
begin
  wsem.SetEvent;
  y.WaitFor;
   dec(wc);
   if wc=0 then begin
    rsem.SetEvent;
   end;
   y.SetEvent;
end;

{ TFOS_LOCK }

procedure TFOS_LOCK.Acquire;
begin
 CS.Acquire;
end;

constructor TFOS_LOCK.Create;
begin
 CS:=SyncObjs.TCriticalSection.Create;
end;

destructor TFOS_LOCK.Destroy;
begin
 CS.Free;
 inherited;
end;

procedure TFOS_LOCK.Release;
begin
 if assigned(CS) then begin
  try
   CS.Release;
  except
   CS:=nil;
  end;
 end else begin
  CS:=nil;
 end;
end;

procedure TFOS_LOCK.Finalize;
begin
  Free;
end;

{ TFOS_DATA_TE }

function TFOS_DATA_TE.GetData: string;
begin
 result:=FData;
end;

function TFOS_DATA_TE.GetData2: string;
begin
 result:=FData2;
end;

function TFOS_DATA_TE.GetData3: string;
begin
 result:=FData3;
end;

function TFOS_DATA_TE.GetData4: string;
begin
 result:=FData4;
end;

procedure TFOS_DATA_TE.SetData(const data: string);
begin
 FData:=Data;
end;

procedure TFOS_DATA_TE.SetData2(const data: string);
begin
 FData2:=Data;
end;

procedure TFOS_DATA_TE.SetData3(const data: string);
begin
 FData3:=Data;
end;

procedure TFOS_DATA_TE.SetData4(const data: string);
begin
 FData4:=Data;
end;

end.

