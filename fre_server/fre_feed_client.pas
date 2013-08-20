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
  Classes, SysUtils,fre_base_client,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE;

var
  cFEEDUser      : string = '';
  cFEEDPass      : string = '';

type


  { TFRE_SAMPLE_FEED_CLIENT }

  TFRE_SAMPLE_FEED_CLIENT=class(TFRE_BASE_CLIENT)
  private
    FEED_Timer            : IFRE_APS_TIMER;
    FFeeding              : Boolean;
    FFeedAppClass         : TFRE_DB_String;
    FFeedAppUid           : TGUid;
  public
    procedure  MySessionEstablished    ; override;
    procedure  MySessionDisconnected   ; override;
    procedure  QueryUserPass           (out user, pass: string); override;
    procedure  MyInitialize            ; override;
    procedure  MyFinalize              ; override;
    procedure  GenerateFeedDataTimer   (const ES:IFRE_APS_EVENTSOURCE;const TID:integer;const Data:Pointer;const cp:integer=0);
  end;


implementation

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionEstablished;
begin
  if Get_AppClassAndUid('testapp',FFeedAppClass,FFeedAppUid) then begin
    FFeeding := True;
  end;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MySessionDisconnected;
begin
  FFeeding   := false;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.QueryUserPass(out user, pass: string);
begin
  user := cFEEDUser;
  pass := cFEEDPass;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.MyInitialize;
begin
  FEED_Timer      := GFRE_S.AddPeriodicTimer (1000,@GenerateFeedDataTimer);
end;
procedure TFRE_SAMPLE_FEED_CLIENT.MyFinalize;
begin
  FEED_Timer.FinalizeIt;
end;

procedure TFRE_SAMPLE_FEED_CLIENT.GenerateFeedDataTimer(const ES: IFRE_APS_EVENTSOURCE; const TID: integer; const Data: Pointer; const cp: integer);
var vmo : IFRE_DB_Object;
begin
  if FFeeding then
    begin
      try
        vmo := GFRE_DBI.NewObject;
        vmo.Field('LIVE STATUS FEED').AsString := 'LSF_0.0.1';
        vmo.Field('TIMESTAMP').AsDateTimeUTC   := GFRE_DT.Now_UTC;
        vmo.Field('SAMPLE_VALUE').AsInt32      := Random(1000)-500;
        SendServerCommand(FFeedAppClass,'RAW_DATA_FEED',TFRE_DB_GUIDArray.Create(FFeedAppUid),vmo);
        writeln('LIVE UPDATE SENT! ' , GFRE_DT.Now_UTC);
      except on e:exception do begin
        writeln('FEED EXCEPTION : ',e.Message);
      end;end;
    end;
end;


end.
