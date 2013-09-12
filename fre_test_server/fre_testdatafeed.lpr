program fre_testdatafeed;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
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

{$mode objfpc}{$H+}
{$LIBRARYPATH ../../lib}


// ./fre_testdatafeed -U root -H 10.220.251.10 -u feeder@system -p a1234
// lazarus+debugger: => ./fre_testdatafeed -U root -H 10.220.251.10 -D

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  FRE_SYSTEM,FOS_DEFAULT_IMPLEMENTATION,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,
  FRE_DB_CORE,

  FRE_APS_IMPL_LE, FOS_FCOM_DEFAULT, FRE_DB_EMBEDDED_IMPL,
  FRE_CONFIGURATION,FRE_BASE_SERVER,
  fre_base_client, fre_feed_client
  ;

{$I fos_version_helper.inc}

type

  { TFRE_TESTDATA_FEED }

  TFRE_TESTDATA_FEED = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;

{ TFRE_TESTDATA_FEED }

procedure TFRE_TESTDATA_FEED.DoRun;
var
  ErrorMsg   : String;
  FeedClient : TFRE_SAMPLE_FEED_CLIENT;
begin
  ErrorMsg:=CheckOptions('hDU:H:u:p:',['help','debugger','remoteuser:','remotehost:','user:','pass:']);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('D','debugger') then
    G_NO_INTERRUPT_FLAG:=true;

  if HasOption('U','remoteuser') then begin
    cFRE_REMOTE_USER := GetOptionValue('U','remoteuser');
  end;

  if HasOption('u','user') then begin
    cFRE_Feed_User := GetOptionValue('u','user');
  end;

  if HasOption('p','pass') then begin
    cFRE_Feed_Pass := GetOptionValue('p','pass');
  end;

  if HasOption('H','remotehost') then begin
    cFRE_REMOTE_HOST:= GetOptionValue('H','remotehost');
  end else begin
    cFRE_REMOTE_HOST:= '127.0.0.1';
  end;

  Initialize_Read_FRE_CFG_Parameter;
  InitEmbedded;
  Init4Server;
  GFRE_DBI.SetLocalZone('Europe/Vienna');
  SetupAPS;
  FeedClient := TFRE_SAMPLE_FEED_CLIENT.Create;
  GFRE_S.Start(FeedClient);
  GFRE_S.Run;
  TearDownAPS;
  Shutdown_Done;
  FeedClient.Free;
  Terminate;
end;

constructor TFRE_TESTDATA_FEED.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TFRE_TESTDATA_FEED.Destroy;
begin
  inherited Destroy;
end;

procedure TFRE_TESTDATA_FEED.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('  -U            | --remoteuser           : user for remote commands');
  writeln('  -H            | --remotehost           : host for remote commands');
end;

var
  Application : TFRE_TESTDATA_FEED;
begin
  Application:=TFRE_TESTDATA_FEED.Create(nil);
  Application.Title:='FRE_Testdatafeed';
  Application.Run;
  Application.Free;
end.

