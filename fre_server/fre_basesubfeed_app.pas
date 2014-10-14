unit fre_basesubfeed_app;

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
  Classes, SysUtils, CustApp,
  FRE_SYSTEM,FOS_DEFAULT_IMPLEMENTATION,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,
  FRE_DB_CORE,fre_aps_comm_impl,
  FRE_DB_EMBEDDED_IMPL,
  FRE_CONFIGURATION,
  fre_basedbo_server,ctypes
  ;

type


  { TFRE_BASESUBDATA_FEED }

  TFRE_BASESUBDATA_FEED = class(TCustomApplication)
  protected
    FSFServer : TFRE_DBO_SERVER;
    procedure   DoRun; override;
  public
    procedure   MyRunMethod ; virtual;
    constructor Create      (TheOwner: TComponent ; const Server : TFRE_DBO_SERVER) ; reintroduce ;
    destructor  Destroy     ;override;
    procedure   WriteHelp   ; virtual;
    procedure   WriteVersion; virtual;
    procedure   TestMethod  ; virtual;
    procedure   CfgTestLog  ;
  end;

  {$ifdef netbsd}
    { NetBSD has a new setlocale function defined in /usr/include/locale.h
      that should be used }
  function fos_setlocale(category: cint; locale: pchar): pchar; cdecl; external 'c' name '__setlocale_mb_len_max_32';
  {$else}
  function fos_setlocale(category: cint; locale: pchar): pchar; cdecl; external 'c' name 'setlocale';
  {$endif}

implementation

const
 __LC_CTYPE    = 0;
 __LC_NUMERIC  = 1;
 __LC_TIME     = 2;
 __LC_COLLATE  = 3;
 __LC_MONETARY = 4;
 __LC_MESSAGES = 5;
 __LC_ALL      = 6;

{ TFRE_BASESUBDATA_FEED }

procedure TFRE_BASESUBDATA_FEED.DoRun;
var
  ErrorMsg   : String;
  loc        : String;
begin
  loc := fos_setlocale(__LC_ALL,'C');
  ErrorMsg:=CheckOptions('thvDU:H:T:',['test','help','version','debugger','remoteuser:','remotehost:','toolpath:','test-log']);
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

  if HasOption('v','version') then begin
    WriteVersion;
    Terminate;
    Exit;
  end;

  if HasOption('D','debugger') then
    G_NO_INTERRUPT_FLAG:=true;

  if HasOption('U','remoteuser') then begin
    cFRE_REMOTE_USER := GetOptionValue('U','remoteuser');
  end;

  if HasOption('H','remotehost') then begin
    cFRE_REMOTE_HOST:= GetOptionValue('H','remotehost');
  end else begin
    cFRE_REMOTE_HOST:= '';
  end;

  if HasOption('T','toolpath') then begin
    cFRE_ToolsPath:= GetOptionValue('T','toolpath');
  end;

  Initialize_Read_FRE_CFG_Parameter;
  if HasOption('*','test-log') then
    begin
      writeln('configuring testlogging');
      CfgTestLog;
    end;
  InitEmbedded;
  Init4Server;
  GFRE_DBI.SetLocalZone('Europe/Vienna');
  Setup_APS_Comm;
  FSFServer.Setup;
  if HasOption('t','test') then begin
    TestMethod;
  end;
  MyRunMethod;
  Teardown_APS_Comm;
  FSFServer.Free;
  GFRE_DB_PS_LAYER.Finalize;
  Terminate;
end;

procedure TFRE_BASESUBDATA_FEED.MyRunMethod;
begin
  GFRE_SC.RunUntilTerminate;
end;


constructor TFRE_BASESUBDATA_FEED.Create(TheOwner: TComponent; const Server: TFRE_DBO_SERVER);
//var s:string;
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FSFServer       := Server;
  //s := GFRE_BT.StringFromFile('/home/fosbuild/repos/testjson.json');
  //GFRE_DB.JSONObject2Object(s,false);
  //TFRE_DB_Object.CreateFromJSONString(s);
  //writeln();
end;

destructor TFRE_BASESUBDATA_FEED.Destroy;
begin
  inherited Destroy;
end;


procedure TFRE_BASESUBDATA_FEED.WriteHelp;
begin
   { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('  -U            | --remoteuser           : user for remote commands');
  writeln('  -H            | --remotehost           : host for remote commands');
  writeln('  -h            | --help                 : print this help');
  writeln('  -v            | --version              : print version info');
  writeln('  -T            | --toolpath             : base path for tools e.g. sg3utils');
  writeln('                  --debugger             : set debug flag');
  writeln('                  --test-log             : enable console test-log');
end;

procedure TFRE_BASESUBDATA_FEED.WriteVersion;
begin
end;

procedure TFRE_BASESUBDATA_FEED.TestMethod;
begin

end;

procedure TFRE_BASESUBDATA_FEED.CfgTestLog;
begin
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry);  // Server / Connection Start/Close
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTPSRV],fll_Info,'*',flra_DropEntry); // Http/Header / Content
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTPSRV],fll_Debug,'*',flra_DropEntry); // Http/Header / Content
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry); // Server / Dispatch / Input Output
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WEBSOCK],fll_Debug,'*',flra_DropEntry); // Websock / JSON / IN / OUT
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Debug,'*',flra_DropEntry); // Persistance Layer Debugging
  //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Debug,'*',flra_DropEntry); // Database /Filter / Layer Debugging
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_LogToOnConsole,false); // All To Console
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_DropEntry); // No File  Logging
  GFRE_LOG.DisableSyslog;
  GFRE_LOG.Log('TESTENTRY','START');
end;

end.

