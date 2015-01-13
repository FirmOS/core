unit fre_basefeed_app;

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
  Classes, SysUtils,
  CustApp,
  FRE_SYSTEM,FOS_DEFAULT_IMPLEMENTATION,FOS_TOOL_INTERFACES,FOS_FCOM_TYPES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,
  FRE_DB_CORE,fre_aps_comm_impl,fre_dbbase,
  //FRE_DB_EMBEDDED_IMPL,
  FRE_CONFIGURATION,
  fre_base_client
   ;

type


  { TFRE_BASEDATA_FEED }

  TFRE_BASEDATA_FEED = class(TCustomApplication)
  protected
    FBaseClient : TFRE_BASE_CLIENT;
    procedure   DoRun; override;
  public
    procedure   MyRunMethod;virtual;
    procedure   MyRegisterClasses;virtual;
    constructor Create  (TheOwner: TComponent;const client : TFRE_BASE_CLIENT);reintroduce;
    procedure   WriteHelp; virtual;
    procedure   WriteVersion; virtual;
    procedure   TestMethod; virtual;
    procedure   CfgTestLog;
  end;


implementation


{ TFRE_BASEDATA_FEED }

procedure TFRE_BASEDATA_FEED.DoRun;
var
  ErrorMsg   : String;
begin
  ErrorMsg:=CheckOptions('thvDU:H:u:p:',['test','help','version','debugger','remoteuser:','remotehost:','user:','pass:','testlog','subfeederip:','mwsip:','machine:','mac:']);
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

  if HasOption('*','subfeederip') then
    cFRE_SUBFEEDER_IP := GetOptionValue('*','subfeederip');

  if HasOption('*','mwsip') then
    cFRE_MWS_IP := GetOptionValue('*','mwsip');

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

  if HasOption('*','machine') then
    cFRE_MACHINE_NAME := GetOptionValue('*','machine');
  if HasOption('*','mac') then
    cFRE_MACHINE_MAC := GetOptionValue('*','mac');

  if HasOption('H','remotehost') then begin
    cFRE_REMOTE_HOST:= GetOptionValue('H','remotehost');
  end else begin
    cFRE_REMOTE_HOST:= '127.0.0.1';
  end;

  Initialize_Read_FRE_CFG_Parameter;

  if HasOption('*','testlog') then
    begin
      writeln('configuring testlogging');
      CfgTestLog;
    end;

  //InitEmbedded;
  InitMinimal();
  Init4Server;
  fre_dbbase.Register_DB_Extensions;
  GFRE_DBI.SetLocalZone('Europe/Vienna');
  Setup_APS_Comm;
  MyRegisterClasses;
  FBaseClient.Setup;
  if HasOption('t','test') then begin
    TestMethod;
  end;
  MyRunMethod;
  Teardown_APS_Comm;
  FBaseClient.Free;
  GFRE_DB_PS_LAYER.Finalize;
  Terminate;
end;

procedure TFRE_BASEDATA_FEED.MyRunMethod;
begin
  GFRE_SC.RunUntilTerminate;
end;

procedure TFRE_BASEDATA_FEED.MyRegisterClasses;
begin
  FBaseClient.MyRegisterClasses;
  GFRE_DB.Initialize_Extension_ObjectsBuild;
end;


constructor TFRE_BASEDATA_FEED.Create(TheOwner: TComponent; const client: TFRE_BASE_CLIENT);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FBaseClient := client;
end;


procedure TFRE_BASEDATA_FEED.WriteHelp;
begin
   { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('  -U            | --remoteuser             : user for remote commands');
  writeln('  -H            | --remotehost             : host for remote commands');
  writeln('  -h            | --help                   : print this help');
  writeln('  -v            | --version                : print version info');
  writeln('  -u <user>     | --user=<user>            : specify user');
  writeln('  -p <password> | --pass=<password>        : specify password');
  writeln('                | --machine=<MachineName>  : specify machine id of this feeder same as NAME entry in [MACHINE] section in ini file');
  writeln('                | --mac=<00:11:22:33:44:55>: specify machine id of this feeder same as MAC entry in [MACHINE] section in ini file');
  writeln('                  --debugger               : set debug flag');
  writeln('                  --testlog                : enable console test-log');
  writeln('                  --subfeederip=<ip>       : connect to subfeeder via ip');
  writeln('                  --mwsip=<ip>             : connect to mws via ip');
  writeln;
end;

procedure TFRE_BASEDATA_FEED.WriteVersion;
begin
end;

procedure TFRE_BASEDATA_FEED.TestMethod;
begin

end;

procedure TFRE_BASEDATA_FEED.CfgTestLog;
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

