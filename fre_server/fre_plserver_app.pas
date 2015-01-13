unit fre_plserver_app;

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
  FRE_DB_PERSISTANCE_FS_SIMPLE,
  FRE_CONFIGURATION,
  fre_basedbo_server,
  fre_basecli_app
  ;

type


  { TFRE_PLSERVER_APP }

  TFRE_PLSERVER_APP = class(TFRE_CLISRV_APP)
  protected
    FSFServer : TFRE_DBO_SERVER;
    procedure   DoRun; override;
  public
    procedure   MyRunMethod ; virtual;
    constructor Create      (TheOwner: TComponent ; const Server : TFRE_DBO_SERVER) ; reintroduce ;
    destructor  Destroy     ;override;
    procedure   WriteHelp   ; virtual;
    procedure   TestMethod  ; virtual;
    procedure   CfgTestLog  ; override;
  end;


implementation


{ TFRE_PLSERVER_APP }

procedure TFRE_PLSERVER_APP.DoRun;
var
  ErrorMsg   : String;
begin
  ErrorMsg:=CheckOptions('hv',['help','version','testlog']);
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

  Initialize_Read_FRE_CFG_Parameter;
  if HasOption('*','testlog') then
    begin
      writeln('configuring testlogging');
      CfgTestLog;
    end;

  //GDISABLE_WAL              := TRUE;
  //GDBPS_TRANS_WRITE_THROUGH := TRUE;
  //GDISABLE_SYNC             := TRUE;
  //GDROP_WAL                 := TRUE;

  //InitEmbedded;
  GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'db');
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
  //GFRE_DB_PS_LAYER.SyncSnapshot(true);
  GFRE_DB_PS_LAYER.Finalize;
  Terminate;
end;

procedure TFRE_PLSERVER_APP.MyRunMethod;
begin
  GFRE_SC.RunUntilTerminate;
end;


constructor TFRE_PLSERVER_APP.Create(TheOwner: TComponent; const Server: TFRE_DBO_SERVER);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FSFServer       := Server;
end;

destructor TFRE_PLSERVER_APP.Destroy;
begin
  inherited Destroy;
end;


procedure TFRE_PLSERVER_APP.WriteHelp;
begin
   { add your help code here }
  writeln('Usage: ',ExeName,' -h');
  writeln('  -h            | --help                 : print this help');
  writeln('  -v            | --version              : print version info');
  writeln('                  --testlog              : enable console test-log');
end;


procedure TFRE_PLSERVER_APP.TestMethod;
begin

end;

procedure TFRE_PLSERVER_APP.CfgTestLog;

    procedure Setup_HTTP_Request_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_ZIP],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_CACHE],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_REQ],fll_Info,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_REQ],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_RES],fll_Info,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_HTTP_RES],fll_Debug,'*',flra_DropEntry);
    end;

    procedure Setup_DB_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_DB],fll_Warning,'*',flra_DropEntry);
    end;

    procedure Setup_Server_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Debug,'*',flra_DropEntry);     // DROP : Server / DEBUG
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER_DATA],fll_Debug,'*',flra_DropEntry);// DROP : Server / Dispatch / Input Output
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Info,'*',flra_DropEntry);      // DROP : Server / INFO
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SERVER],fll_Notice,'*',flra_DropEntry);    // DROP : Server / NOTICE
    end;

    procedure Setup_WS_Session_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WEBSOCK],fll_Debug,'*',flra_DropEntry);    // DROP : Websock / JSON / IN / OUT
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_WS_JSON],fll_Debug,'*',flra_DropEntry);    // DROP : JSON
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SESSION],fll_Debug,'*',flra_DropEntry);    // DROP SESSION  DEBUG
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_SESSION],fll_Info,'*',flra_DropEntry);     // DROP SESSION INFO
    end;

    procedure Setup_APS_COMM_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Info,'*',flra_DropEntry);     // DROP APSCOMM INFO
      GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Debug,'*',flra_DropEntry);    // DROP APSCOMM DEBUG
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_APSCOMM],fll_Notice,'*',flra_DropEntry);
    end;

    procedure Setup_Persistance_Layer_Logging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Info,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE_NOTIFY],fll_Info,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_PERSISTANCE_NOTIFY],fll_Debug,'*',flra_DropEntry);
    end;

    procedure Setup_FlexcomLogging;
    begin
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_FLEXCOM],fll_Debug,'*',flra_DropEntry);
      //GFRE_Log.AddRule(CFRE_DB_LOGCATEGORY[dblc_FLEX_IO],fll_Debug,'*',flra_DropEntry);
    end;

begin
  Setup_DB_Logging;
  Setup_HTTP_Request_Logging;
  Setup_Server_Logging;
  Setup_WS_Session_Logging;
  Setup_APS_COMM_Logging;
  Setup_Persistance_Layer_Logging;
  Setup_FlexcomLogging;
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_LogToOnConsole,false); // All To Console
  GFRE_Log.AddRule('*',fll_Invalid,'*',flra_DropEntry); // No File  Logging
  GFRE_LOG.DisableSyslog;
end;



end.

