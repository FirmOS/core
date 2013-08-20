unit fre_configuration;

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
  Classes, SysUtils,IniFiles,FRE_SYSTEM,FOS_TOOL_INTERFACES;

procedure Initialize_Read_FRE_CFG_Parameter;

implementation

// Read config file order:
// (1) .fre_ini in current dir
// (2) .fre_ini in global dir
// (3) .fre_ini in HOMEDIR/.firmos/.fre_ini

procedure Initialize_Read_FRE_CFG_Parameter;
var cfgfile  : string;
    userfile : string;

  procedure ReadConfigFile;
  var ini : TMemIniFile;

    procedure ConfigureLogging;
    var sl              : TStringList;
         i              : integer;
         target         : string;
         targetdir      : string;
         turnaround     : integer;
         generations    : integer;
         loglevel       : TFOS_LOG_LEVEL;
         facility       : TFOS_LOG_FACILITY;
         category       : string;
         no_log         : TFOS_BoolType;
         not_in_fulllog : TFOS_BoolType;
         filename       : string;
         rule           : string;
         logaction      : TFOS_LOG_RULE_ACTION;
         stoprule       : boolean;

    begin
      GFRE_Log.ClearLogRules;
      if ini.SectionExists('LOGGER') then begin
        if ini.ReadBool('LOGGER','SYSLOG',false) then begin
          GFRE_LOG.EnableSyslog;
        end;
        sl:=TStringList.Create;
        try
          sl.CommaText := ini.ReadString('LOGGER','TARGETS','');
          for i:=0 to sl.Count-1 do begin
            target      := Uppercase(trim(sl[i]));
            targetdir   := ini.ReadString ('LOGGER','TARGET_DIR_'+target,'');
            turnaround  := ini.ReadInteger('LOGGER','TARGET_TURNAROUND_'+target,-1);
            generations := ini.ReadInteger('LOGGER','TARGET_GENERATIONS_'+target,-1);
            loglevel    := FOSTI_StringToLogLevel    (ini.ReadString('LOGGER','TARGET_LOGLEVEL_'+target,'DEBUG'));
            facility    := FOSTI_StringToLogFacility (ini.ReadString('LOGGER','TARGET_FACILITY_'+target,'USER'));
            GFRE_LOG.RegisterTarget(target,targetdir,turnaround,generations,loglevel,facility);
          end;
          sl.CommaText := ini.ReadString('LOGGER','CATEGORIES','');
          for i:=0 to sl.Count-1 do begin
            category    := Uppercase(trim(sl[i]));
            filename    := ini.ReadString ('LOGGER','CATEGORY_FILENAME_'+category,'');
            turnaround  := ini.ReadInteger('LOGGER','CATEGORY_TURNAROUND_'+category,-1);
            generations := ini.ReadInteger('LOGGER','CATEGORY_GENERATIONS_'+category,-1);
            loglevel    := FOSTI_StringToLogLevel    (ini.ReadString('LOGGER','CATEGORY_LOGLEVEL_'+category,'DEBUG'));
            if ini.ValueExists('LOGGER','CATEGORY_NOLOG_'+category) then begin
              if ini.ReadBool ('LOGGER','CATEGORY_NOLOG_'+category,false) then begin
               no_log:=fbtTrue;
              end else begin
               no_log:=fbtFalse;
              end;
            end else begin
              no_log      := fbtNotSet;
            end;
            if ini.ValueExists('LOGGER','CATEGORY_NOTINFULLLOG_'+category) then begin
              if ini.ReadBool ('LOGGER','CATEGORY_NOTINFULLLOG_'+category,false) then begin
               not_in_fulllog:=fbtTrue;
              end else begin
               not_in_fulllog:=fbtFalse;
              end;
            end else begin
              not_in_fulllog :=fbtNotSet;
            end;
            GFRE_LOG.RegisterCategory(category,filename,turnaround,generations,loglevel,no_log,not_in_fulllog);
          end;
          sl.CommaText := ini.ReadString('LOGGER','RULES','');
          for i:=0 to sl.Count-1 do begin
            rule        := Uppercase(trim(sl[i]));
            category    := ini.ReadString ('LOGGER','RULE_CATEGORY_'+rule,'*');
            loglevel    := FOSTI_StringToLogLevel    (ini.ReadString('LOGGER','RULE_LOGLEVEL_'+rule,'INVALID'));
            target      := ini.ReadString ('LOGGER','RULE_TARGET_'+rule,'*');
            logaction   := FOSTI_StringToLogRuleAction   (ini.ReadString ('LOGGER','RULE_ACTION_'+rule,'DROPENTRY'));
            stoprule    := ini.ReadBool   ('LOGGER','RULE_STOPRULE_'+rule,true);
            GFRE_LOG.AddRule(category,loglevel,target,logaction,stoprule);
          end;
        finally
          sl.Free;
        end;
      end else begin
        GFRE_LOG.EnableSyslog;
      end;
      GFRE_LOG.SetLocalZone(cFRE_SERVER_DEFAULT_TIMEZONE);
    end;

  begin
    try
      //writeln('READING CONFIG FILE [',cfgfile,']');
      ini := TMemIniFile.Create(cfgfile);
      try
        cFRE_SERVER_DEFAULT_DIR         := ini.ReadString('BASE','DIR'               , cFRE_SERVER_DEFAULT_DIR);
        cFRE_SERVER_WWW_ROOT_DIR        := ini.ReadString('BASE','WWWROOT'           , cFRE_SERVER_WWW_ROOT_DIR);
        cFRE_SERVER_DEFAULT_TIMEZONE    := ini.ReadString('BASE','TIMEZONE'          , cFRE_SERVER_DEFAULT_TIMEZONE);
        cFRE_WebServerLocation_HixiedWS := ini.ReadString('BASE','HIXIE_WS_LOCATION' , cFRE_WebServerLocation_HixiedWS);
        cFRE_SSL_CERT_FILE              := ini.ReadString('SSL','CERT'               , cFRE_SSL_CERT_FILE);
        cFRE_SSL_PRIVATE_KEY_FILE       := ini.ReadString('SSL','KEY'                , cFRE_SSL_PRIVATE_KEY_FILE);
        cFRE_SSL_ROOT_CA_FILE           := ini.ReadString('SSL','CA'                 , cFRE_SSL_ROOT_CA_FILE);
        cG_OVERRIDE_USER                := ini.ReadString('DBG','USER'               , cG_OVERRIDE_USER);
        cG_OVERRIDE_PASS                := ini.ReadString('DBG','PASS'               , cG_OVERRIDE_PASS);
        cFRE_MONITORING_HOST            := ini.ReadString('MONITORING','HOST'        , cFRE_MONITORING_HOST);
        cFRE_MONITORING_USER            := ini.ReadString('MONITORING','USER'        , cFRE_MONITORING_USER);
        cFRE_MONITORING_KEY_FILE        := ini.ReadString('MONITORING','KEY_FILE'    , cFRE_MONITORING_KEY_FILE);
        cFRE_MONITORING_DEST_DIR        := ini.ReadString('MONITORING','DIRECTORY'   , cFRE_MONITORING_DEST_DIR);
        ConfigureLogging;
      finally
        ini.Free;
      end;
    except
      on E:Exception do begin
        GFRE_LOG.LogEmergency('EXCEPTION ON LOADING CONFIG FILE:'+E.Message);
      end;
    end;
  end;

begin
  if FileExists('.fre_ini') then begin
    cfgfile:='.fre_ini';
  end else if FileExists(cFRE_GLOBAL_DIRECTORY+DirectorySeparator+'.fre_ini') then begin
    cfgfile:=cFRE_GLOBAL_DIRECTORY+DirectorySeparator+'.fre_ini';
  end else begin
    userfile := GetUserDir+'.firmos'+DirectorySeparator+'.fre_ini';
    if FileExists(userfile) then begin
      cfgfile:=userfile;
    end;
  end;
  if cfgfile<>'' then begin
    ReadConfigFile;
  end;
  if pos('~',cFRE_SERVER_DEFAULT_DIR)>0 then begin
    cFRE_SERVER_DEFAULT_DIR := StringReplace(cFRE_SERVER_DEFAULT_DIR,'~',GetUserDir,[]);
    cFRE_SERVER_DEFAULT_DIR := StringReplace(cFRE_SERVER_DEFAULT_DIR,DirectorySeparator+DirectorySeparator,DirectorySeparator,[]);
  end;
  if cFRE_SERVER_WWW_ROOT_DIR='' then begin
    cFRE_SERVER_WWW_ROOT_DIR := cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'www';
  end;

  cFRE_HAL_CFG_DIR     := cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'hal'+DirectorySeparator;
  cFRE_PID_LOCK_DIR    := cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'pidlocks'+DirectorySeparator;
  cFRE_JOB_RESULT_DIR  := cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'job'+DirectorySeparator;
  cFRE_JOB_ARCHIVE_DIR := cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'jobarchive'+DirectorySeparator;

  //GFRE_DBI.LogInfo(dblc_PERSITANCE,'DB BASE DIR : '+cFRE_SERVER_DEFAULT_DIR);
  //GFRE_DBI.LogInfo(dblc_PERSITANCE,'DB WWW  DIR : '+cFRE_SERVER_WWW_ROOT_DIR);
  //GFRE_DBI.LogInfo(dblc_PERSITANCE,'DB TIMEZONE : '+cFRE_SERVER_DEFAULT_TIMEZONE);
  //GFRE_DBI.LogInfo(dblc_PERSITANCE,'DB HIXIE WS : '+cFRE_WebServerLocation_HixiedWS);
  //GFRE_DBI.LogInfo(dblc_PERSITANCE,'DB SSL FILES [Key=%s Cert=%s CA=%s] : ',[cFRE_SSL_PRIVATE_KEY_FILE,cFRE_SSL_CERT_FILE,cFRE_SSL_ROOT_CA_FILE]);
  ForceDirectories(cFRE_HAL_CFG_DIR);
  ForceDirectories(cFRE_PID_LOCK_DIR);
  ForceDirectories(cFRE_JOB_RESULT_DIR);
  ForceDirectories(cFRE_JOB_ARCHIVE_DIR);
  if not DirectoryExists(cFRE_HAL_CFG_DIR) then GFRE_BT.CriticalAbort('FRE_SYSTEM STARTUP COULD NOT/CREATE FIND HAL BASEDIR [%s]',[cFRE_HAL_CFG_DIR]);
  if not DirectoryExists(cFRE_PID_LOCK_DIR) then GFRE_BT.CriticalAbort('FRE_SYSTEM STARTUP COULD NOT/CREATE FIND PIDLOCK BASEDIR [%s]',[cFRE_PID_LOCK_DIR]);
  if not DirectoryExists(cFRE_JOB_RESULT_DIR) then GFRE_BT.CriticalAbort('FRE_SYSTEM STARTUP COULD NOT/CREATE FIND JOB RESULT BASEDIR [%s]',[cFRE_JOB_RESULT_DIR]);
  if not DirectoryExists(cFRE_JOB_ARCHIVE_DIR) then GFRE_BT.CriticalAbort('FRE_SYSTEM STARTUP COULD NOT/CREATE FIND JOB ARCHIVE BASEDIR [%s]',[cFRE_JOB_ARCHIVE_DIR]);

  cFRE_ALERTING_CONFIG_FILE := cFRE_HAL_CFG_DIR+'alerting_config.dbo';
  cFRE_ALERTING_STATUS_FILE := cFRE_HAL_CFG_DIR+'alerting_status.dbo';
end;

end.

