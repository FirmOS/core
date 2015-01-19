unit fre_system;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.-Ing. Helmut Hartl, Dipl.-Ing. Franz Schober, Dipl.-Ing. Christian Koch
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
{$modeswitch nestedprocvars}

interface

uses sysutils;

const cFRE_GLOBAL_DIRECTORY = '/opt/local/fre';

var
      cFRE_WEB_STYLE                  :string = 'firmos';
      cFRE_LOGIN_OVERRIDE             :string = '';
      cFRE_TITLE_OVERRIDE             :string = '';
      cFRE_JS_DEBUG                   :Boolean= false;
      cFRE_SERVER_DEFAULT_DIR         :string = '~/.firmos/fre';
      cFRE_SERVER_DEFAULT_SSL_DIR     :string = '~/.firmos/fre/ssl/server_files/';
      cFRE_PS_LAYER_IP                :string = '';
      cFRE_PS_LAYER_PORT              :string = '';
      cFRE_PS_LAYER_HOST              :string = '';
      cFRE_PS_LAYER_USE_EMBEDDED      :boolean= false;
      cFRE_PS_LAYER_UXSOCK_NAME       :string = 'plsrv';
      cFRE_UX_SOCKS_DIR               :string = '';      { override for standard server path/sockets/file}
      cFRE_HAL_CFG_DIR                :string = '';
      cFRE_TMP_DIR                    :string = '';
      cFRE_JOB_RESULT_DIR             :string = '';
      cFRE_JOB_PROGRESS_DIR           :string = '';
      cFRE_JOB_ARCHIVE_DIR            :string = '';
      cFRE_PID_LOCK_DIR               :string = '';
      cFRE_SERVER_WWW_ROOT_DIR        :string = '';
      cFRE_SERVER_WWW_ROOT_DYNAMIC    :string = '';
      cFRE_SERVER_WWW_ROOT_FILENAME   :string = 'FREROOT.html';
      cFRE_SERVER_DEFAULT_TIMEZONE    :string = 'Europe/Vienna';
      cFRE_WebServerLocation_HixiedWS :string = '127.0.0.1:44000';
      cFRE_SSL_CERT_FILE              :string = 'server_cert.pem';
      cFRE_SSL_PRIVATE_KEY_FILE       :string = 'server_key.pem';
      cFRE_SSL_ROOT_CA_FILE           :string = 'ca_cert.pem';
      cG_OVERRIDE_USER                :string = ''; { used for web autologin }
      cG_OVERRIDE_PASS                :string = '';
      cFRE_ADMIN_USER                 :string = 'admin@system';
      cFRE_ADMIN_PASS                 :string = 'admin';
      cFRE_PL_ADMIN_USER              :string = 'sysdba';
      cFRE_PL_ADMIN_PASS              :string = 'sysdba';
      cFRE_TASKER_PASS                :string = 'tasker';
      cFRE_MONITORING_HOST            :string = '';
      cFRE_MONITORING_USER            :string = '';
      cFRE_MONITORING_KEY_FILE        :string = 'monitoring_id';
      cFRE_MONITORING_DEST_DIR        :string = '';
      cFRE_ALERTING_CONFIG_FILE       :string = '';
      cFRE_ALERTING_STATUS_FILE       :string = '';
      cFRE_REMOTE_USER                :string = '';
      cFRE_REMOTE_HOST                :string = '';
      cFRE_Feed_User                  :string = '';
      cFRE_Feed_Pass                  :string = '';
      cFRE_MWS_IP                     :string = ''; // connect the MWS non standard over IP
      cFRE_SUBFEEDER_IP               :string = ''; // use an IP connect for all subfeeders (portmapping is static subfeederdefined)
      cFRE_ToolsPath                  :string = '/usr';
      cFRE_FORCE_CLEAR_HTTP_META      :boolean=false;
      cFRE_FORCE_CLEAN_ZIP_HTTP_FILES :boolean=false;
      cFRE_BUILD_ZIP_HTTP_FILES       :boolean=true;
      cFRE_USE_STATIC_CACHE           :boolean=true;
      cFRE_STATIC_HTTP_CACHE_EXTS     :string = '.js,.css,.html,.htm,.xhtml'; // comma seperated,lowercase (!!)
      cFRE_DEPLOY_CONTENT_EXTS        :string = '.js,.css'; // comma seperated,lowercase (!!)
      cFRE_MACHINE_NAME               :string = '';
      cFRE_MACHINE_MAC                :string = '';
      cFRE_DEFAULT_DOMAIN             :string = 'system';
      cFRE_DB_ALLOWED_APPS            :string = '';
      cFRE_DB_ALLOW_WEAKMEDIATORS     :boolean=true;
      cFRE_DB_RESET_ADMIN             :boolean=false;
      cFRE_DB_CACHETAG                :ShortString='';
      cFRE_CMDLINE_DEBUG              :ShortString='';
      cFRE_SAFEJOB_BIN                :string = '';

      cFRE_PL_ADMINS                  :string='sysdba';
      cFRE_PL_ADMINS_PWS              :String='e1NTSEF9THV4UGIzeUJqMUZ5eUl4dy82NkUrWStELzl0SU0wcExNVlZwVFE9PQ==';

      G_DEBUG_TRIGGER_1               :boolean=false;
      G_DEBUG_TRIGGER_2               :boolean=false;
      G_DEBUG_TRIGGER_3               :boolean=false;
      G_DEBUG_TRIGGER_4               :boolean=false;
      G_DEBUG_TRIGGER_5               :boolean=false;
      G_DEBUG_COUNTER                 :NativeInt = 0;
      G_DEBUG_RUNONLY_MS              :NativeInt = 0;

const
      cG_Tuneable_LocalStackBuffer = 4096;


type
  EFRE_Exception=class(Exception)
  end;

  TFRE_LogMsg  = procedure (msg:String) of object;
  TFRE_LogMsgN = procedure (msg:String) is nested;

  function GET_FOS_CACHE_TAG:ShortString;inline;
  function POS_FOS_CACHE_TAG(const url:string):NativeInt;inline;

implementation

function GET_FOS_CACHE_TAG: ShortString;
begin
  result :='.*FCT'+cFRE_DB_CACHETAG;
end;

function POS_FOS_CACHE_TAG(const url: string): NativeInt;
begin
  result := Pos('.*FCT',url);
end;


initialization
  G_DEBUG_RUNONLY_MS := StrToIntDef(GetEnvironmentVariable('FOS_DBG_RUNONLY_MS'),0);


end.

