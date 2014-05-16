unit fre_dbbase;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE;

type

  { TFRE_DB_GLOBAL_TEXTS }

  TFRE_DB_GLOBAL_TEXTS=class(TFRE_DB_ObjectEx)
  public
  protected
    class procedure  InstallDBObjects       (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

procedure Register_DB_Extensions;

implementation

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ObjectEx);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_GLOBAL_TEXTS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_APPLICATION);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_APPLICATION_MODULE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_NOTE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_UNCONFIGURED_MACHINE);
  GFRE_DBI.Initialize_Extension_Objects;
end;

{ TFRE_DB_GLOBAL_TEXTS }

class procedure TFRE_DB_GLOBAL_TEXTS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';

  if (currentVersionId='') then begin
    currentVersionId:='1.0';

    //all apps
    StoreTranslateableText(conn,'$error_no_access','Access denied');
    StoreTranslateableText(conn,'$button_save','Save');
    //fre_db_common
    StoreTranslateableText(conn,'input_confirm_prefix','Confirm');
    //dojo
    StoreTranslateableText(conn,'search_label','Search:');
    StoreTranslateableText(conn,'gf_n_eq','Equal');
    StoreTranslateableText(conn,'gf_n_lt','Less than');
    StoreTranslateableText(conn,'gf_n_gt','Greater than');
    StoreTranslateableText(conn,'gf_n_gtlt','Between');
    StoreTranslateableText(conn,'gf_d_eq','Equal');
    StoreTranslateableText(conn,'gf_d_lt','Before');
    StoreTranslateableText(conn,'gf_d_gt','After');
    StoreTranslateableText(conn,'gf_d_gtlt','Between');
    StoreTranslateableText(conn,'gf_filter_label','Filter');
    StoreTranslateableText(conn,'gf_filter_set','Set');
    StoreTranslateableText(conn,'gf_filter_clear','Clear');
    StoreTranslateableText(conn,'in_file_select','Select');
    StoreTranslateableText(conn,'in_combo_placeholder','Please select');
    StoreTranslateableText(conn,'msg_confirm_yes','Yes');
    StoreTranslateableText(conn,'msg_confirm_no','No');
    StoreTranslateableText(conn,'msg_ok','OK');
    StoreTranslateableText(conn,'msg_abort','Abort');
    StoreTranslateableText(conn,'editor_save','Save');
    StoreTranslateableText(conn,'editor_reset','Reset');
    StoreTranslateableText(conn,'vnc_cad','Send Ctrl+Alt+Del');
    StoreTranslateableText(conn,'vnc_wakeup','Wake Up');
    StoreTranslateableText(conn,'vnc_mount','Mount ISO');
    StoreTranslateableText(conn,'rec_once','Once');
    StoreTranslateableText(conn,'rec_minute','Every minute');
    StoreTranslateableText(conn,'rec_hour','Hourly');
    StoreTranslateableText(conn,'rec_day','Daily');
    StoreTranslateableText(conn,'rec_week','Weekly');
    StoreTranslateableText(conn,'rec_month','Monthly');
    StoreTranslateableText(conn,'rec_quarter','Quarterly');
    StoreTranslateableText(conn,'rec_year','Yearly');
    StoreTranslateableText(conn,'rec_mo','M');
    StoreTranslateableText(conn,'rec_tu','T');
    StoreTranslateableText(conn,'rec_we','W');
    StoreTranslateableText(conn,'rec_th','T');
    StoreTranslateableText(conn,'rec_fr','F');
    StoreTranslateableText(conn,'rec_sa','S');
    StoreTranslateableText(conn,'rec_su','S');
    StoreTranslateableText(conn,'rec_start','Start');
    StoreTranslateableText(conn,'rec_noend','Forever');
    StoreTranslateableText(conn,'rec_end','Until');
    StoreTranslateableText(conn,'rec_interval','Interval');
    StoreTranslateableText(conn,'rec_count','Count');
    StoreTranslateableText(conn,'ow_error','Unable to open window! Popup Blocker?');
    StoreTranslateableText(conn,'close','Close');
  end;
end;


end.

