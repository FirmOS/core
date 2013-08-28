unit fre_db_sysright_constants;

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

uses Sysutils;

const
  cSYS_APPNAME         = 'SYS';
  cSYSROLE_DB_ADMIN    = 'SYSROLE_DB_ADMIN';
  cSYSR_CREATE_DB      = 'SYSR_CREATE_DB';
  cSYSR_DELETE_DB      = 'SYSR_DELETE_DB';

  cSYSROLE_DB_MANAGE   = 'SYSROLE_DB_MANAGE';
  cSYSR_ADD_USER       = 'SYSR_ADD_USER';
  cSYSR_DEL_USER       = 'SYSR_DEL_USER';
  cSYSR_MOD_UG         = 'SYSR_MOD_UG';
  cSYSR_MOD_RIGHT      = 'SYSR_MOD_RIGHT'; // Create, Delete, Rights, Rightgroups,Reset passowrd

  cSYSROLE_MANAGE_APPS = 'SYSROLE_DB_MANAGE_APPS';
  cSYSR_INSTALL_APP    = 'SYSR_INSTALL_APP';
  cSYSR_UNINSTALL_APP  = 'SYSR_UNINSTALL_APP';

  cSYSROLE_DB_USER     = 'SYSROLE_DB_USER';
  cSYSR_LOGIN_DB       = 'SYSR_LOGIN_DB';
  cSYSR_READ_DBO       = 'SYSR_READ_DBO';
  cSYSR_EXEC_DBO       = 'SYSR_EXEC_DBO';
  cSYSR_WRITE_DBO      = 'SYSR_WRITE_DBO';
  cSYSR_DELETE_DBO     = 'SYSR_DELETE_DBO';

  cSYSROLE_DB_GUEST    = 'SYSROLE_DB_GUEST';

const //SYS_GROUPS
  cSYS_DOMAIN          = 'SYSTEM';

  cSYSUG_DB_GUESTS     = 'SYSUG_DB_GUESTS';
  cSYSUG_DB_USERS      = 'SYSUG_DB_USERS';
  cSYSUG_MANAGE_USERS  = 'SYSUG_MANAGE_USERS';
  cSYSUG_ADMIN_USERS   = 'SYSUG_ADMIN_USERS';



  function  Get_Rightname_App_Role_Subrole   (const app_name,sub_role_name:string):string;inline;
  function  Get_Groupname_App_Group_Subgroup (const app_name,sub_group_name:string):string;inline;
  function  Get_Rightname_App_Helper         (const app_name,sub_right_name:string):string;inline;

implementation

function Get_Rightname_App_Role_Subrole(const app_name, sub_role_name: string): string;
begin
   result := '$AP_RG_'+uppercase(app_name+'_'+sub_role_name);
end;

function Get_Groupname_App_Group_Subgroup(const app_name, sub_group_name: string): string;
begin
   result := '$AP_UG_'+uppercase(app_name+'_'+sub_group_name);
end;

function Get_Rightname_App_Helper(const app_name, sub_right_name: string): string;
begin
   result := '$AP_R_'+uppercase(app_name+'_'+sub_right_name);
end;

end.

