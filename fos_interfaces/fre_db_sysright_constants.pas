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

  cSYS_DOMAIN          = 'SYSTEM';

  function  Get_Rightname_App_Role_Subrole   (const app_name,sub_role_name:string):string;inline;
  function  Get_Groupname_App_Group_Subgroup (const app_name,sub_group_name:string):string;inline;
  function  Get_Rightname_App_Helper         (const app_name,sub_right_name:string):string;inline;

implementation

function Get_Rightname_App_Role_Subrole(const app_name, sub_role_name: string): string;
begin
   result := '$AP_RO_'+uppercase(app_name+'_'+sub_role_name);
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

