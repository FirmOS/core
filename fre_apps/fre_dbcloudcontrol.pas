unit fre_dbcloudcontrol;

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
{$codepage utf-8}

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  Process,

  FRE_HAL_UTILS,
  FRE_DB_COMMON,
  FRE_DBBUSINESS,
  FRE_DB_INTERFACE,
  FRE_DBMONITORING,
  fos_dbcorebox_machine,
  FRE_HAL_TRANSPORT,
  FRE_DBSERVICEGROUPCONTROL,
  FRE_DB_SYSRIGHT_CONSTANTS,
  fre_system,
  fre_testcase,
  fre_alert,
  fre_zfs;

procedure Register_DB_Extensions;
procedure CreateDB(const real: boolean;const dbname: string; const user, pass: string);


type

  { TFRE_DB_DEVICE }

  TFRE_DB_DEVICE=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;

  { TFRE_DB_NETWORK_GROUP }

  TFRE_DB_NETWORK_GROUP=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;

  { TFRE_DB_CMS }

  TFRE_DB_CMS=class(TFRE_DB_SERVICE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;

  TFRE_DB_REDIRECTION_FLOW=class(TFRE_DB_ObjectEx)
   protected
     class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
   end;

  { TFRE_DB_ROUTE }

  TFRE_DB_ROUTE=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;




  { TFRE_DB_Site_Captive_Extension }

  TFRE_DB_Site_Captive_Extension = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;




  { TFRE_DB_Endpoint }

  TFRE_DB_Endpoint = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
    function IMI_Content         (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Configuration   (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Monitoring      (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Monitoring_Con  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Monitoring_All  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Monitoring_Data (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Provision       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_addOpenWifiNetwork  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_addWPA2Network  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_ChildrenData    (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_Accesspoint }

  TFRE_DB_Accesspoint = class (TFRE_DB_Endpoint)
  private
    function  HasAnotherAP        (const site_id:TGUID)  : boolean;
  protected
    class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION); override;
    class procedure AccessPointOnChange   (const conn: IFRE_DB_CONNECTION; const is_dhcp:boolean ; const dhcp_id : TGUID; const mac : TFRE_DB_String); virtual;
  published
   class function  IMC_NewOperation       (const input: IFRE_DB_Object): IFRE_DB_Object; override;
   function        IMI_Menu               (const input:IFRE_DB_Object): IFRE_DB_Object;
   function        IMI_SaveOperation      (const input:IFRE_DB_Object):IFRE_DB_Object;virtual;
  end;

  { TFRE_DB_AP_Linksys }

  TFRE_DB_AP_Linksys = class (TFRE_DB_Accesspoint)
  private
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects      (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
    function IMI_Configuration   (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys_E1000 }

  TFRE_DB_AP_Linksys_E1000 = class (TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
   function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys_E1200 }

  TFRE_DB_AP_Linksys_E1200 = class (TFRE_DB_AP_Linksys)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
   function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Linksys_E1200V2 }

  TFRE_DB_AP_Linksys_E1200V2 = class (TFRE_DB_AP_Linksys_E1200)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
   function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Lancom }

  TFRE_DB_AP_Lancom = class (TFRE_DB_Accesspoint)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_AP_Lancom_IAP321 }

  TFRE_DB_AP_Lancom_IAP321 = class (TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_AP_Lancom_OAP321 }

  TFRE_DB_AP_Lancom_OAP321 = class (TFRE_DB_AP_Lancom)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_GetDisplayName  (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_Monitoring_Status }

  TFRE_DB_Monitoring_Status = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_GetStatusIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
  end;




  { TFRE_DB_CMS_PAGE }

  TFRE_DB_CMS_PAGE = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
    function IMI_Content  (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_Menu     (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_CMS_ADPAGE }

  TFRE_DB_CMS_ADPAGE = class(TFRE_DB_CMS_PAGE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
  end;


  { TFRE_DB_WAC }

  TFRE_DB_WAC = class (TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure ; override;
    function        InstallAppDefaults        (const conn : IFRE_DB_SYS_CONNECTION):TFRE_DB_Errortype; override;
    procedure       _UpdateSitemap            (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize       (const session:TFRE_DB_UserSession);override;
    procedure       MySessionPromotion        (const session: TFRE_DB_UserSession); override;
    function        ShowInApplicationChooser  (const session:TFRE_DB_UserSession): Boolean;override; //TODO
    function        _ActualVersion            : TFRE_DB_String; override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function        IMI_Content          (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_BAC }

  TFRE_DB_BAC = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_WF }

  TFRE_DB_WF = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure   RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure         SetupAppModuleStructure; override;
  private
    CLS_wf_Content           : TFRE_DB_SUBSECTIONS_DESC;
    DC_WorkfFlowSchemes      : IFRE_DB_DERIVED_COLLECTION;
    DC_WorkfFlowsFiltered    : IFRE_DB_DERIVED_COLLECTION;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_SIAC }

  TFRE_DB_SIAC = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_SIAC_CAP }

  TFRE_DB_SIAC_CAP = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_SIAC_CA }

  TFRE_DB_SIAC_CA = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme        (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure     ; override;
    procedure       MySessionInitializeModule   (const session: TFRE_DB_UserSession); override;
  published
    function IMI_Content                 (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Menu                    (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_addCertificateAuthority (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_SIAC_DHCP }

  TFRE_DB_SIAC_DHCP = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme       (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure    ;  override;
    procedure       MySessionInitializeModule  (const session: TFRE_DB_UserSession); override;
  published
    function IMI_Content  (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_SIAC_CMS }

  TFRE_DB_SIAC_CMS = class (TFRE_DB_APPLICATION_MODULE)
  protected
    CMS_Layout   : TFRE_DB_LAYOUT_DESC;
    DC_CMS_Pages  : IFRE_DB_DERIVED_COLLECTION;
    DC_AD_Pages   : IFRE_DB_DERIVED_COLLECTION;
    class procedure RegisterSystemScheme          (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure       ; override;
    procedure       MySessionInitializeModule     (const session: TFRE_DB_UserSession); override;
  published
    function IMI_Content        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_MenuAD         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_MenuCP         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_ChildrenDataAD (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_ChildrenDataCP (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_addADPage      (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_addCMSPage     (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_MONAC }

  TFRE_DB_MONAC = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure; override;
  private
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;


  { TFRE_DB_MobileDevice }

  TFRE_DB_MobileDevice = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Content  (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_Menu     (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_unassign (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Network }

  TFRE_DB_Network = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION); override;
    class procedure NetworkOnChange      (const dbc : IFRE_DB_Connection; const is_dhcp:boolean; const subnet : string; const ep_id: TGUID; const dns:string; const range_start, range_end : integer ); virtual;
  published
   class function  IMC_NewOperation      (const input:IFRE_DB_Object): IFRE_DB_Object; override;
   function IMI_Content                  (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_Menu                     (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_SaveOperation            (const input:IFRE_DB_Object):IFRE_DB_Object; override;
  end;

  { TFRE_DB_WifiNetwork }

  TFRE_DB_WifiNetwork = class (TFRE_DB_Network)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Content(const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_OpenWifiNetwork }

  TFRE_DB_OpenWifiNetwork = class (TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_WPA2Network }

  TFRE_DB_WPA2Network = class (TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  end;


  { TFRE_DB_RadiusNetwork }

  TFRE_DB_RadiusNetwork = class (TFRE_DB_WifiNetwork)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Content(const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_CA }

  TFRE_DB_CA = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Menu           (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_Content        (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_ChildrenData   (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_AddCertificate (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Certificate }

  TFRE_DB_Certificate = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Menu       (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_Content    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP }

  TFRE_DB_DHCP = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Content      (const input:IFRE_DB_Object) : IFRE_DB_Object;
   function IMI_Menu         (const input:IFRE_DB_Object) : IFRE_DB_Object;
   function IMI_ChildrenData (const input:IFRE_DB_Object) : IFRE_DB_Object;
   function IMI_addSubnet    (const input:IFRE_DB_Object) : IFRE_DB_Object;
   function IMI_addFixedHost (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP_Subnet }

  TFRE_DB_DHCP_Subnet = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
    function IMI_Content  (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu     (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_DHCP_Fixed }

  TFRE_DB_DHCP_Fixed = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
    function IMI_Content  (const input:IFRE_DB_Object) : IFRE_DB_Object;
    function IMI_Menu     (const input:IFRE_DB_Object) : IFRE_DB_Object;
  end;

  { TFRE_DB_VPN }

  TFRE_DB_VPN = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_Menu       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content(const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Radius }

  TFRE_DB_Radius = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_Menu       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Captiveportal }

  TFRE_DB_Captiveportal = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_Menu       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_Routing }

  TFRE_DB_Routing = class (TFRE_DB_Service)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION); override;
  published
   function IMI_Menu       (const input:IFRE_DB_Object):IFRE_DB_Object;
   function IMI_Content    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

//procedure CreateWacDB(const conn:IFRE_DB_CONNECTION);

implementation

procedure InitializeCaptive(const dbname: string; const user, pass: string);
var conn : IFRE_DB_SYS_CONNECTION;
    res  : TFRE_DB_Errortype;
    i       : integer;
    login   : string;

begin
  CONN := GFRE_DBI.NewSysOnlyConnection;
  try
    res  := CONN.Connect('admin','admin');
    if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);

      conn.InstallAppDefaults('WAC');

      for i:= 1 to 3 do begin
        login  := 'admin'+inttostr(i);
        if conn.UserExists(login) then begin
          writeln('Modify Groups for User '+login);
          CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('WAC','ADMIN'),Get_Groupname_App_Group_Subgroup('WAC','USER')]),true),'cannot set usergroups '+login);
        end;
      end;

      for i:= 1 to 3 do begin
        login  := 'user'+inttostr(i);
        if conn.UserExists(login) then begin
          writeln('Modify Groups for User '+login);
          CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('WAC','USER')]),true),'cannot set usergroups '+login);
        end;
      end;

      CheckDbResult(conn.ModifyUserGroups('guest',GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('WAC','GUEST')]),true),'cannot set usergroups guest');

  finally
    conn.Finalize;
  end;
 end;


 procedure SetReprovision (const dbc: IFRE_DB_Connection; const id:TGUID);
 var
     obj       :    IFRE_DB_Object;
 begin
   writeln  ('set reprovision :'+GUIDToString(id));
   if  not  dbc.Fetch(id,obj)  then raise EFRE_DB_Exception.Create(edb_ERROR,'NO OBJ FOUND FOR REPROVISION '+GUIDToString(id));
   obj.Field('reprovision').asboolean:=true;
   writeln  (obj.DumpToString());
   CheckDbResult(dbc.Update(obj),'failure on cloned/update');
 end;

 function GetService     (const dbc: IFRE_DB_Connection; const serviceclass:string): TGUID;
 var
     coll    : IFRE_DB_COLLECTION;
     id      : TGUID;

     function _get(const obj:IFRE_DB_Object):boolean;
     begin
       result := false;
       writeln('SERVICE '+obj.UID_String);
       if obj.IsA(serviceclass) then begin
         writeln('FOUND '+serviceclass+' '+obj.UID_String);
         id:=obj.uid;
         Result := true;
       end;
     end;


 begin
   writeln('GET SERVICE');
   coll   := dbc.Collection('service');
   coll.ForAllBreak(@_get);
   result := id;
 end;


 procedure HasNets(const ep_id : IFRE_DB_Object; out has_open, has_wpa2: boolean);
 var
     childs              : TFRE_DB_GUIDArray;

 begin
   writeln('HAS NETS');
   has_open:=false; has_wpa2:=false;
   childs:=ep_id.ReferencedByList('TFRE_DB_OPENWIFINETWORK');
   has_open:=length(childs)>0;
   writeln ('WIFI :',length(childs));
   childs:=ep_id.ReferencedByList('TFRE_DB_WPA2NETWORK');
   writeln ('WPA2 :',length(childs));
   has_wpa2:=length(childs)>0;
 end;


 function GetNextNet (const dbc: IFRE_DB_CONNECTION; const ep_id:TGUID) : string;
 var
     ep_obj     : IFRE_DB_Object;
     cap_id     : TGUID;
     cap_obj    : IFRE_DB_Object;
     net        : string;
     colln      : IFRE_DB_COLLECTION;
     highest    : integer;
     cp_net     : string;
     cp_ip      : TFRE_HAL_IP4;
     result_ip  : TFRE_HAL_IP4;
     mask       : string;

     procedure _getnets(const obj:IFRE_DB_Object);
     var
         lcurrent     : string;
         lnet         : string;
         lsplit       : string;
         lmask        : string;
         lip          : TFRE_HAL_IP4;

     begin
       lnet       := obj.Field('ip_net').AsString;
       SplitCIDR(lnet,lcurrent,lmask);
       lip        := StringtoIP4 (lcurrent);
       if (cp_ip._bytes[0]=lip._bytes[0]) and (cp_ip._bytes[1]=lip._bytes[1]) then begin
        writeln (lcurrent);
        if lip._bytes[2]>highest then begin
         highest  := lip._bytes[2];
        end;
       end;
     end;

 begin
   if not dbc.Fetch(ep_id,ep_obj) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO EP FOUND IN GET NEXT NET '+GUIDToString(ep_id));
   cap_id:=GetService(dbc,'TFRE_DB_CAPTIVEPORTAL');
   if not dbc.Fetch(cap_id,cap_obj) then raise EFRE_DB_Exception.Create(edb_ERROR,'NO CAPSERVICE FOUND IN GET NEXT NET '+GUIDToString(cap_id));

   if ep_obj.IsA('TFRE_DB_AP_Lancom') then begin
     net := cap_obj.Field('lancom_net').asstring;
     writeln('lancom ');

   end else begin
     writeln('linksys');
     if ep_obj.FieldExists('vpn_crtid') then begin
      net := cap_obj.Field('vpn_net').asstring;
      writeln('vpn');
     end else begin
      net := cap_obj.Field('linksys_net').asstring;
      writeln('no vpn');
     end;
   end;
  writeln(net);

  SplitCIDR(net,cp_net ,mask);


  cp_ip   := StringtoIP4(cp_net );
  highest := cp_ip._bytes[2];
  writeln (highest);

  colln   :=dbc.Collection('network');
  colln.ForAll(@_getnets);

  writeln (highest);

  result_ip._long     := cp_ip._long;
  result_ip._bytes[2] := highest + 1;
  result_ip._bytes[3] := 1;
  result              := IP4toString(result_ip)+'/24';
  writeln (result);
 end;

 function CheckClass (const new_net:string) : boolean;
 var
     mask       : string;
 begin
  result := true;
  writeln('CHECK CLASS :'+new_net+':');
  if new_net='' then exit;                     // accept empty nets

  mask:= Copy(new_net,Pos('/',new_net)+1,maxint);
  if mask<>'24' then begin
   result:=false;
  end;
 end;

 function UniqueNet (const dbc: IFRE_DB_CONNECTION; const network_id:TGUID; const new_net: string) : boolean;
  var
      colln      : IFRE_DB_COLLECTION;
      check_net  : TFRE_HAL_IP4;
      ip         : string;
      mask       : string;
      gresult    : boolean;

      procedure _checknets(const obj:IFRE_DB_Object);
      var
          lcurrent     : string;
          lnet         : string;
          lsplit       : string;
          lmask        : string;
          lip          : TFRE_HAL_IP4;

      begin
        lnet       := obj.Field('ip_net').AsString;
        SplitCIDR(lnet,lcurrent,lmask);
        lip        := StringtoIP4 (lcurrent);
        if FREDB_Guids_Same(obj.UID,network_id)=false then begin    // check all other nets
         if (check_net._bytes[0]=lip._bytes[0]) and (check_net._bytes[1]=lip._bytes[1]) and (check_net._bytes[2]=lip._bytes[2]) then begin        // TODO FIXXME  Other Than /24 Networks
          writeln (lcurrent);
          gresult := false;
         end;
        end;
      end;

  begin

   gresult  := true;

   if new_net='' then exit;                     // accept empty nets

   SplitCIDR (new_net,ip,mask);
   check_net := StringtoIP4(ip);

   colln   := dbc.Collection('network');
   colln.ForAll(@_checknets);

   result   := gresult;
  end;


{ TFRE_DB_OpenWifiNetwork }

class procedure TFRE_DB_OpenWifiNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
end;

{ TFRE_DB_Monitoring_Status }

class procedure TFRE_DB_Monitoring_Status.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('status',fdbft_String).SetupFieldDef(true,false,'signal_status');
  scheme.AddSchemeField         ('provisioned_time',fdbft_DateTimeUTC);
  scheme.AddSchemeField         ('online_time',fdbft_DateTimeUTC);
  scheme.AddCalculatedField     ('status_icon','GetStatusIcon',cft_OnStoreUpdate);
end;

function TFRE_DB_Monitoring_Status.IMI_GetStatusIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:=GetStatusIconURI(Field('status').asstring);
end;

{ TFRE_DB_AP_Lancom_OAP321 }

class procedure TFRE_DB_AP_Lancom_OAP321.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LANCOM');
end;

function TFRE_DB_AP_Lancom_OAP321.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Accesspoint Lancom OAP321 ('+Field('provisioningmac').AsString+')';
end;

{ TFRE_DB_AP_Lancom_IAP321 }

class procedure TFRE_DB_AP_Lancom_IAP321.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LANCOM');
end;

function TFRE_DB_AP_Lancom_IAP321.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Accesspoint Lancom IAP321 ('+Field('provisioningmac').AsString+')';
end;

{ TFRE_DB_AP_Lancom }

class procedure TFRE_DB_AP_Lancom.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ACCESSPOINT');
end;

{ TFRE_DB_AP_Linksys_E1200V2 }

class procedure TFRE_DB_AP_Linksys_E1200V2.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS_E1200');
end;

function TFRE_DB_AP_Linksys_E1200V2.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Accesspoint Linksys E1200V2 ('+Field('provisioningmac').AsString+')';
end;

{ TFRE_DB_AP_Linksys_E1200 }

class procedure TFRE_DB_AP_Linksys_E1200.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS');
end;

function TFRE_DB_AP_Linksys_E1200.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Accesspoint Linksys E1200 ('+Field('provisioningmac').AsString+')';
end;

{ TFRE_DB_AP_Linksys_E1000 }

class procedure TFRE_DB_AP_Linksys_E1000.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_AP_LINKSYS');
end;



function TFRE_DB_AP_Linksys_E1000.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Accesspoint Linksys E1000 ('+Field('reprovision').AsString+') ('+Field('provisioningmac').AsString+')';
end;

{ TFRE_DB_AP_Linksys }


class procedure TFRE_DB_AP_Linksys.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ACCESSPOINT');

  scheme.AddSchemeField('routing',fdbft_String).SetupFieldDef(true,false,'routing');
  scheme.AddSchemeField('vpn_crtid',fdbft_ObjLink);

  group:=scheme.AddInputGroup('options').Setup('$scheme_TFRE_DB_AP_LINKSYS_options_group');
  group.AddInput('routing','$scheme_TFRE_DB_AP_LINKSYS_routing');
  group.AddInput('vpn_crtid','$scheme_TFRE_DB_AP_LINKSYS_vpn_cert',false,false,'certificate');

end;

class procedure TFRE_DB_AP_Linksys.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_AP_LINKSYS_options_group','Device Options'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_AP_LINKSYS_routing','Routing'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_AP_LINKSYS_vpn_cert','VPN Certificate'));
end;

function TFRE_DB_AP_Linksys.IMI_Configuration(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  result:=inherited;

  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=result.Implementor_HC as TFRE_DB_FORM_PANEL_DESC;
  res.AddSchemeFormGroup(scheme.GetInputGroup('options'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));

end;

{ TFRE_DB_Accesspoint }



class procedure TFRE_DB_Accesspoint.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_ENDPOINT');

  scheme.AddSchemeField('external_ip',fdbft_String).SetupFieldDef(False,False,'','ip');
  scheme.AddSchemeField('dhcp',fdbft_Boolean).addDepField('external_ip');
  scheme.AddSchemeField('channel',fdbft_UInt16).required:=true;
  scheme.AddSchemeField('password',fdbft_String);
  scheme.AddSchemeField('serialnumber',fdbft_String);
  scheme.AddSchemeField('mountingdetail',fdbft_String);

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_ACCESSPOINT_main_group');
  group.UseInputGroup('TFRE_DB_ENDPOINT','main');
  group.AddInput('serialnumber','Serialnumber');
  group.AddInput('external_ip','$scheme_TFRE_DB_ACCESSPOINT_exip');
  group.AddInput('dhcp','$scheme_TFRE_DB_ACCESSPOINT_dhcp');
  group.AddInput('channel','$scheme_TFRE_DB_ACCESSPOINT_channel');
  group.AddInput('password','$scheme_TFRE_DB_ACCESSPOINT_pw');
end;

class procedure TFRE_DB_Accesspoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_main_group','Accesspoint Configuration'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_serial','Serialnumber'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_exip','IP'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_dhcp','DHCP'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_channel','Channel'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ACCESSPOINT_pw','Password'));
end;



class function TFRE_DB_Accesspoint.IMC_NewOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var dbc        :   IFRE_DB_CONNECTION;
    raw_object :   IFRE_DB_Object;
    dhcp       :   boolean;
    mac        :   string;
    dhcp_id    :   TGUID;


begin
  writeln ('AP AddOperation');
  writeln(input.DumpToString());
  dbc          := input.GetReference as IFRE_DB_CONNECTION;
  raw_object   := input.Field('data').AsObject;

  dhcp    := raw_object.Field('dhcp').asboolean;
  mac     := lowercase(raw_object.Field('provisioningmac').AsString);
  writeln('now get dhcp');
  dhcp_id := GetService(dbc,'TFRE_DB_DHCP');
  raw_object.Field('reprovision').AsString:='true';

  Result:=inherited IMC_NewOperation(input);
  writeln ('After new AP');
  AccesspointOnChange (dbc, dhcp, dhcp_id, mac);
end;

function TFRE_DB_Accesspoint.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       :   TFRE_DB_MENU_DESC;
  has_open  :   boolean;
  has_wpa2  :   boolean;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  if HasAnotherAP(Field('site').asguid)=false then begin
   HasNets(self, has_open,has_wpa2);
   if not has_open then res.AddEntry.Describe('Add Open Wifi Network','images_apps/cloudcontrol/add_open_wifi.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addopenwifinetwork'));
   if not has_wpa2 then res.AddEntry.Describe('Add WPA2 Wifi Network','images_apps/cloudcontrol/add_wpa2_wifi.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addwpa2network'));
  end;
  res.AddEntry.Describe('Update Provisioning','images_apps/cloudcontrol/update_provisioning.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'provision'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_accesspoint.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;


function TFRE_DB_Accesspoint.HasAnotherAP(const site_id: TGUID): boolean;
var site_object       : IFRE_DB_Object;
    site_fnd          : boolean;
    childs            : TFRE_DB_GUIDArray;
    i                 : integer;
    has_open          : boolean;
    has_wpa2          : boolean;
    ep_obj            : IFRE_DB_Object;
begin
  result:=false;
  site_fnd          :=GetDBConnection.Fetch(site_id,site_object);
  if site_fnd        then begin
    writeln (site_object.DumpToString());
    childs:=site_object.ReferencedByList('TFRE_DB_ACCESSPOINT');
    for i := 0 to Length(childs) - 1 do begin
      if FREDB_Guids_Same(UID,childs[i])=false then begin
        GetDBConnection.Fetch(childs[i],ep_obj);
        HasNets(ep_obj,has_open,has_wpa2);
        if (has_open or has_wpa2) then begin
         result := true;
         break;
        end;
      end;
    end;
  end;
end;


class procedure TFRE_DB_Accesspoint.AccessPointOnChange(const conn: IFRE_DB_CONNECTION; const is_dhcp: boolean; const dhcp_id: TGUID; const mac: TFRE_DB_String);
var dhcp_obj        : IFRE_DB_Object;
    childs          : TFRE_DB_GUIDArray;
    dhcp_fixed_obj  : IFRE_DB_Object;
    i               : integer;
    collf           : IFRE_DB_Collection;
    current_ip      : string;
    highest         : integer;
    sub_ip          : integer;
begin
  writeln('PostSave');
  if is_dhcp then begin
    writeln('DHCP');
    if       not conn.Fetch(dhcp_id,dhcp_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DHCP SERVICE FOUND IN AP AFTER SAVE');
    writeln('ChiLDS');
    writeln(dhcp_obj.DumpToString);
    current_ip := dhcp_obj.Field('fixed_start').AsString;
    writeln(current_ip);
    highest    := StringtoIP4(current_ip)._bytes[3];                    // TODO implement for other classes than /24
    childs     := dhcp_obj.ReferencedByList('TFRE_DB_DHCP_FIXED');
    for i := 0 to Length(childs) - 1 do begin
      writeln('FETCH ChiLDS');
      conn.Fetch(childs[i],dhcp_fixed_obj);
      if lowercase(dhcp_fixed_obj.Field('mac').AsString)=mac then begin
        writeln('ALREADY IN DHCP !');
        exit;
      end else begin
        current_ip := dhcp_fixed_obj.Field('ip').AsString;
        sub_ip     := StringtoIP4(current_ip)._bytes[3];
        writeln(sub_ip);
        if sub_ip>highest then highest:=sub_ip;
      end;
    end;
    writeln('HIGHEST :', highest);
    inc(highest);
    // create new dhcp_fixed
    writeln('NOW ADD DHCP');
    collf          := conn .Collection('dhcp_fixed');
    dhcp_fixed_obj := conn .NewObject('TFRE_DB_DHCP_Fixed');
    dhcp_fixed_obj.Field('ip').AsString      := GetIPDots(dhcp_obj.Field('fixed_start').AsString,3)+inttostr(highest);
    dhcp_fixed_obj.Field('mac').AsString     := lowercase (mac);
    dhcp_fixed_obj.Field('objname').AsString := 'Automatic'+StringReplace(lowercase(mac),':','',[rfReplaceAll]);
    dhcp_fixed_obj.Field('dhcp').AsObjectLink:= dhcp_id;
    writeln('NOW STORE');
    CheckDBResult(COLLF.Store(dhcp_fixed_obj),'Add DHCP Fixed');                            // TODO Update DHCP Tree
    writeln ('DHCP FIXED CREATED !!');
  end;
end;


function TFRE_DB_Accesspoint.IMI_SaveOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
    dbc               : IFRE_DB_CONNECTION;
    dhcp              : boolean;
    dhcp_id           : TGUID;
    mac               : TFRE_DB_String;


begin
  if assigned(Parent) then begin
    result := TFRE_DB_MESSAGE_DESC(result).Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  dbc := GetDBConnection;
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;

  scheme.SetObjectFieldsWithScheme(raw_object,self,false,GetDBConnection);

  dhcp    := Field('dhcp').asboolean;
  mac     := lowercase(Field('provisioningmac').AsString);
  dhcp_id := GetService(dbc,'TFRE_DB_DHCP');
  Field('reprovision').AsBoolean:=true;

  CheckDbResult(dbc.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)
  result := GFRE_DB_NIL_DESC;

  AccesspointOnChange (dbc, dhcp, dhcp_id, mac);
end;


{ TFRE_DB_WF }


class procedure TFRE_DB_WF.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_WF.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('wf','$wf_description');
end;


procedure TFRE_DB_WF.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var //   sec : TFRE_DB_SECTION_DESC;
       CLS_WF_SchemesTransform : IFRE_DB_SIMPLE_TRANSFORM;
begin
  inherited;
  if session.IsInteractiveSession then begin
    DC_WorkfFlowSchemes   := session.NewDerivedCollection('workflowschemes_s');
    DC_WorkfFlowsFiltered := session.NewDerivedCollection('workflowschemes_f');

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,CLS_WF_SchemesTransform);

    CLS_WF_SchemesTransform.AddOneToOnescheme   ('objname','wf_key','Workflow Key');
    CLS_WF_SchemesTransform.AddDBTextShortToOne ('desc','wf_short','Workflow Short');
    CLS_WF_SchemesTransform.AddDBTextLongToOne  ('desc','wf_long','Workflow Long');
    CLS_WF_SchemesTransform.AddDBTextHintToOne  ('desc','wf_desc','Description');
    CLS_WF_SchemesTransform.AddFulltextFilterOnTransformed(TFRE_DB_StringArray.Create('wf_key','wf_short','wf_long','wf_desc')); //TODO:IMPLEMENT

    DC_WorkfFlowSchemes.SetDeriveParent         (session.GetDBConnection.Collection('SysWorkflowSchemes'));
    DC_WorkfFlowSchemes.SetDeriveTransformation (CLS_WF_SchemesTransform);
    DC_WorkfFlowSchemes.SetDisplayType          (cdt_Listview,[cdgf_ShowSearchbox],'Workflows');

    DC_WorkfFlowsFiltered.SetDeriveParent         (session.GetDBConnection.Collection('SysWorkflowSchemes'));
    DC_WorkfFlowsFiltered.SetDeriveTransformation (CLS_WF_SchemesTransform);
    DC_WorkfFlowsFiltered.SetDisplayType          (cdt_Listview,[cdgf_Sortable],'Filtered');

    CLS_wf_Content := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
    CLS_wf_Content.AddSection.Describe(DC_WorkfFlowSchemes.GetDisplayDescriptionFunction,'Workflow Types',1);
    CLS_wf_Content.AddSection.Describe(DC_WorkfFlowSchemes.GetDisplayDescriptionFunction('RUN_WF'),'Running',2);
    CLS_wf_Content.AddSection.Describe(DC_WorkfFlowSchemes.GetDisplayDescriptionFunction('ERR_WF'),'Failed',3);
    CLS_wf_Content.AddSection.Describe(DC_WorkfFlowSchemes.GetDisplayDescriptionFunction('DONE_WF'),'Done',4);
    CLS_wf_Content.AddSection.Describe(DC_WorkfFlowSchemes.GetDisplayDescriptionFunction('LOG_WF'),'Logged',5);

    //CLS_wf_Content.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(self,'RunningWFs'),'Running',1);
    //CLS_wf_Content.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(self,'FailedWFs'),'Failed',1);
    //CLS_wf_Content.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(self,'FinishedWFs'),'Finished',2);
  end;
end;

function TFRE_DB_WF.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := CLS_wf_Content;
end;






{ TFRE_DB_Site_Captive_Extension }

class procedure TFRE_DB_Site_Captive_Extension.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('site',fdbft_ObjLink);
  scheme.AddSchemeField('captiveportal',fdbft_ObjLink);
end;

{ TFRE_DB_Captiveportal }

class procedure TFRE_DB_Captiveportal.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('vpn_caid',fdbft_ObjLink);
end;

function TFRE_DB_Captiveportal.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_Captiveportal.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

{ TFRE_DB_REDIRECTION_FLOW }

class procedure TFRE_DB_REDIRECTION_FLOW.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('redirection_start',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_customer',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb_ipad',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb_open',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_end',fdbft_ObjLink);
end;


class procedure TFRE_DB_NETWORK_GROUP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('networks',fdbft_ObjLink).multiValues:=true;
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_NETWORK_GROUP_main_group');
  group.AddInput('objname','$scheme_TFRE_DB_NETWORK_GROUP_name');
  group.AddInput('networks','$scheme_TFRE_DB_NETWORK_GROUP_networks');
end;

class procedure TFRE_DB_NETWORK_GROUP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_GROUP_main_group','Network Group'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_GROUP_name','Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_GROUP_networks','Networks'));
end;

class procedure TFRE_DB_ROUTE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('subnet',fdbft_String).required:=true;
  scheme.AddSchemeField('gateway',fdbft_String).required:=true;
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_ROUTE_main_group');
  group.AddInput('subnet','$scheme_TFRE_DB_ROUTE_subnet');
  group.AddInput('gateway','$scheme_TFRE_DB_ROUTE_gateway');
end;

class procedure TFRE_DB_ROUTE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ROUTE_main_group','General Information'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ROUTE_subnet','Subnet'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ROUTE_gateway','Gateway'));
end;

class procedure TFRE_DB_CMS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('baseurl',fdbft_String).required:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  group := scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_CMS_main_group');
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('baseurl','$scheme_TFRE_DB_CMS_baseurl');
  group.AddInput('urlexceptions','$scheme_TFRE_DB_CMS_execeptions');
end;

class procedure TFRE_DB_CMS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_main_group','CMS'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_baseurl','Base URL'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_execeptions','Url Exceptions'));
end;



class procedure TFRE_DB_DEVICE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField('provisioningmac',fdbft_String).SetupFieldDef(true,false,'','mac');
  scheme.AddSchemeField('provisioning_serial',fdbft_Int32);
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_DEVICE_main_group');
  group.AddInput('provisioningmac','$scheme_TFRE_DB_DEVICE_pmac');
end;

class procedure TFRE_DB_DEVICE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DEVICE_main_group','General Information'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DEVICE_pmac','Mac'));
end;

class procedure TFRE_DB_CMS_ADPAGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_CMS_PAGE');
  scheme.AddSchemeField('start_time',fdbft_DateTimeUTC).required:=true;
  scheme.AddSchemeField('end_time',fdbft_DateTimeUTC).required:=true;
  scheme.AddSchemeField('start_daily',fdbft_String).required:=true;
  scheme.AddSchemeField('end_daily',fdbft_String).required:=true;
  scheme.AddSchemeField('insertpoint',fdbft_Int16).required:=true;
  scheme.AddSchemeField('max_inserts',fdbft_UInt32).required:=true;
  scheme.AddSchemeField('shown_inserts',fdbft_UInt32);
  scheme.AddSchemeField('networkgroups',fdbft_ObjLink).multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup('Page');
  group.UseInputGroup('TFRE_DB_CMS_PAGE','main');
  group.AddInput('start_time','$scheme_TFRE_DB_CMS_ADPAGE_starttime');
  group.AddInput('end_time','$scheme_TFRE_DB_CMS_ADPAGE_endtime');
  group.AddInput('start_daily','$scheme_TFRE_DB_CMS_ADPAGE_startdaily');
  group.AddInput('end_daily','$scheme_TFRE_DB_CMS_ADPAGE_enddaily');
  group.AddInput('insertpoint','$scheme_TFRE_DB_CMS_ADPAGE_insertpoint');
  group.AddInput('max_inserts','$scheme_TFRE_DB_CMS_ADPAGE_max_inserts');
  group.AddInput('shown_inserts','$scheme_TFRE_DB_CMS_ADPAGE_show_inserts',true);
  group.AddInput('networkgroups','$scheme_TFRE_DB_CMS_ADPAGE_networkgroups');
end;

class procedure TFRE_DB_CMS_ADPAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_starttime','Start Time'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_endtime','End Time'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_startdaily','Start Daily'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_enddaily','End Daily'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_insertpoint','Insertion Point'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_max_inserts','Maximum Insertion Count'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_show_inserts','Already Shown Inserts'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_ADPAGE_networkgroups','Network Groups'));
end;

class procedure TFRE_DB_WPA2Network.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
  scheme.AddSchemeField('wpa2psk',fdbft_String).required:=true;
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_WPA2NETWORK_main_group');
  group.UseInputGroup('TFRE_DB_WIFINETWORK','main');
  group.AddInput('wpa2psk','$scheme_TFRE_DB_WPA2NETWORK_wpa2psk');
end;

class procedure TFRE_DB_WPA2Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_WPA2NETWORK_main_group','WPA2 Wifi Network'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_WPA2NETWORK_wpa2psk','WPA2PSK'));
end;


{ TFRE_DB_CMS_PAGE }

class procedure TFRE_DB_CMS_PAGE.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('cms',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('active',fdbft_Boolean).required:=true;
  scheme.AddSchemeField('relativeurl',fdbft_Boolean).required:=true;
  scheme.AddSchemeField('url',fdbft_String).required:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  scheme.SetSysDisplayField(GFRE_DBI.ConstructStringArray(['url']),'%s');

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_CMS_PAGE_main_group');
  group.AddInput('cms','',false,true);
  group.AddInput('active','$scheme_TFRE_DB_CMS_PAGE_active');
  group.AddInput('relativeurl','$scheme_TFRE_DB_CMS_PAGE_relurl');
  group.AddInput('url','$scheme_TFRE_DB_CMS_PAGE_url');
  group.AddInput('urlexceptions','$scheme_TFRE_DB_CMS_PAGE_exceptions');
end;

class procedure TFRE_DB_CMS_PAGE.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_PAGE_main_group','Page'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_PAGE_active','Active'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_PAGE_relurl','Relative Url'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_PAGE_url','Url'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CMS_PAGE_exceptions','Url Exceptions'));
end;

function TFRE_DB_CMS_PAGE.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('CMS Page');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_CMS_PAGE.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_cms_page.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

{ TFRE_DB_SIAC_CMS }

class procedure TFRE_DB_SIAC_CMS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_SIAC_CMS.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('siac_cms','$cms_description');
end;

procedure TFRE_DB_SIAC_CMS.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
    DC_VIRT_NODES : IFRE_DB_DERIVED_COLLECTION;
    entry         : IFRE_DB_Object;
begin
  inherited;
  if session.IsInteractiveSession then begin
    DC_CMS_Pages := Session.NewDerivedCollection('cms_pages');
    with DC_CMS_PAGES do begin
      SetDeriveParent(session.GetDBConnection.Collection('cmspage'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Functions',TFRE_DB_StringArray.create('url'));
      AddSchemeFilter('SCH',TFRE_DB_StringArray.Create('TFRE_DB_CMS_ADPAGE'),true);
    end;

    DC_AD_Pages := Session.NewDerivedCollection('ad_pages');
    with DC_AD_Pages do begin
      SetDeriveParent(session.GetDBConnection.Collection('cmspage'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Functions',TFRE_DB_StringArray.create('url'));
      AddSchemeFilter('SCH',TFRE_DB_StringArray.Create('TFRE_DB_CMS_ADPAGE'),false);
    end;

    DC_VIRT_NODES := Session.NewDerivedCollection('Rootnodes');
    DC_VIRT_NODES.SetDisplayType(cdt_Treeview,[],'Functions',TFRE_DB_StringArray.create('objname','url'));
    DC_VIRT_NODES.SetVirtualMode;

    DC_VIRT_NODES.AddVirtualChildEntry('Content Pages','TFRE_DB_SIAC_CMS',Self.GetUIDPath,'ChildrenDataCP','Content','MENUCP');
    DC_VIRT_NODES.AddVirtualChildEntry('AD Pages','TFRE_DB_SIAC_CMS',Self.GetUIDPath,'ChildrenDataAD','Content','MENUAD');
  end;
end;

function TFRE_DB_SIAC_CMS.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var  DC_ROOT_NODES : IFRE_DB_DERIVED_COLLECTION;
begin
  DC_ROOT_NODES := GetSession(input).FetchDerivedCollection('Rootnodes');
  CMS_Layout :=TFRE_DB_LAYOUT_DESC.create.Describe();
  CMS_Layout.AddSection(DC_ROOT_NODES.GetDisplayDescription,lt_left);
  CMS_Layout.setContentSize(4);
  result := CMS_Layout;
end;

function TFRE_DB_SIAC_CMS.IMI_MenuAD(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res            : TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add AD Page','images_apps/cloudcontrol/add_ad_page.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addADPage'));
  Result:=res;
end;

function TFRE_DB_SIAC_CMS.IMI_MenuCP(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res            : TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add CMS Page','images_apps/cloudcontrol/add_cms_page.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addCMSPage'));
  Result:=res;
end;

function TFRE_DB_SIAC_CMS.IMI_ChildrenDataAD(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=DC_AD_Pages.IMI_GET_CHILDREN_DATA(input);
end;

function TFRE_DB_SIAC_CMS.IMI_ChildrenDataCP(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=DC_CMS_Pages.IMI_GET_CHILDREN_DATA(input);
end;

function TFRE_DB_SIAC_CMS.IMI_addADPage(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_DIALOG_DESC;
  scheme: IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection(input).GetScheme('TFRE_DB_CMS_ADPAGE',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add AD Page');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));

  res.SetElementValue('cms',GetDBSessionData(input).Field('activeCMS').AsString);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_CMS_ADPAGE','newOperation');
  serverFunc.AddParam.Describe('collection','cmspage');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_SIAC_CMS.IMI_addCMSPage(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection(input).GetScheme('TFRE_DB_CMS_PAGE',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add CMS Page');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));

  res.SetElementValue('cms',GetDBSessionData(input).Field('activeCMS').AsString);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_CMS_PAGE','newOperation');
  serverFunc.AddParam.Describe('collection','cmspage');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

{ TFRE_DB_Routing }

class procedure TFRE_DB_Routing.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('default',fdbft_String).required:=true;
  scheme.AddSchemeFieldSubscheme('static','TFRE_DB_ROUTE').multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_ROUTING_main_group');
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('default','$scheme_TFRE_DB_ROUTING_default');
end;

class procedure TFRE_DB_Routing.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ROUTING_main_group','Routing'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ROUTING_default','Default Routing'));
end;

function TFRE_DB_Routing.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_Routing.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

{ TFRE_DB_Radius }

class procedure TFRE_DB_Radius.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
end;

function TFRE_DB_Radius.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_Radius.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

{ TFRE_DB_VPN }

class procedure TFRE_DB_VPN.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  Scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
end;

function TFRE_DB_VPN.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

function TFRE_DB_VPN.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
 // result := nil;
end;

{ TFRE_DB_DHCP_Fixed }

class procedure TFRE_DB_DHCP_Fixed.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('dhcp',fdbft_ObjLink).required:=true;
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('mac',fdbft_String).required:=true;
  scheme.AddSchemeField('ip',fdbft_String).required:=true;
  scheme.AddSchemeField('router',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('dns',fdbft_String).multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_DHCP_FIXED_main_group');
  group.AddInput('dhcp','',false,true);
  group.AddInput('objname','$scheme_TFRE_DB_DHCP_FIXED_objname');
  group.AddInput('mac','$scheme_TFRE_DB_DHCP_FIXED_mac');
  group.AddInput('ip','$scheme_TFRE_DB_DHCP_FIXED_ip');
  group.AddInput('router','$scheme_TFRE_DB_DHCP_FIXED_router');
  group.AddInput('dns','$scheme_TFRE_DB_DHCP_FIXED_dns');
end;

class procedure TFRE_DB_DHCP_Fixed.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_main_group','DHCP Fixed'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_objname','Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_mac','Mac'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_ip','Ip'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_router','Router'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_FIXED_dns','DNS'));
end;

function TFRE_DB_DHCP_Fixed.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Fixed Host');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP_Fixed.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_fixed_dhcp.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

{ TFRE_DB_DHCP_Subnet }

class procedure TFRE_DB_DHCP_Subnet.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('dhcp',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('subnet',fdbft_String).required:=true;
  scheme.AddSchemeField('range_start',fdbft_String).required:=true;
  scheme.AddSchemeField('range_end',fdbft_String).required:=true;
  scheme.AddSchemeField('router',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('dns',fdbft_String).multiValues:=true;

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_DHCP_SUBNET_main_group');
  group.AddInput('dhcp','',false,true);
  group.AddInput('subnet','$scheme_TFRE_DB_DHCP_SUBNET_subnet');
  group.AddInput('range_start','$scheme_TFRE_DB_DHCP_SUBNET_range_start');
  group.AddInput('range_end','$scheme_TFRE_DB_DHCP_SUBNET_range_end');
  group.AddInput('router','$scheme_TFRE_DB_DHCP_SUBNET_router');
  group.AddInput('dns','$scheme_TFRE_DB_DHCP_SUBNET_dns');
end;

class procedure TFRE_DB_DHCP_Subnet.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_main_group','DHCP Subnet'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_subnet','Subnet'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_range_start','Range Start'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_range_end','Range End'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_router','Router'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_SUBNET_dns','DNS'));
end;

function TFRE_DB_DHCP_Subnet.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Subnet');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP_Subnet.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_subnet.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

{ TFRE_DB_DHCP }

class procedure TFRE_DB_DHCP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.AddSchemeField('default_domainname',fdbft_String).required:=true;
  scheme.AddSchemeField('default_dns',fdbft_String).required:=true;
  scheme.AddSchemeField('default_leasetime',fdbft_Int16).required:=true;
  scheme.AddSchemeField('fixed_start',fdbft_String).required:=true;
  scheme.AddSchemeField('fixed_end',fdbft_String).required:=true;


  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_DHCP_main_group');
  group.UseInputGroup('TFRE_DB_SERVICE','main');
  group.AddInput('default_domainname','$scheme_TFRE_DB_DHCP_default_domainname');
  group.AddInput('default_dns','$scheme_TFRE_DB_DHCP_default_domainname');
  group.AddInput('default_leasetime','$scheme_TFRE_DB_DHCP_default_leasetime');
  group.AddInput('fixed_start','$scheme_TFRE_DB_DHCP_fixed_start');
  group.AddInput('fixed_end','$scheme_TFRE_DB_DHCP_fixed_end');
end;

class procedure TFRE_DB_DHCP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_main_group','General Information'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_default_domainname','Default Domainname'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_default_dns','Default DNS'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_default_leasetime','Default Leasetime'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_fixed_start','Begin of fixed addresses'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_DHCP_fixed_end','End of fixed addresses'));
end;

function TFRE_DB_DHCP.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('DHCP Service');
  res.AddSchemeFormGroup(getscheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Subnet','images_apps/cloudcontrol/add_subnet.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addSubnet'));
  res.AddEntry.Describe('Add Fixed Host','images_apps/cloudcontrol/add_fixed_host.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addFixedHost'));
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_ChildrenData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;
  txt   : String;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  childs:=ReferencedByList;
  for i := 0 to Length(childs) - 1 do begin
    getDBConnection.Fetch(childs[i],dbo);
    if dbo.IsA('TFRE_DB_DHCP_SUBNET') or dbo.IsA('TFRE_DB_DHCP_FIXED') then begin
      if dbo.IsA('TFRE_DB_DHCP_SUBNET') then begin
        txt:=dbo.field('subnet').AsString;
      end else begin
        txt:=dbo.field('objname').AsString;
      end;

      entry:=GFRE_DBI.NewObject;
      entry.Field('text').AsString:=txt;
      entry.Field('uid').AsGUID:=dbo.UID;
      entry.Field('uidpath').AsStringArr:=dbo.GetUIDPath;
      entry.Field('_funcclassname_').AsString:=dbo.SchemeClass;
      entry.Field('_childrenfunc_').AsString:='ChildrenData';
      entry.Field('_menufunc_').AsString:='Menu';
      entry.Field('_contentfunc_').AsString:='Content';
      res.addEntry(entry);

    end;
  end;
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_addSubnet(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       :TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection.GetScheme('TFRE_DB_DHCP_SUBNET',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_SUBNET','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_subnet');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_DHCP.IMI_addFixedHost(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection.GetScheme('TFRE_DB_DHCP_FIXED',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Subnet');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('dhcp',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_DHCP_FIXED','newOperation');
  serverFunc.AddParam.Describe('collection','dhcp_fixed');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;



{ TFRE_DB_Certificate }

class procedure TFRE_DB_Certificate.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('ca',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('cn',fdbft_String).required:=true;
  scheme.AddSchemeField('c',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('st',fdbft_String);
  scheme.AddSchemeField('l',fdbft_String);
  scheme.AddSchemeField('ou',fdbft_String);
  scheme.AddSchemeField('crt',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoked',fdbft_DateTimeUTC);
  scheme.AddSchemeField('valid',fdbft_DateTimeUTC);
  scheme.AddSchemeField('revoke',fdbft_Boolean);
  scheme.SetSysDisplayField(GFRE_DBI.ConstructStringArray(['cn']),'%s');

  group:=scheme.AddInputGroup('main_create').Setup('$scheme_TFRE_DB_CERTIFICATE_main_group');
  group.AddInput('ca','',false,true);
  group.AddInput('cn','$scheme_TFRE_DB_CERTIFICATE_cn');
  group.AddInput('c','$scheme_TFRE_DB_CERTIFICATE_c');
  group.AddInput('email','$scheme_TFRE_DB_CERTIFICATE_email');
  group.AddInput('st','$scheme_TFRE_DB_CERTIFICATE_st');
  group.AddInput('l','$scheme_TFRE_DB_CERTIFICATE_l');
  group.AddInput('ou','$scheme_TFRE_DB_CERTIFICATE_ou');

  group:=scheme.AddInputGroup('main_edit').Setup('$scheme_TFRE_DB_CERTIFICATE_main_group');
  group.UseInputGroup(scheme.DefinedSchemeName,'main_create');
  group.AddInput('revoke','$scheme_TFRE_DB_CERTIFICATE_revoke',true);
  group.AddInput('issued','$scheme_TFRE_DB_CERTIFICATE_issued',true);
  group.AddInput('revoked','$scheme_TFRE_DB_CERTIFICATE_revoked',true);
  group.AddInput('valid','$scheme_TFRE_DB_CERTIFICATE_valid',true);
  group.AddInput('crt','$scheme_TFRE_DB_CERTIFICATE_crt');
  group.AddInput('key','$scheme_TFRE_DB_CERTIFICATE_key');
end;

class procedure TFRE_DB_Certificate.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_main_group','Certificate'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_cn','Common Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_c','Country'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_email','EMail'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_st','State'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_l','Location'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_ou','Organization Unit'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_revoke','Revoke'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_issued','Issued'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_revoked','Revoked'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_valid','Valid'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_crt','Certificate'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CERTIFICATE_key','Key'));
end;

function TFRE_DB_Certificate.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Revoke Certificate','images_apps/cloudcontrol/revoke_certificate.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'revoke'));
  Result:=res;
end;

function TFRE_DB_Certificate.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    :TFRE_DB_FORM_PANEL_DESC;
  scheme :IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Certificate Authority');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

{ TFRE_DB_CA }

class procedure TFRE_DB_CA.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_SERVICE');
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('cn',fdbft_String).required:=true;
  scheme.AddSchemeField('c',fdbft_String);
  scheme.AddSchemeField('email',fdbft_String);
  scheme.AddSchemeField('st',fdbft_String);
  scheme.AddSchemeField('l',fdbft_String);
  scheme.AddSchemeField('o',fdbft_String);
  scheme.AddSchemeField('ou',fdbft_String);
  scheme.AddSchemeField('crt',fdbft_String);
  scheme.AddSchemeField('key',fdbft_String);
  scheme.AddSchemeField('pass',fdbft_String);
  scheme.AddSchemeField('issued',fdbft_DateTimeUTC);

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_CA_main_group');
  group.AddInput('objname','$scheme_TFRE_DB_CA_objname');
  group.AddInput('cn','$scheme_TFRE_DB_CA_cn');
  group.AddInput('c','$scheme_TFRE_DB_CA_c');
  group.AddInput('email','$scheme_TFRE_DB_CA_email');
  group.AddInput('st','$scheme_TFRE_DB_CA_st');
  group.AddInput('l','$scheme_TFRE_DB_CA_l');
  group.AddInput('o','$scheme_TFRE_DB_CA_o');
  group.AddInput('ou','$scheme_TFRE_DB_CA_ou');
  group.AddInput('crt','$scheme_TFRE_DB_CA_crt');
  group.AddInput('key','$scheme_TFRE_DB_CA_key');
  group.AddInput('pass','$scheme_TFRE_DB_CA_pass');
  group.AddInput('issued','$scheme_TFRE_DB_CA_issued',true);
end;

class procedure TFRE_DB_CA.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_main_group','Certificate Authority'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_objname','Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_cn','Common Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_c','Country'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_email','EMail'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_st','State'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_l','Location'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_o','Organization Unit'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_ou','Organization Unit'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_crt','Certificate'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_key','Key'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_pass','Password'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_CA_issued','Issued'));
end;

function TFRE_DB_CA.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Certificate','images_apps/cloudcontrol/add_certificate.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addCertificate'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_ca.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

function TFRE_DB_CA.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Certificate Authority');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_CA.IMI_ChildrenData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  childs:=ReferencedByList;
  for i := 0 to Length(childs) - 1 do begin
    GetDBConnection.Fetch(childs[i],dbo);
    if dbo.IsA('TFRE_DB_CERTIFICATE') then begin
      entry:=GFRE_DBI.NewObject;
      entry.Field('text').AsString:=dbo.field('cn').AsString;
      entry.Field('uid').AsGUID:=dbo.UID;
      entry.Field('uidpath').AsStringArr:=dbo.GetUIDPath;
      entry.Field('_funcclassname_').AsString:=dbo.SchemeClass;
      entry.Field('_childrenfunc_').AsString:='ChildrenData';
      entry.Field('_menufunc_').AsString:='Menu';
      entry.Field('_contentfunc_').AsString:='Content';
      res.addEntry(entry);
    end;
  end;
  Result:=res;
end;

function TFRE_DB_CA.IMI_AddCertificate(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  scheme    : IFRE_DB_SchemeObject;
  res       : TFRE_DB_DIALOG_DESC;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection.GetScheme('TFRE_DB_Certificate',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Certificate');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_create'),GetSession(input));
  res.SetElementValue('ca',UID_String);
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_Certificate','newOperation');
  serverFunc.AddParam.Describe('collection','certificate');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;


{ TFRE_DB_WifiNetwork }

class procedure TFRE_DB_WifiNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_NETWORK');
  scheme.AddSchemeField('ssid',fdbft_String).required:=true;
  scheme.AddSchemeField('hidden',fdbft_Boolean);
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_WIFINETWORK_main_group');
  group.AddInput('ssid','$scheme_TFRE_DB_WIFINETWORK_ssid');
  group.AddInput('hidden','$scheme_TFRE_DB_WIFINETWORK_hidden');
  group.UseInputGroup('TFRE_DB_NETWORK','main');
end;

class procedure TFRE_DB_WifiNetwork.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_WIFINETWORK_main_group','Wifi Network'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_WIFINETWORK_ssid','SSID'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_WIFINETWORK_hidden','Hidden Network'));
end;

function TFRE_DB_WifiNetwork.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=inherited IMI_Content(input);
end;

class procedure TFRE_DB_RadiusNetwork.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_WIFINETWORK');
  scheme.AddSchemeField('caid',fdbft_ObjLink).required:=false; //TODO FRANZ
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_RADIUSNETWORK_main_group');
  group.UseInputGroup('TFRE_DB_WIFINETWORK','main');
  group.AddInput('caid','$scheme_TFRE_DB_RADIUSNETWORK_caid');
end;

class procedure TFRE_DB_RadiusNetwork.InstallDBObjects( const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_RADIUSNETWORK_main_group','Radius Wifi Network'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_RADIUSNETWORK_caid','CAID'));
end;

function TFRE_DB_RadiusNetwork.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=inherited IMI_Content(input);
end;

class procedure TFRE_DB_Network.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('endpoint',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('ip_net',fdbft_String).SetupFieldDef(true,false,'','ip');
  scheme.AddSchemeField('dns',fdbft_String);
  scheme.AddSchemeField('dhcp',fdbft_Boolean);
  scheme.AddSchemeField('dhcp_range_start',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_range_end',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_leasetime',fdbft_UInt16);
  scheme.AddSchemeField('dhcp_parameters',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('urlexceptions',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('redirection_start',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_customer',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_agb',fdbft_ObjLink);
  scheme.AddSchemeField('redirection_end',fdbft_ObjLink);
  scheme.AddSchemeField('sessiontimeout',fdbft_UInt32);
  scheme.AddSchemeField('vlan_id',fdbft_Uint16);

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_NETWORK_main_group');
  group.AddInput('endpoint','',false,true);
  group.AddInput('ip_net','$scheme_TFRE_DB_NETWORK_ip_net');
  group.AddInput('dns','$scheme_TFRE_DB_NETWORK_dns');
  group.AddInput('dhcp','$scheme_TFRE_DB_NETWORK_dhcp');
  group.AddInput('dhcp_range_start','$scheme_TFRE_DB_NETWORK_dhcp_range_start');
  group.AddInput('dhcp_range_end','$scheme_TFRE_DB_NETWORK_dhcp_range_end');
  group.AddInput('dhcp_leasetime','$scheme_TFRE_DB_NETWORK_dhcp_leasetime');
  group.AddInput('dhcp_parameters','$scheme_TFRE_DB_NETWORK_dhcp_parameters');
  group.AddInput('urlexceptions','$scheme_TFRE_DB_NETWORK_urlexceptions');
  group.AddInput('redirection_start','$scheme_TFRE_DB_NETWORK_redirection_start',false,false,'cmspage');
  group.AddInput('redirection_customer','$scheme_TFRE_DB_NETWORK_redirection_customer',false,false,'cmspage');
  group.AddInput('redirection_agb','$scheme_TFRE_DB_NETWORK_redirection_agb',false,false,'cmspage');
  group.AddInput('redirection_end','$scheme_TFRE_DB_NETWORK_redirection_end',false,false,'cmspage');
  group.AddInput('sessiontimeout','$scheme_TFRE_DB_NETWORK_sessiontimeout');
  group.AddInput('vlan_id','$scheme_TFRE_DB_NETWORK_vlan_id');
end;

class procedure TFRE_DB_Network.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_main_group','General Information'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_ip_net','Subnet'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dns','DNS'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dhcp','DHCP'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dhcp_range_start','DHCP Range Start'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dhcp_range_end','DHCP Range End'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dhcp_leasetime','DHCP Leasetime'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_dhcp_parameters','DHCP Parameters'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_urlexceptions','Url Exceptions'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_redirection_start','Redirection Start'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_redirection_customer','Redirection Customer'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_redirection_agb','Redirection AGB'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_redirection_end','Redirection End'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_sessiontimeout','Sessiontimeout'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_NETWORK_vlan_id','VLAN Identifier'));
end;

class procedure TFRE_DB_Network.NetworkOnChange(const dbc: IFRE_DB_Connection; const is_dhcp: boolean; const subnet: string; const ep_id: TGUID; const dns: string; const range_start, range_end: integer);
  var dhcp_obj        : IFRE_DB_Object;
      dhcp_id         : TGUID;
      childs          : TFRE_DB_GUIDArray;
      dhcp_subnet_obj : IFRE_DB_Object;
      i               : integer;
      colls           : IFRE_DB_Collection;
      current_ip      : string;
      highest         : integer;
      sub_ip          : integer;
      do_update       : boolean;
      routing_id      : TGUID;
      routing_obj     : IFRE_DB_Object;
      route_obj       : IFRE_DB_Object;
      ep_obj          : IFRE_DB_Object;
      gw              : string;

      procedure _setfields;
      begin
        dhcp_subnet_obj.Field('subnet').AsString:=subnet;
        dhcp_subnet_obj.Field('range_start').AsString:=GetIPDots(subnet,3)+inttostr(range_start);
        dhcp_subnet_obj.Field('range_end').AsString:=GetIPDots(subnet,3)+inttostr(range_end);
        dhcp_subnet_obj.Field('router').AsString:=GetIPDots(subnet,3)+'1';
        dhcp_subnet_obj.Field('dns').AsString:=dns;
      end;

begin
    writeln('NetworkOnChange');
    if is_dhcp then begin
      writeln('DHCP');
      dhcp_id        := GetService(dbc,'TFRE_DB_DHCP');
      if       not dbc.Fetch(dhcp_id,dhcp_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO DHCP SERVICE FOUND IN NETWORK ON CHANGE');
      childs     := dhcp_obj.ReferencedByList('TFRE_DB_DHCP_SUBNET');
      writeln('ChiLDS');

      do_update  := false;

      for i := 0 to Length(childs) - 1 do begin
        dbc.Fetch(childs[i],dhcp_subnet_obj);
        if dhcp_subnet_obj.Field('subnet').AsString=subnet then begin
          writeln('ALREADY IN DHCP, UPDATING !');
          do_update  := true;
          break;
        end;
      end;

      if do_update then begin
        _setfields;
        CheckDbResult(dbc.Update(dhcp_subnet_obj),'failure on cloned/update');
      end else begin
        colls          := dbc.Collection('dhcp_subnet');
        dhcp_subnet_obj:= dbc.NewObject('TFRE_DB_DHCP_Subnet');
        _setfields;
        dhcp_subnet_obj.Field('dhcp').AsObjectLink:=dhcp_id;
        CheckDbResult(COLLS.Store(dhcp_subnet_obj),'Add DHCP Subnet');
      end;
    end;

    // check routing
    if   not dbc.Fetch(ep_id,ep_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO EP FOUND IN NETWORK ON CHANGE');
    if   ep_obj.IsA('TFRE_DB_AP_Lancom') then begin
      // no routing
      gw := '';
    end else begin
      if ep_obj.FieldExists('vpn_crtid') then begin
        gw := '';
      end else begin
        gw := '1.2.3.4';    // get from mac / dhcp   // TODO XXXX
      end;
    end;

    if gw <>'' then begin
      do_update  := false;

      routing_id := GetService  (dbc,'TFRE_DB_ROUTING');
      if       not dbc.Fetch    (routing_id, routing_obj)          then raise EFRE_DB_Exception.Create(edb_ERROR,'NO ROUTING SERVICE FOUND IN NETWORK ON CHANGE');
      for i := 0 to routing_obj.Field('static').ValueCount-1 do begin
        route_obj   :=    routing_obj.Field('static').AsObjectItem[i];
        if route_obj.Field('subnet').AsString=subnet then begin
          writeln('ALREADY IN ROUTING, UPDATING !');
          do_update  := true;
          break;
        end;
      end;

      if do_update then begin
       route_obj.Field('gateway').AsString := '1.1.1.1';
      end else begin
       route_obj:=dbc.NewObject('TFRE_DB_Route');
       route_obj.Field('subnet').AsString:=subnet;
       route_obj.Field('gateway').AsString:='2.2.2.2';
       routing_obj.Field('static').AddObject(route_obj);
      end;
      routing_obj.Field('reprovision').AsBoolean := true;
      writeln(routing_obj.DumpToString);
      CheckDbResult (dbc.Update(routing_obj),'failure on cloned/update');
    end;

    SetReprovision(dbc,dhcp_id);
end;

class function TFRE_DB_Network.IMC_NewOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
 var
      dbc        :   IFRE_DB_CONNECTION;
      raw_object :   IFRE_DB_Object;
      new_net    :   string;
      ep_id      :   TGUID;
      dhcp       :   boolean;
      range_start:   integer;
      range_end  :   integer;
      dns        :   string;
      s          :   string;

begin
  writeln ('NETWORK NewOperation');
  writeln(input.DumpToString());
  dbc          := input.GetReference as IFRE_DB_CONNECTION;
  raw_object   := input.Field('data').AsObject;
  new_net      := raw_object.Field('ip_net').asstring;
  dhcp         := raw_object.Field('dhcp').asboolean;
  ep_id        := raw_object.Field('endpoint').AsGUID;

  s            := raw_object.Field('dhcp_range_start').Asstring;
  if s<>'' then range_start:=strtoint(s) else range_start:=20;
  s            := raw_object.Field('dhcp_range_end').Asstring;
  if s<>'' then range_start:=strtoint(s) else range_end:=20;
  dns          := raw_object.Field('dns').Asstring;

  if CheckClass(new_net)=false then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on creating! Only Class C networks are currently allowed !',fdbmt_error);
    exit;
  end;

  if UniqueNet(dbc,GUID_NULL,new_net) then begin
    writeln('UNIQUE');
  end else begin
    writeln('NOT UNIQUE');
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on creating! The network is not unique !',fdbmt_error);
    exit;
  end;

  Result:=inherited IMC_NewOperation(input);

  // set reprovision on endpoint
  SetReprovision(dbc,ep_id);
  NetworkOnChange(dbc,dhcp,new_net,ep_id,dns,range_start,range_end);
end;

function TFRE_DB_Network.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    :TFRE_DB_FORM_PANEL_DESC;
  scheme :IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Network');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Network.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_network.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

function TFRE_DB_Network.IMI_SaveOperation(const input: IFRE_DB_Object): IFRE_DB_Object;
var scheme            : IFRE_DB_SCHEMEOBJECT;
    update_object_uid : TGUid;
    raw_object        : IFRE_DB_Object;
    dbc               : IFRE_DB_CONNECTION;
    ep_id             : TGUID;
    dhcp              : boolean;
    subnet            : string;
    dns               : string;
    range_start       : integer;
    range_end         : integer;

begin
  writeln('NETWORK SAVE');
  if assigned(Parent) then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! Saving of Subobject not supported!',fdbmt_error);
    exit;
  end;
  dbc := GetDBConnection;
  result            := nil;
  scheme            := GetScheme;
  update_object_uid := UID;
  raw_object        := input.Field('data').AsObject;

  if CheckClass(raw_object.Field('ip_net').AsString)=false then begin
    result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! Only Class C networks are currently allowed !',fdbmt_error);
    exit;
  end;

  if UniqueNet(dbc,UID,raw_object.Field('ip_net').AsString)=false then begin
   result := TFRE_DB_MESSAGE_DESC.Create.Describe('SAVE','Error on saving! The network is not unique !',fdbmt_error);
   exit;
  end;

  scheme.SetObjectFieldsWithScheme(raw_object,self,false,GetDBConnection);

  ep_id             := Field('endpoint').AsGUID;
  dhcp              := Field('dhcp').AsBoolean;
  subnet            := Field('ip_net').AsString;
  range_start       := Field('dhcp_range_start').AsUInt16;
  range_end         := Field('dhcp_range_end').AsUInt16;
  dns               := Field('dns').AsString;

  CheckDbResult     (dbc.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)

  SetReprovision    (dbc,ep_id);
  NetworkOnChange   (dbc,dhcp,subnet,ep_id,dns,range_start,range_end);

  result := GFRE_DB_NIL_DESC;

end;


{ TFRE_DB_SIAC_DHCP }

class procedure TFRE_DB_SIAC_DHCP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_SIAC_DHCP.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('siac_dhcp','$dhcp_description');
end;

procedure TFRE_DB_SIAC_DHCP.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var serviceGroupId : TGuid;
    DC_DHCP        : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    serviceGroupId:=session.GetSessionGlobalData.Field('WAC_activeServiceGroup').AsGUID;
    DC_DHCP := session.NewDerivedCollection('dhcp');
    with DC_DHCP do begin
      SetDeriveParent(session.GetDBConnection.Collection('service'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Functions',TFRE_DB_StringArray.create('default_domainname','text'));
      AddUIDFieldFilter('SG','servicegroup',TFRE_DB_GUIDArray.Create(serviceGroupId),dbnf_EXACT,false);
      AddSchemeFilter('SCH',TFRE_DB_StringArray.Create('TFRE_DB_DHCP'));
    end;
  end;
end;

function TFRE_DB_SIAC_DHCP.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var DHCP_Layout : TFRE_DB_LAYOUT_DESC;
    DC_DHCP     : IFRE_DB_DERIVED_COLLECTION;
begin
  DC_DHCP := GetSession(input).FetchDerivedCollection('dhcp');
  DHCP_Layout :=TFRE_DB_LAYOUT_DESC.create.Describe();
  DHCP_Layout.AddSection(DC_DHCP.GetDisplayDescription,lt_left);
  DHCP_Layout.setContentSize(4);
  result := DHCP_Layout;
end;

class procedure TFRE_DB_SIAC_CA.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_SIAC_CA.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('siac_ca','$cert_description');
end;

procedure TFRE_DB_SIAC_CA.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var  serviceGroupId : TGuid;
     DC_CA     : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    serviceGroupId:=session.GetSessionGlobalData.Field('WAC_activeServiceGroup').AsGUID;
    DC_CA := session.NewDerivedCollection('ca');
    with DC_CA do begin
      SetDeriveParent(session.GetDBConnection.Collection('service'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Functions',TFRE_DB_StringArray.create('objname','text'));
      AddUIDFieldFilter('SG','servicegroup',TFRE_DB_GUIDArray.Create(serviceGroupId),dbnf_EXACT,false);
      AddSchemeFilter('SCH',TFRE_DB_StringArray.Create('TFRE_DB_CA'));
    end;
  end;
end;

function TFRE_DB_SIAC_CA.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var DC_CA     : IFRE_DB_DERIVED_COLLECTION;
    CA_Layout : TFRE_DB_LAYOUT_DESC;
begin
  DC_CA := GetSession(input).FetchDerivedCollection('ca');
  CA_Layout := TFRE_DB_LAYOUT_DESC.create.Describe();
  CA_Layout.AddSection(DC_CA.GetDisplayDescription,lt_left);
  CA_Layout.setContentSize(4);
  result := CA_Layout;
end;

function TFRE_DB_SIAC_CA.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res            : TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  res.AddEntry.Describe('Add Certificate Authority','images_apps/cloudcontrol/add_ca.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'addCertificateAuthority'));
  Result:=res;
end;

function TFRE_DB_SIAC_CA.IMI_addCertificateAuthority(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection(input).GetScheme('TFRE_DB_CA',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Certificate Authority');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('servicegroup',GetDBSessionData(input).Field('WAC_activeServiceGroup').AsString);

  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_CA','newOperation');
  serverFunc.AddParam.Describe('collection','service');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

{ TFRE_DB_SIAC_CAP }

class procedure TFRE_DB_SIAC_CAP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_SIAC_CAP.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('siac_cp','$cap_description');
end;

procedure TFRE_DB_SIAC_CAP.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var  res: TFRE_DB_LAYOUT_DESC;
     DC_Sites        : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    DC_Sites := session.NewDerivedCollection('custfunctions');
    with DC_Sites do begin
      SetDeriveParent(session.GetDBConnection.Collection('site'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Customers',TFRE_DB_StringArray.create('objname','text'));
    end;
  end;
end;

function TFRE_DB_SIAC_CAP.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var CAP_Layout : TFRE_DB_LAYOUT_DESC;
    DC_Sites   : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  DC_Sites := GetSession(input).FetchDerivedCollection('custfunctions');
  CAP_Layout:=TFRE_DB_LAYOUT_DESC.create.Describe();
  CAP_Layout.AddSection(DC_Sites.GetDisplayDescription,lt_left);
  CAP_Layout.setContentSize(4);
  result:=CAP_Layout;
end;

{ TFRE_DB_MONAC }
class procedure TFRE_DB_MONAC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_MONAC.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('monac','$monitoring_description');
end;

procedure TFRE_DB_MONAC.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var dc_ap_monitoring : IFRE_DB_DERIVED_COLLECTION;
    tr_Monitoring    : IFRE_DB_SIMPLE_TRANSFORM;

begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Monitoring);
    with tr_Monitoring do begin
      AddOneToOnescheme          ('provisioningmac','','MAC');
      AddOneToOnescheme          ('external_ip','','IP');
      AddOneToOnescheme          ('displayname','','ART');
      AddMatchingReferencedField('site','objname','','Standort');
      AddMatchingReferencedField (TFRE_DB_StringArray.Create('site','customerid'),'COMPANY','','Kunde');
      AddMatchingReferencedField('status_uid','status_icon','','Icon',dt_icon);
      AddMatchingReferencedField('status_uid','provisioned_time','','ProvTime',dt_date);
      AddMatchingReferencedField('status_uid','online_time','','Last Online',dt_date);
    end;
    //Customer_Grid_LongTrans.AddOneToOnescheme ('LASTNAME');
    //Customer_Grid_LongTrans.AddCollectorscheme('%s',GFRE_DBI.ConstructStringArray(['businessphone.number']),'BUSINESSPHONE_NUMBER');
    //Customer_Grid_LongTrans.AddCollectorscheme('%s',GFRE_DBI.ConstructStringArray(['mobilephone.number']),'MOBILEPHONE_NUMBER');
    //Customer_Grid_LongTrans.AddCollectorscheme('%s %s | %5.5s (%s)',GFRE_DBI.ConstructStringArray(['MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'ADDRESS');
    //Customer_Grid_LongTrans.AddCollectorscheme('%s-%s-%s-%s-%s-%s-%s-%s-%s',GFRE_DBI.ConstructStringArray(['CUSTOMERNUMBER','FIRSTNAME','LASTNAME','COMPANY','CUSTOMERNUMBER','MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'FTX_SEARCH',true);

    dc_ap_monitoring := session.NewDerivedCollection('ap_mon');
    with dc_ap_monitoring do begin
      SetDeriveParent(session.GetDBConnection.Collection('ENDPOINT'));
      SetDeriveTransformation(tr_Monitoring);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable],'');
    end;
  end;
end;


function TFRE_DB_MONAC.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var  dc_ap_monitoring : IFRE_DB_DERIVED_COLLECTION;
     lvd_Monitoring   : TFRE_DB_VIEW_LIST_DESC;
begin
  dc_ap_monitoring := GetSession(input).FetchDerivedCollection('ap_mon');
  lvd_Monitoring := dc_ap_monitoring.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  result := lvd_Monitoring;
end;

{ TFRE_DB_SIAC }

class procedure TFRE_DB_SIAC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName  ('TFRE_DB_APPLICATION_MODULE');
  scheme.AddSchemeFieldSubscheme('siac_cp'  , 'TFRE_DB_SIAC_CAP');
  scheme.AddSchemeFieldSubscheme('siac_ca'  , 'TFRE_DB_SIAC_CA');
  scheme.AddSchemeFieldSubscheme('siac_dhcp', 'TFRE_DB_SIAC_DHCP');
  scheme.AddSchemeFieldSubscheme('siac_cms' , 'TFRE_DB_SIAC_CMS');
end;

procedure TFRE_DB_SIAC.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('siac','$siac_description');
  AddApplicationModule(TFRE_DB_SIAC_CAP.create);
  AddApplicationModule(TFRE_DB_SIAC_CA.create);
  AddApplicationModule(TFRE_DB_SIAC_DHCP.create);
  AddApplicationModule(TFRE_DB_SIAC_CMS.create);
end;

procedure TFRE_DB_SIAC.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var Customer_Grid_ShortTrans : IFRE_DB_SIMPLE_TRANSFORM;
    DC_Customer_Grid_Short   : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,Customer_Grid_ShortTrans);
    with Customer_Grid_ShortTrans do begin
      AddOneToOnescheme ('COMPANY','','Schnarch');
      AddOneToOnescheme ('CUSTOMERNUMBER','','ZwerG');
      AddCollectorscheme('%s %s | %5.5s (%s)',GFRE_DBI.ConstructStringArray(['MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'ADDRESS',false,'Elvis');
      AddCollectorscheme('%s-%s-%s-%s-%s-%s',GFRE_DBI.ConstructStringArray(['COMPANY','CUSTOMERNUMBER','MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'FTX_SEARCH',true);
    end;

    DC_Customer_Grid_Short := session.NewDerivedCollection('cust_page_short');
    with DC_Customer_Grid_Short do begin
      SetDeriveParent(session.GetDBConnection.Collection('customer'));
      SetDeriveTransformation(Customer_Grid_ShortTrans);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox],'Customers',nil,'',TFRE_DB_SERVER_FUNC_DESC.create.Describe('TFRE_DB_CUSTOMER','menu'));
      //TODO:CHRIS -> SetDisplayType(cdt_Listview,[cdf_ShowSearchbox,cdf_ShowCheckBoxSelection,cdf_Paging],'Customers');
    end;
  end;
end;


function TFRE_DB_SIAC.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var   a_tenant : TGuid;
      Customer_LV                 : TFRE_DB_VIEW_LIST_DESC;
      DC_Customer_Grid_Short      : IFRE_DB_DERIVED_COLLECTION;
      DC_Filter_Coll              : IFRE_DB_DERIVED_COLLECTION;

begin
  a_tenant :=  GetDBSessionData(input).Field('WAC_activeTenant').AsGUID;
  DC_Customer_Grid_Short :=  GetSession(input).FetchDerivedCollection('cust_page_short');
  Customer_LV := DC_Customer_Grid_Short.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  Customer_LV.AddButton.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_TENANT',a_tenant,'addcustomer'),'images_apps/cloudcontrol/add_customer.png','Add','Add Customer');
  DC_Filter_Coll :=  GetSession(input).FetchDerivedCollection('custfunctions');
  Customer_LV.addFilterEvent(DC_Filter_Coll.getDescriptionStoreId,'customerId');
  result := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(Customer_LV,GetAppModulesAsSubSection(input),nil);
end;

{ TFRE_DB_BAC }

class procedure TFRE_DB_BAC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_BAC.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('bac','$business_description')
end;

procedure TFRE_DB_BAC.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var Customer_Grid_LongTrans : IFRE_DB_SIMPLE_TRANSFORM;
    DC_Customer_Grid_Long   : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.LogInfo(dblc_APPLICATION,'--- BAC INIT---');
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,Customer_Grid_LongTrans);
    with Customer_Grid_LongTrans do begin
      AddOneToOnescheme ('CUSTOMERNUMBER','','Number');
      AddOneToOnescheme ('COMPANY','','Company');
      AddOneToOnescheme ('FIRSTNAME','','Firstname');
      AddOneToOnescheme ('LASTNAME','','Lastname');
      AddCollectorscheme('%s',GFRE_DBI.ConstructStringArray(['businessphone.number']),'BUSINESSPHONE_NUMBER',false,'Business Phone');
      AddCollectorscheme('%s',GFRE_DBI.ConstructStringArray(['mobilephone.number']),'MOBILEPHONE_NUMBER',false,'Mobile Phone');
      AddCollectorscheme('%s %s | %s (%s)',GFRE_DBI.ConstructStringArray(['MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'ADDRESS',false,'Address');
      AddCollectorscheme('%s-%s-%s-%s-%s-%s-%s-%s-%s',GFRE_DBI.ConstructStringArray(['CUSTOMERNUMBER','FIRSTNAME','LASTNAME','COMPANY','CUSTOMERNUMBER','MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'FTX_SEARCH',true);
    end;

    DC_Customer_Grid_Long := session.NewDerivedCollection('cust_p_long');
    with DC_Customer_Grid_Long do begin
      SetDeriveParent(session.GetDBConnection.Collection('customer'));
      SetDeriveTransformation(Customer_Grid_LongTrans);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_Sortable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_ColumnDragable],'Customers',nil,'',TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_CUSTOMER','menu'));
    end;
  end;
end;

function TFRE_DB_BAC.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var lvd_BusinessGrid       : TFRE_DB_VIEW_LIST_DESC;
    DC_Customer_Grid_Long  : IFRE_DB_DERIVED_COLLECTION;
    a_tenant               : TGuid;
begin
  a_tenant := GetDBSessionData(input).Field('WAC_activeTenant').AsGUID;
  DC_Customer_Grid_Long := GetSession(input).FetchDErivedCollection('cust_p_long');
  lvd_BusinessGrid := DC_Customer_Grid_Long.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lvd_BusinessGrid.AddButton.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_TENANT',a_tenant,'addcustomer'),'images_apps/cloudcontrol/add_customer.png','Add','Add Customer');
  result := lvd_BusinessGrid;
end;


{ TFRE_DB_WAC }

procedure TFRE_DB_WAC.SetupApplicationStructure;
begin
  InitAppDesc('wac','$description');
  AddApplicationModule(TFRE_DB_BAC.create);
  AddApplicationModule(TFRE_DB_SIAC.create);
  AddApplicationModule(TFRE_DB_MONAC.create);
  AddApplicationModule(TFRE_DB_WF.create);
end;

function TFRE_DB_WAC.InstallAppDefaults(const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;
var admin_app_rg  : IFRE_DB_ROLE;
     user_app_rg  : IFRE_DB_ROLE;
     guest_app_rg : IFRE_DB_ROLE;
     old_version  : TFRE_DB_String;
begin
  case _CheckVersion(conn,old_version) of
    NotInstalled : begin
                      _SetAppdataVersion(conn,_ActualVersion);
                      admin_app_rg  := _CreateAppRole('ADMIN','WAC ADMIN','Captive Controller Administration Rights');
                      user_app_rg   := _CreateAppRole('USER','WAC USER','Captive Controller Default User Rights');
                      guest_app_rg  := _CreateAppRole('GUEST','WAC USER','Captive Controller Default User Rights');
                      _AddAppRight(admin_app_rg,'ADMIN','WAC Admin','Administration of Captive Controller');
                      _AddAppRight(user_app_rg ,'START','WAC Start','Startup of Captive Controller');
                      _AddAppRight(guest_app_rg ,'START','WAC Start','Startup of Captive Controller');
                      conn.StoreRole(ObjectName,cSYS_DOMAIN,admin_app_rg);
                      conn.StoreRole(ObjectName,cSYS_DOMAIN,guest_app_rg);
                      conn.StoreRole(ObjectName,cSYS_DOMAIN,user_app_rg);

                      _AddSystemGroups(conn,cSYS_DOMAIN);

                      CreateAppText(conn,'$description','Captive Portal','Captive Portal','Captive Portal');
                      CreateAppText(conn,'$wf_description','Workflows','Workflows','Workflows');
                      CreateAppText(conn,'$cms_description','CMS','CMS','CMS');
                      CreateAppText(conn,'$dhcp_description','DHCP','DHCP','DHCP');
                      CreateAppText(conn,'$cert_description','Certificate','Certificate','Certificate');
                      CreateAppText(conn,'$cap_description','Captive Portal','Captive Portal','Captive Portal');
                      CreateAppText(conn,'$monitoring_description','Monitoring','Monitoring','Monitoring');
                      CreateAppText(conn,'$siac_description','Administration','Administration','Administration');
                      CreateAppText(conn,'$business_description','Business','Business','Business');
                   end;
    SameVersion  : begin
                      writeln('Version '+old_version+' already installed');
                   end;
    OtherVersion : begin
                      writeln('Old Version '+old_version+' found, updateing');
                      // do some update stuff
                      _SetAppdataVersion(conn,_ActualVersion);
                   end;
  else
    raise EFRE_DB_Exception.Create('Undefined App _CheckVersion result');
  end;
end;

procedure TFRE_DB_WAC._UpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData  : IFRE_DB_Object;
  i            : integer;

begin
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'siac','Captive Portal','images_apps/cloudcontrol/network_white.svg','',0,true);
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  Session.GetSessionAppData(ObjectName).Field('SITEMAP').AsObject := SiteMapData;
end;

class procedure TFRE_DB_WAC.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var field_def : IFRE_DB_FieldSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
  scheme.AddSchemeFieldSubscheme('bac'      , 'TFRE_DB_BAC');
  scheme.AddSchemeFieldSubscheme('siac'     , 'TFRE_DB_SIAC');
  scheme.AddSchemeFieldSubscheme('monac'    , 'TFRE_DB_MONAC');
  scheme.AddSchemeFieldSubscheme('wf'       , 'TFRE_DB_WF');
end;


procedure TFRE_DB_WAC.MySessionInitialize(const session: TFRE_DB_UserSession);
var
  coll: IFRE_DB_COLLECTION;

  function _getTenant(const obj:IFRE_DB_Object):boolean;
  begin
    Result:=false;
    if obj.SchemeClass='TFRE_DB_TENANT' then begin
      session.GetSessionGlobalData.Field('WAC_activeTenant').AsGUID:=obj.UID;
      GFRE_DBI.LogInfo(dblc_APPLICATION,'SETTING ACTIVE TENANT :::::::::: '+obj.UID_String);
      writeln('**************************   ',obj.DumpToString);
      result := true;
    end;
  end;

  function _getCMS(const obj:IFRE_DB_Object):boolean;
  begin
    result:=false;
    if obj.IsA('TFRE_DB_CMS') then begin
      session.GetSessionGlobalData.Field('WAC_activeCMS').AsGUID:=obj.UID;
      GFRE_DBI.LogInfo(dblc_APPLICATION,'SETTING ACTIVE CMS :::::::::: '+obj.UID_String);
      Result:=true;
    end;
  end;


begin
  Inherited;
  if session.IsInteractiveSession then begin
    coll:=session.GetDBConnection.Collection('tenant');
    coll.ForAllBreak(@_getTenant);
    coll:=session.GetDBConnection.Collection('service');
    coll.ForAllBreak(@_getCMS);
    session.GetSessionGlobalData.Field('WAC_activeServiceGroup').AsGUID:=DummyGetServiceGroupID(session.GetDBConnection);
    _UpdateSitemap(session);
  end;
end;

procedure TFRE_DB_WAC.MySessionPromotion(const session: TFRE_DB_UserSession);
begin
  inherited MySessionPromotion(session);
  _UpdateSitemap(session);
end;

function TFRE_DB_WAC.ShowInApplicationChooser(const session: TFRE_DB_UserSession): Boolean;
begin
  Result := True;
end;

function TFRE_DB_WAC._ActualVersion: TFRE_DB_String;
begin
  Result := '1.0';
end;


function TFRE_DB_WAC.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
//var res    : TFRE_DB_LAYOUT_DESC;
//    header : TFRE_DB_HTML_DESC;
//    footer : TFRE_DB_HTML_DESC;
begin
  result := GetAppModulesAsSubSection(input);
  //header :=  TFRE_DB_HTML_DESC.create.Describe('<div class=''firmosHeader''><div class=''firmosHeaderLeft''>Cityaccess/CityPAD Monitoring and Provisioning V1.0</div><div class=''firmosHeaderRight''></div></div>',20);
  //footer :=  TFRE_DB_HTML_DESC.create.Describe('<dive class=''firmosFooter''><div class=''firmosFooterLeft''></div><div class=''firmosFooterRight''>powered by FirmOS Business Solutions</div></div>',-1,-1,false);
  //res    :=  TFRE_DB_LAYOUT_DESC.create.Describe('',false).SetLayout(nil,GetAppModulesAsSubSection(input),nil,header,footer,false);
  //res.updateId:='FirmOSViewport';
  //res.windowCaption:='FirmOS CityAccess Captive Portal Management';
  //result :=  res;
end;

{ TFRE_DB_MobileDevice }

class procedure TFRE_DB_MobileDevice.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_DEVICE');
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('site',fdbft_ObjLink);
  scheme.AddSchemeField('crtid',fdbft_ObjLink);
  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_MOBILEDEVICE_main_group');
  group.AddInput('objname','$scheme_TFRE_DB_MOBILEDEVICE_name');
  group.UseInputGroup('TFRE_DB_DEVICE','main');
  group.AddInput('site','',false,true);
  group.AddInput('crtid','$scheme_TFRE_DB_MOBILEDEVICE_certificate');
end;

class procedure TFRE_DB_MobileDevice.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_MOBILEDEVICE_main_group','Mobile Device'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_MOBILEDEVICE_name','Device Name'));
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_MOBILEDEVICE_certificate','Certificate'));
end;

function TFRE_DB_MobileDevice.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Mobile Device');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_MobileDevice.IMI_Menu(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe();
  //TODO - check if mobiledevice is assigned to a site?
  res.AddEntry.Describe('Unassign','images_apps/cloudcontrol/unassign_mobile_device.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'unassign'));
  res.AddEntry.Describe('Delete','images_apps/cloudcontrol/delete_mobile_device.png',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'deleteOperation'));
  Result:=res;
end;

function TFRE_DB_MobileDevice.IMI_unassign(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result := TFRE_DB_MESSAGE_DESC.Create.Describe('Unassign Mobile Device','Not implemented yet!',fdbmt_error);
end;

{ TFRE_DB_Endpoint }

class procedure TFRE_DB_Endpoint.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_DEVICE');

  scheme.AddSchemeField     ('site',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField     ('status_uid',fdbft_ObjLink);
  scheme.AddSchemeField     ('reprovision',fdbft_Boolean);
  scheme.AddCalculatedField ('displayname','GetDisplayName',cft_OnStoreUpdate);

  group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_ENDPOINT_main_group');
  group.UseInputGroup('TFRE_DB_DEVICE','main');
  group.AddInput('site','',false,true);
end;

class procedure TFRE_DB_Endpoint.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION);
begin
  inherited InstallDBObjects(conn);
  conn.StoreTranslateableText(GFRE_DBI.CreateText('$scheme_TFRE_DB_ENDPOINT_main_group','End Point Configuration'));
end;

function TFRE_DB_Endpoint.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_SUBSECTIONS_DESC;
  sec   : TFRE_DB_SECTION_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  sec:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Configuration'),'Configuration',1);
  res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring'),'Monitoring',2);

  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Configuration(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_PANEL_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  GetDBConnection.GetScheme(SchemeClass,scheme);
  res:=TFRE_DB_FORM_PANEL_DESC.Create.Describe('Endpoint');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));

  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res  : TFRE_DB_SUBSECTIONS_DESC;
  sub  : TFRE_DB_SECTION_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.create.Describe(sec_dt_vertical);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_Con'),'Connected',1);
//  sub.SetContentDesc(IMI_Statistics_Con(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_All'),'All',2,'',2);
//  sub.SetContentDesc(IMI_Statistics_All(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  sub:=res.AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'Monitoring_All'),'All',0);
//  sub.SetContentDesc(IMI_Statistics_All(nil).Implementor_HC as TFRE_DB_CONTENT_DESC);

  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_Con(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_VIEW_LIST_DESC;
  layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  store : TFRE_DB_STORE_DESC;
begin

  layout:=TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  layout.AddDataElement.Describe('customernumber','Number');
  layout.AddDataElement.Describe('company','Company');
  layout.AddDataElement.Describe('firstname','Firstname');
  layout.AddDataElement.Describe('lastname','Lastname');

  store:=TFRE_DB_STORE_DESC.create.Describe('Statistics',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring_Data'));

  res:=TFRE_DB_VIEW_LIST_DESC.create.Describe(store, layout, nil, 'Monitoring Con',[]);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_All(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_VIEW_LIST_DESC;
  layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  store : TFRE_DB_STORE_DESC;
begin

  layout:=TFRE_DB_VIEW_LIST_LAYOUT_DESC.create.Describe();
  layout.AddDataElement.Describe('customernumber','Number');
  layout.AddDataElement.Describe('company','Company');
  layout.AddDataElement.Describe('firstname','Firstname');
  layout.AddDataElement.Describe('lastname','Lastname');

  store:=TFRE_DB_STORE_DESC.create.Describe('Statistics',TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'Monitoring_Data'));

  res:=TFRE_DB_VIEW_LIST_DESC.create.Describe(store, layout, nil, 'Monitoring All',[]);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_Monitoring_Data(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
begin
  res:=TFRE_DB_STORE_DATA_DESC.create.Describe(3);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=1;
  entry.Field('company').AsString:='A';
  entry.Field('firstname').AsString:='AF';
  entry.Field('lastname').AsString:='AL';
  res.addEntry(entry);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=2;
  entry.Field('company').AsString:='B';
  entry.Field('firstname').AsString:='BF';
  entry.Field('lastname').AsString:='BL';
  res.addEntry(entry);
  entry:=GFRE_DBI.NewObject;
  entry.Field('customernumber').AsInt16:=3;
  entry.Field('company').AsString:='C';
  entry.Field('firstname').AsString:='CF';
  entry.Field('lastname').AsString:='CL';
  res.addEntry(entry);
  Result:=res;
end;


function TFRE_DB_Endpoint.IMI_Provision(const input: IFRE_DB_Object): IFRE_DB_Object;
var pnr:integer;
    AProcess: TProcess;
    res: TFRE_DB_MESSAGE_DESC;
begin
  writeln('YEAH GUT PROVISION --- ');
  if FieldExists('provisioning_serial') then begin
   pnr:=Field('provisioning_serial').asint32;
  end else begin
   pnr:=0;
  end;
  inc(pnr);
  writeln('new frehash:',pnr);
  Field('provisioning_serial').asint32:=pnr;
  writeln(DumpToString());
  Field('reprovision').asboolean := false;

  CheckDbResult(GetDBConnection.Update(self),'failure on cloned/update');  // This instance is freed by now, so rely on the stackframe only (self) pointer is garbage(!!)

  res := TFRE_DB_MESSAGE_DESC.Create.Describe('PROVISIONING','Provisioning OK',fdbmt_info); //TODO - add nil message (nothing to do response)
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_addOpenWifiNetwork(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection.GetScheme('TFRE_DB_OPENWIFINETWORK',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add Open Wifi Network');
  res.SendChangedFieldsOnly(false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('endpoint',GFRE_BT.GUID_2_HexString(UID));
  res.SetElementValue('hidden','false');
  res.SetElementValue('ip_net',GetNextNet(GetDBConnection,UID));
  res.SetElementValue('dns','172.17.0.1');
  res.SetElementValue('dhcp','true');
  res.SetElementValue('dhcp_range_start','10');
  res.SetElementValue('dhcp_range_end','250');
  res.SetElementValue('dhcp_leasetime','600');
  res.SetElementValue('dhcp_leasetime','600');
  res.SetElementValue('sessiontimeout','1800');
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_OPENWIFINETWORK','newOperation');
  serverFunc.AddParam.Describe('collection','network');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_addWPA2Network(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res       : TFRE_DB_DIALOG_DESC;
  scheme    : IFRE_DB_SchemeObject;
  serverFunc: TFRE_DB_SERVER_FUNC_DESC;
begin
  GetDBConnection.GetScheme('TFRE_DB_WPA2NETWORK',scheme);
  res:=TFRE_DB_DIALOG_DESC.Create.Describe('Add WPA2 Network');
  res.SendChangedFieldsOnly(false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.SetElementValue('endpoint',GFRE_BT.GUID_2_HexString(UID));
  res.SetElementValue('hidden','true');
  res.SetElementValue('ip_net',GetNextNet(GetDBConnection,UID));
  res.SetElementValue('dns','172.17.0.1');
  res.SetElementValue('dhcp','true');
  res.SetElementValue('dhcp_range_start','10');
  res.SetElementValue('dhcp_range_end','250');
  res.SetElementValue('dhcp_leasetime','600');
  res.SetElementValue('dhcp_leasetime','600');
  res.SetElementValue('sessiontimeout','1800');
  serverFunc:=TFRE_DB_SERVER_FUNC_DESC.Create.Describe('TFRE_DB_WPA2NETWORK','newOperation');
  serverFunc.AddParam.Describe('collection','network');
  res.AddButton.Describe('Save',serverFunc,fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_Endpoint.IMI_GetDisplayName(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:='Endpoint('+Field('provisioningmac').AsString+')';
end;

function TFRE_DB_Endpoint.IMI_ChildrenData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_STORE_DATA_DESC;
  entry : IFRE_DB_Object;
  childs: TFRE_DB_GUIDArray;
  i     : Integer;
  dbo   : IFRE_DB_Object;

begin
  res := TFRE_DB_STORE_DATA_DESC.create;
  childs:=ReferencedByList;
  for i := 0 to Length(childs) - 1 do begin
    GetDBConnection.Fetch(childs[i],dbo);
    if dbo.IsA('TFRE_DB_NETWORK') then begin
      entry:=GFRE_DBI.NewObject;
      entry.Field('text').AsString:=dbo.field('ssid').AsString;
      entry.Field('uid').AsGUID:=dbo.UID;
      entry.Field('uidpath').AsStringArr:=dbo.GetUIDPath;
      entry.Field('_funcclassname_').AsString:=dbo.SchemeClass;
      entry.Field('_childrenfunc_').AsString:='ChildrenData';
      entry.Field('_menufunc_').AsString:='Menu';
      entry.Field('_contentfunc_').AsString:='Content';
      res.addEntry(entry);
    end;
  end;
  Result:=res;
end;

// Captive Portal Data

procedure CreateDB(const real: boolean;const dbname: string; const user, pass: string);

const ctenants             =   1;
      ccustomerpertenant   =  50;
      csitepercustomer     =   1;
      ceaddresspercustomer =   3;
      cuser                = 500;
      cusergroup           =  20;
      cendpoints           =   1;
      cmobilepersite       =   5;
      ccountry             =  20;
//      cdhcpsubnets         =  3;
//      cdhcpdevices         =  5;
      ccmspath             =  'http://portal.cityaccess.at';
//      ccmspath             =  'http://wlanclocal.firmos.at';
      cremoteuser               = 'root';
      cremotehost               = '10.1.0.116';
      cremotehosttester         = '10.1.0.138';
      cwinrmurl                 = 'https://10.4.0.234:5986/wsman';
      cwinrmuser                = 'winrmi';
      cwinrmpassword            = 'mgo54rb5A';

var   si,customer_id:TGUid;

      g_endpoint_number:integer;
      g_network_number:integer;
      g_mobile_number:integer;
      g_cust_number:integer;

      cp_uid     : TGUID;
      vpn_ap_uid : TGUID;
      vpn_ap_uid2: TGUID;
      vpn_uid    : TGUID;
      ca_r_uid   : TGUID;
      ca_r_s_uid : TGUID;
      cms_uid    : TGUID;
      startcms_uid : TGUID;  //TODO FS
      agbcms_uid   : TGUID;  //TODO FS
      defaultcms_uid : TGUID; //TODO FS
      wlan_machine_uid    : TGUID;
      vpn_machine_uid     : TGUID;
      dhcp_uid : TGUID;

      realmode : boolean;

      function CreateBCDB:TGuid;
      var CONN : IFRE_DB_CONNECTION;
          COLL : IFRE_DB_COLLECTION;
          COLL2: IFRE_DB_COLLECTION;
          COLL3: IFRE_DB_COLLECTION;
          ICOLL: IFRE_DB_COLLECTION;


          OB   : IFRE_DB_OBJECT;
          OB2  : IFRE_DB_OBJECT;
          OB3  : IFRE_DB_OBject;
          cao  : IFRE_DB_Object;
          cuid : TGUID;
          tenant_uid : TGUID;
          c    : IFRE_DB_Object;

          i:integer;
          j:integer;

          function _getcountry:IFRE_DB_Object;
           var r:integer;
               lCOLL: IFRE_DB_COLLECTION;
               s:string;
               o:IFRE_DB_Object;
           begin
            r:=random(ccountry)+1;
            s:='countryname'+inttostr(r);
      //      lcoll:=conn.CollectionCC('country',TFRE_DB_INDEXED_COLLECTION) as TFRE_DB_INDEXED_COLLECTION;
            lCOLL := conn.Collection('country');
            if lcoll.GetIndexedObj(s,o)=false then begin
             gfre_bt.CriticalAbort('Error on randomized _getcountry !'+s);
            end;
            result:=Conn.NewObject('TFRE_DB_COUNTRY');
            result.Field('objname').AsString:=o.Field('objname').AsString;
            result.Field('tld').AsString:=o.Field('tld').AsString;
           end;

          function _addaddress  :IFRE_DB_Object;
          var a :IFRE_DB_Object;
              nr:integer;
          begin
           nr:=random(10000);
           a:=CONN.NewObject('TFRE_DB_ADDRESS');
           a.Field('street').asstring:='street'+inttostr(nr);
           a.Field('nr').asstring:='nr'+inttostr(nr);
           a.Field('stair').asstring:='st'+inttostr(nr);
           a.Field('block').asstring:='block'+inttostr(nr);
           a.Field('floor').asstring:='floor'+inttostr(nr);
           a.Field('door').asstring:='door'+inttostr(nr);
           a.Field('co').asstring:='co'+inttostr(nr);
           a.Field('city').asstring:='city'+inttostr(nr);
           a.Field('zip').asstring:='zip'+inttostr(nr);
           a.Field('country').AsObject:=_getcountry;
           result:=a;
          end;

          procedure _createcontact(const o:IFRE_DB_Object;const company,firstname,lastname:string);
          begin
           o.Field('company').AsString   := company;
           o.Field('firstname').AsString := firstname;
           o.Field('lastname').AsString  := lastname;
          end;

          procedure _addaddresses(const o:IFRE_DB_Object);
          begin
           o.Field('mainaddress').AsObject:=_addaddress;
           o.Field('deliveryadress').AsObject:=_addaddress;
          end;

          function _addposition: IFRE_DB_OBject;
          begin
           result:=conn.NewObject('TFRE_DB_GEOPOSITION');
           result.Field('longitude').AsReal64:=(random*360)-180;
           result.Field('latitude').AsReal64:=(random*360)-180;
          end;

          procedure  _addsite(const customerid:TGUID;name:string;sitekey:string='');
          var nr          : integer;
              site_object : IFRE_DB_Object;
          begin
           nr:=random(10000);
           site_object:=conn.NewObject('TFRE_DB_Site');
           if name='' then begin
            name:='site'+inttostr(nr);
           end;
           site_object.Field('objname').asstring := name;
           site_object.Field('sitekey').asstring := sitekey;
           site_object.Field('position').asobject:=_addposition;
           if realmode=false then begin
             site_object.Field('address').AsObject:=_addaddress;
           end;
           site_object.Field('customerid').AsObjectLink:=customerid;
           CheckDbResult(COLL3.Store(site_object),'Add Site');
          end;

          procedure _addsites(const c:TGUID);
          var i:integer;
          begin
            for i:=1 to csitepercustomer do begin
             _addsite(c,'');
            end;
          end;

          function _addeaddress(const scheme:string)  :IFRE_DB_Object;
          var nr:integer;
          begin
           nr:=random(10000);
           result:=conn.NewObject(scheme);
           result.Field('url').asstring:='url'+inttostr(nr);
          end;

          procedure _addeaddresses(const o:IFRE_DB_Object);
          begin
           o.Field('mailaddress').AsObject:=_addeaddress('TFRE_DB_MAILADDRESS');
           o.Field('webaddress').AsObject:=_addeaddress('TFRE_DB_WEBADDRESS');
          end;

          function _addphone(const scheme:string)  :IFRE_DB_Object;
          var nr:integer;
          begin
           nr:=random(10000);
           result:=conn.NewObject(scheme);
           result.Field('number').asstring:='+43'+inttostr(nr);
          end;

          procedure _addphones(const o:IFRE_DB_Object);
          begin
           o.Field('businessphone').AsObject:=_addphone('TFRE_DB_PHONE');
           o.Field('mobilephone').AsObject:=_addphone('TFRE_DB_PHONE');
           o.Field('privatephone').AsObject:=_addphone('TFRE_DB_PHONE');
          end;

         function _addcustomer(const company,firstname,lastname,cnr,street,nr,city,zip,country,countrytld:string;const tenant_id:TGUID):TGUID;
         var ob  : IFRE_DB_Object;
             cao : IFRE_DB_Object;
             c   : IFRE_DB_Object;
         begin
          ob:=CONN.Newobject('TFRE_DB_Customer');
          _createcontact(ob,company,firstname,lastname);
          ob.Field('customernumber').AsString:=cnr;
          ob.Field('TENANTID').AsObjectLink:=tenant_id;
          cao:=conn.NewObject('TFRE_DB_ADDRESS');
          cao.Field('street').asstring:=street;
          cao.Field('nr').asstring:=nr;
          cao.Field('city').asstring:=city;
          cao.Field('zip').asstring:=zip;
          c:=Conn.NewObject('TFRE_DB_COUNTRY');
          c.Field('objname').AsString:=country;
          c.Field('tld').AsString:=countrytld;
          cao.Field('country').AsObject:=c;
          ob.Field('mainaddress').AsObject:=cao;
          result:=ob.UID;
          CheckDbResult(COLL.Store(ob),'add Customer');
         end;

      begin
        writeln('INIT DB');
        randomize;

        CONN := GFRE_DBI.NewConnection;
        CONN.Connect(dbname,'admin','admin');

      //  if not conn.CollectionExists('country') then begin
      ////   icoll := conn.CollectionCC('country',TFRE_DB_INDEXED_COLLECTION) as TFRE_DB_INDEXED_COLLECTION;
      //   conn.CollectionAsIntf('country',IFRE_DB_INDEXED_COLLECTION,icoll);
      //   icoll.SetIndexField('name');
      //  end else begin
      ////   icoll := conn.CollectionCC('country',IFRE_DB_INDEXED_COLLECTION) as IFRE_DB_INDEXED_COLLECTION;
      //   conn.CollectionAsIntf('country',IFRE_DB_INDEXED_COLLECTION,icoll);
      //  end;

        if conn.CollectionAsIntf('country',IFRE_DB_COLLECTION,icoll,true) = true then begin
          icoll.DefineIndexOnField('objname',fdbft_String,true,true);
        end else begin
          //test case icoll.SetIndexField('name');
      //   abort
        end;


        if realmode=false then begin
         for i:=1 to ccountry do begin
          OB:=Conn.NewObject('TFRE_DB_COUNTRY');
          ob.Field('objname').AsString:='countryname'+inttostr(i);
          ob.Field('tld').AsString:='abc'+inttostr(i);
          CheckDbResult(ICOLL.Store(ob),'Add country');
         end;
        end else begin
         OB:=Conn.NewObject('TFRE_DB_COUNTRY');
         ob.Field('objname').AsString:='Österreich';
         ob.Field('tld').AsString:='at';
         CheckDbResult(ICOLL.Store(ob),'Add Country');
        end;

        COLL := CONN.Collection('customer');
        COLL2 := CONN.Collection('tenant');
        COLL3 := CONN.Collection('site');

        if realmode=false then begin
          for i:=1 to ctenants do begin
           OB2:=CONN.NewObject('TFRE_DB_Tenant');
           tenant_uid:= ob2.Field('UID').AsGUID;
           _createcontact(ob2,'T'+'Company '+inttostr(i),'Firstname '+inttostr(i),'Lastname '+inttostr(i));
           cao:=conn.NewObject('TFRE_DB_ADDRESS');
           cao.Field('street').asstring:='tstreet';
           cao.Field('nr').asstring:='1';
           cao.Field('city').asstring:='tcity';
           cao.Field('zip').asstring:='tzip';
           c:=Conn.NewObject('TFRE_DB_COUNTRY');
           c.Field('objname').AsString:='Österreich';
           c.Field('tld').AsString:='at';
           cao.Field('country').AsObject:=c;
           ob2.Field('mainaddress').AsObject:=cao;
           tenant_uid := ob2.UID;

           CheckDbResult(COLL2.Store(ob2),'Add Tenant');
            for j:=1 to ccustomerpertenant do begin
            OB:=CONN.NewObject('TFRE_DB_Customer');
            _createcontact(ob,'Company'+inttostr(g_cust_number),'Firstname'+inttostr(g_cust_number),'Lastname'+inttostr(g_cust_number));
            ob.Field('customernumber').AsString:='cnr'+inttostr(g_cust_number);
            inc(g_cust_number);
            _addaddresses(ob);
            OB.Field('TENANTID').AsObjectLink:=tenant_uid;
            cuid := OB.UID;
            _addeaddresses(ob);
            _addphones(ob);
            CheckDbResult(COLL.Store(ob),'Add Customer');
            _addsites(cuid);
           end;
          end;
        end else begin
         OB2:=CONN.NewObject('TFRE_DB_Tenant');
         _createcontact(ob2,'Citycom Telekommunikation Gmbh','GF Igo','Huber');
         cao:=conn.NewObject('TFRE_DB_ADDRESS');
         cao.Field('street').asstring:='Steyrergasse';
         cao.Field('nr').asstring:='111';
         cao.Field('city').asstring:='Graz';
         cao.Field('zip').asstring:='8010';
         c:=Conn.NewObject('TFRE_DB_COUNTRY');
         c.Field('objname').AsString:='Österreich';
         c.Field('tld').AsString:='at';
         cao.Field('country').AsObject:=c;
         ob2.Field('mainaddress').AsObject:=cao;
         tenant_uid := ob2.UID;
         CheckDbResult(COLL2.Store(ob2),'Add Tenant');
        end;
        cuid:=_addcustomer('Dr. Schilhan Gebäudereinigung','Mag. Clemens','Schilhan','000-001','Andritzer Reichstraße','13','Graz','8054','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Andritz');
        cuid:=_addcustomer('FirmOS Business Solutions Gmbh','DI Franz','Schober','000-002','Obstweg','4','Neu-Pirka','8073','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Neu-Pirka');
        _addsite(cuid,'Standort FirmOS Local');
        _addsite(cuid,'Standort Demo');
        _addsite(cuid,'Standort Test');
        cuid:=_addcustomer('Citycom Telekommunikation Gmbh','GF Igo','Huber','000-003','Steyergasse','111','Graz','8010','Österreich','at',tenant_uid);
        result := cuid;
        _addsite(cuid,'Standort Intern1');
        _addsite(cuid,'Standort TestVPN1');
        _addsite(cuid,'Standort VPN2');
        _addsite(cuid,'Standort VPN3');
        _addsite(cuid,'Standort VPN');
        _addsite(cuid,'Standort Test1');
        _addsite(cuid,'Standort Test2');
        _addsite(cuid,'Standort Test3');
        _addsite(cuid,'Standort Kunde1');
        _addsite(cuid,'Standort Kunde3');
        _addsite(cuid,'Standort Kunde4');
        _addsite(cuid,'Standort Kunde7');
        _addsite(cuid,'Standort Kunde8');
        _addsite(cuid,'Standort Kunde10');
        _addsite(cuid,'Standort Event','EVENT');

        cuid:=_addcustomer('Igo Huber','Igo','Huber','000-011','Steyergasse','111','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Huber');
        cuid:=_addcustomer('Paul Droneberger','Paul','Droneberger','000-011','Steyergasse','111','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Droneberger');
        cuid:=_addcustomer('n-box','n-box','n-box','000-012','Steyergasse','111','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort n-box');
        cuid:=_addcustomer('Tribeka','Tribeka','Tribeka','000-013','Grieskai','2','Graz','8020','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Tribeka1');
        _addsite(cuid,'Standort Tribeka2');
        _addsite(cuid,'Standort Tribeka3');
        _addsite(cuid,'Standort Tribeka4');

        cuid:=_addcustomer('Forum Stadtpark','Heidrun','Primas','000-004','Stadtpark','1','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Forum','FORUM');
        cuid:=_addcustomer('Spielstätten Graz','Christoph','Thoma','000-005','ಬಾ ಇಲ್ಲಿ ಸಂಭವಿಸು ಇಂದೆನ್ನ ಹೃದಯದಲಿOrpheumgasse','8','Graz','8020','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Dom im Berg');
        _addsite(cuid,'Standort Oper','OPER');
        _addsite(cuid,'Standort Schauspielhaus','SPHAUS');
        cuid:=_addcustomer('Joanneum','Andreas','Graf','000-007','Mariahilferstraße','2-4','Graz','8020','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Universalmuseum Neutorgasse','JOANNEUM');
        cuid:=_addcustomer('Gady',' ',' ','000-008','Liebenauer Hauptstraße','60','Liebenau','8041','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Gady','GADY');
        cuid:=_addcustomer('Shopping Nord',' ',' ','000-009','Wiener Straße','351','Graz','8051','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort ShoppingNord','SCN');
        cuid:=_addcustomer('Stadt Graz',' ',' ','000-010','Hauptplatz','1','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Hauptplatz','HPL');
        _addsite(cuid,'Standort Jakominiplatz','JAKO');
        _addsite(cuid,'Standort Citybeach','CITYBEACH');
        _addsite(cuid,'Standort Lendplatz','LEND');
        _addsite(cuid,'Standort Eisernes Tor','EISERNES');
        _addsite(cuid,'Standort Hauptbahnhof','HBF');
        _addsite(cuid,'Standort Novapark','NOVA');
        _addsite(cuid,'Standort Schlossberg','SCHLOSSBG');
        _addsite(cuid,'Standort Orpheum','ORPH');
        _addsite(cuid,'Standort Karmeliterplatz','KARMELITER');
        _addsite(cuid,'Standort Auster','AUSTER');
        _addsite(cuid,'Standort Eishalle','EISHALLE');
        _addsite(cuid,'Standort Event','EVENT');
        _addsite(cuid,'Standort Eggenberg','EGGENBERG');


        _addsite(cuid,'Standort MonitoringTest','MONITORTEST');

        _addsite(cuid,'Standort Margarethenbad');
        cuid:=_addcustomer('Igo Huber','Igo','Huber','000-011','Steyergasse','111','Graz','8010','Österreich','at',tenant_uid);
        _addsite(cuid,'Standort Huber');

        CONN.Finalize;
      end;

      function CreateSGDB(const customerid:TGuid):TGuid;
      var CONN : IFRE_DB_CONNECTION;
          COLL : IFRE_DB_COLLECTION;
          OB   : IFRE_DB_OBJECT;
          i:integer;

          COLL2: IFRE_DB_COLLECTION;
          hob  : IFRE_DB_OBJECT;

      begin
        randomize;

        CONN := GFRE_DBI.NewConnection;
        CONN.Connect(dbname,'admin','admin');
        CreateSGSchemes(conn);

        COLL   := CONN.Collection('servicegroup');
        OB:=conn.NewObject('TFRE_DB_SERVICEGROUP');
        OB.Field('objname').AsString:='MAINSG';
        ob.Field('CUSTOMERID').AsObjectLink := customerid;
        CheckDbResult(COLL.Store(ob),'Add ServiceGroup');

        result:=DummyGetServiceGroupID(conn);

        COLL2 := CONN.Collection('machine');
        hob:=conn.NewObject('TFRE_DB_VMACHINE');
        hob.Field('servicegroup').AsObjectLink:=result;
        hob.Field('objname').AsString:='WLAN Controller';
        hob.Field('ip').AsString:='172.17.0.1';
        wlan_machine_uid:=hob.UID;
        CheckDbResult(COLL2.Store(hob),'Add VMachine');

        hob:=conn.NewObject('TFRE_DB_VMACHINE');
        hob.Field('servicegroup').AsObjectLink:=result;
        hob.Field('objname').AsString:='VPN Gateway 1';
        hob.Field('ip').AsString:='172.17.0.2';
        vpn_machine_uid:=hob.UID;
        CheckDbResult(COLL2.Store(hob),'Add VMachine');

        result:=DummyGetServiceGroupID(conn);
      //  writeln(GFRE_BT.GUID_2_HexString(result));


        CONN.Finalize;
        conn := nil;
      end;

      function AddCMSPage(const url:string;const conn:IFRE_DB_CONNECTION;const adpage:boolean=false;const insertpoint:integer=1;const starttime:TFRE_DB_DateTime64=0;const endtime:TFRE_DB_DateTime64=0;const relativeurl:boolean=true;const max_count:integer=10000000):TGUID;
      var ob  :IFRE_DB_Object;
          coll:IFRE_DB_Collection;
          cms :IFRE_DB_Object;
      begin
       COLL := CONN.Collection('cmspage');
       if adpage then begin
        ob   :=CONN.NewObject('TFRE_DB_CMS_ADPAGE');
       end else begin
        ob   :=CONN.NewObject('TFRE_DB_CMS_PAGE');
       end;
       if conn.Fetch(cms_uid,cms)=false then begin
        gfre_bt.CriticalAbort('cant fetch cms ');
       end;
       ob.Field  ('cms').AsObjectLink:=cms_uid;
       ob.Field  ('active').AsBoolean:=true;
       ob.Field  ('url').AsString:=url;
       ob.Field  ('relativeurl').AsBoolean:=relativeurl;
       // ob.Field  ('edituser').addobjectLink
       if adpage then begin
        ob.Field('start_time').AsDateTimeUTC := starttime;
        ob.Field('end_time').AsDateTimeUTC   := endtime;
        ob.Field('start_daily').AsString:= '00:00';
        ob.Field('end_daily').AsString:= '23:59';
        ob.Field('insertpoint').AsInt16   := insertpoint;
        ob.Field('max_inserts').AsUInt32  := max_count;
        ob.Field('shown_inserts').AsUInt32:= 0;
       end;
       result:=ob.UID;
       CheckDbResult(COLL.Store(ob),'Add CMS Page');
      // writeln(ob.DumpToString());

      end;

      procedure AssignNetworkToGroup(const nw_id: TGUID; const nwgname: string; const conn: IFRE_DB_CONNECTION);
      var lnwgob       :      IFRE_DB_Object;
          lcoll        :      IFRE_DB_COLLECTION;

          function _findnwg(const field:IFRE_DB_Field):boolean;
          begin
           result:=field.AsString=nwgname;
          end;


      begin
       lcoll           := CONN.Collection('networkgroup');
       lnwgob          := lcoll.LinearScan('objname',@_findnwg);

       if assigned( lnwgob) then begin
        lnwgob.Field('networks').AddObjectLink (nw_id);
        writeln(lnwgob.DumpToString());
        CONN.Update(lnwgob);
       end else begin
        raise Exception.Create('cant fetch network group '+nwgname);
       end;
      end;

      procedure AddExceptionToCMSPage(const ex:string;const cms_id: TGUID; const conn: IFRE_DB_CONNECTION);
      var cms:IFRE_DB_Object;
      begin
       if conn.Fetch(cms_id,cms)=false then begin
        gfre_bt.CriticalAbort('cant fetch page');
       end;
       cms.Field('urlexceptions').AddString(ex);
       conn.Update(cms);
      end;

      procedure AddRedirectionFlow(const name: string; const startpage, customerpage, agbpage_open, agbpage_ipad, endpage: string; const cms_uid: TGUID; const urlexceptions: array of string; const conn: IFRE_DB_CONNECTION);
      var lredirob       : IFRE_DB_Object;
          lcoll          : IFRE_DB_Collection;
          lurlexi        : integer;
          lurlex         : string;




          procedure      _setcmspage(const field:string;const url:string);
          var lcmscoll   : IFRE_DB_COLLECTION;
              lcmsobj    : IFRE_DB_Object;

              function _findurl(const field:IFRE_DB_Field):boolean;
              begin
                result:=field.AsString=url;
              end;

          begin
           if url<>'' then begin
            lcmscoll      := conn.Collection('cmspage');
            lcmsobj       := lcmscoll.LinearScan('url',@_findurl);
            if assigned (lcmsobj) then begin
             lredirob.Field (field).AddObjectLink(lcmsobj.UID);
            end else begin
             raise Exception.Create('Cant find '+url+' for redirection flow  '+name);
            end;
           end;
          end;

      begin
        lcoll     := CONN.Collection('redirectionflow');
        lredirob  := CONN.NewObject('TFRE_DB_REDIRECTION_FLOW');
        lredirob.Field ('cms').AsObjectLink := cms_uid;
        lredirob.Field ('objname').asstring    := name;
        _setcmspage('redirection_start',startpage);
        _setcmspage('redirection_customer',customerpage);
        _setcmspage('redirection_agb_open',agbpage_open);
        _setcmspage('redirection_agb_ipad',agbpage_ipad);
        _setcmspage('redirection_end',endpage);
        for lurlexi:=low(urlexceptions) to high(urlexceptions) do begin
         lredirob.Field('urlexceptions').AddString(urlexceptions[lurlexi]);
        end;
        writeln(lredirob.DumpToString());
        CheckDbResult(lcoll.Store(lredirob),'Add RedirectionFlow');
      end;

      procedure AssignNGToAdPage(const nwg_id: TGUID; const ad_id: TGUID; const conn: IFRE_DB_CONNECTION);
      var ad:IFRE_DB_Object;
      begin
       if conn.Fetch(ad_id,ad)=false then begin
        gfre_bt.CriticalAbort('cant fetch ad page');
       end;
       ad.Field('networkgroups').AddObjectLink(nwg_id);
       conn.Update(ad);
      // writeln(ad.DumpToString());
      end;

      procedure CreateCMS(const si: TGuid);
      var conn:IFRE_DB_Connection;
          coll:IFRE_DB_Collection;
          cob :IFRE_DB_Object;
          sdt :TFRE_DB_DateTime64;
          edt :TFRE_DB_DateTime64;

          CollN            :      IFRE_DB_Collection;
          ad_uid           :      TGUID;
          nwg_uid          :      TGUID;

          function _createnetworkgroup               (const name:string)        :TGUID;
          var nw           :      IFRE_DB_Object;
          begin
           nw:=CONN.NewObject('TFRE_DB_NETWORK_GROUP');
           nw.Field('objname').AsString:=lowercase(name);
           result := nw.UID;
           CheckDbResult(COLLN.Store(nw),'Add Networkgroup');
          end;

      begin
       CONN := GFRE_DBI.NewConnection;
       CONN.Connect(dbname,'admin','admin');

       COLL := CONN.Collection('service');
       COLLN := CONN.Collection('networkgroup');

       COB:=CONN.NewObject('TFRE_DB_CMS');
       cms_uid:=COB.UID;
       cob.Field('machineid').AsObjectLink:=wlan_machine_uid;
       COb.Field('servicegroup').AsObjectLink:=si;
       COb.Field('baseurl').AsString:=ccmspath;
       COb.Field('statetxt').addstring('Weiter');
       COb.Field('statetxt').addstring('Weiter');
       COb.Field('statetxt').addstring('Weiter');
       COb.Field('statetxt').addstring('Weiter');
       COb.Field('statetxt').addstring('Bestätigt');
       COb.Field('statetxt').addstring('Logout');
       Cob.Field('urlexceptions').AddString(ccmspath);
       Cob.Field('urlexceptions').AddString('http://wlanclocal.firmos.at');
//       Cob.Field('urlexceptions').AddString('http://www.apple.com/library/test');
       Cob.Field('urlexceptions').AddString('http://www.google-analytics.com');
       CheckDbResult(COLL.Store(cob),'Add CMS');

      // COB.Field('user').AddObjectLink();
       startcms_uid:=AddCMSPage('/?q=cityaccess',conn,false);
       agbcms_uid  :=AddCMSPage('/?q=agb',conn,false);
       defaultcms_uid  :=AddCMSPage('/?q=node/6',conn,false);


       AddCMSPage('/?q=agb_open',conn,false);
       AddCMSPage('/?q=agb_ipad',conn,false);


       // TEST DATA FOR UNIT TEST
       nwg_uid:=_createnetworkgroup('test');
//       sdt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,-1));
//       sdt:=GFRE_DBI.LocalTimeToUTCDB64(sdt);
//       edt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,1));
//       edt:=GFRE_DBI.LocalTimeToUTCDB64(edt);
//       ad_uid :=AddCMSPage('/?q=test1',conn,true,1,sdt,edt,true,1000000);
//       AssignNGToAdPage(nwg_uid,ad_uid,conn);
//       ad_uid :=AddCMSPage('/?q=test2',conn,true,2,sdt,edt,true,1000000);
//       AssignNGToAdPage(nwg_uid,ad_uid,conn);
//       AddExceptionToCMSPage('http://www.test.com',ad_uid,conn);
       AddCMSPage('http://www.firmos.at',conn,false,1,0,0,false);
       AddCMSPage('/?q=teststart',conn,false);
       AddCMSPage('/?q=testcustom',conn,false);
       AddRedirectionFlow('test','/?q=teststart','/?q=testcustom','/?q=agb_open','/?q=agb_ipad','http://www.firmos.at',cms_uid,['http://www.firmos.at','http://www.test.at'],conn);

       // DEMO DATA FOR EXTERNAL SSL DEMO
       nwg_uid:=_createnetworkgroup('demo');
//       sdt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,-1));
//       sdt:=GFRE_DBI.LocalTimeToUTCDB64(sdt);
//       edt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,1));
//       edt:=GFRE_DBI.LocalTimeToUTCDB64(edt);
//       ad_uid :=AddCMSPage('https://portal.cityaccess.at/?q=demo1',conn,true,1,sdt,edt,false,1000000);
       AddCMSPage('https://portal.cityaccess.at/?q=demo1',conn,false,1,0,0,false);
//       AssignNGToAdPage(nwg_uid,ad_uid,conn);
       AddCMSPage('https://portal.cityaccess.at/?q=agb_open',conn,false,1,0,0,false);
       AddCMSPage('https://portal.cityaccess.at/?q=agb_ipad',conn,false,1,0,0,false);
       AddCMSPage('http://www.citycom-austria.com',conn,false,1,0,0,false);
       AddRedirectionFlow('demo','','https://portal.cityaccess.at/?q=demo1','https://portal.cityaccess.at/?q=agb_open','https://portal.cityaccess.at/?q=agb_ipad','http://www.citycom-austria.com',cms_uid,['http://www.citycom-austria.com'],conn);



       AddCMSPAge('/?q=schilhan',conn,false);
       AddCMSPAge('http://www.schilhan.net',conn,false,1,0,0,false);

       nwg_uid:=_createnetworkgroup('citypad');

       AddRedirectionFlow('schilhan','','/?q=schilhan','/?q=agb_open','/?q=agb_ipad','http://www.schilhan.net',cms_uid,['http://www.schilhan.net'],conn);

       nwg_uid:=_createnetworkgroup('cityaccess');
       AddCMSPAge('http://www.citycom.co.at',conn,false,1,0,0,false);
       AddRedirectionFlow('cityaccess','','','/?q=agb_open','/?q=agb_ipad','http://www.citycom.co.at',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at'],conn);

       nwg_uid:=_createnetworkgroup('joanneum');
       AddCMSPAge('http://www.joanneumsviertel.at',conn,false,1,0,0,false);
       AddRedirectionFlow('joanneum','','','/?q=agb_open','/?q=agb_ipad','http://www.joanneumsviertel.at',cms_uid,['http://www.joanneumsviertel.at','http://www.citycom.co.at','http://www.verwaltung.steiermark.at'],conn);

       nwg_uid:=_createnetworkgroup('gady');
       AddCMSPAge('http://www.gady.at',conn,false,1,0,0,false);
       AddRedirectionFlow('gady','','','/?q=agb_open','/?q=agb_ipad','http://www.gady.at',cms_uid,['http://www.gady.at','http://www.citycom.co.at'],conn);

       nwg_uid:=_createnetworkgroup('ekz');
       AddCMSPAge('http://www.shoppingnord.at',conn,false,1,0,0,false);
//       sdt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,-1));
//       sdt:=GFRE_DBI.LocalTimeToUTCDB64(sdt);
//       edt:=GFRE_DT.DateTimeToDBDateTime64(IncMonth(now,120));
//       edt:=GFRE_DBI.LocalTimeToUTCDB64(edt);
       AddCMSPage('/?q=shoppingnord1',conn,false);
//       ad_uid :=AddCMSPage('/?q=shoppingnord1',conn,true,1,sdt,edt,true,1000000);
//       AssignNGToAdPage(nwg_uid,ad_uid,conn);
//       AddExceptionToCMSPage('http://www.shoppingnord.at/',ad_uid,conn);
       AddRedirectionFlow('ekz','','/?q=shoppingnord1','/?q=agb_open','/?q=agb_ipad','http://www.shoppingnord.at',cms_uid,['http://www.shoppingnord.at','http://www.citycom.co.at'],conn);

       nwg_uid:=_createnetworkgroup('stadt');
       AddCMSPAge('http://www.citycom.co.at',conn,false,1,0,0,false);
       AddRedirectionFlow('stadt','','','/?q=agb_open','/?q=agb_ipad','http://www.citycom.co.at',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at'],conn);

       AddCMSPAge('http://www.citybeach-graz.com',conn,false,1,0,0,false);
       AddRedirectionFlow('citybeach','','','/?q=agb_open','/?q=agb_ipad','http://www.citybeach-graz.com',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.citybeach-graz.com'],conn);

       AddCMSPAge('http://www.oper-graz.com',conn,false,1,0,0,false);
       AddRedirectionFlow('oper','','','/?q=agb_open','/?q=agb_ipad','http://www.oper-graz.com',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.oper-graz.com'],conn);

       AddCMSPAge('http://www.holding-graz.at/freizeit/baeder/margarethenbad.html',conn,false,1,0,0,false);
       AddRedirectionFlow('margarethenbad','','','/?q=agb_open','/?q=agb_ipad','http://www.holding-graz.at/freizeit/baeder/margarethenbad.html',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.holding-graz.at'],conn);

       AddCMSPAge('http://www.tribeka.at',conn,false,1,0,0,false);
       AddRedirectionFlow('tribeka','','','/?q=agb_open','/?q=agb_ipad','http://www.tribeka.at',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.tribeka.at'],conn);

       AddCMSPAge('http://www.schauspielhaus-graz.com',conn,false,1,0,0,false);
       AddRedirectionFlow('schauspielhaus','','','/?q=agb_open','/?q=agb_ipad','http://www.schauspielhaus-graz.com',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.schauspielhaus-graz.com'],conn);

       AddCMSPAge('http://www.spielstaetten.at',conn,false,1,0,0,false);
       AddRedirectionFlow('orpheum','','','/?q=agb_open','/?q=agb_ipad','http://www.spielstaetten.at',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.spielstaetten.at'],conn);

       AddCMSPAge('http://www.holding-graz.at/nc/freizeit/baeder.html',conn,false,1,0,0,false);
       AddRedirectionFlow('baeder','','','/?q=agb_open','/?q=agb_ipad','http://www.holding-graz.at/nc/freizeit/baeder.html',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.holding-graz.at'],conn);

       AddCMSPAge('http://www.museum-joanneum.at/de/schloss_eggenberg',conn,false,1,0,0,false);
       AddRedirectionFlow('eggenberg','','','/?q=agb_open','/?q=agb_ipad','http://www.museum-joanneum.at/de/schloss_eggenberg',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at','http://www.museum-joanneum.at'],conn);

       nwg_uid:=_createnetworkgroup('citypad');
       AddRedirectionFlow('citypad','','','/?q=agb_open','/?q=agb_ipad','http://www.citycom.co.at',cms_uid,['http://www.citycom-austria.com','http://www.citycom.co.at'],conn);

       CONN.Finalize;
      end;

      procedure AddFixedDHCP(const ip,mac,name:string;const conn :IFRE_DB_Connection);
      var fx:IFRE_DB_Object;
          collf      : IFRE_DB_Collection;
      begin
       fx:=CONN.NewObject('TFRE_DB_DHCP_Fixed');
       collf := CONN.Collection('dhcp_fixed');
       fx.Field('ip').AsString:=ip;
       fx.Field('mac').AsString:=mac;
       fx.Field('objname').AsString:=name;
       //fx.Field('displayname').AsString:='Host '+name;
       fx.Field('dhcp').AsObjectLink:=dhcp_UID;
       CheckDBResult(COLLF.Store(fx),'Add DHCP Fixed');
      end;

      procedure AddSubnetDHCP(const net,startip,endip,routerip,dnsip:string;const conn :IFRE_DB_Connection);
      var sn:IFRE_DB_Object;
          colls       : IFRE_DB_Collection;
      begin
       sn:=CONN.NewObject('TFRE_DB_DHCP_Subnet');
       COLLS := CONN.Collection('dhcp_subnet');
       //sn.Field('displayname').AsString:='Subnet '+net;
       sn.Field('subnet').AsString:=net;
       sn.Field('range_start').AsString:=startip;
       sn.Field('range_end').AsString:=endip;
       sn.Field('router').AsString:=routerip;
       sn.Field('dns').AsString:=dnsip;
       sn.Field('dhcp').AsObjectLink:=dhcp_UID;
       CheckDbResult(COLLS.Store(sn),'Add DHCP Subnet');
      end;

      procedure CreateDHCPDB(const si: TGuid);
      var conn  :IFRE_DB_Connection;
          coll  :IFRE_DB_Collection;
          lcob  :IFRE_DB_Object;
          i     :integer;



      begin
       CONN := GFRE_DBI.NewConnection;
       CONN.Connect(dbname,'admin','admin');

       COLL := CONN.Collection('service');



       lCOB:=CONN.NewObject('TFRE_DB_DHCP');
       lcob.Field('machineid').AsObjectLink:=wlan_machine_uid;
       lCOb.Field('servicegroup').AsObjectLink:=si;
       lCOB.Field('default_domainname').AsString:='ipad.citycom.co.at';
       lCOB.Field('default_dns').AsString:='172.17.0.1';
       lCOB.Field('default_leasetime').AsInt16:=600;
       lCOB.Field('fixed_start').AsString:='172.17.0.20';
       lCOB.Field('fixed_end').AsString:='172.17.0.99';
       dhcp_uid:=lcob.UID;
       CheckDbResult(COLL.Store(lcob),'Add DHCP');

       AddSubnetDHCP('172.17.0.0/24','172.17.0.100','172.17.0.200','172.17.0.1','172.17.0.1',conn);

       //_AddFixed('172.17.0.21','c0:c1:c0:20:ba:4a','firmos_ap');
       //_AddFixed('172.17.0.22','c0:c1:c0:3f:9a:5f','schilhan_ap');
       //_AddFixed('172.17.0.23','c0:c1:c0:3f:9a:93','inhouse_ap1');

       if realmode=false then begin
        // add direct subnets
        for i:=0 to 254 do begin
         AddSubnetDHCP('10.0.'+inttostr(i)+'.0/24','10.0.'+inttostr(i)+'.20','10.0.'+inttostr(i)+'.200','10.0.'+inttostr(i)+'.1','172.17.0.1',conn);
        end;
        // add vpn subnets
        for i:=0 to 254 do begin
         AddSubnetDHCP('10.100.'+inttostr(i)+'.0/24','10.100.'+inttostr(i)+'.20','10.100.'+inttostr(i)+'.200','10.100.'+inttostr(i)+'.1','172.17.0.1',conn);
        end;
       end else begin
        //_AddFixed('172.17.0.21','c0:c1:c0:20:ba:4a','firmos_ap');
        //_AddSubnet('10.0.1.0/24','10.0.1.20','10.0.1.200','10.0.1.1','127.17.0.1');   //fos intern
        //_AddSubnet('10.0.2.0/24','10.0.2.20','10.0.2.200','10.0.2.1','127.17.0.1');
        //_AddSubnet('10.0.5.0/24','10.0.5.20','10.0.5.200','10.0.5.1','127.17.0.1');   // schilhan
        //_AddSubnet('10.0.6.0/24','10.0.6.20','10.0.6.200','10.0.6.1','127.17.0.1');
        //_AddSubnet('10.0.7.0/24','10.0.7.20','10.0.7.200','10.0.7.1','127.17.0.1');   // citycom intern 1
        //_AddSubnet('10.0.8.0/24','10.0.8.20','10.0.8.200','10.0.8.1','127.17.0.1');
        //_AddSubnet('10.100.1.0/24','10.100.1.20','10.100.1.200','10.100.1.1','127.17.0.1');  // VPN1
        //_AddSubnet('10.100.2.0/24','10.100.2.20','10.100.2.200','10.100.2.1','127.17.0.1');
        //_AddSubnet('10.100.3.0/24','10.100.3.20','10.100.3.200','10.100.3.1','127.17.0.1');  // VPN2
        //_AddSubnet('10.100.4.0/24','10.100.4.20','10.100.4.200','10.100.4.1','127.17.0.1');
        //_AddSubnet('10.100.5.0/24','10.100.5.20','10.100.5.200','10.100.5.1','127.17.0.1');  // VPN3
        //_AddSubnet('10.100.6.0/24','10.100.6.20','10.100.6.200','10.100.6.1','127.17.0.1');
        //_AddSubnet('10.100.7.0/24','10.100.7.20','10.100.7.200','10.100.7.1','127.17.0.1');  // VPN FirmOS
        //_AddSubnet('10.100.8.0/24','10.100.8.20','10.100.8.200','10.100.8.1','127.17.0.1');
       end;
       CONN.Finalize;
      end;

      procedure CreateMonitoring(const conn: IFRE_DB_CONNECTION);

      type
           tmadress = (tmFOSLab,tmKSMServ,tmFOSServ,tmFOSMB,tmcc);
      var
           mon          : IFRE_DB_Object;
           mon_id       : TGUID;
           mon_key      : string;
           sg           : IFRE_DB_Object;
           sg_id        : TGUID;
           service      : IFRE_DB_Object;
           service_id   : TGUID;
           machine      : IFRE_DB_Object;
           machine_id   : TGUID;
           tester       : TFRE_DB_Tester;
           coll_mon     : IFRE_DB_COLLECTION;
           coll_sg      : IFRE_DB_COLLECTION;
           coll_service : IFRE_DB_COLLECTION;
           coll_machine : IFRE_DB_COLLECTION;
           coll_testcase       : IFRE_DB_COLLECTION;
           coll_testcasestatus : IFRE_DB_COLLECTION;
           jobdbo       : IFRE_DB_Object;
           cronl        : TStringList;

           function GetRemoteKeyFilename: string;
           begin
            result := SetDirSeparators(cFRE_GLOBAL_DIRECTORY+'/ssl/user/id_rsa');         // must be authorized for remoteuser on remotehost
           end;

           function GetBackupKeyFilename: string;
           begin
             result := SetDirSeparators(cFRE_GLOBAL_DIRECTORY+'/ssl/user/zfsbackup_rsa');
           end;



           procedure AddServiceGroup(const name :string);
           begin
             sg                                  := conn.NewObject('TFRE_DB_SERVICEGROUP');
             sg.Field('objname').AsString        := name;
             // sg.Field('CUSTOMERID').AsObjectLink := customerid;
             sg_id                               := sg.UID;
             sg.Field('monitoring').AsObjectLink := mon_id;
             CheckDbResult(coll_sg.Store(sg),'Add ServiceGroup');
           end;

           procedure AddService(const name :string);
           begin
             service                                     := conn.NewObject('TFRE_DB_SERVICE');
             service.Field('objname').AsString           := name;
             service_id                                  := service.UID;
             service.Field('servicegroup').AsObjectLink  := sg_id;
             CheckDbResult(coll_service.Store(service),'Add Service');
           end;

           procedure Troubleshooting(const obj:IFRE_DB_Object; const tshoot:string);
           begin
             obj.Field('troubleshooting').asstring := tshoot;
           end;

           procedure AddTestCaseStatus(const tc_id: TGUID;const periodic:TFRE_TestPeriodic);
            var
              tcs      : IFRE_DB_Object;
            begin
              tcs      := Conn.NewObject(TFRE_DB_TESTCASESTATUS.ClassName);
              tcs.Field('periodic_ord').AsInt16  := Ord(periodic);
              tcs.Field('testcase').AsObjectLink        :=  tc_id;
              TFRE_DB_TestcaseStatus(tcs.Implementor_HC).IMI_ClearStatus(nil);
              writeln(tcs.DumpToString);
              CheckDbResult(coll_testcasestatus.Store(tcs),'Add Testcasestatus');
            end;

           procedure StoreAndAddToMachine(obj:IFRE_DB_Object);
           var
             id       : TGUID;
             tc       : TFRE_DB_Testcase;
             periodic : TFRE_TestPeriodic;
           begin
             id       := obj.UID;
             obj.Field('machine').AsObjectLink     := machine_id;
             tc       := TFRE_DB_Testcase(obj.Implementor_HC);
             periodic := tc.GetPeriodic;
             writeln(obj.DumpToString);
             CheckDbResult(coll_testcase.Store(obj),'Add Testcase');
             AddTestCaseStatus(id,periodic);
           end;

           procedure AddVMPingTestCase(const vmhost: string; const vmarray: TFRE_DB_StringArray;const rtt:integer);
           var
             job      : TFRE_DB_MultiPingTestcase;
             i        : integer;
             obj      : IFRE_DB_Object;

           begin
             obj    :=conn.NewObject(TFRE_DB_MultiPingTestcase.ClassName);
             job    :=TFRE_DB_MultiPingTestcase(obj.Implementor_HC);
             job.SetJobkeyDescription(mon_key+'_'+uppercase(vmhost),'Host '+vmhost);
             job.SetInformation(vmhost);
             job.SetRTTTimeout_ms(rtt);
             job.SetPingCount(10);
             Troubleshooting(job,'Check Status of Machines');
             for i := low(vmarray) to high(vmarray) do begin
               writeln('I:',i);
               job.AddPingTest(vmarray[i],vmarray[i]);
             end;
        //     writeln(job.DumpToString());
             StoreAndAddToMachine(job);
           end;

           procedure AddMachine(const name :string; const ip:string;const where:tmadress;const vmid:string='';const noping:boolean=false);
           var rtt:integer;

              procedure AddAddress;
              var c_dbo : IFRE_DB_Object;
                  a_dbo : IFRE_DB_Object;
              begin
                c_dbo:=Conn.NewObject('TFRE_DB_COUNTRY');
                c_dbo.Field('objname').AsString:='Österreich';
                c_dbo.Field('tld').AsString:='at';
                a_dbo:=CONN.NewObject('TFRE_DB_ADDRESS');
                if where in [tmFOSLab,tmFOSServ,tmKSMServ] then begin
                 a_dbo.Field('street').asstring:='Obstweg';
                 a_dbo.Field('nr').asstring:='4';
                 a_dbo.Field('city').asstring:='Neu-Pirka';
                 a_dbo.Field('zip').asstring:='8073';
                 if where = tmFOSLab then begin
                   a_dbo.Field('co').asstring:='Labor 1.Stock';
                 end else begin
                   a_dbo.Field('co').asstring:='Serverraum Parterre';
                 end;
                 rtt := 1000;
                end else if where=tmFOSMB then begin
                  a_dbo.Field('street').asstring:='Marburgerkai';
                  a_dbo.Field('nr').asstring:='1';
                  a_dbo.Field('city').asstring:='Graz';
                  a_dbo.Field('zip').asstring:='8010';
                  rtt := 1000;
                 end else begin
                  a_dbo.Field('street').asstring:='Steirergasse';
                  a_dbo.Field('nr').asstring:='111';
                  a_dbo.Field('city').asstring:='Graz';
                  a_dbo.Field('zip').asstring:='8010';
                  rtt := 1000;
                 end;
                a_dbo.Field('country').AsObject:=c_dbo;
                machine.Field('address').AsObject:=a_dbo;
                if vmid<>'' then begin
                  machine.Field('vmid').AsString:=vmid;
                end;
              end;

           begin
             machine                                := conn.NewObject('TFRE_DB_MACHINE');
             machine.Field('objname').AsString      := name;
             machine.Field('ip').AsString           := ip;
             AddAddress;
             machine_id                             := machine.UID;
             machine.Field('service').AsObjectLink  := service_id;
             writeln(machine.DumpToString);
             CheckDbResult(coll_machine.Store(machine),'Add Machine');

             if noping=false then begin
               AddVMPingTestCase(StringReplace(name+'_PING','.','_',[rfReplaceAll]),TFRE_DB_StringArray.Create(ip),rtt);
             end;
           end;




           procedure _create_jobdbo(const obj:IFRE_DB_Object);
           var jobkey : string;
           begin
             jobkey := obj.Field('objname').asstring;
             if Pos(mon_key,jobkey)=1 then begin
             //  writeln(obj.DumpToString);
               jobdbo.Field(jobkey).asobject := obj;
             end;
           end;

           procedure _create_cronjobs(const obj:IFRE_DB_Object);
           var jobkey  : string;
               line    : string;
               periodic: string;
           begin
             jobkey := obj.Field('objname').asstring;
             if Pos(mon_key,jobkey)=1 then begin
              case TFRE_DB_Testcase(obj.Implementor_HC).GetPeriodic of
                everyDay :   periodic :='0 0 * * * ';
                everyHour:   periodic :='15 * * * *';
                everyMinute: periodic :='* * * * * ';
              end;
              line := periodic+'      fre_safejob '+jobkey;
              cronl.Add(line);
             end;
           end;

           procedure CreateAlert;
           var alert: TFRE_Alert;

                procedure _create_alerts(const obj:IFRE_DB_Object);
                var jobkey : string;
                begin
                  jobkey := obj.Field('objname').asstring;
      //            if Pos('CITYACCESS_AP_',jobkey)>0 then begin
                    alert.AddAlertKeys(TFRE_DB_StringArray.Create(jobkey));
      //            end;
                end;

           begin
             alert := TFRE_Alert.Create;
             try
               alert.ClearStatus;
               alert.ClearConfig;
               alert.SetSMTPHost('mail.firmos.at','25','mailer','45mfj345l2094pc1');
               alert.SetMailFrom('alert@firmos.at');
               alert.SetMailSubject('Alert from FirmOS Monitoring System Controller');
               alert.AddAlertingEMailCommon(TFRE_DB_StringArray.Create('franz.schober@firmos.at','noc@firmos.at'));
               alert.AddAlertTypeCommon(alty_Mail);
               alert.SetAlertModeCommon(almo_ChangeTimeout);
               alert.SetChangeTimeoutCommon(300);
               coll_testcase.ForAll(@_create_alerts);
               alert.SaveAlertConfig;
             finally
               alert.free;
             end;

           end;

           procedure AddTestCase(const name : string);
           var
             obj    : IFRE_DB_Object;
           begin
             obj   :=conn.NewObject(TFRE_DB_Testcase.ClassName);
             TFRE_DB_Testcase(obj.Implementor_HC).SetJobkeyDescription('DUMMY'+'_'+name,'Description TODO:'+name);
             Troubleshooting(obj,'fix it :-) '+name);
             StoreAndAddToMachine(obj);
           end;

           procedure AddZpoolStatusTestCase(const server: string; const jobkey: string; const desc: string);
           var
             obj    : IFRE_DB_Object;
             z      : TFRE_DB_ZFSJob;
           begin
             obj   :=conn.NewObject(TFRE_DB_ZFSJob.ClassName);
             z     :=TFRE_DB_ZFSJob(obj.Implementor_HC);
             z.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             z.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             z.SetPeriodic(everyHour);
             z.SetPoolStatus('zones',7,14);
             Troubleshooting(obj,'Check Disks ! ');
             StoreAndAddToMachine(obj);
           end;

           procedure AddZFSSpaceTestCase(const server: string; const jobkey: string; const desc: string);
           var
             obj    : IFRE_DB_Object;
             z      : TFRE_DB_ZFSJob;
           begin
             obj   :=conn.NewObject(TFRE_DB_ZFSJob.ClassName);
             z     :=TFRE_DB_ZFSJob(obj.Implementor_HC);
             z.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             z.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             z.SetDatasetspace('zones',80,95);
             Troubleshooting(obj,'Delete data or remove snapshots in your dataset !');
             StoreAndAddToMachine(obj);
           end;

           procedure AddDiskSpaceTestCase(const server: string; const jobkey: string; const desc: string; const mountpoint:string);
           var
             obj    : IFRE_DB_Object;
             z      : TFRE_DB_DiskspaceTestcase;
           begin
             obj   :=conn.NewObject(TFRE_DB_DiskspaceTestcase.ClassName);
             z     :=TFRE_DB_DiskspaceTestcase(obj.Implementor_HC);
             z.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             z.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             z.SetMountpoint(mountpoint,80,95);
             Troubleshooting(obj,'Delete data or extend the device !');
             StoreAndAddToMachine(obj);
           end;

           procedure AddHTTPTestCase(const url: string; const jobkey: string; const desc: string; const header:string; const responsematch:string);
           var
             obj    : IFRE_DB_Object;
             z      : TFRE_DB_HTTPTestcase;
           begin
             obj   :=conn.NewObject(TFRE_DB_HTTPTestcase.ClassName);
             z     :=TFRE_DB_HTTPTestcase(obj.Implementor_HC);
             z.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
//             z.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             z.SetURL(url,header,responsematch);
             z.SetPeriodic(everyHour);
             Troubleshooting(obj,'Delete data or extend the device !');
             StoreAndAddToMachine(obj);
           end;

           procedure AddCPUTestCase(const server: string; const jobkey: string; const desc: string; const warning_load:integer; const error_load:integer);
           var
             obj    : IFRE_DB_Object;
             t      : TFRE_DB_CPULoadTestcase;
           begin
             obj   :=conn.NewObject(TFRE_DB_CPULoadTestcase.ClassName);
             t     :=TFRE_DB_CPULoadTestcase(obj.Implementor_HC);
             t.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             t.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             t.SetLimits(warning_load,error_load);
             t.SetPeriodic(everyHour);
             Troubleshooting(obj,'Check for slow processes or hangs !');
             StoreAndAddToMachine(obj);
           end;


           procedure AddProcessTestCase(const server: string;const jobkey:string;const desc:string;const processname:TFRE_DB_StringArray;const error_count:TFRE_DB_UInt32Array);
           var
             obj    : IFRE_DB_Object;
             p      : TFRE_DB_ProcessTestcase;
             i      : integer;
           begin
             obj   :=conn.NewObject(TFRE_DB_ProcessTestcase.ClassName);
             p     :=TFRE_DB_ProcessTestcase(obj.Implementor_HC);
             p.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             p.SetRemoteSSH(cremoteuser, server, Getremotekeyfilename);
             for i := 0 to high(processname) do begin
               p.AddProcessTest(processname[i],1,error_count[i],processname[i]);
             end;
             Troubleshooting(obj,'Restart failing Processes !');
             StoreAndAddToMachine(obj);
           end;

           procedure AddZFSReplication(const jobkey: string; const desc : string; const sourcehost: string; const desthost:string; const sourceds:string; const destds: string;const checkperiod_sec: integer);
            var
              obj    : IFRE_DB_Object;
              zf     : TFRE_DB_ZFSJob;
            begin
              obj   :=conn.NewObject(TFRE_DB_ZFSJob.ClassName);
              zf    :=TFRE_DB_ZFSJob(obj.Implementor_HC);
              zf.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
              zf.SetRemoteSSH(cremoteuser, sourcehost, Getremotekeyfilename);
              zf.SetReplicate(sourceds,destds,'AUTO',desthost,'zfsback',GetbackupKeyFileName,'/zones/firmos/zfsback/.ssh/id_rsa');
              zf.SetPeriodic(everyDay);
              Troubleshooting(obj,'Check Replication ! ');
              StoreAndAddToMachine(obj);

              obj   :=conn.NewObject(TFRE_DB_ZFSJob.ClassName);
              zf    :=TFRE_DB_ZFSJob(obj.Implementor_HC);
              zf.SetJobkeyDescription(mon_key+'_'+jobkey+'_CHECK',desc);
              zf.SetRemoteSSH(cremoteuser, desthost, Getremotekeyfilename);
              zf.SetSnapshotCheck(destds,'AUTO',checkperiod_sec*2,checkperiod_sec*4);
              zf.SetPeriodic(everyHour);
              Troubleshooting(obj,'Check Snapshot ! ');
              StoreAndAddToMachine(obj);
           end;


           procedure AddInternetTestCase(const office:boolean);
           var
             obj    : IFRE_DB_Object;
             job    : TFRE_DB_InternetTestcase;
           begin
             obj    :=conn.NewObject(TFRE_DB_InternetTestcase.ClassName);
             job    :=TFRE_DB_InternetTestcase(obj.Implementor_HC);
             job.SetRemoteSSH(cremoteuser, cremotehost, Getremotekeyfilename);
             if office then begin
               job.SetJobkeyDescription(mon_key+'_'+'INTERNETTESTCASE','Internetverbindung FirmOS Office');
               job.PrepareTest('91.114.28.42','91.114.28.41',TFRE_DB_StringArray.Create ('10.1.0.1'));
             end else begin
              job.SetJobkeyDescription(mon_key+'_'+'INTERNETTESTCASE_HOUSING','Internetverbindung FirmOS Housing');
             job.PrepareTest('10.220.251.1','80.120.208.113',TFRE_DB_StringArray.Create ('8.8.8.8'));
             end;
             job.SetPeriodic(everyHour);
             Troubleshooting(job,'Restart Modem !');
             StoreAndAddToMachine(job);
           end;


           procedure AddWinRMTestCase(const server: string; const jobkey: string; const desc: string);
           var
             obj    : IFRE_DB_Object;
             wm     : TFRE_DB_WinRMTestcase;
             wmt    : TFRE_DB_WinRMTarget;
           begin
             obj   :=conn.NewObject(TFRE_DB_WinRMTestcase.ClassName);

             wm    :=TFRE_DB_WinRMTestcase(obj.Implementor_HC);
             wm.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             wm.SetRemoteSSH(cremoteuser, cremotehosttester, Getremotekeyfilename);
             wmt      := wm.AddWinRMTarget(cwinrmurl, cwinrmuser, cwinrmpassword);
             wmt.AddTestDiskSpace('E422D5D9',40,90);
             wmt.AddTestServiceRunning('MSExchangeServiceHost');
             wmt.AddTestServiceRunning('DHCPServer');
             wmt.SetTestCPUUsage(90);
             Troubleshooting(obj,'Check KSM Server ! ');
             StoreAndAddToMachine(obj);
           end;

           procedure AddMailCheck(const name : string);
           var
             sm     : TFRE_DB_MailSendTestcase;
             cm     : TFRE_DB_MailCheckTestcase;
             obj    : IFRE_DB_Object;
           begin
             obj      := conn.NewObject(TFRE_DB_MailSendTestcase.ClassName);
             sm       := TFRE_DB_MailSendTestcase(obj.Implementor_HC);
             sm.SetJobkeyDescription(mon_key+'_'+name,'Senden von Mails über den KSM Exchange Server');
             sm.SetRemoteSSH(cremoteuser, cremotehosttester, GetRemoteKeyFilename);
             sm.AddMailserver('10.4.0.234',cwinrmuser,cwinrmpassword,'tester@ksm.at','monsys@monsys.firmos.at');
             StoreAndAddToMachine(obj);

             obj      := conn.NewObject(TFRE_DB_MailCheckTestcase.ClassName);
             cm       := TFRE_DB_MailCheckTestcase(obj.Implementor_HC);
             cm.SetJobkeyDescription(mon_key+'_'+name+'_CHECK','Empfangen von über den KSM Exchange Server gesendenten Mails');
             cm.Field('MAX_ALLOWED_TIME').AsInt32 := 60;
             cm.SetRemoteSSH(cremoteuser, cremotehosttester, Getremotekeyfilename);
             cm.SetMailserver('10.4.0.234',mon_key+'_'+name,300,600);
             StoreAndAddToMachine(cm);
            end;

           procedure AddSMBTestCase(const server: string; const jobkey: string; const desc: string; const user : string; const pass: string; const fileshare : string; const mountpoint : string);
           var
             obj    : IFRE_DB_Object;
             smb    : TFRE_DB_SMBTestcase;
           begin
             obj   :=conn.NewObject(TFRE_DB_SMBTestcase.ClassName);
             smb   :=TFRE_DB_SMBTestcase(obj.Implementor_HC);
             smb.SetJobkeyDescription(mon_key+'_'+jobkey,desc);
             smb.SetRemoteSSH(cremoteuser, cremotehosttester, Getremotekeyfilename);
             smb.SetFileshare(server,user,pass,fileshare,mountpoint);
             Troubleshooting(obj,'Check Fileservice ! ');
             StoreAndAddToMachine(obj);
           end;

           procedure AddSBFWZones;
           var
             obj      : IFRE_DB_Object;
             job      : TFRE_DB_MultiPingTestcase;
             i        : integer;

             procedure AddZone(const ip:string; const detail:string);
             begin
               writeln('ZONE:',detail,' ',ip);
               job.AddPingTest(ip,detail);
             end;

           begin
             obj    :=conn.NewObject(TFRE_DB_MultiPingTestcase.ClassName);
             job    :=TFRE_DB_MultiPingTestcase(obj.Implementor_HC);
             job.SetJobkeyDescription(mon_key+'_'+'SBFW_ZONES_PING','Small Business Firewall');
             job.SetInformation('Small Business Firewall');
             job.SetRTTTimeout_ms(100);
             job.SetPingCount(5);
             Troubleshooting(job,'Check SBFW System');
             AddZone('109.73.149.69','Droneberger');
             AddZone('109.73.149.70','TestCustomer');
             AddZone('109.73.149.71','Huber');
             AddZone('109.73.149.72','tribeka_leonhard');
             AddZone('109.73.149.73','tribeka_technikerstrasse');
             AddZone('109.73.149.74','tribeka_grieskai');
             AddZone('109.73.149.75','tribeka_muchargasse');
             StoreAndAddToMachine(job);
           end;

           procedure AddCityaccessAPTestCase;
           var
             collsite   : IFRE_DB_COLLECTION;

             procedure _addCheck;
             var
               obj      : IFRE_DB_Object;
               check    : TFRE_AlertHTMLJob;
             begin
              obj      :=conn.NewObject(TFRE_AlertHTMLJob.ClassName);
              check    :=TFRE_AlertHTMLJob(obj.Implementor_HC);
              check.SetJobkeyDescription(mon_key+'_'+'ALERT_HTML_REPORT','HTML_REPORT');
              StoreAndAddToMachine(obj);
             end;


             procedure _site_endpoints(const site:IFRE_DB_Object);
             var
               aps      : TFRE_DB_GUIDArray;
               ap       : IFRE_DB_Object;
               obj      : IFRE_DB_Object;
               job      : TFRE_DB_MultiPingTestcase;
               i        : integer;

             begin
               writeln ('SITE:',site.Field('objname').asstring);
               aps   := site.ReferencedByList(TFRE_DB_AP_Lancom.Classname);
               if length(aps)>0 then begin
                 writeln('AP:',length(aps));
                 obj    :=conn.NewObject(TFRE_DB_MultiPingTestcase.ClassName);
                 job    :=TFRE_DB_MultiPingTestcase(obj.Implementor_HC);
                 job.SetJobkeyDescription(mon_key+'_'+'CITYACCESS_AP_'+site.Field('sitekey').asstring,'Cityaccess Accesspoints '+site.Field('objname').asstring);
      //           job.SetRemoteSSH(cremoteuser, cremotehost, Getremotekeyfilename);
                 job.SetInformation(site.Field('objname').asstring);
                 job.SetRTTTimeout_ms(50);
                 job.SetPingCount(5);
                 Troubleshooting(job,'Check Connections and Backbone Routing/VLAN');

                 for i := low(aps) to high(aps) do begin
                   CONN.Fetch(aps[i],ap);
                   writeln('I:',i);
                   job.AddPingTest(ap.Field('external_ip').asstring,ap.Field('mountingdetail').asstring);
                 end;
                 StoreAndAddToMachine(job);
               end;


             end;

           begin
             collsite   := CONN.Collection('site');
             collsite.ForAll(@_site_endpoints);

//             _addCheck;

           end;


      begin
       jobdbo       := GFRE_DBI.NewObject;

       coll_mon            := CONN.Collection('monitoring');
       coll_sg             := CONN.Collection('servicegroup');
       coll_service        := CONN.Collection('service');
       coll_machine        := CONN.Collection('machine');
       coll_testcasestatus := CONN.Collection('testcasestatus');

       if conn.CollectionAsIntf('testcase',IFRE_DB_COLLECTION,coll_testcase,true) = true then begin
         coll_testcase.DefineIndexOnField('objname',fdbft_String,true,true);
       end;

       mon_key                               := 'MONFOS';

       mon                                := CONN.NewObject('TFRE_DB_Monitoring');
       mon.Field('objname').AsString      := mon_key;
       mon_id                             := mon.UID;
       CheckDbResult(coll_mon.Store(mon),'Add Monitoring');



         // Precondition: Hierarchie ist immer gleich, gleiche Levels

        // AddTester('Tester1','10.1.0.138');
       ///    AddSCPJob;
         AddServiceGroup('FirmOS Office');         // Firmos Office/Internet/Gateway/INTERNETTESTCASE            STATUS            STATUSDETAILS
           AddService('Internet');
             AddMachine('Gateway','91.114.28.42',tmFOSServ);
             AddInternetTestcase(true);
             AddProcessTestCase('10.1.0.1','OFFICE_GW_PROCESS','Processes on FirmOS Office Gateway',TFRE_DB_StringArray.Create('openvpn','named','dhcpd'),TFRE_DB_UInt32Array.Create(4,1,1));
           AddService('Storage');
             AddMachine('FirmOSStorage','10.1.0.116',tmFOSLab);
               AddCPUTestCase('10.1.0.116','CPU_FOSDATA','CPU Load FirmOS Office Storage',5,10);
               AddZpoolStatusTestCase('10.1.0.116','ZPOOL_FOS_FOSDATA','Zones FirmOS Office Storage');
               AddZFSSpaceTestCase('10.1.0.116','ZFS_SPACE_FOSDATA','Zones FirmOS Office Storage Space');
               AddZFSReplication('ZFS_REPL_FOSDATA','Replication FirmOS Fosdata','10.1.0.116','smartstore.firmos.at','zones/568ac8fd-d282-4848-a5b7-c5df2ae6f3f8',
                                 'zones/backup/firmos/568ac8fd-d282-4848-a5b7-c5df2ae6f3f8',86400);
               AddZFSReplication('ZFS_REPL_FOSDATA_DOK','Replication FirmOS Fosdata Dokumente','10.1.0.116','smartstore.firmos.at','zones/FirmOS_Data/FirmOS_Dokumente',
                                 'zones/backup/firmos/FirmOSData/FirmOS_Dokumente',86400);
               AddZFSReplication('ZFS_REPL_FOSDATA_LIB','Replication FirmOS Fosdata Library','10.1.0.116','smartstore.firmos.at','zones/FirmOS_Data/FirmOS_Library',
                                 'zones/backup/firmos/FirmOSData/FirmOS_Library',86400);
             AddMachine('fosdata','10.1.0.132',tmFOSLab,'568ac8fd-d282-4848-a5b7-c5df2ae6f3f8');
               AddProcessTestCase('10.1.0.132','OFFICE_FOSDATA_PROCESS','Processes on Office Fosdata',TFRE_DB_StringArray.Create('smbd','afpd','netatalk'),TFRE_DB_UInt32Array.Create(1,1,1));
             AddMachine('win2008dc','10.1.0.110',tmFosLab,'24d6d43c-24f2-4087-9e37-692c79f1ce91');
             AddMachine('win2008vsphere','10.1.0.111',tmFosLab,'e59ea635-7400-42bc-925b-d9ee9168ccf3');
             AddMachine('nexenta','10.1.0.112',tmFosLab,'cdb518dc-b40c-47e6-a492-4ae359f9049d');
             AddMachine('testzone','10.1.0.113',tmFosLab,'128581ed-52d6-4777-8533-de05261ef23b');
             AddMachine('smartosbuild','10.1.0.114',tmFosLab,'1d53d0eb-cdde-4a99-a32e-4a3bbe7d519b');
             AddMachine('windev','10.1.0.115',tmFosLab,'1a1bc352-0a92-4aad-8aa9-edb293e54ea0');
             AddMachine('old_build','10.1.0.117',tmFosLab,'bd2a5ef3-a11c-459e-8d26-460939758fed');
             AddMachine('wintest','10.1.0.118',tmFosLab,'d8aea6ea-94b7-4aab-b2c7-16b69c9ea156');
           AddService('Lab');
         AddServiceGroup('KSM');
           AddService('Office');
             AddMachine('SBS Server','10.4.0.234',tmKSMServ);
               AddWinRMTestCase('10.4.0.234','WINRM_KSM_SBS','SBS Server KSM WinRM');
               AddTestCase('Services Exchange + POP3 Connector');
               AddSMBTestCase('10.4.0.234','KSM_SBS_FILESHARE','SBS Server KSM Fileshare KSM_Buero', cwinrmuser, cwinrmpassword, 'KSM_BUERO', '/usr/local/testsmb');
               AddMailCheck('MAILKSM');
             AddMachine('Linux','10.4.0.3',tmKSMServ);
               AddDiskSpaceTestCase('10.4.0.3','KSM_LINUX_SPACE','Diskspace on KSM Linux','/');
               AddProcessTestCase('10.4.0.3','KSM_LINUX_PROCESS','Processes on KSM Linux',TFRE_DB_StringArray.Create('fbguard','fbserver','Dispatcher'),TFRE_DB_UInt32Array.Create(1,1,1));
           AddService('Storage');
             AddMachine('KSMStorage','10.1.0.129',tmKSMServ);
               AddZpoolStatusTestCase('10.1.0.129','ZPOOL_KSM_ZONES','Diskpool KSM Storage');
               AddZFSSpaceTestCase('10.1.0.129','ZFS_SPACE_KSM_ZONES','Zones FirmOS Office Storage Space');
               AddCPUTestCase('10.1.0.129','CPU_KSM','CPU Load KSM Storage',5,10);
               AddZFSReplication('ZFS_REPL_KSMLINUX','Replication KSM Faktura','10.1.0.129','smartstore.firmos.at','zones/2cd2e934-f21e-4fea-b46e-f3098f4e3be3',
                                 'zones/backup/ksm/2cd2e934-f21e-4fea-b46e-f3098f4e3be3',86400);
               AddZFSReplication('ZFS_REPL_KSMLINUX_DISK','Replication KSM Faktura Disk0','10.1.0.129','smartstore.firmos.at','zones/2cd2e934-f21e-4fea-b46e-f3098f4e3be3-disk0',
                                 'zones/backup/ksm/2cd2e934-f21e-4fea-b46e-f3098f4e3be3-disk0',86400);
               AddZFSReplication('ZFS_REPL_KSMSBS','Replication KSM SBS','10.1.0.129','smartstore.firmos.at','zones/a560f5a3-35ae-4552-9f25-be1308c9db65',
                                 'zones/backup/ksm/a560f5a3-35ae-4552-9f25-be1308c9db65',86400);
               AddZFSReplication('ZFS_REPL_KSMSBS_DISK','Replication KSM SBS Disk0','10.1.0.129','smartstore.firmos.at','zones/a560f5a3-35ae-4552-9f25-be1308c9db65-disk0',
                                 'zones/backup/ksm/a560f5a3-35ae-4552-9f25-be1308c9db65-disk0',86400);
         AddServiceGroup('FirmOS Marburgerkai');
           AddService('Backoffice');
             AddMachine('Firewall','80.120.208.114',tmFOSMB);
               AddInternetTestcase(false);
//             AddMachine('Belkin KVM','80.120.208.115',tmFOSMB);
             AddMachine('DRAC_FIREWALL','80.120.208.116',tmFOSMB);
             AddMachine('HP_SWITCH','80.123.225.54',tmFOSMB,'',true);
             AddMachine('BACKOFFICE','10.220.252.101',tmFOSMB);
               AddTestCase('FirmOSMail');
             AddMachine('FIP','10.220.252.21',tmFOSMB);
               AddProcessTestCase('10.220.252.21','FIP2_PROCESS','Processes on FIP2',TFRE_DB_StringArray.Create('postgres','mysqld','Dispatcher','fbguard','fbserver'),TFRE_DB_UInt32Array.Create(1,1,1,1,1));
               AddDiskSpaceTestCase('10.220.252.21','FIP2_DISKSPACE','Diskspace on FIP2 /','/');
           AddService('SmartOSStorage');
               AddMachine('SmartOS','10.220.251.10',tmFOSMB,'',true);
               AddCPUTestCase('10.220.251.10','CPU_SMARTSTORAGE','CPU Load SmartStorage',10,15);
               AddZpoolStatusTestCase('10.220.251.10','ZPOOL_FOS_SMARTSTORAGE','Diskpool FirmOS SmartStorage');
               AddZFSSpaceTestCase('10.220.251.10','ZFS_SPACE_SMARTSTORAGE','Zones FirmOS SmartStorage');
               AddMachine('firmgit.firmos.at','10.220.252.120',tmFOSMB,'7b161e9f-36a4-4abf-925d-45c1b09ca1dc');
               AddHTTPTestCase('http://fosbuild.firmos.at','HTTP_FOSBUILD','FirmOS Building System (Jenkins)','','Jenkins');
               AddMachine('ns1int.firmos.at','10.220.252.11',tmFOSMB);
               AddMachine('ns2int.firmos.at','10.220.252.12',tmFOSMB);
               AddMachine('fpclin32.firmos.at','10.220.252.31',tmFOSMB);
               AddMachine('fpclin64.firmos.at','10.220.252.32',tmFOSMB);
               AddMachine('fpcwinxp32.firmos.at','10.220.252.42',tmFOSMB);
               AddMachine('fpcvista64.firmos.at','10.220.252.41',tmFOSMB);
               AddMachine('freelove832.firmos.at','10.220.252.51',tmFOSMB);
               AddMachine('freelove864.firmos.at','10.220.252.52',tmFOSMB);
               AddMachine('openbsd32.firmos.at','10.220.252.53',tmFOSMB);
               AddMachine('openbsd64.firmos.at','10.220.252.54',tmFOSMB);
               AddMachine('openindiana.firmos.at','10.220.252.55',tmFOSMB);
               AddMachine('freelove932.firmos.at','10.220.252.56',tmFOSMB);
               AddMachine('freelove964.firmos.at','10.220.252.57',tmFOSMB);
               AddMachine('netbsd32.firmos.at','10.220.252.58',tmFOSMB);
               AddMachine('netbsd64.firmos.at','10.220.252.59',tmFOSMB);
               AddMachine('fpcopensolaris.firmos.at','10.220.252.61',tmFOSMB);
               AddMachine('fpcwin764.firmos.at','10.220.252.63',tmFOSMB);
               AddMachine('fpcwin2008r2.firmos.at','10.220.252.64',tmFOSMB);
               AddMachine('fpcJenkins.firmos.at','10.220.252.66',tmFOSMB);
  //             AddMachine('openindiana2.firmos.at','10.220.252.67',tmFOSMB);
               AddMachine('oraclesolaris.firmos.at','10.220.252.68',tmFOSMB);
               AddMachine('fpcamiga.firmos.at','10.220.252.69',tmFOSMB);
               AddMachine('fpcreactos.firmos.at','10.220.252.70',tmFOSMB,'',true);
               AddMachine('lazarusteam32.firmos.at','10.220.252.80',tmFOSMB);
               AddHTTPTestCase('http://10.220.252.81','HTTP_LAZARUS','Lazarus Forum','','forumtitle');
               AddMachine('newlazarusteam.firmos.at','10.220.252.82',tmFOSMB);
               AddMachine('fpcfed32.firmos.at','10.220.252.91',tmFOSMB);
               AddMachine('fpcfed64.firmos.at','10.220.252.92',tmFOSMB);
               AddMachine('fpcqemu.firmos.at','10.220.252.95',tmFOSMB,'',true);
               AddMachine('fpchaiku32.firmos.at','10.220.252.97',tmFOSMB,'',true);
               AddMachine('freepascaldos.firmos.at','10.220.252.98',tmFOSMB,'',true);
               AddMachine('fpcfreedos11.firmos.at','10.220.252.99',tmFOSMB,'',true);
               AddMachine('firmbsd82leg.firmos.at','10.220.252.121',tmFOSMB,'06dec646-802f-4d99-8eb3-4b50db030224');
               AddMachine('firmbsd90.firmos.at','10.220.252.122' ,tmFOSMB,'c7e81ecb-107e-424e-9de3-bb4ed2e7e8c7');
               AddMachine('firmbsd9032.firmos.at','10.220.252.123' ,tmFOSMB,'d8402ea1-02ba-44a2-b0d5-c467b28a618a');  //id
               AddMachine('firmdebian64.firmos.at','10.220.252.124',tmFOSMB,'5c8727f9-89b7-4e47-aa1e-5da46ba8aecf');
               AddMachine('firmdebian32.firmos.at','10.220.252.125',tmFOSMB,'7a6999cf-bd18-49c8-8eaf-1590b26e86c8');
               AddMachine('monsysprod','10.220.252.130',tmFOSMB,'1160215a-c3f8-4aac-b932-46db11a5b398');
               AddMachine('openwrt_ubnt_build','10.220.252.132',tmFOSMB,'1e0fc382-9a1d-4fd4-9101-70714b7e6124',true);
      //       AddMachine('ddwrt','10.220.252.133',tmFOSMB,'6c0117b4-c452-4b56-adf0-6824e5694a7c');
      //       AddMachine('evercity','10.220.252.134',tmFOSMB,'92c2014b-e636-4d64-ba02-5bdf168b33e4');
      //       AddMachine('smos_package_build','10.220.252.135',tmFOSMB,'15843d0e-d5c0-4e19-bd4d-6fd6f7cfd657');
               AddMachine('win7dev','10.220.252.136',tmFOSMB,'79387d0d-4a59-4bae-b29d-55474744733a');
               AddMachine('ebay_sniper','10.220.252.140',tmFOSMB,'183934d6-8475-44a7-a6df-e93ea4b1320f',true);
               AddMachine('meeting','10.220.252.142',tmFOSMB,'4d242470-2994-4536-8fea-3b80fe52b944');
      //       AddMachine('openerp_debian64','10.220.252.143',tmFOSMB,'05c88c76-65aa-470d-918d-2d260b2dfbd8');
               AddMachine('nagios','10.220.252.144',tmFOSMB,'9aa85aa6-091d-43f4-8cd4-c75c01dfd856',true);
      //       AddMachine('testubuntu2','10.220.252.145',tmFOSMB,'2bceffb3-5b9d-4851-8ecc-3f82a8684da8');
               AddMachine('firmosweb','10.220.252.146',tmFOSMB,'13bad66b-dcbc-4eb1-9779-9b0465fc4159',true);
               AddMachine('firmosdrupal','10.220.252.147',tmFOSMB,'ad49d4be-156f-4e5a-b108-35339228f196');
               AddHTTPTestCase('http://10.220.252.147','HTTP_FIRMOS','FirmOS Homepage','Host: www.firmos.at','FirmOS Business Solutions');
               AddHTTPTestCase('http://10.220.252.147','HTTP_KSM','KSM Homepage','Host: www.ksm.at','KSM');
               AddMachine('herrengasse','10.220.252.148',tmFOSMB,'563f9e83-71f8-422a-9d69-06a97c9c9193');
               AddMachine('franzdata','10.220.252.149',tmFOSMB,'5c1eae31-67bc-4f19-a148-9ede039d7ad8');

               AddMachine('webext','10.220.252.150',tmFOSMB,'');
         AddServiceGroup('Citycom');
           AddService('Cityaccess');
             AddMachine('WlanController','109.73.148.178',tmCC);
                AddDiskSpaceTestCase('172.17.0.1','WLANC_SPACE_USR','Diskspace on WLANC /usr','/usr');
                AddDiskSpaceTestCase('172.17.0.1','WLANC_SPACE_VAR','Diskspace on WLANC /var','/var');
                AddTestCase('Cityaccess CPU Load');
                AddProcessTestCase('172.17.0.1','WLANC_PROCESS','Processes on WLAN Controller',TFRE_DB_StringArray.Create('squid','httpd','dhcpd','named','postgres','cron'),TFRE_DB_UInt32Array.Create(1,1,1,1,1,1));
                AddCityaccessAPTestCase;
           AddService('Small Business Firewall');
             AddMachine('SBFW','109.73.149.68',tmCC);
                AddZFSSpaceTestCase('109.73.149.68','ZFS_SPACE_SBFW','Zones SBFW Space');
                AddCPUTestCase('109.73.149.68','CPU_SBFW','CPU Load SBFW',4,8);
                AddSBFWZones;

        coll_testcase.ForAll(@_create_jobdbo);

        cronl := TStringList.Create;
        try
          coll_testcase.ForAll(@_create_cronjobs);
          cronl.SaveToFile(cFRE_HAL_CFG_DIR+'cronjobs');
        finally
          cronl.Free;
        end;

        CreateAlert;

        jobdbo.SaveToFile(cFRE_HAL_CFG_DIR+'jobs.dbo');
        writeln(jobdbo.DumpToString);
        //abort;
      end;


     procedure CreateCPDB (const si:TGuid);
      var CONN :   IFRE_DB_CONNECTION;
          COLL :   IFRE_DB_COLLECTION;
          COLL2:   IFRE_DB_COLLECTION;
          COLL3:   IFRE_DB_COLLECTION;
          COLLE:   IFRE_DB_COLLECTION;
          COLLN:   IFRE_DB_COLLECTION;
          COLLM:   IFRE_DB_COLLECTION;
          COLLS: IFRE_DB_COLLECTION;


          lCOB : IFRE_DB_OBJECT;
          OB2  : IFRE_DB_OBJECT;
          OB3  : IFRE_DB_OBject;
          ola  : TFRE_DB_ObjLinkArray;


          i:integer;
          j:integer;

        function _addnetwork(const endpoint_id:TGUID;const opennw:boolean;const ssid:string;const ip:string;const wpakey:string;const sessiontimeout:integer=600):IFRE_DB_Object;
        var onb,cnb:IFRE_DB_Object;
        begin
         if opennw then begin
          onb:=conn.NewObject('TFRE_DB_OPENWIFINETWORK');
         end else begin
      //    onb:=conn.NewObject('TFRE_DB_RadiusNetwork');
          onb:=conn.NewObject('TFRE_DB_WPA2Network');
          onb.Field('hidden').AsBoolean:=true;
          onb.Field('wpa2psk').Asstring:=wpakey;
         end;
         onb.Field('ssid').Asstring:=ssid;
         onb.Field('endpoint').AsObjectLink := endpoint_id;
         onb.Field('ip_net').AsString:=ip;
         onb.Field('dns').AsString:='172.17.0.1';
         onb.Field('dhcp').AsBoolean:=true;
         onb.Field('dhcp_range_start').AsUInt16:=10;
         onb.Field('dhcp_range_end').AsUInt16:=250;
         onb.Field('dhcp_leasetime').AsUInt16:=600;
         onb.Field('dhcp_parameters').AsString:='';
         onb.Field('wds_extensionmacs').AsString:='';
         onb.Field('sessiontimeout').asuint32:=sessiontimeout;
         result:=onb;
        end;

        procedure _addendpoint_survey(const ep_uid:TGUID);
        var eps:IFRE_DB_Object;

            procedure _addClient(const mac:string;const ip:string;const signal,noise:integer);
            var cl :IFRE_DB_Object;
            begin
             cl:=GFRE_DBI.NewObject;
             cl.Field('mac').AsString:=mac;
             cl.Field('ip').AsString:=ip;
             cl.Field('signal').AsInt16:=signal;
             cl.Field('noise').AsInt16:=noise;
             eps.Field('clients').AddObject(cl);
            end;

            procedure _addOwnSSID(const ssid:string;const mac:string;const channel,noise:integer;const rates,caps:string);
            var sl :IFRE_DB_Object;
            begin
             sl:=GFRE_DBI.NewObject;
             sl.Field('ssid').AsString:=ssid;
             sl.Field('mac').AsString:=mac;
             sl.Field('channel').AsInt16:=channel;
             sl.Field('noise').AsInt16:=noise;
             sl.Field('rates').AsString:=rates;
             sl.Field('capability').AsString:=caps;
             eps.Field('ssid').AddObject(sl);
            end;

            procedure _addSurvey(const ssid:string;const mac:string;const channel,signal,noise,beacon,cap,dtim,rate:integer;const enc:string);
            var sl :IFRE_DB_Object;
            begin
             sl:=GFRE_DBI.NewObject;
             sl.Field('ssid').AsString:=ssid;
             sl.Field('mac').AsString:=mac;
             sl.Field('channel').AsInt16:=channel;
             sl.Field('signal').AsInt16:=signal;
             sl.Field('noise').AsInt16:=noise;
             sl.Field('beacon').AsInt16:=beacon;
             sl.Field('cap').AsInt16:=cap;
             sl.Field('dtim').AsInt16:=dtim;
             sl.Field('rate').AsInt16:=rate;
             sl.Field('enc').AsString:=enc;
             eps.Field('survey').AddObject(sl);
            end;

        begin
         eps:=COnn.NewObject('TFRE_DB_EP_SURVEY');
         eps.Field('endpointid').AsObjectLink:=ep_uid;
         _addClient('A4:67:06:36:59:6C','10.100.8.21',-34,-83);
         _addClient('D4:9A:20:5A:E0:68','10.100.8.23',-63,-83);
         _addClient('00:18:DE:5B:A4:EA','10.100.7.24',-24,-83);
         _addOwnSSID('FirmOS VPN1 Open','68:7F:74:CB:72:B3',-83,3,'1(b) 2(b) 5.5(b) 6 9 11(b) 12 18 24 36 48 54','ESS');
         _addSurvey('FirmOS','10:9A:DD:89:1C:4F',1,-36,-82,100,431,0,12,'Group-AES-CCMP Pair-AES-CCMP WPA2-PSK');
         _AddSurvey('CC Test Closed','6A:7F:74:CB:72:B6',6,-1,-90,100,411,0,12,'Group-AES-CCMP Pair-AES-CCMP WPA2');
         _AddSurvey('CC Test Open','68:7F:74:CB:72:B5',6,-3,-90,100,401,0,12,'Open');
         writeln(eps.DumpToString());
      //   CheckDbResult(COLLEPD.Store(eps),'Add Survey');
        end;

        procedure _addendpoints(const sob:IFRE_DB_Object;const ce:integer;const sn:string);
        var lEo:IFRE_DB_OBject;
            no :IFRE_DB_Object;
            cms_uid,cms_uid2:TGUID;
             i:integer;
             s:string;
             endpoint_id:TGUid;
             no_uid:TGuid;



        begin
         for i:=0 to ce-1 do begin
          lEo:=conn.NewObject('TFRE_DB_AP_LINKSYS_E1200V2');
          lEo.Field('external_ip').asstring:='';
          lEo.Field('dhcp').asboolean:=true;
          lEo.Field('routing').AsString:='disabled';
      //    lEo.Field('provisioned_time').AsDateTimeUTC:=GFRE_DT.Now_UTC;
      //    lEo.Field('online_time').AsDateTimeUTC:=GFRE_DT.Now_UTC+86400;
          lEo.Field('channel').AsUInt16:=0;
          lEo.Field('site').AsObjectlink:=sob.Field('site').AsObjectLink;
          endpoint_id:=lEo.UID;
          CheckDbResult(COLLE.Store(lEo),'Add Endpoint');

          s:='0'+inttostr(g_endpoint_number);
          s:='00:01:02:03:04:'+Copy(s,length(s)-1,2);
      //    writeln(sn);
          case sn of
           'dummy':;
          else
            no:=_addnetwork(endpoint_id,true,'CC EP '+inttostr(g_endpoint_number),'10.1.'+inttostr(g_network_number)+'.1/24','');
      //      writeln('start  :',GFRE_BT.GUID_2_HexString(startcms_uid));
      //      writeln('default:',GFRE_BT.GUID_2_HexString(defaultcms_uid));
      //      writeln('agb    :',GFRE_BT.GUID_2_HexString(agbcms_uid));

            no.Field('redirection_start').AddObjectLink(startcms_uid);
            no.Field('redirection_customer').AddObjectLink(defaultcms_uid);
      //      no.Field('redirection_customer').AddObjectLink(defaultcms_uid);
            no.Field('redirection_agb').AddObjectLink(agbcms_uid);
            no.Field('redirection_end').AddObjectLink(defaultcms_uid);
            CheckDbResult(COLLN.Store(no),'Add Network');

            inc(g_network_number);
            no:=_addnetwork(endpoint_id,false,'CC EP '+inttostr(g_endpoint_number),'10.1.'+inttostr(g_network_number)+'.1/24','test1234');
            no.Field('redirection_start').AddObjectLink(startcms_uid);
            no.Field('redirection_customer').AddObjectLink(defaultcms_uid);
            no.Field('redirection_agb').AddObjectLink(agbcms_uid);
      //      no.Field('redirection_end').AddObjectLink(defaultcms_uid);
            CheckDbResult(COLLN.Store(no),'Add Network');
            inc(g_network_number);
             //:9f;
          end;

          inc(g_endpoint_number);
          CONN.Fetch(endpoint_id,lEo);
          lEo.Field('provisioningmac').AsString:=s;
          lEo.Field('password').AsString:=lowercase(Copy(s,1,8));
          CONN.Update(lEo);
      //    CheckDbResult(COLLN.Store(no),'Add Network');
         end;
        end;

        procedure _addmobile(const sob:IFRE_DB_Object);
        var mo:IFRE_DB_OBject;
             s:string;
        begin
         mo:=CONN.NewObject('TFRE_DB_MobileDevice');
         mo.Field('objname').asstring:='Ipad '+inttostr(g_mobile_number);
         s:='0'+inttostr(g_mobile_number);
         inc(g_mobile_number);
         mo.Field('provisioningmac').AsString:='05:04:03:02:01:'+Copy(s,length(s)-1,2);
         //mo.Field('displayname').AsString:='Mobile Device '+mo.Field('provisioningmac').AsString;
         mo.Field('site').AsObjectLink:=sob.Field('site').AsObjectLink;
         CheckDbResult(COLLM.Store(mo),'Add Mobiledevice');
        end;

        procedure _addsiteext(const obj:IFRE_DB_Object);
        var sob:IFRE_DB_Object;
             uo:IFRE_DB_Object;
            sid:TGUID;
        begin
          sob:=CONN.NewObject('TFRE_DB_SITE_CAPTIVE_EXTENSION');
          sob.Field('site').asObjectLink:=obj.Field('UID').AsGUID;
          sob.Field('captiveportal').asobjectLink:=cp_uid;
          if realmode=false then begin
            _addendpoints(sob,cendpoints,obj.Field('objname').asstring);
            _addmobile(sob);
          end else begin
      //      _addendpoints(sob,1,obj.Field('name').asstring);
          end;
          sid:=sob.UID;
          CheckDbResult(COLL3.Store(sob),'Add Site Extension');
          CONN.Fetch(obj.UID,uo);
          uo.Field('extension').AddObjectLink(sid);
          conn.Update(uo);

        end;
        procedure _setredirectionflow(const name:string; const open:boolean; const nob: IFRE_DB_OBJECT);
        var lcoll          :         IFRE_DB_COLLECTION;
            lredirob       :         IFRE_DB_OBJECT;
            li             :         integer;

            procedure _setredir      (const redirfield,networkfield:string);
            begin
             if lredirob.FieldExists(redirfield) then begin
              nob.Field(networkfield).asObjectLink  :=  lredirob.Field(redirfield).AsObjectLink;
             end else begin
              // leave unset
             end;
            end;

            function _findredirection(const field:IFRE_DB_Field):boolean;
            begin
             result:=field.AsString=name;
            end;

        begin
          lcoll     := CONN.Collection('redirectionflow');
          lredirob  := lcoll.LinearScan('objname',@_findredirection);
          if assigned (lredirob) then begin
           _setredir('redirection_start','redirection_start');
           _setredir('redirection_customer','redirection_customer');
           _setredir('redirection_end','redirection_end');
           if open then begin
            _setredir('redirection_agb_open','redirection_agb');
           end else begin
            _setredir('redirection_agb_ipad','redirection_agb');
           end;
           for li:=0 to lredirob.Field('urlexceptions').ValueCount-1 do begin
            nob.Field('urlexceptions').AddString(lredirob.Field('urlexceptions').AsStringItem[li]);
           end;
           writeln(nob.DumpToString());
          end else begin
           raise Exception.Create ('Cant find redirection flow '+name);
          end;
        end;

        procedure _addMonitorStatus (const endpoint_uid:TGUID);
        var lmob     :  IFRE_DB_Object;
            leob     :  IFRE_DB_Object;
            lmob_uid :  TGUID;
            lstatus  :  TFRE_DB_String;
            lindex   :  integer;

        begin
          lmob :=  conn.NewObject('TFRE_DB_Monitoring_Status');

          lindex      := random (4);
          case lindex of
           0: lstatus :='OK';
           1: lstatus :='WARNING';
           2: lstatus :='FAILURE';
           3: lstatus :='UNKNOWN';
          end;

          lmob .Field ('status').AsString :=lstatus;

          lmob .Field ('provisioned_time').AsDateTimeUTC:=GFRE_DT.Now_UTC;
          lmob .Field ('online_time').AsDateTimeUTC:=GFRE_DT.Now_UTC+10000;
          lmob_uid    := lmob.UID;
          writeln(lmob.DumpToString());
          CheckDbResult(COLLS.Store(lmob),'Add Monitoring Status');

          CONN.Fetch(endpoint_uid,leob);
          leob .Field ('status_uid').AsObjectLink:=lmob_uid;
          writeln(leob.DumpToString());
          CONN.Update(leob);

        end;

        procedure _addendpoint(const sitename,mac,serial,deviceclass,redirection,networkgroup,ssidopen,ssidclosed,netopen,netclosed,wpakey:string;const ip:string='';const dhcp_ip:string='';const mounting_detail:string=''; const vlan_id:ShortInt=0);
        var lsiteob        :         IFRE_DB_Object;
            leob           :         IFRE_DB_Object;
            leob_uid       :         TGUID;
            lnetworkob     :         IFRE_DB_Object;
            lnetworkob_uid :         TGUID;

            function _findsite       (const field:IFRE_DB_Field):boolean;
            begin
             result:=field.AsString=sitename;
            end;

            procedure _addsubnet     (const net:string);
            var       lip            :         string;
            begin
             lip:=StringReplace(GetNetworkIDfromString(net),'_','.',[rfReplaceall]);
             writeln(lip);
             AddSubnetDHCP(lip+'0/24',lip+'20',lip+'200',lip+'1','172.17.0.1',conn);
            end;

        begin
         lsiteob:=COLL2.LinearScan('objname',@_findsite);
         if assigned(lsiteob) then begin
          lEob:=conn.NewObject(deviceclass);               // Create Endpoint
          lEob.Field('external_ip').asstring   :=ip;
          if ip='' then begin
           lEob.Field('dhcp').asboolean        :=true;
          end else begin
           lEob.Field('dhcp').asboolean        :=false;
          end;
          lEob.Field('routing').AsString       :='disabled';
          lEob.Field('channel').AsUInt16       :=0;
          lEob.Field('site').AsObjectlink      :=lsiteob.UID;
          lEob.Field('serialnumber').AsString  :=serial;
          leob.Field('provisioningmac').AsString:=lowercase(mac);
          leob.Field('password').AsString:=lowercase(Copy(mac,1,8));
          leob.Field('mountingdetail').AsString:=mounting_detail;
          leob_uid       :=lEob.UID;
          CheckDbResult(COLLE.Store(lEob),'Add Endpoint');        // Store Endpoint

          if (netopen='') and (netclosed='') then begin
           exit; // dont create subnets
          end;

          lnetworkob     :=      _addnetwork(leob_uid,true,ssidopen,netopen,'',1800);
          if vlan_id<>0 then begin
            lnetworkob.Field('vlan_id').AsUInt16:=vlan_id;
          end;
          _setredirectionflow    (redirection,true,lnetworkob);
          lnetworkob_uid :=      lnetworkob.UID;
          CheckDbResult          (COLLN.Store(lnetworkob),'Add Network');
          AssignNetworkToGroup   (lnetworkob_uid,networkgroup,CONN);

          if (netclosed<>'') then begin
            lnetworkob     :=      _addnetwork(leob_uid,false,ssidclosed,netclosed,wpakey,1800);
            _setredirectionflow    (redirection,false,lnetworkob);
            lnetworkob_uid :=      lnetworkob.UID;
            CheckDbResult          (COLLN.Store(lnetworkob),'Add Network');
            AssignNetworkToGroup   (lnetworkob_uid,networkgroup,CONN);
          end;

          _addmonitorStatus(leob_uid);

          if dhcp_ip<>'' then begin
           AddFixedDHCP(dhcp_ip,mac,StringReplace(sitename,' ','',[rfReplaceAll])+StringReplace(mac,':','',[rfReplaceAll]),conn);
          end;
          _AddSubnet(netopen);
          if (netclosed<>'') then begin
            _addSubnet(netclosed);
          end;
          //   halt;
         end else begin
          raise Exception.Create('Sitename '+sitename+' not found in _addendpoint');
         end;

        end;

      begin
        g_network_number:=1;
        g_endpoint_number:=1;
        g_mobile_number:=1;
        g_cust_number:=1;

        randomize;

        CONN := GFRE_DBI.NewConnection;
        CONN.Connect(dbname,'admin','admin');



        COLL := CONN.Collection('service');

        lCOB:=CONN.NewObject('TFRE_DB_CAPTIVEPORTAL');
        lcob.Field('machineid').AsObjectLink:=wlan_machine_uid;
        lcob.Field('linksys_net').AsString:='10.0.0.0/24';
        lcob.Field('lancom_net').AsString:='10.200.0.0/24';
        lcob.Field('vpn_net').AsString:='10.100.0.0/24';
        lCOb.Field('servicegroup').AsObjectLink:=si;
        cp_uid:=lCOB.UID;
        CheckDbResult(COLL.Store(lcob),'Add CaptivePortal');

        COLL2:= CONN.Collection('site');
        COLL3:= CONN.Collection('sitecaptiveextension');
        COLLE:= CONN.Collection('endpoint');
        COLLN:= CONN.Collection('network');
        COLLM:= CONN.Collection('mobiledevice');

        COLLS := CONN.Collection('monitorstatus');

        COLL2.ForAll(@_addsiteext);

        // AddEndpoint
        _addendpoint('Standort Andritz','c0:c1:c0:3f:9a:5f','','TFRE_DB_AP_LINKSYS_E1000','schilhan','citypad','CC Schilhan Open','CC Schilhan iPad','10.0.5.1/24','10.0.6.1/24','SqZ7eXNy','','172.17.0.21');
      //  _addendpoint('Standort Intern1','c0:c1:c0:3f:9a:9f','','E1000V2','elevate','citypad','Citypad Inhouse1 Open','Citypad Inhouse1 iPad','10.0.7.1/24','10.0.8.1/24','ih3476ap1','','172.17.0.23');

        _addendpoint('Standort TestVPN1','C0:C1:C0:3F:9A:37','','TFRE_DB_AP_LINKSYS_E1000','stadt','citypad','FirmOS VPN1 Open','FirmOS VPN1 iPad','10.100.7.1/24','10.100.8.1/24','key4me123');
        _addendpoint('Standort Test1','C0:C1:C0:3F:9A:0F','','TFRE_DB_AP_LINKSYS_E1000','stadt','citypad','Citypad Test1','Citypad Test1 i','10.0.11.1/24','10.0.12.1/24','key4me123','','172.17.0.25');
      //  addendpoint('Standort Test3','58:6D:8F:51:65:37','','E1200V1','elevate','citypad','Citypad Test3 Open','Citypad Test3 iPad','10.0.13.1/24','10.0.14.1/24','key4me123','','172.17.0.26');
        _addendpoint('Standort FirmOS Local','ff:ff:ff:ff:ff:00','','TFRE_DB_AP_LINKSYS_E1000','cityaccess','cityaccess','FirmOS Local','FirmOS Test','10.1.0.0/24','91.114.28.0/24','key4me123');         // for redirection tests
        _addendpoint('Standort Demo','ff:ff:ff:ff:ff:00','','TFRE_DB_AP_LINKSYS_E1000','demo','demo','FirmOS Demo ','','0.0.0.1/24','','key4me123');     // for demo usage
        _addendpoint('Standort Test','ff:ff:ff:ff:ff:ff','','TFRE_DB_AP_LINKSYS_E1000','test','test','FirmOS Test ','','172.24.0.1/24','','test');         // for test usage

        _addendpoint('Standort VPN3','58:6D:8F:51:65:37','','TFRE_DB_AP_LINKSYS_E1200','stadt','citypad','Citypad VPN3','Citypad VPN3 i','10.100.5.1/24','10.100.6.1/24','key4me123');

        _addendpoint('Standort Forum','00:a0:57:18:97:cf','4002050118100200','TFRE_DB_AP_LANCOM_OAP321','cityaccess','cityaccess','Cityaccess Forum Open','','10.200.1.1/24','','key4me123','10.200.1.11');
        _addendpoint('Standort Forum','00:a0:57:18:2c:03','4002082018100004','TFRE_DB_AP_LANCOM_IAP321','','','','','','','key4me123','10.200.1.12');
        _addendpoint('Standort Forum','00:a0:57:18:2c:02','4002082018100003','TFRE_DB_AP_LANCOM_IAP321','','','','','','','key4me123','10.200.1.13');
        _addendpoint('Standort Forum','00:a0:57:18:2c:00','4002082018100001','TFRE_DB_AP_LANCOM_IAP321','','','','','','','key4me123','10.200.1.21');

        _addendpoint('Standort Gady','00:a0:57:18:2c:01','','TFRE_DB_AP_LANCOM_IAP321','gady','gady','Cityaccess Open','','10.200.3.1/24','','key4me123','10.200.3.11');

        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2D:11','4002105818100054','TFRE_DB_AP_LANCOM_IAP321','joanneum','joanneum','Cityaccess Open','','10.200.5.1/24','','ucx71MA1','10.200.5.11','','Joanneum AP1',1700);
        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2D:12','4002105818100055','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.5.12','','Joanneum AP2');
        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2D:10','4002105818100053','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.5.13','','Joanneum AP3');
        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2D:0F','4002105818100052','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.5.14','','Joanneum AP4');
        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2D:0E','4002105818100051','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.5.15','','Joanneum Cafe');
        _addendpoint('Standort Universalmuseum Neutorgasse','00:A0:57:18:2F:4D','4002256018100100','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.5.16','','Joanneum 2.OG');

        _addendpoint('Standort ShoppingNord','00:00:00:00:00:03','4002105818100090','TFRE_DB_AP_LANCOM_IAP321','ekz','ekz','Cityaccess Open','','10.200.7.1/24','','key4me123','10.200.7.11','','EKZ-Nord AP1',1206);
        _addendpoint('Standort ShoppingNord','00:00:00:00:00:03','4002105818100070','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.7.12','','EKZ-Nord AP2');

        _addendpoint('Standort Hauptplatz','	00:A0:57:18:2D:1A','4002105818100060','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess Open','','10.200.9.1/24','','key4me123','10.200.9.11');
        _addendpoint('Standort Jakominiplatz','00:A0:57:18:EC:7C','4002256418100100','TFRE_DB_AP_LANCOM_OAP321','stadt','stadt','Cityaccess Open','','10.200.11.1/24','','key4me123','10.200.11.11','','',1212);
        _addendpoint('Standort Citybeach','00:A0:57:18:EB:F1','','TFRE_DB_AP_LANCOM_OAP321','citybeach','stadt','Cityaccess Open','','10.200.13.1/24','','key4me123','10.200.13.11','','',1214);

        _addendpoint('Standort Oper','00:A0:57:18:2F:49','4002256018100096','TFRE_DB_AP_LANCOM_IAP321','oper','cityaccess','Cityaccess Open','','10.200.15.1/24','','key4me123','10.200.15.11','','Oper Erdgeschoss',1221);
        _addendpoint('Standort Oper','00:A0:57:18:2F:83','4002289618100044','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.15.12','','Oper 1.OG');
        _addendpoint('Standort Oper','00:A0:57:18:2F:46','4002256018100093','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.15.13','','Oper 3.OG');
        _addendpoint('Standort Oper','00:A0:57:18:2F:48','4002256018100095','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.15.14','','Oper Kantine');

        _addendpoint('Standort Schauspielhaus','00:A0:57:18:2F:70','4002289618100025','TFRE_DB_AP_LANCOM_IAP321','schauspielhaus','cityaccess','Cityaccess Open','','10.200.17.1/24','','key4me123','10.200.17.11','','Schauspielhaus EG',1224);
        _addendpoint('Standort Schauspielhaus','00:A0:57:18:2F:84','4002289618100045','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.17.12','','Schauspielhaus 1.OG');
        _addendpoint('Standort Schauspielhaus','00:A0:57:18:2F:81','4002289618100042','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.17.13','','Schauspielhaus 3.OG');
        _addendpoint('Standort Schauspielhaus','00:A0:57:18:EC:62','4002256418100071','TFRE_DB_AP_LANCOM_OAP321','','','','','','','','10.200.17.14','','Freiheitsplatz');
        _addendpoint('Standort Schauspielhaus','00:A0:57:18:30:e6','4002541818100119','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.17.15','','Schauspielhaus Gaeste');

        _addendpoint('Standort Lendplatz','00:A0:57:18:EC:A6','4002256618100039','TFRE_DB_AP_LANCOM_OAP321','stadt','stadt','Cityaccess Open','','10.200.19.1/24','','key4me123','10.200.19.11','','',1226);
        _addendpoint('Standort Eisernes Tor','00:A0:57:18:EF:0B','4002542018100016','TFRE_DB_AP_LANCOM_OAP321','stadt','stadt','Cityaccess','','10.200.21.1/24','','key4me123','10.200.21.11','','',1229);

        _addendpoint('Standort Hauptbahnhof','00:A0:57:18:30:B7','4002541818100072','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess','','10.200.23.1/24','','key4me123','10.200.23.11','','Hauptbahnhof Bus',411);
        _addendpoint('Standort Hauptbahnhof','00:A0:57:18:30:B5','4002541818100070','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.23.12','','Hauptbahnhof Bim');

        _addendpoint('Standort Novapark','00:A0:57:18:30:B3','4002541818100068','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess','','10.200.25.1/24','','key4me123','10.200.25.11','','',1249);
        _addendpoint('Standort Schlossberg','00:A0:57:18:ef:26','4002542018100043','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess','','10.200.27.1/24','','key4me123','10.200.27.11','','',1252);
        _addendpoint('Standort Orpheum','00:A0:57:18:32:0E','4002610618100139','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess','','10.200.29.1/24','','key4me123','10.200.29.11','','Orpehum AP1',1155);
        _addendpoint('Standort Orpheum','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.29.12','','Orpehum AP2');
        _addendpoint('Standort Orpheum','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','','','','','','','','10.200.29.13','','Orpehum AP3');
        _addendpoint('Standort Karmeliterplatz','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','stadt','stadt','Cityaccess','','10.200.33.1/24','','key4me123','10.200.33.11','','Karmeliterplatz VP',303);

        _addendpoint('Standort MonitoringTest','ff:ff:ff:ff:ff:ff','','TFRE_DB_AP_LANCOM_IAP321','test','test','FirmOS Test ','FirmOS Test i','10.199.1.1/24','10.199.2.1/24','test','10.199.1.1','','VLAN Interface');

        _addendpoint('Standort Kunde1','20:AA:4B:05:9A:D7','10820C61234077','TFRE_DB_AP_LINKSYS_E1200V2','stadt','citypad','Citypad VPN1 Open','Citypad VPN1 iPad','10.100.1.1/24','10.100.2.1/24','kunde4me123','','');
        _addendpoint('Standort Margarethenbad','20:AA:4B:0A:62:8A','10820C61259334','TFRE_DB_AP_LINKSYS_E1200V2','margarethenbad','citypad','Citypad Margarethenbad','Citypad Margarethenbad i','10.0.23.1/24','10.0.24.1/24','kunde4me123','','172.17.0.28');
        _addendpoint('Standort Huber','20:AA:4B:0A:73:36','10820C61260401','TFRE_DB_AP_LINKSYS_E1200V2','stadt','citypad','Citypad Huber','Citypad Huber i','10.0.29.1/24','10.0.30.1/24','fk7tn5md','','172.17.0.31');
        _addendpoint('Standort n-box','20:AA:4B:0A:88:0A','10820C61261734','TFRE_DB_AP_LINKSYS_E1200V2','stadt','citypad','Citypad n-box','Citypad n-box i','10.0.31.1/24','10.0.32.1/24','vklro732','','172.17.0.32');
        _addendpoint('Standort Droneberger','20:AA:4B:05:9F:E7','10820C61234401','TFRE_DB_AP_LINKSYS_E1200V2','stadt','citypad','Citypad Droneberger','Citypad Droneberger i','10.0.33.1/24','10.0.34.1/24','kflmd13x','','172.17.0.33');
        _addendpoint('Standort Tribeka1','20:AA:4B:05:85:DB','10820C61232734','TFRE_DB_AP_LINKSYS_E1200V2','tribeka','citypad','Citypad Tribeka','Citypad Tribeka i','10.0.37.1/24','10.0.38.1/24','ico48sod','','172.17.0.35');
        _addendpoint('Standort Tribeka2','20:AA:4B:0A:78:82','10820C61260740','TFRE_DB_AP_LINKSYS_E1200V2','tribeka','citypad','Citypad Tribeka','Citypad Tribeka i','10.0.27.1/24','10.0.28.1/24','ico48sod','','172.17.0.30');
        _addendpoint('Standort Tribeka3','20:AA:4B:05:85:53','10820C61232700','TFRE_DB_AP_LINKSYS_E1200V2','tribeka','citypad','Citypad Tribeka','Citypad Tribeka i','10.0.35.1/24','10.0.36.1/24','ico48sod','','172.17.0.34');
        _addendpoint('Standort Tribeka4','20:AA:4B:0A:5C:86','10820C61258949','TFRE_DB_AP_LINKSYS_E1200V2','tribeka','citypad','Citypad Tribeka','Citypad Tribeka i','10.0.25.1/24','10.0.26.1/24','ico48sod','','172.17.0.29');
      //  _addendpoint('Standort Kunde10','20:AA:4B:0A:78:5A','10820C61260730','TFRE_DB_AP_LINKSYS_E1200V2','stadt','citypad','Citypad Kunde10 Open','Citypad Kunde10 iPad','10.0.39.1/24','10.0.40.1/24','kunde4me123','','172.17.0.36');
        _addendpoint('Standort Intern1','20:AA:4B:0A:78:5A','10820C61260730','TFRE_DB_AP_LINKSYS_E1200V2','cityaccess','citypad','Citypad Inhouse1 Open','Citypad Inhouse1 iPad','10.0.7.1/24','10.0.8.1/24','ih3476ap1','','172.17.0.23');

        _addendpoint('Standort Auster','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','baeder','stadt','Cityaccess','','10.200.35.1/24','','key4me123','10.200.35.11','','Auster',1008);
        _addendpoint('Standort Eishalle','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','cityaccess','cityaccess','Cityaccess','','10.200.31.1/24','','key4me123','10.200.31.11','','Eishalle',1258);
        _addendpoint('Standort Event','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','cityaccess','cityaccess','Cityaccess','','10.200.37.1/24','','key4me123','10.200.37.11','','Event',1222);
        _addendpoint('Standort Eggenberg','00:00:00:00:00:00','','TFRE_DB_AP_LANCOM_IAP321','eggenberg','cityaccess','Cityaccess','','10.200.39.1/24','','key4me123','10.200.39.11','','Eggenberg',1166);


//        for i := 4 to 50 do begin
//         _addendpoint('Standort VPN','aa:00:00:00:00:'+IntToHex(i,2),'','TFRE_DB_AP_LINKSYS_E1000','stadt','citypad','Citypad VPN'+inttostr(i),'Citypad VPN'+inttostr(i),'10.101.'+inttostr(i*2-1)+'.1/24','10.101.'+inttostr(i*2)+'.1/24','key4me123');
//         writeln('aa:00:00:00:00:'+IntToHex(i,2));
//        end;

        writeln('CREATE MONITORING');
        CreateMonitoring (conn);

        CONN.Finalize;
      end;


     procedure CreateRouteDB(const si: TGuid);
     var conn:IFRE_DB_Connection;
         coll:IFRE_DB_Collection;
         cob :IFRE_DB_Object;
         sn  :IFRE_DB_Object;

       procedure _addroute(const gw,subnet:string);
       var r:IFRE_DB_Object;
       begin
        r:=Conn.NewObject('TFRE_DB_Route');
        r.Field('subnet').AsString:=subnet;
        r.Field('gateway').AsString:=gw;
        cob.Field('static').AddObject(r);
       end;

     begin
      CONN := GFRE_DBI.NewConnection;
      CONN.Connect(dbname,'admin','admin');

      COLL := CONN.Collection('service');

      COB:=CONN.NewObject('TFRE_DB_ROUTING');
      cob.Field('machineid').AsObjectLink:=wlan_machine_uid;
      COb.Field('servicegroup').AsObjectLink:=si;
      COb.Field('default').Asstring:='10.220.250.1';

      _AddRoute('172.17.0.21','10.0.1.0/24');  //fos intern
      _AddRoute('172.17.0.21','10.0.2.0/24');
      _AddRoute('172.17.0.22','10.0.5.0/24');  //schilhan
      _AddRoute('172.17.0.22','10.0.6.0/24');
      _AddRoute('172.17.0.23','10.0.7.0/24');  //citycom intern 1
      _AddRoute('172.17.0.23','10.0.8.0/24');

      _AddRoute('172.17.0.2','10.100.0.0/16');    // vpn server
      _AddRoute('172.17.0.2','192.168.99.0/24');
      CheckDbResult(COLL.Store(cob),'Add Routing');

     // writeln('Routing:');
     // writeln(cob.DumpToString());

      CONN.Finalize;
     end;

     procedure CreateVPNDB(const si: TGuid; const name: string);
     var conn:IFRE_DB_Connection;
         coll:IFRE_DB_Collection;
         cob :IFRE_DB_Object;
         i   : integer;
     begin

      CONN := GFRE_DBI.NewConnection;
      CONN.Connect(dbname,'admin','admin');

      COLL := CONN.Collection('service');

      COB:=CONN.NewObject('TFRE_DB_VPN');
      cob.Field('machineid').AsObjectLink:=vpn_machine_uid;
      cob.Field('servicegroup').AsObjectLink:=si;
      COB.Field('objname').AsString  :=name;

      vpn_uid:=COB.UID;
      cob.Field('external_address').AsString:='0.0.0.0';
      cob.Field('protocol').AsString:='tcp';
      cob.Field('port').AsUInt16:=1194;
      cob.Field('compression').asboolean:=true;
      cob.Field('mtu').AsUInt16:=1500;
      cob.Field('server_ip').AsString:='192.168.99.0/24';

      cob.Field('push_routes').AddString('172.17.0.0/16');
      for i:= 0 to 99 do begin
       cob.Field('routes').AddString('10.100.'+inttostr(i)+'.0/24');
      end;

      cob.Field('cipher').AsString:='BF-CBC';
      cob.Field('max_clients').AsUInt16:=50;
      cob.Field('redirect_gateway').AsBoolean:=true;
      CheckDbResult(COLL.Store(cob),'Add VPN');

     // writeln('VPN:');
     // writeln(cob.DumpToString());
      CONN.Finalize;
     end;

     procedure CreateCADB(const si: TGuid;const name:string);
     var conn:IFRE_DB_Connection;
         coll:IFRE_DB_Collection;
         colls:IFRE_DB_Collection;
         lcob:IFRE_DB_Object;
         cp:IFRE_DB_Object;
         vpn_ap:IFRE_DB_Object;
         vpn:IFRE_DB_Object;
         cob_uid:TGUID;
         hio:IFRE_DB_Object;
         i:integer;

       function _addCert(const cn,c,email,st,o,ou,l:string;const server:boolean):TGUID;
       var crt:IFRE_DB_Object;
           lcoll     :        IFRE_DB_COLLECTION;
           lmac      :        string;
           nr        :        integer;

           function _findmac(const field:IFRE_DB_Field):boolean;
           begin
            result:=field.AsString=lmac;
           end;

       begin
        crt:=CONN.NewObject('TFRE_DB_Certificate');
        crt.Field('ca').AsObjectLink :=cob_uid;
        crt.Field('cn').asstring     :=cn;
        crt.Field('c').asstring      :=c;
        crt.Field('email').asstring  :=email;
        crt.Field('l').asstring      :=l;
        crt.Field('st').asstring     :=st;
        crt.Field('o').asstring      :=o;
        crt.Field('ou').asstring     :=ou;
        //Crt.Field('displayname').AsString:=cn;
     //   if cn='ap5' then begin
     //     crt.Field('revoke').AsBoolean:=true;
     //   end;
        crt.Field('server').AsBoolean:=server;
        result:=crt.UID;
        CheckDbResult(COLLS.Store(crt),'Add Certificate');
        if cn='ap1' then begin
         lcoll   := CONN.Collection('endpoint');
         lmac    := lowercase('20:AA:4B:05:9A:D7');
         vpn_ap  := lcoll.LinearScan('provisioningmac',@_findmac);
         if assigned (vpn_ap) then begin
          writeln('VPN AP '+GFRE_BT.GUID_2_HexString(vpn_ap.uid));
         end else begin
          raise Exception.Create('VPN AP1 not found in DB !');
         end;
         vpn_ap.Field('vpn_crtid').AsObjectLink:=result;
         conn.Update(vpn_ap);
        end else
        if cn='ap3' then begin
         lcoll   := CONN.Collection('endpoint');
         lmac    := lowercase('58:6D:8F:51:65:37');
         vpn_ap  := lcoll.LinearScan('provisioningmac',@_findmac);
         if assigned (vpn_ap) then begin
          writeln('VPN AP '+GFRE_BT.GUID_2_HexString(vpn_ap.uid));
         end else begin
          raise Exception.Create('VPN AP3 not found in DB !');
         end;
         vpn_ap.Field('vpn_crtid').AsObjectLink:=result;
         conn.Update(vpn_ap);
        end else
        if Pos('ap',cn)=1 then begin
         nr      := strtoint(copy(cn,3,maxint));
         if nr > 3 then begin
           lcoll   := CONN.Collection('endpoint');
           lmac    := lowercase('aa:00:00:00:00:'+IntToHex(nr,2));
           writeln (lmac);
           vpn_ap  := lcoll.LinearScan('provisioningmac',@_findmac);
           if assigned (vpn_ap) then begin
             writeln('VPN AP '+GFRE_BT.GUID_2_HexString(vpn_ap.uid));
           end else begin
             raise Exception.Create('VPN AP'+inttostr(nr)+' not found in DB !');
           end;
           vpn_ap.Field('vpn_crtid').AsObjectLink:=result;
           conn.Update(vpn_ap);
         end;
        end;
       end;

     begin
      vpn:=nil;
      CONN := GFRE_DBI.NewConnection;
      CONN.Connect(dbname,'admin','admin');

      COLL := CONN.Collection('service');
      COLLS := CONN.Collection('certificate');

      lCOB:=CONN.NewObject('TFRE_DB_CA');
      lcob.Field('machineid').AsObjectLink:=wlan_machine_uid;
      lCOb.Field('servicegroup').AsObjectLink:=si;
      lCOB.Field('objname').AsString  :=name;
      lCOB.Field('cn').asstring    :=name;
      lCOB.Field('c').asstring     :='AT';
      lCOB.Field('email').asstring :='';
      lCOB.Field('l').asstring     :='Graz';
      lCOB.Field('st').asstring    :='Steiermark';
      lCOB.Field('o').asstring     :='Citycom Graz';
      lCOB.Field('ou').asstring    :=name+' CA';
      //lCOb.Field('displayname').AsString:=name;
      cob_uid := lcob.uid;
      CheckDbResult(COLL.Store(lcob),'Add CA');
      writeln('CA '+name);
      if name<>'VPN' then begin
       ca_r_uid:=cob_uid;
       ca_r_s_uid:=_AddCert('Server','AT','server@ipad.citypad.at','Steiermark','Citycom Graz','iPad CA','Graz',true);

       _AddCert('iPadDQTFP9UTDFHW','AT','citycomDQTFP9UTDFHW@ipad.citypad.at','Steiermark','Citycom Graz','iPad CA','Graz',false);
       _AddCert('iPadDQTFLG1SDFHW','AT','citycomDQTFLG1SDFHW@ipad.citypad.at','Steiermark','Citycom Graz','iPad CA','Graz',false);
      end else begin
       if Conn.Fetch(cp_uid,cp)=false then raise Exception.Create('no captivportal cp_uid set');
       cp.Field('vpn_gateway').AsString:='vpn1.citypad.at';
       cp.Field('vpn_caid').AsObjectLink:=cob_uid;
       conn.Update(cp);

       if Conn.Fetch(vpn_uid,vpn)=false then raise Exception.Create('no vpn vpn_uid set');
       vpn.Field('caid').AsObjectLink  := cob_uid;
       vpn.Field('crtid').AsObjectLink := _AddCert('Server','AT','server@vpn.citypad.at','Steiermark','Citycom Graz','VPN CA','Graz',true);
       conn.Update(vpn);
       vpn:=nil;

//       for i:= 1 to 50 do begin
//         _AddCert('ap'+inttostr(i),'AT','ap'+inttostr(i)+'@vpn.citypad.at','Steiermark','Citycom Graz','VPN CA','Graz',false);
//         writeln('Cert '+inttostr(i));
//       end;
      end;

     // writeln('CA:');
     // writeln(cob.DumpToString());

      // test
      writeln('CREATE HAL TRANSPORT --------------------------------------');
      hio:=CONN.NewObject('TFRE_DB_HAL_CA_TRANSPORT_V10');

      CONN.Finalize;
     end;

begin
 realmode := real;
 customer_id := CreateBCDB;
 si:=CreateSGDB(customer_id);
 CreateCMS(si);
 CreateDHCPDB(si);
 CreateCPDB(si);
 CreateRouteDB(si);
 CreateVPNDB(si,'server01');
// CreateCADB(si,'iPad');
 CreateCADB(si,'VPN');
// CreateRadius(si);
end;



procedure Register_DB_Extensions;
var enum      : IFRE_DB_Enum;
begin
  enum:=GFRE_DBI.NewEnum('routing').Setup(GFRE_DBI.CreateText('$enum_routing','Routing Enum'));
  enum.addEntry('enabled',GFRE_DBI.CreateText('$enum_routing_enabled','Enabled'));
  enum.addEntry('disabled',GFRE_DBI.CreateText('$enum_routing_disabled','Disabled'));
  enum.addEntry('nat',GFRE_DBI.CreateText('$enum_routing_nat','NAT'));
  GFRE_DBI.RegisterSysEnum(enum);


  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DEVICE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_NETWORK_GROUP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ROUTE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Captiveportal);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Site_Captive_Extension);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Endpoint);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Accesspoint);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1000);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1200);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Linksys_E1200V2);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom_IAP321);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_AP_Lancom_OAP321);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MobileDevice);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Network);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WifiNetwork);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_OpenWifiNetwork);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WPA2Network);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_RadiusNetwork);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Monitoring_Status);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CA);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Certificate);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Subnet);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_DHCP_Fixed);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_VPN);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_RADIUS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Routing);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_PAGE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_CMS_ADPAGE);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_REDIRECTION_FLOW);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MONAC);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_BAC);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WF);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SIAC_CAP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SIAC_CA);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SIAC_DHCP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SIAC_CMS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_SIAC);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WAC);
  GFRE_DBI.Initialize_Extension_Objects;
end;

procedure CAPTIVE_MetaRegister;
begin
   FRE_DBBUSINESS.Register_DB_Extensions;
   FRE_DBMONITORING.Register_DB_Extensions;
   fre_testcase.Register_DB_Extensions;
   fre_zfs.Register_DB_Extensions;
   FRE_DBCLOUDCONTROL.Register_DB_Extensions;
   FRE_HAL_TRANSPORT.Register_DB_Extensions;
end;

procedure CAPTIVE_MetaInitializeDatabase(const dbname: string; const user, pass: string);
begin
  writeln('METAINIT CAPTIVE');
  InitializeCAPTIVE(dbname,user,pass);
  CreateDB(true,dbname,user,pass);
end;

procedure CAPTIVE_MetaRemove(const dbname: string; const user, pass: string);
var conn  : IFRE_DB_SYS_CONNECTION;
     res   : TFRE_DB_Errortype;
     i     : integer;
     login : string;
begin
   CONN := GFRE_DBI.NewSysOnlyConnection;
   try
     res  := CONN.Connect('admin','admin');
     if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);
     conn.RemoveApp('WAC');
   finally
     conn.Finalize;
   end;
end;


initialization
  GFRE_DBI_REG_EXTMGR.RegisterNewExtension('CAPTIVE',@CAPTIVE_MetaRegister,@CAPTIVE_MetaInitializeDatabase,@CAPTIVE_MetaRemove);
end.

