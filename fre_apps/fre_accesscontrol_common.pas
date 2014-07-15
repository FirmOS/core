unit fre_accesscontrol_common;
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  FOS_TOOL_INTERFACES,
  FRE_DB_INTERFACE,
  FRE_DBBASE,
  FRE_DB_COMMON
  ;

const
  CHIDE_INTERNAL  = true;

type

  { TFRE_COMMON_ACCESSCONTROL_APP }

  TFRE_COMMON_ACCESSCONTROL_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;
    procedure       _UpdateSitemap                (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize           (const session: TFRE_DB_UserSession);override;
    procedure       MySessionPromotion            (const session: TFRE_DB_UserSession); override;
    class procedure InstallDBObjects              (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); override;
    class procedure InstallDBObjects4SysDomain    (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); override;
  public
    class procedure RegisterSystemScheme          (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    function        isMultiDomainApp             : Boolean; override;
  published
  end;

  { TFRE_COMMON_USER_MOD }

  TFRE_COMMON_USER_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _getUsersString           (const logins: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION;const ses: IFRE_DB_UserSession): String;
    function        _getDetails               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _getNoUserDetails         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _checkGGConditions        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):Boolean;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    procedure       CalculateRoleIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateGroupFields      (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateUserIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UserSelected          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentNoUserSel      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentInfo           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentNote           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentRoles          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddUser               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteUser            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteUserConfirmed   (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GOGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GOGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromGroup       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToGroup            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_GROUP_MOD }

  TFRE_COMMON_GROUP_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _getGroupsString          (const groups: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION;const ses: IFRE_DB_UserSession): String;
    function        _addremoverole            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION;const addrole:boolean): IFRE_DB_Object;
    function        _getDetails               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _getNoGroupDetails        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _checkUGConditions        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):Boolean;
    function        _checkRGConditions        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):Boolean;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    procedure       CalculateRoleFields       (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateGroupIcon        (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateUserIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentNoGroupSel     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentRoles          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddGroup              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CreateGroup           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ModifyGroup           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_SaveGroup             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteGroup           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteGroupConfirmed  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GGNotification        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UOGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UIGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UOGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromUser        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToUser             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ROGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RIGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ROGNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToRole             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromRole        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_ROLE_MOD }

  TFRE_COMMON_ROLE_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _addremoverole            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION; const addrole:boolean):IFRE_DB_Object;
    function        _getDetails               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _getNoRoleDetails         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):TFRE_DB_CONTENT_DESC;
    function        _checkGGConditions        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):Boolean;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    procedure       CalculateRoleIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateGroupIcon        (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateUserIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentNoRoleSel      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RoleNotification      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToRole             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIRMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GORMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIRNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GORNotification       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromRole        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_DOMAIN_MOD }

  TFRE_COMMON_DOMAIN_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    procedure       CalculateDomainIcon       (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateGroupIcon        (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
    procedure       CalculateUserIcon         (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object);
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddDomain             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CreateDomain          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ModifyDomain          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeActivateDomain      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_SaveDomain            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DGNotification        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_WF_MOD }

  TFRE_COMMON_WF_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

procedure Register_DB_Extensions;

implementation

{ TFRE_COMMON_WF_MOD }

class procedure TFRE_COMMON_WF_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_WF_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('wf_description')
end;

class procedure TFRE_COMMON_WF_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.9';
  if currentVersionId='' then begin
    currentVersionId:='0.9';
    CreateModuleText(conn,'gc_wf_caption','Caption');
    CreateModuleText(conn,'gc_wf_id','Id');
  end;
end;

procedure TFRE_COMMON_WF_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  app       : TFRE_DB_APPLICATION;
  conn      : IFRE_DB_CONNECTION;
  transform : IFRE_DB_SIMPLE_TRANSFORM;
  dc        : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited MySessionInitializeModule(session);
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,transform);
    with transform do begin
      AddMultiToOnescheme(TFRE_DB_NameTypeArray.create('caption','step_caption'),'caption',FetchModuleTextShort(session,'gc_wf_caption'));
      AddOneToOnescheme('step_id','step_id',FetchModuleTextShort(session,'gc_wf_id'));
      AddOneToOnescheme('state','state','STATE');
    end;
    dc := session.NewDerivedCollection('WFMOD_WF_GRID');
    with dc do begin
      SetDeriveParent(session.GetDBConnection.AdmGetWorkFlowCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[cdgf_Children],'');//,nil,'',CWSF(@WEB_DGMenu),nil,CWSF(@WEB_DGNotification));
      SetParentToChildLinkField('<STEP_PARENT');
      SetDefaultOrderField('step_id',true);
    end;
  end;
end;

function TFRE_COMMON_WF_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  CheckClassVisibility4AnyDomain(ses);

  Result:=ses.FetchDerivedCollection('WFMOD_WF_GRID').GetDisplayDescription;
end;

{ TFRE_COMMON_DOMAIN_MOD }

class procedure TFRE_COMMON_DOMAIN_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_DOMAIN_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('domain_description')
end;

class procedure TFRE_COMMON_DOMAIN_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    CreateModuleText(conn,'groups_tab','Groups');
    CreateModuleText(conn,'users_tab','Users');

    CreateModuleText(conn,'gc_domain','Domain');
    CreateModuleText(conn,'gc_user','User');
    CreateModuleText(conn,'gc_group','Group');
    CreateModuleText(conn,'gc_domain_desc','Description');
    CreateModuleText(conn,'gcap_UinD','User belongs to Domain');
    CreateModuleText(conn,'gcap_UnotinD','User does not belong to Domain');
    CreateModuleText(conn,'gcap_GinD','Group belongs to Domain');
    CreateModuleText(conn,'gcap_GnotinD','Group does not belong to Domain');

    CreateModuleText(conn,'tb_modify_domain','Modify','','Modify Domain');
    CreateModuleText(conn,'tb_add_domain','Add','','Add Domain');
    CreateModuleText(conn,'cm_modify_domain','Modify');
    CreateModuleText(conn,'add_domain_diag_cap','Add new domain');
    CreateModuleText(conn,'modify_domain_diag_cap','Modify domain');
    CreateModuleText(conn,'modify_domain_diag_no_system_domain_msg','Editing of the System Domain is not possible.');
    CreateModuleText(conn,'domain_group_in_diag_cap','Adding Group to a domain');
    CreateModuleText(conn,'domain_user_in_diag_cap','Adding User to a domain');
    CreateModuleText(conn,'tb_deactivate_domain','Suspend','','Suspend Domain');
    CreateModuleText(conn,'tb_activate_domain','Activate','','Activate Domain');
    CreateModuleText(conn,'cm_deactivate_domain','Suspend');
    CreateModuleText(conn,'cm_activate_domain','Activate');

    CreateModuleText(conn,'domain_modify_error_cap','Error');
    CreateModuleText(conn,'domain_modify_error_msg','Modify failed %error_msg%');
  end;
end;

procedure TFRE_COMMON_DOMAIN_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var domain_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_domain     : IFRE_DB_SIMPLE_TRANSFORM;

    userin_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_UserIn     : IFRE_DB_SIMPLE_TRANSFORM;

    groupin_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_GroupIn    : IFRE_DB_SIMPLE_TRANSFORM;

    app           : TFRE_DB_APPLICATION;
    conn          : IFRE_DB_CONNECTION;

begin
  inherited MySessionInitializeModule(session);
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_domain);
    with tr_domain do begin
      AddOneToOnescheme('displayname','displayname',FetchModuleTextShort(session,'gc_domain'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('suspended','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateDomainIcon);
    end;
    domain_Grid := session.NewDerivedCollection('DOMAINMOD_DOMAIN_GRID');
    with domain_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetDomainCollection);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_domain);
      SetDisplayType(cdt_Listview,[],'',nil,'',CWSF(@WEB_DGMenu),nil,CWSF(@WEB_DGNotification));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_user'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateUserIcon);
    end;
    userin_Grid := session.NewDerivedCollection('DOMAINMOD_USERIN_GRID');
    with userin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_USER<DOMAINIDLINK'],false,'uids');
      domain_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_UinD'));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupIn);
    with tr_groupIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_group'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateGroupIcon);
    end;
    groupin_Grid := session.NewDerivedCollection('DOMAINMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<DOMAINIDLINK'],false);
      domain_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_groupIn);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_GinD'));
      SetDefaultOrderField('displayname',true);
    end;
  end;
end;

procedure TFRE_COMMON_DOMAIN_MOD.CalculateDomainIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if transformed_object.DomainID=ut.GetSysDomainID then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico_lck.svg');
  end else begin
    if ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_DOMAIN,transformed_object.DomainID) then begin
      if transformed_object.Field('suspended').AsBoolean then begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico_sus.svg');
      end else begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico.svg');
      end;
    end else begin
      if transformed_object.Field('suspended').AsBoolean then begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_lck_sus.svg');
      end else begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_lck.svg');
      end;
    end;
  end;
end;

procedure TFRE_COMMON_DOMAIN_MOD.CalculateGroupIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if transformed_object.Field('protected').AsBoolean or not ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_lck.svg');
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/group_ico.svg');
  end;
end;

procedure TFRE_COMMON_DOMAIN_MOD.CalculateUserIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/user_ico.svg');
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/user_ico_lck.svg');
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sec           : TFRE_DB_SUBSECTIONS_DESC;
  domaingrid    : TFRE_DB_VIEW_LIST_DESC;
  dc_domain     : IFRE_DB_DERIVED_COLLECTION;
  dc_userin     : IFRE_DB_DERIVED_COLLECTION;
  dc_groupin    : IFRE_DB_DERIVED_COLLECTION;
  txt           : IFRE_DB_TEXT;

begin
  CheckClassVisibility4AnyDomain(ses);

  ses.GetSessionModuleData(ClassName).DeleteField('selectedDomain');
  dc_domain     := ses.FetchDerivedCollection('DOMAINMOD_DOMAIN_GRID');
  domaingrid    := dc_domain.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_DOMAIN) and
     conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) and
     conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) and
     conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_ROLE) then begin  //class right without domain
    txt:=FetchModuleTextFull(ses,'tb_add_domain');
    domaingrid.AddButton.Describe(CWSF(@WEB_AddDomain),'',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
    txt:=FetchModuleTextFull(ses,'tb_modify_domain');
    domaingrid.AddButton.DescribeManualType('tb_modify_domain',CWSF(@WEB_ModifyDomain),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) and conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN) then begin
    txt:=FetchModuleTextFull(ses,'tb_deactivate_domain');
    domaingrid.AddButton.DescribeManualType('tb_deactivate_domain',CWSF(@WEB_DeActivateDomain),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;

  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) or
     conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then begin

    sec     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;

    if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then begin
      sec.AddSection.Describe(CWSF(@WEB_ContentUsers),FetchModuleTextShort(ses,'users_tab'),2);
    end;

    if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then begin
      sec.AddSection.Describe(CWSF(@WEB_ContentGroups),FetchModuleTextShort(ses,'groups_tab'),1);
    end;

    Result:=TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(domaingrid,sec,nil,nil,nil,true);
  end else begin
    Result:=domaingrid;
  end;

end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ContentUsers(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userin     : IFRE_DB_DERIVED_COLLECTION;
  useringrid    : TFRE_DB_VIEW_LIST_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_userin   := ses.FetchDerivedCollection('DOMAINMOD_USERIN_GRID');
  useringrid  := dc_userin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  Result  := useringrid;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ContentGroups(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_groupin    : IFRE_DB_DERIVED_COLLECTION;
  groupingrid   : TFRE_DB_VIEW_LIST_DESC;

begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_groupin  := ses.FetchDerivedCollection('DOMAINMOD_GROUPIN_GRID');
  groupingrid := dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  Result  := groupingrid;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_AddDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_FORM_DIALOG_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_DOMAIN) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_ROLE)) then  //class right without domain
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_DOMAIN',scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(FetchModuleTextShort(ses,'add_domain_diag_cap'),600);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CWSF(@WEB_CreateDomain),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_CreateDomain(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_DOMAIN) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) and
          conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_ROLE)) then  //class right without domain
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  //FIXXME - please handle input and error
  CheckDbResult(conn.AddDomain(input.Field('data').AsObject.Field('objname').AsString,input.Field('data').AsObject.Field('desc').AsObject.Field('txt').AsString,input.Field('data').AsObject.Field('desc').AsObject.Field('txt_s').AsString));
  Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe();
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ModifyDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_FORM_DIALOG_DESC;
  domain: IFRE_DB_DOMAIN;
  sf     : TFRE_DB_SERVER_FUNC_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_DOMAIN',scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(FetchModuleTextShort(ses,'modify_domain_diag_cap'),600);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);

  CheckDbResult(conn.sys.FetchDomainById(FREDB_String2Guid(input.Field('selected').AsString),domain),'ModifyDomain');
  if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(FetchModuleTextShort(ses,'modify_domain_diag_cap'),FetchModuleTextShort(ses,'modify_domain_diag_no_system_domain_msg'),fdbmt_warning,nil));
  end;

  sf:=CWSF(@WEB_SaveDomain);
  sf.AddParam.Describe('selected',input.Field('selected').AsString);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),sf,fdbbt_submit);
  res.FillWithObjectValues(domain.Implementor_HC as IFRE_DB_Object,ses);
  Result:=res;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DeActivateDomain(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  i        : Integer;
  domain   : IFRE_DB_DOMAIN;
  domain_id: TGuid;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) and conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  Result:=GFRE_DB_NIL_DESC;
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    domain_id:=FREDB_String2Guid(input.Field('selected').AsStringItem[i]);
    CheckDbResult(conn.SYS.FetchDomainById(domain_id,domain));
    CheckDbResult(conn.SYS.SuspendContinueDomainById(domain_id,not domain.Suspended));
    if ses.GetSessionModuleData(ClassName).FieldExists('selectedDomain') then begin
      input.Field('selected').AsString:=ses.GetSessionModuleData(ClassName).Field('selectedDomain').AsString;
      Result:=WEB_DGNotification(input,ses,app,conn);
    end;
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_SaveDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbo_uid          : TGUID;
    dn               : TFRE_DB_String;
    txt              : TFRE_DB_String;
    txt_s            : TFRE_DB_String;
begin
 if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN)) then
   raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

 data    := input.Field('DATA').asobject;

 FREDB_SetStringFromExistingFieldPathOrNoChange(data,'objname',dn);
 FREDB_SetStringFromExistingFieldPathOrNoChange(data,'desc.txt',txt);
 FREDB_SetStringFromExistingFieldPathOrNoChange(data,'desc.txt_s',txt_s);

 dbo_uid := FREDB_String2Guid(input.Field('selected').Asstring);

 res := conn.sys.ModifyDomainById(dbo_uid,dn,txt,txt_s);
 if res=edb_OK then
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe(FetchModuleTextShort(ses,'domain_modify_error_cap'),StringReplace(FetchModuleTextShort(ses,'domain_delete_error_msg'),'%error_msg%',CFRE_DB_Errortype[res],[rfReplaceAll]),fdbmt_error,nil));
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DGMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res      : TFRE_DB_MENU_DESC;
  func     : TFRE_DB_SERVER_FUNC_DESC;
  domain   : IFRE_DB_DOMAIN;
  txt      : TFRE_DB_String;
  domainUid: TGuid;
begin
  if input.Field('selected').ValueCount=1 then begin
    domainUid:=FREDB_String2Guid(input.Field('selected').AsString);
    CheckDbResult(conn.sys.FetchDomainById(domainUid,domain),'TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification');
    if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
      Result:=GFRE_DB_NIL_DESC;
    end else begin
      res:=TFRE_DB_MENU_DESC.create.Describe;
      if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
        func:=CWSF(@WEB_ModifyDomain);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(FetchModuleTextShort(ses,'cm_modify_domain'),'',func);
      end;

      if conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_DOMAIN,domainUid) and conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_DOMAIN,domainUid) then begin
        func:=CWSF(@WEB_DeActivateDomain);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        if domain.Suspended then begin
          txt:=FetchModuleTextShort(ses,'cm_activate_domain');
        end else begin
          txt:=FetchModuleTextShort(ses,'cm_deactivate_domain');
        end;
        res.AddEntry.Describe(txt,'',func);
      end;

      Result:=res;
    end;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  domain   : IFRE_DB_DOMAIN;
  txt      : IFRE_DB_TEXT;
  domainUid: TGuid;
begin
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
    if input.Field('selected').ValueCount=1 then begin
      ses.GetSessionModuleData(ClassName).Field('selectedDomain').AsString:=input.Field('selected').AsString;
      domainUid:=FREDB_String2Guid(input.Field('selected').AsString);
      CheckDbResult(conn.sys.FetchDomainById(domainUid,domain),'TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification');
      if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_deactivate_domain',true,FetchModuleTextShort(ses,'tb_deactivate_domain'),FetchModuleTextHint(ses,'tb_deactivate_domain')));
      end else begin
        if conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_DOMAIN,domainUid) then begin
          ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',false));
        end else begin
          ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
        end;
        if domain.Suspended then begin
          txt:=FetchModuleTextFull(ses,'tb_activate_domain');
        end else begin
          txt:=FetchModuleTextFull(ses,'tb_deactivate_domain');
        end;
        if conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_DOMAIN,domainUid) and conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_DOMAIN,domainUid) then begin
          ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_deactivate_domain',false,txt.Getshort,txt.GetHint));
        end else begin
          ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_deactivate_domain',true,txt.Getshort,txt.GetHint));
        end;
        txt.Finalize;
      end;
    end else begin
      ses.GetSessionModuleData(ClassName).DeleteField('selectedDomain');
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_deactivate_domain',true,FetchModuleTextShort(ses,'tb_deactivate_domain'),FetchModuleTextHint(ses,'tb_deactivate_domain')));
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;


{ TFRE_COMMON_ROLE_MOD }

function TFRE_COMMON_ROLE_MOD._addremoverole(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION; const addrole: boolean): IFRE_DB_Object;
var
  role     : IFRE_DB_ROLE;
  group    : IFRE_DB_GROUP;
  i        : Integer;
  res      : TFRE_DB_Errortype;
  roleUid  : TFRE_DB_String;
begin
  if input.FieldExists('uids_ref') then begin
    roleUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      roleUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to AddRemoveRole!');
    end;
  end;

  CheckDbResult(conn.sys.FetchRoleById(FREDB_String2Guid(roleUid),role),'_addremoverole');
  if (role.GetDomain(conn)=CFRE_DB_SYS_DOMAIN_NAME) and not conn.sys.CheckClassRight4Domain('assignRole',TFRE_DB_GROUP,CFRE_DB_SYS_DOMAIN_NAME) then raise EFRE_DB_Exception.Create('You are not allowed to assigne System Roles');

  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchGroupById(FREDB_String2Guid(input.Field('selected').AsStringArr[i]),group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if group.isProtected then raise EFRE_DB_Exception.Create('You cannot modify a protected group.');

    if addrole then begin
      if conn.sys.AddRolesToGroupById(group.ObjectName,group.DomainID,TFRE_DB_GUIDArray.create(role.UID))<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_add_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end else begin
      if conn.sys.RemoveRolesFromGroupById(group.ObjectName,group.DomainID,TFRE_DB_GUIDArray.create(role.UID),false)<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_remove_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_ROLE_MOD._getDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  sec: TFRE_DB_SUBSECTIONS_DESC;
begin
  sec:=TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentUsers),FetchModuleTextShort(ses,'users_tab'),2);
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentGroups),FetchModuleTextShort(ses,'groups_tab'),1);
  end;
  sec.contentId:='ROLE_DETAILS';
  Result:=sec;
end;

function TFRE_COMMON_ROLE_MOD._getNoRoleDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe;
  res.AddSection.Describe(CWSF(@WEB_ContentNoRoleSel),FetchModuleTextShort(ses,'norole_tab'),1);
  res.contentId:='ROLE_DETAILS';
  Result:=res;
end;

function TFRE_COMMON_ROLE_MOD._checkGGConditions(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): Boolean;
var
  role : IFRE_DB_Object;
  group: IFRE_DB_GROUP;
  i    : Integer;
begin
  if input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin

    CheckDbResult(conn.Fetch(FREDB_String2Guid(input.FieldPath('dependency.uids_ref.filtervalues').AsString),role));
    if not conn.sys.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then begin
      Result:=false;
      exit;
    end;

    for i := 0 to input.Field('SELECTED').ValueCount - 1 do begin
      CheckDbResult(conn.sys.FetchGroupById(FREDB_String2Guid(input.Field('SELECTED').AsStringItem[i]),group));
      if group.isProtected or not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,group.DomainID) then begin
        Result:=false;
        exit;
      end;
    end;
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

class procedure TFRE_COMMON_ROLE_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_ROLE_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('role_description');
end;

class procedure TFRE_COMMON_ROLE_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    CreateModuleText(conn,'groups_tab','Groups');
    CreateModuleText(conn,'users_tab','Users');
    CreateModuleText(conn,'gc_domain_role','Domain / Role');
    CreateModuleText(conn,'gc_role','Role');
    CreateModuleText(conn,'gc_group','Group');
    CreateModuleText(conn,'gc_user','User');

    CreateModuleText(conn,'gcap_GhasR','Group has Role');
    CreateModuleText(conn,'gcap_GnotR','Group has not Role');
    CreateModuleText(conn,'gcap_UhasR','User has Role');
    CreateModuleText(conn,'gcap_UnotR','User has not Role');

    CreateModuleText(conn,'tb_remove_group_from_role','Remove');
    CreateModuleText(conn,'tb_add_group_to_role','Add');

    CreateModuleText(conn,'cm_remove_groups_from_role','Remove role from groups');
    CreateModuleText(conn,'cm_add_groups_to_role','Add role to groups');
    CreateModuleText(conn,'cm_remove_group_from_role','Remove role from group');
    CreateModuleText(conn,'cm_add_group_to_role','Add role to group');

    CreateModuleText(conn,'role_details_select_one','Please select a role to get detailed information');
    CreateModuleText(conn,'norole_tab','General');

    CreateModuleText(conn,'error_fetch_group_msg','Could not fetch group with id %group%');
    CreateModuleText(conn,'error_add_role_msg','Could not add role %role% to group %group%');
    CreateModuleText(conn,'error_remove_role_msg','Could not remove role %role% from group %group%');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    CreateModuleText(conn,'gc_domain','Domain');
  end;
end;

procedure TFRE_COMMON_ROLE_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var role_Grid     : IFRE_DB_DERIVED_COLLECTION;
    tr_role       : IFRE_DB_SIMPLE_TRANSFORM;

    userin_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_UserIn     : IFRE_DB_SIMPLE_TRANSFORM;

    userout_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_userOut    : IFRE_DB_SIMPLE_TRANSFORM;

    groupin_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_groupIn    : IFRE_DB_SIMPLE_TRANSFORM;

    groupout_Grid : IFRE_DB_DERIVED_COLLECTION;
    tr_groupOut   : IFRE_DB_SIMPLE_TRANSFORM;

    app           : TFRE_DB_APPLICATION;
    conn          : IFRE_DB_CONNECTION;
    grid_column_cap: TFRE_DB_String;
    show_domains   : Boolean;

begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    app  := GetEmbeddingApp;
    conn := session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Role);

    show_domains := session.HasFeature('DOMAIN') and conn.SYS.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_DOMAIN_MOD);

    with tr_Role do begin
      if show_domains then begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_domain_role');
      end else begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_role');
      end;
      AddOneToOnescheme('displayname','displayname',grid_column_cap,dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateRoleIcon);
      AddFulltextFilterOnTransformed(['displayname']);
   end;
    role_Grid := session.NewDerivedCollection('ROLEMOD_ROLE_GRID');
    with role_Grid do begin
      if show_domains then begin
        SetDeriveParent           (session.GetDBConnection.AdmGetDomainCollection);
        SetDisplayType            (cdt_Listview,[cdgf_Children,cdgf_ShowSearchbox],'',nil,'',nil,nil,CWSF(@WEB_RoleNotification));
        SetParentToChildLinkField ('TFRE_DB_ROLE<DOMAINIDLINK');
      end else begin
        SetDeriveParent           (session.GetDBConnection.AdmGetRoleCollection);
        SetDisplayType            (cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',nil,nil,CWSF(@WEB_RoleNotification));
      end;
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDefaultOrderField('displayname',true);
      SetDeriveTransformation(tr_role);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_user'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateUserIcon);
    end;
    userin_Grid := session.NewDerivedCollection('ROLEMOD_USERIN_GRID');
    with userin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<ROLEIDS','TFRE_DB_USER<USERGROUPIDS'],false);
      role_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_UhasR'));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserOut);
    with tr_UserOut do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_user'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateUserIcon);
    end;
    userout_Grid := session.NewDerivedCollection('ROLEMOD_USEROUT_GRID');
    with userout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<ROLEIDS','TFRE_DB_USER<USERGROUPIDS'],true);
      role_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_UserOut);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_UnotR'));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupIn);
    with tr_groupIn do begin
      AddOneToOnescheme('displayname','displayname',FetchModuleTextShort(session,'gc_group'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('protected','_disabledrag_','',dt_boolean,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateGroupIcon);
    end;
    groupin_Grid := session.NewDerivedCollection('ROLEMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<ROLEIDS'],false);
      role_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_groupIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_GhasR'),nil,'',CWSF(@WEB_GIRMenu),nil,CWSF(@WEB_GIRNotification),nil,CWSF(@WEB_AddToRole));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupOut);
    with tr_GroupOut do begin
      AddOneToOnescheme('displayname','displayname',FetchModuleTextShort(session,'gc_group'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('protected','_disabledrag_','',dt_boolean,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateGroupIcon);
    end;
    groupout_Grid := session.NewDerivedCollection('ROLEMOD_GROUPOUT_GRID');
    with groupout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<ROLEIDS'],true);
      role_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_groupOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_GnotR'),nil,'',CWSF(@WEB_GORMenu),nil,CWSF(@WEB_GORNotification),nil,CWSF(@WEB_RemoveFromRole));
      SetDefaultOrderField('displayname',true);
    end;
  end;
end;

procedure TFRE_COMMON_ROLE_MOD.CalculateRoleIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object ; const session_data : IFRE_DB_Object);
begin
  if transformed_object.PreTransformedWasA('TFRE_DB_DOMAIN') then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico.svg');
  end else begin
    if ut.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,transformed_object.DomainID) then begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico.svg');
    end else begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico_lck.svg');
    end;
  end;
end;

procedure TFRE_COMMON_ROLE_MOD.CalculateGroupIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object ; const session_data : IFRE_DB_Object);
begin
  if not ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_lck.svg');
  end else begin
    if transformed_object.Field('protected').AsBoolean then begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_prt.svg');
    end else begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/group_ico.svg');
    end;
  end;
end;

procedure TFRE_COMMON_ROLE_MOD.CalculateUserIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object ; const session_data : IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/user_ico.svg');
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/user_ico_lck.svg');
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  rolegrid    : TFRE_DB_VIEW_LIST_DESC;
begin
  CheckClassVisibility4AnyDomain(ses);
  ses.GetSessionModuleData(ClassName).DeleteField('selectedRoles');

  rolegrid := ses.FetchDerivedCollection('ROLEMOD_ROLE_GRID').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) or
     conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then begin
    Result:=TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(rolegrid,_getNoRoleDetails(input,ses,app,conn),nil,nil,nil,true);
  end else begin
    Result:=rolegrid;
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_ContentNoRoleSel(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe(FetchModuleTextShort(ses,'role_details_select_one'));
end;

function TFRE_COMMON_ROLE_MOD.WEB_ContentUsers(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userin   : IFRE_DB_DERIVED_COLLECTION;
  useringrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  useroutgrid : TFRE_DB_VIEW_LIST_DESC;
  user        : TFRE_DB_LAYOUT_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_userin   := ses.FetchDerivedCollection('ROLEMOD_USERIN_GRID');
  useringrid  := dc_userin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_userout  := ses.FetchDerivedCollection('ROLEMOD_USEROUT_GRID');
  useroutgrid := dc_userout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  user    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,useroutgrid,nil,useringrid,nil,true,-1,1,-1,1);
  Result  := user;
end;

function TFRE_COMMON_ROLE_MOD.WEB_ContentGroups(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_groupin   : IFRE_DB_DERIVED_COLLECTION;
  groupingrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_groupout  : IFRE_DB_DERIVED_COLLECTION;
  groupoutgrid : TFRE_DB_VIEW_LIST_DESC;
  group        : TFRE_DB_LAYOUT_DESC;
  role         : IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_groupin  := ses.FetchDerivedCollection('ROLEMOD_GROUPIN_GRID');
  groupingrid := dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  groupingrid.contentId:='ROLEMOD_GROUPIN_GRID';
  dc_groupout := ses.FetchDerivedCollection('ROLEMOD_GROUPOUT_GRID');
  groupoutgrid:= dc_groupout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  groupoutgrid.contentId:='ROLEMOD_GROUPOUT_GRID';

  if (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then begin
    groupoutgrid.setDropGrid(groupingrid);
    groupingrid.setDropGrid(groupoutgrid);
    groupoutgrid.AddButton.DescribeManualType('tb_add_group_to_role',CWSF(@WEB_AddToRole),'',FetchModuleTextShort(ses,'tb_add_group_to_role'),'',true);
    groupingrid.AddButton.DescribeManualType('tb_remove_group_from_role',CWSF(@WEB_RemoveFromRole),'',FetchModuleTextShort(ses,'tb_remove_group_from_role'),'',true);
  end;
  if ses.GetSessionModuleData(ClassName).FieldExists('selectedRoles') then begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(ses.GetSessionModuleData(ClassName).Field('selectedRoles').AsString),role));
    if not conn.sys.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then begin
      groupoutgrid.disableDrag;
      groupingrid.disableDrag;
    end;
  end;
  group   := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,groupoutgrid,nil,groupingrid,nil,true,-1,1,-1,1);
  Result  := group;
end;

function TFRE_COMMON_ROLE_MOD.WEB_RoleNotification(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sel_guid    : TGUID;
  selObj      : IFRE_DB_Object;
  oldSelIsRole: Boolean;
  newSelIsRole: Boolean;
  notEditable : Boolean;

begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE)) then raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  oldSelIsRole:=ses.GetSessionModuleData(ClassName).FieldExists('selectedRoles');

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    CheckDbResult(conn.Fetch(sel_guid,selObj),'role fetch failed)');
    if selObj.IsA('TFRE_DB_ROLE') then begin
      ses.GetSessionModuleData(ClassName).Field('selectedRoles').AsString:=input.Field('SELECTED').AsString;
      notEditable:=not conn.sys.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,selObj.DomainID);
      selObj.Finalize;
    end else begin
      ses.GetSessionModuleData(ClassName).DeleteField('selectedRoles');
    end;
  end else begin
    ses.GetSessionModuleData(ClassName).DeleteField('selectedRoles');
  end;

  newSelIsRole:=ses.GetSessionModuleData(ClassName).FieldExists('selectedRoles');

  if newSelIsRole<>oldSelIsRole then begin
    if newSelIsRole then begin
      Result:=_getDetails(input,ses,app,conn);
    end else begin
      Result:=_getNoRoleDetails(input,ses,app,conn);
    end;
  end else begin
    if IsContentUpdateVisible(ses,'ROLEMOD_GROUPOUT_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('ROLEMOD_GROUPOUT_GRID',notEditable));
    end;
    if IsContentUpdateVisible(ses,'ROLEMOD_GROUPIN_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('ROLEMOD_GROUPIN_GRID',notEditable));
    end;
    Result:=GFRE_DB_NIL_DESC;
  end;
end;


function TFRE_COMMON_ROLE_MOD.WEB_AddToRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  Result:=_addremoverole(input,ses,app,conn,true);
end;

function TFRE_COMMON_ROLE_MOD.WEB_GIRMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
  i         : Integer;
  group     : IFRE_DB_GROUP;
  role      : IFRE_DB_Object;
begin
  if _checkGGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_remove_group_from_role');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_remove_groups_from_role');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_GORMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
  i         : Integer;
  group     : IFRE_DB_GROUP;
  role      : IFRE_DB_Object;
begin
  if _checkGGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_add_group_to_role');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_add_groups_to_role');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_GIRNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  buttonDisabled: Boolean;
  group         : IFRE_DB_GROUP;
  i             : Integer;
  role          : IFRE_DB_Object;
begin
  if conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
      buttonDisabled:=not _checkGGConditions(input,ses,app,conn);
    end else begin
      buttonDisabled:=true;
    end;
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_remove_group_from_role',buttonDisabled));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_ROLE_MOD.WEB_GORNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  buttonDisabled: Boolean;
  group         : IFRE_DB_GROUP;
  i             : Integer;
  role          : IFRE_DB_Object;
begin
  if conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
      buttonDisabled:=not _checkGGConditions(input,ses,app,conn);
    end else begin
      buttonDisabled:=true;
    end;
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_add_group_to_role',buttonDisabled));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_ROLE_MOD.WEB_RemoveFromRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  Result:=_addremoverole(input,ses,app,conn,false);
end;

{ TFRE_COMMON_GROUP_MOD }

function TFRE_COMMON_GROUP_MOD._getGroupsString(const groups: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION; const ses: IFRE_DB_UserSession): String;
var
  i    : Integer;
  res  : String;
  group: IFRE_DB_GROUP;
begin
  for i := 0 to Length(groups) - 1 do begin
    CheckDbResult(conn.sys.FetchGroupById(FREDB_String2Guid(groups[i]),group),'_getGroupsString');
    if i>0 then begin
      if i=(Length(groups) - 1) then begin
        res:=res + ' ' +FetchModuleTextShort(ses,'and')+' ';
      end else begin
        res:=res+', ';
      end;
    end;
   res:=res+group.ObjectName + ' ';
    if group.Description.GetLong<>'' then begin
      res:=res+'('+group.Description.GetLong+')';
    end;
  end;
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD._addremoverole(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION; const addrole: boolean): IFRE_DB_Object;
var
  role     : IFRE_DB_ROLE;
  group    : IFRE_DB_GROUP;
  i        : Integer;
  res      : TFRE_DB_Errortype;
  groupUid : TFRE_DB_String;
begin

  if input.FieldExists('uids_ref') then begin
    groupUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      groupUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to AddRemoveRole!');
    end;
  end;

  conn.sys.FetchGroupById(FREDB_String2Guid(groupUid),group);
  if group.isProtected then raise EFRE_DB_Exception.Create('You cannot modify a protected group.');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchRoleById(FREDB_String2Guid(input.Field('selected').AsStringArr[i]),role)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_role_msg'),'%role%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));

    if (role.GetDomain(conn)=CFRE_DB_SYS_DOMAIN_NAME) and not conn.sys.CheckClassRight4Domain('assignRole',TFRE_DB_GROUP,CFRE_DB_SYS_DOMAIN_NAME) then raise EFRE_DB_Exception.Create('You are not allowed to assigne System Roles');
    if addrole then begin
      if conn.sys.AddRolesToGroupById(group.ObjectName,group.DomainID,TFRE_DB_GUIDArray.Create(role.UID))<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_add_role_msg'),'%group%',group.ObjectName+'@'+group.getDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end else begin
      if conn.sys.RemoveRolesFromGroupById(group.ObjectName,group.DomainID,TFRE_DB_GUIDArray.Create(role.UID),false)<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_remove_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_GROUP_MOD._getDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  sec: TFRE_DB_SUBSECTIONS_DESC;
begin
  sec:=TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentUsers),FetchModuleTextShort(ses,'users_tab'),1);
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentRoles),FetchModuleTextShort(ses,'roles_tab'),2);
  end;
  sec.contentId:='GROUP_DETAILS';
  Result:=sec;
end;

function TFRE_COMMON_GROUP_MOD._getNoGroupDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe;
  res.AddSection.Describe(CWSF(@WEB_ContentNoGroupSel),FetchModuleTextShort(ses,'nogroup_tab'),1);
  res.contentId:='GROUP_DETAILS';
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD._checkUGConditions(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): Boolean;
var
  dbo: IFRE_DB_Object;
  i  : Integer;
begin
  Result:=false;
  if input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin

    CheckDbResult(conn.Fetch(FREDB_String2Guid(input.FieldPath('dependency.uids_ref.filtervalues').AsString),dbo));
    if conn.sys.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,dbo.DomainID) then begin
      for i := 0 to input.Field('selected').ValueCount - 1 do begin
        CheckDbResult(conn.Fetch(FREDB_String2Guid(input.Field('selected').AsStringItem[i]),dbo));
        if not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,dbo.DomainID) then begin
          Result:=false;
          exit;
        end;
      end;
      Result:=true;
    end;
  end;
end;

function TFRE_COMMON_GROUP_MOD._checkRGConditions(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): Boolean;
var
  group: IFRE_DB_GROUP;
  role : IFRE_DB_Object;
  i    : Integer;
begin
  if input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin

    CheckDbResult(conn.SYS.FetchGroupById(FREDB_String2Guid(input.FieldPath('dependency.uids_ref.filtervalues').AsString),group));
    if group.isProtected or not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,group.DomainID) then begin
      Result:=false;
      exit;
    end;

    for i := 0 to input.Field('SELECTED').ValueCount - 1 do begin
      CheckDbResult(conn.Fetch(FREDB_String2Guid(input.Field('SELECTED').AsStringItem[i]),role));
      if not conn.sys.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,role.DomainID) then begin
        Result:=false;
        exit;
      end;
    end;

    Result:=true;
  end else begin
    Result:=false;
  end;
end;

class procedure TFRE_COMMON_GROUP_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_GROUP_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('group_description')
end;

class procedure TFRE_COMMON_GROUP_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    CreateModuleText(conn,'roles_tab','Roles');
    CreateModuleText(conn,'users_tab','Users');

    CreateModuleText(conn,'gc_domain_group','Domain / Group');
    CreateModuleText(conn,'gc_group','Group');
    CreateModuleText(conn,'gc_user','User');
    CreateModuleText(conn,'gc_role','Role');
    CreateModuleText(conn,'gcap_UinG','User is in Group');
    CreateModuleText(conn,'gcap_UnotG','User is not in Group');
    CreateModuleText(conn,'gcap_GhasR','Group has Role');
    CreateModuleText(conn,'gcap_GnotR','Group has not Role');
    CreateModuleText(conn,'group_details_select_one','Please select a group to get detailed information');
    CreateModuleText(conn,'nogroup_tab','General');

    CreateModuleText(conn,'delete_group_diag_cap','Confirm: Delete group');
    CreateModuleText(conn,'delete_groups_diag_cap','Confirm: Delete multiple groups');
    CreateModuleText(conn,'delete_group_diag_msg','Group %group_str% will be deleted permanently! Please confirm to continue.');
    CreateModuleText(conn,'delete_groups_diag_msg','Groupss %group_str% will be deleted permanently! Please confirm to continue.');
    CreateModuleText(conn,'group_deleted_diag_cap','Group deleted');
    CreateModuleText(conn,'groups_deleted_diag_cap','Groups deleted');
    CreateModuleText(conn,'group_deleted_diag_msg','Group %group_str% successfully deleted.');
    CreateModuleText(conn,'groups_deleted_diag_msg','Groups %group_str% successfully deleted.');

    CreateModuleText(conn,'add_group_diag_cap','Add new group');
    CreateModuleText(conn,'modify_group_diag_cap','Modify group');
    CreateModuleText(conn,'modify_group_diag_no_system_group_msg','You can not modify a system group.');
    CreateModuleText(conn,'delete_not_empty_group_error_diag_cap','Delete group');
    CreateModuleText(conn,'delete_not_empty_group_error_diag_msg','You are not allowed to modify Users. Therefore you can only delete empty groups.');

    CreateModuleText(conn,'cm_modify_group','Modify','','Modify Group');
    CreateModuleText(conn,'cm_delete_group','Delete','','Delete Group');
    CreateModuleText(conn,'cm_delete_groups','Delete','','Delete Groups');
    CreateModuleText(conn,'cm_remove_group_from_users','Remove users from group');
    CreateModuleText(conn,'cm_add_group_to_users','Add users to group');
    CreateModuleText(conn,'cm_remove_group_from_user','Remove user from group');
    CreateModuleText(conn,'cm_add_group_to_user','Add user to group');
    CreateModuleText(conn,'cm_remove_group_from_roles','Remove roles from group');
    CreateModuleText(conn,'cm_add_group_to_roles','Add roles to group');
    CreateModuleText(conn,'cm_remove_group_from_role','Remove role from group');
    CreateModuleText(conn,'cm_add_group_to_role','Add role to group');

    CreateModuleText(conn,'tb_add_group','Add','','Add Group');
    CreateModuleText(conn,'tb_modify_group','Modify','','Modify Group');
    CreateModuleText(conn,'tb_delete_group','Delete','','Delete Group');
    CreateModuleText(conn,'tb_remove_group_from_user','Remove');
    CreateModuleText(conn,'tb_add_group_to_user','Add');
    CreateModuleText(conn,'tb_remove_group_from_role','Remove');
    CreateModuleText(conn,'tb_add_group_to_role','Add');

    CreateModuleText(conn,'group_modify_error_cap','Error');
    CreateModuleText(conn,'group_modify_error_msg','Modify failed %error_msg%');

    CreateModuleText(conn,'and','and');

    CreateModuleText(conn,'error_fetch_role_msg','Could not fetch role with id %role%');
    CreateModuleText(conn,'error_fetch_user_msg','Could not fetch user with id %user%');
    CreateModuleText(conn,'error_add_role_msg','Could not add role %role% to group %group%');
    CreateModuleText(conn,'error_remove_role_msg','Could not remove role %role% from group %group%');
    CreateModuleText(conn,'error_add_group_msg','Could not add user %user% to group %group%');
    CreateModuleText(conn,'error_remove_group_msg','Could not remove user %user% from group %group%');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    CreateModuleText(conn,'gc_domain','Domain');
  end;
end;

procedure TFRE_COMMON_GROUP_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var group_Grid    : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid       : IFRE_DB_SIMPLE_TRANSFORM;

    userin_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_UserIn     : IFRE_DB_SIMPLE_TRANSFORM;

    userout_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_userOut    : IFRE_DB_SIMPLE_TRANSFORM;

    rolein_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_RoleIn     : IFRE_DB_SIMPLE_TRANSFORM;

    roleout_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_RoleOut    : IFRE_DB_SIMPLE_TRANSFORM;

    app           : TFRE_DB_APPLICATION;
    conn          : IFRE_DB_CONNECTION;
    grid_column_cap: TFRE_DB_String;
    show_domains   : Boolean;

begin
  inherited MySessionInitializeModule(session);
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    show_domains := session.HasFeature('DOMAIN') and conn.SYS.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_DOMAIN_MOD);

    with tr_Grid do begin
      if show_domains then begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_domain_group');
      end else begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_group');
      end;
      AddOneToOnescheme('displayname','',grid_column_cap,dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateGroupIcon);
      AddFulltextFilterOnTransformed(['displayname']);
    end;
    group_Grid := session.NewDerivedCollection('GROUPMOD_GROUP_GRID');
    with group_Grid do begin
      if show_domains then begin
        SetDeriveParent           (session.GetDBConnection.AdmGetDomainCollection);
        SetDisplayType            (cdt_Listview,[cdgf_Children,cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_GGMenu),nil,CWSF(@WEB_GGNotification));
        SetParentToChildLinkField ('TFRE_DB_GROUP<DOMAINIDLINK');
      end else begin
        SetDeriveParent           (session.GetDBConnection.AdmGetGroupCollection);
        SetDisplayType            (cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_GGMenu),nil,CWSF(@WEB_GGNotification));
      end;
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_Grid);
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_user'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateUserIcon);
    end;
    userin_Grid := session.NewDerivedCollection('GROUPMOD_USERIN_GRID');
    with userin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_USER<USERGROUPIDS'],false); // UserGroupIDS
      group_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_UinG'),nil,'',CWSF(@WEB_UIGMenu),nil,CWSF(@WEB_UIGNotification),nil,CWSF(@WEB_AddToUser));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserOut);
    with tr_UserOut do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_user'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateUserIcon);
    end;
    userout_Grid := session.NewDerivedCollection('GROUPMOD_USEROUT_GRID');
    with userout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_USER<USERGROUPIDS'],true); // UserGroupIDS
      group_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_UserOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_UnotG'),nil,'',CWSF(@WEB_UOGMenu),nil,CWSF(@WEB_UOGNotification),nil,CWSF(@WEB_RemoveFromUser));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleIn);
    with tr_RoleIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_role'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('_disabledrag_','','',dt_boolean,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateRoleFields);
    end;
    rolein_Grid := session.NewDerivedCollection('GROUPMOD_ROLEIN_GRID');
    with rolein_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['ROLEIDS>TFRE_DB_ROLE'],false);
      group_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_RoleIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_GhasR'),nil,'',CWSF(@WEB_RIGMenu),nil,CWSF(@WEB_RIGNotification),nil,CWSF(@WEB_AddToRole));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleOut);
    with tr_RoleOut do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_role'),dt_string,true,false,false,6,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('_disabledrag_','','',dt_boolean,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateRoleFields);
    end;
    roleout_Grid := session.NewDerivedCollection('GROUPMOD_ROLEOUT_GRID');
    with roleout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['ROLEIDS>TFRE_DB_ROLE'],true);
      group_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_RoleOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_GnotR'),nil,'',CWSF(@WEB_ROGMenu),nil,CWSF(@WEB_ROGNotification),nil,CWSF(@WEB_RemoveFromRole));
      SetDefaultOrderField('displayname',true);
    end;
  end;
end;

procedure TFRE_COMMON_GROUP_MOD.CalculateRoleFields(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico.svg');
    transformed_object.Field('_disabledrag_').AsBoolean:=false;
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico_lck.svg');
    transformed_object.Field('_disabledrag_').AsBoolean:=true;
  end;
end;

procedure TFRE_COMMON_GROUP_MOD.CalculateGroupIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if transformed_object.PreTransformedWasA('TFRE_DB_DOMAIN') then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico.svg');
  end else begin
    if not ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,transformed_object.DomainID) then begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_lck.svg');
    end else begin
      if transformed_object.Field('protected').AsBoolean then begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_prt.svg');
      end else begin
        transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/group_ico.svg');
      end;
    end;
  end;
end;

procedure TFRE_COMMON_GROUP_MOD.CalculateUserIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/user_ico.svg');
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/user_ico_lck.svg');
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sec     : TFRE_DB_SUBSECTIONS_DESC;
  groupgrid   : TFRE_DB_VIEW_LIST_DESC;
  txt         : IFRE_DB_TEXT;
begin
  CheckClassVisibility4AnyDomain(ses);
  ses.GetSessionModuleData(ClassName).DeleteField('selectedGroups');

  groupgrid := ses.FetchDerivedCollection('GROUPMOD_GROUP_GRID').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) then begin
    txt:=FetchModuleTextFull(ses,'tb_add_group');
    groupgrid.AddButton.Describe(CWSF(@WEB_AddGroup),'',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    txt:=FetchModuleTextFull(ses,'tb_modify_group');
    groupgrid.AddButton.DescribeManualType('tb_modify_group',CWSF(@WEB_ModifyGroup),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then begin
    txt:=FetchModuleTextFull(ses,'tb_delete_group');
    groupgrid.AddButton.DescribeManualType('tb_delete_group',CWSF(@WEB_DeleteGroup),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;

  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) or conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE) then begin
    Result:=TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(groupgrid,_getNoGroupDetails(input,ses,app,conn),nil,nil,nil,true);
  end else begin
    Result:=groupgrid;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_ContentNoGroupSel(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe(FetchModuleTextShort(ses,'group_details_select_one'));
end;

function TFRE_COMMON_GROUP_MOD.WEB_ContentUsers(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userin   : IFRE_DB_DERIVED_COLLECTION;
  useringrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  useroutgrid : TFRE_DB_VIEW_LIST_DESC;
  user        : TFRE_DB_LAYOUT_DESC;
  group       : IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_userin   := ses.FetchDerivedCollection('GROUPMOD_USERIN_GRID');
  useringrid  := dc_userin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  useringrid.contentId:='GROUPMOD_USERIN_GRID';
  dc_userout  := ses.FetchDerivedCollection('GROUPMOD_USEROUT_GRID');
  useroutgrid := dc_userout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  useroutgrid.contentId:='GROUPMOD_USEROUT_GRID';

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then begin
    useroutgrid.setDropGrid(useringrid);
    useringrid.setDropGrid(useroutgrid);
    useroutgrid.AddButton.DescribeManualType('tb_add_group_to_user',CWSF(@WEB_AddToUser),'',FetchModuleTextShort(ses,'tb_add_group_to_user'),'',true);
    useringrid.AddButton.DescribeManualType('tb_remove_group_from_user',CWSF(@WEB_RemoveFromUser),'',FetchModuleTextShort(ses,'tb_remove_group_from_user'),'',true);
  end;

  if ses.GetSessionModuleData(ClassName).FieldExists('selectedGroups') then begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(ses.GetSessionModuleData(ClassName).Field('selectedGroups').AsString),group));
    if not conn.sys.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,group.DomainID) then begin
      useroutgrid.disableDrag;
      useringrid.disableDrag;
    end;
  end;

  user    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,useroutgrid,nil,useringrid,nil,true,-1,1,-1,1);
  Result  := user;
end;


function TFRE_COMMON_GROUP_MOD.WEB_ContentRoles(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_rolein   : IFRE_DB_DERIVED_COLLECTION;
  roleingrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  roleoutgrid : TFRE_DB_VIEW_LIST_DESC;
  role        : TFRE_DB_LAYOUT_DESC;
  group       : IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_rolein   := ses.FetchDerivedCollection('GROUPMOD_ROLEIN_GRID');
  roleingrid  := dc_rolein.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  roleingrid.contentId:='GROUPMOD_ROLEIN_GRID';
  dc_roleout  := ses.FetchDerivedCollection('GROUPMOD_ROLEOUT_GRID');
  roleoutgrid := dc_roleout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  roleoutgrid.contentId:='GROUPMOD_ROLEOUT_GRID';

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    roleoutgrid.setDropGrid(roleingrid);
    roleingrid.setDropGrid(roleoutgrid);
    roleoutgrid.AddButton.DescribeManualType('tb_add_group_to_role',CWSF(@WEB_AddToRole),'',FetchModuleTextShort(ses,'tb_add_group_to_role'),'',true);
    roleingrid.AddButton.DescribeManualType('tb_remove_group_from_role',CWSF(@WEB_RemoveFromRole),'',FetchModuleTextShort(ses,'tb_remove_group_from_role'),'',true);
  end;

  if ses.GetSessionModuleData(ClassName).FieldExists('selectedGroups') then begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(ses.GetSessionModuleData(ClassName).Field('selectedGroups').AsString),group));
    if (group.Implementor_HC as IFRE_DB_GROUP).isProtected or not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,group.DomainID) then begin
      roleoutgrid.disableDrag;
      roleingrid.disableDrag;
    end;
  end;

  role    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,roleoutgrid,nil,roleingrid,nil,true,-1,1,-1,1);
  Result  := role;
end;

function TFRE_COMMON_GROUP_MOD.WEB_AddGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_FORM_DIALOG_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_GROUP',scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(FetchModuleTextShort(ses,'add_group_diag_cap'),600);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CWSF(@WEB_CreateGroup),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD.WEB_CreateGroup(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  CheckDbResult(conn.sys.AddGroup(input.Field('data').AsObject.Field('objname').AsString,input.Field('data').AsObject.Field('desc').AsObject.Field('txt').AsString,input.Field('data').AsObject.Field('desc').AsObject.Field('txt_s').AsString,FREDB_String2Guid(input.Field('data').AsObject.Field('DOMAINIDLINK').AsString)));
  Result:=TFRE_DB_CLOSE_DIALOG_DESC.create.Describe();
end;

function TFRE_COMMON_GROUP_MOD.WEB_ModifyGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_FORM_DIALOG_DESC;
  group : IFRE_DB_GROUP;
  sf     : TFRE_DB_SERVER_FUNC_DESC;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_GROUP',scheme);
  CheckDbResult(conn.sys.FetchGroupById(FREDB_String2Guid(input.Field('selected').AsString),group),'ModifyGroup');

  if group.isProtected then raise EFRE_DB_Exception.Create('You cannot modify a protected group.');

  if Pos('$',group.ObjectName)=1 then begin
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(FetchModuleTextShort(ses,'modify_group_diag_cap'),FetchModuleTextShort(ses,'modify_group_diag_no_system_group_msg'),fdbmt_warning,nil));
  end;

  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(FetchModuleTextShort(ses,'modify_group_diag_cap'));
  res.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),ses);

  sf:=CWSF(@WEB_SaveGroup);
  sf.AddParam.Describe('selected',input.Field('selected').AsString);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),sf,fdbbt_submit);

  res.FillWithObjectValues(group.Implementor_HC as IFRE_DB_Object,ses);
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD.WEB_SaveGroup(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbo_uid          : TGUID;
    gn               : TFRE_DB_String;
    txt              : TFRE_DB_String;
    txt_s            : TFRE_DB_String;
    group            : IFRE_DB_GROUP;

begin
   if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  data    := input.Field('DATA').asobject;

  FREDB_SetStringFromExistingFieldPathOrNoChange(data,'objname',gn);
  FREDB_SetStringFromExistingFieldPathOrNoChange(data,'desc.txt',txt);
  FREDB_SetStringFromExistingFieldPathOrNoChange(data,'desc.txt_s',txt_s);

  dbo_uid := FREDB_String2Guid(input.Field('selected').Asstring);

  CheckDbResult(conn.sys.FetchGroupById(dbo_uid,group),'SaveGroup');
  if group.isProtected then raise EFRE_DB_Exception.Create('You cannot modify a protected group.');

  res := conn.sys.ModifyGroupById(dbo_uid,gn,txt,txt_s);
  if res=edb_OK then
    exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
  else
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(FetchModuleTextShort(ses,'group_modify_error_cap'),StringReplace(FetchModuleTextShort(ses,'group_modify_error_msg'),'%error_msg%',CFRE_DB_Errortype[res],[rfReplaceAll]),fdbmt_error,nil));
end;

function TFRE_COMMON_GROUP_MOD.WEB_DeleteGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
  dbo_uid: TGuid;
  group  : IFRE_DB_GROUP;
  i      : Integer;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  sf:=CWSF(@WEB_DeleteGroupConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  msg:=_getGroupsString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
  for i := 0 to input.Field('selected').ValueCount-1 do begin
    dbo_uid:=FREDB_String2Guid(input.Field('selected').AsStringItem[i]);
    CheckDbResult(conn.sys.FetchGroupById(dbo_uid,group),'DeleteGroup');
    if group.isProtected then raise EFRE_DB_Exception.Create('You cannot delete a protected group.');
    if conn.IsReferenced(group.UID,false,'TFRE_DB_USER') and not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then begin
      Result:=TFRE_DB_MESSAGE_DESC.create.Describe(FetchModuleTextShort(ses,'delete_not_empty_group_error_diag_cap'),FetchModuleTextShort(ses,'delete_not_empty_group_error_diag_msg'),fdbmt_error);
      exit;
    end;
  end;
  if input.Field('selected').ValueCount>1 then begin
    cap:=FetchModuleTextShort(ses,'delete_groups_diag_cap');
    msg:=StringReplace(FetchModuleTextShort(ses,'delete_groups_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
  end else begin
    cap:=FetchModuleTextShort(ses,'delete_group_diag_cap');
    msg:=StringReplace(FetchModuleTextShort(ses,'delete_group_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
  end;
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
end;

function TFRE_COMMON_GROUP_MOD.WEB_DeleteGroupConfirmed(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  cap,msg: String;
  i      : NativeInt;
  dbo_uid: TGuid;
  group  : IFRE_DB_GROUP;
  users  : TFRE_DB_GUIDArray;
  j      : Integer;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.field('confirmed').AsBoolean then begin
    msg:=_getGroupsString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
    if input.Field('selected').ValueCount>1 then begin
      cap:=FetchModuleTextShort(ses,'groups_deleted_diag_cap');
      msg:=StringReplace(FetchModuleTextShort(ses,'groups_deleted_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
    end else begin
      cap:=FetchModuleTextShort(ses,'group_deleted_diag_cap');
      msg:=StringReplace(FetchModuleTextShort(ses,'group_deleted_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
    end;
    for i := 0 to input.Field('selected').ValueCount-1 do begin
      dbo_uid:=FREDB_String2Guid(input.Field('selected').AsStringItem[i]);
      CheckDbResult(conn.sys.FetchGroupById(dbo_uid,group),'DeleteGroupConfirmed');
      if group.isProtected then raise EFRE_DB_Exception.Create('You cannot delete a protected group.');

      conn.ExpandReferences(TFRE_DB_GUIDArray.create(group.UID),TFRE_DB_NameTypeRLArray.create('TFRE_DB_USER<USERGROUPIDS'),users);
      for j := 0 to High(users) do begin
        CheckDbResult(conn.sys.RemoveUserGroupsById(users[j],TFRE_DB_GUIDArray.Create(group.UID)));
      end;

      CheckDbResult(conn.sys.DeleteGroupById(dbo_uid),'DeleteGroupConfirmed');
    end;
    Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_info);
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_GGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  dtxt,mtxt : TFRE_DB_String;
  i         : Integer;
  dbo       : IFRE_DB_Object;
begin
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) or conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then begin
    for i := 0 to input.Field('selected').ValueCount - 1 do begin
      CheckDbResult(conn.Fetch(FREDB_String2Guid(input.Field('selected').AsStringItem[i]),dbo));
      if not dbo.IsA('TFRE_DB_GROUP') or ((dbo.Implementor_HC as IFRE_DB_GROUP).isProtected) then begin
        Result:=GFRE_DB_NIL_DESC;
        exit;
      end;
    end;
    if input.Field('selected').ValueCount=1 then begin
      mtxt:=FetchModuleTextShort(ses,'cm_modify_group');
      dtxt:=FetchModuleTextShort(ses,'cm_delete_group');
    end else begin
      mtxt:=FetchModuleTextShort(ses,'cm_modify_group');
      dtxt:=FetchModuleTextShort(ses,'cm_delete_groups');
    end;
    res:=TFRE_DB_MENU_DESC.create.Describe;
    if conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,dbo.DomainID) then
      begin
        func:=CWSF(@WEB_ModifyGroup);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(mtxt,'',func,input.Field('selected').ValueCount>1);
      end;
    if conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_GROUP,dbo.DomainID) then
      begin
        func:=CWSF(@WEB_DeleteGroup);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(dtxt,'',func);
      end;
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_GGNotification(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sel_guid      : TGUID;
  oldSelIsGroup : Boolean;
  newSelIsGroup : Boolean;
  selObj        : IFRE_DB_Object;
  notEditable   : Boolean;
  domainUid     : TGuid;
  groupProtected: Boolean;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  oldSelIsGroup:=ses.GetSessionModuleData(ClassName).FieldExists('selectedGroups');

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    CheckDbResult(GetDBConnection(input).Fetch(sel_guid,selObj),'group fetch failed)');
    if selObj.IsA('TFRE_DB_GROUP') then begin
      ses.GetSessionModuleData(ClassName).Field('selectedGroups').AsString:=input.Field('SELECTED').AsString;
      domainUid:=selObj.DomainID;
      groupProtected:=(selObj.Implementor_HC as IFRE_DB_GROUP).isProtected;
      notEditable:=not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_GROUP,domainUid);
      selObj.Finalize;
    end else begin
      ses.GetSessionModuleData(ClassName).DeleteField('selectedGroups');
    end;
  end else begin
    ses.GetSessionModuleData(ClassName).DeleteField('selectedGroups');
  end;

  newSelIsGroup:=ses.GetSessionModuleData(ClassName).FieldExists('selectedGroups');

  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then begin
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_group',not (newSelIsGroup and conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_GROUP,domainUid)) or groupProtected));
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_group',not newSelIsGroup or notEditable or groupProtected));
  end;
  if newSelIsGroup<>oldSelIsGroup then begin
    if IsContentUpdateVisible(ses,'GROUP_DETAILS') then begin
      if newSelIsGroup then begin
        Result:=_getDetails(input,ses,app,conn);
      end else begin
        Result:=_getNoGroupDetails(input,ses,app,conn);
      end;
    end else begin
      Result:=GFRE_DB_NIL_DESC;
    end;
  end else begin
    if IsContentUpdateVisible(ses,'GROUPMOD_ROLEOUT_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('GROUPMOD_ROLEOUT_GRID',notEditable or groupProtected));
    end;
    if IsContentUpdateVisible(ses,'GROUPMOD_ROLEIN_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('GROUPMOD_ROLEIN_GRID',notEditable or groupProtected));
    end;
    if IsContentUpdateVisible(ses,'GROUPMOD_USEROUT_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('GROUPMOD_USEROUT_GRID',notEditable));
    end;
    if IsContentUpdateVisible(ses,'GROUPMOD_USERIN_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('GROUPMOD_USERIN_GRID',notEditable));
    end;
    Result:=GFRE_DB_NIL_DESC;
  end;
end;



function TFRE_COMMON_GROUP_MOD.WEB_UIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkUGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromUser);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_remove_group_from_user');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_remove_group_from_users');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_UOGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkUGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToUser);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_add_group_to_user');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_add_group_to_users');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_UIGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  removeDisabled: Boolean;
begin
  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    removeDisabled:=not _checkUGConditions(input,ses,app,conn);
  end else begin
    removeDisabled:=true;
  end;
  Result:=TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_remove_group_from_user',removeDisabled);
end;

function TFRE_COMMON_GROUP_MOD.WEB_UOGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  addDisabled: Boolean;
begin
  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    addDisabled:=not _checkUGConditions(input,ses,app,conn);
  end else begin
    addDisabled:=true;
  end;
  Result:=TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_add_group_to_user',addDisabled);
end;

function TFRE_COMMON_GROUP_MOD.WEB_RemoveFromUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user     : IFRE_DB_USER;
  group    : IFRE_DB_GROUP;
  i        : Integer;
  res      : TFRE_DB_Errortype;
  groupUid : TFRE_DB_String;
  user_id  : TGuid;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('uids_ref') then begin
    groupUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      groupUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to WEB_RemoveFromUser!');
    end;
  end;

  CheckDbResult(conn.sys.FetchGroupById(FREDB_String2Guid(groupUid),group),'RemoveFromUser');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    user_id:=FREDB_String2Guid(input.Field('selected').AsStringArr[i]);
    if conn.sys.FetchUserById(user_id,user)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_user_msg'),'%user%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.RemoveUserGroupsById(user_id,TFRE_DB_GUIDArray.Create(group.UID))<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_remove_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;

end;

function TFRE_COMMON_GROUP_MOD.WEB_AddToUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user     : IFRE_DB_USER;
  group    : IFRE_DB_GROUP;
  i        : Integer;
  res      : TFRE_DB_Errortype;
  groupUid : TFRE_DB_String;
  user_id  : TGuid;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('uids_ref') then begin
    groupUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      groupUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to WEB_AddToUser!');
    end;
  end;

  CheckDbResult(conn.sys.FetchGroupById(FREDB_String2Guid(groupUid),group),'AddToUser');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    user_id:=FREDB_String2Guid(input.Field('selected').AsStringArr[i]);
    if conn.sys.FetchUserById(user_id,user)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_user_msg'),'%user%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.ModifyUserGroupsById(user_id,TFRE_DB_GUIDArray.Create(group.UID),true)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_add_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkRGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_remove_group_from_role');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_remove_group_from_roles');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_ROGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkRGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_add_group_to_role');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_add_group_to_roles');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;

  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RIGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  removeDisabled: Boolean;
begin
  if conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
      removeDisabled:=not _checkRGConditions(input,ses,app,conn);
    end else begin
      removeDisabled:=true;
    end;
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_remove_group_from_role',removeDisabled));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_GROUP_MOD.WEB_ROGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  addDisabled: Boolean;
begin
  if conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
      addDisabled:=not _checkRGConditions(input,ses,app,conn);
    end else begin
      addDisabled:=true;
    end;
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_add_group_to_role',addDisabled));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RemoveFromRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  result := _addremoverole(input,ses,app,conn,false);
end;

function TFRE_COMMON_GROUP_MOD.WEB_AddToRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));
  result := _addremoverole(input,ses,app,conn,true);
end;

{ TFRE_COMMON_USER_MOD }

function TFRE_COMMON_USER_MOD._getUsersString(const logins: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION; const ses: IFRE_DB_UserSession): String;
var
  i   : Integer;
  res : String;
  user: IFRE_DB_USER;
begin
  for i := 0 to Length(logins) - 1 do begin
    CheckDbResult(conn.sys.FetchUserById(FREDB_String2Guid(logins[i]),user),'_getUsersString');
    if i>0 then begin
      if i=(Length(logins) - 1) then begin
        res:=res + ' ' +FetchModuleTextShort(ses,'and')+' ';
      end else begin
        res:=res+', ';
      end;
    end;
    res:=res+user.Login + ' ';
    if (user.Firstname<>'') or (user.Lastname<>'') then begin
      res:=res+'(';
      if user.Firstname<>'' then begin
        res:=res+user.Firstname;
        if user.Lastname<>'' then begin
          res:=res+' ';
        end;
      end;
      if user.Lastname<>'' then begin
        res:=res+user.Lastname;
      end;
      res:=res+')';
    end;
  end;
  Result:=res;
end;

function TFRE_COMMON_USER_MOD._getDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  sec : TFRE_DB_SUBSECTIONS_DESC;
begin
  sec:=TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  sec.AddSection.Describe(CWSF(@WEB_ContentInfo),FetchModuleTextShort(ses,'userinfo_tab'),1);
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_NOTE) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentNote),FetchModuleTextShort(ses,'usernote_tab'),2);
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentGroups),FetchModuleTextShort(ses,'groups_tab'),3);
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE) then begin
    sec.AddSection.Describe(CWSF(@WEB_ContentRoles),FetchModuleTextShort(ses,'roles_tab'),4);
  end;
  sec.contentId:='USER_DETAILS';
  Result:=sec;
end;

function TFRE_COMMON_USER_MOD._getNoUserDetails(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): TFRE_DB_CONTENT_DESC;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe;
  res.AddSection.Describe(CWSF(@WEB_ContentNoUserSel),FetchModuleTextShort(ses,'nouser_tab'),1);
  res.contentId:='USER_DETAILS';
  Result:=res;
end;

function TFRE_COMMON_USER_MOD._checkGGConditions(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): Boolean;
var
  i  : Integer;
  dbo: IFRE_DB_Object;
begin
  if not (input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1)) then begin
    Result:=false;
    exit;
  end;

  CheckDbResult(conn.Fetch(FREDB_String2Guid(input.FieldPath('dependency.uids_ref.filtervalues').AsString),dbo));
  if not conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,dbo.DomainID) then begin
    Result:=false;
    exit;
  end;

  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(input.Field('selected').AsStringItem[i]),dbo));
    if not conn.sys.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,dbo.DomainID) then begin
      Result:=false;
      exit;
    end;
  end;
  Result:=true;
end;

class procedure TFRE_COMMON_USER_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_USER_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('user_description')
end;

class procedure TFRE_COMMON_USER_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.1';
  if currentVersionId='' then begin
    currentVersionId := '1.0';

    CreateModuleText(conn,'roles_tab','Roles');
    CreateModuleText(conn,'groups_tab','Groups');
    CreateModuleText(conn,'usernote_tab','Notes');
    CreateModuleText(conn,'userinfo_tab','User Properties');
    CreateModuleText(conn,'user_content_header','Informations about the selected user.');

    CreateModuleText(conn,'gc_user','User');
    CreateModuleText(conn,'gc_group','Group');
    CreateModuleText(conn,'gc_role','Role');
    CreateModuleText(conn,'gc_domain_user','Domain / User');
    CreateModuleText(conn,'gcap_UinG','User is in Group');
    CreateModuleText(conn,'gcap_UnotG','User is not in Group');
    CreateModuleText(conn,'gcap_UhasR','User has Role');
    CreateModuleText(conn,'gcap_UnotR','User has not Role');
    CreateModuleText(conn,'user_details_select_one','Please select a user to get detailed information');
    CreateModuleText(conn,'nouser_tab','General');

    CreateModuleText(conn,'add_user_diag_cap','Add new user');
    CreateModuleText(conn,'delete_user_diag_cap','Confirm: Delete user');
    CreateModuleText(conn,'delete_users_diag_cap','Confirm: Delete multiple users');
    CreateModuleText(conn,'delete_user_diag_msg','User %user_str% will be deleted permanently! Please confirm to continue.');
    CreateModuleText(conn,'delete_users_diag_msg','Users %user_str% will be deleted permanently! Please confirm to continue.');
    CreateModuleText(conn,'user_deleted_diag_cap','User deleted');
    CreateModuleText(conn,'users_deleted_diag_cap','Users deleted');
    CreateModuleText(conn,'user_deleted_diag_msg','User %user_str% successfully deleted.');
    CreateModuleText(conn,'users_deleted_diag_msg','Users %user_str% successfully deleted.');

    CreateModuleText(conn,'cm_delete_user','Delete','','Delete User');
    CreateModuleText(conn,'cm_delete_users','Delete','','Delete Users');
    CreateModuleText(conn,'cm_remove_user_from_groups','Remove user from groups');
    CreateModuleText(conn,'cm_add_user_to_groups','Add user to groups');
    CreateModuleText(conn,'cm_remove_user_from_group','Remove user from group');
    CreateModuleText(conn,'cm_add_user_to_group','Add user to group');

    CreateModuleText(conn,'tb_delete_user','Delete','','Delete User');
    CreateModuleText(conn,'tb_add_user','Add','','Add User');
    CreateModuleText(conn,'tb_remove_user_from_group','Remove');
    CreateModuleText(conn,'tb_add_user_to_group','Add');

    CreateModuleText(conn,'and','and');

    CreateModuleText(conn,'error_fetch_group_msg','Could not fetch group with id %group%');
    CreateModuleText(conn,'error_fetch_user_msg','Could not fetch user with id %user%');
    CreateModuleText(conn,'error_delete_user_msg','Could not delete user with id %user%');
    CreateModuleText(conn,'error_add_group_msg','Could not add user %user% to group %group%');
    CreateModuleText(conn,'error_remove_group_msg','Could not remove user %user% from group %group%');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId := '1.1';

    CreateModuleText(conn,'gc_domain','Domain');
  end;

end;

procedure TFRE_COMMON_USER_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var user_Grid     : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid       : IFRE_DB_SIMPLE_TRANSFORM;

    groupin_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_GridIn     : IFRE_DB_SIMPLE_TRANSFORM;

    groupout_Grid : IFRE_DB_DERIVED_COLLECTION;
    tr_GridOut    : IFRE_DB_SIMPLE_TRANSFORM;

    rolein_Grid   : IFRE_DB_DERIVED_COLLECTION;
    tr_roleIn     : IFRE_DB_SIMPLE_TRANSFORM;

    roleout_Grid  : IFRE_DB_DERIVED_COLLECTION;
    tr_roleOut    : IFRE_DB_SIMPLE_TRANSFORM;

    app           : TFRE_DB_APPLICATION;
    conn          : IFRE_DB_CONNECTION;
    grid_column_cap: TFRE_DB_String;
    show_domains   : Boolean;
begin
  inherited;
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    show_domains := session.HasFeature('DOMAIN') and conn.SYS.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_DOMAIN_MOD);

    with tr_Grid do begin
      if show_domains then begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_domain_user');
      end else begin
        grid_column_cap:=FetchModuleTextShort(session,'gc_user');
      end;
      AddOneToOnescheme('displayname','',grid_column_cap,dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      SetFinalRightTransformFunction(@CalculateUserIcon);
      AddFulltextFilterOnTransformed(['displayname']);
    end;

    user_grid := session.NewDerivedCollection('USERMOD_USER_GRID');
    with user_grid do begin
      if show_domains then begin
        SetDeriveParent           (session.GetDBConnection.AdmGetDomainCollection);
        SetDisplayType            (cdt_Listview,[cdgf_Children,cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_UGMenu),nil,CWSF(@WEB_UserSelected));
        SetParentToChildLinkField ('TFRE_DB_USER<DOMAINIDLINK');
      end else begin
        SetDeriveParent           (session.GetDBConnection.AdmGetUserCollection);
        SetDisplayType            (cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_UGMenu),nil,CWSF(@WEB_UserSelected));
      end;
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation   (tr_Grid);
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GridIn);
    with tr_GridIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_group'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('_disabledrag_','','',dt_boolean,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateGroupFields);
    end;

    groupin_Grid := session.NewDerivedCollection('USERMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['USERGROUPIDS>TFRE_DB_GROUP'],false);
      user_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_GridIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_UinG'),nil,'',CWSF(@WEB_GIGMenu),nil,CWSF(@WEB_GIGNotification),nil,CWSF(@WEB_AddToGroup));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GridOut);
    with tr_GridOut do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_group'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('_disabledrag_','','',dt_boolean,false);
      AddOneToOnescheme('protected','','',dt_string,False);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateGroupFields);
    end;

    groupout_Grid := session.NewDerivedCollection('USERMOD_GROUPOUT_GRID');
    with groupout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['USERGROUPIDS>TFRE_DB_GROUP'],true);
      user_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_GridOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],FetchModuleTextShort(session,'gcap_UnotG'),nil,'',CWSF(@WEB_GOGMenu),nil,CWSF(@WEB_GOGNotification),nil,CWSF(@WEB_RemoveFromGroup));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleIn);
    with tr_RoleIn do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_role'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateRoleIcon);
    end;

    rolein_Grid := session.NewDerivedCollection('USERMOD_ROLEIN_GRID');
    with rolein_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['USERGROUPIDS>TFRE_DB_GROUP','ROLEIDS>TFRE_DB_ROLE'],false);
      user_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_RoleIn);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_UhasR'));
      SetDefaultOrderField('displayname',true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleOut);
    with tr_RoleOut do begin
      AddOneToOnescheme('displayname','',FetchModuleTextShort(session,'gc_role'),dt_string,true,false,false,1,'icon');
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('internal','','',dt_string,False);
      if show_domains then begin
        AddMatchingReferencedField('DOMAINIDLINK>TFRE_DB_DOMAIN','displayname','domain',FetchModuleTextShort(session,'gc_domain'));
      end;
      SetFinalRightTransformFunction(@CalculateRoleIcon);
    end;
    roleout_Grid := session.NewDerivedCollection('USERMOD_ROLEOUT_GRID');
    with roleout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['USERGROUPIDS>TFRE_DB_GROUP','ROLEIDS>TFRE_DB_ROLE'],true);
      user_Grid.AddSelectionDependencyEvent(CollectionName);
      if CHIDE_INTERNAL then begin
        AddBooleanFieldFilter('internal','internal',false);
      end;
      SetDeriveTransformation(tr_RoleOut);
      SetDisplayType(cdt_Listview,[],FetchModuleTextShort(session,'gcap_UnotR'));
      SetDefaultOrderField('displayname',true);
    end;
  end;
end;

procedure TFRE_COMMON_USER_MOD.CalculateRoleIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId('assignRole',TFRE_DB_ROLE,transformed_object.DomainID) then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico.svg');
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/role_ico_lck.svg');
  end;
end;

procedure TFRE_COMMON_USER_MOD.CalculateGroupFields(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if ut.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,transformed_object.DomainID) then begin
    if transformed_object.Field('protected').AsBoolean then begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_prt.svg');
    end else begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/group_ico.svg');
    end;
    transformed_object.Field('_disabledrag_').AsBoolean:=false;
  end else begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/group_ico_lck.svg');
    transformed_object.Field('_disabledrag_').AsBoolean:=true;
  end;
end;

procedure TFRE_COMMON_USER_MOD.CalculateUserIcon(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object);
begin
  if transformed_object.PreTransformedWasA('TFRE_DB_DOMAIN') then begin
    transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/domain_ico.svg');
  end else begin
    if ut.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,transformed_object.DomainID) then begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/share/user_ico.svg');
    end else begin
      transformed_object.Field('icon').AsString:=FREDB_getThemedResource('images_apps/accesscontrol/user_ico_lck.svg');
    end;
  end;
end;


function TFRE_COMMON_USER_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  usergrid    : TFRE_DB_VIEW_LIST_DESC;
  txt         : IFRE_DB_TEXT;
begin
  CheckClassVisibility4AnyDomain(ses);

  ses.GetSessionModuleData(ClassName).DeleteField('selectedUsers');
  usergrid := ses.FetchDerivedCollection('USERMOD_USER_GRID').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) then begin
    txt:=FetchModuleTextFull(ses,'tb_add_user');
    usergrid.AddButton.Describe(CWSF(@WEB_AddUser),'',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then begin
    txt:=FetchModuleTextFull(ses,'tb_delete_user');
    usergrid.AddButton.DescribeManualType('tb_delete_user',CWSF(@WEB_DeleteUser),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;
  Result:=TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(usergrid,_getNoUserDetails(input,ses,app,conn),nil,nil,nil,true);
end;

function TFRE_COMMON_USER_MOD.WEB_UserSelected(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  sel_guid    : TGUID;
  selObj         : IFRE_DB_Object;
  oldSelIsUser   : Boolean;
  newSelIsUser   : Boolean;
  domainUid      : TGuid;
  delUserDisabled: Boolean;
  notEditable    : Boolean;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  oldSelIsUser:=ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers');

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount=1)  then begin
    sel_guid := input.Field('SELECTED').AsGUID; // is user
    CheckDbResult(conn.Fetch(sel_guid,selObj),StringReplace(FetchModuleTextShort(ses,'error_fetch_user_msg'),'%user%',GUIDToString(sel_guid),[rfReplaceAll]));
    if selObj.IsA('TFRE_DB_USER') then begin
      ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString:=input.Field('SELECTED').AsString;
      domainUid:=selObj.DomainID;
      selObj.Finalize;
    end else begin
      ses.GetSessionModuleData(ClassName).DeleteField('selectedUsers');
    end;
  end else begin
    ses.GetSessionModuleData(ClassName).DeleteField('selectedUsers');
  end;

  newSelIsUser:=ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers');
  delUserDisabled:=true;
  if newSelIsUser then begin
    if conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_USER,domainUid) then begin
      delUserDisabled:=false;
    end;
  end;
  ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_user',delUserDisabled));
  if newSelIsUser<>oldSelIsUser then begin
    if newSelIsUser then begin
      Result:=_getDetails(input,ses,app,conn);
    end else begin
      Result:=_getNoUserDetails(input,ses,app,conn);
    end;
  end else begin
    if not (conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,domainUid) and conn.sys.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,domainUid)) then begin
      notEditable:=true;
    end else begin
      notEditable:=false;
    end;
    if IsContentUpdateVisible(ses,'USERMOD_GROUPIN_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('USERMOD_GROUPIN_GRID',notEditable));
    end;
    if IsContentUpdateVisible(ses,'USERMOD_GROUPOUT_GRID') then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeDrag('USERMOD_GROUPOUT_GRID',notEditable));
    end;
    if IsContentUpdateVisible(ses,'USER_DETAILS') then begin
      if newSelIsUser then begin
        if IsContentUpdateVisible(ses,'USER_INFO') then begin
          Result:=WEB_ContentInfo(input,ses,app,conn);
        end else begin
          if IsContentUpdateVisible(ses,'USER_NOTE') then begin
            Result:=WEB_ContentNote(input,ses,app,conn);
          end else begin
            Result:=GFRE_DB_NIL_DESC;
          end;
        end;
      end else begin
        Result:=GFRE_DB_NIL_DESC;
      end;
    end else begin
      Result:=GFRE_DB_NIL_DESC;
    end;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentNoUserSel(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe(FetchModuleTextShort(ses,'user_details_select_one'));
end;

function TFRE_COMMON_USER_MOD.WEB_ContentInfo(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  html          : TFRE_DB_HTML_DESC;
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
  dc            : IFRE_DB_DERIVED_COLLECTION;
  block         : TFRE_DB_INPUT_BLOCK_DESC;
  user          : IFRE_DB_USER;
  sel_guid      : TGUID;
  userEditable  : Boolean;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers')  then begin
    sel_guid := ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsGUID;
    CheckDbResult(conn.sys.FetchUserById(sel_guid,user),'UserContent');
    userEditable:=conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,user.GetDomainIDLink);
    GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(FetchModuleTextShort(ses,'user_content_header'),true,userEditable);
    block:=panel.AddBlock.Describe();
    block.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),ses,false,false,2);
    block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,false,false);
    panel.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
    panel.FillWithObjectValues(user.Implementor_HC as IFRE_DB_Object,ses);
    if userEditable then begin
      panel.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CSFT('saveOperation',user.Implementor_HC as IFRE_DB_Object),fdbbt_submit);
    end;
  end else begin
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(FetchModuleTextShort(ses,'user_content_header'));
  end;
  panel.contentId:='USER_INFO';
  Result:=panel;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentNote(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  user_editor : TFRE_DB_EDITOR_DESC;
  load_func   : TFRE_DB_SERVER_FUNC_DESC;
  save_func   : TFRE_DB_SERVER_FUNC_DESC;
  start_edit  : TFRE_DB_SERVER_FUNC_DESC;
  stop_edit   : TFRE_DB_SERVER_FUNC_DESC;
  sel_guid    : TGuid;
  user        : IFRE_DB_USER;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  save_func := nil;
  start_edit:= nil;
  stop_edit := nil;
  if ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers')  then begin
    sel_guid := ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsGUID;
    CheckDbResult(conn.sys.FetchUserById(sel_guid,user),'UserContent');
    load_func   := CWSF(@WEB_NoteLoad);
    if conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,user.GetDomainIDLink) and
       conn.sys.CheckClassRight4DomainId(sr_STORE,TFRE_DB_NOTE,user.GetDomainIDLink) and
       conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_NOTE,user.GetDomainIDLink) then begin
      save_func := CWSF(@WEB_NoteSave);
      start_edit:= CWSF(@WEB_NoteStartEdit);
      stop_edit := CWSF(@WEB_NoteStopEdit);
      save_func.AddParam.Describe('linkid',ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString);
    end;
    load_func.AddParam.Describe('linkid',ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString);
  end;

  user_editor := TFRE_DB_EDITOR_DESC.create.Describe(load_func,save_func,start_edit,stop_edit);
  user_editor.contentId := 'USER_NOTE';
  Result      := user_editor;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentGroups(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_groupin   : IFRE_DB_DERIVED_COLLECTION;
  groupingrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_groupout  : IFRE_DB_DERIVED_COLLECTION;
  groupoutgrid : TFRE_DB_VIEW_LIST_DESC;
  group        : TFRE_DB_LAYOUT_DESC;
  user         : IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_groupin := ses.FetchDerivedCollection('USERMOD_GROUPIN_GRID');
  groupingrid:= dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  groupingrid.contentId:='USERMOD_GROUPIN_GRID';
  dc_groupout:= ses.FetchDerivedCollection('USERMOD_GROUPOUT_GRID');
  groupoutgrid:= dc_groupout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  groupoutgrid.contentId:='USERMOD_GROUPOUT_GRID';

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) and conn.sys.CheckClassRight4AnyDomain('assignGroup',TFRE_DB_GROUP) then begin
    groupoutgrid.setDropGrid(groupingrid);
    groupingrid.setDropGrid(groupoutgrid);
    groupoutgrid.AddButton.DescribeManualType('tb_add_user_to_group',CWSF(@WEB_AddToGroup),'',FetchModuleTextShort(ses,'tb_add_user_to_group'),'',true);
    groupingrid.AddButton.DescribeManualType('tb_remove_user_from_group',CWSF(@WEB_RemoveFromGroup),'',FetchModuleTextShort(ses,'tb_remove_user_from_group'),'',true);
  end;
  if ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers') then begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString),user));
    if not (conn.sys.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_USER,user.DomainID) and conn.sys.CheckClassRight4DomainId('assignGroup',TFRE_DB_GROUP,user.DomainID)) then begin
      groupoutgrid.disableDrag;
      groupingrid.disableDrag;
    end;
  end;

  group   := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,groupoutgrid,nil,groupingrid,nil,true,-1,1,-1,1);
  Result  := group;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentRoles(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_rolein   : IFRE_DB_DERIVED_COLLECTION;
  roleingrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  roleoutgrid : TFRE_DB_VIEW_LIST_DESC;
  role        : TFRE_DB_LAYOUT_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  dc_rolein := ses.FetchDerivedCollection('USERMOD_ROLEIN_GRID');
  roleingrid:= dc_rolein.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_roleout:= ses.FetchDerivedCollection('USERMOD_ROLEOUT_GRID');
  roleoutgrid:= dc_roleout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  role    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,roleoutgrid,nil,roleingrid,nil,true,-1,1,-1,1);
  Result  := role;
end;

function TFRE_COMMON_USER_MOD.WEB_AddUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_FORM_DIALOG_DESC;
  block : TFRE_DB_INPUT_BLOCK_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(FetchModuleTextShort(ses,'add_user_diag_cap'),600);
  block:=res.AddBlock.Describe();
  block.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses,false,false,2);
  block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
  res.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CSCF('TFRE_DB_USER','NewUserOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_USER_MOD.WEB_DeleteUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  sf:=CWSF(@WEB_DeleteUserConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  msg:=_getUsersString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
  if input.Field('selected').ValueCount>1 then begin
    cap:=FetchModuleTextShort(ses,'delete_users_diag_cap');
    msg:=StringReplace(FetchModuleTextShort(ses,'delete_users_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
  end else begin
    cap:=FetchModuleTextShort(ses,'delete_user_diag_cap');
    msg:=StringReplace(FetchModuleTextShort(ses,'delete_user_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
  end;
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
end;

function TFRE_COMMON_USER_MOD.WEB_DeleteUserConfirmed(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  cap,msg: String;
  block  : TFRE_DB_INPUT_BLOCK_DESC;
  i      : NativeInt;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.field('confirmed').AsBoolean then begin
    msg:=_getUsersString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
    if input.Field('selected').ValueCount>1 then begin
      cap:=FetchModuleTextShort(ses,'users_deleted_diag_cap');
      msg:=StringReplace(FetchModuleTextShort(ses,'users_deleted_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
    end else begin
      cap:=FetchModuleTextShort(ses,'user_deleted_diag_cap');
      msg:=StringReplace(FetchModuleTextShort(ses,'user_deleted_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
    end;
    for i:=0 to input.Field('selected').ValueCount-1  do
      begin
        if conn.sys.DeleteUserById(FREDB_String2Guid(input.Field('selected').AsStringItem[i]))<>edb_OK then
          raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_delete_user_msg'),'%user%',input.Field('selected').AsStringItem[i],[rfReplaceAll]));
      end;
    Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_info);
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_UGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  dtxt      : TFRE_DB_String;
  i         : Integer;
  dbo       : IFRE_DB_Object;
begin
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    CheckDbResult(conn.Fetch(FREDB_String2Guid(input.Field('selected').AsStringItem[i]),dbo));
    if not (dbo.IsA('TFRE_DB_USER') and conn.sys.CheckClassRight4DomainId(sr_DELETE,TFRE_DB_USER,dbo.DomainID)) then begin
      Result:=GFRE_DB_NIL_DESC;
      exit;
    end;
  end;

  if input.Field('selected').ValueCount=1 then begin
    dtxt := FetchModuleTextShort(ses,'cm_delete_user');
  end else begin
    dtxt := FetchModuleTextShort(ses,'cm_delete_users');
  end;
  res:=TFRE_DB_MENU_DESC.create.Describe;
  func:=CWSF(@WEB_DeleteUser);
  func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  res.AddEntry.Describe(dtxt,'',func);
  Result:=res;
end;

function TFRE_COMMON_USER_MOD.WEB_GIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkGGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromGroup);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_remove_user_from_group');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_remove_user_from_groups');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_GOGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if _checkGGConditions(input,ses,app,conn) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToGroup);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=FetchModuleTextShort(ses,'cm_add_user_to_group');
    end else begin
      txt:=FetchModuleTextShort(ses,'cm_add_user_to_groups');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_GIGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  removeDisabled: Boolean;
begin
  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    removeDisabled:=not _checkGGConditions(input,ses,app,conn);
  end else begin
    removeDisabled:=true;
  end;
  Result:=TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_remove_user_from_group',removeDisabled);
end;

function TFRE_COMMON_USER_MOD.WEB_GOGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  addDisabled: Boolean;
begin
  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    addDisabled:=not _checkGGConditions(input,ses,app,conn);
  end else begin
    addDisabled:=true;
  end;
  Result:=TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_add_user_to_group',addDisabled);
end;

function TFRE_COMMON_USER_MOD.WEB_RemoveFromGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user    : IFRE_DB_USER;
  group   : IFRE_DB_GROUP;
  i       : Integer;
  userUid : String;
  group_id: TGuid;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('uids_ref') then begin
    userUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      userUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on user has to be passed to WEB_RemoveFromGroup!');
    end;
  end;
  CheckDbResult(conn.sys.FetchUserById(FREDB_String2Guid(userUid),user),'RemoveFromGroup');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    group_id:=FREDB_String2Guid(input.Field('selected').AsStringArr[i]);
    if conn.sys.FetchGroupById(group_id,group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.RemoveUserGroupsById(user.UID,TFRE_DB_GUIDArray.Create(group_id))<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(FetchModuleTextShort(ses,'error_remove_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_USER_MOD.WEB_AddToGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user    : IFRE_DB_USER;
  group   : IFRE_DB_GROUP;
  i       : Integer;
  userUid : String;
  group_id: TGuid;
  res     : TFRE_DB_Errortype;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.FieldExists('uids_ref') then begin
    userUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      userUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on user has to be passed to WEB_AddToGroup!');
    end;
  end;
  CheckDbResult(conn.sys.FetchUserById(FREDB_String2Guid(userUid),user),'AddToGroup');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    group_id:=FREDB_String2Guid(input.Field('selected').AsStringArr[i]);
    if conn.sys.FetchGroupById(group_id,group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(FetchModuleTextShort(ses,'error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    res:=conn.sys.ModifyUserGroupsById(user.UID,TFRE_DB_GUIDArray.Create(group_id),true);
    if res<>edb_OK then
      raise EFRE_DB_Exception.Create(res,StringReplace(StringReplace(FetchModuleTextShort(ses,'error_add_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

{ TFRE_COMMON_ACCESSCONTROL_APP }

procedure TFRE_COMMON_ACCESSCONTROL_APP.SetupApplicationStructure;
begin
  inherited SetupApplicationStructure;
  AddApplicationModule(TFRE_COMMON_DOMAIN_MOD.create);
  AddApplicationModule(TFRE_COMMON_USER_MOD.create);
  AddApplicationModule(TFRE_COMMON_GROUP_MOD.create);
  AddApplicationModule(TFRE_COMMON_ROLE_MOD.create);
  AddApplicationModule(TFRE_COMMON_WF_MOD.create);
end;


procedure TFRE_COMMON_ACCESSCONTROL_APP._UpdateSitemap( const session: TFRE_DB_UserSession); //MEMFIX?
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
  pos          : Integer;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status',FetchAppTextShort(session,'sitemap_main'),'images_apps/accesscontrol/accesscontrol.svg','',0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_ACCESSCONTROL_APP));
  if session.HasFeature('DOMAIN') and conn.SYS.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_DOMAIN_MOD) then begin
    FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Domains',FetchAppTextShort(session,'sitemap_domains'),'images_apps/accesscontrol/domain.svg',TFRE_COMMON_DOMAIN_MOD.ClassName);
    pos:=0;
  end else begin
    pos:=-45;
  end;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/User',FetchAppTextShort(session,'sitemap_users'),'images_apps/accesscontrol/user.svg',TFRE_COMMON_USER_MOD.Classname,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_USER_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Groups',FetchAppTextShort(session,'sitemap_groups'),'images_apps/accesscontrol/group.svg',TFRE_COMMON_GROUP_MOD.Classname,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_GROUP_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Roles',FetchAppTextShort(session,'sitemap_roles'),'images_apps/accesscontrol/role.svg',TFRE_COMMON_ROLE_MOD.ClassName,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_ROLE_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/WFs',FetchAppTextShort(session,'sitemap_wfs'),'images_apps/accesscontrol/wf.svg',TFRE_COMMON_WF_MOD.ClassName,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_WF_MOD));
  FREDB_SiteMap_RadialAutoposition(SiteMapData,pos);
  session.GetSessionAppData(ClassName).Field('SITEMAP').AsObject := SiteMapData;
end;

procedure TFRE_COMMON_ACCESSCONTROL_APP.MySessionInitialize(  const session: TFRE_DB_UserSession);
begin
  exit;
  inherited MySessionInitialize(session);
  if session.IsInteractiveSession then
    _UpdateSitemap(session);
end;

procedure TFRE_COMMON_ACCESSCONTROL_APP.MySessionPromotion(  const session: TFRE_DB_UserSession);
begin
  inherited MySessionPromotion(session);
  if session.IsInteractiveSession then
    _UpdateSitemap(session);
end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited;

  newVersionId:='1.1';

  if (currentVersionId='') then begin
    currentVersionId:='1.0';
    CreateAppText(conn,'caption','Access Control','Access Control','Access Control');
    CreateAppText(conn,'user_description','Users','Users','Users');
    CreateAppText(conn,'group_description','Groups','Groups','Groups');
    CreateAppText(conn,'role_description','Roles','Roles','Roles');
    CreateAppText(conn,'domain_description','Domains','Domains','Domains');

    CreateAppText(conn,'sitemap_main','Access Control','','Access Control');
    CreateAppText(conn,'sitemap_users','Users','','Users');
    CreateAppText(conn,'sitemap_groups','Groups','','Groups');
    CreateAppText(conn,'sitemap_roles','Roles','','Roles');
    CreateAppText(conn,'sitemap_domains','Domains','','Domains');
  end;
  if (currentVersionId='1.0') then begin
    currentVersionId:='1.1';
    CreateAppText(conn,'wf_description','Workflows','Workflows','Workflows');
    CreateAppText(conn,'sitemap_wfs','WFs','','WFs');
  end;
end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
var
  group : IFRE_DB_GROUP;
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);

  if currentVersionId='' then begin
    currentVersionId:='1.0';

    CheckDbResult(conn.AddRole('ACADMINUSER','Allowed to create, modify and delete Users','',domainUID),'could not add role ACADMINUSER');

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSER',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      TFRE_COMMON_USER_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSER',domainUID,TFRE_DB_USER.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSER',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSER',domainUID,TFRE_DB_NOTE.GetClassStdRoles));

    CheckDbResult(conn.AddRole('ACADMINGROUP','Allowed to create, modify and delete Groups','',domainUID),'could not add role ACADMINGROUP');

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      TFRE_COMMON_GROUP_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_ROLE_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_GROUP.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_ROLE.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_StringArray.Create(TFRE_DB_ROLE.GetClassRoleName('assignRole'))));

    CheckDbResult(conn.AddRole('ACADMINUSERGROUP','Allowed to modify Users and assign Groups to Users','',domainUID),'could not add role ACADMINUSERGROUP');

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      TFRE_COMMON_USER_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_GROUP_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_ROLE_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_USER.GetClassStdRoles(false,true,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_GROUP.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_ROLE.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_NOTE.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_StringArray.Create(TFRE_DB_GROUP.GetClassRoleName('assignGroup'))));

    CheckDbResult(conn.AddGroup('ACADMINS','Access Control Admins','Access Control Admins',domainUID,true),'could not create admins group');

    CheckDbResult(conn.AddRolesToGroup('ACADMINS',domainUID,TFRE_DB_StringArray.Create('ACADMINUSER','ACADMINGROUP','ACADMINUSERGROUP')),'could not add roles for group Admins');
    CheckDbResult(conn.AddSysRolesToGroup('ACADMINS',domainUID,TFRE_DB_StringArray.Create('ACVIEWSYSTEM')),'could not add roles for group Admins');
  end;
  if currentVersionId='1.0' then begin
    currentVersionId:='1.1';

    CheckDbResult(conn.AddRole('ADMINUSER','Allowed to create, modify and delete Users','',domainUID,true),'could not add role ADMINUSER');

    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSER',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_USER_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSER',domainUID,TFRE_DB_USER.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSER',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSER',domainUID,TFRE_DB_NOTE.GetClassStdRoles));

    CheckDbResult(conn.AddRole('ADMINGROUP','Allowed to create, modify and delete Groups','',domainUID,true),'could not add role ADMINGROUP');

    CheckDbResult(conn.AddRoleRightsToRole('ADMINGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_GROUP_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_ROLE_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINGROUP',domainUID,TFRE_DB_GROUP.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINGROUP',domainUID,TFRE_DB_ROLE.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINGROUP',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINGROUP',domainUID,TFRE_DB_StringArray.Create(TFRE_DB_ROLE.GetClassRoleName('assignRole'))));

    CheckDbResult(conn.AddRole('ADMINUSERGROUP','Allowed to modify Users and assign Groups to Users','',domainUID),'could not add role ADMINUSERGROUP');

    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_USER_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_GROUP_MOD.GetClassRoleNameFetch,
      TFRE_COMMON_ROLE_MOD.GetClassRoleNameFetch
    )));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_USER.GetClassStdRoles(false,true,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_GROUP.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_ROLE.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_DOMAIN.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_NOTE.GetClassStdRoles));
    CheckDbResult(conn.AddRoleRightsToRole('ADMINUSERGROUP',domainUID,TFRE_DB_StringArray.Create(TFRE_DB_GROUP.GetClassRoleName('assignGroup'))));

    CheckDbResult(conn.RemoveAllRolesFromGroup('ACADMINS',domainUID));

    CheckDbResult(conn.DeleteRole('ACADMINUSER',domainUID));
    CheckDbResult(conn.DeleteRole('ACADMINGROUP',domainUID));
    CheckDbResult(conn.DeleteRole('ACADMINUSERGROUP',domainUID));

    CheckDbResult(conn.AddRole('ACADMINUSER','Allowed to create, modify and delete Users','',domainUID),'could not add role ACADMINUSER');
    CheckDbResult(conn.AddRole('ACADMINGROUP','Allowed to create, modify and delete Groups','',domainUID),'could not add role ACADMINGROUP');
    CheckDbResult(conn.AddRole('ACADMINUSERGROUP','Allowed to modify Users and assign Groups to Users','',domainUID),'could not add role ACADMINUSERGROUP');

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSER',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      'ADMINUSER'
    )));

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      'ADMINGROUP'
    )));

    CheckDbResult(conn.AddRoleRightsToRole('ACADMINUSERGROUP',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_ACCESSCONTROL_APP.GetClassRoleNameFetch,
      'ADMINUSERGROUP'
    )));

    CheckDbResult(conn.AddRolesToGroup('ACADMINS',domainUID,TFRE_DB_StringArray.Create('ACADMINUSER','ACADMINGROUP','ACADMINUSERGROUP')),'could not add roles for group Admins');
    CheckDbResult(conn.AddSysRolesToGroup('ACADMINS',domainUID,TFRE_DB_StringArray.Create('ACVIEWSYSTEM')),'could not add roles for group Admins');
  end;

end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.InstallDBObjects4SysDomain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
var
  group : IFRE_DB_GROUP;
begin
  inherited InstallDBObjects4SysDomain(conn, currentVersionId, domainUID);

  if currentVersionId='' then begin
    currentVersionId:='1.0';
    //ADMINS
    CheckDbResult(conn.AddRole('ACVIEWSYSTEM','Allowed to view System Groups and Roles','',domainUID),'could not add role ACVIEWSYSTEM');

    CheckDbResult(conn.AddRoleRightsToRole('ACVIEWSYSTEM',domainUID,TFRE_DB_GROUP.GetClassStdRoles(false,false,false,true)));
    CheckDbResult(conn.AddRoleRightsToRole('ACVIEWSYSTEM',domainUID,TFRE_DB_ROLE.GetClassStdRoles(false,false,false,true)));
  end;
end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.RegisterSystemScheme( const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
end;

function TFRE_COMMON_ACCESSCONTROL_APP.isMultiDomainApp: Boolean;
begin
  Result:=true;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_DOMAIN_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_USER_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_GROUP_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_ROLE_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_WF_MOD);

  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_ACCESSCONTROL_APP);
  GFRE_DBI.Initialize_Extension_Objects;
end;


end.
