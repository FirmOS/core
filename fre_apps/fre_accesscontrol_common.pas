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

type

  { TFRE_COMMON_ACCESSCONTROL_APP }

  TFRE_COMMON_ACCESSCONTROL_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;
    procedure       _UpdateSitemap                (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize           (const session: TFRE_DB_UserSession);override;
    procedure       MySessionPromotion            (const session: TFRE_DB_UserSession); override;
    class procedure InstallDBObjects              (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain       (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); override;
  public
    class procedure RegisterSystemScheme          (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
  end;

  { TFRE_COMMON_USER_MOD }

  TFRE_COMMON_USER_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _getUsersString           (const logins: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION;const ses: IFRE_DB_UserSession): String;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UserSelected          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentInfo           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentRoles          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddUser               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteUser            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteUserConfirmed   (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GOGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromGroup       (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToGroup            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UserContent           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_GROUP_MOD }

  TFRE_COMMON_GROUP_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _getGroupsString          (const groups: TFRE_DB_StringArray; const conn: IFRE_DB_CONNECTION;const ses: IFRE_DB_UserSession): String;
    function        _addremoverole            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION;const addrole:boolean): IFRE_DB_Object;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentRoles          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddGroup              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ModifyGroup           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteGroup           (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteGroupConfirmed  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GGNotification        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_UOGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromUser        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToUser             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RIGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ROGMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToRole             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromRole        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_ROLE_MOD }

  TFRE_COMMON_ROLE_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    function        _addremoverole            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION; const addrole:boolean):IFRE_DB_Object;
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RoleNotification      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddToRole             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GIRMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GORMenu               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_RemoveFromRole        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_COMMON_DOMAIN_MOD }

  TFRE_COMMON_DOMAIN_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
  protected
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure   ; override;
  public
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentUsers          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ContentGroups         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_AddDomain             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_CreateDomain          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ModifyDomain          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteDomain          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DeleteDomainConfirmed (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_SaveDomain            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DGMenu                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_DGNotification        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


procedure Register_DB_Extensions;

implementation

{ TFRE_COMMON_DOMAIN_MOD }

class procedure TFRE_COMMON_DOMAIN_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_DOMAIN_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$domain_description')
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
      AddOneToOnescheme ('objname','domain',app.FetchAppTextShort(session,'$gc_domain'));
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'DOMAIN_DESC',app.FetchAppTextShort(session,'$gc_domain_desc'));
    end;
    domain_Grid := session.NewDerivedCollection('DOMAINMOD_DOMAIN_GRID');
    with domain_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetDomainCollection);
      SetDeriveTransformation(tr_domain);
      SetDisplayType(cdt_Listview,[],'',nil,'',CWSF(@WEB_DGMenu),nil,CWSF(@WEB_DGNotification));
      AddRightFilterForEntryAndUser('RF','VIEWDOM');
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
    end;
    userin_Grid := session.NewDerivedCollection('DOMAINMOD_USERIN_GRID');
    with userin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_USER<DOMAINIDLINK']);
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_UinD'));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupIn);
    with tr_groupIn do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
    end;
    groupin_Grid := session.NewDerivedCollection('DOMAINMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_GROUP<DOMAINIDLINK']);
      SetDeriveTransformation(tr_groupIn);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_GinD'));
    end;
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  main          : TFRE_DB_CONTENT_DESC;
  sec           : TFRE_DB_SUBSECTIONS_DESC;
  domaingrid    : TFRE_DB_VIEW_LIST_DESC;
  dc_domain     : IFRE_DB_DERIVED_COLLECTION;
  dc_userin     : IFRE_DB_DERIVED_COLLECTION;
  dc_groupin    : IFRE_DB_DERIVED_COLLECTION;
  txt           : IFRE_DB_TEXT;

begin
  CheckClassVisibility(ses);

  dc_domain     := ses.FetchDerivedCollection('DOMAINMOD_DOMAIN_GRID');
  domaingrid    := dc_domain.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  result        := domaingrid;
  sec     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;

  dc_userin   := ses.FetchDerivedCollection('DOMAINMOD_USERIN_GRID');

  domaingrid.AddFilterEvent(dc_userin.getDescriptionStoreId(),'uids');

  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_DOMAIN) then begin  //class right without domain
    txt:=app.FetchAppTextFull(ses,'$tb_add_domain');
    domaingrid.AddButton.Describe(CWSF(@WEB_AddDomain),'',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
    txt:=app.FetchAppTextFull(ses,'$tb_modify_domain');
    domaingrid.AddButton.DescribeManualType('tb_modify_domain',CWSF(@WEB_ModifyDomain),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN) then begin
    txt:=app.FetchAppTextFull(ses,'$tb_delete_domain');
    domaingrid.AddButton.DescribeManualType('tb_delete_domain',CWSF(@WEB_DeleteDomain),'',txt.Getshort,txt.GetHint,true);
    txt.Finalize;
  end;

  sec.AddSection.Describe(CWSF(@WEB_ContentUsers),app.FetchAppTextShort(ses,'$users_tab'),2);

  dc_groupin  := ses.FetchDerivedCollection('DOMAINMOD_GROUPIN_GRID');

  domaingrid.AddFilterEvent(dc_groupin.getDescriptionStoreId(),'uids');

  sec.AddSection.Describe(CWSF(@WEB_ContentGroups),app.FetchAppTextShort(ses,'$groups_tab'),1);

  main    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(domaingrid,sec,nil,nil,nil,true);
  result  := TFRE_DB_LAYOUT_DESC.create.Describe.SetAutoSizedLayout(nil,main,nil,TFRE_DB_HTML_DESC.create.Describe('<b>'+app.FetchAppTextShort(ses,'$domain_info')+'</b>'));
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ContentUsers(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userin     : IFRE_DB_DERIVED_COLLECTION;
  useringrid    : TFRE_DB_VIEW_LIST_DESC;
begin
  CheckClassVisibility(ses);

  dc_userin   := ses.FetchDerivedCollection('DOMAINMOD_USERIN_GRID');
  useringrid  := dc_userin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  Result  := useringrid;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ContentGroups(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_groupin    : IFRE_DB_DERIVED_COLLECTION;
  groupingrid   : TFRE_DB_VIEW_LIST_DESC;

begin
  CheckClassVisibility(ses);

  dc_groupin  := ses.FetchDerivedCollection('DOMAINMOD_GROUPIN_GRID');
  groupingrid := dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  Result  := groupingrid;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_AddDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_DIALOG_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_DOMAIN)) then   //class right without domain
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_DOMAIN',scheme);
  res:=TFRE_DB_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'$add_domain_diag_cap'),600,0,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.AddButton.Describe(app.FetchAppTextShort(ses,'$button_save'),CWSF(@WEB_CreateDomain),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_CreateDomain(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Create Domain','Please implement me!',fdbmt_info);
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_ModifyDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_DIALOG_DESC;
  domain: IFRE_DB_DOMAIN;
  sf     : TFRE_DB_SERVER_FUNC_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_DOMAIN',scheme);
  res:=TFRE_DB_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'$modify_domain_diag_cap'),600,0,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);

  CheckDbResult(conn.sys.FetchDomainById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsString),domain),'ModifyDomain');
  if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'$modify_domain_diag_cap'),app.FetchAppTextShort(ses,'$modify_domain_diag_no_system_domain_msg'),fdbmt_warning,nil));
  end;

  sf:=CWSF(@WEB_SaveDomain);
  sf.AddParam.Describe('selected',input.Field('selected').AsString);
  res.AddButton.Describe(app.FetchAppTextShort(ses,'$button_save'),sf,fdbbt_submit);
  res.FillWithObjectValues(domain.Implementor_HC as IFRE_DB_Object,ses);
  Result:=res;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DeleteDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
  domain : IFRE_DB_DOMAIN;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.Field('selected').ValueCount=1 then begin
    sf:=CWSF(@WEB_DeleteDomainConfirmed);
    sf.AddParam.Describe('selected',input.Field('selected').AsString);
    CheckDbResult(conn.sys.FetchDomainById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsString),domain),'DeleteDomain');
    if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
      exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'$delete_domain_diag_cap'),app.FetchAppTextShort(ses,'$delete_domain_diag_no_system_domain_msg'),fdbmt_warning,nil));
    end;
    msg := domain.Domainname(false);
    cap:=app.FetchAppTextShort(ses,'$delete_domain_diag_cap');
    msg:=StringReplace(app.FetchAppTextShort(ses,'$delete_domain_diag_msg'),'%domain_str%',msg,[rfReplaceAll]);

    Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
  end else begin
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_delete_single_select'));
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DeleteDomainConfirmed(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res    : TFRE_DB_Errortype;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.field('confirmed').AsBoolean then begin
    res := conn.sys.DeleteDomainById(GFRE_BT.HexString_2_GUID(input.Field('selected').Asstring));
    if res=edb_OK then
      exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
    else
      exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'$domain_delete_error_cap'),StringReplace(app.FetchAppTextShort(ses,'$domain_delete_error_msg'),'%error_msg%',CFRE_DB_Errortype[res],[rfReplaceAll]),fdbmt_error,nil));
  end else begin
    result := GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_SaveDomain(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var dbo              : IFRE_DB_Object;
    data             : IFRE_DB_Object;
    res              : TFRE_DB_Errortype;
    dbo_uid          : TGUID;
    dn               : TFRE_DB_NameType;
    txt              : TFRE_DB_String;
    txt_s            : TFRE_DB_String;

begin
 if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN)) then
   raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

 data    := input.Field('DATA').asobject;

 dn      := data.Field('objname').AsString;
 if data.FieldExists('desc') then begin
   txt     := data.FieldPath('desc.txt').AsString;
   txt_s   := data.FieldPath('desc.txt_s').AsString;
 end;

 dbo_uid := GFRE_BT.HexString_2_GUID(input.Field('selected').Asstring);

 res := conn.sys.ModifyDomainById(dbo_uid,dn,txt,txt_s);
 if res=edb_OK then
   exit(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe())
 else
   exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'$domain_modify_error_cap'),StringReplace(app.FetchAppTextShort(ses,'$domain_delete_error_msg'),'%error_msg%',CFRE_DB_Errortype[res],[rfReplaceAll]),fdbmt_error,nil));
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DGMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res   : TFRE_DB_MENU_DESC;
  func  : TFRE_DB_SERVER_FUNC_DESC;
  domain: IFRE_DB_DOMAIN;
begin
  if input.Field('selected').ValueCount=1 then begin
    CheckDbResult(conn.sys.FetchDomainById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsString),domain),'TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification');
    if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
      Result:=GFRE_DB_NIL_DESC;
    end else begin
      res:=TFRE_DB_MENU_DESC.create.Describe;
      if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
        func:=CWSF(@WEB_ModifyDomain);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(app.FetchAppTextShort(ses,'$cm_modify_domain'),'',func);
      end;

      if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN) then begin
        func:=CWSF(@WEB_DeleteDomain);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(app.FetchAppTextShort(ses,'$cm_delete_domain'),'',func);
      end;

      Result:=res;
    end;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  domain: IFRE_DB_DOMAIN;
begin
  if input.Field('selected').ValueCount=1 then begin
    CheckDbResult(conn.sys.FetchDomainById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsString),domain),'TFRE_COMMON_DOMAIN_MOD.WEB_DGNotification');
    if domain.Domainname(true)=CFRE_DB_SYS_DOMAIN_NAME then begin
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
      ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_domain',true));
    end else begin
      if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_DOMAIN) then begin
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',false));
      end else begin
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
      end;
      if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_DOMAIN) then begin
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_domain',false));
      end else begin
        ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_domain',true));
      end;
    end;
  end else begin
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_modify_domain',true));
    ses.SendServerClientRequest(TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('tb_delete_domain',true));
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

  CheckDbResult(conn.sys.FetchRoleById(GFRE_BT.HexString_2_GUID(roleUid),role),'_addremoverole');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if addrole then begin
      if conn.sys.AddRolesToGroup(group.ObjectName,group.DomainID,TFRE_DB_StringArray.create(role.ObjectName))<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_add_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end else begin
      if conn.sys.RemoveRolesFromGroup(group.ObjectName,group.DomainID,TFRE_DB_StringArray.create(role.ObjectName),false)<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_remove_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

class procedure TFRE_COMMON_ROLE_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_ROLE_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$role_description');
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

begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    app  := GetEmbeddingApp;
    conn := session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Role);
    with tr_Role do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'ROLE_DESC',app.FetchAppTextShort(session,'$gc_role'));
      AddMatchingReferencedField('DOMAINIDLINK','objname','domain',app.FetchAppTextShort(session,'$gc_domainname'));
   end;
    role_Grid := session.NewDerivedCollection('ROLEMOD_ROLE_GRID');
    with role_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetDeriveTransformation(tr_role);
      AddRightFilterForEntryAndUser('RF','VIEWDOM','DOMAINIDLINK');
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',nil,nil,CWSF(@WEB_RoleNotification));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
    end;
    userin_Grid := session.NewDerivedCollection('ROLEMOD_USERIN_GRID');
    with userin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_GROUP<ROLEIDS','TFRE_DB_USER<USERGROUPIDS']);
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_UhasR'));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserOut);
    with tr_UserOut do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
    end;
    userout_Grid := session.NewDerivedCollection('ROLEMOD_USEROUT_GRID');
    with userout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['ROLEIDS<USERGROUPIDS'],false,true);
      SetDeriveTransformation(tr_UserOut);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_UnotR'));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupIn);
    with tr_groupIn do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
    end;
    groupin_Grid := session.NewDerivedCollection('ROLEMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_GROUP<ROLEIDS']);
      SetDeriveTransformation(tr_groupIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_GhasR'),nil,'',CWSF(@WEB_GIRMenu),nil,nil,nil,CWSF(@WEB_AddToRole));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GroupOut);
    with tr_GroupOut do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
    end;
    groupout_Grid := session.NewDerivedCollection('ROLEMOD_GROUPOUT_GRID');
    with groupout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP<ROLEIDS'],false,true);
      SetDeriveTransformation(tr_groupOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_GnotR'),nil,'',CWSF(@WEB_GORMenu),nil,nil,nil,CWSF(@WEB_RemoveFromRole));
    end;
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  main    : TFRE_DB_CONTENT_DESC;
  sec     : TFRE_DB_SUBSECTIONS_DESC;
  rolegrid    : TFRE_DB_VIEW_LIST_DESC;
  dc_role     : IFRE_DB_DERIVED_COLLECTION;
  dc_userin   : IFRE_DB_DERIVED_COLLECTION;
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  dc_groupin  : IFRE_DB_DERIVED_COLLECTION;
  dc_groupout : IFRE_DB_DERIVED_COLLECTION;
begin
  CheckClassVisibility(ses);

  dc_role     := ses.FetchDerivedCollection('ROLEMOD_ROLE_GRID');
  rolegrid    := dc_role.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  sec     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
  if conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE) then begin

    dc_userin   := ses.FetchDerivedCollection('ROLEMOD_USERIN_GRID');
    dc_userout  := ses.FetchDerivedCollection('ROLEMOD_USEROUT_GRID');

    rolegrid.AddFilterEvent(dc_userin.getDescriptionStoreId(),'uids');
    rolegrid.AddFilterEvent(dc_userout.getDescriptionStoreId(),'uids');

    sec.AddSection.Describe(CWSF(@WEB_ContentUsers),app.FetchAppTextShort(ses,'$users_tab'),2);
  end;
  dc_groupin  := ses.FetchDerivedCollection('ROLEMOD_GROUPIN_GRID');
  dc_groupout := ses.FetchDerivedCollection('ROLEMOD_GROUPOUT_GRID');

  rolegrid.AddFilterEvent(dc_groupin.getDescriptionStoreId(),'uids');
  rolegrid.AddFilterEvent(dc_groupout.getDescriptionStoreId(),'uids');

  sec.AddSection.Describe(CWSF(@WEB_ContentGroups),app.FetchAppTextShort(ses,'$groups_tab'),1);

  main    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(rolegrid,sec,nil,nil,nil,true);
  result  := TFRE_DB_LAYOUT_DESC.create.Describe.SetAutoSizedLayout(nil,main,nil,TFRE_DB_HTML_DESC.create.Describe('<b>'+app.FetchAppTextShort(ses,'$roles_info')+'</b>'));
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
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

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
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  dc_groupin  := ses.FetchDerivedCollection('ROLEMOD_GROUPIN_GRID');
  groupingrid := dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_groupout := ses.FetchDerivedCollection('ROLEMOD_GROUPOUT_GRID');
  groupoutgrid:= dc_groupout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then begin
    groupoutgrid.setDropGrid(groupingrid);
    groupingrid.setDropGrid(groupoutgrid);
  end;
  group   := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,groupoutgrid,nil,groupingrid,nil,true,-1,1,-1,1);
  Result  := group;
end;

function TFRE_COMMON_ROLE_MOD.WEB_RoleNotification(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  dc_groupout : IFRE_DB_DERIVED_COLLECTION;
  sel_guid    : TGUID;
  role        : IFRE_DB_Object;

begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE)) then raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    CheckDbResult(conn.Fetch(sel_guid,role),'role fetch failed)');
    sel_guid := role.Field('DOMAINIDLINK').AsGUID;
    role.Finalize;
    dc_groupout := ses.FetchDerivedCollection('ROLEMOD_GROUPOUT_GRID');
    dc_groupout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
    dc_userout := ses.FetchDerivedCollection('ROLEMOD_USEROUT_GRID');
    dc_userout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
  end;

  result := GFRE_DB_NIL_DESC;
end;


function TFRE_COMMON_ROLE_MOD.WEB_AddToRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));
  Result:=_addremoverole(input,ses,app,conn,true);
end;

function TFRE_COMMON_ROLE_MOD.WEB_GIRMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_role');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_roles');
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
begin
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_role');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_roles');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_ROLE_MOD.WEB_RemoveFromRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP)) then raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));
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
    CheckDbResult(conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(groups[i]),group),'_getGroupsString');
    if i>0 then begin
      if i=(Length(groups) - 1) then begin
        res:=res + ' ' +GetEmbeddingApp.FetchAppTextShort(ses,'$and')+' ';
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

  conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(groupUid),group);
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchRoleById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),role)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_role_msg'),'%role%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if addrole then begin
      if conn.sys.AddRolesToGroup(group.ObjectName,group.DomainID,TFRE_DB_StringArray.Create(role.ObjectName))<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_add_role_msg'),'%group%',group.ObjectName+'@'+group.getDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end else begin
      if conn.sys.RemoveRolesFromGroup(group.ObjectName,group.DomainID,TFRE_DB_StringArray.Create(role.ObjectName),false)<>edb_OK then
        raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_remove_role_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%role%',role.ObjectName+'@'+role.GetDomain(conn),[rfReplaceAll]));
    end;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

class procedure TFRE_COMMON_GROUP_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_GROUP_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$group_description')
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

    tr_Domains    : IFRE_DB_SIMPLE_TRANSFORM;
    domain_cb     : IFRE_DB_DERIVED_COLLECTION;

    app           : TFRE_DB_APPLICATION;
    conn          : IFRE_DB_CONNECTION;

begin
  inherited MySessionInitializeModule(session);
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
      AddMatchingReferencedField('DOMAINIDLINK','objname','domain',app.FetchAppTextShort(session,'$gc_domainname'));
    end;
    group_Grid := session.NewDerivedCollection('GROUPMOD_GROUP_GRID');
    with group_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetDeriveTransformation(tr_Grid);
      AddRightFilterForEntryAndUser('RF','VIEWDOM','DOMAINIDLINK');
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_GGMenu),nil,CWSF(@WEB_GGNotification));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Domains);
    with tr_Domains do begin
      AddOneToOnescheme    ('objname','_simpleformat','egal',dt_string);
    end;

    domain_cb := session.NewDerivedCollection('GROUPMOD_DOMAINS');
    with domain_cb do begin
      SetDeriveParent           (session.GetDBConnection.AdmGetDomainCollection);
      AddRightFilterForEntryAndUser('RF','VIEWDOM');
      SetDeriveTransformation   (tr_Domains);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserIn);
    with tr_UserIn do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
    end;
    userin_Grid := session.NewDerivedCollection('GROUPMOD_USERIN_GRID');
    with userin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_USER<USERGROUPIDS']);
      SetDeriveTransformation(tr_UserIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_UinG'),nil,'',CWSF(@WEB_UIGMenu),nil,nil,nil,CWSF(@WEB_AddToUser));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_UserOut);
    with tr_UserOut do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
    end;
    userout_Grid := session.NewDerivedCollection('GROUPMOD_USEROUT_GRID');
    with userout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetUserCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_USER<USERGROUPIDS'],false,true); // UserGroupIDS
      SetDeriveTransformation(tr_UserOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_UnotG'),nil,'',CWSF(@WEB_UOGMenu),nil,nil,nil,CWSF(@WEB_RemoveFromUser));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleIn);
    with tr_RoleIn do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'ROLE_DESC',app.FetchAppTextShort(session,'$gc_role'));
    end;
    rolein_Grid := session.NewDerivedCollection('GROUPMOD_ROLEIN_GRID');
    with rolein_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_ROLE>ROLEIDS']); //TODO: LOOK AT THIS (was ROLEIDS only)
      SetDeriveTransformation(tr_RoleIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_GhasR'),nil,'',CWSF(@WEB_RIGMenu),nil,nil,nil,CWSF(@WEB_AddToRole));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleOut);
    with tr_RoleOut do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'ROLE_DESC',app.FetchAppTextShort(session,'$gc_role'));
    end;
    roleout_Grid := session.NewDerivedCollection('GROUPMOD_ROLEOUT_GRID');
    with roleout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_ROLE>ROLEIDS'],true,true);
      SetDeriveTransformation(tr_RoleOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_GnotR'),nil,'',CWSF(@WEB_ROGMenu),nil,nil,nil,CWSF(@WEB_RemoveFromRole));
    end;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  main    : TFRE_DB_CONTENT_DESC;
  sec     : TFRE_DB_SUBSECTIONS_DESC;
  groupgrid   : TFRE_DB_VIEW_LIST_DESC;
  dc_group    : IFRE_DB_DERIVED_COLLECTION;
  dc_userin   : IFRE_DB_DERIVED_COLLECTION;
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  dc_rolein   : IFRE_DB_DERIVED_COLLECTION;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  txt         : IFRE_DB_TEXT;
begin
  CheckClassVisibility(ses);

  dc_group := ses.FetchDerivedCollection('GROUPMOD_GROUP_GRID');
  groupgrid := dc_group.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) then begin
    txt:=app.FetchAppTextFull(ses,'$tb_add_group');
    groupgrid.AddButton.Describe(CWSF(@WEB_AddGroup),'',txt.Getshort,txt.GetHint);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    txt:=app.FetchAppTextFull(ses,'$tb_modify_group');
    groupgrid.AddButton.Describe(CWSF(@WEB_ModifyGroup),'',txt.Getshort,txt.GetHint,fdgbd_single);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then begin
    txt:=app.FetchAppTextFull(ses,'$tb_delete_group');
    groupgrid.AddButton.Describe(CWSF(@WEB_DeleteGroup),'',txt.Getshort,txt.GetHint,fdgbd_multi);
    txt.Finalize;
  end;
  if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then begin
    dc_userin   := ses.FetchDerivedCollection('GROUPMOD_USERIN_GRID');
    dc_userout  := ses.FetchDerivedCollection('GROUPMOD_USEROUT_GRID');
    groupgrid.AddFilterEvent(dc_userin.getDescriptionStoreId(),'uids');
    groupgrid.AddFilterEvent(dc_userout.getDescriptionStoreId(),'uids');
  end;

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    dc_rolein   := ses.FetchDerivedCollection('GROUPMOD_ROLEIN_GRID');
    dc_roleout  := ses.FetchDerivedCollection('GROUPMOD_ROLEOUT_GRID');
    groupgrid.AddFilterEvent(dc_rolein.getDescriptionStoreId(),'uids');
    groupgrid.AddFilterEvent(dc_roleout.getDescriptionStoreId(),'uids');
  end;

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    sec     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
    sec.AddSection.Describe(CWSF(@WEB_ContentUsers),app.FetchAppTextShort(ses,'$users_tab'),1);
    sec.AddSection.Describe(CWSF(@WEB_ContentRoles),app.FetchAppTextShort(ses,'$roles_tab'),2);
    main    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(groupgrid,sec,nil,nil,nil,true);
  end else begin
    main:=groupgrid;
  end;
  result  := TFRE_DB_LAYOUT_DESC.create.Describe.SetAutoSizedLayout(nil,main,nil,TFRE_DB_HTML_DESC.create.Describe('<b>'+app.FetchAppTextShort(ses,'$groups_info')+'</b>'));
end;

function TFRE_COMMON_GROUP_MOD.WEB_ContentUsers(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_userin   : IFRE_DB_DERIVED_COLLECTION;
  useringrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  useroutgrid : TFRE_DB_VIEW_LIST_DESC;
  user        : TFRE_DB_LAYOUT_DESC;
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  dc_userin   := ses.FetchDerivedCollection('GROUPMOD_USERIN_GRID');
  useringrid  := dc_userin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_userout  := ses.FetchDerivedCollection('GROUPMOD_USEROUT_GRID');
  useroutgrid := dc_userout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  useroutgrid.setDropGrid(useringrid);
  useringrid.setDropGrid(useroutgrid);

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
begin
  if not (conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_ROLE)) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  dc_rolein   := ses.FetchDerivedCollection('GROUPMOD_ROLEIN_GRID');
  roleingrid  := dc_rolein.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_roleout  := ses.FetchDerivedCollection('GROUPMOD_ROLEOUT_GRID');
  roleoutgrid := dc_roleout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then begin
    roleoutgrid.setDropGrid(roleingrid);
    roleingrid.setDropGrid(roleoutgrid);
  end;

  role    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,roleoutgrid,nil,roleingrid,nil,true,-1,1,-1,1);
  Result  := role;
end;

function TFRE_COMMON_GROUP_MOD.WEB_AddGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_DIALOG_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_GROUP',scheme);
  res:=TFRE_DB_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'$add_group_diag_cap'),600,0,true,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.AddSchemeFormGroup(scheme.GetInputGroup('domain'),ses);
  res.AddButton.Describe(app.FetchAppTextShort(ses,'$button_save'),CSCF('TFRE_DB_GROUP','NewGroupOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD.WEB_ModifyGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  scheme: IFRE_DB_SchemeObject;
  res   : TFRE_DB_DIALOG_DESC;
  group : IFRE_DB_GROUP;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_GROUP',scheme);
  CheckDbResult(conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsString),group),'ModifyGroup');

  if Pos('$',group.ObjectName)=1 then begin
    exit(TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'$modify_group_diag_cap'),app.FetchAppTextShort(ses,'$modify_group_diag_no_system_group_msg'),fdbmt_warning,nil));
  end;


  res:=TFRE_DB_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'$modify_group_diag_cap'));
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.AddButton.Describe(app.FetchAppTextShort(ses,'$button_save'),CSFT('saveOperation',group.Implementor_HC as IFRE_DB_Object),fdbbt_submit);
  res.FillWithObjectValues(group.Implementor_HC as IFRE_DB_Object,ses);
  Result:=res;
end;

function TFRE_COMMON_GROUP_MOD.WEB_DeleteGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  sf:=CWSF(@WEB_DeleteGroupConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  msg:=_getGroupsString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
  if input.Field('selected').ValueCount>1 then begin
    cap:=app.FetchAppTextShort(ses,'$delete_groups_diag_cap');
    msg:=StringReplace(app.FetchAppTextShort(ses,'$delete_groups_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
  end else begin
    cap:=app.FetchAppTextShort(ses,'$delete_group_diag_cap');
    msg:=StringReplace(app.FetchAppTextShort(ses,'$delete_group_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
  end;
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
end;

function TFRE_COMMON_GROUP_MOD.WEB_DeleteGroupConfirmed(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  cap,msg: String;
  i      : NativeInt;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.field('confirmed').AsBoolean then begin
    msg:=_getGroupsString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
    if input.Field('selected').ValueCount>1 then begin
      cap:=app.FetchAppTextShort(ses,'$groups_deleted_diag_cap');
      msg:=StringReplace(app.FetchAppTextShort(ses,'$groups_deleted_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
    end else begin
      cap:=app.FetchAppTextShort(ses,'$group_deleted_diag_cap');
      msg:=StringReplace(app.FetchAppTextShort(ses,'$group_deleted_diag_msg'),'%group_str%',msg,[rfReplaceAll]);
    end;
    for i := 0 to input.Field('selected').ValueCount-1 do begin
      //CheckDbResult(conn.DeleteGroup(),'DeleteGroupConfirmed');
    end;
    //FIXXME: Do real delete and error handling.
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
begin
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) or conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then begin
    if input.Field('selected').ValueCount=1 then begin
      mtxt:=app.FetchAppTextShort(ses,'$cm_modify_group');
      dtxt:=app.FetchAppTextShort(ses,'$cm_delete_group');
    end else begin
      mtxt:=app.FetchAppTextShort(ses,'$cm_modify_group');
      dtxt:=app.FetchAppTextShort(ses,'$cm_delete_groups');
    end;
    res:=TFRE_DB_MENU_DESC.create.Describe;
    if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
      begin
        func:=CWSF(@WEB_ModifyGroup);
        func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
        res.AddEntry.Describe(mtxt,'',func,input.Field('selected').ValueCount>1);
      end;
    if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_GROUP) then
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
  dc_userout  : IFRE_DB_DERIVED_COLLECTION;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  sel_guid    : TGUID;
  group       : IFRE_DB_Object;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    sel_guid := input.Field('SELECTED').AsGUID;
    CheckDbResult(GetDBConnection(input).Fetch(sel_guid,group),'group fetch failed)');
    sel_guid := group.Field('DOMAINIDLINK').AsGUID;
    group.Finalize;
    dc_userout := ses.FetchDerivedCollection('GROUPMOD_USEROUT_GRID');
    dc_userout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
    dc_roleout := ses.FetchDerivedCollection('GROUPMOD_ROLEOUT_GRID');
    dc_roleout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
  end;
  result := GFRE_DB_NIL_DESC;
end;


function TFRE_COMMON_GROUP_MOD.WEB_UIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) and
    input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromUser);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_user');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_users');
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
  if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToUser);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_user');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_users');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RemoveFromUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user     : IFRE_DB_USER;
  group    : IFRE_DB_GROUP;
  i        : Integer;
  res      : TFRE_DB_Errortype;
  groupUid : TFRE_DB_String;
begin
  if not conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('uids_ref') then begin
    groupUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      groupUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to WEB_RemoveFromUser!');
    end;
  end;

  CheckDbResult(conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(groupUid),group),'RemoveFromUser');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchUserById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),user)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_user_msg'),'%user%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.RemoveUserGroups(user.login+'@'+user.getDomain(conn),TFRE_DB_StringArray.Create(group.ObjectName+'@'+group.getDomain(conn)))<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_remove_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
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
begin
  if not conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('uids_ref') then begin
    groupUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      groupUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on group has to be passed to WEB_AddToUser!');
    end;
  end;

  CheckDbResult(conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(groupUid),group),'AddToUser');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchUserById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),user)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_user_msg'),'%user%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.ModifyUserGroups(user.login+'@'+user.GetDomain(conn),TFRE_DB_StringArray.Create((group.ObjectName+'@'+group.GetDomain(conn))),true)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_add_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin

  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_role');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_group_from_roles');
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
  if conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToRole);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_role');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_group_to_roles');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_GROUP_MOD.WEB_RemoveFromRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));
  result := _addremoverole(input,ses,app,conn,false);
end;

function TFRE_COMMON_GROUP_MOD.WEB_AddToRole(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));
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
    CheckDbResult(conn.sys.FetchUserById(GFRE_BT.HexString_2_GUID(logins[i]),user),'_getUsersString');
    if i>0 then begin
      if i=(Length(logins) - 1) then begin
        res:=res + ' ' +GetEmbeddingapp.FetchAppTextShort(ses,'$and')+' ';
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

class procedure TFRE_COMMON_USER_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_USER_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$user_description')
end;

procedure TFRE_COMMON_USER_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var user_Grid     : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid       : IFRE_DB_SIMPLE_TRANSFORM;
    tr_Domains    : IFRE_DB_SIMPLE_TRANSFORM;

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

    domain_cb     : IFRE_DB_DERIVED_COLLECTION;

begin
  inherited;
  app  := GetEmbeddingApp;
  conn := session.GetDBConnection;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme    ('login','',app.FetchAppTextShort(session,'$gc_username'));
      AddOneToOnescheme    ('firstname','',app.FetchAppTextShort(session,'$gc_firstname'));
      AddOneToOnescheme    ('lastname','',app.FetchAppTextShort(session,'$gc_lastname'));
      AddMatchingReferencedField('DOMAINIDLINK','objname','domain',app.FetchAppTextShort(session,'$gc_domainname'));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Domains);
    with tr_Domains do begin
      AddOneToOnescheme    ('objname','_simpleformat','egal',dt_string);
    end;

    user_grid := session.NewDerivedCollection('USERMOD_USER_GRID');
    with user_grid do begin
      SetDeriveParent           (session.GetDBConnection.AdmGetUserCollection);
      SetDeriveTransformation   (tr_Grid);
      AddRightFilterForEntryAndUser('RF','VIEWDOM','DOMAINIDLINK');
      SetDisplayType            (cdt_Listview,[cdgf_ShowSearchbox],'',nil,'',CWSF(@WEB_UGMenu),nil,CWSF(@WEB_UserSelected));
    end;

    domain_cb := session.NewDerivedCollection('USERMOD_DOMAINS');
    with domain_cb do begin
      SetDeriveParent           (session.GetDBConnection.AdmGetDomainCollection);
      AddRightFilterForEntryAndUser('RF','EDITDOM');
      SetDeriveTransformation   (tr_Domains);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GridIn);
    with tr_GridIn do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
    end;

    groupin_Grid := session.NewDerivedCollection('USERMOD_GROUPIN_GRID');
    with groupin_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_GROUP>USERGROUPIDS']); // Gather all objects that the USERGROUPIDS Field points to
      SetDeriveTransformation(tr_GridIn);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_UinG'),nil,'',CWSF(@WEB_GIGMenu),nil,nil,nil,CWSF(@WEB_AddToGroup));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_GridOut);
    with tr_GridOut do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.Create('desc.txt'),'GROUP_DESC',app.FetchAppTextShort(session,'$gc_group'));
    end;

    groupout_Grid := session.NewDerivedCollection('USERMOD_GROUPOUT_GRID');
    with groupout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetGroupCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP>USERGROUPIDS'],true,true);
      SetDeriveTransformation(tr_GridOut);
      SetDisplayType(cdt_Listview,[cdgf_Multiselect],app.FetchAppTextShort(session,'$gcap_UnotG'),nil,'',CWSF(@WEB_GOGMenu),nil,nil,nil,CWSF(@WEB_RemoveFromGroup));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleIn);
    with tr_RoleIn do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.create('desc.txt'),'ROLE_DESC',app.FetchAppTextShort(session,'$gc_role'));
    end;

    rolein_Grid := session.NewDerivedCollection('USERMOD_ROLEIN_GRID');
    with rolein_Grid do begin
      SetReferentialLinkMode(['TFRE_DB_GROUP>USERGROUPIDS','TFRE_DB_ROLE>ROLEIDS']); // Gather all objects that the USERGROUPIDS and then ROLEIDS Field points to
      SetDeriveTransformation(tr_RoleIn);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_UhasR'));
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_RoleOut);
    with tr_RoleOut do begin
      AddCollectorscheme('%s',TFRE_DB_NameTypeArray.create('desc.txt'),'ROLE_DESC',app.FetchAppTextShort(session,'$gc_role'));
    end;
    roleout_Grid := session.NewDerivedCollection('USERMOD_ROLEOUT_GRID');
    with roleout_Grid do begin
      SetDeriveParent(session.GetDBConnection.AdmGetRoleCollection);
      SetUseDependencyAsRefLinkFilter(['TFRE_DB_GROUP>USERGROUPIDS','TFRE_DB_ROLE>ROLEIDS'],true,true);
      SetDeriveTransformation(tr_RoleOut);
      SetDisplayType(cdt_Listview,[],app.FetchAppTextShort(session,'$gcap_UnotR'));
    end;
  end;
end;


function TFRE_COMMON_USER_MOD.WEB_Content(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  main        : TFRE_DB_CONTENT_DESC;
  sec         : TFRE_DB_SUBSECTIONS_DESC;
  usergrid    : TFRE_DB_VIEW_LIST_DESC;
  dc_user     : IFRE_DB_DERIVED_COLLECTION;
  dc_groupin  : IFRE_DB_DERIVED_COLLECTION;
  dc_groupout : IFRE_DB_DERIVED_COLLECTION;
  dc_rolein   : IFRE_DB_DERIVED_COLLECTION;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  txt         : IFRE_DB_TEXT;
  user_count  : String;
begin
  CheckClassVisibility(ses);

  dc_user := ses.FetchDerivedCollection('USERMOD_USER_GRID');
  usergrid := dc_user.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) or conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) or conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then begin
    sec     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
    if conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) then begin
      txt:=app.FetchAppTextFull(ses,'$tb_add_user');
      usergrid.AddButton.Describe(CWSF(@WEB_AddUser),'',txt.Getshort,txt.GetHint);
      txt.Finalize;
    end;
    if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then begin
      txt:=app.FetchAppTextFull(ses,'$tb_delete_user');
      usergrid.AddButton.Describe(CWSF(@WEB_DeleteUser),'',txt.Getshort,txt.GetHint,fdgbd_multi);
      txt.Finalize;
    end;
    sec.AddSection.Describe(CWSF(@WEB_ContentInfo),app.FetchAppTextShort(ses,'$userinfo_tab'),1);
    if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then begin
      dc_groupin := ses.FetchDerivedCollection('USERMOD_GROUPIN_GRID');
      dc_groupout:= ses.FetchDerivedCollection('USERMOD_GROUPOUT_GRID');
      dc_rolein := ses.FetchDerivedCollection('USERMOD_ROLEIN_GRID');
      dc_roleout:= ses.FetchDerivedCollection('USERMOD_ROLEOUT_GRID');

      usergrid.AddFilterEvent(dc_groupin.getDescriptionStoreId(),'uids');
      usergrid.AddFilterEvent(dc_groupout.getDescriptionStoreId(),'uids');
      usergrid.AddFilterEvent(dc_rolein.getDescriptionStoreId(),'uids');
      usergrid.AddFilterEvent(dc_roleout.getDescriptionStoreId(),'uids');

      sec.AddSection.Describe(CWSF(@WEB_ContentGroups),app.FetchAppTextShort(ses,'$groups_tab'),2);
      sec.AddSection.Describe(CWSF(@WEB_ContentRoles),app.FetchAppTextShort(ses,'$roles_tab'),3);
    end;
    main    := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(usergrid,sec,nil,nil,nil,true);
  end else begin
    main    := usergrid;
  end;

  result  := TFRE_DB_LAYOUT_DESC.create.Describe.SetAutoSizedLayout(nil,main,nil,TFRE_DB_HTML_DESC.create.Describe('<b>'+app.FetchAppTextShort(ses,'$users_info')+'</b>'));
end;

function TFRE_COMMON_USER_MOD.WEB_UserSelected(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dc_groupout : IFRE_DB_DERIVED_COLLECTION;
  dc_roleout  : IFRE_DB_DERIVED_COLLECTION;
  user_guid   : TGUID;
  sel_guid    : TGUID;
  user        : IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('SELECTED') and (input.Field('SELECTED').ValueCount>0)  then begin
    ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString:=input.Field('SELECTED').AsString;
    user_guid := input.Field('SELECTED').AsGUID; // is user
    CheckDbResult(conn.Fetch(user_guid,user),StringReplace(app.FetchAppTextShort(ses,'$error_fetch_user_msg'),'%user%',GUIDToString(user_guid),[rfReplaceAll]));
    sel_guid := user.Field('DOMAINIDLINK').AsGUID;
    user.Finalize;
    dc_groupout := ses.FetchDerivedCollection('USERMOD_GROUPOUT_GRID');
    dc_groupout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
    dc_roleout  := ses.FetchDerivedCollection('USERMOD_ROLEOUT_GRID');
    dc_roleout.AddUIDFieldFilter('*domain*','DOMAINIDLINK',TFRE_DB_GUIDArray.Create(sel_guid),dbnf_EXACT,false);
  end else begin
     ses.GetSessionModuleData(ClassName).DeleteField('selectedUsers');
  end;
  if IsContentUpdateVisible(ses,'USER_INFO') then begin
    Result:=WEB_ContentInfo(input,ses,app,conn);
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentInfo(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user_editor : TFRE_DB_EDITOR_DESC;
  user_info   : TFRE_DB_LAYOUT_DESC;
  load_func   : TFRE_DB_SERVER_FUNC_DESC;
  save_func   : TFRE_DB_SERVER_FUNC_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  load_func   := CWSF(@WEB_NoteLoad);
  save_func   := CWSF(@WEB_NoteSave);

  if ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers')  then begin
    load_func.AddParam.Describe('linkid',ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString);
    save_func.AddParam.Describe('linkid',ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsString);
  end;

  user_editor := TFRE_DB_EDITOR_DESC.create.Describe(load_func,save_func,CWSF(@WEB_NoteStartEdit),CWSF(@WEB_NoteStopEdit));
  user_info   := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(nil,user_editor,nil,WEB_UserContent(input,ses,app,conn).Implementor_HC as TFRE_DB_CONTENT_DESC,nil,true,-1,1,-1,1);
  user_info.contentId := 'USER_INFO';
  Result      := user_info;
end;

function TFRE_COMMON_USER_MOD.WEB_ContentGroups(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  dc_groupin   : IFRE_DB_DERIVED_COLLECTION;
  groupingrid  : TFRE_DB_VIEW_LIST_DESC;
  dc_groupout  : IFRE_DB_DERIVED_COLLECTION;
  groupoutgrid : TFRE_DB_VIEW_LIST_DESC;
  group        : TFRE_DB_LAYOUT_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  dc_groupin := ses.FetchDerivedCollection('USERMOD_GROUPIN_GRID');
  groupingrid:= dc_groupin.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  dc_groupout:= ses.FetchDerivedCollection('USERMOD_GROUPOUT_GRID');
  groupoutgrid:= dc_groupout.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  groupoutgrid.setDropGrid(groupingrid);
  groupingrid.setDropGrid(groupoutgrid);

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
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

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
  res   : TFRE_DB_DIALOG_DESC;
  block : TFRE_DB_INPUT_BLOCK_DESC;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_STORE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
  res:=TFRE_DB_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'$add_user_diag_cap'),600);
  block:=res.AddBlock.Describe();
  block.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses,false,false,2);
  block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('domain'),ses,true,false);
  res.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
  res.AddButton.Describe(app.FetchAppTextShort(ses,'$button_save'),CSCF('TFRE_DB_USER','NewUserOperation'),fdbbt_submit);
  Result:=res;
end;

function TFRE_COMMON_USER_MOD.WEB_DeleteUser(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sf     : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg: String;
begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  sf:=CWSF(@WEB_DeleteUserConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  msg:=_getUsersString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
  if input.Field('selected').ValueCount>1 then begin
    cap:=app.FetchAppTextShort(ses,'$delete_users_diag_cap');
    msg:=StringReplace(app.FetchAppTextShort(ses,'$delete_users_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
  end else begin
    cap:=app.FetchAppTextShort(ses,'$delete_user_diag_cap');
    msg:=StringReplace(app.FetchAppTextShort(ses,'$delete_user_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
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
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.field('confirmed').AsBoolean then begin
    msg:=_getUsersString(input.Field('selected').AsStringArr,GetDBConnection(input),ses);
    if input.Field('selected').ValueCount>1 then begin
      cap:=app.FetchAppTextShort(ses,'$users_deleted_diag_cap');
      msg:=StringReplace(app.FetchAppTextShort(ses,'$users_deleted_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
    end else begin
      cap:=app.FetchAppTextShort(ses,'$user_deleted_diag_cap');
      msg:=StringReplace(app.FetchAppTextShort(ses,'$user_deleted_diag_msg'),'%user_str%',msg,[rfReplaceAll]);
    end;
    for i:=0 to input.Field('selected').ValueCount-1  do
      begin
        if conn.sys.DeleteUserById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringItem[i]))<>edb_OK then
          raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_delete_user_msg'),'%user%',input.Field('selected').AsStringItem[i],[rfReplaceAll]));
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
begin

  if conn.sys.CheckClassRight4AnyDomain(sr_DELETE,TFRE_DB_USER) then begin
    if input.Field('selected').ValueCount=1 then begin
      dtxt := app.FetchAppTextShort(ses,'$cm_delete_user');
    end else begin
      dtxt := app.FetchAppTextShort(ses,'$cm_delete_users');
    end;
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_DeleteUser);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    res.AddEntry.Describe(dtxt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_GIGMenu(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res       : TFRE_DB_MENU_DESC;
  func      : TFRE_DB_SERVER_FUNC_DESC;
  txt       : TFRE_DB_String;
begin
  if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_RemoveFromGroup);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_user_from_group');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_remove_user_from_groups');
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
  if conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) and
     input.FieldPathExists('dependency.uids_ref.filtervalues') and
     (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    func:=CWSF(@WEB_AddToGroup);
    func.AddParam.Describe('uids_ref',input.FieldPath('dependency.uids_ref.filtervalues').AsString);
    func.AddParam.Describe('selected',input.Field('selected').AsStringArr);
    if input.Field('selected').ValueCount=1 then begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_user_to_group');
    end else begin
      txt:=app.FetchAppTextShort(ses,'$cm_add_user_to_groups');
    end;
    res.AddEntry.Describe(txt,'',func);
    Result:=res;
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_COMMON_USER_MOD.WEB_RemoveFromGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user   : IFRE_DB_USER;
  group  : IFRE_DB_GROUP;
  i      : Integer;
  userUid: String;
begin
  if not conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('uids_ref') then begin
    userUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      userUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on user has to be passed to WEB_RemoveFromGroup!');
    end;
  end;
  CheckDbResult(conn.sys.FetchUserById(GFRE_BT.HexString_2_GUID(userUid),user),'RemoveFromGroup');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.RemoveUserGroups(user.login+'@'+user.getDomain(conn),TFRE_DB_StringArray.Create(group.ObjectName+'@'+group.GetDomain(conn)))<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_remove_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_USER_MOD.WEB_AddToGroup(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  user   : IFRE_DB_USER;
  group  : IFRE_DB_GROUP;
  i      : Integer;
  userUid: String;
begin
  if not conn.sys.CheckClassRight4AnyDomain('changeuser',TFRE_DB_GROUP) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if input.FieldExists('uids_ref') then begin
    userUid:=input.Field('uids_ref').AsString;
  end else begin
    if input.FieldPathExists('dependency.uids_ref.filtervalues') and (input.FieldPath('dependency.uids_ref.filtervalues').ValueCount=1) then begin
      userUid:=input.FieldPath('dependency.uids_ref.filtervalues').AsString;
    end else begin
      raise EFRE_DB_Exception.Create('Exactly on user has to be passed to WEB_AddToGroup!');
    end;
  end;
  CheckDbResult(conn.sys.FetchUserById(GFRE_BT.HexString_2_GUID(userUid),user),'AddToGroup');
  for i := 0 to input.Field('selected').ValueCount - 1 do begin
    if conn.sys.FetchGroupById(GFRE_BT.HexString_2_GUID(input.Field('selected').AsStringArr[i]),group)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(app.FetchAppTextShort(ses,'$error_fetch_group_msg'),'%group%',input.Field('selected').AsStringArr[i],[rfReplaceAll]));
    if conn.sys.ModifyUserGroups(user.login+'@'+user.getDomain(conn),TFRE_DB_StringArray.Create(group.ObjectName+'@'+group.GetDomain(conn)),true)<>edb_OK then
      raise EFRE_DB_Exception.Create(StringReplace(StringReplace(app.FetchAppTextShort(ses,'$error_add_group_msg'),'%group%',group.ObjectName+'@'+group.GetDomain(conn),[rfReplaceAll]),'%user%',user.login+'@'+user.getdomain(conn),[rfReplaceAll]));
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_COMMON_USER_MOD.WEB_UserContent(const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  html          : TFRE_DB_HTML_DESC;
  panel         : TFRE_DB_FORM_PANEL_DESC;
  scheme        : IFRE_DB_SchemeObject;
  dc            : IFRE_DB_DERIVED_COLLECTION;
  block         : TFRE_DB_INPUT_BLOCK_DESC;
  user          : IFRE_DB_USER;
  sel_guid      : TGUID;

begin
  if not conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_DB_USER) then
    raise EFRE_DB_Exception.Create(app.FetchAppTextShort(ses,'$error_no_access'));

  if ses.GetSessionModuleData(ClassName).FieldExists('selectedUsers')  then begin
    sel_guid := ses.GetSessionModuleData(ClassName).Field('selectedUsers').AsGUID;
    CheckDbResult(conn.sys.FetchUserById(sel_guid,user),'UserContent');
    GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'$user_content_header'));
    block:=panel.AddBlock.Describe();
    block.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses,false,false,2);
    block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,true,false);
    panel.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
    panel.FillWithObjectValues(user.Implementor_HC as IFRE_DB_Object,ses);
    if conn.sys.CheckClassRight4Domain(sr_FETCH,TFRE_DB_USER,user.GetDomain(conn)) then begin
      panel.AddButton.Describe('Save',CSFT('saveOperation',user.Implementor_HC as IFRE_DB_Object),fdbbt_submit);
    end;
    Result:=panel;
  end else begin
    panel :=TFRE_DB_FORM_PANEL_DESC.Create.Describe(app.FetchAppTextShort(ses,'$user_content_header'));
    Result:=panel;
  end;
end;

{ TFRE_COMMON_ACCESSCONTROL_APP }

procedure TFRE_COMMON_ACCESSCONTROL_APP.SetupApplicationStructure;
begin
  inherited SetupApplicationStructure;
  AddApplicationModule(TFRE_COMMON_DOMAIN_MOD.create);
  AddApplicationModule(TFRE_COMMON_USER_MOD.create);
  AddApplicationModule(TFRE_COMMON_GROUP_MOD.create);
  AddApplicationModule(TFRE_COMMON_ROLE_MOD.create);
end;


procedure TFRE_COMMON_ACCESSCONTROL_APP._UpdateSitemap( const session: TFRE_DB_UserSession); //MEMFIX?
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status',FetchAppTextShort(session,'$sitemap_main'),'images_apps/accesscontrol/monitor_white.svg','',0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_ACCESSCONTROL_APP));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Roles',FetchAppTextShort(session,'$sitemap_roles'),'images_apps/accesscontrol/notebook_white.svg',TFRE_COMMON_ROLE_MOD.ClassName,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_ROLE_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Groups',FetchAppTextShort(session,'$sitemap_groups'),'images_apps/accesscontrol/group_white.svg',TFRE_COMMON_GROUP_MOD.Classname,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_GROUP_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/User',FetchAppTextShort(session,'$sitemap_users'),'images_apps/accesscontrol/user_white.svg',TFRE_COMMON_USER_MOD.Classname,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_USER_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Status/Domains',FetchAppTextShort(session,'$sitemap_domains'),'images_apps/accesscontrol/domain_white.svg',TFRE_COMMON_DOMAIN_MOD.ClassName,0,conn.sys.CheckClassRight4AnyDomain(sr_FETCH,TFRE_COMMON_DOMAIN_MOD));
  FREDB_SiteMap_RadialAutoposition(SiteMapData,225);
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

class procedure TFRE_COMMON_ACCESSCONTROL_APP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited;

  newVersionId:='1.0';

  if (currentVersionId='') then begin
    CreateAppText(conn,'$caption','Access Control','Access Control','Access Control');
    CreateAppText(conn,'$user_description','Users','Users','Users');
    CreateAppText(conn,'$group_description','Groups','Groups','Groups');
    CreateAppText(conn,'$role_description','Roles','Roles','Roles');
    CreateAppText(conn,'$domain_description','Domains','Domains','Domains');

    CreateAppText(conn,'$sitemap_main','Access Control','','Access Control');
    CreateAppText(conn,'$sitemap_users','Users','','Users');
    CreateAppText(conn,'$sitemap_groups','Groups','','Groups');
    CreateAppText(conn,'$sitemap_roles','Roles','','Roles');
    CreateAppText(conn,'$sitemap_domains','Domains','','Domains');

    CreateAppText(conn,'$error_delete_single_select','Exactly one object has to be selected for deletion.');

    CreateAppText(conn,'$gc_username','Username');
    CreateAppText(conn,'$gc_firstname','Firstname');
    CreateAppText(conn,'$gc_lastname','Lastname');
    CreateAppText(conn,'$gc_group','Group');
    CreateAppText(conn,'$gc_role','Role');
    CreateAppText(conn,'$gcap_UinG','User is in Group');
    CreateAppText(conn,'$gcap_UnotG','User is not in Group');
    CreateAppText(conn,'$gcap_UhasR','User has Role');
    CreateAppText(conn,'$gcap_UnotR','User has not Role');
    CreateAppText(conn,'$gcap_GhasR','Group has Role');
    CreateAppText(conn,'$gcap_GnotR','Group has not Role');

    CreateAppText(conn,'$roles_tab','Roles');
    CreateAppText(conn,'$groups_tab','Groups');
    CreateAppText(conn,'$users_tab','Users');
    CreateAppText(conn,'$note_tab','Note');
    CreateAppText(conn,'$userinfo_tab','User Properties');
    CreateAppText(conn,'$user_content_header','Informations about the selected user.');

    CreateAppText(conn,'$tb_delete_user','Delete','','Delete User');
    CreateAppText(conn,'$tb_add_user','Add','','Add User');

    CreateAppText(conn,'$cm_delete_user','Delete','','Delete User');
    CreateAppText(conn,'$cm_delete_users','Delete','','Delete Users');
    CreateAppText(conn,'$cm_remove_user_from_groups','Remove user from groups');
    CreateAppText(conn,'$cm_add_user_to_groups','Add user to groups');
    CreateAppText(conn,'$cm_remove_user_from_group','Remove user from group');
    CreateAppText(conn,'$cm_add_user_to_group','Add user to group');

    CreateAppText(conn,'$add_user_diag_cap','Add new user');
    CreateAppText(conn,'$delete_user_diag_cap','Confirm: Delete user');
    CreateAppText(conn,'$delete_users_diag_cap','Confirm: Delete multiple users');
    CreateAppText(conn,'$delete_user_diag_msg','User %user_str% will be deleted permanently! Please confirm to continue.');
    CreateAppText(conn,'$delete_users_diag_msg','Users %user_str% will be deleted permanently! Please confirm to continue.');
    CreateAppText(conn,'$user_deleted_diag_cap','User deleted');
    CreateAppText(conn,'$users_deleted_diag_cap','Users deleted');
    CreateAppText(conn,'$user_deleted_diag_msg','User %user_str% successfully deleted.');
    CreateAppText(conn,'$users_deleted_diag_msg','Users %user_str% successfully deleted.');

    CreateAppText(conn,'$cm_modify_group','Modify','','Modify Group');
    CreateAppText(conn,'$cm_delete_group','Delete','','Delete Group');
    CreateAppText(conn,'$cm_delete_groups','Delete','','Delete Groups');
    CreateAppText(conn,'$tb_add_group','Add','','Add Group');
    CreateAppText(conn,'$tb_modify_group','Modify','','Modify Group');
    CreateAppText(conn,'$tb_delete_group','Delete','','Delete Group');
    CreateAppText(conn,'$cm_remove_group_from_users','Remove users from group');
    CreateAppText(conn,'$cm_add_group_to_users','Add users to group');
    CreateAppText(conn,'$cm_remove_group_from_user','Remove user from group');
    CreateAppText(conn,'$cm_add_group_to_user','Add user to group');
    CreateAppText(conn,'$cm_remove_group_from_roles','Remove roles from group');
    CreateAppText(conn,'$cm_add_group_to_roles','Add roles to group');
    CreateAppText(conn,'$cm_remove_group_from_role','Remove role from group');
    CreateAppText(conn,'$cm_add_group_to_role','Add role to group');

    CreateAppText(conn,'$group_user_in_diag_cap','Add User to Group');
    CreateAppText(conn,'$add_group_diag_cap','Add new group');
    CreateAppText(conn,'$modify_group_diag_cap','Modify group');
    CreateAppText(conn,'$modify_group_diag_no_system_group_msg','You can not modify a system group.');
    CreateAppText(conn,'$group_user_out_diag_cap','Remove User from Group');
    CreateAppText(conn,'$group_role_in_diag_cap','Add role to group');
    CreateAppText(conn,'$group_role_out_diag_cap','Remove role from group');
    CreateAppText(conn,'$role_group_in_diag_cap','Add group to role');
    CreateAppText(conn,'$role_group_out_diag_cap','Remove group from role');

    CreateAppText(conn,'$delete_group_diag_cap','Confirm: Delete group');
    CreateAppText(conn,'$delete_groups_diag_cap','Confirm: Delete multiple groups');
    CreateAppText(conn,'$delete_group_diag_msg','Group %group_str% will be deleted permanently! Please confirm to continue.');
    CreateAppText(conn,'$delete_groups_diag_msg','Groupss %group_str% will be deleted permanently! Please confirm to continue.');
    CreateAppText(conn,'$group_deleted_diag_cap','Group deleted');
    CreateAppText(conn,'$groups_deleted_diag_cap','Groups deleted');
    CreateAppText(conn,'$group_deleted_diag_msg','Group %group_str% successfully deleted.');
    CreateAppText(conn,'$groups_deleted_diag_msg','Groups %group_str% successfully deleted.');

    CreateAppText(conn,'$users_info','Overview of all users and assigned groups.');
    CreateAppText(conn,'$groups_info','Overview of all groups and members.');
    CreateAppText(conn,'$roles_info','Overview of all roles.');
    CreateAppText(conn,'$domain_info','Overview of domains.');

    CreateAppText(conn,'$gc_domainname','Domain');
    CreateAppText(conn,'$gc_domain','Domain');
    CreateAppText(conn,'$gc_domain_desc','Description');
    CreateAppText(conn,'$gcap_UinD','User belongs to Domain');
    CreateAppText(conn,'$gcap_UnotinD','User does not belong to Domain');
    CreateAppText(conn,'$gcap_GinD','Group belongs to Domain');
    CreateAppText(conn,'$gcap_GnotinD','Group does not belong to Domain');

    CreateAppText(conn,'$tb_modify_domain','Modify','','Modify Domain');
    CreateAppText(conn,'$tb_delete_domain','Delete','','Delete Domain');
    CreateAppText(conn,'$tb_add_domain','Add','','Add Domain');
    CreateAppText(conn,'$cm_modify_domain','Modify');
    CreateAppText(conn,'$cm_delete_domain','Delete');
    CreateAppText(conn,'$add_domain_diag_cap','Add new domain');
    CreateAppText(conn,'$modify_domain_diag_cap','Modify domain');
    CreateAppText(conn,'$modify_domain_diag_no_system_domain_msg','Editing of the System Domain is not possible.');
    CreateAppText(conn,'$delete_domain_diag_no_system_domain_msg','Deletion of the System domain is not possible.');
    CreateAppText(conn,'$delete_domain_diag_cap','Confirm: Delete domain');
    CreateAppText(conn,'$delete_domain_diag_msg','Domain %domain_str% will be deleted permanently! Please confirm to continue.');
    CreateAppText(conn,'$domain_group_in_diag_cap','Adding Group to a domain');
    CreateAppText(conn,'$domain_user_in_diag_cap','Adding User to a domain');

    CreateAppText(conn,'$domain_delete_error_cap','Error');
    CreateAppText(conn,'$domain_delete_error_msg','Delete failed %error_msg%');
    CreateAppText(conn,'$domain_modify_error_cap','Error');
    CreateAppText(conn,'$domain_modify_error_msg','Modify failed %error_msg%');

    CreateAppText(conn,'$error_fetch_group_msg','Could not fetch group with id %group%');
    CreateAppText(conn,'$error_fetch_role_msg','Could not fetch role with id %role%');
    CreateAppText(conn,'$error_fetch_user_msg','Could not fetch user with id %user%');
    CreateAppText(conn,'$error_delete_user_msg','Could not delete user with id %user%');
    CreateAppText(conn,'$error_add_role_msg','Could not add role %role% to group %group%');
    CreateAppText(conn,'$error_remove_role_msg','Could not remove role %role% from group %group%');
    CreateAppText(conn,'$error_add_group_msg','Could not add user %user% to group %group%');
    CreateAppText(conn,'$error_remove_group_msg','Could not remove user %user% from group %group%');

    //FIXXME - CHECK
    CreateAppText(conn,'$and','and'); //used as and within a string - need some kind of a template?
    CreateAppText(conn,'$error_no_access','Access denied'); //global text?
    CreateAppText(conn,'$button_save','Save'); //global text?

    currentVersionId:='1.0';
  end;
  if (currentVersionId='1.0') then begin
    //next update code
  end;
end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);
end;

class procedure TFRE_COMMON_ACCESSCONTROL_APP.RegisterSystemScheme( const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_DOMAIN_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_USER_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_GROUP_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_ROLE_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_ACCESSCONTROL_APP);
  GFRE_DBI.Initialize_Extension_Objects;
end;


end.
