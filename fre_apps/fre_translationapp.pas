unit fre_translationapp;
{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH

  Licence conditions
(§LIC_END)
}

{$codepage UTF8}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  FOS_TOOL_INTERFACES,
  FRE_DB_INTERFACE,
  FRE_DB_COMMON;

type

  { TFRE_COMMON_TRANSLATION_APP }

  TFRE_COMMON_TRANSLATION_APP=class(TFRE_DB_APPLICATION)
  private

    procedure       SetupApplicationStructure     ; override;
    procedure       _UpdateSitemap                (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize           (const session: TFRE_DB_UserSession);override;
    procedure       MySessionPromotion            (const session: TFRE_DB_UserSession); override;
  public
    class procedure RegisterSystemScheme          (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects              (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure InstallDBObjects4Domain       (const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID); override;
    class procedure InstallUserDBObjects          (const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_COMMON_TRANSLATION_MOD }

  TFRE_COMMON_TRANSLATION_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme                (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure             ; override;
    class procedure InstallDBObjects                    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  public
    procedure       MySessionInitializeModule           (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content                         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

procedure Register_DB_Extensions;

implementation

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_TRANSLATION_MOD);

  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_TRANSLATION_APP);
end;

{ TFRE_COMMON_TRANSLATION_MOD }

class procedure TFRE_COMMON_TRANSLATION_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_TRANSLATION_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('translation_description')
end;

class procedure TFRE_COMMON_TRANSLATION_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.1';
  if currentVersionId='' then begin
    currentVersionId := '0.1';

    CreateModuleText(conn,'grid_translation_cap','Translation');
    CreateModuleText(conn,'grid_translation_key','Key');

  end;
end;

procedure TFRE_COMMON_TRANSLATION_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  app           : TFRE_DB_APPLICATION;
  conn          : IFRE_DB_CONNECTION;
  translatioins : IFRE_DB_DERIVED_COLLECTION;
  transform     : IFRE_DB_SIMPLE_TRANSFORM;
begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    app  := GetEmbeddingApp;
    conn := session.GetDBConnection;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,transform);
    with transform do begin
      AddOneToOnescheme('uid','',FetchModuleTextShort(session,'grid_translation_key'),dt_string,true,true);
      AddOneToOnescheme('lang_def','','LANG DEF');
      AddOneToOnescheme('lang_trans','','LANG TRANS');
      //AddFulltextFilterOnTransformed(['key']);
    end;

    translatioins := session.NewDerivedCollection('TRANSLATIONS_GRID');
    with translatioins do begin
      SetDeriveParent(conn.AdmGetTextResourcesCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox],FetchModuleTextShort(session,'grid_translation_cap'));
      //SetDefaultOrderField('key',true);
    end;
  end;
end;

function TFRE_COMMON_TRANSLATION_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dc  : IFRE_DB_DERIVED_COLLECTION;
  grid: TFRE_DB_VIEW_LIST_DESC;
begin
  CheckClassVisibility4MyDomain(ses);

  dc:=ses.FetchDerivedCollection('TRANSLATIONS_GRID');
  grid:=dc.GetDisplayDescription().Implementor_HC as TFRE_DB_VIEW_LIST_DESC;

  Result:=grid;
end;

{ TFRE_COMMON_TRANSLATION_APP }

procedure TFRE_COMMON_TRANSLATION_APP.SetupApplicationStructure;
begin
  inherited SetupApplicationStructure;
  InitApp('description','images_apps/translation/main.svg');
  AddApplicationModule(TFRE_COMMON_TRANSLATION_MOD.create);
end;

procedure TFRE_COMMON_TRANSLATION_APP._UpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'TRANSLATION',FetchAppTextShort(session,'sitemap_main'),'images_apps/translation/main.svg','',0,conn.sys.CheckClassRight4MyDomain(sr_FETCH,TFRE_COMMON_TRANSLATION_APP));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'TRANSLATION/Translation',FetchAppTextShort(session,'sitemap_translations'),'images_apps/translation/translation.svg',TFRE_COMMON_TRANSLATION_MOD.Classname,0,conn.sys.CheckClassRight4MyDomain(sr_FETCH,TFRE_COMMON_TRANSLATION_MOD));
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  session.GetSessionAppData(Classname).Field('SITEMAP').AsObject := SiteMapData;
end;

procedure TFRE_COMMON_TRANSLATION_APP.MySessionInitialize(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitialize(session);
  if session.IsInteractiveSession then begin
    _UpdateSitemap(session);
  end;
end;

procedure TFRE_COMMON_TRANSLATION_APP.MySessionPromotion(const session: TFRE_DB_UserSession);
begin
  inherited MySessionPromotion(session);
  if session.IsInteractiveSession then
    _UpdateSitemap(session);
end;

class procedure TFRE_COMMON_TRANSLATION_APP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
end;

class procedure TFRE_COMMON_TRANSLATION_APP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
  newVersionId:='0.1';

  if (currentVersionId='') then begin
    currentVersionId := '0.1';
    CreateAppText(conn,'caption','Translation','Translation','Translation');
    CreateAppText(conn,'sitemap_main','Main','','Main');
    CreateAppText(conn,'sitemap_translations','Translation','','Translation');

    //TFRE_COMMON_TRANSLATION_MOD;
    CreateAppText(conn,'translation_description','Translation','Translation','Translation');
  end;
end;

class procedure TFRE_COMMON_TRANSLATION_APP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TFRE_DB_GUID);
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);

  if currentVersionId='' then begin
    currentVersionId:='0.1';

    CheckDbResult(conn.AddRole('ADMINTRANSLATION','Allowed to create, modify and delete Translations','',domainUID),'could not add role ADMINTRANSLATION');

    CheckDbResult(conn.AddRoleRightsToRole('ADMINTRANSLATION',domainUID,TFRE_DB_StringArray.Create(
      TFRE_COMMON_TRANSLATION_APP.GetClassRoleNameFetch,
      TFRE_COMMON_TRANSLATION_MOD.GetClassRoleNameFetch
    )));

    CheckDbResult(conn.AddGroup('TRANSLATIONADMINS','Admins of Translation App','Translation Admins',domainUID),'could not create admins group');
    CheckDbResult(conn.AddRolesToGroup('TRANSLATIONADMINS',domainUID,TFRE_DB_StringArray.Create('ADMINTRANSLATION')),'could not add role ADMINTRANSLATION for group Admins');

  end;
end;

class procedure TFRE_COMMON_TRANSLATION_APP.InstallUserDBObjects(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType);
var
  coll     : IFRE_DB_COLLECTION;
begin
  if currentVersionId='' then begin
    currentVersionId := '0.1';

  end;
end;

end.

