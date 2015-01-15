unit fre_monitoring_common;
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses
  Classes, SysUtils,
  FOS_TOOL_INTERFACES,
  FRE_DB_INTERFACE,
  FRE_DB_COMMON
  ;

type

  { TFRE_COMMON_WF_MOD }

  TFRE_COMMON_WF_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
  protected
    procedure       SetupAppModuleStructure   ; override;
  public
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object; override;
  end;

  { TFRE_COMMON_JOBS_MOD }

  TFRE_COMMON_JOBS_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
  protected
    procedure       SetupAppModuleStructure   ; override;
  public
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object; override;
  end;

procedure Register_DB_Extensions;

implementation

{ TFRE_COMMON_JOBS_MOD }

procedure TFRE_COMMON_JOBS_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('jobs_description')
end;

class procedure TFRE_COMMON_JOBS_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

class procedure TFRE_COMMON_JOBS_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.9';
  if currentVersionId='' then begin
    currentVersionId:='0.9';
    CreateModuleText(conn,'gc_job_caption','Caption');
    //CreateModuleText(conn,'gc_job_id','Id');
    //CreateModuleText(conn,'gc_job_state','State');
  end;
end;

procedure TFRE_COMMON_JOBS_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  transform : IFRE_DB_SIMPLE_TRANSFORM;
  dc        : IFRE_DB_DERIVED_COLLECTION;
  conn      : IFRE_DB_CONNECTION;
begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    conn:=session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,transform);
    with transform do begin
      AddOneToOnescheme('objname','',FetchModuleTextShort(session,'gc_job_caption'));
    end;
    dc := session.NewDerivedCollection('JOBSMOD_JOBS_GRID');
    with dc do begin
      SetDeriveParent(conn.GetJobsCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[cdgf_Children],'');
      //SetDefaultOrderField('step_id',true);
    end;
  end;
end;

function TFRE_COMMON_JOBS_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  CheckClassVisibility4AnyDomain(ses);

  Result:=ses.FetchDerivedCollection('JOBSMOD_JOBS_GRID').GetDisplayDescription;
end;

{ TFRE_COMMON_WF_MOD }

class procedure TFRE_COMMON_WF_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_COMMON_WF_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('workflows_description')
end;

class procedure TFRE_COMMON_WF_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.9';
  if currentVersionId='' then begin
    currentVersionId:='0.9';
    CreateModuleText(conn,'gc_wf_caption','Caption');
    CreateModuleText(conn,'gc_wf_id','Id');
    CreateModuleText(conn,'gc_wf_state','State');
  end;
end;

procedure TFRE_COMMON_WF_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  transform : IFRE_DB_SIMPLE_TRANSFORM;
  dc        : IFRE_DB_DERIVED_COLLECTION;
  conn      : IFRE_DB_CONNECTION;
begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    conn:=session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,transform);
    with transform do begin
      AddMultiToOnescheme(TFRE_DB_NameTypeArray.create('caption','step_caption'),'caption',FetchModuleTextShort(session,'gc_wf_caption'),dt_string,true,true);
      AddOneToOnescheme('step_id','step_id',FetchModuleTextShort(session,'gc_wf_id'));
      AddOneToOnescheme('state','state',FetchModuleTextShort(session,'gc_wf_state'));
    end;
    dc := session.NewDerivedCollection('WFMOD_WF_GRID');
    with dc do begin
      SetDeriveParent(conn.AdmGetWorkFlowCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[cdgf_Children],'');
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

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_WF_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_JOBS_MOD);
end;

end.
