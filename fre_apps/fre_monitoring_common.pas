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
    procedure       getHRState                (const ut : IFRE_DB_USER_RIGHT_TOKEN ; const transformed_object : IFRE_DB_Object ; const session_data : IFRE_DB_Object; const langres: array of TFRE_DB_String);
    class procedure RegisterSystemScheme      (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects          (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function        WEB_Content               (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object; override;
    function        WEB_GridMenu              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_GridSC                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_FinishWFStep          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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

procedure TFRE_COMMON_WF_MOD.getHRState(const ut: IFRE_DB_USER_RIGHT_TOKEN; const transformed_object: IFRE_DB_Object; const session_data: IFRE_DB_Object; const langres: array of TFRE_DB_String);
var fld : IFRE_DB_Field;
begin
  if transformed_object.FieldOnlyExisting('state',fld) then
    transformed_object.Field('stateHR').AsString:=langres[fld.AsInt16-1];
end;

class procedure TFRE_COMMON_WF_MOD.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='0.9.1';
  if currentVersionId='' then begin
    currentVersionId:='0.9';
    CreateModuleText(conn,'gc_wf_caption','Caption');
    CreateModuleText(conn,'gc_wf_id','Id');
    CreateModuleText(conn,'gc_wf_state','State');

  end;
  if currentVersionId='0.9' then begin
    currentVersionId:='0.9.1';
    CreateModuleText(conn,'gc_wf_group','Assigned group');

    CreateModuleText(conn,'wf_state_waiting','Waiting');
    CreateModuleText(conn,'wf_state_child_in_progress','Child in progress');
    CreateModuleText(conn,'wf_state_in_progress','In progress');
    CreateModuleText(conn,'wf_state_done','Done');
    CreateModuleText(conn,'wf_state_faild','Failed');

    CreateModuleText(conn,'tb_wf_step_finish','Finish step');
    CreateModuleText(conn,'cm_wf_step_finish','Finish step');
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
      AddOneToOnescheme('state','state','',dt_string,false);
      AddOneToOnescheme('stateHR','stateHR',FetchModuleTextShort(session,'gc_wf_state'));
      AddMatchingReferencedField('DESIGNATED_GROUP>TFRE_DB_GROUP','displayname','group',FetchModuleTextShort(session,'gc_wf_group'));
      AddOneToOnescheme('step_id','step_id',FetchModuleTextShort(session,'gc_wf_id'));
      SetFinalRightTransformFunction(@getHRState,[FetchModuleTextShort(session,'wf_state_waiting'),FetchModuleTextShort(session,'wf_state_child_in_progress'),FetchModuleTextShort(session,'wf_state_in_progress'),FetchModuleTextShort(session,'wf_state_done'),FetchModuleTextShort(session,'wf_state_faild')]);
    end;
    dc := session.NewDerivedCollection('WFMOD_WF_GRID');
    with dc do begin
      SetDeriveParent(conn.AdmGetWorkFlowCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[cdgf_Children],'',nil,'',CWSF(@WEB_GridMenu),nil,CWSF(@WEB_GridSC));
      SetParentToChildLinkField('<STEP_PARENT');
      SetDefaultOrderField('step_id',true);
    end;
  end;
end;

function TFRE_COMMON_WF_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res: TFRE_DB_VIEW_LIST_DESC;
begin
  CheckClassVisibility4AnyDomain(ses);

  if conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_WORKFLOW_STEP) and conn.SYS.CheckClassRight4AnyDomain(sr_UPDATE,TFRE_DB_WORKFLOW) then begin
    res:=ses.FetchDerivedCollection('WFMOD_WF_GRID').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
    res.AddButton.DescribeManualType('finish_wfs',CWSF(@WEB_FinishWFStep),'',FetchModuleTextShort(ses,'tb_wf_step_finish'),'',true);
  end;

  Result:=res;
end;

function TFRE_COMMON_WF_MOD.WEB_GridSC(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  disableF: Boolean;
  dbo     : IFRE_DB_Object;
  wfStep  : TFRE_DB_WORKFLOW_STEP;
begin
  disableF:=true;
  if input.FieldExists('selected') and (input.Field('selected').ValueCount=1) then begin
    conn.Fetch(FREDB_H2G(input.Field('selected').AsString),dbo);
    if dbo.Implementor_HC is TFRE_DB_WORKFLOW_STEP then begin
      wfStep:=dbo.Implementor_HC as TFRE_DB_WORKFLOW_STEP;
      if wfStep.getState=3 then begin
        disableF:=false;
      end;
    end;
  end;
  Result:=TFRE_DB_UPDATE_UI_ELEMENT_DESC.create.DescribeStatus('finish_wfs',disableF);
end;

function TFRE_COMMON_WF_MOD.WEB_GridMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res   : TFRE_DB_MENU_DESC;
  dbo   : IFRE_DB_Object;
  wfStep: TFRE_DB_WORKFLOW_STEP;
  sf    : TFRE_DB_SERVER_FUNC_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe;
  CheckDbResult(conn.Fetch(FREDB_H2G(input.Field('selected').AsString),dbo));
  if conn.SYS.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_WORKFLOW_STEP,dbo.DomainID) and conn.SYS.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_WORKFLOW,dbo.DomainID) then begin
    if dbo.Implementor_HC is TFRE_DB_WORKFLOW_STEP then begin
      wfStep:=dbo.Implementor_HC as TFRE_DB_WORKFLOW_STEP;
      if wfStep.getState=3 then begin
        sf:=CWSF(@WEB_FinishWFStep);
        sf.AddParam.Describe('selected',input.Field('selected').AsString);
        res.AddEntry.Describe(FetchModuleTextShort(ses,'cm_wf_step_finish'),'',sf);
      end;
    end;
  end;
  Result:=res;
end;

function TFRE_COMMON_WF_MOD.WEB_FinishWFStep(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  wfStep: TFRE_DB_WORKFLOW_STEP;
begin
  CheckDbResult(conn.FetchAs(FREDB_H2G(input.Field('selected').AsString),TFRE_DB_WORKFLOW_STEP,wfStep));
  if not (conn.SYS.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_WORKFLOW_STEP,wfStep.DomainID) and conn.SYS.CheckClassRight4DomainId(sr_UPDATE,TFRE_DB_WORKFLOW,wfStep.DomainID)) then
    raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  wfStep.setState(conn,4);
  Result:=GFRE_DB_NIL_DESC;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_WF_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_COMMON_JOBS_MOD);
end;

end.
