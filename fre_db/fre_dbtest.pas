unit fre_dbtest;

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
  FRE_DB_COMMON,
  FRE_DB_INTERFACE,
  FRE_DBBUSINESS,
//  FOS_VM_CONTROL_INTERFACE,
  FRE_DB_SYSRIGHT_CONSTANTS;
//  fos_dbcorebox_vm_machines_mod;

procedure MetaRegister_Test;
procedure MetaInitializeDatabase_Test(const dbname :string; const user,pass:string);

procedure Register_DB_Extensions;
procedure CreateTestdata(const dbname: string; const user, pass: string);

type

  { TFRE_DB_TEST_A }

  TFRE_DB_TEST_A=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_B }

  TFRE_DB_TEST_B=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function        IMI_Content         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_ChildrenData    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_ALL_TYPES }

  TFRE_DB_TEST_ALL_TYPES=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    function IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
  end;


  { TFRE_DB_TEST_APP }

  TFRE_DB_TEST_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;
    function        InstallAppDefaults            (const conn : IFRE_DB_SYS_CONNECTION):TFRE_DB_Errortype; override;
    function        InstallSystemGroupsandRoles   (const conn : IFRE_DB_SYS_CONNECTION; const domain : TFRE_DB_NameType):TFRE_DB_Errortype; override;

    procedure       _UpdateSitemap            (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize       (const session: TFRE_DB_UserSession);override;
    procedure       MySessionPromotion        (const session: TFRE_DB_UserSession); override;
    function        CFG_ApplicationUsesRights : boolean; override;
    function        _ActualVersion            : TFRE_DB_String; override;
  public
    class procedure RegisterSystemScheme      (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function        IMI_Messages         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_News             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_Calendar         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_Profile          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_Dialog           (const input:IFRE_DB_Object):IFRE_DB_Object;
    function        IMI_RAW_DATA_FEED    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_WELCOME_MOD }

  TFRE_DB_TEST_APP_WELCOME_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_GRID_MOD }

  TFRE_DB_TEST_APP_GRID_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    procedure ToggleBlast (const session:TFRE_DB_UserSession);
    procedure SetBlast    (const session:TFRE_DB_UserSession);
  protected
    class procedure RegisterSystemScheme    (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    function        GetToolbarMenu          : TFRE_DB_CONTENT_DESC;override;
  public
    procedure SC_ChangeData_Result      (const input:IFRE_DB_Object);
    procedure SC_ChangeData_Error       (const input:IFRE_DB_Object);
    function  PRC_UPDATE_TASK           (const input:IFRE_DB_Object):IFRE_DB_Object;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_ToggleUpdates         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HelloWorld            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ChangeData            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_Dialog                (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ReadOnlyDialog        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_GRID_ITEM_DETAILS     (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_SendADialog           (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_GRID2_MOD }

  TFRE_DB_TEST_APP_GRID2_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  GetToolbarMenu            : TFRE_DB_CONTENT_DESC;override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HelloWorld            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_GRID_ITEM_DETAILS     (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_AddRecordBefore       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_AddRecordAfter        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_DelRecord             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_UpdRecord             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_UpdateCS              (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_CHART_MOD }

  TFRE_DB_TEST_APP_CHART_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    procedure       MyServerInitializeModule  (const admin_dbc : IFRE_DB_CONNECTION); override;
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function        GetToolbarMenu            : TFRE_DB_CONTENT_DESC;override;
  published
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ContentLines          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ContentPie            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ContentColumns        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ChartData             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_UpdateDataColl        (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_LIVE_CHART_MOD }

  TFRE_DB_TEST_APP_LIVE_CHART_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    _idx: Integer;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    function        _SendDataSLC        (const Input:IFRE_DB_Object):IFRE_DB_Object;
    function        _SendDataLC         (const Input:IFRE_DB_Object):IFRE_DB_Object;
  published
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ContentSL             (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_ContentL              (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_StartStopSLC          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_StartStopLC           (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_VNC_MOD }

  TFRE_DB_TEST_APP_VNC_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  //{ TFRE_DB_TEST_APP_VM_CONTROLLER_MOD }
  //
  //TFRE_DB_TEST_APP_VM_CONTROLLER_MOD = class (TFRE_DB_APPLICATION_MODULE)
  //protected
  //  class procedure RegisterSystemScheme      (const scheme    : IFRE_DB_SCHEMEOBJECT); override;
  //  procedure       SetupAppModuleStructure   ; override;
  //  procedure       MyServerInitializeModule  (const admin_dbc : IFRE_DB_CONNECTION); override;
  //  procedure       MySessionInitializeModule (const session   : TFRE_DB_UserSession);override;
  //  procedure       UpdateVMCollection        (const dbc:IFRE_DB_CONNECTION  ; const vmc      : IFRE_DB_INDEXED_COLLECTION ; const vmo : IFRE_DB_Object);
  //  procedure       _GetSelectedVMData        (session : TFRE_DB_UserSession ; const selected : TGUID; var vmkey,vnc_port,vnc_host,vm_state: String);
  //published
  //  function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_VM_ShowInfo           (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_VM_ShowVNC            (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_VM_ShowPerf           (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_VM_Details            (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_StartVM               (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_StopVM                (const input:IFRE_DB_Object):IFRE_DB_Object;
  //  function  IMI_UpdateStatus          (const input:IFRE_DB_Object):IFRE_DB_Object;
  //end;


  { TFRE_DB_TEST_APP_GRIDTREEFORM_MOD }

  TFRE_DB_TEST_APP_GRIDTREEFORM_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_EDITORS_MOD }

  TFRE_DB_TEST_APP_EDITORS_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  IMI_Content               (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HtmlEditor            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HtmlEditorLoad        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HtmlEditorSave        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HtmlEditorStart       (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_HtmlEditorStop        (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_JSEditor              (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_JSEditorLoad          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_JSEditorSave          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_JSEditorStart         (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_JSEditorStop          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_PascalEditor          (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_PascalEditorLoad      (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_PascalEditorSave      (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_PascalEditorStart     (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  IMI_PascalEditorStop      (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

implementation

procedure InitializeTestapp(const dbname: string; const user, pass: string);
var conn  : IFRE_DB_SYS_CONNECTION;
    res   : TFRE_DB_Errortype;
    i     : integer;
    login : string;
begin
  CONN := GFRE_DBI.NewSysOnlyConnection;
  try
    res  := CONN.Connect('admin'+'@'+cSYS_DOMAIN,'admin');
    if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);

      conn.InstallAppDefaults('TESTAPP');

      for i:= 1 to 3 do begin
        login  := 'admin'+inttostr(i)+'@'+cSYS_DOMAIN;
        if conn.UserExists(login) then begin
          writeln('Modify Groups for User '+login);
          CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','ADMIN'+'@'+cSYS_DOMAIN),Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+cSYS_DOMAIN)]),true),'cannot set user groups '+login);
        end;
      end;

      for i:= 1 to 3 do begin
        login  := 'user'+inttostr(i)+'@'+cSYS_DOMAIN;
        if conn.UserExists(login) then begin
          writeln('Modify Groups for User '+login);
          CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+cSYS_DOMAIN)]),true),'cannot set user groups '+login);
        end;
      end;

      CheckDbResult(conn.ModifyUserGroups('guest'+'@'+cSYS_DOMAIN,GFRE_DBI.ConstructStringArray([cSYSUG_DB_GUESTS+'@'+cSYS_DOMAIN,Get_Groupname_App_Group_Subgroup('testapp','GUEST'+'@'+cSYS_DOMAIN)])),'cannot set usergroups guest');

  finally
    conn.Finalize;
  end;
end;

{ TFRE_DB_TEST_APP_LIVE_CHART_MOD }

class procedure TFRE_DB_TEST_APP_LIVE_CHART_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_LIVE_CHART_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('LIVE_CHART','$live_chart_description')
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD._SendDataLC(const Input:IFRE_DB_Object):IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
  data   : TFRE_DB_Real32Array;
  res    : TFRE_DB_LIVE_CHART_DATA_DESC;
  i      : Integer;
begin
  session:=GetSession(input);
  SetLength(data,2);
  for i := 0 to 1 do begin
    data[i]:=Random(100);
  end;
  res:=TFRE_DB_LIVE_CHART_DATA_DESC.create.Describe('lchart',_idx,data);
  _idx:=_idx+1;
  session.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD._SendDataSLC(const Input: IFRE_DB_Object): IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
  data   : TFRE_DB_LIVE_CHART_SAMPLED_DATA_ARRAY;
  res    : TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC;
  i      : Integer;
begin
  session:=GetSession(input);
  SetLength(data,1);
  SetLength(data[0],1200);
  for i := 0 to 1199 do begin
    data[0][i]:=Random(100);
  end;
  res:=TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC.create.Describe('slchart',data);
  session.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe();
  res.AddSection.Describe(CSF(@IMI_ContentL),'LC',0);
  res.AddSection.Describe(CSF(@IMI_ContentSL),'SLC',1);
  Result:=res;
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.IMI_ContentSL(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_LIVE_CHART_DESC.create.DescribeSampledLine('slchart',1,CSF(@IMI_StartStopSLC),0,100,'Sampled Live Chart',nil,nil,10,120000,100);
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.IMI_ContentL(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_LIVE_CHART_DESC.create.Describe('lchart',2,CSF(@IMI_StartStopLC),0,100,'Live Chart',nil,nil,10,nil,120000,1000);
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.IMI_StartStopSLC(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
begin
  session:=GetSession(input);
  if input.Field('action').AsString='start' then begin
    session.RegisterTaskMethod(@_SendDataSLC,40);
  end else begin
    session.RemoveTaskMethod;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.IMI_StartStopLC(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
begin
  session:=GetSession(input);
  if input.Field('action').AsString='start' then begin
    _idx:=0;
    session.RegisterTaskMethod(@_SendDataLC,1000);
  end else begin
    session.RemoveTaskMethod;
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_APP_EDITORS_MOD }

class procedure TFRE_DB_TEST_APP_EDITORS_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_EDITORS_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('EDIT','$edit_description')
end;

procedure TFRE_DB_TEST_APP_EDITORS_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitializeModule(session);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe();
  res.AddSection.Describe(CSF(@IMI_HtmlEditor),'Html',1);
  res.AddSection.Describe(CSF(@IMI_JSEditor),'JS',2);
  res.AddSection.Describe(CSF(@IMI_PascalEditor),'Pascal',3);
  Result:=res;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_HtmlEditor(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CSF(@IMI_HtmlEditorLoad),CSF(@IMI_HtmlEditorSave),CSF(@IMI_HtmlEditorStart),CSF(@IMI_HtmlEditorStop));
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_HtmlEditorLoad(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('Initial Html<BR><BR>Hello');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_HtmlEditorSave(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_HtmlEditorStart(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_HtmlEditorStop(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_JSEditor(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CSF(@IMI_JSEditorLoad),CSF(@IMI_JSEditorSave),CSF(@IMI_JSEditorStart),CSF(@IMI_JSEditorStop),ct_javascript);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_JSEditorLoad(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('if (true) {'+#10+'  console.log("HELLO");'+#10+'}');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_JSEditorSave(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_JSEditorStart(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_JSEditorStop(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_PascalEditor(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CSF(@IMI_PascalEditorLoad),CSF(@IMI_PascalEditorSave),CSF(@IMI_PascalEditorStart),CSF(@IMI_PascalEditorStop),ct_pascal);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_PascalEditorLoad(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('if (true) then begin'+#10+'  writeln("HELLO");'+#10+'end;');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_PascalEditorSave(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_PascalEditorStart(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.IMI_PascalEditorStop(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_APP_VM_CONTROLLER_MOD }

//class procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
//begin
//  inherited RegisterSystemScheme(scheme);
//  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
//end;

//procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.SetupAppModuleStructure;
//begin
//  inherited SetupAppModuleStructure;
//  InitModuleDesc('VMCONTROLLER','VMC','VM Controller','')
//end;

//procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
//var vmcc    : IFRE_DB_INDEXED_COLLECTION;
//    vmc     : IFOS_VM_HOST_CONTROL;
//    vmo     : IFRE_DB_Object;
//    vm      : IFRE_DB_Object;
//    uvm     : IFRE_DB_Object;
//    i       : Integer;
//begin
//  inherited MyServerInitializeModule(admin_dbc);
//  //vmc := Get_VM_Host_Control(cVM_HostUser,cVMHostMachine);
//  //vmc.VM_ListMachines(vmo);
//  //vmc.Finalize;
//  admin_dbc.CollectionAsIntf('VMC',IFRE_DB_INDEXED_COLLECTION,VMCC,true,true);
//  VMCC.SetIndexField('Mkey',fdbft_String,true,true);
//  //UpdateVMCollection(admin_dbc,vmcc,vmo);
//end;
//
//procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
//var vmc        : IFRE_DB_DERIVED_COLLECTION;
//    vmcp       : IFRE_DB_COLLECTION;
//    tr_Grid    : IFRE_DB_SIMPLE_TRANSFORM;
//       vmo     : IFRE_DB_Object;
//       vm      : IFRE_DB_Object;
//       uvm     : IFRE_DB_Object;
//       i       : Integer;
//begin
//  inherited MySessionInitializeModule(session);
//  GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
//  with tr_Grid do begin
//    AddOneToOnescheme('MName','','Name');
//    AddOneToOnescheme('MType','','Typ');
//    AddOneToOnescheme('MState','','State');
//    //AddOneToOnescheme('Mkey','','Key');
//    //AddOneToOnescheme('MBrand','','Brand');
//    //AddCollectorscheme('');
//    AddOneToOnescheme('PERFPCPU','','CPU',dt_number);
//    AddOneToOnescheme('PERFPMEM','','Used Mem',dt_number);
//    AddOneToOnescheme('PERFRSS','','Paged Mem',dt_number);
//    AddOneToOnescheme('PERFVSZ','','Virtual Mem',dt_number);
//    AddOneToOnescheme('MVIOPRIO','','IO Prio',dt_number);
//      //PERFPSET (STRING) : [ '' ]
//      //VNC_PORT (STRING) : [ '6001' ]
//      //MCPUQUOTA (STRING) : [ '10' ]
//      //MCPUSHARES (STRING) : [ '100' ]
//  end;
//  vmcp := session.GetDBConnection.Collection('VMC',false);
//  vmc  := session.NewDerivedCollection('VMC');
//  with VMC do begin
//    SetDeriveTransformation(tr_Grid);
//    SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable],'VM',nil,'',nil,nil,CSF(@IMI_VM_Details));
//    SetDeriveParent(vmcp);
//  end;
//end;
//
//procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.UpdateVMCollection(const dbc: IFRE_DB_CONNECTION; const vmc: IFRE_DB_INDEXED_COLLECTION; const vmo : IFRE_DB_Object);
//var i       : integer;
//    vm      : IFRE_DB_Object;
//    uvm     : IFRE_DB_Object;
//
//  procedure Obj2Obj(const vm,uvm:IFRE_DB_Object);
//  begin
//    uvm.Field('MName').AsString    := vm.Field('MName').AsString;
//    uvm.Field('MType').AsString    := vm.Field('MType').AsString;
//    uvm.Field('MState').AsString   := vm.Field('MState').AsString;
//    uvm.Field('MBrand').AsString   := vm.Field('MBrand').AsString;
//    if vm.FieldExists('PERFPCPU') then begin
//      uvm.Field('PERFPCPU').AsString := vm.Field('PERFPCPU').AsString+'%';
//      uvm.Field('PERFPMEM').AsString := vm.Field('PERFPMEM').AsString+'%';
//    end;
//    if vm.FieldExists('PERFRSS') then begin
//      uvm.Field('PERFRSS').AsString := Format('%2.2f MB',[vm.Field('PERFRSS').AsUInt32 / 1024]);
//    end else begin
//      uvm.Field('PERFRSS').AsString :='-';
//    end;
//    if vm.FieldExists('PERFVSZ') then begin
//      uvm.Field('PERFVSZ').AsString := Format('%2.2f MB',[vm.Field('PERFVSZ').AsUInt32 / 1024]);
//    end else begin
//      uvm.Field('PERFVSZ').AsString :='-';
//    end;
//    uvm.Field('MVIOPRIO').AsString := vm.Field('MVIOPRIO').AsString;
//    uvm.Field('VM_PROPS').AsObject := vm.Field('VM_PROPS').AsObject;
//    uvm.Field('VM_INFO').AsObject  := vm.Field('VM_INFO').AsObject;
//    uvm.Field('VNC_PORT').AsString := vm.Field('VM_INFO').AsObject.Field('VNC').AsObject.Field('PORT').AsString;
//  end;
//
//begin
//  for i := 0 to high(vmo.Field('Machines').AsObjectArr) do begin
//    vm      := vmo.Field('Machines').AsObjectArr[i];
//    if vmc.GetIndexedObj(vm.Field('MKey').AsString,UVM) then begin
//      Obj2Obj(vm,uvm);
//      vmc.Update(uvm);
//    end else begin
//      uvm := GFRE_DBI.NewObject;
//      Obj2Obj(vm,uvm);
//      //uvm.field('UID').AsGUID:=vm.UID;
//      uvm.Field('MKey').AsString    := vm.Field('MKey').AsString;
//      vmc.Store(uvm);
//    end;
//  end;
//end;
//
//procedure TFRE_DB_TEST_APP_VM_CONTROLLER_MOD._GetSelectedVMData(session: TFRE_DB_UserSession; const selected: TGUID; var vmkey, vnc_port,vnc_host,vm_state: String);
//var DC_VMC     : IFRE_DB_DERIVED_COLLECTION;
//      vmo        : IFRE_DB_Object;
//begin
//  DC_VMC := session.FetchDerivedCollection('VMC');
//  if DC_VMC.FetchFromParent(selected,vmo) then begin
//    vmkey    := vmo.Field('MKEY').AsString;
//    vnc_port := vmo.Field('VNC_PORT').AsString;
//    vnc_host := cVMHostMachine;
//    vm_state := vmo.Field('MSTATE').AsString;
//    writeln('VMO: ',vmkey,' ',vnc_port,' ', vm_state);
//  end;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
//var
//  coll   : IFRE_DB_DERIVED_COLLECTION;
//  list   : TFRE_DB_VIEW_LIST_DESC;
//begin
//  coll := GetSession(input).FetchDerivedCollection('VMC');
//  list := coll.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
//  list.AddButton.Describe(CSF(@IMI_StartVM)     , '','Start','start the selected VM',fdgbd_single);
//  list.AddButton.Describe(CSF(@IMI_StopVM)      , '','Stop','',fdgbd_single);
//  list.AddButton.Describe(CSF(@IMI_UpdateStatus), '','Update','',fdgbd_always);
//  Result := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(list,nil,nil);
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_VM_ShowInfo(const input: IFRE_DB_Object): IFRE_DB_Object;
//var vmcc  : IFRE_DB_INDEXED_COLLECTION;
//    vmkey : string;
//      obj : IFRE_DB_Object;
//begin
//  vmkey := input.Field('vmkey').AsString;
//  GetDBConnection(input).CollectionAsIntf('VMC',IFRE_DB_INDEXED_COLLECTION,VMCC,true,true);
//  if vmcc.GetIndexedObj(vmkey,obj) then begin
//    result := TFRE_DB_HTML_DESC.create.Describe(FREDB_String2EscapedJSString('<pre style="font-size: 10px">'+obj.DumpToString+'</pre>'));
//  end else begin
//    result := TFRE_DB_HTML_DESC.create.Describe('- could not get info -');
//  end;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_VM_ShowVNC(const input: IFRE_DB_Object): IFRE_DB_Object;
//var vmkey : string;
//    vmcc  : IFRE_DB_INDEXED_COLLECTION;
//      obj : IFRE_DB_Object;
//begin
//  writeln('VNC INPUT ',input.DumpToString);
//  vmkey  := input.Field('vmkey').AsString;
//  GetDBConnection(input).CollectionAsIntf('VMC',IFRE_DB_INDEXED_COLLECTION,VMCC,true,true);
//  if vmcc.GetIndexedObj(vmkey,obj) then begin
//    if (obj.Field('MSTATE').AsString='running') and (obj.Field('MTYPE').AsString='KVM') then begin
//      result := TFRE_DB_VNC_DESC.create.Describe(input.Field('VNC_HOST').AsString,input.Field('VNC_PORT').AsUInt32);
//    end else begin
//      result := TFRE_DB_HTML_DESC.create.Describe(FREDB_String2EscapedJSString('<pre style="font-size: 10px">'+obj.DumpToString+'</pre>'));
//    end;
//  end else begin
//    result := TFRE_DB_HTML_DESC.create.Describe('- could not get infos -');
//  end;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_VM_ShowPerf(const input: IFRE_DB_Object): IFRE_DB_Object;
//begin
//  result := TFRE_DB_HTML_DESC.create.Describe('SEAS3');
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_VM_Details(const input: IFRE_DB_Object): IFRE_DB_Object;
//var   vm_sub       : TFRE_DB_SUBSECTIONS_DESC;
//      vmo          : IFRE_DB_Object;
//      sf           : TFRE_DB_SERVER_FUNC_DESC;
//      sel_guid     : TGUID;
//      vmkey,vncp,
//      vnch,vmstate : string;
//begin
//  if input.FieldExists('SELECTED') then begin
//    sel_guid := input.Field('SELECTED').AsGUID;
//    _GetSelectedVMData(GetSession(input),sel_guid,vmkey,vncp,vnch,vmstate);
//    vm_sub := TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_tab);
//    sf := CSF(@IMI_VM_ShowInfo); sf.AddParam.Describe('VMKEY',vmkey);
//    vm_sub.AddSection.Describe(sf,'Info',2);
//    sf := CSF(@IMI_VM_ShowVNC); sf.AddParam.Describe('VNC_PORT',vncp) ; sf.AddParam.Describe('VNC_HOST',vnch); sf.AddParam.Describe('VMKEY',vmkey);
//    vm_sub.AddSection.Describe(sf,'Console',1);
//    vm_sub.AddSection.Describe(CSF(@IMI_VM_ShowPerf),'Performance',3);
//    result := vm_sub;
//  end;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_StartVM(const input: IFRE_DB_Object): IFRE_DB_Object;
//var   vmc   : IFOS_VM_HOST_CONTROL;
//      vmkey : string;
//      vncp  : string;
//      vnch  : string;
//    vmstate : string;
//begin
//  if input.FieldExists('SELECTED') then begin
//    _GetSelectedVMData(GetSession(input),input.Field('SELECTED').AsGUID,vmkey,vncp,vnch,vmstate);
//    vmc := Get_VM_Host_Control(cVM_HostUser,cVMHostMachine);
//    vmc.VM_Start(vmkey);
//    vmc.Finalize;
//  end;
//  result := GFRE_DB_NIL_DESC;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_StopVM(const input: IFRE_DB_Object): IFRE_DB_Object;
//var   vmc     : IFOS_VM_HOST_CONTROL;
//      vmkey   : string;
//      vncp    : string;
//      vnch    : string;
//      vmstate : string;
//begin
//  if input.FieldExists('SELECTED') then begin
//    _GetSelectedVMData(GetSession(input),input.Field('SELECTED').AsGUID,vmkey,vncp,vnch,vmstate);
//    vmc := Get_VM_Host_Control(cVM_HostUser,cVMHostMachine);
//    vmc.VM_Halt(vmkey);
//    vmc.Finalize;
//  end;
//  result := GFRE_DB_NIL_DESC;
//end;
//
//function TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.IMI_UpdateStatus(const input: IFRE_DB_Object): IFRE_DB_Object;
//var session  : TFRE_DB_UserSession;
//    //DC_VMC   : IFRE_DB_DERIVED_COLLECTION;
//    //selected : TGUID;
//    vmo      : IFRE_DB_Object;
//    vmc      : IFOS_VM_HOST_CONTROL;
//    vmkey    : string;
//    i        : integer;
//    vmcc     : IFRE_DB_INDEXED_COLLECTION;
//begin
//  result := GFRE_DB_NIL_DESC;
//  vmc := Get_VM_Host_Control(cVM_HostUser,cVMHostMachine);
//  vmc.VM_ListMachines(vmo);
//  vmc.Finalize;
//  GetDBConnection(input).CollectionAsIntf('VMC',IFRE_DB_INDEXED_COLLECTION,VMCC,true,true);
//  UpdateVMCollection(GetDBConnection(input),vmcc,vmo);
//end;

{ TFRE_DB_TEST_APP_VNC_MOD }

class procedure TFRE_DB_TEST_APP_VNC_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_VNC_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('VNC','$vnc_description')
end;

function TFRE_DB_TEST_APP_VNC_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_VNC_DESC.create.Describe('10.1.0.130',36644);
end;

{ TFRE_DB_TEST_APP_WELCOME_MOD }

procedure TFRE_DB_TEST_APP_WELCOME_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('welcome','$welcome_description');
end;

class procedure TFRE_DB_TEST_APP_WELCOME_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

function TFRE_DB_TEST_APP_WELCOME_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  result := TFRE_DB_HTML_DESC.create.Describe('Welcome to the Welcome Module');
end;

{ TFRE_DB_TEST_B }

class procedure TFRE_DB_TEST_B.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('firstname',fdbft_String);
  scheme.AddSchemeField         ('lastname',fdbft_String);
  scheme.AddSchemeField         ('icon',fdbft_String);
end;

function TFRE_DB_TEST_B.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_FORM_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('FORM');
  res.AddInput.Describe('Firstname','firstname');
  res.AddInput.Describe('Lastname','lastname');
  res.AddInput.Describe('Icon','icon');
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',CSF(@IMI_saveOperation),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_TEST_B.IMI_ChildrenData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res : TFRE_DB_STORE_DATA_DESC;
  obj : IFRE_DB_Object;
  i   : Integer;
begin
  res:=TFRE_DB_STORE_DATA_DESC.create.Describe(4);
  for i := 0 to 3 do begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('firstname').AsString:='f'+IntToStr(i);
    obj.Field('lastname').AsString:='l'+IntToStr(i);
    obj.Field('icon').AsString:='i'+IntToStr(i);
    obj.Field('children').AsString:='UNCHECKED';
    obj.Field('uidpath').AsStringArr:=Self.GetUIDPath;
    obj.Field('_funcclassname_').AsString:='TFRE_DB_TEST_B';
    obj.Field('_childrenfunc_').AsString:='CHILDRENDATA';
    res.addEntry(obj);
  end;
  Result:=res;
end;

{ TFRE_DB_TEST_APP_GRIDTREEFORM_MOD }

class procedure TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('TGF','$gtf_description')
end;

procedure TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid      : IFRE_DB_SIMPLE_TRANSFORM;
    DC_Tree      : IFRE_DB_DERIVED_COLLECTION;
begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme ('firstname','','Firstname');
      AddOneToOnescheme ('lastname','','Lastname');
      AddOneToOnescheme ('icon','','Icon');
      AddConstString('_funcclassname_','TFRE_DB_TEST_B');
      AddConstString('_childrenfunc_','CHILDRENDATA');
      AddConstString('children','UNCHECKED');
    end;

    DC_Grid_Long := session.NewDerivedCollection('COLL_TEST_B_DERIVED');
    with DC_Grid_Long do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_B'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Children],'GRID');
    end;

    DC_Tree := session.NewDerivedCollection('COLL_TEST_B_DERIVED_TREE');
    with DC_Tree do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_B'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Tree',TFRE_DB_StringArray.create('firstname'),'icon',nil,nil,nil);//CSF('TreeMenu'));
    end;
  end;
end;

function TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  coll  : IFRE_DB_DERIVED_COLLECTION;
  list  : TFRE_DB_VIEW_LIST_DESC;
begin
  coll := GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED');
  list := coll.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  list.addFilterEvent('COLL_TEST_B_DERIVED_TREE','uid');

  Result := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(list,TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED_TREE').GetDisplayDescription,nil));
end;

{ TFRE_DB_TEST_ALL_TYPES }

class procedure TFRE_DB_TEST_ALL_TYPES.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  //scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  //scheme.AddSchemeField         ('fdbft_GUID',fdbft_GUID);
  //scheme.AddSchemeField         ('fdbft_Byte',fdbft_Byte);
  //scheme.AddSchemeField         ('fdbft_Int16',fdbft_Int16);
  //scheme.AddSchemeField         ('fdbft_UInt16',fdbft_UInt16);
  //scheme.AddSchemeField         ('fdbft_Int32',fdbft_Int32);
  //scheme.AddSchemeField         ('fdbft_UInt32',fdbft_UInt32);
  //scheme.AddSchemeField         ('fdbft_Int64',fdbft_Int64);
  //scheme.AddSchemeField         ('fdbft_UInt64',fdbft_UInt64);
  //scheme.AddSchemeField         ('fdbft_Real32',fdbft_Real32);
  //scheme.AddSchemeField         ('fdbft_Real64',fdbft_Real64);
  //scheme.AddSchemeField         ('fdbft_Currency',fdbft_Currency);
  //scheme.AddSchemeField         ('fdbft_String',fdbft_String);
  //
  //,,,,,,,,,,,,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream,fdbft_Object,fdbft_ObjLink,fdbft_CalcField

  //,fdbft_Byte,fdbft_Int16,fdbft_UInt16,fdbft_Int32,fdbft_UInt32,fdbft_Int64,fdbft_UInt64,fdbft_Real32,fdbft_Real64,fdbft_Currency,fdbft_String,fdbft_Boolean,fdbft_DateTimeUTC,fdbft_Stream,fdbft_Object,fdbft_ObjLink,fdbft_CalcField
  scheme.AddSchemeField         ('string',fdbft_Byte);
  scheme.AddSchemeField         ('boolean',fdbft_Boolean);
  scheme.AddSchemeField         ('status',fdbft_String);
  scheme.AddCalculatedField     ('icon','GetIcon',cft_OnStoreUpdate);
end;

function TFRE_DB_TEST_ALL_TYPES.IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
begin

end;

{ TFRE_DB_TEST_APP_CHART_MOD }

class procedure TFRE_DB_TEST_APP_CHART_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_CHART_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('CHART','$chart_description')
end;

procedure TFRE_DB_TEST_APP_CHART_MOD.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
var vmo       : IFRE_DB_Object;
    vm        : IFRE_DB_Object;
    data_obj  : IFRE_DB_Object;
    i,max     : Integer;
    CHARTDATA : IFRE_DB_COLLECTION;
begin
  inherited MyServerInitializeModule(admin_dbc);
  CHARTDATA := admin_dbc.Collection('CHART_MOD_LINE',true,true);
  max := 60;
  for i := 0 to max - 1 do begin
    data_obj := GFRE_DBI.NewObject;
    data_obj.Field('yval1').AsReal32      := i;
    data_obj.Field('yval1_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('yval2').AsReal32      := i*2;
    data_obj.Field('yval2_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('yval3').AsReal32      := random(100);
    data_obj.Field('yval3_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('yval1_col').AsString  := '#44667'+IntToStr(i mod 10)+'A';
    data_obj.Field('yval1_txt').AsString  := 'S1_TXT ' + IntToStr(i+1);
    data_obj.Field('yval2_col').AsString  := '#CC667'+IntToStr(i mod 10)+'A';
    data_obj.Field('yval2_txt').AsString  := 'S2_TXT ' + IntToStr(i+1);
    data_obj.Field('yval1_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    CHARTDATA.Store(data_obj);
  end;
  CHARTDATA := admin_dbc.Collection('CHART_MOD_PIE',true,true);
  max:=9;
  for i := 0 to max - 1 do begin
    data_obj := GFRE_DBI.NewObject;
    data_obj.Field('val1').AsReal32      := random(100);
    data_obj.Field('val1_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('val1_col').AsString  := '#44667'+IntToStr(i mod 10)+'A';
    data_obj.Field('val1_txt').AsString  := 'PIE:' + IntToStr(i+1);
    CHARTDATA.Store(data_obj);
  end;
  CHARTDATA := admin_dbc.Collection('CHART_MOD_COLUMNS',true,true);
  max:=15;
  for i := 0 to max - 1 do begin
    data_obj := GFRE_DBI.NewObject;
    data_obj.Field('val1').AsReal32 := random(100);
    data_obj.Field('val1_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('val2').AsReal32 := random(100);
    data_obj.Field('val2_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('val1_col').AsString  := '#44667'+IntToStr(i mod 10)+'A';
    data_obj.Field('val1_txt').AsString  := 'COL:' + IntToStr(i+1);
    data_obj.Field('val2_col').AsString  := '#FF667'+IntToStr(i mod 10)+'A';
    data_obj.Field('val2_txt').AsString  := 'COL:' + IntToStr(i+1);
    CHARTDATA.Store(data_obj);
  end;
end;

procedure TFRE_DB_TEST_APP_CHART_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
    DC_CHARTDATA_L : IFRE_DB_DERIVED_COLLECTION;
    DC_CHARTDATA_P : IFRE_DB_DERIVED_COLLECTION;
    DC_CHARTDATA_C : IFRE_DB_DERIVED_COLLECTION;
    CHARTDATA      : IFRE_DB_COLLECTION;
begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    CHARTDATA := session.GetDBConnection.Collection('CHART_MOD_LINE');
    DC_CHARTDATA_L := session.NewDerivedCollection('CHART_L');
    with DC_CHARTDATA_L do begin
      SetDeriveParent(CHARTDATA);
      SetDisplayTypeChart('Line Chart',fdbct_line,TFRE_DB_StringArray.Create('yval1','yval2','yval3'),false,false);
    end;
    CHARTDATA := session.GetDBConnection.Collection('CHART_MOD_PIE');
    DC_CHARTDATA_P := session.NewDerivedCollection('CHART_P');
    with DC_CHARTDATA_P do begin
      SetDeriveParent(CHARTDATA);
      SetDisplayTypeChart('Pie Chart',fdbct_pie,TFRE_DB_StringArray.Create('val1'),false,false);
    end;
    CHARTDATA := session.GetDBConnection.Collection('CHART_MOD_COLUMNS');
    DC_CHARTDATA_C := session.NewDerivedCollection('CHART_CH');
    with DC_CHARTDATA_C do begin
      SetDeriveParent(CHARTDATA);
      SetDisplayTypeChart('ColumnH Chart',fdbct_column,TFRE_DB_StringArray.Create('val1','val2'),false,false);
    end;
  end;
end;

function TFRE_DB_TEST_APP_CHART_MOD.GetToolbarMenu: TFRE_DB_CONTENT_DESC;
var
  menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('Update Collection','',CSF(@IMI_UpdateDataColl));
  Result:=menu;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  sub_sec_s       : TFRE_DB_SUBSECTIONS_DESC;

begin
  sub_sec_s        := TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_tab);
  sub_sec_s.AddSection.Describe(CSF(@IMI_ContentLines),'Lines',1);
  sub_sec_s.AddSection.Describe(CSF(@IMI_ContentPie),'Pie',1);
  sub_sec_s.AddSection.Describe(CSF(@IMI_ContentColumns),'ColumnH',1);
  result           := sub_sec_s;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_ContentLines(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GetSession(input).FetchDerivedCollection('CHART_L').GetDisplayDescription;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_ContentPie(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GetSession(input).FetchDerivedCollection('CHART_P').GetDisplayDescription;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_ContentColumns(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=GetSession(input).FetchDerivedCollection('CHART_CH').GetDisplayDescription;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_ChartData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    : TFRE_DB_CHART_DATA_DESC;
  data   : TFRE_DB_Real32Array;
  uids   : TFRE_DB_GUIDArray;
  colors : TFRE_DB_StringArray;
  texts  : TFRE_DB_StringArray;
  i,max  : Integer;
begin
  max:=15;
  if input.FieldPath('query.sid').AsString='series1' then begin
    SetLength(data,max);
    SetLength(uids,max);
    SetLength(colors,max);
    SetLength(texts,max);
    for i := 0 to max - 1 do begin
      data[i]:=Random(100);
      if i<10 then begin
        uids[i]:=GFRE_BT.HexString_2_GUID('11e7b038157fbedaa8b17938f5a2f00' + IntToStr(i));
      end else begin
        uids[i]:=GFRE_BT.HexString_2_GUID('11e7b038157fbedaa8b17938f5a2f0' + IntToStr(i));
      end;
      colors[i]:='#44667'+IntToStr(i mod 10)+'A';
      texts[i]:='TEXT ' + IntToStr(i+1);
    end;
    res:=TFRE_DB_CHART_DATA_DESC.create.Describe;
    res.setSeries(data,uids,colors,texts);
//    res.setSeries(data);
    Result:=res;
  end else begin //series2
    SetLength(data,max);
    for i := 0 to max - 1 do begin
      data[i]:=Random(100);
    end;
    //data[0]:=10; data[1]:=15; data[2]:=0; data[3]:=10; data[4]:=2;
    res:=TFRE_DB_CHART_DATA_DESC.create.Describe;
    res.setSeries(data);
    Result:=res;
  end;
end;

function TFRE_DB_TEST_APP_CHART_MOD.IMI_UpdateDataColl(const input: IFRE_DB_Object): IFRE_DB_Object;
var CHARTDATA : IFRE_DB_COLLECTION;
    data_obj  : IFRE_DB_Object;

  procedure ChangeData(const data_obj:IFRE_DB_Object);
  begin
    if random(6)=3 then data_obj.Field('yval1').AsReal32 := data_obj.Field('yval1').AsReal32+(random(10)-5);
    if random(6)=3 then data_obj.Field('yval2').AsReal32 := data_obj.Field('yval2').AsReal32+(random(10)-5);
    if random(6)=3 then data_obj.Field('yval3').AsReal32 := data_obj.Field('yval3').AsReal32+(random(10)-5);
    CHARTDATA.Update(data_obj);
  end;

begin
  writeln('DATA CHANGE CHART_MOD_LINE');
  result    := GFRE_DB_NIL_DESC;
  CHARTDATA := GetDBConnection(input).Collection('CHART_MOD_LINE',false);
  CHARTDATA.ForAll(@ChangeData);
end;

{ TFRE_DB_TEST_APP_GRID2_MOD }

class procedure TFRE_DB_TEST_APP_GRID2_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_GRID2_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('GRID2','$grid2_description')
end;

procedure TFRE_DB_TEST_APP_GRID2_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid      : IFRE_DB_SIMPLE_TRANSFORM;

begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme    ('number','','Number',dt_number);
      AddProgressTransform ('number_pb','','Number PB','number_pb','',1000);
      AddOneToOnescheme    ('string','','String');
      AddOneToOnescheme    ('boolean','','Boolean',dt_boolean);
      AddOneToOnescheme    ('date','','Date',dt_date);
      AddOneToOnescheme    ('icon','','Icon',dt_icon);
    end;

    DC_Grid_Long := session.NewDerivedCollection('COLL_TEST_A_DERIVED2');
    with DC_Grid_Long do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_A2'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable],'',TFRE_DB_StringArray.create('objname'),'',nil);
    end;
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.GetToolbarMenu: TFRE_DB_CONTENT_DESC;
var
  submenu,menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('SEAS1','',CSF(@IMI_HelloWorld));
  menu.AddEntry.Describe('SEAS2','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  submenu:=menu.AddMenu.Describe('SEAS3','images_apps/test/cog.png');
  submenu.AddEntry.Describe('SEAS4','',CSF(@IMI_HelloWorld));
  submenu.AddEntry.Describe('SEAS5','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  submenu:=submenu.AddMenu.Describe('SEAS6','');
  submenu.AddEntry.Describe('SEAS7','',CSF(@IMI_HelloWorld));
  submenu.AddEntry.Describe('SEAS8','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  menu.AddEntry.Describe('Update Content Section','',CSF(@IMI_UpdateCS));
  Result:=menu;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var lvd_Grid     : TFRE_DB_VIEW_LIST_DESC;
    DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
    layout       : TFRE_DB_LAYOUT_DESC;
    html         : TFRE_DB_HTML_DESC;
begin
  layout:=TFRE_DB_LAYOUT_DESC.create.Describe();

  DC_Grid_Long := GetSession(input).FetchDerivedCollection('COLL_TEST_A_DERIVED2');
  lvd_Grid := DC_Grid_Long.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button with TT','Tooltip');
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button wo TT');
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button sSel','Enabled on single Select',fdgbd_single);
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button mSel','Enabled on any Selection',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CSF(@IMI_AddRecordBefore),'images_apps/test/add.png','Add Obj Before','Add one Object');
  lvd_Grid.AddButton.Describe(CSF(@IMI_AddRecordAfter),'images_apps/test/add.png','Add Obj After','Add one Object');
  lvd_Grid.AddButton.Describe(CSF(@IMI_DelRecord),'images_apps/test/add.png','Del sel','Delete sel. Objs',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CSF(@IMI_UpdRecord),'images_apps/test/add.png','Upd sel','Update sel. Objs',fdgbd_multi);

  html:=TFRE_DB_HTML_DESC.create.Describe('SEAS INITIAL');
  html.contentId:='GRID2_HTML';

  layout.SetLayout(lvd_Grid,html,nil,nil,nil,true,1);
  //layout.contentId:='GRID2_LAYOUT';
  result := layout;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_HelloWorld(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  writeln(INPUT.DumpToString());
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World '+input.Field('SELECTED').AsStringDump,fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_GRID_ITEM_DETAILS(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe('<B>GUGUG</B><BR>SEAS');
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_AddRecordBefore(const input: IFRE_DB_Object): IFRE_DB_Object;
var col : IFRE_DB_COLLECTION;
    NEW : IFRE_DB_Object;
    selg: TGUID;
    num : UInt32;
begin
  Result := GFRE_DB_NIL_DESC;
  writeln('ADD BEFORE');
  writeln(input.Field('SELECTED').AsString);
  selg := GFRE_BT.HexString_2_GUID(input.Field('SELECTED').AsString);
  col    := GetDBConnection(input).Collection('COLL_TEST_A2');
  if col.Fetch(selg,new) then begin
    num := new.Field('number').AsUint32-1;
    new.Finalize;
    writeln('-NEW OBJECT BEFOR NUM : ',num);
    NEW    := GetDBConnection(input).NewObject(TFRE_DB_TEST_A.ClassName);
    new.Field('number').AsUInt32    := num;
    new.Field('number_pb').AsUInt32 := num * 10;
    new.Field('string').AsString    := 'String_' + IntToStr(new.Field('number').AsUInt32);
    new.Field('boolean').AsBoolean  := false;
    new.Field('date').AsDateTime    := GFRE_DT.Now_UTC;
    new.Field('status').AsString    := 'NEW';
    col.Store(new);
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_AddRecordAfter(const input: IFRE_DB_Object): IFRE_DB_Object;
var col : IFRE_DB_COLLECTION;
    selg: TGUID;
    NEW : IFRE_DB_Object;
    num : UInt32;
begin
  Result := GFRE_DB_NIL_DESC;
  writeln('ADD BEFORE');
  writeln(input.Field('SELECTED').AsString);
  selg := GFRE_BT.HexString_2_GUID(input.Field('SELECTED').AsString);
  col    := GetDBConnection(input).Collection('COLL_TEST_A2');
  if col.Fetch(selg,new) then begin
    num := new.Field('number').AsUint32+1;
    new.Finalize;
    writeln('-NEW OBJECT BEFOR NUM : ',num);
    NEW    := GetDBConnection(input).NewObject(TFRE_DB_TEST_A.ClassName);
    new.Field('number').AsUInt32    := num;
    new.Field('number_pb').AsUInt32 := num  * 10;
    new.Field('string').AsString    := 'String_' + IntToStr(new.Field('number').AsUInt32);
    new.Field('boolean').AsBoolean  := false;
    new.Field('date').AsDateTime    := GFRE_DT.Now_UTC;
    new.Field('status').AsString    := 'NEW';
    col.Store(new);
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_DelRecord(const input: IFRE_DB_Object): IFRE_DB_Object;
var col       : IFRE_DB_COLLECTION;
    del_guids : TFRE_DB_GUIDArray;
    i         : integer;
begin
  col       := GetDBConnection(input).Collection('COLL_TEST_A2');
  del_guids :=  GFRE_DBI.StringArray2GuidArray(input.Field('SELECTED').AsStringArr);
  for i:=0 to high(del_guids) do begin
    if not col.Remove(del_guids[i]) then begin
      writeln('Could not delete Obj uid : '+GFRE_BT.GUID_2_HexString(del_guids[i]));
    end else begin
      writeln('Deleted Obj uid : '+GFRE_BT.GUID_2_HexString(del_guids[i]));
    end;
  end;
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_UpdRecord(const input: IFRE_DB_Object): IFRE_DB_Object;
var col       : IFRE_DB_COLLECTION;
    upd_guids : TFRE_DB_GUIDArray;
    i         : integer;
    obj       : IFRE_DB_Object;
begin
  col       := GetDBConnection(input).Collection('COLL_TEST_A2');
  upd_guids :=  GFRE_DBI.StringArray2GuidArray(input.Field('SELECTED').AsStringArr);
  for i:=0 to high(upd_guids) do begin
    if col.Fetch(upd_guids[i],obj) then begin
      if not obj.IsA(TFRE_DB_TEST_A.ClassName) then begin
        writeln('FETCHED OBJ ',obj.UID_String,' IS BAD!');
      end else begin
        obj.Field('string').AsString:='UPD:'+DateTimeToStr(Now);
        case random(4) of
          0 : obj.Field('status').AsString:='OK';
          1 : obj.Field('status').AsString:='WARNING';
          2 : obj.Field('status').AsString:='FAILURE';
          3 : obj.Field('status').AsString:='UNKNOWN';
        end;
        GetDBConnection(input).Update(obj);
      end;
    end else begin
      writeln('Could not fetch : '+GFRE_BT.GUID_2_HexString(upd_guids[i]));
    end;
  end;
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_UpdateCS(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
  html   : TFRE_DB_HTML_DESC;
begin
  Result:=GFRE_DB_NIL_DESC;
  session := GetSession(input);
  html:=TFRE_DB_HTML_DESC.create.Describe('SEAS ' + IntToStr(Random(1000)));
  //html.updateId:='GRID2_LAYOUT_content';
  html.updateId:='GRID2_HTML';
  session.SendServerClientRequest(html);
end;

{ TFRE_DB_TEST_APP_GRID_MOD }

procedure TFRE_DB_TEST_APP_GRID_MOD.ToggleBlast(const session: TFRE_DB_UserSession);
begin
  session.GetSessionModuleData(ObjectName).Field('BLAST').AsBoolean := not session.GetSessionModuleData(ObjectName).Field('BLAST').AsBoolean;
  SetBlast(session);
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SetBlast(const session: TFRE_DB_UserSession);
begin
  if session.GetSessionModuleData(ObjectName).Field('BLAST').AsBoolean=true then begin
    session.RegisterTaskMethod(@PRC_UPDATE_TASK,25);
  end else begin
    session.RemoveTaskMethod;
  end;
end;

class procedure TFRE_DB_TEST_APP_GRID_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('GRID','$grid_description')
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SC_ChangeData_Result(const input: IFRE_DB_Object);
begin
  writeln('---------- CD RESULT : ',input.DumpToString());
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SC_ChangeData_Error(const input: IFRE_DB_Object);
begin
  writeln('---------- CD  TIMEOUT RESULT : ',input.DumpToString());
end;

function TFRE_DB_TEST_APP_GRID_MOD.PRC_UPDATE_TASK(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res          : TFRE_DB_UPDATE_STORE_DESC;
  entry        : IFRE_DB_Object;
  DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
  k            : integer;
  session      : TFRE_DB_UserSession;
begin
  session := GetSession(input);
  DC_Grid_Long := session.FetchDerivedCollection('COLL_TEST_A_DERIVED');
  k := DC_Grid_Long.ItemCount;
  if k>0 then begin
    entry := DC_Grid_Long.GetItem(random(k)).CloneToNewObject(); //TODO:CHRIS - We must reject updates for data which is unknown clientside
    entry.Field('number').AsUInt32 := random(1000);
    entry.Field('number_pb').AsUInt32 := random(1000);
    res:=TFRE_DB_UPDATE_STORE_DESC.create.Describe(DC_Grid_Long.CollectionName);
    res.addUpdatedEntry(entry);
    session.SendServerClientRequest(res);
  end;
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid      : IFRE_DB_SIMPLE_TRANSFORM;

begin
  inherited;
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme    ('number','','Number',dt_number);
      AddProgressTransform ('number_pb','','Number PB','number_pb','',1000);
      AddOneToOnescheme    ('string','','String');
      AddOneToOnescheme    ('boolean','','Boolean',dt_boolean);
      AddOneToOnescheme    ('date','','Date',dt_date);
      AddOneToOnescheme    ('icon','','Icon',dt_icon);
    end;

    DC_Grid_Long := session.NewDerivedCollection('COLL_TEST_A_DERIVED');
    with DC_Grid_Long do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_A'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Details],'',TFRE_DB_StringArray.create('objname'),'',nil,CSF(@IMI_GRID_ITEM_DETAILS));
    end;
    if not session.GetSessionModuleData(ObjectName).FieldExists('BLAST') then begin
      session.GetSessionModuleData(ObjectName).Field('BLAST').AsBoolean:=false;
    end;
    SetBlast(session);
  end;
end;

function TFRE_DB_TEST_APP_GRID_MOD.GetToolbarMenu: TFRE_DB_CONTENT_DESC;
var
  submenu,menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('LOGOUT','',TFRE_DB_SERVER_FUNC_DESC.create.Describe('FIRMOS','LOGOUT'));
  menu.AddEntry.Describe('SEAS1','',CSF(@IMI_HelloWorld));
  menu.AddEntry.Describe('SEAS2','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  submenu:=menu.AddMenu.Describe('SEAS3','images_apps/test/cog.png');
  submenu.AddEntry.Describe('SEAS4','',CSF(@IMI_HelloWorld));
  submenu.AddEntry.Describe('SEAS5','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  submenu:=submenu.AddMenu.Describe('SEAS6','');
  submenu.AddEntry.Describe('SEAS7','',CSF(@IMI_HelloWorld));
  submenu.AddEntry.Describe('SEAS8','images_apps/test/cog.png',CSF(@IMI_HelloWorld));
  Result:=menu;
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_ToggleUpdates(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  ToggleBlast(GetSession(input));
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var lvd_Grid     : TFRE_DB_VIEW_LIST_DESC;
    DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
begin
  DC_Grid_Long := GetSession(input).FetchDerivedCollection('COLL_TEST_A_DERIVED');
  lvd_Grid := DC_Grid_Long.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button with TT','Tooltip');
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button wo TT');
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button sSel','Enabled on single Select',fdgbd_single);
  lvd_Grid.AddButton.Describe(CSF(@IMI_HelloWorld),'images_apps/test/add.png','Button mSel','Enabled on any Selection',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CSF(@IMI_Dialog),'images_apps/test/add.png','Dialog');
  lvd_Grid.AddButton.Describe(CSF(@IMI_ReadOnlyDialog),'images_apps/test/add.png','Read only Dialog');
  lvd_Grid.AddButton.Describe(CSF(@IMI_ChangeData),'images_apps/test/add.png','Change Dataset');
  lvd_Grid.AddButton.Describe(CSF(@IMI_ToggleUpdates),'images_apps/test/add.png','ToggleUpdates');
  lvd_Grid.AddButton.Describe(CSF(@IMI_SendADialog),'images_apps/test/add.png','ServerClientDialog');
  result := lvd_Grid;
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_HelloWorld(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World',fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_ChangeData(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res          : TFRE_DB_UPDATE_STORE_DESC;
  entry        : IFRE_DB_Object;
  DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
  k            : integer;
  session      : TFRE_DB_UserSession;
begin
  result := GFRE_DB_NIL_DESC;

  session := GetSession(input);
  DC_Grid_Long := session.FetchDerivedCollection('COLL_TEST_A_DERIVED');
  k := DC_Grid_Long.ItemCount;
  entry := DC_Grid_Long.First.CloneToNewObject();
  entry.Field('number').AsUInt32 := random(1000);

  res:=TFRE_DB_UPDATE_STORE_DESC.create.Describe(DC_Grid_Long.CollectionName);
  res.addUpdatedEntry(entry);
  session.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_Dialog(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_DIALOG_DESC;
  g     : TFRE_DB_INPUT_GROUP_DESC;
  store : TFRE_DB_STORE_DESC;
  i     : Integer;
begin
  store:=TFRE_DB_STORE_DESC.create.Describe();
  for i := 0 to 10 - 1 do begin
    store.AddEntry.Describe('C'+IntToStr(i),'v'+IntToStr(i));
  end;

  res:=TFRE_DB_DIALOG_DESC.create.Describe('Dialog');
  g:=res.AddGroup.Describe('MAIN');
  g.AddInput.Describe('Text','text',true);
  g.AddBool.Describe('Bool','bool');
  g.AddDate.Describe('Date','date');
  g.AddNumber.Describe('Number','number');
  g.AddNumber.Describe('Real','real1',false,false,false,false,'',2);
  g.AddChooser.Describe('Chooser','chooser1',store);
  g.AddChooser.Describe('Chooser CHECK','chooser2',store,false,dh_chooser_check);
  g.AddChooser.Describe('Chooser RADIO','chooser3',store,true,dh_chooser_radio);
  g.AddRecurrence.Describe('Recurrence','recurrence');

  g:=res.AddGroup.Describe('GROUP COLLAPSIBLE',true,false);
  g.AddInput.Describe('Text','g2.text');
  g.AddBool.Describe('Bool','g2.bool');
  g.AddDate.Describe('Date','g2.date');
  g.AddNumber.Describe('Number','g2.number');
  g.AddNumber.Describe('Real','g2.real',false,false,false,false,'',2);
  g.AddChooser.Describe('Chooser','g2.chooser1',store);
  g.AddChooser.Describe('Chooser CHECK','g2.chooser2',store,false,dh_chooser_check);
  g.AddChooser.Describe('Chooser RADIO','g2.chooser3',store,true,dh_chooser_radio);

  g:=res.AddGroup.Describe('GROUP COLLAPSED - GROUP REQUIRED',true,true);
  g.AddInput.Describe('Text','g3.text',false,true);
  g.AddBool.Describe('Bool','g3.bool',false,true);
  g.AddDate.Describe('Date','g3.date',false,true);
  g.AddNumber.Describe('Number','g3.number',false,true);
  g.AddNumber.Describe('Real','g3.real',false,true,false,false,'',2);
  g.AddChooser.Describe('Chooser','g3.chooser3',store,true,dh_chooser_combo,false,true);
  g.AddChooser.Describe('Chooser CHECK','g3.chooser3',store,false,dh_chooser_check,false,true);

  res.AddButton.Describe('Save',CSF(@IMI_HelloWorld),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_ReadOnlyDialog(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res  : TFRE_DB_DIALOG_DESC;
  g    : TFRE_DB_INPUT_GROUP_DESC;
  store: TFRE_DB_STORE_DESC;
  i    : Integer;
begin
  store:=TFRE_DB_STORE_DESC.create.Describe();
  for i := 0 to 10 - 1 do begin
    store.AddEntry.Describe('C'+IntToStr(i),'v'+IntToStr(i));
  end;

  res:=TFRE_DB_DIALOG_DESC.create.Describe('Dialog',0,0,true,true,true,false);
  g:=res.AddGroup.Describe('MAIN');
  g.AddInput.Describe('Text','text',true,false,false,false,'Text');
  g.AddBool.Describe('Bool','bool',false,false,false,true);
  g.AddDate.Describe('Date','date',false,false,false,false,'1344937865215');
  g.AddNumber.Describe('Number','number',false,false,false,false,'3');
  g.AddNumber.Describe('Real','real1',false,false,false,false,'3.5',2);
  g.AddChooser.Describe('Chooser','chooser1',store,false,dh_chooser_combo,false,false,false,'v4');
  g.AddChooser.Describe('Chooser CHECK','chooser2',store,false,dh_chooser_check,false,false,false,'v5');
  g.AddChooser.Describe('Chooser RADIO','chooser3',store,true,dh_chooser_radio,false,false,false,'v6');

  g:=res.AddGroup.Describe('GROUP COLLAPSIBLE',true,false);
  g.AddInput.Describe('Text','g2.text');
  g.AddBool.Describe('Bool','g2.bool');
  g.AddDate.Describe('Date','g2.date');
  g.AddNumber.Describe('Number','g2.number');
  g.AddNumber.Describe('Real','g2.real',false,false,false,false,'',2);
  g.AddChooser.Describe('Chooser','g2.chooser1',store);
  g.AddChooser.Describe('Chooser CHECK','g2.chooser2',store,false,dh_chooser_check);
  g.AddChooser.Describe('Chooser RADIO','g2.chooser3',store,true,dh_chooser_radio);

  g:=res.AddGroup.Describe('GROUP COLLAPSED - GROUP REQUIRED',true,true);
  g.AddInput.Describe('Text','g3.text',false,true);
  g.AddBool.Describe('Bool','g3.bool',false,true);
  g.AddDate.Describe('Date','g3.date',false,true);
  g.AddNumber.Describe('Number','g3.number',false,true);
  g.AddNumber.Describe('Real','g3.real',false,true,false,false,'',2);
  g.AddChooser.Describe('Chooser','g3.chooser3',store,true,dh_chooser_combo,false,true);
  g.AddChooser.Describe('Chooser CHECK','g3.chooser3',store,false,dh_chooser_check,false,true);

  Result:=res;
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_GRID_ITEM_DETAILS(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe('<B>GUGUG</B><BR>SEAS');
end;

function TFRE_DB_TEST_APP_GRID_MOD.IMI_SendADialog(const input: IFRE_DB_Object): IFRE_DB_Object;
var res  : TFRE_DB_MESSAGE_DESC;
begin
  result := GFRE_DB_NIL_DESC;
  Res    := TFRE_DB_MESSAGE_DESC.create.Describe('Du wurdest ... ','... ausgeloggt!',fdbmt_info);
  GetSession(input).SendServerClientRequest(res);
end;

{ TFRE_DB_TEST_APP }

procedure TFRE_DB_TEST_APP.SetupApplicationStructure;
begin
  InitAppDesc('testapp','$description');
  AddApplicationModule(TFRE_DB_TEST_APP_WELCOME_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRID_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRID2_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_CHART_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_LIVE_CHART_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_VNC_MOD.create);
  //AddApplicationModule(TFRE_DB_TEST_APP_VM_CONTROLLER_MOD.Create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_EDITORS_MOD.create);
end;

function TFRE_DB_TEST_APP.InstallAppDefaults (const conn: IFRE_DB_SYS_CONNECTION): TFRE_DB_Errortype;
var old_version  : TFRE_DB_String;

    procedure _InstallAllDomains(const obj:IFRE_DB_Object);
    begin
      InstallSystemGroupsandRoles(conn,obj.Field('objname').asstring);
    end;

begin

  case _CheckVersion(conn,old_version) of
    NotInstalled : begin
                      _SetAppdataVersion(conn,_ActualVersion);

                      conn.ForAllDomains(@_InstallAllDomains);

                      CreateAppText(conn,'$description','Test App','Test App','Test App');
                      CreateAppText(conn,'$vnc_description','VNC Test','VNC Test','VNC Test');
                      CreateAppText(conn,'$welcome_description','Welcome Test','Welcome Test','Welcome Test');
                      CreateAppText(conn,'$gtf_description','Grid Tree Form Test','Grid Tree Form Test','Grid Tree Form Test');
                      CreateAppText(conn,'$edit_description','Editors','Editors','Editors');
                      CreateAppText(conn,'$chart_description','Chart Test','Chart Test','Chart Test');
                      CreateAppText(conn,'$live_chart_description','Live Chart Test','Live Chart Test','Live Chart Test');
                      CreateAppText(conn,'$grid_description','Grid Test','Grid Test','Grid Test');
                      CreateAppText(conn,'$grid2_description','Grid 2 Test','Grid 2 Test','Grid 2 Test');
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

function TFRE_DB_TEST_APP.InstallSystemGroupsandRoles(const conn: IFRE_DB_SYS_CONNECTION; const domain: TFRE_DB_NameType): TFRE_DB_Errortype;
var admin_app_rg : IFRE_DB_ROLE;
     user_app_rg : IFRE_DB_ROLE;
    guest_app_rg : IFRE_DB_ROLE;
begin
  if domain=cSYS_DOMAIN then begin
    writeln('Install for Domain:',domain);

    admin_app_rg  := _CreateAppRole('ADMIN','TESTAPP ADMIN','Test App Administration Rights');
    user_app_rg   := _CreateAppRole('USER','TESTAPP USER','Test App Default User Rights');
    guest_app_rg  := _CreateAppRole('GUEST','TESTAPP GUEST','Test App Default Guest User Rights');
    _AddAppRight(admin_app_rg ,'ADMIN'  ,'TESTAPP Admin','Administration of Test APP');
    _AddAppRight(user_app_rg  ,'START'  ,'TESTAPP Start','Startup of Test APP');

    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['grid']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['grid2']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['chart']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['live_chart']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['vnc']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['vmcontroller']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['tgf']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['edit']));

    _AddAppRight(guest_app_rg ,'START','TESTAPP Start','Startup of Test APP'); // Guests are allowed to START the app
    _AddAppRightModules(guest_app_rg,GFRE_DBI.ConstructStringArray(['welcome']));

    conn.StoreRole(ObjectName,domain,admin_app_rg);
    conn.StoreRole(ObjectName,domain,user_app_rg);
    conn.StoreRole(ObjectName,domain,guest_app_rg);

    _AddSystemGroups(conn,domain);
  end;
end;

procedure TFRE_DB_TEST_APP._UpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main','Main','images_apps/test/sitemap_icon.svg','',0,true);
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Welcome','Welcome','images_apps/test/sitemap_icon.svg','WELCOME',0,CheckAppRightModule(conn,'welcome'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Grid','Grid','images_apps/test/sitemap_icon.svg','GRID',1,CheckAppRightModule(conn,'grid'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Chart','Chart','images_apps/test/sitemap_icon.svg','CHART',4,CheckAppRightModule(conn,'chart'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Live_Chart','Live Chart','images_apps/test/sitemap_icon.svg','LIVE_CHART',4,CheckAppRightModule(conn,'live_chart'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Grid2','Grid2','images_apps/test/sitemap_icon.svg','GRID2',5,CheckAppRightModule(conn,'grid2'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/VNC','VNC','images_apps/test/sitemap_icon.svg','VNC',2,CheckAppRightModule(conn,'vnc'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/TGF','TreeGridForm','images_apps/test/sitemap_icon.svg','TGF',0,CheckAppRightModule(conn,'tgf'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/EDIT','Editors','images_apps/test/sitemap_icon.svg','EDIT',0,CheckAppRightModule(conn,'edit'));
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  session.GetSessionAppData(ObjectName).Field('SITEMAP').AsObject := SiteMapData;
end;

procedure TFRE_DB_TEST_APP.MySessionInitialize(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitialize(session);
  if session.IsInteractiveSession then begin
    _UpdateSitemap(session);
  end;
end;

procedure TFRE_DB_TEST_APP.MySessionPromotion(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitialize(session);
  if session.IsInteractiveSession then begin
    _UpdateSitemap(session);
  end;
end;

function TFRE_DB_TEST_APP.CFG_ApplicationUsesRights: boolean;
begin
  Result := true;
end;

function TFRE_DB_TEST_APP._ActualVersion: TFRE_DB_String;
begin
  Result:='1.0';
end;


class procedure TFRE_DB_TEST_APP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
  scheme.AddSchemeFieldSubscheme('gridmod'        , 'TFRE_DB_TEST_APP_GRID_MOD');
  scheme.AddSchemeFieldSubscheme('grid2mod'       , 'TFRE_DB_TEST_APP_GRID2_MOD');
  scheme.AddSchemeFieldSubscheme('chartmod'       , 'TFRE_DB_TEST_APP_CHART_MOD');
  scheme.AddSchemeFieldSubscheme('livechartmod'   , 'TFRE_DB_TEST_APP_LIVE_CHART_MOD');
  scheme.AddSchemeFieldSubscheme('vncmod'         , 'TFRE_DB_TEST_APP_VNC_MOD');
  scheme.AddSchemeFieldSubscheme('gridtreeformmod', 'TFRE_DB_TEST_APP_GRIDTREEFORM_MOD');
  scheme.AddSchemeFieldSubscheme('editors'        , 'TFRE_DB_TEST_APP_EDITORS_MOD');
end;

function TFRE_DB_TEST_APP.IMI_Messages(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Messages',true,false);
  res.AddInput.Describe('Messages','messages',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.IMI_News(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('News',true,false);
  res.AddInput.Describe('News','news',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.IMI_Calendar(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Calendar',true,false);
  res.AddInput.Describe('Calendar','calendar',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.IMI_Profile(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Profile',true,false);
  res.AddInput.Describe('Profile','profile',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.IMI_Dialog(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Message','GUGUG',fdbmt_info);
end;

function TFRE_DB_TEST_APP.IMI_RAW_DATA_FEED(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  writeln('GOT RAW INPUT: ',input.DumpToString());
  result := GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_A }

class procedure TFRE_DB_TEST_A.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('number',fdbft_UInt32);
  scheme.AddSchemeField         ('number_pb',fdbft_UInt32);
  scheme.AddSchemeField         ('string',fdbft_String);
  scheme.AddSchemeField         ('boolean',fdbft_Boolean);
  scheme.AddSchemeField         ('status',fdbft_String);
  scheme.AddCalculatedField     ('icon','GetIcon',cft_OnStoreUpdate);
end;

function TFRE_DB_TEST_A.IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
var    lstatus_icon : TFRE_DB_String;
       lstatus      : TFRE_DB_String;
begin
  lstatus    := Field('status').AsString;
  case lstatus of
    'OK'      : lstatus_icon := getThemedResource('images_apps/test/signal_ok.png');
    'WARNING' : lstatus_icon := getThemedResource('images_apps/test/signal_warning.png');
    'FAILURE' : lstatus_icon := getThemedResource('images_apps/test/signal_failure.png');
    'UNKNOWN' : lstatus_icon := getThemedResource('images_apps/test/signal_unknown.png');
    'NEW'     : lstatus_icon := getThemedResource('images_apps/test/signal_unknown.png');
    else raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN ENUM FIELD VALUE SiGNaL Status');
  end;
  result := GFRE_DBI.NewObject;
  result.Field(CalcFieldResultKey(fdbft_String)).AsString:=lstatus_icon;
end;


procedure CreateTestdata(const dbname: string; const user, pass: string);
var CONN    : IFRE_DB_CONNECTION;
    COLL    : IFRE_DB_COLLECTION;
    lobj    : IFRE_DB_Object;
    i,t1,t2 : Integer;
begin
  t1 := GFRE_BT.Get_Ticks_ms;

  CONN := GFRE_DBI.NewConnection;
  CONN.Connect(dbname,'admin'+'@'+cSYS_DOMAIN,'admin');

  COLL := CONN.Collection('COLL_TEST_A');
  for i := 0 to 100 - 1 do begin
    lobj := CONN.NewObject('TFRE_DB_TEST_A');
    lobj.Field('number').AsUInt32:=i;
    lobj.Field('number_pb').AsUInt32:=i * 10;
    lobj.Field('string').AsString:='String_' + IntToStr(i);
    lobj.Field('boolean').AsBoolean:=(i mod 2)=0;
    lobj.Field('date').AsDateTime:=GFRE_DT.Now_UTC - i*10000;
    case i mod 4 of
      0: lobj.Field('status').AsString:='OK';
      1: lobj.Field('status').AsString:='WARNING';
      2: lobj.Field('status').AsString:='FAILURE';
      3: lobj.Field('status').AsString:='UNKNOWN';
    end;
    COLL.Store(lobj);
  end;
  COLL := CONN.Collection('COLL_TEST_A2');
  for i := 0 to 250 - 1 do begin
    if i mod 100=0 then writeln('ENDLESS ',i);
    lobj := CONN.NewObject('TFRE_DB_TEST_A');
    lobj.Field('number').AsUInt32:=i*10;
    lobj.Field('number_pb').AsUInt32:=i*10;
    lobj.Field('string').AsString:='String_' + IntToStr(i);
    lobj.Field('boolean').AsBoolean:=(i mod 2)=0;
    lobj.Field('date').AsDateTime:=GFRE_DT.Now_UTC - i*10000;
    case i mod 4 of
      0: lobj.Field('status').AsString:='OK';
      1: lobj.Field('status').AsString:='WARNING';
      2: lobj.Field('status').AsString:='FAILURE';
      3: lobj.Field('status').AsString:='UNKNOWN';
    end;
    COLL.Store(lobj);
  end;

  COLL := CONN.Collection('COLL_TEST_B');
  for i := 0 to 100 - 1 do begin
    lobj := CONN.NewObject('TFRE_DB_TEST_B');
    lobj.Field('firstname').AsString:='FN_' + IntToStr(i);
    lobj.Field('lastname').AsString:='LN_' + IntToStr(i);
    lobj.Field('icon').AsString:=getThemedResource('images_apps/test/add.png');
    COLL.Store(lobj);
  end;

  CONN.Finalize;

  t2 := GFRE_BT.Get_Ticks_ms;
  writeln('TIME ',t2-t1);
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_A);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_B);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRID_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRID2_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_CHART_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_LIVE_CHART_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_VNC_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRIDTREEFORM_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_EDITORS_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_WELCOME_MOD);
  //GFRE_DBI.RegisterObjectClassEx(TFRE_DBCOREBOX_VM_MACHINES_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP);
  GFRE_DBI.Initialize_Extension_Objects;
end;

procedure MetaRegister_Test;
begin
  Register_DB_Extensions;
  FRE_DBBUSINESS.Register_DB_Extensions;
end;

procedure MetaInitializeDatabase_Test(const dbname: string; const user, pass: string);
begin
  InitializeTestapp(dbname,user,pass);
  CreateTestdata(dbname,user,pass);
end;

procedure MetaRemove_Test(const dbname: string; const user, pass: string);
var conn  : IFRE_DB_SYS_CONNECTION;
     res   : TFRE_DB_Errortype;
     i     : integer;
     login : string;
begin
   CONN := GFRE_DBI.NewSysOnlyConnection;
   try
     res  := CONN.Connect('admin','admin');
     if res<>edb_OK then gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);
     conn.RemoveApp('TESTAPP');
   finally
     conn.Finalize;
   end;
end;


initialization
  GFRE_DBI_REG_EXTMGR.RegisterNewExtension('TEST',@MetaRegister_Test,@MetaInitializeDatabase_Test,@MetaRemove_Test);
end.

