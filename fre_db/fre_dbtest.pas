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
  Classes, SysUtils,FOS_TOOL_INTERFACES,unixutil,fre_system,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE,
  FRE_DBBUSINESS,typinfo,
  fre_accesscontrol_common;

procedure MetaRegister_Test;
procedure MetaInitializeDatabase_Test(const dbname :string; const user,pass:string);

procedure Register_DB_Extensions;
procedure CreateTestdata(const dbname: string; const user, pass: string);

type

  { TFRE_DB_TEST_A }

  TFRE_DB_TEST_A=class(TFRE_DB_ObjectEx)
  private
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects(const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    procedure   CALC_CalcIconStatus(const calc : IFRE_DB_CALCFIELD_SETTER);
  end;

  { TFRE_DB_TEST_B }

  TFRE_DB_TEST_B=class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects(const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function        WEB_Content         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_ChildrenData    (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_ALL_TYPES }

  TFRE_DB_TEST_ALL_TYPES=class(TFRE_DB_ObjectEx)
  protected
    class procedure InstallDBObjects(const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure   Gamble(const id:int64);
    procedure   CALC_Uint32 (const calc : IFRE_DB_CALCFIELD_SETTER);
    procedure   CALC_String (const calc : IFRE_DB_CALCFIELD_SETTER);
  published
    function  WEB_GetIcon   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Content   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


  { TFRE_DB_TEST_FILEDIR }

  TFRE_DB_TEST_FILEDIR=class(TFRE_DB_ObjectEx)
  protected
    class procedure InstallDBObjects(const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure InternalSetup; override;
    procedure SetIsFile     (const isfile:boolean);
    function  GetIsFile     : Boolean;
  public
    procedure SetProperties (const name : TFRE_DB_String ; const is_file : boolean; const size : NativeInt ; const mode : Cardinal; const time : Longint);
    function  FileDirName   : String;
  published
    function  WEB_Content       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Menu          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_CreateZip     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_CHILDRENDATA  (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_FILE }

  TFRE_DB_TEST_FILE=class(TFRE_DB_TEST_FILEDIR)
  end;

type

  { TFRE_DB_TEST_APP }

  TFRE_DB_TEST_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;

    class procedure  InstallDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;  { system specific data inits go here}
    class procedure  InstallDBObjects4Domain     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); override;                   { system domain specific data inits go here}
    class procedure  InstallUserDBObjects        (const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType); override;                                         { app db specific data inits goes here }
    class procedure  InstallUserDBObjects4Domain (const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID); override;                       { app db domain specific data inits goes here }

    class procedure  RemoveDBObjects            (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType); override;
    class procedure  RemoveDBObjects4Domain     (const conn:IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID : TGUID); override;

    procedure       _UpdateSitemap            (const session: TFRE_DB_UserSession);
  protected
    procedure       MyServerInitialize        (const admin_dbc: IFRE_DB_CONNECTION); override; { initialize in memory collections here }
    procedure       MySessionInitialize       (const session: TFRE_DB_UserSession); override;
    procedure       MySessionPromotion        (const session: TFRE_DB_UserSession); override;
  public
    class procedure RegisterSystemScheme        (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function        WEB_Messages         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_News             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_Calendar         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_Profile          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        WEB_Dialog           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function        IMI_RAW_DATA_FEED    (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_WELCOME_MOD }

  TFRE_DB_TEST_APP_WELCOME_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_FORMTEST_MOD }

  TFRE_DB_TEST_APP_FORMTEST_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
    function        GetToolbarMenu          (const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_UPDATE_FORM_DBO       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_UPDATE_FORM_BYTE_FIELD(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_ALLGRID_MOD }

  TFRE_DB_TEST_APP_ALLGRID_MOD= class(TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    procedure       MySessionInitializeModule  (const session: TFRE_DB_UserSession); override;
  published
    function  WEB_Content                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD }

  TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD= class(TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
    function        GetToolbarMenu       (const ses : IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; override;
  public
    class procedure RegisterSystemScheme       (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    procedure       MySessionInitializeModule  (const session: TFRE_DB_UserSession); override;
  published
    function  WEB_Content                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_SliderChanged          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_DropAction             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_FeederTest             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_FeederTestTimeout      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_FeederTestError        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;



  { TFRE_DB_TEST_APP_GRID_MOD }

  TFRE_DB_TEST_APP_GRID_MOD = class (TFRE_DB_APPLICATION_MODULE)
  private
    _idx: Integer;
    procedure ToggleBlast (const session:TFRE_DB_UserSession);
    procedure SetBlast    (const session:TFRE_DB_UserSession);
    procedure updateWait  (const session: IFRE_DB_UserSession);
  protected
    class procedure RegisterSystemScheme    (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    function        GetToolbarMenu       (const ses : IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; override;
  public
    procedure SC_ChangeData_Result      (const input:IFRE_DB_Object);
    procedure SC_ChangeData_Error       (const input:IFRE_DB_Object);
    procedure PRC_UPDATE_TASK            (const ses: IFRE_DB_Usersession);
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  WEB_ToggleUpdates         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HelloWorld            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_OpenFIRMOS            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_OpenFIRMOSHere        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Reload                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ChangeData            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Dialog                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ReadOnlyDialog        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_GRID_ITEM_DETAILS     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_SendADialog           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_WaitMessage           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_AbortWait             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_GRID2_MOD }

  TFRE_DB_TEST_APP_GRID2_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  GetToolbarMenu            (const ses : IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; override;
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  IMI_HelloWorld            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  WEB_HelloWorld            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Menu                  (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_GRID_ITEM_DETAILS     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_AddRecordBefore       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_AddRecordAfter        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_DelRecord             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_UpdRecord             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_UpdateCS              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_CHART_MOD }

  TFRE_DB_TEST_APP_CHART_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    procedure       MyServerInitializeModule  (const admin_dbc : IFRE_DB_CONNECTION); override;
    procedure       MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
  published
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ContentPie            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_LIVE_CHART_MOD }

  TFRE_DB_TEST_APP_LIVE_CHART_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    _idx: Integer;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    procedure       _SendDataSLC        (const ses: IFRE_DB_Usersession);
    procedure       _SendDataLC         (const ses: IFRE_DB_Usersession);
    procedure       _SendDataCC         (const ses: IFRE_DB_Usersession);
  published
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ContentSL             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ContentL              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_ContentC              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_StartStopSLC          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_StartStopLC           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_StartStopCC           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_GRIDTREEFORM_MOD }

  TFRE_DB_TEST_APP_GRIDTREEFORM_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_EDITORS_MOD }

  TFRE_DB_TEST_APP_EDITORS_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HtmlEditor            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HtmlEditorLoad        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HtmlEditorSave        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HtmlEditorStart       (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_HtmlEditorStop        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_JSEditor              (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_JSEditorLoad          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_JSEditorSave          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_JSEditorStart         (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_JSEditorStop          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_PascalEditor          (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_PascalEditorLoad      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_PascalEditorSave      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_PascalEditorStart     (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_PascalEditorStop      (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_APP_SVG_MOD }

  TFRE_DB_TEST_APP_SVG_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure       SetupAppModuleStructure ; override;
    function        GetToolbarMenu          (const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC; override;
  published
    procedure MySessionInitializeModule (const session : TFRE_DB_UserSession);override;
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_UpdateSVG             (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

implementation

procedure GenerateTestData(const dbname: string; const user, pass: string);

type _tusertype = (utguest,utuser,utadmin,utdemo);

var conn    : IFRE_DB_CONNECTION;
    res     : TFRE_DB_Errortype;
    i       : integer;
    domainId: TGuid;
    group   : IFRE_DB_GROUP;
    userObj : IFRE_DB_USER;

  procedure _AddUser(const user: string; const domain : TGuid; const usertype:_tusertype;firstname:string='';lastname: string='';passwd : string='');
  var ims    : TFRE_DB_Stream;
  begin
    if passwd='' then begin
      case usertype of
       utadmin: passwd:='a';
       utuser:  passwd:='u';
       utdemo:  passwd:='demo1234';
      end;
    end;
    if lastname='' then lastname:='Lastname '+user;
    if firstname='' then firstname:='Firstname '+user;

    if conn.sys.UserExists(user,domain) then CheckDbResult(conn.sys.DeleteUser(user,domain),'cannot delete user '+user);
    CheckDbResult(conn.sys.AddUser(user,domain,passwd,firstname,lastname),'cannot add user '+user);

    //ims := TFRE_DB_Stream.Create;
    //ims.LoadFromFile(cFRE_SERVER_WWW_ROOT_DIR+'/fre_css/'+ cFRE_WEB_STYLE + '/images/LOGIN.png');
    //conn.ModifyUserImage(login,ims);
  end;

begin
  CONN := GFRE_DBI.NewConnection;
  try
    res  := CONN.Connect(dbname,user,pass);
    if res<>edb_OK then
      gfre_bt.CriticalAbort('cannot connect system : %s',[CFRE_DB_Errortype[res]]);

    if conn.sys.DomainExists('firmos') then
      CheckDbResult(conn.sys.DeleteDomain('firmos'),'cannot delete domain firmos');
    if conn.sys.DomainExists('fpc') then
      CheckDbResult(conn.sys.DeleteDomain('fpc'),'cannot delete domain fpc');
    if conn.sys.DomainExists('demo') then
      CheckDbResult(conn.sys.DeleteDomain('demo'),'cannot delete domain demo');

    CheckDbResult(conn.AddDomain('firmos','FirmOS Domain','FirmOS Domain'),'cannot add domain firmos');
    CheckDbResult(conn.AddDomain('fpc','FPC Domain','FPC Domain'),'cannot add domain fpc');
    CheckDbResult(conn.AddDomain('demo','Demo Domain','Demo Domain'),'cannot add domain demo');

    domainId:=conn.sys.DomainID(CFRE_DB_SYS_DOMAIN_NAME);
    _AddUser('admin1',domainId,utadmin);
    _AddUser('admin2',domainId,utadmin);
    _AddUser('city',domainId,utadmin,'','','city');

    _AddUser('user1',domainId,utuser);
    _AddUser('user2',domainId,utuser);

    _AddUser('demo1',domainId,utdemo);
    _AddUser('demo2',domainId,utdemo);

    domainId:=conn.sys.DomainID('demo');
    _AddUser('myadmin',domainId,utadmin);
    _AddUser('user1',domainId,utuser);
    _AddUser('user2',domainId,utuser);

    domainId:=conn.sys.DomainID('fpc');
    _AddUser('myadmin',domainId,utadmin);
    _AddUser('user1',domainId,utuser);
    _AddUser('user2',domainId,utuser);

    domainId:=conn.sys.DomainID('firmos');
    _AddUser('myadmin',domainId,utadmin);
    _AddUser('user1',domainId,utuser);
    _AddUser('user2',domainId,utuser);
    _AddUser('hhartl',domainId,utadmin,'Helmut','Hartl');
    _AddUser('fschober',domainId,utadmin,'Franz','Schober');
    _AddUser('ckoch',domainId,utadmin,'Christian','Koch');



    domainId:=conn.sys.DomainID(CFRE_DB_SYS_DOMAIN_NAME);

    CheckDbResult(conn.sys.AddUser('testfeeder',domainId,'x','testfeeder','testfeeder'),'cannot add user testfeeder');
    CheckDbResult(conn.sys.FetchGroup('TESTFEEDER',domainId,group));
    CheckDbResult(conn.sys.FetchUser('testfeeder',domainId,userObj));
    CheckDbResult(conn.sys.ModifyUserGroupsById(userObj.UID,TFRE_DB_GUIDArray.Create(group.UID),true),'cannot set user groups testfeeder');



    //abort;
    //for i:= 1 to 3 do begin
    //    login  := 'admin'+inttostr(i)+'@'+CFRE_DB_SYS_DOMAIN_NAME;
    //    if conn.UserExists(login) then begin
    //      writeln('Modify Groups for User '+login);
    //      CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','ADMIN'+'@'+CFRE_DB_SYS_DOMAIN_NAME),Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+CFRE_DB_SYS_DOMAIN_NAME)]),true),'cannot set user groups '+login);
    //    end;
    //  end;
    //
    //  for i:= 1 to 3 do begin
    //    login  := 'user'+inttostr(i)+'@'+CFRE_DB_SYS_DOMAIN_NAME;
    //    if conn.UserExists(login) then begin
    //      writeln('Modify Groups for User '+login);
    //      CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+CFRE_DB_SYS_DOMAIN_NAME)]),true),'cannot set user groups '+login);
    //    end;
    //  end;
    //
    //login  := 'feeder@'+CFRE_DB_SYS_DOMAIN_NAME;
    //if conn.UserExists(login) then begin
    //  writeln('Modify Groups for User '+login);
    //  CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','ADMIN'+'@'+CFRE_DB_SYS_DOMAIN_NAME),Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+CFRE_DB_SYS_DOMAIN_NAME)]),true),'cannot set user groups '+login);
    //end;

    CreateTestdata(dbname,user,pass);

  finally
    conn.Finalize;
  end;
end;

{ TFRE_DB_TEST_APP_SVG_MOD }

class procedure TFRE_DB_TEST_APP_SVG_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_SVG_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$svg_description')
end;

function TFRE_DB_TEST_APP_SVG_MOD.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
var
  menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('Update SVG','',CWSF(@WEB_UpdateSVG));
  Result:=menu;
end;

procedure TFRE_DB_TEST_APP_SVG_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitializeModule(session);
end;

function TFRE_DB_TEST_APP_SVG_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  try
    Result:=TFRE_DB_SVG_DESC.create.Describe(GFRE_BT.StringFromFile(cFRE_SERVER_WWW_ROOT_DIR+PathDelim+'testfiles/test.svg'),'test_svg');
  except
    on E:Exception do begin
      Result:=TFRE_DB_HTML_DESC.create.Describe('Error on reading the svg test file. Please place a valid test.svg file into the binary directoy.');
    end;
  end;
end;

function TFRE_DB_TEST_APP_SVG_MOD.WEB_UpdateSVG(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var r,g,b : Byte;
    style : string;
begin
  r := random(255);
  g := random(255);
  b := random(255);
  style := 'fill:#'+hexStr(r,2)+hexStr(g,2)+hexStr(b,2);
  Result:=TFRE_DB_UPDATE_SVG_DESC.create.Describe('test_svg','Alpha','style',style);
end;

{ TFRE_DB_TEST_FILEDIR }

class procedure TFRE_DB_TEST_FILEDIR.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId := '1.0';
end;

class procedure TFRE_DB_TEST_FILEDIR.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_TEST_FILEDIR.InternalSetup;
begin
  inherited InternalSetup;
  Field('isfile').AsBoolean:=true;
  Field('mypath').AsString :='/';
end;

procedure TFRE_DB_TEST_FILEDIR.SetIsFile(const isfile: boolean);
begin
  Field('isfile').AsBoolean := isfile;
  if isfile then begin
    Field('children').AsString:='';
  end else begin
    Field('children').AsString:='UNCHECKED';
  end;
end;

function TFRE_DB_TEST_FILEDIR.GetIsFile: Boolean;
begin
  result := Field('isfile').AsBoolean;
end;

procedure TFRE_DB_TEST_FILEDIR.SetProperties(const name: TFRE_DB_String; const is_file: boolean; const size: NativeInt; const mode: Cardinal; const time: Longint);
var
    y, mon, d, h, min, s: word;
    fosdt : TFRE_DB_DateTime64;
    icon  : String;
    hrtype: String;

   procedure mimeTypeToIconAndHRType(const mt: String; var icon: String; var hrtype: String);
   var
     mtp: TFRE_DB_StringArray;
   begin
     GFRE_BT.SeperateString(LowerCase(mt),'/',mtp);
     icon:='images_apps/test/file.png';
     hrtype:='Unknown';
     case mtp[0] of
       'audio': begin
                  icon:='images_apps/test/audio-basic.png';
                  hrtype:='Audio';
                end;
       'video': begin
                  icon:='images_apps/test/video-x-generic-mplayer.png';
                  hrtype:='Video';
                end;
       'image': begin
                  hrtype:='Image';
                  case mtp[1] of
                    'bmp': icon:='images_apps/test/image-bmp.png';
                    'jpeg': icon:='images_apps/test/image-jpeg.png';
                    'tiff': icon:='images_apps/test/image-tiff.png';
                    'gif': icon:='images_apps/test/image-gif.png';
                    'png': icon:='images_apps/test/image-png.png';
                  end;
                end;
       'application': begin
                        hrtype:='Application file';
                        case mtp[1] of
                          'zip': icon:='images_apps/test/application-zip.png';
                          'pdf': icon:='images_apps/test/application-pdf.png';
                          'msword': icon:='images_apps/test/page-word.png';
                          'postscript': icon:='images_apps/test/application-postscript-2.png';
                          'rtf': icon:='images_apps/test/application-rtf.png';
                          'wordperfect5.1': icon:='images_apps/test/application-vnd.wordperfect-abiword.png';
                          'octet-stream': hrtype:='Unknown';
                        end;
                      end;
     end;
   end;

begin
  EpochToLocal(time,y,mon,d,h,min,s);
  Field('date').AsDateTime := GFRE_DT.EncodeTime(y,mon,d,h,min,s,0);
  Field('name').AsString   := name;
  Field('size').AsUInt64   := size;
  if is_file then begin
    Field('sizeHR').AsString := GFRE_BT.ByteToString(size);
    mimeTypeToIconAndHRType(FREDB_Filename2MimeType(name),icon,hrtype);
    Field('typeHR').AsString   := hrtype;
    Field('icon').AsString:=FREDB_getThemedResource(icon);
    Field('objectclass').AsString:='TFRE_DB_TEST_FILE';
  end else begin
    Field('typeHR').AsString   := 'Folder';
    Field('sizeHR').AsString := '';
    Field('icon').AsString:=FREDB_getThemedResource('images_apps/test/folder.png');
    Field('icon_open').AsString:=FREDB_getThemedResource('images_apps/test/folder-open.png');
    Field('objectclass').AsString:='TFRE_DB_TEST_FILEDIR';
  end;
  Field('mode').AsUInt32   := mode;
  SetIsFile(is_file);
end;

function TFRE_DB_TEST_FILEDIR.FileDirName: String;
begin
  result := Field('name').AsString;
end;

function TFRE_DB_TEST_FILEDIR.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
    res : TFRE_DB_STORE_DATA_DESC;
    obj : IFRE_DB_Object;
    i   : Integer;
begin
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_FILEDIR.WEB_Menu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  sel: TFRE_DB_String;
  opd: IFRE_DB_Object;
  inp: IFRE_DB_Object;

  procedure GotAnswer(const ses: IFRE_DB_UserSession; const new_input: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const ocid: Qword; const opaquedata: IFRE_DB_Object);
  var
    res: TFRE_DB_MENU_DESC;
    fd : TFRE_DB_TEST_FILEDIR;
  begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    if new_input.FieldExists('info') then begin
      fd:=new_input.Field('info').AsObject.Implementor_HC as TFRE_DB_TEST_FILEDIR;
      if fd.GetIsFile then begin
        res.AddEntry.DescribeDownload('Download','','/download'+opaquedata.Field('fileid').AsString);
        ses.SendServerClientAnswer(res,ocid);
      end else begin
        res.AddEntry.Describe('Create ZIP','',TFRE_DB_SERVER_FUNC_DESC.create.Describe('TFRE_DB_TEST_FILEDIR',opaquedata.Field('rootGUID').AsGUID,'CreateZip'));
        ses.SendServerClientAnswer(res,ocid);
      end;
    end else begin
      ses.SendServerClientAnswer(res,ocid);
      ses.SendServerClientRequest(TFRE_DB_MESSAGE_DESC.create.Describe('Error','File/Directory not found!',fdbmt_error));
    end;
  end;

begin
  opd := GFRE_DBI.NewObject;
  inp := GFRE_DBI.NewObject;
  inp.Field('fileid').AsString:=copy(input.Field('selected').AsString,1,Length(input.Field('selected').AsString)-1);
  opd.Field('fileid').AsString:=inp.Field('fileid').AsString;
  opd.Field('rootGUID').AsGUID:=UID;
  if ses.InvokeRemoteRequest('SAMPLEFEEDER','GETFILEDIRINFO',inp,@GotAnswer,opd)=edb_OK then begin
    result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
  end else begin
    result := TFRE_DB_STORE_DATA_DESC.create.Describe(0);
  end;
end;

function TFRE_DB_TEST_FILEDIR.WEB_CreateZip(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res : TFRE_DB_FORM_DIALOG_DESC;
begin
  Result:=GFRE_DB_NIL_DESC;
  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe('ZIP');
  res.AddDescription.Describe('','Your ZIP file is ready to download.');
  res.AddButton.DescribeDownload('Download','/download/test.zip',true);
  ses.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_FILEDIR.WEB_CHILDRENDATA(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var obj      : IFRE_DB_Object;
    inp      : IFRE_DB_Object;
    response : IFRE_DB_Object;
    opd      : IFRE_DB_Object;
    lvl      : String;

    procedure GotAnswer(const ses: IFRE_DB_UserSession; const new_input: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const ocid: Qword; const opaquedata: IFRE_DB_Object);
    var res      : TFRE_DB_STORE_DATA_DESC;
         i       : NativeInt;
         cnt     : NativeInt;
         newnew  : IFRE_DB_Object;

         procedure addEntry(const fld : IFRE_DB_Field);
         var mypath : string;
             entry  : IFRE_DB_Object;
         begin
           if fld.FieldType=fdbft_Object then
             begin
               inc(cnt);
               entry := fld.CheckOutObject;
               entry.Field('uidpath').AsStringArr := opaquedata.Field('UIP').AsStringArr;
               mypath                             := opaquedata.Field('LVL').AsString+ entry.Field('name').AsString +'/';
               entry.Field('mypath').AsString     := mypath;
               res.addTreeEntry(entry,entry.Field('isfile').AsBoolean=false);
             end;
         end;

    begin
      res:=TFRE_DB_STORE_DATA_DESC.create.Describe(0);
      cnt := 0;
      new_input.ForAllFields(@addEntry);
      res.Describe(cnt);
      ses.SendServerClientAnswer(res,ocid);
      opaquedata.Finalize;
    end;

begin
  //writeln('BROWSE CALL INPUT ',input.DumpToString());
  inp := GFRE_DBI.NewObject;
  lvl := input.Field('parentid').AsString;
  inp.Field('level').AsString:= lvl;

  opd := GFRE_DBI.NewObject;
  opd.Field('UIP').AsGUIDArr := self.GetUIDPathUA;
  opd.Field('LVL').AsString  := lvl;

  if ses.InvokeRemoteRequest('SAMPLEFEEDER','BROWSEPATH',inp,@GotAnswer,opd)=edb_OK then
    begin
      result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      exit;
    end
  else
    begin
      result := TFRE_DB_STORE_DATA_DESC.create.Describe(0);
      inp.Finalize;
      opd.Finalize;
    end;
end;

{ TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD }

procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$feedbrowsetree_description');
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
var
  submenu,menu : TFRE_DB_MENU_DESC;
  test         : IFRE_DB_Object;
  myuid          : TGuid;

begin
  test  := ses.GetDBConnection.GetCollection('COLL_TEST_AT').First;
  if assigned(test) then
    begin
      myuid := test.UID;
      test.Finalize;
    end;

  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('Feeder Request','',CWSF(@WEB_FeederTest));
  menu.AddEntry.Describe('Feeder Request (Timeout)','',CWSF(@WEB_FeederTestTimeout));
  menu.AddEntry.Describe('Feeder Request (Error)','',CWSF(@WEB_FeederTestError));
  menu.AddEntry.DescribeDownload('Download a File','','README.md');
  menu.AddEntry.DescribeDownload('Download from a DBO (Attachment)','',ses.GetDownLoadLink4StreamField(myuid,'fdbft_Stream',true,'x-safe-this/file-as-saveas','super_file.txt'));
  menu.AddEntry.DescribeDownload('Download from a DBO (Non Attachment)','',ses.GetDownLoadLink4StreamField(myuid,'fdbft_Stream',false,'application/octet-stream','super_file.txt'));
  Result:=menu;
end;

class procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  DC_Grid      : IFRE_DB_DERIVED_COLLECTION;
  tr_Grid      : IFRE_DB_SIMPLE_TRANSFORM;
  filedir      : TFRE_DB_TEST_FILEDIR;
  coll         : IFRE_DB_COLLECTION;
  conn         : IFRE_DB_CONNECTION;

begin
  inherited;
  if session.IsInteractiveSession then begin
    conn := session.GetDBConnection;
    if not conn.CollectionExists('CFBROWSER:'+session.GetLoginUserAsCollKey) then
      begin
         coll := conn.CreateCollection('CFBROWSER:'+session.GetLoginUserAsCollKey,true);
      end
    else
      begin
        coll := conn.GetCollection('CFBROWSER:'+session.GetLoginUserAsCollKey);
      end;
    if coll.Count=0 then
      begin
        filedir := TFRE_DB_TEST_FILEDIR.CreateForDB;
        filedir.SetProperties('Virtual Rooot',false,0,0,0);
        CheckDbResult(coll.Store(filedir),'Error creating root entry');
      end;
    DC_Grid := session.NewDerivedCollection('FILEBROWSER');
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme('name','','Name',dt_string,true,false,false,3,'icon','icon_open');
      AddOneToOnescheme('sizeHR','','Size',dt_string);
      AddOneToOnescheme('typeHR','','Type',dt_string);
      AddOneToOnescheme('date','','Date',dt_date);
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('icon_open','','',dt_string,false);
      AddOneToOnescheme('mypath','','',dt_string,false);
      AddOneToOnescheme('children','','',dt_string,false);
      AddOneToOnescheme('objectclass','','',dt_string,false);
      AddOneToOnescheme('UIP','uidpath','',dt_string,false);
      AddConstString('_childrenfunc_','ChildrenData',false);
      AddConstString('_funcclassname_','TFRE_DB_TEST_FILEDIR',false);
    end;
    with DC_Grid do begin
      SetDeriveParent(conn.GetCollection('CFBROWSER:'+session.GetLoginUserAsCollKey),'mypath');
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_Children,cdgf_ColumnDragable,cdgf_ColumnResizeable],'',TFRE_DB_StringArray.create('name'),'icon',nil,nil,nil,nil,CWSF(@WEB_DropAction));
    end;
  end;
end;


function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res          : TFRE_DB_LAYOUT_DESC;
  layout_trees : TFRE_DB_LAYOUT_DESC;
  DC_Tree      : IFRE_DB_DERIVED_COLLECTION;
  slider       : TFRE_DB_FORM_DESC;
  block        : TFRE_DB_INPUT_BLOCK_DESC;
  gl,gr        : TFRE_DB_VIEW_LIST_DESC;
begin
  res           := TFRE_DB_LAYOUT_DESC.Create.Describe();
  layout_trees  := TFRE_DB_LAYOUT_DESC.Create.Describe();

  DC_Tree := ses.FetchDerivedCollection('FILEBROWSER');
  slider:=TFRE_DB_FORM_PANEL_DESC.create.Describe('',true,true,CWSF(@WEB_SliderChanged),500);
  slider.contentId:='slider_form';
  block:=slider.AddBlock.Describe();
  block.AddNumber(8).DescribeSlider('','slider',0,100,false,'100',0,101);
  block.AddDate(1).Describe('','slider_date',false,false,true,false,IntToStr(GFRE_DT.Now_UTC));

  gl:=DC_Tree.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  gr:=DC_Tree.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;

  gl.SetDropGrid(gr,TFRE_DB_StringArray.create('TFRE_DB_TEST_FILEDIR'));

  layout_trees.SetLayout(gl,gr,nil,nil,nil,true,1,1);
  res.SetAutoSizedLayout(nil,layout_trees,nil,nil,slider);
  result := res;
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_SliderChanged(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  updateObj: IFRE_DB_Object;
begin
  writeln('SliderChanged ' + input.DumpToString);
  if input.FieldExists('slider') then begin
    updateObj:=GFRE_DBI.NewObject;
    updateObj.Field('slider_date').AsInt64:=GFRE_DT.Now_UTC - (100-input.Field('slider').AsInt32) * 1000 * 60 * 60 * 24;
    Result:=TFRE_DB_UPDATE_FORM_DESC.create.Describe('slider_form',updateObj);
  end else begin
    Result:=GFRE_DB_NIL_DESC;
  end;
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_DropAction(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Drop','You dropped item ' + input.Field('selected').AsString + ' on ' + input.Field('target').AsString,fdbmt_info);
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_FeederTest(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;

    procedure GotAnswer(const ses: IFRE_DB_UserSession; const new_input: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const ocid: Qword; const opaquedata: IFRE_DB_Object);
    var stat : string;
    begin
      case status of
        cdcs_OK:      stat := 'CMD_OK';
        cdcs_TIMEOUT: stat := 'CMD_TIMEOUT';
        cdcs_ERROR:   stat := 'CMD_ERROR';
      end;
      ses.SendServerClientAnswer(TFRE_DB_MESSAGE_DESC.create.Describe('Result',stat+' '+new_input.GetAsJSONString(),fdbmt_info),ocid);
    end;


begin
  result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
  if ses.InvokeRemoteRequest('TFRE_SAMPLE_FEED_CLIENT','TESTMETHOD',nil,@GotAnswer,nil)=edb_OK then
    begin
      result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      exit;
    end
  else
    begin
      result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','no connected feeder that implements the TestMethod',fdbmt_error);
    end;
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_FeederTestTimeout(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;

  procedure GotAnswer(const ses: IFRE_DB_UserSession; const new_input: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const ocid: Qword; const opaquedata: IFRE_DB_Object);
  var stat : string;
  begin
    case status of
      cdcs_OK:      stat := 'CMD_OK       : '+new_input.GetAsJSONString();
      cdcs_TIMEOUT: stat := 'CMD_TIMEOUT  : ';
      cdcs_ERROR:   stat := 'CMD_ERROR    : '+new_input.GetAsJSONString();
    end;
    ses.SendServerClientAnswer(TFRE_DB_MESSAGE_DESC.create.Describe('Result',stat,fdbmt_info),ocid);
  end;

begin
  result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
  if ses.InvokeRemoteRequest('TFRE_SAMPLE_FEED_CLIENT','TESTTIMEOUT',nil,@GotAnswer,nil)=edb_OK then
    begin
      result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      exit;
    end
  else
    begin
      result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','no connected feeder that implements the TestTimeout',fdbmt_error);
    end;
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_FeederTestError(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;

  procedure GotAnswer(const ses: IFRE_DB_UserSession; const new_input: IFRE_DB_Object; const status: TFRE_DB_COMMAND_STATUS; const ocid: Qword; const opaquedata: IFRE_DB_Object);
  var stat : string;
  begin
    case status of
      cdcs_OK:      stat := 'CMD_OK';
      cdcs_TIMEOUT: stat := 'CMD_TIMEOUT';
      cdcs_ERROR:   stat := 'CMD_ERROR';
    end;
    ses.SendServerClientAnswer(TFRE_DB_MESSAGE_DESC.create.Describe('Result',stat+' '+new_input.GetAsJSONString(),fdbmt_info),ocid);
  end;

begin
  result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
  if ses.InvokeRemoteRequest('TFRE_SAMPLE_FEED_CLIENT','TESTERROR',nil,@GotAnswer,nil)=edb_OK then
    begin
      result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      exit;
    end
  else
    begin
      result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','no connected feeder that implements the TestTimeout',fdbmt_error);
    end;
end;

{ TFRE_DB_TEST_APP_ALLGRID_MOD }

procedure TFRE_DB_TEST_APP_ALLGRID_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$allgrid_description');
end;

class procedure TFRE_DB_TEST_APP_ALLGRID_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

procedure TFRE_DB_TEST_APP_ALLGRID_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var DC_Grid  : IFRE_DB_DERIVED_COLLECTION;
    DC_Grid2 : IFRE_DB_DERIVED_COLLECTION;
    tr_Grid  : IFRE_DB_SIMPLE_TRANSFORM;
    tr_Grid2 : IFRE_DB_SIMPLE_TRANSFORM;

begin
  if session.IsInteractiveSession then begin
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme('MYID','','MYID',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_GUID','','GUID',dt_String,true,true,true);
      AddOneToOnescheme('fdbft_Byte','','BYTE',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Int16','','Int16',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_UInt16','','UInt16',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Int32','','Int32',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_UInt32','','Uint32',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Int64','','Int64',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_UInt64','','Uint64',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Real32','','Real32',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Real64','','Real64',dt_number,true,true,true);
      AddOneToOnescheme('fdbft_Currency','','Currency',dt_currency,true,true,true);
      AddOneToOnescheme('fdbft_String','','String',dt_String,true,true,true);
      AddOneToOnescheme('fdbft_Boolean','','Boolean',dt_boolean,true,true,true);
      AddOneToOnescheme('fdbft_DateTimeUTC','','Datetime',dt_date,true,true,true);
      //AddOneToOnescheme('fdbft_Stream','','',dt_String,true,true,true);
      //AddOneToOnescheme('fdbft_ObjLink','','',dt_String,true,true,true);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid2);
    with tr_Grid2 do begin
      AddOneToOnescheme          ('myid','','My Id',dt_number);
      AddOneToOnescheme          ('calc_string','','Calc_String');
      AddOneToOnescheme          ('calc_UInt32','','Calc_U32');
      AddConstString             ('const1','Const1',true,'ConstantT1');
      AddConstString             ('const2','Const2',false,'ConstantT2');
      AddCollectorscheme         ('(%s)-| %s | %s €',TFRE_DB_NameTypeArray.create('myid','fdbft_GUID','fdbft_Currency'),'Coll','Collector');
      AddDBTextToOne             ('dbText',tst_Short,'dbtS','DBText_s');
      AddDBTextToOne             ('dbText',tst_Long,'dbtL','DBText_l');
      AddDBTextToOne             ('dbText',tst_Hint,'dbtH','DBText_h');
      AddDBTextToOne             ('dbText',tst_Key,'dbtK','DBText_k');
      AddCollectorscheme         ('Value=%s%%',TFRE_DB_NameTypeArray.create('fdbft_Byte'),'prg_txt','CollectorPrg',false);
      AddProgressTransform       ('fdbft_Byte','ptb','Progress','prg_txt','txt',100);
      AddMatchingReferencedField ('LINK','data','data','Link to Obj1');
      AddMatchingReferencedField (['LINK>','LINK2>'],'data','data2','Link to Obj2 via Obj1');
    end;


    DC_Grid := session.NewDerivedCollection('DC_ALLTYPES');
    with DC_Grid do begin
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_AT'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable],'This grid test all fieldtypes, beside stream and object');
    end;

    DC_Grid2 := session.NewDerivedCollection('DC_AT_EX');
    with DC_Grid2 do begin
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_AT'));
      SetDeriveTransformation(tr_Grid2);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable],'This grid shows different extended transformation types');
    end;

  end;
end;

function TFRE_DB_TEST_APP_ALLGRID_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var lGrid   : TFRE_DB_VIEW_LIST_DESC;
    lGrid2  : TFRE_DB_VIEW_LIST_DESC;
    layout  : TFRE_DB_LAYOUT_DESC;
begin
  layout := TFRE_DB_LAYOUT_DESC.create.Describe();

  lGrid  := ses.FetchDerivedCollection('DC_ALLTYPES').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lGrid2 := ses.FetchDerivedCollection('DC_AT_EX').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  layout.SetLayout(nil,lGrid,nil,nil,lGrid2,true,0,1,0,0,1);
  result := layout;
end;

{ TFRE_DB_TEST_APP_FORMTEST_MOD }

procedure TFRE_DB_TEST_APP_FORMTEST_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$formtest_description');
end;

function TFRE_DB_TEST_APP_FORMTEST_MOD.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
var
  submenu,menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('Update Form DBO','',CWSF(@WEB_UPDATE_FORM_DBO));
  menu.AddEntry.Describe('Update Form DBO (Byte Fld)','',CWSF(@WEB_UPDATE_FORM_BYTE_FIELD));
  Result:=menu;
end;

class procedure TFRE_DB_TEST_APP_FORMTEST_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

function TFRE_DB_TEST_APP_FORMTEST_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var test : IFRE_DB_Object;
begin
  test := conn.GetCollection('COLL_TEST_AT').First;
  if assigned(test) then
    begin
      result := test.Invoke('Content',input,ses,app,conn);
      test.Finalize;
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','the COLL_TEST_A collection does not have any objects in it',fdbmt_error);
end;

function TFRE_DB_TEST_APP_FORMTEST_MOD.WEB_UPDATE_FORM_DBO(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var test : IFRE_DB_Object;
    allt : TFRE_DB_TEST_ALL_TYPES;
begin
  test := conn.GetCollection('COLL_TEST_AT').First;
  if assigned(test) then
    begin
      if test.IsA(TFRE_DB_TEST_ALL_TYPES,allt) then
        begin
          allt.Gamble(123);
          conn.Update(allt);
          result := GFRE_DB_NIL_DESC;
        end;
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','the COLL_TEST_A collection does not have any objects in it',fdbmt_error);
end;

function TFRE_DB_TEST_APP_FORMTEST_MOD.WEB_UPDATE_FORM_BYTE_FIELD(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var test : IFRE_DB_Object;
    allt : TFRE_DB_TEST_ALL_TYPES;
begin
  test := conn.GetCollection('COLL_TEST_AT').First;
  if assigned(test) then
    begin
      if test.IsA(TFRE_DB_TEST_ALL_TYPES,allt) then
        begin
          allt.Field('fdbft_byte').AsByte := random(255);
          conn.Update(allt);
          result := GFRE_DB_NIL_DESC;
        end;
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe('ERROR','the COLL_TEST_A collection does not have any objects in it',fdbmt_error);
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
  InitModuleDesc('$live_chart_description')
end;

procedure TFRE_DB_TEST_APP_LIVE_CHART_MOD._SendDataLC(const ses: IFRE_DB_Usersession);
var
  session: TFRE_DB_UserSession;
  data   : TFRE_DB_Real32Array;
  res    : TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC;
  i      : Integer;
begin
  SetLength(data,2);
  for i := 0 to 1 do begin
    data[i]:=Random(100);
  end;
  res:=TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC.create.Describe('lchart',_idx,data);
  _idx:=_idx+1;
  ses.SendServerClientRequest(res);
end;

procedure TFRE_DB_TEST_APP_LIVE_CHART_MOD._SendDataCC(const ses: IFRE_DB_Usersession);
var
  data   : TFRE_DB_LIVE_CHART_DATA_ARRAY;
  res    : TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC;
  redef  : TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  i,l    : Integer;
begin
  if _idx<10 then begin
    _idx:=_idx+1;
    l:=8;
  end else begin
    if _idx=10 then begin
      redef:=TFRE_DB_REDEFINE_LIVE_CHART_DESC.create.DescribeColumn('cchart',10,TFRE_DB_StringArray.create('D1','D2','D3','D4','D5','D6','D7','D8','D9','D10'),TFRE_DB_Real32Array.create(50,200),0,nil,TFRE_DB_StringArray.create('AAFFCC'),'Live Chart Columns MOD');
      ses.SendServerClientRequest(redef);
      _idx:=11;
    end;
    l:=10;
  end;
  SetLength(data,1);
  SetLength(data[0],l);
  for i := 0 to l-1 do begin
    data[0][i]:=Random(100);
  end;
  res:=TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC.create.Describe('cchart',data);
  ses.SendServerClientRequest(res);
end;

procedure TFRE_DB_TEST_APP_LIVE_CHART_MOD._SendDataSLC(const ses: IFRE_DB_Usersession);
var
  data   : TFRE_DB_LIVE_CHART_DATA_ARRAY;
  res    : TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC;
  i      : Integer;
begin
  SetLength(data,1);
  SetLength(data[0],4);
  for i := 0 to 3 do begin
    data[0][i]:=Random(100);
  end;
  res:=TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC.create.Describe('slchart',data);
  ses.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe();
  res.AddSection.Describe(CWSF(@WEB_ContentL),'LC',2);
  res.AddSection.Describe(CWSF(@WEB_ContentSL),'SLC',1);
  res.AddSection.Describe(CWSF(@WEB_ContentC),'CC',0);
  Result:=res;
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_ContentSL(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_LIVE_CHART_DESC.create.DescribeSampledLine('slchart',1,4,CWSF(@WEB_StartStopSLC),0,100,'Sampled Live Chart');
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_ContentL(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_LIVE_CHART_DESC.create.DescribeLine('lchart',2,120,CWSF(@WEB_StartStopLC),0,100,'Live Chart',nil,TFRE_DB_StringArray.create('Line 1','Line 2'));
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_ContentC(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_LIVE_CHART_DESC.create.DescribeColumn('cchart',8,CWSF(@WEB_StartStopCC),'Live Chart Columns',100,0,TFRE_DB_StringArray.create('3F5490'),TFRE_DB_StringArray.create('Disk01','Disk_long_label_02','D03','Disk04','Disk05','Disk06','Disk07','Disk08'));
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_StartStopSLC(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if input.Field('action').AsString='start' then begin
    _idx:=0;
    ses.RegisterTaskMethod(@_SendDataSLC,1000,'SLC');
  end else begin
    ses.RemoveTaskMethod('SLC');
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_StartStopLC(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  if input.Field('action').AsString='start' then begin
    _idx:=0;
    ses.RegisterTaskMethod(@_SendDataLC,1000,'LC');
  end else begin
    ses.RemoveTaskMethod('LC');
  end;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_LIVE_CHART_MOD.WEB_StartStopCC(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  data   : TFRE_DB_LIVE_CHART_DATA_ARRAY;
  i      : Integer;
begin
  if input.Field('action').AsString='start' then begin
    _idx:=0;
    ses.RegisterTaskMethod(@_SendDataCC,1000,'CC');
  end else begin
    ses.RemoveTaskMethod('CC');
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
  InitModuleDesc('$edit_description')
end;

procedure TFRE_DB_TEST_APP_EDITORS_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
begin
  inherited MySessionInitializeModule(session);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_SUBSECTIONS_DESC;
begin
  res:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe();
  res.AddSection.Describe(CWSF(@WEB_HtmlEditor),'Html',1);
  res.AddSection.Describe(CWSF(@WEB_JSEditor),'JS',2);
  res.AddSection.Describe(CWSF(@WEB_PascalEditor),'Pascal',3);
  Result:=res;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_HtmlEditor(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CWSF(@WEB_HtmlEditorLoad),CWSF(@WEB_HtmlEditorSave),CWSF(@WEB_HtmlEditorStart),CWSF(@WEB_HtmlEditorStop));
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_HtmlEditorLoad(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('Initial Html<BR><BR>Hello');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_HtmlEditorSave(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_HtmlEditorStart(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_HtmlEditorStop(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_JSEditor(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CWSF(@WEB_JSEditorLoad),CWSF(@WEB_JSEditorSave),CWSF(@WEB_JSEditorStart),CWSF(@WEB_JSEditorStop),ct_javascript);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_JSEditorLoad(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('if (true) {'+#10+'  console.log("HELLO");'+#10+'}');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_JSEditorSave(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_JSEditorStart(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_JSEditorStop(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_PascalEditor(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DESC.create.Describe(CWSF(@WEB_PascalEditorLoad),CWSF(@WEB_PascalEditorSave),CWSF(@WEB_PascalEditorStart),CWSF(@WEB_PascalEditorStop),ct_pascal);
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_PascalEditorLoad(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_EDITOR_DATA_DESC.create.Describe('if (true) then begin'+#10+'  writeln("HELLO");'+#10+'end;');
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_PascalEditorSave(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_PascalEditorStart(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_EDITORS_MOD.WEB_PascalEditorStop(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_APP_WELCOME_MOD }

procedure TFRE_DB_TEST_APP_WELCOME_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('$welcome_description');
end;

class procedure TFRE_DB_TEST_APP_WELCOME_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

function TFRE_DB_TEST_APP_WELCOME_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  result := TFRE_DB_HTML_DESC.create.Describe('Welcome to the Welcome Module');
end;

{ TFRE_DB_TEST_B }

class procedure TFRE_DB_TEST_B.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  input_group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('firstname',fdbft_String);
  scheme.AddSchemeField         ('lastname',fdbft_String);
  scheme.AddSchemeField         ('pass',fdbft_String).SetupFieldDef(true,false,'','',true,true);
  scheme.AddSchemeField         ('icon',fdbft_String);

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_TEST_B');
  input_group.AddInput('firstname','Firstname');
  input_group.AddInput('lastname','Lastname');
  input_group.AddInput('pass','Password');
  input_group.AddInput('icon','Icon');
end;

class procedure TFRE_DB_TEST_B.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId := '1.0';
end;

function TFRE_DB_TEST_B.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
    res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('FORM');
    res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
    res.FillWithObjectValues(Self,GetSession(input));
    res.AddButton.Describe('Save',CWSF(@WEB_saveOperation),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_TEST_B.WEB_ChildrenData(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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
  InitModuleDesc('$gtf_description')
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
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_B'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Children],'GRID');
    end;

    DC_Tree := session.NewDerivedCollection('COLL_TEST_B_DERIVED_TREE');
    with DC_Tree do begin
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_B'));
      SetDisplayType(cdt_Treeview,[cdgf_ShowSearchbox],'Tree',TFRE_DB_StringArray.create('firstname'),'icon',nil,nil,nil);//CSF('TreeMenu'));
    end;
  end;
end;

function TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  coll  : IFRE_DB_DERIVED_COLLECTION;
  list  : TFRE_DB_VIEW_LIST_DESC;
begin
  coll := GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED');
  list := coll.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  list.addFilterEvent(GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED_TREE').getDescriptionStoreId,'uid');

  Result := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(list,TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED_TREE').GetDisplayDescription,nil));
end;

{ TFRE_DB_TEST_ALL_TYPES }

class procedure TFRE_DB_TEST_ALL_TYPES.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId := '1.0';
end;

class procedure TFRE_DB_TEST_ALL_TYPES.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var
  input_group: IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('fdbft_GUID',fdbft_GUID);
  scheme.AddSchemeField         ('fdbft_Byte',fdbft_Byte);
  scheme.AddSchemeField         ('fdbft_Int16',fdbft_Int16);
  scheme.AddSchemeField         ('fdbft_UInt16',fdbft_UInt16);
  scheme.AddSchemeField         ('fdbft_Int32',fdbft_Int32);
  scheme.AddSchemeField         ('fdbft_UInt32',fdbft_UInt32);
  scheme.AddSchemeField         ('fdbft_Int64',fdbft_Int64);
  scheme.AddSchemeField         ('fdbft_UInt64',fdbft_UInt64);
  scheme.AddSchemeField         ('fdbft_Real32',fdbft_Real32);
  scheme.AddSchemeField         ('fdbft_Real64',fdbft_Real64);
  scheme.AddSchemeField         ('fdbft_Currency',fdbft_Currency);
  scheme.AddSchemeField         ('fdbft_String',fdbft_String);
  scheme.AddSchemeField         ('fdbft_Boolean',fdbft_Boolean);
  scheme.AddSchemeField         ('fdbft_DateTimeUTC',fdbft_DateTimeUTC);
  scheme.AddSchemeField         ('fdbft_Stream',fdbft_Stream);
  scheme.AddSchemeField         ('fdbft_ObjLink',fdbft_ObjLink);
  scheme.AddSchemeFieldSubscheme('dbText','TFRE_DB_TEXT');
  scheme.AddCalcSchemeField     ('calc_string',fdbft_String,@CALC_String);
  scheme.AddCalcSchemeField     ('calc_Uint32',fdbft_UInt32,@CALC_Uint32);

  input_group:=scheme.AddInputGroup('main').Setup('$scheme_TFRE_DB_TEST_ALL_TYPES');
  input_group.AddInput('fdbft_GUID');
  input_group.AddInput('fdbft_Byte');
  input_group.AddInput('fdbft_Int16');
  input_group.AddInput('fdbft_UInt16');
  input_group.AddInput('fdbft_Int32');
  input_group.AddInput('fdbft_UInt32');
  input_group.AddInput('fdbft_Int64');
  input_group.AddInput('fdbft_UInt64');
  input_group.AddInput('fdbft_Real32');
  input_group.AddInput('fdbft_Real64');
  input_group.AddInput('fdbft_Currency');
  input_group.AddInput('fdbft_String');
  input_group.AddInput('fdbft_Boolean');
  input_group.AddInput('fdbft_DateTimeUTC');
  input_group.AddInput('fdbft_Stream');
  input_group.AddInput('fdbft_ObjLink');
end;

procedure TFRE_DB_TEST_ALL_TYPES.Gamble(const id: int64);
  const TestChars : TFRE_DB_String = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';//_#*?!"§$%';

  var buf : QWord;
      ids : string;
      dbt : IFRE_DB_TEXT;
      str : TFRE_DB_Stream;

  function GetRandString : TFRE_DB_String;
  var i : integer;
  begin
    result := '';
    for i := 1 to 8 do
      result := result + TestChars[(random(Length(TestChars)-1))];
  end;

  Procedure GetRandomBytes(Var Buf; NBytes : Integer);
  Var
    I : Integer;
    P : PByte;
  begin
    P:=@Buf;
    For I:=0 to NBytes-1 do
      P[i]:=Random(256);
  end;

begin
  Field('fdbft_Stream').AsStream.WriteAnsiString('THIS IS A TESTCONTENT '+#13#10+' ! '+#10+'<'+UID_String+'>');
  Field('fdbft_GUID').AsGUID               := GFRE_DBI.Get_A_Guid;
  Field('fdbft_Byte').AsByte               := Random (100);
  Field('fdbft_Int16').AsInt16             := Random (65535)-32768;
  Field('fdbft_UInt16').AsUInt16           := Random (65535);
  Field('fdbft_Int32').AsInt32             := Int32  (Random(4294967295)-2147483648);
  Field('fdbft_UInt32').AsUInt32           := UInt32 (Random(4294967295));
  GetRandomBytes(buf,8);
  Field('fdbft_Int64').AsInt64             := PInt64(@buf)^;
  GetRandomBytes(buf,8);
  Field('fdbft_UInt64').AsUInt64           := PQWord(@Buf)^;
  Field('fdbft_Real32').AsReal32           := Single (Random (100000)) / Single((Random (123456789)+1));
  Field('fdbft_Real64').AsReal64           := Double (Random (100000)) / Single((Random (123456789)+1));
  Field('fdbft_Currency').AsCurrency       := Random (1000000) / 100;
  Field('fdbft_String').AsString           := Field('MYID').AsString+'_'+GetRandstring;
  Field('fdbft_Boolean').AsBoolean         := random(3)=1;
  Field('fdbft_DateTimeUTC').AsDateTimeUTC := GFRE_DT.Now_UTC;
  ids := IntToStr(id);
  Field('MYID').AsInt64                    := id;
  Field('dbText').AsDBText.SetupText('KEY'+ids,'Short_'+ids,'Long_'+ids,'Hint_'+ids);  // This works only if the type of an implicitly created subobject is known, by defining the field in the scheme !
end;

procedure TFRE_DB_TEST_ALL_TYPES.CALC_Uint32(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  calc.SetAsUInt32((Field('fdbft_Byte').AsByte+1)*111);
end;

procedure TFRE_DB_TEST_ALL_TYPES.CALC_String(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  calc.SetAsString('CALC + '+Field('fdbft_Byte').AsString+' '+Field('myid').AsString);
end;


function TFRE_DB_TEST_ALL_TYPES.WEB_GetIcon(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin

end;

function TFRE_DB_TEST_ALL_TYPES.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('FORM');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses);
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',CWSF(@WEB_saveOperation),fdbbt_submit);
  Result:=res;
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
  InitModuleDesc('$chart_description')
end;

procedure TFRE_DB_TEST_APP_CHART_MOD.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
var vmo       : IFRE_DB_Object;
    vm        : IFRE_DB_Object;
    data_obj  : IFRE_DB_Object;
    i,max     : Integer;
    CHARTDATA : IFRE_DB_COLLECTION;
begin
  CHARTDATA := admin_dbc.GetCollection('CHART_MOD_LINE');
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
  CHARTDATA := admin_dbc.GetCollection('CHART_MOD_PIE');
  max:=9;
  for i := 0 to max - 1 do begin
    data_obj := GFRE_DBI.NewObject;
    data_obj.Field('val1').AsReal32      := random(100);
    data_obj.Field('val1_uid').AsGUID    := GFRE_DBI.Get_A_Guid;
    data_obj.Field('val1_col').AsString  := '#44667'+IntToStr(i mod 10)+'A';
    data_obj.Field('val1_txt').AsString  := 'PIE:' + IntToStr(i+1);
    CHARTDATA.Store(data_obj);
  end;
  CHARTDATA := admin_dbc.GetCollection('CHART_MOD_COLUMNS');
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
    DC_CHARTDATA_P : IFRE_DB_DERIVED_COLLECTION;
    CHARTDATA      : IFRE_DB_COLLECTION;
begin
  inherited MySessionInitializeModule(session);
  if session.IsInteractiveSession then begin
    CHARTDATA := session.GetDBConnection.GetCollection('CHART_MOD_PIE');
    DC_CHARTDATA_P := session.NewDerivedCollection('CHART_P');
    with DC_CHARTDATA_P do begin
      SetDeriveParent(CHARTDATA);
      SetDisplayTypeChart('Pie Chart',fdbct_pie,TFRE_DB_StringArray.Create('val1'),false,false);
    end;
  end;
end;

function TFRE_DB_TEST_APP_CHART_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  sub_sec_s       : TFRE_DB_SUBSECTIONS_DESC;

begin
  sub_sec_s        := TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_tab);
  sub_sec_s.AddSection.Describe(CWSF(@WEB_ContentPie),'Pie',1);
  result           := sub_sec_s;
end;

function TFRE_DB_TEST_APP_CHART_MOD.WEB_ContentPie(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=GetSession(input).FetchDerivedCollection('CHART_P').GetDisplayDescription;
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
  InitModuleDesc('$grid2_description')
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
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_A2'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Multiselect],'',TFRE_DB_StringArray.create('objname'),'',CWSF(@WEB_Menu));
    end;
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
var
  submenu,menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('SEAS1','',CSF(@IMI_HelloWorld));
  menu.AddEntry.Describe('SEAS2','images_apps/test/cog.png',CWSF(@WEB_HelloWorld));
  submenu:=menu.AddMenu.Describe('SEAS3','images_apps/test/cog.png');
  submenu.AddEntry.Describe('SEAS4','',CWSF(@WEB_HelloWorld));
  submenu.AddEntry.Describe('SEAS5','images_apps/test/cog.png',CWSF(@WEB_HelloWorld));
  submenu:=submenu.AddMenu.Describe('SEAS6','');
  submenu.AddEntry.Describe('SEAS7','',CWSF(@WEB_HelloWorld));
  submenu.AddEntry.Describe('SEAS8','images_apps/test/cog.png',CWSF(@WEB_HelloWorld));
  menu.AddEntry.Describe('Update Content Section','',CWSF(@WEB_UpdateCS));
  Result:=menu;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var lvd_Grid     : TFRE_DB_VIEW_LIST_DESC;
    DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
    layout       : TFRE_DB_LAYOUT_DESC;
    html         : TFRE_DB_HTML_DESC;
begin
  layout:=TFRE_DB_LAYOUT_DESC.create.Describe();

  DC_Grid_Long := ses.FetchDerivedCollection('COLL_TEST_A_DERIVED2');

  lvd_Grid := DC_Grid_Long.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button with TT','Tooltip');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button wo TT');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button sSel','Enabled on single Select',fdgbd_single);
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button mSel','Enabled on any Selection',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CWSF(@WEB_AddRecordBefore),'images_apps/test/add.png','Add Obj Before','Add one Object');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_AddRecordAfter),'images_apps/test/add.png','Add Obj After','Add one Object');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_DelRecord),'images_apps/test/add.png','Del sel','Delete sel. Objs',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CWSF(@WEB_UpdRecord),'images_apps/test/add.png','Upd sel','Update sel. Objs',fdgbd_multi);

  html:=TFRE_DB_HTML_DESC.create.Describe('SEAS INITIAL');
  html.contentId:='GRID2_HTML';

  layout.SetLayout(lvd_Grid,html,nil,nil,nil,true,1,1,1,1,1);
  //layout.contentId:='GRID2_LAYOUT';
  result := layout;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.IMI_HelloWorld(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  writeln(INPUT.DumpToString());
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World '+input.Field('SELECTED').AsStringDump,fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_HelloWorld(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World '+ses.GetSessionID+'/'+app.AppClassName+' :: '+input.Field('SELECTED').AsStringDump,fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_Menu(const input: IFRE_DB_Object;const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION;const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe;
  res.AddEntry.DescribeDownload('Menu entry','','/');
  Result:=res;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_GRID_ITEM_DETAILS(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe('<B>GUGUG</B><BR>SEAS');
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_AddRecordBefore(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var col : IFRE_DB_COLLECTION;
    NEW : IFRE_DB_Object;
    selg: TGUID;
    num : UInt32;
begin
  Result := GFRE_DB_NIL_DESC;
  writeln('ADD BEFORE');
  writeln(input.Field('SELECTED').AsString);
  selg := GFRE_BT.HexString_2_GUID(input.Field('SELECTED').AsString);
  col    := GetDBConnection(input).GetCollection('COLL_TEST_A2');
  if col.Fetch(selg,new) then begin
    num := new.Field('number').AsUint32-1;
    new.Finalize;
    writeln('-NEW OBJECT BEFOR NUM : ',num);
    NEW    := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_A);
    new.Field('number').AsUInt32    := num;
    new.Field('number_pb').AsUInt32 := num * 10;
    new.Field('string').AsString    := 'String_' + IntToStr(new.Field('number').AsUInt32);
    new.Field('boolean').AsBoolean  := false;
    new.Field('date').AsDateTime    := GFRE_DT.Now_UTC;
    new.Field('status').AsString    := 'NEW';
    col.Store(new);
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_AddRecordAfter(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var col : IFRE_DB_COLLECTION;
    selg: TGUID;
    NEW : IFRE_DB_Object;
    num : UInt32;
begin
  Result := GFRE_DB_NIL_DESC;
  writeln(input.Field('SELECTED').AsString);
  selg := GFRE_BT.HexString_2_GUID(input.Field('SELECTED').AsString);
  col    := GetDBConnection(input).GetCollection('COLL_TEST_A2');
  if col.Fetch(selg,new) then begin
    num := new.Field('number').AsUint32+1;
    new.Finalize;
    writeln('-NEW OBJECT BEFOR NUM : ',num);
    NEW    := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_A);
    new.Field('number').AsUInt32    := num;
    new.Field('number_pb').AsUInt32 := num  * 10;
    new.Field('string').AsString    := 'String_' + IntToStr(new.Field('number').AsUInt32);
    new.Field('boolean').AsBoolean  := false;
    new.Field('date').AsDateTime    := GFRE_DT.Now_UTC;
    new.Field('status').AsString    := 'NEW';
    col.Store(new);
  end;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_DelRecord(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var col       : IFRE_DB_COLLECTION;
    del_guids : TFRE_DB_GUIDArray;
    i         : integer;
begin
  col       := GetDBConnection(input).GetCollection('COLL_TEST_A2');
  del_guids :=  GFRE_DBI.StringArray2GuidArray(input.Field('SELECTED').AsStringArr);
  for i:=0 to high(del_guids) do begin
    if col.Remove(del_guids[i])<>edb_OK then begin
      writeln('Could not delete Obj uid : '+GFRE_BT.GUID_2_HexString(del_guids[i]));
    end else begin
      writeln('Deleted Obj uid : '+GFRE_BT.GUID_2_HexString(del_guids[i]));
    end;
  end;
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_UpdRecord(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var col       : IFRE_DB_COLLECTION;
    upd_guids : TFRE_DB_GUIDArray;
    i         : integer;
    obj       : IFRE_DB_Object;
begin
  col       := GetDBConnection(input).GetCollection('COLL_TEST_A2');
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

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_UpdateCS(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  session: TFRE_DB_UserSession;
  html   : TFRE_DB_HTML_DESC;
begin
  Result:=GFRE_DB_NIL_DESC;
  session := GetSession(input);
  html:=TFRE_DB_HTML_DESC.create.Describe('UPDATE ' + IntToStr(Random(1000)));
  //html.updateId:='GRID2_HTML';
  html.contentId:='GRID2_HTML';
  session.SendServerClientRequest(html);
end;

{ TFRE_DB_TEST_APP_GRID_MOD }

procedure TFRE_DB_TEST_APP_GRID_MOD.ToggleBlast(const session: TFRE_DB_UserSession);
begin
  session.GetSessionModuleData(ClassName).Field('BLAST').AsBoolean := not session.GetSessionModuleData(ClassName).Field('BLAST').AsBoolean;
  SetBlast(session);
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SetBlast(const session: TFRE_DB_UserSession);
begin
  if session.GetSessionModuleData(Classname).Field('BLAST').AsBoolean=true then begin
     session.GetSessionModuleData(Classname).Field('BLAST_CNT').AsInt32 := 100;
    session.RegisterTaskMethod(@PRC_UPDATE_TASK,10,'GRID_BLAST');
  end else begin
    session.RemoveTaskMethod('GRID');
  end;
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.updateWait(const session: IFRE_DB_UserSession);
begin
  _idx:=_idx+5;
  session.SendServerClientRequest(TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC.create.Describe('zippingProgress',_idx));
  if _idx=100 then begin
    session.RemoveTaskMethod('UPW');
    session.SendServerClientRequest(TFRE_DB_CLOSE_DIALOG_DESC.create.Describe());
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
  InitModuleDesc('$grid_description')
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SC_ChangeData_Result(const input: IFRE_DB_Object);
begin
  writeln('---------- CD RESULT : ',input.DumpToString());
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.SC_ChangeData_Error(const input: IFRE_DB_Object);
begin
  writeln('---------- CD  TIMEOUT RESULT : ',input.DumpToString());
end;

procedure TFRE_DB_TEST_APP_GRID_MOD.PRC_UPDATE_TASK(const ses: IFRE_DB_Usersession);
var
  res          : TFRE_DB_UPDATE_STORE_DESC;
  entry        : IFRE_DB_Object;
  DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
  k            : integer;
  cnt          : integer;
begin
  DC_Grid_Long := ses.FetchDerivedCollection('COLL_TEST_A_DERIVED');
  cnt := ses.GetSessionModuleData(Classname).Field('BLAST_CNT').AsInt32;
  dec(cnt);
  ses.GetSessionModuleData(Classname).Field('BLAST_CNT').AsInt32 := cnt;
  writeln('BLAST CNT ',cnt);
  if cnt=0 then
    begin
      ses.RemoveTaskMethod('GRID_BLAST');
      ses.GetSessionModuleData(ClassName).Field('BLAST').AsBoolean := false;
    end;
  k := DC_Grid_Long.ItemCount;
  if k>0 then begin
    entry := DC_Grid_Long.GetItem(random(k));
    entry.Field('number').AsUInt32 := random(1000);
    entry.Field('number_pb').AsUInt32 := random(1000);
    res:=TFRE_DB_UPDATE_STORE_DESC.create.Describe(DC_Grid_Long.CollectionName);
    res.addUpdatedEntry(entry);
    ses.SendServerClientRequest(res);
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
      SetDeriveParent(session.GetDBConnection.GetCollection('COLL_TEST_A'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Details,cdgf_Multiselect],'',TFRE_DB_StringArray.create('objname'),'',nil,CWSF(@WEB_GRID_ITEM_DETAILS));
      SetDefaultOrderField('number',false);
    end;
    if not session.GetSessionModuleData(ClassName).FieldExists('BLAST') then begin
      session.GetSessionModuleData(Classname).Field('BLAST').AsBoolean:=false;
    end;
    SetBlast(session);
  end;
end;

function TFRE_DB_TEST_APP_GRID_MOD.GetToolbarMenu(const ses: IFRE_DB_Usersession): TFRE_DB_CONTENT_DESC;
var
  submenu,menu: TFRE_DB_MENU_DESC;
begin
  menu:=TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('LOGOUT','',TFRE_DB_SERVER_FUNC_DESC.create.Describe('FIRMOS','LOGOUT'));
  menu.AddEntry.Describe('FIRMOS.AT','',CWSF(@WEB_OpenFIRMOS));
  menu.AddEntry.Describe('REP FIRMOS.AT','',CWSF(@WEB_OpenFIRMOSHere));
  menu.AddEntry.Describe('RELOAD','images_apps/test/cog.png',CWSF(@WEB_Reload));
  submenu:=menu.AddMenu.Describe('SEAS3','images_apps/test/cog.png');
  submenu.AddEntry.Describe('SEAS4','',CWSF(@WEB_HelloWorld));
  submenu.AddEntry.Describe('SEAS5','images_apps/test/cog.png',CWSF(@WEB_HelloWorld));
  submenu:=submenu.AddMenu.Describe('SEAS6','');
  submenu.AddEntry.Describe('SEAS7','',CWSF(@WEB_HelloWorld));
  submenu.AddEntry.Describe('SEAS8','images_apps/test/cog.png',CWSF(@WEB_HelloWorld));
  Result:=menu;
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_ToggleUpdates(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  ToggleBlast(GetSession(input));
  result := GFRE_DB_NIL_DESC;
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_Content(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var lvd_Grid     : TFRE_DB_VIEW_LIST_DESC;
    DC_Grid_Long : IFRE_DB_DERIVED_COLLECTION;
begin
  DC_Grid_Long := GetSession(input).FetchDerivedCollection('COLL_TEST_A_DERIVED');
  lvd_Grid := DC_Grid_Long.GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button with TT','Tooltip');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button wo TT');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button sSel','Enabled on single Select',fdgbd_single);
  lvd_Grid.AddButton.Describe(CWSF(@WEB_HelloWorld),'images_apps/test/add.png','Button mSel','Enabled on any Selection',fdgbd_multi);
  lvd_Grid.AddButton.Describe(CWSF(@WEB_Dialog),'images_apps/test/add.png','Dialog');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_ReadOnlyDialog),'images_apps/test/add.png','Read only Dialog');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_ChangeData),'images_apps/test/add.png','Change Dataset');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_ToggleUpdates),'images_apps/test/add.png','ToggleUpdates');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_SendADialog),'images_apps/test/add.png','ServerClientDialog');
  lvd_Grid.AddButton.Describe(CWSF(@WEB_WaitMessage),'images_apps/test/add.png','Wait Message');
  result := lvd_Grid;
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_HelloWorld(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World',fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_OpenFIRMOS(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('http://www.firmos.at');
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_OpenFIRMOSHere(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('http://www.firmos.at',false);
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_Reload(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false);
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_ChangeData(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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
  entry := DC_Grid_Long.First;
  entry.Field('number').AsUInt32 := random(1000);

  res:=TFRE_DB_UPDATE_STORE_DESC.create.Describe(DC_Grid_Long.CollectionName);
  res.addUpdatedEntry(entry);
  session.SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_Dialog(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_DIALOG_DESC;
  g     : TFRE_DB_INPUT_GROUP_DESC;
  store : TFRE_DB_STORE_DESC;
  i     : Integer;
begin
  store:=TFRE_DB_STORE_DESC.create.Describe();
  for i := 0 to 10 - 1 do begin
    store.AddEntry.Describe('C'+IntToStr(i),'v'+IntToStr(i));
  end;

  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe('Dialog');
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

  res.AddButton.Describe('Save',CWSF(@WEB_HelloWorld),fdbbt_submit);
  Result:=res;
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_ReadOnlyDialog(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res  : TFRE_DB_FORM_DIALOG_DESC;
  g    : TFRE_DB_INPUT_GROUP_DESC;
  store: TFRE_DB_STORE_DESC;
  i    : Integer;
begin
  store:=TFRE_DB_STORE_DESC.create.Describe();
  for i := 0 to 10 - 1 do begin
    store.AddEntry.Describe('C'+IntToStr(i),'v'+IntToStr(i));
  end;

  res:=TFRE_DB_FORM_DIALOG_DESC.create.Describe('Dialog',0,true,true,true,false);
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

function TFRE_DB_TEST_APP_GRID_MOD.WEB_GRID_ITEM_DETAILS(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_HTML_DESC.create.Describe('<B>GUGUG</B><BR>SEAS');
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_SendADialog(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var res  : TFRE_DB_MESSAGE_DESC;
begin
  result := GFRE_DB_NIL_DESC;
  Res    := TFRE_DB_MESSAGE_DESC.create.Describe('Du wurdest ... ','... ausgeloggt!',fdbmt_info);
  GetSession(input).SendServerClientRequest(res);
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_WaitMessage(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  _idx:=0;
  ses.RegisterTaskMethod(@updateWait,1000,'UPW');
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Zipping','Please wait until your zip file is ready for download.',fdbmt_wait,CWSF(@WEB_AbortWait),'zippingProgress');
end;

function TFRE_DB_TEST_APP_GRID_MOD.WEB_AbortWait(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  ses.RemoveTaskMethod('UPW');
  Result:=GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_APP }

procedure TFRE_DB_TEST_APP.SetupApplicationStructure;
begin
  AddApplicationModule(TFRE_DB_TEST_APP_WELCOME_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRID_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRID2_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_CHART_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_LIVE_CHART_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_EDITORS_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_SVG_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_FORMTEST_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_ALLGRID_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.create);
end;

class procedure TFRE_DB_TEST_APP.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId := '1.0';
      CreateAppText(conn,'$caption','Test App','Test App','Test App');
      CreateAppText(conn,'$vnc_description','VNC Test','VNC Test','VNC Test');
      CreateAppText(conn,'$welcome_description','Welcome Test','Welcome Test','Welcome Test');
      CreateAppText(conn,'$gtf_description','Grid Tree Form Test','Grid Tree Form Test','Grid Tree Form Test');
      CreateAppText(conn,'$edit_description','Editors','Editors','Editors');
      CreateAppText(conn,'$svg_description','SVG','SVG','SVG');
      CreateAppText(conn,'$chart_description','Chart Test','Chart Test','Chart Test');
      CreateAppText(conn,'$live_chart_description','Live Chart Test','Live Chart Test','Live Chart Test');
      CreateAppText(conn,'$grid_description','Grid Test','Grid Test','Grid Test');
      CreateAppText(conn,'$grid2_description','Grid 2 Test','Grid 2 Test','Grid 2 Test');
      CreateAppText(conn,'$formtest_description','Fieldtypes Formtest','Form Tests','A form to test all possible validators,data types, gauges etc');
      CreateAppText(conn,'$allgrid_description','Fieldtypes Gridtest','Grid Tests','A grid to test all possible validators,data types, gauges etc');
      CreateAppText(conn,'$feedbrowsetree_description','Feeder Browser','Feeder Browser','A Module implementing a simple test browser tree');
   end;
  if currentVersionId<>newVersionId then
    raise EFRE_DB_Exception.Create(edb_ERROR,'upgrade failed for '+currentVersionId+' : '+newVersionId);
end;


class procedure TFRE_DB_TEST_APP.InstallDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
var group : IFRE_DB_GROUP;
begin
  inherited InstallDBObjects4Domain(conn, currentVersionId, domainUID);
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      CheckDbResult(conn.AddGroup('TESTFEEDER','Group for Test Data Feeder','Test Feeder',domainUID),'could not create Test feeder group');

      CheckDbResult(conn.AddRolesToGroup('TESTFEEDER',domainUID,TFRE_DB_StringArray.Create(
        TFRE_DB_TEST_APP.GetClassRoleNameFetch
        )),'could not add roles for group TESTFEEDER');
    end;


  //admin_app_role  := _CreateAppRole('ADMIN','TESTAPP ADMIN','Test App Administration Rights');
  //user_app_role   := _CreateAppRole('USER','TESTAPP USER','Test App Default User Rights');
  //guest_app_role  := _CreateAppRole('GUEST','TESTAPP GUEST','Test App Default Guest User Rights');

  //_AddAppRight(admin_app_role ,'ADMIN');
  //_AddAppRight(user_app_role  ,'START');
  //
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['grid']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['grid2']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['chart']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['live_chart']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['vmcontroller']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['tgf']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['edit']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['formtest']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['allgrid']));
  //_AddAppRightModules(user_app_role,GFRE_DBI.ConstructStringArray(['feedbrowser']));
  //
  //_AddAppRight(guest_app_role ,'START'); // Guests are allowed to START the app
  //_AddAppRightModules(guest_app_role,GFRE_DBI.ConstructStringArray(['welcome']));
  //
  //conn.StoreRole(admin_app_role,ObjectName,domain);
  //conn.StoreRole(user_app_role,ObjectName,domain);
  //conn.StoreRole(guest_app_role,ObjectName,domain);
  //

//   conn.NewGroup('TESTAPPADMIN','Admin Group for the testapp','Testapp Admins',domainUID,group);


  //conn.AddAppGroup(ObjectName,'USER'+'@'+domain,ObjectName+' UG',ObjectName+' User');  // DEMONSTRATION GROUPS -> make your own in your App
  //conn.AddAppGroup(ObjectName,'ADMIN'+'@'+domain,ObjectName+' AG',ObjectName+' Admin');
  //conn.AddAppGroup(ObjectName,'GUEST'+'@'+domain,ObjectName+' GG',ObjectName+' Guest');
  //
  //conn.AddGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'USER'+'@'+domain)));
  //conn.AddGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'GUEST'+'@'+domain)));
  //conn.AddGroupRoles(Get_Groupname_App_Group_Subgroup(ObjectName,'ADMIN'+'@'+domain));

end;

class procedure TFRE_DB_TEST_APP.InstallUserDBobjects(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType);
begin
  if currentVersionId='' then
    begin
      currentVersionId := '1.0';
      conn.CreateCollection('COLL_TEST_AT');
      conn.CreateCollection('COLL_TEST_A');
      conn.CreateCollection('COLL_TEST_A2');
      conn.CreateCollection('COLL_TEST_B');
      conn.CreateCollection('LINKTARGET');
      conn.CreateCollection('COLL_FILEBROWSER');
    end;
end;

class procedure TFRE_DB_TEST_APP.InstallUserDBObjects4Domain(const conn: IFRE_DB_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin

end;

class procedure TFRE_DB_TEST_APP.RemoveDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType);
begin
  inherited RemoveDBObjects(conn, currentVersionId);
end;

class procedure TFRE_DB_TEST_APP.RemoveDBObjects4Domain(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; domainUID: TGUID);
begin
  inherited RemoveDBObjects4Domain(conn, currentVersionId, domainUID);
end;

procedure TFRE_DB_TEST_APP._UpdateSitemap(const session: TFRE_DB_UserSession);
var
  SiteMapData  : IFRE_DB_Object;
  conn         : IFRE_DB_CONNECTION;
begin
  conn:=session.GetDBConnection;
  SiteMapData  := GFRE_DBI.NewObject;
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main','Main','images_apps/test/sitemap_icon.svg','',0,true);
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Welcome','Welcome','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_WELCOME_MOD.ClassName,0, conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_WELCOME_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Grid','Grid','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_GRID_MOD.ClassName,1,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_GRID_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Chart','Chart','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_CHART_MOD.Classname,4,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_CHART_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Live_Chart','Live Chart','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_LIVE_CHART_MOD.Classname,4,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_LIVE_CHART_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/Grid2','Grid2','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_GRID2_MOD.Classname,5,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_GRID2_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/formtest','Form Test','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_FORMTEST_MOD.Classname,2,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_FORMTEST_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/allgrid','Grid Test','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_ALLGRID_MOD.Classname,2,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_ALLGRID_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/feedbrowser','Feed Browser','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.Classname,2,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/TGF','TreeGridForm','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.Classname,0,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_GRIDTREEFORM_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/EDIT','Editors','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_EDITORS_MOD.Classname,0,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_EDITORS_MOD));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/SVG','SVG','images_apps/test/sitemap_icon.svg',TFRE_DB_TEST_APP_SVG_MOD.Classname,0,conn.SYS.CheckClassRight4MyDomain(sr_FETCH,TFRE_DB_TEST_APP_SVG_MOD));
  FREDB_SiteMap_RadialAutoposition(SiteMapData);
  session.GetSessionAppData(ClassName).Field('SITEMAP').AsObject := SiteMapData;
end;

procedure TFRE_DB_TEST_APP.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin
  admin_dbc.CreateCollection('CHART_MOD_LINE',true);
  admin_dbc.CreateCollection('CHART_MOD_PIE',true);
  admin_dbc.CreateCollection('CHART_MOD_COLUMNS',true);
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
  inherited MySessionPromotion(session);
  if session.IsInteractiveSession then begin
    _UpdateSitemap(session);
  end;
end;


class procedure TFRE_DB_TEST_APP.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION');
end;

function TFRE_DB_TEST_APP.WEB_Messages(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Messages',true,false);
  res.AddInput.Describe('Messages','messages',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.WEB_News(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('News',true,false);
  res.AddInput.Describe('News','news',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.WEB_Calendar(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Calendar',true,false);
  res.AddInput.Describe('Calendar','calendar',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.WEB_Profile(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
var
  res: TFRE_DB_FORM_PANEL_DESC;
begin
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Profile',true,false);
  res.AddInput.Describe('Profile','profile',false,false,true,false,'Value');
  Result:=res;
end;

function TFRE_DB_TEST_APP.WEB_Dialog(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Message','GUGUG',fdbmt_info);
end;

function TFRE_DB_TEST_APP.IMI_RAW_DATA_FEED(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  //writeln('GOT RAW INPUT: ',input.DumpToString());
  result := GFRE_DB_NIL_DESC;
end;

{ TFRE_DB_TEST_A }

class procedure TFRE_DB_TEST_A.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField       ('number',fdbft_UInt32);
  scheme.AddSchemeField       ('number_pb',fdbft_UInt32);
  scheme.AddSchemeField       ('string',fdbft_String);
  scheme.AddSchemeField       ('boolean',fdbft_Boolean);
  scheme.AddSchemeField       ('status',fdbft_String);
  scheme.AddCalcSchemeField   ('icon',fdbft_String,@CALC_CalcIconStatus);
end;

class procedure TFRE_DB_TEST_A.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
   newVersionId := '1.0';
end;

procedure TFRE_DB_TEST_A.CALC_CalcIconStatus(const calc: IFRE_DB_CALCFIELD_SETTER);
var    lstatus_icon : TFRE_DB_String;
       lstatus      : TFRE_DB_String;
begin
  lstatus    := Field('status').AsString;
  case lstatus of
    'OK'      : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_ok.png');
    'WARNING' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_warning.png');
    'FAILURE' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_failure.png');
    'UNKNOWN' : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_unknown.png');
    'NEW'     : lstatus_icon := FREDB_getThemedResource('images_apps/test/signal_unknown.png');
    else raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN ENUM FIELD VALUE SiGNaL Status');
  end;
  calc.SetAsString(lstatus_icon);
end;

//function TFRE_DB_TEST_A.WEB_GetIcon(const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
//var    lstatus_icon : TFRE_DB_String;
//       lstatus      : TFRE_DB_String;
//begin
  //lstatus    := Field('status').AsString;
  //case lstatus of
  //  'OK'      : lstatus_icon := getThemedResource('images_apps/test/signal_ok.png');
  //  'WARNING' : lstatus_icon := getThemedResource('images_apps/test/signal_warning.png');
  //  'FAILURE' : lstatus_icon := getThemedResource('images_apps/test/signal_failure.png');
  //  'UNKNOWN' : lstatus_icon := getThemedResource('images_apps/test/signal_unknown.png');
  //  'NEW'     : lstatus_icon := getThemedResource('images_apps/test/signal_unknown.png');
  //  else raise EFRE_DB_Exception.Create(edb_ERROR,'UNKNOWN ENUM FIELD VALUE SiGNaL Status');
  //end;
  //result := GFRE_DBI.NewObject;
  //result.Field(CalcFieldResultKey(fdbft_String)).AsString:=lstatus_icon;
//end;


procedure CreateTestdata(const dbname: string; const user, pass: string);
var CONN    : IFRE_DB_CONNECTION;
    COLL    : IFRE_DB_COLLECTION;
    lobj    : IFRE_DB_Object;
    lo1,lo2 : IFRE_DB_Object;
    loUID   : TGUID;
    i,t1,t2 : Integer;
    ato     : TFRE_DB_TEST_ALL_TYPES;
begin
  t1 := GFRE_BT.Get_Ticks_ms;

  CONN := GFRE_DBI.NewConnection;
  CONN.Connect(dbname,'admin'+'@'+CFRE_DB_SYS_DOMAIN_NAME,'admin');

  COLL := CONN.GetCollection('COLL_TEST_A');
  for i := 0 to 20 - 1 do begin
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_A);
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
  COLL := CONN.GetCollection('COLL_TEST_A2');
  for i := 0 to 1000 - 1 do begin
    if i mod 100=0 then writeln('ENDLESS ',i);
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_A);
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

  COLL := CONN.GetCollection('COLL_TEST_B');
  for i := 0 to 200 - 1 do begin
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_B);
    lobj.Field('firstname').AsString:='FN_' + IntToStr(i);
    lobj.Field('lastname').AsString:='LN_' + IntToStr(i);
    lobj.Field('pass').AsString:='PASS_' + IntToStr(i);
    lobj.Field('icon').AsString:=FREDB_getThemedResource('images_apps/test/add.png');
    COLL.Store(lobj);
  end;

  COLL := Conn.GetCollection('LINKTARGET');
  lo2  := GFRE_DBI.NewObject;
  lo1  := GFRE_DBI.NewObject;
  lo2.Field('ID').AsString:='LINKOBJ2';
  lo2.Field('data').AsString := 'LO Data2';
  lo1.Field('ID').AsString:='LINKOBJ1';
  lo1.Field('data').AsString := 'LO Data1';
  lo1.Field('LINK2').AsObjectLink := lo2.UID; // Link LO1 to LO2
  loUID := lo1.UID;
  COLL.Store(lo2);
  COLL.Store(lo1);

  COLL := CONN.GetCollection('COLL_TEST_AT');
  CheckDbResult(COLL.DefineIndexOnField('fdbft_GUID',fdbft_GUID,true,true,'ix_uid'),'failed uid index creation ix_uid');
  for i := 0 to 10 - 1 do begin
    //if i mod 100=0 then writeln('AT ENDLESS ',i);
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_ALL_TYPES);
    (lobj.Implementor_HC as TFRE_DB_TEST_ALL_TYPES).Gamble(i);
    lobj.Field('LINK').AsObjectLink := loUID;
    COLL.Store(lobj);
  end;

  CONN.Finalize;

  t2 := GFRE_BT.Get_Ticks_ms;
  //writeln('TIME ',t2-t1);
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_A);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_B);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_ALL_TYPES);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRID_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_ALLGRID_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_FORMTEST_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRID2_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_CHART_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_LIVE_CHART_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_GRIDTREEFORM_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_EDITORS_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_SVG_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_WELCOME_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_FILEDIR);
  GFRE_DBI.Initialize_Extension_Objects;
end;

procedure MetaRegister_Test;
begin
  Register_DB_Extensions;
  FRE_DBBUSINESS.Register_DB_Extensions;
  fre_accesscontrol_common.Register_DB_Extensions;
end;

procedure MetaInitializeDatabase_Test(const dbname: string; const user, pass: string);
begin
end;

procedure MetaRemove_Test(const dbname: string; const user, pass: string);
begin
end;


initialization
  GFRE_DBI_REG_EXTMGR.RegisterNewExtension('TEST',@MetaRegister_Test,@MetaInitializeDatabase_Test,@MetaRemove_Test,@GenerateTestData);
end.

