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
  Classes, SysUtils,FOS_TOOL_INTERFACES,unixutil,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE,
  FRE_DBBUSINESS,
  FRE_DB_SYSRIGHT_CONSTANTS;

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
    procedure   CALC_CalcIconStatus(const calc : IFRE_DB_CALCFIELD_SETTER);
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
    procedure Gamble(const id:int64);
  published
    function  IMI_GetIcon   (const input: IFRE_DB_Object): IFRE_DB_Object;
    function  WEB_Content   (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

  { TFRE_DB_TEST_FILEDIR }

  TFRE_DB_TEST_FILEDIR=class(TFRE_DB_ObjectEx)
  protected
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


var
  G_UNSAFE_MODUL_GLOBAL_DATA : IFRE_DB_Object;

type

  { TFRE_DB_TEST_APP }

  TFRE_DB_TEST_APP=class(TFRE_DB_APPLICATION)
  private
    procedure       SetupApplicationStructure     ; override;
    function        InstallAppDefaults            (const conn : IFRE_DB_SYS_CONNECTION):TFRE_DB_Errortype; override;
    function        InstallSystemGroupsandRoles   (const conn : IFRE_DB_SYS_CONNECTION; const domain : TFRE_DB_NameType):TFRE_DB_Errortype; override;

    procedure       _UpdateSitemap            (const session: TFRE_DB_UserSession);
  protected
    procedure       MySessionInitialize       (const session: TFRE_DB_UserSession); override;
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

  { TFRE_DB_TEST_APP_FORMTEST_MOD }

  TFRE_DB_TEST_APP_FORMTEST_MOD = class (TFRE_DB_APPLICATION_MODULE)
  protected
    procedure       SetupAppModuleStructure ; override;
    procedure       MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION); override;
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
  published
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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
  public
    class procedure RegisterSystemScheme (const scheme:IFRE_DB_SCHEMEOBJECT); override;
    procedure       MySessionInitializeModule  (const session: TFRE_DB_UserSession); override;
    procedure       MyServerInitializeModule   (const admin_dbc: IFRE_DB_CONNECTION); override;
  published
    function  WEB_Content                (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Refresh_Browser        (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Browser_Tree           (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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
    function  WEB_Content               (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  IMI_HelloWorld            (const input:IFRE_DB_Object):IFRE_DB_Object;
    function  WEB_HelloWorld            (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function  WEB_Menu                  (const input:IFRE_DB_Object ; const ses: IFRE_DB_Usersession ; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
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

    login  := 'feeder@'+cSYS_DOMAIN;
    if conn.UserExists(login) then begin
      writeln('Modify Groups for User '+login);
      CheckDbResult(conn.ModifyUserGroups(login,GFRE_DBI.ConstructStringArray([Get_Groupname_App_Group_Subgroup('testapp','ADMIN'+'@'+cSYS_DOMAIN),Get_Groupname_App_Group_Subgroup('testapp','USER'+'@'+cSYS_DOMAIN)]),true),'cannot set user groups '+login);
    end;


  finally
    conn.Finalize;
  end;
end;

{ TFRE_DB_TEST_FILEDIR }

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
  end else begin
    Field('typeHR').AsString   := 'Folder';
    Field('sizeHR').AsString := '';
    Field('icon').AsString:=FREDB_getThemedResource('images_apps/test/folder.png');
    Field('icon_open').AsString:=FREDB_getThemedResource('images_apps/test/folder-open.png');
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
  response: IFRE_DB_Object;
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
        res.AddEntry.Describe('Download','','/download'+opaquedata.Field('fileid').AsString);
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
  if ses.InvokeRemoteRequest('SAMPLEFEEDER','GETFILEDIRINFO',inp,response,@GotAnswer,opd)=edb_OK then begin
    result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
  end else begin
    result := TFRE_DB_STORE_DATA_DESC.create.Describe(0);
  end;
end;

function TFRE_DB_TEST_FILEDIR.WEB_CreateZip(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res : TFRE_DB_DIALOG_DESC;
begin
  Result:=GFRE_DB_NIL_DESC;
  res:=TFRE_DB_DIALOG_DESC.create.Describe('ZIP');
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

         procedure addEntry(const obj : IFRE_DB_Object);
         var mypath : string;
             newe   : IFRE_DB_Object;
             entry  : IFRE_DB_Object;
         begin
           inc(cnt);
           entry := obj;
           entry.Field('uidpath').AsStringArr := opaquedata.Field('UIP').AsStringArr;
           mypath                             := opaquedata.Field('LVL').AsString+ entry.Field('name').AsString +'/';
           entry.Field('mypath').AsString     := mypath;
           newe :=  entry.CloneToNewObject();
           res.addTreeEntry(newe,newe.Field('isfile').AsBoolean=false);
         end;

    begin
      res:=TFRE_DB_STORE_DATA_DESC.create.Describe(0);
      cnt := 0;
      new_input.ForAllObjects(@addEntry);
      res.Describe(cnt);
      ses.SendServerClientAnswer(res,ocid);
    end;

begin
  writeln('BROWSE CALL INPUT ',input.DumpToString());
  inp := GFRE_DBI.NewObject;
  lvl := input.Field('parentid').AsString;
  inp.Field('level').AsString:= lvl;

  opd := GFRE_DBI.NewObject;
  opd.Field('UIP').AsGUIDArr := self.GetUIDPathUA;
  opd.Field('LVL').AsString  := lvl;

  if ses.InvokeRemoteRequest('SAMPLEFEEDER','BROWSEPATH',inp,response,@GotAnswer,opd)=edb_OK then
    begin
      result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      exit;
    end
  else
    begin
      result := TFRE_DB_STORE_DATA_DESC.create.Describe(0);
    end;
end;

{ TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD }

procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('feedbrowser','$feedbrowsetree_description');
end;

class procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
end;

procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.MySessionInitializeModule(const session: TFRE_DB_UserSession);
var
  DC_Grid      : IFRE_DB_DERIVED_COLLECTION;
  tr_Grid      : IFRE_DB_SIMPLE_TRANSFORM;
begin
  inherited;
  if session.IsInteractiveSession then begin
    DC_Grid := session.NewDerivedCollection('FILEBROWSER');
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid);
    with tr_Grid do begin
      AddOneToOnescheme('name','','Name',dt_string,true,3,'icon','icon_open');
      AddOneToOnescheme('sizeHR','','Size',dt_string,true,1);
      AddOneToOnescheme('typeHR','','Type',dt_string,true,1);
      AddOneToOnescheme('date','','Date',dt_date,true,1);
      AddOneToOnescheme('icon','','',dt_string,false);
      AddOneToOnescheme('icon_open','','',dt_string,false);
      AddOneToOnescheme('mypath','','',dt_string,false);
      AddOneToOnescheme('children','','',dt_string,false);
      AddOneToOnescheme('UIP','uidpath','',dt_string,false);
      AddConstString('_childrenfunc_','ChildrenData',false);
      AddConstString('_funcclassname_','TFRE_DB_TEST_FILEDIR',false);
    end;
    with DC_Grid do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_FILEBROWSER'),'mypath');
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_ShowSearchbox,cdgf_Children,cdgf_ColumnDragable,cdgf_ColumnResizeable],'TreeGrid',TFRE_DB_StringArray.create('name'),'icon',nil,nil,nil);
    end;
  end;
end;

procedure TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
var filedir : TFRE_DB_TEST_FILEDIR;
    coll    : IFRE_DB_COLLECTION;
begin
  inherited MyServerInitializeModule(admin_dbc);
  coll := admin_dbc.Collection('COLL_FILEBROWSER',true,true);
  filedir := TFRE_DB_TEST_FILEDIR.CreateForDB;
  filedir.SetProperties('Virtual Rooot',false,0,0,0);
  CheckDbResult(coll.Store(filedir),'Error creating root entry');
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var  res    : TFRE_DB_SUBSECTIONS_DESC;
     sec    : TFRE_DB_SECTION_DESC;
     menu   : TFRE_DB_MENU_DESC;
begin
  res  := TFRE_DB_SUBSECTIONS_DESC.Create.Describe();
  menu := TFRE_DB_MENU_DESC.create.Describe;
  menu.AddEntry.Describe('REFRESH','',CWSF(@WEB_Refresh_Browser));
  sec := res.AddSection.Describe(CWSF(@WEB_Browser_Tree),'Filebrowser',1);
  sec.SetMenu(menu);
  result := res;
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_Refresh_Browser(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var inp      : IFRE_DB_Object;
    response : IFRE_DB_Object;
    res      : TFRE_DB_Errortype;

    procedure GotAnswer(const ses : IFRE_DB_UserSession ; const input : IFRE_DB_Object ; const status : TFRE_DB_COMMAND_STATUS ; const origcid : QWORD ; const opaquedata : IFRE_DB_Object);
    begin
      ses.SendServerClientRequest(TFRE_DB_MESSAGE_DESC.create.Describe('JUHU',input.DumpToString(),fdbmt_info));
    end;

begin
  inp := GFRE_DBI.NewObject;
  inp.Field('level').AsString:='/';
  res := ses.InvokeRemoteRequest('SAMPLEFEEDER','BROWSEPATH',inp,response,@GotAnswer,nil);
  if res=edb_OK then
    begin
      result := GFRE_DB_NIL_DESC;
    end
  else
    result := TFRE_DB_MESSAGE_DESC.create.Describe('REQ FAILED',CFRE_DB_Errortype[res],fdbmt_info);
end;

function TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.WEB_Browser_Tree(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var DC_Tree : IFRE_DB_DERIVED_COLLECTION;
begin
  DC_Tree := ses.FetchDerivedCollection('FILEBROWSER');
  result  := DC_Tree.GetDisplayDescription;
end;

{ TFRE_DB_TEST_APP_ALLGRID_MOD }

procedure TFRE_DB_TEST_APP_ALLGRID_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('allgrid','$allgrid_description');
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
      AddOneToOnescheme('MYID','','MYID',dt_number);
      AddOneToOnescheme('fdbft_GUID','','GUID',dt_String);
      AddOneToOnescheme('fdbft_Byte','','BYTE',dt_String);
      AddOneToOnescheme('fdbft_Int16','','Int16',dt_String);
      AddOneToOnescheme('fdbft_UInt16','','UInt16',dt_String);
      AddOneToOnescheme('fdbft_Int32','','Int32',dt_String);
      AddOneToOnescheme('fdbft_UInt32','','Uint32',dt_String);
      AddOneToOnescheme('fdbft_Int64','','Int64',dt_String);
      AddOneToOnescheme('fdbft_UInt64','','Uint64',dt_String);
      AddOneToOnescheme('fdbft_Real32','','Real32',dt_String);
      AddOneToOnescheme('fdbft_Real64','','Real64',dt_String);
      AddOneToOnescheme('fdbft_Currency','','Currency',dt_String);
      AddOneToOnescheme('fdbft_String','','String',dt_String);
      AddOneToOnescheme('fdbft_Boolean','','Boolean',dt_boolean);
      AddOneToOnescheme('fdbft_DateTimeUTC','','Datetime',dt_date);
      //AddOneToOnescheme('fdbft_Stream','','',dt_String);
      //AddOneToOnescheme('fdbft_ObjLink','','',dt_String);
    end;

    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,tr_Grid2);
    with tr_Grid2 do begin
      AddOneToOnescheme          ('myid','','My Id',dt_number);
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
      AddMatchingReferencedField (TFRE_DB_NameTypeArray.Create('LINK','LINK2'),'data','data2','Link to Obj2 via Obj1');
    end;


    DC_Grid := session.NewDerivedCollection('DC_ALLTYPES');
    with DC_Grid do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_AT'));
      SetDeriveTransformation(tr_Grid);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Sortable],'This grid test all fieldtypes, beside stream and object');
    end;

    DC_Grid2 := session.NewDerivedCollection('DC_AT_EX');
    with DC_Grid2 do begin
      SetDeriveParent(session.GetDBConnection.Collection('COLL_TEST_AT'));
      SetDeriveTransformation(tr_Grid2);
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Sortable],'This grid shows different extended transformation types');
    end;

  end;
end;

function TFRE_DB_TEST_APP_ALLGRID_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var lGrid   : TFRE_DB_VIEW_LIST_DESC;
    lGrid2  : TFRE_DB_VIEW_LIST_DESC;
    layout  : TFRE_DB_LAYOUT_DESC;
begin
  layout := TFRE_DB_LAYOUT_DESC.create.Describe('MyLayout');

  lGrid  := ses.FetchDerivedCollection('DC_ALLTYPES').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  lGrid2 := ses.FetchDerivedCollection('DC_AT_EX').GetDisplayDescription as TFRE_DB_VIEW_LIST_DESC;
  layout.SetLayout(nil,lGrid,nil,nil,lGrid2,true,0,1,0,0,1);
  result := layout;
end;

{ TFRE_DB_TEST_APP_FORMTEST_MOD }

procedure TFRE_DB_TEST_APP_FORMTEST_MOD.SetupAppModuleStructure;
begin
  inherited SetupAppModuleStructure;
  InitModuleDesc('formtest','$formtest_description');
end;

procedure TFRE_DB_TEST_APP_FORMTEST_MOD.MyServerInitializeModule(const admin_dbc: IFRE_DB_CONNECTION);
var coll : IFRE_DB_COLLECTION;
begin
   coll := admin_dbc.Collection('COLL_TEST_AT');
   G_UNSAFE_MODUL_GLOBAL_DATA := coll.First;
end;

class procedure TFRE_DB_TEST_APP_FORMTEST_MOD.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_APPLICATION_MODULE');
end;

function TFRE_DB_TEST_APP_FORMTEST_MOD.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result := G_UNSAFE_MODUL_GLOBAL_DATA.Invoke('Content',input,ses,app,conn);
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

function TFRE_DB_TEST_B.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res   : TFRE_DB_FORM_DESC;
  scheme: IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('FORM');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',CWSF(@WEB_saveOperation),fdbbt_submit);
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
  list.addFilterEvent(GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED_TREE').getDescriptionStoreId,'uid');

  Result := TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(list,TFRE_DB_LAYOUT_DESC.create.Describe.SetLayout(GetSession(input).FetchDerivedCollection('COLL_TEST_B_DERIVED_TREE').GetDisplayDescription,nil));
end;

{ TFRE_DB_TEST_ALL_TYPES }

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
  scheme.AddSchemeField         ('calculated',fdbft_String);

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


function TFRE_DB_TEST_ALL_TYPES.IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
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
      SetDisplayType(cdt_Listview,[cdgf_Filter,cdgf_ShowSearchbox,cdgf_ColumnDragable,cdgf_ColumnHideable,cdgf_ColumnResizeable,cdgf_Sortable],'',TFRE_DB_StringArray.create('objname'),'',CWSF(@WEB_Menu));
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

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_HelloWorld(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe('Hello','World '+ses.GetSessionID+'/'+app.ObjectName+' :: '+input.Field('SELECTED').AsStringDump,fdbmt_info);
end;

function TFRE_DB_TEST_APP_GRID2_MOD.WEB_Menu(const input: IFRE_DB_Object;const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION;const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res: TFRE_DB_MENU_DESC;
begin
  res:=TFRE_DB_MENU_DESC.create.Describe;
  res.AddEntry.Describe('Menu entry','','/');
  Result:=res;
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
  AddApplicationModule(TFRE_DB_TEST_APP_GRIDTREEFORM_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_EDITORS_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_FORMTEST_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_ALLGRID_MOD.create);
  AddApplicationModule(TFRE_DB_TEST_APP_FEEDBROWSETREE_MOD.create);
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
                      CreateAppText(conn,'$formtest_description','Fieldtypes Formtest','Form Tests','A form to test all possible validators,data types, gauges etc');
                      CreateAppText(conn,'$allgrid_description','Fieldtypes Gridtest','Grid Tests','A grid to test all possible validators,data types, gauges etc');
                      CreateAppText(conn,'$feedbrowsetree_description','Feeder Browser','Feeder Browser','A Module implementing a simple test browser tree');
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
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['vmcontroller']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['tgf']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['edit']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['formtest']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['allgrid']));
    _AddAppRightModules(user_app_rg,GFRE_DBI.ConstructStringArray(['feedbrowser']));

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
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/formtest','Form Test','images_apps/test/sitemap_icon.svg','formtest',2,CheckAppRightModule(conn,'formtest'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/allgrid','Grid Test','images_apps/test/sitemap_icon.svg','allgrid',2,CheckAppRightModule(conn,'allgrid'));
  FREDB_SiteMap_AddRadialEntry(SiteMapData,'Main/feedbrowser','Feed Browser','images_apps/test/sitemap_icon.svg','feedbrowser',2,CheckAppRightModule(conn,'feedbrowser'));
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

//function TFRE_DB_TEST_A.IMI_GetIcon(const input: IFRE_DB_Object): IFRE_DB_Object;
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
  CONN.Connect(dbname,'admin'+'@'+cSYS_DOMAIN,'admin');

  COLL := CONN.Collection('COLL_TEST_A');
  for i := 0 to 100 - 1 do begin
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
  COLL := CONN.Collection('COLL_TEST_A2');
  for i := 0 to 250 - 1 do begin
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

  COLL := CONN.Collection('COLL_TEST_B');
  for i := 0 to 100 - 1 do begin
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_B);
    lobj.Field('firstname').AsString:='FN_' + IntToStr(i);
    lobj.Field('lastname').AsString:='LN_' + IntToStr(i);
    lobj.Field('pass').AsString:='PASS_' + IntToStr(i);
    lobj.Field('icon').AsString:=FREDB_getThemedResource('images_apps/test/add.png');
    COLL.Store(lobj);
  end;

  COLL := Conn.Collection('LINKTARGET');
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

  COLL := CONN.Collection('COLL_TEST_AT');
  for i := 0 to 1000 - 1 do begin
    if i mod 100=0 then writeln('AT ENDLESS ',i);
    lobj := GFRE_DBI.NewObjectScheme(TFRE_DB_TEST_ALL_TYPES);
    (lobj.Implementor_HC as TFRE_DB_TEST_ALL_TYPES).Gamble(i);
    lobj.Field('LINK').AsObjectLink := loUID;
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
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP_WELCOME_MOD);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_APP);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_FILEDIR);
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

