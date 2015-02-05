unit fre_db_login;

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

interface

uses
  Classes, SysUtils,FRE_SYSTEM, FRE_DB_COMMON,FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_DB_WEB_STYLING;


type

  { TFRE_DB_LOGIN }

  TFRE_DB_LOGIN = class (TFRE_DB_APPLICATION)
  protected
    class procedure InstallDBObjects     (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure       MySessionInitialize  (const session: TFRE_DB_UserSession); override;
    procedure       MyServerInitialize   (const admin_dbc: IFRE_DB_CONNECTION); override;
  public
    procedure  SetupApplicationStructure ; override;
    procedure  InternalSetup             ; override;
    procedure  InternalFinalize          ; override;
    function   No_Apps_ForGuests         (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  published
    function WEB_OnUIChange                  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_Content                     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_LoginDlg                    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_doLogin                     (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_doLogout                    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_BuildSiteMap                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_BuildAppList                (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_TakeOverSession             (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_SendPageReload              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_NotificationMenu            (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_NotificationSC              (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_NotificationDelete          (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_NotificationDeleteConfirmed (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


implementation


{ TFRE_DB_LOGIN }

class procedure TFRE_DB_LOGIN.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
  newVersionId:='1.1';

  if (currentVersionId='') then begin
    currentVersionId:='1.0';

    CreateAppText(conn,'profile_diag_cap','Profile');
    CreateAppText(conn,'login_diag_cap','FirmOS WebApp Server Login');

    CreateAppText(conn,'login_uname','Username');
    CreateAppText(conn,'login_pass','Password');

    CreateAppText(conn,'window_cap','FirmOS Application Server');
    CreateAppText(conn,'top_messages','Messages');
    CreateAppText(conn,'top_home','Home');
    CreateAppText(conn,'top_login','Login');
    CreateAppText(conn,'top_profile','Profile');

    CreateAppText(conn,'button_login','Login');
    CreateAppText(conn,'button_logout','Logout');
    CreateAppText(conn,'button_abort','Abort');
    CreateAppText(conn,'button_save','Save');

    CreateAppText(conn,'resume_error_cap','SESSION FAIL');
    CreateAppText(conn,'resume_error_message','You can not resume your session [%session_id%], because of an failure, please relogin.');

    CreateAppText(conn,'login_faild_cap','Login Failed');
    CreateAppText(conn,'already_logged_in_cap','Already logged in');

    CreateAppText(conn,'no_takeover_cap','No takeover');
    CreateAppText(conn,'no_takeover_message','You choose to not takeover the existing session');

    CreateAppText(conn,'login_faild_already_1P','You are already logged in with another client => (%s)');
    CreateAppText(conn,'login_faild_oldnotfound_cap','The old session ID to continue your sesison was not found');
    CreateAppText(conn,'login_takeover_failed','The takeover of the existing session failed, try again');
    CreateAppText(conn,'login_faild_access','Invalid Username/Domain/Passsword combination');
    CreateAppText(conn,'login_faild_suspended','Currently the domain is suspended, no login is possible.');
  end;
  if (currentVersionId='1.0') then begin
    currentVersionId:='1.1';

    CreateAppText(conn,'notif_grid_caption','Notification');
    CreateAppText(conn,'cm_delete_notification','Remove');
    CreateAppText(conn,'error_delete_single_select','Exactly one object has to be selected for deletion.');
    CreateAppText(conn,'notification_delete_diag_cap','Remove Notification');
    CreateAppText(conn,'notification_delete_diag_msg','Remove Notification "%notification_str%"?');
  end;
end;

procedure TFRE_DB_LOGIN.MySessionInitialize(const session: TFRE_DB_UserSession);
var
  transform        : IFRE_DB_SIMPLE_TRANSFORM;
  notification_grid: IFRE_DB_DERIVED_COLLECTION;
  conn             : IFRE_DB_CONNECTION;
  uids             : TFRE_DB_ObjLinkArray;
begin
  inherited MySessionInitialize(session);

  if session.IsInteractiveSession then begin
    conn := session.GetDBConnection;
    GFRE_DBI.NewObjectIntf(IFRE_DB_SIMPLE_TRANSFORM,transform);
    with transform do begin
      AddOneToOnescheme('caption','',FetchAppTextShort(session,'notif_grid_caption'),dt_string,true,true);
      AddOneToOnescheme('details','','',dt_description);
    end;
    notification_grid := session.NewDerivedCollection('NOTIFICATION_GRID');
    with notification_grid do begin
      SetDeriveParent(conn.AdmGetNotificationCollection);
      SetDeriveTransformation(transform);
      SetDisplayType(cdt_Listview,[],'',CWSF(@WEB_NotificationMenu),nil,CWSF(@WEB_NotificationSC));
      uids:=conn.SYS.GetCurrentUserTokenRef.GetUserGroupIDS;
      SetLength(uids,Length(uids)+1);
      uids[Length(uids)-1]:=conn.SYS.GetCurrentUserTokenRef.GetUserUID;
      Filters.AddAutoDependencyFilter('FOR',['TFRE_DB_NOTIFICATION<FOR'],uids);
    end;
  end;
end;

procedure TFRE_DB_LOGIN.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin
  inherited MyServerInitialize(admin_dbc);
end;

procedure TFRE_DB_LOGIN.SetupApplicationStructure;
begin

end;

procedure TFRE_DB_LOGIN.InternalSetup;
begin
   inherited;
end;

procedure TFRE_DB_LOGIN.InternalFinalize;
begin
   writeln('LOGIN APP INTERNAL FINALIZE');
  inherited InternalFinalize;
end;

function TFRE_DB_LOGIN.No_Apps_ForGuests(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dlg         : TFRE_DB_LAYOUT_DESC;
    dialog      : TFRE_DB_FORM_DIALOG_DESC;
    html        : TFRE_DB_HTML_DESC;
begin
  dlg        := TFRE_DB_LAYOUT_DESC.create.Describe();
  dialog     := WEB_LoginDlg(input,ses,app,conn).Implementor_HC as TFRE_DB_FORM_DIALOG_DESC;
  //html       := TFRE_DB_HTML_DESC.create.Describe('<div id="FirmOSSpecial" style="width: 100%; height: 100%; background-color: black; position: absolute; top: 0px; left: 0px;"> <div id="stars"></div><div id="stars2"></div><div id="stars3"></div>'+
  //                                                '<div id="title">'+cFRE_TITLE_OVERRIDE+'</div>');
  html       := TFRE_DB_HTML_DESC.create.Describe('<div id="FirmOSSpecial" style="width: 100%; height: 100%; background-color: black; position: absolute; top: 0px; left: 0px;">'+
                                                  '<div id="title">'+cFRE_TITLE_OVERRIDE+'</div>');
  //dlg.AddFormDialog(dialog);
  //dlg.SetLayout(nil,html,nil,nil,nil,false);
  //dlg.updateId:='FirmOSViewport';
  //Result:=dlg;
  ses.SendServerClientRequest(dialog);
  html.updateId:='FirmOSViewport';
  result := html;
end;

function TFRE_DB_LOGIN.WEB_OnUIChange(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  ses.GetSessionAppData(ClassName).Field('activeApp').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_LOGIN.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dialog        : TFRE_DB_FORM_DIALOG_DESC;
  requested_app : String;
  dlg           : TFRE_DB_LAYOUT_DESC;
  serverFunc    : TFRE_DB_SERVER_FUNC_DESC;
  apps          : IFRE_DB_APPLICATION_ARRAY;
  loginapp      : IFRE_DB_APPLICATION;

  function BuildCollapsedSiteMap : IFRE_DB_Object;
  var res           : TFRE_DB_TOPMENU_DESC;
      subids        : TFRE_DB_StringArray;
      i             : integer;
      uname         : String;
      fl,fn,ln,desc : TFRE_DB_String;


  begin
    ses.ClearUpdatable;
    ses.GetDBConnection.SYS.GetCurrentUserTokenRef.GetUserDetails(fl,fn,ln,desc);
    if ses.LoggedIn then begin
      uname := fn+' '+ln+' |';
      if uname='  |' then
        uname := fl;
    end else begin
      uname := '';
    end;
    res := TFRE_DB_TOPMENU_DESC.create.Describe('','images/home.svg',48,[CWSF(@WEB_BuildSiteMap),CWSF(@WEB_BuildAppList)],'Home',['SiteMap','AppContainer'],uname,CWSF(@WEB_LoginDlg),STYLE_Get_TopMenu_SVG_Definitions,ses.FetchDerivedCollection('NOTIFICATION_GRID').GetDisplayDescription);
    res.updateId:='FirmOSViewport';
    res.windowCaption:=app.FetchAppTextShort(ses,'window_cap');
    result :=  res;
  end;

  procedure TestUpdate;
  var
    session: TFRE_DB_UserSession;
    html   : TFRE_DB_HTML_DESC;
  begin
    Result:=GFRE_DB_NIL_DESC;
    session := GetSession(input);
    html:=TFRE_DB_HTML_DESC.create.Describe('<div> UPDATE </div>');
    html.updateId:='FirmOSSpecial';
    //html.contentId:='FirmOSSpecial';
    session.SendServerClientRequest(html);
  end;

begin
  //TestUpdate;
  case ses.GetSessionState of
    sta_BAD:
      begin
        result := TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'resume_error_cap'),StringReplace(app.FetchAppTextShort(ses,'resume_error_message'),'%session_id%',ses.GetSessionID,[rfReplaceAll]),fdbmt_wait);
      end;
    sta_ActiveNew,
    sta_ReUsed:
      begin
        conn.FetchApplications(apps,loginapp,ses.IsInteractiveSession);
        if Length(apps)=0 then begin
          result := No_Apps_ForGuests(input,ses,app,conn); // => Apps for Guests or Login
          exit;
        end else begin
          result := BuildCollapsedSiteMap;
          exit;
        end;
      end;
    //sta_ReUseNotFound:
    //  begin
    //    // Inform about non found session
    //    //result := TFRE_DB_MESSAGE_DESC.create.Describe('SESSION SUSPENDED','You can not resume your session ['+ses.GetSessionID+'], because it is suspended, please relogin.',fdbmt_error,CWSF(@WEB_SendPageReload));
    //    // Silently discard and send reload
    //    result := WEB_SendPageReload(input,ses,app,conn);
    //  end;
  end;
end;

function TFRE_DB_LOGIN.WEB_LoginDlg(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var dialog     : TFRE_DB_FORM_DIALOG_DESC;
    session    : TFRE_DB_UserSession;
    scheme     : IFRE_DB_SchemeObject;
    block      : TFRE_DB_INPUT_BLOCK_DESC;
    user       : IFRE_DB_Object;
    dlg        : TFRE_DB_LAYOUT_DESC;
    group      : TFRE_DB_INPUT_GROUP_DESC;
begin
  if ses.LoggedIn then begin
    dialog:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'profile_diag_cap'),0,false,false,false);
    GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
    block := dialog.AddBlock.Describe();
    group := block.AddSchemeFormGroup(scheme.GetInputGroup('main_edit'),ses,false,false,2);
    group.AddInput.Describe('Old Password','pass.old',false,false,false,false,'',nil,nil,false,true);  //FIXXME : Languagekey -> also for dialog Sitemap Upper RIGHT (!!)
    group.AddInput.Describe('Password','pass.new',false,true,false,False,'',nil,nil,false,true);
    group.AddInput.Describe('Confirm Password','pass.confirm',false,true,false,False,'',nil,nil,false,true,'pass.new');

    block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,true,false);
    dialog.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
    CheckDbResult(conn.Fetch(conn.sys.GetCurrentUserTokenRef.GetUserUID,user)); //conn.SYS.GetCurrentUserTokenRef.User.Implementor_HC as IFRE_DB_Object).UID
    dialog.FillWithObjectValues(user,ses);
    dialog.AddButton.Describe(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('button_save')),CSFT('saveOperation',user.Implementor_HC as IFRE_DB_Object),fdbbt_submit);
    dialog.AddButton.Describe(app.FetchAppTextShort(ses,'button_logout'),CWSF(@WEB_doLogout),fdbbt_button);
    dialog.AddButton.Describe(app.FetchAppTextShort(ses,'button_abort'),nil,fdbbt_close);
    user.Finalize;
  end else begin
    if cFRE_LOGIN_OVERRIDE='' then
      dialog:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(app.FetchAppTextShort(ses,'login_diag_cap'),0,false,false,false,true,nil,0,true,'firmosLogin')
    else
      dialog:=TFRE_DB_FORM_DIALOG_DESC.create.Describe(cFRE_LOGIN_OVERRIDE,0,false,false,false,true,nil,0,true,'firmosLogin');
    dialog.AddDescription.Describe('','Please enter your Username and Password');
    dialog.AddButton.Describe('>',CWSF(@WEB_doLogin),fdbbt_submit);
    dialog.AddInput.Describe('','uname',true);
    dialog.AddInput.Describe('','pass',true,true,false,false,'',nil,nil,false,true);
  end;
  Result:=dialog;
end;

function TFRE_DB_LOGIN.WEB_doLogin(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  pass              : TFRE_DB_INPUT_DESC;
  data              : IFRE_DB_Object;
  res               : TFRE_DB_Errortype;
  promotion_status  : TFRE_DB_String;
  clear_session     : boolean;
  wsf               : TFRE_DB_SERVER_FUNC_DESC;
  domain            : TFRE_DB_String;
  user              : TFRE_DB_String;
  username          : TFRE_DB_String;

  function ExpandPromotionStatus:TFRE_DB_String;
  begin
    if promotion_status<>'' then
      result := app.FetchAppTextShort(ses,promotion_status)
    else
      result :='';
  end;

begin
  data := input.Field('data').AsObject;
  clear_session := false;
  if data.FieldExists('CLR_SESSION') then begin
    clear_session := data.Field('CLR_SESSION').AsBoolean;
  end;
  username := data.field('uname').AsString;
  if pos('@',username)=0 then {no add given, add configured "default" domain fro system}
    begin
      username:=username+'@'+cFRE_DEFAULT_DOMAIN;
    end;
  FREDB_SplitLocalatDomain(username,user,domain);
  if conn.SYS.IsDomainSuspended(domain) then
    begin
      Result := TFRE_DB_MESSAGE_DESC.Create.Describe(app.FetchAppTextShort(ses,'login_faild_cap'),app.FetchAppTextShort(ses,'login_faild_suspended'),fdbmt_error);
      exit;
    end;
  case ses.Promote(username,data.Field('pass').AsString,promotion_status,clear_session,false,false,['WEBUSER']) of
    pr_OK:
      result := WEB_Content(input,ses,app,ses.GetDBConnection);
    pr_Failed:
      begin
        Result := TFRE_DB_MESSAGE_DESC.Create.Describe(app.FetchAppTextShort(ses,'login_faild_cap'),ExpandPromotionStatus,fdbmt_error);
      end;
    pr_TakeoverPrepared:
      begin
        ses.SendServerClientRequest(TFRE_DB_CLOSE_DIALOG_DESC.create);
        Result := TFRE_DB_MESSAGE_DESC.Create.Describe(app.FetchAppTextShort(ses,'already_logged_in_cap'),ExpandPromotionStatus,fdbmt_confirm,CWSF(@WEB_TakeOverSession));
      end;
    pr_Takeover:
      begin
        ; // Silently ignore { dead session case, reload will come }
        result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      end
    else
      Result := TFRE_DB_MESSAGE_DESC.Create.Describe(app.FetchAppTextShort(ses,'login_faild_cap'),'UNSPECIFIED',fdbmt_error);
  end;
end;

function TFRE_DB_LOGIN.WEB_doLogout(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  ses.Logout;
  result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
end;


function TFRE_DB_LOGIN.WEB_BuildSiteMap(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  session : TFRE_DB_UserSession;
  res     : TFRE_DB_SITEMAP_DESC;
  main     : TFRE_DB_SITEMAP_ENTRY_DESC;
  apps     : IFRE_DB_APPLICATION_ARRAY;
  i,nc     : integer;

begin
  session := GetSession(input);
  apps := session.GetSessionAppArray;
  res:=TFRE_DB_SITEMAP_DESC.create.Describe(STYLE_Get_Sitemap_SVG_Definitions);
  for i:=0 to high(apps) do begin
    //InitApp(apps[i].Implementor_HC as TFRE_DB_APPLICATION);
    if (apps[i].ShowInApplicationChooser(Session)) then begin
      main:=res.AddEntry;
      if apps[i].AppClassName='TFRE_DB_TEST_APP' then begin
        nc:=8
      end else begin
        nc:=0;
      end;
      main.Describe(apps[i].GetCaption(ses),apps[i].GetIcon,TFRE_DB_RESTORE_UI_DESC.create.Describe('FirmOSViewport',TFRE_DB_StringArray.create('Home','AppContainer',apps[i].AppClassName)),0,0,apps[i].AppClassName,nc);
      (apps[i].Implementor_HC as TFRE_DB_APPLICATION).AddAppToSiteMap(session,main);
    end;
  end;
  Result:=res;
end;

function TFRE_DB_LOGIN.WEB_BuildAppList(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res     : TFRE_DB_SUBSECTIONS_DESC;
  apps    : IFRE_DB_APPLICATION_ARRAY;
  loginapp: IFRE_DB_APPLICATION;
  i       : Integer;
  sf      : TFRE_DB_SERVER_FUNC_DESC;
  session       : TFRE_DB_UserSession;
  ActiveSection : String;

begin
  session := GetSession(input);
  session.GetDBConnection.FetchApplications(apps,loginapp,ses.IsInteractiveSession);
  res := TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_hiddentab);
  res.OnUIChange(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(self,'OnUIChange'));
  for i := 0 to high(apps) do begin
    sf:=TFRE_DB_SERVER_FUNC_DESC.create.Describe(apps[i].AsObject,'content');
    res.AddSection.Describe(sf,'Title'+IntToStr(i),i,apps[i].AppClassName);
  end;
  ActiveSection := ses.GetSessionAppData(ClassName).Field('activeApp').AsString;
  res.SetActiveSection(ActiveSection);
  result := res;
end;

function TFRE_DB_LOGIN.WEB_TakeOverSession(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var promotion_error   : TFRE_DB_String;
    session           : TFRE_DB_UserSession;
    res               : TFRE_DB_CONTENT_DESC;
begin
  if input.Field('confirmed').AsBoolean=true then
    begin
      GetSession(input,session,false);
      case session.Promote('','',promotion_error,false,true,false,['WEBUSER']) of
        pr_Takeover:
          begin
            result := GFRE_DB_SUPPRESS_SYNC_ANSWER; { This (guest) session is taken over, the Bound RAC is cleared, no answer will be issued, the new session sends a reload/update on the Bound RAC) }
          end;
        else
          ;//GFRE_BT.CriticalAbort('unhandled takeover case 2');
      end;
    end
  else
    begin
      result := TFRE_DB_MESSAGE_DESC.create.Describe(app.FetchAppTextShort(ses,'no_takeover_cap'),app.FetchAppTextShort(ses,'no_takeover_message'),fdbmt_info,CWSF(@WEB_SendPageReload));
    end;
end;

function TFRE_DB_LOGIN.WEB_SendPageReload(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result := TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false);
end;

function TFRE_DB_LOGIN.WEB_NotificationMenu(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res            : TFRE_DB_MENU_DESC;
  sf             : TFRE_DB_SERVER_FUNC_DESC;
  notificationObj: TFRE_DB_NOTIFICATION;
  dbo            : IFRE_DB_Object;
begin
  conn.FetchAs(FREDB_H2G(input.Field('selected').AsString),TFRE_DB_NOTIFICATION,notificationObj);
  Result:=notificationObj.getMenu(input,ses,app,conn);
  if not assigned(Result) then begin
    res:=TFRE_DB_MENU_DESC.create.Describe;
    if conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_NOTIFICATION) then begin
      sf:=CWSF(@WEB_NotificationDelete);
      sf.AddParam.Describe('selected',input.Field('selected').AsString);
      res.AddEntry.Describe(FetchAppTextShort(ses,'cm_delete_notification'),'',sf);
    end;
    Result:=res;
  end;
end;

function TFRE_DB_LOGIN.WEB_NotificationSC(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_LOGIN.WEB_NotificationDelete(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  sf          : TFRE_DB_SERVER_FUNC_DESC;
  cap,msg     : String;
  notification: IFRE_DB_Object;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_NOTIFICATION) then
     raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.Field('selected').ValueCount<>1 then raise EFRE_DB_Exception.Create(FetchAppTextShort(ses,'error_delete_single_select'));

  sf:=CWSF(@WEB_NotificationDeleteConfirmed);
  sf.AddParam.Describe('selected',input.Field('selected').AsStringArr);
  cap:=FetchAppTextShort(ses,'notification_delete_diag_cap');

  CheckDbResult(conn.Fetch(FREDB_H2G(input.Field('selected').AsStringArr[0]),notification));
  msg:=StringReplace(FetchAppTextShort(ses,'notification_delete_diag_msg'),'%notification_str%',notification.Field('caption').AsString,[rfReplaceAll]);
  Result:=TFRE_DB_MESSAGE_DESC.create.Describe(cap,msg,fdbmt_confirm,sf);
end;

function TFRE_DB_LOGIN.WEB_NotificationDeleteConfirmed(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  i : NativeInt;
begin
  if not conn.sys.CheckClassRight4MyDomain(sr_DELETE,TFRE_DB_NOTIFICATION) then
     raise EFRE_DB_Exception.Create(conn.FetchTranslateableTextShort(FREDB_GetGlobalTextKey('error_no_access')));

  if input.field('confirmed').AsBoolean then begin
    for i:= 0 to input.Field('selected').ValueCount-1 do begin
      CheckDbResult(conn.Delete(FREDB_H2G(input.Field('selected').AsStringArr[i])));
    end;
  end;
  Result := GFRE_DB_NIL_DESC;
end;


end.


