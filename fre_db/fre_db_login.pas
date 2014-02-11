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
  private
  public
    procedure  SetupApplicationStructure ; override;
    procedure  InternalSetup             ; override;
    procedure  InternalFinalize          ; override;
    function   No_Apps_ForGuests         (const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
  published
    function WEB_OnUIChange      (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_Content         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_LoginDlg        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_doLogin         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_doLogout        (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_BuildSiteMap    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_BuildAppList    (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_TakeOverSession (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
    function WEB_SendPageReload  (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;


implementation


{ TFRE_DB_LOGIN }

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
    dialog      : TFRE_DB_DIALOG_DESC;
    serverFunc  : TFRE_DB_SERVER_FUNC_DESC;

    function _getUIHeader(const caption:String): String;
    begin
      Result:='<table style=''width: 100%; height: 29px;'' border=''0'' cellpadding=''0'' cellspacing=''0''><tbody><tr>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: left; width:145px''><img style=''width: 25px; height: 25px;'' src=''fre_css/'+cFRE_WEB_STYLE+'/images/LOGIN.png''></td>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: center;''>'+caption+'<br></td>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: right; width:145px''><img style=''width: 142px; height:25px;'' src=''fre_css/'+cFRE_WEB_STYLE+'/images/Firmos_WAPS.png''></td>';
      Result:=Result+'</tr></tbody></table>';
    end;

begin
  dlg        := TFRE_DB_LAYOUT_DESC.create.Describe();
  dialog     := WEB_LoginDlg(input,ses,app,conn).Implementor_HC as TFRE_DB_DIALOG_DESC;
  //dialog     := TFRE_DB_DIALOG_DESC.create.Describe('FirmOS WebApp Server Login',0,500,false,false,false);
  //serverFunc := TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'doLogin');
  //dialog.AddButton.Describe('Login',serverFunc,fdbbt_submit);
  //dialog.AddHeader(TFRE_DB_HTML_DESC.create.Describe(_getUIHeader('Login'),35));
  //dialog.AddInput.Describe('Username','uname',true);
  //dialog.AddInput.Describe('Password','pass',true,true,false,false,'',nil,false,true);
  dlg.AddDialog(dialog);
  Result:=dlg;
end;

function TFRE_DB_LOGIN.WEB_OnUIChange(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  ses.GetSessionAppData(ClassName).Field('activeApp').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_LOGIN.WEB_Content(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  dialog        : TFRE_DB_DIALOG_DESC;
  requested_app : String;
  dlg           : TFRE_DB_LAYOUT_DESC;
  serverFunc    : TFRE_DB_SERVER_FUNC_DESC;
  apps          : IFRE_DB_APPLICATION_ARRAY;


  res           : TFRE_DB_TOPMENU_DESC;
  homefuncs     : TFRE_DB_SERVER_FUNC_DESC_ARRAY;

  function BuildCollapsedSiteMap : IFRE_DB_Object;
  var res    : TFRE_DB_TOPMENU_DESC;
      subids : TFRE_DB_StringArray;
      i      : integer;
  begin
    res := TFRE_DB_TOPMENU_DESC.create.Describe();
    res.AddJiraDialogEntry.Describe('Messages','images_apps/login/messages.svg');
    res.AddEntry.Describe('Home','images_apps/login/home.svg',
                          TFRE_DB_SERVER_FUNC_DESC_ARRAY.Create(
                            CWSF(@WEB_BuildSiteMap),
                            CWSF(@WEB_BuildAppList)
                          ),'Home',TFRE_DB_StringArray.Create('SiteMap','AppContainer'),
                          true,true);
    res.updateId:='FirmOSViewport';
    res.windowCaption:='FirmOS Application Server';
    res.AddDialogEntry.Describe('Login','images_apps/login/profile.svg',CWSF(@WEB_LoginDlg));
    result :=  res;
  end;

begin
  case ses.GetSessionState of
    sta_BAD:
      begin
        result := TFRE_DB_MESSAGE_DESC.create.Describe('SESSION FAIL','You can not resume your session ['+ses.GetSessionID+'], because of an failure, please relogin.',fdbmt_wait);
      end;
    sta_ActiveNew,
    sta_ReUsed:
      begin
        conn.FetchApplications(apps);
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
var dialog     : TFRE_DB_DIALOG_DESC;
    session    : TFRE_DB_UserSession;
    scheme     : IFRE_DB_SchemeObject;
    block      : TFRE_DB_INPUT_BLOCK_DESC;
    user       : IFRE_DB_USER;
begin
  dialog     := TFRE_DB_DIALOG_DESC.create.Describe('FirmOS WebApp Server Login',0,600,false,false,false);
  if ses.LoggedIn then begin
    GFRE_DBI.GetSystemSchemeByName('TFRE_DB_USER',scheme);
    block:=dialog.AddBlock.Describe();
    block.AddSchemeFormGroup(scheme.GetInputGroup('main'),ses,false,false,2);
    block.AddSchemeFormGroup(scheme.GetInputGroup('picture'),ses,true,false);
    dialog.AddSchemeFormGroup(scheme.GetInputGroup('descr'),ses,true,false);
    user:=ses.GetLoginUser;
    dialog.FillWithObjectValues(user.Implementor_HC as IFRE_DB_Object,ses);
    dialog.AddButton.Describe('Save',CSFT('saveOperation',user.Implementor_HC as IFRE_DB_Object),fdbbt_submit);
    dialog.AddButton.Describe('Logout',CWSF(@WEB_doLogout),fdbbt_submit);
    dialog.AddButton.Describe('Abort',nil,fdbbt_close);
  end else begin
    dialog.AddButton.Describe('Login',CWSF(@WEB_doLogin),fdbbt_submit);
    //dialog.AddButton.Describe('Abort',nil,fdbbt_close);
    dialog.AddInput.Describe('Username','uname',true);
    dialog.AddInput.Describe('Password','pass',true,true,false,false,'',nil,false,true);
  end;
  Result:=dialog;
end;

function TFRE_DB_LOGIN.WEB_doLogin(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  pass              : TFRE_DB_INPUT_DESC;
  data              : IFRE_DB_Object;
  res               : TFRE_DB_Errortype;
  promotion_error   : TFRE_DB_String;
  clear_session     : boolean;
  //take_over_content : TFRE_DB_CONTENT_DESC;
  wsf               : TFRE_DB_SERVER_FUNC_DESC;
  //ex_session        : TFRE_DB_UserSession;
begin
  data := input.Field('data').AsObject;
  clear_session := false;
  if data.FieldExists('CLR_SESSION') then begin
    clear_session := data.Field('CLR_SESSION').AsBoolean;
  end;
  case ses.Promote(data.Field('uname').AsString,data.Field('pass').AsString,promotion_error,clear_session,false) of
    pr_OK:
      result := WEB_Content(input,ses,app,ses.GetDBConnection);
    pr_Failed:
      begin
        Result := TFRE_DB_MESSAGE_DESC.Create.Describe('Login Failed',promotion_error,fdbmt_error);
      end;
    pr_TakeoverPrepared:
      begin
        ses.SendServerClientRequest(TFRE_DB_CLOSE_DIALOG_DESC.create);
        Result := TFRE_DB_MESSAGE_DESC.Create.Describe('Already logged in.',promotion_error,fdbmt_confirm,CWSF(@WEB_TakeOverSession));
      end;
    pr_Takeover:
      begin
        ; // Silently ignore {dead session case, reload will come}
        result := GFRE_DB_SUPPRESS_SYNC_ANSWER;
      end
    else
      GFRE_BT.CriticalAbort('unhandled takeover case');
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
  res:=TFRE_DB_SITEMAP_DESC.create.Describe(STYLE_Get_SVG_Definitions);
  for i:=0 to high(apps) do begin
    //InitApp(apps[i].Implementor_HC as TFRE_DB_APPLICATION);
    if (apps[i].ShowInApplicationChooser(Session)) then begin
      main:=res.AddEntry;
      if apps[i].AppClassName='TFRE_DB_TEST_APP' then begin
        nc:=8
      end else begin
        nc:=0;
      end;
      main.Describe(apps[i].GetCaption(ses),'',TFRE_DB_RESTORE_UI_DESC.create.Describe('FirmOSViewport',TFRE_DB_StringArray.create('Home','AppContainer',apps[i].AppClassName)),0,0,apps[i].AppClassName,nc);
      (apps[i].Implementor_HC as TFRE_DB_APPLICATION).AddAppToSiteMap(session,main);
    end;
  end;
  Result:=res;
end;

function TFRE_DB_LOGIN.WEB_BuildAppList(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
var
  res     : TFRE_DB_SUBSECTIONS_DESC;
  apps    : IFRE_DB_APPLICATION_ARRAY;
  i       : Integer;
  sf      : TFRE_DB_SERVER_FUNC_DESC;
  session       : TFRE_DB_UserSession;
  ActiveSection : String;

begin
  session := GetSession(input);
  session.GetDBConnection.FetchApplications(apps);
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
      case session.Promote('','',promotion_error,false,true) of
        pr_Takeover:
          begin
            result := GFRE_DB_NIL_DESC; // answer with nil / the promotion will sent the content as new S-C request ...
            //result := GFRE_DB_SUPPRESS_SYNC_ANSWER; // ANSWER WILL BE SENT AS PART OF PROMOTION
            //result := WEB_Content(input,ex_session,nil,ex_session.GetDBConnection);
          end;
        else
          ;//GFRE_BT.CriticalAbort('unhandled takeover case 2');
      end;
    end
  else
    begin
      result := TFRE_DB_MESSAGE_DESC.create.Describe('No takeover','You choose to not takeover the existing session',fdbmt_info,CWSF(@WEB_SendPageReload));
    end;
end;

function TFRE_DB_LOGIN.WEB_SendPageReload(const input: IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION): IFRE_DB_Object;
begin
  result := TFRE_DB_OPEN_NEW_LOCATION_DESC.create.Describe('/',false);
end;


end.


