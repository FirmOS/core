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
  Classes, SysUtils,FRE_SYSTEM, FRE_DB_COMMON,FRE_DB_CORE,FOS_REDBLACKTREE_GEN,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_DB_WEB_STYLING;


type

  { TFRE_DB_LOGIN }

  TFRE_DB_LOGIN = class (TFRE_DB_APPLICATION)
  private
  public
    procedure  SetupApplicationStructure; override;
    procedure  InternalSetup           ; override;
    procedure  InternalFinalize        ; override;
    function   IMI_No_Apps_ForGuests   (const input:IFRE_DB_Object) : IFRE_DB_Object;
  published
    function IMI_OnUIChange     (const input:IFRE_DB_Object):IFRE_DB_Object;
    function IMI_Content        (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_LoginDlg       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_doLogin        (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_doLogout       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_startApp       (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_BuildSiteMap   (const input:IFRE_DB_Object): IFRE_DB_Object;
    function IMI_BuildAppList   (const input:IFRE_DB_Object): IFRE_DB_Object;
  end;


implementation


{ TFRE_DB_LOGIN }


procedure TFRE_DB_LOGIN.SetupApplicationStructure;
begin
  ObjectName := 'LOGIN';
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

function TFRE_DB_LOGIN.IMI_No_Apps_ForGuests(const input: IFRE_DB_Object): IFRE_DB_Object;
var dlg         : TFRE_DB_LAYOUT_DESC;
    dialog      : TFRE_DB_DIALOG_DESC;
    serverFunc  : TFRE_DB_SERVER_FUNC_DESC;

    function _getUIHeader(const caption:String): String;
    begin
      Result:='<table style=''width: 100%; height: 29px;'' border=''0'' cellpadding=''0'' cellspacing=''0''><tbody><tr>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: left; width:145px''><img style=''width: 25px; height: 25px;'' src=''fre_css/'+cSTYLE+'/images/LOGIN.png''></td>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: center;''>'+caption+'<br></td>';
      Result:=Result+'<td style=''vertical-align: middle; text-align: right; width:145px''><img style=''width: 142px; height:25px;'' src=''fre_css/'+cSTYLE+'/images/Firmos_WAPS.png''></td>';
      Result:=Result+'</tr></tbody></table>';
    end;

begin
  dlg        := TFRE_DB_LAYOUT_DESC.create.Describe('Public Area');
  dialog     := TFRE_DB_DIALOG_DESC.create.Describe('',0,500,false,false,false);
  serverFunc := TFRE_DB_SERVER_FUNC_DESC.Create.Describe(Self,'doLogin');
  //if assigned(input) then begin
  //  requested_app := input.Field('REQUESTED_APP').AsString;
  //  if input.FieldExists('message') then begin
  //    dialog.AddDescription.Describe('Info',input.Field('message').AsString);
  //  end;
  //end;
  //serverFunc.AddParam.Describe('REQUESTED_APP',requested_app);

  dialog.AddButton.Describe('Login',serverFunc,fdbbt_submit);
  //header := TFRE_DB_HTML_DESC.create.Describe(_getUIHeader('Login'),35);
  dialog.AddHeader(TFRE_DB_HTML_DESC.create.Describe(_getUIHeader('Login'),35));

  dialog.AddInput.Describe('Username','uname',true);
  dialog.AddInput.Describe('Password','pass',true,true,false,false,'',nil,false,true);
  dialog.AddBool.Describe('Clear Session','CLR_SESSION');
  dialog.AddBool.Describe('Session Takeover','FORCE_TAKEOVER');
  dlg.AddDialog(dialog);
  Result:=dlg;
end;

function TFRE_DB_LOGIN.IMI_OnUIChange(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  GetSessionAppData(input).Field('activeApp').AsString:=input.Field('sectionid').AsString;
  Result:=GFRE_DB_NIL_DESC;
end;

function TFRE_DB_LOGIN.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
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
                            CSF(@IMI_BuildSiteMap),
                            CSF(@IMI_BuildAppList)
                          ),'Home',TFRE_DB_StringArray.Create('SiteMap','AppContainer'),
                          true,true);
    res.updateId:='FirmOSViewport';
    res.windowCaption:='FirmOS Application Server';
    res.AddDialogEntry.Describe('Login','images_apps/login/profile.svg',CSF(@IMI_LoginDlg));
    result :=  res;
  end;

begin
  GetSession(input).GetDBConnection.FetchApplications(apps);
  if Length(apps)=0 then begin
    result := IMI_No_Apps_ForGuests(input);
    exit;
  end else begin
    result := BuildCollapsedSiteMap;
    exit;
  end;
end;

function TFRE_DB_LOGIN.IMI_LoginDlg(const input: IFRE_DB_Object): IFRE_DB_Object;
var dialog     : TFRE_DB_DIALOG_DESC;
    session    : TFRE_DB_UserSession;
begin
  dialog     := TFRE_DB_DIALOG_DESC.create.Describe('',0,500,false,false,false);
  session    := GetSession(input);
  if session.LoggedIn then begin
    dialog.AddButton.Describe('Logout',CSF(@IMI_doLogout),fdbbt_submit);
    dialog.AddButton.Describe('Abort',nil,fdbbt_close);
  end else begin
    dialog.AddButton.Describe('Login',CSF(@IMI_doLogin),fdbbt_submit);
    dialog.AddButton.Describe('Abort',nil,fdbbt_close);
    dialog.AddInput.Describe('Username','uname',true);
    dialog.AddInput.Describe('Password','pass',true,true,false,false,'',nil,false,true);
    dialog.AddBool.Describe('Clear Session','CLR_SESSION');
    dialog.AddBool.Describe('Session Takeover','FORCE_TAKEOVER');
  end;
  Result:=dialog;
end;

function TFRE_DB_LOGIN.IMI_doLogin(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  dialog            : TFRE_DB_DIALOG_DESC;
  pass              : TFRE_DB_INPUT_DESC;
  data              : IFRE_DB_Object;
  store             : TFRE_DB_STORE_DESC;
  res               : TFRE_DB_Errortype;
  apps              : IFRE_DB_APPLICATION_ARRAY;
  header            : TFRE_DB_HTML_DESC;
  req_app           : string;
  session           : TFRE_DB_UserSession;
  promotion_error   : TFRE_DB_String;
  defname           : string;
  reference         : TObject;
  clear_session     : boolean;
  session_takeover  : boolean;
  take_over_content : TFRE_DB_CONTENT_DESC;
begin
  GetSession(input,session,false);
  data := input.Field('data').AsObject;
  clear_session := false;
  if data.FieldExists('CLR_SESSION') then begin
    clear_session := data.Field('CLR_SESSION').AsBoolean;
  end;
  session_takeover := false;
  if data.FieldExists('FORCE_TAKEOVER') then begin
    session_takeover := data.Field('FORCE_TAKEOVER').AsBoolean;
  end;
  case session.Promote(data.Field('uname').AsString,data.Field('pass').AsString,promotion_error,clear_session,session_takeover,take_over_content) of
    pr_OK:       result := IMI_Content(input);
    pr_Failed:   Result := TFRE_DB_MESSAGE_DESC.Create.Describe('Login Failed',promotion_error,fdbmt_error);
    pr_Takeover: result := take_over_content;  // I am the Session who takes over the "running" session, thus I changed the bound socket and now I am going to heaven ...
  end;
end;

function TFRE_DB_LOGIN.IMI_doLogout(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  GetSession(input).Logout;
  result := IMI_Content(input);
end;


function TFRE_DB_LOGIN.IMI_startApp(const input: IFRE_DB_Object): IFRE_DB_Object;
var appname     : string;
    session     : TFRE_DB_UserSession;
    global_app  : TFRE_DB_APPLICATION;
    fld         : IFRE_DB_Field;
    idx         : integer;
begin
  abort;
end;

function TFRE_DB_LOGIN.IMI_BuildSiteMap(const input: IFRE_DB_Object): IFRE_DB_Object;
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
      if apps[i].ObjectName='testapp' then begin
        nc:=8
      end else begin
        nc:=0;
      end;
      main.Describe(apps[i].GetDescription(session.GetDBConnection).Getshort,'',TFRE_DB_RESTORE_UI_DESC.create.Describe('FirmOSViewport',TFRE_DB_StringArray.create('Home','AppContainer',apps[i].ObjectName)),0,0,apps[i].ObjectName,nc);
      (apps[i].Implementor_HC as TFRE_DB_APPLICATION).AddAppToSiteMap(session,main);
    end;
  end;
  Result:=res;
end;

function TFRE_DB_LOGIN.IMI_BuildAppList(const input: IFRE_DB_Object): IFRE_DB_Object;
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
    res.AddSection.Describe(sf,'Title'+IntToStr(i),i,apps[i].ObjectName);
  end;
  ActiveSection := GetSessionAppData(input).Field('activeApp').AsString;
  res.SetActiveSection(ActiveSection);
  result := res;
end;


end.


