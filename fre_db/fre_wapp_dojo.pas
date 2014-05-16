unit fre_wapp_dojo;

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
{$interfaces corba}

interface

uses
  Classes, SysUtils, FRE_DB_INTERFACE, FRE_DB_COMMON, FRE_JSON_ACTION, jsonparser,fpjson,FOS_TOOL_INTERFACES,fre_system;

type

  { TFRE_DB_WAPP_DOJO }

  TFRE_DB_WAPP_DOJO = class // --- **TRANSFORMS DB CLIENT AGNOSTIC DESCRIPTIONS INTO HTML/DOJO REPRESENTATION**
  private
   jsContent                           : String;
   procedure jsContentClear            ;
   procedure jsContentAdd              (const str: String);
   function  _getStoreById             (const id: String;const stores: IFRE_DB_ObjectArray): TFRE_DB_STORE_DESC;
   procedure _BuildForm                (const session: TFRE_DB_UserSession; const co:TFRE_DB_FORM_DESC;const isDialog:Boolean; var hasCloseButton: Boolean);
   procedure _BuildButton              (const co:TFRE_DB_BUTTON_DESC; const hiddenFields: IFRE_DB_ObjectArray; const isDialog:Boolean; var hasCloseButton: Boolean);
   procedure _BuildInput               (const co:TFRE_DB_INPUT_DESC);
   procedure _BuildInputNumber         (const co:TFRE_DB_INPUT_NUMBER_DESC);
   procedure _BuildInputDate           (const co:TFRE_DB_INPUT_DATE_DESC);
   procedure _BuildInputRecurrence     (const co:TFRE_DB_INPUT_RECURRENCE_DESC);
   procedure _BuildInputFile           (const session:TFRE_DB_UserSession; const co:TFRE_DB_INPUT_FILE_DESC);
   procedure _BuildInputBool           (const co:TFRE_DB_INPUT_BOOL_DESC);
   procedure _BuildInputChooser        (const session:TFRE_DB_UserSession; const co:TFRE_DB_INPUT_CHOOSER_DESC;const stores: IFRE_DB_ObjectArray);
   procedure _handleFormElement        (const session: TFRE_DB_UserSession; const elem: TFRE_DB_CONTENT_DESC; const formName:String; const stores:IFRE_DB_ObjectArray; var hiddenFields: IFRE_DB_ObjectArray; const groupId: String='';const hidden: Boolean=false);
   function  _BuildParamsObject        (const co:IFRE_DB_ObjectArray; const keyProp: String='key'; const valueProp: String='value'):String;
   function  _BuildJSArray             (const arr:TFRE_DB_StringArray):String;
   function  _AddParams                (const jsVarName:String;const co:IFRE_DB_ObjectArray;const keyProp:String='key';const valueProp:String='value'):String;
   procedure _BuildDialog              (const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co: TFRE_DB_CONTENT_DESC);
   procedure _BuildSubSecTabContainer  (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_SUBSECTIONS_DESC; const tabsHidden: Boolean);
   procedure _BuildSubSecVertContainer (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_SUBSECTIONS_DESC);
   procedure _BuildMenu                (const co:TFRE_DB_MENU_DESC);
   procedure _BuildMenuDef             (const co:TFRE_DB_MENU_DESC);
   function  _getText                  (const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_String): TFRE_DB_String;
   //function  _BuildDataArray           (const co:IFRE_DB_ObjectArray):String;
  public
   procedure BuildContextMenu          (const co:TFRE_DB_MENU_DESC; var contentString,contentType:String);
   procedure BuildFormPanel            (const session: TFRE_DB_UserSession; const co:TFRE_DB_FORM_PANEL_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildFormDialog           (const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE; const co:TFRE_DB_FORM_DIALOG_DESC; var contentString,contentType:String);
   procedure BuildDialog               (const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE; const co:TFRE_DB_DIALOG_DESC; var contentString,contentType:String);
   procedure BuildUpdateForm           (const co:TFRE_DB_UPDATE_FORM_DESC; var contentString,contentType:String);
   procedure BuildRefreshStore         (const co:TFRE_DB_REFRESH_STORE_DESC; var contentString,contentType:String);
   procedure BuildCloseDialog          (const co:TFRE_DB_CLOSE_DIALOG_DESC; var contentString,contentType:String);
   procedure BuildUpdateUIElement      (const co:TFRE_DB_UPDATE_UI_ELEMENT_DESC; var contentString,contentType:String);
   procedure BuildUpdateStore          (const co:TFRE_DB_UPDATE_STORE_DESC; var contentString,contentType:String);
   procedure BuildGridContainer        (const session:TFRE_DB_UserSession; const co:TFRE_DB_VIEW_LIST_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildTreeContainer        (const session:TFRE_DB_UserSession; const co:TFRE_DB_VIEW_TREE_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildLayoutContainer      (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_LAYOUT_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildSubsectionContainer  (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_SUBSECTIONS_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildMain                 (const session:TFRE_DB_UserSession; const co:TFRE_DB_MAIN_DESC; var contentString,contentType:String);
   procedure BuildHtml                 (const session:TFRE_DB_UserSession; const co:TFRE_DB_HTML_DESC; var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildStoreData            (const co:TFRE_DB_STORE_DATA_DESC; var contentString,contentType:String);
   procedure BuildMessage              (const session:TFRE_DB_UserSession; const co:TFRE_DB_MESSAGE_DESC;var contentString,contentType:String);
   procedure BuildUpdateMessageProgress(const co:TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC;var contentString,contentType:String);
   procedure BuildChart                (const session:TFRE_DB_UserSession; const co:TFRE_DB_CHART_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildChartData            (const co:TFRE_DB_CHART_DATA_DESC;var contentString,contentType:String);
   procedure BuildLiveChart            (const session:TFRE_DB_UserSession; const co:TFRE_DB_LIVE_CHART_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildRedefineLiveChart    (const session:TFRE_DB_UserSession; const co:TFRE_DB_REDEFINE_LIVE_CHART_DESC;var contentString,contentType:String);
   procedure BuildLiveChartAtIdxData   (const co:TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC;var contentString,contentType:String);
   procedure BuildLiveChartCompleteData(const co:TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC;var contentString,contentType:String);
   procedure BuildLiveChartInitData    (const co:TFRE_DB_LIVE_CHART_INIT_DATA_DESC;var contentString,contentType:String);
   procedure BuildTopMenu              (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_TOPMENU_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildInvalidSessionData   (const co:TFRE_DB_INVALIDATE_SESSION_DATA_DESC;var contentString,contentType:String);
   procedure BuildSitemap              (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_SITEMAP_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildSitemapEntryUpdate   (const co:TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC;var contentString,contentType:String);
   procedure BuildSVG                  (const session:TFRE_DB_UserSession;const co:TFRE_DB_SVG_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildUpdateSVG            (const co:TFRE_DB_UPDATE_SVG_DESC;var contentString,contentType:String);
   procedure BuildVNC                  (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_VNC_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildShell                (const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co:TFRE_DB_SHELL_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildEditor               (const session:TFRE_DB_UserSession;const co:TFRE_DB_EDITOR_DESC;var contentString,contentType:String;const isInnerContent:Boolean);
   procedure BuildEditorData           (const co:TFRE_DB_EDITOR_DATA_DESC;var contentString,contentType:String);
   procedure BuildOpenNewLocation      (const co:TFRE_DB_OPEN_NEW_LOCATION_DESC;var contentString,contentType:String);
   //procedure BuildResource            (const co:TFRE_DB_RESOURCE_DESC;var contentString,contentType:String);
   //procedure BuildInputGroupProxyData (const co:TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC;var contentString,contentType:String);
  end;

  procedure TransformInvocation(const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const result_intf:IFRE_DB_Object; var rawContent:TFRE_DB_RawByteString;var lContentType:string; const isInnerContent:Boolean=false; const TransformType: TFRE_DB_TRANSFORM_TYPE=fdbtt_post2json);

  var
    gWAC_DOJO : TFRE_DB_WAPP_DOJO;

implementation

  procedure TransformInvocation(const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const result_intf: IFRE_DB_Object; var rawContent:TFRE_DB_RawByteString; var lContentType: string; const isInnerContent: Boolean; const TransformType: TFRE_DB_TRANSFORM_TYPE);
  var result_object : TObject;
      lContent      : String;
  begin
    result_object := result_intf.Implementor_HC;
    if result_object is TFRE_DB_NIL_DESC then begin
      lContent:='';
      lContentType:='';
      exit; // Silent no result ...
    end else
    if result_object is TFRE_DB_MENU_DESC then begin
      gWAC_DOJO.BuildContextMenu(TFRE_DB_MENU_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_STORE_DATA_DESC then begin
      gWAC_DOJO.BuildStoreData(TFRE_DB_STORE_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_FORM_PANEL_DESC then begin
      gWAC_DOJO.BuildFormPanel(session,TFRE_DB_FORM_PANEL_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_FORM_DIALOG_DESC then begin
      gWAC_DOJO.BuildFormDialog(session,command_type,result_object as TFRE_DB_FORM_DIALOG_DESC,lContent,lContentType);
    end else
    if result_object is TFRE_DB_DIALOG_DESC then begin
      gWAC_DOJO.BuildDialog(session,command_type,result_object as TFRE_DB_DIALOG_DESC,lContent,lContentType);
    end else
    if result_object is TFRE_DB_UPDATE_FORM_DESC then begin
      gWAC_DOJO.BuildUpdateForm(result_object as TFRE_DB_UPDATE_FORM_DESC,lContent,lContentType);
    end else
    if result_object is TFRE_DB_UPDATE_UI_ELEMENT_DESC then begin
      gWAC_DOJO.BuildUpdateUIElement(TFRE_DB_UPDATE_UI_ELEMENT_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_REFRESH_STORE_DESC then begin
      gWAC_DOJO.BuildRefreshStore(TFRE_DB_REFRESH_STORE_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_CLOSE_DIALOG_DESC then begin
      gWAC_DOJO.BuildCloseDialog(TFRE_DB_CLOSE_DIALOG_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_UPDATE_STORE_DESC then begin
      gWAC_DOJO.BuildUpdateStore(TFRE_DB_UPDATE_STORE_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_MAIN_DESC then begin
      gWAC_DOJO.BuildMain(session,TFRE_DB_MAIN_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_HTML_DESC then begin
      gWAC_DOJO.BuildHtml(session,TFRE_DB_HTML_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_VIEW_LIST_DESC then begin
      gWAC_DOJO.BuildGridContainer(session,TFRE_DB_VIEW_LIST_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_LAYOUT_DESC then begin
      gWAC_DOJO.BuildLayoutContainer(session,command_type,TFRE_DB_LAYOUT_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_VIEW_TREE_DESC then begin
      gWAC_DOJO.BuildTreeContainer(session,TFRE_DB_VIEW_TREE_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_SUBSECTIONS_DESC then begin
      gWAC_DOJO.BuildSubsectionContainer(session,command_type,TFRE_DB_SUBSECTIONS_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_MESSAGE_DESC then begin
      gWAC_DOJO.BuildMessage(session,TFRE_DB_MESSAGE_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC then begin
      gWAC_DOJO.BuildUpdateMessageProgress(TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_CHART_DESC then begin
      gWAC_DOJO.BuildChart(session,TFRE_DB_CHART_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_CHART_DATA_DESC then begin
      gWAC_DOJO.BuildChartData(TFRE_DB_CHART_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_LIVE_CHART_DESC then begin
      gWAC_DOJO.BuildLiveChart(session,TFRE_DB_LIVE_CHART_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_REDEFINE_LIVE_CHART_DESC then begin
      gWAC_DOJO.BuildRedefineLiveChart(session,TFRE_DB_REDEFINE_LIVE_CHART_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC then begin
      gWAC_DOJO.BuildLiveChartAtIdxData(TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC then begin
      gWAC_DOJO.BuildLiveChartCompleteData(TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_LIVE_CHART_INIT_DATA_DESC then begin
      gWAC_DOJO.BuildLiveChartInitData(TFRE_DB_LIVE_CHART_INIT_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_TOPMENU_DESC then begin
      gWAC_DOJO.BuildTopMenu(session,command_type,TFRE_DB_TOPMENU_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_TOPMENU_DESC then begin
      gWAC_DOJO.BuildInvalidSessionData(TFRE_DB_INVALIDATE_SESSION_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_SITEMAP_DESC then begin
      gWAC_DOJO.BuildSitemap(session,command_type,TFRE_DB_SITEMAP_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC then begin
      gWAC_DOJO.BuildSitemapEntryUpdate(TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_SVG_DESC then begin
      gWAC_DOJO.BuildSVG(session,TFRE_DB_SVG_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_UPDATE_SVG_DESC then begin
      gWAC_DOJO.BuildUpdateSVG(TFRE_DB_UPDATE_SVG_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_VNC_DESC then begin
      gWAC_DOJO.BuildVNC(session,command_type,TFRE_DB_VNC_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_SHELL_DESC then begin
      gWAC_DOJO.BuildShell(session,command_type,TFRE_DB_SHELL_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_EDITOR_DESC then begin
      gWAC_DOJO.BuildEditor(session,TFRE_DB_EDITOR_DESC(result_object),lContent,lContentType,isInnerContent);
    end else
    if result_object is TFRE_DB_EDITOR_DATA_DESC then begin
      gWAC_DOJO.BuildEditorData(TFRE_DB_EDITOR_DATA_DESC(result_object),lContent,lContentType);
    end else
    if result_object is TFRE_DB_OPEN_NEW_LOCATION_DESC then begin
      gWAC_DOJO.BuildOpenNewLocation(TFRE_DB_OPEN_NEW_LOCATION_DESC(result_object),lContent,lContentType);
    end else begin
      raise Exception.Create('UNKNOWN TRANSFORM ['+result_object.ClassName+']');
    end;
    rawContent := lContent;
  end;

  { TFRE_DB_WAPP_DOJO }

  function TFRE_DB_WAPP_DOJO._getText(const conn: IFRE_DB_CONNECTION; const key: TFRE_DB_String): TFRE_DB_String;
  var
    txt: IFRE_DB_TEXT;
  begin
    if conn.FetchTranslateableTextOBJ(FREDB_GetGlobalTextKey(key),txt) then begin
      Result:=txt.Getshort;
      txt.Finalize;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.jsContentClear;
  begin
    jsContent:='';
  end;

  procedure TFRE_DB_WAPP_DOJO.jsContentAdd(const str: String);
  begin
    jsContent:=jsContent+str+#13#10;
  end;

  function TFRE_DB_WAPP_DOJO._getStoreById(const id: String; const stores: IFRE_DB_ObjectArray): TFRE_DB_STORE_DESC;
  var
    i: Integer;
  begin
    for i:=0 to Length(stores)-1 do begin
      if stores[i].Field('id').AsString = id then begin
        Result := stores[i].Implementor_HC as TFRE_DB_STORE_DESC;
        exit;
      end;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildForm(const session: TFRE_DB_UserSession; const co: TFRE_DB_FORM_DESC; const isDialog: Boolean; var hasCloseButton: Boolean);
  var
    i           : Integer;
    fieldtype   : String;
    stores      : IFRE_DB_ObjectArray;
    hiddenFields: IFRE_DB_ObjectArray;
    propsPrefix : String;
    conn        : IFRE_DB_CONNECTION;
  begin
    conn:=session.GetDBConnection;
    if co.FieldExists('stores') then begin
      stores:=co.Field('stores').AsObjectArr;
    end else begin
      stores:=nil;
    end;
    jsContentAdd('"<form dojoType=''FIRMOS.Form'' id='''+co.Field('id').AsString+'_form'' sendchanged='+BoolToStr(co.Field('sendChanged').AsBoolean,'true','false')+' displayonly='+BoolToStr(co.Field('editable').AsBoolean,'false','true')+'"+');
    jsContentAdd('"  data-dojo-props=\""+');
    if co.FieldExists('dbos') then begin
      jsContentAdd('"  dbos:'+_BuildJSArray(co.Field('dbos').AsStringArr)+'"+');
      propsPrefix:=', ';
    end else begin
      propsPrefix:='';
    end;
    if co.FieldExists('onChangeFunc') then begin
      jsContentAdd('"    '+propsPrefix+'onChangeClassname:'''+co.FieldPath('onChangeFunc.class').AsString+''', onChangeFunctionname:'''+co.FieldPath('onChangeFunc.func').AsString+''', "+');
      jsContentAdd('"    onChangeUidPath:'+_BuildJSArray(co.Field('onChangeFunc').AsObject.Field('uidPath').AsStringArr)+' ,onChangeParams:'+_BuildParamsObject(co.Field('onChangeFunc').AsObject.Field('params').AsObjectArr)+',onChangeDelay:'+co.Field('onChangeDelay').AsString+'"+');
    end;
    jsContentAdd('"\""+');
    jsContentAdd('">"+');

    jsContentAdd('"<table class=''firmosFormTable'' style=''width:100%''>"+');
    for i:=0 to co.Field('elements').ValueCount-1 do begin
      _handleFormElement(session,co.Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_CONTENT_DESC,co.Field('id').AsString,stores,hiddenFields);
    end;

    hasCloseButton:=false;
    if co.Field('defaultClose').AsBoolean then begin
      co.Field('buttons').AddObject(TFRE_DB_BUTTON_DESC.create.Describe(_getText(conn,'close'),nil,fdbbt_close));
    end;

    jsContentAdd('"<tr><td colspan=''2'' style=''text-align:center;''>"+');
    for i := 0 to co.Field('buttons').ValueCount - 1 do begin
      _BuildButton(co.Field('buttons').AsObjectItem[i].Implementor_HC as TFRE_DB_BUTTON_DESC,hiddenFields,isDialog,hasCloseButton);
      jsContentAdd('+');
    end;
    jsContentAdd('"</td></tr>"+');
    jsContentAdd('"</table>"+');
    jsContentAdd('"</form>"');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildButton(const co: TFRE_DB_BUTTON_DESC; const hiddenFields: IFRE_DB_ObjectArray; const isDialog:Boolean; var hasCloseButton: Boolean);
  var
    propsPrefix: String;
    bt         : TFRE_DB_BUTTON_TYPE;
  begin
    jsContentAdd('"  <button dojoType=''FIRMOS.FormButton'' "+');
    jsContentAdd('"  data-dojo-props=\""+');
    if co.FieldExists('serverFunc') then begin
      jsContentAdd('"    actionClassname:'''+co.FieldPath('serverFunc.class').AsString+''', actionFunctionname:'''+co.FieldPath('serverFunc.func').AsString+''', "+');
      jsContentAdd('"    actionUidPath:'+_BuildJSArray(co.Field('serverFunc').AsObject.Field('uidPath').AsStringArr)+' ,actionParams:'+_BuildParamsObject(co.Field('serverFunc').AsObject.Field('params').AsObjectArr)+',"+');
      jsContentAdd('"    hiddenFields:'+_BuildParamsObject(hiddenFields,'field','defaultValue')+', isDialog:'+BoolToStr(isDialog,'true','false')+'"+');
      propsPrefix:=', ';
    end else begin
      propsPrefix:='';
    end;
    bt:=String2DBButtonType(co.Field('buttonType').AsString);
    case bt of
      fdbbt_button  : jsContentAdd('"    \" type=''button'' "+');
      fdbbt_submit  : jsContentAdd('"    \" type=''submit'' "+');
      fdbbt_close   : begin
                        jsContentAdd('"    '+propsPrefix+' closeDialog: true \" type=''button'' "+');
                        hasCloseButton:=true;
                      end;
      fdbbt_download: begin
                        jsContentAdd('"    '+propsPrefix+' downloadId: '''+co.Field('downloadId').AsString+''', closeDialog: '+BoolToStr(co.Field('closeDialog').AsBoolean,'true','false')+' \" type=''button'' "+');
                        hasCloseButton:=true;
                      end;
    end;
    jsContentAdd('">'+co.Field('caption').AsString+'</button>"');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInput(const co: TFRE_DB_INPUT_DESC);
  var
    fieldtype: String;
  begin
    if co.Field('isPass').AsBoolean then begin
      fieldtype:='password';
    end else begin
      fieldtype:='text';
    end;
    if co.Field('multiValues').AsBoolean then begin
      jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' dojoType=''FIRMOS.MultiValText'' style=''min-height:35px; width:100%''"+');
    end else begin
      jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' dojoType=''FIRMOS.ValidationTextBox'' type='''+fieldtype+''' style=''width:100%''"+');
    end;
    jsContentAdd('" intermediateChanges=true"+');
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsString<>'' then begin
      jsContentAdd('" value= '''+StringReplace(co.Field('defaultValue').AsString,#10,'\n',[rfReplaceAll]) + '''"+');
    end;
    if co.Field('confirms').AsString<>'' then begin
      jsContentAdd('" confirms='''+co.Field('confirms').AsString+'''"+');
    end;
    if co.Field('required').AsBoolean then begin
      jsContentAdd('" required=true"+');
    end else begin
      if co.Field('groupRequired').AsBoolean then begin
        jsContentAdd('" grouprequired=true"+');
      end;
    end;
    if co.FieldExists('vtype') then begin
      if (co.FieldPath('vtype.allowedChars').AsString<>'') then begin
        jsContentAdd('" forbiddenchars= ''/[^' + StringReplace(co.FieldPath('vtype.allowedChars').AsString,'\','\\',[rfReplaceAll])+']/g''"+');
      end;
      jsContentAdd('" regExp= '''+co.FieldPath('vtype.regExp').AsString+'''"+');
      jsContentAdd('" invalidMessage= '''+co.FieldPath('vtype.helpText').AsString+'''"+');
    end;
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputNumber(const co: TFRE_DB_INPUT_NUMBER_DESC);
  var
    preFix: String;
  begin
    jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+'''"+');
    if co.FieldExists('displaySlider') then begin
      jsContentAdd('" dojoType=''FIRMOS.NumberSlider''"+');
      jsContentAdd('" showvalue='''+BoolToStr(co.Field('showValueField').AsBoolean,'true','false')+'''"+');
    end else begin
      jsContentAdd('" dojoType=''FIRMOS.NumberTextBox''"+');
    end;
    jsContentAdd('" style=''width:100%''"+');
    jsContentAdd('" intermediateChanges=true"+');
    jsContentAdd('" constraints=''{"+');
    if co.Field('digits').AsInt16>-1 then begin
      jsContentAdd('" places: '+co.Field('digits').AsString+'"+');
      preFix:=',';
    end else begin
      preFix:='';
    end;
    if co.Field('steps').AsInt16>-1 then begin
      jsContentAdd('" '+preFix+'steps: '+co.Field('steps').AsString+'"+');
      preFix:=',';
    end;
    if co.FieldExists('minMax') then begin
      jsContentAdd('" '+preFix+'min: '+co.Field('minMax').AsStringArr[0]+'"+');
      jsContentAdd('" ,max: '+co.Field('minMax').AsStringArr[1]+'"+');
    end;
    jsContentAdd('" }''"+');
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsString<>'' then begin
      jsContentAdd('" value='+co.Field('defaultValue').AsString+ '"+');
    end;
    if co.Field('required').AsBoolean then begin
      jsContentAdd('" required=true"+');
    end else begin
      if co.Field('groupRequired').AsBoolean then begin
        jsContentAdd('" grouprequired=true"+');
      end;
    end;
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputDate(const co: TFRE_DB_INPUT_DATE_DESC);
  begin
    jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' dojoType=''FIRMOS.DateTextBox''  style=''width:100%''"+');
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsString<>'' then begin
      jsContentAdd('" displayedValue='''+co.Field('defaultValue').AsString + '''"+');
    end;
    jsContentAdd('" required= '+BoolToStr(co.Field('required').AsBoolean,'true','false') + '"+');
    if not co.Field('required').AsBoolean and co.Field('groupRequired').AsBoolean then begin
      jsContentAdd('" grouprequired=true"+');
    end;
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputRecurrence(const co: TFRE_DB_INPUT_RECURRENCE_DESC);
  begin
    jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' dojoType=''FIRMOS.Recurrence''  style=''width:100%''"+');
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsString<>'' then begin
      jsContentAdd('" value='''+ co.Field('defaultValue').AsString + '''"+');
    end;
    jsContentAdd('" data-dojo-props=''"+');
    jsContentAdd('"rionce:'+BoolToStr(co.Field('rIOnce').AsBoolean,'true','false')+',"+');
    jsContentAdd('"riminute:'+BoolToStr(co.Field('rIMinute').AsBoolean,'true','false')+',"+');
    jsContentAdd('"rihour:'+BoolToStr(co.Field('rIHour').AsBoolean,'true','false')+',"+');
    jsContentAdd('"riday:'+BoolToStr(co.Field('rIDay').AsBoolean,'true','false')+',"+');
    jsContentAdd('"riweek:'+BoolToStr(co.Field('rIWeek').AsBoolean,'true','false')+',"+');
    jsContentAdd('"rimonth:'+BoolToStr(co.Field('rIMonth').AsBoolean,'true','false')+',"+');
    jsContentAdd('"riquarter:'+BoolToStr(co.Field('rIQuarter').AsBoolean,'true','false')+',"+');
    jsContentAdd('"riyear:'+BoolToStr(co.Field('rIYear').AsBoolean,'true','false')+'"+');
    jsContentAdd('"''"+');
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputFile(const session:TFRE_DB_UserSession; const co: TFRE_DB_INPUT_FILE_DESC);
  var
    conn: IFRE_DB_CONNECTION;
    isImg : Boolean;
  begin
    conn:=session.GetDBConnection;
    isImg:=co.FieldExists('vtype') and (co.FieldPath('vtype.id').AsString='image');
    if isImg then begin
      jsContentAdd('"<div id='''+co.Field('id').AsString+'_list'' data-dojo-type=''FIRMOS.FileUpload.Image'' uploaderId='''+co.Field('id').AsString+'''"+');
      if co.FieldPathExists('vtype.configParams') then begin
        if co.FieldPathExists('vtype.configParams.width') then begin
          jsContentAdd('" imgwidth='+co.FieldPath('vtype.configParams.width').AsString+'"+');
        end;
        if co.FieldPathExists('vtype.configParams.height') then begin
          jsContentAdd('" imgheight='+co.FieldPath('vtype.configParams.height').AsString+'"+');
        end;
        if co.FieldPathExists('vtype.configParams.absolute') then begin
          jsContentAdd('" imgabs='+BoolToStr(co.FieldPath('vtype.configParams.absolute').AsBoolean,'true','false')+'"+');
        end;
      end;
    end else begin
      jsContentAdd('"<div id='''+co.Field('id').AsString+'_list'' data-dojo-type=''dojox.form.uploader.FileList'' uploaderId='''+co.Field('id').AsString+''' multiple='''+BoolToStr(co.Field('multiValues').AsBoolean,'true','true') +'''"+');
    end;
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsString<>'' then begin
      jsContentAdd('" value='''+StringReplace(co.Field('defaultValue').AsString,#10,'\n',[rfReplaceAll]) + '''"+');
    end;
    if co.Field('required').AsBoolean then begin
      jsContentAdd('" required=true"+');
    end else begin
      if co.Field('groupRequired').AsBoolean then begin
        jsContentAdd('" grouprequired=true"+');
      end;
    end;
    jsContentAdd('"></div>"+');
    jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' type=''file'' data-dojo-type=''FIRMOS.FileUpload'' label='''+_getText(conn,'in_file_select')+''' style=''width:100%''"+');
    if isImg then begin
      jsContentAdd('" usefilter=''image''"+');
    end;
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputBool(const co: TFRE_DB_INPUT_BOOL_DESC);
  var
    i: Integer;
  begin
    jsContentAdd('"<input id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' type=''checkbox'' dojoType=''FIRMOS.BoolCheckBox''"+');
    if co.Field('disabled').AsBoolean then begin
      jsContentAdd('" disabled"+');
    end;
    if co.Field('defaultValue').AsBoolean then begin
      jsContentAdd('" checked"+');
    end;
    if co.FieldExists('dependentFields') then begin
      jsContentAdd('" dependentfields={"+');
      for i:=0 to co.Field('dependentFields').ValueCount - 1 do begin
        jsContentAdd('"'+co.Field('dependentFields').AsObjectItem[i].Field('fieldName').AsString+':'+BoolToStr(co.Field('dependentFields').AsObjectItem[i].Field('disablesField').AsBoolean,'true','false')+'"+');
      end;
      jsContentAdd('"}"+');
    end;
    jsContentAdd('" >"+');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildInputChooser(const session:TFRE_DB_UserSession; const co: TFRE_DB_INPUT_CHOOSER_DESC; const stores: IFRE_DB_ObjectArray);
  var
    store          : TFRE_DB_STORE_DESC;
    i,j            : Integer;
    conn           : IFRE_DB_CONNECTION;
    preFix         : String;
    store_res_descr: TFRE_DB_STORE_DATA_DESC;
    serverFunc     : TFRE_DB_SERVER_FUNC_DESC;
    caption        : String;
  begin
     store:=_getStoreById(co.FieldPath('store.id').AsString,stores);
     case String2DBChooserDH(co.Field('displayHint').AsString) of
       dh_chooser_radio: begin
                           jsContentAdd('"<select id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' data-dojo-type=''dojox.form.CheckedMultiSelect''  style=''width:100%''"+');
                           if co.Field('disabled').AsBoolean then begin
                             jsContentAdd('" disabled "+');
                           end;
                           jsContentAdd('"  data-dojo-props=''"+');
                           if co.Field('defaultValue').AsString<>'' then begin
                             jsContentAdd('" value: \"'+co.Field('defaultValue').AsString+'\""+');
                           end;
                           jsContentAdd('"''>"+');
                           for i := 0 to store.Field('entries').ValueCount - 1 do begin
                             jsContentAdd('"  <option value='''+store.Field('entries').AsObjectItem[i].Field('value').AsString+'''>'+store.Field('entries').AsObjectItem[i].Field('caption').AsString+'</option>"+');
                           end;
                           jsContentAdd('"</select>"+');
                         end;
       dh_chooser_combo: begin
                           conn:=session.GetDBConnection;
                           jsContentAdd('"<select id='''+co.Field('id').AsString+''' name='''+co.Field('field').AsString+''' data-dojo-type=''FIRMOS.FilteringSelect''  style=''width:100%''"+');
                           jsContentAdd('" intermediateChanges=true"+');
                           if co.Field('disabled').AsBoolean then begin
                             jsContentAdd('" disabled "+');
                           end;
                           jsContentAdd('" required='+BoolToStr(co.Field('required').AsBoolean,'true','false')+'"+');
                           if not co.Field('required').AsBoolean and co.Field('groupRequired').AsBoolean then begin
                             jsContentAdd('" grouprequired=true"+');
                           end;
                           jsContentAdd('"  data-dojo-props=''"+');
                           jsContentAdd('" value: \"'+co.Field('defaultValue').AsString+'\", placeHolder:\"'+_getText(conn,'in_combo_placeholder')+'\""+');
                           if co.Field('dependentInputFields').ValueCount>0 then begin
                             jsContentAdd('", depGroup: \"["+');
                             preFix:='';
                             for i := 0 to co.Field('dependentInputFields').ValueCount - 1 do begin
                               jsContentAdd('" '+preFix+'{inputId: \\\"'+co.Field('dependentInputFields').AsObjectArr[i].Field('inputId').AsString +'\\\", value: \\\"'+co.Field('dependentInputFields').AsObjectArr[i].Field('value').AsString +'\\\"}"+');
                               preFix:=',';
                             end;
                             jsContentAdd('"]\""+');
                           end;
                           if co.Field('filteredStore').ValueCount>0 then begin
                             jsContentAdd('", depStores: \"["+');
                             preFix:='';
                             for i := 0 to co.Field('filteredStore').ValueCount - 1 do begin
                               jsContentAdd('" '+preFix+'{storeId: \\\"'+co.Field('filteredStore').AsObjectArr[i].Field('storeId').AsString +'\\\", refId: \\\"'+co.Field('filteredStore').AsObjectArr[i].Field('refId').AsString +'\\\"}"+');
                               preFix:=',';
                             end;
                             jsContentAdd('"]\""+');
                           end;
                           jsContentAdd('"''>"+');
                           if not co.Field('required').AsBoolean then begin
                             jsContentAdd('"  <option value=''''></option>"+');
                           end;
                           for i := 0 to store.Field('entries').ValueCount - 1 do begin
                             jsContentAdd('"  <option value='''+store.Field('entries').AsObjectItem[i].Field('value').AsString+'''>'+store.Field('entries').AsObjectItem[i].Field('caption').AsString+'</option>"+');
                           end;
                           if store.FieldExists('serverFunc') then begin
                             serverFunc:=store.Field('serverFunc').AsObject.Implementor_HC as TFRE_DB_SERVER_FUNC_DESC;
                             serverFunc.AddParam.Describe('start','0');
                             serverFunc.AddParam.Describe('count','1000'); //FIXXME - define "ALL" parameter
                             store_res_descr:=serverFunc.InternalInvoke(session).Implementor_HC as TFRE_DB_STORE_DATA_DESC;
                             if (store.Field('labelFields').ValueCount=0) then begin
                               store.Field('labelFields').AsStringArr:=TFRE_DB_StringArray.create('text','objname');
                             end;
                             for i:=0 to store_res_descr.Field('data').ValueCount - 1 do begin
                               for j:=0 to store.Field('labelFields').ValueCount -1 do begin
                                 if (store_res_descr.Field('data').AsObjectItem[i].FieldExists(store.Field('labelFields').AsStringArr[j])) then begin
                                   caption:=store_res_descr.Field('data').AsObjectItem[i].Field(store.Field('labelFields').AsStringArr[j]).AsString;
                                   Break;
                                 end;
                                 caption:=store_res_descr.Field('data').AsObjectItem[i].Field(store.Field('idField').AsString).AsString;
                               end;
                               jsContentAdd('"  <option value='''+store_res_descr.Field('data').AsObjectItem[i].Field(store.Field('idField').AsString).AsString+'''>'+caption+'</option>"+');
                             end;
                           end;
                           jsContentAdd('"</select>"+');
       end;
     end;
  end;

  procedure TFRE_DB_WAPP_DOJO._handleFormElement(const session: TFRE_DB_UserSession; const elem: TFRE_DB_CONTENT_DESC; const formName: String; const stores: IFRE_DB_ObjectArray; var hiddenFields: IFRE_DB_ObjectArray;const groupId:String; const hidden:Boolean);
  var
    i             : Integer;
    classl,classr : String;
    labelclass    : String;
    addGroupId    : String;
  begin
    if elem is TFRE_DB_INPUT_BLOCK_DESC then begin
      jsContentAdd('"<tr id='''+elem.Field('id').AsString+'_tr''><td colspan=2>"+');
      for i := 0 to elem.Field('elements').ValueCount - 1 do begin
        jsContentAdd('"<div style=''width:'+FloatToStrF(Trunc(elem.Field('elements').AsObjectItem[i].Field('relSize').AsInt16 / elem.Field('sizeSum').AsInt16 * 10000) / 100,ffFixed,3,2)+'%; float:left;''><table class=''firmosFormTable'' style=''width:100%''>"+');
        _handleFormElement(session,elem.Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_CONTENT_DESC,formName,stores,hiddenFields);
        jsContentAdd('"</table></div>"+');
      end;
      jsContentAdd('"</td></tr>"+');
    end else begin
      if elem is TFRE_DB_INPUT_GROUP_DESC then begin
        //elem.FieldExists('loadFunc')
        if elem.Field('collapsible').AsBoolean then begin
          jsContentAdd('"<tr class=''firmosFormGroupHeaderCollapsible'' id='''+elem.Field('id').AsString+'_tr''><td colspan=2 onclick=''G_UI_COM.toggleFormGroupStatus(\"'+formName+'\",\"'+elem.UID_String+'\");''>"+');
          if groupId<>'' then addGroupId:=' ';
          addGroupId:=addGroupId + elem.UID_String;
          if elem.Field('collapsed').AsBoolean then begin
            classl:='firmosFormGroupShowLeft';
            classr:='firmosFormGroupShowRight';
          end else begin
            classl:='firmosFormGroupHideLeft';
            classr:='firmosFormGroupHideRight';
          end;
          jsContentAdd('"<div id='''+elem.UID_String+'_tl'' class='''+classl+'''></div><div id='''+elem.UID_String+'_tr'' class='''+classr+'''></div><div class=''firmosFormGroupHeaderElementCollapsible''>'+elem.Field('caption').AsString+'</div>"+');
          jsContentAdd('"</td></tr>"+');
        end else begin
          if elem.Field('caption').AsString<>'' then begin
            jsContentAdd('"<tr class=''firmosFormGroupHeader'' id='''+elem.Field('id').AsString+'_tr''><td colspan=2>"+');
            jsContentAdd('"<div class=''firmosFormGroupHeaderElement''>'+elem.Field('caption').AsString+'</div>"+');
            jsContentAdd('"</td></tr>"+');
          end;
        end;
        for i := 0 to elem.Field('elements').ValueCount - 1 do begin
          _handleFormElement(session,elem.Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_CONTENT_DESC,formName,stores,hiddenFields,groupId+addGroupId,elem.Field('collapsed').AsBoolean or hidden);
        end;
      end else begin
        if elem.Field('hidden').AsBoolean then begin
          SetLength(hiddenFields,Length(hiddenFields)+1);
          hiddenFields[Length(hiddenFields)-1]:=elem;
        end else begin
          if elem.Field('required').AsBoolean then begin
            labelclass:='firmosFormLabelRequired';
          end else begin
            labelclass:='firmosFormLabel';
          end;
          if groupId<>'' then begin
            jsContentAdd('"<tr firmosGroup='''+groupId+''' '+BoolToStr(hidden,' style=''display:none;''','')+' id='''+elem.Field('id').AsString+'_tr''>"+');
          end else begin
            jsContentAdd('"<tr id='''+elem.Field('id').AsString+'_tr''>"+');
          end;
          if elem.Field('caption').AsString<>'' then begin
            if elem is TFRE_DB_INPUT_FILE_DESC then begin
              jsContentAdd('"<td class=''firmosFormFileLabelTD''>"+');
            end else begin
              jsContentAdd('"<td class=''firmosFormLabelTD''>"+');
            end;
            jsContentAdd('"<label for='''+elem.Field('id').AsString+''' id='''+elem.Field('id').AsString+'_label'' class='''+labelclass+'''>'+elem.Field('caption').AsString+': </label>"+');
            jsContentAdd('"</td>"+');
          end;
          jsContentAdd('"<td "+');
          if elem.Field('caption').AsString='' then begin
            jsContentAdd('"collspan=''2'' "+');
          end;
          jsContentAdd('"class=''firmosFormInputTD''>"+');
          case elem.ClassName of
            'TFRE_DB_INPUT_DESCRIPTION_DESC': begin
                                                jsContentAdd('"'+elem.Field('defaultValue').AsString+'"+');
                                              end;
            'TFRE_DB_INPUT_DESC': begin
                                    _BuildInput(elem.Implementor_HC as TFRE_DB_INPUT_DESC);
                                  end;
            'TFRE_DB_INPUT_NUMBER_DESC': begin
                                           _BuildInputNumber(elem.Implementor_HC as TFRE_DB_INPUT_NUMBER_DESC);
                                         end;
            'TFRE_DB_INPUT_DATE_DESC': begin
                                         _BuildInputDate(elem.Implementor_HC as TFRE_DB_INPUT_DATE_DESC);
                                       end;
            'TFRE_DB_INPUT_BOOL_DESC': begin
                                         _BuildInputBool(elem.Implementor_HC as TFRE_DB_INPUT_BOOL_DESC);
                                       end;
            'TFRE_DB_INPUT_CHOOSER_DESC': begin
                                           _BuildInputChooser(session,elem.Implementor_HC as TFRE_DB_INPUT_CHOOSER_DESC,stores);
                                          end;
            'TFRE_DB_INPUT_FILE_DESC': begin
                                         _BuildInputFile(session,elem.Implementor_HC as TFRE_DB_INPUT_FILE_DESC);
                                       end;
            'TFRE_DB_INPUT_RECURRENCE_DESC': begin
                                               _BuildInputRecurrence(elem.Implementor_HC as TFRE_DB_INPUT_RECURRENCE_DESC);
                                             end;
          end;
          jsContentAdd('"</td></tr>"+');
        end;
      end;
    end;
  end;

  function TFRE_DB_WAPP_DOJO._BuildParamsObject(const co: IFRE_DB_ObjectArray; const keyProp: String; const valueProp: String): String;
  var
    i: Integer;
  begin
    Result:='{';
    for i := 0 to Length(co) - 1 do begin
      if co[i].FieldExists('asArray') and co[i].Field('asArray').AsBoolean then begin
        Result:=Result+co[i].Field(keyProp).AsString+':';
        Result:=Result+_BuildJSArray(co[i].Field(valueProp).AsStringArr);
      end else begin
        Result:=Result+co[i].Field(keyProp).AsString+':'''+co[i].Field(valueProp).AsString+'''';
      end;
      if i<Length(co) - 1 then begin
        Result:=Result+',';
      end;
    end;
    Result:=Result+'}';
  end;

  function TFRE_DB_WAPP_DOJO._BuildJSArray(const arr: TFRE_DB_StringArray): String;
  var
    i: Integer;
  begin
    Result:='';
    Result:=Result+'[';
    for i := 0 to Length(arr) - 1 do begin
      if i>0 then begin
        Result:=Result+',';
      end;
      Result:=Result + '''' + arr[i] + '''';
    end;
    Result:=Result+']';
  end;

    function TFRE_DB_WAPP_DOJO._AddParams(const jsVarName: String; const co: IFRE_DB_ObjectArray; const keyProp: String; const valueProp: String): String;
  var
    i,j: Integer;
  begin
    Result:='';
    for i := 0 to Length(co) - 1 do begin
      if co[i].FieldExists('asArray') and co[i].Field('asArray').AsBoolean then begin
        Result:=Result+jsVarName+'.'+co[i].Field(keyProp).AsString+'=';
        Result:=Result+_BuildJSArray(co[i].Field(valueProp).AsStringArr)+';';
      end else begin
        Result:=Result+jsVarName+'.'+co[i].Field(keyProp).AsString+'='''+co[i].Field(valueProp).AsString+''';';
      end;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildDialog(const session: TFRE_DB_UserSession; const command_type:TFRE_DB_COMMANDTYPE; const co: TFRE_DB_CONTENT_DESC);
  var
    hasCloseButton : Boolean;
    width          : String;
    tmpContent     : TFRE_DB_RawByteString;
    tmpContentType : String;
    i              : Integer;
    hiddenFields   : IFRE_DB_ObjectArray;
  begin
    if co is TFRE_DB_DIALOG_DESC then begin
      TransformInvocation(session,command_type,co.Field('content').AsObject,tmpContent,tmpContentType,true);
    end;
    jsContentAdd('var diag = new FIRMOS.Dialog({');
    jsContentAdd('   id: "'+co.Field('id').AsString+'_diag"');
    jsContentAdd('  ,title: "'+FREDB_String2EscapedJSString(co.Field('dialogCaption').AsString)+'"');
    if not co.Field('draggable').AsBoolean then begin
      jsContentAdd('  ,draggable: false');
    end;
    if co is TFRE_DB_FORM_DIALOG_DESC then begin
      jsContentAdd('  ,content: ');
      if co.Field('width').AsInt16=0 then begin
        width:='450';
      end else begin
        width:=co.Field('width').AsString;
      end;
      jsContentAdd('  "<div style=''width:'+width+'px''>"+');
      _BuildForm(session,co as TFRE_DB_FORM_DIALOG_DESC,true,hasCloseButton);
      jsContentAdd('  +"</div>"');
    end;
    if co is TFRE_DB_DIALOG_DESC then begin
      hasCloseButton:=true;
      jsContentAdd('  ,_contentObj: '+co.FieldPath('content.id').AsString);
      if co.FieldExists('buttons') then begin
        jsContentAdd('  ,_buttonDef: ');
        SetLength(hiddenFields,0);
        for i := 0 to co.Field('buttons').ValueCount - 1 do begin
          if i>0 then begin
            jsContentAdd('+');
          end;
          _BuildButton(co.Field('buttons').AsObjectItem[i].Implementor_HC as TFRE_DB_BUTTON_DESC,hiddenFields,true,hasCloseButton);
        end;
      end;
      jsContentAdd('  ,_maxHeight: '+co.Field('maxHeight').AsString);
      jsContentAdd('  ,_maxWidth: '+co.Field('maxWidth').AsString);
      jsContentAdd('  ,_percHeight: '+co.Field('percHeight').AsString);
      jsContentAdd('  ,_percWidth: '+co.Field('percWidth').AsString);
    end;
    jsContentAdd('  ,closable: '+BoolToStr(hasCloseButton,'true','false'));
    jsContentAdd('});');

    jsContentAdd('G_UI_COM.dialogLoaded(diag);');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildSubSecTabContainer(const session: TFRE_DB_UserSession; const command_type:TFRE_DB_COMMANDTYPE ; const co: TFRE_DB_SUBSECTIONS_DESC; const tabsHidden: Boolean);
  var
    tab             : IFRE_DB_Object;
    tmpContent      : TFRE_DB_RawByteString;
    tmpContentType  : String;
    i               : Integer;
    sectionDesc     : IFRE_DB_Object;
    updateId        : String;
  begin
    if tabsHidden then begin
      jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.MultiContentContainer({');
    end else begin
      jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.TabContainer({');
    end;
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd(' ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    if co.FieldExists('onUIChange') then begin
      jsContentAdd('  ,UIStateChangeClass: "' + co.FieldPath('onUIChange.class').AsString + '"');
      jsContentAdd('  ,UIStateChangeFunc: "' + co.FieldPath('onUIChange.func').AsString + '"');
      jsContentAdd('  ,UIStateChangeUidPath: ' + _BuildJSArray(co.FieldPath('onUIChange.uidPath').AsStringArr));
      jsContentAdd('  ,UIStateChangeParams: ' + _BuildParamsObject(co.Field('onUIChange').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('});');

    jsContentAdd('var ' + co.Field('id').AsString + '_tabs = new Array();');
    for i:= 0 to co.Field('sections').ValueCount-1 do begin
      tab:=co.Field('sections').AsObjectItem[i];
      if tab.FieldExists('menu') then begin
        jsContentAdd(' var ' + tab.Field('id').AsString + '_cp = new dijit.layout.ContentPane({id: "' + tab.Field('id').AsString + '_cp", region: "center"});');
        jsContentAdd(' var ' + tab.Field('id').AsString + '_bc = new dijit.layout.BorderContainer({');
        jsContentAdd('  id: "' + tab.Field('id').AsString + '"');
        jsContentAdd(' ,isTCElem: true');
        updateId:=tab.Field('id').AsString+'_cp';
      end else begin
        jsContentAdd(' var ' + tab.Field('id').AsString + '_cp = new dijit.layout.ContentPane({');
        jsContentAdd('  id: "' + tab.Field('id').AsString + '"');
        updateId:=tab.Field('id').AsString;
      end;
      jsContentAdd('  ,title: "' + FREDB_String2EscapedJSString(tab.Field('title').AsString) + '"');
      if co.Field('activeSection').AsString=tab.UID_String then begin
        jsContentAdd('  ,selected: true');
      end;
      if co.Field('sections').AsObjectItem[i].FieldExists('content') then begin
        jsContentAdd('  ,_fixContent: true');
      end else begin
        jsContentAdd('  ,_fixContent: false');
      end;
      if co.Field('sections').AsObjectItem[i].FieldExists('contentFunc') then begin
        jsContentAdd('  ,_contentFuncParams:'+_BuildParamsObject(co.Field('sections').AsObjectItem[i].Field('contentFunc').AsObject.Field('params').AsObjectArr));
        jsContentAdd('  ,_contentFunc: {class: "'+ co.Field('sections').AsObjectItem[i].FieldPath('contentFunc.class').AsString + '", func: "' + co.Field('sections').AsObjectItem[i].FieldPath('contentFunc.func').AsString
                                   +'", uidPath: '+_BuildJSArray(co.Field('sections').AsObjectItem[i].FieldPath('contentFunc.uidPath').AsStringArr)+'}');
      end;
      jsContentAdd('  ,_updateId: "' + updateId + '"');
      jsContentAdd('  ,_subSecOrd: ');
      if co.Field('sections').AsObjectItem[i].FieldExists('ord') then begin
        jsContentAdd('    '+co.Field('sections').AsObjectItem[i].Field('ord').AsString);
      end else begin
        jsContentAdd('    1');
      end;
      jsContentAdd('});');

      if co.Field('sections').AsObjectItem[i].FieldExists('content') then begin
        TransformInvocation(session,command_type,co.Field('sections').AsObjectItem[i].Field('content').AsObject,tmpContent,tmpContentType,true);
        jsContentAdd(tab.Field('id').AsString + '_cp.set("content",'+co.Field('sections').AsObjectItem[i].FieldPath('content.id').AsString+');');
      end;
      if tab.FieldExists('menu') then begin
        _BuildMenu(tab.Field('menu').AsObject.Implementor_HC as TFRE_DB_MENU_DESC);
        jsContentAdd(tab.Field('id').AsString + '_bc.addChild(toolbar);');
        jsContentAdd(tab.Field('id').AsString + '_bc.addChild(' + tab.Field('id').AsString + '_cp);');
        jsContentAdd(co.Field('id').AsString + '_tabs['+IntToStr(i)+'] = ' + tab.Field('id').AsString + '_bc;');
      end else begin
        jsContentAdd(co.Field('id').AsString + '_tabs['+IntToStr(i)+'] = ' + tab.Field('id').AsString + '_cp;');
      end;
    end;
    jsContentAdd(co.Field('id').AsString+'.addTabs('+co.Field('id').AsString + '_tabs);');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildSubSecVertContainer(const session: TFRE_DB_UserSession; const command_type: TFRE_DB_COMMANDTYPE; const co: TFRE_DB_SUBSECTIONS_DESC);
  var
    child          : IFRE_DB_Object;
    tmpContent     : TFRE_DB_RawByteString;
    tmpContentType : String;
    i              : Integer;
    sectionDesc    : IFRE_DB_Object;

  begin
    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.SplitContainer({');
    jsContentAdd('   id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('  ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd('  ,class: "borderContainer"');
    jsContentAdd('  ,gutters: false');
    jsContentAdd('});');

    jsContentAdd('var ' + co.Field('id').AsString + '_children = new Array();');
    for i:= 0 to co.Field('sections').ValueCount-1 do begin
      child:=co.Field('sections').AsObjectItem[i];

      if child.FieldExists('content') then begin
        sectionDesc := child.Field('content').AsObject;
      end else begin
        sectionDesc := (child.Field('contentFunc').AsObject.Implementor_HC as TFRE_DB_SERVER_FUNC_DESC).InternalInvoke(session);
      end;
      TransformInvocation(session,command_type,sectionDesc,tmpContent,tmpContentType,true);
      if co.Field('sections').AsObjectItem[i].FieldExists('ord') then begin
        jsContentAdd(sectionDesc.Field('id').AsString+'._subSecOrd='+co.Field('sections').AsObjectItem[i].Field('ord').AsString+';');
      end else begin
        jsContentAdd(sectionDesc.Field('id').AsString+'._subSecOrd=1;');
      end;
      jsContentAdd(sectionDesc.Field('id').AsString+'.splitter=true;');
      jsContentAdd(sectionDesc.Field('id').AsString+'._height= '+FloatToStrF(child.Field('size').AsInt16 / co.Field('sizeSum').AsInt16 * 100,ffFixed,3,2)+';');
      jsContentAdd(co.Field('id').AsString + '_children['+IntToStr(i)+'] = '+sectionDesc.Field('id').AsString+';');
    end;
    jsContentAdd(co.Field('id').AsString + '_children.sort(G_UI_COM.sortFuncSubSecOrd);');
    jsContentAdd(co.Field('id').AsString+'.addChildren('+co.Field('id').AsString + '_children);');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildMenu(const co: TFRE_DB_MENU_DESC);
  begin
    _BuildMenuDef(co);
    jsContentAdd('var toolbar=new FIRMOS.Toolbar({region: "top", menuDef: '+co.Field('id').AsString+'});');
  end;

  procedure TFRE_DB_WAPP_DOJO._BuildMenuDef(const co: TFRE_DB_MENU_DESC);
  var
    i: integer;

    procedure _addEntry(const entry: IFRE_DB_Object);
    var
      i: Integer;
    begin
      jsContentAdd('  {caption: "'+FREDB_String2EscapedJSString(entry.Field('caption').AsString) + '"');
      if (entry.Implementor_HC is TFRE_DB_SUBMENU_DESC) then begin
        jsContentAdd('  ,menu: [');
        for i:=0 to entry.Field('entries').ValueCount-1 do begin
          if (i>0) then begin
            jsContentAdd('  ,');
          end;
          _addEntry(entry.Field('entries').AsObjectItem[i]);
        end;
        jsContentAdd('  ]');
      end else begin
        if entry.FieldExists('serverFunc') then begin
          jsContentAdd('  ,action: {');
          jsContentAdd('     class: "'+entry.FieldPath('serverFunc.class').AsString + '"');
          jsContentAdd('    ,func: "'+entry.FieldPath('serverFunc.func').AsString + '"');
          if entry.FieldPathExists('serverFunc.uidPath') then begin
            jsContentAdd('    ,uidPath: '+_BuildJSArray(entry.FieldPath('serverFunc.uidPath').AsStringArr));
          end;
          jsContentAdd('    ,params: '+_BuildParamsObject(entry.Field('serverFunc').AsObject.Field('params').AsObjectArr));
          jsContentAdd('   }');
        end;
        if entry.FieldExists('downloadId') then begin
          jsContentAdd('  ,downloadId: "'+entry.Field('downloadId').AsString+'"');
        end;
      end;
      if entry.FieldExists('id') then begin
        jsContentAdd('  ,id: "'+entry.Field('id').AsString + '"');
      end;
      if entry.FieldExists('icon') then begin
        jsContentAdd('  ,icon: "'+entry.Field('icon').AsString + '"');
      end;
      jsContentAdd('  ,disabled: ' + BoolToStr(entry.Field('disabled').AsBoolean,'true','false'));
      jsContentAdd('  }');
    end;

  begin
    jsContentAdd('var '+co.Field('id').AsString + ' = [');
    for i:=0 to co.Field('entries').ValueCount-1 do begin
      if (i>0) then begin
        jsContentAdd('  ,');
      end;
      _addEntry(co.Field('entries').AsObjectItem[i]);
    end;
    jsContentAdd('];');
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildContextMenu(const co: TFRE_DB_MENU_DESC; var contentString, contentType: String);
  var
    JsonAction : TFRE_JSON_ACTION;

  begin
    JsonAction := TFRE_JSON_ACTION.Create;
    jsContentClear;
    _BuildMenuDef(co);

    jsContentAdd('G_UI_COM.menuLoaded('+co.Field('id').AsString+');');
    JsonAction.ActionType := jat_jsupdate;
    JsonAction.Action     := jsContent;
    JSonAction.ID         := co.Field('id').AsString;

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildFormPanel(const session: TFRE_DB_UserSession; const co: TFRE_DB_FORM_PANEL_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JsonAction    : TFRE_JSON_ACTION;
    hasCloseButton: Boolean;
    childId       : String;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    if (co.Field('caption').AsString<>'') or co.FieldExists('menu') then begin
      jsContentAdd('var '+co.Field('id').AsString + '_cp = new dijit.layout.ContentPane({');
      jsContentAdd('  id: "' + co.Field('id').AsString + '_cp"');
    end else begin
      jsContentAdd('var '+co.Field('id').AsString + ' = new dijit.layout.ContentPane({');
      jsContentAdd('  id: "' + co.Field('id').AsString + '"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
    end;
    jsContentAdd('  ,style: "overflow-y: auto;"');
    jsContentAdd('  ,content: ');
    jsContentAdd('  "<div>"+');
    _BuildForm(session,co,false,hasCloseButton);
    jsContentAdd('  +"</div>"');
    jsContentAdd('});');

    if co.FieldExists('menu') then begin
      childId:=co.Field('id').AsString + '_bl';
      jsContentAdd(' var ' + co.Field('id').AsString + '_bl = new dijit.layout.BorderContainer({');
      jsContentAdd('  id: "' + co.Field('id').AsString + '_bl"');
      jsContentAdd(' });');
      _BuildMenu(co.Field('menu').AsObject.Implementor_HC as TFRE_DB_MENU_DESC);
      jsContentAdd(co.Field('id').AsString + '_bl.addChild(toolbar);');
      jsContentAdd(co.Field('id').AsString + '_cp.region = "center";');
      jsContentAdd(co.Field('id').AsString + '_bl.addChild('+co.Field('id').AsString + '_cp);');
    end else begin
      childId:=co.Field('id').AsString + '_cp';
    end;

    if co.Field('caption').AsString<>'' then begin
      jsContentAdd(childId + '.title = "'+co.Field('caption').AsString+'";');
      jsContentAdd(childId + '.spanLabel = true;');
      jsContentAdd('var '+co.Field('id').AsString + ' = new dijit.layout.AccordionContainer({');
      jsContentAdd('  id: "' + co.Field('id').AsString + '"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
      jsContentAdd('});');
      jsContentAdd(co.Field('id').AsString + '.addChild('+childId+');');
    end;

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildFormDialog(const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE; const co: TFRE_DB_FORM_DIALOG_DESC; var contentString, contentType: String);
  var
    FJsonAction                    : TFRE_JSON_ACTION;
  begin
    FJsonAction := TFRE_JSON_ACTION.Create;
    jsContentClear;
    _BuildDialog(session,command_type,co);

    FJsonAction.ActionType := jat_jsexecute;
    FJsonAction.Action     := jsContent;
    FJsonAction.ID         := co.Field('id').AsString;
    contentString := FJsonAction.AsString;
    contentType:= 'application/json';

    FJsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildDialog(const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE; const co: TFRE_DB_DIALOG_DESC; var contentString, contentType: String);
  var
    FJsonAction                    : TFRE_JSON_ACTION;
  begin
    FJsonAction := TFRE_JSON_ACTION.Create;
    jsContentClear;
    _BuildDialog(session,command_type,co);

    FJsonAction.ActionType := jat_jsexecute;
    FJsonAction.Action     := jsContent;
    FJsonAction.ID         := co.Field('id').AsString;
    contentString := FJsonAction.AsString;
    contentType:= 'application/json';

    FJsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildUpdateForm(const co: TFRE_DB_UPDATE_FORM_DESC; var contentString, contentType: String);
  var
    JSonAction:TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    if co.FieldExists('formId') then begin
      JsonAction.Action     := 'G_UI_COM.updateForm("'+co.Field('formId').AsString+'",'+co.Field('obj').AsObject.GetAsJSONString()+');';
    end else begin
      JsonAction.Action     := 'G_UI_COM.updateFormDBO('+co.Field('obj').AsObject.GetAsJSONString()+');';
    end;

    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildRefreshStore(const co: TFRE_DB_REFRESH_STORE_DESC; var contentString, contentType: String);
  var
    JSonAction:TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.refreshStore("'+co.Field('storeId').AsString+'");';

    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildCloseDialog(const co: TFRE_DB_CLOSE_DIALOG_DESC; var contentString, contentType: String);
  var
    JSonAction:TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.closeDialog();';

    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildUpdateUIElement(const co: TFRE_DB_UPDATE_UI_ELEMENT_DESC; var contentString, contentType: String);
  var
    JSonAction:TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;
    jsContentClear;

    if co.FieldExists('disableDrag') then begin
      jsContentAdd('G_UI_COM.updateGridDrag("'+co.Field('id').AsString+'",'+BoolToStr(co.Field('disableDrag').AsBoolean,'true','false')+');');
    end else begin
      if co.FieldExists('disabled') then begin
        jsContentAdd('G_UI_COM.updateUIElement("'+co.Field('id').AsString+'",'+BoolToStr(co.Field('disabled').AsBoolean,'true','false')+',"'+co.Field('newCaption').AsString+'","'+co.Field('newHint').AsString+'");');
      end else begin
        _BuildMenuDef(co.Field('menu').AsObject.Implementor_HC as TFRE_DB_MENU_DESC);
        jsContentAdd('G_UI_COM.updateUIElementSubmenu("'+co.Field('id').AsString+'",'+co.Field('menu').AsObject.Field('id').AsString+');');
      end;
    end;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := jsContent;
    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildUpdateStore(const co: TFRE_DB_UPDATE_STORE_DESC; var contentString, contentType: String);
  var
    JSonAction        :TFRE_JSON_ACTION;
    data              :String;
    separator         :String;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    separator:='';
    data:='{';
    if co.FieldExists('total') then begin
      data:=data+'total:'+co.Field('total').AsString;
      separator:=',';
    end;
    if co.FieldExists('updated') then begin
      data:=data+separator+'updated:'+co.Field('updated').AsObjectArrayJSONString;
      separator:=',';
    end;
    if co.FieldExists('new') then begin
      data:=data+separator+'new:'+co.Field('new').AsObjectArrayJSONString;
      separator:=',';
    end;
    if co.FieldExists('deleted') then begin
      data:=data+separator+'deleted:'+_BuildJSArray(co.Field('deleted').AsStringArr);
      separator:=',';
    end;
    data:=data+'}';
    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.updateStore("'+co.Field('storeId').AsString+'",'+data+');';
    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildGridContainer(const session:TFRE_DB_UserSession; const co: TFRE_DB_VIEW_LIST_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction  :TFRE_JSON_ACTION;
    layout      :TFRE_DB_VIEW_LIST_LAYOUT_DESC;
    elem        :IFRE_DB_Object;
    i,sizeSum   :Integer;
    firstElement:Boolean;
    store       :TFRE_DB_STORE_DESC;
    lcVar       :String;
    button      :TFRE_DB_VIEW_LIST_BUTTON_DESC;
    cssString   :String;
    chCol       :Boolean;
    conn        :IFRE_DB_CONNECTION;
  begin
    conn:=session.GetDBConnection;
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    layout:=co.Field('layout').AsObject.Implementor_HC as TFRE_DB_VIEW_LIST_LAYOUT_DESC;

    sizeSum:=0;
    for i:=0 to layout.Field('data').ValueCount - 1 do begin
      elem:=layout.Field('data').AsObjectItem[i];
      if elem.Field('display').AsBoolean then begin
        if elem.FieldExists('size') then begin
          sizeSum:=sizeSum+elem.Field('size').AsInt16;
        end else begin
          sizeSum:=sizeSum+1;
          elem.Field('size').AsInt16:=1;
        end;
      end;
    end;

    firstElement:=true;
    jsContentAdd('var gridLayout = {');
    if co.Field('details').AsBoolean then begin
      jsContentAdd('     _details_: FIRMOS.gridDetailsColumn()');
      firstElement:=false;
    end;
    chCol:=co.Field('children').AsBoolean;
    for i := 0 to layout.Field('data').ValueCount - 1 do begin
      elem:=layout.Field('data').AsObjectItem[i];
      if elem.Field('display').AsBoolean then begin
        if firstElement then begin
          firstElement:=false;
        end else begin
          jsContentAdd('    ,');
        end;
        cssString:=cssString+'G_UI_COM.createCSSRule("grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css","width: '+FloatToStrF(Trunc(elem.Field('size').AsInt16 / sizeSum * 10000) / 100,ffFixed,3,2)+'%;';
        if FREDB_String2DBDisplayType(elem.Field('displayType').AsString)=dt_number_pb then begin
          jsContentAdd('     '+elem.Field('id').AsString+': FIRMOS.gridPBColumn({');
          jsContentAdd('       label: "' + elem.Field('caption').AsString + '"');
          jsContentAdd('      ,sortable: '+BoolToStr(elem.Field('sortable').AsBoolean,'true','false'));
          jsContentAdd('      ,filterable: '+BoolToStr(elem.Field('filterable').AsBoolean,'true','false'));
          jsContentAdd('      ,dataType: "' + elem.Field('displayType').AsString + '"');
          jsContentAdd('      ,maxValue: ' + elem.Field('maxValue').AsString);
          if elem.Field('labelId').AsString<>'' then begin
            jsContentAdd('      ,labelId: "' + elem.Field('labelId').AsString + '"');
          end;
          jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridNumber"');
          jsContentAdd('                                                       })');
        end else begin
          if chCol then begin
            jsContentAdd('     '+elem.Field('id').AsString+': dgrid.tree({allowDuplicates:true, unhidable: true, reorderable: false, ');
          end else begin
            jsContentAdd('     '+elem.Field('id').AsString+': {');
          end;
          jsContentAdd('       label: "' + elem.Field('caption').AsString + '"');
          jsContentAdd('      ,sortable: '+BoolToStr(elem.Field('sortable').AsBoolean,'true','false'));
          jsContentAdd('      ,filterable: '+BoolToStr(elem.Field('filterable').AsBoolean,'true','false'));
          jsContentAdd('      ,dataType: "' + elem.Field('displayType').AsString + '"');
          case FREDB_String2DBDisplayType(elem.Field('displayType').AsString) of
            dt_string : begin
                          if elem.FieldExists('iconId') then begin
                            jsContentAdd('      ,renderCell: function(object, value, node, options) {return this.grid._renderIconCell(object, value, node, options,"'+ elem.Field('iconId').AsString +'","'+ elem.Field('openIconId').AsString+'");}');
                          end;
                          jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridString"');
                        end;
            dt_date   : begin
                          //jsContentAdd('      ,type: dojox.grid.cells.DateTextBox');
                          jsContentAdd('      ,renderCell: function(object, value, node, options) {return this.grid._renderDate(object, value, node, options);}');
                          jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridDate"');
                        end;
            dt_number,
            dt_currency: begin
                           jsContentAdd('      ,widgetClass: dijit.form.NumberTextBox, styles: "text-align: right;"');
                           jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridNumber"');
                         end;
            dt_icon   : begin
                          jsContentAdd('      ,editable: false');
                          jsContentAdd('      ,renderCell: function(object, value, node, options) {return this.grid._renderIcons(object, value, node, options);}');
                          jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridIcon"');
                        end;
            dt_boolean: begin
                          //jsContentAdd('      ,widgetClass: dijit.form.CheckBox');
                          jsContentAdd('      ,className: "grid-' + co.Field('id').AsString + '-' + elem.Field('id').AsString + '-css.firmosGridBoolean"');
                        end;
          end;
          if chCol then begin
            jsContentAdd('     })');
            chCol:=false;
          end else begin
            jsContentAdd('     }');
          end;
        end;
        cssString:=cssString+'");';
      end;
    end;
    jsContentAdd('  };');
    jsContentAdd(cssString);

    store:=co.Field('store').AsObject.Implementor_HC as TFRE_DB_STORE_DESC;
    jsContentAdd('var '+store.Field('id').AsString+' = G_UI_COM.getStore("'+store.Field('id').AsString+'",');
    jsContentAdd(' {');
    jsContentAdd('   id:"'+store.FieldPath('id').AsString+'"');
    jsContentAdd('  ,idAttribute:"'+store.Field('idField').AsString+'"');
    if store.Field('labelFields').ValueCount>0 then begin
      jsContentAdd('  ,labelAttributes: '+_BuildJSArray(store.Field('labelFields').AsStringArr));
    end;
    jsContentAdd('  ,getClassname:"'+store.FieldPath('serverFunc.class').AsString+'"');
    jsContentAdd('  ,getFunctionname:"'+store.FieldPath('serverFunc.func').AsString+'"');
    jsContentAdd('  ,getUidPath: '+_BuildJSArray(store.FieldPath('serverFunc.uidPath').AsStringArr));
    jsContentAdd('  ,getParams: '+_BuildParamsObject(store.Field('serverFunc').AsObject.Field('params').AsObjectArr));
    if store.FieldExists('clearQueryIdFunc') then begin
      jsContentAdd('  ,clearClassname:"'+store.FieldPath('clearQueryIdFunc.class').AsString+'"');
      jsContentAdd('  ,clearFunctionname:"'+store.FieldPath('clearQueryIdFunc.func').AsString+'"');
      jsContentAdd('  ,clearUidPath: '+_BuildJSArray(store.FieldPath('clearQueryIdFunc.uidPath').AsStringArr));
      jsContentAdd('  ,clearParams: '+_BuildParamsObject(store.Field('clearQueryIdFunc').AsObject.Field('params').AsObjectArr));
    end;
    if store.FieldExists('destroyFunc') then begin
      jsContentAdd('  ,destroyClassname:"'+store.FieldPath('destroyFunc.class').AsString+'"');
      jsContentAdd('  ,destroyFunctionname:"'+store.FieldPath('destroyFunc.func').AsString+'"');
      jsContentAdd('  ,destroyUidPath: '+_BuildJSArray(store.FieldPath('destroyFunc.uidPath').AsStringArr));
      jsContentAdd('  ,destroyParams: '+_BuildParamsObject(store.Field('destroyFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('saveFunc') then begin
      jsContentAdd('  ,saveClassname:"'+co.FieldPath('saveFunc.class').AsString+'"');
      jsContentAdd('  ,saveFunctionname:"'+co.FieldPath('saveFunc.func').AsString+'"');
      if co.Field('saveFunc').AsObject.FieldExists('uidPath') then begin
        jsContentAdd('  ,saveUidPath: '+_BuildJSArray(co.FieldPath('saveFunc.uidPath').AsStringArr));
      end;
      jsContentAdd('  ,saveParams: '+_BuildParamsObject(co.Field('saveFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('dropFunc') then begin
      jsContentAdd('  ,dropClassname:"'+co.FieldPath('dropFunc.class').AsString+'"');
      jsContentAdd('  ,dropFunctionname:"'+co.FieldPath('dropFunc.func').AsString+'"');
      jsContentAdd('  ,dropUidPath: '+_BuildJSArray(co.FieldPath('dropFunc.uidPath').AsStringArr));
      jsContentAdd('  ,dropParams: '+_BuildParamsObject(co.Field('dropFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('dragFunc') then begin
      jsContentAdd('  ,dragClassname:"'+co.FieldPath('dragFunc.class').AsString+'"');
      jsContentAdd('  ,dragFunctionname:"'+co.FieldPath('dragFunc.func').AsString+'"');
      jsContentAdd('  ,dragUidPath: '+_BuildJSArray(co.FieldPath('dragFunc.uidPath').AsStringArr));
      jsContentAdd('  ,dragParams: '+_BuildParamsObject(co.Field('dragFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd(' }');
    jsContentAdd(');');

    jsContentAdd('var '+co.Field('id').AsString+'_contextmenu = new FIRMOS.Menu({');
    jsContentAdd('  cType:"grid"');
    if co.FieldExists('itemMenuFunc') then begin
      if co.FieldPathExists('itemMenuFunc.uidPath') then begin
        jsContentAdd(' ,gridUIDPath: '+_BuildJSArray(co.FieldPath('itemMenuFunc.uidPath').AsStringArr));
      end;
      jsContentAdd(' ,gridClassname: "'+co.FieldPath('itemMenuFunc.class').AsString + '"');
      jsContentAdd(' ,gridFunctionname: "'+co.FieldPath('itemMenuFunc.func').AsString + '"');
      jsContentAdd(' ,gridParams: '+_BuildParamsObject(co.Field('itemMenuFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('});');
    //if co.Field('paging').AsBoolean then begin
    //  jsContentAdd('var ' + co.Field('id').AsString + '_grid = new FIRMOS.Grid({');
    //  jsContentAdd('  rowsPerPage: '+co.FieldPath('store.pageSize').AsString+',');
    //  jsContentAdd('  pagingTextBox: true,');
    //  jsContentAdd('  pageSizeOptions: [25,50,100],');
    //end else begin
    jsContentAdd('var ' + co.Field('id').AsString + '_grid = new FIRMOS.OnDemandGrid({');
    //end;
    jsContentAdd('   id: "' + co.Field('id').AsString + '_grid"');
    if not co.Field('multiselect').AsBoolean then begin
      jsContentAdd('  ,selectionMode: "single"');
    end;
    jsContentAdd('  ,deselectOnRefresh: "false"');
    if co.FieldExists('dropId') then begin
      jsContentAdd('  ,dndAcceptType: '+_BuildJSArray(co.Field('dropId').AsStringArr));
      if co.FieldExists('dropClassesMultiple') then begin
        jsContentAdd('  ,dropClassesMultiple: '+_BuildJSArray(co.Field('dropClassesMultiple').AsStringArr));
      end;
      if co.FieldExists('dropClassesSingle') then begin
        jsContentAdd('  ,dropClassesSingle: '+_BuildJSArray(co.Field('dropClassesSingle').AsStringArr));
      end;
    end;
    if co.FieldExists('dragId') then begin
      jsContentAdd('  ,dndSourceType: "'+co.Field('dragId').AsString+'"');
      if co.FieldExists('dragClasses') then begin
        jsContentAdd('  ,dragClasses: '+_BuildJSArray(co.Field('dragClasses').AsStringArr));
      end;
      if co.Field('disableDrag').AsBoolean then begin
        jsContentAdd('  ,dndDisabled: true');
      end;
    end else begin
      jsContentAdd('  ,dndDisabled: true');
    end;
    jsContentAdd('  ,parentId: "' + co.Field('id').AsString + '"');
    if not co.Field('columnHide').AsBoolean then begin
      jsContentAdd('  ,hiderDisabled: true');
    end;
    if not co.Field('columnDrag').AsBoolean then begin
      jsContentAdd('  ,reorderDisabled: true');
    end;
    if not co.Field('columnResize').AsBoolean then begin
      jsContentAdd('  ,resizerDisabled: true');
    end;
    jsContentAdd('  ,store: '+store.Field('id').AsString);
    jsContentAdd('  ,contextMenu: '+co.Field('id').AsString+'_contextmenu');
    if co.Field('details').AsBoolean then begin
      jsContentAdd('  ,showDetailsSection: true');
    end;
    if co.FieldExists('detailsFunc') then begin
      jsContentAdd('  ,detailsClassname:"'+co.FieldPath('detailsFunc.class').AsString+'"');
      jsContentAdd('  ,detailsFunctionname:"'+co.FieldPath('detailsFunc.func').AsString+'"');
      if co.Field('detailsFunc').AsObject.FieldExists('uidPath') then begin
        jsContentAdd('  ,detailsUidPath: '+_BuildJSArray(co.FieldPath('detailsFunc.uidPath').AsStringArr));
      end;
      jsContentAdd('  ,detailsParams: '+_BuildParamsObject(co.Field('detailsFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('selectionDepFunc') then begin
      jsContentAdd('  ,selDepClassname:"'+co.FieldPath('selectionDepFunc.class').AsString+'"');
      jsContentAdd('  ,selDepFunctionname:"'+co.FieldPath('selectionDepFunc.func').AsString+'"');
      if co.Field('selectionDepFunc').AsObject.FieldExists('uidPath') then begin
        jsContentAdd('  ,selDepUidPath: '+_BuildJSArray(co.FieldPath('selectionDepFunc.uidPath').AsStringArr));
      end;
      jsContentAdd('  ,selDepParams: '+_BuildParamsObject(co.Field('selectionDepFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('  ,columns: gridLayout');
    jsContentAdd('  ,allowSelectAll: true');
    jsContentAdd('});');

    if co.Field('title').AsString<>'' then begin
      lcVar:=co.Field('id').AsString +'_lc';
      jsContentAdd('var '+ lcVar + ' = new dijit.layout.BorderContainer({');
      jsContentAdd('   title: "' + FREDB_String2EscapedJSString(co.Field('title').AsString) + '"');
      jsContentAdd('  ,id: "'+co.Field('id').AsString+'_lc"');
    end else begin
      lcVar:=co.Field('id').AsString;
      jsContentAdd('var '+ lcVar +' = new dijit.layout.BorderContainer({');
      jsContentAdd('   id: "'+co.Field('id').AsString+'"');
      if co.FieldExists('destroyNotify') then begin
         jsContentAdd('  ,destroyNotify: true');
         session.registerUpdatableContent(co.Field('id').AsString);
      end;
    end;
    jsContentAdd('  ,gutters: false');
    jsContentAdd('  ,style: "padding: 0px;"');
    jsContentAdd(' });');

    jsContentAdd(' var ' + co.Field('id').AsString + '_cp = new dijit.layout.ContentPane({id: "' + co.Field('id').AsString + '_cp", region: "center", content: '+co.Field('id').AsString+'_grid});');
    jsContentAdd(lcVar + '.addChild('+co.Field('id').AsString+'_cp);');

    if co.FieldExists('buttons') or co.Field('showSearch').AsBoolean or co.FieldExists('menu') then begin
      if co.FieldExists('menu') then begin
        _BuildMenu(co.Field('menu').AsObject.Implementor_HC as TFRE_DB_MENU_DESC);
      end else begin
        jsContentAdd('var toolbar = new dijit.Toolbar({region: "top"});');
      end;

      for i := 0 to co.Field('buttons').ValueCount - 1 do begin
        button:=co.Field('buttons').AsObjectItem[i].Implementor_HC as TFRE_DB_VIEW_LIST_BUTTON_DESC;
        if button.FieldExists('icon') then begin
          jsContentAdd('var rName = "' +button.Field('icon').AsString+'".replace(/[\/\.]/g,"");');
          jsContentAdd('G_UI_COM.createCSSRule(rName,"background-image: url('+button.Field('icon').AsString+');background-repeat: no-repeat; height: 18px;text-align: center;width: 18px;");');
        end;

        jsContentAdd('var button = new FIRMOS.GridButton({');
        jsContentAdd('   id: "'+button.Field('id').AsString+'"');
        if (button.Field('icon').AsString<>'') then begin
          jsContentAdd('  ,iconClass: rName');
        end;
        if button.Field('caption').AsString<>'' then begin
          jsContentAdd('  ,label: "'+button.Field('caption').AsString+'"');
        end else begin
          jsContentAdd('  ,showLabel: false');
        end;
        if String2DBGridButtonDep(button.Field('dep').AsString) in [fdgbd_single,fdgbd_multi] then begin
          jsContentAdd('  ,disabled: true');
        end;
        if (String2DBGridButtonDep(button.Field('dep').AsString)=fdgbd_manual) and button.Field('disabled').AsBoolean then begin
          jsContentAdd('  ,disabled: true');
        end;
        jsContentAdd('  ,actionClassname: "'+co.Field('buttons').AsObjectItem[i].FieldPath('serverFunc.class').AsString + '"');
        jsContentAdd('  ,actionFunctionname: "'+co.Field('buttons').AsObjectItem[i].FieldPath('serverFunc.func').AsString + '"');
        if co.Field('buttons').AsObjectItem[i].FieldPathExists('serverFunc.uidPath') then begin
          jsContentAdd('  ,actionUidPath: '+_BuildJSArray(co.Field('buttons').AsObjectItem[i].FieldPath('serverFunc.uidPath').AsStringArr));
        end;
        jsContentAdd('  ,actionParams: '+_BuildParamsObject(co.Field('buttons').AsObjectItem[i].Field('serverFunc').AsObject.Field('params').AsObjectArr));
        jsContentAdd('});');

        if button.Field('tooltip').AsString<>'' then begin
          jsContentAdd('var tooltip = new FIRMOS.Tooltip({');
          jsContentAdd('   id: "'+button.Field('id').AsString+'_tooltip"');
          jsContentAdd('  ,connectId: [button.domNode]');
          jsContentAdd('  ,label: "'+button.Field('tooltip').AsString+'"');
          jsContentAdd('});');
        end;
        jsContentAdd('toolbar.addChild(button);');
        jsContentAdd(co.Field('id').AsString+'_grid.registerButton(button,"'+co.Field('buttons').AsObjectItem[i].Field('dep').AsString+'");');
      end;
      jsContentAdd(lcVar + '.addChild(toolbar);');
      if co.Field('showSearch').AsBoolean then begin
        jsContentAdd('var input = new dijit.form.TextBox({id: "'+co.Field('id').AsString+'_search",');
        jsContentAdd('   grid_: '+co.Field('id').AsString+'_grid');
        jsContentAdd('  ,style: "float: right; margin: 0 4px;"');
        jsContentAdd('  ,onKeyPress: function(event) {if (event.charOrCode==13) { this.grid_.doSearch(this.get("value")); } }});');
        jsContentAdd('toolbar.addChild(input);');
        jsContentAdd('dojo.place("<label for='''+co.Field('id').AsString+'_search'' style=''margin: 4px 0 0; float: right;''>'+_getText(conn,'search_label')+'</label>", input.domNode, "after");');
      end;
    end;

    if co.Field('title').AsString<>'' then begin
      jsContentAdd('var '+co.Field('id').AsString + ' = new dijit.layout.AccordionContainer({');
      jsContentAdd('   id: "'+co.Field('id').AsString+'"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
      jsContentAdd('});');
      jsContentAdd(co.Field('id').AsString + '.addChild('+lcVar+');');
    end;

    for i := 0 to co.Field('filteredStore').ValueCount - 1 do begin
      jsContentAdd(co.Field('id').AsString+'_grid.registerDepStore("'+co.Field('filteredStore').AsObjectItem[i].Field('storeId').AsString+'","'+co.Field('filteredStore').AsObjectItem[i].Field('refId').AsString+'");');
    end;

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildTreeContainer(const session:TFRE_DB_UserSession; const co: TFRE_DB_VIEW_TREE_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction :TFRE_JSON_ACTION;
    store      :TFRE_DB_STORE_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    store:=co.Field('store').AsObject.Implementor_HC as TFRE_DB_STORE_DESC;
    jsContentAdd('var '+store.Field('id').AsString+' = G_UI_COM.getStore("'+store.Field('id').AsString+'",');
    jsContentAdd(' {');
    jsContentAdd('   id:"'+store.FieldPath('id').AsString+'"');
    jsContentAdd('  ,idAttribute:"'+store.Field('idField').AsString+'"');
    if store.Field('labelFields').ValueCount>0 then begin
      jsContentAdd('  ,labelAttributes: '+_BuildJSArray(store.Field('labelFields').AsStringArr));
    end;
    jsContentAdd('  ,getClassname:"'+store.FieldPath('serverFunc.class').AsString+'"');
    jsContentAdd('  ,getFunctionname:"'+store.FieldPath('serverFunc.func').AsString+'"');
    jsContentAdd('  ,getUidPath: '+_BuildJSArray(store.FieldPath('serverFunc.uidPath').AsStringArr));
    jsContentAdd('  ,getParams: '+_BuildParamsObject(store.Field('serverFunc').AsObject.Field('params').AsObjectArr));
    if store.FieldExists('clearQueryIdFunc') then begin
      jsContentAdd('  ,clearClassname:"'+store.FieldPath('clearQueryIdFunc.class').AsString+'"');
      jsContentAdd('  ,clearFunctionname:"'+store.FieldPath('clearQueryIdFunc.func').AsString+'"');
      jsContentAdd('  ,clearUidPath: '+_BuildJSArray(store.FieldPath('clearQueryIdFunc.uidPath').AsStringArr));
      jsContentAdd('  ,clearParams: '+_BuildParamsObject(store.Field('clearQueryIdFunc').AsObject.Field('params').AsObjectArr));
    end;
    if store.FieldExists('destroyFunc') then begin
      jsContentAdd('  ,destroyClassname:"'+store.FieldPath('destroyFunc.class').AsString+'"');
      jsContentAdd('  ,destroyFunctionname:"'+store.FieldPath('destroyFunc.func').AsString+'"');
      jsContentAdd('  ,destroyUidPath: '+_BuildJSArray(store.FieldPath('destroyFunc.uidPath').AsStringArr));
      jsContentAdd('  ,destroyParams: '+_BuildParamsObject(store.Field('destroyFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd(' }');
    jsContentAdd(');');

    jsContentAdd('var '+co.Field('id').AsString + '_model = new dijit.tree.TreeStoreModel({');
    jsContentAdd('   store: ' + store.Field('id').AsString);
    jsContentAdd('  ,root: {');
    jsContentAdd('     text: "ROOT"');
    jsContentAdd('    ,children: []');
    jsContentAdd('    ,methodParams: "{functionname: '''+store.FieldPath('serverFunc.func').AsString+''', classname:'''+store.FieldPath('serverFunc.class').AsString+''', uidPath: '+_BuildJSArray(store.FieldPath('serverFunc.uidPath').AsStringArr) + ', params: '+_BuildParamsObject(store.Field('serverFunc').AsObject.Field('params').AsObjectArr)+' }"');
    jsContentAdd('    ,'+store.Field('idField').AsString+': ''' + store.FieldPath('serverFunc.uidPath').AsString + '''');
    jsContentAdd('    ,__id: ''' + store.FieldPath('serverFunc.uidPath').AsString + '''');
    jsContentAdd('    ,_loadChildren: true');
    jsContentAdd('   }');
    jsContentAdd('  ,deferItemLoadingUntilExpand: true');
    jsContentAdd('});');

    jsContentAdd('var '+ co.Field('id').AsString + '_tree = new FIRMOS.Tree({');
    jsContentAdd('   id: "' + co.Field('id').AsString + '_tree"');
    jsContentAdd('  ,parentId: "' + co.Field('id').AsString + '"');
    jsContentAdd('  ,model: ' + co.Field('id').AsString + '_model');
    jsContentAdd('  ,showRoot: false');
    jsContentAdd('});');

    if co.Field('title').AsString<>'' then begin
      jsContentAdd('var '+co.Field('id').AsString + '_cp = new dijit.layout.ContentPane({');
      jsContentAdd('   title: "' + FREDB_String2EscapedJSString(co.Field('title').AsString) + '",');
      jsContentAdd('   id: "'+co.Field('id').AsString+'_cp"');
    end else begin
      jsContentAdd('var '+co.Field('id').AsString + ' = new dijit.layout.ContentPane({');
      jsContentAdd('   id: "'+co.Field('id').AsString+'"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
    end;
    jsContentAdd('  ,content: '+co.Field('id').AsString+'_tree');
    jsContentAdd('});');

    if co.Field('title').AsString<>'' then begin
      jsContentAdd('var '+co.Field('id').AsString + ' = new dijit.layout.AccordionContainer({');
      jsContentAdd('  id: "' + co.Field('id').AsString + '"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
      jsContentAdd('});');
      jsContentAdd(co.Field('id').AsString + '.addChild('+ co.Field('id').AsString + '_cp);');
    end;

    jsContentAdd('var '+co.Field('id').AsString+'_contextmenu_item = new FIRMOS.Menu({cType:"tree"');
    jsContentAdd('  ,selector: ".dijitTreeNode"');
    if co.FieldExists('itemMenuFunc') then begin
      if co.FieldPathExists('itemMenuFunc.uidPath') then begin
        jsContentAdd(' ,treeItemUIDPath: '+_BuildJSArray(co.FieldPath('itemMenuFunc.uidPath').AsStringArr));
      end;
      jsContentAdd(' ,treeItemClassname: "'+co.FieldPath('itemMenuFunc.class').AsString + '"');
      jsContentAdd(' ,treeItemFunctionname: "'+co.FieldPath('itemMenuFunc.func').AsString + '"');
      jsContentAdd(' ,treeItemParams: '+_BuildParamsObject(co.Field('itemMenuFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('});');
    jsContentAdd(co.Field('id').AsString + '_contextmenu_item.bindDomNode('+co.Field('id').AsString + '_tree.domNode);');

    if co.FieldExists('menuFunc') then begin
      jsContentAdd('var '+co.Field('id').AsString+'_contextmenu_tree = new FIRMOS.Menu({cType:"tree"');
      jsContentAdd(' ,selector: "div:not(.dijitTreeNode)"');
      jsContentAdd(' ,treeClassname: "'+co.FieldPath('menuFunc.class').AsString + '"');
      jsContentAdd(' ,treeFunctionname: "'+co.FieldPath('menuFunc.func').AsString + '"');
      jsContentAdd(' ,treeParams: '+_BuildParamsObject(co.Field('menuFunc').AsObject.Field('params').AsObjectArr));
      jsContentAdd(' ,treeUidPath: ' + _BuildJSArray(co.Field('menuFunc').AsObject.Field('uidPath').AsStringArr));
      jsContentAdd('});');
      jsContentAdd(co.Field('id').AsString + '_contextmenu_tree.bindDomNode('+co.Field('id').AsString + '_tree.domNode.parentNode);');
    end;

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

    procedure TFRE_DB_WAPP_DOJO.BuildLayoutContainer(const session: TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE; const co: TFRE_DB_LAYOUT_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction               :TFRE_JSON_ACTION;
    sec                      :IFRE_DB_Object;
    tmpContent               :TFRE_DB_RawByteString;
    tmpContentType           :String;
    relSizeStr               :String;
    useSizeH,useSizeV        :Boolean;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var '+ co.Field('id').AsString +' = new dijit.layout.BorderContainer({');
    jsContentAdd('                 id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('                ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd('                ,class: "borderContainer"});');

    useSizeH:=false;
    useSizeV:=false;
    if co.FieldExists(CFRE_DB_LAYOUT_POS[lt_left]) then begin
      sec:=co.Field(CFRE_DB_LAYOUT_POS[lt_left]).AsObject.Field('sectionDesc').AsObject;
      TransformInvocation(session,command_type,sec,tmpContent,tmpContentType,true);
      if co.Field(CFRE_DB_LAYOUT_POS[lt_left]).AsObject.Field('resizeable').AsBoolean then begin
        jsContentAdd(sec.Field('id').AsString+'.splitter=true;');
      end;
      jsContentAdd(sec.Field('id').AsString+'.region="left";');
      if co.Field('useSizedSections').AsBoolean then begin
        useSizeH:=true;
        relSizeStr:='"width:'+ FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_left]).AsObject.Field('size').AsInt16 / co.Field('sizeH').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+sec.Field('id').AsString+');');
    end;
    if co.FieldExists(CFRE_DB_LAYOUT_POS[lt_right]) then begin
      sec:=co.Field(CFRE_DB_LAYOUT_POS[lt_right]).AsObject.Field('sectionDesc').AsObject;
      TransformInvocation(session,command_type,sec,tmpContent,tmpContentType,true);
      if co.Field(CFRE_DB_LAYOUT_POS[lt_right]).AsObject.Field('resizeable').AsBoolean then begin
        jsContentAdd(sec.Field('id').AsString+'.splitter=true;');
      end;
      jsContentAdd(sec.Field('id').AsString+'.region="right";');
      if co.Field('useSizedSections').AsBoolean then begin
        useSizeH:=true;
        relSizeStr:='"width:'+FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_right]).AsObject.Field('size').AsInt16 / co.Field('sizeH').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd(sec.Field('id').AsString + '.set("style",'+relSizeStr+');');
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+sec.Field('id').AsString+');');
    end;
    if co.FieldExists(CFRE_DB_LAYOUT_POS[lt_top]) then begin
      sec:=co.Field(CFRE_DB_LAYOUT_POS[lt_top]).AsObject.Field('sectionDesc').AsObject;
      TransformInvocation(session,command_type,sec,tmpContent,tmpContentType,true);
      if co.Field(CFRE_DB_LAYOUT_POS[lt_top]).AsObject.Field('resizeable').AsBoolean then begin
        jsContentAdd(sec.Field('id').AsString+'.splitter=true;');
      end;
      jsContentAdd(sec.Field('id').AsString+'.region="top";');
      if co.Field('useSizedSections').AsBoolean then begin
        useSizeV:=true;
        relSizeStr:='"height:'+FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_top]).AsObject.Field('size').AsInt16 / co.Field('sizeV').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+sec.Field('id').AsString+');');
    end;
    if co.FieldExists(CFRE_DB_LAYOUT_POS[lt_bottom]) then begin
      sec:=co.Field(CFRE_DB_LAYOUT_POS[lt_bottom]).AsObject.Field('sectionDesc').AsObject;
      TransformInvocation(session,command_type,sec,tmpContent,tmpContentType,true);
      if co.Field(CFRE_DB_LAYOUT_POS[lt_bottom]).AsObject.Field('resizeable').AsBoolean then begin
        jsContentAdd(sec.Field('id').AsString+'.splitter=true;');
      end;
      jsContentAdd(sec.Field('id').AsString+'.region="bottom";');
      if co.Field('useSizedSections').AsBoolean then begin
        useSizeV:=true;
        relSizeStr:='"height:'+FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_bottom]).AsObject.Field('size').AsInt16 / co.Field('sizeV').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+sec.Field('id').AsString+');');
    end;
    if co.FieldExists(CFRE_DB_LAYOUT_POS[lt_center]) then begin
      sec:=co.Field(CFRE_DB_LAYOUT_POS[lt_center]).AsObject.Field('sectionDesc').AsObject;
      TransformInvocation(session,command_type,sec,tmpContent,tmpContentType,true);
      jsContentAdd(sec.Field('id').AsString+'.region="center";');
      if useSizeH then begin
        relSizeStr:='"width:'+FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_center]).AsObject.Field('size').AsInt16 / co.Field('sizeH').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      if useSizeV then begin
        relSizeStr:='"height:'+FloatToStrF(co.Field(CFRE_DB_LAYOUT_POS[lt_center]).AsObject.Field('size').AsInt16 / co.Field('sizeV').AsInt16 * 100,ffFixed,3,2)+'%"';
        jsContentAdd('if ('+sec.Field('id').AsString+'.style) var style_str=' +sec.Field('id').AsString+'.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
        jsContentAdd(sec.Field('id').AsString + '.set("style",style_str);');
      end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+sec.Field('id').AsString+');');
    end else begin
      jsContentAdd('var ' + co.Field('id').AsString + '_content = new dijit.layout.ContentPane({');
      jsContentAdd('   content: ""');
      jsContentAdd('  ,id: "' + co.Field('id').AsString + '_content"');
      jsContentAdd('  ,region: "center"');
      jsContentAdd('});');
        if useSizeH then begin
          relSizeStr:='"width:'+FloatToStrF(co.Field('contentSize').AsInt16 / co.Field('sizeH').AsInt16 * 100,ffFixed,3,2)+'%"';
          jsContentAdd('if ('+co.Field('id').AsString+'_content.style) var style_str=' +co.Field('id').AsString+'_content.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
          jsContentAdd(co.Field('id').AsString + '_content.set("style",style_str);');
        end;
        if useSizeV then begin
          relSizeStr:='"height:'+FloatToStrF(co.Field('contentSize').AsInt16 / co.Field('sizeV').AsInt16 * 100,ffFixed,3,2)+'%"';
          jsContentAdd('if ('+co.Field('id').AsString+'_content.style) var style_str=' +co.Field('id').AsString+'_content.style+";"+'+relSizeStr+'; else var style_str='+relSizeStr+';');
          jsContentAdd(co.Field('id').AsString + '_content.set("style",style_str);');
        end;
      jsContentAdd(co.Field('id').AsString +'.addChild('+co.Field('id').AsString + '_content);');
      jsContentAdd(co.Field('id').AsString +'._contentId = "'+ co.Field('id').AsString + '_content";');
    end;

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
    end;

    if co.FieldExists('formdialog') then begin
      _BuildDialog(session,command_type,co.Field('formdialog').AsObject.Implementor_HC as TFRE_DB_FORM_DIALOG_DESC);
    end;

    if not isInnerContent then begin
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildSubsectionContainer(const session: TFRE_DB_UserSession; const command_type: TFRE_DB_COMMANDTYPE; const co: TFRE_DB_SUBSECTIONS_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction               : TFRE_JSON_ACTION;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;
    case String2DBSubSecDisplayType(co.Field('dt').AsString) of
      sec_dt_tab: _BuildSubSecTabContainer(session,command_type,co,false);
      sec_dt_hiddentab: _BuildSubSecTabContainer(session,command_type,co,true);
      sec_dt_vertical: _BuildSubSecVertContainer(session,command_type,co);
    end;
    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildMain(const session:TFRE_DB_UserSession; const co: TFRE_DB_MAIN_DESC; var contentString, contentType: String);
  var
    params: String;
    conn  : IFRE_DB_CONNECTION;
  begin
    conn:=session.GetDBConnection;
    jsContentClear;

    jsContentAdd('<!DOCTYPE HTML>');
    jsContentAdd('<html lang="en">');
    jsContentAdd('<head>');
    jsContentAdd('<meta http-equiv="Content-Type" content="text/html;charset=utf-8">');
    jsContentAdd('<title>'+co.Field('caption').AsString+'</title>');
    jsContentAdd('<script type="text/javascript">');
    jsContentAdd('  G_TEXTS = {};');
    jsContentAdd('  G_TEXTS.gridfilter =');
    jsContentAdd('         {numberOptions: {eq: "'+_getText(conn,'gf_n_eq')+'", lt: "'+_getText(conn,'gf_n_lt')+'", gt: "'+_getText(conn,'gf_n_gt')+'", gtlt: "'+_getText(conn,'gf_n_gtlt')+'"},');
    jsContentAdd('          dateOptions: {eq: "'+_getText(conn,'gf_d_eq')+'", lt: "'+_getText(conn,'gf_d_lt')+'", gt: "'+_getText(conn,'gf_d_gt')+'", gtlt: "'+_getText(conn,'gf_d_gtlt')+'"},');
    jsContentAdd('          filterLabel: "'+_getText(conn,'gf_filter_label')+'",');
    jsContentAdd('          setButton: "'+_getText(conn,'gf_filter_set')+'",');
    jsContentAdd('          clearButton: "'+_getText(conn,'gf_filter_clear')+'"');
    jsContentAdd('         };');
    jsContentAdd('  G_TEXTS.vnc =');
    jsContentAdd('         {cadButton: "'+_getText(conn,'vnc_cad')+'"');
    jsContentAdd('         ,wakeUpButton: "'+_getText(conn,'vnc_wakeup')+'"');
    jsContentAdd('         ,mountButton: "'+_getText(conn,'vnc_mount')+'"');
    jsContentAdd('         };');
    jsContentAdd('  G_TEXTS.editor =');
    jsContentAdd('         {saveButton: "'+_getText(conn,'editor_save')+'",');
    jsContentAdd('          resetButton: "'+_getText(conn,'editor_reset')+'"');
    jsContentAdd('         };');
    jsContentAdd('  G_TEXTS.recurrence =');
    jsContentAdd('         {once: "'+_getText(conn,'rec_once')+'", minute: "'+_getText(conn,'rec_minute')+'", hour: "'+_getText(conn,'rec_hour')+'", day: "'+_getText(conn,'rec_day')+'", week: "'+_getText(conn,'rec_week')+'", month: "'+_getText(conn,'rec_month')+'", quarter: "'+_getText(conn,'rec_quarter')+'", year: "'+_getText(conn,'rec_year')+'",');
    jsContentAdd('          mo: "'+_getText(conn,'rec_mo')+'", tu: "'+_getText(conn,'rec_tu')+'", we:"'+_getText(conn,'rec_we')+'", th: "'+_getText(conn,'rec_th')+'", fr: "'+_getText(conn,'rec_fr')+'", sa: "'+_getText(conn,'rec_sa')+'", su: "'+_getText(conn,'rec_su')+'",');
    jsContentAdd('          startDate: "'+_getText(conn,'rec_start')+'", endDate: "'+_getText(conn,'rec_end')+'", noEndDate: "'+_getText(conn,'rec_noend')+'",');
    jsContentAdd('          count: "'+_getText(conn,'rec_count')+'", interval: "'+_getText(conn,'rec_interval')+'"');
    jsContentAdd('         };');
    jsContentAdd('  G_TEXTS.openWindow =');
    jsContentAdd('         {error: "'+_getText(conn,'ow_error')+'"');
    jsContentAdd('         };');
    jsContentAdd('  G_TEXTS.msg =');
    jsContentAdd('         {yes: "'+_getText(conn,'msg_confirm_yes')+'"');
    jsContentAdd('         ,no: "'+_getText(conn,'msg_confirm_no')+'"');
    jsContentAdd('         ,ok: "'+_getText(conn,'msg_ok')+'"');
    jsContentAdd('         ,abort: "'+_getText(conn,'msg_abort')+'"');
    jsContentAdd('         };');
    jsContentAdd('</script>');

    //jsContentAdd('<link href=''http://fonts.googleapis.com/css?family=Source+Sans+Pro:200,300,400,600,700,900,200italic,300italic,400italic,600italic,700italic,900italic'' rel=''stylesheet'' type=''text/css''>');

    //jsContentAdd('<script type="text/javascript" src=fre_js/fos_deploy.js"></script>');

    if cFRE_JS_DEBUG then begin
      jsContentAdd('<link rel="stylesheet" type="text/css" href="fre_css/'+co.Field('style').AsString+'/app.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="fre_css/'+co.Field('style').AsString+'/fos_fonts.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="aloha/src/css/aloha.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="codemirror/lib/codemirror.css" media="screen">');

      jsContentAdd('<script src="d3/d3.js" ></script>');
      jsContentAdd('<script type="text/javascript" src="fre_js/config.js"></script>');
      jsContentAdd('<script src="dojo/dojo/dojo.js"></script>');
      jsContentAdd('<script>define.amd.jQuery = true;</script>');
      jsContentAdd('<script src="aloha/src/lib/vendor/jquery-1.7.2.js"></script>');
      jsContentAdd('<script src="aloha/src/lib/aloha.js" data-aloha-plugins="common/ui,common/format,common/table,common/list,common/link,common/highlighteditables,common/block,common/undo,common/image,common/contenthandler,common/paste,common/commands,common/abbr "></script>');
      jsContentAdd('<script src="fre_css/'+co.Field('style').AsString+'/charting.js"></script>');
      jsContentAdd('<script type="text/javascript" src="fre_js/dojo_utils.js"></script>');
      jsContentAdd('<script type="text/javascript" src="codemirror/lib/codemirror.js"></script>');
      jsContentAdd('<script type="text/javascript" src="codemirror/mode/javascript/javascript.js"></script>');
      jsContentAdd('<script type="text/javascript" src="codemirror/mode/pascal/pascal.js"></script>');
    end else begin
//      jsContentAdd('<link rel="stylesheet" type="text/css" href="js/all.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="aloha/src/css/aloha.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="codemirror/lib/codemirror.css" media="screen">');
      jsContentAdd('<link rel="stylesheet" type="text/css" href="fre_css/'+co.Field('style').AsString+'/all.css" media="screen">');

      jsContentAdd('<script src="js/framework.js" data-aloha-plugins="common/ui,common/format,common/table,common/list,common/link,common/highlighteditables,common/block,common/undo,common/image,common/contenthandler,common/paste,common/commands,common/abbr "></script>');
      jsContentAdd('<script src="fre_css/'+co.Field('style').AsString+'/charting.js"></script>');
    end;


    if co.Field('jira').AsString<>'' then begin
      jsContentAdd('<script>window.ATL_JQ_PAGE_PROPS =  { "triggerFunction": function(showCollectorDialog) { window._showCollectorDialog = showCollectorDialog; } };</script>');
      jsContentAdd('<script type="text/javascript" src="' + co.Field('jira').AsString + '"></script>');
    end;

    jsContentAdd('<script type="text/javascript">');
    jsContentAdd('  dojo.addOnLoad(function() {');
    jsContentAdd('    G_SERVER_COM.documentLoaded();');
    jsContentAdd('  });');
    jsContentAdd('</script>');
    jsContentAdd('</head>');
    jsContentAdd('<body class="'+co.Field('style').AsString+'">');
    jsContentAdd('  <div id="FirmOSViewport" class="viewport"></div>');
    jsContentAdd('  <iframe id="FirmOSDownload" name="FirmOSDownload" style="width: 1px; height: 1px; visibility: hidden; display: none;"></iframe>');
    jsContentAdd('</body>');
    jsContentAdd('</html>');

    contentString:=jsContent;
    contentType:='text/html';
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildHtml(const session:TFRE_DB_UserSession; const co: TFRE_DB_HTML_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction :TFRE_JSON_ACTION;
    style      : String;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;
    if co.Field('height').AsInt16>-1 then begin
      style:='height: ' + co.Field('height').AsString +'px;';
    end;
    if co.Field('width').AsInt16>-1 then begin
      style:=style+'width: ' + co.Field('width').AsString +'px;';
    end;
    if not co.Field('border').AsBoolean then begin
      style:=style+'border: 0px;';
    end;
    jsContentAdd('var ' + co.Field('id').AsString + ' = new dijit.layout.ContentPane({');
    jsContentAdd('   content: "' + StringReplace(co.Field('html').AsString,'"','\"',[rfReplaceAll])  +'"');
    jsContentAdd('  ,id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('  ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    if style<>'' then begin
      jsContentAdd('  ,style: "' + style + '"');
    end;
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;

  end;

  procedure TFRE_DB_WAPP_DOJO.BuildStoreData(const co: TFRE_DB_STORE_DATA_DESC; var contentString, contentType: String);
  var data_arr  : IFRE_DB_ObjectArray;
      ljdresult : TJSONArray;
      ljd2      : TJSONObject;
      i         : integer;
      count     : string;
  begin
    ljdresult:=TJSONArray.Create;
    try
      if co.FieldExists('data') then begin
        if co.FieldExists('total') then begin
          count:=inttostr(co.Field('total').AsInt32);
        end else begin
          count:=inttostr(co.Field('data').ValueCount);
        end;
        contentString := '{total:'+count+', data:'+ co.Field('data').AsObjectArrayJSONString + '}';
        contentType   := 'application/json';
      end else begin
        contentString := '{total: 0, data: [] }';
        contentType   := 'application/json';
      end;
    finally
      ljdresult.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildMessage(const session:TFRE_DB_UserSession; const co: TFRE_DB_MESSAGE_DESC; var contentString, contentType: String);
  var
    JSonAction  : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;
    jsContentClear;
    jsContentAdd('var message = new FIRMOS.Message({');
    jsContentAdd('   id: "'+co.Field('id').AsString+'_message"');
    jsContentAdd('  ,title: "'+FREDB_String2EscapedJSString(co.Field('caption').AsString)+'"');
    jsContentAdd('  ,msg: "'+FREDB_String2EscapedJSString(co.Field('msg').AsString,true)+'"');
    jsContentAdd('  ,type: "'+co.Field('msgType').AsString+'"');
    if co.FieldExists('serverFunc') then begin
      jsContentAdd('  ,sfClassname:"'+co.FieldPath('serverFunc.class').AsString+'"');
      jsContentAdd('  ,sfFunctionname:"'+co.FieldPath('serverFunc.func').AsString+'"');
      jsContentAdd('  ,sfUidPath:'+_BuildJSArray(co.Field('serverFunc').AsObject.Field('uidPath').AsStringArr));
      jsContentAdd('  ,sfParams:'+_BuildParamsObject(co.Field('serverFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.Field('progressBarId').AsString<>'' then begin
      jsContentAdd('  ,progressBarId: "'+co.Field('progressBarId').AsString+'"');
    end;
    jsContentAdd('});');

    jsContentAdd('G_UI_COM.showMessage(message);');

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := jsContent;
    JSonAction.ID         := co.Field('id').AsString;

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildUpdateMessageProgress(const co: TFRE_DB_UPDATE_MESSAGE_PROGRESS_DESC; var contentString, contentType: String);
  var
    JSonAction  : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.updateMsgProgress("'+co.Field('progressBarId').AsString+'",'+co.Field('percentage').AsString+');';
    JSonAction.ID         := co.Field('id').AsString;

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildChart(const session:TFRE_DB_UserSession; const co: TFRE_DB_CHART_DESC; var contentString, contentType: String; const isInnerContent:Boolean);
  var
    JSonAction :TFRE_JSON_ACTION;
    store      :TFRE_DB_STORE_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    store:=co.Field('store').AsObject.Implementor_HC as TFRE_DB_STORE_DESC;
    jsContentAdd('var '+store.Field('id').AsString+' = G_UI_COM.getStore("'+store.Field('id').AsString+'",');
    jsContentAdd('  {');
    jsContentAdd('   id:"'+store.FieldPath('id').AsString+'"');
    jsContentAdd('  ,idAttribute:"'+store.Field('idField').AsString+'"');
    jsContentAdd('  ,getClassname:"'+store.FieldPath('serverFunc.class').AsString+'"');
    jsContentAdd('  ,getFunctionname:"'+store.FieldPath('serverFunc.func').AsString+'"');
    jsContentAdd('  ,getUidPath: '+_BuildJSArray(store.FieldPath('serverFunc.uidPath').AsStringArr));
    jsContentAdd('  ,getParams: '+_BuildParamsObject(store.Field('serverFunc').AsObject.Field('params').AsObjectArr));
    if store.FieldExists('clearQueryIdFunc') then begin
      jsContentAdd('  ,clearClassname:"'+store.FieldPath('clearQueryIdFunc.class').AsString+'"');
      jsContentAdd('  ,clearFunctionname:"'+store.FieldPath('clearQueryIdFunc.func').AsString+'"');
      jsContentAdd('  ,clearUidPath: '+_BuildJSArray(store.FieldPath('clearQueryIdFunc.uidPath').AsStringArr));
      jsContentAdd('  ,clearParams: '+_BuildParamsObject(store.Field('clearQueryIdFunc').AsObject.Field('params').AsObjectArr));
    end;
    if store.FieldExists('destroyFunc') then begin
      jsContentAdd('  ,destroyClassname:"'+store.FieldPath('destroyFunc.class').AsString+'"');
      jsContentAdd('  ,destroyFunctionname:"'+store.FieldPath('destroyFunc.func').AsString+'"');
      jsContentAdd('  ,destroyUidPath: '+_BuildJSArray(store.FieldPath('destroyFunc.uidPath').AsStringArr));
      jsContentAdd('  ,destroyParams: '+_BuildParamsObject(store.Field('destroyFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('  }');
    jsContentAdd(');');

    jsContentAdd('var '+co.Field('id').AsString+'_chart = new FIRMOS.Chart({');
    jsContentAdd('   id:"'+co.Field('id').AsString+'_chart"');
    jsContentAdd('  ,title:"'+FREDB_String2EscapedJSString(co.Field('caption').AsString)+'"');
    jsContentAdd('  ,type:"'+co.Field('type').AsString+'"');
    jsContentAdd('  ,store:'+store.Field('id').AsString);
    jsContentAdd('  ,seriesids: '+_BuildJSArray(co.Field('seriesIds').AsStringArr));
    jsContentAdd('  ,showlegend: '+BoolToStr(co.Field('showLegend').AsBoolean,'true','false'));
    if co.FieldExists('seriesLabels') then begin
      jsContentAdd('  ,labels: '+_BuildJSArray(co.Field('seriesLabels').AsStringArr));
    end;
    jsContentAdd('  ,maxValue: '+co.Field('maxValue').AsString);
    jsContentAdd('});');

    if co.Field('showLegend').AsBoolean then begin
      jsContentAdd('var '+co.Field('id').AsString+'_legend = new dojox.charting.widget.Legend({chart: ' +co.Field('id').AsString+'_chart.chart});');

      jsContentAdd(' var ' + co.Field('id').AsString + '_c = new dijit.layout.ContentPane({id: "' + co.Field('id').AsString + '_c", content: '+co.Field('id').AsString + '_chart, region: "center"});');
      jsContentAdd(' var ' + co.Field('id').AsString + '_l = new dijit.layout.ContentPane({id: "' + co.Field('id').AsString + '_l", content: '+co.Field('id').AsString + '_legend, region: "bottom", class: "firmosChartLegendPanel"});');

      jsContentAdd('var '+ co.Field('id').AsString +' = new dijit.layout.BorderContainer({');
      jsContentAdd('   id: "'+co.Field('id').AsString+'"');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
      jsContentAdd('  ,gutters: false');
      jsContentAdd('  ,class: "firmosChartBorderContainer"');
      jsContentAdd(' });');

      jsContentAdd(co.Field('id').AsString + '.addChild('+co.Field('id').AsString+'_c);');
      jsContentAdd(co.Field('id').AsString + '.addChild('+co.Field('id').AsString+'_l);');
    end else begin
      jsContentAdd(' var ' + co.Field('id').AsString + ' = new dijit.layout.ContentPane({');
      jsContentAdd('   id: "' + co.Field('id').AsString + '"');
      jsContentAdd('  ,content: '+co.Field('id').AsString + '_chart');
      if co.FieldExists('destroyNotify') then begin
        jsContentAdd('  ,destroyNotify: true');
        session.registerUpdatableContent(co.Field('id').AsString);
      end;
      jsContentAdd('  ,class: "firmosChartContainerPane"});');
    end;

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;

  end;

  procedure TFRE_DB_WAPP_DOJO.BuildChartData(const co: TFRE_DB_CHART_DATA_DESC; var contentString, contentType: String);
  var data_arr  : IFRE_DB_ObjectArray;
      ljdresult : TJSONArray;
      ljd2      : TJSONObject;
      i         : integer;
  begin
    ljdresult:=TJSONArray.Create;
    try
      if co.FieldExists('series') then begin
        contentString := '{total:'+inttostr(co.Field('series').ValueCount)+', data:'+  co.Field('series').AsObjectArrayJSONString + '}';
        contentType   := 'application/json';
      end else begin
        contentString := '{total: 0, data: [] }';
        contentType   := 'application/json';
      end;
    finally
      ljdresult.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildLiveChart(const session:TFRE_DB_UserSession; const co: TFRE_DB_LIVE_CHART_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction :TFRE_JSON_ACTION;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var '+co.Field('id').AsString+' = new FIRMOS.D3Chart({');
    jsContentAdd('   id:"'+co.Field('id').AsString+'"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('  ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd('  ,caption:"'+FREDB_String2EscapedJSString(co.Field('caption').AsString)+'"');
    jsContentAdd('  ,seriesCount:'+co.Field('seriesCount').AsString);
    jsContentAdd('  ,sfClass:"'+co.FieldPath('serverFunc.class').AsString+'"');
    jsContentAdd('  ,sfFunc:"'+co.FieldPath('serverFunc.func').AsString+'"');
    jsContentAdd('  ,sfUidPath: '+_BuildJSArray(co.FieldPath('serverFunc.uidPath').AsStringArr));
    jsContentAdd('  ,sfParams: '+_BuildParamsObject(co.Field('serverFunc').AsObject.Field('params').AsObjectArr));
    jsContentAdd('  ,type:"'+co.Field('type').AsString+'"');
    jsContentAdd('  ,dataMin:'+co.Field('dataMin').AsString);
    jsContentAdd('  ,dataMax:'+co.Field('dataMax').AsString);
    jsContentAdd('  ,dataTickHint:'+co.Field('dataTickHint').AsString);
    jsContentAdd('  ,dataCount:'+co.Field('dataCount').AsString);
    jsContentAdd('  ,updateInterval:'+co.Field('updateInterval').AsString);
    jsContentAdd('  ,bufferSize:'+co.Field('buffer').AsString);
    if co.FieldExists('seriesColor') then begin
      jsContentAdd('  ,seriesColor:'+  _BuildJSArray(co.Field('seriesColor').AsStringArr));
    end;
    if co.FieldExists('dataLabels') then begin
      jsContentAdd('  ,dataLabels:'+  _BuildJSArray(co.Field('dataLabels').AsStringArr));
    end;
    if co.FieldExists('legendLabels') then begin
      jsContentAdd('  ,legendLabels:'+  _BuildJSArray(co.Field('legendLabels').AsStringArr));
    end;
    if co.FieldExists('initDataFunc') then begin
      jsContentAdd('  ,initClass:"'+co.FieldPath('initDataFunc.class').AsString+'"');
      jsContentAdd('  ,initFunc:"'+co.FieldPath('initDataFunc.func').AsString+'"');
      jsContentAdd('  ,initUidPath: '+_BuildJSArray(co.FieldPath('initDataFunc.uidPath').AsStringArr));
      jsContentAdd('  ,initParams: '+_BuildParamsObject(co.Field('initDataFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd('  ,class: "firmosLiveChart"');
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildRedefineLiveChart(const session: TFRE_DB_UserSession; const co: TFRE_DB_REDEFINE_LIVE_CHART_DESC; var contentString, contentType: String);
  var
    JSonAction     : TFRE_JSON_ACTION;
    prefix         : String;
    params         : String;
  begin
    prefix:='';
    params:='{';
    if co.Field('dataCount').AsInt64>0 then begin
      params:=params+'dataCount: '+co.Field('dataCount').AsString;
      prefix:=',';
    end;
    if co.FieldExists('dataLabels') then begin
      params:=params+prefix+'dataLabels: '+_BuildJSArray(co.Field('dataLabels').AsStringArr);
      prefix:=',';
    end;
    if co.FieldExists('dataMin') then begin
      params:=params+prefix+'dataMin: '+co.Field('dataMin').AsString;
      prefix:=',';
    end;
    if co.FieldExists('dataMax') then begin
      params:=params+prefix+'dataMax: '+co.Field('dataMax').AsString;
      prefix:=',';
    end;
    if co.Field('caption').AsString<>'' then begin
      params:=params+prefix+'caption: "'+FREDB_String2EscapedJSString(co.Field('caption').AsString) + '"';
      prefix:=',';
    end;
    if co.Field('seriesCount').AsInt64>0 then begin
      params:=params+prefix+'seriesCount: '+co.Field('seriesCount').AsString;
      prefix:=',';
    end;
    if co.FieldExists('legendLabels') then begin
      params:=params+prefix+'legendLabels: '+_BuildJSArray(co.Field('legendLabels').AsStringArr);
      prefix:=',';
    end;
    if co.FieldExists('seriesColor') then begin
      params:=params+prefix+'seriesColor: '+_BuildJSArray(co.Field('seriesColor').AsStringArr);
      prefix:=',';
    end;
    params:=params+'}';

    JsonAction := TFRE_JSON_ACTION.Create;
    JsonAction.ActionType := jat_jsupdate;
    JsonAction.Action     := 'G_UI_COM.redefineLiveChart("'+co.Field('id').AsString+'",'+params+');';

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildLiveChartAtIdxData(const co: TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC; var contentString, contentType: String);
  var
    JSonAction     : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;
    JsonAction.ActionType := jat_jsupdate;
    JsonAction.Action     := 'G_UI_COM.updateLiveChart("'+co.Field('id').AsString+'",'+_BuildJSArray(co.Field('data').AsStringArr)+','+co.Field('dataIndex').AsString+');';

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildLiveChartCompleteData(const co: TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC; var contentString, contentType: String);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    data       : String;
  begin
    data := '[';
    data:= data + _BuildJSArray(co.Field('data0').AsStringArr);
    for i := 1 to co.Field('dataCount').AsInt16 - 1 do begin
      data:= data + ','+_BuildJSArray(co.Field('data'+IntToStr(i)).AsStringArr);
    end;
    data := data + ']';

    JsonAction := TFRE_JSON_ACTION.Create;
    JsonAction.ActionType := jat_jsupdate;
    JsonAction.Action     := 'G_UI_COM.updateLiveChart("'+co.Field('id').AsString+'",'+data+');';

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;

  end;

  procedure TFRE_DB_WAPP_DOJO.BuildLiveChartInitData(const co: TFRE_DB_LIVE_CHART_INIT_DATA_DESC; var contentString, contentType: String);
  var
    i: Integer;
  begin
    contentString := '[';
    contentString:= contentString + _BuildJSArray(co.Field('data0').AsStringArr);
    for i := 1 to co.Field('dataCount').AsInt16 - 1 do begin
      contentString:= contentString + ','+_BuildJSArray(co.Field('data'+IntToStr(i)).AsStringArr);
    end;
    contentString := contentString + ']';
    contentType:='application/json';
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildTopMenu(const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const co: TFRE_DB_TOPMENU_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction     : TFRE_JSON_ACTION;
    entry          : TFRE_DB_CONTENT_DESC;
    i,j            : Integer;
    subsecs        : TFRE_DB_SUBSECTIONS_DESC;
    subsubsecs     : TFRE_DB_SUBSECTIONS_DESC;
    entries        : String;
    serverfuncs    : IFRE_DB_ObjectArray;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    subsecs:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_hiddentab);
    try
      entries := '[';
      for i := 0 to co.Field('entries').ValueCount - 1 do begin
        entry := co.Field('entries').AsObjectArr[i].Implementor_HC as TFRE_DB_CONTENT_DESC;
        if i>0 then begin
          entries:=entries+',';
        end;
        entries:=entries+'{isBig: '+BoolToStr(entry.Field('big').AsBoolean,'true','false');
        entries:=entries+',entryId: "'+entry.contentId+'"';
        entries:=entries+',icon: "'+entry.Field('icon').AsString+'"';
        if entry is TFRE_DB_TOPMENU_ENTRY_DESC then begin
          entries:=entries+',isDialog: false';
          if entry.Field('serverFuncs').ValueCount=1 then begin
            subsecs.AddSection.Describe(entry.Field('serverFuncs').AsObject.Implementor_HC as TFRE_DB_SERVER_FUNC_DESC,'',i,entry.contentId);
            entry.Field('serverFuncs').Clear(true);
          end else begin
            subsubsecs:=TFRE_DB_SUBSECTIONS_DESC.Create.Describe(sec_dt_hiddentab);
            serverfuncs := entry.Field('serverFuncs').CheckOutObjectArray;
            for j := 0 to high(serverfuncs) do begin
              subsubsecs.AddSection.Describe(serverfuncs[j].Implementor_HC as TFRE_DB_SERVER_FUNC_DESC,'',j,entry.Field('subIds').AsStringArr[j]);
            end;
            subsecs.AddSection._internalDescribe(subsubsecs,'',i,entry.contentId);
            entry.Field('serverFuncs').Clear(true);
          end;
          if entry.Field('active').AsBoolean then begin
            subsecs.SetActiveSection(entry.contentId);
          end;
        end else begin
          entries:=entries+',isDialog: true';
          if entry is TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC then begin
            entries:=entries+',class: "'+entry.FieldPath('serverFunc.class').AsString+'"';
            entries:=entries+',func: "'+entry.FieldPath('serverFunc.func').AsString+'"';
            entries:=entries+',uidpath: '+_BuildJSArray(entry.FieldPath('serverFunc.uidPath').AsStringArr);
            entries:=entries+',params: '+_BuildParamsObject(entry.Field('serverFunc').AsObject.Field('params').AsObjectArr);
          end else begin//TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC;
            entries:=entries+',isJira: true';
          end;
        end;
        entries:=entries+',caption: "'+FREDB_String2EscapedJSString(entry.Field('caption').AsString)+'"}';
      end;
      entries:=entries+']';
      jsContentAdd('var '+co.Field('id').AsString + ' = new FIRMOS.TopMenu({');
      jsContentAdd('                 id: "'+co.Field('id').AsString+'"');
      jsContentAdd('                ,class: "borderContainer firmosTransparent"');
      jsContentAdd('                ,entries: ' + entries);
      jsContentAdd('                ,subSecsId: "' + subsecs.contentId + '"');
      jsContentAdd('});');

      BuildSubsectionContainer(session,command_type,subsecs,contentString,contentType,true);
      jsContentAdd(subsecs.Field('id').AsString+'.region="center";');
      jsContentAdd(subsecs.Field('id').AsString+'.class="firmosTransparent";');
      jsContentAdd(co.Field('id').AsString +'.addChild('+subsecs.Field('id').AsString+');');

      if not isInnerContent then begin
        jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');
      end;

      if co.FieldExists('formdialog') then begin
        _BuildDialog(session,command_type,co.Field('formdialog').AsObject.Implementor_HC as TFRE_DB_FORM_DIALOG_DESC);
      end;
    finally
      subsecs.Finalize;
    end;

    if not isInnerContent then begin
      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildInvalidSessionData(const co: TFRE_DB_INVALIDATE_SESSION_DATA_DESC; var contentString, contentType: String);
  var
    ljdresult : TJSONArray;
  begin
    ljdresult:=TJSONArray.Create;
    try
      contentString := 'G_UI_COM.invalidateSessionData('+  co.Field('stores').AsObjectArrayJSONString + ');';
      contentType   := 'application/json';
    finally
      ljdresult.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildSitemap(const session: TFRE_DB_UserSession; const command_type: TFRE_DB_COMMANDTYPE; const co: TFRE_DB_SITEMAP_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    entry      : TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.Sitemap({');
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('  ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd(' ,entries: '+co.Field('entries').AsObjectArrayJSONString);
    jsContentAdd(' ,defs: ' +co.Field('svgDefs').AsObjectArrayJSONString);
    jsContentAdd(' ,class: "firmosTransparent"');
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildSitemapEntryUpdate(const co: TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC; var contentString, contentType: String);
  var
    JSonAction : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.updateInfoUI('+_BuildJSArray(co.Field('entryPath').AsStringArr)+','+co.Field('newsCount').AsString+');';
    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildSVG(const session: TFRE_DB_UserSession; const co: TFRE_DB_SVG_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    entry      : TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.SVG({');
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    jsContentAdd(' ,svg: "' + FREDB_String2EscapedJSString(co.Field('svg').AsString) + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd('  ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildUpdateSVG(const co: TFRE_DB_UPDATE_SVG_DESC; var contentString, contentType: String);
  var
    JSonAction  : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.updateSVG("'+co.Field('svgId').AsString+'","'+co.Field('elementId').AsString+'","'+co.Field('attrName').AsString+'","'+co.Field('attrValue').AsString+'");';
    JSonAction.ID         := co.Field('id').AsString;

    contentString := JsonAction.AsString;
    contentType:='application/json';

    JsonAction.Free;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildVNC(const session: TFRE_DB_UserSession; const command_type: TFRE_DB_COMMANDTYPE; const co: TFRE_DB_VNC_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    entry      : TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.VNC({');
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd(' ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd(' ,host: "'+co.Field('host').AsString+'"');
    jsContentAdd(' ,port: ' +co.Field('port').AsString);
    jsContentAdd(' ,class: "firmosVNCPanel"');
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildShell(const session: TFRE_DB_UserSession; const command_type: TFRE_DB_COMMANDTYPE; const co: TFRE_DB_SHELL_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    entry      : TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.Shell({');
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd(' ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd(' ,protocol: "'+co.Field('protocol').AsString+'"');
    jsContentAdd(' ,host: "'+co.Field('host').AsString+'"');
    jsContentAdd(' ,port: ' +co.Field('port').AsString);
    jsContentAdd(' ,path: "' +co.Field('path').AsString+'"');
    jsContentAdd(' ,class: "firmosShellPanel"');
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildEditor(const session:TFRE_DB_UserSession;const co: TFRE_DB_EDITOR_DESC; var contentString, contentType: String; const isInnerContent: Boolean);
  var
    JSonAction : TFRE_JSON_ACTION;
    i          : Integer;
    entry      : TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    if not isInnerContent then begin
      JsonAction := TFRE_JSON_ACTION.Create;
      jsContentClear;
    end;

    jsContentAdd('var ' + co.Field('id').AsString + ' = new FIRMOS.Editor({');
    jsContentAdd('  id: "' + co.Field('id').AsString + '"');
    if co.FieldExists('destroyNotify') then begin
      jsContentAdd(' ,destroyNotify: true');
      session.registerUpdatableContent(co.Field('id').AsString);
    end;
    jsContentAdd(' ,contentType: "'+co.Field('contentType').AsString+'"');
    jsContentAdd(' ,loadClass: "'+co.FieldPath('loadFunc.class').AsString+'"');
    jsContentAdd(' ,loadFunc: "'+co.FieldPath('loadFunc.func').AsString+'"');
    jsContentAdd(' ,loadUidPath: '+_BuildJSArray(co.FieldPath('loadFunc.uidPath').AsStringArr));
    jsContentAdd(' ,loadParams: '+_BuildParamsObject(co.Field('loadFunc').AsObject.Field('params').AsObjectArr));
    if co.FieldExists('saveFunc') then begin
      jsContentAdd(' ,saveClass: "'+co.FieldPath('saveFunc.class').AsString+'"');
      jsContentAdd(' ,saveFunc: "'+co.FieldPath('saveFunc.func').AsString+'"');
      jsContentAdd(' ,saveUidPath: '+_BuildJSArray(co.FieldPath('saveFunc.uidPath').AsStringArr));
      jsContentAdd(' ,saveParams: '+_BuildParamsObject(co.Field('saveFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('startEditFunc') then begin
      jsContentAdd(' ,startEditClass: "'+co.FieldPath('startEditFunc.class').AsString+'"');
      jsContentAdd(' ,startEditFunc: "'+co.FieldPath('startEditFunc.func').AsString+'"');
      jsContentAdd(' ,startEditUidPath: '+_BuildJSArray(co.FieldPath('startEditFunc.uidPath').AsStringArr));
      jsContentAdd(' ,startEditParams: '+_BuildParamsObject(co.Field('startEditFunc').AsObject.Field('params').AsObjectArr));
    end;
    if co.FieldExists('stopEditFunc') then begin
      jsContentAdd(' ,stopEditClass: "'+co.FieldPath('stopEditFunc.class').AsString+'"');
      jsContentAdd(' ,stopEditFunc: "'+co.FieldPath('stopEditFunc.func').AsString+'"');
      jsContentAdd(' ,stopEditUidPath: '+_BuildJSArray(co.FieldPath('stopEditFunc.uidPath').AsStringArr));
      jsContentAdd(' ,stopEditParams: '+_BuildParamsObject(co.Field('stopEditFunc').AsObject.Field('params').AsObjectArr));
    end;
    jsContentAdd(' ,tbBottom: '+BoolToStr(co.Field('tbBottom').AsBoolean,'true','false'));
    jsContentAdd('});');

    if not isInnerContent then begin
      jsContentAdd('G_UI_COM.contentLoaded('+co.Field('id').AsString+',"'+co.Field('windowCaption').AsString+'");');

      JsonAction.ActionType := jat_jsupdate;
      JsonAction.Action     := jsContent;
      JSonAction.ID         := co.Field('id').AsString;
      JSonAction.updateID   := co.Field('updateId').AsString;

      contentString := JsonAction.AsString;
      contentType:='application/json';

      JsonAction.Free;
    end;
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildEditorData(const co: TFRE_DB_EDITOR_DATA_DESC; var contentString, contentType: String);
  begin
    contentString := '{"value": "'+FREDB_String2EscapedJSString(co.Field('value').AsString)+'"}';
    contentType   := 'application/json';
  end;

  procedure TFRE_DB_WAPP_DOJO.BuildOpenNewLocation(const co: TFRE_DB_OPEN_NEW_LOCATION_DESC; var contentString, contentType: String);
  var
    JSonAction : TFRE_JSON_ACTION;
  begin
    JsonAction := TFRE_JSON_ACTION.Create;

    JsonAction.ActionType := jat_jsexecute;
    JsonAction.Action     := 'G_UI_COM.openUrl("'+co.Field('url').AsString+'",'+BoolToStr(co.Field('newWindow').AsBoolean,'true','false')+');';
    contentString := JsonAction.AsString;
    contentType:='application/json';
    JsonAction.Free;
  end;

  initialization
    gWAC_DOJO:=TFRE_DB_WAPP_DOJO.Create;
  finalization
    gWAC_DOJO.Free;

end.

