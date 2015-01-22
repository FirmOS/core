unit fre_db_common;

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
  Classes, SysUtils,FRE_DB_INTERFACE,FRE_SYSTEM,FOS_TOOL_INTERFACES;

type

  TFRE_DB_TRANSFORM_TYPE      = (fdbtt_post2json, fdbtt_get2html,fdbtt_WebSocket);
  TFRE_DB_LAYOUT_POS          = (lt_left,lt_center,lt_right,lt_top,lt_bottom);
  TFRE_DB_CLIENT_ACTION       = (fdbca_openContent);
  TFRE_DB_BUTTON_TYPE         = (fdbbt_submit,fdbbt_button,fdbbt_close,fdbbt_download);
  TFRE_DB_GRID_BUTTON_DEP     = (fdgbd_single,fdgbd_multi,fdgbd_always,fdgbd_manual);
  TFRE_DB_CONTENT_TYPE        = (ct_html,ct_javascript,ct_pascal);
  TFRE_DB_REC_INTERVAL_TYPE   = (rit_once,rit_minute,rit_hour,rit_day,rit_week,rit_month,rit_quarter,rit_year);
  TFRE_DB_REC_INTERVAL_TYPES  = set of TFRE_DB_REC_INTERVAL_TYPE;

  TFRE_DB_TRANSFORM_FUNCTION  = procedure(const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const result_intf:IFRE_DB_Object;var rawContent:TFRE_DB_RawByteString;var lContentType:string; const isInnerContent:Boolean=false; const TransformType: TFRE_DB_TRANSFORM_TYPE=fdbtt_post2json);


const
  CFRE_DB_CHOOSER_DH           : array [TFRE_DB_CHOOSER_DH] of string          = ('dh_chooser_radio','dh_chooser_check','dh_chooser_combo');
  CFRE_DB_LAYOUT_POS           : array [TFRE_DB_LAYOUT_POS] of string          = ('lt_left','lt_center','lt_right','lt_top','lt_bottom');
  CFRE_DB_BUTTON_TYPE          : array [TFRE_DB_BUTTON_TYPE] of string         = ('bt_submit','bt_button','bt_close','bt_download');
  CFRE_DB_GRID_BUTTON_DEP      : array [TFRE_DB_GRID_BUTTON_DEP] of string     = ('gbd_single','gbd_multi','gbd_always','gbd_manual');
  CFRE_DB_CHART_TYPE           : array [TFRE_DB_CHART_TYPE] of string          = ('ct_pie','ct_column','ct_line');
  CFRE_DB_LIVE_CHART_TYPE      : array [TFRE_DB_LIVE_CHART_TYPE] of string     = ('lct_line','lct_sampledline','lct_column');
  CFRE_DB_CONTENT_TYPE         : array [TFRE_DB_CONTENT_TYPE] of string        = ('ct_html','ct_javascript','ct_pascal');

type
  { TFRE_DB_HTML_DESC }

  TFRE_DB_HTML_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a HTML content.
    function  Describe   (const html:String; const height:Integer=-1; const width:Integer=-1; const border:Boolean=false): TFRE_DB_HTML_DESC;
  end;


  { TFRE_DB_UPDATE_UI_ELEMENT_DESC }

  TFRE_DB_UPDATE_UI_ELEMENT_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a status change. Used to enable/disable the element or to change the caption/hint.
    function  DescribeStatus     (const elementId:String; const disabled:Boolean; const newCaption:String=''; const newHint:String=''): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
    //@ Describes a submenu change. Used to change a submenu within a toolbar.
    function  DescribeSubmenu    (const elementId:String; const menu: TFRE_DB_MENU_DESC): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
    //@ Describes if drag is disabled for a grid.
    function  DescribeDrag       (const elementId:String; const disabled: Boolean): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
  end;

  { TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC }

  TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an info update of a sitemap entry.
    function  Describe  (const entryPath: TFRE_DB_StringArray; const newsCount:Integer): TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC;
  end;


  TFRE_DB_STORE_DESC = class;

  { TFRE_DB_DATA_ELEMENT_DESC }

  TFRE_DB_DATA_ELEMENT_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    function   _Describe     (const id,caption: TFRE_DB_String; const displayType: TFRE_DB_DISPLAY_TYPE; const sortable: Boolean; const filterable: Boolean; const size: Integer; const display: Boolean; const editable: Boolean; const required: Boolean; const iconId:String; const openIconId:String; const filterValues: TFRE_DB_StringArray):TFRE_DB_DATA_ELEMENT_DESC;
  public
    //@ Describes an entry of a collection view.
    //@ FIXXME: required parameter not implemented yet.
    function   Describe      (const id,caption: TFRE_DB_String; const displayType: TFRE_DB_DISPLAY_TYPE=dt_string; const sortable: Boolean=false; const filterable:Boolean=false; const size: Integer=1; const display: Boolean=true; const editable: Boolean=false; const required: Boolean=false; const iconId:String=''; const openIconId:String=''; const filterValues: TFRE_DB_StringArray=nil):TFRE_DB_DATA_ELEMENT_DESC;
    //@ Describes a 'progressbar' entry.
    //@ If labelId is given the value of this field will be used as label of the progressbar otherwise
    //@ the value (id field) will be used as label followed by a percent sign.
    function   DescribePB    (const id,caption: TFRE_DB_String; const labelId: string=''; const maxValue: Single=100; const sortable: Boolean=false; const filterable:Boolean=false; const size: Integer=1):TFRE_DB_DATA_ELEMENT_DESC;
    //@ Sets the store which holds the possible values of the data field.
    //@ Only useful if the visualisation (e.g. TFRE_DB_VIEW_LIST_DESC) is editable.
    //@ FIXXME: not implemented yet.
    procedure  setValueStore (const store:TFRE_DB_STORE_DESC);
  end;

  { TFRE_DB_VIEW_LIST_LAYOUT_DESC }

  TFRE_DB_VIEW_LIST_LAYOUT_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    procedure AddStore            (const store: TFRE_DB_STORE_DESC);
  public
    //@ Describes the data model of a collection. See also TFRE_DB_STORE_DESC.
    //@ The idAttr defines the unique key of the collection.
    function  Describe            (): TFRE_DB_VIEW_LIST_LAYOUT_DESC;

    //@ Creates a new TFRE_DB_DATA_ELEMENT_DESC and adds it.
    function  AddDataElement     : TFRE_DB_DATA_ELEMENT_DESC;
  end;

  TFRE_DB_INPUT_BLOCK_DESC       = class;
  TFRE_DB_INPUT_GROUP_DESC       = class;
  TFRE_DB_INPUT_GROUP_PROXY_DESC = class;

  { TFRE_DB_FORM_INPUT_DESC }

  //@ Base class for form input elements.
  //@ Do NOT use! Use a derived class instead.
  //@ E.g. TFRE_DB_INPUT_DESC instead.
  TFRE_DB_FORM_INPUT_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    //@ Used internally through inheritance.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil ; const validatorConfigParams : IFRE_DB_Object=nil) : TFRE_DB_FORM_INPUT_DESC;virtual;
  public
    //@ Adds a dependent field.
    procedure AddDependence(const fieldName: String; const disablesField: Boolean=true);
    //@ Adds all fields of the input group as dependent fields.
    procedure AddDependence(const inputGroup: TFRE_DB_INPUT_GROUP_DESC; const disablesFields: Boolean=true);
  end;

  { TFRE_DB_STORE_ENTRY_DESC }

  TFRE_DB_STORE_ENTRY_DESC = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a data entry of a simple store which has a caption and a value data element.
    //@ Used for e.g. a chooser in a form.
    function Describe (const caption,value: string):TFRE_DB_STORE_ENTRY_DESC;
  end;

  { TFRE_DB_STORE_DESC }

  TFRE_DB_STORE_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    function  FindEntryByCaption  (const caption:String):String;
  public
    //@ Describes a store whith the given data model.
    //@ The server function is used to retrieve the data.
    //@ The id will be needed if a grid selection should filter the store.
    function  Describe            (const idField:String='uid'; const serverFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const destroyFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const clearFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const id:String=''): TFRE_DB_STORE_DESC; overload;

    //@ Creates a new entry and adds it to the store. E.g. used for choosers.
    function  AddEntry            : TFRE_DB_STORE_ENTRY_DESC;
  end;

  { TFRE_DB_INPUT_DESC }

  TFRE_DB_INPUT_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a text input field within a form.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil;  const validatorConfigParams : IFRE_DB_Object=nil; const multiValues: Boolean=false; const isPass:Boolean=false; const confirms: String='') : TFRE_DB_INPUT_DESC;
  end;

  { TFRE_DB_INPUT_DESCRIPTION_DESC }

  TFRE_DB_INPUT_DESCRIPTION_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes an info field within a form.
    function  Describe (const caption,description_: String) : TFRE_DB_INPUT_DESCRIPTION_DESC;
  end;

  { TFRE_DB_INPUT_BOOL_DESC }

  TFRE_DB_INPUT_BOOL_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a boolean input field within a form.
    function  Describe(const caption, field_reference:string; const required: boolean=false; const groupRequired: Boolean=false; const disabled: boolean=false; const defaultValue:Boolean=false):TFRE_DB_INPUT_BOOL_DESC;
  end;

  { TFRE_DB_INPUT_NUMBER_DESC }

  TFRE_DB_INPUT_NUMBER_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
    //@ Describes a number input within a form.
    //@ minMax Array has to be of length 2 (min and max definition)
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false;  const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String='';
                        const digits: Integer=-1; const minMax: TFRE_DB_Real64Array=nil) : TFRE_DB_INPUT_NUMBER_DESC;
    //@ Describes a number slider within a form.
    function  DescribeSlider (const caption,field_reference : String; const min,max: Real; const showValueField: Boolean=true; const defaultValue:String=''; const digits: Integer=0; const steps: Integer=-1) : TFRE_DB_INPUT_NUMBER_DESC;
    //@ sets the min and max of the input element
    procedure setMinMax      (const min,max: Real);
  end;

  { TFRE_DB_INPUT_CHOOSER_DESC }

  TFRE_DB_INPUT_CHOOSER_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a chooser within a form.
    function  Describe              (const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const display_hint:TFRE_DB_CHOOSER_DH=dh_chooser_combo;
                                     const required: boolean=false; const groupRequired: Boolean=false; const add_empty_for_required:Boolean=false; const disabled: boolean=false; const defaultValue:String=''): TFRE_DB_INPUT_CHOOSER_DESC;
    function  DescribeMultiValue    (const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const display_hint:TFRE_DB_CHOOSER_DH=dh_chooser_radio;
                                     const required: boolean=false; const groupRequired: Boolean=false; const add_empty_for_required:Boolean=false; const disabled: boolean=false; const defaultValue:TFRE_DB_StringArray=nil): TFRE_DB_INPUT_CHOOSER_DESC;
    //@ FIXXME: only implemented for dh_chooser_combo.
    procedure addFilterEvent        (const filteredStoreId,refId:String);
    //@ Adds a dependent input element. If chooserValue is selected the input element will be updated.
    procedure addDependentInput     (const inputId: String; const chooserValue: String; const visible: TFRE_DB_FieldDepVisibility=fdv_none; const caption: String='';const validator: IFRE_DB_ClientFieldValidator=nil; const validatorConfigParams : IFRE_DB_Object=nil);
    //@ Adds a dependent input group. If chooserValue is selected the input element will be updated.
    procedure addDependentInputGroup(const inputGroup: TFRE_DB_INPUT_GROUP_DESC; const chooserValue: String; const visible: TFRE_DB_FieldDepVisibility=fdv_visible);
    //@ Enables the caption compare.
    //@ Useful for fields which store the caption and not a link to the object.
    //@ Default is false.
    procedure captionCompareEnabled (const enabled:Boolean);
  end;

  { TFRE_DB_INPUT_DATE_DESC }

  TFRE_DB_INPUT_DATE_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a date input within a form.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false;
                        const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil; const validatorConfigParams : IFRE_DB_Object=nil) : TFRE_DB_INPUT_DATE_DESC;
  end;

  { TFRE_DB_INPUT_RECURRENCE_DESC }

  TFRE_DB_INPUT_RECURRENCE_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a recurrence input within a form.
    function  Describe (const caption,field_reference : String; const intervals: TFRE_DB_REC_INTERVAL_TYPES=[rit_once,rit_minute,rit_hour,rit_day,rit_week,rit_quarter,rit_month,rit_year]; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String='') : TFRE_DB_INPUT_RECURRENCE_DESC;
  end;

  { TFRE_DB_INPUT_FILE_DESC }

  TFRE_DB_INPUT_FILE_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes an file input within a form.
    //@ Validator: only 'image' validator implemented.
    //@ Image Validator Configuration: currently only implemented for the preview!
    //@                                width: (integer - def: 100) width in px,
    //@                                height: (integer - def: 100) height in px,
    //@                                absolute: (boolean: def: false) if false image aspect ration will be preserved, width and height params will be maximum settings
    //@                                                                else image will be sized exactly to width and height settings
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false;
                        const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil; const validatorConfigParams : IFRE_DB_Object=nil; const multiValues: Boolean=false) : TFRE_DB_INPUT_FILE_DESC;
  end;

  { TFRE_DB_VIEW_LIST_BUTTON_DESC }

  TFRE_DB_VIEW_LIST_BUTTON_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function _Describe          (const func:TFRE_DB_SERVER_FUNC_DESC;const icon:String; const caption:String; const tooltip:String; const buttonDep:TFRE_DB_GRID_BUTTON_DEP):TFRE_DB_VIEW_LIST_BUTTON_DESC;
  public
    //@ Describes a button of a list view.
    //@ The buttonDep parameter defines the dependence between the button and the selection state of the list view.
    //@ fdgbd_single buttons are enabled when exactly one element is selected.
    //@ fdgbd_multi buttons are enabled when at least one element is selected.
    //@ fdgbd_always buttons are always enabled.
    function Describe           (const func:TFRE_DB_SERVER_FUNC_DESC;const icon:String; const caption:String=''; const tooltip:String='';
                                 const buttonDep:TFRE_DB_GRID_BUTTON_DEP=fdgbd_always):TFRE_DB_VIEW_LIST_BUTTON_DESC;
    //@ Describes a fdgbd_manual type button of a list view.
    function DescribeManualType (const id:String; const func:TFRE_DB_SERVER_FUNC_DESC;const icon:String; const caption:String=''; const tooltip:String='';
                                 const disabled:Boolean=False):TFRE_DB_VIEW_LIST_BUTTON_DESC;
  end;

  { TFRE_DB_VIEW_LIST_DESC }

  TFRE_DB_VIEW_LIST_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a list view representation of the given store.
    //@ Menu:
    //@   * Each entry has to have an uidPath or an uid defined.
    //@   * First: If the entry has a _funcclassname_ and a _menuFunc_ defined _funcclassname_._menufunc_ will be called.
    //@   * Second: itemContextMenuFunc will be called.
    //@ Details:
    //@   * Each entry has to have an uidPath or an uid defined.
    //@   * First: If the entry has a _funcclassname_ and a _detailsfunc_ defined _funcclassname_._detailsfunc_ will be called.
    //@   * Second: detailsFunc will be called.
    //@ If editable is true and no saveFunc is defined each entry has to have a _schemeclass_ defined. _schemeclass_.saveOperation will be called.
    //@
    function  Describe            (const store: TFRE_DB_STORE_DESC; const layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC; const itemContextMenuFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const title:String='';
                                   const displayFlags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS=[cdgf_ShowSearchbox,cdgf_Editable];
                                   const detailsFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const selectionDepFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const saveFunc:TFRE_DB_SERVER_FUNC_DESC=nil;
                                   const dropFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const dragFunc: TFRE_DB_SERVER_FUNC_DESC=nil): TFRE_DB_VIEW_LIST_DESC;
    //@ Sets the title of the list view,
    procedure SetTitle            (const title: String);
    //@ Sets the menu of the list view. Will be displayed like a file menu in a desktop application.
    procedure SetMenu (const menu: TFRE_DB_MENU_DESC);
    //@ Creates a new list view button and adds it.
    function  AddButton           :TFRE_DB_VIEW_LIST_BUTTON_DESC;
    //@ Adds a filtered store to the list view.
    //@ In case of a selection change the selected ids array (usually uids) will be set as dependency.refId parameter of the filtered store.
    //@ Furthermore the view of the dependent store will be refreshed.
    procedure AddFilterEvent      (const filteredStoreId,refId:String);
    //@ Enables drag and drop and defines the destination list view for this one.
    //@ Use objectclassesMultiple to limit the target drop object to the given classes in case of multiple selection.
    //@ Use objectclassesSingle to limit the target drop object to the given classes in case of single selection.
    //@ Data entry _disabledrag_ set to true can be used to disable drag explicitly for objects.
    //@ Data entry _disabledrop_ set to true on entries of the target gird can be used to disable drop explicitly for objects.
    procedure SetDropGrid         (const grid:TFRE_DB_VIEW_LIST_DESC; const DnDClassesMultiple:TFRE_DB_StringArray=nil; const DnDClassesSingle:TFRE_DB_StringArray=nil);
    //@ Limits the dragable objects to those matching one of the given objectclasses.
    //@ Data entry _disabledrag_ set to true can be used to disable drag explicitly for objects.
    //@ Ignored if drag and drop is not enabled. See function SetDropGrid.
    procedure SetDragClasses      (const DnDclasses:TFRE_DB_StringArray);
    //@ Adds an entry to the action column of the list.
    //@ FIXXME: Not implemented yet.
    procedure AddEntryAction      (const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const tooltip:String='');
    //@ Defines a server function which will be called on selection change of the list view to retrieve the dependent content.
    //@ The selected ids will be passed as selected parameter.
    //@ FIXXME: Not implemented yet.
    procedure SetDependentContent (const contentFunc:TFRE_DB_SERVER_FUNC_DESC);
    //@ Disable the drag functionality. TFRE_DB_UPDATE_UI_ELEMENT_DESC.DescribeDrag can be used to enable it again.
    procedure disableDrag         ;
  end;

  { TFRE_DB_VALIDATOR_DESC }

  TFRE_DB_VALIDATOR_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a validator
    //@ Do NOT use! Used internally.
    function Describe(const id,regExp,helpTextKey: String; const allowedChars:String='';const replaceRegExp:String=''; const replaceValue:String=''; const configParams: IFRE_DB_Object=nil): TFRE_DB_VALIDATOR_DESC;
  end;

  { TFRE_DB_BUTTON_DESC }

  TFRE_DB_BUTTON_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a button for a form panel or a dialog.
    //@ fdbbt_close type is only useful for dialogs.
    //@ For fdbbt_close type the serverFunc might be nil (Just closes the dialog).
    function Describe        (const caption: String;const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const buttonType: TFRE_DB_BUTTON_TYPE): TFRE_DB_BUTTON_DESC;
    //@ Describes a download button for a form panel or a dialog.
    //@ closeDialog set to true is only useful for dialogs.
    function DescribeDownload(const caption: String;const downloadId: String; const closeDialog: Boolean): TFRE_DB_BUTTON_DESC;
  end;

  { TFRE_DB_FORM_DESC }

  //@ Base class form panels and dialogs.
  //@ Do NOT use! Use TFRE_DB_FORM_PANEL_DESC or TFRE_DB_FORM_DIALOG_DESC instead.
  TFRE_DB_FORM_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    procedure AddStore                (const store: TFRE_DB_STORE_DESC);virtual;
    procedure AddDBO                  (const id: String; const session: IFRE_DB_UserSession);virtual;
    function  GetStore                (const id:String): TFRE_DB_STORE_DESC;virtual;
    function  Describe                (const caption:String;const defaultClose:Boolean;const sendChangedFieldsOnly: Boolean; const editable: Boolean; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC; const onChangeDelay:Integer; const hideEmptyGroups: Boolean): TFRE_DB_FORM_DESC;
    procedure _FillWithObjectValues   (const obj: IFRE_DB_Object;const session: IFRE_DB_UserSession; const prefix:String);
  public
    //@ Return the form element with the given id.
    function  GetFormElement          (const elementId:String): TFRE_DB_CONTENT_DESC;
    //@ Sets the value of the input element with the given id.
    procedure SetElementValue         (const elementId, value:String);
    //@ Sets the value of the input element with the given id and disables it.
    procedure SetElementValueDisabled (const elementId, value:String);
    //@ Disables the input element with the given id.
    procedure SetElementDisabled      (const elementId:String);
    //@ Fills the form with the values of the given object.
    procedure FillWithObjectValues    (const obj: IFRE_DB_Object; const session: IFRE_DB_UserSession; const groupPreFix:String='');
    //@ Adds the given InputGroupSchemeDefinition to the form and returns the TFRE_DB_INPUT_GROUP_DESC.
    //@ if hideGroupHeader is set to true parameters collapsible and collapsed are ignored
    //@ See TFRE_DB_INPUT_GROUP_DESC.
    function  AddSchemeFormGroup      (const schemeGroup: IFRE_DB_InputGroupSchemeDefinition ; const session : IFRE_DB_UserSession; const collapsible: Boolean=false; const collapsed: Boolean=false; const groupPreFix:String=''; const groupRequired:Boolean=true; const hideGroupHeader:Boolean=false): TFRE_DB_INPUT_GROUP_DESC;
    //@ Creates a new input field and adds it to the form. See also TFRE_DB_INPUT_DESC.
    function  AddInput                : TFRE_DB_INPUT_DESC;
    //@ Creates a new description and adds it to the form. See also TFRE_DB_INPUT_DESCRIPTION_DESC.
    function  AddDescription          : TFRE_DB_INPUT_DESCRIPTION_DESC;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddBool                 : TFRE_DB_INPUT_BOOL_DESC;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddNumber               : TFRE_DB_INPUT_NUMBER_DESC;
    //@ Creates a new chooser and adds it to the form. See also TFRE_DB_INPUT_CHOOSER_DESC.
    function  AddChooser              : TFRE_DB_INPUT_CHOOSER_DESC;
    //@ Creates a new date field and adds it to the form. See also TFRE_DB_INPUT_DATE_DESC.
    function  AddDate                 : TFRE_DB_INPUT_DATE_DESC;
    //@ Creates a new recurrence field and adds it to the form. See also TFRE_DB_INPUT_RECURRENCE_DESC.
    function  AddRecurrence           : TFRE_DB_INPUT_RECURRENCE_DESC;
    //@ Creates a new file field and adds it to the form. See also TFRE_DB_INPUT_FILE_DESC.
    function  AddFile                 : TFRE_DB_INPUT_FILE_DESC;
    //@ Creates a new input group and adds it to the form. See also TFRE_DB_INPUT_GROUP_DESC.
    function  AddGroup                : TFRE_DB_INPUT_GROUP_DESC;
    //@ Creates a new input proxy group and adds it to the form. See also TFRE_DB_INPUT_GROUP_PROXY_DESC.
    function  AddGroupProxy           : TFRE_DB_INPUT_GROUP_PROXY_DESC;
    //@ Creates a new input block and adds it to the form. See also TFRE_DB_INPUT_BLOCK_DESC.
    function  AddBlock                : TFRE_DB_INPUT_BLOCK_DESC;
    //@ Creates a new grid and adds it to the form. See also TFRE_DB_VIEW_LIST_DESC.
    function  AddList                 : TFRE_DB_VIEW_LIST_DESC;
    //@ Creates a new button and adds it to the form. See also TFRE_DB_BUTTON_DESC.
    function  AddButton               : TFRE_DB_BUTTON_DESC;
    //@ Creates a new input field and adds it to the form. See also TFRE_DB_INPUT_DESC.
    //function  GetGroup             (const id:String): TFRE_DB_INPUT_GROUP_DESC;
  end;

  { TFRE_DB_INPUT_GROUP_DESC }

  TFRE_DB_INPUT_GROUP_DESC  = class(TFRE_DB_FORM_DESC)
  protected
    function  _Describe        (const caption:String;const collapsible,collapsed: Boolean):TFRE_DB_INPUT_GROUP_DESC;
    procedure AddStore         (const store: TFRE_DB_STORE_DESC);override;
    procedure AddDBO           (const id: String; const session: IFRE_DB_UserSession);override;
    function  GetStore         (const id: String):TFRE_DB_STORE_DESC;override;
  public
    //@ Describes an input group within a form.
    //@ Collapsed will be ignored if collapsible is false.
    function  Describe         (const caption:String='';const collapsible:Boolean=false;const collapsed:Boolean=false):TFRE_DB_INPUT_GROUP_DESC;
    procedure SetCaption       (const caption:String);
    //@ Sets the collapse state of the input group.
    //@ Useful in case the group was added with TFRE_DB_FORM_DESC.AddSchemeFormGroup.
    procedure SetCollapseState (const collapsed: Boolean=false; const collapsible: Boolean=true);
  end;

  { TFRE_DB_INPUT_BLOCK_DESC }

  TFRE_DB_INPUT_BLOCK_DESC  = class(TFRE_DB_FORM_DESC)
  private
    procedure AddStore    (const store: TFRE_DB_STORE_DESC);override;
    procedure AddDBO      (const id: String; const session: IFRE_DB_UserSession);override;
    function  GetStore    (const id: String):TFRE_DB_STORE_DESC;override;
  public
    //@ Describes an horizontal input block within a form (e.g. Favourite 3 colours: input input input).
    function  Describe    (const caption:String=''):TFRE_DB_INPUT_BLOCK_DESC;
    //@ Adds the given InputGroupSchemeDefinition to the form and returns the TFRE_DB_INPUT_GROUP_DESC.
    //@ See TFRE_DB_INPUT_GROUP_DESC.
    function  AddSchemeFormGroup   (const schemeGroup: IFRE_DB_InputGroupSchemeDefinition ; const session : IFRE_DB_UserSession; const collapsible: Boolean=false; const collapsed: Boolean=false; const relSize:Integer=1): TFRE_DB_INPUT_GROUP_DESC; reintroduce;
    //@ Creates a new input field and adds it to the form. See also TFRE_DB_INPUT_DESC.
    function  AddInput             (const relSize:Integer=1): TFRE_DB_INPUT_DESC; reintroduce;
    //@ Creates a new description and adds it to the form. See also TFRE_DB_INPUT_DESCRIPTION_DESC.
    function  AddDescription       (const relSize:Integer=1): TFRE_DB_INPUT_DESCRIPTION_DESC; reintroduce;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddBool              (const relSize:Integer=1): TFRE_DB_INPUT_BOOL_DESC; reintroduce;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddNumber            (const relSize:Integer=1): TFRE_DB_INPUT_NUMBER_DESC; reintroduce;
    //@ Creates a new chooser and adds it to the form. See also TFRE_DB_INPUT_CHOOSER_DESC.
    function  AddChooser           (const relSize:Integer=1): TFRE_DB_INPUT_CHOOSER_DESC; reintroduce;
    //@ Creates a new date field and adds it to the form. See also TFRE_DB_INPUT_DATE_DESC.
    function  AddDate              (const relSize:Integer=1): TFRE_DB_INPUT_DATE_DESC; reintroduce;
    //@ Creates a new file field and adds it to the form. See also TFRE_DB_INPUT_FILE_DESC.
    function  AddFile              (const relSize:Integer=1): TFRE_DB_INPUT_FILE_DESC; reintroduce;
    //@ Creates a new input group and adds it to the form. See also TFRE_DB_INPUT_GROUP_DESC.
    function  AddGroup             (const relSize:Integer=1): TFRE_DB_INPUT_GROUP_DESC; reintroduce;
    //@ Creates a new input proxy group and adds it to the form. See also TFRE_DB_INPUT_GROUP_PROXY_DESC.
    function  AddGroupProxy        (const relSize:Integer=1): TFRE_DB_INPUT_GROUP_PROXY_DESC; reintroduce;
    //@ Creates a new input block and adds it to the form. See also TFRE_DB_INPUT_BLOCK_DESC.
    function  AddBlock             (const relSize:Integer=1): TFRE_DB_INPUT_BLOCK_DESC; reintroduce;
    //@ Creates a new grid and adds it to the form. See also TFRE_DB_VIEW_LIST_DESC.
    function  AddList              (const relSize:Integer=1): TFRE_DB_VIEW_LIST_DESC; reintroduce;
  end;

  { TFRE_DB_INPUT_GROUP_PROXY_DESC }

  TFRE_DB_INPUT_GROUP_PROXY_DESC  = class(TFRE_DB_INPUT_GROUP_DESC)
  public
    //@ Describes an input group within a form which is collapsed and will retrieve its data once its opened.
    //@ FIXXME: not implemented yet.
    function  Describe    (const caption:String;const loadFunc:TFRE_DB_SERVER_FUNC_DESC):TFRE_DB_INPUT_GROUP_PROXY_DESC;
  end;

  { TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC }

  TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC = class(TFRE_DB_INPUT_GROUP_DESC)
  public
    //@ Describes the content of a TFRE_DB_INPUT_GROUP_PROXY_DESC and has to be the result of the loadFunc of it.
    //@ See TFRE_DB_INPUT_GROUP_PROXY_DESC.
    //@ FIXXME: not implemented yet.
    function Describe   (const elementId: String):TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC; reintroduce;
  end;

  { TFRE_DB_FORM_PANEL_DESC }

  TFRE_DB_FORM_PANEL_DESC    = class(TFRE_DB_FORM_DESC)
  public
    //@ Describes a form content panel. See also TFRE_DB_FORM_DESC.
    function Describe (const caption:String;const sendChangedFieldsOnly: Boolean=true; const editable: Boolean=true; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const onChangeDelay:Integer=0; const hideEmptyGroups:Boolean=true): TFRE_DB_FORM_PANEL_DESC;
    //@ Sets the menu of the form panel. Will be displayed like a file menu in a desktop application.
    procedure SetMenu (const menu: TFRE_DB_MENU_DESC);
  end;

  { TFRE_DB_FORM_DIALOG_DESC }

  TFRE_DB_FORM_DIALOG_DESC    = class(TFRE_DB_FORM_DESC)
  public
    //@ Describes a modal dialog. See also TFRE_DB_FORM_DESC.
    //@ If defaultClose is true a close button will be added to the dialog which simply closes the dialog.
    //@ If defaultClose is false and no explicit close button is added the dialog will not be closable at all (e.g. force login).
    //@ sendChangedFieldsOnly true: good for data updates, false: all field values are send unconditonally, good for new objects
    //@ styleClass: used to implement a non standard styling
    function  Describe    (const caption:String; const width:Integer=0; const defaultClose:Boolean=true; const isDraggable:Boolean=true;const sendChangedFieldsOnly: Boolean=true; const editable: Boolean=true; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const onChangeDelay:Integer=0; const hideEmptyGroups: Boolean=true; const styleClass: String=''): TFRE_DB_FORM_DIALOG_DESC;
  end;

  { TFRE_DB_DIALOG_DESC }

  TFRE_DB_DIALOG_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a modal dialog.
    //@ percWidth/Height (in %) or maxWidth/Height (in px) should be defined. If not 80% will be the default for percWidth/Height.
    //@ In case of given percWidth/Height maxWidth/Height will be ignored.
    //@ If defaultClose is true a close button will be added to the dialog which simply closes the dialog.
    //@ If defaultClose is false and no explicit close button is added the dialog will not be closable at all (e.g. force login).
    //@ styleClass: used to implement a non standard styling
    function  Describe    (const caption:String; const content:TFRE_DB_CONTENT_DESC; const percWidth: Integer=0; const percHeight: Integer=0; const maxWidth:Integer=0; const maxHeight: Integer=0; const isDraggable:Boolean=true; const styleClass: String=''): TFRE_DB_DIALOG_DESC;
    //@ Creates a new button and adds it to the form. See also TFRE_DB_BUTTON_DESC.
    function  AddButton   : TFRE_DB_BUTTON_DESC;
  end;

  { TFRE_DB_TOPMENU_DESC }

  TFRE_DB_TOPMENU_DESC  = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a top menu.
    function  Describe          (const homeCaption,homeIcon: String; const homeIconSize: Integer; const serverFuncs: array of TFRE_DB_SERVER_FUNC_DESC; const mainSectionId: TFRE_DB_String; const sectionsIds: array of TFRE_DB_String; const uname: String; const uServerFunction: TFRE_DB_SERVER_FUNC_DESC; const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY=nil; const notificationPanel: TFRE_DB_CONTENT_DESC=nil; const notificationInitialClosed:Boolean=true; const JIRAenabled: Boolean=false): TFRE_DB_TOPMENU_DESC;
    //@ Adds a dialog to the top menu which will be opened.
    //@ See TFRE_DB_FORM_DIALOG_DESC.
    procedure AddFormDialog     (const dialog:TFRE_DB_FORM_DIALOG_DESC);
  end;

  { TFRE_DB_LAYOUT_DESC }

  TFRE_DB_LAYOUT_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a layout.
    //@ If useSizedSections is switched off each section except the center section has to have a fixed size e.g. a html header section.
    function  Describe          (const useSizedSections:Boolean=true): TFRE_DB_LAYOUT_DESC;
    //@ Adds the given section at the position 'pos'. With the size parameter the relative size of the content can be configured.
    //@ Default values for size are: 3 for center section and 1 otherwise.
    //@ Parameter resizeable will be ignored for the center section because it is resizeable if any other section is resizeable.
    procedure AddSection        (const section:TFRE_DB_CONTENT_DESC; const pos: TFRE_DB_LAYOUT_POS; const resizeable: Boolean=true; const size: Integer=-1);    //deprecated, don't use
    //@ Adds a dialog to the layout which will be opened.
    //@ See TFRE_DB_FORM_DIALOG_DESC.
    procedure AddFormDialog     (const dialog:TFRE_DB_FORM_DIALOG_DESC);
    //@ Sets the relative size of the content section.
    //@ Will only be used if no explicit center section is added with AddSection procedure.
    //@ Default value is 3.
    procedure setContentSize    (const size: Integer);
    //@ Sets the whole Layout at once.
    //@ Warning: This will override the useSizedSections parameter set by the describe method
    function  SetLayout          (const leftSection,centerSection:TFRE_DB_CONTENT_DESC;const rightSection:TFRE_DB_CONTENT_DESC=nil;const topSection:TFRE_DB_CONTENT_DESC=nil;const bottomSection:TFRE_DB_CONTENT_DESC=nil;const resizeable:boolean=true;
                                  const left_size:integer=-1;const center_size:integer=-1;const right_size:integer=-1;const top_size:integer=-1;const bottom_size:integer=-1):TFRE_DB_LAYOUT_DESC;
    //@ Sets the whole Layout at once.
    //@ Warning: This will override the useSizedSections parameter set by the describe method
    function  SetAutoSizedLayout (const leftSection,centerSection:TFRE_DB_CONTENT_DESC;const rightSection:TFRE_DB_CONTENT_DESC=nil;const topSection:TFRE_DB_CONTENT_DESC=nil;const bottomSection:TFRE_DB_CONTENT_DESC=nil):TFRE_DB_LAYOUT_DESC;
  end;

  { TFRE_DB_EDITOR_DESC }

  TFRE_DB_EDITOR_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an editor.
    //@ If no save function is defined the editor is read only. In this case starEditFunc and stopEditFunc are not needed either.
    function Describe        (const loadFunc:TFRE_DB_SERVER_FUNC_DESC; const saveFunc,startEditFunc,stopEditFunc: TFRE_DB_SERVER_FUNC_DESC; const contentType: TFRE_DB_CONTENT_TYPE=ct_html; const toolbarBottom: Boolean=true): TFRE_DB_EDITOR_DESC;
  end;

  { TFRE_DB_VNC_DESC }

  TFRE_DB_VNC_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a vnc view.
    function Describe (const host:String; const port: Integer): TFRE_DB_VNC_DESC;
  end;

  { TFRE_DB_SHELL_DESC }

  TFRE_DB_SHELL_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a shell view.
    function Describe (const host:String; const port: Integer; const path: String; const protocol: String='http'): TFRE_DB_SHELL_DESC;
  end;

  { TFRE_DB_HORDE_DESC }

  TFRE_DB_HORDE_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a shell view.
    function Describe (const host:String; const port: Integer=443; const protocol: String='https'): TFRE_DB_HORDE_DESC;
  end;

  { TFRE_DB_STORE_DATA_DESC }

  TFRE_DB_STORE_DATA_DESC  = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the result of store data request. (e.g. grid, chart...)
    function  Describe (const totalCount: Int32): TFRE_DB_STORE_DATA_DESC;
    //@ Adds an entry to the result.
    procedure addEntry     (const entry: IFRE_DB_Object);
  end;

  { TFRE_DB_UPDATE_STORE_DESC }

  TFRE_DB_UPDATE_STORE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an update of a store.
    function  Describe        (const storeId:String):TFRE_DB_UPDATE_STORE_DESC;
    //@ Adds an updated entry and moves it to a new Position.
    procedure addUpdatedEntry (const entry: IFRE_DB_Object ; const position: Int64 ; const absolutecount: Int64);
    //@ Adds the id of a deleted entry.
    procedure addDeletedEntry (const entryId: String; const position: Int64 ; const absolutecount: Int64);
    //@ Adds a new entry.
    //@ parentId is only useful for tree grids. If not parentId is given the new item is added as root item.
    //@ use nextItemId = '' to insert the new item at the end of the query.
    procedure addNewEntry     (const entry: IFRE_DB_Object; const position: Int64 ; const absolutecount: Int64);
    //@ Sets the new total count.
    procedure setTotalCount   (const count: Integer);
    function  hasChanges      : Boolean;
  end;

  { TFRE_DB_LIVE_CHART_DESC }

  TFRE_DB_LIVE_CHART_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function _Describe           (const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin,dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray;const dataLabels: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray;
                                  const dataTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC;const dataCount: Integer; const updateInterval: Integer; const buffer: Integer; const seriesType: TFRE_DB_LIVE_CHART_TYPE): TFRE_DB_LIVE_CHART_DESC;
  public
    //@ Describes a line live chart.
    //@ id: id of the chart. Needed by the data feeder to address the chart.
    //@ seriesCount: number of series in the chart
    //@ stopStartCB: server function called on start and stop. Parameter action will have the value "start" or "stop".
    //@ dataMin, dataMax: limits the data-axis
    //@ dataTickHint: axis will get approximately 'dataTickHint' ticks
    //@ initData: server function called to get the initial data of the live chart. Has to hold dataCount + buffer + 1 data elements.
    //@ dataCount: data count per series
    //@ updateInterval: in milliseconds
    //@ buffer: number of data elements to hold as buffer
    function DescribeLine        (const id: String; const seriesCount: Integer; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin,dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray=nil;const legendLabels: TFRE_DB_StringArray=nil;
                                  const dataTickHint: Integer=10; const initData: TFRE_DB_SERVER_FUNC_DESC=nil; const updateInterval: Integer=1000; const buffer: Integer=1): TFRE_DB_LIVE_CHART_DESC;

    //@ Describes a sampled line live chart.
    //@ dataCount: data count per series
    function DescribeSampledLine (const id: String; const seriesCount: Integer; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin,dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray=nil;const legendLabels: TFRE_DB_StringArray=nil;
                                  const dataTickHint: Integer=10): TFRE_DB_LIVE_CHART_DESC;

    //@ Describes a column chart.
    //@ dataCount is the number of columns.
    //@ seriesCount is the number of values per column. Currently only a seriesCount of 1 is implemented.
    function DescribeColumn      (const id: String; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const caption: String; const dataMax: Real; const dataMin: Real=0;
                                  const seriesColor: TFRE_DB_StringArray=nil; const dataLabels: TFRE_DB_StringArray=nil;  const legendLabels: TFRE_DB_StringArray=nil; const seriesCount: Integer=1): TFRE_DB_LIVE_CHART_DESC;
  end;

  { TFRE_DB_REDEFINE_LIVE_CHART_DESC }

  TFRE_DB_REDEFINE_LIVE_CHART_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function _Describe           (const id: String; const seriesCount: Integer; const dataCount: Integer; const dataMinMax: TFRE_DB_Real32Array; const dataLabels: TFRE_DB_StringArray;
                                  const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const caption: String): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  public
    //@ Describes a redefinition of a line live chart.
    //@ See DescribeLine of TFRE_DB_LIVE_CHART_DESC
    function DescribeLine        (const id: String; const seriesCount: Integer=0; const dataCount: Integer=0; const dataMinMax: TFRE_DB_Real32Array=nil;
                                  const seriesColor: TFRE_DB_StringArray=nil;const legendLabels: TFRE_DB_StringArray=nil; const caption: String=''): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
    //@ Describes a redefinition of a sampled line live chart.
    //@ See DescribeSampledLine of TFRE_DB_LIVE_CHART_DESC
    function DescribeSampledLine (const id: String; const seriesCount: Integer=0; const dataCount: Integer=0; const dataMinMax: TFRE_DB_Real32Array=nil;
                                  const seriesColor: TFRE_DB_StringArray=nil; const legendLabels: TFRE_DB_StringArray=nil; const caption: String=''): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
    //@ Describes a redefinition of a column chart.
    //@ See DescribeColumn of TFRE_DB_LIVE_CHART_DESC
    function DescribeColumn      (const id: String; const dataCount: Integer=0; const dataLabels: TFRE_DB_StringArray=nil; const dataMinMax: TFRE_DB_Real32Array=nil;
                                  const seriesCount: Integer=0; const legendLabels: TFRE_DB_StringArray=nil; const seriesColor: TFRE_DB_StringArray=nil; const caption: String=''): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  end;

  { TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC }

  TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the data of a live chart at index "idx".
    //@ id: id of the chart.
    //@ dataIndex: index of the data since the last start action
    function Describe        (const id: String; const dataIndex: Integer; const data: TFRE_DB_Real32Array): TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC;
  end;

  TFRE_DB_LIVE_CHART_DATA_ARRAY = array of TFRE_DB_Real32Array;

  { TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC }

  TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the complete data of a live chart.
    //@ id: id of the chart.
    function Describe        (const id: String; const data: TFRE_DB_LIVE_CHART_DATA_ARRAY): TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC;
  end;

  { TFRE_DB_LIVE_CHART_INIT_DATA_DESC }

  TFRE_DB_LIVE_CHART_INIT_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the compleate initial data of a live chart. Has to be used as response within the initData server function.
    function Describe        (const data: TFRE_DB_LIVE_CHART_DATA_ARRAY): TFRE_DB_LIVE_CHART_INIT_DATA_DESC;
  end;


  { TFRE_DB_RESOURCE_DESC }

  TFRE_DB_RESOURCE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a resource. E.g. an icon.
    //@ FIXXME: Not implemented yet.
    function Describe (const data: IFRE_DB_Object): TFRE_DB_RESOURCE_DESC;
  end;

  { TFRE_DB_MAIN_DESC }

  TFRE_DB_MAIN_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the main page.
    //@ Used internally. May be removed.
    function  Describe                (const style: String; const jiraIntegrationJSURL: String=''): TFRE_DB_MAIN_DESC;
  end;

  { TFRE_DB_SVG_DESC }

  TFRE_DB_SVG_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a svg panel.
    function  Describe  (const svg: String; const id:String=''): TFRE_DB_SVG_DESC;
  end;

  { TFRE_DB_UPDATE_SVG_DESC }

  TFRE_DB_UPDATE_SVG_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an update of an existing svg panel.
    function  Describe  (const svgId,elementId,attrName,attrValue:String): TFRE_DB_UPDATE_SVG_DESC;
  end;

  function String2DBChooserDH(const fts: string): TFRE_DB_CHOOSER_DH;
  function String2DBLayoutPos(const fts: string): TFRE_DB_LAYOUT_POS;
  function String2DBSubSecDisplayType(const fts: string): TFRE_DB_SUBSEC_DISPLAY_TYPE;
  function String2DBMessageType(const fts: string): TFRE_DB_MESSAGE_TYPE;
  function String2DBButtonType(const fts: string): TFRE_DB_BUTTON_TYPE;
  function String2DBGridButtonDep(const fts: string): TFRE_DB_GRID_BUTTON_DEP;
  function String2DBContent(const fts: string): TFRE_DB_CONTENT_TYPE;

implementation

  function String2DBChooserDH(const fts: string): TFRE_DB_CHOOSER_DH;
  begin
    for result in TFRE_DB_CHOOSER_DH do begin
       if CFRE_DB_CHOOSER_DH[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBChooserDH specifier : ['+fts+']');
  end;


  function String2DBLayoutPos(const fts: string): TFRE_DB_LAYOUT_POS;
  begin
    for result in TFRE_DB_LAYOUT_POS do begin
       if CFRE_DB_LAYOUT_POS[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBLayoutPos specifier : ['+fts+']');
  end;

  function String2DBSubSecDisplayType(const fts: string): TFRE_DB_SUBSEC_DISPLAY_TYPE;
  begin
    for result in TFRE_DB_SUBSEC_DISPLAY_TYPE do begin
       if CFRE_DB_SUBSEC_DISPLAY_TYPE[result]=fts then exit;
    end;
    raise Exception.Create('invalid short SubsecDisplayType specifier : ['+fts+']');
  end;

  function String2DBMessageType(const fts: string): TFRE_DB_MESSAGE_TYPE;
  begin
    for result in TFRE_DB_MESSAGE_TYPE do begin
       if CFRE_DB_MESSAGE_TYPE[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBMessageType specifier : ['+fts+']');
  end;

  function String2DBButtonType(const fts: string): TFRE_DB_BUTTON_TYPE;
  begin
    for result in TFRE_DB_BUTTON_TYPE do begin
       if CFRE_DB_BUTTON_TYPE[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBButtonType specifier : ['+fts+']');
  end;

  function String2DBGridButtonDep(const fts: string): TFRE_DB_GRID_BUTTON_DEP;
  begin
    for result in TFRE_DB_GRID_BUTTON_DEP do begin
       if CFRE_DB_GRID_BUTTON_DEP[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBGridButtonDep specifier : ['+fts+']');
  end;

  function String2DBContent(const fts: string): TFRE_DB_CONTENT_TYPE;
  begin
    for result in TFRE_DB_CONTENT_TYPE do begin
       if CFRE_DB_CONTENT_TYPE[result]=fts then exit;
    end;
    raise Exception.Create('invalid short DBContentType specifier : ['+fts+']');
  end;

  { TFRE_DB_HORDE_DESC }

    function TFRE_DB_HORDE_DESC.Describe(const host: String; const port: Integer; const protocol: String): TFRE_DB_HORDE_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('host').AsString:=host;
    Field('port').AsInt32:=port;
    Field('protocol').AsString:=protocol;
    Result:=Self;
  end;

  { TFRE_DB_DIALOG_DESC }

  function TFRE_DB_DIALOG_DESC.Describe(const caption: String; const content: TFRE_DB_CONTENT_DESC; const percWidth: Integer; const percHeight: Integer; const maxWidth: Integer; const maxHeight: Integer; const isDraggable: Boolean; const styleClass: String): TFRE_DB_DIALOG_DESC;
  begin
    Field('dialogCaption').AsString:=caption;
    Field('content').AsObject:=content;
    Field('percWidth').AsInt16:=percWidth;
    Field('percHeight').AsInt16:=percHeight;
    Field('maxWidth').AsInt16:=maxWidth;
    Field('maxHeight').AsInt16:=maxHeight;
    Field('draggable').AsBoolean:=isDraggable;
    Field('styleClass').AsString:=styleClass;
    Result:=Self;
  end;

  function TFRE_DB_DIALOG_DESC.AddButton: TFRE_DB_BUTTON_DESC;
  begin
    Result := TFRE_DB_BUTTON_DESC.create;
    Field('buttons').AddObject(Result);
  end;

  { TFRE_DB_UPDATE_SVG_DESC }

  function TFRE_DB_UPDATE_SVG_DESC.Describe(const svgId, elementId, attrName, attrValue: String): TFRE_DB_UPDATE_SVG_DESC;
  begin
    Field('svgId').AsString:=svgId;
    Field('elementId').AsString:=elementId;
    Field('attrName').AsString:=attrName;
    Field('attrValue').AsString:=attrValue;
    Result:=Self;
  end;

  { TFRE_DB_SVG_DESC }

  function TFRE_DB_SVG_DESC.Describe(const svg: String; const id: String): TFRE_DB_SVG_DESC;
  begin
   if id='' then begin
     if not FieldExists('id') then begin
        Field('id').AsString:='id'+UID_String;
      end;
    end else begin
      Field('id').AsString:=id;
    end;
    Field('svg').AsString:=svg;
    Result:=Self;
  end;

  { TFRE_DB_REDEFINE_LIVE_CHART_DESC }

  function TFRE_DB_REDEFINE_LIVE_CHART_DESC._Describe(const id: String; const seriesCount: Integer; const dataCount: Integer; const dataMinMax: TFRE_DB_Real32Array; const dataLabels: TFRE_DB_StringArray; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const caption: String): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  begin
    Field('id').AsString:=id;
    Field('dataCount').AsInt64:=dataCount;
    if Assigned(dataLabels) then begin
      Field('dataLabels').AsStringArr:=dataLabels;
    end;
    Field('caption').AsString:=caption;
    if Assigned(dataMinMax) then begin
      Field('dataMin').AsReal32:=dataMinMax[0];
      if Length(dataMinMax)=2 then begin
        Field('dataMax').AsReal32:=dataMinMax[1];
      end;
    end;
    Field('seriesCount').AsInt16:=seriesCount;
    if Assigned(legendLabels) then begin
      Field('legendLabels').AsStringArr:=legendLabels;
    end;
    if Assigned(seriesColor) then begin
      Field('seriesColor').AsStringArr:=seriesColor;
    end;
    Result:=Self;
  end;

  function TFRE_DB_REDEFINE_LIVE_CHART_DESC.DescribeLine(const id: String; const seriesCount: Integer; const dataCount: Integer; const dataMinMax: TFRE_DB_Real32Array; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const caption: String): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  begin
    Result:=_Describe(id,seriesCount,dataCount,dataMinMax,nil,seriesColor,legendLabels,caption);
  end;

  function TFRE_DB_REDEFINE_LIVE_CHART_DESC.DescribeSampledLine(const id: String; const seriesCount: Integer; const dataCount: Integer; const dataMinMax: TFRE_DB_Real32Array; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const caption: String): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  begin
    Result:=_Describe(id,seriesCount,dataCount,dataMinMax,nil,seriesColor,legendLabels,caption);
  end;

  function TFRE_DB_REDEFINE_LIVE_CHART_DESC.DescribeColumn(const id: String; const dataCount: Integer; const dataLabels: TFRE_DB_StringArray; const dataMinMax: TFRE_DB_Real32Array; const seriesCount: Integer; const legendLabels: TFRE_DB_StringArray; const seriesColor: TFRE_DB_StringArray; const caption: String): TFRE_DB_REDEFINE_LIVE_CHART_DESC;
  begin
    Result:=_Describe(id,seriesCount,dataCount,dataMinMax,dataLabels,seriesColor,legendLabels,caption);
  end;

  { TFRE_DB_INPUT_RECURRENCE_DESC }

  function TFRE_DB_INPUT_RECURRENCE_DESC.Describe(const caption,field_reference: String; const intervals: TFRE_DB_REC_INTERVAL_TYPES; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String): TFRE_DB_INPUT_RECURRENCE_DESC;
  begin
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue);
    Field('rIOnce').AsBoolean:=rit_once in intervals;
    Field('rIMinute').AsBoolean:=rit_minute in intervals;
    Field('rIHour').AsBoolean:=rit_hour in intervals;
    Field('rIDay').AsBoolean:=rit_day in intervals;
    Field('rIWeek').AsBoolean:=rit_week in intervals;
    Field('rIMonth').AsBoolean:=rit_month in intervals;
    Field('rIQuarter').AsBoolean:=rit_quarter in intervals;
    Field('rIYear').AsBoolean:=rit_year in intervals;
    Result:=Self;
  end;

  { TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC }

  function TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC.Describe(const id: String; const data: TFRE_DB_LIVE_CHART_DATA_ARRAY): TFRE_DB_LIVE_CHART_COMPLETE_DATA_DESC;
  var
    i: Integer;
  begin
    Field('id').AsString:=id;
    Field('dataCount').AsInt16:=Length(data);
    for i := 0 to Length(data) - 1 do begin
      Field('data'+IntToStr(i)).AsReal32Arr:=data[i];
    end;
    Result:=Self;
  end;

  { TFRE_DB_SHELL_DESC }

  function TFRE_DB_SHELL_DESC.Describe(const host: String; const port: Integer; const path: String; const protocol: String): TFRE_DB_SHELL_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('host').AsString:=host;
    Field('port').AsInt32:=port;
    Field('path').AsString:=path;
    Field('protocol').AsString:=protocol;
    Result:=Self;
  end;

  { TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC }

  function TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC.Describe(const id: String; const dataIndex: Integer; const data: TFRE_DB_Real32Array): TFRE_DB_LIVE_CHART_DATA_AT_IDX_DESC;
  begin
    Field('id').AsString:=id;
    Field('dataIndex').AsInt64:=dataIndex;
    Field('data').AsReal32Arr:=data;
    Result:=Self;
  end;

  { TFRE_DB_LIVE_CHART_INIT_DATA_DESC }

  function TFRE_DB_LIVE_CHART_INIT_DATA_DESC.Describe(const data: TFRE_DB_LIVE_CHART_DATA_ARRAY): TFRE_DB_LIVE_CHART_INIT_DATA_DESC;
  var
    i: Integer;
  begin
    Field('dataCount').AsInt16:=Length(data);
    for i := 0 to Length(data) - 1 do begin
      Field('data'+IntToStr(i)).AsReal32Arr:=data[i];
    end;
    Result:=Self;
  end;

  { TFRE_DB_UPDATE_UI_ELEMENT_DESC }

  function TFRE_DB_UPDATE_UI_ELEMENT_DESC.DescribeStatus(const elementId: String; const disabled: Boolean; const newCaption: String; const newHint: String): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
  begin
    Field('id').AsString:=elementId;
    Field('disabled').AsBoolean:=disabled;
    Field('newCaption').AsString:=newCaption;
    Field('newHint').AsString:=newHint;
    Result:=Self;
  end;

  function TFRE_DB_UPDATE_UI_ELEMENT_DESC.DescribeSubmenu(const elementId: String; const menu: TFRE_DB_MENU_DESC): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
  begin
    Field('id').AsString:=elementId;
    Field('menu').AsObject:=menu;
    Result:=Self;
  end;

  function TFRE_DB_UPDATE_UI_ELEMENT_DESC.DescribeDrag(const elementId: String; const disabled: Boolean): TFRE_DB_UPDATE_UI_ELEMENT_DESC;
  begin
    Field('id').AsString:=elementId;
    Field('disableDrag').AsBoolean:=disabled;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_FILE_DESC }

  function TFRE_DB_INPUT_FILE_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator; const validatorConfigParams: IFRE_DB_Object; const multiValues: Boolean): TFRE_DB_INPUT_FILE_DESC;
  begin
    if Assigned(validator) and multiValues and (validator.ObjectName='image') then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'Image input is not allowed to have multiple values');
    end;
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator, validatorConfigParams);
    Field('multiValues').AsBoolean := multiValues;
    Result:=Self;
  end;

  { TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC }

  function TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC.Describe(const entryPath: TFRE_DB_StringArray; const newsCount: Integer): TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC;
  begin
    Field('entryPath').AsStringArr:=entryPath;
    Field('newsCount').AsInt16:=newsCount;
    Result:=Self;
  end;

  { TFRE_DB_VNC_DESC }

  function TFRE_DB_VNC_DESC.Describe(const host: String; const port: Integer): TFRE_DB_VNC_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('host').AsString:=host;
    Field('port').AsInt32:=port;
    Result:=Self;
  end;

  { TFRE_DB_TOPMENU_DESC }

  function TFRE_DB_TOPMENU_DESC.Describe(const homeCaption, homeIcon: String; const homeIconSize: Integer; const serverFuncs: array of TFRE_DB_SERVER_FUNC_DESC; const mainSectionId: TFRE_DB_String; const sectionsIds: array of TFRE_DB_String; const uname: String; const uServerFunction: TFRE_DB_SERVER_FUNC_DESC; const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY; const notificationPanel: TFRE_DB_CONTENT_DESC; const notificationInitialClosed: Boolean; const JIRAenabled: Boolean): TFRE_DB_TOPMENU_DESC;
  var
    i: Integer;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;

    Field('homeCaption').AsString:=homeCaption;
    Field('homeIcon').AsString:=FREDB_getThemedResource(homeIcon);
    Field('homeIconSize').AsInt16:=homeIconSize;
    Field('mainSectionId').AsString:=mainSectionId;

    if Length(sectionsIds)<>Length(serverFuncs) then
      raise EFRE_DB_Exception.Create(edb_ERROR,'serverFuncs array and sectionIds array not the same length');
    for i := 0 to High(serverFuncs) do begin
      Field('serverFuncs').AddObject(serverFuncs[i]);
      Field('sectionsIds').AddString(sectionsIds[i]);
    end;

    Field('uname').AsString:=uname;
    Field('uServerFunc').AsObject:=uServerFunction;

    if Assigned(notificationPanel) then begin
      Field('notificationPanel').AsObject:=notificationPanel;
      Field('notificationInitialClosed').AsBoolean:=notificationInitialClosed;
    end;
    if Assigned(svgDefs) then begin
      for i := 0 to Length(svgDefs) - 1 do begin
        Field('svgDefs').AddObject(svgDefs[i]);
      end;
    end;
    Field('JIRAenabled').AsBoolean:=JIRAenabled;
    Result:=Self;
  end;

  procedure TFRE_DB_TOPMENU_DESC.AddFormDialog(const dialog: TFRE_DB_FORM_DIALOG_DESC);
  begin
    Field('formdialog').AsObject:=dialog;
  end;

  { TFRE_DB_LIVE_CHART_DESC }

  function TFRE_DB_LIVE_CHART_DESC._Describe(const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin, dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const dataLabels: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const dataTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC; const dataCount: Integer; const updateInterval: Integer; const buffer: Integer; const seriesType: TFRE_DB_LIVE_CHART_TYPE): TFRE_DB_LIVE_CHART_DESC;
  begin
    Field('id').AsString:=id;
    Field('seriesCount').AsInt16:=seriesCount;
    Field('serverFunc').AsObject:=stopStartCB;
    Field('type').AsString:=CFRE_DB_LIVE_CHART_TYPE[seriesType];
    Field('dataMin').AsReal32:=dataMin;
    Field('dataMax').AsReal32:=dataMax;
    Field('caption').AsString:=caption;
    Field('dataTickHint').AsInt16:=dataTickHint;
    Field('dataCount').AsInt64:=dataCount;
    Field('updateInterval').AsInt16:=updateInterval;
    Field('buffer').AsInt16:=buffer;
    if Assigned(seriesColor) then begin
      Field('seriesColor').AsStringArr:=seriesColor;
    end;
    if Assigned(dataLabels) then begin
      Field('dataLabels').AsStringArr:=dataLabels;
    end;
    if Assigned(legendLabels) then begin
      Field('legendLabels').AsStringArr:=legendLabels;
    end;
    if Assigned(initData) then begin
      Field('initDataFunc').AsObject:=initData;
    end;
    Result:=Self;
  end;

  function TFRE_DB_LIVE_CHART_DESC.DescribeLine(const id: String; const seriesCount: Integer; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin, dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const dataTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC; const updateInterval: Integer; const buffer: Integer): TFRE_DB_LIVE_CHART_DESC;  begin
    Result:=_Describe(id,seriesCount,stopStartCB,dataMin,dataMax,caption,seriesColor,nil,legendLabels,dataTickHint,initData,dataCount,updateInterval,buffer,fdblct_line);
  end;

  function TFRE_DB_LIVE_CHART_DESC.DescribeSampledLine(const id: String; const seriesCount: Integer; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const dataMin, dataMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const dataTickHint: Integer): TFRE_DB_LIVE_CHART_DESC;
  begin
    Result:=_Describe(id,seriesCount,stopStartCB,dataMin,dataMax,caption,seriesColor,nil,legendLabels,dataTickHint,nil,dataCount,0,0,fdblct_sampledline);
  end;

  function TFRE_DB_LIVE_CHART_DESC.DescribeColumn(const id: String; const dataCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const caption: String; const dataMax: Real; const dataMin: Real; const seriesColor: TFRE_DB_StringArray; const dataLabels: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const seriesCount: Integer): TFRE_DB_LIVE_CHART_DESC;
  begin
    if Assigned(dataLabels) and (Length(dataLabels)<>dataCount) then raise EFRE_DB_Exception.Create(edb_ERROR,'dataLabels used but count is not the same as dataCount');
    Result:=_Describe(id,seriesCount,stopStartCB,dataMin,dataMax,caption,seriesColor,dataLabels,legendLabels,dataCount,nil,dataCount,0,0,fdblct_column);
  end;

  { TFRE_DB_INPUT_DATE_DESC }

  function TFRE_DB_INPUT_DATE_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator ; const validatorConfigParams : IFRE_DB_Object): TFRE_DB_INPUT_DATE_DESC;
  begin
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator, validatorConfigParams);
    Result:=Self;
  end;

  { TFRE_DB_UPDATE_STORE_DESC }

  function TFRE_DB_UPDATE_STORE_DESC.Describe(const storeId: String): TFRE_DB_UPDATE_STORE_DESC;
  begin
    Field('storeId').AsString:=storeId;
    Result:=Self;
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addUpdatedEntry(const entry: IFRE_DB_Object; const position: Int64; const absolutecount: Int64);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('item').AddObject(entry.CloneToNewObject());
    obj.Field('pos').AsInt64    := position;
    obj.Field('total').AsInt32:=absolutecount;
    Field('updated').AddObject(obj);
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addDeletedEntry(const entryId: String; const position: Int64; const absolutecount: Int64);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('itemid').AddString(entryId);
    obj.Field('pos').AsInt64:=position;
    obj.Field('total').AsInt32:=absolutecount;
    Field('deleted').AddObject(obj);
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addNewEntry(const entry: IFRE_DB_Object; const position: Int64; const absolutecount: Int64);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    //obj.Field('revid').AsString:=nextItemId;
    obj.Field('item').AsObject:=entry.CloneToNewObject();
    obj.Field('pos').AsInt64:=position;
    obj.Field('total').AsInt32:=absolutecount;
    Field('new').AddObject(obj);
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.setTotalCount(const count: Integer);
  begin
    Field('total').AsInt32:=count;
  end;

  function TFRE_DB_UPDATE_STORE_DESC.hasChanges: Boolean;
  begin
    Result:=(Field('new').ValueCount + Field('deleted').ValueCount + Field('updated').ValueCount)>0;
  end;

  { TFRE_DB_INPUT_NUMBER_DESC }

  function TFRE_DB_INPUT_NUMBER_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean;  const disabled: boolean; const hidden: Boolean; const defaultValue: String; const digits: Integer; const minMax: TFRE_DB_Real64Array): TFRE_DB_INPUT_NUMBER_DESC;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,hidden,defaultValue);
    Field('digits').AsInt16:=digits;
    if Assigned(minMax) then begin
      if Length(minMax)<>2 then raise EFRE_DB_Exception.Create(edb_ERROR,'minMax definition Array has to be of length 2');
      Field('minMax').AsReal64Arr:=minMax;
    end;
    Field('steps').AsInt16:=-1;
    Result:=Self;
  end;

  function TFRE_DB_INPUT_NUMBER_DESC.DescribeSlider(const caption, field_reference: String; const min, max: Real; const showValueField: Boolean; const defaultValue: String; const digits: Integer; const steps: Integer): TFRE_DB_INPUT_NUMBER_DESC;
  begin
    inherited Describe(caption,field_reference,false,false,false,false,defaultValue);
    Field('displaySlider').AsBoolean:=true;
    Field('showValueField').AsBoolean:=showValueField;
    Field('digits').AsInt16:=digits;
    Field('minMax').AsReal64Arr:=TFRE_DB_Real64Array.create(min,max);
    Field('steps').AsInt16:=steps;
    Result:=Self;
  end;

  procedure TFRE_DB_INPUT_NUMBER_DESC.setMinMax(const min, max: Real);
  begin
    Field('minMax').AsReal64Arr:=TFRE_DB_Real64Array.create(min,max);
  end;

  { TFRE_DB_INPUT_DESCRIPTION_DESC }

  function TFRE_DB_INPUT_DESCRIPTION_DESC.Describe(const caption,description_: String): TFRE_DB_INPUT_DESCRIPTION_DESC;
  begin
    inherited Describe(caption, '', false, false, true, false, description_);
    Result:=Self;
  end;

  { TFRE_DB_MAIN_DESC }

  function TFRE_DB_MAIN_DESC.Describe(const style: String; const jiraIntegrationJSURL: String): TFRE_DB_MAIN_DESC;
  begin
    Field('loadFunc').AsObject:=TFRE_DB_SERVER_FUNC_DESC.create.Describe('FIRMOS','init');
    Field('style').AsString:=style;
    Field('jira').AsString:=jiraIntegrationJSURL;
    Result:=Self;
  end;

  { TFRE_DB_VIEW_LIST_BUTTON_DESC }

  function TFRE_DB_VIEW_LIST_BUTTON_DESC._Describe(const func: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const caption: String; const tooltip: String; const buttonDep: TFRE_DB_GRID_BUTTON_DEP): TFRE_DB_VIEW_LIST_BUTTON_DESC;
  begin
    Field('serverFunc').AsObject := func;
    if icon<>'' then begin
      Field('icon').AsString:=FREDB_getThemedResource(icon);
    end;
    Field('caption').AsString:=caption;
    Field('tooltip').AsString:=tooltip;
    Field('dep').AsString:=CFRE_DB_GRID_BUTTON_DEP[buttonDep];
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
  end;

  function TFRE_DB_VIEW_LIST_BUTTON_DESC.Describe(const func: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const caption: String; const tooltip: String; const buttonDep: TFRE_DB_GRID_BUTTON_DEP): TFRE_DB_VIEW_LIST_BUTTON_DESC;
  begin
    if buttonDep=fdgbd_manual then raise EFRE_DB_Exception.Create(edb_ERROR,'Use DescribeManualType to describe a button of type fdgbd_manual.');
    Result:=_Describe(func,icon,caption,tooltip,buttonDep);
  end;

  function TFRE_DB_VIEW_LIST_BUTTON_DESC.DescribeManualType(const id: String; const func: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const caption: String; const tooltip: String; const disabled: Boolean): TFRE_DB_VIEW_LIST_BUTTON_DESC;
  begin
    Field('id').AsString:=id;
    Field('disabled').AsBoolean:=disabled;
    Result:=_Describe(func,icon,caption,tooltip,fdgbd_manual);
  end;

  { TFRE_DB_BUTTON_DESC }

  function TFRE_DB_BUTTON_DESC.Describe(const caption: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const buttonType: TFRE_DB_BUTTON_TYPE): TFRE_DB_BUTTON_DESC;
  begin
    if buttonType=fdbbt_download then raise EFRE_DB_Exception.Create(edb_ERROR,'Please use DescribeDownload to configure a download button (fdbbt_download).');
    Field('caption').AsString:=caption;
    if Assigned(serverFunc) then begin
      Field('serverFunc').AsObject:=serverFunc;
    end;
    Field('buttonType').AsString:=CFRE_DB_BUTTON_TYPE[buttonType];
    Result:=Self;
  end;

  function TFRE_DB_BUTTON_DESC.DescribeDownload(const caption: String; const downloadId: String; const closeDialog: Boolean): TFRE_DB_BUTTON_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('downloadId').AsString:=downloadId;
    Field('closeDialog').AsBoolean:=closeDialog;
    Field('buttonType').AsString:=CFRE_DB_BUTTON_TYPE[fdbbt_download];
    Result:=Self;
  end;

  { TFRE_DB_VALIDATOR_DESC }

  function TFRE_DB_VALIDATOR_DESC.Describe(const id,regExp, helpTextKey: String; const allowedChars: String; const replaceRegExp:String; const replaceValue:String; const configParams: IFRE_DB_Object): TFRE_DB_VALIDATOR_DESC;
  begin
    Field('id').AsString:=id;
    Field('regExp').AsString:=regExp;
    Field('helpTextKey').AsString:=helpTextKey;
    Field('allowedChars').AsString:=allowedChars;
    Field('replaceRegExp').AsString:=replaceRegExp;
    Field('replaceValue').AsString:=replaceValue;
    if Assigned(configParams) then begin
      Field('configParams').AsObject:=configParams.CloneToNewObject();
    end;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_DESC }

  function TFRE_DB_INPUT_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator;  const validatorConfigParams : IFRE_DB_Object; const multiValues:Boolean; const isPass:Boolean; const confirms: String): TFRE_DB_INPUT_DESC;
  begin
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator, validatorConfigParams);
    Field('multiValues').AsBoolean := multiValues;
    Field('isPass').AsBoolean:=isPass;
    Field('confirms').AsString:=LowerCase(confirms);
    Result:=Self;
  end;

  { TFRE_DB_HTML_DESC }

  function TFRE_DB_HTML_DESC.Describe(const html: String; const height: Integer; const width: Integer; const border:Boolean=false): TFRE_DB_HTML_DESC;
  begin
    Field('html').AsString:=html;
    Field('height').AsInt16:=height;
    Field('width').AsInt16:=width;
    Field('border').AsBoolean:=border;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_BLOCK_DESC }

  function TFRE_DB_INPUT_BLOCK_DESC.Describe(const caption: String): TFRE_DB_INPUT_BLOCK_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('caption').AsString:=caption;
    Field('sizeSum').AsInt16:=0;
    Result:=Self;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddSchemeFormGroup(const schemeGroup: IFRE_DB_InputGroupSchemeDefinition; const session: IFRE_DB_UserSession; const collapsible: Boolean; const collapsed: Boolean; const relSize: Integer): TFRE_DB_INPUT_GROUP_DESC;
  begin
    Result:=inherited AddSchemeFormGroup(schemeGroup,session,collapsible,collapsed);
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddInput(const relSize: Integer): TFRE_DB_INPUT_DESC;
  begin
    Result:=inherited AddInput;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddDescription(const relSize: Integer): TFRE_DB_INPUT_DESCRIPTION_DESC;
  begin
    Result:=inherited AddDescription;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddBool(const relSize: Integer): TFRE_DB_INPUT_BOOL_DESC;
  begin
    Result:=inherited AddBool;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddNumber(const relSize: Integer): TFRE_DB_INPUT_NUMBER_DESC;
  begin
    Result:=inherited AddNumber;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddChooser(const relSize: Integer): TFRE_DB_INPUT_CHOOSER_DESC;
  begin
    Result:=inherited AddChooser;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddDate(const relSize: Integer): TFRE_DB_INPUT_DATE_DESC;
  begin
    Result:=inherited AddDate;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddFile(const relSize: Integer): TFRE_DB_INPUT_FILE_DESC;
  begin
    Result:=inherited AddFile;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddGroup(const relSize: Integer): TFRE_DB_INPUT_GROUP_DESC;
  begin
    Result:=inherited AddGroup;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddGroupProxy(const relSize: Integer): TFRE_DB_INPUT_GROUP_PROXY_DESC;
  begin
    Result:=inherited AddGroupProxy;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddBlock(const relSize: Integer): TFRE_DB_INPUT_BLOCK_DESC;
  begin
    Result:=inherited AddBlock;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddList(const relSize: Integer): TFRE_DB_VIEW_LIST_DESC;
  begin
    Result:=inherited AddList;
    Result.Field('relSize').AsInt16:=relSize;
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+relSize;
  end;

  procedure TFRE_DB_INPUT_BLOCK_DESC.AddStore(const store: TFRE_DB_STORE_DESC);
  var
    obj: IFRE_DB_Object;
  begin
    obj:= Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        TFRE_DB_FORM_DESC(obj.Implementor_HC).AddStore(store);
      end else begin
        inherited AddStore(store);
      end;
    end else begin
      raise Exception.Create('Failed to add the store: Root Form not found!');
    end;
  end;

  procedure TFRE_DB_INPUT_BLOCK_DESC.AddDBO(const id: String; const session: IFRE_DB_UserSession);
  var
    obj: IFRE_DB_Object;
  begin
    obj := Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        (obj.Implementor_HC as TFRE_DB_FORM_DESC).AddDBO(id, session);
      end else begin
        inherited AddDBO(id,session);
      end;
    end else begin
      raise Exception.Create('Failed to add the dbo: Root Form not found!');
    end;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.GetStore(const id: String): TFRE_DB_STORE_DESC;
  var
    obj: IFRE_DB_Object;
  begin
    obj := Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        TFRE_DB_FORM_DESC(obj.Implementor_HC).GetStore(id);
      end else begin
        inherited GetStore(id);
      end;
    end else begin
      raise Exception.Create('Failed to get the store: Root Form not found!');
    end;
  end;

  function TFRE_DB_INPUT_GROUP_PROXY_DESC.Describe(const caption: String; const loadFunc: TFRE_DB_SERVER_FUNC_DESC): TFRE_DB_INPUT_GROUP_PROXY_DESC;
  begin
    _Describe(caption,true,true);
    Field('loadFunc').AsObject:=loadFunc;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_GROUP_PROXY_DESC }

  function TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC.Describe(const elementId: String):TFRE_DB_INPUT_GROUP_PROXY_DATA_DESC;
  begin
    Field('elementId').AsString:=elementId;
    Result:=Self;
  end;

  { TFRE_DB_RESOURCE_DESC }

  function TFRE_DB_RESOURCE_DESC.Describe(const data: IFRE_DB_Object): TFRE_DB_RESOURCE_DESC;
  begin
    Field('data').AsStream:=data.Field('data').AsStream;
    Field('mimetype').AsString:=data.Field('mimetype').AsString;
    Result:=Self;
  end;

  { TFRE_DB_STORE_DATA_DESC }

  function TFRE_DB_STORE_DATA_DESC.Describe(const totalCount: Int32): TFRE_DB_STORE_DATA_DESC;
  begin
    Field('total').AsInt32:=totalCount;
    Result:=Self;
  end;

  procedure TFRE_DB_STORE_DATA_DESC.addEntry(const entry: IFRE_DB_Object);
  begin
    Field('data').AddObject(entry);
  end;

  { TFRE_DB_FORM_INPUT_DESC }

  function TFRE_DB_FORM_INPUT_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator; const validatorConfigParams: IFRE_DB_Object): TFRE_DB_FORM_INPUT_DESC;
  begin
    Field('caption').AsString        := caption;
    Field('field').AsString          := LowerCase(field_reference);
    Field('defaultValue').AsString   := defaultValue;
    Field('required').AsBoolean      := required;
    Field('groupRequired').AsBoolean := groupRequired;
    Field('disabled').AsBoolean      := disabled;
    Field('hidden').AsBoolean        := hidden;
    if Assigned(validator) then begin
      Field('vtype').AsObject:=TFRE_DB_VALIDATOR_DESC.create.Describe(validator.ObjectName,validator.getRegExp,validator.getHelpTextKey,validator.getAllowedChars,validator.getReplaceRegExp,validator.getReplaceValue,validatorConfigParams);
    end;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_FORM_INPUT_DESC.AddDependence(const fieldName: String; const disablesField: Boolean);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('fieldName').AsString:=fieldName;
    obj.Field('disablesField').AsBoolean:=disablesField;
    Field('dependentFields').AddObject(obj);
  end;

  procedure TFRE_DB_FORM_INPUT_DESC.AddDependence(const inputGroup: TFRE_DB_INPUT_GROUP_DESC; const disablesFields: Boolean);
  var
    i: Integer;
  begin
    for i := 0 to inputGroup.Field('elements').ValueCount - 1 do begin
      AddDependence(inputGroup.Field('elements').AsObjectItem[i].Field('field').AsString,disablesFields);
    end;
  end;

  { TFRE_DB_STORE_ENTRY_DESC }

  function TFRE_DB_STORE_ENTRY_DESC.Describe(const caption, value: string):TFRE_DB_STORE_ENTRY_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('value').AsString:=value;
    Result:=Self;
  end;

  { TFRE_DB_STORE_DESC }

  function TFRE_DB_STORE_DESC.Describe(const idField: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const destroyFunc: TFRE_DB_SERVER_FUNC_DESC; const clearFunc: TFRE_DB_SERVER_FUNC_DESC; const id: String): TFRE_DB_STORE_DESC;
  begin
    Field('idField').AsString:=idField;
    if Assigned(serverFunc) then begin
      Field('serverFunc').AsObject:=serverFunc;
    end;
    if Assigned(clearFunc) then begin
      Field('clearFunc').AsObject:=clearFunc;
    end;
    if Assigned(destroyFunc) then begin
      Field('destroyFunc').AsObject:=destroyFunc;
    end;
    if id='' then begin
      Field('id').AsString:='id'+UID_String;
    end else begin
      Field('id').AsString:=id;
    end;
    result :=  self;
  end;

  function TFRE_DB_STORE_DESC.AddEntry: TFRE_DB_STORE_ENTRY_DESC;
  begin
    Result := TFRE_DB_STORE_ENTRY_DESC.create;
    Field('entries').AddObject(Result);
  end;

  function TFRE_DB_STORE_DESC.FindEntryByCaption(const caption: String): String;
  var
    i: Integer;
  begin
    Result:='';
    for i := 0 to Field('entries').ValueCount - 1 do begin
      if Field('entries').AsObjectItem[i].Field('caption').AsString=caption then begin
        Result:=Field('entries').AsObjectItem[i].Field('value').AsString;
        exit;
      end;
    end;
  end;

  { TFRE_DB_INPUT_BOOL_DESC }

  function TFRE_DB_INPUT_BOOL_DESC.Describe(const caption, field_reference: string; const required: boolean; const groupRequired: Boolean; const disabled: boolean; const defaultValue: Boolean): TFRE_DB_INPUT_BOOL_DESC;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,false,BoolToStr(defaultValue,'true','false'));
    Result:=Self;
  end;

  { TFRE_DB_INPUT_CHOOSER_DESC }

  procedure TFRE_DB_INPUT_CHOOSER_DESC.addDependentInput(const inputId: String; const chooserValue: String; const visible: TFRE_DB_FieldDepVisibility; const caption: String;const validator: IFRE_DB_ClientFieldValidator; const validatorConfigParams : IFRE_DB_Object);
  var
    obj: IFRE_DB_Object;
  begin
   obj:=GFRE_DBI.NewObject;
   obj.Field('inputId').AsString:=inputId;
   obj.Field('value').AsString:=chooserValue;
   obj.Field('visible').AsString:=CFRE_DB_FIELDDEPVISIBILITY[visible];
   obj.Field('caption').AsString:=caption;
   if Assigned(validator) then begin
     obj.Field('vtype').AsObject:=TFRE_DB_VALIDATOR_DESC.create.Describe(validator.ObjectName,validator.getRegExp,validator.getHelpTextKey,validator.getAllowedChars,validator.getReplaceRegExp,validator.getReplaceValue,validatorConfigParams);
   end;
   Field('dependentInputFields').AddObject(obj);
  end;

  procedure TFRE_DB_INPUT_CHOOSER_DESC.addDependentInputGroup(const inputGroup: TFRE_DB_INPUT_GROUP_DESC; const chooserValue: String; const visible: TFRE_DB_FieldDepVisibility);
  var
    i: Integer;
  begin
    for i := 0 to inputGroup.Field('elements').ValueCount - 1 do begin
      if inputGroup.Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_GROUP_DESC then begin
        addDependentInputGroup(inputGroup.Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_INPUT_GROUP_DESC,chooserValue,visible);
      end else begin
        addDependentInput(inputGroup.Field('elements').AsObjectItem[i].Field('field').AsString,chooserValue,visible);
      end;
    end;
  end;

  function TFRE_DB_INPUT_CHOOSER_DESC.Describe(const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const display_hint: TFRE_DB_CHOOSER_DH; const required: boolean; const groupRequired: Boolean; const add_empty_for_required: Boolean; const disabled: boolean; const defaultValue: String): TFRE_DB_INPUT_CHOOSER_DESC;
  var
    obj   : IFRE_DB_Object;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,false,defaultValue);
    Field('displayHint').AsString:=CFRE_DB_CHOOSER_DH[display_hint];
    Field('addEmptyForRequired').AsBoolean:=add_empty_for_required;
    obj:=GFRE_DBI.NewObject;
    obj.Field('id').AsString:=store.Field('id').AsString;
    obj.Field('serverFuncExists').AsBoolean:=store.FieldExists('serverFunc');
    Field('store').AsObject:=obj;
    Field('cce').AsBoolean:=false;
    (Parent.Parent.Implementor_HC as TFRE_DB_FORM_DESC).AddStore(store);
    Result:=Self;
  end;

  function TFRE_DB_INPUT_CHOOSER_DESC.DescribeMultiValue(const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const display_hint:TFRE_DB_CHOOSER_DH; const required: boolean; const groupRequired: Boolean; const add_empty_for_required: Boolean; const disabled: boolean; const defaultValue:TFRE_DB_StringArray): TFRE_DB_INPUT_CHOOSER_DESC;
  var
    defVal: String;
    i     : Integer;
  begin
    if Assigned(defaultValue) then begin
      defVal:=defaultValue[0];
    end else begin
      defVal:='';
    end;
    Describe(caption,field_reference,store,display_hint,required,groupRequired,add_empty_for_required,disabled,defVal);
    for i := 1 to High(defaultValue) do begin
      Field('defaultValue').AddString(defaultValue[i]);
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_INPUT_CHOOSER_DESC.addFilterEvent(const filteredStoreId,refId: String);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('storeId').AsString:=filteredStoreId;
    obj.Field('refId').AsString:=refId;
    Field('filteredStore').AddObject(obj);
  end;

  procedure TFRE_DB_INPUT_CHOOSER_DESC.captionCompareEnabled(const enabled: Boolean);
  begin
    Field('cce').AsBoolean:=enabled;
  end;

  { TFRE_DB_VIEW_LIST_DESC }

  function TFRE_DB_VIEW_LIST_DESC.AddButton: TFRE_DB_VIEW_LIST_BUTTON_DESC;
  begin
    Result:=TFRE_DB_VIEW_LIST_BUTTON_DESC.create;
    Field('buttons').AddObject(Result);
  end;

  function TFRE_DB_VIEW_LIST_DESC.Describe(const store: TFRE_DB_STORE_DESC;const layout: TFRE_DB_VIEW_LIST_LAYOUT_DESC;const itemContextMenuFunc: TFRE_DB_SERVER_FUNC_DESC; const title: String;const displayFlags: TFRE_COLLECTION_GRID_DISPLAY_FLAGS;const detailsFunc: TFRE_DB_SERVER_FUNC_DESC;const selectionDepFunc: TFRE_DB_SERVER_FUNC_DESC;const saveFunc: TFRE_DB_SERVER_FUNC_DESC;const dropFunc: TFRE_DB_SERVER_FUNC_DESC;const dragFunc: TFRE_DB_SERVER_FUNC_DESC): TFRE_DB_VIEW_LIST_DESC;
  begin
    Field('title').AsString:=title;
    Field('store').AsObject:=store;
    Field('layout').AsObject:=layout;
    if Assigned(itemContextMenuFunc) then begin
      Field('itemMenuFunc').AsObject:=itemContextMenuFunc.CloneToNewObject();
    end;
    Field('showSearch').AsBoolean:=cdgf_ShowSearchbox in displayFlags;
    Field('editable').AsBoolean:=cdgf_Editable in displayFlags;
    if Assigned(detailsFunc) then begin
      Field('detailsFunc').AsObject:=detailsFunc.CloneToNewObject();
    end;
    if Assigned(selectionDepFunc) then begin
      Field('selectionDepFunc').AsObject:=selectionDepFunc.CloneToNewObject();
    end;
    if Assigned(saveFunc) then begin
      Field('saveFunc').AsObject:=saveFunc.CloneToNewObject();
    end;
    if Assigned(dropFunc) then begin
      Field('dropFunc').AsObject:=dropFunc.CloneToNewObject();
    end;
    if Assigned(dragFunc) then begin
      Field('dragFunc').AsObject:=dragFunc.CloneToNewObject();
    end;
    Field('disableDrag').AsBoolean:=false;
    Field('children').AsBoolean:=cdgf_Children in displayFlags;
    Field('columnResize').AsBoolean:=cdgf_ColumnResizeable in displayFlags;
    Field('columnHide').AsBoolean:=cdgf_ColumnHideable in displayFlags;
    Field('columnDrag').AsBoolean:=cdgf_ColumnDragable in displayFlags;
    Field('multiselect').AsBoolean:=cdgf_Multiselect in displayFlags;
    Field('details').AsBoolean:=cdgf_Details in displayFlags;

    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    if not FieldExists('toolbarTop') then Field('toolbarTop').AsBoolean:=false;
    if not FieldExists('toolbarBottom') then Field('toolbarBottom').AsBoolean:=false;
    Result:=Self;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetTitle(const title: String);
  begin
    Field('title').AsString:=title;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetMenu(const menu: TFRE_DB_MENU_DESC);
  begin
    Field('menu').AsObject:=menu;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.AddFilterEvent(const filteredStoreId, refId: String);
  var
    obj  : IFRE_DB_Object;
    fsid : String;
  begin
    obj  := GFRE_DBI.NewObject;
    obj.Field('storeId').AsString := filteredStoreId;
    obj.Field('refId').AsString:=refId;
    Field('filteredStore').AddObject(obj);
  end;

    procedure TFRE_DB_VIEW_LIST_DESC.SetDropGrid(const grid: TFRE_DB_VIEW_LIST_DESC; const DnDClassesMultiple: TFRE_DB_StringArray; const DnDClassesSingle: TFRE_DB_StringArray);
  var
    i,j  : Integer;
    isnew: Boolean;
  begin
    Field('dragId').AsString:=Field('id').AsString+'_grid';
    grid.Field('dropId').AddString(Field('id').AsString+'_grid');
    if Assigned(DnDclassesMultiple) then begin
      for i := 0 to Length(DnDclassesMultiple) - 1 do begin
        isnew:=true;
        for j := 0 to grid.Field('dropClassesMultiple').ValueCount - 1 do begin
          if grid.Field('dropClassesMultiple').AsStringItem[j]=DnDclassesMultiple[i] then begin
            isnew:=false;
            break;
          end;
        end;
        if isnew then begin
          grid.Field('dropClassesMultiple').AddString(DnDclassesMultiple[i]);
        end;
      end;
    end;
    if Assigned(DnDClassesSingle) then begin
      for i := 0 to Length(DnDClassesSingle) - 1 do begin
        isnew:=true;
        for j := 0 to grid.Field('dropClassesSingle').ValueCount - 1 do begin
          if grid.Field('dropClassesSingle').AsStringItem[j]=DnDClassesSingle[i] then begin
            isnew:=false;
            break;
          end;
        end;
        if isnew then begin
          grid.Field('dropClassesSingle').AddString(DnDClassesSingle[i]);
        end;
      end;
    end;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetDragClasses(const DnDclasses: TFRE_DB_StringArray);
  begin
    Field('dragClasses').AsStringArr:=DnDclasses;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.AddEntryAction(const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const tooltip:String);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('serverFunc').AsObject:=serverFunc;
    if icon<>'' then begin
      obj.Field('icon').AsString:=FREDB_getThemedResource(icon);
    end;
    obj.Field('tooltip').AsString:=tooltip;
    Field('entryActions').AddObject(obj);
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetDependentContent(const contentFunc: TFRE_DB_SERVER_FUNC_DESC);
  begin
    Field('depContentFunc').AsObject:=contentFunc;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.disableDrag;
  begin
    Field('disableDrag').AsBoolean:=true;
  end;

  { TFRE_DB_FORM_DESC }

  function TFRE_DB_FORM_DESC.GetFormElement(const elementId: String): TFRE_DB_CONTENT_DESC;
  var
    i: Integer;
  begin
    Result:=nil;
    for i := 0 to Field('elements').ValueCount - 1 do begin
      if Field('elements').AsObjectItem[i].Field('field').AsString=elementId then begin
        Result:=Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_CONTENT_DESC;
        exit;
      end;
      if (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_BLOCK_DESC) or (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_GROUP_DESC) then begin
        Result:= (Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_FORM_DESC).GetFormElement(elementId);
        if Assigned(Result) then begin
          exit;
        end;
      end;
    end;
  end;

  function TFRE_DB_FORM_DESC.Describe(const caption: String; const defaultClose: Boolean; const sendChangedFieldsOnly: Boolean; const editable: Boolean; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC; const onChangeDelay: Integer; const hideEmptyGroups: Boolean): TFRE_DB_FORM_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('defaultClose').AsBoolean:=defaultClose;
    Field('sendChanged').AsBoolean:=sendChangedFieldsOnly;
    Field('editable').AsBoolean:=editable;
    Field('hideEmptyGroups').AsBoolean:=hideEmptyGroups;
    if Assigned(onChangeFunc) then begin
      Field('onChangeFunc').AsObject:=onChangeFunc;
      Field('onChangeDelay').AsInt64:=onChangeDelay;
    end;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_FORM_DESC._FillWithObjectValues(const obj: IFRE_DB_Object; const session: IFRE_DB_UserSession; const prefix:String);
  var
    i,j         : Integer;
    val         : String;
    objField    : IFRE_DB_FIELD;
    objFieldN   : TFRE_DB_NameType;
    store       : TFRE_DB_STORE_DESC;
    scheme      : IFRE_DB_SCHEMEOBJECT;
    fielddef    : IFRE_DB_FieldSchemeDefinition;
    FieldPathStr: String;

    procedure DoPreRender(const plugin : TFRE_DB_OBJECT_PLUGIN_BASE);
    begin
      if plugin.EnhancesFormRendering then
        plugin.RenderFormEntry(self,obj,true);
    end;

    procedure DoPostRender(const plugin : TFRE_DB_OBJECT_PLUGIN_BASE);
    begin
      if plugin.EnhancesFormRendering then
        plugin.RenderFormEntry(self,obj,false);
    end;


  begin
    scheme := obj.GetScheme(true);
    obj.ForAllPlugins(@DoPreRender);
    for i := 0 to Field('elements').ValueCount - 1 do begin
      if (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_BLOCK_DESC) or (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_GROUP_DESC) then
        begin
          (Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_FORM_DESC)._FillWithObjectValues(obj,session,prefix);
        end
      else begin
          if Field('elements').AsObjectItem[i].Field('confirms').AsString<>'' then begin
            FieldPathStr:=Field('elements').AsObjectItem[i].Field('confirms').AsString;
          end else begin
            FieldPathStr:=Field('elements').AsObjectItem[i].Field('field').AsString;
          end;
          if (prefix<>'') then begin
            if (pos(prefix,FieldPathStr)=1) then begin
              FieldPathStr:=Copy(FieldPathStr,Length(prefix)+1,MaxInt);
              objField:=obj.FieldPath(fieldPathStr,true);
            end else begin
              objField:=nil;
            end;
          end else begin
            objField:=obj.FieldPath(fieldPathStr,true);
          end;
          if Assigned(objField) then
            begin
              objFieldN := objField.FieldName;
              if (Field('elements').AsObjectItem[i].Implementor_HC  is TFRE_DB_INPUT_CHOOSER_DESC) and Field('elements').AsObjectItem[i].Field('cce').AsBoolean and (objField.FieldType=fdbft_Object) then
                begin
                  val  := Field('elements').AsObjectItem[i].Field('store').AsObject.field('id').AsString;
                 // val  := Field('elements').AsObjectItem[i].Field('store').AsString;
                  store:=GetStore(val);
                  val:=store.FindEntryByCaption(objField.AsObject.GetFormattedDisplay);
                  if val='' then begin
                    store.AddEntry.Describe(objField.AsObject.GetFormattedDisplay,'__deletedObjId__');
                    Field('elements').AsObjectItem[i].Field('defaultValue').AsString:='__deletedObjId__';
                  end else begin
                    Field('elements').AsObjectItem[i].Field('defaultValue').AsString:=val;
                  end;
                end
              else
                begin
                  if objField.ValueCount > 1 then begin
                    val:='';
                    for j := 0 to objField.ValueCount - 1 do begin
                      if j<>0 then begin
                        val:=val+'\n';
                      end;
                      val:=val+objField.AsStringItem[j];
                    end;
                  end else begin
                    if Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_DATE_DESC then begin
                      val:=IntToStr(objField.AsInt64);
                    end else
                      begin
                        if assigned(scheme)
                           and (scheme.GetSchemeField(objFieldN,fielddef)) then
                             begin
                               if fielddef.isPass then
                                 begin
                                   val := '*BAD*';
                                 end
                               else
                               if fielddef.FieldType = fdbft_Stream then
                                 begin {Session Url Encode the Stream field, automagically, if field is empty no URL should be generated / no filename...}
                                   if obj.FieldExists(objFieldN) then
                                     val := session.GetDownLoadLink4StreamField(obj.UID,objFieldN,false,obj.Field(objFieldN+cFRE_DB_STKEY).AsString,'',obj.Field(objFieldN+cFRE_DB_ST_ETAG).AsString)
                                   else
                                     val := '';
                                 end
                               else
                                 { Fallback }
                                 if fielddef.FieldType = fdbft_ObjLink then begin
                                   val:=FREDB_G2H(objField.AsObjectLink);
                                 end else begin
                                   val:=objField.AsString;
                                 end;
                             end
                        else
                          begin
                            if objField.FieldType = fdbft_ObjLink then begin
                              val:=FREDB_G2H(objField.AsObjectLink);
                            end else begin
                              val:=objField.AsString;
                            end;
                          end
                      end;
                  end;
                  Field('elements').AsObjectItem[i].Field('defaultValue').AsString:=val;
                end;
            end;
      end;
    end;
  end;

  procedure TFRE_DB_FORM_DESC.AddStore(const store: TFRE_DB_STORE_DESC);
  var
    i: Integer;
  begin
    for i := 0 to Field('stores').ValueCount - 1 do begin
      if Field('stores').AsObjectItem[i].Field('id').AsString=store.Field('id').AsString then exit;
    end;
    Field('stores').AddObject(store);
  end;

  procedure TFRE_DB_FORM_DESC.AddDBO(const id: String; const session: IFRE_DB_UserSession);
  var
    i : Integer;
  begin
    for i := 0 to Field('dbos').ValueCount - 1 do begin
      if Field('dbos').AsStringArr[i]=id then exit;
    end;
    Field('dbos').AddString(id);
    session.registerUpdatableDBO(FREDB_H2G(id));
  end;

  function TFRE_DB_FORM_DESC.GetStore(const id: String): TFRE_DB_STORE_DESC;
  var
    i: Integer;
  begin
    Result:=nil;
    for i := 0 to Field('stores').ValueCount - 1 do begin
      if Field('stores').AsObjectItem[i].Field('id').AsString=id then begin
        Result:=Field('stores').AsObjectItem[i].Implementor_HC as TFRE_DB_STORE_DESC;
        exit;
      end;
    end;
  end;

  function TFRE_DB_FORM_DESC.AddSchemeFormGroup(const schemeGroup: IFRE_DB_InputGroupSchemeDefinition; const session: IFRE_DB_UserSession; const collapsible: Boolean; const collapsed: Boolean; const groupPreFix: String; const groupRequired: Boolean; const hideGroupHeader: Boolean): TFRE_DB_INPUT_GROUP_DESC;
  var
    group         : TFRE_DB_INPUT_GROUP_DESC;
    obj           : IFRE_DB_Object;
    val           : String;
    cField        : IFRE_DB_OBJECT;

    function _getText(const key:TFRE_DB_String):TFRE_DB_String;
    begin
      Result:=session.GetDBConnection.FetchTranslateableTextShort(key);
    end;

    procedure _addInput(const obj:IFRE_DB_FieldDef4Group; const prefix:String; const requiredParent:Boolean);
    var
      coll               : IFRE_DB_COLLECTION;
      store              : TFRE_DB_STORE_DESC;
      required           : Boolean;
      enum               : IFRE_DB_Enum;
      enumVals           : IFRE_DB_ObjectArray;
      validator          : IFRE_DB_ClientFieldValidator;
      valParams          : IFRE_DB_Object;
      i                  : Integer;
      objArr             : IFRE_DB_ObjectArray;
      inputField         : TFRE_DB_FORM_INPUT_DESC;
      itext              : IFRE_DB_TEXT;
      dataCollectionName : TFRE_DB_NameType;
      dataCollIsDerived  : Boolean;
      standardColl       : TFRE_DB_STANDARD_COLL;
      chooserField       : TFRE_DB_INPUT_CHOOSER_DESC;
      domainEntries      : Integer;
      domainValue        : String;
      dcoll              : IFRE_DB_DERIVED_COLLECTION;

    procedure addObjects(const obj: IFRE_DB_Object);
    begin
      if (standardColl=coll_NONE) or
         not (obj.FieldExists('internal') and obj.Field('internal').AsBoolean) then
        store.AddEntry.Describe(obj.GetFormattedDisplay,obj.UID_String);
    end;

    procedure DepITerator(const df : R_Depfieldfield);
    begin
      inputField.AddDependence(prefix+df.depFieldName,df.disablesField);
    end;

    procedure EnumDepITerator(const vdf : R_EnumDepfieldfield);
    var
      validator       :IFRE_DB_ClientFieldValidator;
      validatorParams :IFRE_DB_Object;
    begin
      validator:=nil;
      validatorParams:=nil;
      if vdf.valKey<>'' then begin
        GFRE_DBI.GetSystemClientFieldValidator(vdf.valKey,validator);
        if Assigned(vdf.valParams) then begin
          validatorParams:=vdf.valParams;
        end;
      end;
      chooserField.addDependentInput(prefix+vdf.depFieldName,vdf.enumValue,vdf.visible,session.GetDBConnection.FetchTranslateableTextShort(vdf.capTransKey),validator,validatorParams);
    end;

    procedure _addDomain(const domain: IFRE_DB_Domain);
    begin
      if session.GetDBConnection.URT(false).CheckClassRight4Domain(obj.GetStdRight,obj.GetRightClass,domain.Domainname) then begin
        store.AddEntry.Describe(domain.Domainname,domain.Domainkey);
        domainEntries:=domainEntries+1;
        domainValue:=domain.Domainkey;
      end;
    end;

    begin
      required           := requiredParent and obj.GetRequired;
      dataCollectionName := obj.GetDatacollection;
      dataCollIsDerived  := obj.GetCollectionIsDerived;
      standardColl       := obj.GetStandardCollection;
      store              := nil;
      if (dataCollectionName<>'') or (standardColl<>coll_NONE) then begin
        if obj.GetHidden then begin
          inputField:=group.AddInput.Describe('',prefix+obj.GetfieldName,false,false,false,true,obj.GetDefault);
        end else begin
          case standardColl of
            coll_DOMAIN  : coll:=session.GetDBConnection.AdmGetDomainCollection;
            coll_GROUP   : coll:=session.GetDBConnection.AdmGetGroupCollection;
            coll_USER    : coll:=session.GetDBConnection.AdmGetUserCollection;
            coll_WFACTION: coll:=session.GetDBConnection.AdmGetWorkFlowMethCollection;
            coll_NONE    : begin
                             if dataCollIsDerived then begin
                               dcoll:=session.FetchDerivedCollection(dataCollectionName);
                               if not assigned(dcoll) then raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'The specified fieldbacking derived collection was not found : ['+dataCollectionName+']');
                               store:=dcoll.GetStoreDescription as TFRE_DB_STORE_DESC;
                             end else begin
                               coll := session.GetDBConnection.GetCollection(dataCollectionName);
                               if not assigned(coll) then raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'The specified fieldbacking datacollection was not found : ['+dataCollectionName+']');
                               store:=TFRE_DB_STORE_DESC.create.Describe();
                               coll.ForAll(@addObjects);
                             end;
                           end;
          end;
          chooserField:=group.AddChooser.Describe(_getText(obj.GetCaptionKey),prefix+obj.GetfieldName,store,obj.GetChooserType,required,obj.GetRequired,obj.GetChooserAddEmptyValue,obj.GetDisabled,obj.GetDefault);
          inputField:=chooserField;
          chooserField.captionCompareEnabled(true);
        end;
      end else begin
        if obj.GetRightClass<>'' then begin
          store:=TFRE_DB_STORE_DESC.create.Describe();
          domainEntries:=0;
          domainValue:='';
          session.GetDBConnection.SYS.ForAllDomains(@_addDomain);
          if obj.GetHideSingle and (domainEntries=1) then begin
            inputField:=group.AddInput.Describe('',prefix+obj.GetfieldName,false,false,false,true,domainValue);
          end else begin
            chooserField:=group.AddChooser.Describe(_getText(obj.GetCaptionKey),prefix+obj.GetfieldName,store,obj.GetChooserType,required,obj.GetRequired,obj.GetChooserAddEmptyValue,obj.GetDisabled,obj.GetDefault);
            inputField:=chooserField;
            obj.FieldSchemeDefinition.ForAllEnumDepfields(@EnumDepITerator);
          end;
        end else begin
          if obj.FieldSchemeDefinition.getEnum(enum) then begin
            if obj.GetHidden then begin
              inputField:=group.AddInput.Describe('',prefix+obj.GetfieldName,false,false,false,true,obj.GetDefault);
            end else begin
              store:=TFRE_DB_STORE_DESC.create.Describe();
              enumVals:=enum.getEntries;
              for i := 0 to Length(enumVals) - 1 do begin
                store.AddEntry.Describe(_getText(enumVals[i].Field('c').AsString),enumVals[i].Field('v').AsString);
              end;
              chooserField:=group.AddChooser.Describe(_getText(obj.GetCaptionKey),prefix+obj.GetfieldName,store,obj.GetChooserType,required,obj.GetRequired,obj.GetChooserAddEmptyValue,obj.GetDisabled,obj.GetDefault);
              inputField:=chooserField;
              obj.FieldSchemeDefinition.ForAllEnumDepfields(@EnumDepITerator);
            end;
          end else begin
            if obj.GetValidator(validator) then begin
              valParams:=obj.getValidatorParams;
            end else begin
              obj.FieldSchemeDefinition.getValidator(validator);
              valParams:=obj.FieldSchemeDefinition.getValidatorParams;
            end;

            case obj.FieldSchemeDefinition.FieldType of

              fdbft_UInt16,fdbft_UInt32,fdbft_UInt64,
              fdbft_Int16,fdbft_Int32,fdbft_Int64     :
                with obj do
                  inputField:=group.AddNumber.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,GetHidden,'',0);

              fdbft_Currency :
                with obj do
                  inputField:=group.AddNumber.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,Gethidden,'',2);

              fdbft_Real64 :
                 with obj do
                   inputField:=group.AddNumber.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,obj.GetDisabled,obj.GetHidden);

              fdbft_ObjLink,
              fdbft_String :
                with obj do
                  begin
                    inputField:=group.AddInput.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,GetHidden,GetDefault,validator,valParams,FieldSchemeDefinition.MultiValues,FieldSchemeDefinition.isPass);
                    if FieldSchemeDefinition.AddConfirm then
                         group.AddInput.Describe(_getText(FREDB_GetGlobalTextKey('input_confirm_prefix'))+' ' + _getText(GetCaptionKey),prefix+GetfieldName + '_confirm',required,GetRequired,
                                               GetDisabled,GetHidden,obj.GetDefault,validator,valParams,FieldSchemeDefinition.MultiValues,FieldSchemeDefinition.isPass,prefix+obj.GetfieldName);
                  end;
              fdbft_Boolean:
                with obj do
                  begin
                    inputField:=group.AddBool.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,false);
                  end;

              fdbft_DateTimeUTC:
                with obj do
                  inputField:=group.AddDate.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,GetHidden,'',validator,valParams);

              fdbft_Stream:
                with obj do
                  inputField:=group.AddFile.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,Gethidden,'',validator,valParams);
              else { String fallback }
                with obj do
                  inputField:=group.AddInput.Describe(_getText(GetCaptionKey),prefix+GetfieldName,required,GetRequired,GetDisabled,GetHidden,'',validator,valParams,FieldSchemeDefinition.multiValues,FieldSchemeDefinition.isPass);
            end;
          end;
        end;
      end;
      obj.FieldSchemeDefinition.ForAllDepfields(@DepITerator);
    end;

  procedure _addFields(const fields: IFRE_DB_FieldDef4GroupArr; const prefix:String; const groupPreFix:String; const groupRequired: Boolean);
  var
    i          : integer;
    scheme     : IFRE_DB_SchemeObject;
    newPrefix  : String;
    tmpGroup   : TFRE_DB_INPUT_GROUP_DESC;
    inputGroup : IFRE_DB_InputGroupSchemeDefinition;
    required   : Boolean;
    path       : TFOSStringArray;
    fieldDef   : IFRE_DB_FieldSchemeDefinition;
    inputPrefix: String;
  begin
    GFRE_BT.SeperateString(prefix,'.',path);
    required:=groupRequired;

    if Length(path)>0 then begin
      scheme:=schemeGroup.GetParentScheme;
      for i := 0 to Length(path) - 1 do begin
        if not scheme.GetSchemeField(path[i],fieldDef) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: '+path[i]);
        required:=required and fieldDef.required;
        if not GFRE_DBI.GetSystemSchemeByName(fieldDef.SubschemeName,scheme) then
          raise EFRE_DB_Exception.Create(edb_ERROR,'(A) cannot get scheme '+fieldDef.SubschemeName);
      end;
    end;

    inputPrefix := prefix;
    if groupPreFix<>'' then begin
      inputPrefix:=groupPreFix + '.' + inputPrefix;
    end;

    for i := 0 to Length(fields) - 1 do
      if fields[i].GetScheme<>'' then
        begin
          if not GFRE_DBI.GetSystemSchemeByName(fields[i].GetScheme,scheme) then
            raise EFRE_DB_Exception.Create(edb_ERROR,'(B) cannot get scheme '+fields[i].GetScheme);
          newPrefix:=fields[i].GetPrefix;
          if newPrefix<>'' then
            newPrefix:=newPrefix+'.';
          newPrefix:=prefix+newPrefix;
          if fields[i].GetType=igd_UsedSubGroup then
            begin
              tmpGroup   := group;
              inputGroup := scheme.GetInputGroup(fields[i].GetGroup);
              group:=tmpGroup.AddGroup.Describe(_getText(inputGroup.CaptionKey),fields[i].GetCollapsible,fields[i].GetCollapsed);
              _addFields(inputGroup.GroupFields,newPrefix,groupPreFix,required);
              group:=tmpGroup;
            end
          else
            begin
              inputGroup:=scheme.GetInputGroup(fields[i].GetGroup);
              _addFields(inputGroup.GroupFields,newPrefix,groupPreFix,required);
            end;
        end
      else
        begin
          _addInput(fields[i],inputPrefix,required);
        end;
  end;

  begin
    if hideGroupHeader then begin
      Result:=AddGroup.Describe('');
    end else begin
      Result:=AddGroup.Describe(_getText(schemeGroup.CaptionKey),collapsible,collapsed);
    end;
      group:=Result;
    _addFields(schemeGroup.GroupFields,'',groupPreFix,groupRequired);
  end;

  procedure TFRE_DB_FORM_DESC.SetElementValue(const elementId, value: String);
  var
    elem: TFRE_DB_CONTENT_DESC;
  begin
    elem:=GetFormElement(elementId);
    elem.Field('defaultValue').AsString:=value;
  end;

  procedure TFRE_DB_FORM_DESC.SetElementValueDisabled(const elementId, value: String);
  var
    elem: TFRE_DB_CONTENT_DESC;
  begin
    elem:=GetFormElement(elementId);
    elem.Field('defaultValue').AsString:=value;
    elem.Field('disabled').AsBoolean:=true;
  end;

  procedure TFRE_DB_FORM_DESC.SetElementDisabled(const elementId: String);
  var
    elem: TFRE_DB_CONTENT_DESC;
  begin
    elem:=GetFormElement(elementId);
    elem.Field('disabled').AsBoolean:=true;
  end;

  procedure TFRE_DB_FORM_DESC.FillWithObjectValues(const obj: IFRE_DB_Object; const session: IFRE_DB_UserSession; const groupPreFix:String);
  var
    prefix: String;
  begin
    AddDBO(obj.UID_String, session);
    if groupPreFix<>'' then begin
      prefix:=groupPreFix + '.';
    end else begin
      prefix:='';
    end;
    _FillWithObjectValues(obj,session,prefix);
  end;

  function TFRE_DB_FORM_DESC.AddInput: TFRE_DB_INPUT_DESC;
  begin
    Result := TFRE_DB_INPUT_DESC.create;
    Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddDescription: TFRE_DB_INPUT_DESCRIPTION_DESC;
  begin
    Result := TFRE_DB_INPUT_DESCRIPTION_DESC.create;
    Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddBool: TFRE_DB_INPUT_BOOL_DESC;
  begin
   Result := TFRE_DB_INPUT_BOOL_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddNumber: TFRE_DB_INPUT_NUMBER_DESC;
  begin
   Result := TFRE_DB_INPUT_NUMBER_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddChooser: TFRE_DB_INPUT_CHOOSER_DESC;
  begin
   Result := TFRE_DB_INPUT_CHOOSER_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddDate: TFRE_DB_INPUT_DATE_DESC;
  begin
   result := TFRE_DB_INPUT_DATE_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddRecurrence: TFRE_DB_INPUT_RECURRENCE_DESC;
  begin
    result := TFRE_DB_INPUT_RECURRENCE_DESC.create;
    Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddFile: TFRE_DB_INPUT_FILE_DESC;
  begin
   result := TFRE_DB_INPUT_FILE_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddGroup: TFRE_DB_INPUT_GROUP_DESC;
  begin
   result := TFRE_DB_INPUT_GROUP_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddGroupProxy: TFRE_DB_INPUT_GROUP_PROXY_DESC;
  begin
   result := TFRE_DB_INPUT_GROUP_PROXY_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddBlock: TFRE_DB_INPUT_BLOCK_DESC;
  begin
   result := TFRE_DB_INPUT_BLOCK_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddList: TFRE_DB_VIEW_LIST_DESC;
  begin
   result := TFRE_DB_VIEW_LIST_DESC.create;
   Field('elements').AddObject(Result);
  end;

  function TFRE_DB_FORM_DESC.AddButton: TFRE_DB_BUTTON_DESC;
  begin
   Result := TFRE_DB_BUTTON_DESC.create;
   Field('buttons').AddObject(Result);
  end;

  //function TFRE_DB_FORM_DESC.GetGroup(const id: String): TFRE_DB_INPUT_GROUP_DESC;
  //var
  //  i: Integer;
  //begin
  //  for i := 0 to Field('elements').ValueCount - 1 do begin
  //    if Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_GROUP_DESC then begin
  //      if Field('elements').AsObjectItem[i].Field('id').AsString=id then begin
  //        Result:=TFRE_DB_INPUT_GROUP_DESC(Field('elements').AsObjectItem[i]);
  //        exit;
  //      end;
  //    end;
  //  end;
  //  raise Exception.Create('Group not found in form : ['+id+']');
  //end;

  { TFRE_DB_INPUT_GROUP_DESC }

  function TFRE_DB_INPUT_GROUP_DESC._Describe(const caption: String; const collapsible, collapsed: Boolean): TFRE_DB_INPUT_GROUP_DESC;
  var
    i: Integer;
  begin
    Field('caption').AsString:=caption;
    Field('collapsible').AsBoolean:=collapsible;
    Field('collapsed').AsBoolean:=collapsed;
    Result:=Self;
  end;

  function TFRE_DB_INPUT_GROUP_DESC.Describe(const caption: String; const collapsible:Boolean=false;const collapsed:Boolean=false): TFRE_DB_INPUT_GROUP_DESC;
  begin
    Result:=_Describe(caption,collapsible,collapsed);
  end;

  procedure TFRE_DB_INPUT_GROUP_DESC.SetCaption(const caption: String);
  begin
    Field('caption').AsString:=caption;
  end;

  procedure TFRE_DB_INPUT_GROUP_DESC.AddStore(const store: TFRE_DB_STORE_DESC);
  var
    obj: IFRE_DB_Object;
  begin
    obj:= Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        (obj.Implementor_HC as TFRE_DB_FORM_DESC).AddStore(store);
      end else begin
        inherited AddStore(store);
      end;
    end else begin
      raise Exception.Create('Failed to add the store: Root Form not found!');
    end;
  end;

  procedure TFRE_DB_INPUT_GROUP_DESC.AddDBO(const id: String; const session: IFRE_DB_UserSession);
  var
    obj: IFRE_DB_Object;
  begin
    obj := Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        (obj.Implementor_HC as TFRE_DB_FORM_DESC).AddDBO(id, session);
      end else begin
        inherited AddDBO(id,session);
      end;
    end else begin
      raise Exception.Create('Failed to add the dbo: Root Form not found!');
    end;
  end;

  function TFRE_DB_INPUT_GROUP_DESC.GetStore(const id: String): TFRE_DB_STORE_DESC;
  var
    obj: IFRE_DB_Object;
  begin
    obj := Self.ObjectRoot;
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        (obj.Implementor_HC as TFRE_DB_FORM_DESC).GetStore(id);
      end else begin
        inherited GetStore(id);
      end;
    end else begin
      raise Exception.Create('Failed to get the store: Root Form not found!');
    end;
  end;

  procedure TFRE_DB_INPUT_GROUP_DESC.SetCollapseState(const collapsed: Boolean; const collapsible: Boolean);
  begin
    Field('collapsible').AsBoolean:=collapsible;
    Field('collapsed').AsBoolean:=collapsed;
  end;

  { TFRE_DB_FORM_PANEL_DESC }

  function TFRE_DB_FORM_PANEL_DESC.Describe(const caption: String;const sendChangedFieldsOnly: Boolean; const editable: Boolean; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC; const onChangeDelay:Integer; const hideEmptyGroups: Boolean): TFRE_DB_FORM_PANEL_DESC;
  begin
    inherited Describe(caption,false,sendChangedFieldsOnly,editable,onChangeFunc,onChangeDelay,hideEmptyGroups);
    Result:=Self;
  end;

  procedure TFRE_DB_FORM_PANEL_DESC.SetMenu(const menu: TFRE_DB_MENU_DESC);
  begin
    Field('menu').AsObject:=menu;
  end;

  { TFRE_DB_FORM_DIALOG_DESC }

  function TFRE_DB_FORM_DIALOG_DESC.Describe(const caption:String;const width:Integer; const defaultClose,isDraggable:Boolean;const sendChangedFieldsOnly: Boolean; const editable: Boolean; const onChangeFunc: TFRE_DB_SERVER_FUNC_DESC; const onChangeDelay:Integer; const hideEmptyGroups: Boolean; const styleClass: String): TFRE_DB_FORM_DIALOG_DESC;
  begin
    inherited Describe('',defaultClose,sendChangedFieldsOnly,editable,onChangeFunc,onChangeDelay,hideEmptyGroups);
    Field('dialogCaption').AsString:=caption;
    Field('width').AsInt16:=width;
    Field('draggable').AsBoolean:=isDraggable;
    Field('styleClass').AsString:=styleClass;
    Result:=Self;
  end;

  { TFRE_DB_DATA_ELEMENT_DESC }

  function TFRE_DB_DATA_ELEMENT_DESC._Describe(const id, caption: TFRE_DB_String; const displayType: TFRE_DB_DISPLAY_TYPE; const sortable: Boolean; const filterable: Boolean; const size: Integer; const display: Boolean; const editable: Boolean; const required: Boolean; const iconId: String; const openIconId: String; const filterValues: TFRE_DB_StringArray): TFRE_DB_DATA_ELEMENT_DESC;
  begin
   Field('id').AsString:=id;
   Field('caption').AsString:=caption;
   Field('displayType').AsString:=CFRE_DB_DISPLAY_TYPE[displayType];
   Field('display').AsBoolean:=display;
   Field('sortable').AsBoolean:=sortable;
   Field('filterable').AsBoolean:=filterable;
   Field('editable').AsBoolean:=editable;
   Field('required').AsBoolean:=required;
   Field('size').AsInt16:=size;
   if Assigned(filterValues) then begin
     Field('filterValues').AsStringArr:=filterValues;
   end;
   if iconId<>'' then begin
     Field('iconId').AsString:=iconId;
   end;
   if openIconId<>'' then begin
     Field('openIconId').AsString:=openIconId;
   end;
  end;

  function TFRE_DB_DATA_ELEMENT_DESC.Describe(const id, caption: TFRE_DB_String; const displayType: TFRE_DB_DISPLAY_TYPE; const sortable: Boolean; const filterable: Boolean; const size: Integer; const display: Boolean; const editable: Boolean; const required: Boolean; const iconId: String; const openIconId: String; const filterValues: TFRE_DB_StringArray): TFRE_DB_DATA_ELEMENT_DESC;
  begin
    if displayType=dt_number_pb then raise EFRE_DB_Exception.Create(edb_ERROR,'Please use DescribePB to configure a progress bar (dt_number_pb).');
    _Describe(id,caption,displayType,sortable,filterable,size,display,editable,required,iconId,openIconId,filterValues);
    Result:=Self;
  end;

  function TFRE_DB_DATA_ELEMENT_DESC.DescribePB(const id, caption: TFRE_DB_String; const labelId: string; const maxValue: Single; const sortable: Boolean; const filterable: Boolean; const size: Integer): TFRE_DB_DATA_ELEMENT_DESC;
  begin
    _Describe(id,caption,dt_number_pb,sortable,filterable,size,true,false,false,'','',nil);
    Field('labelId').AsString:=labelId;
    Field('maxValue').AsReal32:=maxValue;
    Result:=Self;
  end;

  procedure TFRE_DB_DATA_ELEMENT_DESC.setValueStore(const store: TFRE_DB_STORE_DESC);
  var
    obj: IFRE_DB_Object;
  begin
    obj := GFRE_DBI.NewObject;
    obj.Field('id').AsString:=store.Field('id').AsString;
    obj.Field('serverFuncExists').AsBoolean:=store.FieldExists('serverFunc');
    Field('store').AsObject:=obj;
    (Parent.Implementor_HC as TFRE_DB_VIEW_LIST_LAYOUT_DESC).AddStore(store);
  end;

  { TFRE_DB_VIEW_LIST_LAYOUT_DESC }

  procedure TFRE_DB_VIEW_LIST_LAYOUT_DESC.AddStore(const store: TFRE_DB_STORE_DESC);
  var
    i: Integer;
  begin
    for i := 0 to Field('stores').ValueCount - 1 do begin
      if Field('stores').AsObjectItem[i].Field('id').AsString=store.Field('id').AsString then exit;
    end;
    Field('stores').AddObject(store);
  end;

  function TFRE_DB_VIEW_LIST_LAYOUT_DESC.Describe(): TFRE_DB_VIEW_LIST_LAYOUT_DESC;
  begin
    Result:=Self;
  end;

  function TFRE_DB_VIEW_LIST_LAYOUT_DESC.AddDataElement: TFRE_DB_DATA_ELEMENT_DESC;
  begin
    Result := TFRE_DB_DATA_ELEMENT_DESC.create;
    Field('data').AddObject(Result);
  end;


  { TFRE_DB_LAYOUT_DESC }

  function TFRE_DB_LAYOUT_DESC.Describe(const useSizedSections:Boolean): TFRE_DB_LAYOUT_DESC;
  begin
    Field('useSizedSections').AsBoolean:=useSizedSections;
    Field('contentSize').AsInt16:=3;
    Field('sizeH').AsInt16:=3;
    Field('sizeV').AsInt16:=3;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_LAYOUT_DESC.AddSection(const section: TFRE_DB_CONTENT_DESC; const pos: TFRE_DB_LAYOUT_POS; const resizeable:Boolean; const size: Integer);
  var
    sec: IFRE_DB_Object;
  begin
    sec:=GFRE_DBI.NewObject;
    sec.Field('sectionDesc').AsObject:=section;
    if size=-1 then begin
      if pos=lt_center then begin
        sec.Field('size').AsInt16:=3;
      end else begin
        sec.Field('size').AsInt16:=1;
      end;
    end else begin
      sec.Field('size').AsInt16:=size;
    end;
    case pos of
      lt_center: begin
        Field('sizeH').AsInt16:=Field('sizeH').AsInt16-Field('contentSize').AsInt16+sec.Field('size').AsInt16;
        Field('sizeV').AsInt16:=Field('sizeV').AsInt16-Field('contentSize').AsInt16+sec.Field('size').AsInt16;
      end;
      lt_right,lt_left: begin
        Field('sizeH').AsInt16:=Field('sizeH').AsInt16+sec.Field('size').AsInt16;
      end;
      lt_top,lt_bottom: begin
        Field('sizeV').AsInt16:=Field('sizeV').AsInt16+sec.Field('size').AsInt16;
      end;
    end;
    sec.Field('resizeable').AsBoolean:=resizeable;
    Field(CFRE_DB_LAYOUT_POS[pos]).AsObject:=sec;
  end;

  procedure TFRE_DB_LAYOUT_DESC.AddFormDialog(const dialog: TFRE_DB_FORM_DIALOG_DESC);
  begin
    Field('formdialog').AsObject:=dialog;
  end;

  procedure TFRE_DB_LAYOUT_DESC.setContentSize(const size: Integer);
  begin
    Field('sizeH').AsInt16:=Field('sizeH').AsInt16-Field('contentSize').AsInt16+size;
    Field('sizeV').AsInt16:=Field('sizeV').AsInt16-Field('contentSize').AsInt16+size;
    Field('contentSize').AsInt16:=size;
  end;

    function TFRE_DB_LAYOUT_DESC.SetLayout(const leftSection, centerSection: TFRE_DB_CONTENT_DESC; const rightSection: TFRE_DB_CONTENT_DESC; const topSection: TFRE_DB_CONTENT_DESC; const bottomSection: TFRE_DB_CONTENT_DESC; const resizeable: boolean; const left_size: integer; const center_size: integer; const right_size: integer; const top_size: integer; const bottom_size: integer): TFRE_DB_LAYOUT_DESC;
  begin
    if Assigned(leftSection)   then AddSection(leftSection,lt_left,resizeable,left_size);
    if Assigned(centerSection) then begin
      AddSection(centerSection,lt_center,false,center_size);
    end else begin
      if center_size<>-1 then begin
        setContentSize(center_size);
      end;
    end;
    if Assigned(rightSection)  then AddSection(rightSection,lt_right,resizeable,right_size);
    if Assigned(topSection)    then AddSection(topSection,lt_top,resizeable,top_size);
    if Assigned(bottomSection) then AddSection(bottomSection,lt_bottom,resizeable,bottom_size);
    Field('useSizedSections').AsBoolean:=true;
    result := self;
 end;

  function TFRE_DB_LAYOUT_DESC.SetAutoSizedLayout(const leftSection, centerSection: TFRE_DB_CONTENT_DESC; const rightSection: TFRE_DB_CONTENT_DESC; const topSection: TFRE_DB_CONTENT_DESC; const bottomSection: TFRE_DB_CONTENT_DESC): TFRE_DB_LAYOUT_DESC;
  begin
    SetLayout(leftSection,centerSection,rightSection,topSection,bottomSection,false);
    Field('useSizedSections').AsBoolean:=false;
    Result:=Self;
  end;

  { TFRE_DB_EDITOR_DESC }

  function TFRE_DB_EDITOR_DESC.Describe(const loadFunc,saveFunc,startEditFunc,stopEditFunc: TFRE_DB_SERVER_FUNC_DESC; const contentType: TFRE_DB_CONTENT_TYPE; const toolbarBottom: Boolean): TFRE_DB_EDITOR_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('loadFunc').AsObject:=loadFunc;
    if Assigned(saveFunc) then begin
      Field('saveFunc').AsObject:=saveFunc;
    end;
    if Assigned(startEditFunc) then begin
      Field('startEditFunc').AsObject:=startEditFunc;
    end;
    if Assigned(stopEditFunc) then begin
      Field('stopEditFunc').AsObject:=stopEditFunc;
    end;
    Field('contentType').AsString:=CFRE_DB_CONTENT_TYPE[contentType];
    Field('tbBottom').AsBoolean:=toolbarBottom;
    Result:=Self;
  end;


initialization

end.

