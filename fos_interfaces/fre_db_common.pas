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
  Classes, SysUtils,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES;

const
  cDEBUG_FLAG = true;
var
  cSTYLE:String = 'firmos';

type

  TFRE_DB_TRANSFORM_TYPE      = (fdbtt_post2json, fdbtt_get2html,fdbtt_WebSocket);
  TFRE_DB_CHOOSER_DH          = (dh_chooser_radio,dh_chooser_check,dh_chooser_combo);
  TFRE_DB_LAYOUT_POS          = (lt_left,lt_center,lt_right,lt_top,lt_bottom);
  TFRE_DB_SUBSEC_DISPLAY_TYPE = (sec_dt_tab,sec_dt_vertical,sec_dt_hiddentab);
  TFRE_DB_CLIENT_ACTION       = (fdbca_openContent);
  TFRE_DB_BUTTON_TYPE         = (fdbbt_submit,fdbbt_button,fdbbt_close);
  TFRE_DB_GRID_BUTTON_DEP     = (fdgbd_single,fdgbd_multi,fdgbd_always,fdgbd_manual);
  TFRE_DB_CONTENT_TYPE        = (ct_html,ct_javascript,ct_pascal);
  TFRE_DB_REC_INTERVAL_TYPE   = (rit_once,rit_minute,rit_hour,rit_day,rit_week,rit_month,rit_quarter,rit_year);
  TFRE_DB_REC_INTERVAL_TYPES  = set of TFRE_DB_REC_INTERVAL_TYPE;

  TFRE_DB_TRANSFORM_FUNCTION  =   procedure(const session:TFRE_DB_UserSession;const command_type:TFRE_DB_COMMANDTYPE;const result_intf:IFRE_DB_Object;var rawContent:TFRE_DB_RawByteString;var lContentType:string; const isInnerContent:Boolean=false; const TransformType: TFRE_DB_TRANSFORM_TYPE=fdbtt_post2json);


const
  CFRE_DB_CHOOSER_DH           : array [TFRE_DB_CHOOSER_DH] of string          = ('dh_chooser_radio','dh_chooser_check','dh_chooser_combo');
  CFRE_DB_LAYOUT_POS           : array [TFRE_DB_LAYOUT_POS] of string          = ('lt_left','lt_center','lt_right','lt_top','lt_bottom');
  CFRE_DB_SUBSEC_DISPLAY_TYPE  : array [TFRE_DB_SUBSEC_DISPLAY_TYPE] of string = ('sec_dt_tab','sec_dt_vertical','sec_dt_hiddentab');
  CFRE_DB_BUTTON_TYPE          : array [TFRE_DB_BUTTON_TYPE] of string         = ('bt_submit','bt_button','bt_close');
  CFRE_DB_GRID_BUTTON_DEP      : array [TFRE_DB_GRID_BUTTON_DEP] of string     = ('gbd_single','gbd_multi','gbd_always','gbd_manual');
  CFRE_DB_CHART_TYPE           : array [TFRE_DB_CHART_TYPE] of string          = ('ct_pie','ct_column','ct_line');
  CFRE_DB_LIVE_CHART_TYPE      : array [TFRE_DB_LIVE_CHART_TYPE] of string     = ('lct_line','lct_sampledline');
  CFRE_DB_CONTENT_TYPE         : array [TFRE_DB_CONTENT_TYPE] of string        = ('ct_html','ct_javascript','ct_pascal');

  //TODO -> RENAME TO FREDB_* and move
  function  StringInArray                     (const src:string;const arr:TFRE_DB_StringArray):boolean;
  procedure ConcatStringArrays                (var TargetArr:TFRE_DB_StringArray;const add_array:TFRE_DB_StringArray);
  procedure ConcatGuidArrays                  (var TargetArr:TFRE_DB_GuidArray;const add_array:TFRE_DB_GuidArray);
  function  GuidInArray                       (const check:TGuid;const arr:TFRE_DB_GUIDArray):boolean;
  function  FindNthGuidIdx                    (n:integer;const guid:TGuid;const arr:TFRE_DB_GUIDArray):integer;inline;
  function  CheckAllStringFieldsEmptyInObject (const obj:IFRE_DB_Object):boolean;
  function  CalcFieldResultKey                (const field_type:TFRE_DB_FIELDTYPE):String;
  function  getThemedResource                 (const id: String):String;

type
  { TFRE_DB_HTML_DESC }

  TFRE_DB_HTML_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a HTML content.
    function  Describe   (const html:String; const height:Integer=-1; const width:Integer=-1; const border:Boolean=false): TFRE_DB_HTML_DESC;
  end;


  { TFRE_DB_SET_BUTTON_STATE_DESC }

  TFRE_DB_SET_BUTTON_STATE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a refresh action. E.g. after an add opertion.
    function  Describe       (const buttonId:String; const disabled:Boolean): TFRE_DB_SET_BUTTON_STATE_DESC;
  end;

  { TFRE_DB_RESTORE_UI_DESC }

  TFRE_DB_RESTORE_UI_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a client side function which should be called.
    //@ baseContainerId: Start point of the sectionIds path. Can be a section description or a layout description or 'FirmOSViewport' to start at the top.
    function  Describe       (const baseContainerId: String; const sectionIds: TFRE_DB_StringArray): TFRE_DB_RESTORE_UI_DESC;
  end;

  { TFRE_DB_MENU_ENTRY_DESC }

  TFRE_DB_MENU_ENTRY_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a menu entry. See also TFRE_DB_MENU_DESC and TFRE_DB_SUBMENU_DESC.
    //@ After the execution of the server function the defined refresh is executed. E.g. an add operation on a grid will define fdbrt_direct to refresh the grid.
    //@ fdbrt_dependent refreshes all filtered stores. See TFRE_DB_VIEW_LIST_DESC.addFilterEvent.
    //@ Only implemented for grids and trees.
    function  Describe  (const caption,icon:String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const disabled:Boolean=false):TFRE_DB_MENU_ENTRY_DESC;
  end;

  TFRE_DB_SUBMENU_DESC = class;
  { TFRE_DB_MENU_DESC }

  TFRE_DB_MENU_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a menu.
    function  Describe   : TFRE_DB_MENU_DESC;
    //@ Creates a new menu entry description and adds it.
    function  AddEntry   : TFRE_DB_MENU_ENTRY_DESC;
    //@ Creates a new sub menu description and adds it.
    function  AddMenu    : TFRE_DB_SUBMENU_DESC;
  end;

  { TFRE_DB_SUBMENU }

  TFRE_DB_SUBMENU_DESC    = class(TFRE_DB_MENU_DESC)
  public
    //@ Describes a sub menu. See TFRE_DB_MENU_DESC.
    function  Describe  (const caption,icon: String; const disabled:Boolean=false):TFRE_DB_SUBMENU_DESC;
  end;

  { TFRE_DB_SVG_DEF_ELEM_ATTR_DESC }

  TFRE_DB_SVG_DEF_ELEM_ATTR_DESC    = class(TFRE_DB_CONTENT_DESC)
    //@ Describes an attribute of a SVG definitions element.
    function  Describe  (const name,value:String): TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  end;

  { TFRE_DB_SVG_DEF_ELEM_DESC }

  TFRE_DB_SVG_DEF_ELEM_DESC    = class(TFRE_DB_CONTENT_DESC)
    //@ Describes a SVG definitions element. E.g. linearGradient.
    function  Describe    (const tagName:String): TFRE_DB_SVG_DEF_ELEM_DESC;
    //@ Adds a sub element. E.g. a 'stop' element to a 'linearGradiant'
    function AddElement   :TFRE_DB_SVG_DEF_ELEM_DESC;
    //@ Adds an attribute.
    function AddAttribute :TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  end;
  TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY = array of TFRE_DB_SVG_DEF_ELEM_DESC;

  { TFRE_DB_SITEMAP_ENTRY_DESC }

  TFRE_DB_SITEMAP_ENTRY_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a sitemap entry.
    //@ Top level entries are arranged automatically. Therefore parameters x and y are ignored for top level entries.
    function  Describe  (const caption,icon:String; const restoreUIFunc: TFRE_DB_RESTORE_UI_DESC; const x,y:Integer; const id:String=''; const newsCount:Integer=0;  const disabled:Boolean=false; const scale:Single=1): TFRE_DB_SITEMAP_ENTRY_DESC;
    //@ Creates a new sitemap entry description and adds it.
    function  AddEntry  : TFRE_DB_SITEMAP_ENTRY_DESC;
  end;

  { TFRE_DB_SITEMAP_DESC }

  TFRE_DB_SITEMAP_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a sitemap/structure of the application.
    function  Describe  (const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY=nil): TFRE_DB_SITEMAP_DESC;
    //@ Creates a new sitemap entry description and adds it.
    function  AddEntry  : TFRE_DB_SITEMAP_ENTRY_DESC;
  end;

  { TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC }

  TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an info update of a sitemap entry.
    function  Describe  (const entryPath: TFRE_DB_StringArray; const newsCount:Integer): TFRE_DB_UPDATE_SITEMAP_ENTRY_INFO_DESC;
  end;

  { TFRE_DB_FORM_INPUT_DESC }

  //@ Base class for form input elements.
  //@ Do NOT use! Use a derived class instead.
  //@ E.g. TFRE_DB_INPUT_DESC instead.
  TFRE_DB_FORM_INPUT_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    //@ Used internally through inheritance.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil ; const validatorConfigParams : IFRE_DB_Object=nil) : TFRE_DB_FORM_INPUT_DESC;virtual;
  end;

  TFRE_DB_STORE_DESC = class;

  { TFRE_DB_DATA_ELEMENT_DESC }

  TFRE_DB_DATA_ELEMENT_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    function   _Describe     (const id,caption: string; const displayType: TFRE_DB_DISPLAY_TYPE; const size: Integer; const display: Boolean; const required: Boolean; const iconId:String):TFRE_DB_DATA_ELEMENT_DESC;
  public
    //@ Describes an entry of a collection view.
    //@ FIXXME: required parameter not implemented yet.
    function   Describe      (const id,caption: string; const displayType: TFRE_DB_DISPLAY_TYPE=dt_string; const size: Integer=1; const display: Boolean=true; const required: Boolean=false; const iconId:String=''):TFRE_DB_DATA_ELEMENT_DESC;
    //@ Describes a 'progressbar' entry.
    //@ If labelId is given the value of this field will be used as label of the progressbar otherwise
    //@ the value (id field) will be used as label followed by a percent sign.
    function   DescribePB    (const id,caption: string; const labelId: string=''; const maxValue: Single=100; const size: Integer=1; const required: Boolean=false):TFRE_DB_DATA_ELEMENT_DESC;
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
    function  Describe            (const idField:String='uid'; const serverFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const labelFields:TFRE_DB_StringArray=nil; const destroyFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const clearQueryIdFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const id:String=''; const pageSize:Integer=25): TFRE_DB_STORE_DESC; overload;

    //@ Creates a new entry and adds it to the store. E.g. used for choosers.
    function  AddEntry            : TFRE_DB_STORE_ENTRY_DESC;
  end;

  { TFRE_DB_INPUT_DESC }

  TFRE_DB_INPUT_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a text input field within a form.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil; const multiValues: Boolean=false; const isPass:Boolean=false) : TFRE_DB_INPUT_DESC;
  end;

  { TFRE_DB_INPUT_DESCRIPTION_DESC }

  TFRE_DB_INPUT_DESCRIPTION_DESC   = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an info field within a form.
    function  Describe (const caption,description: String) : TFRE_DB_INPUT_DESCRIPTION_DESC;
  end;

  { TFRE_DB_DEPENDENCE_DESC }

  TFRE_DB_DEPENDENCE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a dependent field of a boolean field.
    //@ Used as parameter of TFRE_DB_INPUT_BOOL_DESC.AddDependence.
    function Describe (const fieldName: String; const disablesField: Boolean=true): TFRE_DB_DEPENDENCE_DESC;
  end;

  { TFRE_DB_INPUT_BOOL_DESC }

  TFRE_DB_INPUT_BOOL_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a boolean input field within a form.
    function  Describe(const caption, field_reference:string; const required: boolean=false; const groupRequired: Boolean=false; const disabled: boolean=false; const defaultValue:Boolean=false):TFRE_DB_INPUT_BOOL_DESC;
    //@ Adds a dependent field.
    //@ See TFRE_DB_DEPENDENCE_DESC.
    procedure AddDependence(const depField: TFRE_DB_DEPENDENCE_DESC);
  end;

  { TFRE_DB_INPUT_NUMBER_DESC }

  TFRE_DB_INPUT_NUMBER_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
    //@ Describes a number input within a form.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false;  const disabled: boolean = false;const hidden:Boolean=false; const defaultValue:String=''; const digits: Integer=-1) : TFRE_DB_INPUT_NUMBER_DESC;
  end;

  { TFRE_DB_INPUT_CHOOSER_DESC }

  TFRE_DB_INPUT_CHOOSER_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a chooser within a form.
    //@ FIXXME: display type dh_chooser_check not implemented yet.
    //@ FIXXME: single_select=false not implemented yet.
    function  Describe             (const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const single_select:Boolean=true; const display_hint:TFRE_DB_CHOOSER_DH=dh_chooser_combo;
                                    const required: boolean=false; const groupRequired: Boolean=false; const disabled: boolean=false; const defaultValue:String=''): TFRE_DB_INPUT_CHOOSER_DESC;
    //@ FIXXME: not implemented yet.
    procedure addFilterEvent       (const filteredStoreId,refId:String);
    //@ Enables the caption compare.
    //@ Useful for fields which store the caption and not a link to the object.
    //@ Default is false.
    procedure captionCompareEnabled(const enabled:Boolean);
  end;

  { TFRE_DB_INPUT_DATE_DESC }

  TFRE_DB_INPUT_DATE_DESC   = class(TFRE_DB_FORM_INPUT_DESC)
  public
    //@ Describes a date input within a form.
    function  Describe (const caption,field_reference : String; const required: Boolean=false; const groupRequired: Boolean=false; const disabled: boolean = false;const hidden:Boolean=false;
                        const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil ; const validatorConfigParams : IFRE_DB_Object=nil) : TFRE_DB_INPUT_DATE_DESC;
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
                        const defaultValue:String=''; const validator: IFRE_DB_ClientFieldValidator=nil; const multiValues: Boolean=false) : TFRE_DB_INPUT_FILE_DESC;
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
                                   const displayFlags:TFRE_COLLECTION_GRID_DISPLAY_FLAGS=[cdgf_ShowSearchbox,cdgf_Editable,cdgf_Filter,cdgf_Sortable];
                                   const detailsFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const selectionDepFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const saveFunc:TFRE_DB_SERVER_FUNC_DESC=nil;
                                   const dropFunc: TFRE_DB_SERVER_FUNC_DESC=nil; const dragFunc: TFRE_DB_SERVER_FUNC_DESC=nil): TFRE_DB_VIEW_LIST_DESC;
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
    procedure SetDropGrid         (const grid:TFRE_DB_VIEW_LIST_DESC; const objectclassesMultiple:TFRE_DB_StringArray=nil; const objectclassesSingle:TFRE_DB_StringArray=nil);
    //@ Limits the dragable objects to those matching one of the given objectclasses.
    //@ Data entry _disabledrag_ set to true can be used to disable drag explicitly for objects.
    //@ Ignored if drag and drop is not enabled. See function SetDropGrid.
    procedure SetDragObjClasses   (const objectclasses:TFRE_DB_StringArray);
    //@ Adds an entry to the action column of the list.
    //@ FIXXME: Not implemented yet.
    procedure AddEntryAction      (const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const tooltip:String='');
    //@ Defines a server function which will be called on selection change of the list view to retrieve the dependent content.
    //@ The selected ids will be passed as selected parameter.
    //@ FIXXME: Not implemented yet.
    procedure SetDependentContent (const contentFunc:TFRE_DB_SERVER_FUNC_DESC);
  end;

  { TFRE_DB_VIEW_TREE_DESC }

  TFRE_DB_VIEW_TREE_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a tree view representation of the given store.
    //@ Menu:
    //@   * Each entry has to have an uidPath or an uid defined.
    //@   * First: If the entry has a funcClassname and a menuFunc defined funcClassname.menuFunc will be called.
    //@   * Second: itemContextMenuFunc will be called.
    function Describe (const store: TFRE_DB_STORE_DESC; const title:String=''; const itemContextMenuFunc:TFRE_DB_SERVER_FUNC_DESC=nil; const contextMenuFunc:TFRE_DB_SERVER_FUNC_DESC=nil): TFRE_DB_VIEW_TREE_DESC;
  end;

  TFRE_DB_INPUT_GROUP_DESC       = class;
  TFRE_DB_INPUT_GROUP_PROXY_DESC = class;
  TFRE_DB_INPUT_BLOCK_DESC       = class;

  { TFRE_DB_VALIDATOR_DESC }

  TFRE_DB_VALIDATOR_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a validator
    //@ Do NOT use! Used internally.
    function Describe(const id,regExp,helpText: String; const allowedChars:String=''; const configParams: IFRE_DB_Object=nil): TFRE_DB_VALIDATOR_DESC;
  end;

  { TFRE_DB_BUTTON_DESC }

  TFRE_DB_BUTTON_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a button for a form panel or a dialog.
    //@ fdbbt_close type is only useful for dialogs.
    //@ For fdbbt_close type the serverFunc might be nil (Just closes the dialog).
    function Describe(const caption: String;const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const buttonType: TFRE_DB_BUTTON_TYPE): TFRE_DB_BUTTON_DESC;
  end;

  { TFRE_DB_FORM_DESC }

  //@ Base class form panels and dialogs.
  //@ Do NOT use! Use TFRE_DB_FORM_PANEL_DESC or TFRE_DB_DIALOG_DESC instead.
  TFRE_DB_FORM_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function  GetFormElement       (const elementId:String): TFRE_DB_CONTENT_DESC;
    procedure AddStore             (const store: TFRE_DB_STORE_DESC);virtual;
    procedure AddDBO               (const id: String; const session: TFRE_DB_UserSession);virtual;
    function  GetStore             (const id:String): TFRE_DB_STORE_DESC;virtual;
    function  Describe             (const caption:String;const defaultClose:Boolean;const sendChangedFieldsOnly: Boolean; const editable: Boolean=true): TFRE_DB_FORM_DESC;
    procedure _FillWithObjectValues(const obj: IFRE_DB_Object);
  public
    //@ Sets the value of the input element with the given id.
    procedure SetElementValue      (const elementId, value:String);
    //@ Fills the form with the values of the given object.
    procedure FillWithObjectValues (const obj: IFRE_DB_Object; const session: TFRE_DB_UserSession);
    //@ Adds the given InputGroupSchemeDefinition to the form and returns the TFRE_DB_INPUT_GROUP_DESC.
    //@ See TFRE_DB_INPUT_GROUP_DESC.
    function  AddSchemeFormGroup   (const schemeGroup: IFRE_DB_InputGroupSchemeDefinition ; const session : TFRE_DB_UserSession; const collapsible: Boolean=false; const collapsed: Boolean=false): TFRE_DB_INPUT_GROUP_DESC;
    //@ Creates a new input field and adds it to the form. See also TFRE_DB_INPUT_DESC.
    function  AddInput             : TFRE_DB_INPUT_DESC;
    //@ Creates a new description and adds it to the form. See also TFRE_DB_INPUT_DESCRIPTION_DESC.
    function  AddDescription       : TFRE_DB_INPUT_DESCRIPTION_DESC;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddBool              : TFRE_DB_INPUT_BOOL_DESC;
    //@ Creates a new boolean field and adds it to the form. See also TFRE_DB_INPUT_BOOL_DESC.
    function  AddNumber            : TFRE_DB_INPUT_NUMBER_DESC;
    //@ Creates a new chooser and adds it to the form. See also TFRE_DB_INPUT_CHOOSER_DESC.
    function  AddChooser           : TFRE_DB_INPUT_CHOOSER_DESC;
    //@ Creates a new date field and adds it to the form. See also TFRE_DB_INPUT_DATE_DESC.
    function  AddDate              : TFRE_DB_INPUT_DATE_DESC;
    //@ Creates a new recurrence field and adds it to the form. See also TFRE_DB_INPUT_RECURRENCE_DESC.
    function  AddRecurrence        : TFRE_DB_INPUT_RECURRENCE_DESC;
    //@ Creates a new file field and adds it to the form. See also TFRE_DB_INPUT_FILE_DESC.
    function  AddFile              : TFRE_DB_INPUT_FILE_DESC;
    //@ Creates a new input group and adds it to the form. See also TFRE_DB_INPUT_GROUP_DESC.
    function  AddGroup             : TFRE_DB_INPUT_GROUP_DESC;
    //@ Creates a new input proxy group and adds it to the form. See also TFRE_DB_INPUT_GROUP_PROXY_DESC.
    function  AddGroupProxy        : TFRE_DB_INPUT_GROUP_PROXY_DESC;
    //@ Creates a new input block and adds it to the form. See also TFRE_DB_INPUT_BLOCK_DESC.
    function  AddBlock             : TFRE_DB_INPUT_BLOCK_DESC;
    //@ Creates a new grid and adds it to the form. See also TFRE_DB_VIEW_LIST_DESC.
    function  AddList              : TFRE_DB_VIEW_LIST_DESC;
    //@ Creates a new button and adds it to the form. See also TFRE_DB_BUTTON_DESC.
    function  AddButton            : TFRE_DB_BUTTON_DESC;
    //@ Creates a new input field and adds it to the form. See also TFRE_DB_INPUT_DESC.
    //function  GetGroup             (const id:String): TFRE_DB_INPUT_GROUP_DESC;
  end;

  { TFRE_DB_UPDATE_FORM_DBO_DESC }

  TFRE_DB_UPDATE_FORM_DBO_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an update of a form dbo.
    function  Describe        (const updateObj:IFRE_DB_Object):TFRE_DB_UPDATE_FORM_DBO_DESC;
  end;

  { TFRE_DB_INPUT_GROUP_DESC }

  TFRE_DB_INPUT_GROUP_DESC  = class(TFRE_DB_FORM_DESC)
  protected
    function  _Describe        (const caption:String;const collapsible,collapsed: Boolean):TFRE_DB_INPUT_GROUP_DESC;
    procedure AddStore         (const store: TFRE_DB_STORE_DESC);override;
    procedure AddDBO           (const id: String; const session: TFRE_DB_UserSession);override;
    function  GetStore         (const id: String):TFRE_DB_STORE_DESC;override;
  public
    //@ Describes an input group within a form.
    //@ Collapsed will be ignored if collapsible is false.
    function  Describe         (const caption:String='';const collapsible:Boolean=false;const collapsed:Boolean=false):TFRE_DB_INPUT_GROUP_DESC;
    //@ Sets the collapse state of the input group.
    //@ Useful in case the group was added with TFRE_DB_FORM_DESC.AddSchemeFormGroup.
    procedure SetCollapseState (const collapsed: Boolean=false; const collapsible: Boolean=true);
  end;

  { TFRE_DB_INPUT_BLOCK_DESC }

  TFRE_DB_INPUT_BLOCK_DESC  = class(TFRE_DB_FORM_DESC)
  private
    procedure AddStore    (const store: TFRE_DB_STORE_DESC);override;
    procedure AddDBO      (const id: String; const session: TFRE_DB_UserSession);override;
    function  GetStore    (const id: String):TFRE_DB_STORE_DESC;override;
  public
    //@ Describes an horizontal input block within a form (e.g. Favourite 3 colours: input input input).
    function  Describe    (const caption:String=''):TFRE_DB_INPUT_BLOCK_DESC;
    //@ Adds the given InputGroupSchemeDefinition to the form and returns the TFRE_DB_INPUT_GROUP_DESC.
    //@ See TFRE_DB_INPUT_GROUP_DESC.
    function  AddSchemeFormGroup   (const schemeGroup: IFRE_DB_InputGroupSchemeDefinition ; const session : TFRE_DB_UserSession; const collapsible: Boolean=false; const collapsed: Boolean=false; const relSize:Integer=1): TFRE_DB_INPUT_GROUP_DESC; reintroduce;
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
    function Describe (const caption:String;const sendChangedFieldsOnly: Boolean=true; const editable: Boolean=true): TFRE_DB_FORM_PANEL_DESC;
    //@ Sets the menu of the form panel. Will be displayed like a file menu in a desktop application.
    procedure SetMenu (const menu: TFRE_DB_MENU_DESC);
  end;

  { TFRE_DB_DIALOG_DESC }

  TFRE_DB_DIALOG_DESC    = class(TFRE_DB_FORM_DESC)
  public
    //@ Describes a modal dialog. See also TFRE_DB_FORM_DESC.
    //@ If defaultClose is true a close button will be added to the dialog which simply closes the dialog.
    //@ If defaultClose is false and no explicit close button is added the dialog will not be closable at all (e.g. force login).
    //@ sendChangedFieldsOnly true: good for data updates, false: all field values are send unconditonally, goot for new objects
    function  Describe    (const caption:String; const width:Integer=0; const maxHeight:Integer=0; const defaultClose:Boolean=true; const isDraggable:Boolean=true;const sendChangedFieldsOnly: Boolean=true; const editable: Boolean=true): TFRE_DB_DIALOG_DESC;
    //@ Adds a html header section to the dialog.
    //@ FIXXME: not implemented yet.
    procedure AddHeader   (const header: TFRE_DB_HTML_DESC);
    procedure SendChangedFieldsOnly  (const value: boolean);
  end;

  { TFRE_DB_TOPMENU_ENTRY_DESC }

  TFRE_DB_TOPMENU_ENTRY_DESC  = class(TFRE_DB_CONTENT_DESC)
  private
    function  _Describe (const caption,icon:String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC;const sectionId:String;const big:Boolean; const active:Boolean):TFRE_DB_TOPMENU_ENTRY_DESC;
  public
    //@ Describes a top menu entry. See also TFRE_DB_TOPMENU_DESC.
    function  Describe  (const caption,icon:String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC;const sectionId:String='';const big:Boolean=false; const active:Boolean=false):TFRE_DB_TOPMENU_ENTRY_DESC;
    //@ Describes a top menu entry with multiple content. See also TFRE_DB_TOPMENU_DESC.
    //@ sectionId defines the id of the automatic generated section and subSectionsIds the ids of the given contents.
    //@ Describe('Caption','icons/icon.png','icons/iconH.png',serverFuncs,'home',TFRE_DB_StringArray.create('menu','app')); => ['home','menu'] defines the path to the menu content and ['home','app'] defines the path to the app content
    function  Describe  (const caption,icon:String; const serverFuncs: TFRE_DB_SERVER_FUNC_DESC_ARRAY;const sectionId:String=''; const subSectionIds:TFRE_DB_StringArray=nil;const big:Boolean=false;const active:Boolean=false):TFRE_DB_TOPMENU_ENTRY_DESC;
  end;

  { TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC }

  TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC  = class(TFRE_DB_CONTENT_DESC)
    //@ Describes a top menu dialog entry. See also TFRE_DB_TOPMENU_DESC.
    function  Describe  (const caption,icon:String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC;const big:Boolean=false):TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC;
  end;

  { TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC }

  TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC  = class(TFRE_DB_CONTENT_DESC)
    //@ Describes a top menu dialog entry. See also TFRE_DB_TOPMENU_DESC.
    function  Describe  (const caption,icon:String; const big:Boolean=false):TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC;
  end;

  { TFRE_DB_TOPMENU_DESC }

  TFRE_DB_TOPMENU_DESC  = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a top menu.
    function  Describe      (): TFRE_DB_TOPMENU_DESC;
    //@ Creates a new top menu entry description and adds it.
    function  AddEntry      : TFRE_DB_TOPMENU_ENTRY_DESC;
    //@ Creates a new top menu dialog entry description and adds it.
    function  AddDialogEntry: TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC;
    //@ Creates a new top menu jira dialog entry description and adds it.
    //@ Jira integration has to be enabled within the TFRE_DB_MAIN_DESC.
    function  AddJiraDialogEntry: TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC;
    //@ Adds a dialog to the top menu which will be opened.
    //@ See TFRE_DB_DIALOG_DESC.
    procedure AddDialog     (const dialog:TFRE_DB_DIALOG_DESC);
  end;

  { TFRE_DB_SECTION_DESC }

  TFRE_DB_SECTION_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    function  _Describe             (const title :String; const ord:Int16; const sectionId:String; const size:Integer): TFRE_DB_SECTION_DESC;
  public
    //@ Describes a single section of the subsections description.
    //@ The contentFunc is used to get the content description of the section.
    //@ The ord parameter can be used to sort the sections.
    function  Describe             (const contentFunc:TFRE_DB_SERVER_FUNC_DESC;const title :String; const ord:Int16; const sectionId:String=''; const size:Integer=-1): TFRE_DB_SECTION_DESC;
    //@ Used by the framework.
    //@ DO NO USE.
    function  _internalDescribe  (const content:TFRE_DB_CONTENT_DESC;const title :String; const ord:Int16; const sectionId:String=''; const size:Integer=-1): TFRE_DB_SECTION_DESC;
    //@ Sets the section as the active one.
    //@ Only usefull in case of display type sec_dt_tab
    procedure SetActive            (const active: Boolean);
    //@ Sets the menu of the section. Will be displayed like a file menu in a desktop application.
    procedure SetMenu              (const menu: TFRE_DB_MENU_DESC);
  end;

  { TFRE_DB_SUBSECTIONS_DESC }

  TFRE_DB_SUBSECTIONS_DESC = class(TFRE_DB_CONTENT_DESC)
  private
    fnoActiveSectionSet: Boolean;
    factiveSectionOrd  : Integer;
    procedure SetActiveSectionUID (const sectionUID: String);
    procedure SectionDescribed    (const sectionUID: String; const ord,size: Integer);
  public
    constructor Create     ;
    //@ Describes a collection of content which is displayed either as tabs or vertical arranged with resize handlers.
    function  Describe        (const displayType: TFRE_DB_SUBSEC_DISPLAY_TYPE=sec_dt_tab): TFRE_DB_SUBSECTIONS_DESC;
    //@ The serverFunction will be called on an ui change. In this case tab change if the display type is sec_dt_tab.
    //@ FIXXME - finish implementation.
    procedure OnUIChange      (const serverFunc: TFRE_DB_SERVER_FUNC_DESC);
    //@ Creates a new section and adds it.
    //@ The size parameter is only useful for the display type sec_dt_vertical.
    function  AddSection       : TFRE_DB_SECTION_DESC;
    //@ Set the section with the given id as active section.
    procedure SetActiveSection (const sectionId: String);
  end;

  { TFRE_DB_LAYOUT_DESC }

  TFRE_DB_LAYOUT_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a layout.
    //@ If useSizedSections is switched off each section except the center section has to have a fixed size e.g. a html header section.
    function  Describe          (const title:String=''; const useSizedSections:Boolean=true): TFRE_DB_LAYOUT_DESC;
    //@ Adds the given section at the position 'pos'. With the size parameter the relative size of the content can be configured.
    //@ Default values for size are: 3 for center section and 1 otherwise.
    //@ Parameter resizeable will be ignored for the center section because it is resizeable if any other section is resizeable.
    procedure AddSection        (const section:TFRE_DB_CONTENT_DESC; const pos: TFRE_DB_LAYOUT_POS; const resizeable: Boolean=true; const size: Integer=-1);    //deprecated, don't use
    //@ Adds a dialog to the layout which will be opened.
    //@ See TFRE_DB_DIALOG_DESC.
    procedure AddDialog         (const dialog:TFRE_DB_DIALOG_DESC);
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
    function Describe        (const loadFunc,saveFunc,startEditFunc,stopEditFunc: TFRE_DB_SERVER_FUNC_DESC; const contentType: TFRE_DB_CONTENT_TYPE=ct_html): TFRE_DB_EDITOR_DESC;
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

  { TFRE_DB_STORE_DATA_DESC }

  TFRE_DB_STORE_DATA_DESC  = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the result of store data request. (e.g. grid, tree, chart...)
    function  Describe (const totalCount: Int32): TFRE_DB_STORE_DATA_DESC;
    //@ Adds an entry to the result.
    procedure addEntry (const entry: IFRE_DB_Object);
  end;

  { TFRE_DB_UPDATE_STORE_DESC }

  TFRE_DB_UPDATE_STORE_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes an update of a store.
    function  Describe        (const storeId:String):TFRE_DB_UPDATE_STORE_DESC;
    //@ Adds an updated entry.
    procedure addUpdatedEntry (const entry: IFRE_DB_Object);
    //@ Adds the id of a deleted entry.
    procedure addDeletedEntry (const entryId: String);
    //@ Adds a new entry. If reverenceItemId = '' the new Item will be added to all open queries (should only be used if the new item is the first one in this store).
    //@ parentId is only useful for tree grids. If not parentId is given the new item is added as root item.
    procedure addNewEntry     (const entry: IFRE_DB_Object; const reverenceItemId: String; const parentId: String='';  const revItemIsPrevious: Boolean=true);
    //@ Sets the new total count.
    procedure setTotalCount   (const count: Integer);
  end;

  { TFRE_DB_INVALIDATE_SESSION_DATA_DESC }

  TFRE_DB_INVALIDATE_SESSION_DATA_DESC = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes invalid session data.
    function  Describe        ():TFRE_DB_INVALIDATE_SESSION_DATA_DESC;
    //@ Adds an the invalid queries of a store.
    //@ If queryIds is nil all queries of the store are invalid.
    procedure addStoreQueries (const storeId: String; const queryIds: TFRE_DB_StringArray=nil);
  end;

  { TFRE_DB_CHART_DESC }

  TFRE_DB_CHART_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes a chart.
    function Describe        (const caption:String; const store:TFRE_DB_STORE_DESC; const seriesIds: TFRE_DB_StringArray; const seriesType: TFRE_DB_CHART_TYPE;const seriesLabels: TFRE_DB_StringArray=nil; const showLegend: Boolean=false; const maxValue: Integer=0): TFRE_DB_CHART_DESC;
  end;

  { TFRE_DB_CHART_DATA_DESC }

  TFRE_DB_CHART_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the data of a chart.
    function  Describe        (): TFRE_DB_CHART_DATA_DESC;
    //@ Add the whole data of a series. E.g. all y values of a certain line.
    procedure setSeries       (const data: TFRE_DB_Real32Array; const uids: TFRE_DB_GUIDArray=nil; const colors: TFRE_DB_StringArray=nil; const texts: TFRE_DB_StringArray=nil; const legend: TFRE_DB_StringArray=nil);
  end;

  { TFRE_DB_LIVE_CHART_DESC }

  TFRE_DB_LIVE_CHART_DESC    = class(TFRE_DB_CONTENT_DESC)
  private
    function _Describe           (const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin,yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray;const legendLabels: TFRE_DB_StringArray;
                                  const yTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC;const displayDuration: Integer; const updateInterval: Integer; const buffer: Integer; const seriesType: TFRE_DB_LIVE_CHART_TYPE): TFRE_DB_LIVE_CHART_DESC;
  public
    //@ Describes a live chart.
    //@ id: id of the chart. Needed by the data feeder to address the chart.
    //@ seriesCount: number of series in the chart
    //@ stopStartCB: server function called on start and stop. Parameter action will have the value "start" or "stop".
    //@ yMin, yMax: limits the y-axis
    //@ yTickHint: axis will get approximately 'yTickHint' ticks
    //@ initData: server function called to get the initial data of the live chart. Has to hold displayDuration / updateInterval + buffer + 1 data elements.
    //@ displayDuration: in milliseconds
    //@ updateInterval: in milliseconds
    //@ buffer: number of data elements to hold as buffer
    function Describe            (const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin,yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray=nil;const legendLabels: TFRE_DB_StringArray=nil;
                                  const yTickHint: Integer=10; const initData: TFRE_DB_SERVER_FUNC_DESC=nil;const displayDuration: Integer=120000; const updateInterval: Integer=1000; const buffer: Integer=1; const seriesType: TFRE_DB_LIVE_CHART_TYPE=fdblct_line): TFRE_DB_LIVE_CHART_DESC;

    //@ Describes a sampled line live chart.
    //@ displayDuration: in milliseconds
    //@ samplingInterval: in milliseconds
    //@ displayDuration / samplingInterval values have to be provided for each line
    function DescribeSampledLine (const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin,yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray=nil;const legendLabels: TFRE_DB_StringArray=nil;
                                  const yTickHint: Integer=10; const displayDuration: Integer=120000; const samplingInterval: Integer=1000): TFRE_DB_LIVE_CHART_DESC;
  end;

  { TFRE_DB_LIVE_CHART_DATA_DESC }

  TFRE_DB_LIVE_CHART_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the data of a live chart at index "idx".
    //@ id: id of the chart.
    //@ dataIndex: index of the data since the last start action
    function Describe        (const id: String; const dataIndex: Integer; const data: TFRE_DB_Real32Array): TFRE_DB_LIVE_CHART_DATA_DESC;
  end;

  TFRE_DB_LIVE_CHART_SAMPLED_DATA_ARRAY = array of TFRE_DB_Real32Array;

  { TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC }

  TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the data of a sampled live chart.
    //@ id: id of the chart.
    function Describe        (const id: String; const data: TFRE_DB_LIVE_CHART_SAMPLED_DATA_ARRAY): TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC;
  end;

  TFRE_DB_LIVE_CHART_INIT_DATA_ARRAY = array of TFRE_DB_Real32Array;

  { TFRE_DB_LIVE_CHART_INIT_DATA_DESC }

  TFRE_DB_LIVE_CHART_INIT_DATA_DESC    = class(TFRE_DB_CONTENT_DESC)
  public
    //@ Describes the initial data of a live chart.
    function Describe        (const data: TFRE_DB_LIVE_CHART_INIT_DATA_ARRAY): TFRE_DB_LIVE_CHART_INIT_DATA_DESC;
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
    function  Describe                (const style: String; const jiraIntegrationJSURL: String=''; const debug: Boolean=false): TFRE_DB_MAIN_DESC;
  end;

  function String2DBChooserDH(const fts: string): TFRE_DB_CHOOSER_DH;
  function String2DBLayoutPos(const fts: string): TFRE_DB_LAYOUT_POS;
  function String2DBSubSecDisplayType(const fts: string): TFRE_DB_SUBSEC_DISPLAY_TYPE;
  function String2DBMessageType(const fts: string): TFRE_DB_MESSAGE_TYPE;
  function String2DBButtonType(const fts: string): TFRE_DB_BUTTON_TYPE;
  function String2DBGridButtonDep(const fts: string): TFRE_DB_GRID_BUTTON_DEP;
  function String2DBContent(const fts: string): TFRE_DB_CONTENT_TYPE;

implementation

  function _getRootObj(const obj:IFRE_DB_Object): IFRE_DB_Object;
  var
    current   : IFRE_DB_Object;
  begin
   current:=obj;
   while Assigned(current.Parent) do begin
     current:=current.Parent;
   end;
   Result:=current;
  end;

  function StringInArray(const src: string; const arr: TFRE_DB_StringArray): boolean;
  var  i: Integer;
  begin
    result := false;
    for i:=0 to High(arr) do begin
      if src=arr[i] then exit(true);
    end;
  end;

  procedure ConcatStringArrays(var TargetArr: TFRE_DB_StringArray; const add_array: TFRE_DB_StringArray);
  var i,len_target,high_add_array,cnt :integer;
  begin
    len_target     := Length(TargetArr);
    SetLength(TargetArr,len_target+Length(add_array));
    high_add_array := high(add_array);
    cnt := 0;
    for i:= 0 to high_add_array do begin
      if not StringInArray(add_array[i],TargetArr) then begin
        TargetArr[len_target+cnt] := add_array[i]; inc(cnt);
      end;
    end;
    SetLength(TargetArr,len_target+cnt);
  end;

  procedure ConcatGuidArrays(var TargetArr: TFRE_DB_GuidArray; const add_array: TFRE_DB_GuidArray);
  var i,len_target,high_add_array,cnt :integer;
   begin
     len_target     := Length(TargetArr);
     SetLength(TargetArr,len_target+Length(add_array));
     high_add_array := high(add_array);
     cnt := 0;
     for i:= 0 to high_add_array do begin
       if not GuidInArray(add_array[i],TargetArr) then begin
         TargetArr[len_target+cnt] := add_array[i]; inc(cnt);
       end;
     end;
     SetLength(TargetArr,len_target+cnt);
   end;

  function GuidInArray(const check: TGuid; const arr: TFRE_DB_GUIDArray): boolean;
  var  i: Integer;
  begin
    result := false;
    for i:=0 to High(arr) do begin
      if FREDB_Guids_Same(check,arr[i]) then exit(true);
    end;
  end;


  function FindNthGuidIdx(n: integer; const guid: TGuid; const arr: TFRE_DB_GUIDArray): integer;
  var i: Integer;
  begin
    result:=-1;
    if n<=0 then raise EFRE_DB_Exception.Create(edb_ERROR,'must specify a positive integer greater than zero');
    for i:=0 to high(arr) do begin
      if FREDB_Guids_Same(guid,arr[i]) then begin
        dec(n);
        if n=0 then exit(i);
      end;
    end;
  end;

  function CheckAllStringFieldsEmptyInObject(const obj: IFRE_DB_Object): boolean;
  var check:boolean;
    function CheckFunc(const field:IFRE_DB_FIELD):boolean;
    begin
      result := false;
      if field.IsUIDField then exit;
      //writeln('checking field ',field.FieldName);
      if field.FieldType=fdbft_Object then begin
        //writeln('checking recurse on ',field.FieldName);
        check  := CheckAllStringFieldsEmptyInObject(field.AsObject);
        result := not check;
      end else begin
        if field.FieldType<>fdbft_String then raise EFRE_DB_Exception.Create(edb_ERROR,'checkempty only works on stringfiled-only objects');
        if field.AsString<>'' then begin
          result:=true;
          check:=false;
        end;
      end;
    end;
  begin
    //writeln('Checking empty');
    Check:=true;
    obj.ForAllFieldsBreak(@CheckFunc);
    result := check;
  end;

  function CalcFieldResultKey(const field_type: TFRE_DB_FIELDTYPE): String;
  begin
    result:='$CR_'+CFRE_DB_FIELDTYPE_SHORT[field_type];
  end;

  function getThemedResource(const id: String): String;
  begin
    Result:='/fre_css/'+cSTYLE+'/'+id;
  end;

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

  { TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC }

  function TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC.Describe(const id: String; const data: TFRE_DB_LIVE_CHART_SAMPLED_DATA_ARRAY): TFRE_DB_LIVE_CHART_SAMPLED_DATA_DESC;
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

  { TFRE_DB_UPDATE_FORM_DBO_DESC }

  function TFRE_DB_UPDATE_FORM_DBO_DESC.Describe(const updateObj: IFRE_DB_Object): TFRE_DB_UPDATE_FORM_DBO_DESC;
  begin
    Field('obj').AsObject:=updateObj;
    Result:=Self;
  end;

  { TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC }

  function TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC.Describe(const caption,icon: String; const big: Boolean): TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('caption').AsString:=caption;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('big').AsBoolean:=big;
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

  { TFRE_DB_LIVE_CHART_DATA_DESC }

  function TFRE_DB_LIVE_CHART_DATA_DESC.Describe(const id: String; const dataIndex: Integer; const data: TFRE_DB_Real32Array): TFRE_DB_LIVE_CHART_DATA_DESC;
  begin
    Field('id').AsString:=id;
    Field('dataIndex').AsInt64:=dataIndex;
    Field('data').AsReal32Arr:=data;
    Result:=Self;
  end;

  { TFRE_DB_LIVE_CHART_INIT_DATA_DESC }

  function TFRE_DB_LIVE_CHART_INIT_DATA_DESC.Describe(const data: TFRE_DB_LIVE_CHART_INIT_DATA_ARRAY): TFRE_DB_LIVE_CHART_INIT_DATA_DESC;
  var
    i: Integer;
  begin
    Field('dataCount').AsInt16:=Length(data);
    for i := 0 to Length(data) - 1 do begin
      Field('data'+IntToStr(i)).AsReal32Arr:=data[i];
    end;
    Result:=Self;
  end;

  { TFRE_DB_SET_BUTTON_STATE_DESC }

  function TFRE_DB_SET_BUTTON_STATE_DESC.Describe(const buttonId: String; const disabled: Boolean): TFRE_DB_SET_BUTTON_STATE_DESC;
  begin
    Field('id').AsString:=buttonId;
    Field('disabled').AsBoolean:=disabled;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_FILE_DESC }

  function TFRE_DB_INPUT_FILE_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator; const multiValues: Boolean): TFRE_DB_INPUT_FILE_DESC;
  begin
    if Assigned(validator) and multiValues and (validator.ObjectName='image') then begin
      raise EFRE_DB_Exception.Create(edb_ERROR,'Image input is not allowed to have multiple values');
    end;
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator);
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

  { TFRE_DB_SVG_DEF_ELEM_ATTR_DESC }

  function TFRE_DB_SVG_DEF_ELEM_ATTR_DESC.Describe(const name, value: String): TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  begin
    Field('name').AsString:=name;
    Field('value').AsString:=value;
    Result:=Self;
  end;

  { TFRE_DB_SVG_DEF_ELEM_DESC }

  function TFRE_DB_SVG_DEF_ELEM_DESC.Describe(const tagName: String): TFRE_DB_SVG_DEF_ELEM_DESC;
  begin
    Field('tagname').AsString:=tagName;
    Result:=Self;
  end;

  function TFRE_DB_SVG_DEF_ELEM_DESC.AddElement: TFRE_DB_SVG_DEF_ELEM_DESC;
  begin
    Result:=TFRE_DB_SVG_DEF_ELEM_DESC.create;
    Field('elems').AddObject(Result);
  end;

  function TFRE_DB_SVG_DEF_ELEM_DESC.AddAttribute: TFRE_DB_SVG_DEF_ELEM_ATTR_DESC;
  begin
    Result:=TFRE_DB_SVG_DEF_ELEM_ATTR_DESC.create;
    Field('attrs').AddObject(Result);
  end;

  { TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC }

  function TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC.Describe(const caption, icon: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const big: Boolean): TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('caption').AsString:=caption;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('serverFunc').AsObject:=serverFunc.CloneToNewObject();
    Field('big').AsBoolean:=big;
    Result:=Self;
  end;

  { TFRE_DB_INVALIDATE_SESSION_DATA_DESC }

  function TFRE_DB_INVALIDATE_SESSION_DATA_DESC.Describe: TFRE_DB_INVALIDATE_SESSION_DATA_DESC;
  begin
    Result:=Self;
  end;

  procedure TFRE_DB_INVALIDATE_SESSION_DATA_DESC.addStoreQueries(const storeId: String; const queryIds: TFRE_DB_StringArray);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('storeid').AsString:=storeId;
    if Assigned(queryIds) then begin
      obj.Field('queryids').AsStringArr:=queryIds;
    end;
    Field('stores').AddObject(obj);
  end;

  { TFRE_DB_RESTORE_UI_DESC }

  function TFRE_DB_RESTORE_UI_DESC.Describe(const baseContainerId: String; const sectionIds: TFRE_DB_StringArray): TFRE_DB_RESTORE_UI_DESC;
  begin
    Field('baseContainerId').AsString:=baseContainerId;
    Field('sectionIds').AsStringArr:=sectionIds;
    Result:=Self;
  end;

  { TFRE_DB_SITEMAP_ENTRY_DESC }

  function TFRE_DB_SITEMAP_ENTRY_DESC.Describe(const caption, icon: String; const restoreUIFunc: TFRE_DB_RESTORE_UI_DESC; const x, y: Integer; const id:String; const newsCount:Integer; const disabled: Boolean; const scale:Single): TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    Field('caption').AsString:=caption;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('sectionpath').AsObject := restoreUIFunc;
    Field('x').AsInt32:=x;
    Field('y').AsInt32:=y;
    Field('id').AsString:=id;
    Field('newscount').AsInt16:=newsCount;
    Field('disabled').AsBoolean:=disabled;
    Field('scale').AsReal32:=scale;
    Result:=Self;
  end;

  function TFRE_DB_SITEMAP_ENTRY_DESC.AddEntry: TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    Result:=TFRE_DB_SITEMAP_ENTRY_DESC.create;
    Field('entries').AddObject(Result);
  end;

  { TFRE_DB_SITEMAP_DESC }

  function TFRE_DB_SITEMAP_DESC.Describe(const svgDefs: TFRE_DB_SVG_DEF_ELEM_DESC_ARRAY=nil): TFRE_DB_SITEMAP_DESC;
  var
    i: Integer;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    if Assigned(svgDefs) then begin
      for i := 0 to Length(svgDefs) - 1 do begin
        Field('svgDefs').AddObject(svgDefs[i]);
      end;
    end;
    Result:=Self;
  end;

  function TFRE_DB_SITEMAP_DESC.AddEntry: TFRE_DB_SITEMAP_ENTRY_DESC;
  begin
    Result:=TFRE_DB_SITEMAP_ENTRY_DESC.create;
    Field('entries').AddObject(Result);
  end;

  { TFRE_DB_TOPMENU_ENTRY_DESC }

    function TFRE_DB_TOPMENU_ENTRY_DESC._Describe(const caption, icon: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const sectionId: String; const big: Boolean; const active: Boolean): TFRE_DB_TOPMENU_ENTRY_DESC;
  begin
    if sectionId<>'' then begin
      Field('id').AsString:=sectionId;
    end else begin
      if not FieldExists('id') then begin
        Field('id').AsString:='id'+UID_String;
      end;
    end;
    Field('caption').AsString:=caption;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('serverFuncs').AddObject(serverFunc);
    Field('big').AsBoolean:=big;
    Field('active').AsBoolean:=active;
    Result:=Self;
  end;

  function TFRE_DB_TOPMENU_ENTRY_DESC.Describe(const caption, icon: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const sectionId: String;const big,active:Boolean): TFRE_DB_TOPMENU_ENTRY_DESC;
  begin
    Result:=_Describe(caption,icon,serverFunc,sectionId,big,active);
  end;

  function TFRE_DB_TOPMENU_ENTRY_DESC.Describe(const caption, icon: String; const serverFuncs: TFRE_DB_SERVER_FUNC_DESC_ARRAY;const sectionId:String; const subSectionIds:TFRE_DB_StringArray;const big,active: Boolean): TFRE_DB_TOPMENU_ENTRY_DESC;
  var
    i: Integer;
  begin
   _Describe(caption,icon,serverFuncs[0],sectionId,big,active);
    if Assigned(subSectionIds) then begin
      for i := 0 to Length(subSectionIds) - 1 do begin
        if subSectionIds[i]='' then begin
          Field('subIds').AddString('id'+UID_String+IntToStr(i));
        end else begin
          Field('subIds').AddString(subSectionIds[i]);
        end;
      end;
      for i := Length(subSectionIds) to Length(serverFuncs) - 1 do begin
        Field('subIds').AddString('id'+UID_String+IntToStr(i));
      end;
    end else begin
      for i := 0 to Length(serverFuncs) - 1 do begin
        Field('subIds').AddString('id'+UID_String+IntToStr(i));
      end;
    end;
    for i := 1 to Length(serverFuncs) - 1 do begin
      Field('serverFuncs').AddObject(serverFuncs[i]);
    end;
    Result:=Self;
  end;

  { TFRE_DB_TOPMENU_DESC }

  function TFRE_DB_TOPMENU_DESC.Describe(): TFRE_DB_TOPMENU_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  function TFRE_DB_TOPMENU_DESC.AddEntry: TFRE_DB_TOPMENU_ENTRY_DESC;
  begin
    Result := TFRE_DB_TOPMENU_ENTRY_DESC.Create;
    Field('entries').AddObject(Result);
  end;

  function TFRE_DB_TOPMENU_DESC.AddDialogEntry: TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC;
  begin
    Result := TFRE_DB_TOPMENU_DIALOG_ENTRY_DESC.create;
    Field('entries').AddObject(Result);
  end;

  function TFRE_DB_TOPMENU_DESC.AddJiraDialogEntry: TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC;
  begin
    Result := TFRE_DB_TOPMENU_JIRA_DIALOG_ENTRY_DESC.create;
    Field('entries').AddObject(Result);
  end;

  procedure TFRE_DB_TOPMENU_DESC.AddDialog(const dialog: TFRE_DB_DIALOG_DESC);
  begin
    Field('dialog').AsObject:=dialog;
  end;

  { TFRE_DB_CHART_DATA_DESC }

  function TFRE_DB_CHART_DATA_DESC.Describe: TFRE_DB_CHART_DATA_DESC;
  begin
    Result:=Self;
  end;

  procedure TFRE_DB_CHART_DATA_DESC.setSeries(const data: TFRE_DB_Real32Array; const uids: TFRE_DB_GUIDArray; const colors: TFRE_DB_StringArray; const texts: TFRE_DB_StringArray; const legend: TFRE_DB_StringArray);
  var
    obj: IFRE_DB_Object;
    i  : Integer;
  begin
    if assigned(uids) and (Length(data)<>Length(uids)) then raise EFRE_DB_Exception.Create(edb_ERROR,'guid array and data array not the same length');
    if assigned(colors) and (Length(data)<>Length(colors)) then raise EFRE_DB_Exception.Create(edb_ERROR,'color array and data array not the same length');
    if assigned(texts) and (Length(data)<>Length(texts)) then raise EFRE_DB_Exception.Create(edb_ERROR,'text array and data array not the same length');
    if assigned(legend) and (Length(data)<>Length(legend)) then raise EFRE_DB_Exception.Create(edb_ERROR,'legend array and data array not the same length');
    for i := 0 to Length(data) - 1 do begin
      obj:=GFRE_DBI.NewObject;
      obj.Field('value').AsReal32:=data[i];
      if assigned(uids) then begin
        obj.Field('uid').AsGUID:=uids[i];
      end;
      if assigned(colors) then begin
        obj.Field('color').AsString:=colors[i];
      end;
      if assigned(texts) then begin
        obj.Field('text').AsString:=texts[i];
      end;
      if assigned(legend) then begin
        obj.Field('legend').AsString:=legend[i];
      end;
      Field('series').AddObject(obj);
    end;
  end;

  { TFRE_DB_CHART_DESC }

  function TFRE_DB_CHART_DESC.Describe(const caption: String; const store: TFRE_DB_STORE_DESC; const seriesIds: TFRE_DB_StringArray; const seriesType: TFRE_DB_CHART_TYPE; const seriesLabels: TFRE_DB_StringArray; const showLegend: Boolean; const maxValue: Integer): TFRE_DB_CHART_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('caption').AsString:=caption;
    Field('store').AsObject:=store;
    Field('seriesIds').AsStringArr:=seriesIds;
    Field('type').AsString:=CFRE_DB_CHART_TYPE[seriesType];
    Field('showLegend').AsBoolean:=showLegend;
    Field('maxValue').AsInt32:=maxValue;
    if Assigned(seriesLabels) then begin
      Field('seriesLabels').AsStringArr:=seriesLabels;
    end;
    Result:=Self;
  end;

  { TFRE_DB_LIVE_CHART_DESC }

  function TFRE_DB_LIVE_CHART_DESC._Describe(const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin, yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const yTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC; const displayDuration: Integer; const updateInterval: Integer; const buffer: Integer;  const seriesType: TFRE_DB_LIVE_CHART_TYPE): TFRE_DB_LIVE_CHART_DESC;
  begin
    Field('id').AsString:=id;
    Field('seriesCount').AsInt16:=seriesCount;
    Field('serverFunc').AsObject:=stopStartCB;
    Field('type').AsString:=CFRE_DB_LIVE_CHART_TYPE[seriesType];
    Field('yMin').AsReal32:=yMin;
    Field('yMax').AsReal32:=yMax;
    Field('caption').AsString:=caption;
    Field('yTickHint').AsInt16:=yTickHint;
    Field('displayDuration').AsInt64:=displayDuration;
    Field('updateInterval').AsInt16:=updateInterval;
    Field('buffer').AsInt16:=buffer;
    if Assigned(seriesColor) then begin
      Field('seriesColor').AsStringArr:=seriesColor;
    end;
    if Assigned(legendLabels) then begin
      Field('legendLabels').AsStringArr:=legendLabels;
    end;
    if Assigned(initData) then begin
      Field('initDataFunc').AsObject:=initData;
    end;
    Result:=Self;
  end;

  function TFRE_DB_LIVE_CHART_DESC.Describe(const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin, yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const yTickHint: Integer; const initData: TFRE_DB_SERVER_FUNC_DESC; const displayDuration: Integer; const updateInterval: Integer; const buffer: Integer; const seriesType: TFRE_DB_LIVE_CHART_TYPE): TFRE_DB_LIVE_CHART_DESC;  begin
    if seriesType=fdblct_sampledline then raise EFRE_DB_Exception.Create(edb_ERROR,'Use DescribeSampledLine to describe a live chart of type fdblct_sampledline.');
    Result:=_Describe(id,seriesCount,stopStartCB,yMin,yMax,caption,seriesColor,legendLabels,yTickHint,initData,displayDuration,updateInterval,buffer,seriesType);
  end;

  function TFRE_DB_LIVE_CHART_DESC.DescribeSampledLine(const id: String; const seriesCount: Integer; const stopStartCB: TFRE_DB_SERVER_FUNC_DESC; const yMin, yMax: Real; const caption: String; const seriesColor: TFRE_DB_StringArray; const legendLabels: TFRE_DB_StringArray; const yTickHint: Integer; const displayDuration: Integer; const samplingInterval: Integer): TFRE_DB_LIVE_CHART_DESC;
  begin
    Result:=_Describe(id,seriesCount,stopStartCB,yMin,yMax,caption,seriesColor,legendLabels,yTickHint,nil,displayDuration,samplingInterval,0,fdblct_sampledline);
  end;

  { TFRE_DB_INPUT_DATE_DESC }

  function TFRE_DB_INPUT_DATE_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator ; const validatorConfigParams : IFRE_DB_Object): TFRE_DB_INPUT_DATE_DESC;
  begin
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator);
    Result:=Self;
  end;

  { TFRE_DB_UPDATE_STORE_DESC }

  function TFRE_DB_UPDATE_STORE_DESC.Describe(const storeId: String): TFRE_DB_UPDATE_STORE_DESC;
  begin
    Field('storeId').AsString:=storeId;
    Result:=Self;
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addUpdatedEntry(const entry: IFRE_DB_Object);
  begin
    Field('updated').AddObject(entry.CloneToNewObject());
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addDeletedEntry(const entryId: String);
  begin
    Field('deleted').AddString(entryId);
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.addNewEntry(const entry: IFRE_DB_Object; const reverenceItemId: String; const parentId: String; const revItemIsPrevious: Boolean);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('revid').AsString:=reverenceItemId;
    obj.Field('revisprev').AsBoolean:=revItemIsPrevious;
    obj.Field('parentid').AsString:=parentId;
    obj.Field('item').AsObject:=entry.CloneToNewObject();
    Field('new').AddObject(obj);
  end;

  procedure TFRE_DB_UPDATE_STORE_DESC.setTotalCount(const count: Integer);
  begin
    Field('total').AsInt32:=count;
  end;

  { TFRE_DB_INPUT_NUMBER_DESC }

  function TFRE_DB_INPUT_NUMBER_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean;  const disabled: boolean; const hidden: Boolean; const defaultValue: String; const digits: Integer): TFRE_DB_INPUT_NUMBER_DESC;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,hidden,defaultValue);
    Field('digits').AsInt16:=digits;
  end;

  { TFRE_DB_SUBMENU_DESC }

  function TFRE_DB_SUBMENU_DESC.Describe(const caption,icon: String; const disabled: Boolean): TFRE_DB_SUBMENU_DESC;
  begin
    inherited Describe();
    Field('caption').AsString:=caption;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('disabled').AsBoolean:=disabled;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_DESCRIPTION_DESC }

  function TFRE_DB_INPUT_DESCRIPTION_DESC.Describe(const caption,description: String): TFRE_DB_INPUT_DESCRIPTION_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('descr').AsString:=description;
  end;

  { TFRE_DB_MAIN_DESC }

  function TFRE_DB_MAIN_DESC.Describe(const style: String; const jiraIntegrationJSURL: String; const debug: Boolean): TFRE_DB_MAIN_DESC;
  begin
    Field('loadFunc').AsObject:=TFRE_DB_SERVER_FUNC_DESC.create.Describe('FIRMOS','init');
    Field('debug').AsBoolean:=debug;
    Field('style').AsString:=style;
    Field('jira').AsString:=jiraIntegrationJSURL;
    Result:=Self;
  end;

  { TFRE_DB_VIEW_LIST_BUTTON_DESC }

  function TFRE_DB_VIEW_LIST_BUTTON_DESC._Describe(const func: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const caption: String; const tooltip: String; const buttonDep: TFRE_DB_GRID_BUTTON_DEP): TFRE_DB_VIEW_LIST_BUTTON_DESC;
  begin
    Field('serverFunc').AsObject := func;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
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

  { TFRE_DB_DEPENDENCE_DESC }

  function TFRE_DB_DEPENDENCE_DESC.Describe(const fieldName: String; const disablesField: Boolean): TFRE_DB_DEPENDENCE_DESC;
  begin
    Field('fieldName').AsString:=fieldName;
    Field('disablesField').AsBoolean:=disablesField;
    Result:=Self;
  end;

  { TFRE_DB_BUTTON_DESC }

  function TFRE_DB_BUTTON_DESC.Describe(const caption: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const buttonType: TFRE_DB_BUTTON_TYPE): TFRE_DB_BUTTON_DESC;
  begin
    Field('caption').AsString:=caption;
    if Assigned(serverFunc) then begin
      Field('serverFunc').AsObject:=serverFunc as IFRE_DB_Object;
    end;
    Field('buttonType').AsString:=CFRE_DB_BUTTON_TYPE[buttonType];
    Result:=Self;
  end;

  { TFRE_DB_VALIDATOR_DESC }

  function TFRE_DB_VALIDATOR_DESC.Describe(const id,regExp, helpText: String; const allowedChars: String; const configParams: IFRE_DB_Object): TFRE_DB_VALIDATOR_DESC;
  begin
    Field('id').AsString:=id;
    Field('regExp').AsString:=regExp;
    Field('helpText').AsString:=helpText;
    Field('allowedChars').AsString:=allowedChars;
    if Assigned(configParams) then begin
      Field('configParams').AsObject:=configParams;
    end;
    Result:=Self;
  end;

  { TFRE_DB_INPUT_DESC }

  function TFRE_DB_INPUT_DESC.Describe(const caption, field_reference: String; const required: Boolean; const groupRequired: Boolean; const disabled: boolean; const hidden: Boolean; const defaultValue: String; const validator: IFRE_DB_ClientFieldValidator; const multiValues:Boolean; const isPass:Boolean): TFRE_DB_INPUT_DESC;
  begin
    inherited Describe(caption, field_reference, required, groupRequired, disabled, hidden, defaultValue, validator);
    Field('multiValues').AsBoolean := multiValues;
    Field('isPass').AsBoolean:=isPass;
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

  { TFRE_DB_REFRESH_DESC }


  { TFRE_DB_INPUT_BLOCK_DESC }

  function TFRE_DB_INPUT_BLOCK_DESC.Describe(const caption: String): TFRE_DB_INPUT_BLOCK_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('sizeSum').AsInt16:=0;
    Result:=Self;
  end;

  function TFRE_DB_INPUT_BLOCK_DESC.AddSchemeFormGroup(const schemeGroup: IFRE_DB_InputGroupSchemeDefinition; const session: TFRE_DB_UserSession; const collapsible: Boolean; const collapsed: Boolean; const relSize: Integer): TFRE_DB_INPUT_GROUP_DESC;
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
    obj:=_getRootObj(Self);
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        TFRE_DB_FORM_DESC(obj).AddStore(store);
      end else begin
        inherited AddStore(store);
      end;
    end else begin
      raise Exception.Create('Failed to add the store: Root Form not found!');
    end;
  end;

  procedure TFRE_DB_INPUT_BLOCK_DESC.AddDBO(const id: String; const session: TFRE_DB_UserSession);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=_getRootObj(Self);
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
    obj:=_getRootObj(Self);
    if obj.Implementor_HC is TFRE_DB_FORM_DESC then begin
      if obj.Implementor_HC<>Self then begin
        TFRE_DB_FORM_DESC(obj).GetStore(id);
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


  { TFRE_DB_MENU_ENTRY_DESC }

  function TFRE_DB_MENU_ENTRY_DESC.Describe(const caption, icon: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const disabled: Boolean): TFRE_DB_MENU_ENTRY_DESC;
  var
    path :String;
    cuid :TGuid;
  begin
    Field('caption').AsString:=caption;
    if Assigned(serverFunc) then begin
      Field('serverFunc').AsObject:=serverFunc;
    end;
    if icon<>'' then begin
      Field('icon').AsString:=getThemedResource(icon);
    end;
    Field('disabled').AsBoolean:=disabled;
    Result:=Self;
  end;

  { TFRE_DB_MENU_DESC }

  function TFRE_DB_MENU_DESC.Describe: TFRE_DB_MENU_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  function TFRE_DB_MENU_DESC.AddEntry: TFRE_DB_MENU_ENTRY_DESC;
  begin
     Result := TFRE_DB_MENU_ENTRY_DESC.Create;
     Field('entries').AddObject(Result);
  end;

  function TFRE_DB_MENU_DESC.AddMenu: TFRE_DB_SUBMENU_DESC;
  begin
    Result := TFRE_DB_SUBMENU_DESC.Create;
    Field('entries').AddObject(Result);
  end;

  { TFRE_DB_FORM_INPUT_DESC }

  function TFRE_DB_FORM_INPUT_DESC.Describe(const caption, field_reference: string; const required: boolean; const groupRequired: Boolean; const disabled: boolean; const hidden:Boolean; const defaultValue:String; const validator: IFRE_DB_ClientFieldValidator ; const validatorConfigParams : IFRE_DB_Object) : TFRE_DB_FORM_INPUT_DESC;
  begin
    Field('caption').AsString        := caption;
    Field('field').AsString          := field_reference;
    Field('defaultValue').AsString   := defaultValue;
    Field('required').AsBoolean      := required;
    Field('groupRequired').AsBoolean := groupRequired;
    Field('disabled').AsBoolean      := disabled;
    Field('hidden').AsBoolean        := hidden;
    if Assigned(validator) then begin
      Field('vtype').AsObject:=TFRE_DB_VALIDATOR_DESC.create.Describe(validator.ObjectName,validator.getRegExp,validator.getHelpText.ShortText,validator.getAllowedChars,validatorConfigParams);
    end;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  { TFRE_DB_STORE_ENTRY_DESC }

  function TFRE_DB_STORE_ENTRY_DESC.Describe(const caption, value: string):TFRE_DB_STORE_ENTRY_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('value').AsString:=value;
    Result:=Self;
  end;

  { TFRE_DB_STORE_DESC }

  function TFRE_DB_STORE_DESC.Describe(const idField: String; const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const labelFields:TFRE_DB_StringArray; const destroyFunc, clearQueryIdFunc: TFRE_DB_SERVER_FUNC_DESC; const id: String; const pageSize: Integer): TFRE_DB_STORE_DESC;
  begin
    Field('idField').AsString:=idField;
    Field('labelFields').AsStringArr:=labelFields;
    if Assigned(serverFunc) then begin
      Field('serverFunc').AsObject:=serverFunc;
    end;
    if Assigned(clearQueryIdFunc) then begin
      Field('clearQueryIdFunc').AsObject:=clearQueryIdFunc;
    end;
    if Assigned(destroyFunc) then begin
      Field('destroyFunc').AsObject:=destroyFunc;
    end;
    Field('pageSize').AsInt16:=pageSize;
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

  procedure TFRE_DB_INPUT_BOOL_DESC.AddDependence(const depField: TFRE_DB_DEPENDENCE_DESC);
  begin
    Field('dependentFields').AddObject(depField);
  end;

  function TFRE_DB_INPUT_BOOL_DESC.Describe(const caption, field_reference: string; const required: boolean; const groupRequired: Boolean; const disabled: boolean; const defaultValue: Boolean): TFRE_DB_INPUT_BOOL_DESC;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,false,BoolToStr(defaultValue,'true','false'));
    Result:=Self;
  end;

  { TFRE_DB_INPUT_CHOOSER_DESC }

  function TFRE_DB_INPUT_CHOOSER_DESC.Describe(const caption, field_reference: string; const store: TFRE_DB_STORE_DESC; const single_select:Boolean; const display_hint:TFRE_DB_CHOOSER_DH; const required: boolean; const groupRequired: Boolean; const disabled: boolean; const defaultValue:String): TFRE_DB_INPUT_CHOOSER_DESC;
  var
    obj: IFRE_DB_Object;
  begin
    inherited Describe(caption,field_reference,required,groupRequired,disabled,false,defaultValue);
    Field('singleSelect').AsBoolean:=true;
    Field('displayHint').AsString:=CFRE_DB_CHOOSER_DH[display_hint];
    obj:=GFRE_DBI.NewObject;
    obj.Field('id').AsString:=store.Field('id').AsString;
    obj.Field('serverFuncExists').AsBoolean:=store.FieldExists('serverFunc');
    Field('store').AsObject:=obj;
    Field('cce').AsBoolean:=false;
    (Parent.Implementor_HC as TFRE_DB_FORM_DESC).AddStore(store);
    result :=  self;
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
    Field('filter').AsBoolean:=cdgf_Filter in displayFlags;
    Field('sortable').AsBoolean:=cdgf_Sortable in displayFlags;
    Field('children').AsBoolean:=cdgf_Children in displayFlags;
    Field('columnResize').AsBoolean:=cdgf_ColumnResizeable in displayFlags;
    Field('columnHide').AsBoolean:=cdgf_ColumnHideable in displayFlags;
    Field('columnDrag').AsBoolean:=cdgf_ColumnDragable in displayFlags;
    Field('multiselect').AsBoolean:=cdgf_enableMultiselect in displayFlags;
    Field('details').AsBoolean:=cdgf_Details in displayFlags;

    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    if not FieldExists('toolbarTop') then Field('toolbarTop').AsBoolean:=false;
    if not FieldExists('toolbarBottom') then Field('toolbarBottom').AsBoolean:=false;
    Result:=Self;
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

  procedure TFRE_DB_VIEW_LIST_DESC.SetDropGrid(const grid: TFRE_DB_VIEW_LIST_DESC; const objectclassesMultiple:TFRE_DB_StringArray; const objectclassesSingle:TFRE_DB_StringArray);
  begin
    Field('dragId').AsString:=Field('id').AsString;
    grid.Field('dropId').AsString:=Field('id').AsString;
    if Assigned(objectclassesMultiple) then begin
      grid.Field('dropObjClassesMultiple').AsStringArr:=objectclassesMultiple;
    end;
    if Assigned(objectclassesSingle) then begin
      grid.Field('dropObjClassesSingle').AsStringArr:=objectclassesSingle;
    end;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetDragObjClasses(const objectclasses: TFRE_DB_StringArray);
  begin
    Field('dragObjClasses').AsStringArr:=objectclasses;
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.AddEntryAction(const serverFunc: TFRE_DB_SERVER_FUNC_DESC; const icon: String; const tooltip:String);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=GFRE_DBI.NewObject;
    obj.Field('serverFunc').AsObject:=serverFunc;
    if icon<>'' then begin
      obj.Field('icon').AsString:=getThemedResource(icon);
    end;
    obj.Field('tooltip').AsString:=tooltip;
    Field('entryActions').AddObject(obj);
  end;

  procedure TFRE_DB_VIEW_LIST_DESC.SetDependentContent(const contentFunc: TFRE_DB_SERVER_FUNC_DESC);
  begin
    Field('depContentFunc').AsObject:=contentFunc;
  end;

  { TFRE_DB_VIEW_TREE_DESC }

  function TFRE_DB_VIEW_TREE_DESC.Describe(const store: TFRE_DB_STORE_DESC; const title: String; const itemContextMenuFunc: TFRE_DB_SERVER_FUNC_DESC; const contextMenuFunc: TFRE_DB_SERVER_FUNC_DESC): TFRE_DB_VIEW_TREE_DESC;
  begin
    Field('store').AsObject:=store;
    Field('title').AsString:=title;
    if Assigned(itemContextMenuFunc) then begin
      Field('itemMenuFunc').AsObject:=itemContextMenuFunc;
    end;
    if Assigned(contextMenuFunc) then begin
      Field('menuFunc').AsObject:=contextMenuFunc;
    end;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
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

  function TFRE_DB_FORM_DESC.Describe(const caption: String;const defaultClose:Boolean;const sendChangedFieldsOnly: Boolean; const editable: Boolean): TFRE_DB_FORM_DESC;
  begin
    Field('caption').AsString:=caption;
    Field('defaultClose').AsBoolean:=defaultClose;
    Field('sendChanged').AsBoolean:=sendChangedFieldsOnly;
    Field('editable').AsBoolean:=editable;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_FORM_DESC._FillWithObjectValues(const obj: IFRE_DB_Object);
  var
    i,j      : Integer;
    val      : String;
    objField : IFRE_DB_FIELD;
    store    : TFRE_DB_STORE_DESC;

  begin
    for i := 0 to Field('elements').ValueCount - 1 do begin
      if (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_BLOCK_DESC) or (Field('elements').AsObjectItem[i].Implementor_HC is TFRE_DB_INPUT_GROUP_DESC) then begin
        (Field('elements').AsObjectItem[i].Implementor_HC as TFRE_DB_FORM_DESC)._FillWithObjectValues(obj);
      end else begin
        objField:=obj.FieldPath(Field('elements').AsObjectItem[i].Field('field').AsString,true);
        if Assigned(objField) then begin
          if (Field('elements').AsObjectItem[i].Implementor_HC  is TFRE_DB_INPUT_CHOOSER_DESC) and Field('elements').AsObjectItem[i].Field('cce').AsBoolean and (objField.FieldType=fdbft_Object) then begin
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
          end else begin
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
              end else begin
                val:=objField.AsString;
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

  procedure TFRE_DB_FORM_DESC.AddDBO(const id: String; const session: TFRE_DB_UserSession);
  var
    i : Integer;
  begin
    for i := 0 to Field('dbos').ValueCount - 1 do begin
      if Field('dbos').AsStringArr[i]=id then exit;
    end;
    Field('dbos').AddString(id);
    session.registerUpdatableDBO(id);
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

    function TFRE_DB_FORM_DESC.AddSchemeFormGroup(const schemeGroup: IFRE_DB_InputGroupSchemeDefinition; const session: TFRE_DB_UserSession; const collapsible: Boolean; const collapsed: Boolean): TFRE_DB_INPUT_GROUP_DESC;
  var
    group         : TFRE_DB_INPUT_GROUP_DESC;
    obj           : IFRE_DB_Object;
    val           : String;
    iField : TFRE_DB_FORM_INPUT_DESC;
    cField : IFRE_DB_OBJECT;

    function _getText(const key:TFRE_DB_String):TFRE_DB_String;
    var
      txt: IFRE_DB_TEXT;
    begin
       if (key<>'')
         and session.GetDBConnection.FetchTranslateableText(key,txt) then
         begin
           Result:=txt.Getshort;
         end
       else
         begin
           Result:=key;
         end;
    end;

    procedure _addInput(const obj:IFRE_DB_Object; const prefix:String; const requiredParent:Boolean);
    var
      coll               : IFRE_DB_COLLECTION;
      store              : TFRE_DB_STORE_DESC;
      required           : Boolean;
      enum               : IFRE_DB_Enum;
      enumVals           : IFRE_DB_ObjectArray;
      validator          : IFRE_DB_ClientFieldValidator;
      i                  : Integer;
      objArr             : IFRE_DB_ObjectArray;
      boolField          : TFRE_DB_INPUT_BOOL_DESC;
      itext              : IFRE_DB_TEXT;
      dataCollectionName : TFRE_DB_NameType;

    procedure addObjects(const obj: IFRE_DB_Object);
    begin
      store.AddEntry.Describe(obj.GetFormattedDisplay,obj.UID_String);
    end;

    begin
      required:=requiredParent and obj.Field('required').AsBoolean;
      dataCollectionName := obj.Field('dataCollection').AsString;
      if dataCollectionName<>'' then begin
        if pos('$SDC:',dataCollectionName)=1 then
          coll := session.FetchDerivedCollection(Copy(dataCollectionName,6,MaxInt))
        else
          coll := session.GetDBConnection.Collection(dataCollectionName,false);
        if assigned(coll) then
          begin
            store:=TFRE_DB_STORE_DESC.create.Describe();
            coll.ForAll(@addObjects);
            group.AddChooser.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,store,true,dh_chooser_combo,required,obj.Field('required').AsBoolean).captionCompareEnabled(true);
          end
        else
          begin
            raise EFRE_DB_Exception.Create(edb_NOT_FOUND,'The specified fieldbacking datacollection was not found : ['+dataCollectionName+']');
          end;
      end else begin
        if obj.Field('enum').AsString<>'' then begin
          if session.GetDBConnection.GetEnum(obj.Field('enum').AsString,enum) then begin
            store:=TFRE_DB_STORE_DESC.create.Describe();
            enumVals:=enum.getEntries;
            for i := 0 to Length(enumVals) - 1 do begin
              store.AddEntry.Describe(enumVals[i].Field('c').AsDBText.ShortText,enumVals[i].Field('v').AsString);
            end;
            group.AddChooser.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,store,true,dh_chooser_radio,required,obj.Field('required').AsBoolean);
          end else begin
            raise EFRE_DB_Exception.Create(edb_ERROR,'Enum '+obj.Field('enum').AsString+' not found in DB');
          end;
        end else begin
          if obj.FieldExists('vtype') then begin
            session.GetDBConnection.GetClientFieldValidator(obj.Field('vtype').AsString,validator);
          end else begin
            validator:=nil;
          end;
          case FieldtypeShortString2Fieldtype(obj.Field('type').AsString) of
            fdbft_UInt16,
            fdbft_UInt32,
            fdbft_Int16 : group.AddNumber.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                           obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean,'',0);
            fdbft_Real64 : group.AddNumber.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                            obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean);
            fdbft_ObjLink,
            fdbft_String : begin
                             group.AddInput.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                     obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean,'',validator,obj.Field('multiValues').AsBoolean,obj.Field('isPass').AsBoolean);
                             if obj.Field('addConfirm').AsBoolean then begin
                               group.AddInput.Describe(_getText('$scheme_input_confirm_prefix')+' ' + _getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString + '_confirm',required,obj.Field('required').AsBoolean,
                                                       obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean,'',validator,obj.Field('multiValues').AsBoolean,obj.Field('isPass').AsBoolean);
                             end;
                           end;
            fdbft_Boolean: begin
                             boolField:=group.AddBool.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                                       obj.Field('disabled').AsBoolean,false);
                             for i := 0 to obj.Field('depFields').ValueCount - 1 do begin
                               boolField.AddDependence(TFRE_DB_DEPENDENCE_DESC.Create.Describe(obj.Field('depFields').AsObjectItem[i].Field('fieldName').AsString,obj.Field('depFields').AsObjectItem[i].Field('disablesField').AsBoolean));
                             end;
                           end;
            fdbft_DateTimeUTC: iField:=group.AddDate.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                              obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean,'',validator);
            fdbft_Stream: begin
                            group.AddFile.Describe(_getText(obj.Field('caption_key').AsString),prefix+obj.Field('field').AsString,required,obj.Field('required').AsBoolean,
                                                   obj.Field('disabled').AsBoolean,obj.Field('hidden').AsBoolean,'',validator);
                          end
            else begin
              raise Exception.Create('Field Type not implemented : ['+obj.Field('type').AsString+']');
            end;
          end;
        end;
      end;
    end;

  procedure _addFields(const fields: IFRE_DB_ObjectArray; const prefix:String);
  var
    i         : integer;
    scheme    : IFRE_DB_SchemeObject;
    newPrefix : String;
    tmpGroup  : TFRE_DB_INPUT_GROUP_DESC;
    inputGroup: IFRE_DB_InputGroupSchemeDefinition;
    required  : Boolean;
    path      : TFOSStringArray;
    fieldDef  : IFRE_DB_FieldSchemeDefinition;
  begin
    GFRE_BT.SeperateString(prefix,'.',path);
    required:=true;

    if Length(path)>0 then begin
      scheme:=schemeGroup.GetParentScheme;
      for i := 0 to Length(path) - 1 do begin
        if not scheme.GetSchemeField(path[i],fieldDef) then raise EFRE_DB_Exception.Create(edb_ERROR,'cannot find scheme field: '+path[i]);
        required:=required and fieldDef.required;
        if not session.GetDBConnection.GetScheme(fieldDef.SubschemeName,scheme) then raise EFRE_DB_Exception.Create(edb_ERROR,'(A) cannot get scheme '+fieldDef.SubschemeName);
      end;
    end;

    for i := 0 to Length(fields) - 1 do begin
      if fields[i].FieldExists('scheme') then begin
        if not session.GetDBConnection.GetScheme(fields[i].Field('scheme').AsString,scheme) then raise EFRE_DB_Exception.Create(edb_ERROR,'(B) cannot get scheme '+fieldDef.SubschemeName);
        newPrefix:=fields[i].Field('prefix').AsString;
        if newPrefix<>'' then begin
          newPrefix:=newPrefix+'.';
        end;
        newPrefix:=prefix+newPrefix;
        if fields[i].Field('asSubGroup').AsBoolean then begin
          tmpGroup:=group;
          inputGroup:=scheme.GetInputGroup(fields[i].Field('group').AsString);
          group:=tmpGroup.AddGroup.Describe(_getText(inputGroup.CaptionKey),fields[i].Field('collapsible').AsBoolean,fields[i].Field('collapsed').AsBoolean);
          _addFields(inputGroup.Fields,newPrefix);
          group:=tmpGroup;
        end else begin
          inputGroup:=scheme.GetInputGroup(fields[i].Field('group').AsString);
          _addFields(inputGroup.Fields,newPrefix);
        end;
      end else begin
        _addInput(fields[i],prefix,required);
      end;
    end;
  end;

  begin
    group:=AddGroup.Describe(_getText(schemeGroup.CaptionKey),collapsible,collapsed);
    _addFields(schemeGroup.Fields,'');
    Result:=group;
  end;

  procedure TFRE_DB_FORM_DESC.SetElementValue(const elementId, value: String);
  var
    elem: TFRE_DB_CONTENT_DESC;
  begin
    elem:=GetFormElement(elementId);
    elem.Field('defaultValue').AsString:=value;
  end;

  procedure TFRE_DB_FORM_DESC.FillWithObjectValues(const obj: IFRE_DB_Object; const session: TFRE_DB_UserSession);
  begin
    AddDBO(obj.UID_String, session);
    _FillWithObjectValues(obj);
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

  procedure TFRE_DB_INPUT_GROUP_DESC.AddStore(const store: TFRE_DB_STORE_DESC);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=_getRootObj(Self);
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

  procedure TFRE_DB_INPUT_GROUP_DESC.AddDBO(const id: String; const session: TFRE_DB_UserSession);
  var
    obj: IFRE_DB_Object;
  begin
    obj:=_getRootObj(Self);
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
    obj:=_getRootObj(Self);
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

  function TFRE_DB_FORM_PANEL_DESC.Describe(const caption: String;const sendChangedFieldsOnly: Boolean; const editable: Boolean): TFRE_DB_FORM_PANEL_DESC;
  begin
    inherited Describe(caption,false,sendChangedFieldsOnly,editable);
    Result:=Self;
  end;

  procedure TFRE_DB_FORM_PANEL_DESC.SetMenu(const menu: TFRE_DB_MENU_DESC);
  begin
    Field('menu').AsObject:=menu;
  end;

  { TFRE_DB_DIALOG_DESC }

  function TFRE_DB_DIALOG_DESC.Describe(const caption:String;const width,maxHeight:Integer; const defaultClose,isDraggable:Boolean;const sendChangedFieldsOnly: Boolean; const editable: Boolean): TFRE_DB_DIALOG_DESC;
  begin
    inherited Describe('',defaultClose,sendChangedFieldsOnly,editable);
    Field('dialogCaption').AsString:=caption;
    Field('width').AsInt16:=width;
    Field('maxHeight').AsInt16:=maxHeight;
    Field('draggable').AsBoolean:=isDraggable;
    Result:=Self;
  end;

  procedure TFRE_DB_DIALOG_DESC.AddHeader(const header: TFRE_DB_HTML_DESC);
  begin
    Field('header').AddObject(header);
  end;

  procedure TFRE_DB_DIALOG_DESC.SendChangedFieldsOnly(const value: boolean);
  begin
    Field('sendChanged').AsBoolean:=value;
  end;

  { TFRE_DB_DATA_ELEMENT_DESC }

  function TFRE_DB_DATA_ELEMENT_DESC._Describe(const id, caption: string; const displayType: TFRE_DB_DISPLAY_TYPE; const size: Integer; const display: Boolean; const required: Boolean; const iconId: String): TFRE_DB_DATA_ELEMENT_DESC;
  begin
   Field('id').AsString:=id;
   Field('caption').AsString:=caption;
   Field('displayType').AsString:=CFRE_DB_DISPLAY_TYPE[displayType];
   Field('display').AsBoolean:=display;
   Field('required').AsBoolean:=required;
   Field('size').AsInt16:=size;
   if iconId<>'' then begin
     Field('iconId').AsString:=iconId;
   end;
  end;

  function TFRE_DB_DATA_ELEMENT_DESC.Describe(const id, caption: string; const displayType: TFRE_DB_DISPLAY_TYPE; const size: Integer; const display: Boolean; const required: Boolean; const iconId: String): TFRE_DB_DATA_ELEMENT_DESC;
  begin
    if displayType=dt_number_pb then raise EFRE_DB_Exception.Create(edb_ERROR,'Please use DescribePB to configure a progress bar (dt_number_pb).');
    _Describe(id,caption,displayType,size,display,required,iconId);
    Result:=Self;
  end;

  function TFRE_DB_DATA_ELEMENT_DESC.DescribePB(const id, caption: string; const labelId: string; const maxValue: Single; const size: Integer; const required: Boolean): TFRE_DB_DATA_ELEMENT_DESC;
  begin
    _Describe(id,caption,dt_number_pb,size,true,required,'');
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

  { TFRE_DB_SECTION_DESC }

  function TFRE_DB_SECTION_DESC._Describe(const title: String; const ord: Int16; const sectionId: String; const size: Integer): TFRE_DB_SECTION_DESC;
  begin
    Field('title').AsString:=title;
    Field('ord').AsInt16:=ord;
    if sectionId<>'' then begin
      Field('id').AsString:=sectionId;
    end else begin
      if not FieldExists('id') then begin
        Field('id').AsString:='id'+UID_String;
      end;
    end;
    if (size=-1) then begin
      Field('size').AsInt16:=1;
    end else begin
      Field('size').AsInt16:=size;
    end;
    (Parent.Implementor_HC as TFRE_DB_SUBSECTIONS_DESC).SectionDescribed(UID_String,ord,Field('size').AsInt16);
  end;

  function TFRE_DB_SECTION_DESC.Describe(const contentFunc: TFRE_DB_SERVER_FUNC_DESC; const title: String; const ord: Int16; const sectionId: String; const size:Integer): TFRE_DB_SECTION_DESC;
  begin
    _Describe(title,ord,sectionId,size);
    Field('contentFunc').AsObject:=contentFunc;
    Result:=Self;
  end;

  function TFRE_DB_SECTION_DESC._internalDescribe(const content: TFRE_DB_CONTENT_DESC; const title: String; const ord: Int16; const sectionId: String; const size: Integer): TFRE_DB_SECTION_DESC;
  begin
    _Describe(title,ord,sectionId,size);
    Field('content').AsObject:=content;
    Result:=Self;
  end;

  procedure TFRE_DB_SECTION_DESC.SetActive(const active: Boolean);
  begin
    (Parent.Implementor_HC as TFRE_DB_SUBSECTIONS_DESC).SetActiveSectionUID(UID_String);
  end;

  procedure TFRE_DB_SECTION_DESC.SetMenu(const menu: TFRE_DB_MENU_DESC);
  begin
    Field('menu').AsObject:=menu;
  end;

  { TFRE_DB_SUBSECTIONS_DESC }

  procedure TFRE_DB_SUBSECTIONS_DESC.SetActiveSectionUID(const sectionUID: String);
  begin
    Field('activeSection').AsString:=sectionUID;
    fnoActiveSectionSet:=false;
  end;

  procedure TFRE_DB_SUBSECTIONS_DESC.SectionDescribed(const sectionUID: String; const ord,size: Integer);
  begin
    Field('sizeSum').AsInt16:=Field('sizeSum').AsInt16+size;
    if fnoActiveSectionSet then begin
      if not FieldExists('activeSection') or (ord<factiveSectionOrd) then begin
        Field('activeSection').AsString:=sectionUID;
        factiveSectionOrd:=ord;
      end;
    end;
  end;

  constructor TFRE_DB_SUBSECTIONS_DESC.Create;
  begin
    fnoActiveSectionSet:=true;
    inherited Create;
  end;

  function TFRE_DB_SUBSECTIONS_DESC.Describe(const displayType: TFRE_DB_SUBSEC_DISPLAY_TYPE): TFRE_DB_SUBSECTIONS_DESC;
  begin
    Field('dt').AsString:=CFRE_DB_SUBSEC_DISPLAY_TYPE[displayType];
    Field('sizeSum').AsInt16:=0;
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Result:=Self;
  end;

  procedure TFRE_DB_SUBSECTIONS_DESC.OnUIChange(const serverFunc: TFRE_DB_SERVER_FUNC_DESC);
  begin
    Field('onUIChange').AsObject:=serverFunc;
  end;

  function TFRE_DB_SUBSECTIONS_DESC.AddSection(): TFRE_DB_SECTION_DESC;
  begin
    Result := TFRE_DB_SECTION_DESC.create;
    Field('sections').AddObject(Result);
  end;

  procedure TFRE_DB_SUBSECTIONS_DESC.SetActiveSection(const sectionId: String);
  var
    i: Integer;
  begin
    for i:=0 to Field('sections').ValueCount - 1 do begin
      if Field('sections').AsObjectItem[i].Field('id').AsString=sectionId then begin
        SetActiveSectionUID(Field('sections').AsObjectItem[i].UID_String);
        break;
      end;
    end;
  end;

  { TFRE_DB_LAYOUT_DESC }

  function TFRE_DB_LAYOUT_DESC.Describe(const title:String; const useSizedSections:Boolean): TFRE_DB_LAYOUT_DESC;
  begin
    Field('title').AsString:=title;
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

  procedure TFRE_DB_LAYOUT_DESC.AddDialog(const dialog: TFRE_DB_DIALOG_DESC);
  begin
    Field('dialog').AsObject:=dialog;
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

  function TFRE_DB_EDITOR_DESC.Describe(const loadFunc,saveFunc,startEditFunc,stopEditFunc: TFRE_DB_SERVER_FUNC_DESC; const contentType: TFRE_DB_CONTENT_TYPE): TFRE_DB_EDITOR_DESC;
  begin
    if not FieldExists('id') then begin
      Field('id').AsString:='id'+UID_String;
    end;
    Field('loadFunc').AsObject:=loadFunc;
    Field('saveFunc').AsObject:=saveFunc;
    Field('startEditFunc').AsObject:=startEditFunc;
    Field('stopEditFunc').AsObject:=stopEditFunc;
    Field('contentType').AsString:=CFRE_DB_CONTENT_TYPE[contentType];
    Result:=Self;
  end;

 function GLOB_GET_APPMODS_AS_SUBSECTIONS_CALLBACK(const obj:TFRE_DB_ObjectEx;const input:IFRE_DB_Object):IFRE_DB_Object;
 var ActiveSection : String;
     conn          : IFRE_DB_CONNECTION;
     app           : TFRE_DB_APPLICATION;
     res           : TFRE_DB_SUBSECTIONS_DESC;
     module        : TFRE_DB_APPLICATION_MODULE;
     is_mod        : boolean;

   procedure DescribeAppModules(const module:IFRE_DB_APPLICATION_MODULE;const module_order:int16);
   var
     menu   : TFRE_DB_MENU_DESC;
     section: TFRE_DB_SECTION_DESC;
   begin
     if app.CheckAppRightModule(conn,module.ObjectName) then begin
       section:=TFRE_DB_SUBSECTIONS_DESC(res).AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(module.AsObject,'content'),module.GetDescription(conn).Getshort,module_order,module.ObjectName);
       menu:=TFRE_DB_MENU_DESC(module.GetToolbarMenu);
       if Assigned(menu) then begin
         section.SetMenu(menu);
       end;
     end;
   end;

  begin
   if obj is TFRE_DB_APPLICATION then begin
     app    := obj as TFRE_DB_APPLICATION;
     is_mod := false;
   end else begin
     module := obj as TFRE_DB_APPLICATION_MODULE;
     app    := module.GetEmbeddingApp;
     is_mod := true;
   end;
   conn    := app.GetSession(input).GetDBConnection;
   res     := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
   TFRE_DB_SUBSECTIONS_DESC(res).OnUIChange(app.CSF(@app.IMI_OnUIChange));
   if is_mod then begin
     module.ForAllAppModules(@DescribeAppModules);
   end else begin
     app.ForAllAppModules(@DescribeAppModules);
   end;
 //  ActiveSection := session.GetSessionAppData(input).Field('activeSection').AsString;
   ActiveSection := app.GetSessionAppData(input).Field('activeSection').AsString;
   GFRE_DBI.LogInfo(dblc_APPLICATION,'GETAPPMODSASSUBSECTION ACTIVE SECTION = [%s]',[ActiveSection]);
   TFRE_DB_SUBSECTIONS_DESC(res).SetActiveSection(ActiveSection);
   result := res;
 end;

 //var ActiveSection:TFRE_DB_String;
//
//  procedure DescribeAppModules(const module:IFRE_DB_APPLICATION_MODULE;const module_order:int16);
//  var
//    menu   : TFRE_DB_MENU_DESC;
//    section: TFRE_DB_SECTION_DESC;
//  begin
//    section:=TFRE_DB_SUBSECTIONS_DESC(result).AddSection.Describe(TFRE_DB_SERVER_FUNC_DESC.Create.Describe(module.AsObject,'content'),module.Description.Getshort,module_order,module.ObjectName);
//    menu:=TFRE_DB_MENU_DESC(module.GetToolbarMenu);
//    if Assigned(menu) then begin
//      section.SetMenu(menu);
//    end;
//  end;
//
//begin
//  result := TFRE_DB_SUBSECTIONS_DESC.create.Describe;
//  TFRE_DB_SUBSECTIONS_DESC(result).OnUIChange(CSF(@IMI_OnUIChange));
//  ForAllAppModules(@DescribeAppModules);
//  ActiveSection := GetDBModuleSessionData(input).Field('activeSection').AsString;
//  GFRE_DBI.LogInfo(dblc_APPLICATION,'MOD GETAPPMODSASSUBSECTION ACTIVE SECTION = [%s]',[ActiveSection]);
//  TFRE_DB_SUBSECTIONS_DESC(result).SetActiveSection(ActiveSection);
//end;


procedure GLOB_AddAppToSiteMap(const app : TFRE_DB_APPLICATION ; const session: TFRE_DB_UserSession; const parent_entry: TFRE_DB_CONTENT_DESC);
var res         : TFRE_DB_SITEMAP_DESC;
    parent_e    : TFRE_DB_SITEMAP_ENTRY_DESC;
    sitemapdata : IFRE_DB_Object;
    ientry      : integer;

    procedure BuildSM(const entry:IFRE_DB_Object);
    var caption,
          icon,id    : String;
          x,y,i,nc   : integer;
          scale      : Single;
          dis        : Boolean;
        next_lev     : TFRE_DB_SITEMAP_ENTRY_DESC;
        old_par      : TFRE_DB_SITEMAP_ENTRY_DESC;
        ial          : TFRE_DB_StringArray;
        oial         : TFRE_DB_StringArray;
        isubentry    : integer;
    begin
        caption  := entry.Field('CAP').AsString;
        id       := entry.Field('ID').AsString;
        nc       := entry.Field('NC').AsInt16;
        icon     := entry.Field('ICO').AsString;
        x        := entry.Field('CRD').AsInt32Arr[0];
        y        := entry.Field('CRD').AsInt32Arr[1];
        oial     := entry.Field('IAL').AsStringArr;
        scale    := entry.Field('SCL').AsReal32;
        dis      := entry.Field('DIS').AsBoolean;
        SetLength(ial,length(oial)+3);
        ial[0]   := 'Home';
        ial[1]   := 'AppContainer';
        ial[2]   := app.ObjectName;
        oial     := entry.Field('IAL').AsStringArr;
        for i:=0 to high(oial) do begin
          ial[i+3] := oial[i];
        end;
        old_par  := parent_e;
        next_lev := parent_e.AddEntry.Describe(caption,icon,TFRE_DB_RESTORE_UI_DESC.create.Describe('FirmOSViewport',ial),x,y,id,nc,dis,scale);
        if true then begin
          parent_e := next_lev;
          for isubentry := 0 to entry.Field('ENTRIES').valuecount-1 do begin
            BuildSM(entry.Field('ENTRIES').AsObjectItem[isubentry]);
          end;
          parent_e := old_par;
        end;
    end;

begin
  parent_e    := parent_entry as TFRE_DB_SITEMAP_ENTRY_DESC;
  SiteMapData :=session.GetSessionAppData(app.ObjectName).FieldOnlyExistingObj('SITEMAP');
  if assigned(sitemapdata) then begin
    for ientry := 0 to sitemapdata.Field('ENTRIES').ValueCount-1 do begin
      BuildSM(sitemapdata.Field('ENTRIES').AsObjectItem[ientry]);
    end;
  end;
end;

initialization
  G_APPMODS_AS_SUBSECTIONS_CALLBACK := @GLOB_GET_APPMODS_AS_SUBSECTIONS_CALLBACK;
  G_ADD_2_SITEMAP_CALLBACK          := @GLOB_AddAppToSiteMap;

end.

