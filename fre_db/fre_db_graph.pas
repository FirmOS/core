unit fre_db_graph;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE, fre_Process;

const

  cplotcmd  = 'dot';


type

  { TFRE_DB_GRAPH }

  TFRE_DB_GRAPH = class
  private
    plotlist           :       TStringList;
    parentlist         :       TStringList;
    embeddedlist       :       TStringList;
    referenceobj       :       IFRE_DB_Object;
    collectionobj      :       IFRE_DB_Object;

    procedure          WriteHeader;
    procedure          WriteFooter;
    procedure          WriteMethods                      (const obj      : IFRE_DB_SCHEMEOBJECT);
    procedure          WriteFields                       (const obj      : IFRE_DB_SCHEMEOBJECT);
    procedure          CheckFieldsSubscheme              (const obj      : IFRE_DB_SCHEMEOBJECT);
    procedure          CheckSubscheme                    (const fieldname:string;   const obj: IFRE_DB_SCHEMEOBJECT);
    procedure          WriteSubscheme                    (const fieldname:string;   const obj: IFRE_DB_SCHEMEOBJECT; const required:boolean);
    procedure          AddParent                         (const parentname : string; const childname : string);
    procedure          WriteParents                      ;
    procedure          WriteReferences                   ;
    procedure          WriteCollectionContains           ;
    function           FormatExplanationToTable          (const explanation: TFRE_DB_String)     : TFRE_DB_String;
    function           FormatSchemeHeader                (const obj      : IFRE_DB_SCHEMEOBJECT) : TFRE_DB_String;

  public
    constructor        Create;
    destructor         Destroy;

    procedure          PlotStart;
    procedure          CollectionIterator                (const coll: IFRE_DB_COLLECTION);
    procedure          EmbeddedIterator                  (const obj : IFRE_DB_SchemeObject);
    procedure          SchemeIterator                    (const obj : IFRE_DB_SchemeObject);
    procedure          ObjectIterator                    (const obj : IFRE_DB_Object; const conn: TObject);
    procedure          PlotEnd;
    procedure          PlotScheme                        (const datastream : TStream);
  end;


implementation

uses FRE_DB_CORE;


{ TFRE_DB_GRAPH }

procedure TFRE_DB_GRAPH.WriteHeader;
begin
  plotlist.Add('digraph G {');
  plotlist.Add('graph [');
  plotlist.add('rankdir="LR"');
  plotlist.Add('fontname=Arial');
  plotlist.Add('bgcolor="transparent"');
 //     plotlist.Add('size="100,70"');
  plotlist.Add(']');
  plotlist.Add('node [shape=plaintext, fontname="Arial"]');
end;

procedure TFRE_DB_GRAPH.WriteFooter;
begin
  plotlist.Add('}');
end;

procedure TFRE_DB_GRAPH.WriteMethods(const obj: IFRE_DB_SCHEMEOBJECT);
var methodarray         :      TFRE_DB_StringArray;
    lmethodcounter      :      integer;

begin
  methodarray         := obj.GetAll_IMI_Methods;
  for lmethodcounter  := low (methodarray) to high (methodarray) do begin
    plotlist.Add('<tr><td align="left" colspan="3" bgcolor="lightblue">'+methodarray[lmethodcounter] +'</td></tr>');
  end;
end;

procedure TFRE_DB_GRAPH.WriteFields(const obj: IFRE_DB_SCHEMEOBJECT);

  procedure IterateFieldDefs(const fielddef : IFRE_DB_FieldSchemeDefinition);
  var svalidator       :         string;
      senum            :         string;
      sfieldname       :         string;
      sfieldtype       :         string;
  begin
    //REWORK
    //if fielddef.FieldType=fdbft_Object then begin
    //  WriteSubscheme   (lowercase(fielddef.FieldName),fielddef.GetSubScheme,fielddef.required);
    //end else begin
    //  if fielddef.getValidator(svalidator)) then begin
    //    svalidator      := string(fielddef.getValidator.GetName)+'['+string(fielddef.getValidator.getInfoText)+']';
    //  end else begin
    //    svalidator      :='';
    //  end;
    //  if assigned      (fielddef.getEnum) then begin
    //    senum     := string(fielddef.getEnum.GetName)+'[ENUM]';
    //  end else begin
    //    senum      :='';
    //  end;
    //  sfieldtype := lowercase(CFRE_DB_FIELDTYPE[fielddef.FieldType]);
    //  if fielddef.multiValues then begin
    //   sfieldtype :=sfieldtype + ' [ ]';
    //  end;
    //  sfieldname:=lowercase(fielddef.FieldName);
    //  if (fielddef.required) then begin
    //    sfieldname:='<FONT COLOR="red">'+sfieldname+'</FONT>'
    //  end;
    //  plotlist.Add      ('<tr  ><td align="left">'+sfieldname+'</td><td align="left">'+sfieldtype+'</td><td align="left" PORT="'+lowercase(fielddef.FieldName)+'">'+svalidator+' '+senum+'</td></tr>');
    //end;
  end;

begin
  obj.ForAllFieldSchemeDefinitions(@IterateFieldDefs);
end;

procedure TFRE_DB_GRAPH.CheckFieldsSubscheme(const obj: IFRE_DB_SCHEMEOBJECT);

  procedure IterateFieldDefs(const fielddef : IFRE_DB_FieldSchemeDefinition);
  var svalidator       :         string;
      senum            :         string;
  begin
    if fielddef.FieldType=fdbft_Object then begin
      embeddedlist.Add (uppercase(fielddef.GetSubSchemeName));
      CheckSubscheme   (lowercase(fielddef.FieldName),fielddef.GetSubScheme);
    end;
  end;

begin
  obj.ForAllFieldSchemeDefinitions(@IterateFieldDefs);
end;

procedure TFRE_DB_GRAPH.CheckSubscheme(const fieldname: string; const obj: IFRE_DB_SCHEMEOBJECT);
begin
  if not assigned(obj) then begin
    raise EFRE_DB_Exception.Create(edb_ERROR,'CHECKSUBSCHEME FIELD [%s] NO SCHEMEOBJECT',[fieldname]);
  end else begin
    CheckFieldsSubscheme(obj);
  end;
end;

procedure TFRE_DB_GRAPH.WriteSubscheme(const fieldname: string; const obj: IFRE_DB_SCHEMEOBJECT; const required:boolean);
var sfieldname        :     string;
begin
  plotlist.add('<tr>');
  sfieldname  :=fieldname;
  if (required) then begin
    sfieldname:='<FONT COLOR="red">'+sfieldname+'</FONT>'
  end;
  plotlist.Add('<td align="left" valign="top">'+sfieldname+'</td>');
  plotlist.Add('<td align="left" colspan="2">');
  plotlist.add('<table align="left" border="0" cellborder="1" cellspacing="0" cellpadding="4">');
  plotlist.add('<tr><td align="left" colspan="3" bgcolor="lightgreen">'+FormatSchemeHeader(obj)+'</td></tr>');
  if length(obj.Explanation)>0 then begin
   plotlist.Add('<tr><td align="left" colspan="3" PORT="sn" bgcolor="lightyellow">'+FormatExplanationToTable(obj.Explanation) +'</td></tr>');
  end;
  WriteFields(obj);
  WriteMethods(obj);
  plotlist.add('</table>');
  plotlist.Add('</td>');
  plotlist.Add('</tr>');
end;

procedure TFRE_DB_GRAPH.AddParent(const parentname: string; const childname: string);
begin
  parentlist.Add('struct'+lowercase(parentname)+':sn -> struct'+lowercase(childname)+':sn;');
end;

procedure TFRE_DB_GRAPH.WriteParents;
begin
  plotlist.AddStrings(parentlist);
end;

procedure TFRE_DB_GRAPH.WriteReferences;

  procedure FieldIterate (const fld: IFRE_DB_Field);
  begin
   if fld.IsUIDField=false then begin
     plotlist.Add(lowercase(fld.FieldName)+ ' [color=blue] [fontcolor=blue] [label="'+fld.AsString+'"];');
   end;
  end;

begin
  referenceobj.ForAllFields(@FieldIterate);
//  writeln(referenceobj.DumpToString());
end;

procedure TFRE_DB_GRAPH.WriteCollectionContains;

  procedure FieldIterate (const fld: IFRE_DB_Field);
  begin
   if fld.IsUIDField=false then begin
     plotlist.Add(lowercase(fld.FieldName)+ ' [color=sandybrown] [fontcolor=sandybrown] [label="'+fld.AsString+'"];');
   end;
  end;

begin
  collectionobj.ForAllFields(@FieldIterate);
end;

function TFRE_DB_GRAPH.FormatExplanationToTable(const explanation: TFRE_DB_String): TFRE_DB_String;
var p      : integer;
    parts  : TFRE_DB_String;
begin
 parts :=explanation;
 result:='<TABLE BORDER="0">';
 repeat
   p:=Pos('<br>',parts);
   if p>0 then begin
    result:=result+'<tr><td>'+Copy(parts,1,p-1)+'</td></tr>';
    parts:=Copy(parts,p+4,length(parts));
   end else begin
    result:=result+'<tr><td>'+parts+'</td></tr>';
   end;
 until p=0;
 result:=result+'</TABLE>';
end;

function TFRE_DB_GRAPH.FormatSchemeHeader(const obj: IFRE_DB_SCHEMEOBJECT): TFRE_DB_String;
var  fparentscheme      : TFRE_DB_String;
begin
 result:=uppercase(obj.DefinedSchemename);
 fparentscheme:=obj.GetParentSchemeName;
 if length(fparentscheme)>0 then begin
  Result:=result+' ('+fparentscheme+')';
 end;
end;

constructor TFRE_DB_GRAPH.Create;
begin
  plotlist      :=      TStringList.Create;
  parentlist    :=      TStringList.Create;
  embeddedlist  :=      TStringList.Create;
  referenceobj  :=      GFRE_DBI.NewObject;
  collectionobj :=      GFRE_DBI.NewObject;
end;

destructor TFRE_DB_GRAPH.Destroy;
begin
  embeddedlist.Free;
  parentlist.Free;
  plotlist.Free;
end;

procedure TFRE_DB_GRAPH.PlotStart;
begin
  WriteHeader;
end;

procedure TFRE_DB_GRAPH.CollectionIterator (const coll: IFRE_DB_COLLECTION);
 var  scolor            : string;
      indexedcollection : IFRE_DB_COLLECTION;
      collectiontype    : string;

   procedure ObjectIterate (const obj: IFRE_DB_OBJECT);
   var  scollection    : string;
        icollcount     : int64;
   begin
     scollection:='coll'+lowercase(coll.CollectionName)+':sn -> struct'+lowercase(obj.SchemeClass)+':sn';
     if collectionobj.FieldExists(scollection) then begin
       icollcount    := collectionobj.Field(scollection).AsInt64;
       inc(icollcount);
     end else begin
       icollcount    := 1;
     end;
     collectionobj.Field (scollection).AsInt64:=icollcount;
   end;

 begin
   if coll.Collectionname='' then begin
     exit;
   end;

   collectiontype := coll.Implementor_HC.ClassName;
   writeln(coll.Implementor_HC.ClassName);
   case collectiontype of
     'TFRE_DB_INDEXED_COLLECTION' : scolor := 'orange';
     'TFRE_DB_SCHEME_COLLECTION'  : scolor := 'green';
     else                           scolor := 'navajowhite';
   end;
   collectiontype := ' ('+collectiontype+')  ';
   plotlist.Add('coll'+lowercase(coll.CollectionName)+' [label=<');
   plotlist.Add('<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">');
   plotlist.Add('<tr><td align="left" colspan="2" PORT="sn" bgcolor="'+scolor+'"><u>'+uppercase(coll.Collectionname)+collectiontype+' ('+inttostr(coll.Count)+')</u></td></tr>');
   //if assigned(indexedcollection) then begin
   //  plotlist.Add('<tr><td align="left">Index</td><td align="left">'+lowercase(indexedcollection.GetIndexField)+'</td></tr>');
   //end;
   plotlist.Add('</TABLE>>];');
   coll.ForAll(@ObjectIterate);
 end;

procedure TFRE_DB_GRAPH.EmbeddedIterator(const obj:IFRE_DB_SCHEMEOBJECT);
begin
  CheckFieldsSubscheme(obj);
end;

procedure TFRE_DB_GRAPH.SchemeIterator(const obj:IFRE_DB_SCHEMEOBJECT);
begin
  if embeddedlist.IndexOf(uppercase(obj.DefinedSchemeName))>=0 then begin
   exit;
  end;
  plotlist.Add('struct'+lowercase(obj.DefinedSchemeName)+' [label=<');
  plotlist.Add('<TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">');
  plotlist.Add('<tr><td align="left" colspan="3" PORT="sn" bgcolor="lightgreen"><u>'+uppercase(obj.DefinedSchemeName)+'</u></td></tr>');
  if length(obj.Explanation)>0 then begin
   plotlist.Add('<tr><td align="left" colspan="3" PORT="sn" bgcolor="lightyellow">'+FormatExplanationToTable(obj.Explanation) +'</td></tr>');
  end;
  WriteFields (obj);
  WriteMethods(obj);
  plotlist.Add('</TABLE>>];');
  if obj.GetParentSchemeName<>'' then begin
   AddParent(obj.GetParentSchemeName,obj.DefinedSchemeName);
  end;
end;

procedure TFRE_DB_GRAPH.ObjectIterator(const obj: IFRE_DB_Object; const conn: TObject);
var sfrom        : string;
    sto          : string;

  procedure FieldIterate (const fld:IFRE_DB_Field);
  var sreference   : string;
      ivaluecount  : integer;
      link_obj     : TFRE_DB_Object;
      irefcount    : int64;
  begin
    if fld.FieldType = fdbft_ObjLink then begin
      if fld.ValueCount>0 then begin
        for ivaluecount     :=  0  to  fld.ValueCount-1 do begin
          CheckDbResult((conn as TFRE_DB_BASE_CONNECTION).Fetch(fld.AsObjectLinkArray[ivaluecount],link_obj),'inconsistency db links');
          sreference:='struct'+lowercase(obj.Schemeclass)+':'+lowercase(fld.Fieldname)+' -> struct'+lowercase(link_obj.SchemeClass)+':sn';
          if referenceobj.FieldExists(sreference) then begin
            irefcount    := referenceobj.Field(sreference).AsInt64;
            inc(irefcount);
          end else begin
            irefcount    := 1;
          end;
          referenceobj.Field (sreference).AsInt64:=irefcount;
        end;
      end;
    end;
  end;

begin
  if obj.ReferencesObjectsFromData then begin
    obj.ForallFields(@FieldIterate);
  end;
end;

procedure TFRE_DB_GRAPH.PlotEnd;
begin
  WriteParents;
  WriteReferences;
  WriteCollectionContains;
  WriteFooter;
end;


procedure TFRE_DB_GRAPH.PlotScheme(const datastream: TStream);
var
   process      :      TFRE_Process;
   instream     :      TStringStream;
   errorstream  :      TStringStream;
   res          :      integer;

begin
  process     := TFRE_Process.Create(nil);
  instream    := TStringStream.Create(plotlist.Text);
  errorstream := TStringStream.Create('');
  try
   instream.Position := 0;
   res               := process.ExecutePipedStream(cplotcmd,TFRE_DB_StringArray.Create('-T','svg'),instream,datastream,errorstream);
   if res<>0 then begin
     writeln('Exit Status:',process.ExitStatus);
     writeln('Error:',errorstream.DataString);
   end;
  finally
   errorstream.Free;
   instream.Free;
   process.Free;
  end;
end;



end.

