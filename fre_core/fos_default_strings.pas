unit fos_default_strings;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

{$MODE objfpc} {$H+}

interface

uses
     FOS_TOOL_INTERFACES,Classes, SysUtils;

 function Get_FOS_DefaultStrings:IFOS_STRINGS;

implementation

 type
   { TFOS_SL }
   TFOS_SL=class(TInterfacedObject,IFOS_STRINGS)
   private
     _sl:TStringlist;
   public
    constructor Create;
    destructor  Destroy;override;
    function  Add               (const S: string): Integer;
    procedure AddStrings        (const strings: IFOS_STRINGS);
    procedure AddToStrings      (const strings: TStrings);
    procedure ReplaceStrings    (const strings: TStrings);
    procedure Clear             ;
    procedure Delete            (Index: Integer);
    procedure Exchange          (Index1, Index2: Integer);
    function  Find              (const S: string; var Index: Integer): Boolean;
    function  IndexOf           (const S: string): Integer;
    procedure Insert            (Index: Integer; const S: string);
    procedure Sort              ;
    procedure SetDuplicates     (const dup:TDuplicates);
    function  Duplicates        :TDuplicates;
    procedure SetSorted         (const srt:boolean);
    function  Sorted            :Boolean;
    procedure SetCaseSensitive  (const casesense:boolean);
    function  CaseSensitive     :Boolean;
    procedure SetCommatext      (const txt:string);
    function  Commatext         :String;
    procedure SetDelimitedText  (const txt:string);
    function  DelimitedText     :String;
    procedure SetText           (const txt:String);
    function  Text              :String;
    procedure SetQuoteChar      (const qc:Char);
    function  QuoteChar         :Char;
    procedure SetLineBreakStyle (const lbs:TTextLineBreakStyle);
    function  LineBreakStyle     :TTextLineBreakStyle;
    function  Count             :Integer;
    procedure SetS              (const index:Integer;const txt:String);
    function  S                 (const index:Integer):string;
    function  GetItems(idx: integer): string;
    procedure SetItems(idx: integer; const AValue: string);
    property  Items             [idx:integer]:string read GetItems write SetItems;default;
   end;

{ TFOS_SL }

constructor TFOS_SL.Create;
begin
 _SL:=TStringList.Create;
end;

destructor TFOS_SL.Destroy;
begin
 _sl.free;
 inherited;
end;

function TFOS_SL.Add(const S: string): Integer;
begin
  result:=_sl.Add(s);
end;

procedure TFOS_SL.AddStrings(const strings: IFOS_STRINGS);
var i:integer;
begin
 for i:=0 to strings.Count-1 do add(strings.S(i));
end;

procedure TFOS_SL.AddToStrings(const strings: TStrings);
begin
 strings.AddStrings(_sl);
end;

procedure TFOS_SL.ReplaceStrings(const strings: TStrings);
begin
 strings.Clear;
 strings.AddStrings(_sl);
end;

procedure TFOS_SL.Clear;
begin
  _sl.Clear;
end;

procedure TFOS_SL.Delete(Index: Integer);
begin
 _sl.Delete(Index);
end;

procedure TFOS_SL.Exchange(Index1, Index2: Integer);
begin
 _sl.Exchange(Index1,index2);
end;

function TFOS_SL.Find(const S: string; var Index: Integer): Boolean;
begin
 result:=_sl.Find(s,Index);
end;

function TFOS_SL.IndexOf(const S: string): Integer;
begin
 result:=_sl.IndexOf(s);
end;

procedure TFOS_SL.Insert(Index: Integer; const S: string);
begin
 _sl.Insert(index,s);
end;

procedure TFOS_SL.Sort;
begin
 _sl.sort;
end;

procedure TFOS_SL.SetDuplicates(const dup: TDuplicates);
begin
 _sl.Duplicates:=dup;
end;

function TFOS_SL.Duplicates: TDuplicates;
begin
 result:=_sl.Duplicates;
end;

procedure TFOS_SL.SetSorted(const srt: boolean);
begin
 _sl.Sorted:=srt;
end;

function TFOS_SL.Sorted: Boolean;
begin
 result:=_sl.Sorted;
end;

procedure TFOS_SL.SetCaseSensitive(const casesense: boolean);
begin
 _sl.CaseSensitive:=casesense;
end;

function TFOS_SL.CaseSensitive: Boolean;
begin
  result:=_sl.CaseSensitive;
end;

procedure TFOS_SL.SetCommatext(const txt: string);
begin
 _sl.CommaText:=txt;
end;

function TFOS_SL.Commatext: String;
begin
 result:=_sl.CommaText;
end;

procedure TFOS_SL.SetDelimitedText(const txt: string);
begin
 _sl.DelimitedText:=txt;
end;

function TFOS_SL.DelimitedText: String;
begin
 result:=_sl.DelimitedText;
end;

procedure TFOS_SL.SetText(const txt: String);
begin
 _sl.Text:=txt;
end;

function TFOS_SL.Text: String;
begin
 result:=_sl.Text;
end;

procedure TFOS_SL.SetQuoteChar(const qc: Char);
begin
 _sl.QuoteChar:=qc;
end;

function TFOS_SL.QuoteChar: Char;
begin
 result:=_sl.QuoteChar;
end;

procedure TFOS_SL.SetLineBreakStyle(const lbs: TTextLineBreakStyle);
begin
 _sl.TextLineBreakStyle:=lbs;
end;

function TFOS_SL.LineBreakStyle: TTextLineBreakStyle;
begin
 result:=_sl.TextLineBreakStyle;
end;

function TFOS_SL.Count: Integer;
begin
  result:=_sl.Count;
end;

procedure TFOS_SL.SetS(const index: Integer; const txt: String);
begin
 _sl[index]:=txt;
end;

function TFOS_SL.S(const index: Integer): string;
begin
 Result:=_sl[index];
end;

function TFOS_SL.GetItems(idx: integer): string;
begin
 Result:=_sl[idx];
end;

procedure TFOS_SL.SetItems(idx: integer; const AValue: string);
begin
 _sl[idx]:=AValue;
end;

function Get_FOS_DefaultStrings:IFOS_STRINGS;
begin
 result:=TFOS_SL.Create;
end;


end.
