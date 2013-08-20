unit fre_http_tools;

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

interface

uses
  Classes, SysUtils,FOS_TOOL_INTERFACES,URIPArser,strutils;

//TODO DEBUG URI PARSING

type
  TFRE_HTTP_PARSER_REQUEST_METHOD = (rprm_OPTIONS,rprm_GET,rprm_HEAD,rprm_POST,rprm_PUT,rprm_DELETE,rprm_TRACE,rprm_CONNECT);

  TFRE_HTTP_URI = record
    Method       : TFRE_HTTP_PARSER_REQUEST_METHOD;
    Protocol     : String;
    //Username     : String;
    //Password     : String;
    Host         : String;
    Port         : Word;
    SplitPath    : TFOSStringArray;
    Document     : String;
    Params       : String;
    Bookmark     : String;
    //HasAuthority : Boolean;
  end;

  TFRE_HTTP_ContentProvider=procedure(const uri:TFRE_HTTP_URI) of object;

  TFRE_HTTP_DISPATCHRECORD=record
    path   : TFOSStringArray;
    method : TFRE_HTTP_ContentProvider;
  end;

  //TODO Domain Handling

  TFRE_HTTP_URL_DISPATCHER=class
  private
    FDomains      : Array of TFRE_HTTP_DISPATCHRECORD;
    FDefault      : TFRE_HTTP_DISPATCHRECORD;
  public
    procedure  RegisterVirtualProvider (const domain,path:string;const method:TFRE_HTTP_ContentProvider);
    procedure  RegisterDefaultProvider (const domain:string;const method:TFRE_HTTP_ContentProvider);
    procedure  DispatchRequest         (const connection_object:TObject;const uri:string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
  end;

  { TFRE_HTTP_COOKIE }

  TFRE_HTTP_COOKIE=class
  private
    FDomain  : string;
    FExpires : TFRE_DB_DateTime64;
    FHttpOnly: boolean;
    FKey: String;
    FMaxAge  : integer;
    FPath    : string;
    FSecure  : boolean;
    FVal: String;


    function  GetAsString: String;
    procedure SetDomain(const AValue: string);
    procedure SetExpires(const AValue: TFRE_DB_DateTime64);
    procedure SetFromString(const AValue: String);
    procedure SetHttpOnly(const AValue: boolean);
    procedure SetKey(const AValue: String);
    procedure SetMaxAge(const AValue: integer);
    procedure SetPath(const AValue: string);
    procedure SetSecure(const AValue: boolean);
    procedure SetVal(const AValue: String);
  public
    constructor create;
    property  Path: string read FPath write SetPath;
    property  Domain  : string read FDomain write SetDomain;
    property  Secure  : boolean read FSecure write SetSecure;
    property  HttpOnly: boolean read FHttpOnly write SetHttpOnly;
    property  MaxAge  : integer read FMaxAge write SetMaxAge;
    property  Expires : TFRE_DB_DateTime64 read FExpires write SetExpires;
    property  CookieKey :String read FKey write SetKey;
    property  CookieVal :String read FVal write SetVal;
    property  CookieString : String read GetAsString write SetFromString;
  end;


implementation

procedure TFRE_HTTP_URL_DISPATCHER.RegisterVirtualProvider(const domain, path: string; const method: TFRE_HTTP_ContentProvider);
var  i: Integer;
begin
  SetLength(FDomains,length(FDomains)+1);
  GFRE_BT.SeperateString(path,'/',FDomains[high(FDomains)].path);
  FDomains[high(FDomains)].method  := method;
  //for i:=0 to high(FDomains[high(FDomains)].path) do begin
  //  writeln('REGISTERD ',path,' ',i,' ',FDomains[high(FDomains)].path[i]);
  //end;
end;

procedure TFRE_HTTP_URL_DISPATCHER.RegisterDefaultProvider(const domain: string; const method: TFRE_HTTP_ContentProvider);
begin
  FDefault.method:=method;
end;

const
  GenDelims = [':', '/', '?', '#', '[', ']', '@'];
  SubDelims = ['!', '$', '&', '''', '(', ')', '*', '+', ',', ';', '='];
  ALPHA = ['A'..'Z', 'a'..'z'];
  DIGIT = ['0'..'9'];
  Unreserved = ALPHA + DIGIT + ['-', '.', '_', '~'];
  ValidPathChars = Unreserved + SubDelims + ['@', ':', '/'];

function HexValue(c: Char): Integer;
begin
  case c of
    '0'..'9': Result := ord(c) - ord('0');
    'A'..'F': Result := ord(c) - (ord('A') - 10);
    'a'..'f': Result := ord(c) - (ord('a') - 10);
  else
    Result := 0;
  end;
end;

function Unescape(const s: String): String;
var
  i, RealLength: Integer;
  P: PChar;
begin
  SetLength(Result, Length(s));
  i := 1;
  P := PChar(Result);  { use PChar to prevent numerous calls to UniqueString }
  RealLength := 0;
  while i <= Length(s) do
  begin
    if s[i] = '%' then
    begin
      P[RealLength] := Chr(HexValue(s[i + 1]) shl 4 or HexValue(s[i + 2]));
      Inc(i, 3);
    end else
    begin
      P[RealLength] := s[i];
      Inc(i);
    end;
    Inc(RealLength);
  end;
  SetLength(Result, RealLength);
end;

function ParseURI(const URI, DefaultProtocol: String; DefaultPort: Word):  TURI;
var
  s, Authority: String;
  i: Integer;
begin
  Result.Protocol := LowerCase(DefaultProtocol);
  Result.Port := DefaultPort;

  s := URI;

  // Extract scheme

  for i := 1 to Length(s) do
    if s[i] = ':' then
    begin
      Result.Protocol := Copy(s, 1, i - 1);
      s := Copy(s, i + 1, MaxInt);
      break;
    end
    else
      if not (((i=1) and (s[i] in ALPHA)) or (s[i] in ALPHA + DIGIT + ['+', '-', '.'])) then
        break;

  // Extract the bookmark

  i := LastDelimiter('#', s);
  if i > 0 then
  begin
    Result.Bookmark := Unescape(Copy(s, i + 1, MaxInt));
    s := Copy(s, 1, i - 1);
  end;

  // Extract the params

  i := LastDelimiter('?', s);
  if i > 0 then
  begin
    Result.Params := Unescape(Copy(s, i + 1, MaxInt));
    s := Copy(s, 1, i - 1);
  end;

  // extract authority

  if (Length(s) > 1) and (s[1] = '/') and (s[2] = '/') then
  begin
    i := 3;
    while (i <= Length(s)) and (s[i] <> '/') do
      Inc(i);
    Authority := Copy(s, 3, i-3);
    s := Copy(s, i, MaxInt);
    Result.HasAuthority := True;    // even if Authority is empty
  end
  else
  begin
    Result.HasAuthority := False;
    Authority := '';
  end;

  // now s is 'hier-part' per RFC3986
  // Extract the document name (nasty...)

  for i := Length(s) downto 1 do
    if s[i] = '/' then
    begin
      Result.Document := Unescape(Copy(s, i + 1, Length(s)));
      if (Result.Document <> '.') and (Result.Document <> '..') then
        s := Copy(s, 1, i)
      else
        Result.Document := '';
      break;
    end else if s[i] = ':' then
      break
    else if i = 1 then
    begin
      Result.Document := Unescape(s);
      if (Result.Document <> '.') and (Result.Document <> '..') then
        s := ''
      else
        Result.Document := '';
      // break - not needed, last iteration
    end;

  // Everything left is a path

  Result.Path := Unescape(s);

  // Extract the port number

  i := LastDelimiter(':@', Authority);
  if (i > 0) and (Authority[i] = ':') then
  begin
    Result.Port := StrToInt(Copy(Authority, i + 1, MaxInt));
    Authority := Copy(Authority, 1, i - 1);
  end;

  // Extract the hostname

  i := Pos('@', Authority);
  if i > 0 then
  begin
    Result.Host := Copy(Authority, i+1, MaxInt);
    Delete(Authority, i, MaxInt);

    // Extract username and password
    if Length(Authority) > 0 then
    begin
      i := Pos(':', Authority);
      if i = 0 then
        Result.Username := Authority
      else
      begin
        Result.Username := Copy(Authority, 1, i - 1);
        Result.Password := Copy(Authority, i + 1, MaxInt);
      end;
    end;
  end
  else
    Result.Host := Authority;
end;

procedure TFRE_HTTP_URL_DISPATCHER.DispatchRequest(const connection_object:TObject;const uri: string ; const method: TFRE_HTTP_PARSER_REQUEST_METHOD);
var i,j         : integer;
    found       : boolean;
    lUri        : String;
    lContent    : string;
    lparsedUri  : TURI;
    lFRE_URI    : TFRE_HTTP_URI;
    lStart,lEnd : integer;
    dispatchm   : TFRE_HTTP_ContentProvider;
begin
  lUri := uri;
  lparsedUri        := ParseURI('http://'+lUri,'http://',80);
  lFRE_URI.Document := lparsedUri.Document;
  lFRE_URI.Bookmark := lparsedUri.Bookmark;
  lFRE_URI.Host     := lparsedUri.Host;
  lFRE_URI.Port     := lparsedUri.Port;
  lFRE_URI.Params   := lparsedUri.Params;
  lFRE_URI.Protocol := lparsedUri.Protocol;
  lFRE_URI.Method   := method;
  if RPos('/',lparsedUri.Path) = length(lparsedUri.Path) then begin
    lparsedUri.Path:=Copy(lparsedUri.Path,1,Length(lparsedUri.Path)-1);
  end;
  GFRE_BT.SeperateString(lparsedUri.Path,'/',lFRE_URI.SplitPath);
  if lparsedUri.Path<>'' then begin
    if lparsedUri.Path[1]='/' then begin
      lFRE_URI.SplitPath := copy(lFRE_URI.SplitPath,1,maxint); // remove first separation
    end else begin
      GFRE_BT.CriticalAbort('relative uri ?');
    end;
  end else begin
    //writeln('NO PATH');
  end;
  for i := 0 to high(FDomains) do begin
    found:=true;
    if high(FDomains[i].path)>high(lFRE_URI.Splitpath) then continue;
    for j:=0 to high(FDomains[i].path) do begin
      if lFRE_URI.Splitpath[j]<>FDomains[i].path[j] then begin
        found := false;
      end;
    end;
    if found then begin
       lStart := high(FDomains[i].path)+1;
       lEnd   := high(lFRE_URI.Splitpath);
       if lStart<=lEnd then begin
         lFRE_URI.SplitPath := copy(lFRE_URI.SplitPath,lStart,lEnd);
       end else begin
         SetLength(lFRE_URI.SplitPath,0);
       end;
       dispatchm         := FDomains[i].method;
       TMethod(dispatchm).data := connection_object;
       dispatchm(lFRE_URI);
       exit; // Found break
    end;
  end;
  if assigned(FDefault.method) then begin
    dispatchm               := FDefault.method;
    TMethod(dispatchm).data := connection_object;
    dispatchm(lFRE_URI);
  end else begin
    raise EXception.Create('HTTP DISPATCHER / NO DEFAULT METHOD');
  end;
end;

{ TFRE_HTTP_COOKIE }


procedure TFRE_HTTP_COOKIE.SetDomain(const AValue: string);
begin
  FDomain:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetExpires(const AValue: TFRE_DB_DateTime64);
begin
  FExpires:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetFromString(const AValue: String);
var rest:String;
    searchval:string;
    spos:integer;
    epos:integer;
    ldate:string;
    lmaxage:string;

    function ExtractAttributeValue(const an:String):string;
    begin
      result := '';
      sPos := pos('; '+an+'=',searchval);
      if spos>0 then begin
        spos:=spos+length(an)+3;
        epos := PosEx(';',rest,spos);
        if epos>0 then begin
          result:=Copy(rest,spos,epos-spos);
        end;
      end;
    end;

    function IsAttributeSet(const an:String):boolean;
    begin
    end;

begin
  FKey:='';
  FVal:='';
  FMaxAge:=0;
  FExpires:=0;
  FDomain:='';
  FHttpOnly:=false;
  FSecure:=false;
  FPath:='';
  FKey := GFRE_BT.SepLeft(AValue,'=');
  rest := GFRE_BT.SepRight(AValue,'=');
  FVal := GFRE_BT.SepLeft(rest,';');
  if FVal='' then begin
    FVal:=rest;
    exit;
  end;
  rest := ';'+rest;
  searchval:=lowerCase(rest);
  if pos('secure',searchval)>0 then FSecure:=true;
  if pos('httponly',searchval)>0    then FHttpOnly:=true;
  FPath   := ExtractAttributeValue('path');
  FDomain := ExtractAttributeValue('domain');
  ldate   := ExtractAttributeValue('expires');
  if ldate<>'' then begin
    FExpires:=GFRE_DT.FromHttp(ldate);
  end;
  lmaxage := ExtractAttributeValue('max-age');
  if lmaxage<>'' then begin
    MaxAge:=StrToIntDef(lmaxage,-1);
  end;
end;

procedure TFRE_HTTP_COOKIE.SetHttpOnly(const AValue: boolean);
begin
  FHttpOnly:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetKey(const AValue: String);
begin
  if FKey=AValue then exit;
  FKey:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetMaxAge(const AValue: integer);
begin
  FMaxAge:=AValue;
end;


function TFRE_HTTP_COOKIE.GetAsString: String;
var val:RFOS_PROPERTY;
begin
  result:=FKey+'='+FVal;
  if FExpires<>0 then result := result + '; Expires='+GFRE_DT.ToStrHTTP(FExpires);
  if FMaxAge<>0  then result := result + '; Max-Age='+IntToStr(FMaxAge);
  if FPath<>''   then result := result + '; Path='+FPath;
  if FDomain<>'' then result := result + '; Domain='+FDomain;
  if FSecure     then result := result + '; Secure';
  if FHttpOnly   then result := result + '; HttpOnly';
end;


procedure TFRE_HTTP_COOKIE.SetPath(const AValue: string);
begin
  FPath:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetSecure(const AValue: boolean);
begin
  FSecure:=AValue;
end;

procedure TFRE_HTTP_COOKIE.SetVal(const AValue: String);
begin
  if FVal=AValue then exit;
  FVal:=AValue;
end;



constructor TFRE_HTTP_COOKIE.create;
begin
  FMaxAge:=0;
end;


end.

