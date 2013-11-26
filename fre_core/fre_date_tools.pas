unit fre_date_tools;

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
  Classes, SysUtils,strutils,upascaltz,upascaltz_types, FOS_TOOL_INTERFACES,zstream;

  //3.3.1 Full Date
  //
  //HTTP applications have historically allowed three different formats for the representation of date/time stamps:
  //
  //      Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
  //      Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
  //      Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
  //
  //The first format is preferred as an Internet standard and represents a fixed-length subset of that defined by RFC 1123 [8] (an update to RFC 822 [9]). The second format is in common use, but is based on the obsolete RFC 850 [12] date format and lacks a four-digit year. HTTP/1.1 clients and servers that parse the date value MUST accept all three formats (for compatibility with HTTP/1.0), though they MUST only generate the RFC 1123 format for representing HTTP-date values in header fields. See section 19.3 for further information.
  //
  //      Note: Recipients of date values are encouraged to be robust in
  //      accepting date values that may have been sent by non-HTTP
  //      applications, as is sometimes the case when retrieving or posting
  //      messages via proxies/gateways to SMTP or NNTP.
  //
  //All HTTP date/time stamps MUST be represented in Greenwich Mean Time (GMT), without exception. For the purposes of HTTP, GMT is exactly equal to UTC (Coordinated Universal Time). This is indicated in the first two formats by the inclusion of "GMT" as the three-letter abbreviation for time zone, and MUST be assumed when reading the asctime format. HTTP-date is case sensitive and MUST NOT include additional LWS beyond that specifically included as SP in the grammar.
  //
  //       HTTP-date    = rfc1123-date | rfc850-date | asctime-date
  //       rfc1123-date = wkday "," SP date1 SP time SP "GMT"
  //       rfc850-date  = weekday "," SP date2 SP time SP "GMT"
  //       asctime-date = wkday SP date3 SP time SP 4DIGIT
  //       date1        = 2DIGIT SP month SP 4DIGIT
  //                      ; day month year (e.g., 02 Jun 1982)
  //       date2        = 2DIGIT "-" month "-" 2DIGIT
  //                      ; day-month-year (e.g., 02-Jun-82)
  //       date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
  //                      ; month day (e.g., Jun  2)
  //       time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
  //                      ; 00:00:00 - 23:59:59
  //       wkday        = "Mon" | "Tue" | "Wed"
  //                    | "Thu" | "Fri" | "Sat" | "Sun"
  //       weekday      = "Monday" | "Tuesday" | "Wednesday"
  //                    | "Thursday" | "Friday" | "Saturday" | "Sunday"
  //       month        = "Jan" | "Feb" | "Mar" | "Apr"
  //                    | "May" | "Jun" | "Jul" | "Aug"
  //                    | "Sep" | "Oct" | "Nov" | "Dec"
  //
  //      Note: HTTP requirements for the date/time stamp format apply only
  //      to their usage within the protocol stream. Clients and servers are
  //      not required to use these formats for user presentation, request
  //      logging, etc.

  procedure Get_FOS_DateTools (out dt : IFOS_DATETOOLS ; var holder:TObject);

implementation

//  tzt.ParseDatabaseFromFile('fos_tz.txt');
//  GFRE_BT.StringToFile('CRES_FOS_TIMEZONES.pp',tzt.SaveTZInternal);
//  exit;
  //tzt.ResourceLoad;
  //tzt.RDump;
  //tzt.ZDump;

uses CRES_FOS_TIMEZONES,dateutils;

const
  UnixStartDate: TDateTime = 25569.0;
  CFRE_DT_WKDAY   : array[1..7]  of string = ('Mon','Tue','Wed','Thu','Fri','Sat','Sun');
  CFRE_DT_WEEKDAY : array[1..7]  of string = ('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday');
  CFRE_DT_MONTH   : array[1..12] of string = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');

type
  { TFRE_TZ_TOOLS }

  TFRE_TZ_TOOLS=class(TPascalTZ,IFOS_DATETOOLS)
  private
    frefcount : longint;
    procedure _DT2TZDT(const dbdate:TFRE_DB_DateTime64;var result:TTZDateTime);
  public
    function  SaveTZInternal:string;
    procedure ResourceLoad;
    procedure RDump;
    procedure ZDump;
//"float" datetime
    function  DateTimeToDBDateTime64 (const ADateTime: TDatetime): TFRE_DB_DateTime64;
    function  DBDateTime64ToDateTime (const dbdate      : TFRE_DB_DateTime64): TDateTime;
//FOS Datetime
    function  LocalTimeToUTC         (const ADateTime64 : TFRE_DB_DateTime64;const FLocalZone:string): TFRE_DB_DateTime64;
    function  UTCToLocalTime         (const ADateTime64 : TFRE_DB_DateTime64;const FLocalZone:string): TFRE_DB_DateTime64;
    function  ToStrFOS               (const dt: TFRE_DB_DateTime64): string;
    function  ToStrUTC               (const dt: TFRE_DB_DateTime64): string;
    function  ToStrHttp              (const dt: TFRE_DB_DateTime64): string;
    function  FromHttp               (const datestring:String):TFRE_DB_DateTime64;
    function  WeekDayOfDT            (const dt: TFRE_DB_DateTime64):integer; // 1=Mon .. 7=Sun
    function  Now_UTC                :TFRE_DB_DateTime64;
    procedure DecodeTime             (const date:TFRE_DB_DateTime64;var year,month,day,hour,minute,second,millisecond:longint);
    function  EncodeTime             (const year,month,day,hour,minute,second,millisecond:longint):TFRE_DB_DateTime64;
    procedure ForAllTimeZones        (const iter: TFRE_TZ_Iterator; const only_geozones: boolean);
  end;

Const
{Date Translation}
  C1970= 2440588;
  D0   =    1461;
  D1   =  146097;
  D2   = 1721119;

//TODO -> verify
Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:longint);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp      := ((JulianDN-D2) shl 2)-1;
  JulianDN  := Temp Div D1;
  XYear     := (Temp Mod D1) or 3;
  YYear     := (XYear Div D0);
  Temp      := ((((XYear mod D0)+4) shr 2)*5)-3;
  Day       := ((Temp Mod 153)+5) Div 5;
  TempMonth :=Temp Div 153;
  if TempMonth>=10 then begin
    inc(YYear);
    dec(TempMonth,12);
  end;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;

function GregorianToJulian(Year,Month,Day:Longint):LongInt;
var Century,XYear: LongInt;
Begin
  If Month<=2 then begin
     Dec(Year);
     Inc(Month,12);
  end;
  Dec(Month,3);
  Century:=(longint(Year Div 100)*D1) shr 2;
  XYear:=(longint(Year Mod 100)*D0) shr 2;
  result := ((((Month*153)+2) div 5)+Day)+D2+XYear+Century;
End;


{ TFRE_TZ_TOOLS }

procedure TFRE_TZ_TOOLS._DT2TZDT(const dbdate: TFRE_DB_DateTime64; var result:TTZDateTime);
var y,d,m : longint;
    epoch   : longint;
    datenum : longint;
begin
  epoch             := dbdate div 1000;
  result.SecsInDay  := Abs(Epoch Mod 86400);
  datenum           := (epoch Div 86400) + c1970;
  JulianToGregorian (datenum,y,m,d);
  result.Year       := y;
  result.Day        := d;
  result.Month      := m;
end;

function TFRE_TZ_TOOLS.SaveTZInternal:string;
var cs       : Tcompressionstream;
    ssm      : TStringStream;
    i        : integer;
begin
  ssm:=TStringStream.Create('');
  cs:=Tcompressionstream.Create(clmax,ssm);
  cs.WriteDWord(length(FRules));
  cs.WriteDWord(length(FZones));
  for i:=0 to high(FRules) do begin
    cs.Write(FRules[i],SizeOf(TTZRules));
  end;
  for i:=0 to high(FZones) do begin
    cs.Write(FZones[i],SizeOf(TTzZone));
  end;
  cs.flush;
  cs.Free;
  result  := 'unit CRES_FOS_TIMEZONES; {$mode objfpc}{$H+} interface const CRES_TIMEZONES : string='''+GFRE_BT.Str2HexStr(ssm.DataString)+''';implementation begin end. ';
  writeln('SZ ',length(result));
end;

procedure TFRE_TZ_TOOLS.ResourceLoad;
var ssm,ssm2:TStringStream;
    ds :Tdecompressionstream;
    i  :integer;
begin
  ssm  := TStringStream.Create(GFRE_BT.HexStr2Str(CRES_TIMEZONES));
  ds   := Tdecompressionstream.create(ssm);
  ssm2 := TStringStream.Create('');
  ssm2.CopyFrom(ds,0);
  ssm.Free;
  ds.Free;
  ssm2.Position:=0;
  SetLength(FRules,ssm2.ReadDWord);
  SetLength(FZones,ssm2.ReadDWord);
  for i:=0 to high(FRules) do begin
    ssm2.Read(FRules[i],SizeOf(TTZRules));
  end;
  for i:=0 to high(FZones) do begin
    ssm2.Read(FZones[i],SizeOf(TTzZone));
  end;
  ssm2.Free;
end;

procedure TFRE_TZ_TOOLS.RDump;
var i:integer;
begin
  for i:=0 to high(FRules) do begin
    writeln(i,' : ',FRules[i].Name,' ',FRules[i].OnRule,' ',FRules[i].TimeZoneLetters,' ');
  end;
end;

procedure TFRE_TZ_TOOLS.ZDump;
var i:integer;
begin
  for i:=0 to high(FZones) do begin
    writeln(i,' : ',FZones[i].Name,' ',FZones[i].Offset,' ',FZones[i].TimeZoneLetters);
  end;
end;



function TFRE_TZ_TOOLS.DateTimeToDBDateTime64(const ADateTime: TDatetime): TFRE_DB_DateTime64;
begin
 result:=(Round((ADateTime-UnixStartDate)*86400)*1000)+MilliSecondOf(ADateTime);
end;


function TFRE_TZ_TOOLS.DBDateTime64ToDateTime(const dbdate: TFRE_DB_DateTime64): TDateTime;
var lmsec:word;
    lsec:int64;
begin
 lmsec:=dbdate mod 1000;
 lsec :=dbdate div 1000;
 result:=(lsec / 86400) + UnixStartDate;
 result:=IncMilliSecond(result,lmsec);
// writeln('Lsec:',lsec,'Lmsec',lmsec,'Res:',result,' ',DateTimeToStr(result));
end;

function TFRE_TZ_TOOLS.LocalTimeToUTC(const ADateTime64: TFRE_DB_DateTime64;const FLocalZone:string): TFRE_DB_DateTime64;
var t1,t2           : TTZDateTime;
    y,mo,d,h,m,s,ms : longint;
begin
  ms := ADateTime64 mod 1000;
  _DT2TZDT(ADateTime64,t1);
  t2           := LocalTimeToGMT(t1,FLocalZone);
  y            := t2.Year;
  d            := t2.Day;
  mo           := t2.Month;
  h            := t2.SecsInDay div 3600;
  s            := t2.SecsInDay mod 60;
  m            := (t2.SecsInDay - (h*3600) -s) div 60;
  result       := EncodeTime(y,mo,d,h,m,s,ms);
end;

function TFRE_TZ_TOOLS.UTCToLocalTime(const ADateTime64: TFRE_DB_DateTime64; const FLocalZone: string): TFRE_DB_DateTime64;
var t1,t2           : TTZDateTime;
    y,mo,d,h,m,s,ms : longint;
    FTZN            : string;
begin
  ms := ADateTime64 mod 1000;
  _DT2TZDT(ADateTime64,t1);
  t2           := GMTToLocalTime(t1,FLocalZone,FTZN);
  y            := t2.Year;
  d            := t2.Day;
  mo           := t2.Month;
  h            := t2.SecsInDay div 3600;
  s            := t2.SecsInDay mod 60;
  m            := (t2.SecsInDay - (h*3600) -s) div 60;
  result       := EncodeTime(y,mo,d,h,m,s,ms);
end;

function TFRE_TZ_TOOLS.ToStrFOS(const dt: TFRE_DB_DateTime64): string;
var y,mo,d,h,m,s,ms : longint;
begin
  DecodeTime(dt,y,mo,d,h,m,s,ms);
  result := Format('%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d:%3.3d',[y,mo,d,h,m,s,ms]);
end;

function TFRE_TZ_TOOLS.ToStrUTC(const dt: TFRE_DB_DateTime64): string;
var y,mo,d,h,m,s,ms : longint;
begin
  DecodeTime(dt,y,mo,d,h,m,s,ms);
  result := Format('%4.4d-%2.2d-%2.2d %2.2d:%2.2d:%2.2d',[y,mo,d,h,m,s]);
end;


function TFRE_TZ_TOOLS.WeekDayOfDT(const dt: TFRE_DB_DateTime64): integer;
var t1 : TTZDateTime;
begin
  _DT2TZDT(dt,t1);
  result := ((ord(WeekDayOf(t1))+5) mod 7)+1;
end;

function TFRE_TZ_TOOLS.ToStrHTTP(const dt: TFRE_DB_DateTime64): string;
var y,mo,d,h,m,s,ms : longint;
    t1 : TTZDateTime;
begin
  DecodeTime(dt,y,mo,d,h,m,s,ms);
  t1.Year      := y;
  t1.Day       := d;
  t1.Month     := mo;
  t1.SecsInDay := h*3600+m*60+s;
  result := CFRE_DT_WKDAY[((ord(WeekDayOf(t1))+5) mod 7)+1]+', ';
  result := result+Format('%2.2d %s %4.4d ',[d,CFRE_DT_MONTH[mo],y]);
  result := result+Format('%2.2d:%2.2d:%2.2d GMT',[h,m,s]);
end;

function TFRE_TZ_TOOLS.FromHttp(const datestring: String): TFRE_DB_DateTime64;
var
     dd       : integer;
     mo       : integer;
     yr       : integer;
     hr       : integer;
     mi       : integer;
     se       : integer;

  procedure _check_valid(const typ:string);
  begin
    if (dd<1) or (dd>31) then raise Exception.Create('invalid '+typ+' date days not between 1-31');
    if (hr<0) or (hr>23) then raise Exception.Create('invalid '+typ+' date hours not between 0-23');
    if (mi<0) or (mi>59) then raise Exception.Create('invalid '+typ+' date minutes not between 0-59');
    if (se<0) or (se>59) then raise Exception.Create('invalid '+typ+' date seconds not between 0-59');
  end;

  procedure _rfc850; //      Sunday, 06-Nov-94 08:49:37 GMT ; RFC 850, obsoleted by RFC 1036
  var parse,rest : string;
      i        : integer;
      wkday    : integer;
  begin
    parse   := GFRE_BT.SepLeft(datestring,',');
    rest    := GFRE_BT.SepRight(datestring,',');
    wkday   :=-1;
    for i:= 1 to 7 do begin
      if parse=CFRE_DT_WEEKDAY[i] then begin
        wkday:=i;
        break;
      end;
    end;
    if wkday=-1 then raise Exception.Create('invalid rfc850 date / no weekday found ['+parse+']');
    parse   := trim(GFRE_BT.SepLeft(rest,'-'));
    rest    := GFRE_BT.SepRight(rest,'-');
    dd      := StrToIntDef(parse,-1);
    if dd=-1 then raise Exception.Create('invalid rfc850 date / no day found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,'-');
    rest    := GFRE_BT.SepRight(rest,'-');
    mo   :=-1;
    for i:= 1 to 12 do begin
      if parse=CFRE_DT_MONTH[i] then begin
        mo:=i;
        break;
      end;
    end;
    if mo=-1 then raise Exception.Create('invalid rfc850 date / no month found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    yr      := StrToIntDef(parse,-1);
    if yr=-1 then raise Exception.Create('invalid rfc850 date / no year found ['+parse+']');
    yr     := 1900+yr;
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    hr      := StrToIntDef(parse,-1);
    if hr=-1 then raise Exception.Create('invalid rfc850 date / no hour found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    mi      := StrToIntDef(parse,-1);
    if mi=-1 then raise Exception.Create('invalid rfc850 date / no minute found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    se      := StrToIntDef(parse,-1);
    if se=-1 then raise Exception.Create('invalid rfc850 date / no second found ['+parse+']');
    if rest<>'GMT' then raise Exception.Create('invalid rfc850 date / expected " GMT" but found ['+rest+']');
    _check_valid('rfc850');
    result := EncodeTime(yr,mo,dd,hr,mi,se,0);
  end;

  procedure _rfc1123; //      Sun, 06 Nov 1994 08:49:37 GMT  ; RFC 822, updated by RFC 1123
  var parse,rest : string;
      i        : integer;
      wkday    : integer;
  begin
    parse   := GFRE_BT.SepLeft(datestring,',');
    rest    := GFRE_BT.SepRight(datestring,',');
    wkday   :=-1;
    for i:= 1 to 7 do begin
      if parse=CFRE_DT_WKDAY[i] then begin
        wkday:=i;
        break;
      end;
    end;
    if wkday=-1 then raise Exception.Create('invalid rfc1123 date / no weekday found ['+parse+']');
    parse   := GFRE_BT.SepLeft(trim(rest),' ');
    rest    := GFRE_BT.SepRight(trim(rest),' ');
    dd      := StrToIntDef(parse,-1);
    if dd=-1 then raise Exception.Create('invalid rfc1123 date / no day found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    mo   :=-1;
    for i:= 1 to 12 do begin
      if parse=CFRE_DT_MONTH[i] then begin
        mo:=i;
        break;
      end;
    end;
    if mo=-1 then raise Exception.Create('invalid rfc1123 date / no month found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    yr      := StrToIntDef(parse,-1);
    if yr=-1 then raise Exception.Create('invalid rfc1123 date / no year found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    hr      := StrToIntDef(parse,-1);
    if hr=-1 then raise Exception.Create('invalid rfc1123 date / no hour found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    mi      := StrToIntDef(parse,-1);
    if mi=-1 then raise Exception.Create('invalid rfc1123 date / no minute found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    se      := StrToIntDef(parse,-1);
    if se=-1 then raise Exception.Create('invalid rfc1123 date / no second found ['+parse+']');
    if rest<>'GMT' then raise Exception.Create('invalid rfc1123 date / expected " GMT" but found ['+rest+']');
    _check_valid('rfc1123');
    result := EncodeTime(yr,mo,dd,hr,mi,se,0);
  end;

  procedure _ansi_c; //      Sun Nov  6 08:49:37 1994       ; ANSI C's asctime() format
  var parse,rest : string;
      i        : integer;
      wkday    : integer;
  begin
    parse   := GFRE_BT.SepLeft(datestring,' ');
    rest    := GFRE_BT.SepRight(datestring,' ');
    wkday   :=-1;
    for i:= 1 to 7 do begin
      if parse=CFRE_DT_WKDAY[i] then begin
        wkday:=i;
        break;
      end;
    end;
    if wkday=-1 then raise Exception.Create('invalid ansi c date / no weekday found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    mo   :=-1;
    for i:= 1 to 12 do begin
      if parse=CFRE_DT_MONTH[i] then begin
        mo:=i;
        break;
      end;
    end;
    if mo=-1 then raise Exception.Create('invalid ansi c date / no month found ['+parse+']');
    parse   := GFRE_BT.SepLeft(trim(rest),' ');
    rest    := GFRE_BT.SepRight(trim(rest),' ');
    dd      := StrToIntDef(parse,-1);
    if dd=-1 then raise Exception.Create('invalid ansi c date / no day found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    hr      := StrToIntDef(parse,-1);
    if hr=-1 then raise Exception.Create('invalid ansi c date / no hour found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,':');
    rest    := GFRE_BT.SepRight(rest,':');
    mi      := StrToIntDef(parse,-1);
    if mi=-1 then raise Exception.Create('invalid ansi c date / no minute found ['+parse+']');
    parse   := GFRE_BT.SepLeft(rest,' ');
    rest    := GFRE_BT.SepRight(rest,' ');
    se      := StrToIntDef(parse,-1);
    if se=-1 then raise Exception.Create('invalid ansi c date / no second found ['+parse+']');
    yr      := StrToIntDef(rest,-1);
    if yr=-1 then raise Exception.Create('invalid ansi c date / no year found ['+parse+']');
    _check_valid('ansi c');
    result := EncodeTime(yr,mo,dd,hr,mi,se,0);
  end;

begin
  if pos('GMT',datestring)>0 then begin
    if pos('-',datestring)>0 then begin
      _rfc850;
    end else begin
      _rfc1123;
    end;
  end else begin
    _ansi_c;
  end;
end;

//       rfc1123-date = wkday "," SP date1 SP time SP "GMT"
//       rfc850-date  = weekday "," SP date2 SP time SP "GMT"
//       asctime-date = wkday SP date3 SP time SP 4DIGIT
//       date1        = 2DIGIT SP month SP 4DIGIT
//                      ; day month year (e.g., 02 Jun 1982)
//       date2        = 2DIGIT "-" month "-" 2DIGIT
//                      ; day-month-year (e.g., 02-Jun-82)
//       date3        = month SP ( 2DIGIT | ( SP 1DIGIT ))
//                      ; month day (e.g., Jun  2)
//       time         = 2DIGIT ":" 2DIGIT ":" 2DIGIT
//                      ; 00:00:00 - 23:59:59
//       wkday        = "Mon" | "Tue" | "Wed"
//                    | "Thu" | "Fri" | "Sat" | "Sun"

function TFRE_TZ_TOOLS.Now_UTC: TFRE_DB_DateTime64;
begin
  result := GFRE_BT.Get_DBTimeNow;
end;

procedure TFRE_TZ_TOOLS.DecodeTime(const date: TFRE_DB_DateTime64; var year, month, day, hour,minute, second, millisecond: longint);
var  DateNum : LongInt;
     epoch   : longint;
begin
  epoch        := date div 1000;
  Datenum      := (Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,year,month,Day);
  Epoch        := Abs(Epoch Mod 86400);
  Hour         := Epoch Div 3600;
  Epoch        := Epoch Mod 3600;
  Minute       := Epoch Div 60;
  Second       := Epoch Mod 60;
  millisecond  := date mod 1000;
end;

function TFRE_TZ_TOOLS.EncodeTime(const year, month, day, hour, minute, second, millisecond: longint): TFRE_DB_DateTime64;
begin
  result:= ((int64(GregorianToJulian(Year,Month,Day)-c1970)*86400)+(Hour*3600)+(Minute*60)+Second)*1000+millisecond;
end;

procedure TFRE_TZ_TOOLS.ForAllTimeZones(const iter: TFRE_TZ_Iterator ; const only_geozones:boolean);
var i : NativeInt;
   LT : AnsiString;
begin
  LT := '';
  for i := 0 to High(FZones) do
    if FZones[i].Name<>LT then
      begin
        LT:=FZones[i].Name;
        if only_geozones then
          begin
            if Pos('/',LT)>0 then
              with Fzones[i] do
                with RuleValidUntil do
                  iter(trim(Name),trim(TimeZoneLetters),Offset,RuleFixedOffset,Year,Month,Day,SecsInDay,RuleValidUntilGMT);
          end
        else
          begin
            with Fzones[i] do
              with RuleValidUntil do
                iter(Name,TimeZoneLetters,Offset,RuleFixedOffset,Year,Month,Day,SecsInDay,RuleValidUntilGMT);
          end;
      end;
end;


procedure Get_FOS_DateTools (out dt : IFOS_DATETOOLS ; var holder:TObject);
begin
  holder := TFRE_TZ_TOOLS.Create;
  TFRE_TZ_TOOLS(holder).ResourceLoad;
  dt := TFRE_TZ_TOOLS(holder);
end;

initialization

finalization


end.
h
