unit test_fos_nps;

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

uses SysUtils, FOS_NPS, dateutils, Classes,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,paszlib;

procedure c_test_nps;

implementation

function MyPower(const base, exp: longword): QWord;
var
  i: cardinal;
begin
  if exp = 0 then
  begin
    Result := 1;
    exit;
  end;
  Result := base;
  for i := 2 to exp do
  begin
    Result := qword(Result) * base;
  end;
end;


procedure Assign_Types(nps: IFOS_Nps;const nonest:boolean=false);
var
  PS, pps: IFOS_NPS;
  str:     TStringstream;
  i, j:    integer;
  ba       :Array of Boolean;
  bya      :Array of Byte;
  sma      :Array of ShortInt;
  ssma     :Array of SmallInt;
  wa       :Array of word;
  ca       :Array of Longword;
  li       :Array of Longint;
  i64a     :array of int64;
  qwa      :array of QWord;
  sia      :array of single;
  dua      :array of double;
  cura     :array of Currency;
  stra     :array of string;

begin
  nps.SetPropNull('vNull');
  nps.SetProp('vString', 'FirmOS_Test_String');
  nps.SetPropCurrency('vCurrency', -922337203685477.5807);
  nps.SetPropDouble('vDouble', 1.12345678901234567890);
  nps.SetProp('vSingle', 1.12345678901234567890);
  nps.SetProp('vQWord', MyPower(2, 63));
  nps.SetProp('vInt64', -9223372036854775808);
  nps.SetProp('vLongWord', 4294967295);
  nps.SetProp('vLongInt', -2147483648);
  nps.SetProp('vWord', 65535);
  nps.SetProp('vSmallInt', -32768);
  nps.SetProp('vShortInt', -127);
  nps.SetProp('vByte', 255);
  nps.SetProp('vBoolean', True);
  str := TStringStream.Create('A');
  nps.SetPropObj('vObject', str);
  nps.SetPropDateTime('vDate', EncodeDateTime(1973, 04, 23, 02, 10, 10, 00));
  str := TStringStream.Create('FirmOS_BusinessSolutions');
  nps.SetPropStream('vStream', str);
  str.Free;
  SetLength(ba,4);ba[0]:=true;ba[1]:=False;ba[2]:=true;ba[3]:=False;nps.SetProp('vBoolArray',ba);
  SetLength(bya,5);bya[0]:=1;bya[1]:=2;bya[2]:=3;bya[3]:=4;bya[4]:=5;nps.SetProp('vByteArray',bya);
  SetLength(sma,5);sma[0]:=1;sma[1]:=-2;sma[2]:=-3;sma[3]:=-4;sma[4]:=-5;nps.SetProp('vShortIntArray',sma);
  SetLength(ssma,5);ssma[0]:=-10;ssma[1]:=-20;ssma[2]:=-300;ssma[3]:=400;ssma[4]:=500;nps.SetProp('vSmallIntArray',ssma);
  SetLength(wa,5);wa[0]:=1000;wa[1]:=2000;wa[2]:=3000;wa[3]:=4000;wa[4]:=5000;nps.SetProp('vWordArray',wa);
  SetLength(ca,5);ca[0]:=1;ca[1]:=2;ca[2]:=3;ca[3]:=4;ca[4]:=5;nps.SetProp('vCardinalArray',ca);
  SetLength(li,5);li[0]:=1;li[1]:=2;li[2]:=3;li[3]:=4;li[4]:=5;nps.SetProp('vLongintArray',li);
  SetLength(i64a,5);i64a[0]:=-1;i64a[1]:=2;i64a[2]:=-3;i64a[3]:=4;i64a[4]:=-5;nps.SetProp('vInt64Array',i64a);
  SetLength(qwa,5);qwa[0]:=1;qwa[1]:=2;qwa[2]:=3;qwa[3]:=4;qwa[4]:=5;nps.SetProp('vQWordArray',qwa);
  SetLength(sia,5);sia[0]:=1.1;sia[1]:=2.1;sia[2]:=3.1;sia[3]:=4.1;sia[4]:=5.1;nps.SetProp('vSingleArray',sia);
  SetLength(dua,5);dua[0]:=1.12;dua[1]:=2.13;dua[2]:=3.14;dua[3]:=4.15;dua[4]:=5.16;nps.SetProp('vDoubleArray',dua);
  SetLength(cura,5);cura[0]:=120.22;cura[1]:=2.12;cura[2]:=3.14;cura[3]:=4.14;cura[4]:=5.99;nps.SetProp('vCurrArray',cura);
  SetLength(stra,5);stra[0]:='S1';stra[1]:='s2';stra[2]:='S3';stra[3]:='s4';stra[4]:='S5 END';nps.SetProp('vStringArray',stra);
  if nonest then exit;
  ps := Get_IFOS_NPS;
  Assign_Types(ps,true);
  nps.SetPropNPS('vNPS-1',ps);
  ps := Get_IFOS_NPS;
  Assign_Types(ps,true);
  nps.SetPropNPS('vNPS-2', ps);
  for i := 1 to 12 do begin
    ps := Get_IFOS_NPS;
    ps.Setprop('ITEM_' + IntToStr(i), i);
    if i <= 6 then begin
      nps.AddPropListPS('LIST_A', ps);
      for j := 1 to 6 do begin
        pps := Get_IFOS_NPS;
        PPS.Setprop('SUBITEM_' + IntToStr(i) + '_' + IntToStr(j), (i-1)*6+j);
        ps.SetPropNPS('ITEM_PS_' + IntToStr(j), PPS);
        ps := pps; // Down
      end;
    end else begin
      nps.AddPropListPS('LIST_B', ps);
      for j := 1 to 6 do begin
        pps := Get_IFOS_NPS;
        PPS.Setprop('SUBITEM_' + IntToStr(j), (i-1)*6+j);
        ps.AddPropListPS('ITEM_PS_LIST', PPS);
      end;
    end;
  end;
end;

procedure Test_Types(nps: IFOS_NPS);
var
  R:   RFOS_PROPERTY;
  pt:  TFOS_PropType;
  s:   Ansistring;
  c:   currency;
  d:   double;
  si:  single;
  qw:  QWord;
  i64: int64;
  lw:  longword;
  li:  longint;
  w:   word;
  smi: smallint;
  shi: shortint;
  by:  byte;
  bo:  boolean;
  da:  TDAtetime;
  str: TStringstream;
  ps:  IFOS_NPS;
  ba       :Array of Boolean;
  bya      :Array of Byte;
  sma      :Array of ShortInt;
  ssma     :Array of SmallInt;
  wa       :Array of word;
  ia       :Array of integer;
  ca       :Array of Cardinal;
  lia      :Array of Longint;
  i64a     :array of int64;
  qwa      :array of QWord;
  sia      :array of single;
  dua      :array of double;
  cura     :array of Currency;
  stra     :array of string;


begin
  nps.GetProp('vNull', R);
  if R.PropType <> ptNull then raise Exception.Create('NULL ASSIGNMENT FAILURE');
  pt := nps.GetProp('vString', s);
  if (pt <> ptString) or (s <> 'FirmOS_Test_String') then raise Exception.Create('STRING ASSIGNMENT FAILURE');
  pt := nps.GetPropCurrency('vCurrency', c);
  if (pt <> ptCurrency) or (c <> -922337203685477.5807) then raise Exception.Create('CURRENCY ASSIGNMENT FAILURE');
  pt := nps.GetPropDouble('vDouble', d);
  if (pt <> ptDouble) or (d - 1.12345678901234567890 > 1e-16) then raise Exception.Create('DOUBLE ASSIGNMENT FAILURE');
  pt := nps.GetProp('vSingle', si);
  if (pt <> ptSingle) or (si - 1.12345678901234567890 > 1e-7) then
    raise Exception.Create('SINGLE ASSIGNMENT FAILURE');
  pt := nps.GetProp('vQWord', qw);
  if (pt <> ptQWord) or (qw <> 9223372036854775808) then
    raise Exception.Create('QWORD ASSIGNMENT FAILURE');
  pt := nps.GetProp('vInt64', i64);
  if (pt <> ptInt64) or (i64 <> -9223372036854775808) then
    raise Exception.Create('INT64 ASSIGNMENT FAILURE');
  pt := nps.GetProp('vLongWord', lw);
  if (pt <> ptLongword) or (lw <> 4294967295) then
    raise Exception.Create('LONGWORD ASSIGNMENT FAILURE');
  pt := nps.GetProp('vLongInt', li);
  if (pt <> ptLongInt) or (li <> -2147483648) then
    raise Exception.Create('LONGINT ASSIGNMENT FAILURE');
  pt := nps.GetProp('vWord', w);
  if (pt <> ptWord) or (w <> 65535) then
    raise Exception.Create('LONGINT ASSIGNMENT FAILURE');
  pt := nps.GetProp('vSmallInt', smi);
  if (pt <> ptSmallInt) or (smi <> -32768) then
    raise Exception.Create('SMALLINT ASSIGNMENT FAILURE');
  pt := nps.GetProp('vShortInt', shi);
  if (pt <> ptShortInt) or (shi <> -127) then
    raise Exception.Create('SHORTINT ASSIGNMENT FAILURE');
  pt := nps.GetProp('vByte', by);
  if (pt <> ptByte) or (by <> 255) then
    raise Exception.Create('BYTE ASSIGNMENT FAILURE');
  pt := nps.GetProp('vBoolean', bo);
  if (pt <> ptBool) or (bo <> True) then
    raise Exception.Create('BOOLEAN ASSIGNMENT FAILURE');
  pt := nps.GetPropDateTime('vDate', da);
  if (pt <> ptDateTime) or (abs(da - 26777.090394) < 1E-7) then
    raise Exception.Create('DATETIME ASSIGNMENT FAILURE');
  str := TStringStream.Create('');
  nps.GetPropStream('vStream', TStream(str));
  s := str.DataString;
  if S <> 'FirmOS_BusinessSolutions' then
    raise Exception.Create('STREAM ASSIGNMENT FAILURE <'+s+'>');
  str.Free;
  pt := NPS.CheckOutNPS('vNPS', ps);
  if (pt <> ptPS) and (pt <> ptNotFound) then
    raise Exception.Create('NPS ASSIGNMENT FAILURE');

  if nps.GetProp('vBoolArray',ba)<>ptBoolArray then raise Exception.Create('array assignment failure (bool array) type mismatch');
  if (length(ba)<>4) then raise Exception.Create('array assignment failure (bool array) length mismatch');
  if (ba[0]<>true) or (ba[1]<>false) or (ba[2]<>true) or (ba[3]<>false) then raise Exception.Create('array assignment failure (bool array) value mismatch');

  //if nps.GetProp('vStringArray',stra)<>ptStringArray then raise Exception.Create('array assignment failure (string array) type mismatch');
  //if (length(stra)<>5) then raise Exception.Create('array assignment failure (string array) length mismatch');
  //if (stra[0]<>'S1') or (stra[1]<>'s2') or (stra[2]<>'S3') or (stra[3]<>'s4') or (stra[4]<>'S5 END') then raise Exception.Create('array assignment failure (string array) value mismatch');

  if assigned(ps) then  begin
    Test_Types(ps);
  end;
end;

procedure c_test_nps;
var
  NPS,NNPS: IFOS_NPS;
  sl:  TStringList;
   f:  TFilestream;

 procedure DumpPS(const ps:IFOS_NPS);
 begin
  sl := TStringList.Create;
  ps.Dump(sl);
  writeln(sl.Text);
  sl.Free;
 end;

 var   x:TObject;
     wps:IFOS_NPS;
begin
  writeln('--');
  writeln('Sizeof PROPREC ',sizeof(RFOS_PROPERTY));
  writeln('--');
  NPS := Get_IFOS_NPS;

  Assign_Types(nps);
  Test_Types(nps);
  dumpps(nps);

  nps.GetPropNPS('vNPS-1',wps);
  if assigned(wps) then begin
    wps.GetPropObj('vObject',x);x.Free;wps.DelProp('vObject');
    wps:=nil;
    writeln('DEL vNPS-1');
    nps.DelProp('vNPS-1');
  end;
  nps.GetPropNPS('vNPS-2',wps);
  if assigned(wps) then begin
    wps.GetPropObj('vObject',x);
    x.Free;wps.DelProp('vObject');
    writeln('DEL vNPS-2');
    nps.DelProp('vNPS-2');
  end;
  nps.GetPropObj('vObject',x);x.Free;nps.DelProp('vObject');
  f:=TFileStream.Create('tet.nps',fmCreate);
  NPS.Compression:=ncl_MAX;
  NPS.SavetoStream(f,'SAVE','SEC');
  f.free;
  nps:=nil;
  NNPS:= Get_IFOS_NPS;
  f:=TFileStream.Create('tet.nps',fmOpenRead);
  f.Position:=0;
  NNPS.LoadfromStream(f,'SAVE','SEC');
  f.free;
  writeln('STREAM RELOAD OK');
  writeln;writeln;
  dumpps(nnps);
  Test_Types(nnps);
end;

end.

