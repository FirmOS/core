unit fos_strutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,strutils;

function FOS_AnsiContainsText(const AText, ASubText: string): Boolean;inline;
function FOS_AnsiStartsText(const ASubText, AText: string): Boolean;inline;
function FOS_AnsiEndsText(const ASubText, AText: string): Boolean;inline;
function FOS_AnsiReplaceText(const AText, AFromText, AToText: string): string;inline;
Function FOS_PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
Function FOS_PosEx(const SubStr, S: string): Integer;inline; // Offset: Cardinal = 1
Function FOS_PosEx(c:char; const S: string; Offset: Cardinal): Integer;



implementation

function FOS_AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  result := AnsiContainsText(atext,asubtext);
end;

function FOS_AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  result := AnsiStartsText(ASubText, AText);
end;

function FOS_AnsiEndsText(const ASubText, AText: string): Boolean;
begin
  result := AnsiEndsText(ASubText, AText);
end;

function FOS_AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  result := AnsiReplaceText(AText, AFromText, AToText);
end;

function FOS_PosEx(const SubStr, S: string; Offset: Cardinal): Integer;
begin
  result := posex(substr,s,offset);
end;

function FOS_PosEx(const SubStr, S: string): Integer;
begin
  result := posex(substr,s);
end;

function FOS_PosEx(c: char; const S: string; Offset: Cardinal): Integer;
begin
  result := posex(c,s,offset);
end;

end.

