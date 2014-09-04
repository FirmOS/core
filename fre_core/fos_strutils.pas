unit fos_strutils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,strutils;

function FOS_AnsiContainsText(const AText, ASubText: string): Boolean;
Function FOS_AnsiStartsText(const ASubText, AText: string): Boolean;
Function FOS_AnsiEndsText(const ASubText, AText: string): Boolean;


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

end.

