program test_default_memtest;

{$mode objfpc}{$H+}

uses
  cthreads,
  FOS_TOOL_INTERFACES;

begin
  writeln(GFRE_BT.CreateGuid_HEX);
  GFRE_LOG.Sync_Logger;
end.

