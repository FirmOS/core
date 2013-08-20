program test_cputools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,FOS_TOOL_INTERFACES,FOS_CPU_TOOLS,xmlread,dom;

begin
  writeln;
  writeln('FRE CPU Tools Architecture Summary');
  writeln;
  writeln(GFRE_CPU.Get_CPU_Data.DumpText);
  writeln;
  writeln('PACKS=',GFRE_CPU.Packages,' CORES=',GFRE_CPU.Cores,' LOGICAL=',GFRE_CPU.Logical);
end.

