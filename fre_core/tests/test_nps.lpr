program test_nps;

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,FOS_DEFAULT_IMPLEMENTATION,
  test_fos_nps in 'test_fos_nps.pas';


begin
   writeln('TEST NPS (FPC) Interfaced Edition');
   c_test_nps;
  writeln('TEST OK');
end.
