program con_db_concurrentloadtest;

{$mode objfpc}
{$H+}
{$codepage utf8}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  fre_configuration,
  FOS_DEFAULT_IMPLEMENTATION,sysutils,
  FRE_DB_CORE,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES;

var DBO : IFRE_DB_Object;
    CNT : QWord=0;

begin
  InitMinimal(true);
  if ParamStr(1)='read' then begin
    repeat
      inc(cnt);
      writeln('READ: ',cnt);
      try
        DBO := GFRE_DBI.CreateFromFile('conc_test.dbo');
        writeln(dbo.DumpToString());
      except on E:Exception do begin
        writeln('READ EX : ',E.Message);
        //writeln(gfre_bt.DumpExceptionsBacktrace);
      end;end;
      if assigned(dbo) then begin
        DBO.Finalize;
      end;
    until false;
  end else
  if ParamStr(1)='write' then begin
    repeat
      try
        inc(cnt);
        writeln('WRITE: ',cnt);
        DBO := GFRE_DBI.NewObject;//
        DBO.Field('W_CNT').AsUInt64 := CNT;
        DBO.SaveToFile('conc_test.dbo');
        DBO.Finalize;
      except on E:Exception do begin
        writeln('WRITE EX : ',E.Message);
      end;end;
    until false;
  end else begin
    writeln('start with param "read" or param "write"');
  end;
end.

