program test_pgsql;

{$mode objfpc}{$H+}
{$LIBRARYPATH ../../fre_external/fre_ext_libs}

uses
  cthreads,sysutils,
  Classes, fre_postgres_ll;

var pg_conn : TFRE_POSTGRES_CONNECION;
    pg_cmd  : TFRE_PQ_CMD;

begin
  writeln('xxx');
  //PG_ConnectionTest;
  pg_conn := TFRE_POSTGRES_CONNECION.Create;
  pg_conn.Host:=''; // dont set when using ip
  pg_conn.Port:=5432;
  pg_conn.HostAddr:='10.54.3.242';
  pg_conn.ConnTimeout:=10;
  pg_conn.DBname:='fostestdb';
  pg_conn.User:='firmos';
  pg_conn.Password:='gimmehard';
  pg_conn.SSLMode:=fpsm_allow;
  pg_conn.Connect;
  pg_cmd := pg_conn.GetNewCommand;
  pg_cmd.CommandText := 'Select * from pg_type;';
  if not pg_cmd.Execute then begin
    writeln(pg_cmd.LastError);
  end else begin
    pg_cmd.DumpResult;
  end;
  pg_cmd.Free;

  pg_cmd := pg_conn.GetNewCommand;
  pg_cmd.CommandText := 'INSERT INTO testtable(id, txt) VALUES ('+IntToStr(1)+', '+'''Juhu Uhu'''+');';
  if not pg_cmd.Execute then begin
    writeln(pg_cmd.LastError);
  end else begin
    pg_cmd.DumpResult;
  end;
end.

