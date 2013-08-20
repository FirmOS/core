program test_pgsql;

{$mode objfpc}{$H+}
{$LIBRARYPATH ../../fre_external/fre_ext_libs}

uses
  cthreads,
  Classes, fre_postgres_ll;

var pg_conn : TFRE_POSTGRES_CONNECION;
    pg_cmd  : TFRE_PQ_CMD;

begin
  //PG_ConnectionTest;
  pg_conn := TFRE_POSTGRES_CONNECION.Create;
  pg_conn.Host:='localhost';
  pg_conn.Port:=5432;
  pg_conn.HostAddr:='127.0.0.1';
  pg_conn.ConnTimeout:=10;
  pg_conn.DBname:='helly';
  pg_conn.Password:='x';
  pg_conn.SSLMode:=fpsm_disable;
  pg_conn.Connect;
  pg_cmd := pg_conn.GetNewCommand;
  pg_cmd.CommandText := 'Select * from pg_type;';
  if not pg_cmd.Execute then begin
    writeln(pg_cmd.LastError);
  end else begin
    pg_cmd.DumpResult;
  end;
end.

