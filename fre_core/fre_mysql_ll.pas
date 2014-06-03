unit fre_mysql_ll;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,mysql55dyn,mysql55conn;

type

  { TFOSMySqlConn }

  TFOSMySqlConn=class(TMySQL55Connection)
    procedure DoInternalConnect; override;
  end;


implementation

{ TFOSMySqlConn }

procedure TFOSMySqlConn.DoInternalConnect;
var
  FullVersion,
  ClientVerStr: string;
begin
  InitialiseMysql;
  Fullversion:=strpas(mysql_get_client_info());
  ClientVerStr := copy(FullVersion,1,3);
  //If (ClientVerStr<>MySQLVersion) then
  //  Raise EInOutError.CreateFmt(SErrVersionMisMatch,[ClassName,MySQLVersion,FullVersion]);
  ConnectToServer;
  SelectDatabase;
end;

end.

