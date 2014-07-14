unit fre_mysql_ll;

{$mode objfpc}{$H+}

{$IFDEF DARWIN}
  {$DEFINE FREMYSQL}
{$ENDIF}

interface

uses
  Classes, SysUtils
  {$IFDEF FREMYSQL}
  ,mysql55dyn,mysql55conn
  {$ENDIF}
  ;

{$IFDEF FREMYSQL}
type

  { TFOSMySqlConn }
  TFOSMySqlConn=class(TMySQL55Connection)
    procedure DoInternalConnect; override;
  end;
{$ENDIF}


implementation

{$IFDEF FREMYSQL}
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
{$ENDIF}

end.

