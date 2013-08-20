program con_db_test;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  sysutils,
  Classes, db_unittest,FOS_TOOL_INTERFACES,FOS_NPS, FOS_AlignedArray,
  FRE_DATE_TOOLS,Dateutils,fpcunit,fpcunitreport,
  FRE_CONSTANTS, FRE_DB_PERSISTANCE_FS_SIMPLE, FRE_DB_PERSISTANCE_FS_BLOCKS,
  FOS_DEFAULT_IMPLEMENTATION, FRE_DB_INTERFACE,FRE_DB_CORE,FRE_APS_IMPL_LE
  //FRE_DB_EDITOR
  //,FRE_DBCLOUDCONTROL
  ;



var mode,dbparam   : string;
   cnt             : integer;

begin
  //writeln(GFRE_BT.GetUserName);
  //writeln(GFRE_BT.GetUserDir,'-', GetUserDir);
  //writeln(GFRE_BT.GetTempDir,'-- ',GetTempDir(false));
  SetupAPS;
  GFRE_DB_DEFAULT_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFREBaseDirectory+'/db');
//  GFRE_DB.SetupBaseDirectory(cFREBaseDirectory+'/db');
  GFRE_DB.LocalZone := 'Europe/Vienna';
  mode    := ParamStr(1);
  dbparam := paramstr(2);
  cnt     := StrToIntDef(paramstr(3),1);

  GFRE_DBI := GFRE_DB;

  //FRE_DB_EDITOR.Register_DB_Extensions;
  //FRE_DBCLOUDCONTROL.Register_DB_Extensions;

 // mode   :='connect'; dbparam:='test_db'; cnt := 1;
 //mode   :='create'; dbparam:='test_db' ; cnt := 1;
 //mode   :='deletedb'; dbparam:='test_db';
 // mode   :='createdb'; dbparam:='test_db';
  //mode   :='dump'; dbparam:='test_db';
//   mode := 'basic';
//  mode := 'createsys';
//  mode := 'connectsys';
  case mode of
    'basic'             : BasicTest;
    'connect'           : ConnectTest(dbparam);
    'connectsys'        : ConnectSystem;
    'dumpsys'           : DumpSystem;
    'createdb'          : CreateDB(dbparam);
    'deletedb'          : DeleteDB(dbparam);
    'createsys'         : CreateSystemDB;
    'listdb'            : ListDBTest;
    'create'            : CreateCollectionTest(dbparam,cnt);
    'dump'              : ReadTest(dbparam);
    'transform'         : TransformTest;
    'scheme_update'     : Scheme_Update_Test;
    //'serve'             : ServerTest;
    //'client'            : ClientTest;
    'scheme'            : SchemeTest;
    'forclear'          : ClearTest;
    'fulldump'          : ReadTest(dbparam);
    'change'            : TestChange;
    else  writeln('use a known ooption :-)');
  end;

  Randomize;

  writeln('');
  GFRE_DB_DEFAULT_PS_LAYER.Finalize;
  GFRE_DB_DEFAULT_PS_LAYER:=nil;
  writeln('');
end.

