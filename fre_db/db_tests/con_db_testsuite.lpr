program con_db_testsuite;

{$mode objfpc}
{$H+}
{$codepage utf8}
{$LIBRARYPATH ../../fre_external/fre_ext_libs}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  cmem,
  FRE_SYSTEM,FRE_CONFIGURATION,math,
  FRE_DB_PERSISTANCE_FS_SIMPLE,
  //fre_db_persistance_fs_postgres,
  consoletestrunner,
  FOS_DEFAULT_IMPLEMENTATION,
  FRE_DB_CORE,FRE_DB_INTERFACE,FRE_dbbase,
  FOS_TOOL_INTERFACES,
  sysutils,
  fre_db_testsuite;

var App: TTestRunner;
      x: TFRE_DB_Object;
      //k  : Currency;
      //kk : int64;
      //u  : UInt64;
      //s  : Int64;
      //ui : uint64;

begin
  //u := 23;
  //s := 23;
  //writeln('Unsigned ',GFRE_BT.Dump_Binary(@u,8),' ',u);
  //writeln('Signed ',GFRE_BT.Dump_Binary(@s,8),' ',s);
  //s:=23;
  //u:=-23;
  //writeln('Signed ',GFRE_BT.Dump_Binary(@s,8),' ',s);
  //writeln('Unsigned ',GFRE_BT.Dump_Binary(@u,8),' ',u);
  //ui := 1 shl 63;
  //uint64(s) := uint64(s) XOR ui;
  //writeln('Signed ',GFRE_BT.Dump_Binary(@s,8),' ',s,' ',GFRE_BT.Dump_Binary(@ui,8));
  //exit;
  //k:= -23.233;
  //kk := Int64(k);
  //writeln(k,' ',kk,' ',PInt64(@k)^);
  //exit;
  Initialize_Read_FRE_CFG_Parameter;
  GFRE_DB_DEFAULT_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'db');
  x := TFRE_DB_Object.Create;
  x.Field('Y').AsString:='S';
  x.Set_Store_Locked(true);
  x.Set_Store_Locked(false);
  x.Field('SS');

  Init4Server;
  Register_DB_Extensions;
  RegisterTestCodeClasses;

  TEST_GUID_1 := StringToGUID('{00000000-0000-0000-0000-000000000001}');
  TEST_GUID_2 := StringToGUID('{00000000-0000-0000-0000-000000000002}');
  TEST_GUID_3 := StringToGUID('{00000000-0000-0000-0000-000000000003}');
  GFRE_DB.LocalZone        := 'Europe/Vienna';
  //GFRE_DB_DEFAULT_PS_LAYER := Get_PersistanceLayer_PS_POSTGRES('helly','','localhost','');

  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE Database Testsuite';
  App.Run;
  App.Free;
end.

