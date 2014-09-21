program con_db_testsuite;

{$mode objfpc}
{$H+}
{$codepage utf8}
{$LIBRARYPATH ../../../lib}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  cmem,
  FRE_SYSTEM,FRE_CONFIGURATION,math,
  FRE_DB_PERSISTANCE_FS_SIMPLE,
  consoletestrunner,
  FOS_DEFAULT_IMPLEMENTATION,
  FRE_DB_CORE,FRE_DB_INTERFACE,FRE_dbbase,
  FOS_TOOL_INTERFACES,
  sysutils,
  fre_db_testsuite, fre_net_pl_client,fre_aps_comm_impl;

var App: TTestRunner;

    //tst  : TFRE_DB_PL_NET_CLIENT;
    //name : String;

begin
  Initialize_Read_FRE_CFG_Parameter;
  Setup_APS_Comm;

  //GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Net('','','');
  GFRE_DB_PS_LAYER := Get_PersistanceLayer_PS_Simple(cFRE_SERVER_DEFAULT_DIR+DirectorySeparator+'db');

  DefaultFormat      := fPlain;
  DefaultRunAllTests := true;

  GDBPS_TRANS_WRITE_THROUGH := TRUE;
  GDISABLE_SYNC             := TRUE;

  //CfgTestLog;
  Init4Server;
  Register_DB_Extensions;
  RegisterTestCodeClasses;

  TEST_GUID_1.ClearGuid;
  TEST_GUID_2.ClearGuid;
  TEST_GUID_3.ClearGuid;
  TEST_GUID_1.D[15]:=1;
  TEST_GUID_2.D[15]:=2;
  TEST_GUID_3.D[15]:=3;
  GFRE_DB.LocalZone        := 'Europe/Vienna';

  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Title := 'FirmOS FRE Database Testsuite';
  App.Run;
  App.Free;
end.

