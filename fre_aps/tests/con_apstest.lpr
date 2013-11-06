program CON_apstest;

{$mode objfpc}{$H+}
{$LIBRARYPATH ../../../lib}

uses
 {$IFDEF UNIX}
  cthreads,Sockets,
 {$ENDIF}
  Classes,
  sysutils, FRE_APS_INTERFACE,FOS_TOOL_INTERFACES
  ,FOS_BASIS_TOOLS,syncobjs,FOS_LOCKING,FOS_INTERLOCKED,FRE_FCOM_SSL, FRE_LIBEVENT_CORE,
  FOS_DEFAULT_IMPLEMENTATION,
  //fre_aps_test,
 //FRE_APS_IMPL_LE
  fre_aps_comm_impl;

begin
  if paramstr(1)='testle' then
     begin
       Test_LE;
       exit;
     end;
  Setup_APS_Comm;

  Test_APSC;
  GFRE_SC.RunUntilTerminate;
  Teardown_APS_Comm;
  exit;
  //GFRE_Log.AddRule('*',fll_Invalid,'*',flra_LogToOnConsole,false); // All To Console
  //GFRE_Log.AddRule('*',fll_Invalid,'*',flra_DropEntry); // No File  Logging
  //SetupAPS;
  //GFRE_S.Start(TFRE_APS_Test.Create);
  //GFRE_S.Start(TFRE_APS_SockTest.Create);
  //GFRE_S.Run;
  //TearDownAPS;
  //writeln('GOOD CLIENT READS ',G_GoodClientReads,'/',G_TotalCreatedClients);
  //Shutdown_Done;
end.

