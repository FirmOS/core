program fpmake_test;

{$Mode objfpc}
{$H+}
{$inline on}

 uses fpmkunit,classes,sysutils,fos_buildtools;

 Var
   P   : TPackage;
   T   : TTarget;

 begin
   With Installer(TFOSInstaller) do begin
//     Defaults.BinInstallDir  := GetCurrentDir+DirectorySeparator+'fos_bin/tests'+FOSBuild.Bits(Defaults.CPU)+'_'+cFOS_BUILD_SUFFIX;
     Defaults.BinInstallDir  := GetCurrentDir+'/../../bin';
     P := AddPackage('CORETESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_CORE');
         Add('fcl-xml');
         Add('fcl-fpcunit');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_core/tests/';
       //InstallProgramPrefix:='core';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('test_version_info.lpr').ExtraEnvironment.Values['FOS_PRODUCT_NAME'] := '*Test =Version= Info *';
         AddProgram('test_basis_tools.lpr');
         AddProgram('test_sort.lpr');
         AddProgram('con_listarraytest.lpr');
         AddProgram('con_lfqtest.lpr');
         AddProgram('con_partvarqtest.pas');
         AddProgram('test_cputools.lpr');
         AddProgram('test_default_memtest.lpr');
         AddProgram('test_file_logger.lpr');
         AddProgram('test_genrbtrees.lpr');
         AddProgram('test_interlocked.lpr');
         AddProgram('test_nps.lpr');
       end;
     end;
     P := AddPackage('FSYNATESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_SYNAPSE');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_external/tests/';
       //InstallProgramPrefix:='fsyna';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('con_synatest.lpr');
       end;
     end;
     P := AddPackage('FBLOCKCOMTESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('fcl-xml');
         Add('fcl-fpcunit');
         Add('FRE_CORE');
         Add('FRE_BLKCOM');
         Add('FRE_INTF');
         Add('FRE_DB');
         Add('FRE_SYNAPSE');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_blockcom/tests/';
       //InstallProgramPrefix:='fre_blockcom';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('test_mail.lpr');
         AddProgram('test_http.lpr');
       end;
     end;
     P := AddPackage('FCOMTESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_CORE');
         Add('FRE_FCOM');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_fcom/tests/';
       //InstallProgramPrefix:='fcom';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('con_ssl_test.lpr');
       end;
     end;
     P := AddPackage('APSTESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_CORE');
         Add('FRE_FCOM');
         Add('FRE_APS');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_aps/tests/';
       //InstallProgramPrefix:='aps';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         T:=AddProgram('con_apstest.lpr');
       end;
     end;

     P := AddPackage('FREDBTESTSERVER');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_CORE');
         Add('FRE_INTF');
         Add('FRE_FCOM');
         Add('FRE_APS');
         Add('FRE_DB');
         Add('FRE_APPS');
         Add('FRE_BLKCOM');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_test_server/';
       with Targets do begin
         AddUnit('../fre_db/fre_dbtest.pas');
       end;
       //InstallProgramPrefix:='fre_testserver';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('fre_testserver.lpr').ExtraEnvironment.Values['FOS_PRODUCT_NAME'] := 'FRE TestServer';
         AddProgram('fre_testdatafeed.lpr').ExtraEnvironment.Values['FOS_PRODUCT_NAME'] := 'FRE TestFeeder';
       end;
     end;
     P := AddPackage('FREPROCESSTESTS');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('fcl-xml');
         Add('fcl-fpcunit');
         Add('FRE_CORE');
         Add('FRE_INTF');
         Add('FRE_DB');
         Add('FRE_APPS');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_core/tests/';
       //InstallProgramPrefix:='testprocess';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('testprog_process.lpr');
         AddProgram('test_process.lpr');
       end;
     end;
     P := AddPackage('FREDBCORE');
     with p do begin
       OSes := cFOS_BUILD_OSes;
       FOS_OS_SPEC_OPTIONS(Options);
       with Dependencies do begin
         Add('FRE_CORE');
         Add('FRE_INTF');
         Add('FRE_FCOM');
         Add('FRE_APS');
         Add('FRE_DB');
       end;
       Directory:=cFOS_BUILD_PREFIX+'fre_db/db_tests/';
       //InstallProgramPrefix:='con_db_concurrentloadtest';
       InstallProgramSuffix := FOSBuild.FOS_Suffix;
       with targets do begin
         AddProgram('con_db_concurrentloadtest.lpr');
       end;
     end;
     Run;
   end;
 end.
