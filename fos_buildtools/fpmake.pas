program fpmake;

{$Mode objfpc}
{$H+}
{$inline on}

 {$UNITPATH ../fre_core/}

 uses fpmkunit,classes,sysutils,fos_buildtools,FOS_AlignedArray;

 // ../;../../fre_aps;../../fre_core;../../fre_fcom;../../fre_db
 const cFOS_BUILD_PREFIX = '../';

 Var
   P   : TPackage;
   T   : TTarget;
  type

    { TFOSDefaults }

    TFOSDefaults=class(TCustomDefaults)
    public
      constructor Create;
      procedure   CompilerDefaults; override;
    end;

    { TFOSInstaller }

    TFOSInstaller=class(TCustomInstaller)
    public
      constructor Create(AOwner: TComponent);override;
    end;

 { TFOSInstaller }

 constructor TFOSInstaller.Create(AOwner: TComponent);
 begin
   Defaults:=TFOSDefaults.Create;
   inherited Create(Aowner);
 end;

 { TFOSDefaults }

 constructor TFOSDefaults.Create;
 begin
   Compiler := SetDirSeparators(GetUserDir+'fosbuild/fpcbin/'+FOSBuild.OS+'/bin/fpc');
   writeln('COMPILER : ',Compiler);
   inherited Create;
 end;

 procedure TFOSDefaults.CompilerDefaults;
 begin
   inherited CompilerDefaults;
   BaseInstallDir := '~/fosbuild/fos_pkg/';
   GlobalUnitDir  := lowercase(IncludeTrailingPathDelimiter(SetDirSeparators(GetUserDir+'fosbuild/fpcbin/'+FOSBuild.OS+'/lib/fpc/'+FOSBuild.FPC_Version+'/units/'))+PathDelim+Target);
   //   writeln('BASEINSTALL: ',BaseInstallDir,' GUDIR: ',GlobalUnitDir);
 end;

const  cFOS_BUILD_OSes:TOSes = [solaris,darwin,linux,freebsd];

 begin
    //writeln(FOSBuild.BuildString);
    //halt;

   With Installer(TFOSInstaller) do begin
     P := AddPackage('FRE_Interfaces');
     P.OSes := cFOS_BUILD_OSes;
     P.Directory:=cFOS_BUILD_PREFIX+'fos_interfaces';
     //P.Targets.AddUnit('fre_db_interface.pas');


     //P := AddPackage('FRE_APS');
     //P.OSes := cFOS_BUILD_OSes;
     //
     //
     P := AddPackage('FRE_CORE');
     P.OSes := cFOS_BUILD_OSes;
     P.Directory:=cFOS_BUILD_PREFIX+'fre_core';
     p.Dependencies.Add('FRE_Interfaces');
     P.targets.addunit('fos_interlocked.pas');
     p.targets.addunit('fos_arraygen.pas');
     p.targets.addunit('fos_arraysetgen.pas');
     p.targets.addunit('fos_basis_tools.pas');
     p.targets.addunit('fos_bsd_core.pas');
     p.targets.addunit('fos_cpu_lowlevel.pas');
     p.targets.addunit('fos_cpu_tools.pas');
     p.targets.addunit('fos_default_implementation.pas');
     p.targets.addunit('fos_default_stable_store.pas');
     p.targets.addunit('fos_default_strings.pas');
     p.targets.addunit('fos_dynlib_tools.pas');
     p.targets.addunit('fos_file_logger.pas');
     p.targets.addunit('fos_generic_sort.pas');
     p.targets.addunit('fos_genq.pas');
     p.targets.addunit('fos_lfq.pas');
     p.targets.addunit('fos_listarray.pas');
     p.targets.addunit('fos_locking.pas');
     p.targets.addunit('fos_nps.pas');
     p.targets.addunit('fos_partvarq.pas');
     p.targets.addunit('fos_profiling.pas');
     p.targets.addunit('fos_redblacktree_gen.pas');
     p.targets.addunit('fos_timing.pas');
     p.targets.addunit('fos_tool_factory.pas');
     p.targets.addunit('fos_tool_interfaces.pas');
     p.targets.addunit('fre_constants.pas');
     p.targets.addunit('fre_core_instrument.pas');
     p.targets.addunit('fre_date_tools.pas');
     p.targets.addunit('fre_firmos_mm.pas');
     p.targets.addunit('fre_system.pas');
     p.targets.addunit('fos_alignedarray.pas');
     p.targets.addunit('upascaltz.pas');


     //NonCore
   //p.targets.addunit('fre_process.pas');
   //p.targets.addunit('fos_vm_control_interface.pas');


     //TODO: CORE PACKAGE KILL
     //p.targets.addunit('fos_jobscheduler.pas');
     //p.targets.addunit('fos_ng_appsyncer.pas');
     //p.targets.addunit('fos_ng_appsyncer_console.pas');
     //p.targets.addunit('fos_ng_worker.pas');
     //p.targets.addunit('fos_null_logger.pas');



     //P.UnitPath.Add('..');
     //P.SourcePath.Add('./tests');
     //P.UnitPath.Add('../fre_aps');
     //P.UnitPath.Add('../fre_fcom');
     //P.UnitPath.Add('../fre_db');
     //T := P.Targets.AddProgram('tests/test_basis_tools.lpr');
     //T := P.Targets.AddProgram('tests/test_cputools.lpr');
     //T := P.Targets.AddProgram('tests/test_cpu_ll.lpr');
     //T := P.Targets.AddProgram('tests/test_file_logger.lpr');
     //T := P.Targets.AddProgram('tests/test_genrbtrees.lpr');
     //T := P.Targets.AddProgram('tests/test_interlocked.lpr');
     //T := P.Targets.AddProgram('tests/test_sort.lpr');
     //T := P.Targets.AddProgram('tests/test_timing.lpr');
     //T := P.Targets.AddProgram('tests/test_nps.dpr');
     //T := P.Targets.AddProgram('tests/CON_LFQTEST.lpr');
     //T := P.Targets.AddProgram('tests/CON_LISTARRAYTEST.lpr');
     //T := P.Targets.AddProgram('tests/CON_PARTVARQ.lpr');
     Run;
   end;
 end.
