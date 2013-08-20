program fpmake_products;

{$Mode objfpc}
{$H+}
{$inline on}

 uses fpmkunit,classes,sysutils,fos_buildtools;

 Var
   P   : TPackage;

begin
  With Installer(TFOSInstaller) do begin
    Defaults.BinInstallDir  := GetCurrentDir+DirectorySeparator+'fos_bin/apps'+FOSBuild.Bits(Defaults.CPU)+'_'+cFOS_BUILD_SUFFIX;
    //P := AddPackage('DBSERVER');
    //with p do begin
    //  OSes := cFOS_BUILD_OSes;
    //  FOS_OS_SPEC_OPTIONS(Options);
    //  with Dependencies do begin
    //    Add('FRE_COREBOX');
    //    Add('FRE_INTF');
    //    Add('FRE_CORE');
    //    Add('FRE_DB');
    //    Add('FRE_HAL');
    //    Add('FRE_APPS');
    //    Add('fcl-xml');
    //    Add('fcl-fpcunit');
    //  end;
    //  Directory:=cFOS_BUILD_PREFIX+'fre_server';
    //  InstallProgramPrefix:='db_apps';
    //  with targets do begin
    //    AddProgram('fre_server.lpr');
    //  end;
    //end;
    //P := AddPackage('DBCOREBOX');
    //with p do begin
    //  OSes := cFOS_BUILD_OSes;
    //  FOS_OS_SPEC_OPTIONS(Options);
    //  with Dependencies do begin
    //    Add('FRE_INTF');
    //    Add('FRE_CORE');
    //    Add('FRE_DB');
    //    Add('FRE_HAL');
    //    Add('FRE_APPS');
    //    Add('fcl-xml');
    //    Add('fcl-fpcunit');
    //  end;
    //  Directory:=cFOS_BUILD_PREFIX+'fos_corebox';
    //  InstallProgramPrefix:='db_apps';
    //  with targets do begin
    //    AddProgram('fos_corebox.lpr');
    //  end;
    //end;
    P := AddPackage('DBMONSYS');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      FOS_OS_SPEC_OPTIONS(Options);
      with Dependencies do begin
        Add('FRE_INTF');
        Add('FRE_CORE');
        Add('FRE_DB');
        Add('FRE_HAL');
        Add('FRE_APPS');
        Add('fcl-xml');
        Add('fcl-fpcunit');
      end;
      Directory:=cFOS_BUILD_PREFIX+'fre_monsys';
      InstallProgramPrefix:='db_apps';
      with targets do begin
        AddProgram('fre_safejob.lpr');
        AddProgram('foscmd.lpr');
      end;
    end;
    P := AddPackage('CAPTIVEPORTAL');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      FOS_OS_SPEC_OPTIONS(Options);
      with Dependencies do begin
        Add('FRE_INTF');
        Add('FRE_CORE');
        Add('FRE_DB');
        Add('FRE_HAL');
        Add('fcl-xml');
        Add('fcl-fpcunit');
      end;
      Directory:=cFOS_BUILD_PREFIX+'fre_hal';
      InstallProgramPrefix:='captiveportal';
      with targets do begin
        AddProgram('cpadm.lpr');
        AddProgram('fospfupdater.lpr');
        AddProgram('fosipenabler.lpr');
        AddProgram('fosredirect.lpr');
      end;
    end;
    P := AddPackage('DATADEVICEADM');
    with p do begin
      OSes := cFOS_BUILD_OSes;
      FOS_OS_SPEC_OPTIONS(Options);
      with Dependencies do begin
        Add('FRE_INTF');
        Add('FRE_CORE');
        Add('FRE_DB');
        Add('FRE_HAL');
        Add('fcl-xml');
        Add('fcl-fpcunit');
      end;
      Directory:=cFOS_BUILD_PREFIX+'fre_hal';
      InstallProgramPrefix:='datadeviceadm';
      with targets do begin
        AddProgram('datadeviceadm.lpr');
      end;
    end;
    Run;
  end;
end.
