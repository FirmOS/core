unit fos_buildtools;

{$Mode objfpc}
{$H+}
{$inline on}

interface

uses
  Classes, SysUtils,process,fpmkunit;

const cFOS_BUILD_OSes:TOSes     = [solaris,darwin,linux,freebsd];
const cFOS_BUILD_PREFIX         = '../';
var   cFOS_BUILD_SUFFIX:string  ;
      cFOS_STATIC_BUILD:boolean ;

procedure  FOS_OS_SPEC_OPTIONS(const options:TStrings);

type
  { FOSBuild }
  FOSBuild=object
    function CPU         : string; static;
    function OS          : string; static;
    function FPC_Version : string; static;
    function User        : string; static;
    function Host        : string; static;
    function Time        : string; static;
    function Revision    : string; static;
    function Date        : string; static;
    function BuildString : string; static;
    function FOS_Suffix  : string; static;
    function Bits        (const bcpu:TCPU): string; static;
  end;


type

  { TFOSInstaller }
  TFOSInstaller=class(TCustomInstaller)
  private
  public
    constructor Create(AOwner: TComponent);override;
  end;


implementation

{ FOSBuild }
function FOSBuild.CPU: string;
begin
  result := {$I %FPCTARGETCPU%};
end;

function FOSBuild.OS: string;
begin
  result := lowercase({$I %FPCTARGETOS%});
end;

function FOSBuild.FPC_Version: string;
begin
  result := {$I %FPCVERSION%};
end;

function FOSBuild.User: string;
begin
  result := {$I %USER%};
end;

function FOSBuild.Host: string;
begin
  result := {$I %HOSTNAME%};
end;

function FOSBuild.Time: string;
begin
  result := {$I %TIME%};
end;

function FOSBuild.Revision: string;
var tp  : TProcess;
    ssm : TStringStream;
    br  : string;
    rev : string;
begin
  try
    try
      tp := TProcess.Create(nil);
      tp.CommandLine:='git rev-parse --abbrev-ref HEAD';
      tp.Options := tp.Options + [poWaitOnExit, poUsePipes];
      tp.Execute;
      ssm := TStringStream.Create('');
      ssm.CopyFrom(tp.Output,0);
      br:=trim(ssm.DataString);
      tp.CommandLine:='git log -1 --format="%H) [%ci]"';
      tp.Execute;
      ssm.Size:=0;
      ssm.CopyFrom(tp.Output,0);
      rev:=AnsiDequotedStr(ssm.DataString,'"');
      result:=format('(%s/%s',[br,rev]);
      ssm.free;
    finally
      tp.free;
    end;
  except
    result :='(NO REVISION)';
  end;
end;

function FOSBuild.Date: string;
begin
  result := {$I %DATE%};
end;

function FOSBuild.BuildString: string;
begin
  result := Format('FOSBUILD: %s/%s using (fpc %s) user (%s) rev %s',[CPU,OS,FPC_Version,User,Revision])
end;

function FOSBuild.FOS_Suffix: string;
begin
  if cFOS_STATIC_BUILD then
    result := '_'+OS+'_'+cFOS_BUILD_SUFFIX+Bits(Defaults.CPU)+'s'
  else
    result := '_'+OS+'_'+cFOS_BUILD_SUFFIX+Bits(Defaults.CPU)+'d'
end;

function FOSBuild.Bits(const bcpu: TCPU): string;
begin
  case BCPU of
      i386:   result:='32';
      x86_64: result:='64';
      else    result:='0';
  end;
end;

constructor TFOSInstaller.Create(AOwner: TComponent);
begin
  AddCustomFpmakeCommandlineOption('buildsuffix','Add build suffix (--buildsuffix=dbg)');
  AddCustomFpmakeCommandlineOption('fosstatic','Enable static binary building  (--fosstatic=true)');
  Defaults:=TBasicDefaults.Create;
//  Defaults.Compiler        := SetDirSeparators(GetUserDir+'fosbuild/fpcbin/'+FOSBuild.OS+'/bin/fpc');;
  Defaults.Compiler        := './fosfpc.sh';
  inherited Create(Aowner) ;
  cFOS_BUILD_SUFFIX        := GetCustomFpmakeCommandlineOptionValue('buildsuffix');
  cFOS_STATIC_BUILD        := GetCustomFpmakeCommandlineOptionValue('fosstatic')='true';

  if cFOS_BUILD_SUFFIX='' then
    cFOS_BUILD_SUFFIX:='rel';

  if cFOS_STATIC_BUILD then
    begin
      Defaults.BaseInstallDir  := '../../fospkg/static/'+FOSBuild.FPC_Version+cFOS_BUILD_SUFFIX;
      Defaults.LocalUnitDir    := '../../fospkg/static/'+FOSBuild.FPC_Version+cFOS_BUILD_SUFFIX+'/units/'+Defaults.Target;
    end
  else
    begin
      Defaults.BaseInstallDir  := '../../fospkg/dynamic/'+FOSBuild.FPC_Version+cFOS_BUILD_SUFFIX;
      Defaults.LocalUnitDir    := '../../fospkg/dynamic/'+FOSBuild.FPC_Version+cFOS_BUILD_SUFFIX+'/units/'+Defaults.Target;
    end;

  Defaults.GlobalUnitDir   := lowercase(IncludeTrailingPathDelimiter(SetDirSeparators(GetUserDir+'fosbuild/fpcbin/'+FOSBuild.OS+'/lib/fpc/'+FOSBuild.FPC_Version+'/units/'))+PathDelim+Defaults.Target);
  Defaults.NoFPCCfg        := true;
  Defaults.BuildMode       := bmOneByOne;
  Defaults.Options.Add('-Sg'); // Allow goto
  Defaults.Options.Add('-Sh'); // ANSI Strings
  Defaults.Options.Add('-Sc'); // C Style Operators
  Defaults.Options.Add('-Mobjfpc');
  Defaults.Options.Add('-Fi../fos_include');
  Defaults.Options.Add('-P'+CPUToString(Defaults.CPU));
  if cFOS_STATIC_BUILD then
    begin
      Defaults.Options.Add('-dFOS_LINK_STATIC');
      //Defaults.Options.Add('-Xt');
    end;
end;


procedure  FOS_OS_SPEC_OPTIONS(const options:TStrings);
begin
  if cFOS_STATIC_BUILD then
    options.Add('-Fl../../lib/')
  else
    options.Add('-Fl../../lib/lib'+FOSBuild.Bits(Defaults.CPU)+cFOS_BUILD_SUFFIX+'/'+FOSBuild.OS);

  case Defaults.OS of
    darwin :     begin
                   options.add('-WM10.6');
                 end;
    freebsd:     begin
                   options.Add('-k-rpath=''$ORIGIN../../lib/lib'+FOSBuild.Bits(Defaults.CPU)+cFOS_BUILD_SUFFIX+'/'+FOSBuild.OS+'''');
                 end;
    solaris:     begin
                   options.Add('-k-rpath=''$ORIGIN../../lib/lib'+FOSBuild.Bits(Defaults.CPU)+cFOS_BUILD_SUFFIX+'/'+FOSBuild.OS+'''');
                 end;
    win32,win64: begin
                   //
                 end;
    linux      : begin
                   options.Add('-k-rpath=''$ORIGIN../../lib/lib'+FOSBuild.Bits(Defaults.CPU)+cFOS_BUILD_SUFFIX+'/'+FOSBuild.OS+'''');
                 end;
  end;
end;


end.

