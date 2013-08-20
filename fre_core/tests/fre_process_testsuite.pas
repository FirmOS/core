unit fre_process_testsuite;

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,fpcunit,testregistry,testdecorator,iostream, FRE_Process,FRE_DB_INTERFACE,
  FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_SYSTEM;


const

  ctestprog                 = 'testprog_process';


  cremoteuser               = 'root';
  cremotehost               = 'firmosdev';      // must be SmartOS for Testcase RemoteSmartOSUname
//  cremotehost               = '10.1.0.130';      // must be SmartOS for Testcase RemoteSmartOSUname

  cmultistream              = 8;
  cstreamsize               = 1000000;

type

  { TFRE_Process_Tests }

  TFRE_Process_Tests = class (TTestCase)
  private
    procedure internalDMMultistreamTest (const remote:boolean ; const nr : integer);
    procedure RemoteSlow;
  private
    procedure CheckTestprogs;
    procedure Echo;
    procedure Cat;
    procedure StdInToStdOut;
    procedure StdErrorPipe;
    procedure StdOutStdError;
    procedure ArtefaktCloseIn;
    procedure ArtefaktCloseOut;
    procedure AsyncDoubleStart;
    procedure BasicMemstream;
    procedure BasicInputOutPut;
    procedure RemoteSmartOSUname;
    procedure RemoteCat;
    procedure AsyncCat;
    procedure InputStreamNil;
    procedure ArtefaktKill;
    procedure DBMultiStream;
    procedure DBSingleStream;
    procedure DBRemoteMultiStream;
    procedure Reuse;
    procedure Cascading;
    procedure OutStreamCallBack(const stream:TStream);
    procedure ErrorStreamCallBack(const stream:TStream);
    procedure Feed;
  published
    procedure FeedLoop;
  end;

type

  RMultiStream = record
    process    :  TFRE_Process;
    inputdone  :  boolean;
    outputdone :  boolean;
    inmem      :  TMemoryStream;
    outmem     :  TMemoryStream;
    errorstream:  TStringStream;
  end;


implementation

function GetRemoteKeyFilename: string;
begin
  result := SetDirSeparators(cFRE_SERVER_DEFAULT_DIR+'/ssl/user/id_rsa');         // must be authorized for remoteuser on remotehost
end;

{ TFRE_Process_Tests }


procedure TFRE_Process_Tests.internalDMMultistreamTest(const remote: boolean; const nr: integer);
var multi  : TFRE_DB_Multiprocess;
    proc   : TFRE_DB_Process;
    imulti : integer;
    i      : integer;
    im,om  : TFRE_DB_Stream;

begin
  multi := TFRE_DB_Multiprocess.create;
  for imulti := 1 to nr do begin
    proc := TFRE_DB_Process.Create;
    im   := proc.GetInputStream;
    for i:=1 to cstreamsize do begin
      im.WriteByte(random(256));
    end;
    im.position := 0;
    proc.SetupInput('cat');
    multi.AddProcess(proc);
  end;

  if remote then begin
    multi.SetRemoteSSH(cremoteuser,cremotehost, Getremotekeyfilename);
  end;

  multi.ExecuteMulti;

  // Checkresult
  for imulti := 0 to multi.ProcessCount-1 do begin
    writeln ('Check ',imulti);
    proc := multi.GetProcess(imulti);
    AssertEquals ('Exitstatus not 0',0,proc.ExitStatus);
    AssertEquals ('Error not Size 0 ',0,proc.GetErrorStream.Size);
    im   := proc.GetInputStream;
    om   := proc.GetOutputStream;
    im.Position := 0;
    om.Position := 0;
    AssertEquals('Input and Output Size are different :'+inttostr(im.Size)+'<>'+inttostr(om.size),im.Size,om.Size);
    i:=0;
    while (im.Position<im.Size) do begin
      inc (i);
      if im.ReadByte<>om.ReadByte then begin
        raise Exception.Create ('Byte '+inttostr(i)+' is different ');
      end;
    end;
  end;
end;



procedure TFRE_Process_Tests.CheckTestprogs;
begin
  AssertTrue(ctestprog+' is not in working dir',FileExists(ctestprog));
end;

procedure TFRE_Process_Tests.Echo;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------ ECHO --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePiped('echo',TFRE_DB_StringArray.Create('Hello'),'',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals ('Output','Hello'+LineEnding,outs);
    Assert(length(errors)>0,'Error is not empty');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.StdOutStdError;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------- STDOUT --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePiped(ctestprog,TFRE_DB_StringArray.Create('STDOUTERR'),'',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals ('Output','STDOUT'+LineEnding,outs);
    AssertEquals ('Error','STDERR'+LineEnding,errors);
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.StdIntoStdOut;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------- STDINOUT --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePiped('cat',nil,'MyInput',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
 //   AssertEquals ('Output','MyInput'+LineEnding,outs);
    Assert(length(errors)>0,'Error is not empty');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.StdErrorPipe;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------ STDERRORPIPE --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePiped('testprog_process',TFRE_DB_StringArray.Create('STDERR','70000'),'',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals('ERROR String not equal',StringOfChar('x',70000),errors);
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.Cat;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------- CAT --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePiped('cat',nil,'MyInput',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals ('Output','MyInput',outs);
    Assert(length(errors)>0,'Error is not empty');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.BasicInputOutPut;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
    stdinstream : TIOStream;
    stdoutstream: TIOStream;
    stderrstream: TIOStream;

begin
  stdinstream  := TIOStream.Create(iosInput);
  stdoutstream := TIOStream.Create(iosOutPut);
  stderrstream := TIOStream.Create(iosError);

  writeln ('------- BASICINPUTOUTPUT --------');
  process := TFRE_Process.Create(nil);
  try
    res          := process.ExecutePipedStream('cat',nil,TStringStream.Create('1234567890'), stdoutstream, stderrstream);
    AssertEquals ('ResultCode not 0',0,res);
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.BasicMemstream;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
    meminstream : TStream;
    memoutstream: TStream;
    memerrstream: TStream;
    i           : integer;
    iproc       : integer;
begin
  writeln ('------- BASICMEMSTREAM --------');
  for iproc    := 1 to 10 do begin
    meminstream  := TMemoryStream.Create;
    memoutstream := TMemoryStream.Create;
    memerrstream := TMemoryStream.Create;
    try
      for i:=1 to cstreamsize do begin
        meminstream.WriteByte(random(256));
      end;
      meminstream.Position:=0;
      writeln ('------- BASICMEMSTREAM ----- '+inttostr(iproc));
      process := TFRE_Process.Create(nil);
      try
        res          := process.ExecutePipedStream('cat',nil,meminstream, memoutstream, memerrstream);
    //    writeln('------- EXECUTED ----------');
        AssertEquals ('ResultCode not 0',0,res);

        i:=0;
        meminstream.Position :=0;
        memoutstream.Position:=0;
        AssertEquals ('Outstreamsize',meminstream.Size,memoutstream.Size);
        while (meminstream.Position<meminstream.Size) do begin
          inc (i);
          if meminstream.ReadByte<>memoutstream.ReadByte then begin
            raise Exception.Create ('Byte '+inttostr(i)+' is different ');
          end;
        end;
      finally
        process.Free;
      end;
    finally
      meminstream.Free;
      memoutstream.Free;
      memerrstream.Free;
    end;
  end;
end;


procedure TFRE_Process_Tests.DBSingleStream;
begin
  writeln ('-------- DB STREAM --------');
  internalDMMultistreamTest(false, 1);
end;

procedure TFRE_Process_Tests.DBMultiStream;
begin
  writeln ('-------- DB MULTI STREAM --------');
  internalDMMultistreamTest(false, cmultistream);
end;

procedure TFRE_Process_Tests.DBRemoteMultiStream;
begin
  writeln ('-------- DB REMOTE MULTI STREAM --------');
  internalDMMultistreamTest(true, cmultistream);
end;

procedure TFRE_Process_Tests.ArtefaktCloseIn;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------ ARTEFAKTCLOSEIN --------');
  process := TFRE_Process.Create(nil);
  try
    res         := process.ExecutePiped('testprog_process',TFRE_DB_StringArray.Create('CLOSEIN','1000'),'Testtext',outs,errors);
    AssertEquals('ResultCode not 0',0,res);
    AssertEquals('Output not equal',outs,'1234567890');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.ArtefaktCloseOut;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------ ARTEFAKTCLOSEOUT --------');
  process := TFRE_Process.Create(nil);
  try
    res         := process.ExecutePiped('testprog_process',TFRE_DB_StringArray.Create('CLOSEOUT','1000'),'Testtext',outs,errors);
    AssertEquals('ResultCode not 0',0,res);
    AssertEquals('Output not equal',outs,'1234567890');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.InputStreamNil;
var process : TFRE_Process;
    outstream    : TStringStream;
    memerrstream : TStream;
    res          : integer;
    s            : string;
begin
  writeln ('------ INPUTSTREAM NIL --------');
  s:= StringOfChar('x',10)+LineEnding;
  outstream  := TStringStream.Create('');
  outstream.Position:=0;
  memerrstream  := TMemoryStream.Create;
  try
    process  := TFRE_Process.Create(nil);
    try
      res :=  process.ExecutePipedStream('testprog_process',TFRE_DB_StringArray.Create('OUTXWAIT','10'),nil,outstream,memerrstream);
      AssertEquals('ResultCode not 0',0,res);
      AssertEquals('Output not Equal',s,outstream.DataString);
    finally
      process.Free;
    end;
  finally
    outstream.Free;
    memerrstream.Free;
  end;
end;

procedure TFRE_Process_Tests.ArtefaktKill;
var process      : TFRE_Process;
    outstream    : TStringStream;
    memerrstream : TStream;
    res          : integer;
    s            : string;
    kprocess     : TFRE_Process;
    pid          : integer;
    outs         : string;
    errors       : string;
begin
  writeln ('------ ARTEFAKTKILL --------');
  outstream  := TStringStream.Create('');
  outstream.Position:=0;
  memerrstream  := TMemoryStream.Create;
  try
    process  := TFRE_Process.Create(nil);
    kprocess := TFRE_Process.Create(nil);
    try
      process.StartPipedStreamAsync('testprog_process',TFRE_DB_StringArray.Create('OUTXWAIT','10'),nil,outstream,memerrstream);
      pid    := process.ProcessID;
      writeln('PID:',pid);
      res    := kprocess.ExecutePiped('kill',TFRE_DB_StringArray.Create('-s','KILL',inttostr(pid)),'',outs,errors);
      AssertEquals('Errorstring not Empty','',errors);
      AssertEquals('ResultCode not 0',0,res);
      process.WaitForAsyncExecution;
    finally
      kprocess.Free;
      process.Free;
    end;
  finally
    outstream.Free;
    memerrstream.Free;
  end;
end;

procedure TFRE_Process_Tests.Reuse;
var process      : TFRE_Process;
    outstream    : TStringStream;
    instream     : TStringStream;
    memerrstream : TStream;
    res          : integer;
    s            : string;
    outs         : string;
    errors       : string;
    i            : integer;
begin
  writeln ('------ REUSE --------');
  s := '1234567890';
  outstream  := TStringStream.Create('');
  outstream.Position:=0;
  instream  := TStringStream.Create(s);
  instream.Position:=0;
  memerrstream  := TMemoryStream.Create;
  try
    process  := TFRE_Process.Create(nil);
    try
      for i:= 1 to 10 do begin
        res := process.ExecutePiped('testprog_process',TFRE_DB_StringArray.Create('CAT'),s,outs,errors);
        AssertEquals('ResultCode not 0',0,res);
        AssertEquals('Errorstring not Empty','',errors);
        AssertEquals('Output not Equal',s,outs);
      end;
      for i:= 1 to 10 do begin
        outstream.Position:=0;
        instream.Position :=0;
        memerrstream.Position:=0;
        process.StartPipedStreamAsync('testprog_process',TFRE_DB_StringArray.Create('CAT'),instream,outstream,memerrstream);
        res := process.WaitForAsyncExecution;
        AssertEquals('ResultCode not 0',0,res);
        AssertEquals('Output not Equal',s,outstream.DataString);
      end;
    finally
      process.Free;
    end;
  finally
    outstream.Free;
    instream.Free;
    memerrstream.Free;
  end;
end;

procedure TFRE_Process_Tests.AsyncDoubleStart;
var process : TFRE_Process;
    meminstream  : TStream;
    memoutstream : TStream;
    memerrstream : TStream;
    meminstream2 : TStream;
    memoutstream2: TStream;
    memerrstream2: TStream;

    excepted    : boolean;
begin
  writeln ('------ TESTASYNCDOUBLESTART --------');
  meminstream   := TMemoryStream.Create;
  memoutstream  := TMemoryStream.Create;
  memerrstream  := TMemoryStream.Create;
  meminstream2  := TMemoryStream.Create;
  memoutstream2 := TMemoryStream.Create;
  memerrstream2 := TMemoryStream.Create;
  try
    excepted := false;
    process  := TFRE_Process.Create(nil);
    try
      try
        process.StartPipedStreamAsync('testprog_process',TFRE_DB_StringArray.Create('CAT'),meminstream,memoutstream,memerrstream);
        process.StartPipedStreamAsync('testprog_process',TFRE_DB_StringArray.Create('CAT'),meminstream2,memoutstream2,memerrstream2);
        process.WaitForAsyncExecution;
      except on E:EFOS_PROCESS_DOUBLESTART_Exception do begin
        excepted := true;
      end; end;
      AssertEquals('NO EXCEPTION RAISED',true,excepted);
      process.WaitForAsyncExecution;
    finally
      process.Free;
    end;
  finally
    meminstream.Free;
    memoutstream.Free;
    memerrstream.Free;
    meminstream2.Free;
    memoutstream2.Free;
    memerrstream2.Free;
  end;
end;

procedure TFRE_Process_Tests.RemoteSlow;
var multi  : TFRE_DB_Multiprocess;
    proc   : TFRE_DB_Process;
    imulti : integer;
    im,om  : TFRE_DB_Stream;
    nr     : integer;
    outs   : string;
    comps  : string;

begin
  writeln ('------ REMOTE SLOW BUT ONE --------');
  nr    := 20;
  multi := TFRE_DB_Multiprocess.create;
  for imulti := 1 to nr do begin
    proc := TFRE_DB_Process.Create;
    if imulti <> 7 then begin
      proc.SetupInput('/zones/testscript/echosleep.sh');
    end else begin
      proc.SetupInput('/zones/testscript/echofast.sh');
    end;
    multi.AddProcess(proc);
  end;

  multi.SetRemoteSSH(cremoteuser,cremotehost, Getremotekeyfilename);
  multi.ExecuteMulti;

  comps :='a'+LineEnding+'b'+LineEnding+'c'+LineEnding+'d'+LineEnding+'e'+LineEnding;

  // Checkresult
  for imulti := 0 to multi.ProcessCount-1 do begin
    proc := multi.GetProcess(imulti);
    AssertEquals ('Exitstatus not 0',0,proc.ExitStatus);
    outs := proc.OutputToString;
    AssertEquals('Output not equal',outs,comps);
  end;
end;

procedure TFRE_Process_Tests.Cascading;
var process      : TFRE_Process;
    process2     : TFRE_Process;
    process3     : TFRE_Process;
    instream     : TStringStream;
    outstream    : TStringStream;
    memerrstream : TStream;
    memerrstream2: TStream;
    memerrstream3: TStream;
    res1         : integer;
    res2         : integer;
    res3         : integer;
    i            : integer;
    s            : string;
begin
  writeln ('------- CASCADING --------');
  s:='';
  for i:= 0 to 1000000 do begin
    s:=s+'HELLO'+inttostr(i);
  end;
  instream          := TStringStream.Create(s);
  instream.Position := 0;
  outstream         := TStringStream.Create('');
  outstream.Position:= 0;
  memerrstream      := TMemoryStream.Create;
  memerrstream2     := TMemoryStream.Create;

  process  := TFRE_Process.Create(nil);
  process2 := TFRE_Process.Create(nil);
  process3 := TFRE_Process.Create(nil);
  writeln('GO');
  try
    process.PreparePipedStreamAsync('bzip2',nil);
    process2.PreparePipedStreamAsync('cat',nil);
    process3.PreparePipedStreamAsync('bzcat',nil);

    process.SetStreams(instream,process2.Input,memerrstream);
    process2.SetStreams(nil,process3.Input,memerrstream2);
    process3.SetStreams(nil,outstream,memerrstream2);
    process.StartAsync;
    process2.StartAsync;
    process3.StartAsync;
    res1    := process.WaitForAsyncExecution;
    process2.CloseINput;
    res2    := process2.WaitForAsyncExecution;
    process3.CloseINput;
    res3    := process3.WaitForAsyncExecution;
    AssertEquals('Result 1 not 0',0,res1);
    AssertEquals('Result 2 not 0',0,res1);
    AssertEquals('Result 3 not 0',0,res2);
    AssertEquals('Stream Size', length(Instream.Datastring),length(Outstream.Datastring));
    AssertEquals('Outstream not Instream',Instream.DataString,Outstream.DataString);
  finally
    process.Free;
    process2.Free;
    process3.Free;
  end;
end;

procedure TFRE_Process_Tests.OutStreamCallBack(const stream: TStream);
var st: TStringStream;
begin
  writeln('OUTSTREAMCALLBACK:');
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    writeln(st.DataString);
  finally
    st.Free;
  end;
end;

procedure TFRE_Process_Tests.ErrorStreamCallBack(const stream: TStream);
var st: TStringStream;
begin
  writeln('ERRORSTREAMCALLBACK:');
  stream.Position:=0;
  st := TStringStream.Create('');
  try
    st.CopyFrom(stream,stream.Size);
    stream.Size:=0;
    writeln(st.DataString);
  finally
    st.Free;
  end;
end;


procedure TFRE_Process_Tests.Feed;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
    memoutstream: TStream;
    memerrstream: TStream;


begin
  writeln ('------- REMOTE zpool iostat --------');
  process      := TFRE_Process.Create(nil);
  memoutstream := TMemoryStream.Create;
  memerrstream := TMemoryStream.Create;
  try
    process.ConfigureRemote_SSH_Mode(cremoteuser,'10.1.0.102',Getremotekeyfilename);
    process.RegisterCallBacks(@OutStreamCallBack,@ErrorStreamCallBack);
    process.StartPipedStreamAsync('zpool iostat -v zones 1',nil,nil, memoutstream, memerrstream);
//    process.StartPipedStreamAsync(' ps -e -o zone,fname,pid,pmem,pcpu,pri,rss,vsz,zoneid,psr,pset',nil,nil, memoutstream, memerrstream);
    sleep(10000);
    writeln('sleep done');
    process.Terminate(0);
    res := process.WaitForAsyncExecution;
   // AssertEquals ('ResultCode not 0',0,res);
    Assert(length(errors)>0,'Error is not empty');
  finally
    memoutstream.Free;
    memerrstream.Free;
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.FeedLoop;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
    memoutstream: TStream;
    memerrstream: TStream;


begin
  writeln ('------- REMOTE loop enable --------');
  process      := TFRE_Process.Create(nil);
  memoutstream := TMemoryStream.Create;
  memerrstream := TMemoryStream.Create;
  try
    process.ConfigureRemote_SSH_Mode(cremoteuser,'10.1.0.116',Getremotekeyfilename);
    process.RegisterCallBacks(@OutStreamCallBack,@ErrorStreamCallBack);
//    process.StartPipedStreamAsync('zpool iostat -v zones 1',nil,nil, memoutstream, memerrstream);
    process.EnableLoop(1000);
    process.StartPipedStreamAsync('ps',TFRE_DB_StringArray.Create('-e','-o','zone,fname,pid,pmem,pcpu,pri,rss,vsz,zoneid,psr,pset'),nil, memoutstream, memerrstream);
    sleep(5000);
    process.Terminate(0);
    res := process.WaitForAsyncExecution;
   // AssertEquals ('ResultCode not 0',0,res);
    Assert(length(errors)>0,'Error is not empty');
  finally
    memoutstream.Free;
    memerrstream.Free;
    process.Free;
  end;
end;


procedure TFRE_Process_Tests.RemoteSmartOSUname;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------- REMOTE UNAME --------');
  process := TFRE_Process.Create(nil);
  try
    process.ConfigureRemote_SSH_Mode(cremoteuser,cremotehost,Getremotekeyfilename);
    res          := process.ExecutePiped('uname',nil,'',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals ('Output:'+outs,'SunOS'+LineEnding,outs);
    Assert(length(errors)>0,'Error is not empty');
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.RemoteCat;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
begin
  writeln ('------- REMOTE CAT --------');
  process := TFRE_Process.Create(nil);
  try
    process.ConfigureRemote_SSH_Mode (cremoteuser,cremotehost,Getremotekeyfilename);
    res          := process.ExecutePiped('cat',nil,'MyInput',outs,errors);
    AssertEquals ('ResultCode not 0',0,res);
    AssertEquals ('Output','MyInput',outs);
  finally
    process.Free;
  end;
end;

procedure TFRE_Process_Tests.AsyncCat;
var process : TFRE_Process;
    outs    : string;
    errors  : string;
    res     : integer;
    meminstream : TStream;
    memoutstream: TStream;
    memerrstream: TStream;
    i           : integer;
    iproc       : integer;
begin
  writeln ('------- ASYNCMEMSTREAM --------');
  for iproc    := 1 to cmultistream do begin
    meminstream  := TMemoryStream.Create;
    memoutstream := TMemoryStream.Create;
    memerrstream := TMemoryStream.Create;
    try
      for i:=1 to cstreamsize do begin
        meminstream.WriteByte(random(256));
      end;
      meminstream.Position:=0;
      writeln ('------- ASYNCMEMSTREAM ----- '+inttostr(iproc));
      process := TFRE_Process.Create(nil);
      try
        process.StartPipedStreamAsync('cat',nil,meminstream, memoutstream, memerrstream);
        process.WaitForAsyncExecution;
        res    := process.ExitStatus;
    //    writeln('------- EXECUTED ----------');
        AssertEquals ('ResultCode not 0',0,res);

        i:=0;
        meminstream.Position :=0;
        memoutstream.Position:=0;
        AssertEquals ('Outstreamsize',meminstream.Size,memoutstream.Size);
        while (meminstream.Position<meminstream.Size) do begin
          inc (i);
          if meminstream.ReadByte<>memoutstream.ReadByte then begin
            raise Exception.Create ('Byte '+inttostr(i)+' is different ');
          end;
        end;
      finally
        process.Free;
      end;
    finally
      meminstream.Free;
      memoutstream.Free;
      memerrstream.Free;
    end;
  end;
end;


initialization
  RegisterTest(TFRE_Process_Tests);
end.

