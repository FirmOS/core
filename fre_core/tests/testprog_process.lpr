program testprog_process;

{$mode objfpc}{$H+}


uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,Sysutils,BaseUnix,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,FRE_PROCESS,iostream;

var
  count   : integer;
  res     : integer;
  mode    : string;
  i       : integer;
  s       : string;
  buffer  : Array [0..127] of Byte;
  size    : integer;

  procedure Cascade;
  var
    process      : TFRE_Process;
    process2     : TFRE_Process;
    process3     : TFRE_Process;
    res1         : integer;
    res2         : integer;
    res3         : integer;
    stdinstream  : TIOStream;
    stdoutstream : TIOStream;
    stderrstream : TIOStream;


  begin
    stdinstream  := TIOStream.Create(iosInput);
    stdoutstream := TIOStream.Create(iosOutPut);
    stderrstream := TIOStream.Create(iosError);

    process  := TFRE_Process.Create(nil);
    process2 := TFRE_Process.Create(nil);
    process3 := TFRE_Process.Create(nil);
    try
      process.PreparePipedStreamAsync('bzip2',nil);
      process2.PreparePipedStreamAsync('cat',nil);
      process3.PreparePipedStreamAsync('bzcat',nil);

      process.SetStreams(StdInstream,process2.Input,stderrstream);
      process2.SetStreams(nil,process3.Input,stderrstream);
      process3.SetStreams(nil,StdoutStream,stderrstream);
      process.StartAsync;
      process2.StartAsync;
      process3.StartAsync;
      res1    := process.WaitForAsyncExecution;
      process2.CloseINput;
      res2    := process2.WaitForAsyncExecution;
      process3.CloseINput;
      res3    := process3.WaitForAsyncExecution;
    finally
      process.Free;
      process2.Free;
      process3.Free;
    end;
  end;

begin
 mode := uppercase(ParamStr(1));
 if Paramcount>1 then begin
   count   := strtoint(ParamStr(2));
 end else begin
   count := 0;
 end;

 case mode of
  'STDERR': begin
//              for i := 0 to count-1 do begin
//                writestr(s,Copy(inttostr(i)+'________',1,7),LineEnding);
//                filewrite(2,s[1],length(s));
////                writeln (2, Copy(inttostr(i)+'________',1,7));
//              end;
                s:=StringOfChar('x',count);
                res:=fpwrite(StdErrorHandle,s[1],length(s));
                writeln('FilewriteResult:',res);
             end;
  'STDERRNB': begin
                s:=StringOfChar('x',count);

                fpfcntl(StdErrorHandle,F_SETFL,O_NONBLOCK);
                res := fpwrite(StdErrorHandle,s[1],length(s));
                writeln('FilewriteResult:',res);
//                writeln('
             end;
   'CLOSEIN': begin
                FileClose(StdInputHandle);
                sleep(count);
                s:='1234567890';
                res := FileWrite(StdOutputHandle,s[1],length(s));
              end;
   'CLOSEOUT': begin
                s:='1234567890';
                res := FileWrite(StdOutputHandle,s[1],length(s));
                FileClose(StdOutputHandle);
                sleep(count);
              end;
   'CAT':     begin
                repeat
                  size := FileRead(StdInputHandle,buffer,sizeof(buffer));
                  FileWrite(StdOutputHandle,buffer,size);
                until size=0;
              end;
   'OUTXWAIT':begin
                s:=StringOfChar('x',count);
                writeln(s);
                sleep(count*1000);
              end;
   'STDOUTERR':begin
                 writeln('STDOUT');
                 writeln(StdErr,'STDERR');
              end;
   'CASCADE': begin
                Cascade;
   end

   else begin
     writeln('INVALID PARAMETER !');
     halt(99);
   end;
 end;
end.

