unit fre_process;

{
(§LIC)
  (c) Autor,Copyright
      Dipl.Ing.- Helmut Hartl, Dipl.Ing.- Franz Schober, Dipl.Ing.- Christian Koch
      FirmOS Business Solutions GmbH
      www.openfirmos.org
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2013, FirmOS Business Solutions GmbH
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

      * Redistributions of source code must retain the above copyright notice,
        this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright notice,
        this list of conditions and the following disclaimer in the documentation
        and/or other materials provided with the distribution.
      * Neither the name of the <FirmOS Business Solutions GmbH> nor the names
        of its contributors may be used to endorse or promote products derived
        from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
  THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
  PURPOSE ARE DISCLAIMED.
  IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
  AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
(§LIC_END)
} 

{$mode objfpc}{$H+}
{$codepage UTF8}

interface

uses
  Classes, SysUtils,FRE_DB_INTERFACE,FOS_TOOL_INTERFACES,
  process, baseunix, unix, termio, math, pipes;

// sshd on SmartOS only allows 10 unauth. connections as default, change MaxStartups 100:30:200 for 100

const

  c_blocksize        = 131072;
//  c_blocksize        = 192 * 1024;
//  c_blocksize        = 64 * 1024;

type

  EFOS_PROCESS_Exception=class(Exception);
  EFOS_PROCESS_DOUBLESTART_Exception=class(EFOS_PROCESS_Exception);

  TFOS_PROCESS_DataStreamCallback = procedure (const stream:TStream) of object;
  TFOS_PROCESS_ProgressCallback   = procedure (const intotal, outtotal, errortotal: Int64) of object;

  TProcessBuffer = record
    position    : integer;
    size        : integer;
    written     : integer;
    towrite     : integer;
    empty       : boolean;
    btype       : integer;
    totalwritten: int64;
    buffer      : Array [0..c_blocksize-1] of byte;
  end;

  TFRE_Process = class;

  { TFRE_ProcessThread }

  TFRE_ProcessThread = class (TThread)
  private
    fprocess                    :  TFRE_Process;
  public
    constructor                 Create        (const process : TFRE_Process);
    procedure                   Execute;      override;
    property                    Terminated;
  end;

  { TFRE_Process }

  TFRE_Process = class (TProcess)
  private
    fremoteuser                 :  string;
    fremotehost                 :  string;
    fremotekeyfilename          :  string;
    fremoteport                 :  integer;
    fremote                     :  boolean;

    inb                         :  TProcessBuffer;
    outb                        :  TProcessBuffer;
    errb                        :  TProcessBuffer;

    finstream                   :  TStream;
    foutstream                  :  TStream;
    ferrorstream                :  TStream;

    foutstreamcallback          :  TFOS_PROCESS_DataStreamCallback;
    ferrorstreamcallback        :  TFOS_PROCESS_DataStreamCallback;
    fprogresscallback           :  TFOS_PROCESS_ProgressCallback;

    fwait_before_loop_ms        :  Cardinal;

    thread                      :  TFRE_ProcessThread;
    fdebug                      :  boolean;
    floop_enabled               :  boolean;
    fcmd                        :  string;
    fparams                     :  TFRE_DB_StringArray;

    procedure                   InternalStartExecutePiped  (const cmd: string;    const params: TFRE_DB_StringArray);
    procedure                   StartExecutePiped          (const cmd: string;    const params: TFRE_DB_StringArray);
    procedure                   FinishExecutePiped         ;

    procedure                   StreamSelectWrite          (const processinstream : TOutputPipeStream; const outputstream,        stderrorstream       : TStream; out pin_select, out_select, error_select: boolean);
    procedure                   StreamSelectAll            (const instream : TStream; const processoutstream, processerrorstream : TInputPipeStream; const processinstream : TOutputPipeStream; const outputstream,        stderrorstream       : TStream; out in_select, pout_select, perror_select, no_more_input_readable: boolean);
    procedure                   SetNonBlocking             (const stream : TStream);
    procedure                   FileErrorHandler           (const message: string; const errno: integer);
    procedure                   ProcessStreams             (const inputstream, outputstream, stderrorstream: TStream);
    procedure                   Process                    ;

  public
    constructor                 Create (AOwner : TComponent);override;
    function                    Terminate (AExitCode : Integer): Boolean; override;


    procedure                   ConfigureRemote_SSH_Mode   (const user   : string; const host  : string; const keyfilename : string; const port : integer=22);
    procedure                   DisableRemote_SSH_Mode     ;
    function                    ExecutePiped               (const cmd: string;    const params: TFRE_DB_StringArray;  const inputstring: string; out outputstring, errorstring:string): integer;
    function                    ExecutePipedStream         (const cmd: string;    const params: TFRE_DB_StringArray;  const inputstream, outputstream, stderrstream:TStream): integer;
    procedure                   StartPipedStreamAsync      (const cmd: string;    const params: TFRE_DB_StringArray;  const inputstream, outputstream, stderrstream:TStream);
    procedure                   RegisterCallBacks          (const outstreamcallback: TFOS_PROCESS_DataStreamCallback; const errorstreamcallback: TFOS_PROCESS_DataStreamCallback);
    procedure                   RegisterProgressCallback   (const progresscallback: TFOS_PROCESS_ProgressCallback);
    procedure                   EnableLoop                 (const wait_before_loop_ms: Cardinal);
    procedure                   DisableLoop                ;
    function                    EndThreadOrLoop            :boolean;

    procedure                   PreparePipedStreamAsync    (const cmd: string;    const params: TFRE_DB_StringArray);
    procedure                   SetStreams                 (const inputstream, outputstream, stderrorstream: TStream);
    procedure                   StartAsync                 ;
    function                    IsRunning                  : boolean;

    function                    WaitForAsyncExecution      : integer;
  end;


  { TFRE_DB_Process }

  TFRE_DB_Process = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme  (const scheme: IFRE_DB_SCHEMEOBJECT); override;
    function        StreamToString    (const streamname : string) : string;
  public
    procedure       SetupInput        (const cmd:string ; params : TFRE_DB_StringArray=nil ; const input_stream:TFRE_DB_Stream=nil);
    function        ExitStatus        : integer;
    function        GetErrorStream    : TFRE_DB_Stream;
    function        GetInputStream    : TFRE_DB_Stream;
    function        GetOutputStream   : TFRE_DB_Stream;
    function        OutputToString    : string;
    function        ErrorToString     : string;
  end;

  { TFRE_DB_Multiprocess }

  TFRE_DB_Multiprocess =class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  public
    function  ProcessCount              : Integer;
    function  GetProcess                (const idx:integer):TFRE_DB_Process;
    procedure AddProcess                (const proc : TFRE_DB_Process);
    procedure ClearProcess              ;
    procedure SetRemoteSSH              (const user   : string; const host  : string; const keyfilename : string; const port : integer = 22);
    procedure ExecuteMulti              ;
  end;

function   FRE_ProcessCMD(const cmd:string):integer;
function   FRE_ProcessCMD(const cmd:string; var outstring,errorstring:string):integer;
function   FRE_ProcessCMDException(const cmd:string):integer;

procedure  Register_DB_Extensions;


implementation

{ TFRE_ProcessThread }

constructor TFRE_ProcessThread.Create(const process: TFRE_Process);
begin
  fprocess  := process;
  inherited Create(false);
end;

procedure TFRE_ProcessThread.Execute;
begin
  try
    repeat
      fprocess.Process;
    until fprocess.EndThreadOrLoop;
  except on E: Exception do begin
    GFRE_BT.CriticalAbort('THREAD EXCEPTION '+inttostr(cardinal(GetCurrentThreadId))+' :'+E.Message);
  end; end;
  Terminate;
end;

{ TFRE_DB_Multiprocess }

class procedure TFRE_DB_Multiprocess.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField('remoteuser',fdbft_String);
  scheme.AddSchemeField('remotehost',fdbft_String);
  scheme.AddSchemeField('remotekeyfilename',fdbft_String);
  scheme.AddSchemeField('process',fdbft_Object).multiValues:=true;
end;

function TFRE_DB_Multiprocess.ProcessCount: Integer;
begin
  result := Field('process').ValueCount;
end;

function TFRE_DB_Multiprocess.GetProcess(const idx: integer): TFRE_DB_Process;
begin
  result := Field ('process').AsObjectItem[idx].Implementor_HC as TFRE_DB_Process;
end;

procedure TFRE_DB_Multiprocess.AddProcess(const proc: TFRE_DB_Process);
begin
  Field('process').AddObject (proc);
end;

procedure TFRE_DB_Multiprocess.ClearProcess;
begin
  Field('process').Clear;
end;

procedure TFRE_DB_Multiprocess.SetRemoteSSH(const user: string; const host: string; const keyfilename: string; const port: integer);
begin
  if (host<>'') then begin
    Field ('remoteuser').AsString        := user;
    Field ('remotehost').AsString        := host;
    Field ('remotekeyfilename').AsString := keyfilename;
    Field ('remoteport').asint32         := port;
  end;
end;

procedure TFRE_DB_Multiprocess.ExecuteMulti;
var
    imulti     : integer;
    res        : integer;
    processi   : IFRE_DB_Object;
    process    : Array of TFRE_Process;
begin
  if ProcessCount=0 then begin
    raise EFOS_PROCESS_Exception.Create('NO PROCESSES DEFINED FOR EXECUTION !');
  end;

  SetLength (process, Field ('process').ValueCount);
  for imulti := 0 to Field ('process').ValueCount-1 do begin
    processi := Field ('process').AsObjectItem[imulti];
    process [imulti] := TFRE_Process.Create (nil);
    if FieldExists('remotehost') then begin
      process [imulti].ConfigureRemote_SSH_Mode(Field ('remoteuser').AsString, Field ('remotehost').AsString, Field ('remotekeyfilename').AsString, Field ('remoteport').Asint32);
    end;
    process [imulti].PreparePipedStreamAsync(processi.Field('cmd').AsString, processi.Field('params').AsStringArr);
    process [imulti].SetStreams(processi.Field ('instream').AsStream,processi.Field ('outstream').AsStream,processi.Field ('errorstream').AsStream);
  end;
  for imulti := 0 to Field ('process').ValueCount-1 do begin
      process [imulti].StartAsync;
  end;
  try
    for imulti := 0 to Field ('process').ValueCount-1 do begin
      processi :=     Field ('process').AsObjectItem[imulti];
      process[imulti].WaitForAsyncExecution;
      processi.Field ('exitstatus').AsInt32   := Process [imulti].ExitStatus;
      processi.Field ('errorstring').AsString := TFRE_DB_Process (processi.Implementor_HC).ErrorToString;
//      writeln(processi.DumpToString);
//      process[imulti].FinishExecutePiped;
    end;
  finally
    for imulti := 0 to Field ('process').ValueCount-1 do begin
      process[imulti].Free;
    end;
  end;
end;


{ TFRE_DB_Process }

class procedure TFRE_DB_Process.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField('cmd',fdbft_String).required:=true;
  scheme.AddSchemeField('params',fdbft_String).multiValues:=true;
  scheme.AddSchemeField('instream',fdbft_Stream).multiValues:=false;
  scheme.AddSchemeField('outstream',fdbft_Stream).multiValues:=false;
  scheme.AddSchemeField('errorstream',fdbft_Stream).multiValues:=false;
  scheme.AddSchemeField('exitstatus',fdbft_Int32).multiValues:=false;
end;

function TFRE_DB_Process.OutputToString: string;
begin
  result  := StreamToString ('outstream');
end;

function TFRE_DB_Process.ErrorToString: string;
begin
  result  := StreamToString ('errorstream');
end;

function TFRE_DB_Process.StreamToString(const streamname: string): string;
var sstream  : TStringStream;
begin
  sstream := TStringStream.Create('');
  try
    Field   (streamname).asstream.Position:=0;
    sstream.CopyFrom(Field (streamname).asstream,Field (streamname).asstream.Size);
    result  := sstream.DataString;
  finally
    sstream.Free;
  end;
end;

procedure TFRE_DB_Process.SetupInput(const cmd: string; params: TFRE_DB_StringArray; const input_stream: TFRE_DB_Stream);
begin
  Field('cmd').AsString       := cmd;
  Field('params').AsStringArr := params;
  if assigned(input_stream) then begin
    Field('instream').AsStream  := input_stream;
  end;
end;

function TFRE_DB_Process.ExitStatus: integer;
begin
  result := Field('exitstatus').AsInt32;
end;

function TFRE_DB_Process.GetErrorStream: TFRE_DB_Stream;
begin
  result := Field('errorstream').asstream;
end;

function TFRE_DB_Process.GetInputStream: TFRE_DB_Stream;
begin
  result := Field('instream').AsStream;
end;

function TFRE_DB_Process.GetOutputStream: TFRE_DB_Stream;
begin
  result := Field ('outstream').AsStream;
end;


{ TFRE_Process }
{$ifdef solaris}
  const FD_CLOEXEC=1;
{$endif}

{$ifdef linux}
  const FD_CLOEXEC=1;
{$endif}

procedure TFRE_Process.InternalStartExecutePiped(const cmd: string; const params: TFRE_DB_StringArray);
var iparam  : integer;
    fpres   : integer;
begin
  fcmd      := cmd;    // save for reuse in loop
  fparams   := params;
  if assigned (params) then begin
    if fremote then begin
      Executable         := 'ssh';
      Parameters.Clear;
      Parameters.Add     ('-i'+fremotekeyfilename);
      Parameters.Add     ('-p'+inttostr(fremoteport));
      Parameters.Add     (fremoteuser+'@'+fremotehost);
      Parameters.Add     (cmd);
    end else begin
      Executable         := cmd;
      Parameters.Clear;
    end;
    if length(params)>0 then begin
      for iparam:=low(params) to high(params) do begin
        Parameters.Add (params[iparam]);
      end;
    end;
  end else begin       // direct command, no parameters
    Parameters.Clear;
    if fremote then begin
      CommandLine := 'ssh -i'+fremotekeyfilename+' '+fremoteuser+'@'+fremotehost+' '+cmd;
    end else begin
      CommandLine := cmd;
    end;
  end;

  Options            := [poUsePipes];
  Execute;
  fpres := fpfcntl(Input.Handle, F_SETFD, FD_CLOEXEC);
  if (fpres <> 0) then FileErrorHandler('FPCNTL CLOSE EXEC INPUT HANDLER',fpgeterrno);
  fpres := fpfcntl(Output.Handle, F_SETFD, FD_CLOEXEC);
  if (fpres <> 0) then FileErrorHandler('FPCNTL CLOSE EXEC OUTPUT HANDLER',fpgeterrno);
  fpres := fpfcntl(StdErr.Handle, F_SETFD, FD_CLOEXEC);
  if (fpres <> 0) then FileErrorHandler('FPCNTL CLOSE EXEC STDERR HANDLER',fpgeterrno);
end;

constructor TFRE_Process.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fremote       := false;
  floop_enabled := false;
end;

function TFRE_Process.Terminate(AExitCode: Integer): Boolean;
begin
  floop_enabled := false;
  Result:=inherited Terminate(AExitCode);
end;

procedure TFRE_Process.ConfigureRemote_SSH_Mode(const user: string; const host: string; const keyfilename: string; const port: integer);
begin
  if length(user)=0 then begin
    raise EFOS_PROCESS_Exception.Create ('Username for remote process must be set');
  end;
  if length(host)=0 then begin
    raise EFOS_PROCESS_Exception.Create ('Host for remote process must be set');
  end;
  if FileExists(keyfilename)=false then begin
    raise EFOS_PROCESS_Exception.Create ('Keyfile '+keyfilename+' does not exist ');
  end;
  fremoteuser        := user;
  fremotehost        := host;
  fremotekeyfilename := keyfilename;
  fremoteport        := port;
  fremote            := true;
end;

procedure TFRE_Process.DisableRemote_SSH_Mode;
begin

end;


function TFRE_Process.ExecutePiped(const cmd: string; const params: TFRE_DB_StringArray; const inputstring: string; out outputstring, errorstring: string): integer;
var instringstream  : TStringStream;
    outstringstream : TStringStream;
    errstringstream : TStringStream;
begin
  instringstream     := TStringStream.Create(inputstring);
  outstringstream    := TStringStream.Create('');
  errstringstream    := TStringStream.Create('');
  try
    instringstream.Position  := 0;
    outstringstream.Position := 0;
    InternalStartExecutePiped(cmd,params);
    ProcessStreams(instringstream,outstringstream,errstringstream);
    result             := ExitStatus;
    outputstring       := outstringstream.DataString;
    errorstring        := errstringstream.DataString;
  finally
    errstringstream.Free;
    outstringstream.Free;
    instringstream.Free;
  end;
end;

function TFRE_Process.ExecutePipedStream(const cmd: string; const params: TFRE_DB_StringArray; const inputstream, outputstream, stderrstream: TStream): integer;
begin
  InternalStartExecutePiped(cmd,params);
  ProcessStreams(inputstream,outputstream,stderrstream);
  FinishExecutePiped;
  result := ExitStatus;
end;

procedure TFRE_Process.StartPipedStreamAsync(const cmd: string; const params: TFRE_DB_StringArray; const inputstream, outputstream, stderrstream: TStream);
begin
  PreparePipedStreamAsync(cmd,params);
  SetStreams(inputstream,outputstream,stderrstream);
  StartAsync;
end;

procedure TFRE_Process.RegisterCallBacks(const outstreamcallback: TFOS_PROCESS_DataStreamCallback; const errorstreamcallback: TFOS_PROCESS_DataStreamCallback);
begin
  foutstreamcallback   := outstreamcallback;
  ferrorstreamcallback := errorstreamcallback;
end;

procedure TFRE_Process.RegisterProgressCallback(const progresscallback: TFOS_PROCESS_ProgressCallback);
begin
  fprogresscallback  := progresscallback;
end;

procedure TFRE_Process.EnableLoop(const wait_before_loop_ms: Cardinal);
begin
  fwait_before_loop_ms:= wait_before_loop_ms;
  floop_enabled       := true;
end;

procedure TFRE_Process.DisableLoop;
begin
  floop_enabled:=false;
end;

function TFRE_Process.EndThreadOrLoop: boolean;
begin
  if floop_enabled=false then begin
    result := true;  // End Thread
  end else begin
    FinishExecutePiped;
    if fwait_before_loop_ms>0 then begin
      sleep(fwait_before_loop_ms);
    end;
    InternalStartExecutePiped(fcmd,fparams);  // relaunch process
    result := false;
  end;
end;

procedure TFRE_Process.PreparePipedStreamAsync(const cmd: string; const params: TFRE_DB_StringArray);
begin
  if assigned(thread) then begin
    raise EFOS_PROCESS_DOUBLESTART_Exception.Create('ASYNC EXECUTION ALREADY STARTED');
  end;
  InternalStartExecutePiped(cmd,params);
 // writeln('PROCESS STARTED:',ProcessID,' HANDLES INPUT:',Input.Handle,' OUTPUT:',Output.Handle,' ERROR:',Stderr.Handle);
end;

procedure TFRE_Process.Process;
var
  pin_select    : boolean;
  out_select    : boolean;
  error_select  : boolean;
  istream       : TStream;
  poutstream    : TInputPipeStream;
  perrstream    : TInputPipeStream;
  in_select     : boolean;
  pout_select   : boolean;
  perror_select : boolean;
  errno         : integer;
  no_more_input_readable: boolean;
  process_input_closed  : boolean;
  closeresult   : integer;
  pinstream     : TOutputPipeStream;
  ostream       : TStream;
  estream       : TStream;

  procedure _Write(var b:TProcessBuffer;const stream:TStream);
  begin
    b.towrite     := b.size-b.position;
//    writeln ('TID:',cardinal(GetCurrentThreadId),' BTYPE:',b.btype,' TOWRITE:',b.towrite,' POSITION:',b.position);
    if (stream is THandleStream) then begin
      b.written     := FileWrite(THandleStream(stream).Handle,b.buffer[b.position],b.towrite);
    end else begin
      b.written     := stream.Write(b.buffer,b.size);
    end;
    if b.written=-1 then begin
      errno       := fpgeterrno;
      case errno of
        ESysEAGAIN : begin
//                      writeln('SYSEAGAIN');
                      b.empty := false;  // no bytes written
                      exit;
                    end;
        else begin
//          writeln('BTYPE:',b.btype,' POSTION:',b.position,' TO WRITE:',b.towrite,' WRITTEN:',b.written,' SIZE:',b.size,' POSITION:',b.position);
//          writeln('FILE WRITE ERROR ERRNO:',errno);
          FileErrorHandler('FILE WRITE ',errno);
        end;
      end;
    end;
//    writeln ('TID:',cardinal(GetCurrentThreadId),' BTYPE:',b.btype,' WRITE:',b.written,' POSITION:',b.position);
    inc(b.totalwritten,b.written);
    if (b.written < b.towrite) then begin
      b.position  := b.position + b.written;
      b.empty     := false;
//      writeln('NOT EMPTY:',b.position);
    end else begin
      b.position  := 0;
      b.empty     := true;
//      writeln('EMPTY');
    end;
//    writeln('TID:',cardinal(GetCurrentThreadId),' BTYPE:',b.btype,' POS:',b.position,' EMPTY:',Booltostr(b.empty,true));
    if Assigned(fprogresscallback) then
      fprogresscallback(inb.totalwritten,outb.totalwritten,errb.totalwritten);
  end;

  procedure _Read(var b:TProcessBuffer;const stream:TStream;const readsize:integer);
  begin
    //if b.position<>0 then begin
    //  GFRE_BT.CriticalAbort('POSTION BEFORE READ IS NOT 0 ! BTYPE:'+inttostr(b.btype)+' POSITION:'+inttostr(b.position)+' EMPTY:'+BoolToStr(b.empty,true));
    //end;
    if (stream is THandleStream) then begin
      b.size     := FileRead(THandleStream(stream).handle,b.buffer,readsize);
      if b.size=-1 then FileErrorHandler('FILE READ PROCESS STREAM',fpgeterrno);
    end else begin
      b.size     := stream.Read(b.buffer,c_blocksize);
    end;
//    writeln ('TID:',cardinal(GetCurrentThreadId),' BTYPE:',b.btype,' READ POS:',b.position,' SIZE:',b.size);
  end;

  procedure _ClearBuffer(var b:TProcessBuffer);
  begin
    b.position     := 0;
    b.totalwritten := 0;
    b.empty        := true;
  end;

begin
//  writeln('PROCESS STARTED TID:',cardinal(GetCurrentThreadId));
  SetNonBlocking(Input);
  SetNonBlocking(foutstream);
  SetNonBlocking(ferrorstream);

  _ClearBuffer(inb);  inb.btype  := 0;
  _ClearBuffer(outb); outb.btype := 1;
  _ClearBuffer(errb); errb.btype := 2;

  no_more_input_readable:=false;
  process_input_closed  :=false;

  while Running do begin
//    writeln('SELW PREPARE:',cardinal(GetCurrentThreadId));
    StreamSelectWrite(Input, foutstream, ferrorstream, pin_select, out_select, error_select);
//    writeln('SELW RESULT:',cardinal(GetCurrentThreadId),' INPUT:',BoolToStr(pin_select,true),' OUTSTREAM:',BoolToStr(out_select,true),' ERRSTREAM:',BoolToStr(error_select,true));
    if pin_select then begin
      if (inb.empty=false) then begin
        _Write(inb,Input);
      end;
    end;
    if out_select then begin
      if (outb.empty=false) then begin
        _Write(outb,foutstream);
        if Assigned(foutstreamcallback) then begin
          foutstreamcallback(foutstream);
        end;
      end;
    end;
    if error_select then begin
      if (errb.empty=false) then begin
        _Write(errb,ferrorstream);
        if Assigned(ferrorstreamcallback) then begin
          foutstreamcallback(ferrorstream);
        end;
      end;
    end;

    if inb.empty and (not no_more_input_readable)      then   istream    := finstream    else istream    := nil;
    if outb.empty                                      then   poutstream := Output       else poutstream := nil;
    if errb.empty                                      then   perrstream := Stderr       else perrstream := nil;
    if (pin_select=false) or (inb.empty=false)         then   pinstream  := Input        else pinstream  := nil;
    if (out_select=false) or (outb.empty=false)        then   ostream    := foutstream   else ostream    := nil;
    if (error_select=false) or (errb.empty=false)      then   estream    := ferrorstream else estream    := nil;

//    writeln('SELR PREPARE:',cardinal(GetCurrentThreadId),' IN:',BoolToStr(assigned(istream),true),' POUT:',BoolToStr(assigned(poutstream),true),' PERR:',BoolToStr(assigned(perrstream),true));
    StreamSelectAll(istream, poutstream,perrstream, pinstream, ostream, estream, in_select, pout_select, perror_select,no_more_input_readable);
//    writeln('SELR RESULT:',cardinal(GetCurrentThreadId),' IN:',BoolToStr(in_select,true),' POUT:',BoolToStr(pout_select,true),' PERR:',BoolToStr(perror_select,true));
    if (in_select) and (inb.empty) then begin
      if (not no_more_input_readable) then begin
        _Read(inb,istream,c_blocksize);
        _Write(inb,Input);
      end else begin
        if (process_input_closed=false) then begin
//          writeln('CLOSING INPUT ',Input.Handle);
          closeresult := FpClose(Input.Handle);
          if closeresult<>0 then FileErrorHandler('FILE INPUT PIPE CLOSE',fpgeterrno);
          CloseInput;
          process_input_closed := true;
        end;
      end;
    end;
    if (pout_select) and (outb.empty) then begin
//      writeln('OUTP ',cardinal(GetCurrentThreadId),' EMPTY:',Booltostr(outb.empty,true));
      _Read(outb,poutstream,Min(c_blocksize,poutstream.NumBytesAvailable));
      _Write(outb,foutstream);
      if Assigned(foutstreamcallback) then begin
        foutstreamcallback(foutstream);
      end;
    end;
    if (perror_select) and (errb.empty) then begin
      _Read(errb,perrstream,Min(c_blocksize,perrstream.NumBytesAvailable));
      _Write(errb,ferrorstream);
      if Assigned(ferrorstreamcallback) then begin
        ferrorstreamcallback(ferrorstream);
      end;
    end;
  end;

  repeat
    if assigned(Output) and (outb.empty) then begin
      _Read(outb,Output,Min(c_blocksize,Output.NumBytesAvailable));
    end;
    _Write(outb,foutstream);
    if Assigned(foutstreamcallback) then begin
      foutstreamcallback(foutstream);
    end;
    if assigned(StdErr) and (errb.empty) then begin
      _Read(errb,Stderr,Min(c_blocksize,StdErr.NumBytesAvailable));
    end;
    _Write(errb,ferrorstream);
    if Assigned(ferrorstreamcallback) then begin
      ferrorstreamcallback(ferrorstream);
    end;
  until (outb.empty) and (outb.size=0) and (errb.empty) and (errb.size=0);

//  writeln('TOTAL IN WRITTEN:',inb.totalwritten);
//  writeln('TOTAL OUT WRITTEN:',outb.totalwritten);
end;

procedure TFRE_Process.StartExecutePiped(const cmd: string; const params: TFRE_DB_StringArray);
begin
  InternalStartExecutePiped(cmd,params);
end;

procedure TFRE_Process.FinishExecutePiped;
begin
  CloseOutput;
  CloseStderr;
  CloseInput;
end;

procedure TFRE_Process.StreamSelectWrite(const processinstream: TOutputPipeStream; const outputstream, stderrorstream: TStream; out pin_select, out_select, error_select: boolean);
var
  ds           : Tfdset;
  ihandles     : integer;
  maxhandle    : integer;
  sresult      : integer;
  errno        : integer;
  timeout      : integer;
begin
//  writeln('SELW');
  fpfd_zero(ds);
  maxhandle   := 0;
  pin_select  := false;
  out_select  := false;
  error_select:= false;

  if assigned(processinstream) then begin
    fpfd_set(processinstream.Handle,ds);
    maxhandle := Max (maxhandle, processinstream.Handle);
  end;
  if assigned(outputstream) then begin
    if (outputstream is THandleStream) then begin
      fpfd_set(THandleStream(outputstream).Handle,ds);
      maxhandle  := Max (maxhandle, THandleStream(outputstream).Handle);
    end else begin
      out_select := true;
    end;
  end;
  if assigned(stderrorstream) then begin
    if (stderrorstream is THandleStream) then begin
      fpfd_set(THandleStream(stderrorstream).Handle,ds);
      maxhandle  := Max (maxhandle, THandleStream(stderrorstream).Handle);
    end else begin
      error_select := true;
    end;
  end;
  if (error_select) or (out_select) then begin
    timeout        := 0;  //immediate, poll
  end else begin
    timeout        := -1; //infinite
  end;

  if (maxhandle>0) then begin
    sresult   := fpSelect (maxhandle+1,nil,@ds,nil,timeout);
    if (sresult=-1) then begin
      errno   := fpgeterrno;
      case errno of
        ESysEINTR : begin
                     //ignore
                    end;
        else FileErrorHandler('ERROR ON STREAM SELECT WRITE :',errno);
      end;
    end else begin
      if (sresult>0) then begin
        if assigned(processinstream)                                      then pin_select   := (fpFD_ISSET(processinstream.Handle,ds)<>0);
        if assigned(outputstream) and (outputstream is THandleStream)     then out_select   := (fpFD_ISSET(THandleStream(outputstream).Handle,ds)<>0);
        if assigned(stderrorstream) and (stderrorstream is THandleStream) then error_select := (fpFD_ISSET(THandleStream(stderrorstream).Handle,ds)<>0);
      end;
    end;
  end;
end;

procedure TFRE_Process.StreamSelectAll(const instream: TStream; const processoutstream, processerrorstream: TInputPipeStream; const processinstream: TOutputPipeStream; const outputstream, stderrorstream: TStream; out in_select, pout_select, perror_select, no_more_input_readable: boolean);
var
  ds           : Tfdset;
  wds          : Tfdset;
  ihandles     : integer;
  maxhandle    : integer;
  sresult      : integer;
  errno        : integer;
  timeout      : integer;

  function InputBytesAvailable(const stream : THandleStream): Dword;
  begin
    if fpioctl(stream.Handle, FIONREAD, @Result)<0 then Result := 0;
  end;

begin
  fpfd_zero(ds);
  fpfd_zero(wds);

  maxhandle   := 0;
  in_select              := false;
  pout_select            := false;
  perror_select          := false;
  no_more_input_readable := false;

  // prevent lock, only inserted if previous not writeable
  if assigned(processinstream) then begin
    fpfd_set(processinstream.Handle,wds);
    maxhandle := Max (maxhandle, processinstream.Handle);
  end;
  if assigned(outputstream) then begin
    if (outputstream is THandleStream) then begin
      fpfd_set(THandleStream(outputstream).Handle,wds);
      maxhandle  := Max (maxhandle, THandleStream(outputstream).Handle);
    end;
  end;
  if assigned(stderrorstream) then begin
    if (stderrorstream is THandleStream) then begin
      fpfd_set(THandleStream(stderrorstream).Handle,wds);
      maxhandle  := Max (maxhandle, THandleStream(stderrorstream).Handle);
    end;
  end;

  if assigned(instream) then begin
    if (instream is THandleStream) then begin
      fpfd_set(THandleStream(instream).Handle,ds);
      maxhandle  := Max (maxhandle, THandleStream(instream).Handle);
    end else begin
      if (instream.Position<instream.Size) then begin
        in_select  := true;
      end else begin
        in_select              := true;
        no_more_input_readable := true;
      end;
    end;
  end;

  if assigned(processoutstream) then begin
    fpfd_set(processoutstream.Handle,ds);
    maxhandle := Max (maxhandle, processoutstream.Handle);
  end;

  if assigned(processerrorstream) then begin
    fpfd_set(processerrorstream.Handle,ds);
    maxhandle := Max (maxhandle, processerrorstream.Handle);
  end;

  if (in_select) then begin
    timeout        := 0;  //immediate, poll
  end else begin
    timeout        := -1; //infinite
//    timeout        := 1000;
  end;

  if (maxhandle>0) then begin
    sresult   := fpSelect (maxhandle+1,@ds,@wds,nil,timeout);
    if (sresult=-1) then begin
      errno   := fpgeterrno;
      case errno of
        ESysEINTR : begin
                     //ignore
                    end;
        else FileErrorHandler('ERROR ON STREAM SELECT READ :',errno);
      end;
    end else begin
      if (sresult>0) then begin
        if (assigned(instream) and (instream is THandleStream)) then begin
          in_select      := (fpFD_ISSET(THandleStream(instream).Handle,ds)<>0);
          if in_select then begin
            no_more_input_readable := (InputBytesAvailable(THandleStream(instream))=0);
          end;
        end;
        if assigned(processoutstream)                           then pout_select    := (fpFD_ISSET(THandleStream(processoutstream).Handle,ds)<>0);
        if assigned(processerrorstream)                         then perror_select  := (fpFD_ISSET(THandleStream(processerrorstream).Handle,ds)<>0);
      end;
    end;
  end;
end;

procedure TFRE_Process.SetNonBlocking(const stream: TStream);
begin
  if (stream is THandleStream) then begin
    fpfcntl(THandleStream(stream).Handle,F_SETFL,O_NONBLOCK);
  end;
end;

procedure TFRE_Process.FileErrorHandler(const message: string; const errno: integer);
begin
  raise  EXception.Create('PROCESS FILE ERROR:'+message+' ERRNO:'+inttostr(errno));
end;

procedure TFRE_Process.SetStreams(const inputstream, outputstream, stderrorstream: TStream);
begin
  finstream      := inputstream;
  foutstream     := outputstream;
  ferrorstream   := stderrorstream;
end;

procedure TFRE_Process.StartAsync;
begin
  thread    := TFRE_ProcessThread.Create(self);
end;

function TFRE_Process.IsRunning: boolean;
begin
  if assigned(thread) then
    result := not thread.Terminated
  else
    result := false;
end;

procedure TFRE_Process.ProcessStreams(const inputstream, outputstream, stderrorstream: TStream);
begin
  SetStreams(inputstream,outputstream,stderrorstream);
  Process;
end;

function TFRE_Process.WaitForAsyncExecution: integer;
begin
  thread.WaitFor;
  thread.Free;
  thread  :=  nil;
  result  :=  ExitStatus;
end;

function FRE_ProcessCMD(const cmd: string): integer;
var outstring   : string;
    errorstring : string;
begin
  result := FRE_ProcessCMD(cmd,outstring,errorstring);
  if length(outstring)<>0 then writeln (outstring);
  if length(errorstring)<>0 then writeln (errorstring);
end;

function FRE_ProcessCMD(const cmd: string; var outstring, errorstring: string): integer;
var prc         : TFRE_Process;
begin
  prc      := TFRE_Process.Create(nil);
  try
    writeln(cmd);
    result := prc.ExecutePiped(cmd,nil,'',outstring,errorstring);
  finally
    prc.free;
  end;
end;

function FRE_ProcessCMDException(const cmd: string): integer;
var outstring   : string;
    errorstring : string;
begin
  result := FRE_ProcessCMD(cmd,outstring,errorstring);
  if result<>0 then begin
    raise Exception.Create('Exception raised: Resultcode:'+inttostr(result)+' ERROR:'+errorstring+' OUTPUT:'+outstring);
  end;
end;

procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Process);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Multiprocess);
  //GFRE_DBI.Initialize_Extension_Objects;
end;

end.

