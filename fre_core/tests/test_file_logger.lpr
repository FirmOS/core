program test_file_logger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils,Classes,systemlog,
  FOS_TOOL_INTERFACES;

   type

     { TTestThread }

     TTestThread=class(TThread)
       public
        CAT,TARGET:String;
        procedure Execute;override;
     end;

   { TTestThread }

   procedure TTestThread.Execute;
   var i:integer;
   begin
    i:=1;
    while not terminated do begin
      inc(i);
      case random(5) of
        1: CAT:='CAT A';
        2: CAT:='CAT B';
        3: CAT:='CAT C';
        else CAT:='CAT X'
      end;
      GFRE_LOG.Log('ENTRY '+inttostr(i),CAT,fll_Error,TARGET);
      SLEEP(1);
    end;
   end;

   var i:integer;
       A:Array [1..3] of TTestThread;


begin
  writeln('STARTING Log test');
  RemoveDir('testlogs');

  writeln('TEST SYSTEMLOG');
//  GFRE_LOG.LogSystem('HAVE A KERN TEST',flf_Kernel,fll_Alert);
  GFRE_LOG.SetDefaults('main.log','full.log',GetCurrentDir+DirectorySeparator+'testlogs',1000000,10,fll_Error);
  GFRE_LOG.LogSystem('HAVE A KERN ALERT',flf_Kernel,fll_Alert);
  GFRE_LOG.LogSystem('HAVE A LOCAL ERROR',flf_Local1,fll_Error);
  GFRE_LOG.LogSystem('HAVE A LOCAL WARNING',flf_Local1,fll_Warning);
  GFRE_LOG.LogSystem('HAVE A LOCAL DEBUG',flf_Local1,fll_Debug);
  GFRE_LOG.EnableSyslog;

  GFRE_LOG.RegisterTarget('VOICE','voice',100000,5,fll_Warning);
  GFRE_LOG.RegisterTarget('LOGIC','logic',-1,-1,fll_Debug,flf_Local1);
  GFRE_LOG.RegisterTarget('MAIN','main',-1,-1,fll_Error);

  GFRE_LOG.Log ('WARNING LOGIC','',fll_Warning,'LOGIC',false);
  GFRE_LOG.Log ('WARNING MAIN','',fll_Warning,'MAIN',false);
  GFRE_LOG.Log ('WARNING VOICE','',fll_Warning,'VOICE',false);
  GFRE_LOG.Log ('ERROR LOGIC','',fll_Error,'LOGIC',false);
  GFRE_LOG.Log ('ERROR VOICE','',fll_Error,'VOICE',false);
  GFRE_LOG.Log ('ERROR MAIN','',fll_Error,'MAIN',false);
  GFRE_LOG.Log ('DEBUG LOGIC','',fll_debug,'LOGIC',false);
  GFRE_LOG.Log ('DEBUG VOICE','',fll_Debug,'VOICE',false);
  GFRE_LOG.Log ('DEBUG MAIN','',fll_Debug,'MAIN',false);

  GFRE_LOG.RegisterCategory('CAT A','cata.log',3000000,3,fll_Error);
  GFRE_LOG.RegisterCategory('CAT B','catb.log',4000000,4);
  GFRE_LOG.RegisterCategory('CAT C','catc.log',5000000,5);

  GFRE_LOG.Log('LOGSTART A 1','CAT A');
  GFRE_LOG.Log('LOGSTART B 2','CAT B',fll_Alert,'UNKNOWN');
  GFRE_LOG.Log('LOGSTART C 3','CAT C',fll_Critical,'UNKNOWN');
  GFRE_LOG.Log('LOGSTART D 4','CAT D',fll_Warning,'MAIN');

  GFRE_LOG.Log('ENTRY Logged to target MAIN 5','CAT N',fll_Error,'MAIN');
  GFRE_LOG.Log('ENTRY Logged to CAT A / MAIN 6','CAT A',fll_Error,'MAIN');
  GFRE_LOG.Log('ENTRY Logged to CAT B / MAIN 7','CAT B',fll_Error,'MAIN');
  GFRE_LOG.Log('ENTRY Logged to CAT C / MAIN 8','CAT C',fll_Error,'MAIN');

  GFRE_LOG.Log('ENTRY Logged to target VOICE 9','CAT N',fll_Error,'VOICE');
  GFRE_LOG.Log('ENTRY Logged to CAT A / VOICE 10','CAT A',fll_Error,'VOICE');
  GFRE_LOG.Log('ENTRY Logged to CAT B / VOICE 11','CAT B',fll_Error,'VOICE');
  GFRE_LOG.Log('ENTRY Logged to CAT C / VOICE 12','CAT C',fll_Error,'VOICE');

  GFRE_LOG.Log('ENTRY Logged to target LOGIC 13','CAT N',fll_Error,'LOGIC');
  GFRE_LOG.Log('ENTRY Logged to CAT A / LOGIC 14','CAT A',fll_Error,'LOGIC');
  GFRE_LOG.Log('ENTRY Logged to CAT B / LOGIC 15','CAT B',fll_Error,'LOGIC');
  GFRE_LOG.Log('ENTRY Logged to CAT C / LOGIC 16','CAT C',fll_Error,'LOGIC');

  GFRE_LOG.Sync_Logger;
  exit;

  GFRE_LOG.DisableSyslog;

  for i:=1 to 3 do begin
    A[i]:=TTestThread.Create(true);
  end;
  A[1].TARGET:='MAIN';
  A[2].TARGET:='LOGIC';
  A[3].TARGET:='VOICE';
  for i:=1 to 3 do begin
    A[i].Resume;
  end;
  GFRE_LOG.Sync_Logger;
  sleep(2000);
  writeln('Terminating Threads');
  for i:=1 to 3 do begin
    A[i].Terminate;
    writeln(i);
    A[i].WaitFor;
  end;
  writeln('DONE');
end.

