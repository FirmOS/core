unit fos_file_logger;

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

interface

{$IFDEF FPC}
 {$MODE objfpc}{$H+}
{$ENDIF}

uses FOS_TOOL_INTERFACES;

procedure GetFOS_FILE_LOGGER(out logger:IFOS_FILE_LOGGER;var holder:TObject);

implementation

uses

     FOS_REDBLACKTREE_GEN,
     classes,sysutils,FOS_LOCKING,FOS_INTERLOCKED,FRE_DATE_TOOLS,systemlog
     {$IFDEF WIN32}
     ,windows
     {$ENDIF}
     ;

const

  CSYSLOG_LEVEL       : Array[TFOS_LOG_LEVEL]      of longint = (-1, LOG_EMERG, LOG_ALERT, LOG_CRIT, LOG_ERR, LOG_WARNING, LOG_NOTICE, LOG_INFO, LOG_DEBUG);
  CSYSLOG_FACILITY    : Array[TFOS_LOG_FACILITY]   of longint = (-1, LOG_KERN, LOG_USER, LOG_MAIL, LOG_DAEMON, LOG_AUTH, LOG_SYSLOG, LOG_LPR, LOG_NEWS, LOG_UUCP,
                                                                 LOG_CRON, LOG_AUTHPRIV, LOG_FTP, LOG_LOCAL0, LOG_LOCAL1, LOG_LOCAL2, LOG_LOCAL3,
                                                                 LOG_LOCAL4, LOG_LOCAL5, LOG_LOCAL6, LOG_LOCAL7);

type

  TChangeObject=class(TObject)
  public
    procedure Change(const Sender:TObject);virtual;abstract;
  end;

  TThreadregistryChange = class(TChangeObject)
  private
    FTID:Cardinal;
    FName:String;
  public
    constructor Create(const name:String);
    procedure   Change(const Sender:TObject);override;
  end;

  RLogoptions=record
    filename,fullfilename:string;
    turnaround,generations:integer;
    level:TFOS_LOG_LEVEL;
    nolog,not_in_full_log:TFOS_BoolType;
    syslogfacility : TFOS_LOG_FACILITY;
  end;

  TLogOptions=class(TChangeObject)
    opts:RLogoptions;
  end;

  { TTargetLogOptions }

  TTargetLogOptions=class(TLogoptions)
    target:string;
    procedure   Change(const Sender:TObject);override;
  end;

  { TCategoryLogOptions }

  TCategoryLogOptions=class(TLogoptions)
    category:string;
    procedure   Change(const Sender:TObject);override;
  end;

  { TDefaultLogOptions }

  TDefaultLogOptions=class(TLogoptions)
    basedir:string;
    procedure   Change(const Sender:TObject);override;
  end;

  { TClearRules }

  TClearRules=class(TChangeObject)
    procedure Change(const Sender:TObject);override;
  end;

  { TAddRule }
  TLOG_RULE=record
    category  : string;
    level     : TFOS_LOG_LEVEL;
    target    : string;
    action    : TFOS_LOG_RULE_ACTION;
    stop_rule : boolean;
  end;

  TAddRule=class(TChangeObject)
    rule : TLOG_RULE;
    procedure Change(const Sender:TObject);override;
  end;

  TThreadRegister = specialize TGFOS_RBTree<cardinal,string>;
  TLogOpts        = specialize TGFOS_RBTree<string,RLogoptions>;

  { TFOSDebugLogger }
  RLogObj=record
    Entry,Category,Target  : String;
    ThreadId               : Cardinal;
    Level                  : TFOS_LOG_LEVEL;
    Time                   : TFRE_DB_DateTime64;
  end;
  RLogObjP=^RLogObj;

  { TFileLoggerThread }

  TFileLoggerThread=class(TThread,IFOS_FILE_LOGGER)
  private
   ThreadRegister          : TThreadRegister;
   TargetOpts,CategoryOpts : TLogOpts;
   InQ                     : IFOS_LFQ;
   ChQ                     : IFOS_LFQ;
   interval                : integer;
   BaseTime                : Qword;
   FE                      : TFOS_TE;
   DefaultLogOptions       : RLogoptions;
   BaseDir                 : String;
   FLogRules               : Array of TLOG_RULE;
   FlocalZone              : String;
   FOpenSysLogFacility     : TFOS_LOG_FACILITY;
   FEnabledSyslog          : boolean;
   glogcycledone           : integer;
   FIsMultiThreaded        : Boolean;
   single_threaded_LOL     : RLogObj;

   procedure   GenCheck             (gens: integer;fullpathfilename:string=''); // Removes Logfiles Exceedin Generations Limit
   function    LogToFile            (const filename, entry: string;const Turnaround:integer;const Gens:Integer):boolean;
   procedure   LogEmergency         (const msg:string);
  public
   procedure   Configure            (CFO:TObject);
   procedure   SetInterval          (const iv:integer);
   procedure   Sync_Logger          ;
   procedure   Terminate            ;
   procedure   Execute              ;override;
   procedure   Log                  (const msg,cat:String;Level:TFOS_LOG_LEVEL=fll_Debug;const target:string='';const sync:boolean=false);overload;
   procedure   Log                  (const msg:String;params:array of const;cat:String;Level:TFOS_LOG_LEVEL=fll_Debug;const target:string='';const sync:boolean=false);overload;
   procedure   LogConsole           (const msg:String);
   procedure   LogSystem            (const msg:String;const facility: TFOS_LOG_FACILITY; const level: TFOS_LOG_LEVEL);overload;
   procedure   RegisterCategory     (const cat:string;filename:string;turnaround:integer=-1;generations:integer=-1;const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const nolog:TFOS_BoolType=fbtNotSet;const not_in_full_log:TFOS_BoolType=fbtNotSet);
   procedure   RegisterTarget       (const target:string;targetdir:string;turnaround:integer=-1;generations:integer=-1;const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const facility:TFOS_LOG_FACILITY=flf_User);
   procedure   SetDefaults          (const defaultfilename:string;fullfilename,base_dir:string;const turnaround,generations:cardinal;const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const facility: TFOS_LOG_FACILITY=flf_Kernel);
   procedure   RegisterThread       (const name:string);
   constructor Create               ;reintroduce;
   destructor  Destroy              ;override;
   procedure   ClearLogRules        ;
   procedure   AddRule              (const category:string;const level:TFOS_LOG_LEVEL;const target:string;const action:TFOS_LOG_RULE_ACTION;const stop_rule:boolean=true);
   procedure   SetLocalZone         (const zone:string);
   procedure   EnableSyslog         ;
   procedure   DisableSyslog        ;
  end;


{ TAddRule }

procedure TAddRule.Change(const Sender: TObject);
var idx : integer;
begin
  SetLength(TFileLoggerThread(Sender).FLogRules,length(TFileLoggerThread(Sender).FLogRules)+1);
  idx := High(TFileLoggerThread(Sender).FLogRules);
  TFileLoggerThread(Sender).FLogRules[idx] := rule;
end;

{ TClearRules }

procedure TClearRules.Change(const Sender: TObject);
begin
  SetLength(TFileLoggerThread(Sender).FLogRules,0);
end;


{ TDefaultLogOptions }

procedure TDefaultLogOptions.Change(const Sender: TObject);
begin
 TFileLoggerThread(Sender).DefaultLogOptions:=opts;
 TFileLoggerThread(Sender).Basedir:=basedir;
end;

{ TCategoryLogOptions }

procedure TCategoryLogOptions.Change(const Sender: TObject);
begin
  TFileLoggerThread(Sender).CategoryOpts.AddCheck(category,opts,true);
end;

{ TTargetLogOptions }

procedure TTargetLogOptions.Change(const Sender: TObject);
begin
  TFileLoggerThread(Sender).TargetOpts.AddCheck(target,opts,true);
end;

{ TChangeObject }

{$HINTS OFF}
function _Get_ThreadID:cardinal;
begin
  {$IFDEF MSWINDOWS}
  result:=GetCurrentThreadId;
  {$ELSE}
  result:=cardinal(GetCurrentThreadId);
  {$ENDIF}
end;
{$HINTS ON}

//procedure TChangeObject.SetChangeInterface(const CIF: IUnknown);
//begin
//  FChangeIf:=Cif;
//end;

{ TThreadregistryChange }

constructor TThreadregistryChange.Create(const name: String);
begin
  FTID:=_Get_ThreadID;
  FName:=name;
end;

procedure TThreadregistryChange.Change(const Sender: TObject);
begin
  TFileLoggerThread(Sender).ThreadRegister.AddCheck(FTID,FName,true);
end;


  { TFileLoggerThread }



procedure TFileLoggerThread.Configure(CFO: TObject);
begin
 if assigned(CFO) then ;
end;

constructor TFileLoggerThread.Create;
begin
  BaseTime := GFRE_BT.Get_Ticks_us;
  GFRE_TF.Get_LFQ(INQ);
  GFRE_TF.Get_LFQ(ChQ);
  FE:=TFOS_TE.Create;
  interval:=1000;

  ThreadRegister := TThreadRegister.Create(@Default_RB_Cardinal_Compare);
  TargetOpts     := TLogOpts.Create(@Default_RB_String_Compare);
  CategoryOpts   := TLogOpts.Create(@Default_RB_String_Compare);

  DefaultLogOptions.nolog:=fbtFalse;
  DefaultLogOptions.not_in_full_log:=fbtFalse;
  DefaultLogOptions.turnaround:= 1000000;
  DefaultLogOptions.generations:=10;
  DefaultLogOptions.fullfilename:='log'+DirectorySeparator+'full.log';
  DefaultLogOptions.filename:='log'+DirectorySeparator+'main.log';
  DefaultLogOptions.syslogfacility := flf_Kernel;
  DefaultLogOptions.level := fll_Debug;

  FOpenSysLogFacility := flf_Invalid;
  FEnabledSyslog      := false;

  RegisterThread('MAIN');
  if FRE_ThreadingEnabled then begin
    inherited Create(False);
    FIsMultiThreaded := true;
  end else begin
    FIsMultiThreaded := false;
  end;
end;

destructor TFileLoggerThread.Destroy;
var LO:RLogObjP;
    co:TChangeObject;
begin
  Terminate;
  if FIsMultiThreaded then  WaitFor;
  FE.Free;
  while true do begin
   LO:=InQ.Pop;
   if not assigned(LO) then break;
   Dispose(LO);
  end;
  while true do begin
   CO:=TChangeObject(ChQ.Pop);
   if not assigned(CO) then break;
   CO.Free;
  end;
  ThreadRegister.Free;
  TargetOpts.Free;
  CategoryOpts.Free;
  InQ.Finalize;
  ChQ.Finalize;
  inherited;
end;


procedure TFileLoggerThread.ClearLogRules;
var clr : TClearRules;
begin
  clr:=TClearRules.Create;
  Chq.Push(clr);
end;

procedure TFileLoggerThread.AddRule(const category: string; const level: TFOS_LOG_LEVEL; const target: string; const action: TFOS_LOG_RULE_ACTION; const stop_rule: boolean);
var RULE:TAddRule;
begin
  rule := TAddRule.Create;
  RULE.rule.action    := action;
  RULE.rule.category  := uppercase(category);
  RULE.rule.level     := level;
  RULE.rule.target    := uppercase(target);
  RULE.rule.stop_rule := stop_rule;
  Chq.Push(rule);
end;

procedure TFileLoggerThread.SetLocalZone(const zone: string);
begin
  FlocalZone := zone;
end;

procedure TFileLoggerThread.EnableSyslog;
begin
  FEnabledSyslog := true;
end;

procedure TFileLoggerThread.DisableSyslog;
begin
  FEnabledSyslog := false;
end;

procedure TFileLoggerThread.Execute;
var LO:RLogObjP;
    Entry_Opts:RLogoptions;
    tid:String;
    targetpreselect : string;
    formated_entry  : string;

 procedure SetupPref;
 var co:RLogoptions;
   procedure OverloadOpts(const op:RLogoptions;const target:boolean);
   begin
     if not target then begin
       if op.filename<>''     then Entry_Opts.filename        := op.filename;
     end else begin
       targetpreselect:=op.filename;
     end;
     if op.fullfilename<>'' then Entry_Opts.fullfilename    := op.fullfilename;
     if op.generations<>-1  then Entry_Opts.generations     := op.generations;
     if op.level<>fll_Invalid then Entry_Opts.level           := op.level;
     if op.nolog<>fbtNotSet then Entry_Opts.nolog           := op.nolog;
     if op.not_in_full_log<>fbtNotSet
                            then Entry_Opts.not_in_full_log := op.not_in_full_log;
     if op.syslogfacility<>flf_Invalid
                            then Entry_Opts.syslogfacility  := op.syslogfacility;
   end;
   function _writeopts(const op:RLogoptions):string;
   begin
      result:=(format(' FN=%s FFN=%s LV=%d GEN=%d TURN=%d NOLOG=%d NOFULL=%d',[op.filename,op.fullfilename,op.level,op.generations,op.turnaround,op.nolog,op.not_in_full_log]));
   end;

 begin
   Entry_Opts:=DefaultLogOptions;
   if TargetOpts.Find(LO^.Target,co) then begin
     OverloadOpts(co,true);
   end;
   if CategoryOpts.Find(Lo^.Category,co) then begin
     OverloadOpts(co,false);
   end;
 end;

 procedure ChangeParams;
 var co:TChangeObject;
 begin
   repeat
     CO:=TChangeObject(Chq.Pop);
     if assigned(co) then begin
       co.change(self);
       co.free;
     end else begin
       exit;
     end;
   until false;
 end;

 procedure FormatEntry;
 begin
   ThreadRegister.Find(LO^.ThreadId,TID);
   if TID='' then TID:=inttostr(LO^.ThreadId);
   if FlocalZone<>'' then begin
     LO^.Entry      := Format('%s|%8.8s|%6.6s|%8.8s|> %s',[GFRE_DT.ToStrFOS(GFRE_DT.UTCToLocalTime(Lo^.Time,FlocalZone)),CFOS_LOG_LEVEL[lo^.Level],tid,lo^.Category,lo^.Entry]);
   end else begin
    LO^.Entry      := Format('%s|%8.8s|%6.6s|%8.8s|> %s',[GFRE_DT.ToStrFOS(Lo^.Time)+' UTC',CFOS_LOG_LEVEL[lo^.Level],tid,lo^.Category,lo^.Entry]);
   end;
   LO^.Entry      := StringReplace(LO^.Entry,LineEnding,LineEnding+'                                             ',[rfReplaceAll]);
   formated_entry := LO^.Entry;
 end;

 function  ProcessRules:boolean;
 var match     : boolean;
     i         : integer;
     rule_t    : string;
     rule_c    : string;
     rule_l    : TFOS_LOG_LEVEL;
     rule_stop : boolean;
 begin
   result := false;
   for i:=0 to high(FLogRules) do begin
     match     := true;
     rule_t    := FLogRules[i].target;
     rule_c    := FLogRules[i].category;
     rule_l    := FLogRules[i].level;
     rule_stop := FLogRules[i].stop_rule;
     if rule_t <>'*' then begin
       if rule_t <> LO^.Target then match := false;
     end;
     if rule_c<>'*' then begin
       if rule_c<>LO^.Category then match := false;
     end;
     if rule_l<>fll_Invalid then begin
       if rule_l <> LO^.Level then match := false;
     end;
     if match then begin
       case FLogRules[i].action of
         flra_LogToOnConsole: begin
           if formated_entry='' then FormatEntry;
           writeln(formated_entry);
           if rule_stop then exit(false);
         end;
         flra_DropEntry: begin
           exit(true);
         end;
       end;
     end;
   end;
 end;

 procedure LogEvents(const single_threaded:boolean=false);
 var break_it:boolean;
 begin
   break_it := false;
   while true do begin
   if break_it then break;
    ChangeParams;
    if single_threaded then begin
      LO := @single_threaded_LOL;
    end else begin
      LO:=InQ.Pop;
      if not assigned(lo) then break;
    end;
    LO^.Category   := uppercase(LO^.Category);
    LO^.Target     := uppercase(LO^.Target);
    formated_entry := '';
    try
       SetupPref;
       if ProcessRules=true then begin //True = drop entry
         continue;
       end else begin
         if Entry_Opts.NoLog=fbtTrue then begin
           continue;
         end;
         if (LO^.Level>Entry_Opts.level) and (Entry_Opts.level>fll_Invalid) then begin
           continue;
         end;
         if formated_entry='' then FormatEntry;
         if FEnabledSyslog then begin
          with Entry_Opts do LogSystem(formated_entry,syslogfacility,LO^.Level);
         end else begin
          if (Entry_Opts.fullfilename<>'') and (Entry_Opts.not_in_full_log=fbtFalse) then begin
            with Entry_Opts do begin
             if LogToFile(basedir+targetpreselect+fullfilename,formated_entry,turnaround,generations)=true then exit;
            end;
          end;
          with Entry_Opts do begin
            if LogToFile(basedir+targetpreselect+filename,formated_entry,turnaround,generations)=true then exit;
          end;
         end;
       end;
    finally
      if not single_threaded then Dispose(lo);
      break_it := single_threaded;
    end;
   end;
 end;

begin
  try
    if FIsMultiThreaded then begin
      while not Terminated do begin
        LogEvents;
        FE.WaitFor(interval);
        if glogcycledone=1 then begin
          LogEvents;
          FOS_IL_Exchange(glogcycledone,2);
        end;
      end;
      LogEvents;
    end else begin
      LogEvents(true);
    end;
    FOS_IL_Exchange(glogcycledone,2);
 except on e:exception do begin
   FOS_IL_Exchange(glogcycledone,2);
   LogEmergency('CRITICAL LOGGER ERROR '+e.message);
 end;end;
end;

procedure TFileLoggerThread.GenCheck(gens: integer; fullpathfilename: string);
var st,i:integer;
    DSR: TSearchRec;
    min,act,cnt,mindex,maxcnt:LongInt;
    dir, SNAME,deldir:ansistring;

    srecs : Array of TSearchRec;


begin
  dir:=fullpathfilename;
  SNAME:=ExtractFileName(dir);
  deldir:=ExtractFilePath(fullpathfilename);

  setlength(srecs,100);
  maxcnt   := 0;
  {$IFDEF UNIX}
  st :=FindFirst(dir+'*', faAnyFile, DSR);
  {$ELSE}
  st :=FindFirst(dir+'*.*', faAnyFile, DSR);
  {$ENDIF}
  try
    while St = 0 do begin
     if (DSR.Name <> '.') and (DSR.Name <> '..') then begin
      if pos(SNAME,DSR.name)=1 then begin
        if maxcnt>length(srecs) then
          setlength(srecs,length(srecs)+100);
        srecs[maxcnt]:=dsr;
        inc(maxcnt);
      end;
     end;
     St := FindNext(DSR);
    end;
  finally
    sysutils.FindClose(DSR);
  end;
  setlength(srecs,maxcnt);
  cnt:=length(srecs);
  while cnt>gens do begin
    min:=MAXINT;
    mindex:=0;
    for i:=0 to high(srecs) do
      begin
        act:=srecs[i].Time;
        if min>act then
          begin
            min:=act;
            mindex:=i;
         end;
       end;
    Sysutils.DeleteFile(deldir+srecs[mindex].Name);
    srecs[mindex].Time:=MaxInt;
    dec(cnt);
  end;
end;


procedure TFileLoggerThread.Log(const msg, cat: String; Level: TFOS_LOG_LEVEL;  const target: string;const sync:boolean=false);
var LO  : RLogObjP;
begin
  if FIsMultiThreaded then begin
    LO:=New(RLogObjP);
    LO^.Entry    := msg;
    Lo^.Category := cat;
    LO^.Level    := level;
    LO^.Target   := target;
    LO^.ThreadID := _Get_ThreadID;
    LO^.Time     := GFRE_DT.Now_UTC;
    InQ.Push(LO);
    if sync then Sync_Logger;
  end else begin
    single_threaded_LOL.Entry    := msg;
    single_threaded_LOL.Category := cat;
    single_threaded_LOL.Level    := level;
    single_threaded_LOL.Target   := target;
    single_threaded_LOL.ThreadID := 0; // Single Threaded
    single_threaded_LOL.Time     := GFRE_DT.Now_UTC;
    Execute;
  end;
end;

procedure TFileLoggerThread.Log(const msg: String; params: array of const; cat: String; Level: TFOS_LOG_LEVEL; const target: string;const sync:boolean=false);
begin
  Log(Format(msg,params),cat,Level,target);
  if sync then Sync_Logger;
end;

procedure TFileLoggerThread.LogConsole(const msg: String);
begin
 {$I-}
 writeln(msg);
 {$I+}
end;

procedure TFileLoggerThread.LogSystem(const msg: String; const facility: TFOS_LOG_FACILITY; const level: TFOS_LOG_LEVEL);
begin
 if facility<>FOpenSysLogFacility then begin
  if FOpenSysLogFacility<>flf_Invalid then begin
    FOpenSysLogFacility:=flf_Invalid;
    closelog;
  end;
//  writeln('FACILITY:',CSYSLOG_FACILITY[facility]);
  OpenLog(Pchar('FRE'),LOG_NOWAIT,CSYSLOG_FACILITY[facility]);
  FOpenSysLogFacility   := facility;
 end;
// writeln('LEVEL:',CSYSLOG_LEVEL[level]);
 syslog(CSYSLOG_LEVEL[level],'%s',[PChar(Msg)]);
end;

function TFileLoggerThread.LogToFile(const filename, entry: string; const Turnaround: integer; const Gens: Integer): boolean;
var nam:string;
    f:Text;
    ff:file of byte;
    lbasedir,nn:string;
    noclose:boolean;
    k:cardinal;
begin
   result := false;
   try
    lbasedir:=ExtractFilePath(filename);
    nam:=ExtractFileName(filename);
    try
     if lbasedir<>''then ForceDirectories(lBaseDir);
    except on e:exception do begin
    end;end;
    try
     if fileexists(filename) then begin
       noclose:=false;
       AssignFile(ff,filename);
       try
         {$I-}
         k:=0;
         repeat
          inc(k);
          if k=10 then begin
           nam:=nam;
           LogEmergency('Can not log to file '+filename);
           Terminate;
           exit(true);
          end;
          Reset(ff);
          if IOResult=0 then break;
          sleep(100);
         until false;
         {$I+}
         if ((FileSize(ff)>Turnaround) and (Turnaround>0)) then begin
          Close(ff); // Must Close before Rename or Delete !!!!
          noclose:=true;
          if (Gens>1) then begin
           nn:=filename+'_'+FormatDateTime('yyyymmdd_hhnnss',now);
           RenameFile(filename,nn);
          end else begin
           SysUtils.DeleteFile(filename);
          end;
          GenCheck(Gens,filename);
         end;
       finally
        if not noclose then Close(ff);
       end;
     end;
    except on E:Exception do begin
     nam:=nam;
    end;end;

    AssignFile(f,lBaseDir+nam);
    try
      {$I-}
      k:=0;
      repeat
       inc(k);
       if k=10 then begin
        nam:=nam;
        LogEmergency('Can not log to file '+lBaseDir+nam);
        Terminate;
        exit(true);
       end;
       if FileExists(lBaseDir+nam) then begin
        Append(f);
        if IOResult<>0 then begin sleep(100);continue;end;
       end else begin
        Rewrite(f);
        if IOResult<>0 then begin sleep(100);continue;end;
       end;
       writeln(f,entry);
       if IOResult<>0 then begin sleep(100);continue;end;
       break;
      until false;
      {$I+}
     finally
     Close(f);
    end;
   except on E:Exception do begin
    nam:=nam;
   end;end;
end;


procedure TFileLoggerThread.RegisterCategory(const cat: string; filename: string; turnaround: integer; generations: integer; const minseveritylevel: TFOS_LOG_LEVEL; const nolog: TFOS_BoolType; const not_in_full_log: TFOS_BoolType);
var CATG:TCategoryLogOptions;
begin
  CATG:=TCategoryLogOptions.Create;
  CATG.category:=uppercase(cat);
  CATG.opts.filename:=Extractfilename(filename);
  CATG.opts.fullfilename:='';
  CATG.opts.turnaround:=turnaround;
  CATG.opts.generations:=generations;
  CATG.opts.level:=minseveritylevel;
  CATG.opts.nolog:=nolog;
  CATG.opts.not_in_full_log:=not_in_full_log;
  Chq.Push(CATG);
end;

procedure TFileLoggerThread.RegisterTarget(const target: string; targetdir: string; turnaround: integer; generations: integer; const minseveritylevel: TFOS_LOG_LEVEL; const facility: TFOS_LOG_FACILITY);
var TARG:TTargetLogOptions;
begin
 TARG:=TTargetLogOptions.Create;
 TARG.target:=uppercase(target);
 TARG.opts.filename:=ExtractFilePath(targetdir+DirectorySeparator);
 TARG.opts.fullfilename:='';
 TARG.opts.turnaround:=turnaround;
 TARG.opts.generations:=generations;
 TARG.opts.level:=minseveritylevel;
 TARG.opts.nolog:=fbtNotSet;
 TARG.opts.not_in_full_log:=fbtNotSet;
 TARG.opts.syslogfacility := facility;
 Chq.Push(TARG);
end;

procedure TFileLoggerThread.SetDefaults(const defaultfilename: string; fullfilename, base_dir: string; const turnaround, generations: cardinal; const minseveritylevel:TFOS_LOG_LEVEL=fll_Debug;const facility: TFOS_LOG_FACILITY=flf_Kernel);
var DEFO:TDefaultLogOptions;
begin
  DEFO:=TDefaultLogOptions.Create;
  DEFO.opts.filename:=ExtractFileName(defaultfilename);
  DEFO.opts.fullfilename:=ExtractFileName(fullfilename);
  DEFO.opts.generations:=generations;
  DEFO.opts.turnaround:=turnaround;
  DEFO.opts.level:=minseveritylevel;
  DEFO.opts.syslogfacility:= facility;
  DEFO.Basedir:=ExtractFilePath(base_dir+DirectorySeparator);
  Chq.Push(DEFO);
  FE.SetEvent;
end;

procedure TFileLoggerThread.RegisterThread(const name: string);
begin
  ChQ.Push(TThreadRegistryChange.Create(name));
  FE.SetEvent;
end;

procedure TFileLoggerThread.SetInterval(const iv: integer);
begin
 InterLockedExchange(interval,iv);
end;

procedure TFileLoggerThread.Terminate;
begin
  inherited Terminate;
  FE.SetEvent;
end;

procedure TFileLoggerThread.Sync_Logger;
begin
 if not FIsMultiThreaded then exit;
 if glogcycledone=2 then exit;
 FOS_IL_Exchange(glogcycledone,1);
 FE.SetEvent;
 repeat
  sleep(1);
  if glogcycledone=2 then break;
 until false;
 FOS_IL_Exchange(glogcycledone,0);
end;


procedure TFileLoggerThread.LogEmergency(const msg: string);
begin
  try
    writeln('<************* EMERGENCY LOG *************');
    writeln(msg);
    writeln('************* EMERGENCY LOG *************>');
  except
  end;
  LogToFile('emergency.log',msg,0,0);
end;

var FLogger:TFileLoggerThread;

procedure GetFOS_FILE_LOGGER(out logger: IFOS_FILE_LOGGER; var holder: TObject);
begin
  if not assigned(FLogger) then begin
    FLogger:=TFileLoggerThread.Create;
  end;
  logger := FLogger;
  holder := FLogger;
end;


initialization
 {$IFNDEF LINUX}
  {$IFNDEF FPC}
  GetLocaleFormatSettings(0,DATEFORMAT);
  {$ENDIF}
 {$ENDIF}

finalization

end.
