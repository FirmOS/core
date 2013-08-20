program CON_partiontest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Sysutils,FRE_CORE, FOS_PARTITIONTREE,FOS_REDBLACKTREE_GEN,
  FRE_APS_INTERFACE,FRE_APS_IMPL_BSD,FOS_TOOL_INTERFACES;

 type

   { TFlowDataTaskObject }

   { TFlowRecord }

   TFlowRecord=class
     Key       : String;
     DeviID    : String;
     procedure RRD;
     procedure Bundle;
   end;

   { TFlowDataAddCmd }

   { TFlowDataCmd }

   TFlowDataCmd=class
    FlowID:string;
    PartID,MacA,MAcB:string;
    procedure  Found(const obj:TFlowRecord;const new:boolean);
    procedure  ForAll(const key:int64;const obj:TFlowRecord;const P:Pointer);
    procedure  DumpText (const key:int64; const obj:TFlowRecord;const P:Pointer);
   end;

   TXkey = packed record
    a:integer;
    b:integer;
   end;

   { TFlowXDataCmd }

   TFlowXDataCmd=class
    FlowID:string;
    PartID,MacA,MAcB:string;
    procedure  Found(const obj:TFlowRecord;const new:boolean);
    procedure  ForAll(const key:Txkey;const obj:TFlowRecord;const P:Pointer);
   end;

   TKtree=specialize  TFOS_PARTITION_TREE<int64,TFlowRecord,TFlowDataCmd>;
   TXtree=specialize  TFOS_PARTITION_TREE<TXkey,TFlowRecord,TFlowXDataCmd>;

 //  PFlowDataPassRec=^TFlowDataPassRec;

  { TTEST_ENGINE }

  TTEST_ENGINE=class(TInterfacedObject,IFRE_APS_PROCESS)
  private
    ktree:TKtree;
  public
   procedure Setup;
   procedure Terminate; // KILL
   procedure ReInit;    // HUP
   procedure Interrupt; // CTRL-C
   function  GetName:String;
   procedure Weiter(const data:Pointer;const cp:integer=0);
   procedure Dumpi (const data:Pointer;const cp:integer=0);
  end;

 { TFlowXDataCmd }

 procedure TFlowXDataCmd.Found(const obj: TFlowRecord; const new: boolean);
 begin

 end;

 procedure TFlowXDataCmd.ForAll(const key: Txkey; const obj: TFlowRecord;
 const P: Pointer);
 begin

 end;

 { TFlowDataAddCmd }
var globi:integer=1;

 procedure TFlowDataCmd.Found(const obj: TFlowRecord; const new: boolean);
 begin
   writeln('(',GFRE_S.CURRENT_THREAD,') FLOW COMMAND FOUND ',integer(obj),' ',new);
   if new then begin
     obj.DeviID:='NEW DEVI ('+inttostr(globi)+')';
     InterLockedIncrement(globi);
   end else begin
    writeln('------------->>>>>>>>>>> FOUND DEVI ',obj.DeviID);
   end;
 end;

 procedure TFlowDataCmd.ForAll(const key: int64; const obj: TFlowRecord;const P: Pointer);
 begin
   writeln('Time:',gfre_bt.Get_Ticks_us,'Thread:',GFRE_S.CURRENT_THREAD,' FOR ALL DEVI ',key,' ',obj.DeviID,' FlowID ', TFlowDataCmd(p).FlowID);
 end;

  procedure TFlowDataCmd.DumpText(const key: int64; const obj: TFlowRecord;
   const P: Pointer);
 begin

 end;


 { TFlowRecord }

  procedure TFlowRecord.RRD;
 begin

 end;

  procedure TFlowRecord.Bundle;
 begin

 end;

 { TFlowDataTaskObject }



 { TTEST_ENGINE }

 procedure TTEST_ENGINE.Setup;
 begin
   ktree:=TKtree.Create(@Default_RB_Int64_Compare);
   GFRE_S.AddPeriodicTimer(100,@Dumpi,nil);
   GFRE_S.AddOneShotTimer(0,@Weiter,nil);
 end;

 procedure TTEST_ENGINE.Terminate;
 var
   fa:TFlowDataCmd;
 begin
   writeln;
   writeln('TERMINATE DUMP');
   writeln;
   fa:=TFlowDataCmd.Create;
   ktree.Dumptrees(fa);
   fa.free;
   writeln;
 end;

 procedure TTEST_ENGINE.ReInit;
 begin
  GFRE_S.Quit;
 end;

 procedure TTEST_ENGINE.Interrupt;
 begin
  GFRE_S.Quit;
 end;

 function TTEST_ENGINE.GetName: String;
 begin

 end;

 procedure TTEST_ENGINE.Weiter(const data: Pointer; const cp: integer);
 var i:integer;
    x:Pinteger;
    fa:TFlowDataCmd;
    label _Find,_Add;
 begin
   writeln('ONCE WEITER ',cp);
   for i:=1 to 10 do begin
     fa:=TFlowDataCmd.Create;
     fa.FlowID:=inttostr(i+200);
     ktree.FindAdd(i,fa);
     if (i mod 10000)=0 then begin
       writeln(i);
     end;
   end;
   WriteLn('DONE ADD');
   fa:=TFlowDataCmd.Create;
   fa.FlowID:='FORALL EARLY';
   ktree.ForAll(fa);
   sleep(100);
   writeln('Again');
   for i:=1 to 100 do begin
     fa:=TFlowDataCmd.Create;
     fa.FlowID:=inttostr((i mod 10)+200);
     ktree.FindAdd(i mod 10,fa);
     if (i mod 100)=0 then begin
       writeln(i);
     end;
   end;
   writeln('DONE2');
   sleep(1000);
//   ktree.Dumptrees;
   fa:=TFlowDataCmd.Create;
   fa.FlowID:='FORALL A';
   ktree.ForAll(fa);
   fa:=TFlowDataCmd.Create;
   fa.FlowID:='FORALL B';
   ktree.ForAll(fa);
   fa:=TFlowDataCmd.Create;
   fa.FlowID:='FORALL C';
   ktree.ForAll(fa);
   fa:=TFlowDataCmd.Create;
   fa.FlowID:='FORALL D';
   ktree.ForAll(fa);
 end;





 procedure TTEST_ENGINE.Dumpi(const data: Pointer;const cp:integer);
 begin
   exit;
   ktree.DumpCounters;
 end;

 var
  e: IFRE_APS_PROCESS;
  i: Integer;
  j: Integer;


begin

  E:=TTEST_ENGINE.Create;
  GFRE_S.Start(E);
  GFRE_S.Run;
  E:=nil;

end.

