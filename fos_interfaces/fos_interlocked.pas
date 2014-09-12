unit fos_interlocked;

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

{$IFDEF CPU32}
 {$MODE DELPHI}
{$ENDIF}
{$IFDEF CPU64}
 {$MODE objfpc}
{$ENDIF}

interface

uses  sysutils
      {$IFDEF CPU32}
      ;
      {$ELSE}
      ,CPU;
      {$ENDIF}

type
  RFOS_TAGPOINTER = packed record
    case Integer of
      0: (Ptr:Pointer ; Tag: PtrUint);
      {$IFDEF CPU32}
      1: (val: Int64);
      2: (Test:array[0..7] of Byte);
      {$ENDIF}
      {$IFDEF CPU64}
      1: (val : int128rec);
      2: (Test:array[0..15] of Byte);
      {$ENDIF}
  end;

PFOS_TAGPOINTER=^RFOS_TAGPOINTER;


function  FOS_IL_Increment(var Addend: longword): longword;overload;inline;
function  FOS_IL_Increment(var Addend: longint):  longint; overload;inline;
function  FOS_IL_Decrement(var Addend: longword): longword;overload;inline;
function  FOS_IL_Decrement(var Addend: longint):  longint; overload;inline;

function  FOS_IL_Exchange(var Target: longword; Value: longword): longword;overload;inline;
function  FOS_IL_Exchange(var Target: integer; Value: integer): integer;overload;inline;
function  FOS_IL_ExchangeAdd(var Addend: longword; Value: integer): longword;inline;

function  FOS_IL_CompareExchange(var Destination: longword; Exchange, Comparand: longword): longword;inline;
function  FOS_IL_CompareExchange64(var Destination: Int64; Exchange, Comparand: Int64): Int64;//inline;
function  FOS_IL_Increment64(var Addend: Int64): Int64; //inline;
function  FOS_IL_Decrement64(var Addend: Int64): Int64; //inline;
function  FOS_IL_Exchange64(var Target: Int64; Value: Int64): Int64; //inline;
function  FOS_IL_ExchangeAdd64(var Addend: Int64; Value: Int64): Int64;// inline;
{$IFDEF CPU64}
//function  FOS_IL_CompareExchange128(var Destination:Int128Rec;Exchange,Comparand:Int128rec):Int128Rec;inline;
{$ENDIF}

function  FOS_IL_CAS32          (var Destination: longword; Comparand, Exchange: longword): boolean; inline;
function  FOS_IL_CAS64          (var Destination: Int64;  Comparand,Exchange: Int64): boolean; inline;
function  FOS_IL_CAS_NATIVE     (var Destination: NativeUInt; Comparand, Exchange :NativeUInt):boolean; inline;
function  FOS_IL_INC_NATIVE     (var Destination: NativeUInt):NativeUint; inline;
function  FOS_IL_DEC_NATIVE     (var Destination: NativeUInt):NativeUint; inline;
function  FOS_IL_EXC_NATIVE     (var Destination: NativeUint; Value: longword):NativeUint; inline;
function  FOS_IL_CEX_NATIVE     (var Destination: NativeUint; Exchange, Comparand: NativeUint): NativeUint;
function  FOS_IL_CEX_NATIVE_CHK (var Destination: NativeUint; var HasVal:NativeUint; Exchange, Comparand: NativeUint):Boolean;inline;

{$IFDEF CPU64}
function  FOS_IL_CAS128(var Destination: Int128Rec;  Comparand,Exchange: Int128Rec): boolean; //inline;
{$ENDIF}
function  FOS_GETTAGPTR(const Ptr:Pointer;Tag:PtrUint):RFOS_TAGPOINTER;//inline

{$IFDEF CPU64}
operator = (const a,b:Int128rec) res: boolean;
{$ENDIF}

type
   TFOS_NATIVE_LOCK     = NativeUint;
   TFOS_NATIVE_LOCK_KEY = NativeUint;
   PFOS_NATIVE_LOCK     = ^TFOS_NATIVE_LOCK;
   PFOS_NATIVE_LOCK_KEY = ^TFOS_NATIVE_LOCK_KEY;

 function   FOS_N_LockTryEnter  (var N_LOCK:TFOS_NATIVE_LOCK;out   N_LOCK_KEY:NativeUint):boolean;inline; //True on success
 procedure  FOS_N_LockLeave     (var N_LOCK:TFOS_NATIVE_LOCK;const N_LOCK_KEY:NativeUint);inline;
 procedure  FOS_N_LockEnterSpin (var N_LOCK:TFOS_NATIVE_LOCK;out   N_LOCK_KEY:NativeUint);

implementation
{.$IFDEF CPU64}
uses FOS_LOCKING;
{.$ENDIF}

var LOCK:TFOS_LOCK;

function FOS_IL_CAS_NATIVE(var Destination: NativeUInt; Comparand, Exchange: NativeUInt):boolean;
begin
 {$IFDEF CPU32}
   result := FOS_IL_CAS32(Destination,Comparand,Exchange);
 {$ELSE}
   {$IFDEF CPU64}
     result := FOS_IL_CAS64(Int64(Destination),Int64(Comparand),Exchange);
   {$ELSE}
      Implement that
   {$ENDIF}
 {$ENDIF}
end;

function FOS_IL_INC_NATIVE(var Destination: NativeUInt):NativeUint;
begin
 {$IFDEF CPU32}
   result := FOS_IL_Increment(Destination);
 {$ELSE}
   {$IFDEF CPU64}
     result := FOS_IL_Increment64(Int64(Destination));
   {$ELSE}
      Implement that
   {$ENDIF}
 {$ENDIF}
end;

function FOS_IL_DEC_NATIVE(var Destination: NativeUInt): NativeUint;
begin
 {$IFDEF CPU32}
   result := FOS_IL_Decrement(Destination);
 {$ELSE}
   {$IFDEF CPU64}
     result := FOS_IL_Decrement64(Int64(Destination));
   {$ELSE}
      Implement that
   {$ENDIF}
 {$ENDIF}
end;

function FOS_IL_EXC_NATIVE(var Destination: NativeUint; Value: longword): NativeUint;
begin
  {$IFDEF CPU32}
    result := FOS_IL_Exchange(Destination,Value);
  {$ELSE}
    {$IFDEF CPU64}
      result := FOS_IL_Exchange64(Int64(Destination),Value);
    {$ELSE}
       Implement that
    {$ENDIF}
  {$ENDIF}
end;

function FOS_IL_CEX_NATIVE_CHK(var Destination: NativeUint; var HasVal: NativeUint; Exchange, Comparand: NativeUint): Boolean;
begin
  HasVal := FOS_IL_CEX_NATIVE(Destination,Exchange,Comparand);
  result := HasVal=Comparand;
end;

function FOS_IL_CEX_NATIVE(var Destination: NativeUint; Exchange, Comparand: NativeUint): NativeUint;
begin
 {$IFDEF CPU32}
   result := FOS_IL_CompareExchange(Destination,Exchange,Comparand);
 {$ELSE}
   {$IFDEF CPU64}
     result := FOS_IL_CompareExchange64(Int64(Destination),Exchange,Comparand);
   {$ELSE}
      Implement that
   {$ENDIF}
 {$ENDIF}
end;

function  FOS_GETTAGPTR(const Ptr:Pointer;Tag:PtrUint):RFOS_TAGPOINTER;
begin
 result.Ptr:=Ptr;
 result.Tag:=Tag;
end;

var T_L_Key : NativeUint=100000;

function FOS_N_LockTryEnter(var N_LOCK: TFOS_NATIVE_LOCK; out N_LOCK_KEY: NativeUint): boolean;
var LKEY:NativeUint;
begin
  LKey   := FOS_IL_INC_NATIVE(T_L_Key);
  if LKEY=0 then LKey   := FOS_IL_INC_NATIVE(T_L_Key);
  result := FOS_IL_CAS_NATIVE(N_LOCK,0,LKEY);
  if Result then N_LOCK_KEY:=LKey else N_LOCK_KEY:=0;
end;

procedure FOS_N_LockLeave(var N_LOCK: TFOS_NATIVE_LOCK; const N_LOCK_KEY: NativeUint);
begin
  if not FOS_IL_CAS_NATIVE(N_LOCK,N_LOCK_KEY,0) then begin
    raise Exception.Create(format('N_Lock failure unexpected Lock Value on leave got %d but wanted %d',[N_LOCK_KEY,N_LOCK]));
  end;
end;

procedure FOS_N_LockEnterSpin(var N_LOCK: TFOS_NATIVE_LOCK; out N_LOCK_KEY: NativeUint);
var DeadLockDet : int64 = 0;
begin
  while not FOS_N_LockTryEnter(N_LOCK,N_LOCK_KEY) do begin
    inc(DeadLockDet);
    if DeadLockDet>200000000 then raise Exception.Create(format('deadlock wait on FOS_N_LockEnterSpin %x',[N_LOCK]));
  end;
  if N_LOCK_KEY=0 then raise Exception.Create('runlock key=0 !!');
end;


{$IFDEF CPU64}
operator=(const a, b: Int128rec) res:boolean;
begin
  result := (a.Lo=b.Lo) and (a.Hi=b.Hi);
end;
{$ENDIF}

function  FOS_IL_Decrement(var Addend: longword): longword;
//asm
//{     ->          EAX     Addend }
//{     <-          EAX     Result }
//          MOV     EDX, EAX
//          MOV     EAX, -1
//LOCK      XADD    [EDX], EAX
//          DEC     EAX
begin
 result:=InterLockedDecrement(Addend);
end;

function  FOS_IL_Decrement(var Addend: Integer): Integer;
begin
  result:=FOS_IL_Decrement(cardinal(Addend));
end;

function  FOS_IL_Increment(var Addend: longword): longword;
//asm
//{     ->          EAX     Addend }
//{     <-          EAX     Result }
//          MOV     EDX, EAX
//          MOV     EAX, 1
//LOCK      XADD    [EDX], EAX
//          INC     EAX
begin
  result:=InterLockedIncrement(Addend);
end;

function  FOS_IL_Increment(var Addend: longint):  longint; overload;
begin
  result:=FOS_IL_Increment(longword(addend));
end;

function  FOS_IL_CompareExchange(var Destination: longword; Exchange, Comparand: longword): longword;
//asm
//{     ->          EAX     Destination }
//{                 EDX     Exchange    }
//{                 ECX     Comparand   }
//{     <-          EAX     Result      }
//          XCHG    EAX, ECX
//LOCK      CMPXCHG [ECX], EDX
begin
  result := InterlockedCompareExchange(destination,exchange,comparand);
end;

{$IFDEF CPU64}
function  FOS_IL_CompareExchange128(var Destination:Int128Rec;Exchange,Comparand:Int128rec):Int128Rec;
begin
  result := InterlockedCompareExchange128(Destination,Exchange,Comparand);
end;

function FOS_IL_CompareExchange64(var Destination: Int64; Exchange, Comparand: Int64): Int64;
begin
  result := InterlockedCompareExchange64(DEstination,Exchange,Comparand);
end;

function FOS_IL_Increment64(var Addend: Int64): Int64;
begin
  result := InterLockedIncrement64(Addend);
end;

function FOS_IL_Decrement64(var Addend: Int64): Int64;
begin
 result := InterLockedDecrement64(Addend);
end;

function FOS_IL_Exchange64(var Target: Int64; Value: Int64): Int64;
begin
 result := InterLockedExchange64(target,value);
end;

function FOS_IL_ExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
begin
  result := InterLockedExchangeAdd64(Addend,value);
end;
{$ENDIF}

{$IFDEF CPU32}
  {$IFDEF CPUARM}
  function  FOS_IL_Exchange64(var Target: Int64; Value: Int64): Int64;
  begin
    abort;
  end;

  {$ELSE}
  function  FOS_IL_Exchange64(var Target: Int64; Value: Int64): Int64; assembler;
  asm
  {     ->          EAX     Target }
  {                 ESP+4   Value  }
  {     <-          EDX:EAX Result }
            PUSH    EDI
            PUSH    EBX

            MOV     EDI, EAX

            MOV     EBX, DWORD PTR [Value]
            MOV     ECX, DWORD PTR [Value+4]

            MOV     EAX, [EDI]
            MOV     EDX, [EDI+4]
  @@1:
  LOCK      CMPXCHG8B [EDI]
            JNZ     @@1
            POP     EBX
            POP     EDI
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF CPU32}
  {$IFDEF CPUARM}
  function  FOS_IL_CompareExchange64(var Destination: Int64; Exchange, Comparand: Int64): Int64;
  begin
    if not assigned(lock) then begin
      lock:=TFOS_LOCK.Create;
    end;
    LOCK.Acquire;
     result := Destination;
     if Destination=Comparand then
       begin
         Destination := Exchange;
       end;
    LOCK.Release;
  end;
  {$ELSE}
  function  FOS_IL_CompareExchange64(var Destination: Int64; Exchange, Comparand: Int64): Int64; assembler;
    asm
    {     ->          EAX     Destination }
    {                 ESP+4   Exchange    }
    {                 ESP+12  Comparand   }
    {     <-          EDX:EAX Result      }
              PUSH    EBX
              PUSH    EDI

              MOV     EDI, EAX

              MOV     EBX, DWORD PTR [Exchange]
              MOV     ECX, DWORD PTR [Exchange+4]

              MOV     EAX, DWORD PTR [Comparand]
              MOV     EDX, DWORD PTR [Comparand+4]

    LOCK      CMPXCHG8B [EDI]
              POP     EDI
              POP     EBX
    end;
  {$ENDIF}
{$ENDIF}


{$IFDEF CPU32}
  {$IFDEF CPUARM}
  function  FOS_IL_Increment64(var Addend: Int64): Int64;
  begin
    if not assigned(lock) then begin
      lock:=TFOS_LOCK.Create;
    end;
    LOCK.Acquire;
    inc(Addend);
    result := Addend;
    LOCK.Release;
  end;
  {$ELSE}
  function  FOS_IL_Increment64(var Addend: Int64): Int64;
  asm
  {     ->          EAX     Addend }
  {     <-          EDX:EAX Result }
            PUSH    EDI
            PUSH    EBX

            MOV     EDI, EAX

            MOV     EAX, [EDI]    // Fetch original Int64 at memory location
            MOV     EDX, [EDI+4]
  @@1:
            MOV     ECX, EDX
            MOV     EBX, EAX

            ADD     EBX, 1
            ADC     ECX, 0

  LOCK      CMPXCHG8B [EDI]
            JNZ     @@1
            // Returns updated value of Addend

            MOV     EAX, EBX
            MOV     EDX, ECX

            POP     EBX
            POP     EDI
  end;
  {$ENDIF}
{$ENDIF}

{$IFDEF CPU32}
  {$IFDEF CPUARM}
  function  FOS_IL_Decrement64(var Addend: Int64): Int64;
  begin
    if not assigned(lock) then begin
      lock:=TFOS_LOCK.Create;
    end;
    LOCK.Acquire;
    dec(Addend);
    result := Addend;
    LOCK.Release;
  end;
  {$ELSE}
  function  FOS_IL_Decrement64(var Addend: Int64): Int64;
  asm
  {     ->          EAX     Addend }
  {     <-          EDX:EAX Result }
            PUSH    EDI
            PUSH    EBX

            MOV     EDI, EAX

            MOV     EAX, [EDI]    // Fetch original Int64 at memory location
            MOV     EDX, [EDI+4]
  @@1:
            MOV     ECX, EDX
            MOV     EBX, EAX

            SUB     EBX, 1
            SBB     ECX, 0

  LOCK      CMPXCHG8B [EDI]
            JNZ     @@1
            // Returns updated value of Addend

            MOV     EAX, EBX
            MOV     EDX, ECX

            POP     EBX
            POP     EDI
  end;
  {$ENDIF}
{$ENDIF}

function  FOS_IL_Exchange(var Target: longword; Value: longword): longword;
//asm
//{     ->          EAX     Target }
//{                 EDX     Value  }
//{     <-          EAX     Result }
//          XCHG    EAX, EDX
//LOCK      XCHG    [EDX], EAX    // LOCK is implicit on XCHG with memory
begin
  result := InterLockedExchange(target,value);
end;

function  FOS_IL_Exchange(var Target: integer; Value: integer): integer; overload;
begin
 result:=FOS_IL_Exchange(cardinal(target),cardinal(value));
end;

function  FOS_IL_ExchangeAdd(var Addend: longword; Value: integer): longword;
//asm
//{     ->          EAX     Addend }
//{                 EDX     Value  }
//{     <-          EAX     Result }
//          XCHG    EAX, EDX
//LOCK      XADD    [EDX], EAX
begin
 result:=InterLockedExchangeAdd(Addend,Value);
end;


{$IFDEF CPU32}
  {$IFDEF CPUARM}
  function  FOS_IL_ExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
  begin
    abort;
  end;
  {$ELSE}
  {$WARNINGS OFF}
  function  FOS_IL_ExchangeAdd64(var Addend: Int64; Value: Int64): Int64;
  asm
  {     ->          EAX     Addend }
  {                 ESP+4   Value  }
  {     <-          EDX:EAX Result }
            PUSH    EDI
            PUSH    ESI
            PUSH    EBP
            PUSH    EBX

            MOV     ESI, DWORD PTR [Value]    // EDI:ESI = Value
            MOV     EDI, DWORD PTR [Value+4]
            MOV     EBP, EAX

            MOV     EAX, [EBP]    // EDX:EAX = Addend (fetch original Int64 value)
            MOV     EDX, [EBP+4]
  @@1:
            MOV     ECX, EDX      // ECX:EBX = Addend
            MOV     EBX, EAX

            ADD     EBX, ESI
            ADC     ECX, EDI

  LOCK      CMPXCHG8B [EBP]
            JNZ     @@1
            // Returns initial value in Addend

            POP     EBX
            POP     EBP
            POP     ESI
            POP     EDI
  end;
  {$WARNINGS ON}
  {$ENDIF}
{$ENDIF}


function  FOS_IL_CAS32(var Destination: longword; Comparand,Exchange: longword): boolean;
var test:LongWord;
begin
 test:=FOS_IL_CompareExchange(Destination,Exchange,Comparand);
 result:=test=Comparand;
end;


function  FOS_IL_CAS64(var Destination: Int64; Comparand,Exchange: Int64): boolean;
var test:int64;
begin
 test:=FOS_IL_CompareExchange64(Destination,Exchange,Comparand);
 result:=test=Comparand;
end;

{$IFDEF CPU64}

function  FOS_IL_CAS128(var Destination: Int128Rec;  Comparand,Exchange: Int128Rec): boolean;
var test:Int128Rec;
begin
// test:=FOS_IL_CompareExchange128(Destination,Exchange,Comparand);
 if not assigned(lock) then begin
   lock:=TFOS_LOCK.Create;
 end;
 LOCK.Acquire;
 if Destination=Comparand then begin
   Destination:=Exchange;
   LOCK.Release;
   result:=true;
 end else begin
   LOCK.Release;
   result:=false;
 end;
 // Crash on OSX
 //test:=InterlockedCompareExchange128(Destination,Exchange,Comparand);
 //result:=(test.Hi=Comparand.hi) and (test.lo=comparand.lo);
end;
{$ENDIF}

var testp_a,testp_b,testp_c:RFOS_TAGPOINTER;

initialization
 //  Non locked basic selftest of the Interlocked Functions
 {$IFDEF CPU64}
 //assert(sizeof(testp_a)=8,'critical architecture problem sizeof RFOS_TAGPOINTER<>8');
 //testp_a.val.lo:=$1122334455667787;
 //testp_a.val.hi:=$99AABBCCDDEEFF01;
 //testp_b.val.lo:=$1122334554332211;
 //testp_b.val.hi:=$1122334554332211;
 //assert((testp_a.Test[0]=$87) and (testp_a.Test[7]=$11),'critical architecture problem endianess different then designed');
 //FOS_IL_Increment(testp_a.Tag); assert(testp_a.val=$1122334555667787,'critical architecture problem FOS_IL_Increment failed');
 //FOS_IL_Increment64(testp_a.val);assert(testp_a.val=$1122334555667788,'critical architecture problem FOS_IL_Increment64 failed');
 //FOS_IL_Decrement(testp_a.Tag); assert(testp_a.val=$1122334455667788,'critical architecture problem FOS_IL_Decrement failed');
 //FOS_IL_Decrement64(testp_a.val);assert(testp_a.val=$1122334455667787,'critical architecture problem FOS_IL_Decrement64 failed');
 //testp_a.val:=$1122334455667788;
 //testp_b.val:=$1122334554332211;
 //testp_c.val:=$0000000100000000;
 //assert(FOS_IL_CAS32(testp_c.Tag,testp_a.Tag,$00)=false,'critical architecture problem FOS_IL_CAS32 failed result (A)');
 //assert(testp_c.Tag=$01,'critical architecture problem FOS_IL_CompareExchange failed (A)');
 //assert(FOS_IL_CAS32(testp_c.Tag,$01,testp_a.Tag)=true,'critical architecture problem FOS_IL_CAS32 failed result (B)');
 //assert(testp_c.Tag=$11223344,'critical architecture problem FOS_IL_CompareExchange failed (B)');
 //
 //testp_c.val:=$0000000000000001;
 //assert(FOS_IL_CAS64(testp_c.val,testp_b.val,$00)=false,'critical architecture problem FOS_IL_CompareExchange64 failed result (A)');
 //assert(testp_c.val=$01,'critical architecture problem FOS_IL_CompareExchange64 failed (A)');
 //assert(FOS_IL_CAS64(testp_c.val,$01,testp_b.val),'critical architecture problem FOS_IL_CompareExchange64 failed result (B)');
 //assert(testp_c.val=$1122334554332211,'critical architecture problem FOS_IL_CompareExchange64 failed (B)');
{$ENDIF}
{$IFDEF CPU32}
 testp_a.val:=$1122334455667787;
 testp_b.val:=$1122334554332211;
 assert((testp_a.Test[0]=$87) and (testp_a.Test[7]=$11),'critical architecture problem endianess different then designed');
 FOS_IL_Increment(testp_a.Tag); assert(testp_a.val=$1122334555667787,'critical architecture problem FOS_IL_Increment failed');
 FOS_IL_Increment64(testp_a.val);assert(testp_a.val=$1122334555667788,'critical architecture problem FOS_IL_Increment64 failed');
 FOS_IL_Decrement(testp_a.Tag); assert(testp_a.val=$1122334455667788,'critical architecture problem FOS_IL_Decrement failed');
 FOS_IL_Decrement64(testp_a.val);assert(testp_a.val=$1122334455667787,'critical architecture problem FOS_IL_Decrement64 failed');
 testp_a.val:=$1122334455667788;
 testp_b.val:=$1122334554332211;
 testp_c.val:=$0000000100000000;
 assert(FOS_IL_CAS32(testp_c.Tag,testp_a.Tag,$00)=false,'critical architecture problem FOS_IL_CAS32 failed result (A)');
 assert(testp_c.Tag=$01,'critical architecture problem FOS_IL_CompareExchange failed (A)');
 assert(FOS_IL_CAS32(testp_c.Tag,$01,testp_a.Tag)=true,'critical architecture problem FOS_IL_CAS32 failed result (B)');
 assert(testp_c.Tag=$11223344,'critical architecture problem FOS_IL_CompareExchange failed (B)');

 testp_c.val:=$0000000000000001;
 assert(FOS_IL_CAS64(testp_c.val,testp_b.val,$00)=false,'critical architecture problem FOS_IL_CompareExchange64 failed result (A)');
 assert(testp_c.val=$01,'critical architecture problem FOS_IL_CompareExchange64 failed (A)');
 assert(FOS_IL_CAS64(testp_c.val,$01,testp_b.val),'critical architecture problem FOS_IL_CompareExchange64 failed result (B)');
 assert(testp_c.val=$1122334554332211,'critical architecture problem FOS_IL_CompareExchange64 failed (B)');
{$ENDIF}
finalization
   if assigned(lock) then
     lock.Finalize;

end.