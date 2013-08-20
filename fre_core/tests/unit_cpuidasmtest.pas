unit unit_cpuidasmtest;

{$mode objfpc}{$H+}
{$asmmode INTEL}

interface

uses
  Classes, SysUtils;
type

  TickType = Int64; // Global declaration for all tick processing routines

  function OHasHardwareCapabilityData: Boolean;
  function OHasHardwareTickCounter: longword;
  function OHardwareTicks: int64;
  type TCPUID=array[1..4] of longword;
  function OGetCPUID : TCPUID;
implementation
{$IFDEF CPU32}
function OGetCPUID : TCPUID; assembler; register;
asm
  PUSH EBX {Save affected register}
  PUSH EDI
  MOV EDI,EAX {@Resukt}
  MOV EAX,1
  DW $A20F {CPUID Command}
  STOSD {CPUID[1]}
  MOV EAX,EBX
  STOSD {CPUID[2]}
  MOV EAX,ECX
  STOSD {CPUID[3]}
  MOV EAX,EDX
  STOSD {CPUID[4]}
  POP EDI {Restore registers}
  POP EBX
end;

// Pentium specific... push and pop the flags and check for CPUID availability
function OHasHardwareCapabilityData: Boolean;
begin
  asm
   PUSHFD
   POP    EAX
   MOV    EDX,EAX
   XOR    EAX,$200000
   PUSH   EAX
   POPFD
   PUSHFD
   POP    EAX
   XOR    EAX,EDX
   JZ     @EXIT
   MOV    AL,TRUE
   @EXIT:
  end;
end;


function OHasHardwareTickCounter: longword;
  var FeatureFlags: Longword;
  begin
    FeatureFlags:=0;
    asm
      PUSH   EBX
      XOR    EAX,EAX
      DW     $A20F
      POP    EBX
      CMP    EAX,1
      JL     @EXIT
      XOR    EAX,EAX
      MOV    EAX,1
      PUSH   EBX
      DW     $A20F
      MOV    FEATUREFLAGS,EDX
      POP    EBX
      @EXIT:
    end;
    Result := FeatureFlags;
  end;

// Execute the Pentium's RDTSC instruction to access the counter value.
function OHardwareTicks: TickType; assembler; asm DW 0310FH end;

{$ELSE}
function OHasHardwareCapabilityData: Boolean;
begin
  abort; //TODO CPU64 CODE
end;

function OGetCPUID : TCPUID;
begin
  abort; //TODO CPU64 CODE
end;

function OHasHardwareTickCounter: longword;
begin
  abort;
end;

function OHardwareTicks: TickType;
begin
  abort;
end;

{$ENDIF}




end.

