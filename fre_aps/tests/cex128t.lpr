program cex128t;

uses sysutils;

function InterlockedCompareExchange128(var Target: Int128Rec; NewValue: Int128Rec; Comperand: Int128Rec): Int128Rec; assembler;
 {
    win64:
      rcx ... pointer to result
      rdx ... target
      r8  ... NewValue
      r9  ... Comperand
  }
{$ifdef win64}
  asm
    pushq %rbx

    { store result pointer for later use }
    pushq %rcx

    { load new value }
    movq (%r8),%rbx
    movq 8(%r8),%rcx

    { save target pointer for later use }
    movq %rdx,%r8

    { load comperand }
    movq (%r9),%rax
    movq 8(%r9),%rdx

    {$ifdef oldbinutils}
       .byte 0xF0,0x49,0x0F,0xC7,0x08
    {$else}
    lock cmpxchg16b (%r8)
    {$endif}
    { restore result pointer }
    popq %rcx

    { store result }
    movq %rax,(%rcx)
    movq %rdx,8(%rcx)

    popq %rbx
  end;
{$else win64}
{
  linux:
    rdi       ... target
    [rsi:rdx] ... NewValue
    [rcx:r8]  ... Comperand
    [rdx:rax] ... result
}
  asm
    pushq %rbx

    movq %rsi,%rbx          // new value low
    movq %rcx,%rax          // comperand low
    movq %rdx,%rcx          // new value high
    movq %r8,%rdx           // comperand high
    {$ifdef oldbinutils}
    .byte 0xF0,0x48,0x0F,0xC7,0x0F
    {$else}
    lock cmpxchg16b (%rdi)
    {$endif}

    popq %rbx
  end;
{$endif win64}

var a,b,c,d : Int128Rec;
begin
  a.Lo:=0;
  a.Hi:=0;
  b.Lo:=0;
  b.Hi:=0;
  c.Lo:=0;
  c.hi:=0;
  d := InterlockedCompareExchange128(a,b,c);
end.

