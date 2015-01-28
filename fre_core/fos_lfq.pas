unit fos_lfq;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

{$MODE objfpc} {$H+}

interface

uses SysUtils,Classes,FOS_INTERLOCKED,FOS_TOOL_INTERFACES;

type

 RFOS_QNode=record
   next_node : RFOS_TAGPOINTER; { make shure that the TAGPOINTER is first in structure and thus aligned }
   data      : Pointer;
 end;

 PFOS_QNode=^RFOS_QNode;

 { TFOS_LFQ }

 TFOS_LFQ=class(TInterfacedObject,IFOS_LFQ)
 private
   base_node:PFOS_QNode;
   queue_head,queue_tail:RFOS_TAGPOINTER;
   FQCnt : NativeInt;
 public
   constructor Create        ;
   destructor  Destroy       ;override;
   procedure   Push          (AItem: Pointer);inline;
   function    Pop: Pointer  ;inline;
   function    SomethingOnQ  : NativeInt;
   procedure   Finalize      ;
 end;


implementation


procedure MyFreeMem(const P: Pointer); inline;
begin
 FreeMem(P);
end;

procedure MyGetMem(var P:Pointer;Size: Integer); inline;
begin
 GetMem(P,size);
end;


{ TFOS_LFQ }

constructor TFOS_LFQ.Create;
begin
 MyGetMem(Pointer(base_node),SizeOf(RFOS_QNode));
 base_node^.next_node.Ptr:=nil;
 base_node^.next_node.Tag:=0;
 base_node^.data:=nil;
 queue_head.Ptr:=base_node;
 queue_head.Tag:=0;
 queue_tail.Ptr:=base_node;
 queue_tail.Tag:=0;
 FQCnt:=0;
end;

destructor TFOS_LFQ.Destroy;
begin
  MyFreeMem(queue_head.Ptr);
  inherited;
end;

function TFOS_LFQ.Pop: Pointer;
var next,head,tail:RFOS_TAGPOINTER;
begin
 result:=nil;
 while(true) do begin
  head.val:=queue_head.val;
  tail.val:=queue_tail.val;
  next:=PFOS_QNode(head.Ptr)^.next_node;
  if head.val=queue_head.val then begin // consistent ?
   if head.Ptr=tail.Ptr then begin // queue empty ? or tail falling behind
    if next.Ptr=nil then begin // queue is empty
     exit;
    end;
    {$IFDEF CPU64}
    FOS_IL_CAS128(queue_tail.val,tail.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val);
    {$ELSE}
    FOS_IL_CAS64(queue_tail.val,tail.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val);
    {$ENDIF}
   end else begin // No need to deal with tail
    result:=PFOS_QNODE(next.Ptr)^.data;
    {$IFDEF CPU64}
    if FOS_IL_CAS128(queue_head.val,head.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val) then begin
    {$ELSE}
    if FOS_IL_CAS64(queue_head.val,head.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val) then begin
    {$ENDIF}
    {$IFDEF CPU64}
    FOS_IL_Decrement64(FQCnt);
    {$ELSE}
    FOS_IL_Decrement(FQCnt);
    {$ENDIF}
     break; // dequeue is done
    end else begin
     result:=nil; // dont report false results...
    end;
   end;
  end;
 end;
 MyFreeMem(head.Ptr);
end;

function TFOS_LFQ.SomethingOnQ: NativeInt;
//var next,head,tail:RFOS_TAGPOINTER;
begin
 result := FQCnt;
 assert(result>=0);
 //abort;
 //result:=false;
 //while(true) do begin
 // head.val:=queue_head.val;
 // tail.val:=queue_tail.val;
 // next:=PFOS_QNode(head.Ptr)^.next_node;
 // if head.val=queue_head.val then begin // consistent ?
 //   if head.Ptr=tail.Ptr then begin // queue empty ? or tail falling behind
 //     if next.Ptr=nil then begin // queue is empty
 //       exit(false);
 //     end else begin
 //       exit(true);
 //     end;
 //  end else begin // No need to deal with tail
 //   exit((PFOS_QNODE(next.Ptr)^.data)<>nil);
 //  end;
 // end;
 //end;
end;

procedure TFOS_LFQ.Finalize;
begin
  free;
end;

{$Q-}
procedure TFOS_LFQ.Push(AItem: Pointer);
var node:PFOS_QNode;
    tail,next,x:RFOS_TAGPOINTER;
begin
 node:=nil;
 MyGetMem(Pointer(node),SizeOf(RFOS_QNode));
 node^.data:=AItem;
 node^.next_node.Ptr:=nil;
 while(true) do begin
  tail.val:=queue_tail.val;
  next.val:=PFOS_QNode(tail.Ptr)^.next_node.val;
  if tail.val=queue_tail.val then begin // Consistent ?
   if next.Ptr=nil then begin // Tail on Last node ?
    {$IFDEF CPU64}
    if FOS_IL_CAS128(PFOS_QNode(tail.Ptr)^.next_node.val,next.val,FOS_GETTAGPTR(node,next.Tag+1).val) then begin // try to link node in
    {$ELSE}
    if FOS_IL_CAS64(PFOS_QNode(tail.Ptr)^.next_node.val,next.val,FOS_GETTAGPTR(node,next.Tag+1).val) then begin // try to link node in
    {$ENDIF}
      break;
    end;
   end else begin
     {$IFDEF CPU64}
     FOS_IL_CAS128(queue_tail.val,tail.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val); // Try to swing tail to next node
     {$ELSE}
     FOS_IL_CAS64(queue_tail.val,tail.val,FOS_GETTAGPTR(next.Ptr,tail.Tag+1).val); // Try to swing tail to next node
     {$ENDIF}
   end;
  end;
 end; // end while
 {$IFDEF CPU64}
 FOS_IL_CAS128(queue_tail.val,tail.val,FOS_GETTAGPTR(node,tail.Tag+1).val); // Try to swing tail to inserted node
 FOS_IL_Increment64(FQCnt);
 {$ELSE}
 FOS_IL_CAS64(queue_tail.val,tail.val,FOS_GETTAGPTR(node,tail.Tag+1).val); // Try to swing tail to inserted node
 FOS_IL_Increment(FQCnt);
 {$ENDIF}
end;
 {$Q+}


initialization

finalization

end.
