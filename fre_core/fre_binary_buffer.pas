unit fre_binary_buffer;

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

interface

uses
  Classes, SysUtils,FOS_BASIS_TOOLS,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE;

type
  TFRE_BINARYDATA=class
  private
    FBuffer          : PByte;
    FBufferPosition  : Pointer;
    FBufferPeekPos   : Pointer;
    FBufferWPosition : Pointer;
    FBufferEndPos    : Pointer;
    FBufferCapacity  : NativeInt; // = size
    FAutoCheck       : Boolean;
    procedure   InternalAutoCheckResult      (const result: boolean);
    procedure   InternalMoveSplit            (const data:pointer;const length:NativeInt;const peek:boolean);
    procedure   InternalFillSplit            (const data:pointer;const length:NativeInt);
    procedure   InternalCheckBufferCapacity  (const length:NativeInt);
    procedure   InternalPeekCheck            ;
  public
    constructor Create                 (const initial_size:NativeInt=1024*4;const maximal_size:NativeInt=0);
    procedure   ResetPeekPosition      ;
    procedure   StartPeek              ;
    function    TailLength             (const peek:boolean=false): NativeInt;
    function    ReadAnsiString         (const length:NativeInt ; out data:String ; const peek:boolean=false):Boolean; // copies full length
    function    ReadU8                 (out    data:Byte       ; const peek:boolean=false):boolean;
    function    ReadU16                (out    data:Word       ; const ConvBEtoN : boolean=false ; const peek:boolean=false):boolean;
    function    ReadI32                (out    data:Integer    ; const ConvBEtoN : boolean=false ; const peek:boolean=false):boolean;
    function    SkipDataBytes          (const length:NativeInt ; const peek:boolean=false):Boolean;
    function    PeekedCount            :NativeInt;

    function    GetCurrentPosition     (const peek:boolean=false): Pointer;
    procedure   ReadDataCopy           (const data:Pointer;const length : Nativeint); // Read Data into Buffer
    function    GetReadDataPointer     (const length:NativeInt):Pointer;
    destructor  Destroy                ; override;
  end;


implementation

procedure TFRE_BINARYDATA.InternalMoveSplit(const data: pointer; const length: NativeInt; const peek: boolean);
var first : NativeInt;
    rest  : NativeInt;
begin
  if peek then begin
    if FBufferPeekPos+length <= FBufferEndpos then begin
      Move(FBufferPeekPos^,data^,length);
      FBufferPeekPos := FBufferPeekPos + length;
      if FBufferPeekPos>=FBufferEndPos then begin
        assert(FBufferPeekPos=FBufferEndPos,'read too much');
        FBufferPeekPos := FBuffer;
      end;
    end else begin
      //writeln('*** BUFFER READ WRAP A ***');
      first           := FBufferEndPos-FBufferPeekPos;
      Move(FBufferPeekPos^,data^,first);
      FBufferPeekPos := FBuffer;
      rest            := length - first;
      Move(FBufferPeekPos^,(data+first+1)^,rest);
      FBufferPeekPos := FBuffer + rest;
    end;
  end else begin
    //writeln('MOVE NO PEEK ');
    if FBufferPosition+length <= FBufferEndpos then begin
      Move(FBufferPosition^,data^,length);
      FBufferPosition := FBufferPosition + length;
      if FBufferPosition>=FBufferEndPos then begin
        assert(FBufferPosition=FBufferEndPos,'read too much');
        FBufferPosition := FBuffer;
      end;
      //writeln('MOVE NO PEEK PL=',TailLength(true),' TL=',TailLength(false));
    end else begin
      //writeln('*** BUFFER READ WRAP B ***');
      first           := FBufferEndPos-FBufferPosition;
      Move(FBufferPosition^,data^,first);
      FBufferPosition := FBuffer;
      rest            := length - first;
      Move(FBufferPosition^,(data+first)^,rest);
      FBufferPosition := FBuffer + rest;
    end;
  end;
end;

procedure TFRE_BINARYDATA.InternalFillSplit(const data: pointer; const length: NativeInt);
var firstlen : NativeInt;
    restlen  : NativeInt;
begin
  if FBufferWPosition+length <= FBufferEndpos then begin
    Move(data^,FBufferWPosition^,length);
    FBufferWPosition := FBufferWPosition + length;
    if FBufferWPosition>=FBufferEndPos then begin
      assert(FBufferWPosition=FBufferEndPos,'wrote too much');
      FBufferWPosition := FBuffer;
    end;
  end else begin
    //writeln('*** BUFFER FILL WRAP A ***');
    firstlen         := FBufferEndPos-FBufferWPosition;
    Move             (data^,FBufferWPosition^,firstlen);
    FBufferWPosition := FBuffer;
    restlen          := length - firstlen;
    Move             ((data+firstlen)^,FBufferWPosition^,restlen);
    FBufferWPosition := FBuffer + restlen;
  end;
end;

procedure TFRE_BINARYDATA.InternalCheckBuffercapacity(const length: NativeInt);
var rest : NativeInt;
begin
  rest := FBufferCapacity - TailLength;  // TailLength must always be greater then Taillength(Peek=true)
  if rest <= length then begin
    GFRE_BT.CriticalAbort('buffer extension not implemented by now / Cap=%d have=%d needed=%d',[FBufferCapacity,rest,length]);
  end;
end;

procedure TFRE_BINARYDATA.InternalPeekCheck;
begin
  if (FBufferPeekPos>FBufferEndPos) or (FBufferPeekPos<FBuffer) then begin
    raise Exception.Create('internal logic fault peek position invalid');
  end;
end;

procedure TFRE_BINARYDATA.InternalAutoCheckResult(const result:boolean);
begin
  if not FAutoCheck  then exit;
  if not result then raise Exception.Create('binary check failed read too much');
end;

constructor TFRE_BINARYDATA.Create(const initial_size: NativeInt; const maximal_size: NativeInt);
begin
  FBuffer          := Getmem(initial_size);
  FBufferEndPos    := FBuffer+initial_size;
  FBufferCapacity  := initial_size;
  FBufferWPosition := FBuffer;
  FBufferPosition  := FBuffer;
  FillByte         (Fbuffer^,initial_size,$EA);
  FAutoCheck       := true;
end;

procedure TFRE_BINARYDATA.ResetPeekPosition;
begin
  FBufferPeekPos := nil;
end;

procedure TFRE_BINARYDATA.StartPeek;
begin
  FBufferPeekPos := FBufferPosition;
end;

function TFRE_BINARYDATA.TailLength(const peek: boolean): NativeInt;
begin
  if peek then begin
  //  InternalPeekCheck;
    result := FBufferWPosition-FBufferPeekPos;
    if Result<0 then begin
      result := result + FBufferCapacity;
    end;
  end else begin
    result := FBufferWPosition-FBufferPosition;
    if Result<0 then begin
      result := result + FBufferCapacity;
    end;
  end;
end;

function TFRE_BINARYDATA.ReadAnsiString(const length: NativeInt; out data: String; const peek:boolean=false): Boolean;
begin
  result := length<=TailLength(peek);
  InternalAutoCheckResult(result);
  if not result then exit;
  SetLength(data,length);
  InternalMoveSplit(@data[1],length,peek);
end;


function TFRE_BINARYDATA.ReadU8(out data: Byte; const peek:boolean): boolean;
begin
  result := 1 <= TailLength(peek);
  InternalAutoCheckResult(result);
  if not result then exit;
  if peek then begin
    data := PByte(FBufferPeekPos)^;
    Inc(FBufferPeekPos);
  end else begin
    data := PByte(FBufferPosition)^;
    Inc(FBufferPosition);
  end;
end;

function TFRE_BINARYDATA.ReadU16(out data: Word; const ConvBEtoN: boolean; const peek:boolean=false): boolean;
begin
  result := 2 <= TailLength(peek);
  InternalAutoCheckResult(result);
  if not result then exit;
  InternalMoveSplit(@data,2,peek);
  if ConvBEtoN then begin
    data := BEtoN(data);
  end;
end;

function TFRE_BINARYDATA.ReadI32(out data: Integer; const ConvBEtoN: boolean; const peek:boolean=false): boolean;
begin
  result := 4 <= TailLength(peek);
  InternalAutoCheckResult(result);
  if not result then exit;
  InternalMoveSplit(@data,4,peek);
  if ConvBEtoN then begin
    data := BEtoN(data);
  end;
end;

function TFRE_BINARYDATA.SkipDataBytes(const length: NativeInt; const peek:boolean=false): Boolean;
begin
  result := length<=TailLength(peek);
  InternalAutoCheckResult(result);
  if peek then begin
    if FBufferPeekPos+length>FBufferEndpos then begin
      FBufferPeekPos := FBuffer + length-(FBufferEndPos-FBufferPeekPos);
    end else begin
      FBufferPeekPos := FBufferPeekPos + length;
    end;
  end else begin
    if FBufferPosition+length>FBufferEndpos then begin
      FBufferPosition := FBuffer + length-(FBufferEndPos-FBufferPosition);
    end else begin
      FBufferPosition := FBufferPosition + length;
    end;
  end;
end;

function TFRE_BINARYDATA.PeekedCount: NativeInt;
begin
  InternalPeekCheck;
  result := FBufferPeekPos-FBufferPosition;
  if result<0 then begin
    result := result + FBufferCapacity;
  end;
end;


function TFRE_BINARYDATA.GetCurrentPosition(const peek: boolean): Pointer;
begin
  if peek then begin
    result := FBufferPeekPos;
  end else begin
    result := FBufferPosition;
  end;
end;


procedure TFRE_BINARYDATA.ReadDataCopy(const data: Pointer; const length: Nativeint);
begin
  InternalCheckBufferCapacity(length);
  InternalFillSplit(data,length);
end;

function TFRE_BINARYDATA.GetReadDataPointer(const length: NativeInt): Pointer;
begin
  abort;
end;

destructor TFRE_BINARYDATA.Destroy;
begin
  Freemem(FBuffer);
end;



end.

