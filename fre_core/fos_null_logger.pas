unit fos_null_logger;

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
 {$MODE DELPHI}{$H+}
{$ENDIF}
uses FOS_TOOL_INTERFACES;

procedure GetFOS_NULL_LOGGER(out logger:IFOS_FILE_LOGGER);

implementation
{$HINTS OFF}


type

  { TNullLogger }

  TNullLogger=class(TInterfacedObject,IFOS_FILE_LOGGER)
     procedure Sync_Logger;
    procedure LogConsole       (const msg:String);
    procedure Log              (const msg,cat:String;Level:integer=1;const target:string='';const sync:boolean=false);overload;
    procedure Log              (const msg:String;params:array of const;cat:String;Level:integer=1;const target:string='';const sync:boolean=false);overload;
    procedure RegisterCategory (const cat:string;filename:string;turnaround:integer=-1;generations:integer=-1;const level:Integer=-1;const nolog:TFOS_BoolType=fbtNotSet;const not_in_full_log:TFOS_BoolType=fbtNotSet);
    procedure RegisterTarget   (const target:string;targetdir:string;turnaround:integer=-1;generations:integer=-1;const level:Integer=-1);
    procedure RegisterThread   (const Name:string);
    procedure SetDefaults      (const defaultfilename:string;fullfilename,basedir:string;const turnaround,generations,level:cardinal);
  end;

procedure TNullLogger.Sync_Logger;
begin

end;

procedure TNullLogger.LogConsole(const msg: String);
begin

end;

procedure TNullLogger.Log(const msg, cat: String; Level: integer;
  const target: string; const sync: boolean);
begin

end;

procedure TNullLogger.Log(const msg: String; params: array of const;
  cat: String; Level: integer; const target: string; const sync: boolean);
begin

end;

procedure TNullLogger.RegisterCategory(const cat: string; filename: string;
  turnaround: integer; generations: integer; const level: Integer;
  const nolog: TFOS_BoolType; const not_in_full_log: TFOS_BoolType);
begin

end;

procedure TNullLogger.RegisterTarget(const target: string; targetdir: string;
  turnaround: integer; generations: integer; const level: Integer);
begin

end;

procedure TNullLogger.RegisterThread(const Name: string);
begin

end;

procedure TNullLogger.SetDefaults(const defaultfilename: string; fullfilename,
  basedir: string; const turnaround, generations, level: cardinal);
begin

end;

procedure GetFOS_NULL_LOGGER(out logger:IFOS_FILE_LOGGER);
begin
 logger:=TNullLogger.Create;
end;

{ TNullLogger }



end.
