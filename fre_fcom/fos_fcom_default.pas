unit fos_fcom_default;

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

{$MINENUMSIZE 4}
{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}
{$H+}

interface

uses FOS_FCOM_INTERFACES,FOS_FCOM_TYPES;

function Get_FCOM_AI      :IFCOM_AI;
function Get_FCOM_NETSOCK (const IPL:FCOM_IP_LAYER;const Proto:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR):IFCOM_SOCK;


implementation

uses FOS_FCOM_HANDLES;

type
  { TFCOM_Factory }

  TFCOM_Factory=class(TObject,IFCOM_Factory)
   function New_FCOM_AI      :IFCOM_AI;
   function New_FCOM_NETSOCK (const IPL:FCOM_IP_LAYER;const Proto:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR):IFCOM_SOCK;
  end;

var gFF : TFCOM_Factory;

{ TFCOM_Factory }

function TFCOM_Factory.New_FCOM_AI: IFCOM_AI;
begin
  result:=Get_FCOM_AI;
end;

function TFCOM_Factory.New_FCOM_NETSOCK(const IPL: FCOM_IP_LAYER;  const Proto: FCOM_SOCKET_PROTCOL; out Error: EFOS_OS_ERROR): IFCOM_SOCK;
begin
  result:=Get_FCOM_NETSOCK(IPL,Proto,Error);
end;


function Get_FCOM_AI: IFCOM_AI;
begin
  result:=TFCOM_AI.Create;
end;

function Get_FCOM_NETSOCK(const IPL:FCOM_IP_LAYER;const Proto:FCOM_SOCKET_PROTCOL;out Error:EFOS_OS_ERROR):IFCOM_SOCK;
begin
 result:=TFCOM_SOCK.Create(IPL,PROTO,ERROR);
 if error<>EFOS_OS_OK then begin
   result.Finalize;
 end;
end;


initialization
  gFF     := TFCOM_Factory.Create;
  GFRE_FF := gFF; // TFCOM_Factory.Create;

finalization
  gFF.Free;

end.

