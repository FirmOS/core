unit fos_default_implementation;

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

{$mode objfpc}{$H+}

interface

uses FOS_TOOL_INTERFACES;

implementation

uses FOS_TOOL_FACTORY,FOS_BASIS_TOOLS,FOS_FILE_LOGGER,FOS_CPU_TOOLS,FRE_DATE_TOOLS;

var gi_TF : TFOS_TOOL_FACTORY;
    gi_BT : TFOS_DEFAULT_BASISTOOLS;
    gi_FL : TObject;
    gi_DT : TObject;
    gi_CT : TObject;


procedure Register_Default_Implementations;
begin
  if not assigned(GFRE_TF)  then begin
    gi_TF   := TFOS_TOOL_FACTORY.Create;
    GFRE_TF := gi_TF;
  end;
  if not assigned(GFRE_BT)  then begin
    gi_BT    := TFOS_DEFAULT_BASISTOOLS.Create;
    GFRE_BT  := gi_BT;
  end;
  if not assigned(GFRE_LOG) then GetFOS_FILE_LOGGER(GFRE_LOG,gi_FL);
  if not assigned(GFRE_CPU) then GetFOS_CPU_TOOL(GFRE_CPU,gi_CT);
  if not assigned(GFRE_DT)  then Get_FOS_DateTools(GFRE_DT,gi_DT);
end;

initialization
  Register_Default_Implementations;
finalization
 gi_TF.FRee;
 gi_BT.Free;
 gi_FL.Free;
 gi_DT.Free;
 gi_CT.Free;

end.

