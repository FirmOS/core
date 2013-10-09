unit fre_openssl_interface;
{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
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

{$MODE objfpc} {$H+}
{$interfaces corba}
{$modeswitch nestedprocvars}

uses Classes,Sysutils,fre_db_interface;

type

  TFRE_SSL_RESULT       = (sslOK,sslNOK);

  RFRE_CA_BASEINFORMATION = record
    index      : string;
    index_attr : string;
    serial     : string;
    crlnumber  : string;
    crl        : string;
    crt        : string;
    key        : string;
    random     : string;
  end;

  RFRE_CRT_BASEINFORMATION = record
    crt        : string;
    key        : string;
  end;


  { IFRE_SSL }

  IFRE_SSL=interface
      function  CreateCA           (const cn,c,st,l,o,ou,email:string; const ca_pass:string; out ca_base_information:RFRE_CA_BASEINFORMATION) : TFRE_SSL_RESULT;
      function  CreateCrt          (const cn,c,st,l,o,ou,email:string; const ca_pass:string; var ca_base_information:RFRE_CA_BASEINFORMATION; const server:boolean; out crt_base_information: RFRE_CRT_BASEINFORMATION ) : TFRE_SSL_RESULT;
      function  RevokeCrt          (const cn:string;const ca_pass:string;  const crt:string; var ca_base_information:RFRE_CA_BASEINFORMATION) : TFRE_SSL_RESULT;
  end;

  function GET_SSL_IF : IFRE_SSL;

  procedure Setup_SSL_IF(const sslif : IFRE_SSL);

var

  SSL_IF_VERSION:string='1.0';

implementation

var
  PRIVATE_FRE_SSL : IFRE_SSL;

function GET_SSL_IF: IFRE_SSL;
begin
  abort; // => DISCUSS global interfaces, refcounts and how to handle them, $interfaces corba, finalization, initialization
  if not assigned(PRIVATE_FRE_SSL) then
    raise  EFRE_DB_Exception.Create(edb_ERROR,'no assigned SSL interface');
  result := PRIVATE_FRE_SSL;
end;

procedure Setup_SSL_IF(const sslif: IFRE_SSL);
begin
  if assigned(sslif) and assigned(PRIVATE_FRE_SSL) then
    raise  EFRE_DB_Exception.Create(edb_ERROR,'double assigned SSL interface');
  PRIVATE_FRE_SSL := sslif;
end;

end.

