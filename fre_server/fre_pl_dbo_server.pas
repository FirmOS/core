unit fre_pl_dbo_server;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,FRE_APS_INTERFACE,FRE_DB_INTERFACE,fre_basedbo_server,fre_system;

type


  { TFRE_PL_DBO_SERVER }

  TFRE_PL_DBO_SERVER=class(TFRE_DBO_SERVER)
  private
    FDataTimer : IFRE_APSC_TIMER;
  protected
    procedure SetupPersistanceLayer;
    procedure Setup            ; override;
    procedure WatchDog         (const timer : IFRE_APSC_TIMER ; const flag1,flag2 : boolean);
  public
  end;


implementation

{ TFRE_PL_DBO_SERVER }

procedure TFRE_PL_DBO_SERVER.SetupPersistanceLayer;
begin

end;

procedure TFRE_PL_DBO_SERVER.Setup;
begin
  SetupPersistanceLayer;
  FDBO_Srv_Cfg.SpecialFile := cFRE_UX_SOCKS_DIR+'plsrv';
  FDBO_Srv_Cfg.Id          := 'FRE:PLServer';
  FDBO_Srv_Cfg.Port        := '44010';
  FDBO_Srv_Cfg.IP          := '0.0.0.0';
  inherited Setup;
  GFRE_SC.AddTimer('WD',1000,@WatchDog);
end;

procedure TFRE_PL_DBO_SERVER.WatchDog(const timer: IFRE_APSC_TIMER; const flag1, flag2: boolean);
//var obj : IFRE_DB_Object;
begin
   //obj := GFRE_DBI.NewObject;
   //obj.Field('MyData_TS').AsDateTimeUTC := GFRE_DT.Now_UTC;
   //obj.Field('MyData_Dta').AsUInt32 := random(10000);
   //PushDataToClients(obj);
end;



initialization

end.
