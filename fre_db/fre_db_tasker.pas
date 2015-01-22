unit fre_db_tasker;

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
{$modeswitch nestedprocvars}
interface

uses
  Classes, SysUtils,FRE_SYSTEM, FRE_DB_COMMON,FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_DB_WEB_STYLING;

type

  { TFRE_DB_TASKER }

  TFRE_DB_TASKER = class (TFRE_DB_APPLICATION)
  protected
    class procedure InstallDBObjects        (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
    procedure       MySessionInitialize     (const session: TFRE_DB_UserSession); override;
    procedure       MySessionPromotion      (const session: TFRE_DB_UserSession); override;
    procedure       MyServerInitialize      (const admin_dbc: IFRE_DB_CONNECTION); override;
    function        ShowInApplicationChooser(const session:IFRE_DB_UserSession): Boolean;override;
  public
    procedure       TASKER_METHOD        (const ses: IFRE_DB_Usersession);    { gets called every second }
    procedure       TASKER_REQUEST       (const ses: IFRE_DB_Usersession ; const flag1,flag2 : boolean); { called on request }
    procedure       InitSession          (const session:TFRE_DB_UserSession); { HACK : Should be done with Rights, in general session initialice (session creation) }
  published
    //function WEB_Content         (const input:IFRE_DB_Object; const ses: IFRE_DB_Usersession; const app: IFRE_DB_APPLICATION; const conn: IFRE_DB_CONNECTION):IFRE_DB_Object;
  end;

implementation

{ TFRE_DB_TASKER }

class procedure TFRE_DB_TASKER.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  inherited InstallDBObjects(conn, currentVersionId, newVersionId);
  newVersionId:='1.0';
  if (currentVersionId='') then begin
    currentVersionId:='1.0';
    //CreateAppText(conn,'profile_diag_cap','Profile');
  end;
  if (currentVersionId='1.0') then begin
    //currentVersionId:='1.1';
  end;
end;

procedure TFRE_DB_TASKER.MySessionInitialize(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_TASKER.MySessionPromotion(const session: TFRE_DB_UserSession);
begin

end;

procedure TFRE_DB_TASKER.MyServerInitialize(const admin_dbc: IFRE_DB_CONNECTION);
begin
  inherited MyServerInitialize(admin_dbc);
end;

function TFRE_DB_TASKER.ShowInApplicationChooser(const session: IFRE_DB_UserSession): Boolean;
begin
  Result:=false;
end;

procedure TFRE_DB_TASKER.TASKER_METHOD(const ses: IFRE_DB_Usersession);
var
   // wf_coll : IFRE_DB_COLLECTION; wf_coll :=  ses.GetDBConnection.AdmGetWorkFlowCollection;
    live    : IFRE_DB_Object;
    g       : TFRE_DB_GUID;

begin
  if not G_LiveFeeding then
    begin
      GFRE_TF.Get_Lock(G_LiveStatLock);
      G_LiveStats   := GFRE_DBI.NewObject;
      G_LiveFeeding := true;
    end;
  //live := GFRE_DBI.NewObject;
  //live.Field('zpool_desc').AsString:='SCRUBBING FAST '+IntToStr(random(100))+'%';
  //g.SetFromHexString('a35864a4d66063a6474f39ce5f27e9f9');
  //live.Field('statuid').AsGUID := g;
  //G_LiveStats.Field('a35864a4d66063a6474f39ce5f27e9f9').AsObject := live;
  G_LiveStatLock.Acquire;
  try
    //GFRE_DB_TCDM.UpdateLiveStatistics(G_LiveStats);
  finally
    G_LiveStatLock.Release;
  end;
end;

procedure TFRE_DB_TASKER.TASKER_REQUEST(const ses: IFRE_DB_Usersession; const flag1, flag2: boolean);
begin

end;

procedure TFRE_DB_TASKER.InitSession(const session: TFRE_DB_UserSession);
begin
  MySessionInitialize(session);
end;

end.

