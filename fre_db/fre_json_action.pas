unit fre_json_action;

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
  Classes, SysUtils,fpjson,FRE_DB_INTERFACE;

type
  TFRE_JSON_ACTIONTYPE = (jat_jsupdate,jat_jsnew,jat_jsreplace,jat_jsexecute);
const
  CFRE_JSON_ACTIONTYPE : Array [TFRE_JSON_ACTIONTYPE] of String = ('jsupdate','jsnew','jsreplace','jsexecute');

type
  { TFRE_JSON_ACTION }

  TFRE_JSON_ACTION=class
  private
    FAction: String;
    FID         : String;
    FupdateID   : String;
    FJSONObj    : IFRE_DB_Object;
    FActionType : TFRE_JSON_ACTIONTYPE;
    procedure SetAction(const AValue: String);
    procedure SetActionType(const AValue: TFRE_JSON_ACTIONTYPE);
    procedure SetID         (const AValue: String);
    procedure SetupdateID   (const AValue: String);
  public
    constructor Create;
    destructor  Destroy;
    property    ActionType :TFRE_JSON_ACTIONTYPE read FActionType write SetActionType;
    property    ID         :string read FID write SetID;
    property    updateID   :string read FupdateID write SetupdateID;
    property    Action     :String read FAction write SetAction;
    function    AsString   :string;
  end;

implementation

{ TFRE_JSON_ACTION }

procedure TFRE_JSON_ACTION.SetActionType(const AValue: TFRE_JSON_ACTIONTYPE);
begin
  FActionType:=AValue;
  FJSONObj.Field('actiontype').AsString:=CFRE_JSON_ACTIONTYPE[AValue];
end;

procedure TFRE_JSON_ACTION.SetAction(const AValue: String);
begin
  FAction:=AValue;
  FJSONObj.Field('action').AsString:=AValue;
end;


procedure TFRE_JSON_ACTION.SetID(const AValue: string);
begin
  FID:=AValue;
  FJSONObj.Field('id').AsString:=AValue;
end;

procedure TFRE_JSON_ACTION.SetupdateID(const AValue: String);
begin
  FupdateID:=AValue;
  FJSONObj.Field('updateid').AsString:=AValue;
end;

constructor TFRE_JSON_ACTION.Create;
begin
  FJSONObj   := GFRE_DBI.NewObject;
  ActionType := jat_jsupdate;
  Action     := '';
end;

destructor TFRE_JSON_ACTION.Destroy;
begin
  inherited;
end;

function TFRE_JSON_ACTION.AsString: string;
begin
  result := FJSONObj.GetAsJSON(true).AsJSON;
end;

end.

