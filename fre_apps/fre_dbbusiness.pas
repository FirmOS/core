unit fre_dbbusiness;

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
  Classes, SysUtils,FOS_TOOL_INTERFACES,
  FRE_DB_COMMON,
  FRE_DB_INTERFACE;

const

    CFOS_DB_CUSTOMERS_COLLECTION                         = 'customers';

procedure Register_DB_Extensions;

type

  { TFRE_DB_COUNTRY }

  TFRE_DB_COUNTRY = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_ADDRESS }

  TFRE_DB_ADDRESS = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_Site }

  TFRE_DB_Site = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  published
    function IMI_Content            (const input:IFRE_DB_Object):IFRE_DB_Object;
  end;


  { TFRE_DB_Phone }

  TFRE_DB_Phone  = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;


  TFRE_DB_EADDRESS = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  TFRE_DB_MAILADDRESS = class(TFRE_DB_EADDRESS)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  TFRE_DB_WEBADDRESS = class(TFRE_DB_EADDRESS)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  end;

  { TFRE_DB_Contact }

  TFRE_DB_Contact  = class (TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_Tenant }

  TFRE_DB_Tenant  = class (TFRE_DB_Contact)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
  published
    class procedure InstallDBObjects (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

  { TFRE_DB_GEOPOSITION }

  TFRE_DB_GEOPOSITION = class(TFRE_DB_ObjectEx)
  protected
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    class procedure InstallDBObjects    (const conn:IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType); override;
  end;

implementation

class procedure TFRE_DB_WEBADDRESS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_EADDRESS');
end;

class procedure TFRE_DB_MAILADDRESS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_EADDRESS');
end;

class procedure TFRE_DB_EADDRESS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('url',fdbft_String).required:=true;
end;

class procedure TFRE_DB_COUNTRY.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('tld',fdbft_String);
  scheme.SetSysDisplayField(TFRE_DB_NameTypeArray.Create('objname','tld'),'%s (%s)');
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_country_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
  group.AddInput('tld',GetTranslateableTextKey('scheme_tld'));
end;

class procedure TFRE_DB_COUNTRY.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      CheckDbResult(StoreTranslateableText(conn,'scheme_country_group','Country'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_name','Country Name'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_tld','Country TLD'));
    end;
   
end;

class procedure TFRE_DB_ADDRESS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('street',fdbft_String).required:=true;
  scheme.AddSchemeField('nr',fdbft_String).required:=true;
  scheme.AddSchemeField('stair',fdbft_String);
  scheme.AddSchemeField('floor',fdbft_String);
  scheme.AddSchemeField('door',fdbft_String);
  scheme.AddSchemeField('co',fdbft_String);
  scheme.AddSchemeField('city',fdbft_String).required:=true;
  scheme.AddSchemeField('zip',fdbft_String).required:=true;
  scheme.AddSchemeFieldSubscheme('country','TFRE_DB_COUNTRY').required:=true;

  group:=scheme.AddInputGroup('main').Setup('scheme');
  group.AddInput('street',GetTranslateableTextKey('scheme_street'));
  group.AddInput('nr',GetTranslateableTextKey('scheme_nr'));
  group.AddInput('stair',GetTranslateableTextKey('scheme_stair'));
  group.AddInput('floor',GetTranslateableTextKey('scheme_floor'));
  group.AddInput('door',GetTranslateableTextKey('scheme_door'));
  group.AddInput('co',GetTranslateableTextKey('scheme_co'));
  group.AddInput('city',GetTranslateableTextKey('scheme_city'));
  group.AddInput('zip',GetTranslateableTextKey('scheme_zip'));
  group.AddInput('country',GetTranslateableTextKey('scheme_country'),false,false,'country');
end;

class procedure TFRE_DB_ADDRESS.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      StoreTranslateableText(conn,'scheme_street','Street');
      StoreTranslateableText(conn,'scheme_nr','Nr');
      StoreTranslateableText(conn,'scheme_stair','Stair');
      StoreTranslateableText(conn,'scheme_floor','Floor');
      StoreTranslateableText(conn,'scheme_door','Door');
      StoreTranslateableText(conn,'scheme_co','Care off');
      StoreTranslateableText(conn,'scheme_city','City');
      StoreTranslateableText(conn,'scheme_zip','ZIP');
      StoreTranslateableText(conn,'scheme_country','Country');
    end;
   
end;

class procedure TFRE_DB_GEOPOSITION.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('longitude',fdbft_Real64).required:=true;
  scheme.AddSchemeField('latitude',fdbft_Real64).required:=true;
  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('longitude',GetTranslateableTextKey('scheme_longitude'));
  group.AddInput('latitude',GetTranslateableTextKey('scheme_latitude'));
end;

class procedure TFRE_DB_GEOPOSITION.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      CheckDbResult(StoreTranslateableText(conn,'scheme_main_group','Geoposition'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_longitude','Longitude'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_latitude','Latitude'));
    end;
   
end;

class procedure TFRE_DB_Phone.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.AddSchemeField('number',fdbft_String).required:=true;
end;

class procedure TFRE_DB_Contact.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.Explanation:='Base scheme for all contacts<br>(persons, customers, tenant)';
  scheme.AddSchemeField('parent_contact_id',fdbft_ObjLink);

  scheme.AddSchemeField('firstname',fdbft_String).required:=true;
  scheme.AddSchemeField('lastname',fdbft_String).required:=true;
  scheme.AddSchemeField('company',fdbft_String);
  scheme.AddSchemeFieldSubscheme('mainaddress','TFRE_DB_ADDRESS').required:=true;
  scheme.AddSchemeFieldSubscheme('deliveryaddress','TFRE_DB_ADDRESS');
  scheme.AddSchemeFieldSubscheme('billingaddress','TFRE_DB_ADDRESS');
  scheme.AddSchemeFieldSubscheme('businessphone','TFRE_DB_PHONE');
  scheme.AddSchemeFieldSubscheme('mobilephone','TFRE_DB_PHONE');
  scheme.AddSchemeFieldSubscheme('privatephone','TFRE_DB_PHONE');
  scheme.AddSchemeFieldSubscheme('mail','TFRE_DB_MAILADDRESS');
  scheme.AddSchemeFieldSubscheme('http','TFRE_DB_WEBADDRESS');

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('company',GetTranslateableTextKey('scheme_company'));
  group.AddInput('firstname',GetTranslateableTextKey('scheme_firstname'));
  group.AddInput('lastname',GetTranslateableTextKey('scheme_lastname'));

  group:=scheme.AddInputGroup('address').Setup(GetTranslateableTextKey('scheme_address_group'));
  group.UseInputGroup('TFRE_DB_ADDRESS','main','mainaddress');

  group:=scheme.AddInputGroup('address_delivery').Setup(GetTranslateableTextKey('scheme_delivery_group'));
  group.UseInputGroup('TFRE_DB_ADDRESS','main','deliveryaddress');

  group:=scheme.AddInputGroup('address_billing').Setup(GetTranslateableTextKey('scheme_billingadress_group'));
  group.UseInputGroup('TFRE_DB_ADDRESS','main','billingaddress');

  group:=scheme.AddInputGroup('number').Setup(GetTranslateableTextKey('scheme_number_group'));
  group.AddInput('businessphone.number',GetTranslateableTextKey('scheme_bnumber'));
  group.AddInput('mobilephone.number',GetTranslateableTextKey('scheme_mnumber'));
  group.AddInput('privatephone.number',GetTranslateableTextKey('scheme_pnumber'));

  group:=scheme.AddInputGroup('eaddresses').Setup(GetTranslateableTextKey('scheme_eaddress_group'));
  group.AddInput('mail.url',GetTranslateableTextKey('scheme_mail'));
  group.AddInput('http.url',GetTranslateableTextKey('scheme_web'));

  scheme.AddSchemeField('tenantid',fdbft_ObjLink).required:=true;
  scheme.AddSchemeField('customernumber',fdbft_String).required:=true;

  group:=scheme.AddInputGroup('customer').Setup(GetTranslateableTextKey('scheme_customer_group'));
  group.AddInput('customernumber',GetTranslateableTextKey('scheme_number'));
  group.AddInput('tenantid','',False,true);


end;

class procedure TFRE_DB_Contact.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      CheckDbResult(StoreTranslateableText(conn,'scheme_main_group','General Information'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_address_group','Main Address'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_delivery_group','Delivery Address'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_billingadress_group','Billing Address'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_number_group','Phone Numbers'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_eaddress_group','EContact'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_company','Company'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_firstname','Firstname'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_lastname','Lastname'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_bnumber','Business'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_mnumber','Mobile'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_pnumber','Private'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_mail','EMail'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_web','Web'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_customer_group','Customer Information'));
      CheckDbResult(StoreTranslateableText(conn,'scheme_customernumber','Customer Number'));

    end;
   
end;


{ TFRE_DB_Tenant }

class procedure TFRE_DB_Tenant.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  inherited RegisterSystemScheme(scheme);
  scheme.SetParentSchemeByName('TFRE_DB_CONTACT');
  scheme.Explanation:='Extended scheme from TFRE_DB_CONTACT for tenant information';

   //constraints
   // TFRE_DB_CUSTOMER:
   // tenantid must link to existend TFRE_DB_TENANT
   // customernumber must be unique per tenant
   // TFRE_DB_COUNTRY:
   // name and tld must be unique
   // TFRE_DB_ADDRESS:
   // country must link to existend TFRE_DB_COUNTRY
end;

class procedure TFRE_DB_Tenant.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      { Franz weiß warum, einfach nachfragen}
    end;
   
end;


{ TFRE_DB_Site }

class procedure TFRE_DB_Site.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
var group : IFRE_DB_InputGroupSchemeDefinition;
begin
  inherited RegisterSystemScheme(scheme);
  scheme.GetSchemeField('objname').required:=true;
  scheme.AddSchemeField('sitekey',fdbft_String);
  scheme.AddSchemeField('customerid',fdbft_ObjLink).required:=true;
  scheme.AddSchemeFieldSubscheme('position','TFRE_DB_GEOPOSITION').required:=false;
  scheme.AddSchemeFieldSubscheme('address','TFRE_DB_ADDRESS').required:=false;
  scheme.AddSchemeField('extension',fdbft_ObjLink);

  group:=scheme.AddInputGroup('main').Setup(GetTranslateableTextKey('scheme_main_group'));
  group.AddInput('objname',GetTranslateableTextKey('scheme_name'));
  group.AddInput('sitekey',GetTranslateableTextKey('scheme_key'));
  group.AddInput('customerid','',false,true);
  group:=scheme.AddInputGroup('address').Setup(GetTranslateableTextKey('scheme_address_group'));
  group.UseInputGroup('TFRE_DB_ADDRESS','main','address');
  group.UseInputGroup('TFRE_DB_GEOPOSITION','main','position');
end;

class procedure TFRE_DB_Site.InstallDBObjects(const conn: IFRE_DB_SYS_CONNECTION; var currentVersionId: TFRE_DB_NameType; var newVersionId: TFRE_DB_NameType);
begin
  newVersionId:='1.0';
  if currentVersionId='' then
    begin
      currentVersionId:='1.0';
      StoreTranslateableText(conn,'scheme_main_group','General Information');
      StoreTranslateableText(conn,'scheme_address_group','Site Address');
      StoreTranslateableText(conn,'scheme_name','Name');
      StoreTranslateableText(conn,'scheme_key','Key');
    end;
   
end;

function TFRE_DB_Site.IMI_Content(const input: IFRE_DB_Object): IFRE_DB_Object;
var
  res    : TFRE_DB_FORM_PANEL_DESC;
  scheme : IFRE_DB_SchemeObject;
begin
  scheme := GetScheme;
  res:=TFRE_DB_FORM_PANEL_DESC.create.Describe('Site');
  res.AddSchemeFormGroup(scheme.GetInputGroup('main'),GetSession(input));
  res.AddSchemeFormGroup(scheme.GetInputGroup('address'),GetSession(input));
  res.FillWithObjectValues(Self,GetSession(input));
  res.AddButton.Describe('Save',TFRE_DB_SERVER_FUNC_DESC.create.Describe(Self,'saveOperation'),fdbbt_submit);
  Result:=res;
end;




procedure Register_DB_Extensions;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_EADDRESS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_MAILADDRESS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_WEBADDRESS);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_GEOPOSITION);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_COUNTRY);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_ADDRESS);

  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Site);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Phone);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Contact);
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_Tenant);
  //GFRE_DBI.Initialize_Extension_Objects;
end;

end.

