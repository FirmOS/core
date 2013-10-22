unit fre_openssl_cmd;

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
{$codepage UTF8}

interface


uses
  Classes, SysUtils,fre_process,fos_tool_interfaces,fre_openssl_interface,fre_system;


const
  csslcnf    ='openssl.cnf';

type

  { TFRE_SSL_OPENSSLCMD }

  TFRE_SSL_OPENSSLCMD = class(TObject,IFRE_SSL)
    private
      procedure  _Checkdir                    (const cdir:string);
      function   _sslDateToDateTime64         (const datestr:string):TFRE_DB_DateTime64;
    protected
      procedure  ClearCABaseInformation       (var ca_base_information:RFRE_CA_BASEINFORMATION);
      function   PrepareDirectory             : string;
      function   PrepareCADirectory           (const ca_base_information:RFRE_CA_BASEINFORMATION) : string;
      procedure  WriteConf                    (const dir:string; const cn,c,st,l,o,ou,email:string);
      procedure  ReadCABaseInformation        (const dir:string; var ca_base_information:RFRE_CA_BASEINFORMATION);
    public
      function   CreateCA                     (const cn,c,st,l,o,ou,email:string; const ca_pass:string; out ca_base_information:RFRE_CA_BASEINFORMATION) : TFRE_SSL_RESULT;
      function   CreateCrt                    (const cn,c,st,l,o,ou,email:string; const ca_pass:string; var ca_base_information:RFRE_CA_BASEINFORMATION; const server:boolean;  out crt_base_information: RFRE_CRT_BASEINFORMATION) : TFRE_SSL_RESULT;
      function   RevokeCrt                    (const cn:string;const ca_pass:string; const crt:string ; var ca_base_information:RFRE_CA_BASEINFORMATION) : TFRE_SSL_RESULT;
      function   ReadCrtInformation           (const crt: string; out cn,c,st,l,o,ou,email:string; out issued_date,end_date:TFRE_DB_DateTime64) : TFRE_SSL_RESULT;

  end;

  procedure Setup_SSL_Interface;
  procedure Cleanup_SSL_Interface;

implementation

var   FSSL: TFRE_SSL_OPENSSLCMD;

procedure LogError(const msg:string);
begin
 raise Exception.Create('Exception raised: ERROR :'+msg);
end;

procedure LogInfo(const msg:string);
begin
 writeln('INFO:',msg);
end;

procedure CheckError(const resultcode: integer; const msg:string='');
begin
  if resultcode<>0 then begin
    raise Exception.Create('Exception raised: Resultcode:'+inttostr(resultcode)+' '+msg);
  end;
end;

procedure Setup_SSL_Interface;
begin
  if not assigned(FSSL)  then
    begin
      FSSL     := TFRE_SSL_OPENSSLCMD.Create;
      Setup_SSL_IF(FSSL);
    end;
end;

procedure Cleanup_SSL_Interface;
begin
 if Assigned(FSSL) then
   begin
     FSSL.Free;
     FSSL:=nil;
     Setup_SSL_IF(FSSL);
   end;
end;


{ TFRE_SSL_OPENSSLCMD }

procedure TFRE_SSL_OPENSSLCMD._Checkdir(const cdir: string);
begin
  ForceDirectories(cdir);
  if not DirectoryExists(cdir) then begin
    raise Exception.Create('Could not create CA Dir '+cdir);
   end;
end;

function TFRE_SSL_OPENSSLCMD._sslDateToDateTime64(const datestr: string): TFRE_DB_DateTime64;
var
    mon     : string;
    d,m,y   : NativeInt;
    h,min,s : NativeInt;
    i       : NativeInt;
const
   CFRE_DT_MONTH   : array[1..12] of string = ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');

begin
  mon := Copy(datestr,1,3);
  for i:= 1 to 12 do
    begin
      if mon=CFRE_DT_MONTH[i] then begin
        m:=i;
        break;
      end;
    end;
  d   :=  StrToInt(trim(Copy(datestr,5,2)));
  h   :=  StrToInt(trim(Copy(datestr,8,2)));
  min :=  StrToInt(trim(Copy(datestr,11,2)));
  s   :=  StrToInt(trim(Copy(datestr,14,2)));
  y   :=  StrToInt(trim(Copy(datestr,17,4)));

  result := GFRE_DT.EncodeTime(y,m,d,h,min,s,0);
end;

procedure TFRE_SSL_OPENSSLCMD.ClearCABaseInformation(var ca_base_information: RFRE_CA_BASEINFORMATION);
begin
  ca_base_information.index:='';
  ca_base_information.index_attr:='';
  ca_base_information.serial:='';
  ca_base_information.crlnumber:='';
  ca_base_information.crl:='';
  ca_base_information.crt:='';
  ca_base_information.key:='';
  ca_base_information.random:='';
end;

function TFRE_SSL_OPENSSLCMD.PrepareDirectory: string;
var
  guid        : TGUID;

begin
 CreateGUID(guid);
 result:=cFRE_TMP_DIR+GFRE_BT.GUID_2_HexString(guid);
 _CheckDir(result);
end;

function TFRE_SSL_OPENSSLCMD.PrepareCADirectory(const ca_base_information: RFRE_CA_BASEINFORMATION): string;
var
  dir         : string;
  dir_signed  : string;
  dir_private : string;


  procedure _WriteFile(const content:string; const filename: string; const create:boolean=false);
  begin
    if content<>'' then begin
      GFRE_BT.StringToFile(dir+DirectorySeparator+filename,content);
    end else begin
      if create then begin
        CheckError(FRE_ProcessCMD('touch '+dir+DirectorySeparator+filename));
      end;
    end;
  end;

  procedure _WriteExtensions;
  var sl : TStringList;
  begin
    sl:=TStringList.Create;
    try
      sl.Add('[ xpclient_ext]');
      sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.2');
      sl.Add('[ xpserver_ext ]');
      sl.Add('extendedKeyUsage = 1.3.6.1.5.5.7.3.1');
      sl.SaveToFile(dir+'/xpxtensions');
    finally
      sl.Free;
    end;
  end;

begin
 dir := PrepareDirectory;

 dir_signed :=dir+DirectorySeparator+'signed_certs';
 dir_private:=dir+DirectorySeparator+'private';
 _CheckDir(dir_signed);
 _CheckDir(dir_private);
 _CheckDir(dir+DirectorySeparator+'crl');
 _WriteExtensions;
 _WriteFile(ca_base_information.index,'index.txt',true);
 _WriteFile(ca_base_information.index_attr,'index.txt.attr');
 _WriteFile(ca_base_information.serial,'serial');
 _WriteFile(ca_base_information.crlnumber,'crlnumber');
 _WriteFile(ca_base_information.crt,'ca.crt');
 _WriteFile(ca_base_information.key,'private/ca.key');
 result := dir;
end;

procedure TFRE_SSL_OPENSSLCMD.WriteConf(const dir: string; const cn, c, st, l, o, ou, email: string);
var lsl:TStringList;
begin
  lsl:=TStringList.Create;
  try

    lsl.Add('RANDFILE                = $ENV::HOME/.rnd');
    lsl.Add('oid_section             = new_oids');
    lsl.Add('[ new_oids ]');
    lsl.Add('[ ca ]');
    lsl.Add('default_ca      = CA_default            # The default ca section');
    lsl.Add('[ CA_default ]');
    lsl.Add('dir             = '+dir+'               # Where everything is kept');
    lsl.Add('certs           = $dir/                 # Where the issued certs are kept');
    lsl.Add('crl_dir         = $dir/crl              # Where the issued crl are kept');
    lsl.Add('database        = $dir/index.txt        # database index file.');
    lsl.Add('#unique_subject = no                    # Set to no to allow creation of');
    lsl.Add('new_certs_dir   = $dir/signed_certs     # default place for new certs.');
    lsl.Add('certificate     = $dir/ca.crt           # The CA certificate');
    lsl.Add('serial          = $dir/serial           # The current serial number');
    lsl.Add('crlnumber       = $dir/crlnumber        # the current crl number');
    lsl.Add('crl             = $dir/crl.pem          # The current CRL');
    lsl.Add('private_key     = $dir/private/ca.key   # The private key');
    lsl.Add('RANDFILE        = $dir/private/.rand    # private random number file');
    lsl.Add('x509_extensions = v3_ca                 # The extentions to add to the cert');
    lsl.Add('name_opt        = ca_default            # Subject Name options');
    lsl.Add('cert_opt        = ca_default            # Certificate field options');
    lsl.Add('default_days    = 3650                  # how long to certify for');
    lsl.Add('default_crl_days= 30                    # how long before next CRL');
    lsl.Add('default_md      = sha1                  # which md to use.');
    lsl.Add('preserve        = no                    # keep passed DN ordering');
    lsl.Add('policy          = policy_match');
    lsl.Add('[ policy_match ]');
    lsl.Add('countryName             = match');
    lsl.Add('stateOrProvinceName     = match');
    lsl.Add('organizationName        = match');
    lsl.Add('organizationalUnitName  = optional');
    lsl.Add('commonName              = supplied');
    lsl.Add('emailAddress            = optional');
    lsl.Add('[ policy_anything ]');
    lsl.Add('countryName             = optional');
    lsl.Add('stateOrProvinceName     = optional');
    lsl.Add('localityName            = optional');
    lsl.Add('organizationName        = optional');
    lsl.Add('organizationalUnitName  = optional');
    lsl.Add('commonName              = supplied');
    lsl.Add('emailAddress            = optional');
    lsl.Add('[ req ]');
    lsl.Add('default_bits            = 1024');
    lsl.Add('default_keyfile         = privkey.pem');
    lsl.Add('distinguished_name      = req_distinguished_name');
    lsl.Add('attributes              = req_attributes');
    lsl.Add('prompt = no');
    lsl.Add('x509_extensions = v3_ca # The extentions to add to the self signed cert');
    lsl.Add('string_mask = nombstr');
    lsl.Add('# req_extensions = v3_req # The extensions to add to a certificate request');
    lsl.Add('[ req_distinguished_name ]');
    if c<>''  then lsl.Add('C  = '+c);
    if st<>'' then lsl.Add('ST = '+st);
    if l<>''  then lsl.Add('L  = '+l);
    if o<>''  then lsl.Add('O  = '+o);
    if ou<>'' then lsl.Add('OU = '+ou);
    if cn<>'' then lsl.Add('CN = '+cn);
    if email<>''  then lsl.Add('emailAddress = '+email);
    lsl.Add('[ req_attributes ]');
    lsl.Add('challengePassword               = A challenge password');
    lsl.Add('[ usr_cert ]');
    lsl.Add('basicConstraints=CA:FALSE');
    lsl.Add('nsComment                       = "OpenSSL Generated Certificate"');
    lsl.Add('subjectKeyIdentifier=hash');
    lsl.Add('authorityKeyIdentifier=keyid,issuer');
    lsl.Add('[ v3_req ]');
    lsl.Add('basicConstraints = CA:FALSE');
    lsl.Add('keyUsage = nonRepudiation, digitalSignature, keyEncipherment');
    lsl.Add('[ v3_ca ]');
    lsl.Add('subjectKeyIdentifier=hash');
    lsl.Add('authorityKeyIdentifier=keyid:always,issuer:always');
    lsl.Add('basicConstraints = CA:true');
    lsl.Add('[ crl_ext ]');
    lsl.Add('authorityKeyIdentifier=keyid:always,issuer:always');

    lsl.SaveToFile(dir+'/'+csslcnf);
  finally
    lsl.Free;
  end;
end;

procedure TFRE_SSL_OPENSSLCMD.ReadCABaseInformation(const dir: string; var ca_base_information: RFRE_CA_BASEINFORMATION);

  procedure _ReadFile(var content:string; const filename: string);
  begin
    if FileExists(dir+DirectorySeparator+filename) then begin
      content := GFRE_BT.StringFromFile(dir+DirectorySeparator+filename);
    end else begin
      content := '';
    end;
  end;

begin
  _ReadFile(ca_base_information.index,'index.txt');
  _ReadFile(ca_base_information.index_attr,'index.txt.attr');
  _ReadFile(ca_base_information.serial,'serial');
  _ReadFile(ca_base_information.crlnumber,'crlnumber');
  _ReadFile(ca_base_information.crt,'ca.crt');
  _ReadFile(ca_base_information.key,'private/ca.key');
  _ReadFile(ca_base_information.crl,'crl'+DirectorySeparator+'ca.crl');
end;

function TFRE_SSL_OPENSSLCMD.CreateCA(const cn, c, st, l, o, ou, email: string; const ca_pass: string; out ca_base_information: RFRE_CA_BASEINFORMATION): TFRE_SSL_RESULT;
var
  dir   : string;
  sl    : TStringList;

begin
  ClearCABaseInformation(ca_base_information);
  dir      := PrepareCADirectory(ca_base_information);
  try
    WriteConf(dir,cn,c,st,l,o,ou,email);

    FRE_ProcessCMDException('openssl req -passout pass:'+ca_pass+' -new -keyout '+dir+'/private/ca.key -out '+dir+'/careq.pem -config '+dir+'/'+csslcnf);
    LogInfo('Sign CA Req ');
    FRE_ProcessCMDException('openssl ca -batch -passin pass:'+ca_pass+' -create_serial -out '+dir+'/ca.crt -keyfile '+dir+'/private/ca.key -selfsign -extensions v3_ca -in '+dir+'/careq.pem -config '+dir+'/'+csslcnf);
//    FRE_ProcessCMDException('openssl x509 -inform PEM -outform DER -in '+dir+'/ca.crt -out '+dir+'/ca.der');
    LogInfo('Create DH ');
    FRE_ProcessCMDException('openssl dhparam -out '+dir+'/dh1024.pem 1024');
    FRE_ProcessCMDException('dd if=/dev/urandom of='+dir+'/random count=2');
    //Create CRL
    sl:=TStringList.Create;
    try
      sl.Add('00');
      sl.SaveToFile(dir+'/crlnumber');
    finally
      sl.Free;
    end;
    LogInfo('Create CA Done ');
    ReadCABaseInformation(dir,ca_base_information);
  finally
    FRE_ProcessCMD('rm -rf '+dir);
  end;
end;

function TFRE_SSL_OPENSSLCMD.CreateCrt(const cn, c, st, l, o, ou, email: string; const ca_pass: string; var ca_base_information: RFRE_CA_BASEINFORMATION; const server: boolean; out crt_base_information: RFRE_CRT_BASEINFORMATION): TFRE_SSL_RESULT;
var
  dir       : string;
begin
  LogInfo('Creating Crt '+cn);
  LogInfo('Creating SSL Conf '+cn);

  dir      := PrepareCADirectory(ca_base_information);
  WriteConf(dir,cn,c,st,l,o,ou,email);
  LogInfo('Creating Crt Req '+cn);

  FRE_ProcessCMDException('openssl req -nodes -new -keyout '+dir+'/private/crt.key -out '+dir+'/crt_req.pem -config '+dir+'/'+csslcnf);
  LogInfo('Sign Crt Req '+cn);
  if server=true then begin
    FRE_ProcessCMDException('openssl ca -verbose -batch -passin pass:'+ca_pass+' -out '+dir+'/signed_certs/crt.crt -extensions xpserver_ext -extfile '+dir+'/xpxtensions -in '+dir+'/crt_req.pem -config '+dir+'/'+csslcnf);
  end else begin
    FRE_ProcessCMDException('openssl ca -verbose -batch -passin pass:'+ca_pass+' -out '+dir+'/signed_certs/crt.crt -extensions xpclient_ext -extfile '+dir+'/xpxtensions -in '+dir+'/crt_req.pem -config '+dir+'/'+csslcnf);
  end;
//  CheckError(Command('openssl pkcs12 -export -passout pass:'+ob.Field('pass').AsString+' -clcerts -in '+cadir_signed+'/'+ob.Field('cn').asstring+'.crt -inkey '+cadir+'/private/'+ob.Field('cn').asstring+'.key -out '+cadir+'/private/'+ob.Field('cn').asstring+'.p12'));
  ReadCABaseInformation(dir,ca_base_information);
  crt_base_information.crt  := GFRE_BT.StringFromFile(dir+DirectorySeparator+'signed_certs'+DirectorySeparator+'crt.crt');
  crt_base_information.key  := GFRE_BT.StringFromFile(dir+DirectorySeparator+'private'+DirectorySeparator+'crt.key');
  LogInfo('Create Crt Done '+cn);
end;

function TFRE_SSL_OPENSSLCMD.RevokeCrt(const cn: string; const ca_pass: string; const crt: string; var ca_base_information: RFRE_CA_BASEINFORMATION): TFRE_SSL_RESULT;
var
  dir : string;
begin
  LogInfo('Revoke Crt '+cn);
  LogInfo('Creating SSL Conf '+cn);

  dir      := PrepareCADirectory(ca_base_information);
  WriteConf(dir,cn,'','','','','','');
  GFRE_BT.StringToFile(dir+DirectorySeparator+'signed_certs'+DirectorySeparator+'crt.crt',crt);
  FRE_ProcessCMDException('openssl ca -passin pass:'+ca_pass+' -revoke '+dir+'/signed_certs/crt.crt -config '+dir+'/'+csslcnf);
  FRE_ProcessCMDException('openssl ca -passin pass:'+ca_pass+' -config '+dir+'/'+csslcnf+' -gencrl -crldays 365 -out '+dir+'/crl/ca.crl');
  ReadCABaseInformation(dir,ca_base_information);
  LogInfo('Revoke Crt Done '+cn);
end;

function TFRE_SSL_OPENSSLCMD.ReadCrtInformation(const crt: string; out cn, c, st, l, o, ou, email: string; out issued_date,end_date:int64): TFRE_SSL_RESULT;
var dir      : string;
    outstr   : string;
    errorstr : string;
    res      : NativeInt;
    sl       : TStringList;
    subject  : string;
    ss       : TFOSStringArray;
    i        : NativeInt;
    ls,r     : string;
begin
  dir  := PrepareDirectory;
  try
    GFRE_BT.StringToFile(dir+DirectorySeparator+'crt.crt',crt);
    res  := FRE_ProcessCMD('openssl x509 -in '+dir+DirectorySeparator+'crt.crt -noout -issuer -startdate -enddate -subject',outstr,errorstr);
    if res=0 then
      begin
        sl := TStringList.Create;
        try
          sl.Text := outstr;
          issued_date :=  _sslDateToDateTime64(trim(GFRE_BT.SepRight(sl[1],'=')));
          end_date    :=  _sslDateToDateTime64(trim(GFRE_BT.SepRight(sl[2],'=')));
          subject     :=  trim(GFRE_BT.SepRight(sl[3],'='));
          GFRE_BT.SeperateString(subject,'/',ss);
          for i := Low(ss) to high(ss) do begin
            r := GFRE_BT.SepRight(ss[i],'=');
            ls:= GFRE_BT.SepLeft (ss[i],'=');
            case ls of
              'C'     : c     := r;
              'CN'    : cn    := r;
              'ST'    : st    := r;
              'L'     : l     := r;
              'O'     : o     := r;
              'OU'    : ou    := r;
              'EMAIL' : email := r;
            end;
          end;
          result:=sslOK;
        finally
          sl.Free;
        end;
      end
    else
      result:=sslNOK;
  finally
    FRE_ProcessCMD('rm -rf '+dir);
  end;
end;



end.

