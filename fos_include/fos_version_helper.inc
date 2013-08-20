var
      cFOS_PRODUCT_NAME               :string = {$I %FOS_PRODUCT_NAME%};
      cFOS_PRODUCT_MAJOR_VERSION      :string = {$I %FOS_PRODUCT_MAJOR_VERSION%};
      cFOS_PRODUCT_MINOR_VERSION      :string = {$I %FOS_PRODUCT_MINOR_VERSION%};
      cFOS_PRODUCT_BUILD_NUMBER       :string = {$I %FOS_PRODUCT_BUILD_NUMBER%};
      cFOS_PRODUCT_BUILD_HASH         :string = {$I %FOS_PRODUCT_BUILD_HASH%};
      cFOS_PRODUCT_BUILD_TIME         :string = {$I %TIME%};
      cFOS_PRODUCT_BUILD_DATE         :string = {$I %DATE%};
      //cFOS_PRODUCT_FPC_VERSION        :string = {$I %FPCVERSION%};
      //cFOS_PRODUCT_FPC_TARGET         :string = {$I %FPCTARGET%};


procedure GFOS_VHELP_INIT;
begin
  if cFOS_PRODUCT_NAME           = ''  then cFOS_PRODUCT_NAME          := 'UNKNOWN SOLDIER';
  if cFOS_PRODUCT_MAJOR_VERSION  = ''  then cFOS_PRODUCT_MAJOR_VERSION := '0';
  if cFOS_PRODUCT_MINOR_VERSION  = ''  then cFOS_PRODUCT_MINOR_VERSION := '0';
  if cFOS_PRODUCT_BUILD_NUMBER   = ''  then cFOS_PRODUCT_BUILD_NUMBER  := '0';
  if cFOS_PRODUCT_BUILD_HASH     = ''  then cFOS_PRODUCT_BUILD_HASH    := '#';
end;

function GFOS_VHELP_GET_VERSION_STRING:string;
var s:String;
begin
  GFOS_VHELP_INIT;
  s := '';
  WriteStr(s,cFOS_PRODUCT_NAME,' Version ',cFOS_PRODUCT_MAJOR_VERSION,'.',cFOS_PRODUCT_MINOR_VERSION,'.',cFOS_PRODUCT_BUILD_NUMBER,' (',cFOS_PRODUCT_BUILD_HASH,')', ' from ',cFOS_PRODUCT_BUILD_DATE,' ',cFOS_PRODUCT_BUILD_TIME);
  result := LineEnding+s+LineEnding;
  result := result +'(c) FirmOS Business Solutions GmbH, Austria'+LineEnding+'(www.firmos.at)'+LineEnding;
end;


