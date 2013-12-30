unit fre_db_testsuite; 
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}
{$codepage UTF8}

// RAW Testsuite of DB Core

interface

uses
  Classes, SysUtils,fpcunit,testregistry,testdecorator,
  FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE;

  var TEST_GUID_1,TEST_GUID_2,TEST_GUID_3 : TGUID;

  procedure RegisterTestCodeClasses;

  procedure Check_Test_Object(const field_prefix:string;const obj:IFRE_DB_Object;const check_domid:boolean=false);

type

  { TFRE_DB_TEST_CODE_CLASS }

  TFRE_DB_TEST_CODE_CLASS=class(TFRE_DB_ObjectEx)
  public
    procedure InternalSetup; override;
    class procedure RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT); override;
    procedure   CALC_Uint32 (const calc : IFRE_DB_CALCFIELD_SETTER);
    procedure   CALC_String (const calc : IFRE_DB_CALCFIELD_SETTER);
  published
    function IMI_SimpleTest(const input:IFRE_DB_Object):IFRE_DB_Object;
  end;



// fdbft_Object,fdbft_ObjLink,fdbft_CalcField
  { TFRE_DB_ObjectTests }

  TFRE_DB_ObjectTests = class (TTestCase)
  private
    TestObject : TFRE_DB_Object;
    procedure SetUp; override;
    procedure TearDown;override;
  published
    procedure CheckCodePage;
    procedure Dirty;
    procedure FieldCount;
    procedure UID_NotNull;
    procedure UID_Exists;
    procedure UID_Delete;
    procedure UID_Delete2;
    procedure UID_Path_Simple;
    procedure UID_String;
    procedure UID_Set;

    procedure DOMAIND_Exists;
    procedure DOMAIND_Delete;
    procedure DOMAIND_Delete2;
    procedure DOMAIND_Set;

    procedure Mediator_Empty;
    procedure ObjectProperties;
    procedure FieldTypes;
    procedure DumpTest;
    procedure DumpJSONTestFull;
    procedure StreamTest;
    procedure StreamTest2;
    procedure SetNullArray;
    procedure CloneToNewChangeGUIDS;
    procedure GenericChangelistTest;
    procedure TestStreamFieldClone;
    procedure ForAllHierarchicTest;


  end;

  { TFRE_DB_PersistanceTests }

  TFRE_DB_PersistanceTests = class(TTestcase)
  private
    FSysconn  : IFRE_DB_SYS_CONNECTION;
    FWorkConn : IFRE_DB_CONNECTION;
    procedure ConnectDB(const user,pw:string);
    procedure WriteObject(const obj : IFRE_DB_Object);

  published
    procedure PreCleanup;
    procedure CreateTestDatabase;
    procedure LayerListDatabases;
    procedure DropTestDatabase;
    procedure SystemCreate;
    procedure SystemConnect;
    procedure SystemWrongUser;
    procedure SystemAdduser;
    procedure SystemCheckUser;
    procedure SystemSyncSnapshot;
    procedure SetupTestWorkDB;
    procedure SetupTestCollections;
    procedure FetchTestColletion;
    procedure DoReflinkTests;
    procedure DefineIndices;
    procedure GenerateIndexTestData;
    procedure TestIdxRangeQueries;
    procedure TestIdxUpdate;
    procedure ReconnectNotSyncedFromWAL;
    procedure DumpDatabase;
  end;

implementation


procedure Fill_Test_Object(const field_prefix:string;const obj:IFRE_DB_Object);
var dbs : TFRE_DB_Stream;
begin
  obj.ClearAllFields;
  obj.Field(field_prefix+'STRING').AsStringArr               := TFRE_DB_StringArray.Create('äüö ÄÜÖ ß','מדוע לא דברו עברית?','ག་རེ་བྱས་ཁོ་རང་ཚོས་བོད་སྐད་ཆ་དེ་ག་རང་བཤད་ཀྱི་མ་རེད།','लोकांना मराठी का बोलता येत नाही?');
  obj.Field(field_prefix+'STRING_NA').SetAsEmptyStringArray  ;

  obj.Field(field_prefix+'UID').AsGUIDArr                    := TFRE_DB_GuidArray.Create(TEST_GUID_1,TEST_GUID_2,TEST_GUID_3);
  obj.Field(field_prefix+'BYTE').AsByteArr                   := TFRE_DB_ByteArray.Create(0,255,256,-1);
  obj.Field(field_prefix+'INT16').AsInt16Arr                 := TFRE_DB_Int16Array.Create(-32768,32767,0,65535);
  obj.Field(field_prefix+'INT32').AsInt32Arr                 := TFRE_DB_Int32Array.Create(-2147483648,2147483647,0,4294967295);
  obj.Field(field_prefix+'INT64').AsInt64Arr                 := TFRE_DB_Int64Array.Create(-9223372036854775808,9223372036854775807,0,18446744073709551615);
  obj.Field(field_prefix+'UINT16').AsUInt16Arr               := TFRE_DB_UInt16Array.Create(-32768,32767,0,65535);
  obj.Field(field_prefix+'UINT32').AsUInt32Arr               := TFRE_DB_UInt32Array.Create(-2147483648,2147483647,0,4294967295);
  obj.Field(field_prefix+'UINT64').AsUInt64Arr               := TFRE_DB_UInt64Array.Create(-9223372036854775808,9223372036854775807,0,18446744073709551615);

  obj.Field(field_prefix+'REAL32').AsReal32Arr               := TFRE_DB_Real32Array.Create(pi,0,1,-2.2,3.3,-4.4);
  obj.Field(field_prefix+'REAL64').AsReal64Arr               := TFRE_DB_Real64Array.Create(pi,0,1,-2.2,3.3,-4.4);

  obj.Field(field_prefix+'CURRENCY').AsCurrencyArr           := TFRE_DB_CurrencyArray.Create(122.93,100.2,33.90);
  obj.Field(field_prefix+'BOOLEAN').AsBooleanArr             := TFRE_DB_BoolArray.Create(true,false,false,true);
  obj.Field(field_prefix+'DATE').AsDateTimeArr               := TFRE_DB_DateTimeArray.Create(10000000,20000000,30000000,40000000);
  obj.Field(field_prefix+'DATE_UTC').AsDateTimeUTCArr        := TFRE_DB_DateTimeArray.Create(10000000,20000000,30000000,40000000);
  obj.Field(field_prefix+'DATE_NOW').AsDateTime              := GFRE_DT.DateTimeToDBDateTime64(now);
  obj.Field(field_prefix+'STREAM1').AsStream.WriteAnsiString('THIS IS A TESTSTREAM');
  dbs := TFRE_DB_Stream.Create;
  dbs.WriteAnsiString('THIS IS A TESTSTREAM');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM');
  obj.Field(field_prefix+'STREAM2').AddStream(dbs);
  dbs := TFRE_DB_Stream.Create;
  dbs.WriteAnsiString('THIS IS A TESTSTREAM2');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM2');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM2');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM2');
  dbs.WriteAnsiString('THIS IS A TESTSTREAM2');
  obj.Field(field_prefix+'STREAM2').AddStream(dbs);
  obj.field('domainid').AsGUID:=TEST_GUID_3;
end;

procedure Check_Test_Object(const field_prefix:string;const obj:IFRE_DB_Object;const check_domid:boolean=false);
var dbs :  TFRE_DB_Stream;
    sa   : TFRE_DB_StringArray;
    cs   : TFRE_DB_String;
    ga   : TFRE_DB_GUIDArray;
    ba   : TFRE_DB_ByteArray;
    i16  : TFRE_DB_Int16Array;
    i32  : TFRE_DB_Int32Array;
    i64  : TFRE_DB_Int64Array;
    u16  : TFRE_DB_UInt16Array;
    u32  : TFRE_DB_UInt32Array;
    u64  : TFRE_DB_UInt64Array;
    r32  : TFRE_DB_Real32Array;
    r64  : TFRE_DB_Real64Array;
    ca   : TFRE_DB_CurrencyArray;
    da   : TFRE_DB_DateTimeArray;
    boa  : TFRE_DB_BoolArray;
    str  : string;
    i    : integer;

begin
  {$ASSERTIONS ON}
  sa := obj.Field(field_prefix+'STRING').AsStringArr;
  Assert(Length(sa)=4);
  cs := 'äüö ÄÜÖ ß'; Assert(sa[0] = cs);
  cs :='מדוע לא דברו עברית?' ; Assert(sa[1] = cs);
  cs := 'ག་རེ་བྱས་ཁོ་རང་ཚོས་བོད་སྐད་ཆ་དེ་ག་རང་བཤད་ཀྱི་མ་རེད།' ; assert(sa[2] = cs );
  cs := 'लोकांना मराठी का बोलता येत नाही?' ; assert(sa[3] = cs );
  assert(obj.Field(field_prefix+'STRING_NA').IsEmptyArray);

  ga := obj.Field(field_prefix+'UID').AsGUIDArr;
  assert(ga[0] = TEST_GUID_1);
  assert(ga[1] = TEST_GUID_2);
  assert(ga[2] = TEST_GUID_3);

  ba := obj.Field(field_prefix+'BYTE').AsByteArr;
  assert(ba[0] = 0);
  assert(ba[1] = 255);
  assert(ba[2] = 0);
  assert(ba[3] = 255);

  i16 := obj.Field(field_prefix+'INT16').AsInt16Arr;
  assert(i16[0] = -32768);
  assert(i16[1] =  32767);
  assert(i16[2] =      0);
  assert(i16[3] =     -1);

  i32 := obj.Field(field_prefix+'INT32').AsInt32Arr;
  assert(i32[0] = -2147483648);
  assert(i32[1] =  2147483647);
  assert(i32[2] =          0);
  assert(i32[3] =         -1);

  i64 := obj.Field(field_prefix+'INT64').AsInt64Arr;
  assert(i64[0]= -9223372036854775808);
  assert(i64[1]=  9223372036854775807);
  assert(i64[2]=                    0);
  assert(i64[3]=                   -1);

  u16 := obj.Field(field_prefix+'UINT16').AsUInt16Arr;
  assert(u16[0] =  32768);
  assert(u16[1] =  32767);
  assert(u16[2] =      0);
  assert(u16[3] =  65535);

  u32 := obj.Field(field_prefix+'UINT32').AsUInt32Arr;
  assert(u32[0] =  2147483648);
  assert(u32[1] =  2147483647);
  assert(u32[2] =           0);
  assert(u32[3] =  4294967295);

  u64 := obj.Field(field_prefix+'UINT64').AsUInt64Arr;
  assert(u64[0]=  9223372036854775808);
  assert(u64[1]=  9223372036854775807);
  assert(u64[2]=                    0);
  assert(u64[3]= 18446744073709551615);

   r32 := obj.Field(field_prefix+'REAL32').AsReal32Arr;
   assert(abs(r32[0] - 3.14159265358979323846) < 1E-7);
   assert(r32[1] = 0 );
   assert(r32[2] = 1 );
   assert(abs(r32[3] +2.2) < 1E-7);
   assert(abs(r32[4] -3.3) < 1E-7);
   assert(abs(r32[5] +4.4) < 1E-7);

   r64 := obj.Field(field_prefix+'REAL64').AsReal64Arr;
   assert(abs(r64[0] - 3.14159265358979323846) < 1E-15);
   assert(r64[1] = 0 );
   assert(r64[2] = 1 );
   assert(abs(r64[3] +2.2) < 1E-15);
   assert(abs(r64[4] -3.3) < 1E-15);
   assert(abs(r64[5] +4.4) < 1E-15);

   ca := obj.Field(field_prefix+'CURRENCY').AsCurrencyArr;
   assert(ca[0] = 122.93);
   assert(ca[1] = 100.20);
   assert(ca[2] =  33.90);

   boa := obj.Field(field_prefix+'BOOLEAN').AsBooleanArr ;
   assert(boa[0] = true);
   assert(boa[1] = false);
   assert(boa[2] = false);
   assert(boa[3] = true);

   da := obj.Field(field_prefix+'DATE').AsDateTimeUTCArr;
   assert(GFRE_DT.ToStrFOS(da[0]) = '1970-01-01 01:46:40:000');
   assert(GFRE_DT.ToStrFOS(da[1]) = '1970-01-01 04:33:20:000');
   assert(GFRE_DT.ToStrFOS(da[2]) = '1970-01-01 07:20:00:000');
   assert(GFRE_DT.ToStrFOS(da[3]) = '1970-01-01 10:06:40:000');

   da := obj.Field(field_prefix+'DATE_UTC').AsDateTimeUTCArr;
   assert(GFRE_DT.ToStrFOS(da[0]) = '1970-01-01 02:46:40:000');
   assert(GFRE_DT.ToStrFOS(da[1]) = '1970-01-01 05:33:20:000');
   assert(GFRE_DT.ToStrFOS(da[2]) = '1970-01-01 08:20:00:000');
   assert(GFRE_DT.ToStrFOS(da[3]) = '1970-01-01 11:06:40:000');

   dbs := obj.Field(field_prefix+'STREAM1').AsStream;
   dbs.Position := 0;
   str := dbs.ReadAnsiString;
   assert(str = 'THIS IS A TESTSTREAM');

   dbs := obj.Field(field_prefix+'STREAM2').AsStreamArr[0];
   dbs.Position := 0;
   for i:= 1 to 5 do begin
     assert(dbs.ReadAnsiString='THIS IS A TESTSTREAM');
   end;

   dbs := obj.Field(field_prefix+'STREAM2').AsStreamArr[1];
   dbs.Position := 0;
   for i:= 1 to 5 do begin
     assert(dbs.ReadAnsiString='THIS IS A TESTSTREAM2');
   end;
   if check_domid then
     assert(obj.DomainID=TEST_GUID_3);
end;

{ TFRE_DB_TEST_CODE_CLASS }

procedure TFRE_DB_TEST_CODE_CLASS.InternalSetup;
begin
  inherited InternalSetup;
  Field('TST_STRING').AsString := 'HULAHULA';
  Field('TST_BOOLEAN').AsInt16 := -33;
end;

class procedure TFRE_DB_TEST_CODE_CLASS.RegisterSystemScheme(const scheme: IFRE_DB_SCHEMEOBJECT);
begin
  scheme.SetParentSchemeByName('TFRE_DB_OBJECTEX');
  scheme.AddSchemeField         ('fdbft_GUID',fdbft_GUID);
  scheme.AddSchemeField         ('fdbft_Byte',fdbft_Byte);
  scheme.AddSchemeField         ('fdbft_Int16',fdbft_Int16);
  scheme.AddSchemeField         ('fdbft_UInt16',fdbft_UInt16);
  scheme.AddSchemeField         ('fdbft_Int32',fdbft_Int32);
  scheme.AddSchemeField         ('fdbft_UInt32',fdbft_UInt32);
  scheme.AddSchemeField         ('fdbft_Int64',fdbft_Int64);
  scheme.AddSchemeField         ('fdbft_UInt64',fdbft_UInt64);
  scheme.AddSchemeField         ('fdbft_Real32',fdbft_Real32);
  scheme.AddSchemeField         ('fdbft_Real64',fdbft_Real64);
  scheme.AddSchemeField         ('fdbft_Currency',fdbft_Currency);
  scheme.AddSchemeField         ('fdbft_String',fdbft_String);
  scheme.AddSchemeField         ('fdbft_Boolean',fdbft_Boolean);
  scheme.AddSchemeField         ('fdbft_DateTimeUTC',fdbft_DateTimeUTC);
  scheme.AddSchemeField         ('fdbft_Stream',fdbft_Stream);
  scheme.AddSchemeField         ('fdbft_ObjLink',fdbft_ObjLink);
  scheme.AddSchemeFieldSubscheme('dbText','TFRE_DB_TEXT');
  //scheme.AddCalcSchemeField     ('calc_string',fdbft_String,@CALC_String);
  //scheme.AddCalcSchemeField     ('calc_Uint32',fdbft_UInt32,@CALC_Uint32);
end;

procedure TFRE_DB_TEST_CODE_CLASS.CALC_Uint32(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  calc.SetAsUInt32((Field('fdbft_Byte').AsByte+1)*111);
end;

procedure TFRE_DB_TEST_CODE_CLASS.CALC_String(const calc: IFRE_DB_CALCFIELD_SETTER);
begin
  calc.SetAsString('CALC/'+Field('fdbft_Byte').AsString+':'+Field('myid').AsString);
end;

function TFRE_DB_TEST_CODE_CLASS.IMI_SimpleTest(const input: IFRE_DB_Object): IFRE_DB_Object;
begin
  Check_Test_Object('TST_',self);
end;

{ TFRE_DB_PersistanceTests }

procedure TFRE_DB_PersistanceTests.SystemConnect;
var result_code : TFRE_DB_Errortype;
begin
  FSysconn    := GFRE_DBI.NewSysOnlyConnection;
  result_code := FSysconn.Connect('admin@system','admin');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  FSysconn.DumpSystem;
  //GFRE_DB_DEFAULT_PS_LAYER.SyncSnapshot;
  //GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('SYSTEM');
  //FSysconn.Finalize;
  //FSysconn    := GFRE_DBI.NewSysOnlyConnection;
  //result_code := FSysconn.Connect('admin@system','admin');
  //AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  //FSysconn.DumpSystem;
  //halt;
end;

procedure TFRE_DB_PersistanceTests.SystemAdduser;
var result_code : TFRE_DB_Errortype;
begin
  FSysconn    := GFRE_DBI.NewSysOnlyConnection;
  result_code := FSysconn.Connect('admin@system','admin');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  result_code := FSysconn.AddUser('test1@system','test1','Egon','Semmerl');
  AssertTrue('SYS ADDUSER FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  FSysconn.DumpSystem;
end;

procedure TFRE_DB_PersistanceTests.SystemCheckUser;
var result_code : TFRE_DB_Errortype;
begin
  FSysconn    := GFRE_DBI.NewSysOnlyConnection;
  result_code := FSysconn.Connect('admin@system','admin');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  result_code := FSysconn.AddUser('test1@system','test1','Egon','Semmerl');
  AssertTrue('SYS ADDUSER FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_EXISTS);
  FSysconn.DumpSystem;
end;

procedure TFRE_DB_PersistanceTests.SystemSyncSnapshot;
begin
  GFRE_DB_DEFAULT_PS_LAYER.SyncSnapshot;
end;

procedure TFRE_DB_PersistanceTests.SetupTestWorkDB;
var res : TFRE_DB_Errortype;
begin
  res := GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('WORKTEST');
  AssertTrue('CREATE DB FAILED '+CFRE_DB_Errortype[res],res=edb_OK);
end;

procedure TFRE_DB_PersistanceTests.SetupTestCollections;
var coll_v,coll_p : IFRE_DB_COLLECTION;
begin
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('SYSTEM',true);
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('WORKTEST');
  ConnectDB('test1@system','test1');
  coll_v := FWorkConn.Collection('TEST_1_VOL',true,true);
  coll_p := FWorkConn.Collection('TEST_1_PERS',true,false);
  coll_v := FWorkConn.Collection('TEST_1_VOL_U',true,true);   // unique
  coll_p := FWorkConn.Collection('TEST_1_PERS_U',true,false); // unique
  coll_p := FWorkConn.Collection('REFTEST',true,false); // unique
end;

procedure TFRE_DB_PersistanceTests.FetchTestColletion;
var coll_v,coll_p : IFRE_DB_COLLECTION;
begin
  ConnectDB('test1@system','test1');
  coll_v := FWorkConn.Collection('TEST_1_VOL',false,true);
  coll_p := FWorkConn.Collection('TEST_1_PERS',false,false);
  AssertNotNull(coll_v);
  AssertNotNull(coll_p);
  FWorkConn.DeleteCollection('TEST_1_VOL');
  FWorkConn.DeleteCollection('TEST_1_PERS');
  coll_v := FWorkConn.Collection('TEST_1_VOL',false,true);
  AssertNull(coll_v);
  coll_p := FWorkConn.Collection('TEST_1_PERS',false,false);
  AssertNull(coll_p);
  coll_v := FWorkConn.Collection('TEST_1_VOL',true,true);
  coll_p := FWorkConn.Collection('TEST_1_PERS',true,false);
  AssertNotNull(coll_v);
  AssertNotNull(coll_p);
end;

procedure TFRE_DB_PersistanceTests.DoReflinkTests;
var coll_p    : IFRE_DB_COLLECTION;
    n1,n2,n3  : IFRE_DB_Object;
    u1,u2,u3  : TGuid;
    ra        : TFRE_DB_GUIDArray;
    obr       : TFRE_DB_ObjectReferences;

begin
  ConnectDB('admin@system','admin');
  coll_p := FWorkConn.Collection('REFTEST',false,true);
  AssertTrue(assigned(coll_p));

  n1 := GFRE_DBI.NewObject;
  n2 := GFRE_DBI.NewObject;
  n3 := GFRE_DBI.NewObject;
  u1 := n1.UID;
  u2 := n2.UID;
  u3 := n3.UID;

  n1.Field('ID').AsString:='FIRST';
  CheckDbResultColl(coll_p.Store(n1),coll_p);
  n2.Field('ID').AsString:='SECOND';
  n2.Field('LINK').AsObjectLink := u1; // Points to FIRST
  CheckDbResultColl(coll_p.Store(n2),coll_p);
  n3.Field('ID').AsString:='THIRD';
  n3.Field('LINK').AsObjectLink := u2; // Points to SECOND
  CheckDbResultColl(coll_p.Store(n3),coll_p);

  ra  := FWorkConn.GetReferences(u1,true,'');
  AssertTrue(length(ra)=0);
  obr := FWorkConn.GetReferences(u1,true);
  AssertTrue(length(obr)=0);

  ra := FWorkConn.GetReferences(u1,false,'');
  AssertTrue(length(ra)=1);
  AssertTrue(ra[0]=u2);
  obr := FWorkConn.GetReferences(u1,false);
  AssertTrue(length(obr)=1);

  ra := FWorkConn.GetReferences(u2,true,'');
  AssertTrue(length(ra)=1);
  AssertTrue(ra[0]=u1);
  obr := FWorkConn.GetReferences(u2,true);
  AssertTrue(length(obr)=1);


  ra := FWorkConn.GetReferences(u2,false,'');
  AssertTrue(length(ra)=1);
  AssertTrue(ra[0]=u3);



  //assert(ra.);

end;

procedure TFRE_DB_PersistanceTests.DefineIndices;
var coll_v,coll_p   : IFRE_DB_COLLECTION;
    coll_vu,coll_pu : IFRE_DB_COLLECTION;
  procedure DefineTestIndices(const coll : IFRE_DB_COLLECTION ; const unique : boolean);
  begin
    CheckDbResult(coll.DefineIndexOnField('fdbft_String',fdbft_String,unique,false,'ixs'),'error creating ixs');
    CheckDbResult(coll.DefineIndexOnField('fdbft_UInt64',fdbft_UInt64,unique,false,'ixui64'),'error creating ixu64');
    CheckDbResult(coll.DefineIndexOnField('fdbft_UInt32',fdbft_UInt32,unique,false,'ixui32'),'error creating ixu32');
    CheckDbResult(coll.DefineIndexOnField('fdbft_UInt16',fdbft_UInt16,unique,false,'ixui16'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Byte',fdbft_Byte,unique,false,'ixb'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Boolean',fdbft_Boolean,unique,false,'ixbo'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Guid',fdbft_GUID,unique,false,'ixu'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_ObjLink',fdbft_ObjLink,unique,false,'ixol'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Int64',fdbft_Int64,unique,false,'ixi64'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Int32',fdbft_Int32,unique,false,'ixi32'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Int16',fdbft_Int16,unique,false,'ixi16'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_DateTimeUTC',fdbft_DateTimeUTC,unique,false,'ixdt'),'error creating ixu');
    CheckDbResult(coll.DefineIndexOnField('fdbft_Currency',fdbft_Currency,unique,false,'ixc'),'error creating ixu');

    //CheckDbResult(coll.DefineIndexOnField('fdbft_Real32',fdbft_Real32,unique,false,'ixr32'),'error creating ixu');
    //CheckDbResult(coll.DefineIndexOnField('fdbft_Real64',fdbft_Real64,unique,false,'ixur32'),'error creating ixu');
    //CheckDbResult(coll.DefineIndexOnField('fdbft_Stream',fdbft_Stream,unique,false,'ixst'),'error creating ixu');
    //CheckDbResult(coll.DefineIndexOnField('fdbft_Object',fdbft_Object,unique,false,'ixo'),'error creating ixu');
  end;

begin
  ConnectDB('test1@system','test1');
  coll_v  := FWorkConn.Collection('TEST_1_VOL',false,true);
  coll_p  := FWorkConn.Collection('TEST_1_PERS',false,false);
  coll_vu := FWorkConn.Collection('TEST_1_VOL_U',false,true);
  coll_pu := FWorkConn.Collection('TEST_1_PERS_U',false,false);
  DefineTestIndices(coll_v,false);
  DefineTestIndices(coll_p,false);
  DefineTestIndices(coll_vu,True);
  DefineTestIndices(coll_pu,True);
end;

procedure TFRE_DB_PersistanceTests.GenerateIndexTestData;
var coll_v,coll_p   : IFRE_DB_COLLECTION;
    coll_vu,coll_pu : IFRE_DB_COLLECTION;
    coll_link       : IFRE_DB_COLLECTION;
    obj             : IFRE_DB_Object;
    guid            : TGUID;
    inserts         : array [0..12] of string   = ('a','aa','b','aaa','aaaa','bar','baz','bazaaar','bazaaaroni','a','b','bar','aa');
    u64inserts      : array [0..12] of Qword    = (800000000,10,1234,1,2,77,99,800000000,-1,10,11,21,-2);
    i64inserts      : array [0..12] of Int64    = (-800000000,-10,-20,0,11,22,99,800000000,-1,10,34,33,-2);
    u32inserts      : array [0..12] of Cardinal = (800000000,10,1234,1,2,77,99,800000000,-1,10,11,21,-2);
    u16inserts      : array [0..12] of Word     = (800000000,10,1234,1,2,77,99,800000000,-1,10,11,21,-2);
    bytinserts      : array [0..12] of Byte     = (800000000,10,1234,1,2,77,99,800000000,-1,10,11,21,-2);
    curinserts      : array [0..12] of Currency = (-800000000.1234,-10.5678,-20.6789,0,1.23456789,22.22233,99.3332,800000000.213123,-1.123123,10.2333,11.333,333.33,-2.33);
    booinserts      : array [0..12] of Boolean  = (true,false,true,false,false,true,true,true,false,true,false,true,false);

  procedure WriteObjectIdx(const obj : IFRE_DB_Object ; var halt : boolean);
  begin
    WriteObject(obj);
  end;

  procedure GendataforColl(const coll:IFRE_DB_COLLECTION;const genlinkobjs : boolean);
  var i : integer;
  begin
    if genlinkobjs then
      begin
        guid := CFRE_DB_NullGUID;
        for i:= 0 to high(inserts) do
          begin
           TGUID_Access(guid).Part2 := i;
           obj := GFRE_DBI.NewObject;
           obj.Field('myid').AsUInt32 := 100;
           obj.Field('uid').AsGUID:=guid;
           CheckDbResultColl(coll_link.Store(obj),coll_link,'Gentestdata: ');
          end;
      end;

    guid := CFRE_DB_NullGUID;

    obj := GFRE_DBI.NewObject;
    obj.Field('myid').AsUInt32 := 0;
    obj.Field('fdbft_String').AsString      := '';
    obj.Field('fdbft_Int64').AsInt64        := 0;
    obj.Field('fdbft_UInt64').AsUInt64      := 0;
    obj.Field('fdbft_UInt32').AsUInt32      := 0;
    obj.Field('fdbft_UInt16').AsUInt16      := 0;
    obj.Field('fdbft_Int32').AsInt32        := 0;
    obj.Field('fdbft_Int16').AsInt16        := 0;
    obj.Field('fdbft_Byte').AsByte          := 0;
    obj.Field('fdbft_Currency').AsCurrency  := 0;
    obj.Field('fdbft_DateTimeUTC').AsDateTimeUTC := 0;
    obj.Field('fdbft_Boolean').AsBoolean    := false;
    obj.Field('fdbft_GUID').AsGUID          := CFRE_DB_NullGUID;
    if not coll.IsVolatile then
      obj.Field('fdbft_ObjLink').AsObjectLink := CFRE_DB_NullGUID;
    coll.Store(obj);
    obj := GFRE_DBI.NewObject;
    obj.Field('myid').AsUInt32 := 1;
    coll.Store(obj);
    obj := GFRE_DBI.NewObject;
    obj.Field('myid').AsUInt32 := 2;
    coll.Store(obj);
    obj := GFRE_DBI.NewObject;
    obj.Field('myid').AsUInt32 := 3;
    obj.Field('fdbft_String').AsString     := '';
    obj.Field('fdbft_Int64').AsInt64       := 0;
    obj.Field('fdbft_UInt64').AsUInt64     := 0;
    obj.Field('fdbft_UInt32').AsUInt32     := 0;
    obj.Field('fdbft_UInt16').AsUInt16     := 0;
    obj.Field('fdbft_Int32').AsInt32       := 0;
    obj.Field('fdbft_Int16').AsInt16       := 0;
    obj.Field('fdbft_Byte').AsByte         := 0;
    obj.Field('fdbft_Boolean').AsBoolean   := false;
    obj.Field('fdbft_GUID').AsGUID         := CFRE_DB_NullGUID;
    obj.Field('fdbft_DateTimeUTC').AsDateTimeUTC := -1;
    if not coll.IsVolatile then
      obj.Field('fdbft_ObjLink').AsObjectLink := CFRE_DB_NullGUID;
    coll.Store(obj);
    for i := high(inserts) downto 0 do
      begin
        TGUID_Access(guid).Part2 := i;
        obj := GFRE_DBI.NewObject;
        obj.Field('myid').AsUInt32 := 10+i;
        obj.Field('fdbft_String').AsString   := inserts[i];
        obj.Field('fdbft_UInt64').AsUInt64   := u64inserts[i];
        obj.Field('fdbft_UInt32').AsUInt32   := u32inserts[i];
        obj.Field('fdbft_UInt16').AsUInt16   := u16inserts[i];
        obj.Field('fdbft_Byte').AsByte       := bytinserts[i];
        obj.Field('fdbft_Boolean').AsBoolean := booinserts[i];
        obj.Field('fdbft_Int64').AsInt64     := i64inserts[i];
        obj.Field('fdbft_Int32').AsInt32     := i64inserts[i];
        obj.Field('fdbft_Int16').AsInt16     := i64inserts[i];
        obj.Field('fdbft_Currency').AsCurrency  := curinserts[i];
        obj.Field('fdbft_GUID').AsGUID       := guid;
        sleep(1);
        obj.Field('fdbft_DateTimeUTC').AsDateTimeUTC := GFRE_DT.Now_UTC;
        if not coll.IsVolatile then
          obj.Field('fdbft_ObjLink').AsObjectLink := guid;
        coll.Store(obj);
      end;
  end;

  procedure DumpColl(const coll:IFRE_DB_COLLECTION;const idxname :TFRE_DB_NameType);
  var hlt : boolean;
  begin
    writeln('----');
    writeln('<<ORDER DUMP ',coll.CollectionName,' ',idxname,' ASC');
    hlt := false;
    coll.ForAllIndexed(@WriteObjectIdx,hlt,idxname,true);
    writeln('');
    writeln('<<ORDER DUMP ',coll.CollectionName,' ',idxname,' DESC');
    hlt := false;
    coll.ForAllIndexed(@WriteObjectIdx,hlt,idxname,false);
    writeln('----');
  end;

begin
   //ConnectDB('test1@system','test1');
   ConnectDB('admin@system','admin'); //TODO: Setup Rights so that testuser can create index test data
   coll_v    := FWorkConn.Collection('TEST_1_VOL',false,true);
   coll_p    := FWorkConn.Collection('TEST_1_PERS',false,false);
   coll_vu   := FWorkConn.Collection('TEST_1_VOL_U',false,true);
   coll_pu   := FWorkConn.Collection('TEST_1_PERS_U',false,false);
   coll_link := FWorkConn.Collection('TEST_1_LINKO',true,false);
   GendataforColl(coll_p,true);
   GendataforColl(coll_v,false);
   //GendataforColl(coll_pu,false);
   //GendataforColl(coll_vu,false);
   //DumpColl(coll_p,'ixs');
   //DumpColl(coll_p,'ixui64');
   //DumpColl(coll_p,'ixui32');
   //DumpColl(coll_p,'ixui16');
   DumpColl(coll_p,'ixb');
   //DumpColl(coll_p,'ixbo');
   //DumpColl(coll_p,'ixu');
   //DumpColl(coll_p,'ixol');
   //DumpColl(coll_p,'ixi64');
   //DumpColl(coll_p,'ixi32');
   //DumpColl(coll_p,'ixi16');
   //DumpColl(coll_p,'ixdt');
   //DumpColl(coll_p,'ixc');
end;

procedure TFRE_DB_PersistanceTests.TestIdxRangeQueries;
var coll_v,coll_p   : IFRE_DB_COLLECTION;
    coll_vu,coll_pu : IFRE_DB_COLLECTION;
    coll_link       : IFRE_DB_COLLECTION;
    obj             : IFRE_DB_Object;
    hlt             : boolean;

  procedure WriteObjectIdx(const obj : IFRE_DB_Object ; var halt:boolean);
  begin
    WriteObject(obj);
  end;

begin
  exit;
  ConnectDB('test1@system','test1');
  coll_v    := FWorkConn.Collection('TEST_1_VOL',false,true);
  coll_p    := FWorkConn.Collection('TEST_1_PERS',false,false);
  coll_vu   := FWorkConn.Collection('TEST_1_VOL_U',false,true);
  coll_pu   := FWorkConn.Collection('TEST_1_PERS_U',false,false);

  assert(assigned(coll_v));
  assert(assigned(coll_p));
  assert(assigned(coll_vu));
  assert(assigned(coll_pu));

  //writeln('--RANGE QUERY TEST---');
  //coll_p.ForAllIndexedSignedRange(-11,1000,@WriteObjectIdx,'ixi64');
  //writeln('--RANGE QUERY TEST--- END');
  writeln('--RANGE QUERY TEST---');
  writeln('-30 -> 33');
  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64');
  writeln('-30 -> MAX');
  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64',true,false,true);
  writeln('NULL -> 30');
  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64',true,true,false);
  writeln('UNSIGNED 1->1300');
  hlt := false;
  coll_p.ForAllIndexedUnsignedRange(1,1300,@WriteObjectIdx,hlt,'ixui64',true,false,false,4,3);
  writeln('--RANGE QUERY TEST--- END');

  writeln('--REVERSE RANGE QUERY TEST---');
  writeln('-30 -> 30');

  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64',false);
  writeln('-30 -> MAX');
  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64',false,false,true);
  writeln('NULL -> 30');
  hlt := false;
  coll_p.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,hlt,'ixi64',false,true,false,4,3);
  writeln('--REVERSE RANGE QUERY TEST--- END');

  writeln('--STRING RANGE QUERY--');
  hlt := false;
  coll_p.ForAllIndexedStringRange('a','b',@WriteObjectIdx,hlt,'ixs');
  writeln('--STRING RANGE QUERY-- END');

  writeln('--STRING PREFIX QUERY--');
  hlt := false;
  coll_p.ForAllIndexPrefixString('ba',@WriteObjectIdx,hlt,'ixs');
  writeln('--STRING PREFIX QUERY-- END');

end;

procedure TFRE_DB_PersistanceTests.TestIdxUpdate;
var coll_v,coll_p   : IFRE_DB_COLLECTION;
    coll_vu,coll_pu : IFRE_DB_COLLECTION;
    coll_link       : IFRE_DB_COLLECTION;
    obj             : IFRE_DB_Object;
    hlt             : boolean;

  procedure WriteObjectIdx(const obj : IFRE_DB_Object ; var halt : boolean);
  begin
    WriteObject(obj);
  end;

  procedure UpdateObjectIdx(const obj : IFRE_DB_Object ; var halt : boolean);
  begin
    //obj.Field('fdbft_String').AsString    := 'Updated';
    //obj.Field('fdbft_UInt64').AsUInt64    := 12345678;
    //obj.Field('fdbft_UInt32').AsUInt32    := 12345;
    //obj.Field('fdbft_UInt16').AsUInt16    := 123;
    //obj.Field('fdbft_Int64').AsInt64      := -123456;
    //obj.Field('fdbft_Int32').AsInt32      := -12345;
    //obj.Field('fdbft_Int16').AsInt16      := -123;
    //obj.Field('fdbft_Boolean').AsBoolean  := true;
    //obj.Field('fdbft_GUID').AsGUID        := TEST_GUID_3;
    //obj.Field('fdbft_DateTimeUTC').AsDateTimeUTC := 120000;

    //writeln('UPDATE ',obj.UID_String,' ',obj.Field('fdbft_Byte').AsString);
    obj.Field('fdbft_Byte').AsByte        := 12;
    //obj.DeleteField('fdbft_Byte');
    coll_p.Update(obj);
    //coll_p.Remove(obj.UID);
    //halt := true;
  end;

begin
  //exit;
  ConnectDB('admin@system','admin');
  coll_v    := FWorkConn.Collection('TEST_1_VOL',false,true);
  coll_p    := FWorkConn.Collection('TEST_1_PERS',false,false);
  coll_vu   := FWorkConn.Collection('TEST_1_VOL_U',false,true);
  coll_pu   := FWorkConn.Collection('TEST_1_PERS_U',false,false);

  writeln('--- INDEX UPDATE TEST UNSIGNED ---');
  hlt := false;
  coll_p.ForAllIndexedUnsignedRange(0,30,@UpdateObjectIdx,hlt,'ixb',false,true,true,1,0,true);
  hlt := false;
  coll_p.ForAllIndexedUnsignedRange(0,30,@WriteObjectIdx,hlt,'ixb',true,true,true,1,0,true);
  writeln('--- INDEX UPDATE TEST UNSIGNED --- END');
  hlt:=false;
  writeln('--ALLL---');
  coll_p.ForAllBreak(@WriteObjectIdx,hlt);
  writeln('--ALLL---INDEXED');
  coll_p.ForAllIndexed(@WriteObjectIdx,hlt,'ixb',true);
  writeln('--ALLL---END');


  //writeln('--- INDEX UPDATE TEST SIGNED ---');
  //coll_pu.ForAllIndexedSignedRange(-30,30,@WriteObjectIdx,'ixi64',true,true,true,3);
  //writeln('--- INDEX UPDATE TEST SIGNED --- END');
end;

procedure TFRE_DB_PersistanceTests.ReconnectNotSyncedFromWAL;
begin
  writeln('***** ------------------------------------------------------');
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('SYSTEM',true);
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('WORKTEST');
  ConnectDB('test1@system','test1');
end;


procedure TFRE_DB_PersistanceTests.DumpDatabase;
begin
end;

procedure TFRE_DB_PersistanceTests.ConnectDB(const user, pw: string);
begin
  FWorkConn := GFRE_DB.NewConnection;
  CheckDbResult(FWorkConn.Connect('WORKTEST',user,pw),'connect failed');
end;

procedure TFRE_DB_PersistanceTests.WriteObject(const obj: IFRE_DB_Object);
var boo,byt,ui16,ui32,ui64,s,gs,ol,i64,i32,i16,dt: string;
    cu : string;
begin
  //if obj.FieldExists('fdbft_Currency') then
  //  x := obj.Field('fdbft_Currency').AsInt64
  //else
  //  x := 0;
  if obj.FieldExists('fdbft_Currency') then
    cu := obj.Field('fdbft_Currency').AsString
  else
    cu := '[]';
  if obj.FieldExists('fdbft_DateTimeUTC') then
    dt := '['+GFRE_DT.ToStrFOS(obj.Field('fdbft_DateTimeUTC').AsDateTime)+']'
  else
    dt := '[]';
  if obj.FieldExists('fdbft_Int32') then
    i32 := '['+obj.Field('fdbft_Int32').AsString+']'
  else
    i32 := '[]';
  if obj.FieldExists('fdbft_Int16') then
    i16 := '['+obj.Field('fdbft_Int16').AsString+']'
  else
    i16 := '[]';
  if obj.FieldExists('fdbft_Int64') then
    i64 := '['+obj.Field('fdbft_Int64').AsString+']'
  else
    i64 := '[]';
  if obj.FieldExists('fdbft_GUID') then
    gs := '['+obj.Field('fdbft_GUID').AsString+']'
  else
    gs := '[]';
  if obj.FieldExists('fdbft_ObjLink') then
    ol :=  obj.Field('fdbft_ObjLink').AsString
  else
    ol := '[]';
  if obj.FieldExists('fdbft_Boolean') then
    boo := '['+obj.Field('fdbft_Boolean').AsString+']'
  else
    boo := '[]';
  if obj.FieldExists('fdbft_Byte') then
    byt := '['+obj.Field('fdbft_Byte').AsString+']'
  else
    byt := '[]';
  if obj.FieldExists('fdbft_UInt16') then
    ui16 := '['+obj.Field('fdbft_UInt16').AsString+']'
  else
    ui16 := '[]';
  if obj.FieldExists('fdbft_UInt32') then
    ui32 := '['+obj.Field('fdbft_UInt32').AsString+']'
  else
    ui32 := '[]';
  if obj.FieldExists('fdbft_UInt64') then
    ui64 := '['+obj.Field('fdbft_UInt64').AsString+']'
  else
    ui64 := '[]';
  if obj.FieldExists('fdbft_String') then
    s := '['+obj.Field('fdbft_String').AsString+']'
  else
    s := '[]';
  writeln(obj.field('myid').AsString:2,'S:',s:12,' U64:',ui64:22,' U32:',ui32:12,' U16:',ui16:8,' B:',byt:6,' BOOL:',boo:4,' G/OBL:',gs:34,' I64: ',i64:12,' I32:',i32:12,' I16:',i16:7,dt:26,' CURR:',cu:16,' |',obj.UID_String);
end;


procedure TFRE_DB_PersistanceTests.PreCleanup;
begin
  GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase('TEST_/|\%DB%''');
  GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase('SYSTEM');
  GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase('WORKTEST');
end;

procedure TFRE_DB_PersistanceTests.CreateTestDatabase;
var res : TFRE_DB_Errortype;
begin
  res := GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('TEST_/|\%DB%''');
  AssertTrue('CREATE DB FAILED '+CFRE_DB_Errortype[res],res=edb_OK);
end;

procedure TFRE_DB_PersistanceTests.LayerListDatabases;
var idx : integer;
begin
  AssertTrue(GFRE_DB_DEFAULT_PS_LAYER.DatabaseList.Find('TEST_/|\%DB%''',idx));
end;

procedure TFRE_DB_PersistanceTests.DropTestDatabase;
var res : TFRE_DB_Errortype;
begin
  res := GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase('TEST_/|\%DB%''');
  AssertTrue(res=edb_OK);
end;

procedure TFRE_DB_PersistanceTests.SystemCreate;
var res : TFRE_DB_Errortype;
begin
  res := GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('SYSTEM');
end;

procedure TFRE_DB_PersistanceTests.SystemWrongUser;
var result_code : TFRE_DB_Errortype;
begin
  FSysconn    := GFRE_DBI.NewSysOnlyConnection;
  result_code := FSysconn.Connect('wrong@system','wrong');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_NOT_FOUND);
  result_code := FSysconn.Connect('admin@system','wrong');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_ACCESS);
  result_code := FSysconn.Connect('admin@system','admin');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
  result_code := FSysconn.Connect('guest@system','');
  AssertTrue('SYS CONNECT FAILED: '+CFRE_DB_Errortype[result_code],result_code = edb_OK);
end;

{ TFRE_DB_ObjectTests }

procedure TFRE_DB_ObjectTests.SetUp;
begin
  TestObject := GFRE_DB.NewObject;
end;

procedure TFRE_DB_ObjectTests.TearDown;
begin
  FreeAndNil(TestObject);
end;

procedure TFRE_DB_ObjectTests.CheckCodePage;
begin
  AssertTrue(DefaultSystemCodePage=CP_UTF8);
end;

procedure TFRE_DB_ObjectTests.Dirty;
begin
  //AssertEquals(True,TestObject.IsDirty);
end;

procedure TFRE_DB_ObjectTests.FieldCount;
begin
  AssertEquals(2,TestObject.FieldCount(false)); // UID / DOMAINID
end;

procedure TFRE_DB_ObjectTests.UID_NotNull;
begin
  AssertTrue(not FREDB_Guids_Same(TestObject.UID,CFRE_DB_NullGUID));
end;

procedure TFRE_DB_ObjectTests.UID_Exists;
begin
  AssertTrue(TestObject.FieldExists('uId'));
end;

procedure TFRE_DB_ObjectTests.UID_Delete;
begin
  try
    TestObject.DeleteField('UID');
  except
    exit;
  end;
  Fail('UID Field was deleted');
end;

procedure TFRE_DB_ObjectTests.UID_Delete2;
begin
  TestObject.ClearAllFields;
  AssertTrue('UID Field was cleared',TestObject.FieldExists('uid'));
end;

procedure TFRE_DB_ObjectTests.UID_Path_Simple;
var sa : TFRE_DB_StringArray;
begin
  sa := TestObject.GetUIDPath;
  AssertTrue(length(sa)=1);
  AssertTrue(sa[0]=TestObject.UID_String);
end;

procedure TFRE_DB_ObjectTests.UID_String;
begin
  AssertTrue(TestObject.UID_String=TestObject.Field('uid').AsString);
end;

procedure TFRE_DB_ObjectTests.UID_Set;
begin
  TestObject.Field('UID').AsGUID:=TEST_GUID_1;
  AssertTrue(TestObject.UID=TEST_GUID_1);
end;

procedure TFRE_DB_ObjectTests.DOMAIND_Exists;
begin
  AssertTrue(TestObject.FieldExists('doMainId'));
end;

procedure TFRE_DB_ObjectTests.DOMAIND_Delete;
begin
  try
    TestObject.DeleteField('DomaiNid');
  except
    exit;
  end;
  Fail('DomainID Field was deleted');
end;

procedure TFRE_DB_ObjectTests.DOMAIND_Delete2;
begin
  TestObject.ClearAllFields;
  AssertTrue('DomainID Field was cleared',TestObject.FieldExists('domainid'));
end;


procedure TFRE_DB_ObjectTests.DOMAIND_Set;
begin
  TestObject.Field('DoMaInId').AsGUID:=TEST_GUID_2;
  AssertTrue(TestObject.DomainID=TEST_GUID_2);
end;

procedure TFRE_DB_ObjectTests.Mediator_Empty;
begin
  AssertNull(TestObject.Mediator);
end;

procedure TFRE_DB_ObjectTests.ObjectProperties;
begin
  AssertTrue(TestObject.Properties = []);
end;

procedure TFRE_DB_ObjectTests.FieldTypes;
begin
  Fill_Test_Object('TST_',TestObject);
  Check_Test_Object('TST_',TestObject);
end;

procedure TFRE_DB_ObjectTests.DumpTest;
begin
  Fill_Test_Object('TST_',TestObject);
  Fill_Test_Object('SUB_TST',TestObject.Field('SUB_OBJECT').AsObject);
  writeln('-----STRING DUMP----');
  writeln(TestObject.DumpToString);
  writeln('-----STRING DUMP END----');
end;

procedure TFRE_DB_ObjectTests.DumpJSONTestFull;
begin
  Fill_Test_Object('TST_',TestObject);
  Fill_Test_Object('SUB_TST',TestObject.Field('SUB_OBJECT').AsObject);
  writeln('-----JSON STRING DUMP----');
  writeln(TestObject.GetAsJSONString(false,true,nil));
  writeln('-----JSON STRING DUMP END----');
end;

procedure TFRE_DB_ObjectTests.SetNullArray;
var sa   : TFRE_DB_StringArray;
    obj2 : TFRE_DB_Object;
begin
  setlength(sa,0);
  TestObject.Field('TST').AsStringArr := sa;
  AssertTrue(TestObject.FieldExists('TST'));
  AssertTrue(TestObject.Field('TST').ValueCount=0);
  obj2 := TestObject.CloneToNewObject();
  AssertTrue(obj2.FieldExists('TST'));
  AssertTrue(obj2.Field('TST').ValueCount=0);
end;

procedure TFRE_DB_ObjectTests.StreamTest;
var Object2 : TFRE_DB_Object;
    Object3 : TFRE_DB_Object;
    Object4 : TFRE_DB_Object;
    len     : integer;
    data    : RawByteString;
    mp      : Pointer;
begin
  Fill_Test_Object('TST_',TestObject);
  writeln(TestObject.DumpToString);
  len:=TestObject.NeededSize;
  writeln('Streamingsize =========== ',len);
  SetLength(data,len);
  mp:=@data[1];
  TestObject.CopyToMemory(mp);
  writeln('********************* STREAMED ***********************');
  mp:=@data[1];
  Object2:=TFRE_DB_Object.CreateFromMemory(mp);
  Check_Test_Object('TST_',TestObject);
  Check_Test_Object('TST_',Object2);
  writeln(Object2.dumpToString);
  writeln(GFRE_BT.GUID_2_HexString(Object2.UID));

  Object3 := Object2.CloneToNewObject();
  Object4 := Object3.CloneToNewObject();

  Check_Test_Object('TST_',Object3,true);
  Check_Test_Object('TST_',Object4,true);

  Object2.free;
end;

procedure TFRE_DB_ObjectTests.StreamTest2;
var Object1 : TFRE_DB_TEST_CODE_CLASS;
    Object2 : IFRE_DB_Object;
    Object3 : TFRE_DB_TEST_CODE_CLASS;
    Object4 : IFRE_DB_Object;
    len     : integer;
    data    : RawByteString;
    mp      : Pointer;
    i: Integer;
begin
  Object1 := TFRE_DB_TEST_CODE_CLASS.CreateForDB;
  Fill_Test_Object('TST_',Object1);
  writeln(Object1.DumpToString);
  len:=Object1.NeededSize;
  writeln('Streamingsize =========== ',len);
  SetLength(data,len);
  mp:=@data[1];
  Object1.CopyToMemory(mp);
  writeln('********************* STREAMED ***********************');

  mp:=@data[1];
  Object2:=TFRE_DB_Object.CreateFromMemory(mp);
  Check_Test_Object('TST_',Object2);
  writeln(Object2.dumpToString);
  writeln(GFRE_BT.GUID_2_HexString(Object2.UID));
  //Object2.Finalize;
  for i := 0 to 10 do
    begin
      Object3 := Object2.CloneToNewObject().Implementor_HC as TFRE_DB_TEST_CODE_CLASS;
      Object4 := Object3.CloneToNewObject();
      Check_Test_Object('TST_',Object3);
      Check_Test_Object('TST_',Object4);
      Object4.IsA('TFRE_DB_TEST_CODE_CLASS');
      assert(Object4.Implementor_HC.ClassName = 'TFRE_DB_TEST_CODE_CLASS');
      Check_Test_Object('TST_',Object3);
      Check_Test_Object('TST_',Object4);
      Object3.Finalize;
      Object4.Finalize;
    end;
  Object2.Finalize;
  Object1.Finalize;
end;

procedure TFRE_DB_ObjectTests.CloneToNewChangeGUIDS;
var Object1 : TFRE_DB_TEST_CODE_CLASS;
    Object2 : IFRE_DB_Object;
begin
  writeln('--- Clone2New---');
  Object1 := TFRE_DB_TEST_CODE_CLASS.CreateForDB;
  Fill_Test_Object('TST_',Object1);
  Object1.Field('SUBOBJECT').AsObject := TFRE_DB_TEST_CODE_CLASS.CreateForDB;
  writeln(Object1.DumpToString);
  Object2 := Object1.CloneToNewObject(true);
  writeln('----');
  writeln(Object2.DumpToString);
  writeln('--- Clone2New---END');
end;

procedure TFRE_DB_ObjectTests.GenericChangelistTest;
var obj1,obj2 : TFRE_DB_Object;
    d1,d2     : TFRE_DB_Object;

  procedure Insert(const o : IFRE_DB_Object);
  begin
    writeln('INSERT STEP : ',o.UID_String,' ',o.SchemeClass,' ',BoolToStr(o.IsObjectRoot,' ROOT OBJECT ',' CHILD OBJECT '));
    writeln(o.DumpToString(2));
  end;

  procedure Delete(const o : IFRE_DB_Object);
    function  _ParentFieldnameIfExists:String;
    begin
      if not o.IsObjectRoot then
        result := o.ParentField.FieldName
      else
        result := '';
    end;

  begin
    writeln('DELETE STEP : ',o.UID_String,' ',o.SchemeClass,BoolToStr(o.IsObjectRoot,' ROOT OBJECT ',' CHILD OBJECT '),_ParentFieldnameIfExists);
    writeln(o.DumpToString(2));
  end;

  procedure Update(const is_child_update : boolean ; const update_obj : IFRE_DB_Object ; const update_type :TFRE_DB_ObjCompareEventType  ;const new_field, old_field: IFRE_DB_Field);
  var nfn,nft,ofn,oft,updt,ofv,nfv : TFRE_DB_NameType;
  begin
    if assigned(new_field) then
      begin
        nfn := new_field.FieldName;
        nft := new_field.FieldTypeAsString;
        if new_field.IsEmptyArray then
          nfv := '(empty array)'
        else
          nfv := new_field.AsString;
      end;
    if assigned(old_field) then
      begin
        ofn := old_field.FieldName;
        oft := old_field.FieldTypeAsString;
        if old_field.IsEmptyArray then
          ofv := '(empty array)'
        else
          ofv := old_field.AsString;
      end;
    case update_type of
      cev_FieldDeleted: updt := 'DELETE FIELD '+nfn+'('+nft+')';
      cev_FieldAdded:   updt := 'ADD FIELD '+nfn+'('+nft+')';
      cev_FieldChanged: updt := 'CHANGE FIELD : '+nfn+' FROM '+ofv+':'+oft+' TO '+nfv+':'+nft;
    end;
    writeln('UPDATE STEP : ',BoolToStr(is_child_update,' CHILD UPDATE ',' ROOT UPDATE '), update_obj.UID_String,' ',update_obj.SchemeClass,' '+updt);
  end;

begin
  obj1 := GFRE_DB.NewObject;
  Fill_Test_Object('chang_',obj1);
  obj2 := obj1.CloneToNewObject();
  Fill_Test_Object('chung_',obj2);
  Fill_Test_Object('sub',obj1.Field('newo').AsObject);
  Fill_Test_Object('subsub',obj1.Field('newo').AsObject.Field('newnewo').AsObject);

  Fill_Test_Object('old',obj2.Field('oldo').AsObject);

  writeln('-----');
  writeln('-----');
  writeln('COMPARE - chang-chung');
  GFRE_DBI.GenerateAnObjChangeList(obj1,obj2,@Insert,@Delete,@Update);
  writeln('------');
end;

procedure TFRE_DB_ObjectTests.TestStreamFieldClone;
var obj1,obj2 : TFRE_DB_Object;
begin
   obj1 := GFRE_DB.NewObject;
   obj2 := GFRE_DB.NewObject;
   obj1.Field('TESTSTREAM').AsStream.SetFromRawByteString('TeStStream#091'+#254+'A');
   obj2.Field('STR').CloneFromField(obj1.Field('TESTSTREAM'));
   writeln('--');
   writeln(obj2.DumpToString());
   writeln('--');
end;

procedure TFRE_DB_ObjectTests.ForAllHierarchicTest;
var obj2 : TFRE_DB_Object;
    obj       : IFRE_DB_Object;
    copyo     : IFRE_DB_Object;
    cnt       : integer;

    procedure Iterator(const obji : IFRE_DB_Object ; var halt : Boolean);
    begin
      inc(cnt);
      writeln('----CNT --- ',cnt,' ',obji.UID_String);
      case cnt of
        1: assert(obji.UID=TEST_GUID_1);
        2: assert(obji.UID=TEST_GUID_2);
        3: assert(obji.UID=TEST_GUID_3);
      end;
    end;

begin
  cnt := 0;
  obj2 := GFRE_DB.NewObject;
  obj2.Field('uid').AsGUID:=TEST_GUID_1;
  Fill_Test_Object('chung_',obj2);
  Fill_Test_Object('sub',obj2.Field('newo').AsObject);
  obj2.Field('newo').AsObject.Field('uid').AsGUID:=TEST_GUID_2;
  Fill_Test_Object('subsub',obj2.Field('newo').AsObject.Field('newnewo').AsObject);
  obj2.Field('newo').AsObject.Field('newnewo').AsObject.Field('uid').AsGUID:=TEST_GUID_3;
  obj2.ForAllObjectsBreakHierarchicI(@Iterator);

  assert(obj2.FetchObjByUIDI(TEST_GUID_3,obj));
  writeln('Fetched SUBO 3 ',obj.DumpToString());
  writeln('----');
  obj:=nil;
  assert(obj2.FetchObjWithStringFieldValue('SUBSTRING','äüö ÄÜÖ ß',obj,'XX')=false);
  writeln('----');
  obj:=nil;
  assert(obj2.FetchObjWithStringFieldValue('SUBSTRING','äüö ÄÜÖ ß',obj));
  writeln('Fetched SUBO 2 via stringkey ',obj.DumpToString());
  writeln('----');
  copyo := GFRE_DBI.NewObject;
  copyo.SetAllSimpleObjectFieldsFromObject(obj);
  writeln('--- COPYO SET',copyo.DumpToString());
  writeln('--');
end;

procedure RegisterTestCodeClasses;
begin
  GFRE_DBI.RegisterObjectClassEx(TFRE_DB_TEST_CODE_CLASS);
  GFRE_DBI.Initialize_Extension_Objects;
end;

initialization
  RegisterTest(TFRE_DB_ObjectTests);
  RegisterTest(TFRE_DB_PersistanceTests);

end.

