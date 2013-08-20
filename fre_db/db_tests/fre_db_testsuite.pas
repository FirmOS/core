unit fre_db_testsuite; 
{$mode objfpc}{$H+}
{$codepage UTF8}

// RAW Testsuite of DB Core

interface

uses
  Classes, SysUtils,fpcunit,testregistry,testdecorator,
  FRE_DB_CORE,FOS_TOOL_INTERFACES,FRE_DB_INTERFACE,FRE_DB_SYSRIGHT_CONSTANTS;

  var TEST_GUID_1,TEST_GUID_2,TEST_GUID_3 : TGUID;

type
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
    procedure Mediator_Empty;
    procedure ObjectProperties;
    procedure DBConnection;
    procedure FieldTypes;
    procedure DumpTest;
    procedure DumpJSONTestFull;
    procedure SetNullArray;
    procedure StreamTest;
  end;

  { TFRE_DB_PersistanceTests }

  TFRE_DB_PersistanceTests = class(TTestcase)
  private
    FSysconn  : IFRE_DB_SYS_CONNECTION;
    FWorkConn : IFRE_DB_CONNECTION;
    procedure ConnectDB(const user,pw:string);
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
    procedure DumpDatabase;
  end;

implementation


procedure Fill_Test_Object(const field_prefix:string;const obj:TFRE_DB_Object);
var dbs : TFRE_DB_Stream;
begin
  obj.Field(field_prefix+'STRING').AsStringArr               := GFRE_DB.ConstructStringArray(['äüö ÄÜÖ ß','מדוע לא דברו עברית?','ག་རེ་བྱས་ཁོ་རང་ཚོས་བོད་སྐད་ཆ་དེ་ག་རང་བཤད་ཀྱི་མ་རེད།','लोकांना मराठी का बोलता येत नाही?']);
  obj.Field(field_prefix+'STRING_NA').SetAsEmptyStringArray  ;

  obj.Field(field_prefix+'UID').AsGUIDArr                    := GFRE_DB.ConstructGuidArray([TEST_GUID_1,TEST_GUID_2,TEST_GUID_3]);
  obj.Field(field_prefix+'BYTE').AsByteArr                   := GFRE_DB.ConstructByteArray([0,255,256,-1]);
  obj.Field(field_prefix+'INT16').AsInt16Arr                 := GFRE_DB.ConstructInt16Array([-32768,32767,0,65535]);
  obj.Field(field_prefix+'INT32').AsInt32Arr                 := GFRE_DB.ConstructInt32Array([-2147483648,2147483647,0,4294967295]);
  obj.Field(field_prefix+'INT64').AsInt64Arr                 := GFRE_DB.ConstructInt64Array([-9223372036854775808,9223372036854775807,0,18446744073709551615]);
  obj.Field(field_prefix+'UINT16').AsUInt16Arr               := GFRE_DB.ConstructUInt16Array([-32768,32767,0,65535]);
  obj.Field(field_prefix+'UINT32').AsUInt32Arr               := GFRE_DB.ConstructUInt32Array([-2147483648,2147483647,0,4294967295]);
  obj.Field(field_prefix+'UINT64').AsUInt64Arr               := GFRE_DB.ConstructUInt64Array([-9223372036854775808,9223372036854775807,0,18446744073709551615]);

  obj.Field(field_prefix+'REAL32').AsReal32Arr               := GFRE_DB.ConstructReal32Array ([pi,0,1,-2.2,3.3,-4.4]);
  obj.Field(field_prefix+'REAL64').AsReal64Arr               := GFRE_DB.ConstructReal64Array ([pi,0,1,-2.2,3.3,-4.4]);

  obj.Field(field_prefix+'CURRENCY').AsCurrencyArr           := GFRE_DB.ConstructCurrencyArray([122.93,100.2,33.90]);
  obj.Field(field_prefix+'BOOLEAN').AsBooleanArr             := GFRE_DB.ConstructBooleanArray([true,false,false,true]);
  obj.Field(field_prefix+'DATE').AsDateTimeArr               := GFRE_DB.ConstructDateTimeArray([10000000,20000000,30000000,40000000]);
  obj.Field(field_prefix+'DATE_UTC').AsDateTimeUTCArr        := GFRE_DB.ConstructDateTimeArray([10000000,20000000,30000000,40000000]);
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
end;

procedure Check_Test_Object(const field_prefix:string;const obj:TFRE_DB_Object);
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
begin
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('SYSTEM');
  GFRE_DB_DEFAULT_PS_LAYER.DEBUG_DisconnectLayer('WORKTEST');
  ConnectDB('test1@system','test1');
  FWorkConn.Collection('TEST_1_VOL',true,true);
  FWorkConn.Collection('TEST_1_PERS',true,true);
  //FWorkConn.C;
end;

procedure TFRE_DB_PersistanceTests.DumpDatabase;
begin
end;

procedure TFRE_DB_PersistanceTests.ConnectDB(const user, pw: string);
begin
  FWorkConn := GFRE_DB.NewConnection;
  CheckDbResult(FWorkConn.Connect('WORKTEST',user,pw),'connect failed');
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
  AssertEquals(1,TestObject.FieldCount(false));
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

procedure TFRE_DB_ObjectTests.Mediator_Empty;
begin
  AssertNull(TestObject.Mediator);
end;

procedure TFRE_DB_ObjectTests.ObjectProperties;
begin
  AssertTrue(TestObject.Properties = []);
end;

procedure TFRE_DB_ObjectTests.DBConnection;
begin
  try
   TestObject.GetDBConnection;
  except
    exit;
  end;
  Fail('GetDBConnection must raise an exception on unmanaged objects')
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
  writeln(Object2.dumpToString);
  writeln(GFRE_BT.GUID_2_HexString(Object2.UID));
  Object2.free;
end;


initialization
  //RegisterTest(TFRE_DB_ObjectTests);
  RegisterTest(TFRE_DB_PersistanceTests);

end.

