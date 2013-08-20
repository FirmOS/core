unit db_unittest;

{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

interface

uses  Classes, SysUtils,FRE_DB_CORE,FRE_DB_INTERFACE, FOS_TOOL_INTERFACES,FOS_NPS,fpjson,jsonparser;


procedure CreateCollectionTest(const db:String;count:integer);
procedure ConnectTest         (const DB:string);
procedure CreateDB           (const DB:string);
procedure DeleteDB           (const DB:string);
procedure CreateSystemDB;
procedure ConnectSystem;
procedure DumpSystem;

procedure ReadTest            (const db:String='');
procedure TestChange;
procedure BasicTest;
procedure SchemeTest;
procedure ClearTest;
procedure TransformTest;
procedure Scheme_Update_Test;
procedure ListDBTest;

implementation


var TEST_GUID1,TEST_GUID2,TEST_GUID3 : TGuid;


function CreateFieldTestObject(const ob_name:string;const nonest:boolean=false;const nostreams:boolean=false;const nodates:boolean=false;const realshort:boolean=false):TFRE_DB_Object;
var ob,ob1,ob2,ob3,ob4 : TFRE_DB_Object;
    m  : TFRE_DB_Stream;
begin
  OB:=GFRE_DB.NewObject;
  result := ob;

  ob.Field('N').AsString := ob_name;
  if realshort then exit(ob);
  ob.Field('UID2').AddGuid(TEST_GUID1);
  ob.Field('UID2').AddGuid(TEST_GUID2);
  ob.Field('UID2').AddGuid(TEST_GUID3);
  {$WARNINGS OFF}
  ob.Field('Bytes').AsByteArr          := GFRE_DB.ConstructByteArray ([0,255,256,-1]);
  ob.Field('Bytes').AddByte(42);
  ob.Field('INT16').AsInt16Arr         := GFRE_DB.ConstructInt16Array([-32768,32767,0,65535]);
  ob.Field('INT32').AsInt32Arr         := GFRE_DB.ConstructInt32Array([-2147483648,2147483647,0,4294967295]);
  ob.Field('INT64').AsInt64Arr         := GFRE_DB.ConstructInt64Array([-9223372036854775808,9223372036854775808,0,18446744073709551615]);
  ob.Field('UINT16').AsUInt16Arr       := GFRE_DB.ConstructUInt16Array([-32768,32767,0,65535]);
  ob.Field('UINT32').AsUInt32Arr       := GFRE_DB.ConstructUInt32Array([-2147483648,2147483647,0,4294967295]);
  ob.Field('UINT64').AsUInt64Arr       := GFRE_DB.ConstructUInt64Array([-9223372036854775808,9223372036854775808,0,18446744073709551615]);

  ob.Field('REAL32').AsReal32Arr       := GFRE_DB.ConstructReal32Array ([pi,1,-2.2,3.3,-4.4]);
  ob.Field('REAL64').AsReal64Arr       := GFRE_DB.ConstructReal64Array ([pi,-2.2,3.3,-4.4]);

  ob.Field('curr').AsCurrencyArr       := GFRE_DB.ConstructCurrencyArray([122.93,100.2,33.90]);
  ob.Field('STRING').AsStringArr       := GFRE_DB.ConstructStringArray(['FirstString','The second','third','last one']);
  //ob.Field('UNICODE').AddUnicodeString (AnsiToUtf8('äöü ÄÜÖ Österreich'));
  ob.Field('STRING2').AsString         := 'äöü ÄÜÖ Österreich';
  ob.Field('bool').AsBooleanArr        := GFRE_DB.ConstructBooleanArray([true,false,false,true]);
  if not nodates then begin
    ob.Field('date').AsDateTimeArr     := GFRE_DB.ConstructDateTimeArray([100,200,300,400]);
    ob.Field('date2').AsDateTimeUTCArr := GFRE_DB.ConstructDateTimeArray([100,200,300,400]);
    ob.Field('now').AsDateTime         := GFRE_DT.DateTimeToDBDateTime64(now);
  end;

  if ob.FieldExists('THEUNKNOWN') then begin
   writeln('The Unknown Field Found!');
   abort;
  end;


  ob.Field('UID3').AddGuid(TEST_GUID1);
  ob.Field('UID3').AddGuid(TEST_GUID2);
  ob.Field('UID3').AddGuid(TEST_GUID3);

  ob.Field('UID3').RemoveIndex(1);
  ob.Field('UID3').RemoveIndex(1);
  ob.Field('UID3').Removeindex(0);
  if ob.Field('UID3').ValueCount>0 then begin
   writeln('COULD NOT REMOVE ALL INDICES !');
   abort;
  end;

 {$WARNINGS ON}
  if not nostreams then begin
    m:=TFRE_DB_Stream .Create;
    m.WriteAnsiString('Aloha From Hawaii');
    ob.Field('Stream').AddStream(m);
    m:=TFRE_DB_Stream .Create;
    m.WriteAnsiString('Servus From Hawaii');
    ob.Field('Stream').AddStream(m);
  end;

  if not nonest then begin
    //OB1:=CreateFieldTestObject(ob_name+'_NEST1',true);
    //OB2:=CreateFieldTestObject(ob_name+'_NEST2',true);
    //ob.Field('OBJECTS').AsObjectArr := GFRE_DB.ConstructObjectArray([ob1,ob2]);
    //OB3:=CreateFieldTestObject(ob_name+'_NEST3',true);
    //ob3.Field('OB3').AsString:='This is object 3';
    //ob2.Field('EX').AsObject := ob3;
    //OB4:=CreateFieldTestObject(ob_name+'_NEST4',true);
    //ob4.Field('OB').AsString := 'This is object 4';
    //ob3.Field('EX').AsObject := ob4;
    //
    OB1:=CreateFieldTestObject(ob_name+'_NEST1',true);
    ob.Field('OBJECTS').AsObject := ob1;
    exit;
    OB1:=CreateFieldTestObject(ob_name+'_NEST1',true);
    OB2:=CreateFieldTestObject(ob_name+'_NEST2',true);
    ob.Field('OBJECTS').AsObjectArr := GFRE_DB.ConstructObjectArray([ob2,ob1]);
    OB3:=CreateFieldTestObject(ob_name+'_NEST3',true);
    ob3.Field('OB3').AsString:='This is object 3';

    ob2.Field('EX').AsObject  := ob3;

    OB4:=CreateFieldTestObject(ob_name+'_NEST4',true);
    ob4.Field('OB').AsString := 'This is object 4';
    ob3.Field('EX').AsObject := ob4;

    //ob.Field('X').AsObject:=ob; // circle
   // ob4.Field('LOOP').AsObject:=ob; //circle

    ob.Field('SchemeObject').AsObject:=GFRE_DB.NewObject(TFRE_DB_SchemeObject);
    ob.Field('LINK').AsObjectLink := ob4.Uid;
//    ob.Field('LINK').AsGUID := ob4.Uid;

    //BAD ORDER : ->
    //ob2.Field('EX').AsObject  := ob3;
  end;
//  result := ob;
end;

procedure CreateCollectionTest(const db: String; count: integer);
var CONN : TFRE_DB_CONNECTION;
    COLL : TFRE_DB_COLLECTION;
    COLL2: TFRE_DB_COLLECTION;
    COLL3: TFRE_DB_COLLECTION;
    NO   : TFRE_DB_Object;
    i    : Integer;
    old,new  : Cardinal;
    res      : TFRE_DB_Errortype;

begin
  CONN := GFRE_DB.NewConnection;
  res := CONN.Connect(db,'admin','admin');
  if res<>edb_OK then begin
    writeln('Cannot connect : ',db,' ',res);
    exit;
  end;
  COLL := CONN.Collection('TestColl');
  //COLL2:= CONN.Collection('TestColl1');
  //COLL3:= CONN.Collection('TestColl2');
  writeln('CREATING Objects');
  old := GFRE_BT.Get_Ticks_ms;
  for i:= 1 to count do begin
    NO := CreateFieldTestObject('OBJ_'+IntToStr(i),true,true,true,True);
    if COLL.Store(no)<>edb_OK then gfre_bt.CriticalAbort('internal uid twice??');
    //if COLL2.Store(no)<>edb_OK then gfre_bt.CriticalAbort('internal uid twice??');
    //if COLL3.Store(no)<>edb_OK then gfre_bt.CriticalAbort('internal uid twice??');
    //if CONN.Collection('MASTER').Store(no)<>edb_EXISTS then GFRE_BT.CriticalAbort('logic / existt failure');
    //if COLL.Store(no)<>edb_EXISTS then GFRE_BT.CriticalAbort('logic / exists failure 1');
    //if COLL2.Store(no)<>edb_EXISTS then GFRE_BT.CriticalAbort('logic / exists failure 2');
    //if COLL3.Store(no)<>edb_EXISTS then GFRE_BT.CriticalAbort('logic / exists failure 3');
    //if i mod 1000 = 0 then begin
    //  writeln(i,' - ',no.Field('uid').AsString);
    //end;
  end;
  new := GFRE_BT.Get_Ticks_ms-old;
  writeln('time ',new);
  writeln('Created ',COLL.Count,' new Objects');

  writeln('ForceFlush -> Persistance');

  old := GFRE_BT.Get_Ticks_ms;
  //conn.CacheFlush_Collections(true);
  //conn.CacheFlush_Objects(true);
  new := GFRE_BT.Get_Ticks_ms-old;
  writeln('time ',new);

  CONN.Free;
  writeln('END');
end;


procedure ConnectTest(const DB: string);
var l_Connection : TFRE_DB_CONNECTION;
    apps         : TFRE_DB_APPLICATION_ARRAY;
begin
  l_Connection := GFRE_DB.NewConnection;
  WriteLn('TRY CONNECT [',l_Connection.Connect(db,'admin','admin'),']');
  l_Connection.FetchApplications(apps);
  l_Connection.Free;
end;

procedure CreateDB(const DB: string);
begin
  writeln('CREATE ',db,' [',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase(db),']');
end;

procedure DeleteDB(const DB: string);
begin
  writeln('DELETE ',db,' [',GFRE_DB_DEFAULT_PS_LAYER.DeleteDatabase(db),']');
end;

procedure CreateSystemDB;
begin
  writeln('SYSCREATE [',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('SYSTEM'),']');
end;

procedure ConnectSystem;
var l_Connection : TFRE_DB_SYSTEM_CONNECTION;
    l_result     : TFRE_DB_Errortype;
    apps         : TFRE_DB_APPLICATION_ARRAY;
    val          : TFRE_DB_ClientFieldValidator;
    enum         : TFRE_DB_Enum;
begin
  l_Connection := GFRE_DB.NewDirectSysConnection();
  l_result     := l_Connection.Connect('admin','admin');
  if l_result  = edb_DB_NOT_FOUND then begin
    writeln('Creating System DB');
    writeln('SYSCREATE [',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('SYSTEM'),']');
    l_result     := l_Connection.Connect('admin','admin');
  end;
  writeln('LOGIN x : ',l_result);
  //val:=l_Connection.NewClientFieldValidator('test_Validator').Describe('((a))',TFRE_DB_TEXT.CreateText('$TVAL','Test Validator','long text','hint text'),nil,'');
  //l_Connection.StoreClientFieldValidator(val);
  //enum:=l_Connection.NewEnum('test_enum').Describe(TFRE_DB_TEXT.CreateText('$TENUM','Test Enum','long text','hint text'));
  //l_Connection.StoreEnum(enum);
  if l_result=edb_OK then begin
    l_Connection.DumpSystem;
  end else begin
    writeln('Unexpected : ',l_result);
  end;
  l_Connection.Free;
end;

procedure DumpSystem;
var l_Connection : TFRE_DB_SYSTEM_CONNECTION;
    l_result     : TFRE_DB_Errortype;
    apps         : TFRE_DB_APPLICATION_ARRAY;
    val          : TFRE_DB_ClientFieldValidator;
    enum         : TFRE_DB_Enum;
    colllist     : IFOS_STRINGS;
    i            : Integer;
    master       : TFRE_DB_COLLECTION;
    user         : TFRE_DB_USER;

    procedure DumpObject(const obj:TFRE_DB_Object);
    begin
      if obj.SchemeClass='TFRE_DB_USER' then begin
        writeln(obj.DumpToString);
      end;
    end;

begin
  l_Connection := GFRE_DB.NewDirectSysConnection();
  l_result     := l_Connection.Connect('admin','admin');
  if l_result  = edb_DB_NOT_FOUND then begin
    writeln('Creating System DB');
    writeln('SYSCREATE [',GFRE_DB_DEFAULT_PS_LAYER.CreateDatabase('SYSTEM'),']');
    l_result     := l_Connection.Connect('admin','admin');
  end;
  writeln('LOGIN x : ',l_result);
  if l_result=edb_OK then begin
    colllist := l_Connection.CollectionList;
    writeln('Collections [',colllist.Commatext+']');
    for i:=0 to l_Connection.CollectionList.Count-1 do begin
      writeln(colllist[i],' CNT= ',l_Connection.Collection(colllist[i]).Count);
    end;
    master := l_Connection.Collection('MASTER');
    master.ForAll(@DumpObject);
    writeln(' ******************************* ');
    writeln(' ******************************* ');
    writeln(' ******************************* ');
    l_Connection.FetchUser('admin',user);
//    writeln(user.Field('SUPERCALC').AsObject.Field('SUPERDATA').AsString);
    //writeln(user.Field('SUPERCALC').AsObject.Field('SUPERDATA').AsString);
    //writeln(' ******************************* ');
    //writeln(user.Field('SUPERCALC').AsByte);
    //writeln(' ******************************* ');
    //writeln(user.Field('SUPERCALC').AsString);
    //writeln(' ******************************* ');
    //writeln('GUID: ',gfre_Bt.GUID_2_HexString(user.Field('SUPERCALC').AsGUID));
    //writeln(' ******************************* ');
    //writeln(' ******************************* ');
    //user.Field('SUPERCALC').AsObject := nil;
  end;
  l_Connection.free;
end;

procedure ReadTest(const db: String);
var CONN     : TFRE_DB_CONNECTION;
    COLL     : TFRE_DB_COLLECTION;
    counter  : integer;
    fulldump : boolean;
    colllist : IFOS_STRINGS;
    i        : Integer;
    res      : TFRE_DB_Errortype;

  procedure DumpObject(const obj:TFRE_DB_Object);
  begin
    if ((counter mod 1000)=0) or fulldump then begin
      if fulldump then begin
        writeln(obj.DumpToString());
      end else begin
        writeln(format('<  DUMPING OBJECT (%d) ID=%s STAMP = %s >',[counter,obj.Field('UID').AsString,obj.Field('STAMP').AsString]));
      end;
    end;
    inc(counter);
  end;

begin
  counter:=0;
  CONN := GFRE_DB.NewConnection;
  if db='' then begin
    res := CONN.Connect('test_db','admin','admin');
  end else begin
    res := CONN.Connect(db,'admin','admin');
    fulldump :=  true;
  end;
  if res<>edb_OK then begin
    writeln('Cannot connect ',db);
    halt;
  end;
  COLL := CONN.Collection('MASTER');
  colllist := conn.CollectionList;
  writeln('Collections [',colllist.Commatext+']');
  for i:=0 to conn.CollectionList.Count-1 do begin
    writeln(colllist[i],' CNT= ',conn.Collection(colllist[i]).Count);
  end;
  writeln('Collection Dumps ',COLL.Count);
  coll.ForAll(@DumpObject);
  CONN.Free;
  writeln('Processed ',counter,' Objects');
  writeln;
end;


procedure TestChange;
var CONN    : TFRE_DB_CONNECTION;
    COLL    : TFRE_DB_COLLECTION;
    counter : integer;

  procedure DumpObject(const obj:TFRE_DB_Object);
  begin
    if (counter mod 1000)=0 then begin
      writeln('<<<<  DUMPING OBJECT (',counter,')'+obj.Field('UID').AsString,' >>>>> ');
    end;
    obj.Field('STAMP').AsDateTime:=GFRE_DT.DateTimeToDBDateTime64(Now);
    inc(counter);
  end;
begin
  counter:=0;
  CONN := GFRE_DB.NewConnection;
  CONN.Connect('test_db','admin','admin');
  COLL := CONN.Collection('MASTER');
  writeln('Collection Dump ReadChange ',COLL.Count);
  coll.ForAll(@DumpObject);
  writeln('Processed ',counter,' Objects');
  CONN.Free;
  writeln;
end;

procedure BasicTest;
var ob,obl : TFRE_DB_Object;
    len    : qword;
    data   : String;
    mp     : Pointer;
    fld    : TFRE_DB_FIELD;
    tob    : TFRE_DB_Object;
    sc     : TFRE_DB_SCHEME_COLLECTION;
begin
//  ob:=TFRE_DB_SCHEME_COLLECTION.Create(nil,'Scheme');
////  ob:=TFRE_DB_COLLECTION.Create(nil,'Scheme');
//  //writeln(ob.DumpToString);
//  len:=ob.NeededSize;
//  writeln('Streamingsize =========== ',len);
//  SetLength(data,len);
//  mp:=@data[1];
//  ob.CopyToMemory(mp);
//  ob.free;
//  ob:=nil;
//  writeln('********************* STREAMED ***********************');
//  mp:=@data[1];
//  obl:=TFRE_DB_Object.CreateFromMemory(mp);
//  writeln(obl.dumpToString);
//  writeln('********************* STREAMED ***********************');
//  len:=obl.NeededSize;
//  writeln('Streamingsize =========== ',len);
//  SetLength(data,len);
//  mp:=@data[1];
//  obl.CopyToMemory(mp);
//  writeln(obl.dumpToString);
//  obl.free;
//
//exit;
  ob:=CreateFieldTestObject('TestObject');
//  ob:=CreateFieldTestObject('TestObject',true,true,true);
  writeln(ob.DumpToString);
  len:=ob.NeededSize;
  writeln('Streamingsize =========== ',len);
  SetLength(data,len);
  mp:=@data[1];
  ob.CopyToMemory(mp);
  ob.free;
  writeln('********************* STREAMED ***********************');

  mp:=@data[1];
  obl:=TFRE_DB_Object.CreateFromMemory(mp);
  writeln(obl.dumpToString);
  writeln(GFRE_BT.GUID_2_HexString(obl.UID));
  fld := obl.FieldPath('OBJECTs.EX.EX.N');
  if assigneD(fld) then begin
    writeln('PATH :::: (',fld.AsString,')');
  end else begin
    gfre_bt.CriticalAbort('FIELDPATH FAILED');
  end;
  fld := obl.FieldPath('N');
  if assigneD(fld) then begin
    writeln('PATH :::: (',fld.AsString,')');
  end else begin
    gfre_bt.CriticalAbort('FIELDPATH FAILED');
  end;

  tob := obl.FieldPath('OBJECTs.EX.EX').AsObject;
  writeln('********');
  writeln(tob.DumpToString);
  writeln('********');
  writeln(tob.Parent.Parent.Parent.DumpToString);
  writeln('+++++++++');
  obl.free;
  //writeln('Unicode');
  //writeln(obl.DumpToUniString);
end;

procedure SchemeTest;
var ob,obl : TFRE_DB_Object;
    len    : qword;
    data   : String;
    mp     : Pointer;
begin
  ob  :=  CreateFieldTestObject('TestObject',true,true,true);
//  writeln(ob.DumpToString);
  ob.free;
end;

procedure ClearTest;
var CONN    : TFRE_DB_CONNECTION;
    COLL    : TFRE_DB_COLLECTION;
    counter : integer;
    no:TFRE_DB_Object;
    i:integer;

  procedure DeleteObject(const obj:TFRE_DB_Object);
  var uid:TGuid;
  begin
    uid := obj.Field('UID').AsGUID;
    CONN.Delete(uid);
  end;

begin
  counter:=0;
  CONN := GFRE_DB.NewConnection;
  CONN.Connect('test_db','admin','admin');
  COLL := CONN.Collection('MASTER');
  writeln('Collection Dump ReadChange ',COLL.Count);
  coll.ForAllModify(@DeleteObject);
  writeln('Processed ',counter,' Objects');
//  COLL.Free;

  COLL := CONN.Collection('TestColl');
  writeln('CREATING Object');

  NO := CreateFieldTestObject('OBJ_'+IntToStr(i),true,true,true);
  if COLL.Store(no)<>edb_OK then gfre_bt.CriticalAbort('internal uid twice??');

//  COLL.Free;

  CONN.Free;
  writeln;
end;


procedure TransformTest;
var TOB    : TFRE_DB_SIMPLE_TRANSFORM;
    CONN   : TFRE_DB_CONNECTION;
    coll   : TFRE_DB_COLLECTION;
    derived: TFRE_DB_DERIVED_COLLECTION;
    i: Integer;
    dumped:boolean;

    procedure Dump(const ob:TFRE_DB_Object);
    begin
//      writeln(ob.DumpToString());
      writeln(ob.GetAsJSON.AsJSON);
    end;

    procedure Dump2(const ob:TFRE_DB_Object);
    begin
      if not dumped then begin
        writeln(ob.DumpToString());
        dumped:=true;
      end;
    end;

begin
  CONN := GFRE_DB.NewConnection;
  CONN.Connect('ADMIN_DB','admin','admin');
  coll := CONN.Collection('customer');
  dumped:=false;
  coll.ForAll(@Dump2);

  tob := GFRE_DB.NewObject(TFRE_DB_SIMPLE_TRANSFORM) as TFRE_DB_SIMPLE_TRANSFORM;

  tob.AddOneToOnescheme ('COMPANY');
  tob.AddOneToOnescheme ('CUSTOMERNUMBER');

  tob.AddCollectorscheme('%s %s | %5.5s (%s)',GFRE_DB.ConstructStringArray(['MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'ADRESS');
  tob.AddCollectorscheme('%s-%s-%s-%s-%s-%s',GFRE_DB.ConstructStringArray(['COMPANY','CUSTOMERNUMBER','MAINADDRESS.STREET','MAINADDRESS.NR','MAINADDRESS.CITY','MAINADDRESS.ZIP']),'FTX_SEARCH',true);

//  tob.SetupViewFields(GFRE_DB.ConstructStringArray(['COMPANY','CUSTOMERNUMBER','ADRESS','MAINADDRESS']);
  derived := Conn.CollectionCC('customer#page',TFRE_DB_DERIVED_COLLECTION) as TFRE_DB_DERIVED_COLLECTION;

//  derived.AddStringFieldFilter ('FTX','FTX_SEARCH','street',dbft_PART,false,true);
//  derived.AddStringFieldFilter ('TENANT','TENANTID','8584133ac9b1399126e6acd608c2282a',dbft_EXACT,false);

  derived.AddStringFieldFilter ('adress','ADRESS',TFRE_DB_StringArray.Create('nr1'),dbft_PART,True);
  derived.AddOrderField        ('1','customernumber',true);

  derived.Derive;
  //derived.SetPageSize(25);

  //derived.ReFilter;

  //writeln('Pagecount ',derived.PageCount,' ',derived.ItemCount,' ',derived.Currentpage);
  //for i:=1 to derived.PageCount do begin
  //  writeln('---  PAGE OPERATION (',i,')');
  //  derived.SetCurrentPage(i);
  //  derived.ApplyToPage(@dump);
  //end;
  //writeln('');
  //writeln('');
  //writeln('******************************************************* **** ** * *** * ** * ');
  //writeln('');
  //
  //derived.RemoveStringFieldFilter('adress',true);
  //derived.ReFilter;
  //writeln('Pagecount ',derived.PageCount,' ',derived.ItemCount,' ',derived.Currentpage);
  //for i:=0 to derived.PageCount do begin
  //  writeln('---  PAGE OPERATION (',i,')');
  //  derived.SetCurrentPage(i);
  //  derived.ApplyToPage(@dump);
  //end;

end;

procedure Scheme_Update_Test;
var scheme    : TFRE_DB_SchemeObject;
    pscheme   : TFRE_DB_SchemeObject;
    field_def : TFRE_DB_FieldSchemeDefinition;
    conn      : TFRE_DB_CONNECTION;
    err       : TFRE_DB_Errortype;
    raw_obj   : TFRE_DB_Object;
    upd_obj   : TFRE_DB_Object;

    procedure DumpSchemes(const db:TFRE_DB_SchemeObject);
    begin
      writeln(TFRE_DB_SchemeObject(db).DefinedSchemeName,' / ',TFRE_DB_SchemeObject(db).GetParentSchemeName);
    end;


begin
  CONN:=GFRE_DB.NewConnection;
  err:= CONN.Connect(cFREAdminDB,'admin','admin');
  writeln(CONN.CollectionList(true).Commatext);
  CONN.ForAllSchemes(@DumpSchemes);

  raw_obj := conn.NewObject;
  upd_obj := conn.NewObject('TFRE_DB_RadiusNetwork');

  raw_obj.Field('displayname').AsString:='newDisplayname';
  raw_obj.Field('sessiontimeout').AsString:='1234';
//  raw_obj.Field('wrong_one').asstring:='pfeiff drauf'; // geh oh
  raw_obj.Field('caid').AsString:='8584133ac9b1399126e6acd608c2282a';

  upd_obj.GetScheme.SetObjectFieldsWithScheme(raw_obj,upd_obj,false,conn);

  writeln('');
  writeln('');
  writeln('');
  writeln(upd_obj.DumpToString());

end;

procedure ListDBTest;
begin
  writeln('DATABASES : ['+GFRE_DB_DEFAULT_PS_LAYER.DatabaseList.Commatext+']');
end;


procedure SetupDefaults;
begin
  TEST_GUID1:=StringToGUID('{D890CDD6-4353-14E6-5EB6-10DF7ABE9067}');
  TEST_GUID2:=StringToGUID('{EC8F2E9B-54CE-34E8-6E77-D2376DC9F715}');
  TEST_GUID3:=StringToGUID('{CF55C302-6D7E-31CF-4A6F-ED8B55DDCF31}');
end;


initialization
  SetupDefaults;
finalization

end.


