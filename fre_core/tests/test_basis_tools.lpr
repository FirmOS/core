program test_basis_tools;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,unix,
  {$ENDIF}
  classes,FOS_BASIS_TOOLS,SysUtils,FOS_TOOL_INTERFACES,fos_default_implementation;

 {$ASSERTIONS ON}

procedure TestBasisTools;
var test:ansistring;

   procedure TestAES(input,check:string;const crypt:boolean=true;const mode:integer=0);
   begin
     writeln;
     if crypt then begin
      case mode of
       0: begin
        writeln(' TEST AES 128 CRYPT');
        GFRE_BT.EncryptAESStreamCBC128(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('2b7e151628aed2a6abf7158809cf4f3c'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       1: begin
        writeln(' TEST AES 192 CRYPT');
        GFRE_BT.EncryptAESStreamCBC192(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       2: begin
        writeln(' TEST AES 256 CRYPT');
        GFRE_BT.EncryptAESStreamCBC256(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       end;
     end else begin
      case mode of
       0: begin
        writeln(' TEST AES 128 DECRYPT');
        GFRE_BT.DecryptAESStreamCBC128(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('2b7e151628aed2a6abf7158809cf4f3c'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       1: begin
        writeln(' TEST AES 192 DECRYPT');
        GFRE_BT.DecryptAESStreamCBC192(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       2: begin
        writeln(' TEST AES 256 DECRYPT');
        GFRE_BT.DecryptAESStreamCBC256(GFRE_BT.HexStr2Str(input),GFRE_BT.HexStr2Str('603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4'),
                                  GFRE_BT.HexStr2Str('000102030405060708090a0b0c0d0e0f'),test);
        end;
       end;
     end;
     writeln(input);
     if crypt then writeln(' -CRYPT-> ') else writeln(' -DECRYPT-> ');
     writeln(GFRE_BT.Str2HexStr(test));
     writeln(' should be ');
     writeln(check);
     assert(GFRE_BT.Str2HexStr(test)=check);
     writeln(' OK ');
     writeln;
   end;

begin
 test:=GFRE_BT.HexStr2Str('A0b0C0d0E0f0d41d8cd98f00b204e9800998ecf8427e');
 assert(GFRE_BT.Str2HexStr(test)='a0b0c0d0e0f0d41d8cd98f00b204e9800998ecf8427e');
 writeln('HexStr2Str <-> Str2HexStr OK');
 test:=GFRE_BT.HashString_MD5_HEX('');
 assert(test='d41d8cd98f00b204e9800998ecf8427e');
 test:=GFRE_BT.HashString_MD5_HEX('a');
 assert(test='0cc175b9c0f1b6a831c399e269772661');
 test:=GFRE_BT.HashString_MD5_HEX('abc');
 assert(test='900150983cd24fb0d6963f7d28e17f72');
 test:=GFRE_BT.HashString_MD5_HEX('message digest');
 assert(test='f96b697d7cb7938d525a2f31aaf161d0');
 test:=GFRE_BT.HashString_MD5_HEX('abcdefghijklmnopqrstuvwxyz');
 assert(test='c3fcd3d76192e4007dfb496cca67e13b');
 test:=GFRE_BT.HMAC_MD5_HEX('what do ya want for nothing?','Jefe');
 assert(test='750c783e6ab0b503eaa86e310a5db738');
 setlength(test,16);
 fillchar(test[1],16,$b);
 test:=GFRE_BT.HMAC_MD5_HEX('Hi There',test);
 assert(test='9294727a3638bb1c13f48ef8158bfc9d');
 writeln('MD5 / HMAC Test OK');

 // NIST Test Vectors : sp800-38a.pdf
 //128
 TestAES('6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',
         '7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7');
 TestAES('7649abac8119b246cee98e9b12e9197d5086cb9b507219ee95db113a917678b273bed6b8e3c1743b7116e69e222295163ff1caa1681fac09120eca307586e1a7',
         '6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',false);
 //192
 TestAES('6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',
         '4f021db243bc633d7178183a9fa071e8b4d9ada9ad7dedf4e5e738763f69145a571b242012fb7ae07fa9baac3df102e008b0e27988598881d920a9e64f5615cd',true,1);
 TestAES('4f021db243bc633d7178183a9fa071e8b4d9ada9ad7dedf4e5e738763f69145a571b242012fb7ae07fa9baac3df102e008b0e27988598881d920a9e64f5615cd',
         '6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',false,1);
 //256
 TestAES('6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',
         'f58c4c04d6e5f1ba779eabfb5f7bfbd69cfc4e967edb808d679f777bc6702c7d39f23369a9d9bacfa530e26304231461b2eb05e2c39be9fcda6c19078c6a9d1b',true,2);
 TestAES('f58c4c04d6e5f1ba779eabfb5f7bfbd69cfc4e967edb808d679f777bc6702c7d39f23369a9d9bacfa530e26304231461b2eb05e2c39be9fcda6c19078c6a9d1b',
         '6bc1bee22e409f96e93d7e117393172aae2d8a571e03ac9c9eb76fac45af8e5130c81c46a35ce411e5fbc1191a0a52eff69f2445df4f9b17ad2b417be66c3710',false,2);

end;

var test:string;
      ov,nv:int64;
      s,ss:string;
      i :integer;
      benchnew:integer;

begin
 TestBasisTools;

 s:='';
 for i:=0 to 1000 do begin
  s:=s+'a0b0c0d0e0f0d41d8cd98f00b204e9800998ecf8427e';
 end;
 ov:=GFRE_BT.Get_Ticks_ms;
 for i:=0 to 1000 do begin
  test:=GFRE_BT.HexStr2Str(s);
 end;
 nv:=GFRE_BT.Get_Ticks_ms;
 benchnew:=nv-ov;
 writeln('BENCH 10000 HEXSTR 2 STR ',benchnew,' ms');


 ov:=GFRE_BT.Get_Ticks_ms;
 for i:=0 to 1000 do begin
  s:=s+char(random(255));
 end;
 for i:=0 to 1000 do begin
  ss:=GFRE_BT.HashString_MD5_HEX(s);
 end;
 nv:=GFRE_BT.Get_Ticks_ms;
 benchnew:=nv-ov;
 writeln('BENCH MD5 ',benchnew,' ms');


end.
