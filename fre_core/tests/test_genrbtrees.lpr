program test_genrbtrees;

//super

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX} cthreads,{$ENDIF}
  CMem,Classes, SysUtils, test_genrb_tree;

var cnt:integer;
    rand:boolean;


begin
 writeln('basic rb tree test tool');
 writeln('usage [maxcount[1000000]] [randomekeys[0|1]]');
 cnt:=strtointdef(ParamStr(1),1000000);
 rand:=strtointdef(paramstr(2),0)=1;
// test_rb_ss(cnt,rand);
 writeln;
 test_rb_ii(cnt,rand);
 exit;
 writeln;
 test_rb_irec(cnt,rand);
 writeln;
 test_rb_objects(cnt,rand);
 writeln;
 test_rb_interface(cnt,rand);
 writeln;
 test_rb_localproc;
end.

