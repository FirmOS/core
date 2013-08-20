program CON_LISTARRAYTEST;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,FOS_LISTARRAY, SysUtils;


type

    TFOS_LISTTestArray = specialize TFOS_LISTARRAY <integer>;



  procedure basic;
  var la   :TFOS_LISTTestArray;
       i   :integer;
      fi,li:TListIndex;
      s    :integer;
      si   :integer;
      lai  :TListindex;
  begin
   writeln('BASIC TEST');
   la:=TFOS_LISTTestArray.Create;
   fi:=-1;
   li:=-1;
   for i:= 0 to c_array_length-1 do begin
    s:=i;
    li:=la.Store(s,li);
    if li=-1 then begin
     writeln('NO MORE SPACE !!!');
     abort;
    end;
    if (i mod 10000)=0 then writeln(li);
    if fi=-1 then begin
     fi:=li;
    end;
   end;
   writeln('ARRAY FULL TEST');
   li:=la.Store(s+1,li);
   if li<>-1 then begin
    writeln('ARRAY SHOULD BE FULL ERROR, NO MORE SPACE !!!');
    abort;
   end else begin
    writeln('ARRAY IS FULL, OK');
   end;
   writeln('LINEAR CLEAR');
   si:=fi;
   for i:= 0 to c_array_length-1 do begin
    si:=la.Get(si,s,lai,true);
    if (si<0) and (i<>(c_array_length-1))then begin
     writeln('NO MORE DATA FOUND, ABORT');
     abort;
    end;
    if s<>i then begin
     writeln('WRONG DATA FOUND, ABORT ',s);
     abort;
    end;
   end;
   if si<>-1 then begin
    writeln('ARRAY SHOULD BE EMPTY ERROR, DATA LEFT !!!');
    abort;
   end else begin
    writeln('ARRAY IS EMPTY, OK');
   end;
   la.free;
   writeln('BASIC TEST DONE');
  end;

  procedure interleaved;
  var la   :TFOS_LISTTestArray;
       i   :integer;
      fi1,li1,fi2,li2:TListIndex;
      s    :integer;
      si   :integer;
      lai  : TListindex;
  begin
   writeln('INTERLEAVED TEST');
   la:=TFOS_LISTTestArray.Create;
   fi1:=-1;
   li1:=-1;
   fi2:=-1;
   li2:=-1;
   for i:= 0 to 10000 do begin
    s:=i+100000;
    li1:=la.Store(s,li1);
    if fi1=-1 then begin
     fi1:=li1;
    end;
    s:=i+200000;
    li2:=la.Store(s,li2);
    if fi2=-1 then begin
     fi2:=li2;
    end;
   end;
   writeln('LINEAR CLEAR ROW 1');
   si:=fi1;
   for i:= 0 to 10000 do begin
    si:=la.Get(si,s,lai,true);
    if (si>=0) then begin
     s:=s-100000;
     if s<>i then begin
      writeln('WRONG DATA FOUND, ABORT ',s);
      abort;
     end;
    end;
   end;
   writeln('LINEAR CLEAR ROW 2');
   si:=fi2;
   for i:= 0 to 10000 do begin
    si:=la.Get(si,s,lai,true);
    if (si>=0) then begin
     s:=s-200000;
     if s<>i then begin
      writeln('WRONG DATA FOUND, ABORT ',s);
      abort;
     end;
    end;
   end;
   if la.Count<>0 then begin
    writeln('ARRAY NOT EMPTY, ABORT ');
    abort;
   end;
   writeln('INTERLEAVED TEST DONE');
   la.free;
  end;

  procedure random_delete;
  var la   :TFOS_LISTTestArray;
       i   :integer;
      fi1,li1,fi2,li2,lai:TListIndex;
      s    :integer;
      si   :integer;
      a    :Array [1..10000] of TListIndex;
      lc   :integer;
      sum  :int64;
      gsum :int64;
  begin
   writeln('RANDOM DELETE TEST');
   randomize;
   la:=TFOS_LISTTestArray.Create;
   fi1:=-1;
   li1:=-1;
   fi2:=-1;
   li2:=-1;
   for i:= 1 to 10000 do begin
    s:=i;
    li1:=la.Store(s,li1);
    if fi1=-1 then begin
     fi1:=li1;
    end;
    a[i]:=li1;
    s:=i+200000;
    li2:=la.Store(s,li2);
    if fi2=-1 then begin
     fi2:=li2;
    end;
   end;

   writeln('DELETEING ROW1 RANDOMIZED');
   sum:=0;
   lc:=10000;
   while lc>0 do begin
    i:=random(10000)+1;
    if a[i]<>-1 then begin
     si:=la.Get(a[i],s,lai,true);
     a[i]:=-1;
//     writeln('fetched ',s);
     sum:=sum+s;
     dec(lc);
     if (lc mod 1000)=0 then begin
      writeln('DELETEING');
     end;
    end;
   end;
   writeln(sum);
   gsum:=(10000*(10000+1)) div 2;
   if sum=gsum then begin
    writeln('SUM TEST OK !!');
   end else begin
    writeln('SUM TEST FAILED !!',sum);
    abort;
   end;


   writeln('LINEAR CLEAR ROW 2');
   si:=fi2;
   for i:= 1 to 10000 do begin
    si:=la.Get(si,s,lai,true);
    if (si>=0) then begin
     s:=s-200000;
     if s<>i then begin
      writeln('WRONG DATA FOUND, ABORT ',s);
      abort;
     end;
    end;
   end;
   if la.Count<>0 then begin
    writeln('ARRAY NOT EMPTY, ABORT ');
    abort;
   end;
   writeln('RANDOM DELETE TEST DONE');
  end;

begin

 basic;
 interleaved;
 random_delete;



end.

