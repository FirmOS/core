unit fos_cpu_tools;

{
(§LIC)
  (c) Autor,Copyright Dipl.Ing.- Helmut Hartl
      FirmOS Business Solutions GmbH
      New Style BSD Licence (OSI)

  Copyright (c) 2001-2009, FirmOS Business Solutions GmbH
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

// FirmOS FRE CPU Tools OS Independend CPU/CACHE/THREAD Information/Binding

//TODO: FREEBSD, LINUX, WINDOWS specific implementations

uses FOS_TOOL_INTERFACES,classes;

procedure GetFOS_CPU_TOOL(out CPUTOOL:IFOS_CPU ; var holder: TObject);


implementation

{$IFDEF FREEBSD}
  {$LINKLIB c}
  {$LINKLIB rt}
{$ENDIF}
{$IFDEF UNIX}
  {$IFDEF FREEBSD}
   uses sysutils,unix,sysctl,BaseUnix,initc,xmlread,dom;
  {$ENDIF}
  {$IFDEF DARWIN}
   uses sysutils,unix,sysctl;
  {$ENDIF}
  {$IFDEF LINUX}
   uses sysutils,unix,baseunix;
  {$ENDIF}
  {$IFDEF SOLARIS}
   uses sysutils,unix,baseunix;
  {$ENDIF}
{$ENDIF}


{$IFDEF BSD}
type
 TFOS_QWordArray=Array of QWord;

 function FOS_GetSysctlByName(const name:PChar;var Vals:TFOS_QWordArray;const len:cardinal=10):integer;
 var sz:size_t;
 begin
   setlength(Vals,len);
   sz:=len*sizeof(QWord);
   result:=FPsysctlbyname(name,@vals[0],@sz,nil,0);
   if result=0 then begin
     setlength(vals,sz div sizeof(QWord));
   end;
 end;

 function FOS_GetSysctlByName(const name:PChar):int64;
 var sz:size_t;
     Val:QWord;
 begin
   sz:=sizeof(val);
   val:=0;
   result:=FPsysctlbyname(name,@val,@sz,nil,0);
   if result=0 then begin
     result:=val;
   end else result:=-1;
 end;
{$ENDIF}
{$IFDEF FREEBSD}
 const
   CPU_SETSIZE=128;
   CPU_LEVEL_ROOT = 1;
   CPU_LEVEL_CPUSET = 2;
   CPU_LEVEL_WHICH = 3;
   CPU_WHICH_TID = 1;
   CPU_WHICH_PID = 2;
   CPU_WHICH_CPUSET = 3;
   CPU_WHICH_IRQ = 4;
   CPU_WHICH_JAIL = 5;
   CPUSET_INVALID = -(1);
   CPUSET_DEFAULT = 0;
 type
   cpulevel_t= longint;
   cpuwhich_t= longint;
   id_t      = int64;
   Pcpuset_t = pointer;
   _cpuset = record
     __bits : array[0..3] of longint;
   end;
   cpuset_t = _cpuset;
   function cpuset_getaffinity(_para1:cpulevel_t; _para2:cpuwhich_t; _para3:id_t; _para4:size_t; _para5:Pcpuset_t):longint;cdecl; external 'c';
   function cpuset_setaffinity(_para1:cpulevel_t; _para2:cpuwhich_t; _para3:id_t; _para4:size_t; _para5:Pcpuset_t):longint;cdecl; external 'c';

   function CPU_ISSET(fdno:cint;const nset : cpuset_t):boolean;
   begin
    if (fdno<0) or (fdno>CPU_SETSIZE) then exit(false);
     if ((nset.__bits[fdno shr ln2bitsinword]) and (culong(1) shl ((fdno) and ln2bitmask)))>0 then begin
      result:=true;
     end else begin
      result:=false;
     end;
   end;

   procedure CPU_ZERO(var nset : cpuset_t);
   var i :longint;
   begin
     for i:=0 to high(nset.__bits) do nset.__bits[i]:=0;
   end;

   function CPU_SET(fdno:cint;var nset : cpuset_t): cint;
   begin
     if (fdno<0) or (fdno > CPU_SETSIZE) then exit(-1);
     nset.__bits[fdno shr ln2bitsinword]:=nset.__bits[(fdno) shr ln2bitsinword] OR (culong(1) shl ((fdno) and ln2bitmask));
     result:=0;
   end;

   function get_cpu_count(var count:integer):integer;
   var myset:cpuset_t;
       i:integer;
   begin
    result:=cpuset_getaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, sizeof(myset), @myset);
    if result<>0 then exit;
    count:=0;
    for i:=0 to CPU_SETSIZE do begin
     if CPU_ISSET(i,myset) then inc(count);
    end;
   end;

   function set_to_cpu(const cpuid:integer):integer;
   var myset:cpuset_t;
       i:integer;
   begin
    CPU_ZERO(myset);
    CPU_SET(cpuid,myset);
    result:=cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1, sizeof(myset), @myset);
    if result<>0 then exit;
   end;
{$ENDIF}

type

  { TCPU_TOOL }

  TCPU_TOOL=class(TInterfacedObject,IFOS_CPU)
  private
    fpacks,fcores,flogical,lpp:integer;
    read:boolean;
    procedure _InternalRead;
  public
    procedure TestSetup       (const packages,cores,logical:cardinal);
    function  Bind_to_logical (const logical_id:integer;out error:string):boolean;
    function  Get_CPU_Data    :IFOS_NPS;
    function  Packages        :cardinal;
    function  Cores           :cardinal;
    function  Logical         :cardinal;
  end;

{$IFDEF DARWIN}
procedure TCPU_TOOL._InternalRead;
begin
  if read then exit;
  flogical := FOS_GetSysctlByName('hw.logicalcpu');
  fcores   := FOS_GetSysctlByName('hw.ncpu');
  fpacks   := FOS_GetSysctlByName('hw.packages');
  lpp      := flogical div fpacks;
  //result.setprop('CORE PER PACK',cardinal(FOS_GetSysctlByName('machdep.cpu.cores_per_package')));
  read:=true;
end;
{$ELSE}
{$IFDEF FREEBSD}
procedure TCPU_TOOL._InternalRead;
var
  ac: integer;
  i: integer;
  ssm: TStringstream;
  cpunode: TDOMNode;
  node: TDOMNode;
  topo: TXMLDocument;
  s: string;
  sz: size_t;
  res: integer;
begin
  if read then exit;
  sz:=2048; setlength(s, sz); fcores:=0; fpacks:=0;
  res:=FPsysctlbyname('kern.sched.topology_spec', @s[1], @sz, nil, 0);
  if res=0 then begin
    SetLength(s, sz);
    ssm:=TStringstream.create(s);
    try
      ReadXMLFile(topo, ssm);
      node:=topo.DocumentElement.FindNode('group');
      if not assigned(node) then GFRE_BT.CriticalAbort('freebsd kern.sched.'
        +'topology_spec format unexcpected no > "group" ');
      node:=node.FindNode('children');
      if not assigned(node) then begin
       //GFRE_BT.CriticalAbort('freebsd kern.sched.'+'topology_spec format unexcpected no > "children"');
       fpacks:=1;
       fcores:=1;
      end else begin
       for i:=0 to node.ChildNodes.Count-1 do begin
         cpunode:=node.ChildNodes.Item[i].FindNode('cpu');
         ac:=StrToIntDef(cpunode.Attributes.GetNamedItem('count'
           ).FirstChild.NodeValue, -1);
         if ac=-1 then GFRE_BT.CriticalAbort('freebsd kern.sched.topology_spec '
           +'format unexcpected > cannot fetch cpucount '+inttostr(i));
         inc(fpacks);
         inc(fcores, ac);
       end;
      end;
    finally
      topo.free;
      ssm.free;
    end;
  end else begin
    GFRE_BT.CriticalAbort('freebsd kern.sched.topology_spec format '
      +'unexcpected > sysctl failure %d', [res]);
  end;
  lpp:=fcores div fpacks; // TODO Hyperthreading
  flogical:=lpp*fpacks;
  read:=true;
end;
{$ELSE}
procedure TCPU_TOOL._InternalRead;
begin
   fpacks:=1;
   fcores:=1;
   flogical:=1;
   lpp:=1;
   read:=true;
end;
{$ENDIF}
{$ENDIF}

 procedure TCPU_TOOL.TestSetup(const packages, cores, logical: cardinal);
 begin
   read     :=  true;
   fpacks   :=  packages;
   fcores   :=  cores;
   flogical :=  logical;
 end;

{$IFDEF FREEBSD}
function TCPU_TOOL.Bind_to_logical(const logical_id: integer; out error: string): boolean;
var res:integer;
begin
   res:=set_to_cpu(logical_id);
   if res<>0 then begin
     result:=false;
     error:='<OS ERROR '+inttostr(cerrno)+'>';
   end else begin
     Result:=true;
     error:='';
   end;
end;
{$ELSE}
function TCPU_TOOL.Bind_to_logical(const logical_id: integer; out error: string): boolean;
begin
   result:=false;
   error:='<NOT IMPLEMENTED>'+inttostr(logical_id);
end;
{$ENDIF}

{ TCPU_TOOL }

{$IFDEF DARWIN}
function TCPU_TOOL.Get_CPU_Data: IFOS_NPS;
var i:integer;
    s:string;
    packs:qword;
    data:TFOS_QWordArray;
begin
  GFRE_TF.Get_NPS(result);
  result.setprop('CPU',Cardinal(FOS_GetSysctlByName('hw.ncpu')));
  packs:=FOS_GetSysctlByName('hw.packages');
  result.setprop('PACKS',cardinal(packs));
  lpp:=FOS_GetSysctlByName('machdep.cpu.logical_per_package');
  result.setprop('LOGICAL PER PACK',cardinal(lpp));
  result.setprop('CORE PER PACK',cardinal(FOS_GetSysctlByName('machdep.cpu.cores_per_package')));
  result.setprop('CACHE LINESIZE',FOS_GetSysctlByName('hw.cachelinesize'));
  result.setprop('FRE_THREADS_SIMPLE',cardinal(packs*lpp));
  SetLength(data,0);
  FOS_GetSysctlByName('hw.cachesize',data);
  for i:=0 to length(data)-1 do begin
    if data[i]=0 then break;
    if i=0 then s:='RAM' else s:='L'+inttostr(i);
    s:=s+' SIZE';
    result.setprop(s,data[i]);
  end;
  result.SetProp('CACHES',i-1);
  FOS_GetSysctlByName('hw.cacheconfig',data);
   for i:=0 to length(data)-1 do begin
     if data[i]=0 then break;
     if i=0 then s:='RAM' else s:='L'+inttostr(i);
     s:=s+' CFG';
     result.setprop(s,data[i]);
   end;
   result.SetProp('CACHE CFG',i);
end;
{$ELSE} {$IFDEF FREEBSD}
function TCPU_TOOL.Get_CPU_Data: IFOS_NPS;
var cnt:integer;
    Val:QWord;
begin
   GFRE_TF.Get_NPS(result);
   _InternalRead;
   result.setprop('FRE_THREADS_SIMPLE',longword(fcores));
   result.setprop('PACKS',cardinal(fpacks));
   result.setprop('CORE PER PACK',cardinal(fcores div fpacks));
   result.setprop('LOGICAL PER PACK',cardinal(lpp));
   result.setprop('CPU',longword(cores));
end;
{$ELSE} {$IFDEF LINUX}
function TCPU_TOOL.Get_CPU_Data: IFOS_NPS;
var cnt:integer;
    Val:QWord;
begin
   GFRE_TF.Get_NPS(result);
   _InternalRead;
   result.setprop('FRE_THREADS_SIMPLE',longword(fcores));
   result.setprop('PACKS',cardinal(fpacks));
   result.setprop('CORE PER PACK',cardinal(fcores div fpacks));
   result.setprop('LOGICAL PER PACK',cardinal(lpp));
   result.setprop('CPU',longword(cores));
end;{$ELSE}
function TCPU_TOOL.Get_CPU_Data: IFOS_NPS;
begin
   GFRE_TF.Get_NPS(result);
   result.setprop('FRE_THREADS_SIMPLE',longword(1)); // unoptimized else
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}

function TCPU_TOOL.Packages: cardinal;
begin
  _InternalRead;
  Result:=fpacks;
end;

function TCPU_TOOL.Cores: cardinal;
begin
  _InternalRead;
  result:=fCores;
end;

function TCPU_TOOL.Logical: cardinal;
begin
 _InternalRead;
 result:=flogical;
end;

procedure GetFOS_CPU_TOOL(out CPUTOOL:IFOS_CPU ; var holder: TObject);
begin
  holder  := TCPU_TOOL.Create;
  CPUTOOL := TCPU_TOOL(holder);
end;


end.

