#!/bin/sh
lowercase(){
 echo "$1" | sed "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/"
}    
platform=`lowercase \`uname\``
if [ $platform = "sunos" ]; then 
  platform="solaris"
fi
fpcbin=./fosfpc.sh
fpcversion=`$fpcbin -iV`
fpcos=`$fpcbin -iSO` 
fpccpu=`$fpcbin -iTP`
fpcunitstr="$fpccpu-$fpcos"
fpcunits="-Fu~/fosbuild/fpcbin/$platform/lib/fpc/$fpcversion/units/$fpcunitstr"
fpc_comp="$fpcbin -n $fpcunits/* $fpcunits/rtl -Fl~/fosbuild/fpcbin/$platform/lib/$fpcversion/lib/$fpcunitstr"
#-Fl/Users/helly/fosbuild/fpcbin/darwin/lib/fpc/$FPCVERSION/lib/$FPCTARGET 
#echo $platform
#echo $fpc_comp
#exit
rm -f fpmake_packages 
rm -f fpmake_test 
rm -f fpmake_products 
$fpc_comp fpmake_packages.pas 1>/dev/null
$fpc_comp fpmake_test.pas 1>/dev/null
$fpc_comp fpmake_products.pas 1>/dev/null
rm -f *.ppu 2>/dev/null
rm -f *.o 2>/dev/null
rm -f *.s 2>/dev/null
if [ -f fpmake_packages ]
then
  echo "OK: a new fpmake_packages was built for $fpcos $fpccpu with fpc $fpcversion"
else 
  echo "ERROR: building fpmake_packages failed for $fpcos $fpccpu with fpc $fpcversion" 1>&2
  exit 99
fi
if [ -f fpmake_test ]
then
  echo "OK: a new fpmake_test was built for $fpcos $fpccpu with fpc $fpcversion"
else 
  echo "ERROR: building fpmake_test failed for $fpcos $fpccpu with fpc $fpcversion" 1>&2
  exit 99
fi
if [ -f fpmake_products ]
then
  echo "OK: a new fpmake_products was built for $fpcos $fpccpu with fpc $fpcversion"
else 
  echo "ERROR: building fpmake_products failed for $fpcos $fpccpu with fpc $fpcversion" 1>&2
  exit 99
fi
exit 0