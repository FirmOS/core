#!/bin/sh
usage()
{
  echo "usage: $0 options"
  echo "makes a build/cycle of firmos software"
  echo "OPTIONS:" 
  echo " -h Show this help"
  echo " -3 Enable 32 Bit build" 
  echo " -6 Enable 64 Bit build" 
  echo " -f param : Passes params as extra compiler options to fpmake"
  echo " -b param : Passes buildsuffix option to fpmake"
  echo " -l : link static"
  exit
}

lstatic=""

while getopts "36hlf:b:" opt; do
  case $opt in
    3)
      build32=1
      ;;
    6)
      build64=1
      ;;      
    f) 
      flags="$OPTARG"
      ;;
    b)
      bsfx=$OPTARG
      ;;
    l)
      lstatic="--fosstatic=true"
      ;; 
    h)
      usage
      ;;
  esac
done
shift $(($OPTIND - 1))
if [ ! "$build32" ]; then   
  if [ ! "$build64" ]; then    
    build32=1
    build64=1
    #echo "ERROR: at least one 32 bit or one 64 build must be enabled (-3 -6)"
    #exit 1
  fi
fi
if [ "$flags" ]; then
  cflags=--options=$flags
fi
if [ "$bsfx" ]; then
  buildsuf=--buildsuffix=$bsfx
fi

if [ ! "$cflags" ]; then  
  cflags=-v
fi
if [ ! "$buildsuf" ]; then  
  buildsuf=--buildsuffix=deb
fi

#echo fpmake install $cflags $buildsuf
echo "--> Makeing fpmake files"
./makefpmake.sh
echo "--> Building "$cflags" "$buildsuf" "$lstatic""
#env
if [ "$build32" = 1 ]; then 
  ./fpmake_packages -C i386 -dc 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Clean fpmake_packages (32)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Clean fpmake_packages (32)" 1>&2  && exit 1
  ./fpmake_packages -C i386 install -v "$cflags" "$buildsuf" $lstatic 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Packages built.    32Bit : $cflags $buildsuf $lstatic" 
  [ $RETVAL -ne 0 ] && echo "FAIL: No Packages built. 32Bit : $cflags $buildsuf $lstatic"  1>&2 && exit 1
  
  ./fpmake_test -C i386 -dc 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Clean fpmake_test (32)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Clean fpmake_packages (32)" 1>&2 && exit 1
  ./fpmake_test -C i386 install -v "$cflags" "$buildsuf" $lstatic 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Tests built. (32)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Tests built. (32)"  1>&2  && exit 1
    
fi
if [ "$build64" = "1" ]; then 
  ./fpmake_packages -C x86_64 -dc 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Clean fpmake_packages (64)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Clean fpmake_packages (64)" 1>&2  && exit 1
  ./fpmake_packages -C x86_64 install -v "$cflags" "$buildsuf" $lstatic 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Packages built. (64)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Packages built. (64)"  1>&2  && exit 1
  
  ./fpmake_test -C x86_64 -dc 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Clean fpmake_test (64)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Clean fpmake_test (64)" 1>&2  && exit 1
  ./fpmake_test -C x86_64 install -v "$cflags" "$buildsuf" $lstatic 1>>full_build_log.txt
  RETVAL=$?
  [ $RETVAL -eq 0 ] && echo "OK:   Tests built. (64)"
  [ $RETVAL -ne 0 ] && echo "FAIL: Tests built. (64)"  1>&2  && exit 1
  
fi  
echo "--> Building Done"
exit 0
