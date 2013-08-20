#!/bin/sh
#
#env >> envlog.txt
lowercase(){
 echo "$1" | sed "y/ABCDEFGHIJKLMNOPQRSTUVWXYZ/abcdefghijklmnopqrstuvwxyz/"
}    
platform=`lowercase \`uname\``
if [ $platform = "sunos" ]; then 
  platform="solaris"
fi
fos_hostbits=`uname -a`
case $fos_hostbits in
  *SunOS*) #solaris 32/64 bit possible  
	    ${HOME}'/fosbuild/fpcbin/'$platform'/bin/fpc' $@
	    ;;
  *Darwin*)
        ${HOME}'/fosbuild/fpcbin/'$platform'/bin/fpc' $@
        ;;
  *i386*)
        ${HOME}'/fosbuild/fpcbin/'$platform'/bin/ppc386' $@
        ;;
  *i686*)
        ${HOME}'/fosbuild/fpcbin/'$platform'/bin/ppc386' $@
        ;;        
  *x86_64*)
        ${HOME}'/fosbuild/fpcbin/'$platform'/bin/ppcx64' $@
  	;;
  *amd64*)
        ${HOME}'/fosbuild/fpcbin/'$platform'/bin/ppcx64' $@
  	;;  	
  *)     
  	  	 echo "uname -a must contain i386 | x86_64 | amd64 | i686" 
  	  	 exit 1;;
esac
