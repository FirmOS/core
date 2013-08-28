#!/bin/sh
FPC_32_REL_DEF="-O3 -OpPENTIUMM -CpPENTIUMM -Xs -CfSSE3" 
FPC_64_REL_DEF="-O3 -CfSSE3 -Xs"
FPC_32_DEB_DEF="-g -gw -gl" 
FPC_64_DEB_DEF="-g -gw -gl"
case "$1" in
  *static*)
        FPC_LINK="-l"
        ;;
  *dynamic*)
        FPC_LINK=""
  	;;
  *)
        FPC_LINK=""
esac              

case "$1" in
  *i386*)   
        FPC_ARC="-3" 
		case "$1" in
		  *rel*)  
		       FPC_FLAGS="$FPC_32_REL_DEF" 
		       FPC_SFX="rel"
		       ;;
		  *deb*) 
		       FPC_FLAGS="$FPC_32_DEB_DEF" 
		       FPC_SFX="deb"
		       ;;
		  *)   
		       echo "target must contain deb or rel"
		       exit 1 
		       ;;
		esac         
        ;;
  *x86_64*)
        FPC_ARC="-6" 
		case "$1" in
		  *rel*)  
		       FPC_FLAGS="$FPC_64_REL_DEF" 
		       FPC_SFX="rel"
		       ;;
		  *deb*) 
		       FPC_FLAGS="$FPC_64_DEB_DEF" 
		       FPC_SFX="deb"
		       ;;
		  *)   
		       echo "target must contain deb or rel"
		       exit 1 
		       ;;
		esac           	     
  	    ;;
  *)     
  	  	 echo "target must contain i386 or x86_64" 
  	  	 exit 1;;
esac
./param_build.sh $FPC_ARC -b $FPC_SFX -f "$FPC_FLAGS" $FPC_LINK
