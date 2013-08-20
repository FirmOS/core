1) copy the universal dylibs to /opt/local/fre/lib/
2) changemod the DIR !

in ~/.profile:
export DYLD_LIBRARY_PATH=/opt/local/fre/lib/

source it, or relogin (source ~/.profile)
