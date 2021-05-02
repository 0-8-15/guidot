PKGURL=https://fossil-scm.org/home/tarball/0af47722/Fossil-0af4772245.tar.gz
PKGHASH=c2e169750e78daead40ac04095b9ffd32408e223

# To rebuild the package do:
#
# make -f Makefile package VERSION=$(VERSION) VERSION_CONTRIB=$VERSION_CONTRIB SAVE_IN=$SYS_PREFIXROOT/packages/

if [ ! -f testing ]; then

  package_download $PKGURL $PKGHASH

  package_patch

else
    echo "Be sure it's patched!"
fi

# FIXME
EXTRACONF="--with-openssl=none --json"

EXTRACONF="${EXTRACONF} --with-zlib=${SYS_PREFIX}"
#EXTRACONF="${EXTRACONF} --with-miniz=1"


package_configure --static ${EXTRACONF}

# package_configure ${EXTRACONF}

sed -i '3i#define FOSSIL_OMIT_DNS 1' autoconfig.h

unset EXTRACONF

case $SYS_PLATFORM in
    win32)
        make -f win/Makefile.mingw \
                     CFLAGS=-pthread MINGW_IS_32BIT_ONLY=1 RCC="$SYS_WINDRES -I src -I ${SYS_PREFIX}/include" TCC="$SYS_CC -I ${SYS_PREFIX}/include -L${SYS_PREFIX}/lib" || true
    ;;
    *)
        package_make CFLAGS=-pthread
    ;;
esac

# build static library without main entry
sed -i  '/FOSSIL_FUZZ/s/$/||1/' src/main.c
rm -f $SYS_PREFIX/lib/libfossil.a

case $SYS_PLATFORM in
    win32)
        make -f win/Makefile.mingw MINGW_IS_32BIT_ONLY=1 \
             TCC="$SYS_CC -I ${SYS_PREFIX}/include -L${SYS_PREFIX}/lib" \
             wbld/main.o
        $SYS_AR crs $SYS_PREFIX/lib/libfossil.a wbld/*.o
    ;;
    *)
        $SYS_STRIP fossil
        cp fossil $SYS_PREFIX/bin
        make bld/main.o
        $SYS_AR crs $SYS_PREFIX/lib/libfossil.a bld/*.o
    ;;
esac

asserterror $?
assertfile $SYS_PREFIX/lib/libfossil.a

# export SYS_AR SYS_PREFIX; echo warte in fossil build; bash -i

if [ ! -f testing ]; then
 package_cleanup
fi


#eof
