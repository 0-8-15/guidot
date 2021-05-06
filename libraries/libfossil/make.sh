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

package_configure --host=${SYS_ARCH} --static ${EXTRACONF}

# package_configure ${EXTRACONF}

sed -i '3i#define FOSSIL_OMIT_DNS 1' autoconfig.h # TBD: likely no longer useful:

unset EXTRACONF

package_make CFLAGS=-pthread

# build static library without main entry
# note: this does not yet work when compiled at window
sed -i  '/FOSSIL_FUZZ/s/$/||1/' src/main.c
rmifexists bld/main.o bld/main_.c
rmifexists $SYS_PREFIX/lib/libfossil.a

# export SYS_AR SYS_PREFIX; echo warte in fossil build auf edit; pwd; bash -i

case $SYS_PLATFORM in
    win32)
        cp fossil.exe $SYS_PREFIX/bin/fossil-new.exe
        # argv is not passed as wide char from Gambit
        sed -i  '/defined(BROKEN_MINGW_CMDLINE)/s/$/ || 1\
#elif 1\
/' src/main.c
        package_make bld/main.o
        assertfile bld/main.o
        $SYS_AR crs $SYS_PREFIX/lib/libfossil.a bld/*.o
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

# export SYS_AR SYS_PREFIX; echo warte in fossil build; pwd; bash -i

if [ ! -f testing ]; then
 package_cleanup
fi


#eof
