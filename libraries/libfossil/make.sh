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

package_configure --static ${EXTRACONF}

unset EXTRACONF

case $SYS_PLATFORM in
    win32)
        package_make -f win/Makefile.mingw -pthread
    ;;
    *)
        package_make CFLAGS=-pthread
    ;;
esac

$SYS_STRIP fossil
cp fossil $SYS_PREFIX/bin
touch $SYS_PREFIX/lib/libfossil.a

if [ ! -f testing ]; then
 package_cleanup
fi


#eof
