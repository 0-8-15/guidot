VERSION=0.1.4
PKGURL=onetierzero-$VERSION.tar.gz
PKGHASH=85fe04763a9394ca0468de9b5ca678caa190e7ef

if [ ! -f testing ]; then

  package_download $PKGURL $PKGHASH

  (cd libraries/*/* && tar -xzf one*.tar.gz)

  package_patch

fi

EXTRACONF=

TARGET_FEATURES=
case $SYS_PLATFORM in
    win32)
     TARGET_FEATURES="-feature mingw32 -feature windows"
     EXTRACONF="$EXTRACONF OSTYPE=Win32"
     CXXFLAGS="${CXXFLAGS} -D_WIN32"
     ;;
    win64) TARGET_FEATURES="-feature mingw64 -feature windows"
           EXTRACONF="$EXTRACONF OSTYPE=Win64"
           ;;
    android) TARGET_FEATURES="-feature android"
             EXTRACONF="$EXTRACONF OSTYPE=Linux ZT_SANITIZE=0"
             if [ $SYS_ANDROIDAPI -lt 24 ]; then
                 CXXFLAGS="${CXXFLAGS} -D__ANDROID__"
             fi
             ;;
    linux) TARGET_FEATURES="-feature linux"
           EXTRACONF="$EXTRACONF OSTYPE=Linux"
           ;;
    # *) echo unhandled SYS_PLATFORM $SYS_PLATFORM ; exit 1;
esac

# build

ALSO=/home/u/build/ln

OBJECTS="ot0cli.o ot0use.o ot0.o ot0-hooks.o"

# Remove install/rename artifact
rm ${SYS_PREFIX}/lib/libzerotiercore.a

package_make ${SYS_PREFIX}/lib/libzerotiercore.a $OBJECTS TARGET_FEATURES=\"$TARGET_FEATURES \" "$EXTRACONF SYS_PREFIX=${SYS_PREFIX} SYS_HOSTPREFIX=${SYS_HOSTPREFIX} SYS_ARCH=${SYS_ARCH}" CXXFLAGS=\'${CXXFLAGS}\' ALSO=$ALSO

$SYS_AR qs ${SYS_PREFIX}/lib/libzerotiercore.a $OBJECTS

# install

# Install install/rename artifact
ln -sf ${SYS_PREFIX}/lib/libzerotiercore.a ${SYS_PREFIX}/lib/libonetierzero.a

# NOQUIET=yes package_make PLATFORM=$SYS_HOSTPLATFORM PREFIX=$SYS_PREFIX $EXTRACONF install

# cp libzerotier.a $SYS_PREFIX/lib
# cp ot0 $SYS_PREFIX/bin

test -d $SYS_PREFIX/lib/onetierzero || mkdir $SYS_PREFIX/lib/onetierzero
cp -a ${OBJECTS} $SYS_PREFIX/lib/onetierzero
cp -a libraries/ot0/onetierzero/bindings/* $SYS_PREFIX/lib/onetierzero
cp -a libraries/ot0/onetierzero/bindings/*.h $SYS_PREFIX/include
cp -a ot0cli.c ot0use.c ot0.c *.o src $SYS_PREFIX/lib/onetierzero

unset TARGET_FEATURES

if [ ! -f testing ]; then
  package_cleanup
fi

#eof
