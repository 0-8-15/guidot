VERSION=0.9.5.9
PKGURL=http://gitlab.com/askemos/ball-zero/askemos-$VERSION.tar.gz
PKGHASH=4d796c3aef48b473efd3b955ea523fe5955be713

if [ ! -f testing ]; then

  package_download $PKGURL $PKGHASH

  package_patch

fi

LIBASKEMOS_CHICKEN_VERSION=4

EXTRACONF=

lncc=`echo $SYS_CC| cut -d ' ' -f 1`

EXTRACONF="$EXTRACONF CC=$lncc LIBRARIAN=$SYS_AR"

EXTRACONF="$EXTRACONF CHICKEN_TARGET_INCLUDE='-I$SYS_PREFIX/include/chicken$LIBASKEMOS_CHICKEN_VERSION -I$SYS_PREFIX/include'"

# qrcodegen uses 'for' loop initial declarations, which are only
# allowed in c99 or c11 mode.
EXTRACONF="$EXTRACONF CFLAGS='-std=gnu11 -DPCRE_STATIC -fPIC'"

TARGET_FEATURES=
case $SYS_PLATFORM in
    win32)
     TARGET_FEATURES="-feature mingw32 -feature windows"
     EXTRACONF="$EXTRACONF CFLAGS+=-D_POSIX_SOURCE"
     ;;
    win64) TARGET_FEATURES="-feature mingw64 -feature windows" ;;
    android) TARGET_FEATURES="-feature android" ;;
    linux) TARGET_FEATURES="-feature linux" ;;
    # *) echo unhandled SYS_PLATFORM $SYS_PLATFORM ; exit 1;
esac

EXTRA_FEATURES="-feature single-binary -feature never-fork"

if [ $SYS_PLATFORM != $SYS_HOSTPLATFORM ]; then
    CSC="$SYS_HOSTPREFIX/bin/$SYS_ARCH-csc$LIBASKEMOS_CHICKEN_VERSION"
else
    CSC="$SYS_HOSTPREFIX/bin/csc$LIBASKEMOS_CHICKEN_VERSION"
fi

# build

if [ -f testing ]; then
  cd askemos-$VERSION/chicken
else
  cd chicken
fi

ln -sf $SYS_PREFIX/include sqlite

package_make libaskemos.a NAME=askemos DBLIBS='' CSC=\"$CSC $TARGET_FEATURES $EXTRA_FEATURES\" "$EXTRACONF"

# install

# NOQUIET=yes package_make PLATFORM=$SYS_HOSTPLATFORM PREFIX=$SYS_PREFIX $EXTRACONF install

cp libaskemos.a $SYS_PREFIX/lib

unset LIBASKEMOS_CHICKEN_VERSION TARGET_FEATURES

if [ ! -f testing ]; then
  package_cleanup
fi

#eof
