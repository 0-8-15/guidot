PKGURL=http://code.call-cc.org/releases/4.13.0/chicken-4.13.0.tar.gz
PKGHASH=ebbef7206f7f2faa3ac430a8c1e50f841d5db23e

PROGRAM_SUFFIX=4

EXTRACONF="PROGRAM_SUFFIX=$PROGRAM_SUFFIX"

# FIXME for mingw32
# cd /usr/i686-w64-mingw32/include && ln -s malloc.h alloca.h

package_download $PKGURL $PKGHASH
package_patch

rmifexists $SYS_PREFIX/include/chicken

C_PLATFORM() {
  case $1 in
    win32) echo cross-linux-mingw ;;
    # win32) echo mingw ;;
    linux) echo linux ;;
    *) echo $SYS_PLATFORM ;;
  esac
}

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" ]; then
  EXTRACONF="$EXTRACONF ARCH='' HOSTSYSTEM=$SYS_ARCH"
fi

lncc=`echo $SYS_CC| cut -d ' ' -f 1`

EXTRACONF="$EXTRACONF C_COMPILER=$lncc LIBRARIAN=$SYS_AR"

EXTRACONF="$EXTRACONF PREFIX=$SYS_PREFIX"

if [ "$SYS_PLATFORM" = android ]; then
    # This is just about crazy: C_COMPILER_STATIC_OPTIONS is normally empty.
    EXTRACONF="$EXTRACONF C_COMPILER_STATIC_OPTIONS=-fPIC"
else
    EXTRACONF="$EXTRACONF STATICBUILD=1"
fi

TARGET_FEATURES=
case $SYS_PLATFORM in
    win32) TARGET_FEATURES="-feature mingw32" ;;
    android) TARGET_FEATURES="-feature android" ;;
    linux) TARGET_FEATURES="-feature linux" ;;
    # *) echo unkhandled SYS_PLATFORM $SYS_PLATFORM ; exit 1;
esac

# configure and build

make PLATFORM=`C_PLATFORM $SYS_PLATFORM` confclean

NOQUIET=yes package_make PLATFORM=`C_PLATFORM $SYS_PLATFORM` TARGET_FEATURES=\"$TARGET_FEATURES\" $EXTRACONF

# install

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" ]; then
    NOQUIET=yes package_make PLATFORM=`C_PLATFORM $SYS_PLATFORM` $EXTRACONF install-dev
else
    NOQUIET=yes package_make PLATFORM=`C_PLATFORM $SYS_PLATFORM` $EXTRACONF install
fi

#  mkdir $SYS_PREFIX/include/chicken
#  cp chicken.h chicken-config.h $SYS_PREFIX/include/chicken

# echo waiting in `pwd` after building the target libraries ; bash || exit 1

# configure and build the cross compiler

EXTRACONF="PROGRAM_SUFFIX=$PROGRAM_SUFFIX"

# EXTRACONF="$EXTRACONF STATICBUILD=1"

if [ "$SYS_PLATFORM" != "$SYS_HOSTPLATFORM" ]; then

  vecho "Bulding cross compiler from ${SYS_HOST_PLATFORM} to ${SYS_PLATFORM}"

  # package_download $PKGURL $PKGHASH
  # package_patch

  make PLATFORM=`C_PLATFORM $SYS_PLATFORM` confclean

  EXTRACONF="$EXTRACONF TARGET_C_COMPILER=$lncc TARGET_CXX_COMPILER=${lncc}++"

  EXTRACONF="$EXTRACONF TARGETSYSTEM=$SYS_ARCH PROGRAM_PREFIX=$SYS_ARCH- PREFIX=$SYS_HOSTPREFIX TARGET_PREFIX=$SYS_PREFIX TARGET_RUN_PREFIX=$SYS_PREFIX"

  make PLATFORM=`C_PLATFORM $SYS_HOSTPLATFORM` $EXTRACONF TARGET_FEATURES=\"$TARGET_FEATURES\"
  make PLATFORM=`C_PLATFORM $SYS_HOSTPLATFORM` $EXTRACONF install

  # echo waiting in `pwd` after building the target libraries ; bash || exit 1

else
  vecho "No cross compiler since '${SYS_HOST_PLATFORM}' matches '${SYS_PLATFORM}'"
fi

# echo waiting in `pwd` after building CHICKEN ; bash || exit 1

unset PROGRAM_SUFFIX
unset EXTRACONF

package_cleanup

#eof
