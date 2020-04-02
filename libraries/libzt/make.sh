VERSION=20200115
PKGURL=scp://pluto.softeyes.net/libzt-$VERSION-source.tar.gz
PKGHASH=abd6db483f197dcac2dbb46efd6c9eac70b3c720

#LIBZT_BUILD=release
LIBZT_BUILD=debug

if [ ! -f testing ]; then

  package_download $PKGURL $PKGHASH

  (cd libzt*/ext/lwip-contrib;  patch -p1 < ../lwip-contrib.patch)
  # (cd libzt*/ext/lwip;  patch -p1 < ../lwip.patch)

else
    echo "Be sure it's patched!"
fi

EXTRACONF=
TCCONF=
ABI=armeabi-v7a

# lncc=`echo $SYS_CC| cut -d ' ' -f 1`

# EXTRACONF="$EXTRACONF CC=$lncc LIBRARIAN=$SYS_AR"

# EXTRACONF="$EXTRACONF CHICKEN_TARGET_INCLUDE='-I$SYS_PREFIX/include/chicken$LIBASKEMOS_CHICKEN_VERSION -I$SYS_PREFIX/include'"

# build

if [ -f testing ]; then
  cd libzt
else
  cd libzt-$VERSION
fi

libzt_build() # Essentially a copy from libzt/dist.sh function host()`
{
    echo "Executing task: " "(" $1 ")"
    set -x
    OSNAME=$(uname | tr '[A-Z]' '[a-z]')
    D0=$2
    cmake_type=$1
    BUILD_TMP=${D0}/tmp
    # rm -rf ${BUILD_TMP}
    NORMALIZED_OSNAME=$OSNAME
    case $OSNAME in
        *"darwin"* )
            DYNAMIC_LIB_NAME="libzt.dylib"
            NORMALIZED_OSNAME="macos"
            ;;
        *"linux"* )
            DYNAMIC_LIB_NAME="libzt.so"
            ;;
    esac
    lncc=`echo $SYS_CC| cut -d ' ' -f 1`
    EXTRACONF="$EXTRACONF -DCMAKE_C_COMPILER=${lncc}"
    EXTRACONF="$EXTRACONF -DCMAKE_CXX_COMPILER=${lncc}"
    lnccflags0=`echo $SYS_CC| cut -d ' ' -f 2-`
    ZT_ARCH=$SYS_ARCH # $(uname -m)
    case $SYS_PLATFORM in
        linux) EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Linux" ;;
        android)
            EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Android -DCMAKE_ANDROID_API=${ANDROIDAPI} -DCMAKE_ANDROID_STANDALONE_TOOLCHAIN=${android_customtoolchain}"
            TCCONF="-DCMAKE_TOOLCHAIN_FILE=/usr/local/android-ndk-r20/build/cmake/android.toolchain.cmake -DANDROID_ABI=$ABI -DANDROID_NATIVE_API_LEVEL=${SYS_ANDROIDAPI}" ;;
        win32) EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Win32" ;;
        *) echo libzt/make.sh unhandled target platform '"'$SYS_PLATFORM'"' ;;
    esac
    # CMake build files
    BUILD_DIR=${D0}/tmp/${NORMALIZED_OSNAME}-${ZT_ARCH}-${cmake_type}
    echo mkdir -p $BUILD_DIR
    # Where to place results
    BIN_OUTPUT_DIR=${D0}/bin/${cmake_type}/${NORMALIZED_OSNAME}-${ZT_ARCH}
    mkdir -p $BIN_OUTPUT_DIR
    rm -rf $BIN_OUTPUT_DIR/*
    LIB_OUTPUT_DIR=${D0}/lib/${cmake_type}/${NORMALIZED_OSNAME}-${ZT_ARCH}
    mkdir -p $LIB_OUTPUT_DIR
    rm -rf $LIB_OUTPUT_DIR/libzt.a $LIB_OUTPUT_DIR/$DYNAMIC_LIB_NAME $LIB_OUTPUT_DIR/libztcore.a
    # Build
    # lnccflags0="${lnccflags0} -DSA_FAMILY_T_DEFINED"
    cmake -H. -B$BUILD_DIR $TCCONF $EXTRACONF -DCMAKE_INSTALL_PREFIX=$SYS_PREFIX \
          "-DCMAKE_C_FLAGS_INIT=${lnccflags0}" "-DCMAKE_CXX_FLAGS_INIT=${lnccflags0}" \
          -DCMAKE_VERBOSE_MAKEFILE=TRUE -DCMAKE_BUILD_TYPE=${cmake_type} \
    # echo Time to check;  exit 1
    case $SYS_PLATFORM in
    android)
        find $BUILD_DIR -name link.txt -exec sed -i "-es/-lpthread//g" '{}' \;
        ;;
    esac
    cmake --build $BUILD_DIR $BUILD_CONCURRENCY
    # Move and clean up
    # mv $BUILD_DIR/bin/* $BIN_OUTPUT_DIR
    # mv $BUILD_DIR/lib/* $LIB_OUTPUT_DIR
}

( libzt_build ${LIBZT_BUILD} `pwd` ) || exit 1

# install

if [ -f tmp/*-${SYS_ARCH}-${LIBZT_BUILD}/libzt.a ]; then
    cp tmp/*-${SYS_ARCH}-${LIBZT_BUILD}/libzt.a $SYS_PREFIX/lib
    cp tmp/*-${SYS_ARCH}-${LIBZT_BUILD}/libztcore.a $SYS_PREFIX/lib
else
    cp tmp/*-${SYS_ARCH}-${LIBZT_BUILD}/lib/libzt.a $SYS_PREFIX/lib
    cp tmp/*-${SYS_ARCH}-${LIBZT_BUILD}/lib/libztcore.a $SYS_PREFIX/lib
fi
cp include/ZeroTier*.h $SYS_PREFIX/include

if [ ! -f testing ]; then
  package_cleanup
fi

unset ABI
unset TCCONF
unset EXTRACONF

#eof
