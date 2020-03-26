VERSION=2.1.2
VERSION_CONTRIB=2.1.0
# PKGURL=http://download.savannah.nongnu.org/releases/lwip/lwip-$VERSION.zip
PKGURL=lwip-$VERSION.tar.gz
PKGHASH=8729119666234b74df299c043e4d419223933bc6

# To rebuild the package do:
#
# make -f Makefile package VERSION=$(VERSION) VERSION_CONTRIB=$VERSION_CONTRIB SAVE_IN=$SYS_PREFIXROOT/packages/

if [ ! -f testing ]; then

  package_download $PKGURL $PKGHASH

  package_patch

else
    echo "Be sure it's patched!"
fi

LWIP_BUILD=debug
EXTRACONF=
TCCONF=
ABI=armeabi-v7a

# lncc=`echo $SYS_CC| cut -d ' ' -f 1`

# EXTRACONF="$EXTRACONF CC=$lncc LIBRARIAN=$SYS_AR"

# EXTRACONF="$EXTRACONF CHICKEN_TARGET_INCLUDE='-I$SYS_PREFIX/include/chicken$LIBASKEMOS_CHICKEN_VERSION -I$SYS_PREFIX/include'"

# build

if [ -f testing ]; then
  cd lwip
else
 : #  cd lwip-$VERSION
fi

linkintoone()
{
  mkdir .tmp
  cd .tmp
  $SYS_AR -x $1/$3
  $SYS_AR r $1/$2 *.o
}

lwip_build() # trick the abdomination going by the name cmake into compiling the thing
{
    echo "Executing task: " "(" $1 ")"
    OSNAME=$(uname | tr '[A-Z]' '[a-z]')
    D0=$2
    cmake_type=$1
    BUILD_TMP=${D0}/tmp
    # rm -rf ${BUILD_TMP}
    NORMALIZED_OSNAME=$OSNAME
    case $OSNAME in
        *"darwin"* )
            DYNAMIC_LIB_NAME="lwip.dylib"
            NORMALIZED_OSNAME="macos"
            ;;
        *"linux"* )
            DYNAMIC_LIB_NAME="lwip.so"
            ;;
    esac
    lncc=`echo $SYS_CC| cut -d ' ' -f 1`
    EXTRACONF="$EXTRACONF -DCMAKE_C_COMPILER=${lncc}"
    EXTRACONF="$EXTRACONF -DCMAKE_CXX_COMPILER=${lncc}"
    lnccflags0=`echo $SYS_CC| cut -d ' ' -f 2-`
    LWIP_ARCH=$SYS_ARCH # $(uname -m)
    case $SYS_PLATFORM in
        linux)
            EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Linux"
            LWIP_PORT_TARGET=lwipcontribportunix
            LWIP_PORT_I=contrib/ports/unix/port
            LWIP_PORT=contrib/ports/unix;;
        android)
            EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Android -DCMAKE_ANDROID_API=${ANDROIDAPI} -DCMAKE_ANDROID_STANDALONE_TOOLCHAIN=${android_customtoolchain}"
            LWIP_PORT_TARGET=lwipcontribportunix
            LWIP_PORT_I=contrib/ports/unix/port
            LWIP_PORT=contrib/ports/unix
            TCCONF="-DCMAKE_TOOLCHAIN_FILE=/usr/local/android-ndk-r20/build/cmake/android.toolchain.cmake -DANDROID_ABI=$ABI -DANDROID_NATIVE_API_LEVEL=${SYS_ANDROIDAPI}" ;;
        win32)
            EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Win32"
            LWIP_PORT_TARGET=lwipcontribportwindows
            LWIP_PORT_I=contrib/ports/win32
            LWIP_PORT=contrib/ports/win32 ;;
        *) echo lwip/make.sh unhandled target platform '"'$SYS_PLATFORM'"' ;;
    esac
    # CMake build files
    BUILD_DIR=${D0}/tmp/${NORMALIZED_OSNAME}-${LWIP_ARCH}-${cmake_type}
    echo mkdir -p $BUILD_DIR
    # Where to place results
    BIN_OUTPUT_DIR=${D0}/bin/${cmake_type}/${NORMALIZED_OSNAME}-${LWIP_ARCH}
    mkdir -p $BIN_OUTPUT_DIR
    rm -rf $BIN_OUTPUT_DIR/*
    LIB_OUTPUT_DIR=${D0}/lib/${cmake_type}/${NORMALIZED_OSNAME}-${LWIP_ARCH}
    mkdir -p $LIB_OUTPUT_DIR
    rm -rf $LIB_OUTPUT_DIR/lwip.a $LIB_OUTPUT_DIR/$DYNAMIC_LIB_NAME $LIB_OUTPUT_DIR/lwipcore.a
    # Prepare cmake - maybe that should be a patch?
    grep -q contrib CMakeCache.txt || echo 'include(${LWIP_PORT}/Filelists.cmake)' >> CMakeLists.txt
    # Build
    # lnccflags0="${lnccflags0} -DSA_FAMILY_T_DEFINED"
    cmake -H. -B$BUILD_DIR $TCCONF $EXTRACONF -DCMAKE_INSTALL_PREFIX=$SYS_PREFIX \
          "-DCMAKE_C_FLAGS_INIT=${lnccflags0} -I${D0}/src/include -I ${D0}/${LWIP_PORT_I}/include" \
          -DLWIP_CONTRIB_DIR=contrib -DLWIP_PORT=${LWIP_PORT} \
          -DCMAKE_VERBOSE_MAKEFILE=TRUE -DCMAKE_BUILD_TYPE=${cmake_type} \
    # echo Time to check;  exit 1
    case $SYS_PLATFORM in
    android)
        find $BUILD_DIR -name link.txt -exec sed -i "-es/-lpthread//g" '{}' \;
        ;;
    esac
    test -f ${D0}/src/include/lwip/lwipopts.h || cp $libdir/lwipopts.h ${D0}/src/include/lwip/
    cmake --build $BUILD_DIR $BUILD_CONCURRENCY  --target lwipcore
    cmake --build $BUILD_DIR $BUILD_CONCURRENCY  --target ${LWIP_PORT_TARGET}
    rmifexists .tmp
    ( linkintoone $BUILD_DIR liblwipcore.a lib${LWIP_PORT_TARGET}.a ) || exit 1
    rm -rf .tmp
    # install: half of it!
    cp -ar ${D0}/${LWIP_PORT_I}/include/* $SYS_PREFIX/include/
    # Move and clean up
    # mv $BUILD_DIR/bin/* $BIN_OUTPUT_DIR
    # mv $BUILD_DIR/lib/* $LIB_OUTPUT_DIR
}

( lwip_build ${LWIP_BUILD} `pwd` ) || exit 1

# install

if [ -f tmp/*-${SYS_ARCH}-${LWIP_BUILD}/liblwipcore.a ]; then
    cp tmp/*-${SYS_ARCH}-${LWIP_BUILD}/liblwipcore.a $SYS_PREFIX/lib
else
    cp tmp/*-${SYS_ARCH}-${LWIP_BUILD}/liblwipcore.a $SYS_PREFIX/lib
fi

cp -ar src/include/* $SYS_PREFIX/include/

# echo warte; exit 1

if [ ! -f testing ]; then
 package_cleanup
fi

unset ABI
unset TCCONF
unset EXTRACONF
unset LWIP_BUILD

#eof
