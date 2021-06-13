VERSION=2.1.2
VERSION_CONTRIB=2.1.0

PKGURL=http://git.savannah.gnu.org/git/lwip.git
PKGHASH=159e31b6    # 159e31b6 == STABLE-2_1_2_RELEASE
CONTRIBURL=http://git.savannah.gnu.org/git/lwip/lwip-contrib.git
CONTRIBHASH=35b011d # 35b011d == STABLE-2_1_0_RELEASE


# To rebuild the package do:
#
# make -f Makefile package VERSION=$(VERSION) VERSION_CONTRIB=$VERSION_CONTRIB SAVE_IN=$SYS_PREFIXROOT/packages/

if [ ! -f testing ]; then

  contrib_git_file="$SYS_PREFIXROOT/packages/"`basename $CONTRIBURL`"-$CONTRIBHASH.tgz"
  if [ ! -f $contrib_git_file ]; then
    ( package_download  $CONTRIBURL $CONTRIBHASH ) # run in a sub shell - so we are back in this dir
  fi
  package_download $PKGURL $PKGHASH

  package_unpack "$contrib_git_file"
  mv lwip-contrib contrib
  sed -i 's/\r$//' ../*.patch
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
  cd lwip-$VERSION
else
 : #  cd lwip-$VERSION
fi

lwip_build() # build using plain make
{
    echo "Executing task: " "(" $1 ")"
    OSNAME=$(uname | tr '[A-Z]' '[a-z]')
    D0=$2
    cmake_type=$1
    BUILD_TMP=${D0}/tmp
    # rm -rf ${BUILD_TMP}
    NORMALIZED_OSNAME=$OSNAME
    lncc=`echo $SYS_CC| cut -d ' ' -f 1`
    EXTRACONF=""
    # EXTRACONF="$EXTRACONF -DCMAKE_C_COMPILER=${lncc}"
    # EXTRACONF="$EXTRACONF -DCMAKE_CXX_COMPILER=${lncc}"
    lnccflags0=`echo $SYS_CC| cut -d ' ' -f 2-`
    LWIP_ARCH=$SYS_ARCH # $(uname -m)
    case $SYS_PLATFORM in
        linux)
            # EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Linux"
            LWIP_PORT_DEF=""
            LWIP_PORT_TARGET=lwipcontribportunix
            LWIP_PORT_I=contrib/ports/unix/port
            LWIP_PORT=ports/unix;;
        android)
            # EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Android -DCMAKE_ANDROID_API=${ANDROIDAPI} -DCMAKE_ANDROID_STANDALONE_TOOLCHAIN=${android_customtoolchain}"
            LWIP_ARCH_VARIANT=Android
            LWIP_PORT_DEF="-fPIC"
            LWIP_PORT_TARGET=lwipcontribportunix
            LWIP_PORT_I=contrib/ports/unix/port
            LWIP_PORT=ports/unix
            TCCONF="-DCMAKE_TOOLCHAIN_FILE=/usr/local/android-ndk-r20/build/cmake/android.toolchain.cmake -DANDROID_ABI=$ABI -DANDROID_NATIVE_API_LEVEL=${SYS_ANDROIDAPI}" ;;
        win32)
            LWIP_PORT_DEF=""
            EXTRACONF="$EXTRACONF -DCMAKE_SYSTEM_NAME=Win32"
            LWIP_PORT_TARGET=lwipcontribportwindows
            LWIP_PORT_I=contrib/ports/win32
            LWIP_PORT=ports/win32
            cp ${D0}/contrib/examples/example_app/lwipcfg.h.example ${D0}/contrib/ports/win32/include/lwipcfg.h
            EXTRACONF="PCAP_DIR=$SYS_PREFIX"
            ;;
        *) echo lwip/make.sh unhandled target platform '"'$SYS_PLATFORM'"' ;;
    esac
    # Where to place results
    test -f ${D0}/src/include/lwip/lwipopts.h || cp $libdir/lwipopts.h ${D0}/src/include/lwip/
# TODO make ...

# sed -i 's/-Werror//' ${D0}/contrib/ports/Common.allports.mk # for really old compilers
    cp $libdir/BuildMakefile ${D0}/Makefile
    make -C ${D0} LWIPARCH=${LWIP_PORT_I} LWIP_PORT=${LWIP_PORT} LWIP_ARCH_VARIANT=${LWIP_ARCH_VARIANT} CC=${lncc} "LWIP_PORT_DEF=${LWIP_PORT_DEF}" $EXTRACONF
    # install
    cp -ar ${D0}/${LWIP_PORT_I}/include/* $SYS_PREFIX/include/
    cp -ar src/include/* $SYS_PREFIX/include/
    # We need to rename it.
    cp liblwipcommon.a $SYS_PREFIX/lib/liblwipcore.a
}


( lwip_build ${LWIP_BUILD} `pwd` ) || exit 1

# echo warte; exit 1

if [ ! -f testing ]; then
 package_cleanup
fi

unset ABI
unset TCCONF
unset EXTRACONF
unset LWIP_BUILD

#eof
