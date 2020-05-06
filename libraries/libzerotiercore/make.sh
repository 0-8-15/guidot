# just a stub for now
echo stub libzerotiercore

if [ ! -f $SYS_PREFIX/lib/libzerotiercore.a ]; then
    cp ~/traces/onetierzero/libzerotiercore.a $SYS_PREFIX/lib/
    mkdir -p $SYS_PREFIX/include/zerotiercore
    cp -a ~/traces/onetierzero/include/*.h $SYS_PREFIX/include/zerotiercore
fi
