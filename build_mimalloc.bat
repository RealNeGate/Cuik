@echo off
call vcvars64

IF NOT EXIST deps\mimalloc\out (
    cd deps/mimalloc
    mkdir out
    cd out

    cmake ../ -DMI_BUILD_STATIC=OFF -DMI_BUILD_OBJECT=OFF -DMI_BUILD_TESTS=OFF
    msbuild libmimalloc.sln -p:Configuration=Release

    copy deps\mimalloc\out\Release\mimalloc.dll bin\
    copy deps\mimalloc\out\Release\mimalloc-redirect.dll bin\
)
