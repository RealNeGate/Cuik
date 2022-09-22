@echo off

IF NOT EXIST mimalloc\out (
    call vcvars64

    cd mimalloc
    mkdir out
    cd out

    cmake ../ -DMI_BUILD_STATIC=OFF -DMI_BUILD_OBJECT=OFF -DMI_BUILD_TESTS=OFF
    msbuild libmimalloc.sln -p:Configuration=Release
    
    cd ../..
    
    copy mimalloc\out\Release\mimalloc.dll bin\
    copy mimalloc\out\Release\mimalloc-redirect.dll bin\
)
