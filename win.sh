#!/usr/bin/env bash
export WindowsSDKDir=/home/negate/Desktop/Workspace/WinSDK
export WindowsSDKVersion=10.0.22621.0
export VCToolsInstallDir=/home/negate/Desktop/Workspace/MSVC

# clang-cl -I ~/Desktop/Workspace/WinSDK/Include/10.0.22621.0/ucrt -I ~/Desktop/Workspace/WinSDK/Include/10.0.22621.0/um -I ~/Desktop/Workspace/WinSDK/Include/10.0.22621.0/shared $@
# ~/Desktop/Workspace/Cuik/bin/cuik -target x64_windows_msvc $@
~/Desktop/Workspace/Cuik/bin/cuik -link lapi.obj lcode.obj ldblib.obj ldump.obj linit.obj lmathlib.obj lobject.obj lparser.obj lstrlib.obj ltm.obj lundump.obj lzio.obj lauxlib.obj lcorolib.obj ldebug.obj lfunc.obj liolib.obj lmem.obj lopcodes.obj lstate.obj ltable.obj lutf8lib.obj lbaselib.obj lctype.obj ldo.obj lgc.obj llex.obj loadlib.obj loslib.obj lstring.obj ltablib.obj lua.obj lvm.obj -j /out:lua.exe
# lld-link --time-trace-granularity=0 --time-trace /libpath:../WinSDK/Lib/10.0.22621.0/um/x64 /libpath:../WinSDK/Lib/10.0.22621.0/ucrt/x64 /libpath:../MSVC/lib/x64 lapi.obj lcode.obj ldblib.obj ldump.obj linit.obj lmathlib.obj lobject.obj lparser.obj lstrlib.obj ltm.obj lundump.obj lzio.obj lauxlib.obj lcorolib.obj ldebug.obj lfunc.obj liolib.obj lmem.obj lopcodes.obj lstate.obj ltable.obj lutf8lib.obj lbaselib.obj lctype.obj ldo.obj lgc.obj llex.obj loadlib.obj loslib.obj lstring.obj ltablib.obj lua.obj lvm.obj /out:lua2.exe


