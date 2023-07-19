@echo off
setlocal enabledelayedexpansion

set APP=windaynight

if not exist bin\ mkdir bin
pushd bin

rem Thanks to @mmozeiko for the gist on how to minimise linking to the CRT:
rem - https://gist.github.com/mmozeiko/81e9c0253cc724638947a53b826888e9

set CL=/nologo /W3 /WX /TC /UTF-8
set LINK=/incremental:no /subsystem:windows kernel32.lib shell32.lib user32.lib advapi32.lib

if "%1" equ "debug" (
	set CL=!CL! /MTd /Od /Zi /RTC1 /fsanitize=address /D_DEBUG
	set LINK=!LINK! libucrtd.lib
) else (
	set CL=!CL! /O2 /GL /GS- /DNDEBUG
	set LINK=!LINK! /fixed /opt:icf /opt:ref libucrt.lib libvcruntime.lib
)

call rc.exe /nologo ..\!APP!.rc
call cl.exe ..\!APP!.c /link ..\!APP!.res

del *.obj >nul
del ..\!APP!.res

popd
endlocal
