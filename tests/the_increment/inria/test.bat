@echo off
setlocal enabledelayedexpansion

set /a tried = 0
set /a passed = 0

for %%f in (*.c) do (
    set /a tried += 1

	cuik %%f --syntax
    if %ERRORLEVEL% EQU 0 (
        echo Success! %%f
        set /a passed += 1
    ) || (
        echo Failed! %%f
    )
)

echo %passed% out of %tried%
pause
