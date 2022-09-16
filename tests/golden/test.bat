@echo off
setlocal enabledelayedexpansion

set /a tried = 0
set /a passed = 0
    
for %%f in (*.c) do (
    set /a tried += 1

	cuik %%f
    if %ERRORLEVEL% EQU 0 (
        %%~nf > tmp.txt
        fc tmp.txt %%f.gold > tmp2.txt && (
            echo Success! %%f
            set /a passed += 1
        ) || (
            type tmp2.txt
            echo Failed! %%f
        )
    )
)

for %%f in (*.c) do (
    set /a tried += 1

	cuik %%f -O1
    if %ERRORLEVEL% EQU 0 (
        %%~nf > tmp.txt
        fc tmp.txt %%f.gold > tmp2.txt && (
            echo Success! %%f
            set /a passed += 1
        ) || (
            type tmp2.txt
            echo Failed! %%f
        )
    )
)

del *.obj
del *.pdb
del *.exe
del tmp.txt
del tmp2.txt

echo %passed% out of %tried%
pause
