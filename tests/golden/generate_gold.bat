@echo off
setlocal enabledelayedexpansion

for %%f in (*.c) do (
	clang %%f
    a > %%f.gold
)

del a.exe
