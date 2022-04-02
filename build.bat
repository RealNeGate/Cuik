@echo off

clang compile.c -o compile.exe
if ERRORLEVEL 1 exit /b 1

compile
if ERRORLEVEL 1 exit /b 1
