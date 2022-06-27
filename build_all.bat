@echo off
clang compile.c && a.exe

wsl gcc compile.c
wsl ./a.out

