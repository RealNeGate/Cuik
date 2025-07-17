@echo off
cuik %1 -O
clang %1 -g -o b.exe

echo Run Cuik
a.exe > foo.txt

echo Run Clang
b.exe > bar.txt

git diff --no-index foo.txt bar.txt
echo Done

