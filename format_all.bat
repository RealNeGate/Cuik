@echo off
for /f %%f in ('dir src /S /B') do clang-format -i %%f
