@echo off

IF NOT EXIST deps\luajit\src\lua51.lib (
    cd deps\luajit\src
    msvcbuild.bat static
)
