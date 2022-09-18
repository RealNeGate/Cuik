@echo off

set opts=-FC -GR- -EHa- -nologo -Zi
set code=%cd%
pushd bin\
cl %opts% %code%\clang -Febin\
popd
