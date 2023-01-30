cd deps/luajit/src
make CC=clang BUILDMODE=static -j4

mkdir -p bin
cd bin

ar -x ../libluajit.a
