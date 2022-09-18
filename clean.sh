#! /bin/sh -e
rm -rf bin
rm -rf tilde-backend/bin
rm -rf tilde-backend/tildebackend.a
rm -rf tilde-backend/tildebackend.lib
rm -rf lib/preproc/dfa.h
rm -rf tilde-backend/deps/luajit/src/bin
cd tilde-backend/deps/luajit/src && make clean
