#! /bin/sh -e
rm -rf bin
rm -rf tilde-backend/bin
rm -rf tilde-backend/tildebackend.a
rm -rf lib/preproc/dfa.h
cd tilde-backend/deps/luajit/src && make clean
rm -rf tilde-backend/deps/luajit/src/bin
