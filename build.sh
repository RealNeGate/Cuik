#! /bin/sh -e
git submodule update --init --recursive

cd main
python build.py
