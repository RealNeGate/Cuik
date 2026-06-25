@echo off
cuik -link -j /defaultlib:libcmt.lib -out:bin/cuik2.exe bin/objs/static.o bin/objs/common.o bin/objs/perf.o bin/objs/emitter.o bin/objs/lexer.o bin/objs/cpp.o bin/objs/diagnostic.o bin/objs/libcuik.o bin/objs/msvc.o bin/objs/gnu.o bin/objs/darwin.o bin/objs/x64_desc.o bin/objs/aarch64_desc.o bin/objs/mips_desc.o bin/objs/wasm_desc.o bin/objs/libtb.o bin/objs/x64_target.o bin/objs/aarch64_target.o bin/objs/mips_target.o bin/objs/wasm_target.o bin/objs/linker.o bin/objs/main_driver.o bin/objs/freestanding.o
certutil -hashfile bin/cuik2.exe SHA256
