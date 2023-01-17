import glob
import os
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles TB')
parser.add_argument('--usetb', action='store_true', help='compiles with the TB integration code')
parser.add_argument('--opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('--shared', action='store_true', help='compile shared object')
parser.add_argument('--asan', action='store_true', help='compile with ASAN')

args = parser.parse_args()

source_patterns = [
	"lib/*.c",
	"lib/driver/*.c",
	"lib/preproc/*.c",
	"lib/front/*.c",
	"lib/targets/*.c",
	"lib/zip/*.c",
	"lib/back/ir_gen.c",
	"lib/back/linker.c"
]

ninja = open('build.ninja', 'w')
cflags = "-g -I ../common/ -I include -I lib -I deps -Wall -Werror -Wno-unused-function -Wno-unused-variable"

if args.shared:
	cflags += " -DCUIK_USE_DLL"

if args.opt:
	cflags += " -O2 -DNDEBUG"

if args.asan:
	cflags += " -fsanitize=address"

if args.usetb:
	cflags += " -I ../tilde-backend/include -DCUIK_USE_TB"

if True:
	cflags += " -finstrument-functions"

os_name = platform.system()
if os_name == "Windows":
	source_patterns.append("lib/toolchains/msvc.c")
	source_patterns.append("lib/back/microsoft_craziness.c")
	cflags += " -ferror-limit=100 -D_CRT_SECURE_NO_WARNINGS"
	cflags += " -I ../c11threads"
elif os_name == "Darwin":
	source_patterns.append("lib/toolchains/darwin.c")
	cflags += " -I ../c11threads"

# configure architecture-specific targeting
if platform.machine() == "AMD64":
	cflags += " -msse4.2"

# write out config
ninja.write(f"""
cflags = {cflags}
""")

# write some rules
ninja.write("""
rule cc
  depfile = $out.d
  command = clang $in $cflags -MD -MF $out.d -c -o $out
  description = CC $in $out

rule shared_obj
  command = clang $in $ldflags -g -o $out -shared
  description = LINK $out
""")

if os_name == "Windows":
	lib_ext = ".lib"
	so_ext = ".dll"
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	lib_ext = ".a"
	so_ext = ".so"
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

# compile libCuik
objs = []

for pattern in source_patterns:
	list = glob.glob(pattern)
	for f in list:
		obj = os.path.basename(f).replace('.c', '.o')
		ninja.write(f"build bin/{obj}: cc {f}\n")
		objs.append("bin/"+obj)

if args.shared:
	ninja.write(f"build libcuik{so_ext}: shared_obj {' '.join(objs)}\n")
else:
	ninja.write(f"build libcuik{lib_ext}: lib {' '.join(objs)}\n")

ninja.close()

exit(subprocess.call(['ninja']))
