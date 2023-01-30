import glob
import os
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles TB')
parser.add_argument('targets', metavar='N', type=str, nargs='+', help='decide which targets to compile with')
parser.add_argument('-cc', help='choose which compiler to use')
parser.add_argument('-mimalloc', action='store_true', help='use mimalloc')
parser.add_argument('-luajit', action='store_true', help='enable using luajit for TB passes')
parser.add_argument('-opt', action='store_true', help='runs optimize on compiled source')
parser.add_argument('-asan', action='store_true', help='compile with ASAN')
parser.add_argument('-autospall', action='store_true', help='instrument code with SpallAuto')

args = parser.parse_args()
if not args.cc:
	args.cc = "clang"

source_patterns = [
	"src/tb/*.c",
	"src/tb/codegen/*.c",
	"src/tb/bigint/*.c",
	"src/tb/objects/*.c",
	"src/tb/system/*.c",
	"src/tb/debug/*.c",
	"src/tb/debug/cv/*.c",
	"src/tb/opt/*.c"
]

for i in args.targets:
	source_patterns.append("src/tb/"+i+"/*.c")

ninja = open('build.ninja', 'w')

is_cl = (args.cc == "cl")
if is_cl:
	cflags = "/nologo /Z7 /I include /I deps/luajit/src /W2 /wd4244 /wd4146 /WX /diagnostics:caret"
	if args.asan: cflags += " /fsanitize=address"
	if args.mimalloc: cflags += " /I ../mimalloc/include /DTB_USE_MIMALLOC"
	if args.luajit: cflags += " /DTB_USE_LUAJIT"
	if args.opt: cflags += " /Ox /DNDEBUG"
	if args.autospall: cflags += " /GH /Gh"
else:
	cflags = "-g -I include -I deps/luajit/src -Wall -Werror -Wno-unused-function"
	if args.asan: cflags += " -fsanitize=address"
	if args.mimalloc: cflags += " -I ../mimalloc/include -DTB_USE_MIMALLOC"
	if args.luajit: cflags += " -DTB_USE_LUAJIT"
	if args.opt: cflags += " -O2 -DNDEBUG"
	if args.autospall: cflags += " -finstrument-functions"

os_name = platform.system()
if os_name == "Windows": cflags += " -D_CRT_SECURE_NO_WARNINGS"

# configure architecture-specific targeting
if platform.machine() == "AMD64":
	if not is_cl: cflags += " -msse4.2"

# write out config
ninja.write(f"cflags = {cflags}\n")

ninja.write(f"rule cc")
if is_cl:
	ninja.write(f"""
  deps = msvc
  command = {args.cc} /showIncludes $in $cflags /c /Fo:$out
  description = CC $in $out
    """)
else:
	ninja.write(f"""
  depfile = $out.d
  command = {args.cc} $in $cflags -MD -MF $out.d -c -o $out
  description = CC $in $out
    """)

if os_name == "Windows":
	lib_ext = ".lib"
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	lib_ext = ".a"
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

# compile TB
objs = []
for pattern in source_patterns:
	list = glob.glob(pattern)
	for f in list:
		obj = os.path.basename(f).replace('.c', '.o')
		if is_cl:
			obj = obj.replace('/', '\\')

		ninja.write(f"build bin/{obj}: cc {f}\n")
		objs.append("bin/"+obj)

ninja.write(f"build tildebackend{lib_ext}: lib {' '.join(objs)}\n")
ninja.close()

exit(subprocess.call(['ninja', '-j3']))
