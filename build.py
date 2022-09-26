import glob
import os
import platform
import subprocess
import argparse

parser = argparse.ArgumentParser(description='Compiles TB')
parser.add_argument('driver', metavar='N', type=str, nargs='+', help='decide which targets to compile with')
parser.add_argument('--mimalloc', action='store_true')

args = parser.parse_args()
subprocess.check_call(['build.py', 'x64', 'aarch64'], shell=True, cwd="tilde-backend")

use_mimalloc = True
source_patterns = [
	"lib/*.c",
	"lib/preproc/*.c",
	"lib/front/*.c",
	"lib/targets/*.c",
	"lib/back/ir_gen.c",
	"lib/back/linker.c"
]

ninja = open('build.ninja', 'w')
cflags = "-I include -I lib -I deps -I tilde-backend/include -Wall -Werror -Wno-unused-function -Wno-unused-variable"
ldflags = " -lDbgHelp"
if args.mimalloc:
	ldflags += " mimalloc/out/Release/mimalloc.lib -Xlinker /include:mi_version"
	ldflags += " -nodefaultlibs -lmsvcrt -lvcruntime -lucrt"

os_name = platform.system()
if os_name == "Windows":
	cflags += " -D_CRT_SECURE_NO_WARNINGS"

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
  description = cc $in $out

""")

if os_name == "Windows":
	ninja.write("""
rule lib
  command = lib /nologo $in /out:$out
  description = LIB $out

""")
else:
	ninja.write("""
rule lib
  command = ar -rcs $out $in
  description = AR $out

""")

ninja.write("""
rule link
  command = clang $in $ldflags -o $out
  description = AR $out

""")

# compile libCuik
sources = []
for pattern in source_patterns:
	list = glob.glob(pattern)
	for f in list:
		ninja.write(f"build bin/{os.path.basename(f).replace('.c', '.o')}: cc {f}\n")

ninja.close()

# run ninja
subprocess.check_call(['ninja'])
