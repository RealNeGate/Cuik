-- get the libcuik shit working *enough*
--
-- game project: semi top-down
--   [ ] village
--   [ ] sword
--   [ ] forest
--   [ ] go in cave
--   [ ] stab monsters
--   [ ] beat boss
--   [ ] win
local cuik = require("libcuik")

test = cuik.compile[[
#define _NO_CRT_STDIO_INLINE
#include <stdio.h>

int foo() {
    printf("%d\n", 20);
    return 16;
}
]]

test.foo()
