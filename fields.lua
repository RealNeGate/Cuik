buffer  = require "string.buffer"

function foo(N)
    local src = buffer.new(1000)
    src:put("#include <stddef.h>\n")
    src:put("void use(int* p);\n")
    src:put("void copy(int* a, int N) {\n")
    src:put(string.format("    int dst[%d];\n", N))
    for i=0,N-1 do
        src:put(string.format("    if (%d >= N) { return; };\n", i))
        src:put(string.format("    dst[%d] = a[%d];\n", i, i))
    end
    src:put("    __builtin_blackhole(dst);\n")
    src:put("}\n")
    return src:tostring()
end

function run_command(cmd)
    local f = assert(io.popen(cmd))
    local content = f:read("*all")
    f:close()
    return content
end

function run_test(N)
    local f = io.open("a.c", "w")
    f:write(foo(N))
    f:close()

    print("Running Tests (", N, ")")
    local avg = 0
    for i=1,10 do
        local t = run_command("cuik -O -c a.c")
        -- print(t)
        avg = avg + tonumber(t)
    end
    print(string.format("   %.3f ms\n", avg / 10))
end

for i=1,10 do
    run_test(100*i)
end

-- run_test(1000)

