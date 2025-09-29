
function trial(n)
    local lines = {}
    lines[#lines + 1] = "void fill(int* dst, int* src, long long a, int long long b) {"
    for i=1,n do
        lines[#lines + 1] = string.format("    dst[a+%d] = src[b+%d];", i, i)
    end
    lines[#lines + 1] = "}"

    local final_src = table.concat(lines, "\n")
    local f = io.open("stress.c", "w")
    f:write(final_src)
    f:close()
end

for i=1,5000 do
    trial(i)

    local f = assert(io.popen("cuik stress.c -lang c11 -O -c -t"))
    local content = f:read("*all")
    f:close()

    local m = string.match(content, "Build: (%d+.%d+)")
    print(string.format("%d,%s", i, m))
end
