
-- Load elim test
function trial(n)
    local lines = {}
    lines[#lines + 1] = "#include <stdio.h>"
    lines[#lines + 1] = "int main() {"
    lines[#lines + 1] = "    int dst[50];"
    for i=1,n do
        local x = math.floor(math.random() * 50)
        local y = math.floor(math.random() * 100000)
        lines[#lines + 1] = string.format("    dst[%d] = %d;", x, y)
    end
    lines[#lines + 1] = "    for (int i = 0; i < 100; i++) { printf(\"%d\", dst[i]); }"
    lines[#lines + 1] = "}"
    return lines
end

-- IPSCCP test
function trial2(n)
    local lines = {}
    lines[#lines + 1] = "#include <stdio.h>"
    lines[#lines + 1] = "#include <stdint.h>"
    for i=1,100 do
        lines[#lines + 1] = string.format("static uint32_t mur%d(uint32_t h, uint32_t k) {", i)
        lines[#lines + 1] = "    k *= 0xcc9e2d51;"
        lines[#lines + 1] = "    k = ((k << 15) | (k >> 17))*0x1b873593;"
        lines[#lines + 1] = "    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;"
        lines[#lines + 1] = string.format("    return mur%d(h, k);", i+1)
        lines[#lines + 1] = "}"
    end
    lines[#lines + 1] = "static int mur101(uint32_t h, uint32_t k) { return h; }"
    lines[#lines + 1] = "int main() {"
    lines[#lines + 1] = "    printf(\"Hash: %#x\", mur1(0, 1));"
    lines[#lines + 1] = "    return 0;"
    lines[#lines + 1] = "}"
    return lines
end

function export(lines, path)
    lines[#lines + 1] = ""

    local final_src = table.concat(lines, "\n")
    local f = io.open(path, "w")
    f:write(final_src)
    f:close()
end

export(trial2(1000), "stress.c")