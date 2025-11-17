
local out = io.open(arg[1], "wb")

out:write[[
#include <stddef.h>

typedef struct InternalFile InternalFile;
struct InternalFile {
    InternalFile* next;
    const char* name;
    size_t size;
    char data[];
};

]]

for i=2,#arg do
    local f = arg[i]:match("^.+/(.+)")

    -- read entire file
    local in_f = io.open(arg[i], "r")
    local buf = in_f:read("*all")
    in_f:close()

    local extra = ""
    if i ~= 2 then
        extra = string.format(", .next = &cuik__ifiles%d", i - 3)
    end

    out:write(string.format("InternalFile cuik__ifiles%d = { .name = \"%s\", .size = %d%s, .data = {\n", i - 2, f, #buf, extra))
    local i = 1
    while i <= #buf do
        local j = math.min(i + 16, #buf)
        local tab = {}
        for k=i,j do
            table.insert(tab, string.format("%d,", string.byte(buf, k)))
        end
        out:write("    "..table.concat(tab, "").."\n")
        i = i + 16
    end
    out:write("}};\n\n")
end

if #arg >= 2 then
    local n = #arg - 2
    out:write("InternalFile* cuik__ifiles_root = &cuik__ifiles"..n..";\n")
end
out:close()
