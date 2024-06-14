-- silly little OS detection
local is_windows = package.config:sub(1,1) == "\\"

local x = {}
if is_windows then
    local cmd = io.popen("dir *.c /B")
    for c in cmd:lines() do
        x[#x + 1] = c
    end
    cmd:close()
else
    local cmd = io.popen("find *.c -maxdepth 1")
    for c in cmd:lines() do
        x[#x + 1] = c
    end
    cmd:close()
end

for i=1,#x do
    print("Testing... "..x[i])
    print("* Cuik")
    os.execute(string.format("cuik %s && a > cuik.txt", x[i]))
    print("* Clang")
    os.execute(string.format("clang %s && a > clang.txt", x[i]))
    print("* Diff")
    os.execute("git diff --no-index cuik.txt clang.txt")
end

for i=1,#x do
    print("Testing optimized... "..x[i])
    print("* Cuik")
    os.execute(string.format("cuik %s -O && a > cuik.txt", x[i]))
    print("* Clang")
    os.execute(string.format("clang %s && a > clang.txt", x[i]))
    print("* Diff")
    os.execute("git diff --no-index cuik.txt clang.txt")
end
