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

local exe_name = "a"
if not is_windows then
    exe_name = "./a.out"
end

configs = {
    "", "-O", "-based", "-O -based"
}

local passed = 0
local total = 0

for i=1,#x do
    total = total + 1

    print("Testing... "..x[i])
    print("  Clang:")
    code = os.execute(string.format("clang %s && %s > clang.txt", x[i], exe_name))
    if code ~= 0 then
        print("    BAD!!!")
        goto skip
    end

    print("  Cuik:", configs[1])
    local code = os.execute(string.format("cuik %s %s && %s > cuik.txt", x[i], configs[1], exe_name))
    if code ~= 0 then
        print("    BAD CUIK!!!", configs[1])
        goto skip
    end

    -- Compare the flavors of cuik compiles first
    for j=2,#configs do
        print("  Cuik:", configs[j])
        code = os.execute(string.format("cuik %s %s && %s > cuik2.txt", x[i], configs[j], exe_name))
        if code ~= 0 then
            print("    BAD CUIK!!!", configs[j])
            goto skip
        end

        local diff = os.execute("diff cuik.txt cuik2.txt")
        if diff ~= 0 then
            print("    BAD DIFF!!!")
            goto skip
        end
    end

    local diff = os.execute("diff cuik.txt clang.txt")
    if diff == 0 then
        print("    Pass!")
        passed = passed + 1
    else
        print("    BAD!!!")
    end
    ::skip::
end

print("Passed", passed, " out of ", total)
