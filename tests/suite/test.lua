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

if #arg >= 1 then
    x = { arg[1] }
end

configs = {
    "", "-O", "-g", "-O -g"
}

local passed = 0
local total = 0

N = 20
samples = {}
for i=1,N do
    samples[i] = 0
end

function perf_test(cmd, args)
    os.execute(cmd)

    local exec_cmd = exe_name.." "..args
    if is_windows then
        exec_cmd = exec_cmd.." > NUL 2>&1"
    end

    local mean = 0
    for i=1,N do
        local x = os.clock()
        os.execute(exec_cmd)
        local t = os.clock() - x

        samples[i] = t
        mean = mean + t
    end
    mean = mean / N

    local var = 0
    for i=1,N do
        var = var + math.pow(samples[i] - mean, 2)
    end
    var = var / (N - 1)

    print(string.format("  Average: %fs +- %f ('%s')", mean, var, cmd))
end

for i=1,#x do
    total = total + 1

    local args = ""
    if x[i] == "nbody.c" then
        args = "100000"
    end

    print("Testing... "..x[i])
    print("  Clang:")
    code = os.execute(string.format("clang %s -O1 && %s %s > clang.txt", x[i], exe_name, args))
    if code ~= 0 then
        print("    BAD!!!", code)
        goto skip
    end

    print("  Cuik:", configs[1])
    local code = os.execute(string.format("cuik %s %s && %s %s > cuik.txt", x[i], configs[1], exe_name, args))
    if code ~= 0 then
        print("    BAD CUIK!!!", configs[1])
        goto skip
    end

    -- Compare the flavors of cuik compiles first
    for j=2,#configs do
        print("  Cuik:", configs[j])
        code = os.execute(string.format("cuik %s %s && %s %s > cuik2.txt", x[i], configs[j], exe_name, args))
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

        -- we're doing the real deal now
        if x[i] == "nbody.c" then
            args = "1000000"
        end

        perf_test(string.format("clang %s", x[i]), args)
        perf_test(string.format("cuik %s", x[i]), args)
        perf_test(string.format("clang %s -O1", x[i]), args)
        perf_test(string.format("cuik %s -O", x[i]), args)
    else
        print("    BAD!!!")
    end
    ::skip::
end

print("Passed", passed, " out of ", total)
