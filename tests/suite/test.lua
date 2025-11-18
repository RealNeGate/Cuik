-- silly little OS detection
local is_windows = package.config:sub(1,1) == "\\"

local x = {}
if is_windows then
    local cmd = io.popen("dir *.c /B")
    for c in cmd:lines() do
        if c ~= "run_suite.c" then
            x[#x + 1] = c
        end
    end
    cmd:close()
else
    local cmd = io.popen("find *.c -maxdepth 1")
    for c in cmd:lines() do
        if c ~= "run_suite.c" then
            x[#x + 1] = c
        end
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
    "", "-O", "-g", "-O -g", "-based", "-based -O"
}

local passed = 0
local total = 0

N = 5
samples = {}
for i=1,N do
    samples[i] = 0
end

function perf_test(cmd)
    local mean = 0
    for i=1,N do
        local x = os.clock()
        os.execute(cmd)
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
    return mean, var
end

function comp_test(cmd, args)
    local mean, var = perf_test(cmd.." -c")

    print("  Test: ")
    print(string.format("    Compile: %fs +- %f ('%s')", mean, var, cmd.." -c"))

    if is_windows then
        exec_cmd = "a.exe "..args.." > NUL 2>&1"
    else
        exec_cmd = "./a.out "..args.." > NUL 2>&1"
    end

    if true then
        os.execute(cmd)

        local mean, var = perf_test(exec_cmd)
        print(string.format("    Runtime: %fs +- %f ('%s')", mean, var, exec_cmd))
    end
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

    -- Compare against clang
    local pass = true
    for j=1,#configs do
        print("  Cuik:", configs[j])
        code = os.execute(string.format("cuik %s %s && %s %s > cuik.txt", x[i], configs[j], exe_name, args))
        if code == 0 then
            local diff = os.execute("diff clang.txt cuik.txt")
            if diff ~= 0 then
                print("    BAD DIFF!!!")
                pass = false
            end
        else
            print("    BAD CUIK!!!", configs[j])
            pass = false
        end
    end

    if pass then
        print("    Pass!")
        passed = passed + 1
    end

    if false then
        -- we're doing the real deal now
        if x[i] == "nbody.c" then
            args = "10000000"
        end

        comp_test(string.format("clang %s", x[i]), args)
        comp_test(string.format("cuik %s", x[i]), args)
        comp_test(string.format("clang %s -O1", x[i]), args)
        comp_test(string.format("cuik %s -O", x[i]), args)
    end
    ::skip::
end

print("Passed", passed, " out of ", total)
