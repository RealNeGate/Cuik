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

local configs = {
    "", "-O", -- "-g", "-O -g", "-based", "-based -O"
}

if #arg >= 1 then
    x = { arg[1] }
end

local exe_suffix = ".out"
if is_windows then
    exe_suffix = ".exe"
end

function flatten0(new_arr, arr)
    for i=1,#arr do
        if type(arr[i]) == "table" then
            flatten0(new_arr, arr[i])
        else
            new_arr[#new_arr + 1] = arr[i]
        end
    end
end

function flatten(arr)
    local new_arr = {}
    flatten0(new_arr, arr)
    return new_arr
end

local repros = {}

function run_command(exe, args, outfile)
    assert(exe)
    assert(args)
    local args_flat = args
    if type(args_flat) == "table" then
        args_flat = table.concat(flatten(args), " ")
    end

    local cmd = nil
    if outfile then
        cmd = string.format("set -o pipefail; timeout --preserve-status -k 2s 5s %s %s | head -c 1M > %s; echo $?", exe, args_flat, outfile)
    else
        cmd = string.format("timeout --preserve-status -k 2s 5s %s %s", exe, args_flat)
    end
    print(cmd)

    local f = io.popen(cmd)
    if not f then
        repros[#repros + 1] = cmd
        return false
    end

    local content = f:read("*all")
    f:close()

    if content ~= "0\n" then
        repros[#repros + 1] = cmd
        return false
    end

    return true
end

function run_command0(cmd)
    local f = assert(io.popen(cmd))
    local content = f:read("*all")
    f:close()
    return content
end

local skips   = {}
local results = {}

function cc_compile_and_test(cc, infile, cc_args, exec_args)
    -- Compile source
    if not run_command(cc, { infile, cc_args, "-o", cc..exe_suffix }, cc.."_cc.txt") then
        return false
    end
    return run_command("./"..cc..exe_suffix, exec_args, cc.."_log.txt")
end

local total  = 0
local passed = 0
function process_test(i)
    -- HACK REMOVE LATER
    local includes = "-I /usr/include/csmith/"
    local args     = ""
    if x[i] == "nbody.c" then
        args = "100000"
    elseif x[i] == "whetstone.c" then
        args = ""
    end

    -- Generate golden test results, if these fail then we skip the test later
    if not cc_compile_and_test("clang", x[i], { "-lm", includes }, args) then
        skips[#skips + 1] = x[i]
        return
    end

    print("Testing", x[i])

    -- Compare against clang
    local r = {}
    local pass = true
    for j=1,#configs do
        if cc_compile_and_test("cuik", x[i], { configs[j], includes }, args) then
            local diff = os.execute("git diff --color-words clang_log.txt cuik_log.txt")
            if diff ~= true and diff ~= 0 then
                r[j] = "DIFF"
                repros[#repros + 1] = cmd
                pass = false
            else
                r[j] = "GOOD"
            end
        else
            r[j] = tostring(code)
            repros[#repros + 1] = cmd
            pass = false
        end
    end
    results[i] = r

    if pass then
        passed = passed + 1
    end
    total = total + 1
end

for i=1,#x do process_test(i) end

local tab = {}
local entry_width = 9

function pad_str(str, w)
    if #str < w then
        return str .. string.rep(" ", w - #str)
    end
    return str:sub(1, w)
end

tab[1] = pad_str("File", entry_width)
for j=2,#configs+1 do
    if configs[j-1] == "" then
        tab[j] = "None"
    else
        tab[j] = configs[j-1]
    end
    tab[j] = pad_str(tab[j], entry_width)
end

print()
print()
print(table.concat(tab, " | "))

tab[1] = string.rep("=", entry_width)
for j=1,#configs do
    tab[j+1] = tab[1]
end
print(table.concat(tab, "=|="))

-- Dump pretty results
for i=1,#x do
    local r = results[i]
    if r then
        tab[1] = pad_str(x[i], entry_width)
        for j=1,#configs do
            tab[j + 1] = pad_str(r[j], entry_width)
        end
        print(table.concat(tab, " | "))
    end
end

print()
print("Skip:")
for i=1,#skips do
    print(skips[i])
end

print()
print("Repros:")
for i=1,#repros do
    print(repros[i])
end

print()
print("Passed", passed, " out of ", total)


