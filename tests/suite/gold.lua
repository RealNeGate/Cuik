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

local exe_name = "a"
if not is_windows then
    exe_name = "./a.out"
end

function run_command(cmd)
    local f = assert(io.popen(cmd))
    local content = f:read("*all")
    f:close()
    return content
end

local repros  = {}
local results = {}
local skips   = {}

local total   = 0
local passed  = 0
function process_test(i)
    -- HACK REMOVE LATER
    local includes = "" -- "-I /home/linuxbrew/.linuxbrew/Cellar/csmith/2.3.0/include/csmith-2.3.0/"
    local args     = ""
    if x[i] == "nbody.c" then
        args = "100000"
    end

    -- Generate golden test results, if these fail then we skip the test later
    local g = os.execute(string.format("clang %s %s -lm -O1 && %s %s > clang.txt", x[i], includes, exe_name, args))
    if g ~= 0 then
        skips[#skips + 1] = x[i]
        return
    end

    print("Testing", x[i])

    -- Compare against clang
    local r = {}
    local pass = true
    for j=1,#configs do
        local cmd = string.format("../../bin/cuik %s %s %s && %s %s", x[i], configs[j], includes, exe_name, args)
        code = os.execute(cmd.." > cuik.txt")
        if code == 0 then
            local diff = os.execute("git diff --color-words clang.txt cuik.txt")
            if diff ~= 0 then
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


