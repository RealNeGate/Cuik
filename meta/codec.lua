inspect = require "meta/inspect"
require "meta/prelude"

function read_file(path)
    local f = assert(io.open(path, "r"))
    local content = f:read("*all")
    f:close()
    return content
end

local src = read_file("tb/mips/mips_mach.dsl")

local dict = {}
local word_buf = {}

local function dict_put(k, v)
    dict[#dict + 1] = { k, v }
end

local function dict_get(k)
    local n = tonumber(k)
    if n then
        return n
    end

    for i=0,#dict-1 do
        if dict[#dict - i][1] == k then
            return dict[#dict - i][2]
        end
    end
end

-- immediate execute
local in_comment = 0
local lex = string.gmatch(src, "%S+")

local recording = nil
local def = dict_get(word)

dict_put(":", function()
    local name = lex()
    recording  = {}
    dict_put(name, recording)
end)

dict_put(";", function()
    recording = nil
end)

local export = 1
for word in lex do
    if word == "(" then
        in_comment = in_comment + 1
    elseif word == ")" then
        assert(in_comment > 0)
        in_comment = in_comment - 1
    elseif in_comment == 0 then
        local def = dict_get(word)
        if word ~= ";" and recording then
            recording[#recording + 1] = def and def or word
        elseif word == "export" then
            export = #dict + 1
        else
            def()
        end
    end
end

print("DICTIONARY")
for i=1,#dict do
    print(inspect(dict[i]))
end

function expand(list, def)
    for i=1,#def do
        if type(def[i]) == "table" then
            expand(list, def[i])
        else
            list[#list + 1] = def[i]
        end
    end
end

-- Generate an AST for the expanded set of words
lines = {}
patterns = {}

function encode_path(name)
    local list = {}
    local def = dict_get(name)
    expand(list, def)

    local stack = nil
    local param_count = 0
    local tmp_count = 0
    local function pop()
        if #stack == 0 then
            param_count = param_count + 1
            return string.format("P%d", param_count)
        end

        local v = stack[#stack]
        stack[#stack] = nil
        return v
    end

    local function push(x)
        stack[#stack + 1] = x
    end

    local function new_tmp(x)
        local id = "t"..tmp_count
        tmp_count = tmp_count + 1

        lines[#lines + 1] = string.format("uint64_t %s = %s;", id, x)
        return id
    end

    local function interp(num_fn, my_env)
        -- print()
        -- print("INTERP")

        stack = {}
        for i=1,#list do
            -- print(list[i], inspect(stack))
            if type(list[i]) == "number" then
                push(num_fn(list[i]))
            else
                local def = my_env[list[i]]
                assert(def, "missing word: "..list[i])
                def()
            end
        end
    end

    local to_patch = #lines + 1
    lines[to_patch] = "PATCH"

    -- ENCODE CASE
    interp(function(x) return x end, {
        ["swap"] = function()
            local x = pop()
            local y = pop()
            push(x)
            push(y)
        end,
        ["and"] = function()
            local r = pop()
            local l = pop()
            push(string.format("(%s & %s)", l, r))
        end,
        ["or"] = function()
            local r = pop()
            local l = pop()
            push(string.format("(%s | %s)", l, r))
        end,
        ["shl"] = function()
            local r = pop()
            local l = pop()
            push(string.format("(%s << %s)", l, r))
        end,
        ["shr"] = function()
            local r = pop()
            local l = pop()
            push(string.format("(%s >> %s)", l, r))
        end,
        ["emit-4"] = function()
            local x = pop()
            lines[#lines + 1] = string.format("    EMIT4(e, %s);", x)
        end,
    })

    local params = {}
    for i=1,param_count do
        params[i] = "uint64_t P"..i
    end

    lines[to_patch] = string.format("static void %s(TB_CGEmitter* e, %s) {", name, table.concat(params, ","))
    lines[#lines + 1] = "}"

    local function pop()
        if #stack == 0 then
            return string.rep("x", 32)
        end

        local v = stack[#stack]
        stack[#stack] = nil
        return v
    end

    local function bin2num(x)
        local y = 0
        for i=1,#x do
            y = y*2 + (string.byte(x, i) == 49 and 1 or 0)
        end
        return y
    end

    -- DECODE CASE
    interp(
        function(x)
            -- convert number into known bits
            local t = {}
            while x > 0 do
                local rest = x % 2
                t[#t + 1] = rest
                x = (x - rest) / 2
            end
            return string.reverse(table.concat(t))
        end, {
        ["swap"] = function()
            local x = pop()
            local y = pop()
            push(x)
            push(y)
        end,
        ["and"] = function()
            local r = pop()
            local l = pop()

            local x = {}
            local m = math.min(#l, #r)

            l = l:sub(-m)
            r = r:sub(-m)
            for i=1,m do
                local ll = l:sub(i,i)
                local rr = r:sub(i,i)
                if ll ~= "0" and rr ~= "0" then
                    x[i] = ll == "1" and rr or ll
                else
                    x[i] = "0"
                end
            end

            push(table.concat(x))
        end,
        ["or"] = function()
            local r = pop()
            local l = pop()

            local x = {}
            local m = math.min(#l, #r)
            local b = math.max(#l, #r) - m

            -- propagate left most bits
            if m < #l then
                -- propagate l
                for i=1,b do
                    x[i] = l:sub(i,i)
                end
            else
                -- propagate r
                for i=1,b do
                    x[i] = r:sub(i,i)
                end
            end
            l = l:sub(-m)
            r = r:sub(-m)

            for i=1,m do
                local ll = l:sub(i,i)
                local rr = r:sub(i,i)
                if ll == "0" or rr == "0" then
                    x[i+b] = ll == "0" and rr or ll
                else
                    x[i+b] = "1"
                end
            end

            push(table.concat(x))
        end,
        ["shl"] = function()
            local r = bin2num(pop())
            local l = pop()
            local x = {}

            for i=1,#l do
                x[i] = l:sub(i,i)
            end
            for i=1,r do
                x[i+#l] = "0"
            end

            push(table.concat(x))
        end,
        ["emit-4"] = function()
            local pat = pop()
            if #pat < 32 then
                pat = string.rep("0", 32 - #pat)..pat
            end

            patterns[#patterns + 1] = { name, pat }
        end
    })
end

print()

for i=export,#dict do
    if dict[i][1] ~= "j-type" then
        print(dict[i][1])
        encode_path(dict[i][1])
    end
end

print()
print("PATTERNS")
for i=1,#patterns do
    print(patterns[i][1], patterns[i][2])
end

print(table.concat(lines, "\n"))


