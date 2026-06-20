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
local i   = 1
local lex = function()
    -- skip whitespace
    while ch_class[str:byte(i)] == "ws" or str:byte(i) == 35 do
        if str:byte(i) == 10 then
            i = i + 1
            line_num = line_num + 1
        elseif ch_class[str:byte(i)] == "ws" then
            i = i + 1
        elseif str:byte(i) == 35 then -- hash are comments
            while str:byte(i) ~= 10 do
                i = i + 1
            end
        end
    end

    if i > #str then
        return nil
    end
end

local recording = nil
local def_type  = nil

local insts = {}

dict_put(":", function()
    local name = lex()
    def_type   = ":"
    recording  = {}
    dict_put(name, recording)
end)

dict_put(":reg", function()
    local name = lex()
    def_type   = ":reg"
    recording  = {}
    dict_put(name, recording)
end)

dict_put(":inst", function()
    local name = lex()
    def_type   = ":inst"
    recording  = {}
    dict_put(name, recording)
end)

dict_put(";", function()
    if def_type == ":reg" then
        local tab = dict[#dict][2]
        print(inspect(tab))
    elseif def_type == ":inst" then
        insts[#insts + 1] = dict[#dict]
    end

    def_type = nil
    recording = nil
end)

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
        else
            def()
        end
    end
end

print("DICTIONARY")
for i=1,#dict do
    print(dict[i][1], inspect(dict[i][2]))
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
    print(inspect(list))

    local stack = nil
    local param_count = 0
    local tmp_count = 0
    local pattern_names = { "x", "y", "z", "w" }
    local function pop()
        if #stack == 0 then
            param_count = param_count + 1
            return pattern_names[param_count]
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

        local in_brackets = false
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
        ["{"] = function() end,
        ["}"] = function() print("DESC", pop()) end,
        ["name@"] = function()
            push(name)
        end,
        ["cat"] = function()
            local x = pop()
            local y = pop()
            push(x..y)
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
        params[i] = "uint64_t "..pattern_names[1 + (param_count - i)]
    end

    lines[to_patch] = string.format("static void %s(TB_CGEmitter* e, %s) {", name, table.concat(params, ", "))
    lines[#lines + 1] = "}"

    local function pop()
        if #stack == 0 then
            param_count = param_count + 1
            return string.rep(pattern_names[param_count], 32)
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
    param_count = 0
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
        ["{"] = function() end,
        ["}"] = function() print("DESC", pop()) end,
        ["name@"] = function()
            push(name)
        end,
        ["cat"] = function()
            local x = pop()
            local y = pop()
            push(x..y)
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

for i=1,#insts do
    local d = insts[i]
    print(d[1])
    encode_path(d[1])
end

print()
print("PATTERNS")

parts = Partitions()
for i=1,#patterns do
    local oper_only = patterns[i][2]:gsub("0", "_"):gsub("1", "_")
    parts:put(oper_only, patterns[i])

    print(patterns[i][1], patterns[i][2])
end

function find_first_oper(base, str)
    for i=base,#str do
        if str:sub(i,i) ~= "0" and str:sub(i,i) ~= "1" then
            return i - 1
        end
    end
    return #str
end

function find_first_opcode(base, str)
    for i=base,#str do
        if str:sub(i,i) == "0" or str:sub(i,i) == "1" then
            return i
        end
    end
    return #str
end

function final_state_str(q)
    local list = {}
    for i=1,#q do
        list[i] = q[i][1]
    end
    return table.concat(list, ",")
end

print()

local live = {}
for k,v in parts:iter() do
    print("PAT", k)
    for i=1,#v do
        print("", v[i][2], v[i][1])
        live[#live + 1] = v[i]
    end
    print()
end

lines = {}
function dfa_compile_terminator(q, base, bits, depth)
    local indent = string.rep("    ", depth)
    lines[#lines + 1] = string.format("%s// %s", indent, final_state_str(q))
    lines[#lines + 1] = string.format("%sreturn SOMETHING;", indent)
end

function dfa_compile(q, base, bits, depth)
    if base > 32 then
        dfa_compile_terminator(q, base, bits, depth)
        return
    end

    print("DFA", base)

    -- For each live case, find how many opcodes
    -- we can match against.
    local parts = Partitions()
    local lwb = base
    local upb = 32
    for i=1,#q do
        local str = q[i][2]
        local d   = find_first_opcode(base, str)
        local d2  = find_first_oper(d, str)

        upb = math.min(upb, d2)
        lwb = math.max(lwb, d)

        local discrim = str:sub(d, d2)
        local key = str:sub(d+d2):gsub("0", "_"):gsub("1", "_")
        print(base, discrim, key, d, d2)
        parts:put(key, i)
    end

    if lwb >= upb then
        dfa_compile_terminator(q, base, bits, depth)
        return
    end

    local bit_off = (bits - upb)
    local bit_len = 1 + (upb - lwb)
    local indent = string.rep("    ", depth)

    lines[#lines + 1] = string.format("%sswitch (BEXTR(inst, %d, %d)) {", indent, bit_off, bit_len)
    for k,v in parts:iter() do
        print(k)

        local next = {}
        local keys = OrderedSet()
        for i=1,#v do
            keys:put(q[v[i]][2]:sub(lwb, upb))
            next[#next + 1] = q[v[i]]
        end

        for k in keys:iter() do
            lines[#lines + 1] = string.format("%s    case 0b%s:", indent, k)
        end
        lines[#lines + 1] = indent.."    {"
        dfa_compile(next, lwb + upb, bits, depth+2)
        lines[#lines + 1] = indent.."    }"

        print("", inspect(keys.ord))
    end
    lines[#lines + 1] = indent.."}"
end

lines[#lines + 1] = "static int disasm_classify_dfa(uint32_t inst) {"
dfa_compile(live, 1, 32, 1)

print(table.concat(lines, "\n"))


