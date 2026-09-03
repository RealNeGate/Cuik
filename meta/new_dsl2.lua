inspect = require "meta/inspect"
require "meta/prelude"

local unpack = unpack or table.unpack
local source = run_command("clang -E -xc "..arg[1])
local lex = lexer(source)

local funcs  = {}
local shapes = {}

local codec_id = 0
local codecs = {}

function union_bits(l, r)
    if not l then
        return r
    elseif not r then
        return l
    end

    -- print("BOR", l, r)

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
    return table.concat(x)
end

function and_bits(l, r)
    if not l then
        return r
    elseif not r then
        return l
    end

    local x = {}
    local m = math.min(#l, #r)
    local b = math.max(#l, #r) - m

    -- propagate left most bits
    l = l:sub(-m)
    r = r:sub(-m)

    for i=1,m do
        local ll = l:sub(i,i)
        local rr = r:sub(i,i)
        if ll ~= "0" and rr ~= "0" then
            x[i+b] = "1"
        else
            x[i+b] = "0"
        end
    end
    return string.rep("0", b) .. table.concat(x)
end

function is_zero(x)
    for i=1,#x do
        if x:sub(i,i) ~= "0" then
            return false
        end
    end

    return true
end

function to_bits(x)
    if type(x) == "string" then
        return x
    elseif x == 0 then
        return "0"
    end

    -- convert number into known bits
    local t = {}
    while x > 0 do
        local rest = x % 2
        t[#t + 1] = rest
        x = (x - rest) / 2
    end
    return string.reverse(table.concat(t))
end

-- if args[1] is args[2] then the value is optional
funcs["opt_if"] = function(args)
    local x = to_bits(args[1])
    local y = to_bits(args[2])

    -- perfect match? ok require it
    local yy = y:gsub("[a-zA-Z]", "0")
    local z = and_bits(x, y) 
    if z == yy then
        return { op="OPT", val=args[2] }
    end
    return args[2]
end

funcs["isa"] = function(args)
    return args[1] == args[2]
end

funcs["select"] = function(args)
    if args[1] then
        return args[2]
    end
    return args[3]
end

funcs["bor"] = function(args)
    local res = nil
    for i=1,#args do
        res = union_bits(res, to_bits(args[i]))
    end 
    return res
end

funcs["bextr"] = function(args)
    local trailing = string.rep("0", args[2])
    if type(args[1]) == "number" then
        -- constant bits
        local y = to_bits(args[1])
        local top = math.max(args[2] + args[4], #y)
        return y..trailing
    else
        local ch = string.char(65 + codec_id)
        codec_id = codec_id + 1
        return string.rep(ch, args[4])..trailing
    end
end

funcs["i8"]  = function(args) return { "xxxxxxxx" } end
funcs["i16"] = function(args) return { "xxxxxxxx", "xxxxxxxx" } end
funcs["i32"] = function(args) return { "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx" } end
funcs["i64"] = function(args) return {  "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx" } end

function clone_table(arr)
    local dst = {}
    for k,v in pairs(arr) do dst[k] = v end
    return dst
end

function eval0(item, globals, params)
    if type(item) == "string" and item:sub(1, 1) == "$" then
        -- param to expand
        local k = item:sub(2)
        if not globals[k] then error("couldn't find "..item) end
        item = globals[k]
    elseif params[item] then
        item = params[item]
    end

    if type(item) == "table" and item[1] then
        -- expand args, then call into pattern
        local fn = funcs[item[1]]
        if not fn then error("couldn't find "..item[1]) end

        if type(fn) == "table" then
            local args = clone_table(params)
            local fn_params = fn[3]
            for j=1,#fn_params do
                local arg = eval0(item[1 + j], globals, params)
                args[fn_params[j]] = arg
            end

            return eval(fn, 4, globals, args)
        else
            local args = {}
            for j=2,#item do
                local arg = eval0(item[j], globals, params)
                args[j - 1] = arg
            end
            -- print("EVAL", item[1], inspect(args))
            return fn(args)
        end
    else
        return item
    end
end

function eval(n, start_i, globals, params)
    local dst = {}
    for i=start_i,#n do
        dst[#dst + 1] = eval0(n[i], globals, params)
    end
    return dst
end

function flatten0(dst, n)
    for i=1,#n do
        if type(n[i]) == "table" and not n[i].op then
            flatten0(dst, n[i])
        else
            dst[#dst + 1] = n[i]
        end
    end
    return dst
end

function flatten(n)
    return flatten0({}, n)
end

local patterns = {}

function shape_str(n) return table.concat(n, ",", 2) end
function expand_combos(dst, pos, mnemonic, str, pat, shape)
    -- print(pos, inspect(pat))

    -- convert all integers to bits
    for i=pos,#pat do
        local src = pat[i]
        if type(src) == "table" and src.op == "OPT" then
            local cloned = {}
            for j=1,#dst do cloned[j] = dst[j] end

            cloned[#cloned + 1] = src.val
            expand_combos(cloned, i + 1, mnemonic, str, pat, shape)
        else
            dst[#dst + 1] = pat[i]
        end
    end

    for i=1,#dst do
        local src = to_bits(dst[i])
        if #src < 8 then
            -- pad to byte
            src = string.rep("0", 8 - #src)..src
        end
        dst[i] = src
    end

    patterns[#patterns + 1] = { mnemonic, str, dst, shape }
end

function expand_shape(name, mnemonic, n, globals)
    print("DECL", name, mnemonic, n[2])
    for i=3,#n do
        if type(n[i]) == "table" and n[i][1] == "pat" then
            local state = clone_table(globals)
            for j=1,#n[i][2] do
                local k = string.format("%d", j - 1)
                local v = n[i][2][j]
                state[k] = v
            end

            codec_id = 0
            codecs = {}

            local pat = eval(n[i], 3, state, {})
            pat = flatten(pat)

            -- compute all possibilities
            local shape = shape_str(n[i][2])
            expand_combos({}, 1, mnemonic, shape, pat, n[i][2])
        end
    end
end

while true do
    t = lex()
    if not t then
        break
    elseif t ~= "(" then
        print("fuck but in parsing "..t)
        os.exit(1)
    end

    local tree = parse_node(lex)
    print(inspect(tree))

    if tree[1] == "defshape" then
        shapes[tree[2]] = tree
    elseif tree[1] == "defun" then
        funcs[tree[2]] = tree
    elseif tree[1] == "definst" then
        local globals = {}
        local i = 5
        while i <= #tree do
            if type(tree[i]) ~= "string" or tree[i]:sub(1, 1) ~= ":" then
                error("Expected param name, got "..tree[i])
            end

            local k = tree[i]
            i = i + 1

            if i > #tree then
                error("Expected value for "..k..", got nothing")
            end

            globals[k:sub(2)] = tree[i]
            i = i + 1
        end

        expand_shape(tree[3], tree[4], shapes[tree[2]], globals)
    end
end

for i=1,#patterns do
    print("PAT", inspect(patterns[i]))
end
print("PATTERN COUNT", #patterns)

-- defines which patterns are equivalent
function NFA_final_key(n)
    local operand_bits = {}
    for i=1,#n[3] do
        operand_bits[i] = n[3][i]:gsub("1", "0"):gsub("[a-zA-Z]", "1")
    end
    return string.format("%s_%s", n[2], table.concat(operand_bits, ""))
end

function NFA_done(n, state)
    while state <= #n[3] and n[3][state]:gsub("[a-zA-Z]", "x") == "xxxxxxxx" do
        state = state + 1
    end
    return state > #n[3]
end

function NFA_match_str(n, state)
    return n[3][state]:gsub("[a-zA-Z]", "_")
end

lines = {}

local DFA_cache = {}
local DFA_state_count = 0

function NFA_final_str(q, state)
    local list = {}
    list[1] = state
    for i=1,#q do
        list[i + 1] = NFA_final_key(q[i])
    end
    return table.concat(list, ",")
end

function NFA_for_q(parts, q, fn)
    for i=1,#q do
        local list = parts.entries[q[i]]
        for j=1,#list do
            if fn(q[i], list[j]) then
                break
            end
        end
    end
end

function NFA_has_leader(parts, q, state)
    for i=1,#q do
        local list = parts.entries[q[i]]
        for j=1,#list do
            -- currently all partition members have the same size
            if NFA_done(list[j], state) then break end

            if not leader then leader = q[1] break
            else return "MANY" end
        end
    end
    return leader
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

-- q is a list of final keys
function NFA_compile0(parts, ws, q, state, dfa_id, base)
    -- find all discriminating bits
    local discrim = {}
    for i=1,8 do
        discrim[i] = "1"
    end

    local lwb = base
    local upb = 32

    -- discover all partitions
    NFA_for_q(parts, q, function(k, v)
        -- all members of the partition have the same length
        if NFA_done(v, state) then return true end

        local str = NFA_match_str(v, state)
        local d   = find_first_opcode(base, str)
        local d2  = find_first_oper(d, str)

        upb = math.min(upb, d2)
        lwb = math.max(lwb, d)

        local discrim = str:sub(d, d2)
        local key = str:sub(d+d2):gsub("0", "_"):gsub("1", "_")
        print(base, discrim, key, d, d2)
        parts:put(key, i)
    end)

    if lwb >= upb then
        lines[#lines + 1] = "TERM"
        return
    end

    local bit_off = (8 - upb)
    local bit_len = 1 + (upb - lwb)
    print(bit_off, bit_len)
    return

--[[
    print("DISCRIM", state - 1, dfa_id, #q)
    print(table.concat(discrim, ""))

    -- for each unique prefix, check live paths
    local inputs = Partitions()
    NFA_for_q(parts, q, function(k, v)
        if NFA_done(v, state) then return true end

        local s = NFA_match_str(v, state)
        local t = {}
        for j=1,8 do
            if discrim[j] == "0" then
                t[j] = "_"
            else
                t[j] = s:sub(j,j)
            end
        end
        print(s, k)
        inputs:put(table.concat(t), k)
    end)

    -- for each input, check live paths
    print("PARTS")

    local mask = table.concat(discrim)
    lines[#lines + 1] = string.format("L%d: switch (read(%d) & 0b%s) {", dfa_id, state - 1, mask)

    local next_state = state + 1
    for k,v in inputs:iter() do
        local finals = OrderedSet()
        local full_list = OrderedSet()
        for i=1,#v do
            if finals:put(v[i]) then
                local list = parts.entries[v[i]]
                for j=1,#list do
                    full_list:put(list[j][1])
                end
            end
        end

        local states_summary = table.concat(full_list.ord, ", ")

        local cache_key = next_state..":"..table.concat(finals.ord, "|")
        local dst = DFA_cache[cache_key]
        local leader = nil
        if not dst then
            print(k, #v, "GROUPS:", cache_key, "STATES:", states_summary)

            leader = NFA_has_leader(parts, finals.ord, next_state)
            if leader == "MANY" then
                dst = DFA_state_count
                DFA_cache[cache_key] = dst
                DFA_state_count = DFA_state_count + 1
                ws[#ws + 1] = { finals.ord, next_state, dst }
            end
        end

        local k_formatted = k:gsub("_", "0")
        if leader == "MANY" then
            lines[#lines + 1] = string.format("    case 0b%s: goto L%d; // %s", k_formatted, dst, cache_key)
        elseif leader then
            lines[#lines + 1] = string.format("    case 0b%s: return 1; // %s  %s", k_formatted, leader, cache_key)
        else
            lines[#lines + 1] = string.format("    // 0b%s %s %s", k_formatted, leader, cache_key)
        end
    end

    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    print()
]]--
end

function NFA_compile(active, state)
    local uf = {}

    -- identify all unique final states
    local parts = Partitions()
    for i,v in ipairs(active) do
        local k = NFA_final_key(v)
        parts:put(k, v)
    end

    local q = {}
    for k,v in parts:iter() do
        print(k, #v)
        for i=1,#v do
            print("PAT", inspect(v[i]))
        end
    end

    DFA_state_count = DFA_state_count + 1

    local ws = { { parts.ord, state, 0 } }
    while #ws > 0 do
        local q = table.remove(ws)
        NFA_compile0(parts, ws, q[1], q[2], q[3])
    end
end

NFA_compile(patterns, 1)
print(DFA_state_count)
print(table.concat(lines, "\n"))
