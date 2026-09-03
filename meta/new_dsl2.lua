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

function andn_bits(l, r)
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
        local rr = r:sub(i,i) ~= "0" and "0" or "1"
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

-- (bextr bits dst src size)
funcs["bextr"] = function(args)
    local trailing = string.rep("0", args[2])
    if type(args[1]) == "number" then
        -- constant bits
        local y = to_bits(args[1])
        local top = math.max(args[2] + args[4], #y)
        local slice = y:sub(args[3] + 1, args[3] + args[4])

        local str = string.rep("0", args[4] - #slice)..slice..trailing
        print("BEXTR", str, slice, inspect(args), args[3] + 1, args[3] + args[4])
        return str
    else
        local ch = string.char(65 + codec_id)
        codec_id = codec_id + 1
        return string.rep(ch, args[4])..trailing
    end
end

funcs["i8"]  = function(args) return { "xxxxxxxx" } end
funcs["i16"] = function(args) return { "xxxxxxxx", "xxxxxxxx" } end
funcs["i32"] = function(args) return { "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx" } end
funcs["i64"] = function(args) return { "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx", "xxxxxxxx" } end

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
            -- print("EVAL", item[1], inspect(args), inspect(item))
            local ret = fn(args)
            -- print("> ", inspect(ret))
            return ret
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

local to_untyped = {
  -- operand types
  ["IMM8"] = "IMM", ["IMM16"] = "IMM", ["IMM32"] = "IMM", ["IMM64"] = "IMM",

  -- instruction types
  ["I8"] = "INT", ["I16"] = "INT", ["I32"] = "INT", ["I64"] = "INT"
}

function shape_str(n)
    local list = {}
    -- remove types from operand, allowing
    -- for more shape coalescing
    for i=2,#n do
        local k = to_untyped[n[i]]
        if not k then
            k = n[i]
        end
        list[i - 1] = k
    end
    -- table.sort(list)

    local k = to_untyped[n[1]]
    if not k then
        k = n[1]
    end

    return k..","..table.concat(list, ",")
end

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
    local skip = 1

    -- skip any known prefixes to allow further
    -- coalescing.
    while skip <= #n[3] do
        local b = n[3][skip]
        if not b:match("0100....") and b ~= "01100110" then
            break
        end
        skip = skip + 1
    end

    for i=skip,#n[3] do
        operand_bits[#operand_bits + 1] = n[3][i]:gsub("1", "0"):gsub("[a-zA-Z]", "1")
    end
 
    -- trim the trailing chars
    local str = table.concat(operand_bits, "")
    local i = #str
    while i > 0 and str:sub(i,i) == "1" do
        i = i - 1
    end

    return string.format("%s_%s", n[2], str:sub(1, i))
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
            if not NFA_done(list[j], state) then
                if not leader then leader = q[1] break
                else return "MANY" end
            end
        end
    end
    return leader
end

function find_first_oper(base, str)
    for i=base,#str do
        if str:sub(i,i) ~= "0" and str:sub(i,i) ~= "1" then
            return i
        end
    end
    return #str + 1
end

function find_first_opcode(base, str)
    for i=base,#str do
        if str:sub(i,i) == "0" or str:sub(i,i) == "1" then
            return i - 1
        end
    end
    return #str
end

function iter_all_cases(str, i, fn)
    if i <= #str then
        local j = find_first_oper(i, str)
        local k = find_first_opcode(j + 1, str)
        if j > #str then
            fn(str)
            return
        end

        -- print(str, i, j, k)

        local bit_count = (k - j) + 1
        local cnt = 2^bit_count 
        local bits = {}
        for l=1,#str do
            bits[l] = str:sub(l,l)
        end

        -- recurse for all cases beyond 0
        for l=0,cnt - 1 do
            local varying = to_bits(l)
            varying = string.rep("0", bit_count - #varying)..varying

            -- copy into the variant space
            for l=1,#varying do
                bits[j + l - 1] = varying:sub(l,l)
            end

            -- print("", "", table.concat(bits), j, bit_count, l)
            iter_all_cases(table.concat(bits), k + 1, fn)
        end
    else
        fn(str)
    end
end

function KnownBits(N)
    local t = { zeros = {}, ones = {} }
    for i=1,N do
        t.zeros[i] = "0"
        t.ones[i]  = "0"
    end

    function t:union(x)
        for i=1,#self.zeros do
            local y = x:sub(i,i)
            if self.zeros[i] == "0" and y == "0" then self.zeros[i] = "1" end
            if self.ones[i]  == "0" and y ~= "0" then self.ones[i]  = "1" end
        end
    end

    function t:to_known_mask()
        local mask = {}
        for i=1,#self.zeros do
            if self.zeros[i] == self.ones[i] then
                mask[i] = "0"
            else
                mask[i] = "1"
            end
        end
        return table.concat(mask)
    end

    function t:to_unknown_mask()
        local mask = {}
        for i=1,#self.zeros do
            if self.zeros[i] == self.ones[i] then
                mask[i] = "1"
            else
                mask[i] = "0"
            end
        end
        return table.concat(mask)
    end

    return t
end

-- q is a list of final keys
function NFA_compile0(uf, parts, ws, q, state, dfa_id)
    -- find all discriminating bits
    local discrim = {}
    for i=1,8 do
        discrim[i] = "1"
    end

    local known = KnownBits(8)

    -- partition inputs based on their cache_keys
    local deltas = Partitions()
    NFA_for_q(parts, q, function(k, v)
        -- all members of the partition have the same length
        if NFA_done(v, state) then return false end

        local str = NFA_match_str(v, state)

        -- compute all input variants
        iter_all_cases(str, 1, function(bits)
            if deltas:put_unique(bits, k) then
                known:union(bits)
            end
        end)
    end)

    -- group deltas based on destination
    local cases = Partitions()
    local next_active = {}
    for k,list in deltas:iter() do
        table.sort(list)

        -- print(k, #list)
        for i=1,#list do
            -- print("", list[i])
        end

        local target_key = table.concat(list, "|")
        if cases:put(target_key, k) then
            next_active[target_key] = list
        end
    end

    local next_state = state + 1

    local function increment_bin(x)
        local l = #x
        x = tonumber(x, 2) + 1
        local y = to_bits(x)
        return string.rep("0", l - #y)..y
    end

    local function compile_delta(k, next, indent)
        local cache_key = next_state..":"..k
        local leader = NFA_has_leader(parts, next, next_state)
        if leader == "MANY" then
            local dst = DFA_state_count
            DFA_cache[cache_key] = dst
            DFA_state_count = DFA_state_count + 1
            ws[#ws + 1] = { next, next_state, dst }

            lines[#lines + 1] = string.format("%sgoto L%d; // %s", indent, dst, cache_key)
        else
            lines[#lines + 1] = string.format("%sreturn 1; // %s", indent, leader)
        end
        lines[#lines + 1] = ""
    end

    local function use_case_ranges(list)
        local min_key = list[1]
        local ranges  = {}
        for i=1,#list do
            local inc = increment_bin(list[i])
            if inc ~= list[i + 1] then
                ranges[#ranges + 1] = { min_key, list[i] }
                min_key = list[i + 1]
            end
        end
        return ranges
    end

    local function compile_split(cases, depth, parent_mask)
        local indent = string.rep("    ", depth)
        local discrim = {}
        for k,list in cases:iter() do
            local local_known = KnownBits(8)
            for i=1,#list do
                local_known:union(list[i])

                -- local with_ignore = andn_bits(list[i], parent_mask)
                -- lines[#lines + 1] = string.format("%s// %s %s %s %s", indent, with_ignore, k, inspect(local_known.zeros), inspect(local_known.ones))
            end

            -- AND combine
            local m = local_known:to_known_mask()
            for i=1,#m do
                if not discrim[i] then
                    discrim[i] = m:sub(i,i)
                elseif m:sub(i,i) == "0" then
                    discrim[i] = "0"
                end
            end

            lines[#lines + 1] = string.format("%s// UNK %s %s", indent, m, inspect(discrim))
        end

        local processed = {}
        if parent_mask then
            for i=1,#discrim do
                processed[i] = discrim[i]

                if parent_mask:sub(i,i) == "1" then
                    discrim[i] = "0"
                    processed[i] = "1"
                end
            end
        else
            for i=1,#discrim do
                processed[i] = discrim[i]
            end
        end
        processed = table.concat(processed)

        -- Split on discriminating bits
        local discrim_mask = table.concat(discrim)
        lines[#lines + 1] = string.format("%s// G %s %s", indent, discrim_mask, processed)

        if #cases.ord == 1 then
            local k = cases.ord[1]
            local next = next_active[k]

            local ranges = use_case_ranges(cases.entries[k])
            if #ranges == 1 then
                if ranges[1][1] == ranges[1][2] then
                    lines[#lines + 1] = string.format("%sif (in == 0b%s) {", indent, ranges[1][1])
                else
                    lines[#lines + 1] = string.format("%sif (in >= 0b%s && in <= 0b%s) {", indent, ranges[1][1], ranges[1][2])
                end

                -- compile_split(sub_cases, depth + 1, discrim_mask)
                lines[#lines + 1] = indent.."}"
                return
            end
        end

        if true then -- discrim_mask == "00000000" then
            lines[#lines + 1] = string.format("%sswitch (in) {", indent, state - 1)
            for k,list in cases:iter() do
                local next = next_active[k]

                local full_list = OrderedSet()
                for i=1,#next do
                    local list2 = parts.entries[next[i]]
                    for i=1,#list2 do
                        full_list:put(list2[i][1])
                    end
                end
                local states_summary = table.concat(full_list.ord, ", ")

                local min_key = list[1]
                for i=1,#list do
                    local inc = increment_bin(list[i])
                    if inc ~= list[i + 1] then
                        if min_key == list[i] then
                            lines[#lines + 1] = string.format("    case 0b%s: // %#x", min_key, tonumber(min_key, 2))
                        else
                            lines[#lines + 1] = string.format("    case 0b%s ... 0b%s: // %#x ... %#x", min_key, list[i], tonumber(min_key, 2), tonumber(list[i], 2))
                        end
                        min_key = list[i + 1]
                    end
                end
                lines[#lines + 1] = string.format("    // %s", states_summary)

                local cache_key = next_state..":"..k
                local leader = NFA_has_leader(parts, next, next_state)
                if leader == "MANY" then
                    local dst = DFA_state_count
                    DFA_cache[cache_key] = dst
                    DFA_state_count = DFA_state_count + 1
                    ws[#ws + 1] = { next, next_state, dst }

                    lines[#lines + 1] = string.format("%s    goto L%d; // %s", indent, dst, cache_key)
                else
                    lines[#lines + 1] = string.format("%s    return 1; // %s", indent, leader)
                end
                lines[#lines + 1] = ""
            end
            lines[#lines + 1] = indent.."}"
            return
        end

        local d  = discrim_mask:find("1")
        local d2 = discrim_mask:find("0", d)
        if d2 then
            d2 = d2 - 1
        else
            d2 = #discrim_mask
        end

        lines[#lines + 1] = string.format("%s// %d %d", indent, d, d2)

        local bit_len = 1 + (d2 - d)
        local bit_off = (#discrim_mask - d2)

        -- partition based on the top-level opcodes
        local splits = Partitions()
        for k,list in cases:iter() do
            local mask_k = list[1]:sub(d, d2)
            splits:put(mask_k, { k, list })
        end

        lines[#lines + 1] = string.format("%sswitch (BEXTR(in, %d, %d)) {", indent, bit_off, bit_len)
        for mask_k,pair in splits:iter() do
            lines[#lines + 1] = string.format("%s    case 0b%s:", indent, mask_k)

            -- differences within a split
            for i=1,#pair do
                local sub_cases = Partitions()
                local k     = pair[i][1]
                local list  = pair[i][2]
                for j=1,#list do
                    local var_k = list[j]
                    sub_cases:put(k, var_k)
                end

                if depth ~= 1 then -- if depth > 4 or processed == "11111111" then
                    lines[#lines + 1] = string.format("%s    goto; // %s", indent, k)

                    -- local next = next_active[k]
                    -- compile_delta(k, next, depth + 1)
                else
                    compile_split(sub_cases, depth + 1, discrim_mask)
                end
            end
        end

        lines[#lines + 1] = indent.."    break;"
        lines[#lines + 1] = indent.."}"
    end

    lines[#lines + 1] = string.format("    L%d:", dfa_id)
    lines[#lines + 1] = string.format("    in = read(%d);", state - 1)
    compile_split(cases, 1)
end

function NFA_compile(active, state)
    local uf = {}

    -- identify all unique final states
    local parts = Partitions()
    for i,v in ipairs(active) do
        local k = NFA_final_key(v)
        parts:put(k, v)
    end
    print("PARTS", #parts.ord)

    local q  = {}
    local id = 0
    for k,v in parts:iter() do
        uf[k] = id
        id = id + 1

        print(k, #v)
        for i=1,#v do
            print("PAT", inspect(v[i]))
        end
    end

    DFA_state_count = DFA_state_count + 1
    local ws = { { parts.ord, state, 0 } }
    local head = 1
    while head <= #ws do
        local q = ws[head]
        head = head + 1

        NFA_compile0(uf, parts, ws, q[1], q[2], q[3])
    end
end

NFA_compile(patterns, 1)
print(table.concat(lines, "\n"))
print(DFA_state_count)
