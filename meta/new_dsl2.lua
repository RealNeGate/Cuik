inspect = require "meta/inspect"
require "meta/prelude"

local unpack = unpack or table.unpack
local source = run_command("clang -E -xc "..arg[1])
local lex = lexer(source)

local decls  = {}
local funcs  = {}
local shapes = {}

local codec_id = 0
local codecs = {}
local MIN_UNIT_WIDTH = 8

lines = {}

function iter_combos(in_combos, fn)
    -- compute all input possibilities
    local combo_i = {}
    local result  = {}

    local total = 1
    for j=1,#in_combos do
        combo_i[j] = 1
        if type(in_combos[j]) == "table" then
            result[j] = in_combos[j][1]
            total = total * #in_combos[j]
        else
            result[j] = in_combos[j]
        end
    end

    for k=1,total do
        fn(result)

        -- tick up, if we can't then carry to the next slot
        for i=1,#combo_i do
            local carry = false
            local e = in_combos[i]
            local idx = combo_i[i]
            if type(e) == "table" then
                idx = idx + 1
                if idx > #e then
                    idx = 1
                    carry = true
                end
                e = e[idx]
            else
                carry = true
            end

            result[i] = e
            combo_i[i] = idx
            if not carry then break end
        end
    end
end

function table_rep(n, x)
    local arr = {}
    for i=1,n do arr[i] = x end
    return arr
end

-- defines which patterns are equivalent
function NFA_final_key(n)
    return n.stage1_key
end

function NFA_done(n, state)
    return state > #n[3]
end

function NFA_match_at(n, state)
    return n[3][state]
end

function NFA_match_str(n, state)
    return n[3][state]:gsub("[a-zA-Z]", "_")
end

-- A matcher maps pattern strings to accept states, pattern strings
-- are variable arrays with variable strings inside.
function Matcher()
    local t = { patterns={}, parts=Partitions(), to_accept={} }

    function t:accept(pat, accept)
        self.parts:put(accept, pat)
        self.to_accept[pat] = accept
        table.insert(self.patterns, pat)
    end

    -- Compile terminator, might also advance to the next unit
    function t:compile0(q, state, depth)
        local indent = string.rep("    ", depth)

        local list = {}
        local max_len = 0
        for _,v in ipairs(q) do
            -- add_if_new(list, v.stage1_key:sub(1 + ((state - 1)*8)))
            add_if_new(list, string.format("%-5s %d:%s", v[1], state, v[3][state]))
            -- add_if_new(list, UF[v])
            max_len = math.max(max_len, #v[3])
        end

        for _,v in ipairs(list) do
            lines[#lines + 1] = string.format("%s// %s", indent, v)
        end

        if state >= max_len then
            lines[#lines + 1] = string.format("%sreturn SOMETHING;", indent)
        else
            local next = NFA_match_at(q[1], state + 1)
            if type(next) == "table" and next.op == "SHAPE" then
                lines[#lines + 1] = string.format("%sreturn DEC_%s(ctx, %s);", indent, next.fn, table.concat(next.args, ", "))
            else
                lines[#lines + 1] = string.format("%sinst = DEC_read(ctx, %d);", indent, MIN_UNIT_WIDTH / 8)

                local root_mask = table_rep(MIN_UNIT_WIDTH, 0)
                self:compile1(q, state + 1, root_mask, depth)
            end
        end
    end

    -- Take the list of patterns, q, and discriminate them by certain bits
    -- to narrow in on the answer. We start by just using unanimous opcode bits
    -- but sometimes that fails (cough cough A64) so we defer to splitting by
    -- whichever sequence of bits is *usually* treated as an opcode.
    function t:compile1(q, state, parent_mask, depth)
        -- if all live cases fit into the same final state then
        -- we've terminated
        local leader = nil
        for _,v in ipairs(q) do
            local k = self.to_accept[v]
            if not leader then leader = k
            elseif leader ~= k then leader = nil break end
        end

        if leader then
            return (self:compile0(q, state, depth))
        end

        -- tracks how often each bit is an opcode
        local histo = table_rep(MIN_UNIT_WIDTH, 0)
        for _,v in ipairs(q) do
            if not NFA_done(v, state) then
                local str = NFA_match_str(v, state)

                -- fill in fresh operand bits
                for i=1,MIN_UNIT_WIDTH do
                    if str:sub(i,i) ~= "_" then
                        histo[i] = histo[i] + 1
                    end
                end
            end
        end

        -- parent bits are automatically treated as operand to skip over
        -- them when discriminating further
        local possible_opcodes = 0
        for i=1,MIN_UNIT_WIDTH do
            if parent_mask[i] == 1 then
                histo[i] = 0
            elseif histo[i] > 0 then
                possible_opcodes = possible_opcodes + 1
            end
        end

        -- there's no more discriminating bits, give up, A64 does this a lot...
        if possible_opcodes == 0 then
            return (self:compile0(q, state, depth))
        end

        -- find sequence of potential opcode bits
        local best = 0
        for j=1,MIN_UNIT_WIDTH do
            if histo[j] > best then
                best = histo[j]
                lwb  = j
            end
        end
        assert(lwb > 0)

        -- check how far we can push the upper bound
        upb = lwb
        for j=upb+1,math.min(upb+4, MIN_UNIT_WIDTH) do
            if histo[j] == 0 then
                break
            end
            upb = j
        end

        local list = {}
        for _,v in ipairs(q) do
            add_if_new(list, v[3][1]:sub(lwb, upb))
        end
        print(string.format("%d %s lwb=%d upb=%d width=%d cnt=%d | %s", depth, string.rep("  ", depth), lwb, upb, 1 + (upb - lwb), #q, table.concat(list, " ")))

        local discrim = table_rep(MIN_UNIT_WIDTH, 0)
        for i=lwb,upb do discrim[i] = 1 end
        -- combine with the parent_mask now, this now represents the
        -- fully processed set of bits.
        for i=1,MIN_UNIT_WIDTH do
            if parent_mask[i] == 1 then discrim[i] = 1 end
        end

        -- once we've picked the bits to focus on, generate deltas per input
        local deltas = Partitions()
        for _,v in ipairs(q) do
            if not NFA_done(v, state) then
                local str = NFA_match_str(v, state)

                -- compute all input variants
                local list = {}
                for i=lwb,upb do
                    local ch = str:sub(i,i)
                    if ch == "0" or ch == "1" then
                        list[#list + 1] = ch
                    else
                        list[#list + 1] = { "0", "1" }
                    end
                end

                iter_combos(list, function(bits)
                    bits = table.concat(bits)
                    deltas:put_unique(bits, v)
                end)
            end
        end

        -- group deltas based on destination
        local cases = Partitions()
        local next_active = {}
        for k,list in deltas:iter() do
            local target_key = {}
            for i=1,#list do
                add_if_new(target_key, self.to_accept[list[i]])
            end

            target_key = table.concat(target_key, "|")
            if cases:put(target_key, k) then
                next_active[target_key] = list
            end
        end

        local bit_off = (MIN_UNIT_WIDTH - upb)
        local bit_len = 1 + (upb - lwb)
        local indent = string.rep("    ", depth)

        -- OR chain from the keys in a case
        local function gen_either_expr(list)
            local strs = {}
            for i=1,#list do
                strs[i] = "key == 0b"..list[i]
            end
            return table.concat(strs, " || ")
        end

        local list2 = {}
        for j=lwb,upb do
            list2[#list2 + 1] = histo[j]
        end

        lines[#lines + 1] = string.format("%s// %s, HISTO: %s", indent, table.concat(discrim), table.concat(list2, " "))
        if #cases.ord == 1 then
            local k = cases.ord[1]
            local next = next_active[k]
            local either = gen_either_expr(cases.entries[k])

            -- figure out if there's more cases where they don't match than
            -- cases where they do and flip the expr based on that.
            local total_cases = 2^bit_len
            if #cases.entries[k] == total_cases then
                return (self:compile0(q, state, depth))
            end
     
            lines[#lines + 1] = string.format("%sif (key = BEXTR(inst, %d, %d), %s) {", indent, bit_off, bit_len, either)
            self:compile1(next, state, discrim, depth+1)
            lines[#lines + 1] = indent.."}"
            return
        end

        lines[#lines + 1] = string.format("%sswitch (BEXTR(inst, %d, %d)) {", indent, bit_off, bit_len)
        for k,list in cases:iter() do
            local next = next_active[k]
            for i=1,#list - 1 do
                lines[#lines + 1] = string.format("%s    case 0b%s:", indent, list[i])
            end
            lines[#lines + 1] = string.format("%s    case 0b%s: {", indent, list[#list])
            print(string.format("%d %s KEY %s", depth, string.rep("  ", depth), table.concat(list, " ")))
            self:compile1(next, state, discrim, depth+2)
            lines[#lines + 1] = indent.."    }"
        end
        lines[#lines + 1] = indent.."}"
    end

    function t:compile()
        local root_mask = table_rep(MIN_UNIT_WIDTH, 0)
        self:compile1(self.patterns, 1, root_mask, 1)
    end

    return t
end

local first_matcher = Matcher()

if false then
local list = {
  { 0, 1, 2, 3 },
  3,
  { 0, 1 },
  5,
  { 0, 2 }
}

iter_combos(list, function(x) print("DUMP", inspect(x)) end)
os.exit(1)
end

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

funcs["prefix_opt"] = function(args)
    return {}
end

funcs["prefix"] = function(args)
    return {}
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
        -- print("BEXTR", str, slice, inspect(args), args[3] + 1, args[3] + args[4])
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

function eval0(item, globals, params, symbolic)
    if not symbolic and type(item) == "string" and item:sub(1, 1) == "$" then
        -- param to expand
        local k = item:sub(2)
        if not globals[k] then error("couldn't find "..item) end
        item = globals[k]
    elseif params[item] then
        item = params[item]
    end

    if type(item) == "table" and item[1] then
        -- expand sub-shape, these help us build cross-unit DFAs
        -- more efficiently.
        if shapes[item[1]] then
            local args = {}
            for j=2,#item do
                local arg = eval0(item[j], globals, params, true)
                args[j - 1] = arg
            end

            return { op="SHAPE", fn=item[1], args=args }
        else
            -- expand args, then call into pattern
            local fn = funcs[item[1]]
            if not fn then error("couldn't find "..item[1]) end

            if type(fn) == "table" then
                local args = clone_table(params)
                local fn_params = fn[3]
                for j=1,#fn_params do
                    local arg = eval0(item[1 + j], globals, params, true)
                    args[fn_params[j]] = arg
                end

                return eval(fn, 4, globals, args)
            else
                local args = {}
                for j=2,#item do
                    local arg = eval0(item[j], globals, params, true)
                    args[j - 1] = arg
                end
                -- print("EVAL", item[1], inspect(args), inspect(item))
                local ret = fn(args)
                -- print("> ", inspect(ret))
                return ret
            end
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

function compute_sub_shape_key(pat, pos)
    local operand_bits = {}
    local final_op = #pat
    for i=pos,#pat do
        if pat[i].op == "SHAPE" then
            final_op = i
            break
        end

        table.insert(operand_bits, pat[i])
    end

    -- trim the trailing chars
    local str = table.concat(operand_bits, "")
    local j = #str
    while j > 0 and str:sub(j,j):match("[a-zA-Z]") do
        j = j - 1
    end

    local trail_b = math.floor(j / MIN_UNIT_WIDTH)*MIN_UNIT_WIDTH
    if trail_b == MIN_UNIT_WIDTH and #str == MIN_UNIT_WIDTH then
        trail_b = 0
    end

    -- recompute lettering for pattern match
    str = str:sub(trail_b + 1)

    local newstr = {}
    local counter = 0
    local remap = {}
    for j=1,#str do
        local old = str:sub(j,j)
        if remap[old] then
            old = remap[old]
        elseif old == "0" or old == "1" then
            old = "0"
        else
            local ch = string.char(65 + counter)
            counter = counter + 1
            remap[old] = ch
            old = ch
        end

        newstr[j] = old
    end
    str = table.concat(newstr)

    local tail = ""
    if final_op then
        local f = pat[final_op]
        tail = string.format("_%s(%s)", f.fn, table.concat(f.args, ","))
    end
    return (str == "" and "VOID" or str)..tail
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
        if type(dst[i]) ~= "table" then
            local src = to_bits(dst[i])
            if #src < MIN_UNIT_WIDTH then
                -- pad to byte
                src = string.rep("0", MIN_UNIT_WIDTH - #src)..src
            end
            dst[i] = src
        end
    end

    -- Split pattern by the sub-stages, currently we only support one
    -- sub-stage but the math works out for whatever number we really
    -- desire i'm just lazy for now.
    local accept_key = compute_sub_shape_key(dst, 1)
    print(inspect(dst), accept_key)
    first_matcher:accept({ mnemonic, str, dst, shape }, accept_key)
end

function expand_shape(name, mnemonic, n, globals)
    print("DECL", name, mnemonic, n[2])
    for i=3,#n do
        if type(n[i]) == "table" and n[i][1] == "pat" then
            local state = clone_table(globals)
            local in_combos = {}
            for j=1,#n[i][2] do
                local k = string.format("%d", j - 1)
                local v = n[i][2][j]
                state[k] = v

                if decls[v] then
                    in_combos[v] = decls[v]
                else
                    in_combos[v] = v
                end
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
    elseif tree[1] == "let" then
        decls[tree[2]] = tree[3]
    elseif tree[1] == "defun" then
        funcs[tree[2]] = tree
    elseif tree[1] == "bits" then
        MIN_UNIT_WIDTH = tree[2]
    elseif tree[1] == "definst" then
        local globals = {}
        local i = 5
        while i <= #tree do
            if type(tree[i]) ~= "string" or tree[i]:sub(1, 1) ~= ":" then
                error("Expected param name, got "..inspect(tree[i]))
            end

            local k = tree[i]
            i = i + 1

            if i > #tree then
                error("Expected value for "..k..", got nothing")
            end

            globals[k:sub(2)] = tree[i]
            i = i + 1
        end

        if not shapes[tree[2]] then
            error("Unkown shape "..tree[2])
        end

        expand_shape(tree[3], tree[4], shapes[tree[2]], globals)
    end
end

first_matcher:compile()

for i=1,#patterns do
    local pat = patterns[i][3]
    local operand_bits = {}
    for j=1,#pat do
        operand_bits[j] = pat[j]
    end

    -- trim the trailing chars
    local str = table.concat(operand_bits, "")
    local j = #str
    while j > 0 and str:sub(j,j):match("[a-zA-Z]") do
        j = j - 1
    end

    local trail_b = math.floor(j / MIN_UNIT_WIDTH)*MIN_UNIT_WIDTH
    if trail_b == MIN_UNIT_WIDTH and #str == MIN_UNIT_WIDTH then
        trail_b = 0
    end

    -- recompute lettering for pattern match
    str = str:sub(trail_b + 1)

    local newstr = {}
    local counter = 0
    local remap = {}
    for j=1,#str do
        local old = str:sub(j,j)
        if remap[old] then
            old = remap[old]
        elseif old == "0" or old == "1" then
            old = "0"
        else
            local ch = string.char(65 + counter)
            counter = counter + 1
            remap[old] = ch
            old = ch
        end

        newstr[j] = old
    end
    str = table.concat(newstr)
    if str == "" then
        str = "VOID"
    end

    -- this defines what is congruent in the first
    -- stage which is mostly downstream of opcode
    -- parsing if possible.
    patterns[i].stage1_key = str
    print("PAT", patterns[i][1], i, inspect(pat), str)
end
print("PATTERN COUNT", #patterns)

local UF = {}
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

function NFA_dump(all_parts, q)
    for _,v in ipairs(q) do
        print("CASE", v[1], v[3][1])
    end
    print(inspect(q))
end

function NFA_compile_term(all_parts, q, state, depth)
    local indent = string.rep("    ", depth)

    local list = {}
    local max_len = 0
    for _,v in ipairs(q) do
        -- add_if_new(list, v.stage1_key:sub(1 + ((state - 1)*8)))
        -- add_if_new(list, v[1])
        -- add_if_new(list, UF[v])
        add_if_new(list, v[3][1])
        max_len = math.max(max_len, #v[3])
    end
    -- lines[#lines + 1] = string.format("%s// %d %d %s", indent, state, max_len, table.concat(list, ","))

    for _,v in ipairs(q) do
        lines[#lines + 1] = string.format("%s// %s %s", indent, v[3][1], v[1])
    end

    if state >= max_len then
        lines[#lines + 1] = string.format("%sreturn SOMETHING;", indent)
    else
        lines[#lines + 1] = string.format("%sinst = read(%d);", indent, MIN_UNIT_WIDTH / 8)
        NFA_compile0(all_parts, q, state + 1, nil, depth)
    end
end

local ALL_BITS = string.rep(MIN_UNIT_WIDTH, "1")
function NFA_compile0(all_parts, q, state, parent_mask, depth)
    -- if all live cases fit into the same final state then
    -- we've terminated
    local leader = nil
    for _,v in ipairs(q) do
        local k = UF[v]
        if not leader then leader = k
        elseif leader ~= k then leader = nil break end
    end

    if leader then
        NFA_compile_term(all_parts, q, state, depth)
        return
    end

    -- Track which bits are usually discriminating
    local histo = {}

    -- assume all bits are opcodes, then flip off
    local discrim = {}
    for i=1,MIN_UNIT_WIDTH do
        histo[i] = 0
        discrim[i] = "1"
    end

    for _,v in ipairs(q) do
        if not NFA_done(v, state) then
            local str = NFA_match_str(v, state)

            -- fill in fresh operand bits
            for i=1,MIN_UNIT_WIDTH do
                if discrim[i] == "1" and str:sub(i,i) == "_" then
                    discrim[i] = "0"
                end

                if str:sub(i,i) ~= "_" then
                    histo[i] = histo[i] + 1
                end
            end
        end
    end

    -- parent bits are automatically treated as operand to skip over
    -- them when discriminating further
    local possible_opcodes = 0
    if parent_mask then
        for i=1,MIN_UNIT_WIDTH do
            if parent_mask[i] == "1" then
                histo[i]   = 0
                discrim[i] = "0"
            end
        end
    end

    -- there's no more discriminating bits, give up
    for i=1,MIN_UNIT_WIDTH do
        if histo[i] > 0 then
            possible_opcodes = possible_opcodes + 1
        end
    end

    if possible_opcodes == 0 then
        NFA_compile_term(all_parts, q, state, depth)
        return
    end

    -- find sequence of potential opcode bits
    local i = 1
    local best_cand = nil
    while i <= MIN_UNIT_WIDTH do
        -- scan past operand 
        while i <= MIN_UNIT_WIDTH and discrim[i] == "0" do i = i + 1 end
        -- we're at the opcodes, scan past 
        local start = i
        while i <= MIN_UNIT_WIDTH and discrim[i] == "1" do i = i + 1 end

        if start < MIN_UNIT_WIDTH then
            local width = i - start
            if not best_cand or width > best_cand[1] then
                best_cand = { width, start, i - 1 }
            end
            -- print("CAND", start, i - 1, width)
        end
    end

    local lwb, upb
    if best_cand then
        lwb = best_cand[2]
        upb = best_cand[3]
    else
        -- print("NO PERFECT FIT, NEXT BEST ", table.concat(parent_mask), inspect(histo))
        -- pick the highest bits which aren't processed already
    end

    for j=0,MIN_UNIT_WIDTH do
        discrim[j] = "0"
    end

    for j=lwb,upb do
        discrim[j] = "1"
    end

    -- combine with the parent_mask now, this now represents the
    -- fully processed set of bits.
    if parent_mask then
        local missing = MIN_UNIT_WIDTH
        for i=1,MIN_UNIT_WIDTH do
            if parent_mask[i] == "1" then discrim[i] = "1" end
            if discrim[i] == "1" then missing = missing - 1 end  
        end
    end

    -- once we've picked the bits to focus on, generate deltas per input
    local deltas = Partitions()
    local known = KnownBits(MIN_UNIT_WIDTH)
    for _,v in ipairs(q) do
        if not NFA_done(v, state) then
            local str = NFA_match_str(v, state)

            -- compute all input variants
            local list = {}
            for i=lwb,upb do
                local ch = str:sub(i,i)
                if ch == "0" or ch == "1" then
                    list[#list + 1] = ch
                else
                    list[#list + 1] = { "0", "1" }
                end
            end

            iter_combos(list, function(bits)
                bits = table.concat(bits)
                if deltas:put_unique(bits, v) then
                    -- known:union(bits)
                end
            end)
        end
    end

    -- group deltas based on destination
    local cases = Partitions()
    local next_active = {}
    for k,list in deltas:iter() do
        -- print(k, #list)

        local target_key = {}
        for i=1,#list do
            add_if_new(target_key, UF[list[i]])
        end

        target_key = table.concat(target_key, "|")
        if cases:put(target_key, k) then
            next_active[target_key] = list
        end
    end

    local bit_off = (MIN_UNIT_WIDTH - upb)
    local bit_len = 1 + (upb - lwb)
    local indent = string.rep("    ", depth)

    -- OR chain from the keys in a case
    local function gen_either_expr(list)
        local strs = {}
        for i=1,#list do
            strs[i] = "key == 0b"..list[i]
        end
        return table.concat(strs, " || ")
    end

    local list2 = {}
    for j=lwb,upb do
        list2[#list2 + 1] = histo[j]
    end

    lines[#lines + 1] = string.format("%s// %s, HISTO: %s", indent, table.concat(discrim), table.concat(list2, " "))
    if #cases.ord == 1 then
        local k = cases.ord[1]
        local next = next_active[k]
        local either = gen_either_expr(cases.entries[k])

        -- figure out if there's more cases where they don't match than
        -- cases where they do and flip the expr based on that.
        local total_cases = 2^bit_len
        if #cases.entries[k] == total_cases then
            NFA_compile0(all_parts, next, state, discrim, depth)
            return
        end
 
        print("TOL", #cases.entries[k], total_cases)
        lines[#lines + 1] = string.format("%sif (key = BEXTR(inst, %d, %d), %s) {", indent, bit_off, bit_len, either)
        NFA_compile0(all_parts, next, state, discrim, depth+1)
        lines[#lines + 1] = indent.."}"
        return
    end

    lines[#lines + 1] = string.format("%sswitch (BEXTR(inst, %d, %d)) {", indent, bit_off, bit_len)
    for k,list in cases:iter() do
        local next = next_active[k]
        for i=1,#list - 1 do
            lines[#lines + 1] = string.format("%s    case 0b%s:", indent, list[i])
        end
        lines[#lines + 1] = string.format("%s    case 0b%s: {", indent, list[#list])
        print(string.format("%d %s KEY %s", depth, string.rep("  ", depth), table.concat(list, " ")))
        NFA_compile0(all_parts, next, state, discrim, depth+2)
        lines[#lines + 1] = indent.."    }"
    end
    lines[#lines + 1] = indent.."}"
end

if false then
local f = io.open("test.c", "w")
f:write(table.concat(lines, "\n"))
fclose()
end

print(table.concat(lines, "\n"))
print(DFA_state_count)
