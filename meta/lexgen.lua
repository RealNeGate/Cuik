local inspect = require "meta/inspect"
require "meta/prelude"

local keywords = {
    "auto",
    "break",
    "case",
    "char",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extern",
    "float",
    "for",
    "goto",
    "if",
    "inline",
    "int",
    "long",
    "register",
    "restrict",
    "return",
    "short",
    "signed",
    "sizeof",
    "static",
    "struct",
    "switch",
    "typedef",
    "union",
    "unsigned",
    "void",
    "volatile",
    "while",
    "_Alignas",
    "_Alignof",
    "_Atomic",
    "_Bool",
    "_Complex",
    "_Embed",
    "_Generic",
    "_Imaginary",
    "_Pragma",
    "_Noreturn",
    "_Static_assert",
    "_Thread_local",
    "_Typeof",
    "_Vector",
    "__asm__",
    "__attribute__",
    "__cdecl",
    "__stdcall",
    "__declspec",

    -- GLSL keywords
    "discard",
    "layout",
    "in",
    "out",
    "inout",
    "uint",
    "buffer",
    "uniform",
    "flat",
    "smooth",
    "noperspective",
    "vec2",
    "vec3",
    "vec4",
    "ivec2",
    "ivec3",
    "ivec4",
    "uvec2",
    "uvec3",
    "uvec4",
    "dvec2",
    "dvec3",
    "dvec4",
}

-- sparse set of rules, unified and optimized later
local patterns = {
    "[", "]", "{", "}", "(", ")",

    ";", ",", "~", "?", ":", "@",

    "=", "==",
    "#", "##",
    "!", "!=",
    "^", "^=",
    "*", "*=",
    "/", "/=",
    "%", "%=",

    "+", "++", "+=",
    "|", "||", "|=",
    "&", "&&", "&=",

    "-", "--", "-=", "->",
    "<", "<=", "<<", "<<=",
    ">", ">=", ">>", ">>=",

    ".", "..", "...",
}

local state_count = 1

local eq_count = 2
local eq_classes = {}
local eq_names = {}

eq_names[1] = "NIL"
eq_names[2] = "SAME"

-- DFA[state][input] = {}
local DFA = {}
for i=1,40 do
    DFA[i] = {}
end

function dfa_gen_class(tag)
    eq_count = eq_count + 1
    eq_names[eq_count] = tag
    return eq_count
end

function dfa_gen_state()
    state_count = state_count + 1
    return state_count
end

function dfa_range(first, last, from, to)
    for i=first,last do
        DFA[from][i] = to
    end
end

function eq_match(str, tag)
    local class = dfa_gen_class(tag)

    -- regex style subpattern
    local i = 1
    while i <= #str do
        local first = str:byte(i)
        if first == string.byte("\\", 1) then
            i = i + 1
            first = str:byte(i)
        end
        i = i + 1

        local last = first
        if str:sub(i, i) == "-" then
            last = str:byte(i + 1)
            if last == string.byte("\\", 1) then
                i = i + 1
                last = str:byte(i)
            end
            i = i + 2
        end

        for j=first,last do
            eq_classes[j] = class
        end
    end
    return class
end

-- print(inspect(rules))

-- Compile complex sigils
function complex_sigil_rule(str)
    local tab = {}
    tab[1] = "C"
    for i=2,#str do
        if str:byte(i,i) == str:byte(1,1) then
            tab[#tab + 1] = "S"
        else
            tab[#tab + 1] = str:sub(i, i)
        end
    end
    return table.concat(tab)
end

do
    local groups = Partitions()
    for i=1,#patterns do
        groups:put(patterns[i]:sub(1, 1), i)
    end

    local common = Partitions()
    for ch,list in groups:iter() do
        local hash = {}
        for _,pat_i in ipairs(list) do
            local pat = complex_sigil_rule(patterns[pat_i])
            hash[#hash + 1] = pat
        end
        hash = table.concat(hash, " | ")

        common:put_list(hash, list)
        -- print(ch, #list, hash)
    end
    -- print()

    -- each partition here gets its own equivalence class
    local spares = {}
    for hash,list in common:iter() do
        local chars = {}
        for i=1,#list do
            local ch = patterns[list[i]]:sub(1,1)
            add_if_new(chars, ch)
        end

        local class = dfa_gen_class(table.concat(chars, " "))
        for i=1,#list do
            local str = patterns[list[i]]
            local ch = patterns[list[i]]:byte(1)
            for j=2,#str do
                if str:byte(j,j) ~= str:byte(1,1) then
                    add_if_new(spares, str:byte(1,1))
                end
            end

            eq_classes[ch] = class
        end
        print(#list, hash, class)
    end

    for i=1,#spares do
        local str = string.char(spares[i])
        print("New class", str)
        eq_classes[spares[i]] = dfa_gen_class(str)
    end

    function key_str(k)
        local strs = {}
        for i=1,#k do
            strs[i] = k[i]
        end
        return table.concat(strs, ",")
    end

    function array_match(a, b)
        if #a ~= #b then return false end
        for i=1,#a do
            if type(a[i]) == type(b[i]) and type(a[i]) == "table" then
                if not array_match(a[i], b[i]) then return false end
            elseif a[i] ~= b[i] then
                return false
            end
        end
        return true
    end

    function array_slice(a, s, e)
        local b = {}
        for i=s,e do b[#b + 1] = a[i] end
        return b
    end

    local active = {}
    for hash,list in common:iter() do
        for i=1,#list do
            local str = patterns[list[i]]
            local pat = complex_sigil_rule(str)

            local key = {}
            for j=1,#str do
                if pat:sub(j,j) == "S" then
                    key[#key + 1] = 2
                else
                    key[#key + 1] = eq_classes[str:byte(j)]
                end
            end

            local found = false
            for k=1,#active do
                if array_match(key, active[k]) then
                    print(string.format("%-8s %-8s %s (CACHED)", str, pat, inspect(key)))
                    found = true
                    break
                end
            end

            if not found then
                print(string.format("%-8s %-8s %s", str, pat, inspect(key)))
                active[#active + 1] = key
            end
        end
    end

    local dfa_cache = {}
    function dfa_compile(state, q, pos, first)
        print("A")
        for input=1,eq_count do
            -- step forward
            local q_prime = {}
            for i=1,#q do
                if q[i][1] == input then
                    q_prime[#q_prime + 1] = array_slice(q[i], 2, #q[i])
                end
            end

            if #q_prime > 0 then
                -- any states already exist for this active set?
                local next = nil
                for k=1,#dfa_cache do
                    if array_match(q_prime, dfa_cache[k][1]) then
                        next = dfa_cache[k][2]
                        print("CACHED", inspect(q_prime), next)
                        break
                    end
                end

                if not next then
                    next = dfa_gen_state()
                    dfa_cache[#dfa_cache + 1] = { q_prime, next }
                    print("ADD", inspect(q_prime), next)
                end

                print("IN", input, eq_names[input], pos, next)
                DFA[state][input] = next

                dfa_compile(next, q_prime, pos+1)
            end
        end
    end

    dfa_compile(1, active, 1, nil)
end

local ident = eq_match("A-Za-z$_", "IDENT")
for i=192,255 do
   eq_classes[i] = ident
end

local Q_class = dfa_gen_class("QUOTE")
eq_classes[string.byte("'")] = Q_class
eq_classes[string.byte("\"")] = Q_class

local num = eq_match("0-9",      "NUM")
local dot = eq_classes[string.byte(".")]

do
    -- Define identifier: IDENT(IDENT | NUM)*
    local state = dfa_gen_state()

    DFA[1][ident]       = state
    DFA[state][ident]   = state
    DFA[state][num]     = state
    DFA[state][2]       = state

    -- Number: (0 IDENT)? (NUM | IDENT)+
    local num_state = dfa_gen_state()
    DFA[1][num]             = num_state
    DFA[num_state][num]     = num_state
    DFA[num_state][ident]   = num_state
    DFA[num_state][2]       = num_state

    DFA[1][Q_class] = 2

    print("Ident", (state-1)*6)
    print("Number", (num_state-1)*6)
end

-- Dump DFA
for i=1,state_count do
    print("State", i)

    for input,next in pairs(DFA[i]) do
        print("", input, eq_names[input], "=>", next)
    end
    print()
end

function imul32(a, b)
    local lo = bit.band(a, 0xFFFF) * bit.band(b, 0xFFFF)
    local hi = bit.band(bit.rshift(a, 16), 0xFFFF) * bit.band(bit.rshift(b, 16), 0xFFFF)
    return bit.lshift(hi, 16) + lo
end

function hash(k, a)
    local m = 24
    -- k = bit.bxor(bit.rshift(k, 32 - m))
    k = bit.band(imul32(a, k), 0xFFFFFFFF)
    return bit.rshift(k, 32 - m)
end

function read32(str, s, e)
    e = math.min(e, #str)
    local b = 0
    for i=s,e do
        local ch = str:byte(i)
        b = bit.bor(b, bit.lshift(ch, (i-s)*8))
    end
    return b
end

function hash_function(a, str)
    local x = hash(read32(str, 1, 4), a)
    local y = hash(read32(str, 5, 8), a)
    if false and str == "typedef" then
        local z = read32(str, 1, 4)
        print(string.format("%#x * %#x = %#x", z, a, imul32(a, z)))
        print(string.format("%s: %d %#x %#x %#x %#x\n", str, a, x, y, read32(str, 1, 4), read32(str, 5, 8)))
    end
    return (x + y) % 179
end

local a = 166379777
local lines = {}
-- for trial=1,10000 do
local trial = 0
local highest = 0
while true do
    trial = trial + 1
    -- print("Trial", a, b, trial, string.format("%#x", a))

    local slots = {}
    local bad = false
    for j=1,#keywords do
        local i = hash_function(a, keywords[j])
        -- print(string.format("%-16s => %d", keywords[j], i))

        if slots[i] then
            -- print(string.format("Trial %3d/0x%s: %.1f%%: collision %#x ('%s' %#x vs '%s' %#x)", trial, bit.tohex(a), (j / #keywords) * 100, i-1, keywords[j], h, slots[i], hash_function(a, slots[i])))
            local prob = (j-1) / #keywords
            if prob > highest then
                print(string.format("Trial %3d/0x%s: %.1f%%", trial, bit.tohex(a), prob*100))
            end
            highest = math.max(highest, prob)
            bad = true
            break
        end
        slots[i] = keywords[j]
    end

    if not bad then
        lines[#lines + 1] = "#define PERFECT_HASH_SEED UINT32_C("..a..")"
        lines[#lines + 1] = "static const uint8_t keywords_table[256] = {"
        for j=1,#keywords do
            local i = hash_function(a, keywords[j])
            lines[#lines + 1] = string.format("    [%d] = %d, // %s", i, j-1, keywords[j])
        end
        lines[#lines + 1] = "};"
        lines[#lines + 1] = ""
        lines[#lines + 1] = "static const char keywords[][16] = {"
        for j=1,#keywords do
            lines[#lines + 1] = string.format("    \"%s\",", keywords[j])
        end
        lines[#lines + 1] = "};"
        lines[#lines + 1] = ""
        break
    end

    a = a + 1
end

lines[#lines + 1] = "static const uint8_t eq_classes[256] = {"
local range_start = 1
for j=0,256 do
    if eq_classes[j] ~= eq_classes[range_start] then
        -- print("R", j, range_start, eq_classes[j], eq_classes[range_start])
        if eq_classes[range_start] then
            if j-1 == range_start then
                lines[#lines + 1] = string.format("    [%d] = %d,", j-1, eq_classes[range_start]-1)
            else
                lines[#lines + 1] = string.format("    [%d ... %d] = %d,", range_start, j-1, eq_classes[range_start]-1)
            end
        end
        range_start = j
    end
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
lines[#lines + 1] = string.format("static const uint64_t dfa[%s] = {", eq_count)
for j=1,eq_count do
    local vec = {}
    for i=1,state_count do
        if DFA[i][j] and DFA[i][j] ~= 1 then
            vec[#vec + 1] = string.format("(%dull << %dull)", (DFA[i][j] - 1)*6, (i - 1)*6)
        end
    end

    if #vec > 0 then
        lines[#lines + 1] = string.format("    [%d] = %s, // EQ%d", j-1, table.concat(vec, " | "), j)
    end
end
lines[#lines + 1] = "};"

local final_src = table.concat(lines, "\n")
local f = io.open("cuik_pp/dfa.h", "w")
f:write(final_src)
f:close()

lines = {}
for i=1,#keywords do
    local base = keywords[i]

    local j = 1
    while j < #base and base:sub(j, j) == "_" do
        j = j + 1
    end
    base = base:sub(j)

    j = #base
    while j > 1 and base:sub(j, j) == "_" do
        j = j - 1
    end
    base = base:sub(1, j)

    local str = ""
    if i == 1 then
        str = " = 0x800000"
    end

    lines[#lines + 1] = string.format("TOKEN_KW_%s%s,", base, str)
end

final_src = table.concat(lines, "\n")
f = io.open("cuik_pp/keywords.h", "w")
f:write(final_src)
f:close()

-- print(table.concat(lines, "\n"))
-- print(state_count, eq_count)
