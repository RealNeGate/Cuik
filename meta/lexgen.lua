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

rules = {}

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

function parse_regex(str, i)
    local pat = {}
    while i <= #str do
        local first = str:byte(i)
        local subpat = nil
        if str:byte(i) == string.byte("(") then
            subpat, i = parse_regex(str, i + 1)
        elseif str:byte(i) == string.byte(")") then
            break
        elseif str:byte(i) == string.byte("[") then
            subpat = {}
            subpat[#subpat + 1] = "or"

            i = i + 1
            while str:byte(i) ~= string.byte("]") do
                local first = str:byte(i)
                if first == string.byte("\\") then
                    i = i + 1
                    first = str:byte(i)
                end
                i = i + 1

                local last = first
                if str:sub(i, i) == "-" then
                    last = str:byte(i + 1)
                    if last == string.byte("\\") then
                        i = i + 1
                        last = str:byte(i)
                    end
                    i = i + 2
                end

                if first ~= last then
                    subpat[#subpat + 1] = { "range", first, last }
                else
                    subpat[#subpat + 1] = first
                end
            end
            i = i + 1
        else
            subpat = first
            i = i + 1
        end

        if str:byte(i) == string.byte("?") then
            subpat = { "or_none", subpat }
            i = i + 1
        elseif str:byte(i) == string.byte("*") then
            subpat = { "star", subpat }
            i = i + 1
        elseif str:byte(i) == string.byte("+") then
            subpat = { "plus", subpat }
            i = i + 1
        end
        pat[#pat + 1] = subpat

        if str:byte(i) == string.byte("|") then
            subpat, i = parse_regex(str, i + 1)

            -- not a real group
            if type(pat) == "table" and #pat == 1 then
                pat = pat[1]
            end

            if type(subpat) == "table" and subpat[1] == "or" then
                pat = { "or", pat }
                for i=2,#subpat do
                    pat[1+i] = subpat[i]
                end
            else
                pat = { "or", pat, subpat }
            end
        end
    end

    -- not a real group
    if type(pat) == "table" and #pat == 1 then
        pat = pat[1]
    end

    return pat, i
end

function add_rule(str, tag)
    local pat = parse_regex(str, 1)
    rules[#rules + 1] = { tag=tag, pat=pat }
end

function add_rules_raw(list)

end

add_rule("(A|B|C)", "A")
add_rule("[A-Za-z_$][A-Za-z0-9_$]*", "IDENT")

add_rule("(0|[1-9][0-9]+)(.[0-9]*)?",    "NUMBER")
add_rule("0[0-7]+",            "NUMBER")
add_rule("0[bB][01]+",         "NUMBER")
add_rule("0[xX][0-9A-Za-z]+",  "NUMBER")

print(inspect(rules))

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
        -- print("New class", str)
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
                    -- print(string.format("%-8s %-8s %s (CACHED)", str, pat, inspect(key)))
                    found = true
                    break
                end
            end

            if not found then
                -- print(string.format("%-8s %-8s %s", str, pat, inspect(key)))
                active[#active + 1] = key
            end
        end
    end

    local dfa_cache = {}
    function dfa_compile(state, q, pos, first)
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
                        -- print("CACHED", inspect(q_prime), next)
                        break
                    end
                end

                if not next then
                    next = dfa_gen_state()
                    dfa_cache[#dfa_cache + 1] = { q_prime, next }
                    -- print("ADD", inspect(q_prime), next)
                end

                -- print("IN", input, eq_names[input], pos, next)
                DFA[state][input] = next

                dfa_compile(next, q_prime, pos+1)
            end
        end
    end

    dfa_compile(1, active, 1, nil)
end

print("HHEHEHE")
os.exit(0)

local ident = 0 -- eq_match("A-Za-z$_", "IDENT")
for i=192,255 do
   eq_classes[i] = ident
end

local Q_class = dfa_gen_class("QUOTE")
eq_classes[string.byte("'")] = Q_class
eq_classes[string.byte("\"")] = Q_class

local num = 0 -- eq_match("0-9",      "NUM")
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

    -- print("Ident", (state-1)*6)
    -- print("Number", (num_state-1)*6)
end

-- Dump DFA
if true then
    for i=1,state_count do
        print("State", i)

        for input,next in pairs(DFA[i]) do
            print("", input, eq_names[input], "=>", next)
        end
        print()
    end
end

local lines = {}
lines[#lines + 1] = "static const char keywords[][16] = {"
for j=1,#keywords do
    lines[#lines + 1] = string.format("    \"%s\",", keywords[j])
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""

function add_lines(str)
    for s in str:gmatch("[^\r\n]+") do
        lines[#lines + 1] = s
    end
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
lines[#lines + 1] = string.format("static const uint8_t dfa[%d][%d] = {", eq_count, state_count)
for j=1,eq_count do
    local set = {}
    for i=1,state_count do
        if DFA[i][j] and DFA[i][j] ~= 1 then
            set[i] = DFA[i][j] - 1
        else
            set[i] = 0
        end
        set[i] = string.format("%2d", set[i])
    end

    if #set > 0 then
        set = table.concat(set, ", ")
        lines[#lines + 1] = string.format("    [%2d] = { %s },", j-1, set)
    end
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
lines[#lines + 1] = string.format("static const uint8_t lexer_final_state[%d] = {", state_count)
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
add_lines[[
static bool lexer_is_space(int ch) {
    uint64_t mask = (1ull << ' ') | (1ull << '\t') | (1ull << '\v') | (1ull << '\r') | (1ull << '\n') | (1ull << '/');
    return ch < 64 ? (mask >> ch) & 1 : false;
}

Token lexer_read(Lexer* restrict l) {
    unsigned char* current = l->current;
    Token t = { 0 };
    t.has_space = *current == ' ';

    // phase 1: skip non-token space
    //   branchless space skip
    current += (*current == ' ');
    //   non-token DFA
    retry: {
        // skip whitespace
        while (lexer_is_space(*current)) {
            t.hit_line = *current++ == '\n';
        }

        // check for comments
        if (current[0] == '/' && current[1] == '/') {
            __debugbreak();
        } else if (current[0] == '/' && current[1] == '*') {
            __debugbreak();
        }
    }

    unsigned char* start = current;
    unsigned char first  = *start;
    if (__builtin_expect(first == '\0', 0)) {
        return (Token){ 0 };
    }
    current++;

    // eval first char
    uint8_t eq_class = eq_classes[first];
    uint64_t state   = dfa[eq_class][0];

    if (state) {
        // eval rest
        for (;;) {
            uint8_t ch = *current;
            // read convert to class (compresses the DFA a lot)
            uint8_t eq_class = eq_classes[ch];
            if (ch == first) { eq_class = 1; }
            // eval DFA
            uint64_t next = dfa[eq_class][state];
            if (next == 0) break;
            state = next, current += 1;
        }
    }

    uint64_t tag = lexer_final_state[state];
    if (tag == 0) {
        // these tokens are gonna get converted to real atoms
        t.atom = atoms_put(current - start, start);
    } else {
        // these tokens have their contents embedded into the
        // Atom pointer.
        int length = current - start;
        assert(length <= 3);

        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);

        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));

        t.atom = (Atom) ((uintptr_t) (chars & mask) | (tag << 56ull));
    }

    // NOTE(NeGate): the lexer will modify code to allow for certain patterns
    // if we wanna get rid of this we should make virtual code regions
    if (__builtin_expect(current[0] == '\\' && (current[1] == '\r' || current[1] == '\n'), 0)) {
        __debugbreak();
    }

    l->current = current;
    t.location = encode_file_loc(l->file_id, start - l->start);
    return t;
}
]]
lines[#lines + 1] = ""

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
