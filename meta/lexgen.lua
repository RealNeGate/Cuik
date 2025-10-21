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
            local p = i
            subpat, i = parse_regex(str, i + 1)
            i = i + 1
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

            if #subpat == 2 then
                subpat = subpat[2]
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
    if type(pat) == "table" and pat[1] == "or" then
        pat = { pat }
    end

    rules[#rules + 1] = { tag=tag, pat=pat }
end

function complex_sigil_rule(str)
    local tab = {}
    tab[1] = str:byte(1)
    for i=2,#str do
        if false and str:byte(i) == str:byte(1) then
            tab[#tab + 1] = "S"
        else
            tab[#tab + 1] = str:byte(i)
        end
    end

    print(inspect(tab), str)
    return tab
end

function add_rules_raw(patterns, tag)
    for i=1,#patterns do
        local pat = complex_sigil_rule(patterns[i])
        rules[#rules + 1] = { tag=tag, pat=pat }
    end
end

add_rule("[A-Za-z_$][A-Za-z0-9_$]*",     "IDENT")
add_rule("(0|[1-9][0-9]+)",              "NUMBER")
add_rule("0[0-7]+",                      "NUMBER")
add_rule("0[bB][01]+",                   "NUMBER")
add_rule("0[xX][0-9A-Fa-f]+",            "NUMBER")
add_rules_raw(patterns,                  "SIGIL")

-- add_rule("(0|[1-9][0-9]+)(\\.[0-9]*)?",  "NUMBER")
-- add_rule("0[0-7]*(A|B)", "NUMBER")

print(inspect(rules))

to_tag = {}
function walk(n, id)
    print(inspect(n), id)
    to_tag[n] = id
    for i=1,#n do
        local kid = n[i]
        if type(kid) == "table" then
            walk(kid, id)
        end
    end
end

for i=1,#rules do
    walk(rules[i].pat, rules[i].tag)
end

local dfa_cache = {}
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

function dfa_transition(q_prime, curr, next)
    if next then
        -- print("", inspect(curr[1][curr[2]]), "=>", inspect(next[1][next[2]]))
        table.insert(q_prime, next)
    else
        -- print("", inspect(curr[1][curr[2]]), "=>", "END")
    end
end

function dfa_pattern_next(n)
    local k = { n[1], n[2]+1, n[3], n[4], n[5] }
    while k and k[2] > k[3] do
        k = k[5]
    end
    return k
end

function dfa_pattern_step(q_prime, og, n, input, peek)
    local pat  = n[1]
    local pos  = n[2]
    local edge = pat[pos]
    if type(edge) == "table" then
        if edge[1] == "or" then
            -- step into each
            local after = dfa_pattern_next(n)
            for i=2,#edge do
                dfa_pattern_step(q_prime, og, { edge, i, i, 0, after }, input, peek)
            end
        elseif edge[1] == "range" then
            if not input then
                for i=edge[2],edge[3] do
                    add_if_new(q_prime, i)
                end
            elseif input >= edge[2] and input <= edge[3] then
                dfa_transition(q_prime, og, n[5])
            end
        elseif edge[1] == "plus" or edge[1] == "star" then
            -- stars can always match against what's past them, plus can
            -- do that after matching once.
            local after = dfa_pattern_next(n)
            if after and (edge[1] == "star" or n[4] > 0) then
                dfa_pattern_step(q_prime, og, after, input, true)
            end

            local primed = n
            if edge[1] == "plus" then
                primed = { n[1], n[2], n[3], 1, n[5] }
            end
            dfa_pattern_step(q_prime, og, { edge, 2, 2, 0, primed }, input)
        else
            -- step into
            local curr = { edge, 1, #edge, 0, n[5] }
            local after = dfa_pattern_next(curr)
            curr[5] = after
            dfa_pattern_step(q_prime, og, curr, input, peek)

            -- assert(false, "TODO "..inspect(edge[1]))
        end
    else
        if not input then
            add_if_new(q_prime, edge)
        elseif edge == input then
            if not peek then
                n = dfa_pattern_next(n)
            end

            dfa_transition(q_prime, og, n)
        end
    end
end

local worklist = {}
function dfa_compile(state, q)
    -- accumulate the inputs based on what
    -- everyone can parse at this time
    local inputs = {}
    for i=1,#q do
        dfa_pattern_step(inputs, q[i], q[i], nil)
    end

    local chars = {}
    for i=1,#inputs do
        chars[i] = string.char(inputs[i])
    end

    -- print("Inputs", state, table.concat(chars))
    for i=1,#inputs do
        local input = inputs[i]
        -- print("INPUT", string.char(input))

        -- step forward
        local q_prime = {}
        for i=1,#q do
            local curr = q[i]
            dfa_pattern_step(q_prime, curr, curr, input)
        end

        if #q_prime > 0 then
            -- any states already exist for this active set?
            local next = nil
            for k=1,#dfa_cache do
                if array_match(q_prime, dfa_cache[k][1]) then
                    next = dfa_cache[k][2]
                    DFA[state][input] = next

                    -- print("CACHED", inspect(q_prime), next)
                    break
                end
            end

            if not next then
                next = dfa_gen_state()
                dfa_cache[#dfa_cache + 1] = { q_prime, next }
                -- print("ADD", input, inspect(q_prime), next)

                DFA[state][input] = next
                table.insert(worklist, { next, q_prime })
            end
        else
            DFA[state][input] = 1
        end
    end
end

-- Every pattern is currently matching for the first char
local active = {}
for i=1,#rules do
    -- { pat pos limit times prev }
    active[i] = { rules[i].pat, 1, #rules[i].pat, 0, nil }
end

table.insert(worklist, { 1, active })
while #worklist > 0 do
    local n = table.remove(worklist)
    dfa_compile(n[1], n[2])
end

-- Dump DFA
if true then
    for i=1,state_count do
        print("State", i)

        local delta = Partitions()
        for input,next in pairs(DFA[i]) do
            delta:put(next, string.char(input))
        end

        for next,list in delta:iter() do
            table.sort(list)
            print("", table.concat(list), "=>", next)
        end
        print()
    end
end

os.exit(0)

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
