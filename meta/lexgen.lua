local inspect = require "meta/inspect"
require "meta/prelude"

if type(jit) == "table" then
    ffi = require "ffi"
end

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
local dfa_to_tag = {}

-- DFA[state][input] = {}
local DFA = {}
function dfa_gen_state()
    state_count = state_count + 1
    if not DFA[state_count] then
        DFA[state_count] = {}
    end
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
        elseif str:byte(i) == string.byte(".") then
            subpat = "any"
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
    if type(pat) == "table" and #pat == 1 and type(pat[1]) == "table" then
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
add_rule("\"",                           "QUOTE")
add_rule("\'",                           "QUOTE")
add_rule("L\"",                          "QUOTE")
add_rule("L\'",                          "QUOTE")
add_rules_raw(patterns,                  "SIGIL")

-- add_rule("(0|[1-9][0-9]+)(\\.[0-9]*)?",  "NUMBER")
-- add_rule("0[0-7]*(A|B)", "NUMBER")
-- print(inspect(rules))

to_tag = {}
function walk(n, id)
    -- print(inspect(n), id)
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
    if type(next) == "string" then
        -- print("", inspect(curr[1][curr[2]]), "=>", next)
        table.insert(q_prime, next)
    elseif next then
        -- print("", inspect(curr[1][curr[2]]), "=>", inspect(next[1][next[2]]))
        table.insert(q_prime, next)
    else
        -- print("", inspect(curr[1][curr[2]]), "=>", "END")
    end
end

function dfa_pattern_next(n)
    local k = { n[1], n[2]+1, n[3], n[4], n[5] }
    while type(k) == "table" and k[2] > k[3] do
        k = k[5]
    end

    if not k then
        return to_tag[n[1]]
    else
        return k
    end
end

function dfa_pattern_step(q_prime, og, n, input, peek)
    if type(n) ~= "table" then
        return
    end

    local pat  = n[1]
    local pos  = n[2]
    local edge = pat[pos]
    if edge == "any" then
        if not input then
            for i=1,255 do
                add_if_new(q_prime, i)
            end
        else
            dfa_transition(q_prime, og, n[5])
        end
    elseif type(edge) == "table" then
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

function int_array_to_str(inputs)
    local chars = {}
    for i=1,#inputs do
        chars[i] = string.char(inputs[i])
    end
    table.sort(chars)
    return table.concat(chars)
end

DFA[1] = {}

local worklist = {}
function dfa_compile(state, q)
    -- accumulate the inputs based on what
    -- everyone can parse at this time
    local inputs = {}
    for i=1,#q do
        dfa_pattern_step(inputs, q[i], q[i], nil)
    end

    -- print("Inputs", state, int_array_to_str(inputs))
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

                local leader = nil
                for k=1,#q_prime do
                    local tag = q_prime[k]
                    if type(q_prime[k]) == "string" then
                        tag = q_prime[k]
                    else
                        tag = to_tag[q_prime[k][1]]
                    end

                    if not leader then
                        leader = tag
                    elseif leader ~= tag then
                        leader = nil
                        break
                    end
                end

                if leader then
                    dfa_to_tag[next] = leader
                end
                -- print("ADD", input, inspect(q_prime), next, leader)

                DFA[state][input] = next
                table.insert(worklist, { next, q_prime })
            end
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
function dump_dfa(eq_classes)
    if eq_classes then
        for i=1,state_count do
            print("State", i-1, dfa_to_tag[i])
            for input,next in pairs(DFA[i]) do
                local eq = eq_classes:get(eq_classes:at(input))
                print("", input, int_array_to_str(eq), "=>", next-1)
            end
            print()
        end
    else
        for i=1,state_count do
            print("State", i, dfa_to_tag[i])

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
end

-- dump_dfa()

local eq_classes = Partitions()
local eq_cnt = 1
local to_eq = {}
do
    local non_F = {}
    local to_part = {}

    local inv_deltas = {}
    for i=1,state_count do
        inv_deltas[i] = {}
    end

    local F = {}
    local P = { non_F }
    for i=1,state_count do
        local deltas = 0
        for input,next in pairs(DFA[i]) do
            table.insert(inv_deltas[next], { input, i })
            deltas = deltas + 1
        end

        if deltas == 0 then
            local tag = dfa_to_tag[i]
            assert(tag, i)

            local A = F[tag]
            if A then
                table.insert(A, i)
                to_part[i] = A
            else
                F[tag] = { i }
                to_part[i] = F[tag]
                table.insert(P, F[tag])
            end
        else
            table.insert(non_F, i)
            to_part[i] = non_F
        end
    end

    local W = {}
    for i=1,#P do
        W[i] = P[i]
    end

    while #W > 0 do
        local A = table.remove(W)

        local sigma = {}
        local uses  = {}
        for i=1,#A do
            local d = inv_deltas[A[i]]
            for j=1,#d do
                add_if_new(sigma, d[j][1])
                add_if_new(uses,  d[j])
            end
        end

        -- print("WORK", inspect(A), int_array_to_str(sigma))
        for i=1,#sigma do
            local touched = {}
            local touched_n = {}
            local input = sigma[i]

            for j=1,#uses do
                if input == uses[j][1] then
                    touched_n[uses[j][2]] = true
                    add_if_new(touched, to_part[uses[j][2]])
                end
            end

            for j=1,#touched do
                local Z = touched[j]
                local split = 0
                for k=1,#Z do
                    if touched_n[Z[k]] then
                        split = split + 1
                    end
                end

                if split > 0 and split < #Z then
                    -- print("SPLIT", #Z, split)
                    local new_z = {}

                    local k = 1
                    while k <= #Z do
                        if touched_n[Z[k]] then
                            to_part[Z[k]] = new_z
                            table.insert(new_z, Z[k])

                            -- remove swap
                            Z[k]  = Z[#Z]
                            Z[#Z] = nil
                        else
                            k = k + 1
                        end
                    end

                    -- print("", string.char(input), ":", inspect(Z), inspect(new_z))

                    table.insert(P, new_z)
                    table.insert(W, new_z)
                end
            end
        end
    end

    -- print()

    local dual = Partitions()
    for i=1,#P do
        local A = P[i]

        local sigma = {}
        local uses  = {}
        for j=1,#A do
            local d = inv_deltas[A[j]]
            for k=1,#d do
                add_if_new(sigma, d[k][1])
                dual:put_if_new(d[k][1], i)
            end
        end

        -- print("PART", i, inspect(A), int_array_to_str(sigma))
    end

    for input,list in dual:iter() do
        local key = table.concat(list,",")
        eq_classes:put(key, input)
    end

    -- print()

    for k,list in eq_classes:iter() do
        local id = eq_cnt
        eq_cnt = eq_cnt + 1

        for i=1,#list do
            to_eq[list[i]] = id
        end
        -- print("CLASS", id, int_array_to_str(list))
    end

    local new_DFA = {}
    local new_state_count = 0
    local new_to_tag = {}
    local function walk(A)
        if A.leader then
            return A.leader
        end

        new_state_count = new_state_count + 1
        local from = new_state_count
        A.leader = from
        new_DFA[new_state_count] = {}
        new_to_tag[new_state_count] = dfa_to_tag[A[1]]

        for input,next in pairs(DFA[A[1]]) do
            local to = walk(to_part[next])
            local eq = to_eq[input]
            new_DFA[from][eq] = to
        end
        return from
    end
    walk(to_part[1])

    dfa_to_tag = new_to_tag
    state_count = new_state_count
    DFA = new_DFA
end

-- dump_dfa(eq_classes)

local lines = {}
function add_lines(str)
    for s in str:gmatch("[^\r\n]+") do
        lines[#lines + 1] = s
    end
end

lines[#lines + 1] = "static const uint8_t eq_classes[256] = {"
local range_start = 1
for j=0,15 do
    local row = {}
    for i=0,15 do
        local k = j*16 + i
        local v = to_eq[k] and to_eq[k] or 0
        row[i+1] = string.format("%-2d", v, k)
    end
    lines[#lines + 1] = string.format("    %s, // 0x%02x", table.concat(row, ", "), j*16)
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
lines[#lines + 1] = string.format("static const uint8_t dfa[%d][%d] = {", eq_cnt+1, state_count)
for j=1,eq_cnt do
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
        lines[#lines + 1] = string.format("    [%2d] = { %s },", j, set)
    end
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""

local tag_enums = {}
for i=1,state_count do
    if dfa_to_tag[i] then
        add_if_new(tag_enums, "L_"..dfa_to_tag[i])
    end
end

lines[#lines + 1] = string.format("enum { L_ERROR, %s };", table.concat(tag_enums, ", "))
lines[#lines + 1] = string.format("static const uint8_t lexer_final_state[%d] = {", state_count)
for i=1,state_count do
    if dfa_to_tag[i] then
        lines[#lines + 1] = string.format("    [%d] = L_%s,", i - 1, dfa_to_tag[i])
    end
end
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
add_lines[[
static bool lexer_is_space(int ch) {
    uint64_t mask = (1ull << ' ') | (1ull << '\t') | (1ull << '\v') | (1ull << '\r') | (1ull << '\n');
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
            t.hit_line |= *current++ == '\n';
        }

        // check for comments
        if (current[0] == '/' && current[1] == '/') {
            current += 2;

            // skip until whitespace
            while (*current && *current != '\n') {
                current++;
            }

            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
            t.hit_line = true;
            goto retry;
        } else if (current[0] == '/' && current[1] == '*') {
            current += 2;

            // skip until comment end
            while (current[0] && !(current[0] == '*' && current[1] == '/')) {
                t.hit_line |= *current++ == '\n';
            }

            current += 2;
            goto retry;
        } else if (current[0] == '\\' && (current[1] == '\r' || current[1] == '\n')) {
            current += 1;
            current += (current[0] + current[1] == '\r' + '\n') ? 2 : 1;
            goto retry;
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
            // eval DFA
            uint64_t next = dfa[eq_class][state];
            if (next == 0) break;
            state = next, current += 1;
        }
    }

    t.content = (String){ current - start, start };

    uint64_t tag = lexer_final_state[state];
    if (tag == L_IDENT) {
        t.type = TOKEN_IDENTIFIER;
    } else if (tag == L_NUMBER) {
        t.type = TOKEN_INTEGER;
    } else if (tag == L_QUOTE) {
        char quote_type = current[-1];
        for (; *current && *current != quote_type; current++) {
            // skip escape codes
            if (*current == '\\') {
                // this will skip twice because of the for loop's next
                //  \  "  . . .
                //  ^     ^
                //  old   new
                current += 1;
            }
        }

        t.type = quote_type;
        if (start[0] == 'L') {
            t.type += 256;
            start += 1;
        }

        t.content = (String){ current - (start + 1), start + 1 };
        current += 1;
    } else {
        // these tokens have their contents embedded into the
        // Atom pointer.
        int length = current - start;
        assert(length <= 3);

        uint32_t mask = UINT32_MAX >> ((4 - length) * 8);

        // potentially unaligned access :P
        uint32_t chars;
        memcpy(&chars, start, sizeof(uint32_t));

        // t.atom = (Atom) ((uintptr_t) (chars & mask) | (tag << 56ull));
        t.type = chars & mask;
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

local keyword_table = {}
local prime = 173
if type(jit) == "table" then
    print("A", #keywords)

    local tries = 0
    local time = os.clock()

    local A = 0xbf58476d1ce4e5b9ull
    local B = 0x94d049bb133111ebull

    local hashes = {}
    for i=1,#keywords do
        local base = keywords[i]
        local x = 0ull
        for i=1,math.min(#base, 8) do
            local y = ffi.new("uint64_t", string.byte(base, i))
            x = bit.bor(x, bit.lshift(y, (i-1) * 8))
        end

        -- splittable64
        local y = x
        -- x = bit.bxor(x, bit.rshift(x, 30))
        -- x = x * A
        -- x = bit.bxor(x, bit.rshift(x, 27))
        -- x = x * B
        -- x = bit.bxor(x, bit.rshift(x, 31))
        hashes[i] = x

        x = bit.tohex(x)
        if keyword_table[x] then
            print("Woah!!!", keywords[keyword_table[x]], base, x)
        end
        keyword_table[x] = i
        -- print(base, x, bit.tohex(y))
    end

    keyword_table = {}
    local magic = 0xb8618d7571637e57ull
    local best = #keywords
    while true do
        local collide = 0

        for i=0,prime-1 do
            keyword_table[i] = nil
        end

        for i=1,#keywords do
            local base = keywords[i]
            local x = hashes[i]
            local h = bit.tobit((x * magic) % prime)
            if keyword_table[h] then
                collide = collide + 1
            end
            keyword_table[h] = i
        end

        if collide == 0 then
            print("Yippee!!! ", bit.tohex(magic))
            break
        end

        if collide < best then
            print("Fail", bit.tohex(magic), collide, best)
            best = collide
        end

        local x = magic
        x = bit.bxor(x, bit.rshift(x, 30))
        x = x * A
        x = bit.bxor(x, bit.rshift(x, 27))
        x = x * B
        magic = bit.bxor(x, bit.rshift(x, 31))

        tries = tries + 1
        if tries == 100000 then
            local elapsed = os.clock() - time
            print("Report ", tries / elapsed)
            time = os.clock()
            tries = 0
        end
    end

    lines[#lines + 1] = string.format("#define LEXER_KEYWORD_HASH(x) ((x * 0x%sull) %% %d)", bit.tohex(magic), prime)
    lines[#lines + 1] = "static const char keywords["..prime.."][16] = {"
    for i=0,prime-1 do
        local j = keyword_table[i]
        if j then
            lines[#lines + 1] = string.format("    [%d] = \"%s\",", i, keywords[j])
        end
    end
    lines[#lines + 1] = "};"
    lines[#lines + 1] = ""
else

end

local final_src = table.concat(lines, "\n")
local f = io.open("cuik_pp/dfa.h", "w")
f:write(final_src)
f:close()

if true then
    lines = {}
    for i=0,prime-1 do
        local k = keyword_table[i]
        if k then
            local base = keywords[k]
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

            lines[#lines + 1] = string.format("TOKEN_KW_%s = 0x800000 + %d,", base, i)
        end
    end

    final_src = table.concat(lines, "\n")

    f = io.open("cuik_pp/keywords.h", "w")
    f:write(final_src)
    f:close()
end

-- print(state_count, eq_count)
