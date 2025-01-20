local buffer  = require "string.buffer"
local inspect = require "meta/inspect"

function string:starts_with(start)
    return self:sub(1, #start) == start
end

function read_all(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

--------------------------
-- lexer
--------------------------
local ch_class = { [32] = "ws", [9] = "ws", [10] = "ws" }
for i=33,126 do ch_class[i] = "sym" end
for i=48,57  do ch_class[i] = "num" end
for i=65,90  do ch_class[i] = "ident" end
for i=97,122 do ch_class[i] = "ident" end
ch_class[36] = "ident"
ch_class[95] = "ident"

local function either(a, b, c) return a == b or a == c end
local function lexer(str)
    local i = 1
    return function()
        -- skip whitespace
        while ch_class[str:byte(i)] == "ws" do
            i = i + 1
        end

        if i > #str then
            return nil
        end

        local start = i
        local class = ch_class[str:byte(i)]
        if str:byte(i) == 34 then
            i = i + 1
            while str:byte(i) ~= 34 do
                i = i + 1
            end
            i = i + 1
            return str:sub(start+1, i-2)
        elseif str:byte(i) == 46 then
            i = i + 1
            while str:byte(i) == 46 do
                i = i + 1
            end
            return tonumber(str:sub(start, i - 1))
        elseif class == "num" then
            i = i + 1
            while ch_class[str:byte(i)] == "num" do
                i = i + 1
            end
            return tonumber(str:sub(start, i - 1))
        elseif class == "ident" then
            i = i + 1
            while either(ch_class[str:byte(i)], "ident", "num") do
                i = i + 1
            end
            return str:sub(start, i - 1)
        elseif class == "sym" then
            if str:byte(i) == string.byte("=") and str:byte(i + 1) == string.byte(">") then
                i = i + 2
            else
                i = i + 1
            end
            return str:sub(start, i - 1)
        else
            error("fuck but in lexing")
        end
    end
end

local source = [[
        (TB_LOAD dt=$dt $ctrl $mem (TB_PTR_OFFSET ___ $base ($imm: TB_ICONST ...))) where "fits_into_int32($dt, $imm)" => (x86_mov dt=$dt mode=MODE_LD $ctrl $mem $base disp="as_int32($imm)")
    ]]

local dfa = {}
local depth = 0
local state_count = 0

local captures = {}
local capture_count = 0

local accept = {}
local lex = lexer(source)

local function parse_node()
    local n = {}
    local t = lex()
    while t ~= ")" do
        if t == "(" then
            n[#n + 1] = parse_node()
            t = lex()
        else
            local peek = lex()
            if peek == "=" then
                n[t] = lex()
                t = lex()
            else
                n[#n + 1] = t
                t = peek
            end
        end
    end
    return n
end

local function dfa_insert(ty, old)
    if dfa[ty] == nil then
        dfa[ty] = {}
    end

    if dfa[ty][old] == nil then
        dfa[ty][old] = state_count
        state_count = state_count + 1
        return state_count - 1
    else
        return dfa[ty][old]
    end
end

local function dfa_compile(n, head, depth)
    local ty = n[1].."+1"
    if dfa[ty] == nil then
        dfa[ty] = {}
    end

    dfa[ty][head] = "R_PUSH("..state_count..")"
    head = state_count
    state_count = state_count + 1

    for i=1,#n do
        local t = n[i]
        if type(t) == "table" then
            head = dfa_compile(t, head, depth + 1)
        elseif t == "..." then
            -- keep chewing up any nodes in this guy
            local ty = 0
            if dfa[ty] == nil then
                dfa[ty] = {}
            end

            if dfa[ty][head] == nil then
                dfa[ty][head] = head
            else
                head = dfa[ty][head]
            end
        elseif t == "___" then
            head = dfa_insert("TB_NULL+1", head)
        elseif t:byte(1) == string.byte("$") then -- capture
            if dfa[0] == nil then
                dfa[0] = {}
            end

            captures[t] = "captures["..capture_count.."]"

            dfa[0][head] = "R_CAPTURE("..state_count..", "..capture_count..")"
            head = state_count
            capture_count = capture_count + 1
            state_count = state_count + 1
        end
    end

    -- pop once we've exhausted all the inputs
    if depth == 0 then
        dfa["TB_NULL+1"][head] = "R_POP("..head..")"
        return head
    else
        dfa["TB_NULL+1"][head] = "R_POP("..state_count..")"
        state_count = state_count + 1
        return state_count - 1
    end
end

local node_cnt = 0
function write_node(strs, ids, n)
    -- create kids first
    local in_cnt = 0
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            write_node(strs, ids, v)
        elseif v:byte(1) ~= string.byte("$") then
            -- non-captures? maybe properties?
        end
        in_cnt = in_cnt + 1
    end

    -- assign unique IDs
    ids[n] = node_cnt
    node_cnt = node_cnt + 1

    -- declaration of a node
    local node_type = n[1]
    local extra_type = "X86MemOp"

    local dt_name = n.dt
    strs[#strs + 1] = string.format("    TB_Node* k%d = tb_alloc_node(f, %s, %s, %d, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type);

    -- input edges
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            strs[#strs + 1] = string.format("    set_input(f, k%d, k%d, %d);", ids[n], ids[v], i - 2)
        elseif v:byte(1) == string.byte("$") then
            strs[#strs + 1] = string.format("    set_input(f, k%d, %s, %d);", ids[n], captures[v], i - 2)
        end
    end

    strs[#strs + 1] = string.format("    %s* k%d_extra = TB_NODE_GET_EXTRA(k%d);", extra_type, ids[n], ids[n])
    for k,v in pairs(n) do
        if type(k) == "string" and k ~= "dt" then
            -- replace all uses of captures with their real name
            local expr = v
            for k2,v2 in pairs(captures) do
                if k2 ~= v2 then
                    expr = string.gsub(expr, k2, v2)
                end
            end

            strs[#strs + 1] = string.format("    k%d_extra->%s = %s;", ids[n], k, expr)
        end
    end

    strs[#strs + 1] = ""
end

function find_non_node_captures(strs, n, expr)
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            find_non_node_captures(strs, v, expr.."->inputs["..(i-2).."]")
        end
    end

    for k,v in pairs(n) do
        -- captured field
        if type(k) == "string" and v:byte(1) == string.byte("$") then
            captures[v] = v
            strs[#strs + 1] = string.format("    TB_DataType %s = %s->%s;", v, expr, k)
        end
    end
end

while true do
    local t = lex()
    if t == nil then
        break
    elseif t ~= "(" then
        print("fuck but in parsing")
        os.exit(1)
    end

    local pattern = parse_node()
    local head = dfa_compile(pattern, 0, 0)
    -- print(inspect(pattern))

    t = lex()
    local where = nil
    if t == "where" then
        where = lex()

        t = lex()
        print(t)
    end

    if t ~= "=>" then
        print("fuck but in parsing")
        os.exit(1)
    end

    local replacement = nil
    t = lex()
    if t == "(" then
        replacement = parse_node()
    end

    print(inspect(replacement))

    local strs = {}
    local ids = {}
    node_cnt = 0

    strs[#strs + 1] = "do {"
    find_non_node_captures(strs, pattern, "n")
    if where then
        -- replace all uses of captures with their real name
        for k,v in pairs(captures) do
            if k ~= v then
                where = string.gsub(where, k, v)
            end
        end

        strs[#strs + 1] = "    if (!("..where..")) {"
        strs[#strs + 1] = "        break;"
        strs[#strs + 1] = "    }"
    end
    strs[#strs + 1] = ""
    write_node(strs, ids, replacement)
    strs[#strs + 1] = string.format("    return k%d;", ids[replacement])
    accept[head] = strs
    strs[#strs + 1] = "} while (0);"

    -- reset muh shit
    captures = {}
    capture_count = 0
end

local out = buffer.new(size)
out:put("#define R_PUSH(next)        ((1u<<29u) | (next))\n")
out:put("#define R_POP(next)         ((2u<<29u) | (next))\n")
out:put("#define R_CAPTURE(next, id) ((3u<<29u) | ((id)<<16u) | (next))\n")
out:put("#define R_PUSHCAP(next, id) ((4u<<29u) | ((id)<<16u) | (next))\n")
out:put("static uint32_t x86_grammar[][")
out:put(state_count)
out:put("] = {\n")
for id,v in pairs(dfa) do
    for k,v2 in pairs(v) do
        out:put("    [")
        out:put(id)
        out:put("][")
        out:put(k)
        out:put("] = ")
        out:put(v2)
        out:put(",\n")
    end
    out:put("\n")
end
out:put("};\n\n")
out:put("static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, TB_Node** captures, int state) {\n")
out:put("    switch (state) {\n")
for k,v in pairs(accept) do
    out:put("        case ")
    out:put(k)
    out:put(": {\n")
    for i,line in pairs(v) do
        out:put("            ")
        out:put(line)
        out:put("\n")
    end
    out:put("        } return NULL;\n")
end
out:put("        // no match?\n")
out:put("        default: return NULL;\n")
out:put("    }\n")
out:put("}\n")

print(out:tostring())

local f = io.open("tb/x64/x64_gen.h", "w")
f:write(out:tostring())
f:close()

