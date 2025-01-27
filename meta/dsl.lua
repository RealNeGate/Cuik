local buffer  = require "string.buffer"
local inspect = require "meta/inspect"

function string:starts_with(start)
    return self:sub(1, #start) == start
end

function run_command(cmd)
    local f = assert(io.popen(cmd))
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
        ::loop::
        if ch_class[str:byte(i)] == "ws" then
            i = i + 1
            goto loop
        elseif str:byte(i) == 35 then -- hash are comments
            while str:byte(i) ~= 10 do
                i = i + 1
            end
            goto loop
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
            return str:sub(start, i - 1)
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

local source = run_command("clang -E -xc tb/x64/x64.machine")
print(source)

local can_memory = {}

local dfa = {}
local depth = 0
local state_count = 1

local accept = {}
local lex = lexer(source)

local function parse_node()
    local n = {}
    local t = lex()

    if t:byte(1) == string.byte("$") then
        n["name"] = t

        t = lex()
        if t ~= ":" then
            print("fuck but in colon")
            os.exit(1)
        end

        t = lex()
    end

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

local function dfa_get(ty, state)
    local s = dfa[ty][state]
    if type(s) == "string" then
        if s:starts_with("R_POP(") then
            s = s:sub(7, #s - 1)
        elseif s:starts_with("R_PUSH(") then
            s = s:sub(8, #s - 1)
        end

        local as_num = tonumber(s)
        if as_num then
            return as_num
        end
    end

    return s
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
        return dfa_get(ty, old)
    end
end

local function dfa_compile(n, head, depth)
    local ty = n[1].."+1"
    if dfa[ty] == nil then
        dfa[ty] = {}
    end

    if n.name and n[1] == "x86_MEMORY" then
        if mem_capture then
            print("Can't capture 2 memory operands in one pattern?!?")
            os.exit(1)
        end

        mem_capture = n
    end

    if dfa[ty][head] ~= nil then
        head = dfa_get(ty, head)
    else
        dfa[ty][head] = "R_PUSH("..state_count..")"
        head = state_count
        state_count = state_count + 1
    end

    for i=2,#n do
        local t = n[i]
        if type(t) == "table" then
            head = dfa_compile(t, head, depth + 1)
        elseif t == "..." then
            -- keep chewing up any nodes in this guy
            local ty = 0
            if dfa[ty] == nil then
                dfa[ty] = {}
            end

            dfa[ty][head] = head
        elseif t == "___" then
            head = dfa_insert("TB_NULL+1", head)
        elseif t:byte(1) == string.byte("$") then -- capture
            head = dfa_insert(0, head)
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
        elseif mem_capture and v == mem_capture.name then
            in_cnt = in_cnt + 1
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
    if dt_name == nil then
        dt_name = "TB_TYPE_VOID"
    end

    local potentially_dynamic_count = false

    strs[#strs + 1] = string.format("    size_t k%d_i = 0;", ids[n])
    strs[#strs + 1] = string.format("    TB_Node* k%d = tb_alloc_node(f, %s, %s, %d, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type)

    -- input edges
    for i=2,#n do
        local v = n[i]
        if v == "___" then
            -- just keep as NULL
            strs[#strs + 1] = string.format("    k%d_i++;", ids[n])
        elseif type(v) == "table" then
            strs[#strs + 1] = string.format("    set_input(f, k%d, k%d, k%d_i++);", ids[n], ids[v], ids[n])
        elseif mem_capture and v == mem_capture.name then
            -- copy all the extra data
            potentially_dynamic_count = true
            -- copy inputs
            strs[#strs + 1] = string.format("    FOR_N(i, 0, %s->input_count) { set_input(f, k%d, %s->inputs[i], k%d_i++); }", v, ids[n], v, ids[n])
        elseif type(v) == "string" and v:byte(1) == string.byte("$") then
            strs[#strs + 1] = string.format("    set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
        end
    end

    strs[#strs + 1] = string.format("    %s* k%d_extra = TB_NODE_GET_EXTRA(k%d);", extra_type, ids[n], ids[n])
    for k,v in pairs(n) do
        if type(k) == "string" and k ~= "dt" then
            -- replace all uses of captures with their real name
            strs[#strs + 1] = string.format("    k%d_extra->%s = %s;", ids[n], k, v)
        elseif mem_capture and v == mem_capture.name then
            strs[#strs + 1] = string.format("    memcpy(k%d_extra, %s->extra, sizeof(X86MemOp));", ids[n], v)
        end
    end

    -- trim size
    if potentially_dynamic_count then
        strs[#strs + 1] = string.format("    k%d->input_count = k%d_i;", ids[n], ids[n])
    end

    strs[#strs + 1] = ""
end

node_basic_fields = {
  ["dt"] = true,
}

function find_captures(strs, n, expr)
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            find_captures(strs, v, expr.."->inputs["..(i-2).."]")
        end
    end

    if n.name then
        strs[#strs + 1] = string.format("    TB_Node* %s = %s;", n.name, expr)
    end

    for k,v in pairs(n) do
        if type(v) == "string" and v:byte(1) == string.byte("$") then
            if type(k) == "string" and k ~= "name" then
                if node_basic_fields[k] then
                    strs[#strs + 1] = string.format("    TB_DataType %s = %s->%s;", v, expr, k)
                else
                    strs[#strs + 1] = string.format("    int %s = TB_NODE_GET_EXTRA_T(%s, X86MemOp)->%s;", v, expr, k)
                end
            elseif type(k) == "number" then
                strs[#strs + 1] = string.format("    TB_Node* %s = %d < %s->input_count ? %s->inputs[%d] : NULL;", v, k-2, expr, expr, k-2)
            end
        end
    end
end

while true do
    local t = lex()
    if t == nil then
        break
    end

    mem_capture = nil
    if t == "pat" then
        t = lex()
        if t ~= "(" then
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
        end

        if t ~= "=>" then
            print("fuck but in parsing 2")
            os.exit(1)
        end

        local replacement = nil
        t = lex()
        if t == "(" then
            replacement = parse_node()
        end

        -- print(inspect(pattern))

        local strs = accept[head]
        if not strs then
            strs = {}
            accept[head] = strs
        end

        if replacement[1] == "x86_MEMORY" then
            can_memory[pattern[1]] = true
        end

        local ids = {}
        node_cnt = 0

        strs[#strs + 1] = "do {"
        find_captures(strs, pattern, "n")
        if where then
            strs[#strs + 1] = "    if (!("..where..")) {"
            strs[#strs + 1] = "        break;"
            strs[#strs + 1] = "    }"
        end
        strs[#strs + 1] = ""
        write_node(strs, ids, replacement)
        strs[#strs + 1] = string.format("    return k%d;", ids[replacement])
        strs[#strs + 1] = "} while (0);"

        -- reset muh shit
        captures = {}
        capture_count = 0
    end
end

local out = buffer.new(size)
out:put("#define R_PUSH(next)        ((1u<<30u) | (next))\n")
out:put("#define R_POP(next)         ((2u<<30u) | (next))\n")
out:put("static bool x86_can_memory[512] = {\n")
for k,v in pairs(can_memory) do
    out:put("    [")
    out:put(k)
    out:put("] = true,\n")
end
out:put("};\n")
out:put("static uint32_t x86_grammar[")
out:put(state_count)
out:put("][512] = {\n")
for id,v in pairs(dfa) do
    for k,v2 in pairs(v) do
        out:put("    [")
        out:put(k)
        out:put("][")
        out:put(id)
        out:put("] = ")
        out:put(v2)
        out:put(",\n")
    end
    out:put("\n")
end
out:put("};\n\n")
out:put("static TB_Node* x86_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {\n")
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

-- print(out:tostring())

local f = io.open("tb/x64/x64_gen.h", "w")
f:write(out:tostring())
f:close()

