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

local is_operand = {}

local dfa = {}
local depth = 0
local state_count = 1

local node_type_count = 0
local node_types = {}

local any = {}
local push = {}
local pop = {}

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

local function dfa_insert(ty, old)
    if dfa[old] == nil then
        dfa[old] = {}
    end

    if dfa[old][ty] == nil then
        dfa[old][ty] = state_count
        state_count = state_count + 1
        return state_count - 1
    else
        return dfa[old][ty]
    end
end

local function insert_2d(t, i, j, v)
    if not t[i] then
        t[i] = {}
    end
    t[i][j] = v
end

local function dfa_compile(n, head, depth)
    local ty = n[1].."+1"
    if dfa[head] == nil then
        dfa[head] = {}
    end

    if n.name then
        if n[1] == "x86_MEMORY" then
            if mem_capture then
                print("Can't capture 2 memory operands in one pattern?!?")
                os.exit(1)
            end

            mem_capture = n
        elseif n.name and n[1] == "x86_COND" then
            if cond_capture then
                print("Can't capture 2 conditional operands in one pattern?!?")
                os.exit(1)
            end

            cond_capture = n
        end
    end

    if dfa[head][ty] ~= nil then
        head = dfa[head][ty]
    else
        insert_2d(push, head, ty, true)
        dfa[head][ty] = state_count
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
            if dfa[head] == nil then
                dfa[head] = {}
            end

            dfa[head][ty] = head
        elseif t == "___" then
            head = dfa_insert("TB_NULL+1", head)
        elseif t:byte(1) == string.byte("$") then -- capture
            local before = head
            head = dfa_insert(0, head)

            -- any paths in this camp which fail should
            -- return to "head"
            any[#any + 1] = { before, head }
        end
    end

    if dfa[head] == nil then
        dfa[head] = {}
    end

    if dfa[head]["TB_NULL+1"] then
        return dfa[head]["TB_NULL+1"]
    end

    -- pop once we've exhausted all the inputs
    insert_2d(pop, head, "TB_NULL+1", 1)
    if depth == 0 then
        dfa[head]["TB_NULL+1"] = head
        return head
    else
        dfa[head]["TB_NULL+1"] = state_count
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

node_extra_desc = {
    ["TB_NodeCompare"] = { cmp_dt="TB_DataType" },
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

    local extra_type = "X86MemOp"
    if  n[1] == "TB_CMP_ULE" or
        n[1] == "TB_CMP_ULT" or
        n[1] == "TB_CMP_SLE" or
        n[1] == "TB_CMP_SLT" or
        n[1] == "TB_CMP_FLE" or
        n[1] == "TB_CMP_FLT" or
        n[1] == "TB_CMP_EQ"  or
        n[1] == "TB_CMP_NE"  then
        extra_type = "TB_NodeCompare"
    end

    for k,v in pairs(n) do
        if type(v) == "string" and v:byte(1) == string.byte("$") then
            if type(k) == "string" and k ~= "name" then
                if node_basic_fields[k] then
                    strs[#strs + 1] = string.format("    TB_DataType %s = %s->%s;", v, expr, k)
                else
                    local t = node_extra_desc[extra_type]
                    if t then
                        t = t[k]
                    else
                        t = "int"
                    end

                    strs[#strs + 1] = string.format("    %s %s = TB_NODE_GET_EXTRA_T(%s, %s)->%s;", t, v, expr, extra_type, k)
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
    cond_capture = nil
    if t == "node" then
        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local desc = parse_node()

        -- define node types
        if not node_types["x86_"..desc[1]] then
            node_types["x86_"..desc[1]] = { id=node_type_count, name="x86_"..desc[1] }
            node_type_count = node_type_count + 1
        end
    elseif t == "pat" then
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

        if replacement[1] == "x86_MEMORY" or replacement[1] == "x86_COND" then
            is_operand[pattern[1]] = true
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

-- propagate any "fail" cases
local visited = {}
local function dfa_crawl(state, fail, depth)
    if visited[state] then
        return
    end
    visited[state] = true

    -- print("[", state, "]", dfa[state][0], fail, depth)

    -- if there's no "ANY" state, then we add one
    if not dfa[state][0] then
        -- we need to undo all the pushes we did to get here
        insert_2d(pop, state, 0, depth)
        dfa[state][0] = fail
    end

    for k,v in pairs(dfa[state]) do
        if state ~= v then
            if push[state] and push[state][k] then
                dfa_crawl(v, fail, depth + 1)
            elseif pop[state] and pop[state][k] then
                -- once depth drops back to 0 we've rejoined
                if depth > 1 then
                    dfa_crawl(v, fail, depth - 1)
                end
            else
                dfa_crawl(v, fail, depth)
            end
        end
    end
end

for i=1,#any do
    local state = any[i][1]
    local times = 0
    for k,v in pairs(dfa[state]) do
        if k ~= 0 then
            times = times + 1
        end
    end

    if times > 0 then
        local fail = any[i][2]
        visited[fail] = true

        for k,v in pairs(dfa[state]) do
            if k ~= 0 then
                local depth = 0
                if push[state][k] then
                    depth = depth + 1
                end

                dfa_crawl(v, fail, depth)
            end
        end
    end
end

if false then
    local graphviz = buffer.new(1000)
    graphviz:put("digraph G {")
    for state=0,#dfa do
        for id,next in pairs(dfa[state]) do
            if id == 0 then
                graphviz:put(string.format("v%d -> v%d [label=\"ANY\"]\n", state, next))
            else
                graphviz:put(string.format("v%d -> v%d [label=\"%s\"]\n", state, next, id:sub(1, #id-2)))
            end
        end
        graphviz:put("\n")
    end
    graphviz:put("}\n")

    local f2 = io.open("foo.dot", "w")
    f2:write(graphviz:tostring())
    f2:close()
else
    local count = 0
    local out = buffer.new(size)

    -- node_types[desc[1]] = { id=node_type_count, name=desc[2] }
    -- node_type_count = node_type_count + 1

    out:put("typedef enum X86NodeType {\n")
    for k,v in pairs(node_types) do
    x86_MEMORY = TB_MACH_X86,
        out:put(string.format("    %s = TB_MACH_X86 + %d,\n", k, v.id))
    end
    out:put("} X86NodeType;\n")

    out:put("static const char* node_name(int n_type) {\n")
    out:put("    switch (n_type) {\n")
    for k,v in pairs(node_types) do
        out:put("        case ")
        out:put(k)
        out:put(": return \"")
        out:put(v.name)
        out:put("\";\n")
    end
    out:put("        default: return NULL;\n")
    out:put("    }\n")
    out:put("}\n\n")
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
    out:put("}\n\n")
    out:put("static bool x86_is_operand[512] = {\n")
    for k,v in pairs(is_operand) do
        out:put("    [")
        out:put(k)
        out:put("] = true,\n")
    end
    out:put("};\n")
    out:put("#define R_PUSH(next)        ((1u  << 16u) | (next))\n")
    out:put("#define R_POP(n, next)      (((n) << 16u) | (next))\n")
    out:put("static void global_init(void) {\n")
    out:put("    static const uint32_t edges[] = {\n")
    for state=0,#dfa do
        for id,next in pairs(dfa[state]) do
            out:put("        (")
            out:put(state)
            out:put(")<<16 | (")
            out:put(id)
            out:put("), ")
            if push[state] and push[state][id] then
                out:put(string.format("R_PUSH(%d)", dfa[state][id]))
            elseif pop[state] and pop[state][id] then
                out:put(string.format("R_POP(%d, %d)", 1+pop[state][id], dfa[state][id]))
            else
                out:put(next)
            end
            out:put(",\n")
            count = count + 1
        end
    end
    out:put("    };\n")
    out:put("    // transitions: ")
    out:put(count)
    out:put("\n")
    out:put("    size_t count = sizeof(edges) / (2*sizeof(uint32_t));\n")
    out:put("    node_grammar_alloc(count);\n")
    out:put("    FOR_N(i, 0, count) {\n")
    out:put("        node_grammar_put(edges[i*2], edges[i*2 + 1]);\n")
    out:put("    }\n")
    out:put("}\n")

    -- print(out:tostring())
    print("Transitions:", count)

    local f = io.open("tb/x64/x64_gen.h", "w")
    f:write(out:tostring())
    f:close()
end

