local buffer  = require "string.buffer"
local inspect = require "meta/lib/inspect"

local mach_prefix = arg[1].."_"

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

local line_num = 1

local function either(a, b, c) return a == b or a == c end
local function lexer(str)
    local i = 1
    return function()
        -- skip whitespace
        while ch_class[str:byte(i)] == "ws" or str:byte(i) == 35 do
            if str:byte(i) == 10 then
                i = i + 1
                line_num = line_num + 1
            elseif ch_class[str:byte(i)] == "ws" then
                i = i + 1
            elseif str:byte(i) == 35 then -- hash are comments
                while str:byte(i) ~= 10 do
                    i = i + 1
                end
            end
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

print(arg[2])
local source = run_command("clang -E -xc "..arg[2])

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

local function dfa_compile(n, head, depth, stack)
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
    stack[#stack + 1] = head

    for i=2,#n do
        local t = n[i]
        if type(t) == "table" then
            head = dfa_compile(t, head, depth + 1, stack)
            stack[#stack + 1] = head
        elseif t == "..." or t == "$REST" then
            -- keep chewing up any nodes in this guy
            local ty = 0
            if dfa[head] == nil then
                dfa[head] = {}
            end

            dfa[head][ty] = head
        elseif t == "___" then
            head = dfa_insert("TB_NULL+1", head)
            stack[#stack + 1] = head
        elseif t:byte(1) == string.byte("$") then -- capture
            local before = head
            head = dfa_insert(0, head)
            stack[#stack + 1] = head

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

node_extra_desc = {
    ["TB_NodeCompare"]  = { cmp_dt="TB_DataType" },
    ["TB_NodeSymbol"]   = { sym="TB_Symbol*" },
    ["TB_NodeLocal"]    = { stack_pos="int" },
    ["TB_NodeMachCopy"] = { def="RegMask*", use="RegMask*" },
    ["TB_NodeMachProj"] = { index="int", def="RegMask*" },
    ["TB_NodeProj"]     = { index="int" },
    ["TB_NodeFloat32"]  = { value="float" },
    ["TB_NodeFloat64"]  = { value="double" },
    ["TB_NodeInt"]      = { value="uint64_t" },
}

function extra_node_type_name(t)
    local extra_type = mach_extra_type
    if  t == "TB_CMP_ULE" or
        t == "TB_CMP_ULT" or
        t == "TB_CMP_SLE" or
        t == "TB_CMP_SLT" or
        t == "TB_CMP_FLE" or
        t == "TB_CMP_FLT" or
        t == "TB_CMP_EQ"  or
        t == "TB_CMP_NE"  then
        extra_type = "TB_NodeCompare"
    elseif t == "TB_SYMBOL" then
        extra_type = "TB_NodeSymbol"
    elseif t == "TB_LOCAL" then
        extra_type = "TB_NodeLocal"
    elseif t == "TB_MACH_SYMBOL" then
        extra_type = "TB_NodeMachSymbol"
    elseif t == "TB_MACH_COPY" then
        extra_type = "TB_NodeMachCopy"
    elseif t == "TB_MACH_PROJ" then
        extra_type = "TB_NodeMachProj"
    elseif t == "TB_PROJ" then
        extra_type = "TB_NodeProj"
    elseif t == "TB_ICONST" then
        extra_type = "TB_NodeInt"
    elseif t == "TB_F32CONST" then
        extra_type = "TB_NodeFloat32"
    elseif t == "TB_F64CONST" then
        extra_type = "TB_NodeFloat64"
    end
    return extra_type
end

local node_cnt = 0
function write_node(strs, ids, n)
    -- create kids first
    local in_cnt = 0
    local uses_rest = false
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            write_node(strs, ids, v)
            in_cnt = in_cnt + 1
        elseif v == "$REST" then
            uses_rest = true
        elseif mem_capture and v == mem_capture.name then
            in_cnt = in_cnt + 2
        else
            in_cnt = in_cnt + 1
        end
    end

    -- assign unique IDs
    ids[n] = node_cnt
    node_cnt = node_cnt + 1

    -- declaration of a node
    local node_type = n[1]
    local extra_type = extra_node_type_name(n[1])

    local dt_name = n.dt
    if dt_name == nil then
        dt_name = "TB_TYPE_VOID"
    end

    local potentially_dynamic_count = false

    strs[#strs + 1] = string.format("    size_t k%d_i = 0;", ids[n])
    if uses_rest then
        strs[#strs + 1] = string.format("    TB_Node* k%d = tb_alloc_node(f, %s, %s, %d + $REST_LEN, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type)
    else
        strs[#strs + 1] = string.format("    TB_Node* k%d = tb_alloc_node(f, %s, %s, %d, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type)
    end

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
            strs[#strs + 1] = string.format("    if (%s->type != %s) {", v, mach_prefix.."MEMORY")
            strs[#strs + 1] = string.format("        set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
            strs[#strs + 1] = string.format("    } else {")
            strs[#strs + 1] = string.format("        FOR_N(i, 0, %s->input_count) { set_input(f, k%d, %s->inputs[i], k%d_i++); }", v, ids[n], v, ids[n])
            strs[#strs + 1] = string.format("    }")
        elseif v == "$REST" then
            strs[#strs + 1] = string.format("    FOR_N(i, 0, $REST_LEN) { set_input(f, k%d, $REST[i], k%d_i++); }", ids[n], ids[n])
        elseif type(v) == "string" and v:byte(1) == string.byte("$") then
            strs[#strs + 1] = string.format("    set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
        else
            strs[#strs + 1] = string.format("    set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
        end
    end

    strs[#strs + 1] = string.format("    %s* k%d_extra = TB_NODE_GET_EXTRA(k%d);", extra_type, ids[n], ids[n])
    for k,v in pairs(n) do
        if type(k) == "string" and k ~= "dt" then
            -- replace all uses of captures with their real name
            strs[#strs + 1] = string.format("    k%d_extra->%s = %s;", ids[n], k, v)
        elseif mem_capture and v == mem_capture.name then
            strs[#strs + 1] = string.format("    memcpy(k%d_extra, %s->extra, sizeof("..mach_extra_type.."));", ids[n], v)
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

    local extra_type = extra_node_type_name(n[1])
    for k,v in pairs(n) do
        if v == "$REST" then
            strs[#strs + 1] = string.format("    size_t %s_LEN = %s->input_count - %d;", v, expr, k-2)
            strs[#strs + 1] = string.format("    TB_Node** %s  = &%s->inputs[%d];", v, expr, k-2)
        elseif type(v) == "string" and v:byte(1) == string.byte("$") then
            if type(k) == "string" and k ~= "name" then
                if node_basic_fields[k] then
                    strs[#strs + 1] = string.format("    TB_DataType %s = %s->%s;", v, expr, k)
                else
                    local t = node_extra_desc[extra_type]
                    if t then
                        t = t[k]
                    else
                        t = "uint64_t"
                    end

                    strs[#strs + 1] = string.format("    %s %s = TB_NODE_GET_EXTRA_T(%s, %s)->%s;", t, v, expr, extra_type, k)
                end
            elseif type(k) == "number" then
                strs[#strs + 1] = string.format("    TB_Node* %s = %d < %s->input_count ? %s->inputs[%d] : NULL;", v, k-2, expr, expr, k-2)
            end
        end
    end
end

local fallback = {}
local subpat_map = {}
while true do
    local t = lex()
    if t == nil then
        break
    end

    mem_capture = nil
    local start_line = line_num
    if t == "node" or t == "extra" then
        local first = t

        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local desc = parse_node()

        if first == "extra" then
            mach_extra_type = desc[1]
        else
            -- define node types
            if not node_types[mach_prefix..desc[1]] then
                node_types[mach_prefix..desc[1]] = { id=node_type_count, name=mach_prefix..desc[1] }
                node_type_count = node_type_count + 1
            end
        end
    elseif t == "pat" or t == "subpat" then
        local is_subpat = t == "subpat"

        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local pattern = parse_node()

        local stack = {}
        local head = dfa_compile(pattern, 0, 0, stack)

        t = lex()
        local where = nil
        if t == "where" then
           fallback[head] = stack
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

            local strs = accept[head]
            if not strs then
                strs = {}
                accept[head] = strs
            end

            if replacement[1] == mach_prefix.."MEMORY" or replacement[1] == mach_prefix.."COND" then
                is_operand[pattern[1]] = true
                if is_subpat then
                    subpat_map[pattern[1]] = true
                end
            end

            local ids = {}
            node_cnt = 0

            strs[#strs + 1] = "do { // line: "..start_line
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
        else
            -- print(inspect(pattern))
            local strs = accept[head]
            if not strs then
                strs = {}
                accept[head] = strs
            end

            strs[#strs + 1] = "{"
            strs[#strs + 1] = string.format("    TB_Node* k = %s;", t)
            strs[#strs + 1] = "    if (k != NULL) { return k; }"
            strs[#strs + 1] = "}"
        end

        -- reset muh shit
        captures = {}
        capture_count = 0
    end
end

local special_return_cases = {}
for k,v in pairs(fallback) do
    -- print("V", v, v[#v], inspect(v))

    local final_state = v[#v]
    local i = #v
    while i > 1 do
        i = i - 1
        local any_case = dfa[v[i]][0]
        if accept[any_case] then
            special_return_cases[final_state] = any_case
            -- print("Final", any_case)
        end
    end
end
-- print(inspect(fallback))

-- propagate any "fail" cases
local visited = {}
local function dfa_crawl(state, fail, depth)
    if visited[state] then
        return
    end
    visited[state] = true

    -- if there's no "ANY" state, then we add one
    -- if (dfa[state][0] == state and depth > 1) or not dfa[state][0] then
    if not dfa[state][0] then
        -- print("[", state, "]", dfa[state][0], fail, depth)

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
                elseif depth == 1 then
                    dfa_crawl(v, dfa[fail][0], depth - 1)
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
                if push[state] and push[state][k] then
                    depth = depth + 1
                end

                dfa_crawl(v, fail, depth)
            end
        end
    end
end

function sort(set, cmp_fn)
    local a = {}
    for k, v in pairs(set) do
        table.insert(a, k)
    end

    table.sort(a, function(a, b)
        return cmp_fn(set, a, b)
    end)
    return a
end

if false then
    local graphviz = buffer.new(1000)
    graphviz:put("digraph G {\n")
    for k,v in pairs(accept) do
        graphviz:put(string.format("v%d [label=\"%s\"]\n", v, k))
    end
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

    local mach_prefix_caps = string.upper(arg[1])
    out:put("typedef enum "..mach_prefix_caps.."NodeType {\n")

    local function cmp_node_types(table, a, b)
        return table[a].id < table[b].id
    end

    local sorted_types = sort(node_types, cmp_node_types)
    for i,k in ipairs(sorted_types) do
        out:put(string.format("    %s = TB_MACH_"..mach_prefix_caps.." + %d,\n", k, node_types[k].id))
    end
    out:put("} "..mach_prefix_caps.."NodeType;\n")

    out:put("static const char* node_name(int n_type) {\n")
    out:put("    switch (n_type) {\n")
    for i,k in ipairs(sorted_types) do
        out:put("        case ")
        out:put(k)
        out:put(": return \"")
        out:put(node_types[k].name)
        out:put("\";\n")
    end
    out:put("        default: return NULL;\n")
    out:put("    }\n")
    out:put("}\n\n")
    out:put("static TB_Node* mach_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {\n")
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
        if k ~= special_return_cases[k] and special_return_cases[k] then
            out:put("        } return mach_dfa_accept(ctx, f, n, ")
            out:put(special_return_cases[k])
            out:put(");\n")
        else
            out:put("        } return NULL;\n")
        end
    end
    out:put("        // no match?\n")
    out:put("        default: return NULL;\n")
    out:put("    }\n")
    out:put("}\n\n")
    out:put("static bool mach_is_operand[512] = {\n")
    for k,v in pairs(is_operand) do
        out:put("    [")
        out:put(k)
        out:put("] = true,\n")
    end
    out:put("};\n")
    out:put("static bool mach_is_subpat[512] = {\n")
    for k,v in pairs(subpat_map) do
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
    -- print("Transitions:", count)

    local f = io.open(arg[3], "w")
    f:write(out:tostring())
    f:close()
end

