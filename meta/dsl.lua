local buffer  = require "string.buffer"
local inspect = require "meta/inspect"

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

function shallowcopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in pairs(orig) do
            copy[orig_key] = orig_value
        end
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
end

local node_enum_types = {}
do
    function magiclines(s)
        if s:sub(-1)~="\n" then s=s.."\n" end
        return s:gmatch("(.-)\n")
    end

    -- Read the enum table from the tb.h
    local f = io.open("include/tb.h", "r")
    local str = f:read("*all")
    f:close()

    local s = str:find("typedef enum TB_NodeTypeEnum")
    local e = str:find("} TB_NodeTypeEnum;")
    str = str:sub(s, e)

    local first = true
    local idx = 0
    for l in magiclines(str) do
        if first then
            first = false
        else
            -- Ignore after comments
            local comment = l:find("//")
            if comment then
                l = l:sub(0, comment-1)
            end

            -- Look for identifier
            local ident = l:gmatch("[A-Za-z_][A-Za-z0-9_]+")()
            if ident then
                node_enum_types[ident] = idx
                idx = idx + 1
            end
        end
    end
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

local function insert_2d(t, i, j, v)
    if not t[i] then
        t[i] = {}
    end
    t[i][j] = v
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

    -- apply GVN
    strs[#strs + 1] = string.format("    k%d = tb_opt_gvn_node(f, k%d);", ids[n], ids[n])
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

local all_patterns = {}
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
        all_patterns[#all_patterns + 1] = { is_subpat, pattern, where, replacement }

        -- reset muh shit
        captures = {}
        capture_count = 0
    end
end

--------------------------
-- DFA construction
--------------------------
function get_pattern_shape(n)
    local str = "("
    if n[1] == "x86_MEMORY" then
        str = str.."MEM_OP"
    elseif n[1] == "x86_COND" then
        str = str.."CC_OP"
    else
        str = str.."ANY_OP"
    end

    for i=2,#n do
        local t = n[i]
        str = str.." "
        if type(t) == "table" then
            str = str..get_pattern_shape(t)
        elseif t == "..." or t == "$REST" then
            str = str.."..."
        elseif t == "___" then
            str = str.."___"
        elseif t:byte(1) == string.byte("$") then -- capture
            str = str.."ANY"
        else
            error("Hehe", t)
        end
    end

    return str..")"
end

NFA = {}

local function nfa_new_edge(from, when)
    if not NFA[from] then
        NFA[from] = {}
    elseif NFA[from][when] then
        return NFA[from][when]
    end

    local to = state_count
    state_count = state_count + 1
    NFA[from][when] = to
    return to
end

local function nfa_edge(from, when, to)
    if not NFA[from] then
        NFA[from] = {}
    end

    assert(not NFA[from][when])
    NFA[from][when] = to
    return to
end

local function active_str(set)
    local strs = {}
    for i=1,#set do
        strs[#strs + 1] = set[i]
    end
    return table.concat(strs, ",")
end

local function add_if_new(set, v)
    for i=1,#set do
        if set[i] == v then
            return i
        end
    end

    set[#set + 1] = v
    return #set
end

local function make_active_str(list)
    local strs = {}
    for i=1,partition_count do
        strs[i] = 0
    end

    for i=1,#list do
        local pat = list[i]
        strs[to_partition[pat]] = 1
    end

    return table.concat(strs)
end

NFA_cache = {}
local function nfa_compile(n, head, active, depth)
    local ty = n[1]

    -- what partitions are we "in" right now
    local next_active = {}
    for i=1,#active do
        if active[i][1] == ty then
            next_active[#next_active + 1] = active[i]
        end
    end

    local active_str = make_active_str(next_active)
    -- print("Active", inspect(n), active_str)

    -- if there's already an edge which matches
    -- the active set, we use that
    if not NFA_cache[head] then
        NFA_cache[head] = {}
    end

    if NFA_cache[head][active_str] then
        -- print("Cached", NFA[head][ty], head, ty, NFA_cache[head][active_str])

        -- reuse head
        insert_2d(push, head, ty, true)
        NFA[head][ty] = NFA_cache[head][active_str]
        head = NFA_cache[head][active_str]
    else
        local old_head = head
        insert_2d(push, head, ty, true)
        head = nfa_new_edge(head, ty)

        NFA_cache[old_head][active_str] = head
    end

    for i=2,#n do
        local t = n[i]
        if type(t) == "table" then
            local kid_active = {}
            for j=1,#next_active do
                kid_active[#kid_active + 1] = next_active[j][i]
            end

            head = nfa_compile(t, head, kid_active, depth + 1)
        elseif t == "..." or t == "$REST" then
            -- match any, but in a cycle
            if not NFA[head] then
                NFA[head] = {}
            end
            NFA[head][0] = head
        elseif t == "___" then
            head = nfa_new_edge(head, "TB_NULL")
        elseif t:byte(1) == string.byte("$") then -- capture
            head = nfa_new_edge(head, 0)
        else
            error("Hehe", t)
        end
    end

    -- pop once we've exhausted all the inputs
    insert_2d(pop, head, "TB_NULL", 1)
    if depth == 0 then
        if not NFA[head] then
            NFA[head] = {}
        end
        NFA[head]["TB_NULL"] = head
    else
        head = nfa_new_edge(head, "TB_NULL")
    end
    return head
end

-- Split partitions by similar pattern shapes
local partitions = {}
local active = {}
for i,pat in ipairs(all_patterns) do
    local k = get_pattern_shape(pat[2])
    if not partitions[k] then
        partitions[k] = {}
    end

    partitions[k][#partitions[k] + 1] = pat
    active[#active + 1] = pat[2]
end

to_partition = {}
partition_count = 0

function walk_into(n, idx)
    for i=2,#n do
        if type(n[i]) == "table" then
            walk_into(n[i], idx)
        end
    end

    to_partition[n] = idx
end

for name,set in pairs(partitions) do
    partition_count = partition_count + 1
    for i=1,#set do
        walk_into(set[i][2], partition_count)
    end
    print(name, partition_count, #set)
end

for name,set in pairs(partitions) do
    local head = nfa_compile(set[1][2], 0, active, 0)
    accept[head] = true
    -- print("FINAL", head)

    for i=2,#set do
        head = nfa_compile(set[i][2], 0, active, 0)
        accept[head] = true
        -- print("FINAL", head)
    end
end

function print_row(head, n)
    print("State", head)
    for k,v in pairs(n) do
        local str = string.format("  %-15s => %d", k, v)
        if push[head] and push[head][k] then
            str = str.." (PUSH)"
        elseif pop[head] and pop[head][k] then
            str = str..string.format(" (POP%d)", pop[head][k])
        end
        print(str)
    end

    if accept[head] then
        print("  FINAL")
    end
    print()
end

--[[print("Dump")
for k,v in pairs(NFA) do
    print_row(k, v)
end]]--

-- Guarantee a list, even if empty
for k,v in pairs(accept) do
    if not NFA[k] then
        NFA[k] = {}
    end
end

visited = {}
function insert_backtrack(head, fail, depth)
    if visited[head] then
        return
    end
    visited[head] = true

    local n = NFA[head]

    -- "forget" any fail cases which are deeper than us
    while fail and fail.depth > depth do
        fail = fail.prev
    end

    -- print("Walk", head, fail and fail.node or nil, depth, fail and fail.depth or nil)

    -- step the fail case forward if we're at the same depth
    local old_fail = fail
    if fail and fail.depth == depth then
        fail = shallowcopy(fail)
        fail.node = NFA[fail.node][0]

        if fail.node == head or not fail.node then
            fail = fail.prev
        end
    end

    -- if later states need to backtrack here, we'll
    -- mark ourselves as a fail state.
    for k,v in pairs(n) do
        if k ~= 0 and n[0] then
            -- print("push fail", head, n[0])
            fail = { prev=fail, node=n[0], depth=depth }
            break
        end
    end

    for k,v in pairs(n) do
        if NFA[v] and v ~= head then
            local next_depth = depth
            if push[head] and push[head][k] then
                next_depth = next_depth + 1
            elseif pop[head] and pop[head][k] then
                next_depth = next_depth - pop[head][k]
            end

            insert_backtrack(v, fail, next_depth)
        end
    end

    if old_fail and not n[0] and head ~= old_fail.node then
        -- print("insert fail", head, old_fail.node, depth, old_fail.depth)

        -- step out of the current node
        if depth ~= old_fail.depth then
            insert_2d(pop, head, 0, depth - old_fail.depth)
        end
        n[0] = old_fail.node
    end
end

insert_backtrack(0, nil, 0)
pred_count = {}

print("Dump")
for k,v in pairs(NFA) do
    print_row(k, v)
    for input,succ in pairs(v) do
        if NFA[succ] ~= v then
            if not pred_count[succ] then
                pred_count[succ] = 1
            else
                pred_count[succ] = pred_count[succ] + 1
            end
        end
    end
end

print(inspect(pred_count))
print("DONE")

function find_leader(n)
    local leader = nil
    local leader_k = nil
    for k,v in pairs(n) do
        if v ~= head then
            if not leader then
                leader = v
                leader_k = k
            elseif leader ~= v then
                return nil
            end
        end
    end
    return leader_k
end

-- Generate C code for the matcher
local lines = {}
visited = {}
function add_line(depth, line)
    lines[#lines + 1] = string.rep(" ", depth*4) .. line
end

function compare_to_node_type(in_str, t, inv)
    if t == "TB_NULL" then
        if inv then
            return in_str.." == NULL"
        else
            return in_str.." != NULL"
        end
    else
        if inv then
            return in_str.."->type != "..t
        else
            return in_str.."->type == "..t
        end
    end
end

if false then
    for k,v in pairs(accept) do
        add_line(0, string.format("static TB_Node* mach_dfa_accept_%d(Ctx* ctx, TB_Function* f, TB_Node* n) {", k))
        for i,line in pairs(v) do
            add_line(1, line)
        end
        add_line(1, "return NULL;")
        add_line(0, "}")
        lines[#lines+1] = ""
    end
end

function is_push(n, input)
    return push[n] and push[n][input]
end

function is_pop(n, input)
    if pop[n] then
        return pop[n][input]
    else
        return 0
    end
end

-- finds shapes for: (NODE_TYPE ...)
function noop_push_pop(head, input)
    if not is_push(head, input) then
        return nil
    end

    local next = NFA[head][input]

    local case_count = 0
    local non_any = nil
    for k,v in pairs(NFA[next]) do
        if k ~= 0 then
            non_any = k
        end
        case_count = case_count + 1
    end

    if case_count == 2 and NFA[next][0] == next and non_any == "TB_NULL" and is_pop(next, non_any) == 1 then
        return NFA[next][non_any]
    else
        return nil
    end
end

var_counter = 0
function gen_c(head, input, depth, stack)
    local in_str = stack[2]
    if stack[3] then
        in_str = in_str.."->inputs["..stack[3].."]"
    end

    local n = NFA[head]
    if input ~= nil then
        if push[head] and push[head][input] then
            local new_name = in_str
            if stack[3] then
                new_name = string.format("n$%d", var_counter)
                add_line(depth, string.format("%s = %s;", new_name, in_str))
                var_counter = var_counter + 1

                in_str = new_name
            else
                in_str = new_name.."->inputs[0]"
            end

            stack = { stack, new_name, 0 }
        elseif pop[head] and pop[head][input] then
            stack = stack[1]
            -- add_line(depth, string.format("pop(%d);", pop[head][input]))
        end

        head = n[input]
        n = NFA[head]
    end

    if visited[head] then
        add_line(depth, string.format("goto STATE%d;", head))
        return
    end
    visited[head] = true
    if stack[3] then
        stack[3] = stack[3] + 1
    end

    -- add_line(depth, string.format("// STATE%d", head))
    if pred_count[head] and pred_count[head] > 1 then
        add_line(depth, string.format("STATE%d:", head))
    end

    -- Accept states
    if accept[head] then
        if n[0] then
            add_line(depth, string.format("k = mach_dfa_accept_%d(ctx, f, n);", head))
            add_line(depth, string.format("if (k) { return k; } else { goto STATE%d; }", n[0]))
        else
            add_line(depth, string.format("return mach_dfa_accept_%d(ctx, f, n);", head))
        end
        return
    end

    print("Gen", head)

    local cyclic = n[0] == head
    local leader = find_leader(n)
    if leader then
        if leader ~= 0 then
            add_line(depth, string.format("if (%s) { return NULL; }", compare_to_node_type(in_str, leader, true)))
            lines[#lines+1] = ""
        end
        gen_c(head, leader, depth, stack)
    else
        local case_count = 0
        local non_any = nil
        for k,v in pairs(n) do
            if k ~= 0 then
                non_any = k
            end
            case_count = case_count + 1
        end

        if case_count == 2 and n[0] then
            local old_vis = visited[n[0]]
            if not old_vis then
                -- if the default case is unvisited, fake a visit to cause a GOTO placement in the nested block
                visited[n[0]] = true

                add_line(depth, string.format("if (%s) {", compare_to_node_type(in_str, non_any, false)))
                gen_c(head, non_any, depth+1, stack)
                add_line(depth, "}")

                visited[n[0]] = nil
                gen_c(head, 0, depth, stack)
            elseif n[0] == head then
                -- if the only non-any case is just a pop on TB_NULL, let's early out
                if non_any ~= "TB_NULL" then
                    add_line(depth, string.format("do {"))
                    add_line(depth, string.format("    // TODO(NeGate): scan until X type"))
                    add_line(depth, string.format("} while (%s);", compare_to_node_type(in_str, non_any, true)))
                end
                gen_c(head, non_any, depth, stack)
            else
                add_line(depth, string.format("if (%s) { goto STATE%d; }", compare_to_node_type(in_str, non_any, true), n[0]))

                local skip_to = noop_push_pop(head, non_any)
                if skip_to then
                    gen_c(skip_to, nil, depth, stack)
                else
                    gen_c(head, non_any, depth, stack)
                end
            end
        else
            add_line(depth, string.format("switch (%s ? %s->type : TB_NULL) {", in_str, in_str))

            local unique_paths = {}
            for k,v in pairs(n) do
                local list = unique_paths[v]
                if not list then
                    unique_paths[v] = {k}
                else
                    list[#list + 1] = k
                end
            end

            for succ,list in pairs(unique_paths) do
                for i=1,#list do
                    if list[i] == 0 then
                        add_line(depth, string.format("    default:"))
                    else
                        add_line(depth, string.format("    case %s:", list[i], succ))
                    end
                end
                add_line(depth, "    {")
                gen_c(head, list[1], depth+2, stack)
                add_line(depth, "    }")
            end
            add_line(depth, "}")
        end
    end
end

local stack = { nil, "n", nil }
gen_c(0, nil, 0, stack)

print(table.concat(lines, "\n"))
os.exit(0)

local special_return_cases = {}
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

if true then
    local graphviz = buffer.new(1000)
    graphviz:put("digraph G {\n")
    for k,v in pairs(accept) do
        graphviz:put(string.format("v%d [label=\"ACCEPT%d\"]\n", k, k))
    end
    for state,n in pairs(NFA) do
        for id,next in pairs(n) do
            local str = id
            if id == 0 then
                str = "ANY"
            end

            if push[state] and push[state][id] then
                str = str.." (PUSH)"
            elseif pop[state] and pop[state][id] then
                str = str..string.format(" (POP%d)", pop[state][id])
            end

            graphviz:put(string.format("v%d -> v%d [label=\"%s\"]\n", state, next, str))
        end
        graphviz:put("\n")
    end
    graphviz:put("}\n")

    print(graphviz:tostring())
    -- local f2 = io.open("foo.dot", "w")
    -- f2:write(graphviz:tostring())
    -- f2:close()
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
    out:put([[
static TB_Node* mach_dfa_bare_memory(Ctx* ctx, TB_Function* f, TB_Node* n) {
    TB_Node* new_n = tb_alloc_node(f, x86_MEMORY, TB_TYPE_VOID, 1, sizeof(X86MemOp));
    set_input(f, new_n, n, 0);
    return tb_opt_gvn_node(f, new_n);
}

]])
    out:put("static TB_Node* mach_dfa_accept(Ctx* ctx, TB_Function* f, TB_Node* n, int state) {\n")
    out:put("    switch (state) {\n")
    --[[for k,v in pairs(accept) do
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
    end]]--
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
    for state=1,state_count do
        for id,next in pairs(NFA[state]) do
            out:put("        (")
            out:put(state)
            out:put(")<<16 | (")
            out:put(id)
            out:put("), ")
            if push[state] and push[state][id] then
                out:put(string.format("R_PUSH(%d)", NFA[state][id]))
            elseif pop[state] and pop[state][id] then
                out:put(string.format("R_POP(%d, %d)", 1+pop[state][id], NFA[state][id]))
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

    -- local f = io.open(arg[3], "w")
    -- f:write(out:tostring())
    -- f:close()
end

