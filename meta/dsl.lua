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

local state_count = 1
local stack_action = {}

local node_type_count = 0
local node_types = {}

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

local all_patterns = {}
local subpat_map = {}

local curr_pipeline = {}
local inst_classes = {}

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
    elseif t == "pipeline" then
        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local desc = parse_node()

        curr_pipeline = { name=desc[1], units={} }
        for i=2,#desc do
            if desc[i][1] == "unit" then
                local name = desc[i][2]
                curr_pipeline.units[name] = 1
            end
        end

        for i=1,desc.ports do
            local name = string.format("p%x", i-1)
            curr_pipeline.units[name] = 1
        end

        print(desc[1], inspect(curr_pipeline.units))
    elseif t == "inst_class" then
        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local desc = parse_node()
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

        t = lex()
        if t == "(" then
            t = parse_node()
        end
        all_patterns[#all_patterns + 1] = { is_subpat, pattern, where, t }

        -- reset muh shit
        captures = {}
        capture_count = 0
    end
end

--------------------------
-- DFA construction
--------------------------
function get_pattern_inner(n, shape)
    local str = "("
    if n.name then
        str = str..n.name..": "
    end

    if shape then
        if n[1] == "x86_MEMORY" then
            str = str.."MEM_OP"
        elseif n[1] == "x86_COND" then
            str = str.."CC_OP"
        else
            str = str.."ANY_OP"
        end
    else
        str = str..n[1]
    end

    for i=2,#n do
        local t = n[i]
        str = str.." "
        if type(t) == "table" then
            str = str..get_pattern_inner(t, shape)
        elseif t == "..." then
            str = str.."..."
        elseif t == "$REST" then
            str = str.."$REST"
        elseif t == "___" then
            str = str.."___"
        else
            str = str..t
        end
    end

    return str..")"
end

function get_pattern_pretty(n)
    return get_pattern_inner(n, false)
end

function get_pattern_shape(n)
    return get_pattern_inner(n, true)
end

DFA = {}

local function nfa_new_edge(from, when)
    if not DFA[from] then
        DFA[from] = {}
    elseif DFA[from][when] then
        return DFA[from][when]
    end

    local to = state_count
    state_count = state_count + 1
    DFA[from][when] = to
    return to
end

local function nfa_edge(from, when, to)
    if not DFA[from] then
        DFA[from] = {}
    end

    assert(not DFA[from][when])
    DFA[from][when] = to
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

DFA_cache = {}
local function nfa_compile(n, head, active, depth, any_loop)
    local ty = n[1]

    -- what partitions are we "in" right now
    local next_active = {}
    local s = {}
    for i=1,#active do
        if active[i][1] == ty then
            -- local last_pat = active[i][#active[i]]
            next_active[#next_active + 1] = active[i]
            s[#s + 1] = active[i]
        end
    end

    local active_str = make_active_str(next_active)
    -- print("Active", inspect(n), active_str)

    -- if there's already an edge which matches
    -- the active set, we use that
    if not DFA_cache[head] then
        DFA_cache[head] = {}
    end

    if DFA_cache[head][active_str] then
        -- print("Cached", DFA[head][ty], head, ty, DFA_cache[head][active_str])

        -- reuse head
        local new_head = DFA_cache[head][active_str]
        if DFA[new_head][0] == new_head then
            -- if there's a loop, we'll always "win" the match but we do need
            -- to hook our first "non-any" case to this loop
            any_loop = new_head
            if DFA_cache[head][active_str] == DFA[head][ty] then
                DFA[head][ty] = nil
            end

            new_head = nfa_new_edge(head, ty)
            if not DFA[new_head] then
                DFA[new_head] = {}
            end
        end

        DFA[head][ty] = new_head
        head = new_head
    else
        local old_head = head
        head = nfa_new_edge(head, ty)
        DFA_cache[old_head][active_str] = head
    end
    stack_action[head] = depth

    for i=2,#n do
        local t = n[i]
        if type(t) == "table" then
            local kid_active = {}
            for j=1,#next_active do
                kid_active[#kid_active + 1] = next_active[j][i]
            end
            head = nfa_compile(t, head, kid_active, depth + 1, any_loop)
        elseif t == "..." or t == "$REST" then
            -- match any, but in a cycle
            if not DFA[head] then
                DFA[head] = {}
            end

            DFA[head][0] = head
        elseif t == "___" then
            head = nfa_new_edge(head, "TB_NULL")

            -- if we fail to match, we'll fallback to the loop
            if any_loop and (not DFA[head] or not DFA[head][0]) then
                -- print("FAIL", head, 0, any_loop)
                nfa_edge(head, 0, any_loop)
            end
        elseif t:byte(1) == string.byte("$") then -- capture
            head = nfa_new_edge(head, 0)
        else
            error("Hehe", t)
        end

        stack_action[head] = depth
    end

    if any_loop and (not DFA[head] or not DFA[head][0]) then
        print("FAIL", head, 0, any_loop)
        nfa_edge(head, 0, any_loop)
    end

    -- pop once we've exhausted all the inputs
    head = nfa_new_edge(head, "END")
    if depth == 1 then
        stack_action[head] = 1
    end

    return head
end

-- Split partitions by similar pattern shapes
local partitions = {}
local ordered = {}

local active = {}
for i,pat in ipairs(all_patterns) do
    local k = get_pattern_shape(pat[2])
    if not partitions[k] then
        partitions[k] = {}
        ordered[#ordered + 1] = k
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

for i,name in ipairs(ordered) do
    local set = partitions[name]
    partition_count = partition_count + 1
    for i=1,#set do
        walk_into(set[i][2], partition_count)
    end
    print(i, name, #set)
end

-- Generate C code for the matcher
local lines = {}
visited = {}
function add_line(depth, line)
    lines[#lines + 1] = string.rep(" ", depth*4) .. line
end

-- inv is true if we're checking if we're not "t"
function compare_to_node_type(stack, t, inv)
    local in_str = get_in(stack)
    if t == "TB_NULL" then
        if inv then
            return in_str.." != NULL"
        else
            return in_str.." == NULL"
        end
    elseif t == "END" then
        if inv then
            return stack[2].."->input_count > "..stack[3]
        else
            return stack[2].."->input_count <= "..stack[3]
        end
    -- elseif t == "x86_MEMORY" or t == "x86_COND" then
    --    return "TRY_OPERAND("..in_str..")" -- "peek = node_isel_raw(ctx, f, "..in_str..", depth+1), peek"
    else
        if inv then
            return in_str.."->type != "..t
        else
            return in_str.."->type == "..t
        end
    end
end

function get_in(stack)
    if stack[3] then
        return stack[2].."->inputs["..stack[3].."]"
    else
        return stack[2]
    end
end

function clone_stack(stack)
    local prev = nil
    if stack[1] then
        prev = clone_stack(stack[1])
    end
    return { prev, stack[2], stack[3], stack[4], stack[5] }
end

var_counter = 0
function gen_edge(head, stack, depth)
    local stack_depth = stack[4]

    -- Push can only really go up once each time
    if not stack_action[head] or stack_action[head] == stack_depth then
        if stack[3] then
            stack[3] = stack[3] + 1
        end
        return stack
    elseif stack_action[head] > stack_depth then
        local in_str = get_in(stack)
        local new_name = nil
        if stack[3] then
            new_name = string.format("n$%d", var_counter)
            add_line(depth, string.format("TB_Node* %s = %s;", new_name, in_str))
            var_counter = var_counter + 1
        else
            new_name = stack[2]
        end
        return { stack, new_name, 0, stack[4]+1 }
    end

    while stack_action[head] < stack_depth do
        stack = stack[1]
        stack_depth = stack[4]
    end
    return stack
end

function push_expr(stack, depth, active)
    local in_str = get_in(stack)
    local new_name = nil
    if stack[3] then
        new_name = string.format("n$%d", var_counter)
        add_line(depth, string.format("TB_Node* %s = %s;", new_name, in_str))
        var_counter = var_counter + 1
    else
        new_name = stack[2]
    end
    return { stack, new_name, 0, stack[4]+1, active }
end

if false then
    local mach_prefix_caps = string.upper(arg[1])

    lines[#lines + 1] = "typedef enum "..mach_prefix_caps.."NodeType {"

    local function cmp_node_types(table, a, b)
        return table[a].id < table[b].id
    end

    local sorted_types = sort(node_types, cmp_node_types)
    for i,k in ipairs(sorted_types) do
        lines[#lines + 1] = string.format("    %s = TB_MACH_"..mach_prefix_caps.." + %d,", k, node_types[k].id)
    end
    lines[#lines + 1] = "} "..mach_prefix_caps.."NodeType;"
    lines[#lines + 1] = ""
    lines[#lines + 1] = "static const char* node_name(int n_type) {"
    lines[#lines + 1] = "    switch (n_type) {"
    for i,k in ipairs(sorted_types) do
        lines[#lines + 1] = string.format("        case %s: return \"%s\";", k, node_types[k].name)
    end
    lines[#lines + 1] = "        default: return NULL;"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""
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

function compute_pattern_hash(strs, expr, set, hashes)
    local n = set[1]
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            local kid = {}
            for j=1,#set do
                kid[j] = set[j][i]
            end

            compute_pattern_hash(strs, expr.."->inputs["..(i-2).."]", kid, hashes)
        end
    end

    -- if the set disagrees, hash it
    local node_type = n[1]
    for i=2,#set do
        if node_type ~= set[i][1] then
            node_type = nil
            break
        end
    end

    if not node_type then
        for i=1,#set do
            local t = node_enum_types[set[i][1]]
            if not t then
                t = 256+node_types[set[i][1]].id
            end
            hashes[i] = hashes[i]*31 + t
        end

        strs[#strs + 1] = string.format("        hash = hash*31 + %s->type;", expr)
    end
end

local node_cnt = 0
function write_node(strs, ids, set, hashes)
    local n = set[1]

    -- create kids first
    local in_cnt = 0
    local uses_rest = false
    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            local kid = {}
            for j=1,#set do
                kid[j] = set[j][i]
            end

            write_node(strs, ids, kid, hashes)
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

    -- if the set disagrees
    local node_type = n[1]
    for i=2,#set do
        if node_type ~= set[i][1] then
            node_type = nil
            break
        end
    end

    if not node_type then
        node_type = "k"..ids[n].."_type"
        strs[#strs + 1] = string.format("        int %s = 0;", node_type)
        strs[#strs + 1] = "        switch (hash) {"
        for i=1,#set do
            strs[#strs + 1] = string.format("            case %d: %s = %s; break;", hashes[i], node_type, set[i][1])
        end
        strs[#strs + 1] = "            default: abort(); return NULL;"
        strs[#strs + 1] = "        }"
    end

    -- declaration of a node
    local extra_type = extra_node_type_name(n[1])
    local dt_name = n.dt
    if dt_name == nil then
        dt_name = "TB_TYPE_VOID"
    end

    local potentially_dynamic_count = false
    strs[#strs + 1] = string.format("        size_t k%d_i = 0;", ids[n])
    if uses_rest then
        strs[#strs + 1] = string.format("        TB_Node* k%d = tb_alloc_node(f, %s, %s, %d + $REST_LEN, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type)
    else
        strs[#strs + 1] = string.format("        TB_Node* k%d = tb_alloc_node(f, %s, %s, %d, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type)
    end

    -- input edges
    for i=2,#n do
        local v = n[i]
        if v == "___" then
            -- just keep as NULL
            strs[#strs + 1] = string.format("        k%d_i++;", ids[n])
        elseif type(v) == "table" then
            strs[#strs + 1] = string.format("        set_input(f, k%d, k%d, k%d_i++);", ids[n], ids[v], ids[n])
        elseif mem_capture and v == mem_capture.name then
            -- copy all the extra data
            potentially_dynamic_count = true
            -- copy inputs
            strs[#strs + 1] = string.format("        if (%s->type != %s) {", v, mach_prefix.."MEMORY")
            strs[#strs + 1] = string.format("            set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
            strs[#strs + 1] = string.format("        } else {")
            strs[#strs + 1] = string.format("            FOR_N(i, 0, %s->input_count) { set_input(f, k%d, %s->inputs[i], k%d_i++); }", v, ids[n], v, ids[n])
            strs[#strs + 1] = string.format("        }")
        elseif v == "$REST" then
            strs[#strs + 1] = string.format("        FOR_N(i, 0, $REST_LEN) { set_input(f, k%d, $REST[i], k%d_i++); }", ids[n], ids[n])
        elseif type(v) == "string" and v:byte(1) == string.byte("$") then
            strs[#strs + 1] = string.format("        set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
        else
            strs[#strs + 1] = string.format("        set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n])
        end
    end

    strs[#strs + 1] = string.format("        %s* k%d_extra = TB_NODE_GET_EXTRA(k%d);", extra_type, ids[n], ids[n])
    for k,v in pairs(n) do
        if type(k) == "string" and k ~= "dt" then
            -- replace all uses of captures with their real name
            strs[#strs + 1] = string.format("        k%d_extra->%s = %s;", ids[n], k, v)
        elseif mem_capture and v == mem_capture.name then
            strs[#strs + 1] = string.format("        memcpy(k%d_extra, %s->extra, sizeof("..mach_extra_type.."));", ids[n], v)
        end
    end

    -- trim size
    if potentially_dynamic_count then
        strs[#strs + 1] = string.format("        k%d->input_count = k%d_i;", ids[n], ids[n])
    end

    -- apply GVN
    strs[#strs + 1] = string.format("        k%d = tb_opt_gvn_node(f, k%d);", ids[n], ids[n])
    strs[#strs + 1] = ""
end

node_basic_fields = {
  ["dt"] = true,
}

function find_captures(strs, n, expr, shared)
    local indent = "    "
    if not shared then
        indent = "        "
    end

    if n.name then
        if n[1] == "x86_MEMORY" then
            if mem_capture then
                print("Can't capture 2 memory operands in one pattern?!?")
                os.exit(1)
            end

            mem_capture = n
        end

        if shared then
            strs[#strs + 1] = string.format("%sTB_Node* %s = %s;", indent, n.name, expr)
        end
    end

    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            find_captures(strs, v, expr.."->inputs["..(i-2).."]", shared)
        elseif shared then
            if v == "$REST" then
                strs[#strs + 1] = string.format("%ssize_t %s_LEN = %s->input_count - %d;", indent, v, expr, i-2)
                strs[#strs + 1] = string.format("%sTB_Node** %s  = &%s->inputs[%d];", indent, v, expr, i-2)
            elseif type(v) == "string" and v:byte(1) == string.byte("$") then
                strs[#strs + 1] = string.format("%sTB_Node* %s = %s->inputs[%d];", indent, v, expr, i-2)
            end
        end
    end

    if not shared then
        local extra_type = extra_node_type_name(n[1])
        for k,v in pairs(n) do
            if type(k) == "string" and type(v) == "string" and k ~= "name" and v ~= "$REST" then
                if node_basic_fields[k] then
                    strs[#strs + 1] = string.format("%sTB_DataType %s = %s->%s;", indent, v, expr, k)
                else
                    local t = node_extra_desc[extra_type]
                    if t then
                        t = t[k]
                    else
                        t = "uint64_t"
                    end

                    strs[#strs + 1] = string.format("%s%s %s = TB_NODE_GET_EXTRA_T(%s, %s)->%s;", indent, t, v, expr, extra_type, k)
                end
            end
        end
    end

    return id_cnt
end

if false then
    for id,head in ipairs(ordered_accept) do
        local set = accept[head]
        -- if id == 5 then goto skip end

        -- split replacement rules based on replacement shape
        local rep_partitions = {}
        local ord_rep = {}

        for i=1,#set do
            local pat = get_pattern_pretty(set[i][2])
            local rep = set[i][4]
            local rep_shape = rep
            if type(rep) ~= "string" then
                rep = get_pattern_pretty(set[i][4])
                rep_shape = get_pattern_shape(set[i][4])
            end

            -- we want the replacement shape to also care about the where expression
            if set[i][3] then
                rep_shape = rep_shape.." && "..set[i][3]
            end

            do -- sort based on replacement shape
                local p = rep_partitions[rep_shape]
                if p then
                    p[#p + 1] = set[i]
                else
                    rep_partitions[rep_shape] = { set[i] }
                    ord_rep[#ord_rep + 1] = rep_shape
                end
            end
        end

        add_line(0, string.format("static TB_Node* mach_dfa_accept_%d(Ctx* ctx, TB_Function* f, TB_Node* n) {", id))

        -- All the captures are the same across the sets
        mem_capture = nil
        find_captures(lines, set[1][2], "n", true)
        lines[#lines+1] = ""

        for i,rep_shape in ipairs(ord_rep) do
            local set = rep_partitions[rep_shape]

            for j=1,#set do
                local pat = get_pattern_pretty(set[j][2])
                local rep = set[j][4]
                if type(rep) ~= "string" then
                    rep = get_pattern_pretty(rep)
                end

                if set[j][3] then
                    add_line(0, string.format("    // %s where '%s' => %s", pat, set[j][3], rep))
                else
                    add_line(0, string.format("    // %s => %s", pat, rep))
                end
            end

            lines[#lines + 1] = "    do {"
            if type(set[1][4]) == "string" then -- this replacement rule is just calling out to C
                lines[#lines + 1] = "        TB_Node* k = "..set[1][4]..";"
                lines[#lines + 1] = "        if (k) { return k; }"
            else
                mem_capture = nil
                find_captures(lines, set[1][2], "n", false)

                local where = set[1][3]
                if where then
                    lines[#lines + 1] = "        if (!("..where..")) {"
                    lines[#lines + 1] = "            break;"
                    lines[#lines + 1] = "        }"
                    lines[#lines + 1] = ""
                end

                local pat = {}
                local rep = {}
                local hashes = {}
                for j=1,#set do
                    pat[j] = set[j][2]
                    rep[j] = set[j][4]
                    hashes[j] = 0
                end

                -- Compute hashes based on information which differs between the
                -- nodes.
                lines[#lines + 1] = "        uint64_t hash = 0;"
                compute_pattern_hash(lines, "n", pat, hashes)
                lines[#lines + 1] = ""

                -- Every replacement has the same shape, just different opcodes
                local ids = {}
                node_cnt = 0
                write_node(lines, ids, rep, hashes)

                lines[#lines + 1] = "        return k"..ids[rep[1]]..";"
            end

            lines[#lines + 1] = "    } while (0);"
            lines[#lines + 1] = ""
        end

        add_line(1, "return NULL;")
        add_line(0, "}")
        lines[#lines+1] = ""

        ::skip::
    end
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

local function make_active_str_from_indices(list)
    local strs = {}
    for i=1,partition_count do
        strs[i] = 0
    end

    for i=1,#list do
        strs[list[i]] = 1
    end

    return table.concat(strs)
end

function canonicalize_type(ty)
    if ty == "..." or ty == "$REST" or (type(ty) == "string" and ty:byte(1) == string.byte("$")) then
        return "ANY"
    else
        return ty
    end
end

function get_pattern_at(pat, i)
    if i > #pat then
        if (pat[#pat] == "..." or pat[#pat] == "$REST") then
            return "ANY"
        else
            return "END"
        end
    end

    if type(pat[i]) == "table" then
        return pat[i][1]
    end

    return canonicalize_type(pat[i])
end

label_count = 0
needs_bail = {}

function gen_c_inner(active, depth, stack)
    local limit = 0
    for i=1,#active do
        limit = math.max(limit, #active[i])
    end

    local needs_bail_label = nil
    local active_str = make_active_str(active)

    local pos = 2
    while true do
        print("Step", stack[3], stack[4])
        -- which types lead to which active sets
        local deltas = {}
        for i=1,#active do
            local ty = get_pattern_at(active[i], pos)
            local list = deltas[ty]
            if list then
                list[#list + 1] = active[i]
            else
                deltas[ty] = { active[i] }
            end
        end

        local case_count = 0
        local leader = nil
        local cases = {}
        for k,v in pairs(deltas) do
            local str = make_active_str(v)
            local list = cases[str]
            if not list then
                list = {}
                cases[str] = list
                case_count = case_count + 1

                if get_pattern_at(v[1], pos) ~= "ANY" then
                    leader = v[1]
                end
            end

            for i=1,#v do
                list[#list + 1] = v[i]
            end
        end

        local in_str = get_in(stack)
        local has_leader = leader and 1 or 0
        if case_count - has_leader <= 1 then
            -- every case agrees, apply matching rule
            if not leader then
                add_line(depth, "// MATCH ANY "..in_str)
            else
                local key = get_pattern_at(leader, pos)
                if key == "___" then
                    add_line(depth, string.format("if (%s != NULL) { goto BAIL%d; }", in_str, label_count))

                    needs_bail_label = label_count
                    label_count = label_count + 1
                elseif key == "END" then
                    add_line(depth, "return ACCEPT(1111);")
                else
                    -- matched against specific node type
                    local then_active = {}
                    local else_active = {}
                    for i=1,#active do
                        local v = active[i]
                        local ret = get_pattern_at(active[i], pos+1)

                        if get_pattern_at(active[i], pos) ~= "ANY" then
                            print("THEN", get_pattern_pretty(active[i][pos]), ret)
                            then_active[#then_active + 1] = active[i][pos]
                        else
                            print("ELSE", get_pattern_pretty(active[i]), ret)
                            else_active[#else_active + 1] = active[i]
                        end
                    end

                    add_line(depth, "if ("..in_str.."->type == "..key..") {")
                    local next_stack = push_expr(stack, depth+1, active)
                    gen_c_inner(then_active, depth+1, next_stack)
                    add_line(depth, "}")

                    active = else_active
                end
            end
        else
            print("MULTI", case_count, has_leader, inspect(cases))
            add_line(depth, "// MULTI-WAY!!! "..case_count)

            local can_bail = nil
            for k,v in pairs(cases) do
                local keys = {}
                for i=1,#v do
                    local ty = get_pattern_at(v[i], pos)
                    if ty == "ANY" then can_bail = k end
                end
            end

            local next_stack = push_expr(stack, depth, active)

            -- each path is stepping into the node so might as well do it up here
            print("SWITCH", in_str)
            add_line(depth, "do {")
            for k,v in pairs(cases) do
                if k ~= can_bail then
                    local keys = {}
                    local then_active = {}
                    for i=1,#v do
                        local ty = get_pattern_at(v[i], pos)
                        add_if_new(keys, ty)

                        if v[i][pos] and type(v[i][pos]) == "table" then
                            print("STEP INTO", v[i][pos][1])
                            then_active[#then_active + 1] = v[i][pos]
                        end
                    end

                    local exprs = {}
                    for i=1,#keys do
                        exprs[#exprs + 1] = string.format("%s == %s", in_str, keys[i])
                    end

                    local expr = table.concat(exprs, " || ")
                    print("CASE", inspect(keys), v, expr)

                    add_line(depth+1, "if ("..expr..") {")
                    -- gen_c_inner(then_active, depth+2, clone_stack(next_stack))
                    add_line(depth+1, "}")
                end
            end

            if can_bail then
                add_line(depth+1, "// ANY CASE")
                -- gen_c_inner(cases[can_bail], depth+1, next_stack)
            end
            add_line(depth, "} while (0);")
        end
        stack[3] = stack[3] + 1

        print("CASES", case_count)
        for k,v in pairs(cases) do
            for i=1,#v do
                print(k, get_pattern_pretty(v[i]))
            end
            print()
        end

        pos = pos + 1
        if pos >= limit then
            -- Pop from stack, continue to process
            stack = clone_stack(stack[1])

            pos = stack[3]
            active = stack[5]
            if stack[4] <= 1 then
                break
            end

            add_line(depth, "// POP "..stack[4])
        end
    end

    -- input_count query on the non-variadic patterns
    local var_final = nil
    local non_var_final = nil

    for i=1,#active do
        local pat = active[i]
        local id = to_partition[pat]

        if pat[#pat] ~= "..." and pat[#pat] ~= "$REST" then
            if not non_var_final then
                non_var_final = id
            elseif non_var_final ~= id then
                non_var_final = "JOVER"
            end
        else
            if not var_final then
                var_final = id
            elseif var_final ~= id then
                var_final = "JOVER"
            end
        end
    end

    if needs_bail_label then
        add_line(depth, string.format("BAIL%d:", needs_bail_label))
    end

    -- We can only hit ACCEPT once we've walked to the end of the stream
    if non_var_final and non_var_final ~= "JOVER" then
        add_line(depth, string.format("if (%s->input_count == %d) { return ACCEPT(%d); }", stack[2], limit-1, non_var_final))
    end

    if var_final and var_final ~= "JOVER" then
        add_line(depth, string.format("return ACCEPT(%d);", var_final))
    end

    -- There's no "next", we should bail
    if not var_final and stack[4] == 1 then
        add_line(depth, "return NULL;")
    end

    return active
end

function gen_c(active, depth, stack, pos)
    local active_str = make_active_str(active)

    -- which types lead to which active sets
    local deltas = {}
    for i=1,#active do
        local ty = active[i][pos]
        local list = deltas[ty]
        if list then
            list[#list + 1] = active[i]
        else
            deltas[ty] = { active[i] }
        end
    end

    local case_count = 0
    local cases = {}
    for k,v in pairs(deltas) do
        local str = make_active_str(v)
        local list = cases[str]
        if not list then
            list = {}
            cases[str] = list
            case_count = case_count + 1
        end

        for i=1,#v do
            list[#list + 1] = v[i]
        end
    end

    -- each path is stepping into the node so might as well do it up here
    local in_str = get_in(stack)

    add_line(depth, "switch ("..in_str.."->type) {")
    for k,v in pairs(cases) do
        local keys = {}
        for i=1,#v do
            add_if_new(keys, v[i][1])
        end

        for i=1,#keys do
            add_line(depth+1, "case "..keys[i]..":")
        end
        add_line(depth+1, "{")
        local next_stack = push_expr(stack, depth, active)
        gen_c_inner(v, depth+2, next_stack)
        add_line(depth+1, "}")
    end
    add_line(depth, "}")
    return stack
end

if true then
    lines[#lines + 1] = "#define ACCEPT(x) mach_dfa_accept_ ## x(ctx, f, n)"
    lines[#lines + 1] = "static TB_Node* mach_dfa_match(Ctx* ctx, TB_Function* f, TB_Node* n, int depth) {"
    lines[#lines + 1] = "    TB_Node* k;"

    local all = {}
    for i=1,#all_patterns do
        all[i] = all_patterns[i][2]
        print(get_pattern_pretty(all[i]))
    end

    local stack = { nil, "n", nil, 0, all }
    gen_c(all, 1, stack, 1)

    lines[#lines + 1] = "    return NULL;"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = "#undef ACCEPT"
    lines[#lines + 1] = ""
end

local final_src = table.concat(lines, "\n")
print(final_src)

if false then
    local f = io.open(arg[3], "w")
    f:write(final_src)
    f:close()
end

os.exit(0)

local special_return_cases = {}
if true then
    local graphviz = buffer.new(1000)
    graphviz:put("digraph G {\n")
    for state,n in pairs(DFA) do
        local label = nil
        if accept[state] then
            label = "A"..state
        else
            label = "v"..state
        end

        if stack_action[state] then
            label = label.."("..stack_action[state]..")"
        end
        graphviz:put(string.format("v%d [label=\"%s\"]\n", state, label))

        for id,next in pairs(n) do
            local str = id
            if id == 0 then
                str = "ANY"
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
        for id,next in pairs(DFA[state]) do
            out:put("        (")
            out:put(state)
            out:put(")<<16 | (")
            out:put(id)
            out:put("), ")
            if push[state] and push[state][id] then
                out:put(string.format("R_PUSH(%d)", DFA[state][id]))
            elseif pop[state] and pop[state][id] then
                out:put(string.format("R_POP(%d, %d)", 1+pop[state][id], DFA[state][id]))
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

