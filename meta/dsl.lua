local buffer  = require "string.buffer"
local inspect = require "meta/inspect"

require "meta/prelude"

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

print(arg[1])
local source = run_command("clang -E -xc "..arg[1])
local lex = lexer(source)

local lines = {}
local reg_classes = {}

local is_operand = {}

local state_count = 1
local stack_action = {}

local node_type_count = 0
local node_types = {}

local all_patterns = {}
local subpat_map = OrderedSet()

function skip_ws(j, def)
    while j < #def and def:byte(j) == 32 do
        j = j + 1
    end
    return j
end

lines[#lines + 1] = "#include \"../emitter.h\""
lines[#lines + 1] = "#include \"../tb_internal.h\""
lines[#lines + 1] = ""

function decl_type(n)
    local name = n[2]
    local kind = n[1]

    if name == "___" then
        lines[#lines + 1] = kind.." {"
    else
        lines[#lines + 1] = "typedef "..kind.." "..name.." {"
    end

    if kind == "struct" or kind == "union" then

    elseif kind == "enum" then
        local ord = 0
        for i=3,#n do
            local name = n[i]
            if type(n[i]) == "table" then
                ord = n[i][2]
                name = n[i][1]
            end

            lines[#lines + 1] = string.format("    %s = %d,", name, ord)
            ord = ord + 1
        end
    else
        assert(false, "bad")
    end

    if name == "___" then
        lines[#lines + 1] = "};"
    else
        lines[#lines + 1] = "} "..name..";"
    end
    lines[#lines + 1] = ""
end

while true do
    local t = lex()
    if t == nil then
        break
    end

    mem_capture = nil
    local start_line = line_num

    if t == "pat" or t == "subpat" then
        local is_subpat = t == "subpat"

        t = lex()
        if t ~= "(" then
            print("fuck but in parsing")
            os.exit(1)
        end

        local pattern = parse_node(lex)

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
            t = parse_node(lex)
            if is_subpat then
                subpat_map:put(pattern[1])
            end
        end
        all_patterns[#all_patterns + 1] = { pattern, where, is_subpat, t }
    elseif t == "(" then
        local n = parse_node(lex)

        if n[1] == "namespace" then
            mach_prefix = n[2].."_"
        elseif n[1] == "reg_class" then
            reg_classes[#reg_classes + 1] = { n[2], math.max(1, #n - 2) }

            if #n > 3 then
                lines[#lines + 1] = "typedef enum "..n[2].." {"
                for i=3,#n do
                    lines[#lines + 1] = "    "..n[i]..","
                end
                lines[#lines + 1] = "    "..n[2].."_NONE = -1,"
                lines[#lines + 1] = "} "..n[2]..";"
                lines[#lines + 1] = ""
            end
        elseif n[1] == "node" then
            -- define node types
            local name = mach_prefix..n[2]
            if node_types[name] then
                print(string.format("node type '%s' has already been defined", n[2]))
                os.exit(1)
            end

            if not n[3] then
                print(string.format("node '%s' must have a defined 'extra type'", n[2]))
                os.exit(1)
            end

            node_types[name] = { id=node_type_count, name=name, extra=n[3] }
            node_type_count = node_type_count + 1
        elseif n[1] == "enum" or n[1] == "struct" or n[1] == "union" then
            decl_type(n)
        elseif n[1] == "pipeline" then
            local pipeline = { units={}, classes={}, usages={} }

            local unit_count = 0
            if n.ports then
                -- initialize execution ports
                for i=1,n.ports do
                    local name = "p"..(i-1)
                    pipeline.units[name] = unit_count
                    unit_count = unit_count + 1
                end
            end

            for i=2,#n do
                if type(n[i]) == "table" then
                    if n[i][1] == "unit" then
                        local name = n[i][2]

                        unit_count = unit_count + 1
                        pipeline.units[name] = unit_count
                    elseif n[i][1] == "class" then
                        local name  = n[i][2]
                        local proto = n[i][3]

                        local usage = {}
                        for j=4,#n do
                            usage[#usage + 1] = n[i][j]
                        end

                        print()
                        print("Class", name, inspect(usage))

                        if false then
                            -- parse definition
                            local j = 1
                            local cycle = 0
                            while j <= #def do
                                j = skip_ws(j, def)

                                -- cycle count
                                local cnt = 0
                                while j <= #def and def:byte(j) >= 48 and def:byte(j) <= 57 do
                                    cnt = cnt*10 + (def:byte(j) - 48)
                                    j = j + 1
                                end
                                assert(j <= #def)
                                assert(def:sub(j, j) == "*")
                                j = j + 1

                                if def:sub(j, j) == "p" then
                                    j = j + 1

                                    local u = {}
                                    while j <= #def and def:byte(j) >= 48 and def:byte(j) <= 57 do
                                        local port = "p"..(def:byte(j) - 48)
                                        assert(pipeline.units[port])
                                        u[#u + 1] = pipeline.units[port]
                                        j = j + 1
                                    end

                                    print("Port", cnt, inspect(u))

                                    u.count = cnt
                                    usage[#usage + 1] = u
                                end

                                j = skip_ws(j, def)
                                if def:sub(j, j) ~= "+" then
                                    break
                                end
                                j = j + 1
                            end
                        end

                        local function enumerate_class(usage, final, head)
                            while head <= #usage do
                                -- find next split point
                                local piece = usage[head]
                                if type(piece) == "table" and #piece > 2 then
                                    -- clone timelines that follow the different paths
                                    for i=3,#piece do
                                        local cloned = { name=usage.name }
                                        for j=1,#usage do
                                            cloned[j] = usage[j]
                                        end
                                        cloned[head] = { usage[head][1], usage[head][i] }
                                        enumerate_class(cloned, final, head+1)
                                    end
                                    usage[head] = { usage[head][1], usage[head][2] }
                                end
                                head = head + 1
                            end

                            final[#final + 1] = usage
                            print("Path", inspect(usage))
                        end

                        usage.name = name

                        -- generate new instruction classes for all
                        -- possible unit utilizations
                        local final = {}
                        enumerate_class(usage, final, 1)

                        for i=1,#final do
                            pipeline.usages[#pipeline.usages + 1] = final[i]
                        end

                        pipeline.classes[name] = final
                    end
                end
            end

            local max_cycles = 0
            for i=1,#pipeline.usages do
                local u = pipeline.usages[i]
                local t = 0
                for j=1,#u do
                    if type(u[j]) == "table" then
                        t = t + u[j][1]
                    else
                        t = t + 1
                    end
                end
                max_cycles = math.max(max_cycles, t)
            end

            local function alloc_bit_matrix(m, n)
                local mat = {}
                for i=1,m do
                    local row = {}
                    for j=1,n do
                        row[j] = "0"
                    end
                    mat[i] = row
                end
                return mat
            end

            local function print_bit_matrix(mat, tag)
                print("Matrix", tag)
                for i=1,#mat do
                    print(i, table.concat(mat[i]))
                end
            end

            print()
            print("Max cycles", max_cycles)
            print("Units", inspect(pipeline.units))

            -- Allocate resource and collision matrices
            local collisions = {}
            local resources = {}
            for i=1,#pipeline.usages do
                local u = pipeline.usages[i]
                collisions[i] = alloc_bit_matrix(max_cycles, #pipeline.usages)

                -- fill in resources
                local t = 1
                local tab = alloc_bit_matrix(max_cycles, unit_count)
                for j=1,#u do
                    local stop = type(u[j]) == "table" and u[j][1] or 1
                    stop = stop + t

                    local unit = type(u[j]) == "table" and u[j][2] or u[j]
                    unit = pipeline.units[unit]

                    while t < stop do
                        tab[t][unit] = "1"
                        t = t + 1
                    end
                end

                print_bit_matrix(tab, "Resource "..u.name)
                resources[i] = tab
            end

            -- Fill collision matrices
            for a=1,#pipeline.usages do
                local ia = resources[a]
                local col = collisions[a]

                local tab = {}
                for t=1,max_cycles do
                    for b=1,#pipeline.usages do
                        local ib = resources[b]

                        -- M[a, b, t] = (I[a, j+t] /\ I[b, j]) ~= 0
                        local busy = false
                        for j=1,max_cycles do
                            for unit=1,unit_count do
                                if (t+j <= max_cycles and ia[t+j][unit]) and ib[j][unit] then
                                    busy = true
                                end
                            end
                        end

                        if busy then
                            -- print("Busy", a, b)
                            col[t][b] = "1"
                        end
                    end
                end

                print_bit_matrix(col, "Collision "..pipeline.usages[a].name)
            end

            print(inspect(pipeline))
        else
            print(inspect(n))
        end
    else
        print("expected either pat, subpat or an S-expr, got '"..t.."'")
        os.exit(1)
    end
end

lines[#lines + 1] = "enum {"
for i=1,#reg_classes do
    lines[#lines + 1] = string.format("    REG_CLASS_%s = %d,", reg_classes[i][1], i);
end
lines[#lines + 1] = "    REG_CLASS_COUNT,"
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
lines[#lines + 1] = "enum {"
lines[#lines + 1] = "    BUNDLE_INST_MAX = 1,"
lines[#lines + 1] = "};"
lines[#lines + 1] = ""
lines[#lines + 1] = "#include \"../codegen_impl.h\""
lines[#lines + 1] = ""
lines[#lines + 1] = "static void mach_dsl_init(Ctx* restrict ctx, TB_ABI abi) {"
for i=1,#reg_classes do
    lines[#lines + 1] = string.format("    ctx->num_regs[REG_CLASS_%s] = %d;", reg_classes[i][1], reg_classes[i][2]);
end
lines[#lines + 1] = "}"
lines[#lines + 1] = ""

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

-- all_patterns: [int](pattern, where_str, ...)
-- shape_fn:     fn(pattern) -> ANY
function PatternMatcher(all_patterns, shape_fn)
    local t = {}

    -- organize the patterns into partitions
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

    function t:gen_c(lines, depth)

    end

    return t
end

-- Split partitions by similar pattern shapes
local partitions = {}
local ordered = {}

local active = {}
for i,pat in ipairs(all_patterns) do
    local k = get_pattern_shape(pat[1])
    if not partitions[k] then
        partitions[k] = {}
        ordered[#ordered + 1] = k
    end

    partitions[k][#partitions[k] + 1] = pat
    active[#active + 1] = pat[1]
end

to_partition = {}
pattern_ids = {}
partition_count = 0

function walk_into(n, idx, idx2)
    for i=2,#n do
        if type(n[i]) == "table" then
            walk_into(n[i], idx, idx2)
        end
    end

    to_partition[n] = idx
    pattern_ids[n] = idx2
end

local pattern_count = 0
for i,name in ipairs(ordered) do
    local set = partitions[name]
    partition_count = partition_count + 1

    assert(partition_count == i)
    for i=1,#set do
        walk_into(set[i][1], partition_count, pattern_count)
        pattern_count = pattern_count + 1
    end
    -- print(i, name, #set)
end

-- Generate C code for the matcher
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

if true then
    local mach_prefix_caps = string.upper(mach_prefix:sub(1, -2))
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
    lines[#lines + 1] = "static void global_init(void) {"
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
    ["TB_NodeIf"]       = { prob="float" },
}

function extra_node_type_name(t)
    local extra_type = ""
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
    elseif t == "TB_IF" then
        extra_type = "TB_NodeIf"
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
    elseif t:sub(1, #mach_prefix) == mach_prefix then
        extra_type = node_types[t].extra
        assert(extra_type)
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

        strs[#strs + 1] = string.format("    hash = hash*31 + %s->type;", expr)
    end
end

local node_cnt = 0
function write_node(ids, set, hashes)
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

            write_node(ids, kid, hashes)
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

    local depth = 4
    if not node_type then
        node_type = "k"..ids[n].."_type"
        add_line(depth, string.format("int %s = 0;", node_type))
        add_line(depth, "switch (hash) {")
        for i=1,#set do
            add_line(depth+1, string.format("case %d: %s = %s; break;", hashes[i], node_type, set[i][1]))
        end
        add_line(depth+1, "default: abort(); return NULL;")
        add_line(depth, "}")
    end

    -- declaration of a node
    local extra_type = extra_node_type_name(n[1])
    local dt_name = n.dt
    if dt_name == nil then
        dt_name = "TB_TYPE_VOID"
    end

    local potentially_dynamic_count = false
    add_line(depth, string.format("size_t k%d_i = 0;", ids[n]))
    if uses_rest then
        add_line(depth, string.format("TB_Node* k%d = tb_alloc_node(f, %s, %s, %d + $REST_LEN, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type))
    else
        add_line(depth, string.format("TB_Node* k%d = tb_alloc_node(f, %s, %s, %d, sizeof(%s));", ids[n], node_type, dt_name, in_cnt, extra_type))
    end

    -- input edges
    for i=2,#n do
        local v = n[i]
        if v == "___" then
            -- just keep as NULL
            add_line(depth, string.format("k%d_i++;", ids[n]))
        elseif type(v) == "table" then
            add_line(depth, string.format("set_input(f, k%d, k%d, k%d_i++);", ids[n], ids[v], ids[n]))
        elseif mem_capture and v == mem_capture.name then
            -- copy all the extra data
            potentially_dynamic_count = true
            -- copy inputs
            add_line(depth, string.format("if (%s->type != %s) {", v, mach_prefix.."MEMORY"))
            add_line(depth+1, string.format("set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n]))
            add_line(depth, "} else {")
            add_line(depth+1, string.format("FOR_N(i, 0, %s->input_count) { set_input(f, k%d, %s->inputs[i], k%d_i++); }", v, ids[n], v, ids[n]))
            add_line(depth, "}")
        elseif v == "$REST" then
            add_line(depth, string.format("FOR_N(i, 0, $REST_LEN) { set_input(f, k%d, $REST[i], k%d_i++); }", ids[n], ids[n]))
        elseif type(v) == "string" and v:byte(1) == string.byte("$") then
            add_line(depth, string.format("set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n]))
        else
            add_line(depth, string.format("set_input(f, k%d, %s, k%d_i++);", ids[n], v, ids[n]))
        end
    end

    add_line(depth, string.format("%s* k%d_extra = TB_NODE_GET_EXTRA(k%d);", extra_type, ids[n], ids[n]))
    for k,v in pairs(n) do
        if type(k) == "string" and k ~= "dt" then
            -- replace all uses of captures with their real name
            add_line(depth, string.format("k%d_extra->%s = %s;", ids[n], k, v))
        elseif mem_capture and v == mem_capture.name then
            add_line(depth, string.format("memcpy(k%d_extra, %s->extra, sizeof("..extra_type.."));", ids[n], v))
        end
    end

    -- trim size
    if potentially_dynamic_count then
        add_line(depth, string.format("k%d->input_count = k%d_i;", ids[n], ids[n]))
    end

    -- apply GVN
    add_line(depth, string.format("k%d = tb_opt_gvn_node(f, k%d);", ids[n], ids[n]))
    add_line(depth, "")
end

node_basic_fields = {
  ["dt"] = true,
}

function find_captures(n, expr, shared, depth)
    if n.name then
        if n[1] == "x86_MEMORY" then
            if mem_capture then
                print("Can't capture 2 memory operands in one pattern?!?")
                os.exit(1)
            end

            mem_capture = n
        end

        if shared then
            add_line(depth, string.format("TB_Node* %s = %s;", n.name, expr))
        end
    end

    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            find_captures(v, expr.."->inputs["..(i-2).."]", shared, depth)
        elseif shared then
            if v == "$REST" then
                add_line(depth, string.format("size_t %s_LEN = %s->input_count - %d;", v, expr, i-2))
                add_line(depth, string.format("TB_Node** %s  = &%s->inputs[%d];", v, expr, i-2))
            elseif type(v) == "string" and v:byte(1) == string.byte("$") then
                add_line(depth, string.format("TB_Node* %s = %s->inputs[%d];", v, expr, i-2))
            end
        end
    end

    if not shared then
        local extra_type = extra_node_type_name(n[1])
        for k,v in pairs(n) do
            if type(k) == "string" and type(v) == "string" and k ~= "name" and v ~= "$REST" then
                if node_basic_fields[k] then
                    add_line(depth, string.format("TB_DataType %s = %s->%s;", v, expr, k))
                else
                    local t = node_extra_desc[extra_type]
                    if t then
                        t = t[k]
                    else
                        t = "uint64_t"
                    end

                    add_line(depth, string.format("%s %s = TB_NODE_GET_EXTRA_T(%s, %s)->%s;", t, v, expr, extra_type, k))
                end
            end
        end
    end

    return id_cnt
end

if true then
    for id,name in ipairs(ordered) do
        local set = partitions[name]
        -- if id ~= 12 then goto skip2 end

        add_line(0, string.format("static TB_Node* mach_dfa_accept_%d(Ctx* ctx, TB_Function* f, TB_Node* n) {", id))

        -- All the captures are the same across the sets
        mem_capture = nil
        find_captures(set[1][1], "n", true, 1)
        lines[#lines+1] = ""

        local hashes = {}
        local pat = {}
        for i=1,#set do
            pat[i] = set[i][1]
            hashes[i] = 0
        end

        -- Compute hashes based on information which differs between the
        -- nodes.
        lines[#lines + 1] = "    uint64_t hash = 0;"
        compute_pattern_hash(lines, "n", pat, hashes)
        lines[#lines + 1] = ""
        lines[#lines + 1] = "    // "..inspect(hashes)
        lines[#lines + 1] = "    switch (hash) {"

        -- group based on matching hash, these must be placed together
        local hash_groups = Partitions()
        for i=1,#set do
            hash_groups:put(hashes[i], i)
        end

        -- group based on shared replacement rules
        local cases = Partitions()
        for hash,list in hash_groups:iter() do
            local strs = {}
            for i=1,#list do
                local rule = set[list[i]]
                local rep_shape = rule[4]
                if type(rep_shape) ~= "string" then
                    rep_shape = get_pattern_shape(rep_shape)
                end
                add_if_new(strs, rep_shape)
            end

            local key = table.concat(strs, ",")
            for i=1,#list do
                cases:put(key, list[i])
            end
        end

        for shape,inner in cases:iter() do
            local keys = OrderedSet()
            add_line(2, string.format("// %s", shape))
            for j=1,#inner do
                local rule = set[inner[j]]

                local pat = get_pattern_pretty(rule[1])
                local rep = rule[4]
                if type(rep) ~= "string" then
                    rep = get_pattern_pretty(rep)
                end

                local hash = hashes[inner[j]]
                keys:put(hash)
            end

            for k in keys:iter() do
                add_line(2, string.format("case %d:", k))
            end

            local packs = Partitions()

            -- place all "where"-based rules above
            for j=1,#inner do
                local rule = set[inner[j]]
                if rule[2] then
                    packs:put(rule[2], inner[j])
                end
            end

            for j=1,#inner do
                local rule = set[inner[j]]
                if not rule[2] then
                    packs:put("true", inner[j])
                end
            end

            add_line(2, "{")
            for where,list in packs:iter() do
                add_line(3, "do {")

                mem_capture = nil
                find_captures(set[list[1]][1], "n", false, 4)

                if where ~= "true" then
                    add_line(4, string.format("if (!(%s)) { break; }", where))
                    lines[#lines + 1] = ""
                end

                local pat = {}
                local rep = {}
                local local_hashes = {}
                for j,i in ipairs(list) do
                    pat[j] = set[i][1]
                    rep[j] = set[i][4]
                    local_hashes[j] = hashes[i]
                end

                if type(rep[1]) == "string" then
                    add_line(4, "return "..rep[1]..";")
                else
                    -- Every replacement has the same shape, just different opcodes
                    local ids = {}
                    node_cnt = 0
                    write_node(ids, rep, local_hashes)

                    add_line(4, "return k"..ids[rep[1]]..";")
                end
                add_line(3, "} while (0);")
            end
            add_line(3, "return NULL;")
            add_line(2, "}")
            lines[#lines + 1] = ""
        end
        lines[#lines + 1] = "    }"
        add_line(1, "return NULL;")
        add_line(0, "}")
        lines[#lines+1] = ""

        ::skip2::
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

function get_active_limit(active)
    local limit = 0
    for i=1,#active do
        local len = #active[i] - 1
        local last = active[i][#active[i]]

        if last == "..." or last == "$REST" then
            len = len - 1
        end
        limit = math.max(limit, len)
    end
    return limit
end

function match_prio(type)
    if type == "ANY" then
        return 2
    elseif type == mach_prefix.."MEMORY" or type == mach_prefix.."COND" then
        return 1
    else
        return 0
    end
end

var_counter = 0
function gen_c_inner(depth, stack, can_bail, operands)
    local active = stack[5]
    assert(depth < 10)

    local limit = get_active_limit(active)
    local active_str = make_active_str(active)

    while true do
        assert(stack[4] >= 1)
        local pos = stack[3] + 2

        -- which types lead to which active sets
        local deltas = Partitions()
        for i=1,#active do
            local ty = get_pattern_at(active[i], pos)
            deltas:put(ty, active[i])
        end

        local leader = nil
        local bail_out = nil
        local cases = Partitions()
        for ty,list in deltas:iter() do
            local str = make_active_str(list)
            if cases:put_list(str, list) then
                if get_pattern_at(list[1], pos) ~= "ANY" then
                    leader = ty
                else
                    bail_out = ty
                end
            end
        end

        cases:sort(function(a, b)
            local aa = get_pattern_at(a[1], pos)
            local bb = get_pattern_at(b[1], pos)
            return match_prio(aa) < match_prio(bb)
        end)

        local in_str = get_in(stack)
        stack[3] = stack[3] + 1

        if false then
            print("Step", stack[4], "[", pos, "]", limit, leader, bail_out, in_str)

            for k,v in cases:iter() do
                for i=1,#v do
                    print("", k, get_pattern_at(v[i], pos), get_pattern_pretty(v[i]))
                end
                print()
            end
        end

        -- add_line(depth, "// WALK "..in_str..", "..make_active_str(active))
        local case_count = cases:count()
        if leader == "___" then
            add_line(depth, string.format("if (%s != NULL) { break; }", in_str))
        elseif case_count == 1 and bail_out then
            add_line(depth, "// MATCH ANY "..in_str)
        elseif case_count == 0 then
            add_line(depth, "break;")
            return active
        else
            -- Handle leader case
            if leader == "END" then
                goto skip
            end

            local new_name = string.format("n__%d", var_counter)
            add_line(depth, string.format("TB_Node* %s = %s;", new_name, in_str))
            var_counter = var_counter + 1

            local consumed = {} -- set of inputs consumed by non-ANY cases
            for str,v in cases:iter() do
                local cond = get_pattern_at(v[1], pos)
                if cond ~= "ANY" then
                    local preds = {}
                    local has = {}
                    local can_match_operand = false
                    for i=1,#v do
                        local cond = get_pattern_at(v[i], pos)
                        has[cond] = true

                        if cond == mach_prefix.."MEMORY" or cond == mach_prefix.."COND" then
                            add_if_new(preds, string.format("TRY_OPERAND(%s, %s)", new_name, cond))
                            can_match_operand = true
                        else
                            add_if_new(preds, string.format("%s->type == %s", new_name, cond))
                        end
                    end
                    local expr = table.concat(preds, " || ")

                    -- compute our active set and while we're doing that, prune the
                    -- active set from the final ANY case. the only way to reach the
                    -- ANY case is to bail so we can consider all the other paths dead
                    -- at that point.
                    local then_active = {}
                    for i=1,#active do
                        local v = active[i]
                        local at = get_pattern_at(active[i], pos)
                        if has[at] then
                            then_active[#then_active + 1] = active[i][pos]
                            consumed[at] = true
                        end
                    end

                    local next_stack = { stack, new_name, 0, stack[4]+1, then_active }
                    local new_operands = operands
                    if can_match_operand then
                        add_line(depth, string.format("do { if (k = NULL, %s) {", expr))

                        local new_name_2 = "n__"..var_counter
                        var_counter = var_counter + 1

                        next_stack[2] = new_name_2
                        add_line(depth+1, string.format("TB_Node* %s = k ? k : %s;", new_name_2, new_name))

                        new_operands = {}
                        for i=1,#operands do
                            new_operands[i] = operands[i]
                        end

                        -- this will be assigned once we reach the accept state
                        new_operands[#new_operands + 1] = {
                            string.format("set_input(f, %s, %s, %d);", stack[2], new_name_2, stack[3] - 1),
                            string.format("set_input(f, %s, %s, %d);", stack[2], new_name, stack[3] - 1)
                        }
                    else
                        add_line(depth, string.format("do { if (%s) {", expr))
                    end
                    gen_c_inner(depth+1, next_stack, true, new_operands)

                    add_line(depth, "} } while (0);")
                    lines[#lines + 1] = ""
                end
            end

            local any_active = {}
            for i=1,#active do
                local v = active[i]
                local at = get_pattern_at(active[i], pos)
                if not consumed[at] then
                    any_active[#any_active + 1] = active[i]
                end
            end

            active = any_active
        end

        ::skip::
        if stack and stack[3] > limit then
            -- Pop from stack, continue to process
            stack = clone_stack(stack[1])

            -- add_line(depth, "// POP "..stack[4])
            if stack[4] < 1 then
                break
            end

            local old_active = stack[5]
            local live = {}
            for i=1,#active do
                local id = pattern_ids[active[i]]
                live[id] = true
            end

            local new_active = {}
            for i=1,#old_active do
                local id = pattern_ids[old_active[i]]
                if live[id] then
                    -- print("KEEP LIVE", old_active[i])
                    new_active[#new_active + 1] = old_active[i]
     	       end
            end

            active = new_active
            limit = get_active_limit(active)
        end
    end

    -- input_count query on the non-variadic patterns
    local var_final = nil
    local non_var_final = nil

    for i=1,#active do
        local pat = active[i]
        local id = to_partition[pat]

        -- add_line(depth, string.format("// ACCEPT: %s", get_pattern_pretty(pat)))
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

    -- add_line(depth, string.format("// DONE %s %s %s", var_final, non_var_final, make_active_str(active)))
    -- add_line(depth, "// REPLACE OPERANDS "..#operands)

    for i=1,#operands do
        add_line(depth, operands[i][1])
    end

    local expr = "NULL"
    if var_final and var_final ~= "JOVER" then
        expr = "ACCEPT("..var_final..")"
    end

    if non_var_final and non_var_final ~= "JOVER" then
        expr = string.format("(%s->input_count == %d ? ACCEPT(%d) : %s)", stack[2], limit, non_var_final, expr)
    end

    if not can_bail then
        add_line(depth, string.format("return %s;", expr))
    elseif expr ~= "NULL" then
        add_line(depth, string.format("k = %s;", expr))
        add_line(depth, "if (k) { return k; }")
        if #operands > 0 then
            add_line(depth, "else {")
            for i=1,#operands do
                add_line(depth+1, operands[i][2])
            end
            add_line(depth, "}")
        end
    end

    return active
end

function gen_c(active, depth, stack, pos)
    local active_str = make_active_str(active)

    -- which types lead to which active sets
    local deltas = Partitions()
    for i=1,#active do
        local ty = get_pattern_at(active[i], pos)
        deltas:put(ty, active[i])
    end

    local cases = Partitions()
    for ty,list in deltas:iter() do
        local str = make_active_str(list)
        cases:put_list(str, list)
    end

    -- each path is stepping into the node so might as well do it up here
    local in_str = get_in(stack)

    add_line(depth, "switch ("..in_str.."->type) {")
    for str,v in cases:iter() do
        local keys = {}
        for i=1,#v do
            add_if_new(keys, v[i][1])
        end

        for i=1,#keys do
            add_line(depth+1, "case "..keys[i]..":")
        end
        add_line(depth+1, "{")
        local next_stack = { stack, stack[2], 0, stack[4]+1, v }
        gen_c_inner(depth+2, next_stack, false, {})
        add_line(depth+1, "}")
    end
    add_line(depth, "}")
    return stack
end

if true then
    lines[#lines + 1] = "static bool mach_is_subpat(int type) {"
    lines[#lines + 1] = "    switch (type) {"
    for k in subpat_map:iter() do
        lines[#lines + 1] = "        case "..k..": return true;"
    end
    lines[#lines + 1] = "        default: return false;"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""
    lines[#lines + 1] = "#define ACCEPT(x) mach_dfa_accept_ ## x(ctx, f, n)"
    lines[#lines + 1] = "#define TRY_OPERAND(in, expected) (k = node_isel_raw(ctx, f, in, depth+1), k && k->type == expected)"
    lines[#lines + 1] = "static TB_Node* node_isel_raw(Ctx* ctx, TB_Function* f, TB_Node* n, int depth) {"
    lines[#lines + 1] = "    if (depth == 1 && mach_is_subpat(n->type)) { return NULL; }"
    lines[#lines + 1] = "    TB_Node* k;"

    local all = {}
    for i=1,#all_patterns do
        all[i] = all_patterns[i][1]
    end

    local stack = { nil, "n", nil, 0, all }
    gen_c(all, 1, stack, 1)

    lines[#lines + 1] = "    return NULL;"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = "#undef ACCEPT"
    lines[#lines + 1] = "#undef TRY_OPERAND"
    lines[#lines + 1] = ""
end

local final_src = table.concat(lines, "\n")
-- print(final_src)

if true then
    local f = io.open(arg[2], "w")
    f:write(final_src)
    f:close()
end
