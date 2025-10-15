inspect = require "meta/inspect"
require "meta/prelude"

local unpack = unpack or table.unpack
local source = run_command("clang -E -xc "..arg[1].." "..arg[4])
local lex = lexer(source)

local all_nodes = OrderedSet()
local node_count = 1

all_nodes:put("NULL", { idx=0, flags={} })

local lines = {}
function add_line(depth, line)
    lines[#lines + 1] = string.rep(" ", depth*4) .. line
end

local targets = {}
local attributes = Partitions()
while true do
    t = lex()
    if not t then
        break
    elseif t ~= "(" then
        print("fuck but in parsing")
        os.exit(1)
    end

    local tree = parse_node(lex)
    if tree[1] ~= "namespace" then
        print("expected (namespace SOME_ARCH_NAME ...)")
        os.exit(1)
    end

    local namespace = tree[2]
    local node_prefix
    if namespace == "___" then
        node_prefix = ""
    else
        node_prefix = namespace.."_"
    end

    print(inspect(tree))

    local all_patterns = {}
    local i = 3
    while i <= #tree do
        local n = tree[i]
        if type(n) == "table" then
            if n[1] == "node" then
                local name = node_prefix..n[2]
                local props = { idx=node_count, parent=n.parent, extra=n.extra, flags={} }

                for i=3,#n do
                    local v = n[i]
                    if type(v) == "string" then
                        props.flags[v] = true
                    end
                end

                all_nodes:put(name, props)
                node_count = node_count + 1
            elseif n[1] == "reg_class" then
            end
        elseif n == "pat" then
            local pattern = tree[i+1]
            i = i + 2

            local where = nil
            while tree[i] == "where" or tree[i] == "require" do
                local expr = tree[i+1]
                if tree[i] == "require" then
                    local feats = {}
                    for s in string.gmatch(expr, "([^,]+)") do
                        feats[#feats + 1] = string.format("TB_%sFeatureSet__get(&f->features.%s, TB_%sFEATURE_%s)", node_prefix:upper(), node_prefix:sub(1, -2), node_prefix:upper(), s:upper())
                    end
                    expr = table.concat(feats, " && ")
                end

                if where then
                    where = where.." && "..expr
                else
                    where = expr
                end
                i = i + 2
            end

            if tree[i] ~= "=>" then
                print("fuck but in parsing 2")
                os.exit(1)
            end
            i = i + 1

            local rep = tree[i]
            table.insert(all_patterns, { pattern, where, is_subpat, rep })
        end
        i = i + 1
    end

    targets[namespace] = { all_patterns=all_patterns }
end
print(inspect(targets))

-- Apply subtyping
local max_len = 0
for k,v in all_nodes:iter() do
    max_len = math.max(max_len, #k)

    local curr = all_nodes:get(v.parent)
    while curr do
        for f,_ in pairs(curr.flags) do
            v.flags[f] = true
        end
        curr = all_nodes:get(curr.parent)
    end

    -- flags
    for f,_ in pairs(v.flags) do
        attributes:put(f, k)
    end
end
print(inspect(attributes.entries))

add_line(0, "#pragma once")
add_line(0, "typedef uint16_t TB_NodeType;")
add_line(0, "typedef enum TB_NodeTypeEnum {")
for k,v in all_nodes:iter() do
    add_line(1, string.format("TB_%s%s = %d,", k, string.rep(" ", max_len - #k), v.idx))
end
add_line(1, "TB_NODE_TYPE_MAX,")
add_line(0, "} TB_NodeTypeEnum;")
add_line(0, "")
add_line(0, "static const uint16_t* tb_node_subtypes[] = {")
for k,v in all_nodes:iter() do
    local limit = 0
    local curr  = k
    while curr do
        limit = limit + 1
        curr  = all_nodes:get(curr).parent
    end

    curr = k
    if limit > 1 then
        -- construct in reverse
        local ids = {}
        local depth = limit
        while curr do
            ids[depth] = all_nodes:get(curr).idx
            curr  = all_nodes:get(curr).parent
            depth = depth - 1
        end
        ids = table.concat(ids, ", ")
        add_line(1, string.format("[TB_%s]%s = (uint16_t[]){ %d, %s },", k, string.rep(" ", max_len - #k), limit, ids))
    end
end
add_line(0, "};")
add_line(0, "")

if true then
    local f = io.open(arg[3], "w")
    f:write(table.concat(lines, "\n"))
    f:close()
    lines = {}
end

add_line(0, "#pragma once")
add_line(0, "typedef enum TB_NodeFlagsEnum {")
local i = 0
for k,v in attributes:iter() do
    add_line(1, string.format("NODE_%-20s = 1ull << %d,", k, i))
    i = i + 1
end
add_line(0, "} TB_NodeFlagsEnum;")
add_line(0, "")

add_line(0, "static const uint64_t tb_node_flags[] = {")
for k,v in all_nodes:iter() do
    local list = {}
    for f,_ in pairs(v.flags) do
        table.insert(list, "NODE_"..f)
    end

    if #list > 0 then
        table.sort(list)

        list = table.concat(list, "|")
        add_line(1, string.format("[TB_%s]%s = %s,", k, string.rep(" ", max_len - #k), list))
    end
end
add_line(0, "};")
add_line(0, "")

-- Generate query functions
for k,v in attributes:iter() do
    add_line(0, string.format("static bool tb_node_is_%s(TB_Node* n) { return tb_node_flags[n->type] & NODE_%s; }", k:lower(), k))
end
add_line(0, "")

add_line(0, "#define NODE_ISA(s, t) tb_node_isa((s)->type, TB_ ## t)")
add_line(0, "static bool tb_node_isa(TB_NodeTypeEnum s, TB_NodeTypeEnum t) {")
add_line(1, "const uint16_t* ss = tb_node_subtypes[s];")
add_line(1, "if (ss == NULL) {")
add_line(2, "return t == s;")
add_line(1, "}")
add_line(1, "int t_depth = tb_node_subtypes[t] ? tb_node_subtypes[t][0] : 1;")
add_line(1, "return t_depth <= ss[0] ? t == ss[t_depth] : false;")
add_line(0, "}")
add_line(0, "")

add_line(0, "#ifndef TB_GEN_IMPL")
add_line(0, "TB_NodeFlagsEnum tb_node_get_flags(TB_Node* n);")
add_line(0, "const char* tb_node_get_name(TB_NodeTypeEnum n_type);")
add_line(0, "#else")
add_line(0, "TB_NodeFlagsEnum tb_node_get_flags(TB_Node* n) {")
add_line(1, "TB_ASSERT(n->type < TB_NODE_TYPE_MAX);")
add_line(1, "return tb_node_flags[n->type];")
add_line(0, "}")
add_line(0, "")
add_line(0, "const char* tb_node_get_name(TB_NodeTypeEnum n_type) {")
add_line(1, "switch (n_type) {")
for k,v in all_nodes:iter() do
    add_line(2, string.format("case TB_%s: return \"%s\";", k, k:lower()))
end
add_line(2, "default: tb_todo(); return NULL;")
add_line(1, "}")
add_line(0, "}")
add_line(0, "#endif")

-- print(table.concat(lines, "\n"))
f = io.open(arg[2], "w")
f:write(table.concat(lines, "\n"))
f:close()
