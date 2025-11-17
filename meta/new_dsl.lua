inspect = require "meta/inspect"
require "meta/prelude"

local unpack = unpack or table.unpack
local source = run_command("clang -E -xc "..arg[1].." "..arg[4])
local lex = lexer(source)

all_nodes = OrderedSet()
node_count = 1

all_nodes:put("NULL", { idx=0, flags={} })

lines = {}

function add_line(depth, line)
    lines[#lines + 1] = string.rep(" ", depth*4) .. line
end
add_line(0, "#pragma once")
add_line(0, "")

function decl_type(storage_class, n, name)
    local name = n[2]
    local kind = n[1]

    local start_line = ""
    if storage_class then
        start_line = storage_class.." "
    end
    start_line = start_line..kind

    if name ~= "___" then
        start_line = start_line.." "..name
    end
    lines[#lines + 1] = start_line.." {"

    if kind == "struct" or kind == "union" then
        for i=3,#n do
            -- function decl_type(storage_class, n, name)
            local field_name = n[i][1]
            local ty = n[i][2]

            local field_ty = ""
            while type(ty) == "table" and ty[1] == "ptr" do
                field_ty = "*"
                ty = ty[2]
            end

            if type(ty) == "table" then
                if ty[1] == "bits" then
                    lines[#lines + 1] = string.format("    %s%s %s : %d;", ty[2], field_ty, field_name, ty[3])
                else
                    decl_type(nil, ty, field_name)
                end
            else
                lines[#lines + 1] = string.format("    %s%s %s;", ty, field_ty, field_name)
            end
        end
    elseif kind == "enum" then
        local ord = 0
        for i=3,#n do
            local name = n[i]
            if type(n[i]) == "table" then
                ord  = n[i][2]
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

    local all_patterns = {}
    local node_groups = Partitions()
    local node2group = {}
    local reg_classes = {}
    local arch_features = nil
    local types = {}

    local start_id = node_count

    local i = 3
    while i <= #tree do
        local n = tree[i]
        if type(n) == "table" then
            if n[1] == "enum" or n[1] == "struct" or n[1] == "union" then
                table.insert(types, n)
            elseif n[1] == "reg_class" then
                reg_classes[#reg_classes + 1] = { n[2], math.max(1, #n - 2), n }
            elseif n[1] == "arch_features" then
                arch_features = n

                local set_tyname = "TB_" .. node_prefix:upper() .. "FeatureSet"
                local enum_tyname = "TB_" .. node_prefix:upper() .. "Feature"
                local entnamepfx = "TB_" .. node_prefix:upper() .. "FEATURE_"

                local function mangle(x)
                    return x:gsub("[-]","_"):gsub("[.]","_")
                end

                local implies_map = OrderedSet()
                local pfx_parser
                for i=2, #n do
                    local feat = n[i]
                    if feat[1] ~= "_prefix-parser" then
                        local feat_name = feat[1]
                        if not feat_name then
                            print(inspect(feat))
                            print("wrong syntax for arch_features")
                            os.exit(1)
                        end

                        if implies_map:get(feat_name) then
                            print(string.format("arch feature '%s' has already been defined", feat_name))
                            os.exit(1)
                        end

                        if feat[2] then
                            if feat[2] == "=>" then
                                implies_map:put(feat_name, { unpack(feat, 3) })
                            else
                                print(inspect(feat))
                                print("wrong syntax for arch_features")
                                os.exit(1)
                            end
                        else
                            implies_map:put(feat_name, true)
                        end
                    end
                end

                -- generate enum --
                add_line(0, "typedef enum {")
                for feat, i in implies_map:iter() do
                    add_line(1, string.format("%s%s,", entnamepfx, mangle(feat):upper()))
                end
                add_line(1, entnamepfx.."_MAX,")
                add_line(0, "} "..enum_tyname..";")
                add_line(0, "")

                -- generate featureset --
                local word_count = math.ceil(implies_map:count() / 64)
                add_line(0, "typedef struct {")
                add_line(0, "    uint64_t set["..word_count.."];")
                add_line(0, "} "..set_tyname..";")
                add_line(0, "")

                add_line(0, string.format("void %s__set(%s* out, %s ent);", set_tyname, set_tyname, enum_tyname))
                add_line(0, string.format("void %s__each_toggled(const %s* set, void (*consumer)(%s, void*), void* user_ptr);", set_tyname, set_tyname, enum_tyname))
                add_line(0, string.format("const char* %s__as_str(%s ent);", set_tyname, enum_tyname))
                add_line(0, string.format("int %s__from_str(%s* out, const char* str, size_t len);", set_tyname, set_tyname))
                add_line(0, string.format("int %s__parse(%s* out, const char* str);", set_tyname, set_tyname, enum_tyname))
                add_line(0, "")
            elseif n[1] == "node" then
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
            elseif n[1] == "op_group" then
                local name = n[2]
                for i=3,#n do
                    node_groups:put(name, n[i])
                    node2group[n[i]] = name
                end
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

    targets[namespace] = { all_patterns=all_patterns, node2group=node2group, node_groups=node_groups, node_prefix=node_prefix, types=types, reg_classes=reg_classes, arch_features=arch_features, range={start_id, node_count} }
end

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
-- print(inspect(attributes.entries))

--------------------------
-- Generate public file
--------------------------
add_line(0, "typedef uint16_t TB_NodeType;")
add_line(0, "typedef enum TB_NodeTypeEnum {")
for k,v in all_nodes:iter() do
    add_line(1, string.format("TB_%s%s = %d,", k, string.rep(" ", max_len - #k), v.idx))
end
add_line(1, "TB_NODE_TYPE_MAX,")
add_line(0, "} TB_NodeTypeEnum;")
add_line(0, "")

if true then
    local f = io.open(arg[3], "w")
    f:write(table.concat(lines, "\n"))
    f:close()
    lines = {}
end

--------------------------
-- Generate private file
--------------------------
add_line(0, "#pragma once")
add_line(0, "")
add_line(0, "extern const uint64_t tb_node_flags[];")
add_line(0, "extern const uint16_t* tb_node_subtypes[];")
add_line(0, "")
add_line(0, "typedef enum TB_NodeFlagsEnum {")
local i = 0
for k,v in attributes:iter() do
    add_line(1, string.format("NODE_%-20s = 1ull << %d,", k, i))
    i = i + 1
end
add_line(0, "} TB_NodeFlagsEnum;")
add_line(0, "")

-- Generate query functions
for k,v in attributes:iter() do
    add_line(0, string.format("static bool tb_node_is_%s(TB_Node* n) { return tb_node_flags[n->type] & NODE_%s; }", k:lower(), k))
end
add_line(0, "")

add_line(0, "#define NODE_ISA(s, t) tb_node_isa((s)->type, TB_ ## t)")

for k,v in pairs(targets) do
    if k ~= "___" then
        local r = v.range
        add_line(0, string.format("static bool tb_node_is_%s(TB_Node* n) { return n->type >= %d && n->type < %d; }", k, r[1], r[2]))
    end
end
add_line(0, "")

for k,v in pairs(targets) do
    if k ~= "___" then
        add_line(0, "#if 1 // namespace: "..k)
        for i=1,#v.types do
            if v.types[i][2] == "___" then
                decl_type(nil, v.types[i], "___")
            else
                decl_type("typedef", v.types[i], v.types[i][1])
            end
        end
        add_line(0, "#endif")
        add_line(0, "")
    end
end

add_line(0, "#ifndef TB_GEN_IMPL")
add_line(0, "TB_NodeFlagsEnum tb_node_get_flags(TB_Node* n);")
add_line(0, "const char* tb_node_get_name(TB_NodeTypeEnum n_type);")
add_line(0, "size_t tb_node_extra_bytes(TB_NodeTypeEnum n_type);")
add_line(0, "bool tb_node_isa(TB_NodeTypeEnum s, TB_NodeTypeEnum t);")
add_line(0, "#else")
add_line(0, "bool tb_node_isa(TB_NodeTypeEnum s, TB_NodeTypeEnum t) {")
add_line(1, "const uint16_t* ss = tb_node_subtypes[s];")
add_line(1, "if (ss == NULL) {")
add_line(2, "return t == s;")
add_line(1, "}")
add_line(1, "int t_depth = tb_node_subtypes[t] ? tb_node_subtypes[t][0] : 1;")
add_line(1, "return t_depth <= ss[0] ? t == ss[t_depth] : false;")
add_line(0, "}")
add_line(0, "")
add_line(0, "const uint16_t* tb_node_subtypes[TB_NODE_TYPE_MAX] = {")
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

add_line(0, "const uint64_t tb_node_flags[TB_NODE_TYPE_MAX] = {")
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
add_line(0, "")
add_line(0, "size_t tb_node_extra_bytes(TB_NodeTypeEnum n_type) {")
add_line(1, "switch (n_type) {")
for k,v in all_nodes:iter() do
    if v.extra then
        add_line(2, string.format("case TB_%s: return sizeof(%s);", k, v.extra))
    else
        add_line(2, string.format("case TB_%s: return 0;", k))
    end
end
add_line(2, "default: tb_todo(); return 0;")
add_line(1, "}")
add_line(0, "}")
add_line(0, "#endif")

if true then
    f = io.open(arg[2], "w")
    f:write(table.concat(lines, "\n"))
    f:close()
    lines = {}
end

local arch_isel, err = loadfile("meta/arch_isel.lua")
-- print(arch_isel, err)

for k,v in pairs(targets) do
    if k ~= "___" then
        all_patterns = v.all_patterns
        node2group   = v.node2group
        node_groups  = v.node_groups
        mach_prefix  = v.node_prefix
        reg_classes  = v.reg_classes
        types        = v.types
        arch_features= v.arch_features
        arch_isel()

        local path = string.format("tb/x64/x64_gen.h")
        f = io.open(path, "w")
        f:write(table.concat(lines, "\n"))
        f:close()
        lines = {}
    end
end
