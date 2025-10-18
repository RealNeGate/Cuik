local subpat_map = OrderedSet()
local operand_map = OrderedSet()

add_line(0, "#include \"../emitter.h\"")
add_line(0, "#include \"../tb_internal.h\"")
add_line(0, "#include <string.h>")
add_line(0, "")

add_line(0, "static void global_init() {}")
add_line(0, "")

local pub_lines = {}
if arch_features then
    local n = arch_features
    local set_tyname = "TB_" .. mach_prefix:upper() .. "FeatureSet"
    local enum_tyname = "TB_" .. mach_prefix:upper() .. "Feature"
    local entnamepfx = "TB_" .. mach_prefix:upper() .. "FEATURE_"

    local function mangle(x)
        return x:gsub("[-]","_"):gsub("[.]","_")
    end

    local implies_map = OrderedSet()
    local pfx_parser
    for i=2, #n do
        local feat = n[i]

        if feat[1] == "_prefix-parser" then
            pfx_parser = feat[2]
            if #feat ~= 2 then
                print("wrong syntax for _prefix-parser")
                os.exit(1)
            end
            goto continue
        end

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
    ::continue::
    end

    local function collect_implied_feats(feat_name, out)
        out = out or {}
        for _, x in ipairs(out) do
            if x == feat_name then
                return
            end
        end
        out[#out + 1] = feat_name

        local li = implies_map:get(feat_name)
        if not li then
            print(string.format("implied arch feature '%s' not defined", feat_name))
            os.exit(1)
        elseif type(li) == "table" then
            for _, other in ipairs(li) do
                collect_implied_feats(other, out)
            end
        end
        return out
    end

    local implied2idx = implies_map:transpose()

    -- generate enum --
    pub_lines[#pub_lines + 1] = "typedef enum {"
    for feat, i in implies_map:iter() do
        pub_lines[#pub_lines + 1] = "    " .. entnamepfx .. mangle(feat):upper() .. ","
    end
    pub_lines[#pub_lines + 1] = "    " .. entnamepfx .. "_MAX" .. ","
    pub_lines[#pub_lines + 1] = "} " .. enum_tyname .. ";"
    pub_lines[#pub_lines + 1] = ""

    -- generate featureset --
    local word_count = math.ceil(implies_map:count() / 64)
    pub_lines[#pub_lines + 1] = "typedef struct {"
    pub_lines[#pub_lines + 1] = "    uint64_t set["..word_count.."];"
    pub_lines[#pub_lines + 1] = "} " .. set_tyname .. ";"
    pub_lines[#pub_lines + 1] = ""

    -- generate featureset_set --
    local sig = "void "..set_tyname.."__set("..set_tyname.."* out, "..enum_tyname.." ent)"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    out->set[ent / 64] |= 1ull << (ent % 64);"
    lines[#lines + 1] = "    switch (ent) {"
    for feat, _ in implies_map:iter() do
        local implied_feats = collect_implied_feats(feat)
        if #implied_feats > 1 then
            local words = Partitions()
            for i=2,#implied_feats do
                local x = implied2idx[implied_feats[i]]
                words:put(math.floor(x / 64), string.format("(1ull << %dull)", x % 64))
            end

            lines[#lines + 1] = "    case "..entnamepfx..mangle(feat):upper()..":"
            for i,v in words:iter() do
                lines[#lines + 1] = string.format("        out->set[%d] |= %s;", i, table.concat(v, " | "))
            end
            lines[#lines + 1] = "        break;"
        end
    end
    lines[#lines + 1] = "    default:"
    lines[#lines + 1] = "        tb_unreachable();"
    lines[#lines + 1] = "        break;"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    -- generate featureset_get --
    -- don't worry, this gets optimized by llvm; at least before llvm 18...
    -- see https://github.com/llvm/llvm-project/issues/162832

    sig = "bool "..set_tyname.."__get("..set_tyname.." const* out, "..enum_tyname.." ent)"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    TB_ASSERT(ent < "..entnamepfx.."_MAX);"
    lines[#lines + 1] = "    return (out->set[ent / 64] >> (ent % 64)) & 1;"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    -- generate each_toggled --
    sig = "void "..set_tyname.."__each_toggled("..set_tyname.." const* set, void (*consumer)("..enum_tyname..", void*), void* userptr)"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    FOR_N(i, 0, "..entnamepfx.."_MAX) {"
    lines[#lines + 1] = "        if ((set->set[i / 64] >> (i % 64)) & 1) { consumer(i, userptr); }"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    -- generate enum as_str --
    sig = "char const* "..enum_tyname.."__as_str("..enum_tyname.." ent)"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    switch (ent) {"
    for feat, _ in implies_map:iter() do
        lines[#lines + 1] = "    case "..entnamepfx..mangle(feat):upper()..":"
        lines[#lines + 1] = "        return \""..mangle(feat).."\";"
    end
    lines[#lines + 1] = "    default:"
    lines[#lines + 1] = "        tb_unreachable();"
    lines[#lines + 1] = "        return 0;"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    -- generate enum from_str --
    sig = "int "..enum_tyname.."__from_str("..enum_tyname.."* out, char const* str, unsigned int str_len)"
    pub_lines[#pub_lines + 1] = "// returns 0 on success"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    if (!str || str_len == 0) return 1;"
    lines[#lines + 1] = "    unsigned char hash = (tolower(str[0]) ^ tolower(str[str_len - 1])) % 8;" -- 2 bytes because of rv Zbb, Zcond, ...
    lines[#lines + 1] = "    switch (hash) {"
    for i=0,7 do
        lines[#lines + 1] = "        case "..i..":"
        for feat, _ in implies_map:iter() do
            local hash = math.fmod(xor8(feat:byte(1), feat:byte(#feat)), 8)
            if hash == i then
                lines[#lines + 1] = "        if (str_len == "..#feat.." && !tb_string_case_cmp(str, \""..feat.."\", "..#feat..")) {"
                lines[#lines + 1] = "            *out = "..entnamepfx..mangle(feat):upper()..";"
                lines[#lines + 1] = "            return 0;"
                lines[#lines + 1] = "        }"
            end
        end
        lines[#lines + 1] = "        break;"
    end
    lines[#lines + 1] = "        default:"
    lines[#lines + 1] = "            tb_unreachable();"
    lines[#lines + 1] = "            return 1;"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "    return 1;"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    -- generate parser --
    sig = "int "..set_tyname.."__parse("..set_tyname.."* out, char const* str)"
    pub_lines[#pub_lines + 1] = "// returns 0 on success"
    pub_lines[#pub_lines + 1] = sig..";"
    lines[#lines + 1] = sig.." {"
    lines[#lines + 1] = "    memset(out, 0, sizeof("..set_tyname.."));"
    lines[#lines + 1] = "    if (!str) return 0;"
    if pfx_parser then
        lines[#lines + 1] = "    str = "..pfx_parser.."(str,  out);"
    end
    lines[#lines + 1] = "    for (; str && *str; ) {"
    lines[#lines + 1] = "        char const* next = strchr(str, ',');"
    lines[#lines + 1] = "        unsigned int len = next ? next - str : strlen(str);"
    lines[#lines + 1] = "        "..enum_tyname.." x;"
    lines[#lines + 1] = "        if ("..enum_tyname.."__from_str(&x, str, len)) return 1;"
    lines[#lines + 1] = "        "..set_tyname.."__set(out, x);"
    lines[#lines + 1] = "        if (next) {str = next + 1;} else {break;}"
    lines[#lines + 1] = "    }"
    lines[#lines + 1] = "    return 0;"
    lines[#lines + 1] = "}"
    lines[#lines + 1] = ""

    pub_lines[#pub_lines + 1] = ""
end
print(inspect(pub_lines))

for i=1,#reg_classes do
    local n = reg_classes[i][3]
    if #n > 3 then
        lines[#lines + 1] = "typedef enum "..n[2].." {"
        for i=3,#n do
            lines[#lines + 1] = "    "..n[i]..","
        end
        lines[#lines + 1] = "    "..n[2].."_NONE = -1,"
        lines[#lines + 1] = "} "..n[2]..";"
        lines[#lines + 1] = ""
    end
end

add_line(0, "enum {")
for i=1,#reg_classes do
    add_line(1, string.format("REG_CLASS_%s = %d,", reg_classes[i][1], i))
end
add_line(0, "    REG_CLASS_COUNT,")
add_line(0, "};")
add_line(0, "")
add_line(0, "enum {")
add_line(0, "    BUNDLE_INST_MAX = 1,")
add_line(0, "};")
add_line(0, "")
add_line(0, "#include \"../codegen_impl.h\"")
add_line(0, "")
add_line(0, "static void mach_dsl_init(Ctx* restrict ctx, TB_ABI abi) {")
for i=1,#reg_classes do
    add_line(1, string.format("ctx->num_regs[REG_CLASS_%s] = %d;", reg_classes[i][1], reg_classes[i][2]))
end
add_line(0, "}")

--------------------------
-- Generate ISel patterns
--------------------------
function get_pattern_inner(n, shape)
    local str = "("
    if n.name then
        str = str..n.name..": "
    end

    if shape then
        if node2group[n[1]] then
            str = str..node2group[n[1]]
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

function active_str(set)
    local strs = {}
    for i=1,#set do
        strs[#strs + 1] = set[i]
    end
    return table.concat(strs, ",")
end

function make_active_str(list)
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

function make_active_str_from_indices(list, partition_count)
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

function match_prio(type)
    if type == "ANY" then
        return 2
    elseif node_groups.entries[type] then
        return 1
    else
        return 0
    end
end

function get_active_limit(active)
    local limit = 0
    for i=1,#active do
        local len  = #active[i] - 1
        local last = active[i][#active[i]]

        if last == "..." or last == "$REST" then
            len = len - 1
        end
        limit = math.max(limit, len)
    end
    return limit
end

var_counter = 0
function gen_c_inner(depth, stack, can_bail)
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

        if false then -- leader == "END" then
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
            add_line(depth, string.format("DFA_LOG(depth+%d, %s, \"Step\");", stack[4], in_str))
            add_line(depth, string.format("if (%s != NULL) { break; }", in_str))
        elseif case_count == 1 and bail_out then
            if stack[3]-1 < limit then
                add_line(depth, string.format("DFA_LOG(depth+%d, %s, \"Step\");", stack[4], in_str))
                add_line(depth, "// MATCH ANY "..in_str)
            end
        elseif case_count == 0 then
            add_line(depth, "break;")
            return active
        else
            -- Handle leader case
            if leader == "END" then
                goto skip
            end

            add_line(depth, string.format("DFA_LOG(depth+%d, %s, \"Step\");", stack[4], in_str))

            local new_name = string.format("n__%d", var_counter)
            add_line(depth, string.format("TB_Node* %s = %s;", new_name, in_str))
            var_counter = var_counter + 1

            local consumed = {} -- set of inputs consumed by non-ANY cases
            for str,v in cases:iter() do
                local cond = get_pattern_at(v[1], pos)
                if cond ~= "ANY" then
                    local match_op_group = get_pattern_at(v[1], pos)
                    if not node_groups.entries[match_op_group] then
                        match_op_group = nil
                    end

                    local old_name = nil
                    local has = {}
                    local log_expr = {}
                    local expr = ""
                    if match_op_group then
                        old_name = "n__"..var_counter
                        var_counter = var_counter + 1

                        expr = string.format("node_isel_op_group(%s_final) == RULE_%s", new_name, match_op_group)
                        add_if_new(log_expr, match_op_group)

                        for i=1,#v do
                            local cond = get_pattern_at(v[i], pos)
                            if match_op_group == cond or match_op_group == node_groups.entries[cond] then
                                has[cond] = true
                            end
                        end
                    else
                        local preds = {}
                        for i=1,#v do
                            local cond = get_pattern_at(v[i], pos)
                            has[cond] = true
                            add_if_new(preds, string.format("%s->type == TB_%s", new_name, cond))
                            add_if_new(log_expr, cond)
                        end
                        expr = new_name.." && "..table.concat(preds, " || ")
                    end

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

                    local next_stack   = { stack, new_name, 0, stack[4]+1, then_active }

                    add_line(depth, string.format("DFA_LOG2(depth+%d, \"TRY '%s'\");", stack[4]+1, table.concat(log_expr, ",")))
                    if match_op_group then
                        add_line(depth, string.format("int %s_mark = ctx->dsl.top;", new_name))
                        add_line(depth, string.format("int %s_final = !set_get(shared, %s->gvn) ? node_isel_raw(ctx, f, shared, %s, depth+%d) : 0;", new_name, new_name, new_name, stack[4]))
                        add_line(depth, string.format("do { if (%s) {", expr))
                        add_line(depth+1, string.format("DFA_LOG(depth+%d, %s, \"Accept as %s\");", stack[4]+1, new_name, match_op_group))
                        add_line(depth+1, string.format("ctx->dsl.accept[ctx->dsl.top++] = (TB_SubMatch){ %s_final, %s, %d };", new_name, stack[2], stack[3]-1))

                        next_stack[2] = new_name

                        -- this will be assigned once we reach the accept state
                        --new_operands[#new_operands + 1] = {
                        --    string.format("set_input(f, %s, %s, %d);", stack[2], new_name_2, stack[3] - 1),
                        --    string.format("set_input(f, %s, %s, %d);", stack[2], new_name, stack[3] - 1)
                        --}
                        gen_c_inner(depth+1, next_stack, true)
                        add_line(depth, string.format("} else { DFA_LOG(depth+%d, %s, \"Reject\"); } } while (0);", stack[4]+1, new_name))
                        add_line(depth, string.format("ctx->dsl.top = %s_mark;", new_name))
                    else
                        add_line(depth, string.format("do { if (%s) {", expr))
                        gen_c_inner(depth+1, next_stack, true)
                        add_line(depth, "} } while (0);")
                    end
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
                -- add_line(depth, "// POP "..stack[4])
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

    local expr = "0"
    if var_final and var_final ~= "JOVER" then
        expr = "RULE("..var_final..")"
    end

    if non_var_final and non_var_final ~= "JOVER" then
        expr = string.format("(%s->input_count == %d ? RULE(%d) : %s)", stack[2], limit, non_var_final, expr)
    end

    if not can_bail then
        add_line(depth, string.format("return %s;", expr))
    elseif expr ~= "0" then
        add_line(depth, string.format("k = %s;", expr))
        add_line(depth, "if (k) { return k; }")
    end

    return active
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

node_basic_fields = {
  ["dt"] = true,
}

function extra_node_type_name(t)
    if node_groups:get(t) then
        t = node_groups:get(t)[1]
    end
    return all_nodes:get(t).extra
end

function find_captures(lines, n, expr, shared, indent)
    if n.name then
        if n[1] == "MEMORY" then
            mem_capture = n
        end

        if shared then
            lines[#lines + 1] = string.format("%sTB_Node* %s = %s;", indent, n.name, expr)
        end
    end

    for i=2,#n do
        local v = n[i]
        if type(v) == "table" then
            find_captures(lines, v, expr.."->inputs["..(i-2).."]", shared, indent)
        elseif shared then
            if v == "$REST" then
                lines[#lines + 1] = string.format("%ssize_t %s_LEN = %s->input_count - %d;", indent, v, expr, i-2)
                lines[#lines + 1] = string.format("%sTB_Node** %s  = &%s->inputs[%d];", indent, v, expr, i-2)
            elseif type(v) == "string" and v:byte(1) == string.byte("$") then
                lines[#lines + 1] = string.format("%sTB_Node* %s = %s->inputs[%d];", indent, v, expr, i-2)
            end
        end
    end

    if not shared then
        local extra_type = extra_node_type_name(n[1])
        for k,v in pairs(n) do
            if type(k) == "string" and type(v) == "string" and k ~= "name" and v ~= "$REST" then
                if node_basic_fields[k] then
                    lines[#lines + 1] = string.format("%sTB_DataType %s = %s->%s;", indent, v, expr, k)
                else
                    local t = node_extra_desc[extra_type]
                    if t then
                        t = t[k]
                    else
                        t = "uint64_t"
                    end

                    lines[#lines + 1] = string.format("%s%s %s = TB_NODE_GET_EXTRA_T(%s, %s)->%s;", indent, t, v, expr, extra_type, k)
                end
            end
        end
    end

    return id_cnt
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
            local t = all_nodes:get(set[i][1]).idx
            hashes[i] = hashes[i]*31 + t
        end

        strs[#strs + 1] = string.format("    hash = hash*31 + %s->type;", expr)
    end
end

-- Split partitions by similar pattern shapes
local partitions = Partitions()
local active = {}
for i,pat in ipairs(all_patterns) do
    local k = get_pattern_shape(pat[1])
    partitions:put(k, pat)
end

to_partition = {}
pattern_ids = {}
partition_count = partitions:count()

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
local i = 0
for name,set in partitions:iter() do
    i = i + 1
    for j=1,#set do
        walk_into(set[j][1], i, pattern_count)
        pattern_count = pattern_count + 1
    end
    -- print(i, name, #set)
end

local rule_lines = {}
local accept_count = 0

function add_rline(line)
    rule_lines[#rule_lines + 1] = line
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
    local dt_name = n.dt
    local node_type = n[1]
    for i=2,#set do
        if node_type ~= set[i][1] or dt_name ~= set[i].dt then
            node_type = nil
            break
        end
    end

    local depth = 1
    if node_type then
        node_type = "TB_"..n[1]
    else
        node_type = "k"..ids[n].."_type"
        dt_name = "k"..ids[n].."_dt"

        add_line(depth, string.format("int %s = 0;", node_type))
        add_line(depth, string.format("TB_DataType %s = TB_TYPE_VOID;", dt_name))
        add_line(depth, "switch (hash) {")
        for i=1,#set do
            if set[i].dt then
                add_line(depth+1, string.format("case %d: %s = TB_%s, %s = %s; break;", hashes[i], node_type, set[i][1], dt_name, set[i].dt))
            else
                add_line(depth+1, string.format("case %d: %s = TB_%s; break;", hashes[i], node_type, set[i][1]))
            end
        end
        add_line(depth+1, "default: abort(); return NULL;")
        add_line(depth, "}")
    end

    -- declaration of a node
    local extra_type = extra_node_type_name(n[1])
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
    local use_mem_capture = false
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
            use_mem_capture = true
            -- copy inputs
            add_line(depth, string.format("if (%s->type != TB_%sMEMORY) {", v, mach_prefix))
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

    local extras = Partitions()
    for i=1,#set do
        local line = {}
        local sorted = {}
        for k,v in pairs(set[i]) do
            if type(k) == "string" and k ~= "dt" then
                sorted[#sorted + 1] = k
            elseif mem_capture and v == mem_capture.name then
                sorted[#sorted + 1] = k
            end
        end

        table.sort(sorted, function(a, b)
            -- put numbers on the left
            if type(a) == "number" then return true  end
            if type(b) == "number" then return false end
            return a < b end
        )

        for j=1,#sorted do
            local k = sorted[j]
            local v = set[i][k]
            if type(k) == "string" and k ~= "dt" then
                -- replace all uses of captures with their real name
                line[#line + 1] = string.format("k%d_extra->%s = %s", ids[n], k, v)
            end
        end

        if #line > 0 then
            line = table.concat(line, ", ")..";"
            -- print(hashes[i], line)
            extras:put(line, hashes[i])
        end
    end

    if use_mem_capture and mem_capture then
        add_line(depth,   string.format("if (%s->type == TB_%sMEMORY) {", mem_capture.name, mach_prefix))
        add_line(depth+1, string.format("memcpy(k%d_extra, %s->extra, sizeof(%s));", ids[n], mem_capture.name, extra_type))
        add_line(depth,   "}")
    end

    if extras:count() == 1 then
        add_line(depth, extras:at(1))
    else
        add_line(depth, "switch (hash) {")
        for line,list in extras:iter() do
            local cases = ""
            for i=1,#list do
                cases = cases..string.format("case %d: ", list[i])
            end
            add_line(depth+1, string.format("%s%s break;", cases, line))
        end
        add_line(depth, "}")
    end

    -- trim size
    if potentially_dynamic_count then
        add_line(depth, string.format("k%d->input_count = k%d_i;", ids[n], ids[n]))
    end

    -- apply GVN
    add_line(depth, string.format("k%d = tb_opt_gvn_node(f, k%d);", ids[n], ids[n]))
    add_line(depth, "")
end

do
    rules_in_groups = {}

    local id = 0
    for name,set in partitions:iter() do
        id = id + 1
        add_rline(string.format("static MatchRuleID mach_dfa_rule_%d(Ctx* ctx, TB_Function* f, TB_Node* n, int depth) {", id))

        -- All the captures are the same across the sets
        find_captures(rule_lines, set[1][1], "n", true, "    ")
        lines[#lines+1] = ""

        local hashes = {}
        local pat = {}
        for i=1,#set do
            pat[i] = set[i][1]
            hashes[i] = 0
        end

        -- Compute hashes based on information which differs between the
        -- nodes.
        add_rline("    uint64_t hash = 0;")
        local hash_start = #rule_lines
        compute_pattern_hash(rule_lines, "n", pat, hashes)
        local hash_end = #rule_lines
        rule_lines[#rule_lines + 1] = ""

        add_rline("    // "..inspect(hashes))
        if #hashes > 1 then
            add_rline("    switch (hash) {")
        end

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
                if rule[3] then
                    rep_shape = rep_shape.."_SUB"
                end
                add_if_new(strs, rep_shape)
            end

            local key = table.concat(strs, ",")
            for i=1,#list do
                cases:put(key, list[i])
            end
        end

        if #hashes > 1 then
            add_rline("        default:")
            add_rline("        return 0;")
        end
        for shape,inner in cases:iter() do
            local keys = OrderedSet()
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

            local indent = "    "
            if #hashes > 1 then
                indent = "            "

                local cases = {}
                for k in keys:iter() do
                    cases[#cases + 1] = string.format("case %d: ", k)
                end
                add_rline(string.format("        // %s", shape))
                add_rline(string.format("        %s{", table.concat(cases)))
            end

            local packs = Partitions()

            -- place all "where"-based rules above
            for j=1,#inner do
                local rule = set[inner[j]]
                if rule[2] then
                    packs:put(rule[2], inner[j])
                end
            end

            local has_default = false
            for j=1,#inner do
                local rule = set[inner[j]]
                if not rule[2] then
                    packs:put("true", inner[j])

                    local is_subpat = set[inner[j]][3]
                    if not is_subpat then has_default = true end
                end
            end

            for where,list in packs:iter() do
                local is_subpat = set[list[1]][3]
                if is_subpat then
                    if where == "true" then
                        where = "depth == 0"
                    else
                        where = "depth == 0 && "..where
                    end
                end

                accept_count = accept_count + 1
                local accept_id = accept_count

                local rep = set[list[1]][4]
                if type(rep) == "table" and node2group[rep[1]] then
                    table.insert(rules_in_groups, { accept_id, node2group[rep[1]] })
                end

                add_rline(string.format("%s{", indent))
                find_captures(rule_lines, set[list[1]][1], "n", false, indent.."    ")

                if where == "true" then
                    add_rline(string.format("%s    return %d;", indent, accept_id))
                else
                    add_rline(string.format("%s    if (%s) { return %d; }", indent, where, accept_id))
                end
                add_rline(string.format("%s}", indent))

                local pat = {}
                local rep = {}
                local local_hashes = {}
                for j,i in ipairs(list) do
                    pat[j] = set[i][1]
                    rep[j] = set[i][4]
                    local_hashes[j] = hashes[i]
                end

                add_line(0, string.format("static TB_Node* mach_dfa_accept_%d(Ctx* ctx, TB_Function* f, TB_Node* n) {", accept_id))

                for j=hash_start,hash_end do
                    add_line(0, rule_lines[j])
                end
                add_line(0, "")
                find_captures(lines, set[list[1]][1], "n", true, "    ")
                find_captures(lines, set[list[1]][1], "n", false, "    ")
                add_line(0, "")

                if type(rep[1]) == "string" then
                    add_line(1, "return "..rep[1]..";")
                else
                    -- Every replacement has the same shape, just different opcodes
                    local ids = {}
                    node_cnt = 0
                    write_node(ids, rep, local_hashes)

                    add_line(1, "return k"..ids[rep[1]]..";")
                end
                add_line(0, "}")
            end

            if not has_default then
                add_rline(indent.."return 0;")
            end

            if #hashes > 1 then
                add_rline("        }")
            end
        end
        if #hashes > 1 then
            add_rline("    }")
        end
        add_rline("}")
        add_rline("")
        ::skip2::
    end

    for i=1,#rule_lines do
        lines[#lines + 1] = rule_lines[i]
    end
end

add_line(0, "static bool mach_is_subpat(int type) {")
add_line(1, "switch (type) {")
for k in subpat_map:iter() do
    add_line(2, "case "..k..": return true;")
end
add_line(2, "default: return false;")
add_line(1, "}")
add_line(0, "}")
add_line(0, "")
add_line(0, "static int mach_is_operand(int type) {")
add_line(1, "switch (type) {")
for k in operand_map:iter() do
    add_line(2, "case "..k..": return "..operand_map.entries[k]..";")
end
add_line(2, "default: return 0;")
add_line(1, "}")
add_line(0, "}")
add_line(0, "")
add_line(0, "enum {")
add_line(1, "RULE_NONE,")
for k,v in node_groups:iter() do
    add_line(1, "RULE_"..k..",")
end
add_line(0, "};")
add_line(0, "")

add_line(0, "static void mach_indent(int depth) {")
add_line(1, "while (depth--) { printf(\"  \"); }")
add_line(0, "}")
add_line(0, "")

add_line(0, "#if TB_OPTDEBUG_ISEL")
add_line(0, "#define DFA_LOG(depth, n, fmt, ...) (mach_indent(depth), printf(fmt, __VA_ARGS__), printf(\": \"), tb_print_dumb_node(NULL, n), printf(\"\\n\"))")
add_line(0, "#define DFA_LOG2(depth, fmt, ...) (mach_indent(depth), printf(fmt, __VA_ARGS__), printf(\"\\n\"))")
add_line(0, "#else")
add_line(0, "#define DFA_LOG(depth, n, fmt, ...)")
add_line(0, "#define DFA_LOG2(depth, fmt, ...)")
add_line(0, "#endif")
add_line(0, "")

add_line(0, "static int node_isel_op_group(MatchRuleID id) {")
add_line(1, "switch (id) {")
for i=1,#rules_in_groups do
    add_line(2, string.format("case %d: return RULE_%s;", rules_in_groups[i][1], rules_in_groups[i][2]))
end
add_line(2, "default: return RULE_NONE;")
add_line(1, "}")
add_line(0, "}")
add_line(0, "")

local depth = 1
add_line(0, "#define RULE(x) mach_dfa_rule_ ## x(ctx, f, n, depth)")
add_line(0, "static MatchRuleID node_isel_raw(Ctx* ctx, TB_Function* f, Set* shared, TB_Node* n, int depth) {")
add_line(0, "    MatchRuleID k;")
do
    local active = {}
    for i=1,#all_patterns do
        active[i] = all_patterns[i][1]
    end

    local stack = { nil, "n", nil, 0, all }
    local active_str = make_active_str(active, partitions:count())

    -- which types lead to which active sets
    local pos = 1
    local deltas = Partitions()
    for i=1,#active do
        local ty = get_pattern_at(active[i], pos)
        deltas:put(ty, active[i])
    end

    local cases = Partitions()
    for ty,list in deltas:iter() do
        local str = make_active_str(list, partitions:count())
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
            add_line(depth+1, "case TB_"..keys[i]..":")
        end
        add_line(depth+1, "{")
        local next_stack = { stack, stack[2], 0, stack[4]+1, v }
        gen_c_inner(depth+2, next_stack, false)
        add_line(depth+1, "}")
    end
    add_line(depth, "}")
    add_line(depth, "return 0;")
end
add_line(0, "}")
add_line(0, "")

add_line(0, "static MatchRule match_rules[] = {")
add_line(0, "    NULL,")
for i=1,accept_count do
    add_line(0, string.format("    mach_dfa_accept_%d,", i))
end
add_line(0, "};")
add_line(0, "#undef ACCEPT")
add_line(0, "")
