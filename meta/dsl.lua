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
        if class == "num" then
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
        (TB_LOAD $ctrl $mem (TB_PTR_OFFSET ___ $base $idx)) => (x86_mov $ctrl)
    ]]

local dfa = {}
local head = 0
local depth = 0
local state_count = 0
local capture_count = 0

local accept = {}
local lex = lexer(source)

local function parse_replacement()
    local t = lex()
    if t ~= "=>" then
        print("fuck but in replacement")
    end

    local strs = {}
    local node_cnt = 0

    t = lex()
    if t == "(" then

        local node_type = lex()

        -- declaration of a node
        local dt_type = "TB_TYPE_VOID"
        local extra_type = "X86MemOp"

        strs[#strs + 1] = string.format("TB_Node* k%d = tb_alloc_node(f, %s, TB_TYPE_PTR, 1, sizeof(%s));", node_cnt, node_type, dt_name, extra_type);
        node_cnt = node_cnt + 1
    end

    return strs
end

while true do
    local t = lex()
    if t == nil then
        break
    elseif t == "(" then -- push if matches
        local type = lex().."+1"
        depth = depth + 1

        -- print("Match: ", type)
        if dfa[type] == nil then
            dfa[type] = {}
        end

        dfa[type][head] = "R_PUSH("..state_count..")"
        head = state_count
        state_count = state_count + 1
    elseif t == ")" then -- pop and accept if we've exhausted the inputs
        dfa["TB_NULL+1"][head] = "R_POP("..head..")"

        depth = depth - 1
        if depth == 0 then
            accept[head] = parse_replacement()
            head = 0
            capture_count = 0
        else
            head = state_count
            state_count = state_count + 1
        end
    elseif t == "___" then
        if dfa["TB_NULL+1"] == nil then
            dfa["TB_NULL+1"] = {}
        end

        dfa["TB_NULL+1"][head] = state_count
        head = state_count
        state_count = state_count + 1
    elseif t:byte(1) == string.byte("$") then -- capture
        if dfa[0] == nil then
            dfa[0] = {}
        end

        dfa[0][head] = "R_CAPTURE("..state_count..", "..capture_count..")"
        head = state_count
        capture_count = capture_count + 1
        state_count = state_count + 1
    end
end

local out = buffer.new(size)
out:put("#define R_PUSH(next)        ((1u<<30u) | (next))\n")
out:put("#define R_POP(next)         ((2u<<30u) | (next))\n")
out:put("#define R_CAPTURE(next, id) ((3u<<30u) | ((id)<<16u) | (next))\n")
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
    out:put("        }\n")
end
out:put("        // no match?\n")
out:put("        default: return NULL;\n")
out:put("    }\n")
out:put("}\n")

print(out:tostring())

local f = io.open("tb/x64/x64_gen.inc", "w")
f:write(out:tostring())
f:close()

