local buffer  = require "string.buffer"
local inspect = require "tb/meta/inspect"

function string:starts_with(start)
    return self:sub(1, #start) == start
end

function read_all(file)
    local f = assert(io.open(file, "rb"))
    local content = f:read("*all")
    f:close()
    return content
end

function ws(c) return c == string.byte(" ") or c == string.byte("\t") or c == string.byte("\n") or c == string.byte("\r") end
function num(c) return c and (c >= string.byte("0") and c <= string.byte("9")) end
function hexnum(c) return c and ((c >= string.byte("0") and c <= string.byte("9")) or (c >= string.byte("A") and c <= string.byte("F")) or (c >= string.byte("a") and c <= string.byte("f"))) end
function atom(c) return c and (c > 32 and c ~= string.byte("(") and c ~= string.byte(")")) end
function parse(str, i, list)
    while true do
        local c = str:byte(i)

        ::loop::
        if ws(c) then
            repeat
                i = i + 1
                c = str:byte(i)
            until not ws(c)
            goto loop
        end

        if c == string.byte(";") then
            repeat
                i = i + 1
                c = str:byte(i)
            until not c or c == string.byte("\n")
            goto loop
        end

        if not c then -- EOF
            return i
        elseif c == string.byte("(") then
            local kid = {}
            i = parse(str, i + 1, kid)
            list[#list + 1] = kid
        elseif c == string.byte(")") then
            return i + 1
        elseif num(c) or c == string.byte("#") or (c == string.byte("-") and num(str:byte(i + 1))) then
            if c == string.byte("#") then
                assert(str:byte(i + 1) == string.byte("x"))

                i = i + 2
                c = str:byte(i)

                local off = i
                while hexnum(c) do
                    i = i + 1
                    c = str:byte(i)
                end
                list[#list + 1] = tonumber(str:sub(off, i - 1), 16)
            else
                if c == string.byte("-") then
                    i = i + 1
                    c = str:byte(i)
                end

                local n = 0
                while num(c) do
                    n = (n * 10) + (c - string.byte("0"))
                    i = i + 1
                    c = str:byte(i)
                end
                list[#list + 1] = n
            end
        elseif atom(c) then
            local off = i
            while atom(c) do
                i = i + 1
                c = str:byte(i)
            end
            list[#list + 1] = str:sub(off, i - 1)
        else
            error("fuck but in lexing")
        end
    end
end

function is_keyword(str)
    return type(str) == "string" and str:starts_with(":")
end

local src  = read_all("tb/meta/cool.lsp")
local root = {}
parse(src, 1, root)

print(inspect(root))

local mnemonics = {
    -- SSE/AVX ops
    vmov="FP_MOV",vadd="FP_ADD",vmul="FP_MUL",vsub="FP_SUB",vmin="FP_MIN",vmax="FP_MAX",
    vdiv="FP_DIV",vcmp="FP_CMP",vand="FP_AND",vor="FP_OR",  vxor="FP_XOR",ucomi="FP_UCOMI",
}

for i,s in ipairs({"add","or","and","sub","xor","cmp","mov","test","lea","shl","shr","rol","ror","sar"}) do
    mnemonics[s] = string.upper(s)
end

local reg_names = {
    -- 64bit regs
    rax=0, rcx=1, rdx=2,  rbx=3,  rsp=4,  rbp=5,  rsi=6,  rdi=7,
    r8=8,  r9=9,  r10=10, r11=11, r12=12, r13=13, r14=14, r15=15,
    -- 32bit regs
    eax=0, ecx=1, edx=2,   ebx=3,   esp=4,  ebp=5,    esi=6,   edi=7,
    r8d=8, r9d=9, r10d=10, r11d=11, r12d=12, r13d=13, r14d=14, r15d=15,
    -- 16bit regs
    ax=0,  cx=1,  dx=2,    bx=3,    sp=4,    bp=5,    si=6,    di=7,
    r8w=8, r9w=9, r10w=10, r11w=11, r12w=12, r13w=13, r14w=14, r15w=15,
    -- 8bit regs
    al=0,  cl=1,  dl=2,    bl=3,    spl=4,   bpl=5,   sil=6,   dil=7,
    r8d=8, r9b=9, r10b=10, r11b=11, r12b=12, r13b=13, r14b=14, r15b=15,
}

local reg_classes = {
    -- 64bit regs
    rax=4, rcx=4, rdx=4, rbx=4, rsp=4, rbp=4, rsi=4, rdi=4,
    r8=4,  r9=4,  r10=4, r11=4, r12=4, r13=4, r14=4, r15=4,
    -- 32bit regs
    eax=3, ecx=3, edx=3,  ebx=3,  esp=3,  ebp=3,  esi=3,  edi=3,
    r8d=3, r9d=3, r10d=3, r11d=3, r12d=3, r13d=3, r14d=3, r15d=3,
    -- 16bit regs
    ax=2,  cx=2,  dx=2,   bx=2,   sp=2,   bp=2,   si=2,   di=2,
    r8w=2, r9w=2, r10w=2, r11w=2, r12w=2, r13w=2, r14w=2, r15w=2,
    -- 8bit regs
    al=1,  cl=1,  dl=1,   bl=1,   spl=1,  bpl=1,  sil=1,  dil=1,
    r8d=1, r9b=1, r10b=1, r11b=1, r12b=1, r13b=1, r14b=1, r15b=1,
}

local op_size_names = { "byte", "word", "dword", "qword", "xmmword", "ss", "sd", "ps", "pd" }
local vec_type = { ["ss"]=6, ["sd"]=7, ["ps"]=8, ["pd"]=9 }

-- types are a tiny lattice, nil is top, "bot" is bottom
function common_type(a, b)
    -- a /\ top = a
    -- a /\ bot = bot
    if not a or b == "bot" then return b end
    if not b or a == "bot" then return a end
    -- a /\ a = a
    if a == b then return a end

    return math.max(a, b)
end

local out = buffer.new(size)

-- ty is the expression type, like the expectation
--   "val" for operands (registers, immediates, memory)
function expr(n, depth, ty)
    if type(n) == "number" then
        out:put(n)
    elseif type(n) == "string" then
        if n:starts_with("xmm") then
            local i = tonumber(n:sub(4))
            out:put(i)
            out:put(" /* xmm */")
            return 5
        else
            local r = reg_names[n]
                if r then
                out:put(r)
                out:put(" /* ")
                out:put(n)
                out:put(" */")
                return reg_classes[n]
            else
                out:put(n)
            end
        end
    elseif type(n) == "table" then
        if n[1] == "case" then
            out:put("switch (")
            expr(n[2], depth)
            out:put(") {\n")

            depth = depth + 1
            for i=3,#n do
                local case = n[i]

                for k=1,depth do out:put("  ") end
                out:put("case ")
                out:put(case[1])
                out:put(":{\n")

                for j=2,#case do
                    for k=0,depth do out:put("  ") end
                    expr(case[j], depth + 1)
                    out:put(";\n")
                end

                for k=1,depth do out:put("  ") end
                out:put("}break;\n")
            end
            depth = depth - 1

            for k=1,depth do out:put("  ") end
            out:put("}")
        elseif n[1] == "emit" then
            -- reserve space
            out:put("do { uint8_t* $p = tb_cgemit_reserve($emitter, ")
            out:put(#n - 1)
            out:put("); ")

            for i=2,#n do
                out:put("*$p++ = ")
                expr(n[i], depth)
                out:put("; ")
            end

            -- commit
            out:put("$emitter->count += ")
            out:put(#n - 1)
            out:put("; } while (0)")
        else
            local name = mnemonics[n[1]]
            if name then
                local dt = -1
                local cnt = #n - 1

                for i=2,#n do if is_keyword(n[i]) then
                    local s = n[i]:sub(2)
                    if vec_type[s] then
                        dt = vec_type[s]
                    end
                    cnt = cnt - 1
                end end

                out:put("inst")
                out:put(cnt)
                out:put("($emitter, ")
                out:put(name)

                for i=2,#n do if not is_keyword(n[i]) then
                    out:put(", ")

                    local op_dt = expr(n[i], depth, "val")
                    dt = common_type(dt, op_dt)
                end end

                out:put(", ")
                out:put(dt)
                out:put(" /* ")
                out:put(op_size_names[dt])
                out:put(" */)")
            end
        end
    end
end

for k,v in ipairs(root) do
    expr(v, 0)
    out:put(";\n")
end

print(out:tostring())
