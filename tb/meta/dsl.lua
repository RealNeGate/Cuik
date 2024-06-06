inspect = require "tb/meta/inspect"

function ws(c) return c == string.byte(" ") or c == string.byte("\t") or c == string.byte("\n") or c == string.byte("\r") end
function num(c) return c and (c >= string.byte("0") and c <= string.byte("9")) end
function atom(c) return c and (c > 32 and c ~= string.byte("(") and c ~= string.byte(")")) end

function parse(str, i, list)
    while true do
        local c = str:byte(i)
        while ws(c) do
            i = i + 1
            c = str:byte(i)
        end

        if not c then -- EOF
            return i
        elseif c == string.byte("(") then
            local kid = {}
            i = parse(str, i + 1, kid)
            list[#list + 1] = kid
        elseif c == string.byte(")") then
            return i + 1
        elseif num(c) or (c == string.byte("-") and num(str:byte(i + 1))) then
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

print("Hello")

local root = {}
parse("(+ a b) (do a (b c 10))", 1, root)

print(inspect(root))

