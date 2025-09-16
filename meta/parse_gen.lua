local buffer  = require "string.buffer"
local inspect = require "meta/inspect"

require "meta/prelude"

print(arg[1])
local source = run_command("clang -E -xc "..arg[1])
local lex = lexer(source)

local rules = {}
while lex() == "(" do
    local stmt = parse_node(lex)
    for i=1,#stmt do
        local n = stmt[i]
        if n[1] == "rule" then
            local name = n[2]
            rules[name] = n[3]
        end
    end
end

print(inspect(rules))
