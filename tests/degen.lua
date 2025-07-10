buffer = require "string.buffer"

local str = buffer.new(1000)
str:put(string.format("static int bar(int x, int y) { return x * y; }\n"))

for i=1,100000 do
    str:put(string.format("int foo%d() { return bar(%d, %d); }\n", i, i, i))
end
print(str:tostring())

local file = assert(io.open('ipsccp2.c', 'w'))
file:write(str:tostring())
file:close()

