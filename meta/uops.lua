local xmlparser = require "meta/xmlparser"
local inspect = require "meta/inspect"
bit = require "bit"
ffi = require "ffi"

if false then
doc, err = xmlparser.parseFile("C:/Workspace/instructions.xml", false)

local insts = {}
for k,v in pairs(doc.children[1].children) do
    -- find extensions we care about
    print(v.attrs.name)
    if v.attrs.name == "BASE" then
        for k2,v2 in pairs(v.children) do
            local arch = nil
            for k3,v3 in pairs(v2.children) do
                if v3.tag == "architecture" and v3.attrs.name == "SKL" then
                    arch = v3
                    break
                end
            end

            if arch then
                local a, b = v2.attrs.string:match"^(%S+)%s+%((.+)%)"
                if a and b then
                    -- some instruction names have weird shit like the exact opcode
                    -- written after it, trim them off
                    local c = a:match"(.+)_.*"
                    if c then
                        a = c
                    end

                    if not insts[a] then
                        insts[a] = {}
                    end

                    local max_lat = 1
                    for k3,v3 in pairs(arch.children[1].children) do
                        for k4,v4 in pairs(v3.attrs) do
                            if k4 ~= "start_op" and k4 ~= "target_op" then
                                local lat = tonumber(v4)
                                if lat and lat > max_lat then
                                    max_lat = lat
                                end
                            end
                        end
                    end

                    if insts[a][b] then
                        print("Duplicate info on ", a, b)
                        if max_lat > insts[a][b].lat then
                            insts[a][b].lat = max_lat
                        end
                    else
                        insts[a][b] = { tp = arch.children[1].attrs.TP_ports, ports = arch.children[1].attrs.ports, lat = max_lat }
                    end

                    -- print(">", a, "|", "("..b..")", arch.attrs.name, arch.children[1].attrs.TP_ports, arch.children[1].attrs.ports)
                else
                    -- print(">", v2.attrs.string, arch.attrs.name, arch.children[1].attrs.TP_ports, arch.children[1].attrs.ports)
                end
            end
        end
    end
end

print(inspect(insts.ADD))
end

-- Construct Bundling DFA based on which ops can be issued each cycle
--   Port0: INT ALU
--   local inst_class = {
--   ["int_rr"] = { tp=1, lat=1, uops="1*p0156B" },
--   ["int_mr"] = { tp=1, lat=1, uops="1*p0156B+1*p23A+1*p49+1*p78" },
--   ["int_rm"] = { tp=1, lat=1, uops="1*p0156B+1*p23A" },
--   }
local inst_class = {
    ["i"]  = "id",
    ["f"]  = "fd",
    ["ls"] = "id+mem mem",
}

local function lexer(str)
    local i = 1
    return function()
        -- skip whitespace
        while str:byte(i) == 32 do
            i = i + 1
        end

        if i > #str then
            return nil
        end

        if str:byte(i) == 43 then
            i = i + 1
            return "+"
        end

        local start = i
        i = i + 1
        while i <= #str and str:byte(i) ~= 32 and str:byte(i) ~= 43 do
            i = i + 1
        end
        return str:sub(start, i - 1)
    end
end

function Set(w, h)
    local t = {}
    t.data = ffi.new("uint32_t[?]", w*h)
    t.w = w
    t.h = h

    function t.put(self, i, j)
        assert(i < self.w and j < self.h)
        i = bit.tobit(i)
        j = bit.tobit(j)

        local pos = j*self.w + i
        local a = math.floor(i / 32)
        local b = math.floor(i % 32)
        self.data[a] = bit.bor(self.data[a], bit.lshift(1, b))
    end

    function t.get(self, i, j)
        assert(i < self.w and j < self.h)
        i = bit.tobit(i)
        j = bit.tobit(j)

        local pos = j*self.w + i
        local a = math.floor(i / 32)
        local b = math.floor(i % 32)
        return bit.band(t.data[a], bit.lshift(1, b))
    end

    function t.print(self)
        print(t)

        local str = ffi.new("char[?]", w+1)
        for j=0,self.h-1 do
            -- fill bits
            for i=0,self.w-1 do
                local k = j*self.w + i
                local a = math.floor(k / 32)
                local b = math.floor(k % 32)

                print(a, b)
                local x = bit.band(bit.rshift(self.data[a], b), 1)
                str[i] = 48 + x
            end

            print(ffi.string(str))
        end
    end

    return t
end

local n_resources = 0
local resources = {}

local pipeline_len = 0

-- find the machine resources
for k,v in pairs(inst_class) do
local plus = false
    local l = 0
    for t in lexer(v) do
        if t == "+" then
            plus = true
        else
            if not resources[t] then
                resources[t] = n_resources
                n_resources = n_resources + 1
            end

            if not plus then
                l = l + 1
            end
            plus = false
        end
    end
    pipeline_len = math.max(pipeline_len, l)
end

print(n_resources, pipeline_len, inspect(resources))

for k,v in pairs(inst_class) do
    print(v)
    local reserves = Set(n_resources, pipeline_len)

    -- construct resource reservation table
    local l = 0
    for t in lexer(v) do
        if t == "+" then
            plus = true
        else
            if not plus then
                l = l + 1
            end
            plus = false

            local r = resources[t]
            print("PUT", t, r, l-1)
            reserves:put(r, l-1)
        end
    end

    reserves:print()
end

print(inspect(inst_class))

