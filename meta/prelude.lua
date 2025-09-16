local inspect = require "meta/inspect"

function OrderedSet()
    local t = { ord={}, entries={} }

    function t:put(k)
        if not self.entries[k] then
            self.entries[k] = true
            self.ord[#self.ord + 1] = k
        end
    end

    function t:iter()
        local i = 0
        local n = #self.ord
        return function ()
            i = i + 1
            if i <= n then
                return self.ord[i]
            end
        end
    end

    return t
end

function Partitions()
    local t = { ord={}, entries={} }

    function t:put(k, v)
        local list = self.entries[k]
        if not list then
            self.entries[k] = { v }
            self.ord[#self.ord + 1] = k
            return true
        else
            list[#list + 1] = v
            return false
        end
    end

    function t:put_list(k, v)
        local list = self.entries[k]
        local is_new = false
        if not list then
            list = {}
            self.entries[k] = list
            self.ord[#self.ord + 1] = k
            is_new = true
        end

        for i=1,#v do
            list[#list + 1] = v[i]
        end
        return is_new
    end

    function t:count()
        return #self.ord
    end

    function t:sort(cmp_fn)
        table.sort(self.ord, function(a, b)
            return cmp_fn(self.entries[a], self.entries[b])
        end)
    end

    function t:iter()
        local i = 0
        local n = #self.ord
        return function ()
            i = i + 1
            if i <= n then
                local k = self.ord[i]
                return k, self.entries[k]
            end
        end
    end

    return t
end

function string:starts_with(start)
    return self:sub(1, #start) == start
end

function run_command(cmd)
    local f = assert(io.popen(cmd))
    local content = f:read("*all")
    f:close()
    return content
end

function sort(set, cmp_fn)
    local a = {}
    for k, v in pairs(set) do
        table.insert(a, k)
    end

    table.sort(a, function(a, b)
        return cmp_fn(set, a, b)
    end)
    return a
end

function shallowcopy(orig)
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        copy = {}
        for orig_key, orig_value in pairs(orig) do
            copy[orig_key] = orig_value
        end
    else -- number, string, boolean, etc
        copy = orig
    end
    return copy
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

local line_num = 1

local function either(a, b, c) return a == b or a == c end
function lexer(str)
    local i = 1
    return function()
        -- skip whitespace
        while ch_class[str:byte(i)] == "ws" or str:byte(i) == 35 do
            if str:byte(i) == 10 then
                i = i + 1
                line_num = line_num + 1
            elseif ch_class[str:byte(i)] == "ws" then
                i = i + 1
            elseif str:byte(i) == 35 then -- hash are comments
                while str:byte(i) ~= 10 do
                    i = i + 1
                end
            end
        end

        if i > #str then
            return nil
        end

        local start = i
        local class = ch_class[str:byte(i)]
        if str:byte(i) == 34 then
            i = i + 1
            while str:byte(i) ~= 34 do
                i = i + 1
            end
            i = i + 1
            return str:sub(start+1, i-2)
        elseif str:byte(i) == 46 then
            i = i + 1
            while str:byte(i) == 46 do
                i = i + 1
            end
            return str:sub(start, i - 1)
        elseif class == "num" then
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

function parse_node(lex)
    local n = {}
    local t = lex()

    if t:byte(1) == string.byte("$") then
        n["name"] = t

        t = lex()
        if t ~= ":" then
            print("fuck but in colon")
            os.exit(1)
        end

        t = lex()
    end

    while t ~= ")" do
        if t == "(" then
            n[#n + 1] = parse_node(lex)
            t = lex()
        else
            local peek = lex()
            if peek == "=" then
                n[t] = lex()
                t = lex()
            else
                n[#n + 1] = t
                t = peek
            end
        end
    end
    return n
end
