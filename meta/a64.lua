local jason = require('lib/jason')
local inspect = require('lib/inspect')



--------------------------------
-- script options
--------------------------------
local options = {
	timer = false,
	graph = false,
}
local bitsize = 1
for _, a in ipairs(arg) do
	if options[a] ~= nil then
		options[a] = true
	end
	if tonumber(a) then
		bitsize = tonumber(a)
	end
end



--------------------------------
-- file io
--------------------------------
local function readfile(path)
	local file = assert(io.open(path, 'rb'))
	local out = file:read('*all')
	file:close()
	return out
end
local function writefile(path, content)
	local file = io.open(path, 'wb')
	file:write(content)
	file:close()
end



--------------------------------
-- timing
--------------------------------
local total_time = os.clock()
local timestamp = os.clock()
local function timer(string)
	if options.timer then
		local stamp = string.format('%08.4f', os.clock() - timestamp)
		print(string .. ' ' .. stamp .. 's')
	end
	timestamp = os.clock()
end



--------------------------------
-- load file
--------------------------------
local file = readfile('Instructions.json'):gsub('\n%s*', '')
timer('readfile')

local data = jason.decode(file)
timer('jason')



--------------------------------
-- helper functions
--------------------------------
local function copy(object)
	if type(object) ~= 'table' then
		return object
	end
	local out = {}
	for key, val in pairs(object) do
		out[copy(key)] = copy(val)
	end
	return out
end

local function tablesize(table)
	local n = 0
	for _, _ in pairs(table) do
		n = n + 1
	end
	return n
end

local function histogram(table) 
	local out = {}
	for _, v in pairs(table) do
		if not out[v] then
			out[v] = 0
		end
		out[v] = out[v] + 1
	end
	return out
end

local function slice(list, a, b)
	local unpack = table.unpack or unpack
	return {unpack(list, a, b)}
end



--------------------------------
-- parse info
--------------------------------
--[[ This part is really annoying
'Encodeset' schema documentation states:
	- If part of an Encodeset is unspecified in both
		the current instruction node
		and all of its parent nodes,
		that part is considered to be an "any-bit"
		('1' or '0', usually denoted as 'x').
	- If part of the local Encodeset is unspecified but
		that same part has a value specified in a parent node,
		the value of that part is inherited in the local Encodeset.
'Instruction.Encodeset.Field' schema documentation states:
	- 'value': Represents the value that the Field takes,
		based on the conditions that constrain the Field.
		An x indicates that this bit can be 0 or 1.
		All bits in a Field are set to x if the value is unconstrained.
how i understand it:
	a bit inherits the latest specified value, if any.
		this includes '1', '0', and 'x' (field)
	a 'bits' is always inherited
	a 'field' is inherited if
		it is not overlapped in any way by a current 'field'
		it is not overlapped completely by 'bits'
			partial overlap with bits will constrain the field
	'EXTR_32_extract' has a 'bits' set within a 'field',
		and the field still needs to be inherited.
		yes it could have been a constrained field.
		no i don't know why it is this way.
]]
local function is_set  (i) return i._type == "Instruction.InstructionSet"   end
local function is_group(i) return i._type == "Instruction.InstructionGroup" end
local function is_inst (i) return i._type == "Instruction.Instruction"      end
local function is_alias(i) return i._type == "Instruction.InstructionAlias" end
local function is_bits (i) return i._type == "Instruction.Encodeset.Bits"   end
local function is_field(i) return i._type == "Instruction.Encodeset.Field"  end

local function encoding(item)
	local pattern = {}
	if item.parent then
		pattern = copy(item.parent.pattern)
	else
		for i = 1, 32 do pattern[i] = 'x' end
	end
	if item.encoding then
		for _, enc in ipairs(item.encoding.values) do
			local bit = enc.range.start
			local len = enc.range.width
			local val = enc.value.value
			for i = 1, len do
				pattern[#pattern - bit - len + i] = val:sub(i + 1, i + 1) -- note: starts with quote
			end
		end
	end
	--?? should i extra handle fields and/or conditions?
	local mask = {}
	local mask_bits = {['0']='1',['1']='1',['x']='0'}
	for i, p in ipairs(pattern) do
		mask[i] = mask_bits[p]
	end
	return pattern, mask
end
local function path(item)
	local out = {}
	while item ~= nil do -- and not is_set(item) do
		table.insert(out, 1, item.name)
		item = item.parent
	end
	return table.concat(out, '$')
end
local filtered_branches = {'sve', 'sme', 'simd_dp'}
for _, v in ipairs(filtered_branches) do filtered_branches[v] = true end
local function walk(item, list)
	table.insert(list, item)
	item.path = path(item)
	item.pattern, item.mask = encoding(item)
	if item.children then
		for index, child in ipairs(item.children) do
			-- if not filtered_branches[child.name] then
				child.parent = item
				walk(child, list)
			-- end
		end
	end
end

local instructions = {}
for _, i in ipairs(data.instructions) do
	-- info(walter): just dealing with a64 for now
	if i.name == 'A64' then
		walk(i, instructions)
	end
end
timer('parse')
-- local count = 0
-- for _, v in ipairs(instructions) do
-- 	if is_inst(v) then count = count + 1 end
-- end
-- print('insts', count)
-- if true then return end

--------------------------------
-- decision tree
--------------------------------
-- disassembler numero uno, absolute ass version
local chain = {}
for _, inst in ipairs(instructions) do
	if is_inst(inst) then
		local str = '((inst & 0b%s) == 0b%s) {\n\t\t%s\n\t}'
		local mask = table.concat(inst.mask)
		local bits = table.concat(inst.pattern):gsub('x', '0')
		local body = 'printf("%08x\\t'..inst.name..'\\n", inst);'
		table.insert(chain, string.format(str, mask, bits, body))
	end
end
local disassembler1 = [[void disassemble1(int inst) {
	if ]]..table.concat(chain, ' else if ')..[[ else {
		printf("%08x\tUNKNOWN\n", inst);
	}
}
]]


-- disassembler 2, follow their own road
local disassembler2 = {}
local function walk(inst, depth)
	local indent = string.rep('\t', depth)
	if is_inst(inst) then
		table.insert(disassembler2, indent..'printf("%08x\\t'..inst.name..'\\n", inst);')
	else
		for i, child in ipairs(inst.children) do
			local str = '((inst & 0b%s) == 0b%s) {'
			local mask = table.concat(child.mask)
			local bits = table.concat(child.pattern):gsub('x', '0')
			local check = string.format(str, mask, bits)
			if i == 1 then
				table.insert(disassembler2, indent..'if '..check)
			else
				table.insert(disassembler2, indent..'} else if '..check)
			end
			walk(child, depth + 1)
		end
		table.insert(disassembler2, indent..'} else {\n'..indent..'\tprintf("%08x\\tUNKNOWN\\n", inst);\n'..indent..'}')
	end
end
walk(instructions[1], 1)
disassembler2 = 'void disassemble2(int inst) {\n'..table.concat(disassembler2, '\n')..'\n}\n'


-- disassembler 3, sort my tree
local sorted_list = {}
for i, inst in ipairs(instructions) do
	if is_inst(inst) then
		table.insert(sorted_list, inst)
	end
end
local function walk(list)
	local function maskout(pattern, mask)
		local pat = copy(pattern)
		for i = 1, 32 do
			if mask[i] == '0' then
				pat[i] = '0'
			end
		end
		return table.concat(pat):gsub('x', '0')
	end
	if #list == 1 then
		return list
	end
	table.sort(list, function (a, b)
		local apat = table.concat(a.pattern):gsub('x', '0')
		local bpat = table.concat(b.pattern):gsub('x', '0')
		return apat < bpat
	end)
	local mid = math.ceil(#list / 2)
	local mask = list[mid].mask
	table.sort(list, function (a, b)
		local apat = maskout(a.pattern, mask)
		local bpat = maskout(b.pattern, mask)
		return apat < bpat
	end)
	while mid < #list and maskout(list[mid].pattern, mask) == maskout(list[mid+1].pattern, mask) do
		mid = mid + 1
	end
	if mid == #list then
		return list
		-- print(string.format('%s\n%s\n%s\t%s\n%s\t%s\n%s', mid, table.concat(mask), table.concat(list[1].pattern), maskout(list[1].pattern, mask), table.concat(list[2].pattern), maskout(list[2].pattern, mask), inspect(list, {depth=2})))
		-- error()
	else
		local lo = slice(list, 1, mid)
		local hi = slice(list, mid + 1, #list)
		return {mid = list[mid], lo = walk(lo), hi = walk(hi)}
	end
end
sorted_list = walk(sorted_list)
-- for _, v in ipairs(sorted_list) do
-- 	print(table.concat(v.pattern), v.name)
-- end
local function test(list, depth)
	local names = {'ADRP_only_pcreladdr', 'SUBS_64S_addsub_imm'} for _, n in ipairs(names) do names[n] = true end
	if not list.mid then
		for _, v in ipairs(list) do
			if names[v.name] then
				print(table.concat(v.pattern), string.rep('.', depth), v.name)
				return true
			end
		end
	else
		local out = test(list.lo, depth + 1)
		local out = test(list.hi, depth + 1) or out
		if out then
			print(table.concat(list.mid.pattern), string.rep('.', depth))
			return true
		end
	end
	return false
end
test(sorted_list, 1)
local disassembler3 = {}
local function walk(tree, depth, path)
	local indent = string.rep('\t', depth)
	if not tree.mid then
		for _, v in ipairs(tree) do
			local str = indent..'printf("%08x\\t%s\\t'..v.name..'\\n", inst, "'..path..'");'
			table.insert(disassembler3, str)
		end
	else
		local str = indent..'if ((inst & 0b%s) <= 0b%s) {'
		local mask = table.concat(tree.mid.mask)
		local split = table.concat(tree.mid.pattern):gsub('x', '0')
		table.insert(disassembler3, string.format(str, mask, split))
		walk(tree.lo, depth + 1, path..'1')
		table.insert(disassembler3, indent..'} else {')
		walk(tree.hi, depth + 1, path..'0')
		table.insert(disassembler3, indent..'}')
	end
end
walk(sorted_list, 1, '')
disassembler3 = 'void disassemble3(int inst) {\n'..table.concat(disassembler3, '\n')..'\n}\n'

writefile('a64_disassembler.h', '#include <stdio.h>\n'..disassembler1..disassembler2..disassembler3)
timer('decision tree')

--------------------------------
-- total time
--------------------------------
if options.timer then
	local stamp = string.format('%08.4f', os.clock() - total_time)
	print('total time ' .. stamp .. 's')
end

if true then return end



--------------------------------
-- pattern cleaning
--------------------------------
-- keep a list of patterns
-- remove trailing any-bits
local patterns = {}
for _, inst in ipairs(instructions) do
	local lim = #inst.pattern
	for i = #inst.pattern, 1, -1 do
		if inst.pattern[i] ~= 'x' then
			lim = i
			break
		end
	end
	local pat = {}
	for i = 1, lim do pat[i] = inst.pattern[i] end
	table.insert(patterns, pat)
end



--------------------------------
-- operand coalescing
--------------------------------
--[[
	trying to coalesce each instruction into groups.
	the group is its mask, but treating certain ranges
	of bits as fully set or as an operand.
]]
local maskgroups = {}
for idx, item in ipairs(instructions) do
	if is_inst(item) then
		local mask = copy(item.mask)
		if table.concat(mask):sub(28, 32):find('0') then
			for i = 28, 32 do mask[i] = '0' end
		end
		if table.concat(mask):sub(23, 27):find('0') then
			for i = 23, 27 do mask[i] = '0' end
		end
		if table.concat(mask):sub(17, 22):find('0') then
			for i = 17, 22 do mask[i] = '0' end
		end
		if table.concat(mask):sub(12, 16):find('0') then
			for i = 12, 16 do mask[i] = '0' end
		end

		local str = table.concat(mask)
		if not maskgroups[str] then
			maskgroups[str] = {}
		end
		table.insert(maskgroups[str], idx)
	end
end

for k, v in pairs(maskgroups) do
	print(k, #v)
end
print(tablesize(maskgroups))
error()



--------------------------------
-- once i was a dfa a little dfa
--------------------------------
local function genSigma(n)
	assert(n >= 1, 'invalid Sigma size (not sigma :sob:)')
	local Sigma = {}
	local function walk(n, l, c)
		table.insert(l, c)
		if n == 1 then
			table.insert(Sigma, table.concat(l))
		else
			walk(n - 1, l, '0')
			walk(n - 1, l, '1')
		end
		table.remove(l)
	end
	walk(n, {}, '0')
	walk(n, {}, '1')
	return Sigma
end
local dfa = {
	Q = { true },
	Sigma = genSigma(bitsize),
	delta = {},
	q0 = 1,
	F = {},
}

-- initial state
local delta_id = 1
local q0 = copy(maskgroups)
local worklist = {
	{active = q0, index = 1, id = 1}
}

local function step_bit(q, bit)
	local removal = {}
	for k, v in pairs(q.active) do
		for i = #v, 1, -1 do
			local pat = patterns[v[i]]
			if q.index <= #pat then
				local c = pat[q.index]
				if c ~= 'x' and c ~= bit then
					table.remove(q.active[k], i)
				end
			end
		end
		if #v == 0 then
			table.insert(removal, k)
		end
	end
	for _, v in ipairs(removal) do
		q.active[v] = nil
	end
	q.index = q.index + 1
end

local function step(q, input)
	for i = 1, #input do
		local bit = input:sub(i, i)
		step_bit(q, bit)
	end
end

local function sets_equal(a, b)
	for k, _ in pairs(a) do if not b[k] then return false end end
	for k, _ in pairs(b) do if not a[k] then return false end end
	return true
end

local function is_final(q)
	-- definitely if there is only 1 active group
	local final = tablesize(q) == 1
	-- potentially if all patterns in a group are finished
	for k, v in pairs(q.active) do
		local finished = true
		for _, p in ipairs(v) do
			if q.index <= #patterns[p] then
				finished = false
				break
			end
		end
		final = final or finished
	end
	return final
end

-- we can cache an id by the active patterns and string index
-- if they are both the same, then the states are indistinguishable
local cache = {}
while #worklist > 0 do
	-- depth first search, using latest work
	local q = table.remove(worklist)
	for _, input in ipairs(dfa.Sigma) do
		-- copy so we can edit destructively
		local qprime = copy(q)
		step(qprime, input)
		-- if any patterns are still alive
		local qprimesize = tablesize(qprime.active)
		if qprimesize > 0 then
			-- work out if a final state
			local final = is_final(qprime)
			-- if final and qprimesize > 1 then
			-- 	print('one pattern finished when more than one is alive')
			-- 	print('INPUT', input, 'INDEX', qprime.index)
			-- 	for k, v in pairs(qprime.active) do
			-- 		for _, p in ipairs(v) do
			-- 			print(p, table.concat(instructions[p].pattern), instructions[p].path)
			-- 		end
			-- 	end
			-- end
			-- is this state cached?
			local cached = false
			for _, oldq in ipairs(cache) do
				local index_equal = qprime.index == oldq.index
				local final_equal = false -- final and oldq.final
				if (index_equal or final_equal) and sets_equal(qprime.active, oldq.active) then
					qprime.id = oldq.id
					cached = true
					break
				end
			end
			-- if new then assign id
			if not cached then
				delta_id = delta_id + 1
				qprime.id = delta_id
				local newq = copy(qprime)
				newq.final = true
				table.insert(cache, newq)
			end
			-- we have a new transition
			if not dfa.delta[q.id] then
				dfa.delta[q.id] = {}
			end
			dfa.delta[q.id][input] = qprime.id
			-- handle new state being added
			if not dfa.Q[qprime.id] then
				dfa.Q[qprime.id] = true
				-- handle being final state
				if not final then
					table.insert(worklist, qprime)
				else
					dfa.F[qprime.id] = {}
					if qprimesize > 1 then
						-- print('multiple')
					end
					for k, v in pairs(qprime.active) do
						table.insert(dfa.F[qprime.id], k)
						if qprimesize > 1 then
							for _, p in ipairs(v) do
								local inst = instructions[p]
								-- print(table.concat(inst.pattern), inst.path)
							end
						end
					end
				end
			end
		end
	end
end
timer('dfa')

-- print(inspect(dfa))
local bitsperedge = math.ceil(math.log(tablesize(dfa.Q), 2))
local bytesperedge = math.ceil(bitsperedge / 8)
local numedges = 2 ^ bitsize
local bytes1 = math.ceil((tablesize(dfa.delta) * numedges * bitsperedge) / 8)
local bytes2 = math.ceil((tablesize(dfa.delta) * numedges * bytesperedge))
-- local bytesperedge = math.ceil((bitsperedge + 7) / 8)
-- print(
-- 	'bits', bitsize,
-- 	'edges', 2 ^ bitsize,
-- 	'bits per edge', bitsperedge,
-- 	'bytes per edge', bytesperedge,
-- 	'states', tablesize(dfa.Q),
-- 	'final', tablesize(dfa.F),
-- 	'size (bints)', string.format('%.2f  kb', (bytes1/1024)),
-- 	'size (bytes)', string.format('%.2f  kb', (bytes2/1024))
-- )
-- print(inspect(dfa.F))
-- for k, v in pairs(dfa.F) do
-- 	if #v > 1 then
-- 		print('final state', k, 'has', inspect(v))
-- 	end
-- end



--------------------------------
-- graph visualisation
--------------------------------

if options.graph then
	local strings = {'digraph G {', '\trankdir=LR', '\tnode [style=filled]'}
	for a, f in pairs(dfa.F) do
		table.insert(strings, string.format('\t%d [fillcolor="%s", label="%s"]', a, 'yellow', f[1]))
	end
	for a, ib in pairs(dfa.delta) do
		local labels = {}
		for i, b in pairs(ib) do
			if not labels[b] then
				labels[b] = i
			else
				labels[b] = labels[b] .. ',' .. i
			end
		end
		for b, l in pairs(labels) do
			table.insert(strings, string.format('\t%d -> %d [label="%s"]', a, b, l))
			-- print(a, b, l)
		end
	end
	table.insert(strings, '}')
	print(table.concat(strings, '\n'))
	timer('graphviz')
end





--------------------------------
-- operation id
--------------------------------
local opids = {}
for _, i in ipairs(instructions) do
	local o = i.operation_id
	if o then
		if not opids[o] then
			opids[o] = {}
			opids[o].pattern = copy(i.pattern)
			opids[o].names = { { i.name, table.concat(i.pattern) } }
		else
			table.insert(opids[o].names, { i.name, table.concat(i.pattern) })
			for n, p in ipairs(i.pattern) do
				if p ~= opids[o].pattern[n] then
					opids[o].pattern[n] = 'x'
				end
			end
		end
	end
end
-- for k, v in pairs(opids) do
-- 	print(string.format('%s\n\t%s', k, table.concat(v.pattern)))
-- 	for _, p in ipairs(v.names) do
-- 		print(string.format('\t%s\t%s', p[2], p[1]))
-- 	end
-- end

--------------------------------
-- condition ast
--------------------------------
local ast_unimplemented = {}
local function ast_pp(tree, path)
	if path == nil then path = '' end
	local out = {}
	local todo = function (tree, path)
		if not ast_unimplemented[tree._type] then
			ast_unimplemented[tree._type] = {}
		end
		table.insert(ast_unimplemented[tree._type], path)
		return 'TODO('..tree._type..'('..path..'))'
	end
	local cases = {
		['AST.BinaryOp']              = function (tree, path)
			table.insert(out, ast_pp(tree.left, path..'>left'))
			table.insert(out, tree.op)
			table.insert(out, ast_pp(tree.right, path..'>right'))
			return table.concat(out, ' ')
		end,
		['AST.Bool']                  = function (tree, path)
			return tree.value
		end,
		['AST.Concat']                = todo,
		['AST.DotAtom']               = function (tree, path)
			for i, v in ipairs(tree.values) do
				if i > 1 then table.insert(out, '.') end
				table.insert(out, ast_pp(v, path..'>'..i))
			end
			return table.concat(out, '')
		end,
		['AST.Function']              = function (tree, path)
			table.insert(out, tree.name)
			table.insert(out, '(')
			for i, a in ipairs(tree.arguments) do
				if i > 1 then table.insert(out, ', ') end
				table.insert(out, ast_pp(a, path..'>'..i))
			end
			table.insert(out, ')')
			return table.concat(out, '')
		end,
		['AST.Identifier']            = function (tree, path)
			return tree.value
		end,
		['AST.Integer']               = function (tree, path)
			return ''..tree.value
		end,
		['AST.Real']                  = todo,
		['AST.Set']                   = function (tree, path)
			table.insert(out, '[')
			for i, v in ipairs(tree.values) do
				if i > 1 then table.insert(out, ', ') end
				table.insert(out, ast_pp(v, path..'>'..i))
			end
			table.insert(out, ']')
			return table.concat(out, '')
		end,
		['AST.SquareOp']              = todo,
		['AST.Tuple']                 = todo,
		['AST.TypeAnnotation']        = todo,
		['AST.UnaryOp']               = function (tree, path)
			table.insert(out, tree.op)
			table.insert(out, '(')
			table.insert(out, ast_pp(tree.expr, path))
			table.insert(out, ')')
			return table.concat(out, '')
		end,
		['Types.Field']               = function (tree, path)
			table.insert(out, string.format('%s.%s', tree.value.name, tree.value.field))
			return table.concat(out, '')
		end,
		['Types.PstateField']         = todo,
		['Types.RegisterMultiFields'] = todo,
		['Types.RegisterType']        = todo,
		['Types.String']              = todo,
		['Values.Value']              = function (tree, path)
			return tree.value
		end,
	}
	if tree then
		if cases[tree._type] then
			return cases[tree._type](tree, path)
		else
			return 'UNKNOWN('..tree._type..')'
		end
	else
		return 'NIL'
	end
end

--------------------------------
-- assembly syntax
--------------------------------
-- local function ast_sym(symbols)
-- 	for _, s 
-- end
-- local test
-- for _, item in ipairs(instructions) do
-- 	if item.name == 'ADD_64_addsub_shift' then
-- 		test = item
-- 		break
-- 	end
-- end
-- -- print(inspect(test.assembly.symbols))
-- for _, v in ipairs(test.assembly.symbols) do
-- 	if v._type == 'Instruction.Symbols.RuleReference' then
-- 		print(inspect(data.assembly_rules[v.rule_id]))
-- 	else
-- 		print(inspect(v))
-- 	end
-- end

--------------------------------
-- print masks
--------------------------------
local bitmasks = {}
for _, item in ipairs(instructions) do
	table.insert(bitmasks, string.format(
		'#define MASK_%s (0b%s)\n#define BITS_%s (0b%s)\n',
		item.path, table.concat(item.mask, ''),
		item.path, table.concat(item.pattern, ''):gsub('x', '0'):gsub('-', '0')
	))
end
writefile('testbitmasks.h', table.concat(bitmasks, ''))
timer('bitmasks')
