local lume = require('lib/lume')
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
		print(string..' '..stamp..'s')
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
			local val = enc.value.value:reverse()
			for i = 1, len do
				pattern[bit + i] = val:sub(i + 1, i + 1) -- surrounded by quotes
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
local filtered_branches = {'sve', 'sme', 'reserved', 'dpimm', 'control', 'ldst', 'simd_dp',
-- DPREG
-- 'dp_2src',
'dp_1src',
'log_shift',
'addsub_shift',
'addsub_ext',
-- 'addsub_carry',
'addsub_pt',
'rmif',
'setf',
'condcmp_reg',
'condcmp_imm',
'condsel',
-- 'dp_3src',
}
for _, v in ipairs(filtered_branches) do filtered_branches[v] = true end
local function walk(item, list)
	table.insert(list, item)
	item.path = path(item)
	item.pattern, item.mask = encoding(item)
	if item.children then
		for index, child in ipairs(item.children) do
			if not filtered_branches[child.name] then
				child.parent = item
				walk(child, list)
			end
		end
	end
end

local items = {}
for _, i in ipairs(data.instructions) do
	-- info(walter): just dealing with a64 for now
	if i.name == 'A64' then
		walk(i, items)
	end
end
local instructions = lume.filter(items, is_inst)
timer('parse')



--------------------------------
-- mnemonic opcodes
--------------------------------
local mnems = {}
for _, i in ipairs(instructions) do
	local mnem = i.assembly.symbols[1].value
	if not mnems[mnem] then
		table.insert(mnems, mnem)
		mnems[mnem] = {}
	end
	table.insert(mnems[mnem], i)
	mnems[i.name] = mnem
end
local ops = {}
for i, m in ipairs(mnems) do
	table.insert(ops, '\tA64_'..m..' = '..i)
end
writefile('a64ops.h', [[
#pragma once
enum A64_Opcode {
]]..table.concat(ops, ',\n')..'\n'..[[
} typedef A64_Opcode;
char* a64_mnemonic[] = {
	"UNDEFINED",
]]..table.concat(lume.map(mnems, function(m)return'\t"'..m..'"'end), ',\n')..'\n'..[[
};
char a64_reg_sizes[] = { 'W', 'X' };
]])



--------------------------------
-- decoder decision tree
--------------------------------
-- disassembler numero uno, absolute ass version
-- local chain = {}
-- for _, inst in ipairs(items) do
-- 	if is_inst(inst) then
-- 		local str = '((inst & 0b%s) == 0b%s) {\n\t\t%s\n\t}'
-- 		local mask = table.concat(inst.mask)
-- 		local bits = table.concat(inst.pattern):gsub('x', '0')
-- 		local body = 'return "'..inst.name..'";'
-- 		table.insert(chain, string.format(str, mask, bits, body))
-- 	end
-- end
-- local disassembler1 = [[#include <stdint.h>
-- char* disassemble(uint32_t inst) {
-- 	if ]]..table.concat(chain, ' else if ')..[[ else {
-- 		return "ERROR";
-- 	}
-- }]]
-- writefile('a64disassembler1.h', disassembler1)


-- -- disassembler 2, follow their own path
-- local disassembler2 = {}
-- local function walk(inst, depth)
-- 	local indent = string.rep('\t', depth)
-- 	if is_inst(inst) then
-- 		table.insert(disassembler2, indent..'return "'..inst.name..'";')
-- 	else
-- 		for i, child in ipairs(inst.children) do
-- 			local str = '((inst & 0b%s) == 0b%s) {'
-- 			local mask = table.concat(child.mask)
-- 			local bits = table.concat(child.pattern):gsub('x', '0')
-- 			local check = string.format(str, mask, bits)
-- 			if i == 1 then
-- 				table.insert(disassembler2, indent..'if '..check)
-- 			else
-- 				table.insert(disassembler2, indent..'} else if '..check)
-- 			end
-- 			walk(child, depth + 1)
-- 		end
-- 		table.insert(disassembler2, indent..'} else {\n'..indent..'\treturn "UNKNOWN";\n'..indent..'}')
-- 	end
-- end
-- walk(items[1], 1)
-- disassembler2 = [[#include <stdint.h>
-- char* disassemble(uint32_t inst) {
-- ]]..table.concat(disassembler2, '\n')..[[
-- }]]
-- writefile('a64disassembler2.h', disassembler2)


-- decoder 3, all the set bits
local function getsetmask(list)
	local mask = {} for i=1,32 do mask[i] = '1' end
	for _, inst in ipairs(list) do
		for i=1,32 do
			if inst.mask[i] == '0' then
				mask[i] = '0'
			end
		end
	end
	return mask
end
local function applymask(pattern, mask)
	local pat = copy(pattern)
	for i=1,32 do
		if mask[i] == '0' then
			pat[i] = 'x'
		end
	end
	return pat
end
local seen = {}
local function walk(list)
	local out = {} -- { ['mask'] = { inst, ... } }
	local mask = getsetmask(list)
	for _, l in ipairs(list) do
		local pat = table.concat(applymask(l.pattern, mask))
		if not out[pat] then
			out[pat] = {}
		end
		table.insert(out[pat], l)
	end
	if tablesize(out) == 1 then -- no differentiating patterns
		out = next(out)
		-- for k, v in pairs(out) do
		-- 	out = lume.map(v, function(x) return x.name end)
		-- end
	else -- walk each new sublist
		for k, v in pairs(out) do
			if #v > 1 and not seen[k] then
				seen[k] = true
				out[k] = walk(v)
			-- else
			-- 	out[k] = lume.map(v, function(x) return x.name end)
			end
		end
	end
	return out
end
local tree3 = walk(instructions)

-- generate the C code
local decoder = {}
local function walk(tree, depth)
	local indent = string.rep('\t', depth)
	if #tree > 0 then -- list of instructions
		local name
		if #tree > 1 then -- need more differentiation
			name = table.concat(lume.map(tree, function(x) return x.name end), '$')
			table.insert(decoder, indent..'tb_todo();')
		else
			local inst = tree[1]
			local fields = lume.filter(inst.encoding.values, function(x)
				return is_field(x) and x.value.value:find('x')
			end)
			name = 'A64_'..mnems[inst.name]
			table.insert(decoder, indent..string.format('out->opcode = %s;', name))
			for _, f in ipairs(fields) do
				if f.name:match('^R[dnma]') then
					local mask = string.rep('1', f.range.width)
					local bit = f.range.start
					local reg = f.name:sub(2, 2)
					local fmt = indent..'out->%s = (inst >> %d) & 0b%s;'
					table.insert(decoder, string.format(fmt, reg, bit, mask))
				end
			end
			local regs = { d = 0, m = 1, n = 2, a = 3 }
			local sizes = {}
			for _, s in ipairs(inst.assembly.symbols) do
				if s.rule_id and s.rule_id:match('^[WX][dnma]') then
					local reg = s.rule_id:sub(2, 2)
					local is64 = s.rule_id:sub(1, 1) == 'X'
					if is64 then
						table.insert(sizes, '(1 << '..regs[reg]..')')
					end
				end
			end
			if #sizes > 0 then
				local fmt = indent..'out->sz = %s;'
				table.insert(decoder, string.format(fmt, table.concat(sizes, ' | ')))
			end
		end
	else -- table
		local mask, _ = next(tree)
		mask = mask:gsub('0', '1'):gsub('x', '0')
		table.insert(decoder, indent..string.format('switch (inst & 0b%s) {', mask:reverse()))
		for k, v in pairs(tree) do
			local bits = k:gsub('x', '0')
			table.insert(decoder, indent..string.format('case 0b%s: {', bits:reverse()))
			walk(v, depth + 1)
			table.insert(decoder, indent..'} break;')
		end
		table.insert(decoder, indent..'default: out->opcode = 0; return false;')
		table.insert(decoder, indent..'}')
	end
end
walk(tree3, 1)
writefile('a64decoder.h', [[
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "../include/tb_a64.h"
#include "a64ops.h"
bool decode(TB_A64_Inst* restrict out, const uint8_t* data) {
	uint32_t inst = *((uint32_t*)data);
]]..table.concat(decoder, '\n')..'\n'..[[
	return true;
}
]])

timer('decoder')

-- wot it look like
-- local depths = {}
-- local function walk(tree, depth)
-- 	for k, v in pairs(tree) do
-- 		if tonumber(k) then
-- 			table.insert(depths, depth)
-- 			break
-- 		else
-- 			walk(v, depth + 1)
-- 		end
-- 	end
-- end
-- walk(tree3, 0)
-- print('depths', inspect(histogram(depths)))



--------------------------------
-- disassembler
--------------------------------
local disassembler = {}
for _, m in ipairs(mnems) do
	table.insert(disassembler, '\tcase A64_'..m..': {')

	local insts = mnems[m]
	--todo: differentiate operand forms
	local inst = insts[1]
	--todo: aliases
	local args = { 'a64_mnemonic[inst->opcode]' }
	local regs = { d = 0, m = 1, n = 2, a = 3 }
	local fmt = {'%s'}
	for _, s in ipairs(inst.assembly.symbols) do
		if s.rule_id and s.rule_id:match('[WX][dnma]') then
			local reg = s.rule_id:match('[WX]([dnma])')
			local size = string.format('(inst->sz >> %d) & 0b1', regs[reg])
			table.insert(args, 'a64_reg_sizes['..size..']')
			table.insert(args, string.format('inst->%s', reg))
			table.insert(fmt, ' %c%d')
		end
	end
	args = table.concat(args, ', ')
	fmt = table.concat(fmt)
	table.insert(disassembler, '\t\tprintf("'..fmt..'\\n", '..args..');')

	table.insert(disassembler, '\t} break;')
end

disassembler = [[
#pragma once
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include "../include/tb_a64.h"
#include "a64ops.h"

void disassemble(TB_A64_Inst* inst) {
	switch (inst->opcode) {
	case 0: {
		printf("%s\n", a64_mnemonic[inst->opcode]);
	} break;
]]..table.concat(disassembler, '\n')..'\n'..[[
	}
}
]]
writefile('a64disassembler.h', disassembler)

timer('disassembler')



local counts = {}
local bits = {}
local names = {}
local bitsnranges = {}
for _, inst in ipairs(instructions) do
	local fields = lume.filter(inst.encoding.values, function(x) return is_field(x) and x.value.value:find('x') end)
	local fieldcount = #fields
	local fieldbits = lume.reduce(fields, function(a, f) return a + f.range.width end, 0)
	-- print(fieldcount..' fields', fieldbits..' bits', inst.name)
	table.insert(counts, fieldcount)
	table.insert(bits, fieldbits)
	lume.each(fields, function(x) table.insert(names, x.name) end)
	lume.each(fields, function(x) table.insert(bitsnranges, string.format('%s:%d:%d', x.name, x.range.start, x.range.width)) end)
end
-- print('counts', inspect(histogram(counts)))
-- print('bits', inspect(histogram(bits)))
-- print('names', inspect(histogram(names)))
-- print('bits n ranges', inspect(histogram(bitsnranges)))



--------------------------------
-- assembly syntax
--------------------------------
local function rules_pp(rule)
	local types = {
		['Instruction.Assembly']              = function(a)
			local out = {}
			for _, sym in ipairs(a.symbols) do
				local str = rules_pp(sym)
				if str ~= nil then
					table.insert(out, str)
				end
			end
			if #out > 0 then
				return out
			else
				return nil
			end
		end,
		['Instruction.Symbols.Literal']       = function(l)
			return nil
		end,
		['Instruction.Symbols.RuleReference'] = function(r)
			return r.rule_id -- rules_pp(data.assembly_rules[r.rule_id])
		end,
		['Instruction.Rules.Token']           = function(t)
			return nil
		end,
		['Instruction.Rules.Rule']            = function(r)
			if r.symbols then
				return rules_pp(r.symbols)
			end
			return nil
		end,
		['Instruction.Rules.Choice']          = function(c)
			local out = {}
			for _, choice in ipairs(c.choices) do
				local str = rules_pp(choice)
				if str ~= nil then
					table.insert(out, str)
				end
			end
			if #out > 0 then
				return out
			else
				return nil
			end
		end,
	}
	local syms = {}
	if types[rule._type] then
		return types[rule._type](rule)
	else
		error('invalid type:', rule._type)
	end
end
-- local strings = {'digraph G {', '\trankdir=LR', '\tnode [style=filled]'}
-- for k, v in pairs(data.assembly_rules) do
-- 	local rule = rules_pp(v)
-- 	local function pp(tbl, parent)
-- 		if type(tbl) == 'table' then
-- 			for _, v in ipairs(tbl) do
-- 				pp(v, parent)
-- 			end
-- 		else
-- 			table.insert(strings, '\t'..parent..' -> '..tbl)
-- 		end
-- 	end
-- 	if rule ~= nil then
-- 		pp(rule, k)
-- 	end
-- end
-- table.insert(strings, '}')
-- print(table.concat(strings, '\n'))

local function asm_pp(asm)
	local function todo(a)
		error(inspect(a))
	end
	local types = {
		['Instruction.Assembly']              = function(a)
			local out = {}
			for _, sym in ipairs(a.symbols) do
				local str = asm_pp(sym)
				if str == nil then return nil end
				table.insert(out, str)
			end
			return table.concat(out)
		end,
		['Instruction.Symbols.Literal']       = function(l)
			return l.value
		end,
		['Instruction.Symbols.RuleReference'] = function(r)
			return asm_pp(data.assembly_rules[r.rule_id])
		end,
		['Instruction.Rules.Token']           = function(t)
			return t.default
		end,
		['Instruction.Rules.Rule']            = function(r)
			if r.symbols then
				local str = asm_pp(r.symbols)
				if str ~= nil then
					return str
				end
			end
			return r.display
		end,
		['Instruction.Rules.Choice']          = function(c)
			for _, choice in ipairs(c.choices) do
				local str = asm_pp(choice)
				if str ~= nil then
					return str
				end
			end
			return c.display
		end,
	}
	local syms = {}
	if types[asm._type] then
		return types[asm._type](asm)
	else
		error('invalid type:', asm._type)
	end
end
for _, i in ipairs(instructions) do
	local ops = asm_pp(i.assembly)
	-- print(ops)
end



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
-- for _, i in ipairs(instructions) do
-- 	print(i.path, ast_pp(i.condition, i.path))
-- end
-- print(inspect(ast_unimplemented))



--------------------------------
-- total time
--------------------------------
if options.timer then
	local stamp = string.format('%08.4f', os.clock() - total_time)
	print('total time '..stamp..'s')
end

if true then return end



--------------------------------
-- pattern cleaning
--------------------------------
-- keep a list of patterns
-- remove trailing any-bits
local patterns = {}
for _, inst in ipairs(items) do
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
for idx, item in ipairs(items) do
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
			-- 			print(p, table.concat(items[p].pattern), items[p].path)
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
								local inst = items[p]
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
				labels[b] = labels[b]..','..i
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
for _, i in ipairs(items) do
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
-- assembly syntax
--------------------------------
-- local function ast_sym(symbols)
-- 	for _, s 
-- end
-- local test
-- for _, item in ipairs(items) do
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
for _, item in ipairs(items) do
	table.insert(bitmasks, string.format(
		'#define MASK_%s (0b%s)\n#define BITS_%s (0b%s)\n',
		item.path, table.concat(item.mask, ''),
		item.path, table.concat(item.pattern, ''):gsub('x', '0'):gsub('-', '0')
	))
end
writefile('testbitmasks.h', table.concat(bitmasks, ''))
timer('bitmasks')
