local ast = require('a64ast')
local ansi = require('lib.ansi')
local lume = require('lib.lume')
local jason = require('lib.jason')
local inspect = require('lib.inspect')

local options = { dry = false, verbose = false, graphvis = false }
for _, a in ipairs(arg) do if options[a] ~= nil then options[a] = true end end

local file = assert(io.open('Instructions.json', 'r'))
local content = file:read('*all')
file:close()
local data = jason.decode(content)

local function list_from_string(str)
	local out = {}
	for i = 1, #str do
		table.insert(out, str:sub(i, i))
	end
	return out
end

local function copy(object)
	if type(object) ~= 'table' then
		return object
	else
		local out = {}
		for key, val in pairs(object) do
			out[copy(key)] = copy(val)
		end
		return out
	end
end

-- a between [x,y)
local function between(a, x, y) return x <= a and a < y end
-- [a,b) overlaps [x,y)
local function overlap(a, b, x, y) return x < b and y > a end

-- produce a frequency map of a list
local function freq(list) local o = {}
	for _, v in pairs(list) do o[v] = (o[v] or 0) + 1 end
return o end
-- a single iteration version for lume.reduce
local function tally(t, v) t[v] = (t[v] or 0) + 1 return t end

local items = {}
local function is_set  (i) return i._type == "Instruction.InstructionSet"   end
local function is_group(i) return i._type == "Instruction.InstructionGroup" end
local function is_inst (i) return i._type == "Instruction.Instruction"      end
local function is_alias(i) return i._type == "Instruction.InstructionAlias" end
local function is_bits (i) return i._type == "Instruction.Encodeset.Bits"   end
local function is_field(i) return i._type == "Instruction.Encodeset.Field"  end

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
local function ranges_overlap(r1, r2)
	local a = r1.start
	local b = r1.width + a
	local x = r2.start
	local y = r2.width + x
	return overlap(a, b, x, y)
end
local function add_bits(list, enc)
	local bit = enc.range.start
	local len = enc.range.width
	local val = lume.trim(enc.value.value, "'")
	for i = 1, len do
		list[#list-bit-len+i] = val:sub(i, i)
	end
	-- write back value without quotes because they're stupid
	-- this will always be the first time the field is parsed
	-- so i don't need to care about trimming from this point
	enc.value.value = val
end
local function constrain_field(field, list)
	local bit = field.range.start
	local len = field.range.width
	local val = list_from_string(field.value.value)
	for i = 1, len do
		local idx = #list - bit - len + i
		local old = list[idx]
		if old == '0' or old == '1' then
			val[i] = old
		end 
	end
	return table.concat(val, '')
end
local function encoding(item)
	local pattern = '--------------------------------'
	if item.parent then
		pattern = item.parent.pattern:gsub('x', '-')
	end
	pattern = list_from_string(pattern)
	local fields = {}
	if item.encoding then
		for _, enc in ipairs(item.encoding.values) do
			add_bits(pattern, enc)
			if is_field(enc) then
				table.insert(fields, enc)
			end
		end
	end
	if item.parent then
		for _, pf in ipairs(item.parent.fields) do
			-- if a parent field overlaps, it's been replaced
			local replaced = false
			for _, f in ipairs(fields) do
				replaced = ranges_overlap(f.range, pf.range)
				if replaced then break end
			end
			if not replaced then
				local new = constrain_field(pf, pattern)
				-- make sure it's not fully constrained
				if new:find('x') then
					if new ~= pf.value.value then
						pf = copy(pf)
						pf.value.value = new
					end
					add_bits(pattern, pf)
					table.insert(fields, pf)
				end
			end
		end
	end
	--?? conditions & preferred
	pattern = table.concat(pattern, '')
	mask = pattern:gsub('0', '1'):gsub('-', 'x'):gsub('x', '0')
	return fields, pattern, mask
end
local function path(item)
	local out = {}
	while item ~= nil and not is_set(item) do
		table.insert(out, 1, item.name)
		item = item.parent
	end
	return table.concat(out, '.')
end

local function walk(item, list)
	table.insert(list, item)
	item.path = path(item)
	item.fields, item.pattern, item.mask = encoding(item)
	if item.children then
		for index, child in ipairs(item.children) do
			if true then
				child.parent = item
				walk(child, items)
			end
		end
	end
end
for _, i in ipairs(data.instructions) do
	walk(i, items)
end



--------------------------------
-- c generation
--------------------------------

local needed = {
	['reserved.perm_undef.UDF_only_perm_undef'] = true,
	['dpimm.addsub_imm']                        = true,
	['dpimm.movewide']                          = true,
	['control.condbranch']                      = true,
	['control.exception']                       = true,
	['control.hints']                           = true,
	['control.systemmove']                      = true,
	['control.branch_reg']                      = true,
	['control.branch_imm']                      = true,
	['control.compbranch']                      = true,
	['control.compbranch_regs']                 = true,
	['control.compbranch_imm']                  = true,
	['dpreg.dp_2src']                           = true,
	['dpreg.dp_1src']                           = true,
	['dpreg.log_shift']                         = true,
	['dpreg.addsub_shift']                      = true,
	['dpreg.condcmp_reg']                       = true,
	['dpreg.condsel']                           = true,
	['dpreg.dp_3src']                           = true,
	['simd_dp.float2int']                       = true,
	['simd_dp.floatdp1']                        = true,
	['simd_dp.floatdp2']                        = true,
}

if not options.dry then
	local bitmasks = {}
	for _, item in ipairs(items) do
		if needed[item.path] then
			local name = item.path:gsub('%.', '_')
			table.insert(bitmasks, string.format(
				'#define MASK_%s (0b%s)\n#define BITS_%s (0b%s)\n',
				name, item.mask,
				name, item.pattern:gsub('x', '0'):gsub('-', '0')))
		end
	end
	print('writing a64bitmasks.h')
	local file = assert(io.open('a64bitmasks.h', 'w'))
	file:write(table.concat(bitmasks, ''))
	file:close()
end

if not options.dry then
	local procs = {[[#include <stdint.h>
#include "a64bitmasks.h"
// provides mask of N bits
#define MASK(N) ((1 << N) - 1)
// get N bits from offset O in value V
#define GET_BITS(O, N, V) (((V) >> O) & MASK(N))
// move N bits of value V to offset O
#define PUT_BITS(O, N, V) (((V) & MASK(N)) << O)
]]}
	local proc_template = [[static uint32_t %s(%s) {
	uint32_t inst = 0;
%s
	inst &= %s;
	inst |= %s;
	return inst;
}]]
	for _, item in ipairs(items) do
		if needed[item.path] then
			local args = lume.map(item.fields, function(f)
				return 'uint32_t '..f.name
			end)
			local body = lume.map(item.fields, function(f)
				return string.format("\tinst |= PUT_BITS(%d, %d, %s);", f.range.start, f.range.width, f.name)
			end)
			local path = item.path:gsub('%.', '_')
			table.insert(procs, string.format(
				proc_template,
				path,
				table.concat(args, ', '),
				table.concat(body, '\n'),
				'~MASK_'..path,
				' BITS_'..path))
		end
	end
	print('writing a64encoder.h')
	local file = assert(io.open('a64encoder.h', 'w'))
	file:write(table.concat(procs, '\n'))
	file:close()
end



local assumptions = {
	{ 'all items have conditions', 0 == #lume.reject(items, 'condition') },
	{ 'no InstructionInstances', 0 == #lume.filter(items, { _type = 'Instruction.InstructionInstance'}) },
	{ 'all paths are unique',
		0 == #lume(items)
		:map('path')
		:reduce(tally, {})
		:filter(lume.lambda 'i -> i > 1')
		:result()
	},
	{ 'no fields contain set bits',
		0 == #lume(items)
		:filter('encoding')
		:map('encoding'):map('values')
		:reduce(lume.concat)
		:filter(is_field)
		:map('value'):map('value')
		:filter(lume.lambda 'i -> i:find("0") or i:find("1")')
		:result()
	},
	{ 'no instructions contain unspecified bits',
		0 == #lume(items)
		:reject(is_set):reject(is_group)
		:map('pattern')
		:filter(lume.lambda 'i -> i:find("-")')
		:result()
	},
}
for _, ass in pairs(assumptions) do
	assert(ass[2], ansi('fore', 'red')..ass[1]..ansi())
end

if options.verbose then
	local function encode_string(item)
		return item.pattern..
			'\t'..item.name..
			'\t'..inspect(lume.map(item.fields, function(f)
				return {name = f.name, bit = f.range.start, len = f.range.width, val = f.value.value}
			end), {newline='', indent=''})
	end
	local function path_encode_string(item)
		local lines = {''}
		local current = item
		while current ~= nil do
			table.insert(lines, 1, encode_string(current))
			current = current.parent
		end
		return table.concat(lines, '\n')
	end
	-- print('all paths', inspect(lume.map(items, 'path')))
	-- print('all masks', inspect(lume.map(items, 'mask')))
	-- print('all patterns', inspect(lume.map(items, 'pattern')))
	-- print('duplicate patterns', inspect(lume.filter(freq(lume.map(items, 'pattern')), lume.lambda 'i -> i > 1', true)))
	local dup_names = lume.filter(freq(lume.map(items, 'name')), lume.lambda 'i -> i > 1', true)
	local dup_name_count = lume.reduce(dup_names, lume.lambda 'a, b -> a + b')
	print('names that are duplicated    ', #lume.keys(dup_names))
	print('items with duplicate names   ', dup_name_count)
	local dup_patterns = lume.filter(freq(lume.map(items, 'pattern')), lume.lambda 'i -> i > 1', true)
	local dup_pattern_count = lume.reduce(dup_patterns, lume.lambda 'a, b -> a + b')
	print('patterns that are duplicated ', #lume.keys(dup_patterns))
	print('items with duplicate patterns', dup_pattern_count)
	print('items                        ', #items)
	print('type frequency', inspect(freq(lume.map(items, '_type'))))
end

if options.graphvis then
	local out = {'digraph G {\n\trankdir=LR'}
	for _, item in ipairs(items) do
		if item.parent then
			local label = '"'
			if is_alias(item) then
				label = '" [label="alias"]'
			end
			table.insert(out, '\t"'..item.parent.name..'" -> "'..item.name..label)
		end
	end
	table.insert(out, '}')
	print(table.concat(out, '\n'))
end
