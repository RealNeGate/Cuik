bit = require "bit"

local jason = require('jason')
local inspect = require('inspect')

--[[ file structure
	0. helper functions
	1. load & parse jason
		- extract bit patterns
	2. generate dfa
	3. cleanup dfa
	4. generate c
	5. data analysis (optional)
		- ask a bunch of questions
]]

--------------------------------
-- helper schtuff
--------------------------------

local options = {
	timings = false,
	listing = false,
	graphviz = false,
	analysis = false,
	transpose = false,
}
for _, a in ipairs(arg) do
	if options[a] ~= nil then
		options[a] = true
	end
end

local total_time = os.clock()
local timestamp = os.clock()
local function timer(string)
	if options.timings then
		local stamp = string.format('%08.4f', os.clock() - timestamp)
		print(string .. ' ' .. stamp .. 's')
	end
	timestamp = os.clock()
end

local function dump(object)
	-- nil, boolean, number, string, userdata, function, thread
	if type(object) ~= 'table' then
		return tostring(object)
	else -- table
		local string = {''}
		table.insert(string, '{')
		for key, val in pairs(object) do
			table.insert(string, dump(key) .. ' = ' .. dump(val))
			table.insert(string, ', ')
		end
		if string[#string] == ', ' then
			table.remove(string)
		end
		table.insert(string, '}')
		return table.concat(string, '')
	end
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

local function sets_equal(a, b)
	for k, _ in pairs(a) do if not b[k] then return false end end
	for k, _ in pairs(b) do if not a[k] then return false end end
	return true
end

local function reverse(t)
	for i = 1, math.floor(#t / 2) do
		t[i], t[#t - i + 1] = t[#t - i + 1], t[i]
	end
end

local function set_to_list(set)
	list = {}
	for k, _ in pairs(set) do
		table.insert(list, k)
	end
	return list
end

local function list_to_set(list)
	set = {}
	for _, v in pairs(list) do
		set[v] = true
	end
	return set
end

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
-- handle jason data
--------------------------------

--[[ formats
input: a tree mess
output: [{name, path, mnemonic, pattern, fields[{ name, bit, len }] }]

jason warnings:
	- the A64 instruction data is inconsistent
	- some instructions may share encoding (because it wasn't specified specifically enough)
		- some instructions have different bits that hasn't been codified (idk why)
		- some instructions are specific to the zero register, and have been split (idk why)
	- Instruction.Instruction may have children (none when checked 2025/02/04)
	- Instruction.Encodeset.Field value may have set bits (none when checked 2025/02/04)

encodings:
	- always have range and value
		- bits provide bits
		- i don't think these can change going down the tree, because
		that would make a distinguishable bit indistinguishable (<- 17 characters)
	- fields provide name
		- these can have set bits
		- they also get overwritten going down the tree, so i'm iterating them backwards
		- idk if fields are allowed to overlap (none when checked 2025/02/04)
]]

--[[
i think i want to patch a bunch of N instructions to be 1 with more fields
this will be fewer instructions, thus smaller dfa
questions
- how much more expensive will parsing be?
case studies
- copy and set memory
	120 instructions that look the same (8/128 are unallocated)
	all listed separately like why
	op2 is actually a boolean matrix ABXY
		|              | read  | write |
		| non-temporal |   A   |   B   |
		| unprivileged |   X   |   Y   |
	(that these aren't documented as separate fields is dumb)
	op1 is just 00=prologue, 01=main, 10=epilogue
		and 11=set (this is special)
	op0 (actually called o0 why) is just "forward only or not"
	set is the same pattern but using different bits because...
		op0 is "with tags or not"
		op2 is AABB
			AA does what op1 did for copy (11 is unallocated)
			BB does what op2 BY did for copy
		with op0 and BB being 3 bits, when
			AA is 11, that explains the 8 missing instructions
	i think all of these values would be miniscule tables of text and
		even letters in some case, so we can just "create" the mnemonic lmao
]]

-- patches, because arm wants to be silly :)
local patches = {
	-- all of these leave size out until the instruction level
	-- but i can add them so the group pattern is filled
	sve_int_reduce_1         = {{ name = 'size', bit = 22, len = 2 }},
	sve_int_reduce_1q        = {{ name = 'size', bit = 22, len = 2 }},
	sve_int_bin_pred_shift_1 = {{ name = 'size', bit = 22, len = 2 }},
	sve_int_bin_pred_shift_2 = {{ name = 'size', bit = 22, len = 2 }},
	sve_int_bin_cons_shift_a = {{ name = 'size', bit = 22, len = 2 }},
	sve_int_ucmp_vi          = {{ name = 'size', bit = 22, len = 2 }},
}

local function parse_encoding(encodings, name)
	--[[ encoding
		i want to turn a list of ranges of bits/fields such as
			[{ value='0101', range={ bit=4, len=4 }}
			,{ value='xxxx', range={ bit=0, len=4 }}]
		into a pattern such as
			'0101____'
	]]
	local fields = {} -- list
	local pattern = {} -- list
	for i = 1,32 do pattern[i] = '-' end -- '-' placeholder

	-- bits (walk down the tree)
	for level = 1, #encodings do
		local set = encodings[level]
		for _, val in ipairs(set.values) do
			if val._type == 'Instruction.Encodeset.Bits' then
				local bit = val.range.start
				local len = val.range.width
				local str = val.value.value -- :)
				-- the value string is actually hi lo, but
				-- the pattern list is lo to hi, so reverse it
				str = string.reverse(str)
				for i = 1, len do
					-- we don't care about overriding previous bits
					-- also the string contains quotes, +1
					pattern[bit + i] = str:sub(i + 1, i + 1)
				end
			end
		end
	end

	-- i think i can just use the latest list for fields
	local set = encodings[#encodings]
	for _, val in ipairs(set.values) do
		if val._type == 'Instruction.Encodeset.Field' then
			local bit = val.range.start
			local len = val.range.width
			local str = val.value.value
			local name = val.name
			str = string.reverse(str)
			for i = 1, len do
				-- string contains quotes, +1
				local c = str:sub(i + 1, i + 1)
				-- field value might be constrained (bit not 'x')
				-- it might be this simple but who knows
				if c == 'x' then
					pattern[bit + i] = '_'
				else
					pattern[bit + i] = c
				end
			end
			field = {
				name = name,
				bit = bit,
				len = len,
			}
			table.insert(fields, field)
		end
	end

	-- patch in fields that arm felt they didn't need to specify even though
	-- it's the same for all the instructions in the group just like the register
	-- fields because they love people working with their big data
	if patches[name] then
		for _, field in ipairs(patches[name]) do
			table.insert(fields, field)
			local bit = field.bit
			local len = field.len
			for i = 1, len do
				pattern[bit + i] = '_'
			end
		end
	end

	pattern = table.concat(pattern, '')
	pattern = string.reverse(pattern)
	return pattern, fields
end

local function walk_A64(list, instructions, parents, encodings)
	--[[ i want to track the path through the tree
		this gives me families of instructions (SVE/SME/etc.)
		also build a list of each level's encoding ]]
	if not parents then parents = {} end
	if not encodings then encodings = {} end
	if not instructions then instructions = {} end

	local function get_path(parents)
		local path = {}
		for _, p in ipairs(parents) do table.insert(path, p.name) end
		return table.concat(path, '/')
	end

	for index, item in pairs(list) do
		table.insert(encodings, item.encoding)
		local pattern, fields = parse_encoding(encodings, item.name)
		--[[ continue traversing until all bits have been
			accounted for by all the encodings ]]
		if pattern:find('-') then
			table.insert(parents, item.name)
			walk_A64(item.children, instructions, parents, encodings)
			table.remove(parents)
		else
			--!! also parse constraints & children for decoding
			table.insert(instructions, {
				name = item.name,
				path = table.concat(parents, '/'),
				pattern = pattern,
				fields = fields,
				original = item,
			})
			instructions[item.name] = #instructions
		end
		table.remove(encodings)
	end
end

local file = assert(io.open('Instructions.json', 'r'))
local content = file:read('*all')
file:close()
timer('load')

local data = jason.decode(content)
timer('decode')

local instructions = {} -- list
walk_A64(data.instructions, instructions)
timer('walk')

if options.listing then
	print(jason.encode(instructions))
	timer('listing')
end



--------------------------------
-- once i was a dfa a little dfa
--------------------------------

-- the patterns scraped are (for my purposes) an nfa
-- '_' means any bit value
-- we can trim them down by removing trailing '_'s
local patterns = {} -- list
for _, inst in ipairs(instructions) do
	local pat = inst.pattern
	local lim = #pat
	for i = #pat, 1, -1 do
		if pat:sub(i, i) ~= '_' then
			lim = i
			break
		end
	end
	pat = pat:sub(1, lim)
	table.insert(patterns, pat)
end

--[[ terms
	Q     = finite set of states
	Sigma = finite set of input symbols, aka alphabet
	delta = set of all transitions, (state, input) -> state
	q0    = initial state
	F     = finite set of final (accepting) states
]]
local dfa = {
	Q = {}, -- set
	Sigma = {}, -- list
	delta = {}, -- set [state][input]=state
	q0 = 1,
	F = {}, -- set [state]=[names]
}
dfa.Q[dfa.q0] = true

-- generate the alphabet
local function genSigma(n)
	local Sigma = {}
	local function walk(n, l)
		if n == 0 then
			table.insert(Sigma, table.concat(l, ''))
		else
			table.insert(l, '0')
			walk(n - 1, l)
			l[#l] = '1'
			walk(n - 1, l)
			table.remove(l)
		end
	end
	walk(n, {})
	return Sigma
end
local alphabet_rank = 4
dfa.Sigma = genSigma(alphabet_rank)

-- initial state
local delta_id = 1 -- ids for transition states, they go up
local q0 = {} -- set
for i = 1, #patterns do q0[i] = true end
local worklist = {
	{active = q0, len = #patterns, index = 1, id = 1}
}

--!! idk if (2 patterns: 1 final, 1 unfinished) matters here
local function is_final(q)
	for i, _ in pairs(q.active) do
		local pat = patterns[i]
		-- pattern is done when index is past its end
		if q.index > #pat then
			return true
		end
	end
	return false
end

local function step_bit(q, bit)
	--[[ a pattern lives if 1/3 things happen
		pattern[q.index] == bit -- we find what we expect
		pattern[q.index] == '_' -- we find a singular 'any'
		q.index >= #pattern     -- we've reached permanent 'any'
	]]
	for i, _ in pairs(q.active) do
		local pat = patterns[i]
		if q.index <= #pat then
			local c = pat:sub(q.index, q.index)
			if c ~= '_' and c ~= bit then
				q.active[i] = nil
				q.len = q.len - 1
			end
		end
	end
	q.index = q.index + 1
end

local function step_input(q, input)
	for i = 1, #input do
		local bit = input:sub(i, i)
		step_bit(q, bit)
	end
end

--[[ we can cache an id by the active patterns and string index.
	if the same set of patterns are active, and
	the index is the same, then
	these states are indistinguishable ]]
local cache = {} -- list
while #worklist > 0 do
	-- do depth first search, by using latest work
	local q = table.remove(worklist)
	-- for each input
	for _, input in ipairs(dfa.Sigma) do
		-- we need a full copy to edit
		local q_prime = copy(q)
		step_input(q_prime, input)

		-- if any patterns are still alive
		if q_prime.len > 0 then
			local final = is_final(q_prime)

			-- have these alive patterns at this index been found already?
			local cached = false
			for _, old_q in ipairs(cache) do
				if q_prime.index == old_q.index and sets_equal(q_prime.active, old_q.active) then
					q_prime.id = old_q.id
					cached = true
					break
				end
			end

			-- if new, assign a new id
			if not cached then
				delta_id = delta_id + 1
				q_prime.id = delta_id
				local new_q = copy(q_prime)
				table.insert(cache, new_q)
			end

			-- we have a new transition
			if not dfa.delta[q.id] then
				dfa.delta[q.id] = {}
			end
			dfa.delta[q.id][input] = q_prime.id

			-- handle new state being added
			if not dfa.Q[q_prime.id] then
				dfa.Q[q_prime.id] = true
				-- is it a final state (any patterns ended?)
				if not final then
					table.insert(worklist, q_prime)
				else
					dfa.F[q_prime.id] = {}
					for i, _ in pairs(q_prime.active) do
						table.insert(dfa.F[q_prime.id], instructions[i].name)
					end
				end
			end
		end
	end
end
timer('dfa')



--------------------------------
-- cleanup
--------------------------------

-- minimisation?
--[[
revus
	sounds like wot i done did
	no work necessary i suppose
hopcroft
	P := {F, Q \ F}
	W := {F, Q \ F}
	while (W is not empty) do
		choose and remove a set A from W
		for each c in Σ do
			let X be the set of states for which a transition on c leads to a state in A
			for each set Y in P for which X ∩ Y is nonempty and Y \ X is nonempty do
				replace Y in P by the two sets X ∩ Y and Y \ X
				if Y is in W
					replace Y in W by the same two sets
				else
					if |X ∩ Y| <= |Y \ X|
						add X ∩ Y to W
					else
						add Y \ X to W
moore's
	Moore's algorithm for DFA minimization is due to Edward F. Moore (1956).
	Like Hopcroft's algorithm, it maintains a partition that starts off
	separating the accepting from the rejecting states, and repeatedly
	refines the partition until no more refinements can be made. At each
	step, it replaces the current partition with the coarsest common
	refinement of s + 1 partitions, one of which is the current one and the
	rest of which are the preimages of the current partition under the
	transition functions for each of the input symbols. The algorithm
	terminates when this replacement does not change the current partition.
	Its worst-case time complexity is O(n2s): each step of the algorithm may
	be performed in time O(ns) using a variant of radix sort to reorder the
	states so that states in the same set of the new partition are
	consecutive in the ordering, and there are at most n steps since each
	one but the last increases the number of sets in the partition. The
	instances of the DFA minimization problem that cause the worst-case
	behavior are the same as for Hopcroft's algorithm. The number of steps
	that the algorithm performs can be much smaller than n, so on average
	(for constant s) its performance is O(n log n) or even O(n log log n)
	depending on the random distribution on automata chosen to model the
	algorithm's average-case behavior.[6][9]
]]

--[[ final state fixing
	some final states point to multiple instructions
	which appear to be differentiated by one having a
	specific bit pattern, so i can remove the others as
	"not" having that pattern.
]]
-- these should "hijack" their final state
local proper_final_states = { -- set
	dp_1src_imm          = true,
	sve_mem_spill        = true,
	sve_mem_cstnt_ss     = true,
	sve_mem_cstnt_si     = true,
	sve_mem_64b_prfm_sv  = true,
	sve_mem_64b_prfm_sv2 = true,
	sve_mem_cldnt_ss     = true,
	sve_mem_cldnt_si     = true,
	sve_mem_prfm_si      = true,
	sve_mem_32b_pfill    = true,
	sve_mem_32b_fill     = true,
	asimdimm             = true,
	sve_int_dup_mask_imm = true,
}

-- each final state needs only one instruction
for s, ns in pairs(dfa.F) do
	if #ns > 1 then
		for _, n in ipairs(ns) do
			if proper_final_states[n] then
				dfa.F[s] = {n}
				break
			end
		end
		if #dfa.F[s] > 1 then
			-- we missed one
			for _, n in pairs(ns) do
				local i = instructions[instructions[n]]
				print('\t'..i.pattern, i.path..'/'..i.name)
			end
			print('\t^^^ missed a busy final state')
		end
	end
end

-- each instruction needs only one final state
local function fix(dfa, s, id)
	for a, ib in pairs(dfa.delta) do
		for i, b in pairs(ib) do
			if b == s then
				dfa.delta[a][i] = id
			end
		end
	end
	dfa.Q[s] = nil
	dfa.F[s] = nil
end

-- reassign node ids to keep them linear and
-- to hopefully bundle the deltas closer together
-- this could help with compression
local function map_ids(old, idmap)
	local new = {Q = {}, F = {}, delta = {}}
	for old_a, _ in pairs(old.Q) do
		-- state
		local new_a = idmap[old_a]
		new.Q[new_a] = true
		-- deltas
		local old_delta = old.delta[old_a]
		if old_delta then
			new.delta[new_a] = {}
			for i, b in pairs(old_delta) do
				new.delta[new_a][i] = idmap[b]
			end
		else -- final states
			new.F[new_a] = old.F[old_a]
		end
	end
	old.Q = new.Q
	old.F = new.F
	old.delta = new.delta
end
-- dumb breadth-first assignment
--!! maybe assign delta then final
local newids = {}
local visited = {}
local towalk = {1}
while #towalk > 0 do
	local id = table.remove(towalk, 1)
	table.insert(newids, id)
	if dfa.delta[id] then
		for _, S in ipairs(dfa.Sigma) do
			local child = dfa.delta[id][S]
			if child and not visited[child] then
				table.insert(towalk, child)
				visited[child] = true
			end
		end
	end
end
local idmap = {}
for i, v in ipairs(newids) do idmap[v] = i end
map_ids(dfa, idmap)

-- i can also make a map between states<->names for ease
local final_states = {} -- set[state]=name & set[name]=state
for s, ns in pairs(dfa.F) do
	local name = ns[1]
	final_states[s] = name
	if final_states[name] == nil then
		final_states[name] = s
	else
		fix(dfa, s, final_states[name])
	end
end

timer('cleanup')



--------------------------------
-- generation C (zoomer)
--------------------------------

--[[ test checklist
	transposed matrix
		delta[state][input] vs delta[input][state]
	early out vs always 8 iterations
		final states need transitions to themselves
]]

local cdfa = {}
local function cgen(str)
	table.insert(cdfa, str)
end

-- prefix
cgen(string.format([[
/*************\
|* GENERATED *|
\*************/
#include <stdint.h>
// provides mask of N bits
#define BIT_MASK(N) ((1 << N) - 1)
// get N bits from value V at offset O
#ifndef GET_BITS
#define GET_BITS(O, N, V) (((V) >> O) & MASK(N))
#endif
#define ALPHABET_RANK %d]], alphabet_rank))

-- decode functions
for _, group in ipairs(instructions) do
	cgen(string.format('static void %s(uint32_t inst) {', group.name))
	-- group name
	cgen(string.format('\tchar* name = "%s";', group.name))
	-- group fields
	for _, f in ipairs(group.fields) do
		cgen(string.format('\tuint32_t %s = GET_BITS(%d, %d, inst);', f.name, f.bit, f.len))
	end
	-- separate instructions
	print(group.name)
	for _, inst in ipairs(group.original.children) do
		local str = {}
		table.insert(str, string.format('\t%s', inst.name))
		for _, e in ipairs(inst.encoding.values) do
			if e._type == 'Instruction.Encodeset.Bits' then
				-- find which field
				local field = nil
				for _, f in ipairs(group.fields) do
					if f.bit == e.range.start then
						field = f
						break
					end
				end
				if field then
					table.insert(str, string.format('%s==%s', field.name, e.value.value))
				else
					table.insert(str, string.format('[%d:%d]==%s', e.range.start, e.range.width, e.value.value))
				end
			end
		end
		--print(table.concat(str, ', '))
	end
	-- print test
	local str = {}
    str[#str + 1] = "    printf(\"";
    str[#str + 1] = "name = %s";

	for _, f in ipairs(group.fields) do
	    str[#str + 1] = string.format("\\t%s = %%d", f.name)
	end
    str[#str + 1] = "\", name";
	for _, f in ipairs(group.fields) do
	    str[#str + 1] = string.format(", %s", f.name)
	end
    str[#str + 1] = ");";
	cgen(table.concat(str, ""))
	cgen('}')
end

-- decode table
cgen(string.format('static void (*names[%d])(uint32_t) = {', delta_id + 1))
-- cgen(string.format('char* names[%d] = {', delta_id + 1))
for i, n in pairs(dfa.F) do
	cgen(string.format('\t[%d] = %s,', i, n[1]))
end
cgen('};')

cgen([[
typedef struct {
    uint32_t lsb;
    uint8_t edges[16];
} DFAState;
]])

-- delta table
local delta_def = string.format('DFAState delta[%d] = {', delta_id + 1)
local delta_part = function (a, i, b) return string.format('\t[%d][0b%s] = %d,', a, i, b) end

cgen(string.format(delta_def, delta_id + 1))
for a, _ in pairs(dfa.Q) do
	if dfa.delta[a] then
	    local lsb = "0"
		for _,S in pairs(dfa.Sigma) do
			local b = dfa.delta[a][S]
			if b then
				local z = tonumber(S, 2)
				local dt = (b - a) + 512
				assert(dt >= 0)

				local x = bit.band(dt, 3)
				if a == 4 then print(i, a, b, x) end
				lsb = lsb..string.format(" | (%du << %du)", x, z*2)
			end
		end

		cgen(string.format("    [%d] = { .lsb = %s, .edges = { ", a, lsb))
		for _,S in pairs(dfa.Sigma) do
			local b = dfa.delta[a][S]
			if b then
    			local dt = (b - a) + 512
    			assert(dt >= 0)

    			local x = bit.rshift(dt, 2)
    			local y = bit.band(dt, 3)
    			cgen(string.format("        [0b%s] = %d, // %d (%d)", S, x, b, y))
            else
    			cgen(string.format("        [0b%s] = 128,", S))
            end
		end
		cgen(string.format("    }},"))
	else
		cgen(string.format("    [%d] = { .edges = { ", a))
		for _,S in pairs(dfa.Sigma) do
			local x = bit.rshift(512, 2)
			cgen(string.format("        [0b%s] = %d,", S, x))
		end
		cgen(string.format("    }},"))
	end
end
cgen('};')

-- walk function
cgen([[
uint32_t walk(uint32_t inst) {
	uint32_t state = 1, key;
    int32_t dt;
    DFAState* curr;

    int shift = 32;
    while (shift > 0) {
        shift -= ALPHABET_RANK;
        curr = &delta[state];
        // unpack 10bit edge
        key  = (inst >> shift) & BIT_MASK(ALPHABET_RANK);
        dt   = (((uint32_t) curr->edges[key] << 2) | ((curr->lsb >> (key*2)) & 3)) - 512;
        // early out
        if (dt == 0) { return state; }
        state += dt;
    }
    return state;
}

]])

-- write it
local file = assert(io.open('a64dfa.h', 'w'))
file:write(table.concat(cdfa, '\n'))
file:close()
timer('c-nile')



--------------------------------
-- graph visualisation
--------------------------------

if options.graphviz then
	local strings = {}
	table.insert(strings, 'digraph G {')
	table.insert(strings, '\trankdir=LR')
	for a, ib in pairs(dfa.delta) do
		local labels = {}
		for i, b in pairs(ib) do
			if dfa.F[b] then
				b = dfa.F[b][1]
			end
			if not labels[b] then
				labels[b] = i
			else
				labels[b] = labels[b] .. ',' .. i
			end
		end
		for b, l in pairs(labels) do
			table.insert(strings, '\t' .. a .. ' -> ' .. b .. ' [label="' .. l .. '"]')
		end
	end
	table.insert(strings, '}')
	print(table.concat(strings, '\n'))
	timer('graphviz')
end



--------------------------------
-- data analysis
--------------------------------

if options.analysis then
	--------------------------------
	-- arm analysis first
	--------------------------------
	print('\nARM analysis:')

	-- how many patterns and duplicates?
	local patset = {}
	local patdup = {}
	for _, i in ipairs(instructions) do
		if patset[i.pattern] then
			patdup[i.path..'/'..i.name] = i.pattern
			patdup[patset[i.pattern]] = i.pattern
		end
		patset[i.pattern] = i.path..'/'..i.name
	end
	local unique_patterns = set_to_list(patset)
	print(string.format('number of instructions = %d', #instructions))
	print(string.format('number of patterns     = %d', #patterns))
	print(string.format('unique patterns        = %d', #unique_patterns))
	-- for k, v in pairs(patdup) do
	-- 	print(v, k)
	-- end

	-- are any groups not actually groups?
	-- are there any child groups? (hope not)
	-- do any instructions have aliases?
	-- are any patterns shared by their children?
	local not_groups = 0
	local insts_with_alias = 0
	local groups_with_sub_groups = {}
	local inst_shared_patterns = {}
	for _, group in ipairs(instructions) do
		local parent_pattern = group.pattern
		if group.original._type ~= 'Instruction.InstructionGroup' then
			not_groups = not_groups + 1
		else
			for _, inst in ipairs(group.original.children) do
				if inst._type == 'Instruction.InstructionGroup' then
					table.insert(groups_with_sub_groups, group.name)
				else
					if #inst.children > 0 then
						insts_with_alias = insts_with_alias + 1
					end
				end

				local pattern = {}
				for c in parent_pattern:gmatch('.') do
					table.insert(pattern, c)
				end
				local bits_count = 0
				for _, bits in ipairs(inst.encoding.values) do
					if bits._type == 'Instruction.Encodeset.Bits' then
						bits_count = bits_count + 1
						local bit = bits.range.start
						local len = bits.range.width
						local str = bits.value.value
						for i = 1, len do
							local c = str:gsub(#str - i, #str - i)
							pattern[#pattern - (bit + (i - 1))] = c
						end
					end
				end
				if table.concat(pattern, '') == parent_pattern then
					if not inst_shared_patterns[group.name] then
						inst_shared_patterns[group.name] = {}
					end
					table.insert(inst_shared_patterns[group.name], inst.name)
				end
			end
		end
	end
	print(string.format('patterns that are insts = %d', not_groups))
	print(string.format('instructions with alias = %d', insts_with_alias))
	print(string.format('groups with sub groups  = %s', dump(groups_with_sub_groups)))
	print(inspect(inst_shared_patterns))



	--------------------------------
	-- dfa analysis second
	--------------------------------
	print('\nDFA analysis:')

	-- how many nodes?
	local total_nodes = 0
	local final_nodes = 0
	local transition_nodes = 0
	for _, _ in pairs(dfa.Q) do
		total_nodes = total_nodes + 1
	end
	for _, _ in pairs(dfa.F) do
		final_nodes = final_nodes + 1
	end
	for _, _ in pairs(dfa.delta) do
		transition_nodes = transition_nodes + 1
	end
	print(string.format('total nodes      = %d', total_nodes))
	print(string.format('final nodes      = %d', final_nodes))
	print(string.format('transition nodes = %d', transition_nodes))

	-- what is the table size?
	local table_size = 2 * (delta_id + 1) * (2 ^ alphabet_rank)
	local k_table_size = math.floor(table_size / 1024)
	print(string.format('delta table size = %d bytes (%dk)', table_size, k_table_size))

	-- how many edges?
	-- what's the biggest difference in dfa.delta[A][i]=B between A and B?
	-- how many nodes have how many total transitions?
	local function delta_walk(dfa)
		local edges = 0
		local max_delta = 0
		local histogram = {}
		for a, ib in pairs(dfa.delta) do
			local edge_count = 0
			for i, b in pairs(ib) do
				edge_count = edge_count + 1
				local dt = math.abs(b - a)
				if dt > max_delta then
					max_delta = dt
				end
			end
			edges = edges + edge_count
			if not histogram[edge_count] then
				histogram[edge_count] = 0
			end
			histogram[edge_count] = histogram[edge_count] + 1
		end
		return max_delta, edges, histogram
	end
	local biggest_delta, total_edges, edge_histogram = delta_walk(dfa)
	print(string.format('total edges      = %d', total_edges))
	print(string.format('transition histogram %s', dump(edge_histogram)))

	-- what is each node's tree size?
	-- what nodes are at which depth?
	-- how many nodes are at each depth?
	local function walk(dfa) -- sizes, depth, depth histogram
		local sizes = {}
		local hist = {}
		local depths = {}
		local function walk_ids(id, level)
			local size = 1
			if not depths[level] then
				depths[level] = {}
			end
			depths[level][id] = true
			if not hist[level] then
				hist[level] = 0
			end
			hist[level] = hist[level] + 1
			if dfa.delta[id] then
				for _, b in pairs(dfa.delta[id]) do
					if not depths[level + 1] or not depths[level + 1][b] then
						size = size + walk_ids(b, level + 1)
					else
						size = size + sizes[b]
					end
				end
			end
			sizes[id] = size
			return size
		end
		walk_ids(1, 1)
		return sizes, depths, hist
	end
	local node_sizes, node_depths, depth_hist = walk(dfa)
	print(string.format('total tree size  = %d', node_sizes[1]))
	print(string.format('depth histogram %s', dump(depth_hist)))

	-- what depths are the final states?
	local final_depths = copy(node_depths)
	local final_hists = {}
	for l, ns in ipairs(final_depths) do
		for n, _ in pairs(ns) do
			if not dfa.F[n] then
				final_depths[l][n] = nil
			end
		end
		local list = set_to_list(final_depths[l])
		final_hists[l] = #list
	end
	print(string.format('final node depth = %s', dump(final_hists)))

	-- what is the average depth of the final nodes
	local avg_depth = 0
	for i, v in pairs(final_hists) do
		avg_depth = avg_depth + (i - 1) * v
	end
	avg_depth = avg_depth / final_nodes
	print(string.format('average steps to final node = %f', avg_depth))

	-- i just wanna make a point graph of node vs depth
	local function idgraph(filename, dfa)
		_, node_depths, _ = walk(dfa)
		local file = assert(io.open(filename, 'w'))
		for level, set in ipairs(node_depths) do
			for node, _ in pairs(set) do
				file:write(string.format('%d,%d\n', node, level))
			end
		end
		file:close()
	end
	-- idgraph('bumbread.csv', dfa)

	-- calculate predecessors
	local parents = {}
	for a, ib in pairs(dfa.delta) do
		for i, b in pairs(ib) do
			if not parents[b] then parents[b] = {} end
			parents[b][a] = true
		end
	end

	-- yasser suggested a fancier algorithm for minimising the deltas
	-- performing worse atm for some reason
	local ready = {1}
	local up_there = {}
	local order = {}
	local n2ord = {}
	local function is_ready(n)
		for p, _ in pairs(parents[n]) do
			if not n2ord[p] then
				return false
			end
		end
		return true
	end
	local function pick_best(ready)
		local reach = #order + 1
		local index = 1
		local delta = 0
		local children = 0
		for i, n in ipairs(ready) do
			if parents[n] then
				for p, _ in pairs(parents[n]) do
					local d = reach - n2ord[p]
					if d > delta then
						index = i
						delta = d
						children = 0
					end
					if d == delta then
						local cset = {}
						if dfa.delta[n] then
							for _, c in pairs(dfa.delta[n]) do
								cset[c] = true
							end
						end
						local clist = set_to_list(cset)
						local cnum = #clist
						if cnum > children then
							index = i
							children = cnum
						end
					end
				end
			end
		end
		return table.remove(ready, index)
	end
	while #ready > 0 do
		local n = pick_best(ready)
		-- we need the ordinal for our "best" node calc
		table.insert(order, n)
		n2ord[n] = #order
		-- after a node is retired, it'll ready up new nodes
		if dfa.delta[n] then
			for _, S in ipairs(dfa.Sigma) do
				succ = dfa.delta[n][S]
				if succ and is_ready(succ) and not up_there[succ] then
					up_there[succ] = true
					ready[#ready + 1] = succ
				end
			end
		end
	end
	local idmap = {}
	for i, v in ipairs(order) do idmap[v] = i end
	local test_dfa = copy(dfa)
	map_ids(test_dfa, idmap)
	local biggest_delta_supersmart, _, _ = delta_walk(test_dfa)
	-- idgraph('smarest.csv', dfa)

	print(string.format('biggest delta (dumb bread)  = %d', biggest_delta))
	print(string.format('biggest delta (super smart) = %d', biggest_delta_supersmart))

	--!! i can rework this to handle any alphabet size
	-- how many transitions happen in pairs/quads/octs/all
	-- that is, how many inputs ignore the low bits
	-- this is specific to stepping 4 bits
	if #dfa.Sigma[1] == 4 then
		local alls_count = 0
		local octs_count = 0
		local quad_count = 0
		local pair_count = 0
		local octs = {
			{'0000', '0001', '0010', '0011', '0100', '0101', '0110', '0111'},
			{'1000', '1001', '1010', '1011', '1100', '1101', '1110', '1111'},
		}
		local quads = {
			{'0000', '0001', '0010', '0011'},
			{'0100', '0101', '0110', '0111'},
			{'1000', '1001', '1010', '1011'},
			{'1100', '1101', '1110', '1111'},
		}
		local pairings = {
			{'0000', '0001'}, {'0010', '0011'},
			{'0100', '0101'}, {'0110', '0111'},
			{'1010', '1001'}, {'1010', '1011'},
			{'1110', '1101'}, {'1110', '1111'},
		}
		for a, ib in pairs(dfa.delta) do
			local allsed = true
			local octsed = true
			local quaded = true
			local paired = true
			for _, S in ipairs(dfa.Sigma) do
				if ib[dfa.Sigma[1]] ~= ib[S] then
					allsed = false
					break
				end
			end
			for _, o in ipairs(octs) do
				for _, i in ipairs(o) do
					if ib[o[1]] ~= ib[i] then
						octsed = false
						break
					end
				end
				if not octsed then break end
			end
			for _, q in ipairs(quads) do
				for _, i in ipairs(q) do
					if ib[q[1]] ~= ib[i] then
						quaded = false
						break
					end
				end
				if not quaded then break end
			end
			for _, p in ipairs(pairings) do
				if ib[p[1]] ~= ib[p[2]] then
					paired = false
					break
				end
			end
			if allsed then
				alls_count = alls_count + 1
			elseif octsed then
				octs_count = octs_count + 1
			elseif quaded then
				quad_count = quad_count + 1
			elseif paired then
				pair_count = pair_count + 1
			end
		end
		print(string.format('total nodes paired = %d', pair_count))
		print(string.format('total nodes quaded = %d', quad_count))
		print(string.format('total nodes octsed = %d', octs_count))
		print(string.format('total nodes allsed = %d', alls_count))
		print(string.format('                   = %d', pair_count + quad_count + octs_count + alls_count))
	end

	timer('data analysis')
end



--[[ timings
getting here takes
	lua    around 4.9 seconds
	luajit around 2.5 seconds (no jit)
	luajit around 1.2 seconds (ye jit)
]]
if options.timings then
	local ttime_string = string.format('%08.4f', os.clock() - total_time)
	print('total time: ' .. ttime_string)
end
