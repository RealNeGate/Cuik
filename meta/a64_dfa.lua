--[[ TODO
	! add multiple names for final states, as there are duplicate patterns
	? might need to patch specific instructions before moving on to dfa
		- arm doesn't like defining their shit accurately :)
		AUTIA_64P_dp_1src,   AUTIZA_64Z_dp_1src
		AUTIB_64P_dp_1src,   AUTIZB_64Z_dp_1src
		AUTDA_64P_dp_1src,   AUTDZA_64Z_dp_1src
		AUTDB_64P_dp_1src,   AUTDZB_64Z_dp_1src
		PACIA_64P_dp_1src,   PACIZA_64Z_dp_1src
		PACIB_64P_dp_1src,   PACIZB_64Z_dp_1src
		PACDA_64P_dp_1src,   PACDZA_64Z_dp_1src
		PACDB_64P_dp_1src,   PACDZB_64Z_dp_1src
		REV16_64_dp_1src,    RBIT_64_dp_1src
		BRAAZ_64_branch_reg, RETAA_64E_branch_reg, RETAB_64E_branch_reg
		BLR_64_branch_reg,   RETAA_64E_branch_reg, RETAASPPCR_64M_branch_reg, RETAB_64E_branch_reg, RETABSPPCR_64M_branch_reg
		REV_32_dp_1src,      RBIT_32_dp_1src
	? trim edges where a node can only reach one final state
]]

local jason = require('jason')
local inspect = require('inspect')

--------------------------------
-- helper schtuff
--------------------------------

local print_listings = false
local print_graphviz = false
local timer_printing = true
local data_analysis = true

local total_time = os.clock()
local timestamp = os.clock()
local function timer(string)
	if timer_printing then
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
		into a pattern such as '0101____'
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
			-- again reverse the string
			str = string.reverse(str)
			for i = 1, len do
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

local function walk_A64(list, instructions, parent, encodings)
	--[[ i want to track the path through the tree
		this gives me families of instructions (SVE/SME/etc.)
		also build a list of each level's encoding ]]
	if not parent then parent = {} end
	if not encodings then encodings = {} end
	if not instructions then instructions = {} end

	for index, item in pairs(list) do
		table.insert(encodings, item.encoding)
		local pattern, fields = parse_encoding(encodings, item.name)
		--[[ continue traversing until all bits have been
			accounted for by all the encodings ]]
		if pattern:find('-') then
			table.insert(parent, item.name)
			walk_A64(item.children, instructions, parent, encodings)
			table.remove(parent)
		else
			table.insert(instructions, {
				name = item.name,
				path = table.concat(parent, '/'),
				pattern = pattern,
				fields = fields,
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

if print_listings then
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
	Sigma = { -- list
		-- '0', '1',
		-- '00', '01', '10', '11',
		-- '000', '001', '010', '011',
		-- '100', '101', '110', '111',
		'0000', '0001', '0010', '0011',
		'0100', '0101', '0110', '0111',
		'1000', '1001', '1010', '1011',
		'1100', '1101', '1110', '1111',
		-- '00000', '00001', '00010', '00011',
		-- '00100', '00101', '00110', '00111',
		-- '01000', '01001', '01010', '01011',
		-- '01100', '01101', '01110', '01111',
		-- '10000', '10001', '10010', '10011',
		-- '10100', '10101', '10110', '10111',
		-- '11000', '11001', '11010', '11011',
		-- '11100', '11101', '11110', '11111',
	},
	delta = {}, -- set [state][input]=state
	q0 = 1,
	F = {}, -- set [state]=[names]
}
dfa.Q[dfa.q0] = true

-- initial state
local delta_id = 1 -- ids for transition states, they go up
local final_id = 0 -- ids for final states, they go backwards
local q0 = {} -- set
for i = 1, #patterns do q0[i] = true end
local worklist = {
	{active = q0, len = #patterns, index = 1, id = 1}
}

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
			for _, old_q in pairs(cache) do
				if q_prime.index == old_q.index and sets_equal(q_prime.active, old_q.active) then
					q_prime.id = old_q.id
					cached = true
					break
				end
			end
			
			-- if new, assign a new id
			if not cached then
				-- if final then
					-- final_id = final_id - 1
					-- q_prime.id = final_id
				-- else
					delta_id = delta_id + 1
					q_prime.id = delta_id
				-- end
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

if print_graphviz then
	local function graphviz(dfa)
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
		return table.concat(strings, '\n')
	end
	print(graphviz(dfa))
	timer('graphviz')
end



--------------------------------
-- data analysis
--------------------------------

if data_analysis then
	-- how many patterns and duplicates?
	local patset = {}
	local patdup = {}
	for _, i in ipairs(instructions) do
		if patset[i.pattern] then
			patdup[i.path..'/'..i.name] = i.pattern
			-- print(inspect(i))
			patdup[patset[i.pattern]] = i.pattern
		end
		patset[i.pattern] = i.path..'/'..i.name
	end
	local unique_patterns = set_to_list(patset)
	print(string.format('number of instructions = %d', #instructions))
	print(string.format('number of patterns     = %d', #patterns))
	print(string.format('unique patterns        = %d', #unique_patterns))
	-- print(string.format('duplicate patterns %s', inspect(patdup)))
	for k, v in pairs(patdup) do
		print(v, k)
	end
	
	-- how many unique field groups?
	local fields = {}
	for _, i in ipairs(instructions) do
		local mask = i.pattern:gsub('1', '0')
		if not fields[mask] then
			fields[mask] = {0}
		end
		fields[mask][1] = fields[mask][1] + 1
		table.insert(fields[mask], i.name)
	end
	print(string.format('unique field patterns  = %d', #set_to_list(fields)))
	-- print(inspect.inspect(fields))
	-- error()

	-- how many nodes?
	local total_nodes = 0
	for _, _ in pairs(dfa.Q) do
		total_nodes = total_nodes + 1
	end
	local final_nodes = 0
	for _, _ in pairs(dfa.F) do
		final_nodes = final_nodes + 1
	end
	-- 25,389,524 cache no
	--      7,939 cacne ye
	
	-- how many nodes
	print(string.format('total nodes      = %d', total_nodes))
	print(string.format('final nodes      = %d', final_nodes))
	print(string.format('transition nodes = %d', total_nodes - final_nodes))
	-- final_id is negative, so we subtract
	-- assert(total_nodes == delta_id - final_id, 'total nodes should be the sum of both ids')

	-- how many transitions happen in pairs/quads/octs/all
	-- that is, how many inputs ignore the low bits
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

	-- how many names share a final state with other names
	local duped_names = {}
	for _, s in pairs(dfa.F) do
		if #s > 1 then
			for _, n in pairs(s) do
				print(inspect(instructions[instructions[n]]))
				duped_names[n] = true
			end
		end
	end
	local duped_names_count = 0
	for _, _ in pairs(duped_names) do
		duped_names_count = duped_names_count + 1
	end
	print(string.format('names sharing final states with others = %d', duped_names_count))

	-- how many edges?
	-- what's the biggest transition id delta?
	-- how many nodes have how many total transitions?
	local function delta_walk(dfa)
		local edges = 0
		local max_delta = 0
		local histogram = {}
		for a, ib in pairs(dfa.delta) do
			local edge_count = 0
			for i, b in pairs(ib) do
				edge_count = edge_count + 1
				if b - a > max_delta then
					max_delta = b - a
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
	print(string.format('total edges                 = %d', total_edges))
	print(string.format('transition histogram %s', dump(edge_histogram)))

	-- how are the node ids assigned?
	-- i think i can use node sizes to assign nodes better
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
	print(string.format('total tree size = %d', node_sizes[1]))
	print(string.format('depth histogram %s', dump(depth_hist)))

	-- where are the final states
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
	print(string.format('final node depths = %s', dump(final_hists)))

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
	-- idgraph('normal.csv', dfa)

	local function new_delta(dfa, idmap)
		local newdelta = {}
		for a, ib in pairs(dfa.delta) do
			newdelta[idmap[a]] = {}
			for i, b in pairs(ib) do
				newdelta[idmap[a]][i] = idmap[b]
			end
		end
		return newdelta
	end

	-- calculate predecessors
	local parents = {}
	for a, ib in pairs(dfa.delta) do
		for i, b in pairs(ib) do
			if not parents[b] then parents[b] = {} end
			parents[b][a] = true
		end
	end
	-- for c, ps in pairs(parents) do
	-- 	parents[c] = set_to_list(ps)
	-- 	table.sort(parents[c])
	-- end

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
	dfa.delta = new_delta(dfa, idmap)
	local biggest_delta_supersmart, _, _ = delta_walk(dfa)
	-- idgraph('smarest.csv', dfa)
	-- dfa.delta = new_delta(dfa, order)
	--[[
	-- try using node sizes to sort nodes for bread-first search
	local newids = {}
	for level, nodes in ipairs(node_depths) do
		local list = set_to_list(nodes)
		table.sort(list, function(a,b)
			return node_sizes[a] < node_sizes[b]
		end)
		for _, n in ipairs(list) do
			table.insert(newids, n)
		end
	end
	local idmap = {}
	for i, v in ipairs(newids) do idmap[v] = i end
	dfa.delta = new_delta(dfa, idmap)
	local biggest_delta_smart_bread, _, _ = delta_walk(dfa)
	idgraph('smartbread.csv', dfa)
	dfa.delta = new_delta(dfa, newids)

	-- depth-first ids, reverse post-order
	local newids = {}
	local visited = {}
	local function traverse(id)
		if dfa.delta[id] then
			for _, S in ipairs(dfa.Sigma) do
				local child = dfa.delta[id][S]
				if child and not visited[child] then
					traverse(child)
				end
			end
		end
		visited[id] = true
		table.insert(newids, id)
	end
	traverse(1)
	reverse(newids)
	local idmap = {}
	for i, v in ipairs(newids) do idmap[v] = i end
	dfa.delta = new_delta(dfa, idmap)
	local biggest_delta_deepd, _, _ = delta_walk(dfa)
	idgraph('deepd.csv', dfa)
	dfa.delta = new_delta(dfa, newids)
	]]
	-- dumb breadth-first assignment
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
	dfa.delta = new_delta(dfa, idmap)
	local biggest_delta_dumb_bread, _, _ = delta_walk(dfa)
	-- idgraph('bread.csv', dfa)
	-- dfa.delta = new_delta(dfa, newids)
	--[[
	-- what's the biggest transition id delta?
	print(string.format('biggest delta (dumb deeped) = %d', biggest_delta_deepd))
	print(string.format('biggest delta (smart bread) = %d', biggest_delta_smart_bread))
	]]
	print(string.format('biggest delta (normal)      = %d', biggest_delta))
	print(string.format('biggest delta (dumb bread)  = %d', biggest_delta_dumb_bread))
	print(string.format('biggest delta (super smart) = %d', biggest_delta_supersmart))

	timer('data analysis')
end

--------------------------------
-- cleanup
--------------------------------

-- might be able to delta compress if
-- i turn depth-first node numbers to
-- bread-first node numbers

-- minimisation?
--[[
revus
	sounds like wot i done did
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



--------------------------------
-- C generation
--------------------------------

local names_final_states = {}
for q, ns in pairs(dfa.F) do
	for _, n in pairs(ns) do
		if not names_final_states[n] then
			names_final_states[n] = {}
		end
		table.insert(names_final_states[n], q)
	end
end
-- print(jason.encode(names_final_states))

-- make a table of names->final states

local cdfa = {}
table.insert(cdfa, '#include <stdint.h>')
table.insert(cdfa, string.format('int16_t dfa[16][%d] = {', delta_id + 1))
for a, ib in pairs(dfa.delta) do
	for i, b in pairs(ib) do
		table.insert(cdfa, string.format('\t[0b%s][%d] = %d,', i, a, b))
	end
end
table.insert(cdfa, '};')
local file = assert(io.open('a64dfa.c', 'w'))
file:write(table.concat(cdfa, '\n'))
file:close()
timer('c-nile')





--[[ timings
getting here takes
	lua    around 30 seconds
	luajit around 15 seconds (no jit)
	luajit around 8  seconds (ye jit)
]]
if timer_printing then
	local ttime_string = string.format('%08.4f', os.clock() - total_time)
	print('total time: ' .. ttime_string)
end
