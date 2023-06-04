function test(file)
	local f = io.open(file, "rb")

	-- find expected list
	local expected = {}
	for l in f:lines() do
		local comment = l:match('//#(.*)')
		if comment ~= nil then
			expected[#expected + 1] = comment
		end
	end

	f:close()

	-- run compiler
	local cmd = "cuik "..file.." -o test/a.out 2>&1"
	print(cmd)

	local compiler_result = io.popen(cmd)
	local got = {}
	for l in compiler_result:lines() do
		got[#got + 1] = l
	end

	local exit = compiler_result:close()
	if exit ~= 0 then
		print("Failed to compile "..file)
		print("Output:")
		for i, l in ipairs(got) do
			print(l)
		end
		os.exit(1)
	end

	local correct = true
	for i, l in ipairs(got) do
		if not supress then
			if i <= #expected then
				-- not enough expected
				print("output is too short!")
			elseif expected[i] ~= l then
				print("line "..i.." is incorrect:")
				print("  expected: '"..expected[i].."'")
				print("  got:      '"..l.."'")
				correct = false
			end
		end
	end

	if not correct then
		os.exit(1)
	end
end

test("tests/hello_world.c")

print("Hello")
