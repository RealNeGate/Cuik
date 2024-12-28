local tally = 0
local succ  = 0

function test_single(path, args, flags)
    tally = tally + 1

    if not os.execute(string.format("cuik tests/collection/%s %s -o tests/collection/foo.exe", path, flags)) then
        print("  NAY COMPILE "..flags)
        return
    end

    os.execute("tests\\collection\\foo.exe "..args.." > tests/collection/foo.txt")
    local exit = os.execute(string.format("git diff -b --no-index tests/collection/foo.txt tests/collection/%s.gold", path))
    if exit ~= 0 then
        print("  NAY "..flags)
    else
        print("  Yay "..flags)
        succ = succ + 1
    end
end

function test(path, args, flags)
    print("testing "..path.."...")
    test_single(path, args, "")
    test_single(path, args, "-g")
    test_single(path, args, "-O1")
    test_single(path, args, "-O1 -g")
end

test("crc32.c", "")
test("mur.c", "tests/collection/mur.c")

print(string.format("run %d / %d", succ, tally))
