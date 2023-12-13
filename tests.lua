local tally = 0
local succ  = 0

function test_single(path, flags)
    tally = tally + 1

    if not os.execute(string.format("cuik tests/collection/%s %s -o tests/collection/foo.exe", path, flags)) then
        print("  NAY COMPILE "..flags)
        return
    end

    os.execute("tests\\collection\\foo.exe > tests/collection/foo.txt", path)
    local exit = os.execute(string.format("git diff -b --no-index tests/collection/foo.txt tests/collection/%s.gold", path))
    if exit ~= 0 then
        print("  NAY "..flags)
    else
        print("  Yay "..flags)
        succ = succ + 1
    end
end

function test(path, flags)
    print("testing "..path.."...")
    test_single(path, "")
    test_single(path, "-g")
    test_single(path, "-O1")
    test_single(path, "-O1 -g")
end

test("crc32.c")

print(string.format("run %d / %d", succ, tally))
