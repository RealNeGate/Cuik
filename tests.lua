local tally = 0
local succ  = 0

function test(path)
    tally = tally + 1

    if not os.execute(string.format("cuik tests/collection/%s -g -o tests/collection/foo.exe", path)) then
        print("NAY COMPILE "..path)
        return
    end

    os.execute("tests\\collection\\foo.exe > tests/collection/foo.txt", path)
    local exit = os.execute(string.format("git diff -b --no-index tests/collection/foo.txt tests/collection/%s.gold", path))
    if exit ~= 0 then
        print("NAY "..path)
    else
        print("Yay "..path)
        succ = succ + 1
    end
end

print(test("hello.c"))
print(string.format("run %d / %d", succ, tally))
