return function (f)
    for n in tb.all_nodes_iter(f) do
        if tb.is_int_binop(n) then
            print("r"..n.reg.." : "..tostring(n))
        end
    end

    tb.print_func(f)
    return 0
end

-- example strength reduction
--[[
for n in f:all_nodes_iter(f) do
    if n:is_int_binop() then
        local t = n:type()
        local a, b = n:getbinops()

        if b:is_izero() then
            if n:is_imul() then
                f:set_pass(n, b)
            else if n:is_iadd() then
                -- we're taking an existing node and replacing it
                f:set_pass(n, a)
            else if n:is_idiv() then
                -- this works for both types of idiv
                f:set_undef(n)
            end
        end
    end
end
--]]

