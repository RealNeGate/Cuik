return function (f)
	local changes = 0

    for n in tb.all_nodes_iter(f) do
        if tb.is_int_binop(n) then
			local a, b = tb.get_binops(n)

			if tb.is_izero(b) then
            	if tb.is_imul(n) then
					tb.set_pass(n, b) changes = 1
				elseif tb.is_iadd(n) then
					tb.set_pass(n, a) changes = 1
				elseif tb.is_idiv(n) then
					tb.set_poison(n) changes = 1
				end
			end
        end
    end

    -- tb.print_func(f)
    return changes
end

