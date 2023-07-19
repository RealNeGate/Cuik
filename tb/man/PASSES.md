The optimizer is assembled by telling every function which passes to apply
and then synchronizing to do module level passes

	-func -mod -func

	thread 1: funcA:func  sync   funcA:func
	thread 2: funcB:func  module funcB:func
