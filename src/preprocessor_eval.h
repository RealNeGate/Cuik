// TODO(NeGate): I don't like this code either...
// TODO(NeGate): It's also not particularly fast
static int eval(Context* restrict c, const char* input);

static const char* eval_atom(Context* restrict c, int* val, const char* input) {
	bool flip = false;
	
	SKIP_SPACE();
	if (*input == '!') {
		flip = true;
		input++;
		SKIP_SPACE();
	}
	
	int out = 0;
	if (classify(*input) == PPROC_STATE_IDENT) {
		const char* name_start = input;
		READ_IDENT();
		size_t name_len = input - name_start;
		
		if (name_len == 7 && memcmp(name_start, "defined", 7) == 0) {
			SKIP_SPACE();
			
			if (classify(*input) == PPROC_STATE_IDENT) {
				const char* ident_start = input;
				READ_IDENT();
				size_t ident_len = input - ident_start;
				
				if (ident_len == 0) abort();
				out = is_defined(c, ident_start, ident_len);
			} else {
				if (*input != '(') abort();
				input++;
				SKIP_SPACE();
				
				const char* ident_start = input;
				READ_IDENT();
				size_t ident_len = input - ident_start;
				
				SKIP_SPACE();
				if (*input != ')') abort();
				input++;
				SKIP_SPACE();
				
				if (ident_len == 0) abort();
				out = is_defined(c, ident_start, ident_len);
			}
		} else {
			string def;
			if (find_define(c, &def, name_start, name_len)) {
				// NOTE(NeGate): This is kinda odd, it sorta assumes that the
				// define value is followed by a newline which should be true
				out = eval(c, def.data);
			} else abort();
		}
	} else if (*input >= '0' && *input <= '9') {
		// TODO(NeGate): Fix this up to support all kinds of integers
		const char* end = input;
		while (*end >= '0' && *end <= '9') end++;
		
		char temp[16];
		memcpy(temp, input, end - input);
		temp[end - input] = 0;
		
		out = atoi(temp);
	} else {
		abort();
	}
	
	*val = flip ? !out : out;
	
	SKIP_SPACE();
	return input;
}

static const char* eval_relational(Context* restrict c, int* val, const char* input) {
	int left;
	input = eval_atom(c, &left, input);
	
	while ((input[0] == '!' || input[0] == '=') && input[1] == '=') {
		char op = input[0];
		input += 2;
		
		int right;
		input = eval_atom(c, &right, input);
		left = op == '=' ? (left == right) : (left != right);
	}
	
	*val = left;
	return input;
}

static const char* eval_and(Context* restrict c, int* val, const char* input) {
	int left;
	input = eval_relational(c, &left, input);
	
	while (input[0] == '&' && input[1] == '&') {
		input += 2;
		
		int right;
		input = eval_relational(c, &right, input);
		left = left && right;
	}
	
	*val = left;
	return input;
}

static const char* eval_or(Context* restrict c, int* val, const char* input) {
	int left;
	input = eval_and(c, &left, input);
	
	while (input[0] == '|' && input[1] == '|') {
		input += 2;
		
		int right;
		input = eval_and(c, &right, input);
		left = left || right;
	}
	
	*val = left;
	return input;
}

static int eval(Context* restrict c, const char* input) {
	int val;
	eval_or(c, &val, input);
	return val;
}
