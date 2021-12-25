#include "preproc.h"
#include "memory.h"
#include "file_io.h"

#include "stb_ds.h"

#if _WIN32
#include <windows.h>
#define strdup(x) _strdup(x)
#endif

static void preprocess_file(CPP_Context* restrict c, TokenStream* restrict s, const char* directory, const char* filepath);
static uint64_t hash_ident(const unsigned char* at, size_t length);
static bool is_defined(CPP_Context* restrict c, const unsigned char* start, size_t length);
static void expect(Lexer* l, char ch);
static void skip_directive_body(Lexer* l);
static int eval(CPP_Context* restrict c, Lexer* l);
static _Noreturn void generic_error(Lexer* l, const char* msg);

static unsigned char* expand_ident(CPP_Context* restrict c, unsigned char* restrict out, Lexer* l);
static unsigned char* expand(CPP_Context* restrict c, unsigned char* restrict out, Lexer* l);

static bool find_define(CPP_Context* restrict c, string* out_val, const unsigned char* start, size_t length);

// this one outputs the location of the arguments list, if there's not one
// then it's just not a parenthesis
static bool find_define2(CPP_Context* restrict c, size_t* out_index, const unsigned char* start, size_t length);

void cpp_init(CPP_Context* ctx) {
	size_t sz = sizeof(void*) * MACRO_BUCKET_COUNT * SLOTS_PER_MACRO_BUCKET;
	
	*ctx = (CPP_Context){
		.macro_bucket_keys = malloc(sz),
		.macro_bucket_keys_length = malloc(sz),
		.macro_bucket_values_start = malloc(sz),
		.macro_bucket_values_end = malloc(sz),
		
		.the_shtuffs = malloc(THE_SHTUFFS_SIZE)
	};
	
	tls_init();
}

void cpp_deinit(CPP_Context* ctx) {
	if (ctx->macro_bucket_keys) {
		cpp_finalize(ctx);
	}
	
	free((void*)ctx->the_shtuffs);
	ctx->the_shtuffs = NULL;
}

void cpp_finalize(CPP_Context* ctx) {
	free((void*)ctx->macro_bucket_keys);
	free((void*)ctx->macro_bucket_keys_length);
	free((void*)ctx->macro_bucket_values_start);
	free((void*)ctx->macro_bucket_values_end);
	
	ctx->macro_bucket_keys = NULL;
	ctx->macro_bucket_keys_length = NULL;
	ctx->macro_bucket_values_start = NULL;
	ctx->macro_bucket_values_end = NULL;
}

void cpp_add_include_directory(CPP_Context* ctx, const char dir[]) {
	arrput(ctx->system_include_dirs, strdup(dir));
}

void cpp_define_empty(CPP_Context* ctx, const char key[]) {
	size_t len = strlen(key);
	
	// Hash name
	uint64_t slot = hash_ident((const unsigned char*)key, len);
	uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);
	
	// Insert into buckets
	ctx->macro_bucket_count[slot] += 1;
	ctx->macro_bucket_keys[e] = (const unsigned char*)key;
	ctx->macro_bucket_keys_length[e] = len;
	
	ctx->macro_bucket_values_start[e] = NULL;
	ctx->macro_bucket_values_end[e] = NULL;
}

void cpp_define(CPP_Context* ctx, const char key[], const char value[]) {
	size_t len = strlen(key);
	
	// Hash name
	uint64_t slot = hash_ident((const unsigned char*)key, len);
	uint64_t e = ctx->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);
	
	// Insert into buckets
	ctx->macro_bucket_count[slot] += 1;
	ctx->macro_bucket_keys[e] = (const unsigned char*)key;
	ctx->macro_bucket_keys_length[e] = len;
	
	ctx->macro_bucket_values_start[e] = (const unsigned char*)value;
	ctx->macro_bucket_values_end[e] = (const unsigned char*) value + strlen(value);
}

void cpp_dump(CPP_Context* ctx) {
	int count = 0;
	
	for (int i = 0; i < MACRO_BUCKET_COUNT; i++) {
		for (int j = 0; j < ctx->macro_bucket_count[i]; j++) {
			size_t e = (i * SLOTS_PER_MACRO_BUCKET) + j;
			
			size_t keylen = ctx->macro_bucket_keys_length[e];
			const char* key = (const char*)ctx->macro_bucket_keys[e];
			
			size_t vallen = ctx->macro_bucket_values_end[e] - ctx->macro_bucket_values_start[e];
			const char* val = (const char*)ctx->macro_bucket_values_start[e];
			
			printf("\t'%.*s' -> '%.*s'\n", (int)keylen, key, (int)vallen, val);
		}
		
		printf("\n");
		count += ctx->macro_bucket_count[i];
	}
	
	printf("\n# Macro defines active: %d\n", count);
}

TokenStream cpp_process(CPP_Context* ctx, const char filepath[]) {
	TokenStream s = { 0 };
	preprocess_file(ctx, &s, "", filepath);
	
	Token t = { 0, NULL, NULL, 0 };
	arrput(s.tokens, t);
	
	return s;
}

// TODO(NeGate): Fix this up please...
static void expand_double_hash(CPP_Context* restrict c, Token* last, Lexer* restrict l, int line) {
	unsigned char* out_start = c->the_shtuffs + c->the_shtuffs_size;
	c->the_shtuffs_size += 4096;
	assert(c->the_shtuffs_size < THE_SHTUFFS_SIZE);
	
	unsigned char* out = out_start;
	
	// if the concat fails we return here.
	Lexer savepoint;
	{
		memcpy(out, last->start, last->end - last->start);
		out += last->end - last->start;
		
		lexer_read(l);
		
		memcpy(out, l->token_start, l->token_end - l->token_start);
		out += l->token_end - l->token_start;
		
		*out++ = '\0';
		
		savepoint = *l;
		lexer_read(l);
	}
	
	// join tokens
	Lexer tmp_lex = (Lexer) { l->filepath, out_start, out_start };
	lexer_read(&tmp_lex);
	
	// make nice joined token
	if (tmp_lex.token_type == TOKEN_IDENTIFIER) {
		*last = (Token){ 
			classify_ident(tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start),
			tmp_lex.token_start,
			tmp_lex.token_end,
			line
		};
	} else {
		*last = (Token){ tmp_lex.token_type, tmp_lex.token_start, tmp_lex.token_end, line };
	}
	lexer_read(&tmp_lex);
	
	// NOTE(NeGate): So you're not supposed to have multiple tokens
	// once you've concaternated but... eh
	if (tmp_lex.token_type) {
		*l = savepoint;
	}
}

static void preprocess_file(CPP_Context* restrict c, TokenStream* restrict s, const char* directory, const char* filepath) {
	unsigned char* text = (unsigned char*)read_entire_file(filepath);
	Lexer l = (Lexer) { filepath, text, text, 1 };
	
	lexer_read(&l);
	do {
		l.hit_line = false;
		
		if (l.token_type == TOKEN_IDENTIFIER) {
			int line = l.current_line;
			
			if (!is_defined(c, l.token_start, l.token_end - l.token_start)) {
				// FAST PATH
				Token t = { 
					classify_ident(l.token_start, l.token_end - l.token_start),
					l.token_start, l.token_end,
					line 
				};
				arrput(s->tokens, t);
				
				lexer_read(&l);
			} else {
				// SLOW SHIT WHICH ALLOCATES SOME SPACE OUT THE SHTUFFS
				// Expand
				unsigned char* out_start = &c->the_shtuffs[c->the_shtuffs_size];
				unsigned char* out_end = expand_ident(c, out_start, &l);
				
				if (out_start == out_end) {
					// Empty macro
				} else {
					*out_end++ = '\0';
					
					c->the_shtuffs_size += out_end - out_start;
					assert(c->the_shtuffs_size < THE_SHTUFFS_SIZE);
					
					Lexer tmp_lex = (Lexer) { filepath, out_start, out_start };
					lexer_read(&tmp_lex);
					
					while (tmp_lex.token_type) {
						Token t;
						if (tmp_lex.token_type == TOKEN_IDENTIFIER) {
							t = (Token){ 
								classify_ident(tmp_lex.token_start, tmp_lex.token_end - tmp_lex.token_start),
								tmp_lex.token_start,
								tmp_lex.token_end,
								line
							};
							
							arrput(s->tokens, t);
							lexer_read(&tmp_lex);
						} else if (tmp_lex.token_type == TOKEN_DOUBLE_HASH) {
							assert(arrlen(s->tokens) > 0);
							Token* last = &s->tokens[arrlen(s->tokens) - 1];
							
							expand_double_hash(c, last, &tmp_lex, line);
						} else {
							t = (Token){ tmp_lex.token_type, tmp_lex.token_start, tmp_lex.token_end, line };
							
							arrput(s->tokens, t);
							lexer_read(&tmp_lex);
						}
					}
				}
			}
		} else if (l.token_type == TOKEN_DOUBLE_HASH) {
			int line = l.current_line;
			lexer_read(&l);
			
			assert(arrlen(s->tokens) > 0);
			Token* last = &s->tokens[arrlen(s->tokens) - 1];
			
			expand_double_hash(c, last, &l, line);
		} else if (l.token_type == '#') {
			lexer_read(&l);
			
			if (l.token_type == TOKEN_IDENTIFIER) {
				if (lexer_match(&l, 2, "if")) {
					lexer_read(&l);
					
					assert(!l.hit_line);
					if (eval(c, &l)) {
						c->depth++;
						c->scope_eval[c->depth] = 1;
					} else {
						c->scope_eval[c->depth] = 0;
						skip_directive_body(&l);
					}
					
					// we should be one a different line now
					assert(l.hit_line);
				} else if (lexer_match(&l, 4, "else")) {
					lexer_read(&l);
					assert(l.hit_line);
					
					// if it didn't evaluate any of the other options
					// do this
					if (c->scope_eval[c->depth] == 0) {
						c->depth++;
						c->scope_eval[c->depth] = 1;
					} else {
						skip_directive_body(&l);
					}
				} else if (lexer_match(&l, 6, "define")) {
					lexer_read(&l);
					
					if (l.token_type != TOKEN_IDENTIFIER) {
						generic_error(&l, "expected identifier!");
					}
					
					// Hash name
					uint64_t slot = hash_ident(l.token_start, l.token_end - l.token_start);
					uint64_t e = c->macro_bucket_count[slot] + (slot * SLOTS_PER_MACRO_BUCKET);
					
					// Insert into buckets
					c->macro_bucket_count[slot] += 1;
					c->macro_bucket_keys[e] = l.token_start;
					c->macro_bucket_keys_length[e] = l.token_end - l.token_start;
					
					// if there's a parenthesis directly after the identifier
					// it's a macro function
					if (*l.token_end == '(') {
						lexer_read(&l);
						expect(&l, '(');
						
						int arg_count = 0;
						while (l.token_type != ')') {
							if (arg_count) {
								expect(&l, ',');
							}
							
							if (l.token_type != TOKEN_TRIPLE_DOT &&
								l.token_type != TOKEN_IDENTIFIER) {
								generic_error(&l, "expected identifier!");
							}
							
							lexer_read(&l);
							arg_count++;
						}
						
						assert(!l.hit_line);
						expect(&l, ')');
					} else {
						// Skip until we hit a newline
						assert(!l.hit_line);
						lexer_read(&l);
					}
					
					const unsigned char* start = l.token_start;
					const unsigned char* end = l.token_start;
					while (!l.hit_line) {
						end = l.token_end;
						lexer_read(&l);
					}
					
					c->macro_bucket_values_start[e] = start;
					c->macro_bucket_values_end[e] = end;
				} else if (lexer_match(&l, 4, "elif")) {
					lexer_read(&l);
					
					assert(!l.hit_line);
					// if it didn't evaluate any of the other options
					// try to do this
					if (c->scope_eval[c->depth] == 0 && eval(c, &l)) {
						c->depth++;
						c->scope_eval[c->depth] = 1;
					} else {
						skip_directive_body(&l);
					}
					
					// we should be one a different line now
					assert(l.hit_line);
				} else if (lexer_match(&l, 6, "ifndef")) {
					lexer_read(&l);
					
					if (l.token_type != TOKEN_IDENTIFIER) {
						generic_error(&l, "expected identifier!");
					}
					
					if (is_defined(c, l.token_start, l.token_end - l.token_start)) {
						c->scope_eval[c->depth] = 0;
						skip_directive_body(&l);
					} else {
						c->depth++;
						c->scope_eval[c->depth] = 1;
						lexer_read(&l);
					}
				} else if (lexer_match(&l, 5, "ifdef")) {
					lexer_read(&l);
					
					if (l.token_type != TOKEN_IDENTIFIER) {
						generic_error(&l, "expected identifier!");
					}
					
					if (is_defined(c, l.token_start, l.token_end - l.token_start)) {
						c->depth++;
						c->scope_eval[c->depth] = 1;
						lexer_read(&l);
					} else {
						c->scope_eval[c->depth] = 0;
						skip_directive_body(&l);
					}
				} else if (lexer_match(&l, 5, "endif")) {
					lexer_read(&l);
					
					if (c->depth == 0) {
						generic_error(&l, "Too many endifs\n");
					}
					
					c->depth--;
				} else if (lexer_match(&l, 7, "include")) {
					lexer_read(&l);
					
					const unsigned char* start = NULL;
					const unsigned char* end = NULL;
					bool is_system_include = false;
					
					if (l.token_type == '<') {
						is_system_include = true;
						start = l.token_start + 1;
						
						do {
							lexer_read(&l);
						} while (l.token_type != '>' && !l.hit_line);
						
						end = l.token_end - 1;
						expect(&l, '>');
					} else if (l.token_type == TOKEN_STRING_DOUBLE_QUOTE) {
						start = l.token_start + 1;
						end = l.token_end - 1;
					} else {
						generic_error(&l, "expected file path!");
					}
					
					// Search for file in system libs
					char path[260];
					bool success = false;
					if (is_system_include) {
						size_t num_system_include_dirs = arrlen(c->system_include_dirs);
						
						for (size_t i = 0; i < num_system_include_dirs; i++) {
							sprintf_s(path, 260, "%s%.*s",
									  c->system_include_dirs[i],
									  (int) (end - start), start);
							
							if (file_exists(path)) {
								success = true;
								break;
							}
						}
					}
					
					if (!success) {
						// Try local includes
						sprintf_s(path, 260, "%s%.*s", directory, (int) (end - start), start);
						
						if (file_exists(path)) success = true;
					}
					
					if (!success) {
						int loc = l.current_line;
						printf("error %s:%d: Could not find file! %.*s\n", l.filepath, loc, (int) (end - start), start);
						abort();
					}
					
					CPP_IncludeOnce* e = shgetp_null(c->include_once, path);
					if (e == NULL) {
						// TODO(NeGate): Remove these heap allocations later they're... evil!!!
						char* new_path = strdup(path);
						char* new_dir = strdup(path);
						
						char* slash = strrchr(new_dir, '\\');
						if (!slash) {
							slash = strrchr(new_dir, '/');
							if (!slash) abort();
						}
						slash[1] = '\0';
						
						//printf("  %s\n", new_path);
						preprocess_file(c, s, new_dir, new_path);
					}
				} else if (lexer_match(&l, 6, "pragma")) {
					lexer_read(&l);
					
					if (lexer_match(&l, 4, "once")) {
						CPP_IncludeOnce e = (CPP_IncludeOnce){ 
							.key = (char*)filepath,
							.value = 0
						};
						shputs(c->include_once, e);
						
						lexer_read(&l);
						
						// We gotta hit a line by now
						assert(l.hit_line);
					} else {
						// TODO(NeGate): Actually implement these...
						// Skip until we hit a newline
						assert(!l.hit_line);
						do {
							lexer_read(&l);
						} while (!l.hit_line);
					}
				} else if (lexer_match(&l, 5, "undef")) {
					lexer_read(&l);
					
					if (l.token_type != TOKEN_IDENTIFIER) {
						generic_error(&l, "expected identifier!");
					}
					
					const unsigned char* start = l.token_start;
					size_t length = l.token_end - l.token_start;
					
					lexer_read(&l);
					
					// Hash name
					uint64_t slot = hash_ident(start, length);
					uint64_t base = slot * SLOTS_PER_MACRO_BUCKET;
					uint64_t count = c->macro_bucket_count[slot];
					
					// TODO(NeGate): We might wanna invest into a faster data structure.
					for (size_t i = 0; i < count; i++) {
						uint64_t e = base + i;
						
						if (c->macro_bucket_keys_length[e] == length &&
							memcmp(c->macro_bucket_keys[e], start, length) == 0) {
							// remove swap
							uint64_t last = c->macro_bucket_count[slot] - 1;
							
							if (c->macro_bucket_count[slot] > 1) {
								c->macro_bucket_keys_length[e]  = c->macro_bucket_keys_length[last];
								c->macro_bucket_keys[e]         = c->macro_bucket_keys[last];
								c->macro_bucket_values_start[e] = c->macro_bucket_values_start[last];
								c->macro_bucket_values_end[e]   = c->macro_bucket_values_end[last];
							}
							c->macro_bucket_count[slot]--;
							break;
						}
					}
				} else {
					generic_error(&l, "unknown directive!");
				}
			} else {
				generic_error(&l, "unknown directive!");
			}
		} else {
			int line = l.current_line;
			Token t = { l.token_type, l.token_start, l.token_end, line };
			arrput(s->tokens, t);
			lexer_read(&l);
		}
	} while (l.token_type);
}

static void skip_directive_body(Lexer* l) {
	int depth = 0;
	
	do {
		if (l->token_type == '#') {
			// TODO(NeGate): Future me... fix it
			Lexer saved = *l;
			lexer_read(l);
			
			if (l->token_type == TOKEN_IDENTIFIER) {
				if (lexer_match(l, 2, "if")) depth++;
				if (lexer_match(l, 5, "ifdef")) depth++;
				else if (lexer_match(l, 6, "ifndef")) depth++;
				else if (lexer_match(l, 4, "elif") || lexer_match(l, 4, "else")) {
					// else/elif does both entering a scope and exiting one
					if (depth == 0) {
						*l = saved;
						return;
					}
				} else if (lexer_match(l, 5, "endif")) {
					if (depth == 0) {
						lexer_read(l);
						return;
					}
					
					depth--;
				}
			}
		}
		
		lexer_read(l);
	} while (l->token_type);
	
	generic_error(l, "Unclosed macro conditional");
}

static _Noreturn void generic_error(Lexer* l, const char* msg) {
	int loc = l->current_line;
	printf("error %s:%d: %s\n", l->filepath, loc, msg);
	abort();
}

static void expect(Lexer* l, char ch) {
	if (l->token_type != ch) {
		int loc = l->current_line;
		printf("error %s:%d: expected '%c' got '%.*s'", l->filepath, loc, ch, (int)(l->token_end - l->token_start), l->token_start);
		abort();
	}
	
	lexer_read(l);
}

//
// Hashing
// based on the refterm hash by Casey Muratori
//
static char unsigned overhang_mask[32] = {
	255, 255, 255, 255, 255, 255, 255, 255,
	255, 255, 255, 255, 255, 255, 255, 255,
	0,   0,   0,   0,   0,   0,   0,   0,
	0,   0,   0,   0,   0,   0,   0,   0
};

static char unsigned default_seed[16] = {
	178, 201, 95, 240, 40, 41, 143, 216,
	2, 209, 178, 114, 232, 4, 176, 188
};

static uint64_t hash_ident(const unsigned char* at, size_t length) {
	__m128i hash = _mm_cvtsi64_si128(length);
	hash = _mm_xor_si128(hash, _mm_loadu_si128((__m128i*)default_seed));
	
	size_t chunk_count = length / 16;
	while(chunk_count--) {
		__m128i in = _mm_loadu_si128((__m128i*) at);
		at += 16;
		
		hash = _mm_xor_si128(hash, in);
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
		hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	}
	
	size_t overhang = length % 16;
	__m128i in = _mm_loadu_si128((__m128i*) at);
	
	in = _mm_and_si128(in, _mm_loadu_si128((__m128i*) (overhang_mask + 16 - overhang)));
	hash = _mm_xor_si128(hash, in);
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	hash = _mm_aesdec_si128(hash, _mm_setzero_si128());
	
	return _mm_extract_epi8(hash, 0) & 31;
}

static bool is_defined(CPP_Context* restrict c, const unsigned char* start, size_t length) {
	uint64_t slot = hash_ident(start, length);
	size_t count = c->macro_bucket_count[slot];
	size_t base = (slot * SLOTS_PER_MACRO_BUCKET);
	
	for (size_t i = 0; i < count; i++) {
		if (c->macro_bucket_keys_length[base + i] == length &&
			memcmp(c->macro_bucket_keys[base + i], start, length) == 0) {
			return true;
		}
	}
	
	return false;
}

static bool find_define(CPP_Context* restrict c, string* out_val, const unsigned char* start, size_t length) {
	uint64_t slot = hash_ident(start, length);
	size_t count = c->macro_bucket_count[slot];
	size_t base = (slot * SLOTS_PER_MACRO_BUCKET);
	
	for (size_t i = 0; i < count; i++) {
		if (c->macro_bucket_keys_length[base + i] == length &&
			memcmp(c->macro_bucket_keys[base + i], start, length) == 0) {
			
			const unsigned char* s = c->macro_bucket_values_start[base + i];
			const unsigned char* e = c->macro_bucket_values_end[base + i];
			
			*out_val = (string) { .data = s, .length = e - s };
			return true;
		}
	}
	
	return false;
}

static bool find_define2(CPP_Context* restrict c, size_t* out_index, const unsigned char* start, size_t length) {
	uint64_t slot = hash_ident(start, length);
	size_t count = c->macro_bucket_count[slot];
	size_t base = (slot * SLOTS_PER_MACRO_BUCKET);
	
	for (size_t i = 0; i < count; i++) {
		size_t e = base + i;
		
		if (c->macro_bucket_keys_length[e] == length &&
			memcmp(c->macro_bucket_keys[e], start, length) == 0) {
			*out_index = e;
			return true;
		}
	}
	
	return false;
}

////////////////////////////////
// Macro expressions
////////////////////////////////
static int eval_l0(CPP_Context* restrict c, Lexer* l) {
	bool flip = false;
	while (l->token_type == '!') {
		flip = !flip;
		lexer_read(l);
	}
	
	int val;
	if (l->token_type == TOKEN_INTEGER) {
		char temp[16];
		memcpy_s(temp, 16, l->token_start, l->token_end - l->token_start);
		temp[l->token_end - l->token_start] = '\0';
		
		val = atoi(temp);
		lexer_read(l);
	} else if (l->token_type == TOKEN_IDENTIFIER) {
		assert(!is_defined(c, l->token_start, l->token_end - l->token_start));
		
		val = 0;
		lexer_read(l);
	} else if (l->token_type == '(') {
		lexer_read(l);
		val = eval(c, l);
		expect(l, ')');
	} else {
		generic_error(l, "could not parse expression!");
	}
	
	return flip ? !val : val;
}

static int eval_l6(CPP_Context* restrict c, Lexer* l) {
	int left = eval_l0(c, l);
	
	while (l->token_type == '>' ||
		   l->token_type == '<' ||
		   l->token_type == TOKEN_GREATER_EQUAL ||
		   l->token_type == TOKEN_LESS_EQUAL) {
		int t = l->token_type;
		lexer_read(l);
		
		int right = eval_l0(c, l);
		switch (t) {
			case '>': left = left > right; break;
			case '<': left = left < right; break;
			case TOKEN_GREATER_EQUAL: left = left >= right; break;
			case TOKEN_LESS_EQUAL: left = left <= right; break;
		}
	}
	
	return left;
}

static int eval_l7(CPP_Context* restrict c, Lexer* l) {
	int left = eval_l6(c, l);
	
	while (l->token_type == TOKEN_NOT_EQUAL ||
		   l->token_type == TOKEN_EQUALITY) {
		int t = l->token_type;
		lexer_read(l);
		
		int right = eval_l6(c, l);
		if (t == TOKEN_EQUALITY) left = (left == right);
		else left = (left != right);
	}
	
	return left;
}

static int eval_l11(CPP_Context* restrict c, Lexer* l) {
	int left = eval_l7(c, l);
	
	while (l->token_type == TOKEN_DOUBLE_AND) {
		lexer_read(l);
		
		int right = eval_l7(c, l);
		left = left && right;
	}
	
	return left;
}

static int eval_l12(CPP_Context* restrict c, Lexer* l) {
	int left = eval_l11(c, l);
	
	while (l->token_type == TOKEN_DOUBLE_OR) {
		lexer_read(l);
		
		int right = eval_l11(c, l);
		left = left || right;
	}
	
	return left;
}

static unsigned char* expand_ident(CPP_Context* restrict c, unsigned char* restrict out, Lexer* l) {
	size_t token_length = l->token_end - l->token_start;
	const unsigned char* token_data = l->token_start;
	
	if (lexer_match(l, 7, "defined")) {
		// optional parenthesis
		lexer_read(l);
		
		const unsigned char* start = NULL;
		const unsigned char* end = NULL;
		if (l->token_type == '(') {
			lexer_read(l);
			
			if (l->token_type != TOKEN_IDENTIFIER) {
				generic_error(l, "expected identifier!");
			}
			
			start = l->token_start;
			end = l->token_end;
			
			lexer_read(l);
			expect(l, ')');
		} else if (l->token_type == TOKEN_IDENTIFIER) {
			start = l->token_start;
			end = l->token_end;
			lexer_read(l);
		} else {
			generic_error(l, "expected identifier!");
		}
		
		bool found = is_defined(c, start, end - start);
		
		*out++ = found ? '1' : '0';
		*out++ = ' ';
		return out;
	}
	
	size_t def_i;
	if (find_define2(c, &def_i, token_data, token_length)) {
		lexer_read(l);
		
		string def = { 
			.data = c->macro_bucket_values_start[def_i],
			.length = c->macro_bucket_values_end[def_i] - c->macro_bucket_values_start[def_i]
		};
		
		const unsigned char* args = c->macro_bucket_keys[def_i] + c->macro_bucket_keys_length[def_i];
		
		// function macro
		if (*args == '(' && l->token_type == '(') {
			////////////////////////////////
			// Parse the parameters into a map
			////////////////////////////////
			// make the start and end of the params, interleaved
			// start: value_ranges[i*2 + 0], end: value_ranges[i*2 + 1]
			const unsigned char** value_ranges = tls_save();
			int value_count = 0;
			
			lexer_read(l);
			while (l->token_type != ')') {
				tls_push(2 * sizeof(const unsigned char*));
				int i = value_count++;
				
				value_ranges[i*2 + 0] = l->token_start;
				
				int paren_depth = 0;
				while (true) {
					if (l->token_type == '(') {
						paren_depth++;
					} else if (l->token_type == ')') {
						if (paren_depth == 0) break;
						paren_depth--;
					} else if (l->token_type == ',') {
						if (paren_depth == 0) break;
					}
					lexer_read(l);
				}
				
				value_ranges[i*2 + 1] = l->token_start;
				if (l->token_type == ',') lexer_read(l);
			}
			
			expect(l, ')');
			
			// We dont need to parse this part if it expands into nothing
			if (def.length) {
				// Parse macro function arg names
				const unsigned char** key_ranges = tls_save();
				int key_count = 0;
				
				{
					Lexer arg_lex = (Lexer) { l->filepath, args, args };
					lexer_read(&arg_lex);
					expect(&arg_lex, '(');
					
					while (arg_lex.token_type != ')') {
						if (key_count) {
							expect(&arg_lex, ',');
						}
						
						if (arg_lex.token_type != TOKEN_IDENTIFIER) {
							generic_error(&arg_lex, "expected identifier!");
						}
						
						tls_push(2 * sizeof(const unsigned char*));
						
						int i = key_count++;
						key_ranges[i*2 + 0] = arg_lex.token_start;
						key_ranges[i*2 + 1] = arg_lex.token_end;
						
						lexer_read(&arg_lex);
					}
					
					expect(&arg_lex, ')');
				}
				
				////////////////////////////////
				// Stream over the text hoping
				// to replace some identifiers
				// then expand the result one more
				// time
				////////////////////////////////
				unsigned char* temp_expansion_start = tls_push(4096);
				unsigned char* temp_expansion = temp_expansion_start;
				
				Lexer def_lex = (Lexer) { l->filepath, def.data, def.data };
				lexer_read(&def_lex);
				
				// set when a # happens, we expect a macro parameter afterwards
				bool as_string = false;
				while (!def_lex.hit_line) {
					// shadowing...
					size_t token_length = def_lex.token_end - def_lex.token_start;
					const unsigned char* token_data = def_lex.token_start;
					
					if (def_lex.token_type == TOKEN_HASH) {
						// TODO(NeGate): Error message
						if (as_string) abort();
						
						as_string = true;
						lexer_read(&def_lex);
						continue;
					}
					
					if (def_lex.token_type != TOKEN_IDENTIFIER) {
						// TODO(NeGate): Error message
						if (as_string) abort();
						
						memcpy(temp_expansion, token_data, token_length);
						temp_expansion+= token_length;
						*temp_expansion++ = ' ';
						
						lexer_read(&def_lex);
						continue;
					}
					
					int index = -1;
					for (int i = 0; i < key_count; i++) {
						size_t key_length = key_ranges[i*2 + 1] - key_ranges[i*2 + 0];
						const unsigned char* key = key_ranges[i*2 + 0];
						
						if (token_length == key_length &&
							memcmp(key, token_data, token_length) == 0) {
							index = i;
							break;
						}
					}
					
					if (index >= 0) {
						const unsigned char* end   = value_ranges[index*2 + 1];
						const unsigned char* start = value_ranges[index*2 + 0];
						
						if (as_string) *temp_expansion++ = '\"';
						
						memcpy(temp_expansion, start, end-start);
						temp_expansion += end-start;
						
						if (as_string) *temp_expansion++ = '\"';
						*temp_expansion++ = ' ';
						
						as_string = false;
					} else {
						// TODO(NeGate): Error message
						if (as_string) abort();
						
						memcpy(temp_expansion, token_data, token_length);
						temp_expansion += token_length;
						*temp_expansion++ = ' ';
					}
					
					lexer_read(&def_lex);
				}
				
				if (temp_expansion_start != temp_expansion) {
					// expand and append
					Lexer temp_lex = (Lexer) { l->filepath, temp_expansion_start, temp_expansion_start };
					lexer_read(&temp_lex);
					
					*temp_expansion++ = '\0';
					
					// NOTE(NeGate): We need to disable the current macro define
					// so it doesn't recurse.
					size_t saved_length = c->macro_bucket_keys_length[def_i];
					c->macro_bucket_keys_length[def_i] = 0;
					
					out = expand(c, out, &temp_lex);
					tls_restore(temp_expansion_start);
					
					c->macro_bucket_keys_length[def_i] = saved_length;
				}
			}
		} else if (def.length) {
			// expand and append
			Lexer temp_lex = (Lexer) { l->filepath, def.data, def.data };
			lexer_read(&temp_lex);
			
			out = expand(c, out, &temp_lex);
		}
		
		return out;
	}
	
	// Normal identifier
	memcpy(out, token_data, token_length);
	out += token_length;
	*out++ = ' ';
	
	lexer_read(l);
	return out;
}

static unsigned char* expand(CPP_Context* restrict c, unsigned char* restrict out, Lexer* l) {
	int depth = 0;
	
	while (!l->hit_line) {
		if (l->token_type == '(') {
			depth++;
			
			*out++ = '(';
			*out++ = ' ';
			
			lexer_read(l);
		} else if (l->token_type == ')') {
			if (depth == 0) break;
			depth--;
			
			*out++ = ')';
			*out++ = ' ';
			
			lexer_read(l);
		} else if (l->token_type != TOKEN_IDENTIFIER) {
			size_t token_length = l->token_end - l->token_start;
			const unsigned char* token_data = l->token_start;
			
			memcpy(out, token_data, token_length);
			out += token_length;
			*out++ = ' ';
			
			lexer_read(l);
		} else {
			out = expand_ident(c, out, l);
		}
	}
	
	return out;
}

static int eval(CPP_Context* restrict c, Lexer* l) {
	// Expand
	int line = l->current_line + 1;
	unsigned char* out_start = tls_push(4096);
	unsigned char* out_end = expand(c, out_start, l);
	*out_end++ = '\0';
	
	// Evaluate
	Lexer temp_lex = (Lexer) { l->filepath, out_start, out_start, line };
	lexer_read(&temp_lex);
	
	int val = eval_l12(c, &temp_lex);
	
	tls_restore(out_start);
	return val;
}

