////////////////////////////////
// TYPES
////////////////////////////////
static bool skip_over_declspec(TokenStream* restrict s) {
	if (tokens_get(s)->type == TOKEN_KW_declspec ||
		tokens_get(s)->type == TOKEN_KW_Pragma) {
		tokens_next(s);
		expect(s, '(');
		
		// TODO(NeGate): Correctly parse declspec instead of
		// ignoring them.
		int depth = 1;
		while (depth) {
			if (tokens_get(s)->type == '(') depth++;
			else if (tokens_get(s)->type == ')') depth--;
			
			tokens_next(s);
		}
		return true;
	}
	
	return false;
}

static Decl parse_declarator(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, bool is_abstract, bool disabled_paren) {
	// handle calling convention
	// TODO(NeGate): Actually pass these to the AST
	parse_another_qualifier2: {
		switch (tokens_get(s)->type) {
			case TOKEN_KW_cdecl:
			case TOKEN_KW_stdcall:
			tokens_next(s);
			goto parse_another_qualifier2;
			default: break;
		}
	}
	
	// handle pointers
	while (tokens_get(s)->type == '*') {
		type = type ? new_pointer(tu, type) : 0;
		tokens_next(s);
		
		parse_another_qualifier: {
			switch (tokens_get(s)->type) {
				case TOKEN_KW_Atomic: {
					tu->types[type].is_atomic = true;
					tokens_next(s);
					goto parse_another_qualifier;
				}
				case TOKEN_KW_restrict: {
					tu->types[type].is_ptr_restrict = true;
					tokens_next(s);
					goto parse_another_qualifier;
				}
				
				case TOKEN_KW_const:
				case TOKEN_KW_volatile:
				case TOKEN_KW_cdecl:
				case TOKEN_KW_stdcall: {
					tokens_next(s);
					goto parse_another_qualifier;
				}
				
				default: break;
			}
		}
	}
	
	skip_over_declspec(s);
	
	bool is_nested_declarator = tokens_get(s)->type == '(';
	
	// disambiguate
	if (!out_of_order_mode && is_nested_declarator && is_abstract) {
		tokens_next(s);
		
		if (is_typename(s)) {
			is_nested_declarator = false;
		}
		
		tokens_prev(s);
	}
	
	if (is_nested_declarator) {
		// TODO(NeGate): I don't like this code...
		// it essentially just skips over the stuff in the
		// parenthesis to do the suffix then comes back
		// for the parenthesis after wards, restoring back to
		// the end of the declarator when it's done.
		//
		// should be right after the (
		SourceLoc* opening_loc = &s->line_arena[tokens_get(s)->location];
		
		tokens_next(s);
		size_t saved = s->current;
		
		parse_declarator(tu, s, TYPE_NONE, is_abstract, false);
		
		expect_closing_paren(s, opening_loc);
		type = parse_type_suffix(tu, s, type, NULL);
		
		size_t saved_end = s->current;
		s->current = saved;
		
		Decl d = parse_declarator(tu, s, type, is_abstract, false);
		
		// inherit name
		// TODO(NeGate): I'm not sure if this is correct ngl
		if (!d.name) {
			TypeIndex t = d.type;
			
			if (tu->types[t].kind == KIND_PTR) {
				t = tu->types[d.type].ptr_to;
				
				if (tu->types[t].kind == KIND_FUNC) {
					d.name = tu->types[t].func.name;
				} else if (tu->types[t].kind == KIND_STRUCT) {
					d.name = tu->types[t].record.name;
				} else if (tu->types[t].kind == KIND_UNION) {
					d.name = tu->types[t].record.name;
				}
			}
		}
		
		s->current = saved_end;
		return d;
	}
	
	Atom name = NULL;
	Token* t = tokens_get(s);
	SourceLocIndex loc = 0;
	if (!is_abstract && t->type == TOKEN_IDENTIFIER) {
		loc = t->location;
		name = atoms_put(t->end - t->start, t->start);
		tokens_next(s);
	}
	
	type = parse_type_suffix(tu, s, type, name);
	return (Decl){ type, name, loc };
}

static TypeIndex parse_typename(TranslationUnit* tu, TokenStream* restrict s) {
	// TODO(NeGate): Check if attributes are set, they shouldn't
	// be in this context.
	Attribs attr = { 0 };
	TypeIndex type = parse_declspec(tu, s, &attr);
	return parse_declarator(tu, s, type, true, false).type;
}

static TypeIndex parse_type_suffix(TranslationUnit* tu, TokenStream* restrict s, TypeIndex type, Atom name) {
	assert(s->current > 0);
	SourceLoc* loc = &s->line_arena[s->tokens[s->current - 1].location];
	
	// type suffixes like array [] and function ()
	if (tokens_get(s)->type == '(') {
		tokens_next(s);
		
		TypeIndex return_type = type;
		
		type = new_func(tu);
		tu->types[type].func.name = name;
		tu->types[type].func.return_type = return_type;
		
		if (tokens_get(s)->type == TOKEN_KW_void && tokens_peek(s)->type == ')') {
			tokens_next(s);
			tokens_next(s);
			
			tu->types[type].func.param_list = 0;
			tu->types[type].func.param_count = 0;
			return type;
		}
		
		size_t param_count = 0;
		void* params = tls_save();
		bool has_varargs = false;
		
		while (tokens_get(s)->type != ')') {
			if (param_count) {
				expect(s, ',');
			}
			
			if (tokens_get(s)->type == TOKEN_TRIPLE_DOT) {
				tokens_next(s);
				
				has_varargs = true;
				break;
			}
			
			Attribs arg_attr = { 0 };
			TypeIndex arg_base_type = parse_declspec(tu, s, &arg_attr);
			
			Decl param_decl = parse_declarator(tu, s, arg_base_type, false, false);
			TypeIndex param_type = param_decl.type;
			
			if (tu->types[param_type].kind == KIND_ARRAY) {
				// Array parameters are desugared into pointers
				param_type = new_pointer(tu, tu->types[param_type].array_of);
			} else if (tu->types[param_type].kind == KIND_FUNC) {
				// Function parameters are desugared into pointers
				param_type = new_pointer(tu, param_type);
			}
			
			// TODO(NeGate): Error check that no attribs are set
			*((Param*)tls_push(sizeof(Param))) = (Param) {
				.type = param_type,
				.name = param_decl.name
			};
			param_count++;
		}
		
		if (tokens_get(s)->type != ')') {
			generic_error(s, "Unclosed parameter list!");
		}
		tokens_next(s);
		
		// Allocate some more permanent storage
		ParamIndex start = big_array_length(tu->params);
		big_array_put_uninit(tu->params, param_count);
		memcpy(&tu->params[start], params, param_count * sizeof(Param));
		
		tu->types[type].func.param_list = start;
		tu->types[type].func.param_count = param_count;
		tu->types[type].func.has_varargs = has_varargs;
		
		tls_restore(params);
	} else if (tokens_get(s)->type == '[') {
		size_t depth = 0;
		size_t* counts = tls_save();
		
		// TODO(NeGate): read some array qualifiers... then throw them away
		do {
			tokens_next(s);
			
			long long count;
			if (tokens_get(s)->type == ']') {
				count = 0; 
				tokens_next(s);
			} else if (tokens_get(s)->type == '*') {
				count = 0; 
				tokens_next(s);
				expect(s, ']');
			} else {
				count = parse_const_expr(tu, s);
				expect(s, ']');
			}
			
			tls_push(sizeof(size_t));
			counts[depth++] = count;
		} while (tokens_get(s)->type == '[');
		
		size_t expected_size = tu->types[type].size;
		while (depth--) {
			assert(tu->types[type].size == expected_size);
			
			uint64_t a = expected_size;
			uint64_t b = counts[depth];
			uint64_t result = a * b;
			
			// size checks
			if (result >= INT32_MAX) {
				report(REPORT_ERROR, loc, "cannot declare an array that exceeds 0x7FFFFFFE bytes (got 0x%zX)", result);
				abort();
			}
			
			type = new_array(tu, type, counts[depth]);
			expected_size = result;
		}
		
		tls_restore(counts);
	}
	
	return type;
}

// https://github.com/rui314/chibicc/blob/90d1f7f199cc55b13c7fdb5839d1409806633fdb/parse.c#L381
static TypeIndex parse_declspec(TranslationUnit* tu, TokenStream* restrict s, Attribs* attr) {
	enum {
		VOID     = 1 << 0,
		BOOL     = 1 << 2,
		CHAR     = 1 << 4,
		SHORT    = 1 << 6,
		INT      = 1 << 8,
		LONG     = 1 << 10,
		FLOAT    = 1 << 12,
		DOUBLE   = 1 << 14,
		OTHER    = 1 << 16,
		SIGNED   = 1 << 17,
		UNSIGNED = 1 << 18,
	};
	
	int counter = 0;
	TypeIndex type = TYPE_NONE;
	
	bool is_atomic = false;
	bool is_const  = false;
	
	// _Alignas(N) or __declspec(align(N))
	// 0 means no forced alignment
	int forced_align = 0;
	
	do {
		TknType tkn_type = tokens_get(s)->type;
		switch (tkn_type) {
			case TOKEN_KW_void: counter += VOID; break;
			case TOKEN_KW_Bool: counter += BOOL; break;
			case TOKEN_KW_char: counter += CHAR; break;
			case TOKEN_KW_short: counter += SHORT; break;
			case TOKEN_KW_int: counter += INT; break;
			case TOKEN_KW_long: counter += LONG; break;
			case TOKEN_KW_float: counter += FLOAT; break;
			case TOKEN_KW_double: counter += DOUBLE; break;
			
			case TOKEN_KW_unsigned: counter |= UNSIGNED; break;
			case TOKEN_KW_signed: counter |= SIGNED; break;
			
			case TOKEN_KW_register: /* lmao */ break;
			case TOKEN_KW_Noreturn: /* lmao */ break;
			case TOKEN_KW_static: attr->is_static = true; break;
			case TOKEN_KW_typedef: attr->is_typedef = true; break;
			case TOKEN_KW_inline: attr->is_inline = true; break;
			case TOKEN_KW_extern: attr->is_extern = true; break;
			case TOKEN_KW_Thread_local: attr->is_tls = true; break;
			
			case TOKEN_KW_cdecl: break;
			case TOKEN_KW_stdcall: break;
			
			case TOKEN_KW_Complex:
			case TOKEN_KW_Imaginary: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				report(REPORT_ERROR, loc, "Complex types are not supported in CuikC");
				break;
			}
			
			case TOKEN_KW_Vector: {
				// _Vector '(' TYPENAME ',' CONST-EXPR ')'
				if (counter) goto done;	
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				tokens_next(s);
				
				SourceLoc* opening_loc = &s->line_arena[tokens_get(s)->location];
				expect(s, '(');
				
				type = parse_typename(tu, s);
				if (!(tu->types[type].kind >= KIND_CHAR && tu->types[type].kind <= KIND_LONG) &&
					!(tu->types[type].kind == KIND_FLOAT && tu->types[type].kind == KIND_DOUBLE)) {
					report(REPORT_ERROR, loc, "Only integers and floats can be used for _Vector types");
					return 0;
				}
				
				expect(s, ',');
				
				intmax_t count = parse_const_expr(tu, s);
				
				if (count <= 0) {
					report(REPORT_ERROR, loc, "_Vector types must have a positive width");
					return 0;
				}
				
				if (count == 1) {
					report(REPORT_ERROR, loc, "It's not even a _Vector type... that's a scalar...");
					return 0;
				}
				
				if (count > 64) {
					report(REPORT_ERROR, loc, "_Vector type is too wide (%"PRIiMAX", max is 64)", count);
					return 0;
				}
				
				// only allow power of two widths
				if ((count & (count - 1)) != 0) {
					report(REPORT_ERROR, loc, "_Vector types can only have power-of-two widths");
					return 0;
				}
				
				type = new_vector(tu, type, count);
				counter += OTHER;
				
				expect_closing_paren(s, opening_loc);
				tokens_prev(s);
				break;
			}
			case TOKEN_KW_Atomic: {
				tokens_next(s);
				if (tokens_get(s)->type == '(') {
					SourceLoc* opening_loc = &s->line_arena[tokens_get(s)->location];
					tokens_next(s);
					
					type = parse_typename(tu, s);
					counter += OTHER;
					is_atomic = true;
					
					SourceLoc* closing_loc = &s->line_arena[tokens_get(s)->location];
					if (tokens_get(s)->type != ')') {
						report_two_spots(REPORT_ERROR, opening_loc, closing_loc, "expected closing parenthesis for _Atomic", "open", "close?", NULL);
						return 0;
					}
				} else {
					// walk back, we didn't need to read that
					is_atomic = true;
					tokens_prev(s);
				}
				break;
			}
			case TOKEN_KW_const: is_const = true; break;
			case TOKEN_KW_volatile: break;
			case TOKEN_KW_auto: break;
			
			case TOKEN_KW_Typeof: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				
				tokens_next(s);
				if (tokens_get(s)->type != '(') {
					report(REPORT_ERROR, loc, "expected opening parenthesis for _Typeof");
					return 0;
				}
				tokens_next(s);
				
				if (out_of_order_mode) {
					// _Typeof ( SOMETHING )
					TknType terminator;
					size_t current = skip_expression_in_parens(s, &terminator);
					
					if (terminator || terminator == ',') {
						report(REPORT_ERROR, loc, "expected closing parenthesis for _Typeof%s", terminator ? " (got EOF)" : "");
						return 0;
					}
					
					// Add to pending list
					printf("MSG: Add _Typeof to pending list %zu ending at %c\n", current, terminator);
				} else {
					if (is_typename(s)) {
						type = parse_typename(tu, s);
					} else {
						// we don't particularly resolve typeof for expressions immediately.
						// instead we just wait until all symbols are resolved properly
						ExprIndex src = parse_expr(tu, s);
						type = new_typeof(tu, src);
					}
					
					if (tokens_get(s)->type != ')') {
						report(REPORT_ERROR, loc, "expected closing parenthesis for _Typeof");
						return 0;
					}
				}
				break;
			}
			
			case TOKEN_KW_Alignas: {
				SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
				
				tokens_next(s);
				expect(s, '(');
				
				if (out_of_order_mode) {
					// _Alignas ( SOMETHING )
					TknType terminator;
					size_t current = skip_expression_in_parens(s, &terminator);
					
					if (terminator || terminator == ',') {
						report(REPORT_ERROR, loc, "expected closing parenthesis for _Alignas%s", terminator ? " (got EOF)" : "");
						return 0;
					}
					
					// Add to pending list
					printf("MSG: Add _Alignas to pending list %zu ending at %c\n", current, terminator);
				} else {
					if (is_typename(s)) {
						int16_t new_align = parse_typename(tu, s);
						if (new_align == 0) {
							report(REPORT_ERROR, loc, "_Alignas cannot operate with incomplete");
						} else {
							forced_align = new_align;
						}
					} else {
						intmax_t new_align = parse_const_expr(tu, s);
						if (new_align == 0) {
							report(REPORT_ERROR, loc, "_Alignas cannot be applied with 0 alignment", new_align);
						} else if (new_align >= INT16_MAX) {
							report(REPORT_ERROR, loc, "_Alignas(%zu) exceeds max alignment of %zu", new_align, INT16_MAX);
						} else {
							forced_align = new_align;
						}
					}
					
					if (tokens_get(s)->type != ')') {
						report(REPORT_ERROR, loc, "expected closing parenthesis for _Alignas");
						return 0;
					}
				}
				break;
			}
			
			case TOKEN_KW_declspec: {
				// TODO(NeGate): Correctly parse declspec instead of
				// ignoring them.
				tokens_next(s);
				expect(s, '(');
				
				int depth = 1;
				while (depth) {
					if (tokens_get(s)->type == '(') depth++;
					else if (tokens_get(s)->type == ')') depth--;
					
					tokens_next(s);
				}
				
				tokens_prev(s);
				break;
			}
			
			case TOKEN_KW_struct:
			case TOKEN_KW_union: {
				if (counter) goto done;
				SourceLocIndex record_loc = tokens_get(s)->location;
				tokens_next(s);
				
				bool is_union = tkn_type == TOKEN_KW_union;
				
				while (skip_over_declspec(s)) {
					// TODO(NeGate): printf("Don't forget about declspec\n");
				}
				
				Atom name = NULL;
				Token* t = tokens_get(s);
				if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
					name = atoms_put(t->end - t->start, t->start);
					record_loc = t->location;
					tokens_next(s);
				}
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					type = name ? find_incomplete_tag((char*)name) : 0;
					if (type) {
						// can't re-complete a struct
						//assert(!tu->types[type].is_incomplete);
					} else {
						type = new_record(tu, is_union);
						tu->types[type].loc = record_loc;
						tu->types[type].is_incomplete = false;
						tu->types[type].record.name = name;
						
						// can't forward decl unnamed records so we
						// don't track it
						if (name) shput(incomplete_tags, name, type);
					}
					counter += OTHER;
					
					size_t member_count = 0;
					Member* members = tls_save();
					
					// for unions this just represents the max size
					int offset = 0;
					int last_member_size = 0;
					int current_bit_offset = 0;
					// struct/union are aligned to the biggest member alignment
					int align = 0;
					
					while (tokens_get(s)->type != '}') {
						if (skip_over_declspec(s)) continue;
						
						Attribs member_attr = { 0 };
						TypeIndex member_base_type = parse_declspec(tu, s, &member_attr);
						
						// error recovery, if we couldn't parse the typename we skip the declaration
						if (member_base_type == 0) {
							while (tokens_get(s)->type != ';') tokens_next(s);
							
							tokens_next(s);
							continue;
						}
						
						// continues on commas, exists on semicolon
						// int a,   *b,  c[3]    ;
						//     ^    ^~   ^~~~    ^
						//     one  two  three   DONE
						do {
							Decl decl = { 0 };
							TypeIndex member_type = member_base_type;
							
							// not all members have declarators for example
							// char : 3; or struct { ... };
							if (tokens_get(s)->type != ';' &&
								tokens_get(s)->type != ':') {
								decl = parse_declarator(tu, s, member_base_type, false, false); 
								member_type = decl.type;
							}
							
							if (tu->types[member_type].kind == KIND_FUNC) {
								generic_error(s, "Cannot put function types into a struct, try a function pointer");
							}
							
							int member_align = tu->types[member_type].align;
							int member_size = tu->types[member_type].size;
							if (!is_union) {
								int new_offset = align_up(offset, member_align);
								
								// If we realign, reset the bit offset
								if (offset != new_offset) {
									current_bit_offset = last_member_size = 0;
								}
								offset = new_offset;
							}
							
							// Append member
							tls_push(sizeof(Member));
							Member* member = &members[member_count++];
							
							*member = (Member) {
								.type = member_type,
								.name = decl.name,
								.offset = is_union ? 0 : offset,
								.align = member_align
							};
							
							if (tokens_get(s)->type == ':') {
								if (is_union) {
									generic_error(s, "Bitfield... unions... TODO?!");
								}
								
								tokens_next(s);
								
								int bit_width = parse_const_expr(tu, s);
								int bits_in_region = tu->types[member_type].kind == KIND_BOOL ? 1 : (member_size * 8);
								if (bit_width > bits_in_region) {
									generic_error(s, "Bitfield cannot fit in this type.");
								}
								
								if (current_bit_offset + bit_width > bits_in_region) {
									current_bit_offset = 0;
									
									offset = align_up(offset + member_size, member_align);
									member->offset = offset;
								}
								
								member->is_bitfield = true;
								member->bit_offset = current_bit_offset;
								member->bit_width = bit_width;
								
								current_bit_offset += bit_width;
							} else {
								if (is_union) {
									if (member_size > offset) offset = member_size;
								} else {
									offset += member_size;
								}
							}
							
							// the total alignment of a struct/union is based on the biggest member
							last_member_size = member_size;
							if (member_align > align) align = member_align;
							
							// i just wanted to logically split this from the top stuff, this is a breather comment
							if (tokens_get(s)->type == ',') {
								tokens_next(s);
								continue;
							} else if (tokens_get(s)->type == ';') break;
						} while (true);	
						
						expect(s, ';');
					}
					
					if (tokens_get(s)->type != '}') {
						generic_error(s, "Unclosed member list!");
					}
					
					offset = align_up(offset, align);
					tu->types[type].is_incomplete = false;
					tu->types[type].size = offset;
					tu->types[type].align = align;
					
					// put members into more permanent storage
					Member* permanent_store = arena_alloc(&tu->ast_arena, member_count * sizeof(Member), _Alignof(Member));
					memcpy(permanent_store, members, member_count * sizeof(Member));
					
					tu->types[type].record.kids = permanent_store;
					tu->types[type].record.kid_count = member_count;
					
					tls_restore(members);
				} else {
					// TODO(NeGate): must be a forward decl, handle it
					if (name == NULL) generic_error(s, "Cannot have unnamed forward struct reference.");
					
					type = find_incomplete_tag((const char*)name);
					if (!type) {
						type = new_record(tu, is_union);
						tu->types[type].record.name = name;
						tu->types[type].is_incomplete = true;
						
						shput(incomplete_tags, name, type);
					}
					counter += OTHER;
					
					// push back one because we push it forward one later but 
					// shouldn't
					tokens_prev(s);
				}
				break;
			}
			
			case TOKEN_KW_enum: {
				if (counter) goto done;
				tokens_next(s);
				
				Token* t = tokens_get(s);
				Atom name = NULL;
				if (tokens_get(s)->type == TOKEN_IDENTIFIER) {
					name = atoms_put(t->end - t->start, t->start);
					tokens_next(s);
				}
				
				if (tokens_get(s)->type == '{') {
					tokens_next(s);
					
					type = name ? find_incomplete_tag((char*)name) : 0;
					if (type) {
						// can't re-complete a enum
						// TODO(NeGate): error messages
						size_t count = tu->types[type].enumerator.count;
						if (count) {
							generic_error(s, "Cannot recomplete an enumerator");
						}
					} else {
						type = new_enum(tu);
						tu->types[type].is_incomplete = true;
						tu->types[type].enumerator.name = name;
						
						if (name) shput(incomplete_tags, name, type);
					}
					
					// starts at zero and after any entry it increments
					// you can override it by using:
					// identifier = int-const-expr
					int cursor = 0;
					
					size_t count = 0;
					EnumEntry* start = tls_save();
					
					while (tokens_get(s)->type != '}') {
						// parse name
						Token* t = tokens_get(s);
						if (t->type != TOKEN_IDENTIFIER) {
							generic_error(s, "expected identifier for enum name entry.");
						}
						
						Atom name = atoms_put(t->end - t->start, t->start);
						tokens_next(s);
						
						if (tokens_get(s)->type == '=') {
							tokens_next(s);
							
							cursor = parse_const_expr(tu, s);
						}
						
						tls_push(sizeof(EnumEntry));
						start[count++] = (EnumEntry){ name, cursor };
						
						shput(enum_entries, name, cursor);
						cursor += 1;
						
						if (tokens_get(s)->type == ',') tokens_next(s);
					}
					
					if (tokens_get(s)->type != '}') {
						generic_error(s, "Unclosed enum list!");
					}
					
					// move to more permanent storage
					EnumEntry* permanent_store = arena_alloc(&tu->ast_arena, count * sizeof(EnumEntry), _Alignof(EnumEntry));
					memcpy(permanent_store, start, count * sizeof(EnumEntry));
					tls_restore(start);
					
					tu->types[type].enumerator.entries = permanent_store;
					tu->types[type].enumerator.count = count;
					tu->types[type].is_incomplete = false;
				} else {
					type = find_incomplete_tag((char*)name);
					if (!type) {
						type = new_enum(tu);
						tu->types[type].record.name = name;
						tu->types[type].is_incomplete = true;
						
						shput(incomplete_tags, name, type);
					}
					counter += OTHER;
					
					// push back one because we push it forward one later but 
					// shouldn't
					tokens_prev(s);
				}
				break;
			}
			
			case TOKEN_IDENTIFIER: {
				if (counter) goto done;
				
				if (out_of_order_mode) {
					Token* t = tokens_get(s);
					Atom name = atoms_put(t->end - t->start, t->start);
					
					// if the typename is already defined, then reuse that type index
					Symbol* sym = find_global_symbol((const char*) name);
					// if not, we assume this must be a typedef'd type and reserve space
					if (sym != NULL) {
						if (sym->storage_class != STORAGE_TYPEDEF) {
							SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
							report(REPORT_ERROR, loc, "symbol '%s' is not a typedef", name);
							return 0;
						}
						
						type = sym->type;
						counter += OTHER;
					} else {
						printf("MSG: Add typename to pending list '%s'\n", name);
						
						// add placeholder
						Symbol sym = (Symbol){
							.name = name,
							.type = new_blank_type(tu),
							.storage_class = STORAGE_TYPEDEF
						};
						type = sym.type;
						counter += OTHER;
						
						shput(global_symbols, name, sym);
					}
					
					tokens_next(s);
				} else {
					Symbol* sym = find_local_symbol(s);
					if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
						type = sym->type;
						counter += OTHER;
						break;
					}
					
					Token* t = tokens_get(s);
					Atom name = atoms_put(t->end - t->start, t->start);
					
					sym = find_global_symbol((const char*) name);
					if (sym != NULL && sym->storage_class == STORAGE_TYPEDEF) {
						type = sym->type;
						counter += OTHER;
						break;
					}
				}
				
				// if not a typename, this isn't a typedecl
				goto done;
			}
			default: goto done;
		}
		
		switch (counter) {
			case 0: break; // not resolved yet
			case VOID:
			type = TYPE_VOID;
			break;
			case BOOL:
			type = TYPE_BOOL;
			break;
			case CHAR:
			case SIGNED + CHAR:
			type = TYPE_CHAR;
			break;
			case UNSIGNED + CHAR:
			type = TYPE_UCHAR;
			break;
			case SHORT:
			case SHORT + INT:
			case SIGNED + SHORT:
			case SIGNED + SHORT + INT:
			type = TYPE_SHORT;
			break;
			case UNSIGNED + SHORT:
			case UNSIGNED + SHORT + INT:
			type = TYPE_USHORT;
			break;
			case INT:
			case LONG:
			case LONG + INT:
			case SIGNED:
			type = TYPE_INT;
			break;
			case SIGNED + INT:
			case SIGNED + LONG:
			case SIGNED + LONG + INT:
			type = settings.is_windows_long ? TYPE_INT : TYPE_LONG;
			break;
			case UNSIGNED:
			case UNSIGNED + INT:
			type = TYPE_UINT;
			break;
			case UNSIGNED + LONG:
			case UNSIGNED + LONG + INT:
			type = settings.is_windows_long ? TYPE_UINT : TYPE_ULONG;
			break;
			case LONG + LONG:
			case LONG + LONG + INT:
			case SIGNED + LONG + LONG:
			case SIGNED + LONG + LONG + INT:
			type = TYPE_LONG;
			break;
			case UNSIGNED + LONG + LONG:
			case UNSIGNED + LONG + LONG + INT:
			type = TYPE_ULONG;
			break;
			case FLOAT:
			type = TYPE_FLOAT;
			break;
			case DOUBLE:
			case LONG + DOUBLE:
			type = TYPE_DOUBLE;
			break;
			case OTHER:
			assert(type);
			break;
			default:
			generic_error(s, "invalid type");
			break;
		}
		
		tokens_next(s);
	} while (true);
	
	done:;
	SourceLoc* loc = &s->line_arena[tokens_get(s)->location];
	if (type == 0) {
		report(REPORT_ERROR, loc, "unknown typename");
		return 0;
	}
	
	if (is_atomic || is_const || (forced_align && tu->types[type].align != forced_align)) {
		if (forced_align && forced_align < tu->types[type].align) {
			report(REPORT_ERROR, loc, "forced alignment %d cannot be smaller than original alignment %d", forced_align, tu->types[type].align);
			return 0;
		}
		
		type = copy_type(tu, type);
		
		if (forced_align) {
			tu->types[type].align = forced_align;
		}
		
		tu->types[type].is_atomic = is_atomic;
		tu->types[type].is_const = is_const;
	}
	
	return type;
}

static bool is_typename(TokenStream* restrict s) {
	Token* t = tokens_get(s);
	
	switch (t->type) {
		case TOKEN_KW_void: case TOKEN_KW_char: case TOKEN_KW_short:
		case TOKEN_KW_int: case TOKEN_KW_long: case TOKEN_KW_float:
		case TOKEN_KW_double: case TOKEN_KW_Bool: case TOKEN_KW_signed:
		case TOKEN_KW_unsigned: case TOKEN_KW_struct: case TOKEN_KW_union:
		case TOKEN_KW_enum: case TOKEN_KW_extern: case TOKEN_KW_static:
		case TOKEN_KW_typedef: case TOKEN_KW_inline: case TOKEN_KW_const:
		case TOKEN_KW_volatile: case TOKEN_KW_declspec: case TOKEN_KW_Thread_local:
		case TOKEN_KW_Alignas: case TOKEN_KW_Atomic: case TOKEN_KW_auto:
		case TOKEN_KW_cdecl: case TOKEN_KW_stdcall: case TOKEN_KW_Typeof:
		return true;
		
		case TOKEN_IDENTIFIER: {
			// good question...
			Token* t = tokens_get(s);
			Atom name = atoms_put(t->end - t->start, t->start);
			
			Symbol* loc = find_local_symbol(s);
			if (loc != NULL && loc->storage_class == STORAGE_TYPEDEF) return true;
			
			Symbol* glob = find_global_symbol((const char*)name);
			if (glob != NULL && glob->storage_class == STORAGE_TYPEDEF) return true;
			
			return false;
		}
		
		default:
		return false;
	}
}
