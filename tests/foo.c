void* GetModuleHandleA(const char* module_name);

void* foo(void) {
	return GetModuleHandleA((void*) 0);
}
