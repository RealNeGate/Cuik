
typedef unsigned long long size_t;

void* memset(void* s, int c, size_t n) {
	unsigned char* ptr = (unsigned char*) s;
	unsigned char data = (unsigned char)c;
	
	size_t i = 0;
	while (i < n) {
		ptr[i] = data;
		i += 1;
	}
	
	return s;
}

void* memcpy(void* restrict s1, const void* restrict s2, size_t n) {
	unsigned char* ptr1 = (unsigned char*) s1;
	unsigned char* ptr2 = (unsigned char*) s2;
	
	size_t i = 0;
	while (i < n) {
		ptr1[i] = ptr2[i];
		i += 1;
	}
	
	return s1;
}

