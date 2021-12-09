
typedef unsigned long long size_t;

#define NULL ((void*) 0)

void* memset(void* s, int c, size_t n) {
	unsigned char* ptr = (unsigned char*) s;
	unsigned char data = (unsigned char) c;
	
	size_t i = 0;
	while (i < n) {
		ptr[i] = data;
		i++;
	}
	
	return s;
}

void* memcpy(void* restrict s1, const void* restrict s2, size_t n) {
	unsigned char* ptr1 = (unsigned char*) s1;
	unsigned char* ptr2 = (unsigned char*) s2;
	
	size_t i = 0;
	while (i < n) {
		ptr1[i] = ptr2[i];
		i++;
	}
	
	return s1;
}

int memcmp(const void* s1, const void* s2, size_t n) {
	unsigned char* ptr1 = (unsigned char*) s1;
	unsigned char* ptr2 = (unsigned char*) s2;
	
	size_t i = 0;
	while (i < n) {
		if (ptr1[i] != ptr2[i]) return ptr1[i] - ptr2[i];
		i++;
	}
	
	return 0;
}

void* memchr(const void* s, int c, size_t n) {
	unsigned char* ptr = (unsigned char*) s;
	
	size_t i = 0;
	while (i < n) {
		if (ptr[i] == c) return &ptr[i];
		i++;
	}
	
	return NULL;
}

int strcmp(const char* s1, const char* s2) {
	while (1) {
		int c = *s1 - *s2;
		if (c != 0) return c;
		s1++; s2++;
	}
	
	return 0;
}

int strncmp(const char* s1, const char* s2, size_t n) {
	size_t i = 0;
	while (i < n) {
		int c = s1[i] - s2[i];
		if (c != 0) return c;
		i++;
	}
	
	return 0;
}
