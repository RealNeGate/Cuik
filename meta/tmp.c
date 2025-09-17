#include <stdio.h>
#if DISS == 1
#include "a64disassembler1.h"
#elif DISS == 2
#include "a64disassembler2.h"
#elif DISS == 3
#include "a64disassembler3.h"
#endif
#include "a64bytes.h"

int main(int argc, char** argv) {
	volatile unsigned long out = 0;
	for (int n = 0; n < 100; n++) {
		for (int i = 0; i < (sizeof(bytes)/sizeof(bytes[0])); i++) {
			uint32_t inst = bytes[i];
			char* str = (char*)disassemble(inst);
			uint8_t* byte = (uint8_t*)&inst;
			out += (unsigned long)inst ^ (unsigned long)str[0];
			// printf("0x1%08x %02x %02x %02x %02x: %s\n", i * 4, byte[0], byte[1], byte[2], byte[3], str);
		}
	}
	printf("%016lx", out);
}
