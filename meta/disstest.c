#include "a64decoder.h"
#include "a64disassembler.h"
#include "a64bytes.h"

int main(int argc, char** argv) {
	for (int i = 0; i < (sizeof(bytes)/sizeof(bytes[0])); i++) {
		TB_A64_Inst info = {0};
		printf("%d\t", i + 3);
		decode(&info, (uint8_t*)(&bytes[i]));
		disassemble(&info);
	}
}
