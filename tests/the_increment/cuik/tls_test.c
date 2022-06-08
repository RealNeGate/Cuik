
#ifdef __CUIKC__
_Thread_local int apple = 16;
#else
__declspec(thread) int apple = 16;
#endif

int main() {
	apple++;
	return 0;
}

