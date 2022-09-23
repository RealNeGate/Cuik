__declspec(dllexport) extern int MessageBoxA(
  void*        hWnd,
  const char*  lpText,
  const char*  lpCaption,
  unsigned int uType
);

void mainCRTStartup() {
    MessageBoxA(0, "hello, world", "caption", 0);
}
