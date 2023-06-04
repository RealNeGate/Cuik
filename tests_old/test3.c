
int WinMain() {
	char name[4];
	name[0] = 84;
	name[1] = 83;
	name[2] = 84;
	name[3] = 0;

	WNDCLASSA wc;
	wc.hInstance = GetModuleHandleA(0);
	wc.lpfnWndProc = DefWindowProcA;
	wc.lpszClassName = &name;
	RegisterClassA(&wc);
	
	HWND window = CreateWindowExA(262144, &name,
									&name, 768,
									400, 400, 1600, 900,
								    0, 0, wc.hInstance, 0);

	ShowWindow(window, SW_SHOWDEFAULT);
	SetFocus(window);
	
	while (1) {
		MSG msg;
		GetMessageA(&msg, 0, 0, 0);
		TranslateMessage(&msg);
		DispatchMessageA(&msg);
	}

	return 0;
}
