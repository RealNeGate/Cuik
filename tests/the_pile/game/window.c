
LRESULT main_wnd_proc(HWND wnd, UINT message, WPARAM wparam, LPARAM lparam) {
	if (message == WM_DESTROY) {
		ExitProcess(0);
	}
	
	return DefWindowProcA(wnd, message, wparam, lparam);
}
