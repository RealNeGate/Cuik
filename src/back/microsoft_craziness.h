#pragma once

#include <stdlib.h>

//
//
// HOW TO USE THIS CODE
//
// The purpose of this file is to find the folders that contain libraries
// you may need to link against, on Windows, if you are linking with any
// compiled C or C++ code. This will be necessary for many non-C++ programming
// language environments that want to provide compatibility.
//
// We find the place where the Visual Studio libraries live (for example,
// libvcruntime.lib), where the linker and compiler executables live
// (for example, link.exe), and where the Windows SDK libraries reside
// (kernel32.lib, libucrt.lib).
//
// We all wish you didn't have to worry about so many weird dependencies,
// but we don't really have a choice about this, sadly.
//
// I don't claim that this is the absolute best way to solve this problem,
// and so far we punt on things (if you have multiple versions of Visual Studio
// installed, we return the first one, rather than the newest). But it
// will solve the basic problem for you as simply as I know how to do it,
// and because there isn't too much code here, it's easy to modify and expand.
//
//
// Here is the API you need to know about:
//

//
// Call find_visual_studio_and_windows_sdk, look at the resulting
// paths, then call free_resources on the result.
//
// Everything else in this file is implementation details that you
// don't need to care about.
//

#ifdef __cplusplus
extern "C" {
#endif
	typedef struct MicrosoftCraziness_Find_Result {
		int windows_sdk_version;   // Zero if no Windows SDK found.
		
		wchar_t* windows_sdk_root;
		wchar_t* windows_sdk_include;
		wchar_t* windows_sdk_um_library_path;
		wchar_t* windows_sdk_ucrt_library_path;
		
		wchar_t* vs_exe_path;
		wchar_t* vs_include_path;
		wchar_t* vs_library_path;
	} MicrosoftCraziness_Find_Result;
	
	MicrosoftCraziness_Find_Result MicrosoftCraziness_find_visual_studio_and_windows_sdk();
	
	inline void MicrosoftCraziness_free_resources(MicrosoftCraziness_Find_Result* result) {
		free(result->windows_sdk_root);
		free(result->windows_sdk_um_library_path);
		free(result->windows_sdk_ucrt_library_path);
		free(result->vs_exe_path);
		free(result->vs_library_path);
	}
	
#ifdef __cplusplus
}
#endif
