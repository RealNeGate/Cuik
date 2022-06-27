#pragma once

// Makes all unhandled exceptions pipe to a custom handler in
// crash_handler.c
void hook_crash_handler();
