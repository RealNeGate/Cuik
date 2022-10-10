// Preprocessor errors
DIAG(unknown_directive, REPORT_ERROR, "unknown directive %0", DIAG_STRING)

DIAG(pp_message, REPORT_INFO,    "message %0", DIAG_STRING)
DIAG(pp_warning, REPORT_WARNING, "warning %0", DIAG_STRING)
DIAG(pp_error,   REPORT_ERROR,   "error %0", DIAG_STRING)

DIAG(filename_too_long, REPORT_ERROR, "#include filename is too long", DIAG_VOID)

// Type check errors
DIAG(param_mismatch, REPORT_ERROR, "%0 requires %1 arguments", DIAG_CSTR, DIAG_INT)

#undef DIAG