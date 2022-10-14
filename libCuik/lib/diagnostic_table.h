// Generic errors
DIAG(expected_ident, REPORT_ERROR, "expected identifier")
DIAG(unexpected_eof, REPORT_ERROR, "unexpected EOF")

// Preprocessor errors
DIAG(unknown_directive,  REPORT_ERROR, "unknown directive %0", DIAG_STRING)
DIAG(too_many_if_scopes, REPORT_ERROR, "exceeded max #if scope depth")
DIAG(too_many_endifs,    REPORT_ERROR, "too many #endif")

DIAG(pp_message, REPORT_INFO,    "message %0", DIAG_STRING)
DIAG(pp_warning, REPORT_WARNING, "warning %0", DIAG_STRING)
DIAG(pp_error,   REPORT_ERROR,   "error %0", DIAG_STRING)

DIAG(filename_too_long, REPORT_ERROR, "#include filename is too long", DIAG_VOID)
DIAG(too_many_macros, REPORT_ERROR, "too many macros, out of memory")

// Parser
DIAG(expected_comma, REPORT_ERROR, "Expected a comma after %0", DIAG_CSTR)
DIAG(invalid_type, REPORT_ERROR, "could not parse type")

// Type check
DIAG(param_mismatch, REPORT_ERROR, "parameter mismatch (got %0, expected %1)", DIAG_INT, DIAG_INT)

#undef DIAG