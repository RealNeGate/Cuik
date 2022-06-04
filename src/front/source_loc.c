#include "parser.h"

/*
	SourceLine* line;
	short columns;
	short length;
*/

SourceLoc merge_source_locations(const SourceLoc* start, const SourceLoc* end) {
    if (start->line != end->line) {
        return *start;
    }

    // We can only merge if it's on the same line... for now...
    size_t start_columns = start->columns;
    size_t end_columns = end->columns + end->length;
    if (start_columns == end_columns) {
        end_columns = start_columns + 1;
    } else if (start_columns > end_columns) {
        // swap
        printf("TIMES\n");

        size_t tmp = start_columns;
        start_columns = end_columns;
        end_columns = tmp;
    }

    return (SourceLoc){ start->line, start_columns, end_columns - start_columns };
}

SourceLocIndex generate_location(TranslationUnit* tu, const SourceLoc* loc) {
    size_t i = arraddnindex(tu->source_locations, 1);
    tu->source_locations[i] = *loc;
    return i;
}

SourceLocIndex generate_location_range(TranslationUnit* tu, const SourceLoc* start, const SourceLoc* end) {
    size_t i = arraddnindex(tu->source_locations, 1);
    tu->source_locations[i] = merge_source_locations(start, end);
    return i;
}
