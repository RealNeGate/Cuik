
int foo(int* a) {
    static const struct {
        const char * zName ; 
        const char * zCols ; 
    } table[] = {
        { "sqlite_stat1" , "tbl,idx,stat" } , 
        { "sqlite_stat4" , 0 } , 
        { "sqlite_stat3" , 0 } , 
    };

    return ++*a;
}
