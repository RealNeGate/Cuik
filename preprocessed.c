
#line 1 "tests/test.c"	
/* line   1 */	typedef int Index ; 
/* line   3 */	void ExitProcess ( unsigned int code ) ; 
/* line   4 */	int foo ( ) { return 16 ; } 
/* line   5 */	int bar ( ) { int x = 16 ; return x ++ ; } 
/* line   6 */	int baz ( ) { int * x = ( void * ) 0 ; x ++ ; x -- ; x += 16 ; x -= 16 ; * x ++ = 16 ; return * x ; } 
/* line   7 */	Index foo2 ( ) { return 16 ; } 
/* line   8 */	int params ( int x , int y ) { return x + y ; } 
/* line  10 */	int counter = 0 ; 
/* line  12 */	int beans ( ) { 
/* line  13 */	counter += 4 ; 
/* line  14 */	} 
/* line  16 */	int baz2 ( int * a , int * b ) 
/* line  17 */	{ 
/* line  18 */	* a = 5 ; 
/* line  19 */	* b = 6 ; 
/* line  20 */	return * a + * b ; 
/* line  21 */	} 
/* line  23 */	int bar2 ( int x ) { 
/* line  24 */	int y = x + 1 , z = x + 2 ; 
/* line  25 */	int w , a ; 
/* line  26 */	return y + z ; 
/* line  27 */	} 
/* line  29 */	int sum_of ( int n , int * arr ) { 
/* line  30 */	int i = 0 ; 
/* line  31 */	int sum = 0 ; 
/* line  32 */	while ( i < n ) { 
/* line  33 */	sum += arr [ i ] ; 
/* line  34 */	} 
/* line  35 */	return sum ; 
/* line  36 */	} 
/* line  38 */	int sum_of2 ( int n , int * arr ) { 
/* line  39 */	int sum = 0 ; 
/* line  40 */	for ( int i = 0 ; i < n ; i ++ ) { 
/* line  41 */	sum += arr [ i ] ; 
/* line  42 */	} 
/* line  43 */	return sum ; 
/* line  44 */	} 
/* line  46 */	int fast_fib ( int n ) { 
/* line  47 */	int a = 0 ; 
/* line  48 */	int b = 1 ; 
/* line  50 */	int i = n - 1 ; 
/* line  51 */	while ( i -- ) { 
/* line  52 */	int c = a + b ; 
/* line  53 */	a = b ; 
/* line  54 */	b = c ; 
/* line  55 */	} 
/* line  57 */	return b ; 
/* line  58 */	} 
/* line  60 */	int main ( int argc , char * argv [ ] ) { 
/* line  61 */	int a = params ( 1 , 56 ) ; 
/* line  62 */	ExitProcess ( 0 ) ; 
/* line  64 */	int x = 16 ; 
/* line  65 */	int * ptr = & x ; 
/* line  66 */	int * * ptr2 = & ptr ; 
/* line  67 */	int val = * * ptr2 ; 
/* line  69 */	x = 640 ; 
/* line  70 */	x += 16 ; 
/* line  71 */	x -= 16 ; 
/* line  72 */	x *= 64 ; 
/* line  73 */	x /= 16 ; 
/* line  74 */	x &= 16 ; 
/* line  75 */	x |= 16 ; 
/* line  76 */	x ^= 16 ; 
/* line  77 */	x <<= 16 ; 
/* line  79 */	struct Point * start = ( struct Point * ) 0 ; 
/* line  80 */	start -> x = 16 ; 
/* line  82 */	int y = ( x * 2 ) ; 
/* line  83 */	if ( y ) y = 16 ; 
/* line  84 */	if ( x ) { y = 16 ; } else { x = 16 ; } 
/* line  86 */	while ( x ) { x -= 1 ; } 
/* line  88 */	short apple [ 16 ] ; 
/* line  89 */	apple [ 0 ] = 16 ; 
/* line  91 */	char table [ 8 ] [ 8 ] ; 
/* line  92 */	char z = ( table [ 0 ] [ 0 ] + 1 ) ; 
/* line  94 */	int w = 4 ; 
/* line  95 */	do { table [ z ] [ z ] = 16 ; } while ( w -- ) ; 
/* line  97 */	if ( x && z ) { 
/* line  98 */	x = 16 ; 
/* line  99 */	} 
/* line 105 */	return y ; 
/* line 106 */	} 
/* line 108 */	struct Point { 
/* line 109 */	int x , y ; 
/* line 110 */	} ; 
/* line   1 */	 