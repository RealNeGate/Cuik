
#line 1 "tests/test.c"	
/* line   1 */	typedef int Index ; 
/* line   3 */	void ExitProcess ( unsigned int code ) ; 
/* line   4 */	int foo ( ) { return 16 ; } 
/* line   5 */	int bar ( ) { int x = 16 ; return x ++ ; } 
/* line   6 */	int baz ( ) { int * x = ( void * ) 0 ; x ++ ; x -- ; x += 16 ; x -= 16 ; * x ++ = 16 ; return * x ; } 
/* line   7 */	Index foo2 ( ) { return 16 ; } 
/* line   8 */	int params ( int x , int y ) { return x + y ; } 
/* line  10 */	int baz2 ( int * a , int * b ) 
/* line  11 */	{ 
/* line  12 */	* a = 5 ; 
/* line  13 */	* b = 6 ; 
/* line  14 */	return * a + * b ; 
/* line  15 */	} 
/* line  17 */	int bar2 ( int x ) { 
/* line  18 */	int y = x + 1 , z = x + 2 ; 
/* line  19 */	int w , a ; 
/* line  20 */	return y + z ; 
/* line  21 */	} 
/* line  23 */	int sum_of ( int n , int * arr ) { 
/* line  24 */	int i = 0 ; 
/* line  25 */	int sum = 0 ; 
/* line  26 */	while ( i < n ) { 
/* line  27 */	sum += arr [ i ] ; 
/* line  28 */	} 
/* line  29 */	return sum ; 
/* line  30 */	} 
/* line  32 */	int sum_of2 ( int n , int * arr ) { 
/* line  33 */	int sum = 0 ; 
/* line  34 */	for ( int i = 0 ; i < n ; i ++ ) { 
/* line  35 */	sum += arr [ i ] ; 
/* line  36 */	} 
/* line  37 */	return sum ; 
/* line  38 */	} 
/* line  40 */	int fast_fib ( int n ) { 
/* line  41 */	int a = 0 ; 
/* line  42 */	int b = 1 ; 
/* line  44 */	int i = n - 1 ; 
/* line  45 */	while ( i -- ) { 
/* line  46 */	int c = a + b ; 
/* line  47 */	a = b ; 
/* line  48 */	b = c ; 
/* line  49 */	} 
/* line  51 */	return b ; 
/* line  52 */	} 
/* line  54 */	int main ( int argc , char * argv [ ] ) { 
/* line  55 */	int a = params ( 1 , 56 ) ; 
/* line  56 */	ExitProcess ( 0 ) ; 
/* line  58 */	int x = 16 ; 
/* line  59 */	int * ptr = & x ; 
/* line  60 */	int * * ptr2 = & ptr ; 
/* line  61 */	int val = * * ptr2 ; 
/* line  63 */	x = 640 ; 
/* line  64 */	x += 16 ; 
/* line  65 */	x -= 16 ; 
/* line  66 */	x *= 64 ; 
/* line  67 */	x /= 16 ; 
/* line  68 */	x &= 16 ; 
/* line  69 */	x |= 16 ; 
/* line  70 */	x ^= 16 ; 
/* line  71 */	x <<= 16 ; 
/* line  73 */	struct Point * start = ( struct Point * ) 0 ; 
/* line  74 */	start -> x = 16 ; 
/* line  76 */	int y = ( x * 2 ) ; 
/* line  77 */	if ( y ) y = 16 ; 
/* line  78 */	if ( x ) { y = 16 ; } else { x = 16 ; } 
/* line  80 */	while ( x ) { x -= 1 ; } 
/* line  82 */	short apple [ 16 ] ; 
/* line  83 */	apple [ 0 ] = 16 ; 
/* line  85 */	char table [ 8 ] [ 8 ] ; 
/* line  86 */	char z = ( table [ 0 ] [ 0 ] + 1 ) ; 
/* line  88 */	int w = 4 ; 
/* line  89 */	do { table [ z ] [ z ] = 16 ; } while ( w -- ) ; 
/* line  91 */	if ( x && z ) { 
/* line  92 */	x = 16 ; 
/* line  93 */	} 
/* line  95 */	float abc = 0.0f ; 
/* line  96 */	float def = 4.34f ; 
/* line  97 */	return y ; 
/* line  98 */	} 
/* line 100 */	struct Point { 
/* line 101 */	int x , y ; 
/* line 102 */	} ; 
/* line   1 */	 