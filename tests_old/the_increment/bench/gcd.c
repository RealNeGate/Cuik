int gcd1(int a, int b) {
    int t;
    while (b != 0) {
        t = b; 
        b = a % b;  
        a = t;
    }
    return a;
}

int gcd2(int a, int b) {
    while (a != b) {
        if (a > b) a -= b;
        else b -= a;
    }
	
    return a;
}

int gcd3(int a, int b) {
    if (b == 0) return a;
	
    return gcd3 ( b, a % b );
}
