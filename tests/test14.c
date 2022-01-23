extern int putchar(int ch);

int m=1811939329,N=1,t[1<<26]={2},a,*p,i,e=73421233,s,c,U=1;

void g(int d, int h){
	for(i=s;i<1<<25;i*=2)
		d=d*1LL*d%m;
	
	for (p=t;p<t+N;p+=s) {
		for (i=s,c=1;i;i--) {
			a=p[s]*(h?c:1LL)%m;
			p[s]=(m*1U+*p-a)*(h?1LL:c)%m;
			*p=(a*1U+*p)%m;
			p++;
			c=c*1LL*d%m;
		}
	}
}

int main(){
	putchar('A');
	
	while(e/=2){
		N*=2;
		U=U*1LL*(m+1)/2%m;
		
		for (s=N;s/=2;) {
			g(136,0);
		}
		
		for (p=t;p<t+N;p++) {
			*p=*p*1LL**p%m*U%m;
		}
		
		for(s=1;s<N;s*=2) g(839354248,1);
		
		for(a=0,p=t;p<t+N;) {
			a+=*p<<(e&1);
			*p++=a%10;
			a/=10;
		}	
	}
	
	while(!*--p);
	
	for(t[0]--;p>=t;) {
		putchar(48+*p--);
	}
}
