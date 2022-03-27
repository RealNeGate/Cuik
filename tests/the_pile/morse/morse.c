extern void* malloc(unsigned long long);
extern int _write(int fd, const void *buffer, unsigned int count);
extern char* gets(char* buffer);

#define DIT (
#define DAH )
#define __DAH   ++
#define DITDAH  *
#define DAHDIT  for
#define DIT_DAH malloc
#define DAH_DIT gets

char _DAH_[]="ETIANMSURWDKGOHVFaLaPJBXCYZQb54a3d2f16g7c8a90l?e'b.s;i,d:";

int __DIT(int n) {
	_write(1, &n, 1);
}

int _DAH(int n) {
	__DIT(n > 3 ? _DAH(n >> 1) : '\0');
	return n & 1 ? '-' : '.';
}

int main() {
	char *_DIT, *DAH_, *DIT_, *_DIT_;
	
	for (_DIT = malloc(81), DIT_=_DIT++; _DIT==gets ( _DIT ); __DIT ('\n')) {
		for (DAH_=_DIT; * DAH_; __DIT(*_DIT_ ? _DAH(*DIT_) : '?'), __DIT (' '), DAH_++) {
			for (*DIT_=2,_DIT_=_DAH_;
				*_DIT_ && (*_DIT_ != (*DAH_>='a' ? *DAH_ & 223 : *DAH_));
				(*DIT_)++, _DIT_++) {
				*DIT_ += (*_DIT_ >= 'a' ? *_DIT_ - 'a':0);
			}
		}
	}
}
