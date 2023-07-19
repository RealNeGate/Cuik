
int count_newlines(int n, char arr[]) {
	int count = 0;
	for (int i = 0; i < n; i++) {
		count += (arr[i] == 10);
	}

	return count;
}
