int main() {
	int x = readInt();
	printInt(fib(x));
	return 0;
}

int fib(int x) {
	if (x==0 || x==1) return 1;
	return fib(x-1) + fib(x-2);
}
