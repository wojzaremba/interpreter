int main() {
	int x = readInt();
	int a=1, b=1;
	for (int i=2;i<=x;i++) {
		b = a + b;
		a = b - a;
	}
	printInt(b);
	return 0;
}
