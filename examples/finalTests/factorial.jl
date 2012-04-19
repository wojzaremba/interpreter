int main() {
	int x = readInt();
	printInt(fac(x));
	return 0;
}

int fac(int x) {
	if (x==0) return 1;
	return x*fac(x-1);
}
