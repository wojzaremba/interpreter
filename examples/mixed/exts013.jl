// Assignment as value.

int main() {
  int a = 10;
  int[a] b;
  int c;
  c = b[a / 3] = a;
  printInt(a);
  printInt(b[3]);
  printInt(c);
  return 0;
}

