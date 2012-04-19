// Assignment as value.

int main() {
  int a = 10;
  int b;
  int[b = a * 5] c;
  c[b = a / 3] = 10;
  printInt(b);
  printInt(c[3]);
  return 0;
}

