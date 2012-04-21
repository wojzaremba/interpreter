// Casting.

int main() {
  int[10] a;
  a[1] = 3;
  boolean b = (boolean)a[1];
  printInt(a[1]);
  if (b) printString("true");
  return 0;
}

