int main() {
  int a = 2, b = 6, c = 100;
  printInt(++a);
  c = (b=++a)+5;
  printInt(b);
  printInt(c);
  printInt(c++);
  return 0;
}
