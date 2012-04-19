/* Testowanie generalne */

int fooBar(int x, int y, int z)
{
  int a = 3, b = 1, c = 2;
  int d;
  d = x * a + y * b + c * z;
  return d;
}

int main()
{
  int x = 0, y = 1;
  printInt(fooBar(y, x, 73));
  return 0;
}

