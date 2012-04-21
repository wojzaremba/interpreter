// Arrays.

int getArrayLength()
{
  return 5;
}

int main() {
  int[5] b;
  int[getArrayLength()] a;
  int i;
  for (i = 0; i < getArrayLength(); i = i + 1)
  {
    a[i] = i;
    b[i] = getArrayLength() - i;
  }
  for (int i=0;i<5;i++) {
     printInt(a[i]*b[i]);
  }
  return 0;
}

