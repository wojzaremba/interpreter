// Arrays.

int main() {
  int[5] a;
  sort(a,5);
  for (int i=0;i<5;i++) {
  	printInt(a[i]);
  }
  return 0;
}

void sort(int[] tab, int size)
{
  for (int i=0;i<size;i++) {
  	tab[i] = i*i*i;
  }
  return;
}

