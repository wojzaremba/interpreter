int main() {
	int x = readInt();
	int[x] array;
	for (int i=0;i<x;i++) array[i] = readInt();
	qs(array, 0, x-1);
	for (int i=0;i<x;i++) printInt(array[i]);
	return 0;
}

void swap(int[] a, int x,int y) {
   int temp;
   temp = a[x];
   a[x] = a[y];
   a[y] = temp;
}
 
int choose_pivot(int i,int j ) {
   return((i+j) /2);
}
 
void qs(int[] list, int m, int n) {
   int key,i,j,k;
   if( m < n) {
      k = choose_pivot(m,n);
      swap(list, m, k);
      key = list[m];
      i = m+1;
      j = n;
      while(i <= j)
      {
         while((i <= n) && (list[i] <= key))
                i++;
         while((j >= m) && (list[j] > key))
                j--;
         if( i < j)
                swap(list, i, j);
      }
      swap(list, m, j);
      qs(list,m,j-1);
      qs(list,j+1,n);
   }
}
