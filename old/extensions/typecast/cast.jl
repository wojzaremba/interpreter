int main() {
  int a = (int)(-1);
  printInt(a);
  a = (int)(-2.);
  printInt(a);
  a = (int)true;
  printInt(a);
  a = (int)false;
  printInt(a);
  double b = (double)(-5);
  printDouble(b);
  b = (double)6;
  printDouble(b);
  b = (double)6.;
  printDouble(b);  
  b = (double)true;
  printDouble(b);
  b = (double)false;
  printDouble(b);
  boolean c = (boolean)(-1);
  printBoolean(c);
  c = (boolean)1;
  printBoolean(c);
  c = (boolean)(-1.);
  printBoolean(c);
  c = (boolean)1.;
  printBoolean(c);
  c = (boolean)0.;
  printBoolean(c);
  c = (boolean)0;
  printBoolean(c);      
  return 0;
}

void printBoolean(boolean b) {
	if (b) {
		printInt(1);
	} else
	{
		printInt(0);
	}
	return;
}