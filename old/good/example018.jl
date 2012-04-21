/* if */

void loopTest()
{
  int i;
  if (true)
    for (i = 0; i < 3; i++)
      if (true)
        return;
}

int main()
{
  loopTest();
  return 0;
}

