/* if */

void loopTest()
{
  if (true)
    while (false)
      if (true)
        return;
}

int main()
{
  loopTest();
  return 0;
}

