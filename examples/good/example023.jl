/* if */

int singleUnmatched()
{
  if (true)
    return 1;
}

int singleMatched()
{
  if (true)
    return 2;
  else
    return 3;
}

int firstUnmatchedSecondMatched()
{
  if (true)
    if (false)
      return 3;
    else
      return 4;
}

int firstRetSecondMatched()
{
  if (true)
    return 5;
  else
    if (false)
      return 6;
    else
      return 7;
}

int firstMatchedSecondRet()
{
  if (true)
    if (false)
      return 8;
    else
      return 9;
  else
    return 10;
}

int firstMatchedSecondMatched()
{
  if (true)
    if (false)
      return 11;
    else
      return 12;
  else
    if (true)
      return 13;
    else
      return 14;
}

// incorrect
/*
int loopTest()
{
  if (true)
    while (false)
      if (true)
        while (false)
          if (true)
            return 15;
          else
            return 16;
      else
        return 17;
}
*/

int main()
{
  int x = 0, y = 1;
  return 0;
}

