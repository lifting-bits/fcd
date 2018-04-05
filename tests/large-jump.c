// gcc -o large-jump.out large-jump.c
int i;
int main(int a)
{
  switch(a)
  {
    case 0:   i = 20; break;
    case 1:   i = 50; break;
    case 2:   i = 29; break;
    case 10:  i = 20; break;
    case 11:  i = 50; break;
    case 12:  i = 29; break;
    case 20:  i = 20; break;
    case 21:  i = 50; break;
    case 22:  i = 29; break;
    case 52:  i = 79; break;
    case 110: i = 27; break;
    default:  i = 77; break;
  }
  return i;
}
