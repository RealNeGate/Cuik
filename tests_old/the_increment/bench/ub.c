#include <stdint.h>
#include <limits.h>

void unreachable_1 (void)
{
  int x;
  x++;
}
 
void unreachable_2 (void)
{
  1/0;
}
 
void unreachable_3 (void)
{
  int *p = 0; 
  *p;
}
 
void unreachable_4 (void)
{
  int x = INT_MAX;
  x++;
}
 
void unreachable_5 (void)
{
  __builtin_unreachable ();
}

