#include "FortWrap.h"

int main(void)
{

  Circle c1(1);
  if (c1.get_area() != 3) {
    return 1;
  }

  Shape *s1;
  s1 = new Circle(2);
  if (s1->get_area() != 12) {
    return 3;
  }

  return 0;
}

