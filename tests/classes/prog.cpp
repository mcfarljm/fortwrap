#include "FortWrap.h"

int main(void)
{
  Shape *s;
  Polygon *p;

  Circle c1(1);
  if (c1.get_area() != 3)
    return 1;

  s = new Circle(2);
  if (s->get_area() != 12)
    return 3;
  delete s;

  Square sq1(2);
  if (sq1.get_area() != 4)
    return 5;
  if (sq1.num_sides() != 4)
    return 6;

  s = new Square(3);
  if (s->get_area() != 9)
    return 7;
  delete s;

  p = new Square(4);
  if (p->get_area() != 16)
    return 9;
  if (p->num_sides() != 4)
    return 10;

  delete p;
  
  return 0;
}

