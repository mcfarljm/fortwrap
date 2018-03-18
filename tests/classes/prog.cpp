#include "FortWrap.h"

int main(void)
{
  Shape *s;
  Polygon *p;

  if (Circle::is_round() != 1)
    return 1;

  if (Square::is_round() != 0)
    return 2;

  if (Square::get_area_static(3) != 9)
    return 3;

  Circle c1(1);
  if (c1.get_area() != 3)
    return 5;
  if (c1.get_diameter() != 2)
    return 6;

  s = new Circle(2);
  if (s->get_area() != 12)
    return 7;
  delete s;

  Square sq1(2);
  if (sq1.get_area() != 4)
    return 9;
  if (sq1.num_sides() != 4)
    return 10;

  s = new Square(3);
  if (s->get_area() != 9)
    return 11;
  delete s;

  p = new Square(4);
  if (p->get_area() != 16)
    return 13;
  if (p->num_sides() != 4)
    return 14;

  delete p;
  
  return 0;
}

