#include "FortWrap.h"

int main(void)
{
  Shape *s;
  Polygon *p;

  Circle c1(1);
  if (c1.get_area() != 3)
    return 1;
  if (c1.get_diameter() != 2)
    return 2;
  if (! c1.is_circle())
    return 20;

  s = new Circle(2);
  if (s->get_area() != 12)
    return 3;
  delete s;

  Square sq1(2);
  if (sq1.get_area() != 4)
    return 5;
  if (sq1.num_sides() != 4)
    return 6;
  if (! sq1.is_square())
    return 21;

  s = new Square(3);
  if (s->get_area() != 9)
    return 7;

  p = new Square(4);
  if (p->get_area() != 16)
    return 9;
  if (p->num_sides() != 4)
    return 10;

  p->dummy();

  if (s->add_area(p) != 25)
    return 30;

  Polygon *p2 = new Square(5);
  
  if (p->poly_add(p2) != 16 + 25)
    return 31;

  Object o(2);
  if (o.get_value() != 2)
    return 40;

  delete p;
  delete s;
  delete p2;
  
  return 0;
}

