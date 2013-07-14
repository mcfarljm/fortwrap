#include "FortWrap.h"
#include <iostream>

using namespace std;

int main(void)
{

  Square sq1(2);
  if (sq1.get_area() != 4)
    return 1;
  if (sq1.square_area_dt() != 4)
    return 2;

  Circle c1(4);
  if (c1.get_area() != 48)
    return 3;

  Shape *s1, *s2;
  s1 = new Square(3);
  if (s1->get_area() != 9)
    return 4;

  s2 = new Circle(1);
  if (s2->get_area() != 3)
    return 5;
  
  delete s1, s2;

  Polygon *p1;
  p1 = new Square(5);
  if (p1->get_area() != 25)
    return 6;
  if (p1->num_sides() != 4)
    return 7;

  delete p1;

  return 0;
}

