#include "Shape.h"
#include <iostream>

using namespace std;

Square::Square(int side) {
  __cppwrappers_MOD_allocate_square(&class_data.data);
  // Get pointer to vtab
  class_data.vptr = &__shapes_MOD___vtab_shapes_Square;
  __shapes_MOD_square_ctor(class_data.data, &side);
}

int Shape::get_num(void) {
  return __shapes_MOD_get_num(&class_data);
}

void Shape::what_am_i(void) {
  __shapes_MOD_what_am_i(&class_data);
}

int Square::get_area(void) {
  // Call to class method must pass address of pointer, since it
  // expects a class container
  //cout << "In Square::get_area\n";
  return __shapes_MOD_square_area(&class_data);
}

int Square::get_area_dt(void) {
  return __shapes_MOD_square_area_dt(class_data.data);
}

int Polygon::num_sides(void) {
  return __shapes_MOD_num_sides(&class_data);
}
