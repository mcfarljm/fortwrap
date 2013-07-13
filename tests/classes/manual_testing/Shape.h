#ifndef SHAPE_H_
#define SHAPE_H_

#include <cstdlib>
#include "InterfaceDefs.h"

// vtab entries:
extern int __shapes_MOD___vtab_shapes_Square;


//ADDRESS square_vtab = &__shapes_MOD___vtab_shapes_Square;
//ADDRESS __shapes_MOD___vtab_shapes_Circle;
//ADDRESS __shapes_MOD___vtab_shapes_Shape;

extern "C" {
  void __cppwrappers_MOD_allocate_square(ADDRESS *caddr);
  void __cppwrappers_MOD_deallocate_square(ADDRESS caddr);
  void __cppwrappers_MOD_allocate_circle(ADDRESS *caddr);
  void __cppwrappers_MOD_deallocate_circle(ADDRESS caddr);
  //  void __shapes_MOD_shape_of_rv(ADDRESS c, ADDRESS rv);
  int __polymorphicwrappers_MOD_shape_get_area(ADDRESS s);
  void __shapes_MOD_square_ctor(ADDRESS s, int* side);
  void __shapes_MOD_circle_ctor(ADDRESS s, int* radius);
  int __shapes_MOD_circle_diameter(ADDRESS s);
  void __shapes_MOD_circlet_of_rv(ADDRESS c, ADDRESS rv);
  void __shapes_MOD_circled_of_rv(ADDRESS c, ADDRESS rv);
  int __shapes_MOD_square_area(ADDRESS c);
  int __shapes_MOD_square_area_dt(ADDRESS c);
  int __shapes_MOD_get_num(ADDRESS c);
  void __shapes_MOD_what_am_i(ADDRESS c);
  int __shapes_MOD_num_sides(ADDRESS c);
}

class Shape {

 protected:
  // Shape can not be instantiated
  Shape() {}
  
 public:
  FClassContainer class_data;
  int get_num(void);
  void what_am_i(void);
  virtual int get_area(void) = 0;
};

class Polygon : public Shape {
 protected:
  Polygon() {}
 public:
  int num_sides(void);
};


class Square : public Polygon {
 public:
  Square(int side);
  int get_area(void);
  int get_area_dt(void);
};



#endif /* SHAPE_H_ */
