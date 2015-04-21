#include <iostream>
#include <cstdio>
//#include "Shape_manual.h"
#include "Shape.h"

using namespace std;

void test_constructor(void);

// int main(void) {
//   Square sq1(2);
//   cout << "sq1 area dt: " << sq1.get_area_dt() << endl;
//   cout << "sq1 area: " << sq1.get_area() << endl;
//   cout << "sq1 get_num: " << sq1.get_num() << endl;

//   cout << "Square vtab: " << &__shapes_MOD___vtab_shapes_Square << endl;

//   Shape *s;
//   s = new Square(4);
//   cout << "s area: " << s->get_area() << endl;

//   cout << "Calling what am i\n";
//   sq1.what_am_i();
//   s->what_am_i();

//   Polygon *p;
//   p = new Square(6);
//   cout << "p area: " << p->get_area() << endl;
//   cout << "p nsides: " << p->num_sides() << endl;

//   Circle c(3);
//   cout << "Circle area:" << c.get_area() << endl;
// }

int main(void) {
  test_constructor();
}

// Test how to call the Fortran constructor that returns a derived type
void test_constructor(void) {
  void* c_ptr = NULL;
  int i=10;
  __cppwrappers_MOD_allocate_circle(&c_ptr);
  cout << "c_ptr: " << (long) c_ptr << endl;
  __shapes_MOD_circle_ctor_f(c_ptr, &i);
  // cout << "Circ area: " << __shapes_MOD_circle_area_dt(c_ptr) << endl;
  // __cppwrappers_MOD_deallocate_circle(c_ptr);
  cout << "Leaving test_constructor" << endl;
  // Now, why does the program crash at end?  Seems to have
  // successuflly constructed the derived type...
}


// extern "C" {
//   void inspect_type_(void* handle);
//   void c_test_func_(void* handle);
// }


// Automatic interface defs
// int main(void) 
// {
//   Circle c1(1);
//   Circle c2(5); // Pointer data is overwriting old data...
//   cout << "Area1: " << c1.get_area() << endl;
//   cout << "Area2: " << c2.get_area() << endl;
//   cout << "Diameter1: " << c1.get_diameter() << endl;

//   Square s1(2);
//   cout << "Square area1: " << s1.get_area() << endl;
//   cout << "Circle area2: " << c2.get_area() << endl;

// }



// Manual interface definitions

// int main(void) 
// {
//   Circlem c(1000);

//   //c.test();
//   cout << "CArea: " << c.carea() << endl;
//   cout << "Area: " << c.area() << endl;
  
// }


// int main(void) 
// {

//   void *handle;
//   int r=13;
//   get_circle_c(&handle);
//   printf("C address: %d   %p\n", handle, handle);
//   inspect_type_(handle);
//   printf("\n\n");
//   //__shapes_MOD_circle_ctor(handle, &r);
//   //__shapes_MOD_circle_area(handle);
//   c_test_func_(&handle);
  
// }

