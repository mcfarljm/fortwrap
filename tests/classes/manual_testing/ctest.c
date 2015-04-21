#include <stdio.h>

void __cppwrappers_MOD_allocate_circle(void*);
void __cppwrappers_MOD_deallocate_circle(void*);

void __shapes_MOD_circle_ctor_f(void*, int *);
int __shapes_MOD_circle_area_dt(void*);


int main(void) {
  /* Note behavior changes based on order of c and radius definitions */
  void *c;
  int radius = 10;
  int area;
  __cppwrappers_MOD_allocate_circle(&c);
  printf("alocd addr: %p\n", c);
  /* This call appears to be overwriteing the memory location for c */
  __shapes_MOD_circle_ctor_f(c, &radius);
  printf("addr after ctor: %p, %d\n", c, (int)c);
  
  area = __shapes_MOD_circle_area_dt(c);
  printf("Area: %d\n", area);
  __cppwrappers_MOD_deallocate_circle(c);
}
