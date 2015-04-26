#include <stdio.h>

void __cppwrappers_MOD_allocate_circle(void*);
void __cppwrappers_MOD_deallocate_circle(void*);

/* Noticed from examing the assembly code that this function differs in that the "ret" at the end has a size argument.  From wikipedia: "Functions which utilize these conventions are easy to recognize in ASM code because they will unwind the stack prior to returning. The x86 ret instruction allows an optional 16-bit parameter that specifies the number of stack bytes to unwind before returning to the caller. Such code looks like this:

 ret 12

" */
__attribute__((stdcall)) void __shapes_MOD_circle_ctor_f(void*, int *);
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
