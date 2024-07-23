#include <math.h>

// compile with: gcc iterate.c -lm -shared -fpic -o libiterate.so

typedef struct _complex_num
{
    double r, i;
} complex_num;

complex_num quad_c = {0.0, 0.0};

void set_quad_c(complex_num c)
{
    quad_c = c;
}

double mag(complex_num *x)
{
    return sqrt((x->r * x->r) + (x->i * x->i));
}

// square the complex number x and add c
complex_num quad_func(complex_num *x)
{
    double r, i;

    r = ((x->r * x->r) - (x->i * x->i)) + quad_c.r;
    i = ((x->r * x->i) + (x->i * x->r)) + quad_c.i;
    
    return (complex_num){ .r=r, .i=i };
}
