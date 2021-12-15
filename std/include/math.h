#pragma once

double sin(double);
double cos(double);
double tan(double);
double sinh(double);
double cosh(double);
double tanh(double);
double asin(double);
double acos(double);
double atan(double);
double atan2(double, double);
double exp(double);
double log(double);
double log10(double);
double pow(double, double);
double sqrt(double);
double ceil(double);
double floor(double);
double fabs(double);
double ldexp(double, int);
double frexp(double, int*);
double modf(double, double*);
double fmod(double, double); 

/* round away from zero, regardless of fpu control word settings */
extern double round (double);
extern float roundf (float);

extern double trunc (double);
extern float truncf (float);

extern double fmax  (double, double);
extern double fmin (double, double);
extern float fmaxf (float, float);
float fminf (float, float);

extern double fma (double, double, double);
extern float fmaf (float, float, float);

extern double log2 (double _x);
extern float log2f (float _x);

double copysign (double, double);
float copysignf (float, float);
double logb (double);
float logbf (float);
double nextafter (double, double);
float nextafterf (float, float);
double scalb (double, long);
float scalbf (float, long);

// these functions are like the normal x > y variants and such
// except they don't throw an invalid FP exception if x and y are
// unordered.
#define isgreater(x, y)      ((x) >  (y))
#define isgreaterequal(x, y) ((x) >= (y))
#define isless(x, y)         ((x) <  (y))
#define islessequal(x, y)    ((x) <= (y))
#define islessgreater(x, y)  ((x) <  (y) || (x) > (y))
#define isunordered(x, y)    ((x) >= (y))
