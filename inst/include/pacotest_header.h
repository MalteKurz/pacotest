#ifndef _PACOTEST_HEADER_H_
#define _PACOTEST_HEADER_H_


inline double max(const double x, const double y)
{
    return (x > y)? x : y;
}

inline int max(const int x, const int y)
{
    return (x > y)? x : y;
}

inline double max(const int x, const double y)
{
    return (((double) x) > y)? ((double) x) : y;
}

inline double min(double x, double y)
{
    return (x > y)? y : x;
}

inline int IndTria(int i, int j, int n)
{
    return (j > i)? (i*n-(i-1)*i/2+j-i) : (j*n-(j-1)*j/2+i-j);
}


// headers
#include <RcppArmadillo.h>

#define ARMA_DEFAULT_OSTREAM Rcpp::Rcout
#include <cmath>


// User written headers
#include "pacotest.h"

#endif

