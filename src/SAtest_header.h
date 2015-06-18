#ifndef _SATEST_HEADER_H_
#define _SATEST_HEADER_H_

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
//#include "armaMex.hpp"
#include <iostream>
#include <cmath>
#include <vector>
#include <armadillo>

// User written headers
#include "SAtest.h"


#endif


#ifndef _NORMALCDF_
#define _NORMALCDF_

#include <Rmath.h>

inline double NormalCDF(double Z)
{
    return pnorm(Z,0,1,1,0);
}
#endif


#ifndef _NORMALCDF_
#define _NORMALCDF_

#include <boost/math/distributions/normal.hpp>

inline double NormalCDF(double Z)
{
    boost::math::normal dist(0,1);
    return boost::math::cdf(dist, Z);
}
#endif


#ifndef _NORMALCDF_
#define _NORMALCDF_

#include <mex.h>

inline double NormalCDF(double Z)
{
    mxArray *x, *y;
    double *X, *Y;
    x = mxCreateDoubleScalar(mxREAL);
    y = mxCreateDoubleScalar(mxREAL);
    X = mxGetPr(x);
    X[0] = Z;
    mexCallMATLAB(1,&y,1, &x, "normcdf");
    Y = mxGetPr(y);
    return Y[0];
}
#endif


#ifndef _NORMALCDF_
#define _NORMALCDF_

#include <stdio.h>
#include <gsl/gsl_cdf.h>

inline double NormalCDF(double Z)
{
    return gsl_cdf_ugaussian_P(Z);
}
#endif



