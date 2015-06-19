#include "SAtest_header.h"
#include <Rcpp.h>

double NormalCDF(double Z)
{
    return R::pnorm(Z,0,1,1,0);
}


void NormalRand(arma::mat &X)
{
  int i,j;
  int N = X.n_rows;
  int M = X.n_cols;
  
  for (i=0;i<N;i++)
  {
    for (j=0;j<M;j++)
    {
      X(i,j) = R::rnorm(0,1);
    }
  }
}
    