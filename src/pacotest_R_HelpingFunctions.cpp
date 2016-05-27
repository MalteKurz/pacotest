#include <pacotest_header.h>
#include <RcppArmadilloExtensions/sample.h>

double NormalCDF(double Z)
{
    return R::pnorm(Z,0,1,1,0);
}

double Chi2CDF(double Z, double df)
{
    return R::pchisq(Z,df,1,0);
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
 
 void RandPerm(arma::uvec &X)
 {
   int N = X.n_elem;
   X = arma::linspace<arma::uvec>(0,N-1,N);
   X = Rcpp::RcppArmadillo::sample(X,N,FALSE);
 }
