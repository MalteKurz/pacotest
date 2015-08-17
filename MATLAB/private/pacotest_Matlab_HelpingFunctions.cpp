#include "pacotest_header.h"
#include <mex.h>

double NormalCDF(double Z)
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
    mxDestroyArray(y);
    mxDestroyArray(x);
}


void NormalRand(arma::mat &X)
{
  mxArray *size, *Out;
  double *PtrSize, *PtrOut;
  size = mxCreateDoubleMatrix(1,2,mxREAL);
  PtrSize = mxGetPr(size);
  PtrSize[0] = (double) X.n_rows;
  PtrSize[1] = (double) X.n_cols;
  Out = mxCreateDoubleMatrix(PtrSize[0],PtrSize[1],mxREAL);
  
  mexCallMATLAB( 1, &Out, 1, &size, "randn" );
  
  PtrOut = mxGetPr(Out);
  
  int i,j;
  int N = X.n_rows;
  int M = X.n_cols;
  
  for (i=0;i<N;i++)
  {
    for (j=0;j<M;j++)
    {
      X(i,j) = PtrOut[i+(j-1)*N];
    }
  }
  
  mxDestroyArray(size);
  mxDestroyArray(Out);
}

void RandPerm(arma::uvec &X)
{
  mxArray *size, *Out;
  double *PtrSize, *PtrOut;
  size = mxCreateDoubleMatrix(1,1,mxREAL);
  PtrSize = mxGetPr(size);
  PtrSize[0] = (double) X.n_elem;
  Out = mxCreateDoubleMatrix(PtrSize[0],1,mxREAL);
  
  mexCallMATLAB( 1, &Out, 1, &size, "randperm" );
  
  PtrOut = mxGetPr(Out);
  
  int i;
  int N = X.n_elem;
  
  for (i=0;i<N;i++)
  {
      X(i) = PtrOut[i]-1;
  }
  
  mxDestroyArray(size);
  mxDestroyArray(Out);
}

