#include "SAtest_header.h"
#include "armaMex.hpp"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    try
    {
        // Check the number of input arguments.
        if (nrhs != 3)
            mexErrMsgTxt("Incorrect number of input arguments.");
        
        // Associate inputs
        mat Udata = armaGetPr(prhs[0]);
        mat Wdata = armaGetPr(prhs[1]);
        double *NumbBoot = mxGetPr(prhs[2]);
        int N = (int) *NumbBoot;
        
        // Check type of input.
        if ( (mxGetClassID(prhs[0]) != mxDOUBLE_CLASS) || (mxGetClassID(prhs[1]) != mxDOUBLE_CLASS) || Udata.n_rows != Wdata.n_rows)
            mexErrMsgTxt("Input must me of type double.");
        
        // Check if input is real.
        if ( (mxIsComplex(prhs[0])) || (mxIsComplex(prhs[1])) )
            mexErrMsgTxt("Input must be real.");
        
        
        // Associate outputs
        double *TestStat;
        plhs[1] = mxCreateDoubleScalar(mxREAL);
        TestStat = mxGetPr(plhs[1]);
        double *pValue;
        plhs[0] = mxCreateDoubleScalar(mxREAL);
        pValue = mxGetPr(plhs[0]);
        arma::mat S(N,1);
        plhs[2] = armaCreateMxMatrix(N, 1);
        armaSetPr(plhs[2], S);
        
        VecIndepTest(Udata, Wdata, N, TestStat, pValue, S);
        
    }
    catch (std::exception& e)
    {
        std::string msg = std::string("SAtest Error: ") + e.what();
        mexErrMsgTxt(msg.c_str());
    }
    return;
}

