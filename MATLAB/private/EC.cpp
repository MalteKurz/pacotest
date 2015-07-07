#include "SAtest_header.h"
#include "armaMex.hpp"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    try
    {
        // Check the number of input arguments.
        if (nrhs != 6)
            mexErrMsgTxt("Incorrect number of input arguments.");
        
        // Associate inputs
        mat Udata = armaGetPr(prhs[0]);
        mat Wdata = armaGetPr(prhs[1]);
        double *NumbBoot = mxGetPr(prhs[2]);
        int N = (int) *NumbBoot;
        double *grouping = mxGetPr(prhs[3]);
        int Group = (int) *grouping;
        double ExpMinSampleSize;
        double TrainingDataFraction;
        if (!mxIsEmpty(prhs[4]))
        {
            double *expMinSampleSize = mxGetPr(prhs[4]);
            ExpMinSampleSize = *expMinSampleSize;
        }
        else
        {
            ExpMinSampleSize = 50;
        }
        if (!mxIsEmpty(prhs[5]))
        {
            double *trainingDataFraction = mxGetPr(prhs[5]);
            TrainingDataFraction = *trainingDataFraction;
        }
        else
        {
            TrainingDataFraction = 0.5;
        }
        
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
        
        if (nlhs > 3)
        {
            arma::mat Xdata(1,2);
            arma::mat Ydata(1,2);
            
            EqualCopTest(Udata, Wdata, N, Group, TestStat, pValue, S, Xdata, Ydata, ExpMinSampleSize,TrainingDataFraction);
            plhs[3] = armaCreateMxMatrix(Xdata.n_rows, 2);
            armaSetPr(plhs[3], Xdata);
            plhs[4] = armaCreateMxMatrix(Ydata.n_rows, 2);
            armaSetPr(plhs[4], Ydata);
        }
        else
        {
            EqualCopTest(Udata, Wdata, N, Group, TestStat, pValue, S, ExpMinSampleSize,TrainingDataFraction);
        }
        
    }
    catch (std::exception& e)
    {
        std::string msg = std::string("SAtest Error: ") + e.what();
        mexErrMsgTxt(msg.c_str());
    }
    return;
}

