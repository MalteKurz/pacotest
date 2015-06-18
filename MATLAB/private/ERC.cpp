#include "SAtest_header.h"
#include "armaMex.hpp"


void mexFunction(int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[])
{
    try
    {
        // Check the number of input arguments.
        if (nrhs != 4)
            mexErrMsgTxt("Incorrect number of input arguments.");
        
        // Associate inputs
        mat Udata = armaGetPr(prhs[0]);
        mat Wdata = armaGetPr(prhs[1]);
        double *grouping = mxGetPr(prhs[2]);
        int Grouping = (int) *grouping;
        double *aggPvalsNumbRep = mxGetPr(prhs[3]);
        int AggPvalsNumbRep = (int) *aggPvalsNumbRep;
        
        // Check type of input.
        if ( (mxGetClassID(prhs[0]) != mxDOUBLE_CLASS) || (mxGetClassID(prhs[1]) != mxDOUBLE_CLASS) || Udata.n_rows != Wdata.n_rows || Udata.n_cols != 2 )
            mexErrMsgTxt(".");
        
        // Check if input is real.
        if ( (mxIsComplex(prhs[0])) || (mxIsComplex(prhs[1])) )
            mexErrMsgTxt(".");
        
        
        // Associate outputs
        double *pValue;
        plhs[0] = mxCreateDoubleScalar(mxREAL);
        pValue = mxGetPr(plhs[0]);
        
        if (AggPvalsNumbRep == 0)
        {
            double *TestStat;
            plhs[1] = mxCreateDoubleScalar(mxREAL);
            TestStat = mxGetPr(plhs[1]);
            
            if (nlhs > 2)
            {
                arma::mat Xdata(1,2);
                arma::mat Ydata(1,2);
                
                EqualRankCorrTest(Udata, Wdata, Grouping, TestStat, pValue, Xdata, Ydata);
                plhs[2] = armaCreateMxMatrix(Xdata.n_rows, 2);
                armaSetPr(plhs[2], Xdata);
                plhs[3] = armaCreateMxMatrix(Ydata.n_rows, 2);
                armaSetPr(plhs[3], Ydata);
            }
            else
            {
                EqualRankCorrTest(Udata, Wdata, Grouping, TestStat, pValue);
            }
        }
        else
        {
            arma::mat pValues(AggPvalsNumbRep,1);
            
            EqualRankCorrTest(Udata, Wdata, pValues, pValue, AggPvalsNumbRep);
            
            
            plhs[1] = armaCreateMxMatrix(AggPvalsNumbRep, 1);
            armaSetPr(plhs[1], pValues);
        }
        
    }
    catch (std::exception& e)
    {
        std::string msg = std::string("SAtest Error: ") + e.what();
        mexErrMsgTxt(msg.c_str());
    }
    return;
}
