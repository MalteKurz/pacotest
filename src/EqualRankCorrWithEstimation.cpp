#include <pacotest_header.h>

double EqualRankCorrTestStat(arma::uvec &indXdata, arma::uvec &indYdata, arma::mat data, Rcpp::DataFrame svcmDataFrame)
{
  arma::umat ind(data.n_rows,2);
  ind.zeros();
  
  arma::uvec Cols(1);
  
  arma::umat matrixWithOnes(data.n_rows,2);
  matrixWithOnes.ones();
  
  Cols[0] = 0;
  ind.submat(indXdata,Cols) = matrixWithOnes.submat(indXdata,Cols);
  
  Cols[0] = 1;
  ind.submat(indYdata,Cols) = matrixWithOnes.submat(indYdata,Cols);
  
  double testStat;
  
  testStatEqualCorrWithEstimationFromCpp(data, svcmDataFrame, ind, testStat);
  
  return testStat;
  
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame)
{
  
    arma::uvec Cols(2);
    
    Cols(0) =0;
    Cols(1) =1;
    
    arma::uvec indXdata;
    arma::uvec indYdata;
    
    Grouping(Udata, Wdata, indXdata, indYdata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame);
    
    Xdata = Udata.submat(indXdata, Cols);
    Ydata = Udata.submat(indYdata, Cols);
    
    *TestStat = EqualRankCorrTestStat(indXdata, indYdata, data, svcmDataFrame);
    
    *pValue = 2*(1-NormalCDF(std::abs(*TestStat)));
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame);
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame)
{
    arma::uvec indXdata;
    arma::uvec indYdata;
    double S;
    int i;
    
    arma::umat splitVariable(1,4);
    arma::umat splitQuantile(1,4);
    arma::mat splitThreshold(1,3);
    
    splitVariable.zeros();
    splitQuantile.zeros();
    splitThreshold.zeros();
    
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, indXdata, indYdata, 1, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame);
        
        SplitVariable.row(i) = splitVariable;
        SplitQuantile.row(i) = splitQuantile;
        SplitThreshold.row(i) = splitThreshold;
        
        S = EqualRankCorrTestStat(indXdata, indYdata, data, svcmDataFrame);
        
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}
