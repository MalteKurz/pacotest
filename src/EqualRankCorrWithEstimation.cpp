#include <pacotest_header.h>

double EqualRankCorrTestStat(arma::uvec &indXdata, arma::uvec &indYdata, const arma::mat &Udata, arma::mat data, Rcpp::DataFrame svcmDataFrame)
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
    
    *TestStat = EqualRankCorrTestStat(indXdata, indYdata, Udata, data, svcmDataFrame);
    
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
        
        S = EqualRankCorrTestStat(indXdata, indYdata, Udata, data, svcmDataFrame);
        
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}



void EqualRankCorrChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta)
{
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
  int nGroups = 2;
  int iGroup;
  theta.set_size( nGroups+4 );
  
  theta(0) = mean(cPit1); // Mu for the first CPIT
  theta(1) = mean(square(cPit1-theta(0))); // Variance for the first CPIT
  
  theta(2) = mean(cPit2); // Mu for the second CPIT
  theta(3) = mean(square(cPit2-theta(2))); // Variance for the second CPIT
  
  // Obtain the variance-covariance matrix without estimation uncertainty
  sigma.set_size(nGroups+4,nGroups+4);
  sigma.zeros();
  
  // Variance of the mean estimators
  sigma(0,0) = theta(0);
  sigma(2,2) = theta(3);
  
  // Variance of the variance estimators
  sigma(1,1) = mean(pow(cPit1-theta(1),4) - pow(theta(1),2));
  sigma(3,3) = mean(pow(cPit2-theta(3),4) - pow(theta(3),2));
  
  
  arma::vec cPit1InGroup;
  arma::vec cPit2InGroup;
  
  arma::vec cPit1InGroupStandardized;
  arma::vec cPit2InGroupStandardized;
  
  //double rhoInGroup;
  double lambdaInGroup;
  
  double nObs = Udata.n_rows;
  double nObsInGroup;
  
  for (iGroup=0;iGroup<nGroups;iGroup++)
  {
    // Obtain the subsample
    cPit1InGroup = cPit1.elem(arma::find(ind.col(iGroup)));
    cPit2InGroup = cPit2.elem(arma::find(ind.col(iGroup)));
    
    // Obtain standardized CPITs
    cPit1InGroupStandardized = (cPit1InGroup-theta(0))/sqrt(theta(1));
    cPit2InGroupStandardized = (cPit2InGroup-theta(2))/sqrt(theta(3));
    
    nObsInGroup = cPit1InGroup.n_elem;
    
    // Place the correlation parameters in the parameter vector
    theta(4+iGroup) = mean((cPit1InGroup-theta(0)) % (cPit2InGroup-theta(2))/sqrt(theta(1)*theta(3))); // Rho in the group
    
    lambdaInGroup = nObsInGroup/nObs;
    
    sigma(4+iGroup,4+iGroup) = 1/lambdaInGroup*var(theta(4+iGroup)/2*(square(cPit2InGroupStandardized)+square(cPit1InGroupStandardized))-cPit2InGroupStandardized % cPit1InGroupStandardized);
    
  }
  
  int nCol = sigma.n_cols;
  
  arma::mat sigmaRhos = sigma.submat(nCol-nGroups,nCol-nGroups,nCol-1,nCol-1);
  arma::mat rhos(1,nGroups);
  rhos = theta.subvec(4,3+nGroups);
  
  
  //A = getMatrixForPairwiseComparison(nGroups)
  arma::mat A(1,2);
  
  A(0,0) = 1;
  A(0,1) = -1;
  
  *testStat = arma::as_scalar(sqrt(nObs) * (trans(A*rhos) * inv(A * sigmaRhos * trans(A)) * A*rhos));
  
}









