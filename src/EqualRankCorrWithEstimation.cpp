#include <pacotest_header.h>

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert,int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  
  unsigned int n=Udata.n_rows;
  arma::umat indexVectors(n,2);
  arma::uvec nObsPerVector(2);
  indexVectors.zeros();
  nObsPerVector.zeros();
  
  Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  if (withEstUncert)
  {
    *TestStat = EqualRankCorrChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, data, svcmDataFrame, cPitData);
  }
  else
  {
    *TestStat = EqualRankCorrChi2TestStat(Udata, indexVectors, nObsPerVector);
  }
  
  int nGroups = indexVectors.n_cols;
  double df = nGroups-1;
  
  *pValue = 1-Chi2CDF(*TestStat, df);
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, withEstUncert, finalComparisonMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
    arma::mat Xdata;
    arma::mat Ydata;
    double S;
    int i;
    
    arma::uvec splitVariable(4);
    arma::uvec splitQuantile(4);
    arma::vec splitThreshold(3);
    
    splitVariable.zeros();
    splitQuantile.zeros();
    splitThreshold.zeros();
    
    unsigned int n=Udata.n_rows;
    arma::umat indexVectors(n,2);
    arma::uvec nObsPerVector(2);
    indexVectors.zeros();
    nObsPerVector.zeros();
    int nGroups;
    double df;
    
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, splitVariable, splitQuantile, splitThreshold);
        
        SplitVariable.col(i) = splitVariable;
        SplitQuantile.col(i) = splitQuantile;
        SplitThreshold.col(i) = splitThreshold;
        
        if (withEstUncert)
        {
          S = EqualRankCorrChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, data, svcmDataFrame, cPitData);
        }
        else
        {
          S = EqualRankCorrChi2TestStat(Udata, indexVectors, nObsPerVector);
        }
        
        nGroups = indexVectors.n_cols;
        df = nGroups-1;
        pValues(i,0) = 1-Chi2CDF(S, df);
        
        
        indexVectors.set_size(n,2);
        nObsPerVector.set_size(2);
        indexVectors.zeros();
        nObsPerVector.zeros();
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}

void getMatrixForPairwiseComparison(int nGroups, arma::mat &A)
{
  
  switch ( nGroups )
  {
    
    case 2 : 
    A.set_size(1,2);
    
    A(0,0) = 1; A(0,1) = -1;
    break;
    
    case 3 : 
    A.set_size(2,3);
    A.zeros();
    
    A(0,0) = 1; A(0,1) = -1;
    A(1,0) = 1; A(1,2) = -1;
    break;
    
    default : 
    A.set_size(3,4);
    A.zeros();
    
    A(0,0) = 1; A(0,1) = -1;
    A(1,1) = 1; A(1,2) = -1;
    A(2,2) = 1; A(2,3) = -1;
    
  }
  return;
}

void EqualRankCorrChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta)
{
  double nObs = ind.n_rows;
  int nGroups = ind.n_cols;
  
  arma::uvec indInGroup;
  arma::umat indexVectors(nObs,nGroups);
  arma::uvec nObsPerVector(nGroups);
  
  indInGroup = arma::find(ind.col(0));
  nObsPerVector(0) = indInGroup.n_elem;
  indexVectors.submat(0,0,nObsPerVector(0)-1,0) = indInGroup;
  
  indInGroup = arma::find(ind.col(1));
  nObsPerVector(1) = indInGroup.n_elem;
  indexVectors.submat(0,1,nObsPerVector(1)-1,1) = indInGroup;
  
  if (nGroups > 2)
  {
    indInGroup = arma::find(ind.col(2));
    nObsPerVector(2) = indInGroup.n_elem;
    indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indInGroup;
  }
  if (nGroups == 4)
  {
    indInGroup = arma::find(ind.col(3));
    nObsPerVector(3) = indInGroup.n_elem;
    indexVectors.submat(0,3,nObsPerVector(3)-1,3) = indInGroup;
  }
  // Maybe (depending on whether the function is callable from outside) add exception for nGroups>4
  
  EqualRankCorrChi2TestStat(Udata, indexVectors, nObsPerVector, testStat, sigma, theta);
  
}


double EqualRankCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector)
{
  arma::mat sigma;
  arma::vec theta;
  double testStat;
  
  EqualRankCorrChi2TestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta);
  
  return testStat;
}

void EqualRankCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta)
{
  int nGroups = nObsPerVector.n_elem;
  
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
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
  sigma(0,0) = theta(1);
  sigma(2,2) = theta(3);
  
  // Variance of the variance estimators
  sigma(1,1) = mean(pow(cPit1-theta(0),4) - pow(theta(1),2));
  sigma(3,3) = mean(pow(cPit2-theta(2),4) - pow(theta(3),2));
  
  
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
    // Obtain the indices of the subsample
    arma::uvec indInGroup = indexVectors.submat(0,iGroup,nObsPerVector(iGroup)-1,iGroup);
    
    // Obtain the subsample
    cPit1InGroup = cPit1.elem(indInGroup);
    cPit2InGroup = cPit2.elem(indInGroup);
    
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
  
  //arma::mat bla = diagvec(sigma);
  //bla.print("Variances");
  //rhos.print("Rhos:");
  
  arma::mat A;
  getMatrixForPairwiseComparison(nGroups, A);
  
  *testStat = arma::as_scalar(nObs * (trans(A*rhos) * inv(A * sigmaRhos * trans(A)) * A*rhos));
  
  return;
  
}


void EqualRankCorrChi2WithEstimationTestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  double nObs = ind.n_rows;
  int nGroups = ind.n_cols;
  
  arma::uvec indInGroup;
  arma::umat indexVectors(nObs,nGroups);
  arma::uvec nObsPerVector(nGroups);
  
  indInGroup = arma::find(ind.col(0));
  nObsPerVector(0) = indInGroup.n_elem;
  indexVectors.submat(0,0,nObsPerVector(0)-1,0) = indInGroup;
  
  indInGroup = arma::find(ind.col(1));
  nObsPerVector(1) = indInGroup.n_elem;
  indexVectors.submat(0,1,nObsPerVector(1)-1,1) = indInGroup;
  
  if (nGroups > 2)
  {
    indInGroup = arma::find(ind.col(2));
    nObsPerVector(2) = indInGroup.n_elem;
    indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indInGroup;
  }
  if (nGroups == 4)
  {
    indInGroup = arma::find(ind.col(3));
    nObsPerVector(3) = indInGroup.n_elem;
    indexVectors.submat(0,3,nObsPerVector(3)-1,3) = indInGroup;
  }
  // Maybe (depending on whether the function is callable from outside) add exception for nGroups>4
  
  EqualRankCorrChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, testStat, sigma, theta, data, svcmDataFrame, cPitData);
  
}


double EqualRankCorrChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  arma::mat sigma;
  arma::vec theta;
  double testStat;
  
  EqualRankCorrChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta, data, svcmDataFrame, cPitData);
  
  return testStat;
}


void EqualRankCorrChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  int nGroups = nObsPerVector.n_elem;
  
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
  int iGroup;
  theta.set_size( nGroups+4 );
  
  theta(0) = mean(cPit1); // Mu for the first CPIT
  theta(1) = mean(square(cPit1-theta(0))); // Variance for the first CPIT
  
  theta(2) = mean(cPit2); // Mu for the second CPIT
  theta(3) = mean(square(cPit2-theta(2))); // Variance for the second CPIT
  
  
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
    // Obtain the indices of the subsample
    arma::uvec indInGroup = indexVectors.submat(0,iGroup,nObsPerVector(iGroup)-1,iGroup);
    
    // Obtain the subsample
    cPit1InGroup = cPit1.elem(indInGroup);
    cPit2InGroup = cPit2.elem(indInGroup);
    
    // Obtain standardized CPITs
    cPit1InGroupStandardized = (cPit1InGroup-theta(0))/sqrt(theta(1));
    cPit2InGroupStandardized = (cPit2InGroup-theta(2))/sqrt(theta(3));
    
    nObsInGroup = cPit1InGroup.n_elem;
    
    // Place the correlation parameters in the parameter vector
    theta(4+iGroup) = mean((cPit1InGroup-theta(0)) % (cPit2InGroup-theta(2))/sqrt(theta(1)*theta(3))); // Rho in the group
    
  }
  
  covOfCorrelationsWithEstimationFromCpp(data, svcmDataFrame, indexVectors, nObsPerVector, cPitData, theta, sigma);
  
  int nCol = sigma.n_cols;
  
  arma::mat sigmaRhos = sigma.submat(nCol-nGroups,nCol-nGroups,nCol-1,nCol-1);
  arma::mat rhos(1,nGroups);
  rhos = theta.subvec(4,3+nGroups);
  
  //arma::mat bla = diagvec(sigma);
  //bla.print("Variances");
  //rhos.print("Rhos:");
  
  arma::mat A;
  getMatrixForPairwiseComparison(nGroups, A);
  
  *testStat = arma::as_scalar(nObs * (trans(A*rhos) * inv(A * sigmaRhos * trans(A)) * A*rhos));
  
  return;
  
}

