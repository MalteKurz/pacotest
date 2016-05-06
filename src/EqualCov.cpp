#include <pacotest_header.h>

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
    A(1,1) = 1; A(1,2) = -1;
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

void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert,int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  
  unsigned int n=Udata.n_rows;
  arma::umat indexVectors(n,2);
  arma::uvec nObsPerVector(2);
  indexVectors.zeros();
  nObsPerVector.zeros();
  
  Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  if (withEstUncert)
  {
    *TestStat = EqualCovChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, data, svcmDataFrame, cPitData);
  }
  else
  {
    *TestStat = EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector);
  }
  
  
  int nGroups = indexVectors.n_cols;
  double df = nGroups-1;
  
  *pValue = 1-Chi2CDF(*TestStat, df);
}


void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualCovTest(Udata, Wdata, GroupingMethod, withEstUncert, finalComparisonMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
}


void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
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
          S = EqualCovChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, data, svcmDataFrame, cPitData);
        }
        else
        {
          S = EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector);
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

void EqualCovTestWithPenalty(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert,int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double penaltyLevel, double penaltyPower, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  
  unsigned int n=Udata.n_rows;
  arma::umat indexVectors(n,2);
  arma::uvec nObsPerVector(2);
  indexVectors.zeros();
  nObsPerVector.zeros();
  
  
  Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, SplitVariable, SplitQuantile, SplitThreshold);
  
  if (withEstUncert)
  {
    *TestStat = EqualCovChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, data, svcmDataFrame, cPitData);
  }
  else
  {
    *TestStat = EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector);
  }
  
  int nGroups = indexVectors.n_cols;
  double df = nGroups-1;
  
  if (GroupingMethod<=2)
  {
    unsigned int lastCol = Wdata.n_cols -1;
    arma::uvec X = sort_index(Wdata.col(lastCol));
    
    arma::uvec J(5);
    
    J(0) = 0;
    J(1) = floor(n/4)-1;
    J(2) = floor(n/2)-1;
    J(3) = floor(3*n/4)-1;
    J(4) = n-1;
    
    arma::umat indexVectorsMedian(n,4);
    arma::uvec nObsPerVectorMedian(4);
    indexVectorsMedian.zeros();
    
    nObsPerVectorMedian(0) = J(1)-J(0)+1;
    nObsPerVectorMedian(1) = J(2)-J(1);
    nObsPerVectorMedian(2) = J(3)-J(2);
    nObsPerVectorMedian(3) = J(4)-J(3);
    
    indexVectorsMedian.submat(0,0,nObsPerVectorMedian(0)-1,0) = X.subvec(J(0),J(1));
    indexVectorsMedian.submat(0,1,nObsPerVectorMedian(1)-1,1) = X.subvec(J(1)+1,J(2));
    indexVectorsMedian.submat(0,2,nObsPerVectorMedian(2)-1,2) = X.subvec(J(2)+1,J(3));
    indexVectorsMedian.submat(0,3,nObsPerVectorMedian(3)-1,3) = X.subvec(J(3)+1,J(4));
    
//    arma::umat indexVectorsMedian(n,2);
//    arma::uvec nObsPerVectorMedian(2);
//    indexVectorsMedian.zeros();
//    
//    nObsPerVectorMedian(0) = J(2)-J(0)+1;
//    nObsPerVectorMedian(1) = J(4)-J(2);
//    
//    indexVectorsMedian.submat(0,0,nObsPerVectorMedian(0)-1,0) = X.subvec(J(0),J(2));
//    indexVectorsMedian.submat(0,1,nObsPerVectorMedian(1)-1,1) = X.subvec(J(2)+1,J(4));
    
    double n_double = (double) n;
    double testStatMedianWithPenalty;
    
    if (withEstUncert)
    {
      testStatMedianWithPenalty = EqualCovChi2WithEstimationTestStat(Udata, indexVectorsMedian, nObsPerVectorMedian, data, svcmDataFrame, cPitData) + n_double*penaltyLevel/(pow(n_double,penaltyPower));
    }
    else
    {
      testStatMedianWithPenalty = EqualCovChi2TestStat(Udata, indexVectorsMedian, nObsPerVectorMedian) + n_double*penaltyLevel/(pow(n_double,penaltyPower));
    }
    
//    arma::vec bla(4);
//    bla(0) = *TestStat;
//    bla(1) = testStatMedianWithPenalty;
//    bla(2) = n_double*penaltyLevel/(pow(n_double,penaltyPower));
    
    
    nGroups = indexVectorsMedian.n_cols;
    df = nGroups-1;
    *TestStat = max(*TestStat,testStatMedianWithPenalty) - n_double*penaltyLevel/(pow(n_double,penaltyPower));
    
//    bla(3) = *TestStat;
//    bla.print("Test statistics plus penalty:");
  }
  
  *pValue = 1-Chi2CDF(*TestStat, df);
}


void EqualCovTestWithPenalty(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double penaltyLevel, double penaltyPower, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualCovTestWithPenalty(Udata, Wdata, GroupingMethod, withEstUncert, finalComparisonMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, penaltyLevel, penaltyPower, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
}


void EqualCovChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta)
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
  
  EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector, testStat, sigma, theta);
  
}


double EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector)
{
  arma::mat sigma;
  arma::vec theta;
  double testStat;
  
  EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta);
  
  return testStat;
}


void EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta)
{
  int nGroups = nObsPerVector.n_elem;
  double nObs = Udata.n_rows;
  
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
  int iGroup;
  theta.set_size( nGroups+2*nGroups );
  
  // variance-covariance matrix
  sigma.set_size(nGroups,nGroups);
  sigma.zeros();
  
  arma::vec cPit1InGroup;
  arma::vec cPit2InGroup;
  
  arma::vec cPit1InGroupDemeaned;
  arma::vec cPit2InGroupDemeaned;
  
  //double rhoInGroup;
  double lambdaInGroup;
  
  double nObsInGroup;
  
  arma::mat aInGroup;
  arma::vec bInGroup;
  
  for (iGroup=0;iGroup<nGroups;iGroup++)
  {
    // Obtain the indices of the subsample
    arma::uvec indInGroup = indexVectors.submat(0,iGroup,nObsPerVector(iGroup)-1,iGroup);
    
    // Obtain the subsample
    cPit1InGroup = cPit1.elem(indInGroup);
    cPit2InGroup = cPit2.elem(indInGroup);
    
    // Mean and variance in the subsample
    theta(0 + 2*iGroup) = mean(cPit1InGroup); // Mu for the first CPIT
    
    theta(1 + 2*iGroup) = mean(cPit2InGroup); // Mu for the second CPIT
  
    // Obtain standardized CPITs
    cPit1InGroupDemeaned = (cPit1InGroup-theta(0 + 2*iGroup));
    cPit2InGroupDemeaned = (cPit2InGroup-theta(1 + 2*iGroup));
    
    nObsInGroup = cPit1InGroup.n_elem;
    
    // Place the correlation parameters in the parameter vector
    theta(2*nGroups+iGroup) = mean(cPit1InGroupDemeaned  % cPit2InGroupDemeaned); // Rho in the group
    
    lambdaInGroup = nObsInGroup/nObs;
    bInGroup.set_size(nObsInGroup);
    bInGroup = theta(2*nGroups+iGroup) - (cPit1InGroupDemeaned % cPit2InGroupDemeaned);
    
    sigma(iGroup,iGroup) = mean(square(bInGroup))/lambdaInGroup;
  }
  
  arma::mat rhos(1,nGroups);
  rhos = theta.subvec(2*nGroups, 3*nGroups-1);
  
  arma::mat A;
  getMatrixForPairwiseComparison(nGroups, A);
  
  *testStat = arma::as_scalar(nObs * (trans(A*rhos) * inv(A * sigma * trans(A)) * A*rhos));
  
  return;
  
}




void EqualCovChi2WithEstimationTestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
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
  
  EqualCovChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, testStat, sigma, theta, data, svcmDataFrame, cPitData);
  
}


double EqualCovChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  arma::mat sigma;
  arma::vec theta;
  double testStat;
  
  EqualCovChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta, data, svcmDataFrame, cPitData);
  
  return testStat;
}

void EqualCovChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  int nGroups = nObsPerVector.n_elem;
  
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
  int iGroup;
  theta.set_size( nGroups+2*nGroups );
  
  
  arma::vec cPit1InGroup;
  arma::vec cPit2InGroup;
  
  arma::vec cPit1InGroupDemeaned;
  arma::vec cPit2InGroupDemeaned;
  
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
    
    // Mean and variance in the subsample
    theta(0 + 2*iGroup) = mean(cPit1InGroup); // Mu for the first CPIT
    
    theta(1 + 2*iGroup) = mean(cPit2InGroup); // Mu for the second CPIT
  
    // Obtain standardized CPITs
    cPit1InGroupDemeaned = (cPit1InGroup-theta(0 + 2*iGroup));
    cPit2InGroupDemeaned = (cPit2InGroup-theta(1 + 2*iGroup));
    
    nObsInGroup = cPit1InGroup.n_elem;
    
    // Place the correlation parameters in the parameter vector
    theta(2*nGroups+iGroup) = mean(cPit1InGroupDemeaned  % cPit2InGroupDemeaned); // Rho in the group
    
  }
  
  covOfCovariancesWithEstimationFromCpp(data, svcmDataFrame, indexVectors, nObsPerVector, cPitData, theta, sigma);
  
  int nCol = sigma.n_cols;
  
  arma::mat sigmaRhos = sigma.submat(nCol-nGroups,nCol-nGroups,nCol-1,nCol-1);
  arma::mat rhos(1,nGroups);
  rhos = theta.subvec(2*nGroups, 3*nGroups-1);
  
  arma::mat A;
  getMatrixForPairwiseComparison(nGroups, A);
  
  *testStat = arma::as_scalar(nObs * (trans(A*rhos) * inv(A * sigmaRhos * trans(A)) * A*rhos));
  
  return;
  
}