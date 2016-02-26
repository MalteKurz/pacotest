#include <pacotest_header.h>

void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod,int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  
  unsigned int n=Udata.n_rows;
  arma::umat indexVectors(n,2);
  arma::uvec nObsPerVector(2);
  indexVectors.zeros();
  nObsPerVector.zeros();
  
  Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  *TestStat = EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector);
  
  int nGroups = indexVectors.n_cols;
  double df = nGroups-1;
  
  *pValue = 1-Chi2CDF(*TestStat, df);
}


void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualCovTest(Udata, Wdata, GroupingMethod, finalComparisonMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
}


void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
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
        
        S = EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector);
        
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


void EqualCovChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat)
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
  
  EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector, testStat);
  
}


double EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector)
{
  double testStat;
  
  EqualCovChi2TestStat(Udata, indexVectors, nObsPerVector, &testStat);
  
  return testStat;
}


void EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat)
{
  int nGroups = nObsPerVector.n_elem;
  double nObs = Udata.n_rows;
  
  arma::vec cPit1 = Udata.col(0);
  arma::vec cPit2 = Udata.col(1);
  
  int iGroup;
  arma::vec mus(2*nGroups);
  arma::vec covs(nGroups);
  
  // variance-covariance matrix
  arma::vec sigmas(nGroups);
  sigmas.zeros();
  
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
    
    // Mean in the subsample
    mus(0 + 2*iGroup) = mean(cPit1InGroup); // Mu for the first CPIT
    
    mus(1 + 2*iGroup) = mean(cPit2InGroup); // Mu for the second CPIT
  
    // Obtain standardized CPITs
    cPit1InGroupDemeaned = (cPit1InGroup-mus(0 + 2*iGroup));
    cPit2InGroupDemeaned = (cPit2InGroup-mus(1 + 2*iGroup));
    
    nObsInGroup = cPit1InGroup.n_elem;
    
    // covariance in the group
    covs(iGroup) = mean(cPit1InGroupDemeaned  % cPit2InGroupDemeaned);
    
    lambdaInGroup = nObsInGroup/nObs;
    bInGroup.set_size(nObsInGroup);
    bInGroup = covs(iGroup) - (cPit1InGroupDemeaned % cPit2InGroupDemeaned);
    
    sigmas(iGroup) = mean(square(bInGroup))/lambdaInGroup;
  }
  
  
  arma::mat sigmaOfDifferences(nGroups-1,nGroups-1);
  arma::mat covDifferences(1,nGroups-1);
  
  sigmaOfDifferences.zeros();
  for (iGroup=0;iGroup<(nGroups-1);iGroup++)
  {
    covDifferences(0,iGroup) = covs(iGroup) - covs(iGroup+1);
    sigmaOfDifferences(iGroup,iGroup) = sigmas(iGroup) + sigmas(iGroup+1);
  }
  for (iGroup=0;iGroup<(nGroups-2);iGroup++)
  {
    sigmaOfDifferences(iGroup,iGroup+1) = -sigmas(iGroup+1);
    sigmaOfDifferences(iGroup+1,iGroup) = -sigmas(iGroup+1);
  }
  
  *testStat = arma::as_scalar(nObs * (covDifferences* inv(sigmaOfDifferences) * trans(covDifferences)));
  
  return;
  
}

