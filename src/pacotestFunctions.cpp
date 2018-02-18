#include <pacotest_header.h>

// [[Rcpp::export]]
Rcpp::List VI(arma::mat Udata, arma::mat Wdata, double NumbBoot) {
  
  // initialize output variable
  Rcpp::List out;
  
  try
  {
    int N = (int) NumbBoot;
    
    // Associate outputs
    double testStat;
    double pValue;
    arma::mat S(N,1);
    
    VecIndepTest(Udata, Wdata, N, &testStat, &pValue, S);
    
    out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("testStat")=testStat,Rcpp::Named("S")=S);
  }
  catch( std::exception& __ex__ ) {
    forward_exception_to_r( __ex__ );
  }
  catch(...) {
    ::Rf_error( "c++ exception" );
  }
  
  return out;
  
}


// [[Rcpp::export]]
Rcpp::List EC(arma::mat Udata, arma::mat Wdata, double NumbBoot, double Grouping, double finalComparison, double ExpMinSampleSize = 50, double TrainingDataFraction = 0.5) {
  
  // initialize output variable
  Rcpp::List out;
  
  try
  {
    int N = (int) NumbBoot;
    int grouping = (int) Grouping;
    
    // Associate outputs
    double testStat;
    double pValue;
    arma::mat S(N,1);
    arma::uvec SplitVariable(4);
    arma::uvec SplitQuantile(4);
    arma::vec SplitThreshold(3);
    SplitVariable.zeros();
    SplitQuantile.zeros();
    SplitThreshold.zeros();
    
    
    EqualCopTest(Udata, Wdata, N, grouping, finalComparison, &testStat, &pValue, S, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
    
    out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("testStat")=testStat,Rcpp::Named("S")=S,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
  }
  catch( std::exception& __ex__ ) {
    forward_exception_to_r( __ex__ );
  }
  catch(...) {
    ::Rf_error( "c++ exception" );
  }
  
  return out;
  
}


// [[Rcpp::export]]
Rcpp::List ecorrOrEcov(double TestTypeNumber, arma::mat Udata, arma::mat Wdata, double doubleDimCondSet, double Grouping, double doubleWithEstUncert, double doubleEstUncertWithRanks, double finalComparison, arma::mat & data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, double AggPvalsNumbRep = NA_REAL, double ExpMinSampleSize = NA_REAL, double TrainingDataFraction = NA_REAL, double penaltyLevel = NA_REAL, double penaltyPower = NA_REAL, double Gamma0Partition = NA_REAL) {
  
  // initialize output variable
  Rcpp::List out;
  
  try
  {
    int testTypeNumber = (int) TestTypeNumber;
    int grouping = (int) Grouping;
    int intWithEstUncert = (int) doubleWithEstUncert;
    int intEstUncertWithRanks = (int) doubleEstUncertWithRanks;
    int dimCondSet = (int) doubleDimCondSet;
    int gamma0Partition;
    
    // Associate outputs
    double pValue;
    
    if ((!arma::is_finite(AggPvalsNumbRep) && !arma::is_finite(TrainingDataFraction)) || AggPvalsNumbRep == 0)
    {
      double testStat;
      arma::uvec SplitVariable(4);
      arma::uvec SplitQuantile(4);
      arma::vec SplitThreshold(3);
      SplitVariable.zeros();
      SplitQuantile.zeros();
      SplitThreshold.zeros();
      
      if (arma::is_finite(penaltyLevel) && arma::is_finite(penaltyPower) && arma::is_finite(Gamma0Partition))
      {
        gamma0Partition = (int) Gamma0Partition;
        if (testTypeNumber == 1)
        {
          EqualCovTestWithPenalty(Udata, Wdata, dimCondSet, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, &testStat, &pValue, ExpMinSampleSize, penaltyLevel, penaltyPower, gamma0Partition, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
        }
        else
        {
          EqualCorrTestWithPenalty(Udata, Wdata, dimCondSet, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, &testStat, &pValue, ExpMinSampleSize, penaltyLevel, penaltyPower, gamma0Partition, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
        }
        
      }
      else
      {
        if (testTypeNumber == 1)
        {
          EqualCovTest(Udata, Wdata, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, &testStat, &pValue, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
        }
        else
        {
          EqualCorrTest(Udata, Wdata, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, &testStat, &pValue, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
        }
        
      }
      
      out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("testStat")=testStat,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
    }
    else
    {
      arma::mat pValues(AggPvalsNumbRep,1);
      arma::umat SplitVariable(4,AggPvalsNumbRep);
      arma::umat SplitQuantile(4,AggPvalsNumbRep);
      arma::mat SplitThreshold(3,AggPvalsNumbRep);
      SplitVariable.zeros();
      SplitQuantile.zeros();
      SplitThreshold.zeros();
      
      if (testTypeNumber == 1)
      {
        EqualCovTest(Udata, Wdata, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, pValues, &pValue, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
      }
      else
      {
        EqualCorrTest(Udata, Wdata, grouping, intWithEstUncert, intEstUncertWithRanks, finalComparison, pValues, &pValue, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, data, svcmDataFrame, cPitData);
      }
      
      out = Rcpp::List::create(Rcpp::Named("pValue")=pValue,Rcpp::Named("pValues")=pValues,Rcpp::Named("SplitVariable")=SplitVariable,Rcpp::Named("SplitQuantile")=SplitQuantile,Rcpp::Named("SplitThreshold")=SplitThreshold);
    }
  }
  catch( std::exception& __ex__ ) {
    forward_exception_to_r( __ex__ );
  }
  catch(...) {
    ::Rf_error( "c++ exception" );
  }
  
  return out;
  
}


// [[Rcpp::export]]
void covOfCorrelationsWithEstimationFromCpp(arma::mat & data, Rcpp::DataFrame svcmDataFrame, arma::umat & indexVectors, arma::uvec & nObsPerVector, Rcpp::List cPitData, arma::vec & theta, arma::mat & sigma, int intEstUncertWithRanks)
{
  int nGroups = nObsPerVector.n_elem;
  
  Rcpp::List indListForR;
  
  arma::uvec indInFirstGroup = indexVectors.submat(0,0,nObsPerVector(0)-1,0);
  indInFirstGroup++;
  arma::uvec indInSecondGroup = indexVectors.submat(0,1,nObsPerVector(1)-1,1);
  indInSecondGroup++;
  
  if (nGroups == 2)
  {
    indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup);
  }
  else
  {
    if (nGroups > 2)
    {
      arma::uvec indInThirdGroup = indexVectors.submat(0,2,nObsPerVector(2)-1,2);
      indInThirdGroup++;
      if (nGroups == 3)
      {
        indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup,indInThirdGroup);
      }
      else
      {
        arma::uvec indInFourthGroup = indexVectors.submat(0,3,nObsPerVector(3)-1,3);
        indInFourthGroup++;
        indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup,indInThirdGroup,indInFourthGroup);
      }
    }
  }
  
  Rcpp::Environment pacotest = Rcpp::Environment::namespace_env("pacotest");
  Rcpp::Function covOfCorrelationsWithEstimation = pacotest["covOfCorrelationsWithEstimation"];
  
  SEXP outFromR = covOfCorrelationsWithEstimation(Rcpp::Named("data", data), Rcpp::Named("svcmDataFrame", svcmDataFrame), Rcpp::Named("indList", indListForR), Rcpp::Named("cPitData", cPitData), Rcpp::Named("theta", theta), Rcpp::Named("estUncertWithRanks", intEstUncertWithRanks));
  
  sigma = Rcpp::as<arma::mat>(outFromR);
  
  return;
  
}



// [[Rcpp::export]]
void covOfCovariancesWithEstimationFromCpp(arma::mat & data, Rcpp::DataFrame svcmDataFrame, arma::umat & indexVectors, arma::uvec & nObsPerVector, Rcpp::List cPitData, arma::vec & theta, arma::mat & sigma, int intEstUncertWithRanks)
{
  int nGroups = nObsPerVector.n_elem;
  
  Rcpp::List indListForR;
  
  arma::uvec indInFirstGroup = indexVectors.submat(0,0,nObsPerVector(0)-1,0);
  indInFirstGroup++;
  arma::uvec indInSecondGroup = indexVectors.submat(0,1,nObsPerVector(1)-1,1);
  indInSecondGroup++;
  
  if (nGroups == 2)
  {
    indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup);
  }
  else
  {
    if (nGroups > 2)
    {
      arma::uvec indInThirdGroup = indexVectors.submat(0,2,nObsPerVector(2)-1,2);
      indInThirdGroup++;
      if (nGroups == 3)
      {
        indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup,indInThirdGroup);
      }
      else
      {
        arma::uvec indInFourthGroup = indexVectors.submat(0,3,nObsPerVector(3)-1,3);
        indInFourthGroup++;
        indListForR = Rcpp::List::create(indInFirstGroup,indInSecondGroup,indInThirdGroup,indInFourthGroup);
      }
    }
  }
  
  Rcpp::Environment pacotest = Rcpp::Environment::namespace_env("pacotest");
  Rcpp::Function covOfCovariancesWithEstimation = pacotest["covOfCovariancesWithEstimation"];
  
  SEXP outFromR = covOfCovariancesWithEstimation(Rcpp::Named("data", data), Rcpp::Named("svcmDataFrame", svcmDataFrame), Rcpp::Named("indList", indListForR), Rcpp::Named("cPitData", cPitData), Rcpp::Named("theta", theta), Rcpp::Named("estUncertWithRanks", intEstUncertWithRanks));
  
  sigma = Rcpp::as<arma::mat>(outFromR);
  
  return;
  
}


// [[Rcpp::export]]
Rcpp::List testStatEqualCorrWithoutEstimationCpp(arma::umat indexVectors, arma::uvec nObsPerVector, arma::mat Udata)
{
  
  // initialize output variable
  Rcpp::List out;
  
  try
  {
    // Associate outputs
    double testStat;
    arma::mat sigma;
    arma::vec theta;
    
    EqualCorrChi2TestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta);
    
    out = Rcpp::List::create(Rcpp::Named("testStat")=testStat,Rcpp::Named("sigma")=sigma,Rcpp::Named("theta")=theta);
  }
  catch( std::exception& __ex__ ) {
    forward_exception_to_r( __ex__ );
  }
  catch(...) {
    ::Rf_error( "c++ exception" );
  }
  
  return out;
  
}

// [[Rcpp::export]]
Rcpp::List testStatEqualCorrWithEstimationCpp(arma::umat indexVectors, arma::uvec nObsPerVector, arma::mat Udata, arma::mat data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData)
{
  
  // initialize output variable
  Rcpp::List out;
  
  try
  {
    // Associate outputs
    double testStat;
    arma::mat sigma;
    arma::vec theta;
    
    int intEstUncertWithRanks = 0;
    
    EqualCorrChi2WithEstimationTestStat(Udata, indexVectors, nObsPerVector, &testStat, sigma, theta, data, svcmDataFrame, cPitData, intEstUncertWithRanks);
    
    out = Rcpp::List::create(Rcpp::Named("testStat")=testStat,Rcpp::Named("sigma")=sigma,Rcpp::Named("theta")=theta);
  }
  catch( std::exception& __ex__ ) {
    forward_exception_to_r( __ex__ );
  }
  catch(...) {
    ::Rf_error( "c++ exception" );
  }
  
  return out;
  
}

