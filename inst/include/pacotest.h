#include <pacotest_header.h>

double NormalCDF(double Z);
double Chi2CDF(double Z, double df);

void NormalRand(arma::mat &X);
void RandPerm(arma::uvec &X);

double EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector);
void EqualCovChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta);
void EqualCovChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta);

double EqualCovChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);
void EqualCovChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);
void EqualCovChi2WithEstimationTestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);

void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);
void EqualCovTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);

void EqualCovTestWithPenalty(const arma::mat &Udata, const arma::mat &Wdata, int dimCondSet, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double penaltyLevel, double penaltyPower, int gamma0Partition, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);

double EqualCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector);
void EqualCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta);
void EqualCorrChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta);

double EqualCorrChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);
void EqualCorrChi2WithEstimationTestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);
void EqualCorrChi2WithEstimationTestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData, int intEstUncertWithRanks);

void EqualCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);
void EqualCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);

void EqualCorrTestWithPenalty(const arma::mat &Udata, const arma::mat &Wdata, int dimCondSet, int GroupingMethod, int withEstUncert, int intEstUncertWithRanks, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double penaltyLevel, double penaltyPower, int gamma0Partition, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);

void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S);

double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &S, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, int withTraining);

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, int finalComparisonMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod, int finalComparisonMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);


void getMatrixForPairwiseComparison(int nGroups, arma::mat &A);

void covOfCorrelationsWithEstimationFromCpp(arma::mat &data, Rcpp::DataFrame svcmDataFrame, arma::umat &indexVectors, arma::uvec &nObsPerVector, Rcpp::List cPitData, arma::vec &theta, arma::mat &sigma, int intEstUncertWithRanks);

void covOfCovariancesWithEstimationFromCpp(arma::mat &data, Rcpp::DataFrame svcmDataFrame, arma::umat &indexVectors, arma::uvec &nObsPerVector, Rcpp::List cPitData, arma::vec &theta, arma::mat &sigma, int intEstUncertWithRanks);
