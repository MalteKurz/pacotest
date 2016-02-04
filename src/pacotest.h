
double NormalCDF(double Z);
double Chi2CDF(double Z, double df);

void NormalRand(arma::mat &X);
void RandPerm(arma::uvec &X);

double EqualRankCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector);
void EqualRankCorrChi2TestStat(const arma::mat &Udata, arma::umat &indexVectors, arma::uvec &nObsPerVector, double *testStat, arma::mat &sigma, arma::vec &theta);
void EqualRankCorrChi2TestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta);

void EqualRankCorrChi2WithEstimationTestStat(arma::umat &ind, const arma::mat &Udata, double *testStat, arma::mat &sigma, arma::vec &theta, arma::mat &data, Rcpp::DataFrame svcmDataFrame, Rcpp::List cPitData);

//double testStatEqualCorrWithEstimationFromCpp(arma::mat data, Rcpp::DataFrame svcmDataFrame, arma::umat ind);
SEXP testStatEqualCorrWithEstimationFromCpp(arma::mat &data, Rcpp::DataFrame svcmDataFrame, arma::umat &ind, double out);
SEXP covOfCorrelationsWithEstimationFromCpp(arma::mat &data, Rcpp::DataFrame svcmDataFrame, arma::umat &ind, Rcpp::List cPitData, arma::vec &theta, arma::mat &sigma);

void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S);

double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep);

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction);

void EqualRankCorrTest_chi2(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_chi2(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_chi2(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);


double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);

//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S);
//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &S, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);

//void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, int finalComparisonMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod, int finalComparisonMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);



double EqualRankCorrTestStat_noRanks(const arma::mat &Xdata, const arma::mat &Ydata);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

double EqualRankCorrTestStat_oracle(const arma::mat &Xdata, const arma::mat &Ydata);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, int finalComparisonMethod, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
