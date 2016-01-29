
double NormalCDF(double Z);
void NormalRand(arma::mat &X);
void RandPerm(arma::uvec &X);

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

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction);



double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);

//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S);
//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int TestType, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

//void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);




void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);
void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int TestType, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold, arma::mat data, Rcpp::DataFrame svcmDataFrame);



double EqualRankCorrTestStat_noRanks(const arma::mat &Xdata, const arma::mat &Ydata);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

double EqualRankCorrTestStat_oracle(const arma::mat &Xdata, const arma::mat &Ydata);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
