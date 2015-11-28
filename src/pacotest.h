
double NormalCDF(double Z);
void NormalRand(arma::mat &X);
void RandPerm(arma::uvec &X);

void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S);

double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep);

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction);
//void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction);



double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);

//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S);
//void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int TestType, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);

//void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double ExpExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold);
