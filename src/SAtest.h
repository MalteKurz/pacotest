
double NormalCDF(double Z);
void NormalRand(arma::mat &X);
void RandPerm(arma::uvec &X);

void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S);

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double MinSampleSize, double TrainingDataPercentage);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double MinSampleSize, double TrainingDataPercentage);
double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata);


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double MinSampleSize, double TrainingDataPercentage);

double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, double MinSampleSize, double TrainingDataPercentage);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata, double MinSampleSize, double TrainingDataPercentage);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int TestType, double MinSampleSize, double TrainingDataPercentage);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double MinSampleSize, double TrainingDataPercentage);
