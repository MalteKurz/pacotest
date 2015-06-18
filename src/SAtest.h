
void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S);

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int Grouping, double *TestStat, double *pValue);
void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep);
double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata);


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int Grouping, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata);

double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2);
void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int Grouping, double *TestStat, double *pValue, arma::mat &S);

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int Grouping, double *TestStat, double *pValue, arma::mat &S, arma::mat &Xdata, arma::mat &Ydata);

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int TestType);
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int Grouping);
