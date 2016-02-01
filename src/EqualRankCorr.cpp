#include <pacotest_header.h>

void rankObs(const arma::mat &Xdata, const arma::mat &Ydata, arma::mat &UU, arma::mat &VV)
{
    arma::mat X = arma::linspace<arma::mat>(1, Xdata.n_rows, Xdata.n_rows)/(Xdata.n_rows+1);
    arma::mat Y = arma::linspace<arma::mat>(1, Ydata.n_rows, Ydata.n_rows)/(Ydata.n_rows+1);
    
    arma::mat U1(Xdata.n_rows,1);
    arma::mat U2(Xdata.n_rows,1);
    arma::mat V1(Ydata.n_rows,1);
    arma::mat V2(Ydata.n_rows,1);
    
    U1(sort_index(Xdata.col(0))) = X;
    U2(sort_index(Xdata.col(1))) = X;
    V1(sort_index(Ydata.col(0))) = Y;
    V2(sort_index(Ydata.col(1))) = Y;
    
    UU = join_rows(U1,U2);
    VV = join_rows(V1,V2);
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
    
    *TestStat = EqualRankCorrTestStat(Xdata, Ydata);
    
    *pValue = 2*(1-NormalCDF(std::abs(*TestStat)));
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
}


void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
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
    
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, Xdata, Ydata, 1, ExpMinSampleSize, TrainingDataFraction, splitVariable, splitQuantile, splitThreshold);
        
        SplitVariable.col(i) = splitVariable;
        SplitQuantile.col(i) = splitQuantile;
        SplitThreshold.col(i) = splitThreshold;
        
        S = EqualRankCorrTestStat(Xdata, Ydata);
        
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}
    
double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata)
{
    
    // Transfomring the data to ranks
    arma::mat UU(Xdata.n_rows,2);
    arma::mat VV(Ydata.n_rows,2);
    rankObs(Xdata, Ydata, UU, VV);
    
    arma::mat U1 = UU.col(0);
    arma::mat U2 = UU.col(1);
    arma::mat V1 = VV.col(0);
    arma::mat V2 = VV.col(1);
    
    double n = (double) Xdata.n_rows;
    double m = (double) Ydata.n_rows;
    double nm = n+m;
    
    int N = (int) Xdata.n_rows;
    int M = (int) Ydata.n_rows;
    int NM = N+M;
    
    
    arma::mat R1 = cov(UU);
    arma::mat R2 = cov(VV);
    
    
    double r1 = R1(0,1)/sqrt(R1(0,0))/sqrt(R1(1,1));
    double r2 = R2(0,1)/sqrt(R2(0,0))/sqrt(R2(1,1));
    
    double nom = sqrt(n*m/nm)*(r1-r2);
    
    arma::mat S(NM,2);
    
    S.submat(0,0,N-1,0) = (U1-as_scalar(mean(U1)))/sqrt(R1(0,0));
    S.submat(0,1,N-1,1) = (U2-as_scalar(mean(U2)))/sqrt(R1(1,1));
    S.submat(n,0,NM-1,0) = (V1-as_scalar(mean(V1)))/sqrt(R2(0,0));
    S.submat(N,1,NM-1,1) = (V2-as_scalar(mean(V2)))/sqrt(R2(1,1));
    
    arma::mat Z(NM,1);
    
    Z = prod(S,1) -0.5*(n/nm*r1+m/nm*r2)*(square(S.col(0))+square(S.col(1)));
    
    double sigmaX = accu(square(Z.rows(0,N-1)-as_scalar(mean(Z.rows(0,N-1)))))/(n-1);
    double sigmaY = accu(square(Z.rows(n,NM-1)-as_scalar(mean(Z.rows(N,NM-1)))))/(m-1);
    
    double V = sigmaX*n/nm + sigmaY*m/nm;
    
    return nom/sqrt(V);
}


// NO RANK STUFF
void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
    
    *TestStat = EqualRankCorrTestStat_noRanks(Xdata, Ydata);
    
    *pValue = 2*(1-NormalCDF(std::abs(*TestStat)));
}


void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest_noRanks(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
}


void EqualRankCorrTest_noRanks(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
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
    
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, Xdata, Ydata, 1, ExpMinSampleSize, TrainingDataFraction, splitVariable, splitQuantile, splitThreshold);
        
        SplitVariable.col(i) = splitVariable;
        SplitQuantile.col(i) = splitQuantile;
        SplitThreshold.col(i) = splitThreshold;
        
        S = EqualRankCorrTestStat_noRanks(Xdata, Ydata);
        
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}
    
double EqualRankCorrTestStat_noRanks(const arma::mat &Xdata, const arma::mat &Ydata)
{
    
    arma::mat UU = Xdata;
    arma::mat VV = Ydata;
    
    arma::mat U1 = UU.col(0);
    arma::mat U2 = UU.col(1);
    arma::mat V1 = VV.col(0);
    arma::mat V2 = VV.col(1);
    
    double n = (double) Xdata.n_rows;
    double m = (double) Ydata.n_rows;
    double nm = n+m;
    
    int N = (int) Xdata.n_rows;
    int M = (int) Ydata.n_rows;
    int NM = N+M;
    
    
    arma::mat R1 = cov(UU);
    arma::mat R2 = cov(VV);
    
    
    double r1 = R1(0,1)/sqrt(R1(0,0))/sqrt(R1(1,1));
    double r2 = R2(0,1)/sqrt(R2(0,0))/sqrt(R2(1,1));
    
    double nom = sqrt(n*m/nm)*(r1-r2);
    
    arma::mat S(NM,2);
    
    S.submat(0,0,N-1,0) = (U1-as_scalar(mean(U1)))/sqrt(R1(0,0));
    S.submat(0,1,N-1,1) = (U2-as_scalar(mean(U2)))/sqrt(R1(1,1));
    S.submat(n,0,NM-1,0) = (V1-as_scalar(mean(V1)))/sqrt(R2(0,0));
    S.submat(N,1,NM-1,1) = (V2-as_scalar(mean(V2)))/sqrt(R2(1,1));
    
    arma::mat Z(NM,1);
    
    Z = prod(S,1) -0.5*(n/nm*r1+m/nm*r2)*(square(S.col(0))+square(S.col(1)));
    
    double sigmaX = accu(square(Z.rows(0,N-1)-as_scalar(mean(Z.rows(0,N-1)))))/(n-1);
    double sigmaY = accu(square(Z.rows(n,NM-1)-as_scalar(mean(Z.rows(N,NM-1)))))/(m-1);
    
    double V = sigmaX*n/nm + sigmaY*m/nm;
    
    return nom/sqrt(V);
}

// ORACLE STUFF
void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
    
    *TestStat = EqualRankCorrTestStat_oracle(Xdata, Ydata);
    
    *pValue = 2*(1-NormalCDF(std::abs(*TestStat)));
}


void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest_oracle(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
}


void EqualRankCorrTest_oracle(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
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
    
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, Xdata, Ydata, 1, ExpMinSampleSize, TrainingDataFraction, splitVariable, splitQuantile, splitThreshold);
        
        SplitVariable.col(i) = splitVariable;
        SplitQuantile.col(i) = splitQuantile;
        SplitThreshold.col(i) = splitThreshold;
        
        S = EqualRankCorrTestStat_oracle(Xdata, Ydata);
        
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}
    
double EqualRankCorrTestStat_oracle(const arma::mat &Xdata, const arma::mat &Ydata)
{
    
    double n = (double) Xdata.n_rows;
    double m = (double) Ydata.n_rows;
    double nm = n+m;
    
    int N = (int) Xdata.n_rows;
    int M = (int) Ydata.n_rows;
    int NM = N+M;
    
    
    arma::mat R1 = cov(Xdata);
    arma::mat R2 = cov(Ydata);
    
    
    double r1 = R1(0,1)/sqrt(R1(0,0))/sqrt(R1(1,1));
    double r2 = R2(0,1)/sqrt(R2(0,0))/sqrt(R2(1,1));
    
    double nom = sqrt(nm)*(r1-r2);
    
    arma::mat S1(N,2);
    arma::mat S2(M,2);
    
    S1.col(0) = (Xdata.col(0)-mean(Xdata.col(0)))/sqrt(R1(0,0));
    S1.col(1) = (Xdata.col(1)-mean(Xdata.col(1)))/sqrt(R1(1,1));
    S2.col(0) = (Ydata.col(0)-mean(Ydata.col(0)))/sqrt(R2(0,0));
    S2.col(1) = (Ydata.col(1)-mean(Ydata.col(1)))/sqrt(R2(1,1));
    
    arma::mat Z1(N,1);
    arma::mat Z2(M,1);
    
    Z1 = 0.5*r1*(square(S1.col(0))+square(S1.col(1))) - prod(S1,1);
    Z2 = 0.5*r2*(square(S2.col(0))+square(S2.col(1))) - prod(S2,1);
    
    double lambdaX = n/nm;
    double lambdaY = m/nm;
    
    double sigmaX =  arma::as_scalar(var(Z1)) / lambdaX;
    double sigmaY =  arma::as_scalar(var(Z2)) / lambdaY;
    
    double V = sigmaX + sigmaY;
    
    return nom/sqrt(V);
}
