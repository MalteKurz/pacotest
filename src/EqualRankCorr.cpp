#include "SAtest_header.h"

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata)
{
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, 50, 0.5);
}

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, arma::mat &Xdata, arma::mat &Ydata, double ExpMinSampleSize, double TrainingDataFraction)
{
    Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction);
    
    *TestStat = EqualRankCorrTestStat(Xdata, Ydata);
    
    //boost::math::normal dist(0,1);
    //*pValue = 2*(1-boost::math::cdf(dist, std::abs(*TestStat)));
    //*pValue = 2*(1-pnorm(std::abs(*TestStat),0,1,1,0));
    *pValue = 2*(1-NormalCDF(std::abs(*TestStat)));
}

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue)
{
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, TestStat, pValue, 50, 0.5);
}

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, int GroupingMethod, double *TestStat, double *pValue, double ExpMinSampleSize, double TrainingDataFraction)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    EqualRankCorrTest(Udata, Wdata, GroupingMethod, TestStat, pValue, Xdata, Ydata, ExpMinSampleSize, TrainingDataFraction);
}

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep)
{
     EqualRankCorrTest(Udata, Wdata, pValues, pValue, AggPvalsNumbRep, 50, 0.5);
}

void EqualRankCorrTest(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &pValues, double *pValue, int AggPvalsNumbRep, double ExpMinSampleSize, double TrainingDataFraction)
{
    arma::mat Xdata;
    arma::mat Ydata;
    double S;
    int i;
    
    //boost::math::normal dist(0,1);
        
    for (i=0;i<AggPvalsNumbRep;i++)
    {
        Grouping(Udata, Wdata, Xdata, Ydata, 1, ExpMinSampleSize, TrainingDataFraction);
        
        S = EqualRankCorrTestStat(Xdata, Ydata);
        
        //pValues(i,0) = 2*(1-boost::math::cdf(dist,std::abs(S)));
        //pValues(i,0) = 2*(1-pnorm(std::abs(S),0,1,1,0));
        pValues(i,0) = 2*(1-NormalCDF(std::abs(S)));
    }
    
    *pValue = (double) arma::as_scalar(arma::median(pValues,0));
    
}
    
double EqualRankCorrTestStat(const arma::mat &Xdata, const arma::mat &Ydata)
{
    // Transfomring the data to ranks
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
    
    arma::mat UU = join_rows(U1,U2);
    arma::mat VV = join_rows(V1,V2);
    
    
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

