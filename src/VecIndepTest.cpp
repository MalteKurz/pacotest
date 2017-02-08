#include <pacotest_header.h>


void VecIndepTest(const arma::mat &Udata, const arma::mat &Wdata, int N, double *TestStat, double *pValue, arma::mat &S)
{
    // Transfomring the data to ranks
    int i,j,l, d1 = Udata.n_cols, d2 = Wdata.n_cols, n=Udata.n_rows;
    arma::mat X = arma::linspace<arma::mat>(1, n, n)/((double)n);
    
    arma::mat U1(n,1);
    
    arma::mat U(n,Udata.n_cols);
    arma::mat V(n,Wdata.n_cols);
    
    U1(sort_index(Udata.col(0))) = X;
    U.col(0) = U1;
    
    U1(sort_index(Wdata.col(0))) = X;
    V.col(0) = U1;
    
    if (d1 > 1)
    {
        for (i=1;i<d1;i++)
        {
            U1(sort_index(Udata.col(i))) = X;
            U.col(i) = U1;
        }
    }
    
    if (d2 > 1)
    {
        for (i=1;i<d2;i++)
        {
            U1(sort_index(Wdata.col(i))) = X;
            V.col(i) = U1;
        }
    }
    
    //
    arma::mat V1(n,n);
    arma::mat V2(n,n);
    arma::mat V1dot(1,n);
    arma::mat V2dot(1,n);
    double V1dotdot;
    double V2dotdot;
    
//doing the computations
    V1dotdot = 0;
    V2dotdot = 0;
    *TestStat = 0;
    
    for (i=0;i<n;i++)
    {
        V1(i,i) = 1-U(i,0);
        if (d1>1)
        {
            for (l=1;l<d1;l++)
            {
                V1(i,i) *= (1-U(i,l));
            }
        }
        for (j=i+1;j<n;j++)
        {
            V1(i,j) = 1-max(U(i,0),U(j,0));
            V1(j,i) = V1(i,j);
        }
        if (d1>1)
        {
            for (l=1;l<d1;l++)
            {
                for (j=i+1;j<n;j++)
                {
                    V1(i,j) *= (1-max(U(i,l),U(j,l)));
                    V1(j,i) = V1(i,j);
                }
            }
        }
    }
    
    for (i=0;i<n;i++)
    {
        V2(i,i) = 1-V(i,0);
        if (d2>1)
        {
            for (l=1;l<d2;l++)
            {
                V2(i,i) *= (1-V(i,l));
            }
        }
        for (j=i+1;j<n;j++)
        {
            V2(i,j) = 1-max(V(i,0),V(j,0));
            V2(j,i) = V2(i,j);
        }
        if (d2>1)
        {
            for (l=1;l<d2;l++)
            {
                for (j=i+1;j<n;j++)
                {
                    V2(i,j) *= (1-max(V(i,l),V(j,l)));
                    V2(j,i) = V2(i,j);
                }
            }
        }
    }
    
    V1dot = mean(V1);
    V2dot = mean(V2);
    
    V1dotdot = as_scalar(mean(V1dot,1));
    V2dotdot = as_scalar(mean(V2dot,1));
    
    *TestStat = accu(mean(V1 % V2)) + (double)n * V1dotdot * V2dotdot - 2*accu(prod(join_cols(V1dot,V2dot)));
    
    arma::mat R(n,n);
    
    R = (V1dotdot - repmat(V1dot,n,1) - repmat(V1dot.t(),1,n) + V1) % (V2dotdot - repmat(V2dot,n,1) - repmat(V2dot.t(),1,n) + V2);
    
    arma::mat xi(n,N);
    NormalRand(xi);
    
    for (i=0;i<N;i++)
    {
        S(i,0) = as_scalar(trans(xi.col(i)) * R * xi.col(i))/((double)n);
    }
    
    *pValue = as_scalar(sum(S>*TestStat))/((double)N);
}

