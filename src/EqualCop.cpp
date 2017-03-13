#include <pacotest_header.h>

using std::vector;

void ranks(double *X, int n, vector<double> &U, vector<double> &V)
{
    int i,j;
    std::vector<std::pair<double, double> > XX(n);

    // Computing the ranks
    for (i=0;i<n;i++)
    {
        XX[i] = std::make_pair(X[i],X[i+n]);
    }

    std::sort(&XX[0],&XX[n]);
    
    int Replications;
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (XX[i+Replications-1].first==XX[i+Replications].first));
        
        for (j=0;j<Replications;j++)
        {
            U[i+j] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
    
    std::vector<std::pair<double, int> > XV(n);
    for (i=0;i<n;i++)
    {
        XV[i] = std::make_pair(XX[i].second,i);
    }
    
    std::sort(&XV[0],&XV[n]);
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (XX[XV[i+Replications-1].second].second==XX[XV[i+Replications].second].second));
        
        for (j=0;j<Replications;j++)
        {
            V[XV[i+j].second] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
}


double EqualCopTestStat(double *Xdata, double *Ydata, int n1, int n2)
{
//declare variables
    double Sn1n2=0, A=0, B=0, C=0;
    int i,j;
    double n1_double = (double) n1;
    double n2_double = (double) n2;
    
    vector<double> U1(n1);
    vector<double> U2(n1);
    vector<double> V1(n2);
    vector<double> V2(n2);
    
// Obtaining ranks
    ranks(Xdata,n1,U1,U2);
    ranks(Ydata,n2,V1,V2);
    
    for (i=0;i<n1;i++)
    {
        A += (1-U1[i])*(1-max(U2[i],U2[i]));
        for (j=i+1;j<n1;j++)
        {
            A += 2.0*(1.0-U1[j])*(1.0-max(U2[i],U2[j]));
        }
    }
    
    for (i=0;i<n2;i++)
    {
        C += (1-V1[i])*(1-max(V2[i],V2[i]));
        for (j=i+1;j<n2;j++)
        {
            C += 2.0*(1.0-V1[j])*(1.0-max(V2[i],V2[j]));
        }
    }
    
    for (i=0;i<n1;i++)
    {
        for (j=0;j<n2;j++)
        {
            B += (1.0-max(U1[i],V1[j]))*(1.0-max(U2[i],V2[j]));
        }
    }
    
    double h1,h2,h3,h4,h;
    h3 = n1_double*n2_double;
    h1 = pow(n1_double,2);
    h2 = pow(n2_double,2);
    h4 = n1_double + n2_double;
    h= h3/h4;
    
    Sn1n2 = h*(A/h1-2.0*B/h3+C/h2);
    
    return Sn1n2;
}


void ranksA(const double *X,int n, double *A1, double *A2, double *AA2)
{
    int i,j;
    std::vector<std::pair<double, double> > X1(n);

    // Computing the ranks
    for (i=0;i<n;i++)
    {
        X1[i] = std::make_pair(X[i],X[i+n]);
    }

    std::sort(&X1[0],&X1[n]);
    
    int Replications;
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (X1[i+Replications-1].first==X1[i+Replications].first));
        
        for (j=0;j<Replications;j++)
        {
            A1[i+j] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
    
    std::vector<std::pair<double, int> > X2(n);
    for (i=0;i<n;i++)
    {
        X2[i] = std::make_pair(X1[i].second,i);
    }
    
    std::sort(&X2[0],&X2[n]);
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (X1[X2[i+Replications-1].second].second==X1[X2[i+Replications].second].second));
        
        for (j=0;j<Replications;j++)
        {
            A2[X2[i+j].second] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
    for (i=0;i<n;i++)
    {
        AA2[IndTria(i,i,n)]= A2[i];
        for (j=i+1;j<n;j++)
        {
            AA2[IndTria(i,j,n)]= max(A2[i],A2[j]);
        }
    }
}

void ranksB(const double *X,int n, double *B1, double *B2, double *BB1)
{
    int i,j;
    std::vector<std::pair<double, double> > X2(n);

    // Computing the ranks
    for (i=0;i<n;i++)
    {
        X2[i] = std::make_pair(X[i+n],X[i]);
    }

    std::sort(&X2[0],&X2[n]);
    
    int Replications;
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (X2[i+Replications-1].first==X2[i+Replications].first));
        
        for (j=0;j<Replications;j++)
        {
            B2[i+j] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
    
    std::vector<std::pair<double, int> > X1(n);
    for (i=0;i<n;i++)
    {
        X1[i] = std::make_pair(X2[i].second,i);
    }
    
    std::sort(&X1[0],&X1[n]);
    
    for (i=0;i<n;i++)
    {
        Replications = 0;
        do {
            Replications++;
        } while( ((i+Replications)<n) && (X2[X1[i+Replications-1].second].second==X2[X1[i+Replications].second].second));
        
        for (j=0;j<Replications;j++)
        {
            B1[X1[i+j].second] = (double) (2*i+Replications+1)/(2*n+2);
        }
        i+=Replications-1;
    }
    for (i=0;i<n;i++)
    {
        BB1[IndTria(i,i,n)]= B1[i];
        for (j=i+1;j<n;j++)
        {
            BB1[IndTria(i,j,n)]= max(B1[i],B1[j]);
        }
    }
}

/*
 * The function GetAAMatrices gives back a vector which does only depend on
 * the ranks of observed values for one of the two copula. The vector which
 * is given back by the function is a vectorized (n x n) or (m x m) matrix,
 * which has to be multiplied elementwise with xi_i and xi_j or eta_i and
 * eta_j, depending whether the input values correspond to the first or the
 * second copula. By summing up all elements of the resulting vector and
 * multiplying the resulting value by some constants, one obtains the value
 * of the integral of the function C squared or D squared depending on the
 * input (so either U and the corresponding matrices or V and the
 * corresponding matrices).
 */
static void GetAAMatrices (double *A1, double *A2, double *A1plush, double *A1minush, double *A2plush, double *A2minush, double *AA2, double *IntASquaredij, int n, double h)
{
    int j,o,p;
    
    int i;
    
    double N =n;
    
    bool I1 = 0;
    double I2 = 0;
    const double H = h*(n+1);
    int I3 = 0;
    
    //#pragma omp parallel for private(i,j,p,o)
    for (i=0;i<(int) n;i++)
    {
        for (j=0;j<n;j++)
        {
            IntASquaredij[j*n+i] = (1-A1[max(i,j)]) * (1-AA2[IndTria(i,j,n)]);
        }
        
        I1 = (i<(n-H));
        I2 = H+max(i,H-1);
        I3 = max(0.0,min(i-H,n-2*H)+1);
        for (j=0;j<i;j++)
        {
            for (o=I3;o<n;o++)
            {
                if (I1 && o>I2)
                {
                    IntASquaredij[j*n+i] -= (A1plush[o]-A1minush[o])*(1-AA2[IndTria(i,o,n)])/(N*h);
                }
                else
                {
                    IntASquaredij[j*n+i] -= (A1plush[o]-A1[max(i,j)])*(1-AA2[IndTria(i,o,n)])/(N*h);
                }
            }
        }
        for (j=i;j<n;j++)
        {
        I1 = (j<(n-H));
        I2 = H+max(j,H-1);
        I3 = max(0.0,min(j-H,n-2*H)+1);
            for (o=I3;o<n;o++)
            {
                if (I1 && o>I2)
                {
                    IntASquaredij[j*n+i] -= (A1plush[o]-A1minush[o])*(1-AA2[IndTria(i,o,n)])/(N*h);
                }
                else
                {
                    IntASquaredij[j*n+i] -= (A1plush[o]-A1[max(i,j)])*(1-AA2[IndTria(i,o,n)])/(N*h);
                }
            }
        }
        
        for (j=0;j<n;j++)
        {
            for (o=0;o<n;o++)
            {
                if (A2minush[o]>AA2[IndTria(i,j,n)])
                {
                    IntASquaredij[j*n+i] -= (A2plush[o]-A2minush[o])*(1-A1[max(i,o)])/(N*h);
                }
                else if (A2plush[o]>AA2[IndTria(i,j,n)])
                {
                    IntASquaredij[j*n+i] -= (A2plush[o]-AA2[IndTria(i,j,n)])*(1-A1[max(i,o)])/(N*h);
                }
            }
            I1 = (i<(n-H));
            I2 = H+max(i,H-1);
            I3 = max(0.0,min(i-H,n-2*H)+1);
            for (p=0;p<i;p++)
            {
                for (o=I3;o<n;o++)
                {
                    if (I1 && o>I2)
                    {
                        if (A2minush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1minush[o])*(A2plush[p]-A2minush[p])/(N*N*2*h*h);
                        }
                        else if (A2plush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1minush[o])*(A2plush[p]-AA2[IndTria(j,o,n)])/(N*N*2*h*h);
                        }
                    }
                    else
                    {
                        if (A2minush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1[max(i,p)])*(A2plush[p]-A2minush[p])/(N*N*2*h*h);
                        }
                        else if (A2plush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1[max(i,p)])*(A2plush[p]-AA2[IndTria(j,o,n)])/(N*N*2*h*h);
                        }
                    }
                }
            }
            for (p=i;p<n;p++)
            {
            I1 = (p<(n-H));
            I2 = H+max(p,H-1);
            I3 = max(0.0,min(p-H,n-2*H)+1);
                for (o=I3;o<n;o++)
                {
                    if (I1 && o>I2)
                    {
                        if (A2minush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1minush[o])*(A2plush[p]-A2minush[p])/(N*N*2*h*h);
                        }
                        else if (A2plush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1minush[o])*(A2plush[p]-AA2[IndTria(j,o,n)])/(N*N*2*h*h);
                        }
                    }
                    else
                    {
                        if (A2minush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1[max(i,p)])*(A2plush[p]-A2minush[p])/(N*N*2*h*h);
                        }
                        else if (A2plush[p]>AA2[IndTria(j,o,n)])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o]-A1[max(i,p)])*(A2plush[p]-AA2[IndTria(j,o,n)])/(N*N*2*h*h);
                        }
                    }
                }
            }
            
            for (o=0;o<n;o++)
            {
                for (p=0;p<o;p++)
                {
                    if(A1minush[o]>A1[max(i,j)])
                    {
                        if (A1plush[p]>A1minush[o])
                        {
                            IntASquaredij[j*n+i] += (A1plush[p] - A1minush[o])*(1-AA2[IndTria(o,p,n)])/(N*N*4*h*h);
                        }
                    }
                    else if (A1plush[p]>A1[max(i,j)])
                    {
                        IntASquaredij[j*n+i] += (A1plush[p] - A1[max(i,j)])*(1-AA2[IndTria(o,p,n)])/(N*N*4*h*h);
                    }
                }
                for (p=o;p<n;p++)
                {
                    if(A1minush[p]>A1[max(i,j)])
                    {
                        if (A1plush[o]>A1minush[p])
                        {
                            IntASquaredij[j*n+i] += (A1plush[o] - A1minush[p])*(1-AA2[IndTria(o,p,n)])/(N*N*4*h*h);
                        }
                    }
                    else if (A1plush[o]>A1[max(i,j)])
                    {
                        IntASquaredij[j*n+i] += (A1plush[o] - A1[max(i,j)])*(1-AA2[IndTria(o,p,n)])/(N*N*4*h*h);
                    }
                }
            }

            for (o=0;o<n;o++)
            {
                for (p=0;p<n;p++)
                {
                    if(max(A2minush[o],A2minush[p])>AA2[IndTria(i,j,n)])
                    {
                        if (A2[o]>A2[p])
                        {
                            if (A2plush[p]>A2minush[o])
                            {
                                IntASquaredij[j*n+i] += (A2plush[p] - A2minush[o])*(1-A1[max(o,p)])/(N*N*4*h*h);
                            }
                        }
                        else
                        {
                            if (A2plush[o]>A2minush[p])
                            {
                                IntASquaredij[j*n+i] += (A2plush[o] - A2minush[p])*(1-A1[max(o,p)])/(N*N*4*h*h);
                            }
                        }
                    }
                    else if (min(A2plush[o],A2plush[p])>AA2[IndTria(i,j,n)])
                    {
                        if (A2[o]>A2[p])
                        {
                            IntASquaredij[j*n+i] += (A2plush[p] - AA2[IndTria(i,j,n)])*(1-A1[max(o,p)])/(N*N*4*h*h);
                        }
                        else
                        {
                            IntASquaredij[j*n+i] += (A2plush[o] - AA2[IndTria(i,j,n)])*(1-A1[max(o,p)])/(N*N*4*h*h);
                        }
                    }
                }
                
            }
        }
    }
    
    return;
}

/*
 * The function GetABMatrices gives back a vector which does depend on the
 * ranks of observed values of both copula. The vector which is given back
 * by the function is a vectorized (n x m) matrix, which has to be
 * multiplied elementwise with xi_i and eta_j. By summing up all elements
 * of the resulting vector and multiplying the resulting value by some
 * constants, one obtains the value of the integral of the function C times
 * the function D.
 */
static void GetABMatrices (double *A1,  double *B1, double *A2,  double *B2, double *A1plush, double *A1minush, double *B1plush, double *B1minush, double *A2plush, double *A2minush, double *B2plush, double *B2minush, double *AB1, double *AB2, double *IntABij, int n1, int n2, double h1, double h2)
{
    int i,j,o,p;
  
    double n1_double = (double) n1;
    double n2_double = (double) n2;
    
    
    for (i=0;i<n1;i++)
    {
        for (j=0;j<n2;j++)
        {
            IntABij[j*n1+i] = (1-AB1[j*n1+i]) * (1-AB2[j*n1+i]);
            
            for (p=0;p<n2;p++)
            {
                if (B1minush[p]>AB1[j*n1+i])
                {
                    IntABij[j*n1+i] -= (B1plush[p]-B1minush[p])*(1-AB2[p*n1+i])/(n2_double*2*h2);
                }
                else if (B1plush[p]>AB1[j*n1+i])
                {
                    IntABij[j*n1+i] -= (B1plush[p]-AB1[j*n1+i])*(1-AB2[p*n1+i])/(n2_double*2*h2);
                }
                if (B2minush[p]>AB2[j*n1+i])
                {
                    IntABij[j*n1+i] -= (B2plush[p]-B2minush[p])*(1-AB1[p*n1+i])/(n2_double*2*h2);
                }
                else if (B2plush[p]>AB2[j*n1+i])
                {
                    IntABij[j*n1+i] -= (B2plush[p]-AB2[j*n1+i])*(1-AB1[p*n1+i])/(n2_double*2*h2);
                }
            }
            
            for (o=0;o<n1;o++)
            {
                if (A1minush[o]>AB1[j*n1+i])
                {
                    IntABij[j*n1+i] -= (A1plush[o]-A1minush[o])*(1-AB2[j*n1+o])/(n1_double*2*h1);
                }
                else if (A1plush[o]>AB1[j*n1+i])
                {
                    IntABij[j*n1+i] -= (A1plush[o]-AB1[j*n1+i])*(1-AB2[j*n1+o])/(n1_double*2*h1);
                }
                if (A2minush[o]>AB2[j*n1+i])
                {
                    IntABij[j*n1+i] -= (A2plush[o]-A2minush[o])*(1-AB1[j*n1+o])/(n1_double*2*h1);
                }
                else if (A2plush[o]>AB2[j*n1+i])
                {
                    IntABij[j*n1+i] -= (A2plush[o]-AB2[j*n1+i])*(1-AB1[j*n1+o])/(n1_double*2*h1);
                }
                
                for (p=0;p<n2;p++)
                {
                    if (A1minush[o]>AB1[p*n1+i])
                    {
                        if (B2minush[p]>AB2[j*n1+o])
                        {
                            IntABij[j*n1+i] += (A1plush[o]-A1minush[o])*(B2plush[p]-B2minush[p])/(n1_double*n2_double*4*h1*h2);
                        }
                        else if (B2plush[p]>AB2[j*n1+o])
                        {
                            IntABij[j*n1+i] += (A1plush[o]-A1minush[o])*(B2plush[p]-AB2[j*n1+o])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                    else if (A1plush[o]>AB1[p*n1+i])
                    {
                        if (B2minush[p]>AB2[j*n1+o])
                        {
                            IntABij[j*n1+i] += (A1plush[o]-AB1[p*n1+i])*(B2plush[p]-B2minush[p])/(n1_double*n2_double*4*h1*h2);
                        }
                        else if (B2plush[p]>AB2[j*n1+o])
                        {
                            IntABij[j*n1+i] += (A1plush[o]-AB1[p*n1+i])*(B2plush[p]-AB2[j*n1+o])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                    
                    if (B1minush[p]>AB1[j*n1+o])
                    {
                        if (A2minush[o]>AB2[p*n1+i])
                        {
                            IntABij[j*n1+i] += (B1plush[p]-B1minush[p])*(A2plush[o]-A2minush[o])/(n1_double*n2_double*4*h1*h2);
                        }
                        else if (A2plush[o]>AB2[p*n1+i])
                        {
                            IntABij[j*n1+i] += (B1plush[p]-B1minush[p])*(A2plush[o]-AB2[p*n1+i])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                    else if (B1plush[p]>AB1[j*n1+o])
                    {
                        if (A2minush[o]>AB2[p*n1+i])
                        {
                            IntABij[j*n1+i] += (B1plush[p]-AB1[j*n1+o])*(A2plush[o]-A2minush[o])/(n1_double*n2_double*4*h1*h2);
                        }
                        else if (A2plush[o]>AB2[p*n1+i])
                        {
                            IntABij[j*n1+i] += (B1plush[p]-AB1[j*n1+o])*(A2plush[o]-AB2[p*n1+i])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                    
                    if(max(A1minush[o],B1minush[p])>AB1[n1*j+i])
                    {
                        if (A1[o]>B1[p])
                        {
                            if (B1plush[p]>A1minush[o])
                            {
                                IntABij[j*n1+i] += (B1plush[p] - A1minush[o])*(1-AB2[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                            }
                        }
                        else
                        {
                            if (A1plush[o]>B1minush[p])
                            {
                                IntABij[j*n1+i] += (A1plush[o] - B1minush[p])*(1-AB2[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                            }
                        }
                    }
                    else if (min(A1plush[o],B1plush[p])>AB1[n1*j+i])
                    {
                        if (A1[o]>B1[p])
                        {
                            IntABij[j*n1+i] += (B1plush[p] - AB1[n1*j+i])*(1-AB2[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                        }
                        else
                        {
                            IntABij[j*n1+i] += (A1plush[o] - AB1[n1*j+i])*(1-AB2[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                    
                    if(max(A2minush[o],B2minush[p])>AB2[n1*j+i])
                    {
                        if (A2[o]>B2[p])
                        {
                            if (B2plush[p]>A2minush[o])
                            {
                                IntABij[j*n1+i] += (B2plush[p] - A2minush[o])*(1-AB1[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                            }
                        }
                        else
                        {
                            if (A2plush[o]>B2minush[p])
                            {
                                IntABij[j*n1+i] += (A2plush[o] - B2minush[p])*(1-AB1[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                            }
                        }
                    }
                    else if (min(A2plush[o],B2plush[p])>AB2[n1*j+i])
                    {
                        if (A2[o]>B2[p])
                        {
                            IntABij[j*n1+i] += (B2plush[p] - AB2[n1*j+i])*(1-AB1[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                        }
                        else
                        {
                            IntABij[j*n1+i] += (A2plush[o] - AB2[n1*j+i])*(1-AB1[n1*p+o])/(n1_double*n2_double*4*h1*h2);
                        }
                    }
                }
            }
        }
    }
    
    return;
}

/*
 * The function IntASquared computes the value of the integral of the
 * function C squared or D squared (depending on which data is used as
 * input argument) for each of the N different boostrapped samples.
 */
static void IntASquared (double *A1, double *A2, double *A1plush, double *A1minush, double *A2plush, double *A2minush, double *AA2, double *X, double *IntA, int n1, int N, double h)
{
    int k,i,j;
    
    vector<double> IntASquaredij(n1*n1);
    
    double n1_double =n1;
    
    GetAAMatrices (A1,A2,A1plush,A1minush,A2plush,A2minush,AA2, &IntASquaredij[0],n1,h);
    
    for (k=0;k<N;k++)
    {
        for (i=0;i<n1;i++)
        {
            for (j=0;j<n1;j++)
            {
                IntA[k] += X[i*N+k]*X[j*N+k]*IntASquaredij[j*n1+i];
            }
        }
        IntA[k] /= n1_double;
    }
    
    return;
}

static void IntBSquared (double *B1, double *B2, double *B1plush, double *B1minush, double *B2plush, double *B2minush, double *BB1, double *X, double *IntB, int n2, int N, double h)
{
    int k,i,j;
    
    
    vector<double> IntBSquaredij(n2*n2);
    
    double n2_double = (double) n2;
    
    GetAAMatrices (B2,B1,B2plush,B2minush,B1plush,B1minush,BB1, &IntBSquaredij[0],n2,h);
    
    for (k=0;k<N;k++)
    {
        for (i=0;i<n2;i++)
        {
            for (j=0;j<n2;j++)
            {
                IntB[k] += X[i*N+k]*X[j*N+k]*IntBSquaredij[j*n2+i];
            }
        }
        IntB[k] /= n2_double;
    }
    
    return;
}

/*
 * The function CalcIntAB computes the value of the integral of the
 * function C times the function D for each of the N different boostrapped
 * samples.
 */
static void CalcIntAB (double *A1, double *B1, double *A2, double *B2, double *A1plush, double *A1minush, double *B1plush, double *B1minush, double *A2plush, double *A2minush, double *B2plush, double *B2minush, double *AB1, double *AB2, double *X1, double *X2, double *IntAB, int n1, int n2, int N, double h1, double h2)
{
    int k,i,j;
    
    
    vector<double> IntABij(n1*n2);
    
    GetABMatrices (A1,B1,A2,B2,A1plush,A1minush,B1plush,B1minush,A2plush,A2minush,B2plush,B2minush,AB1,AB2,&IntABij[0],n1,n2,h1,h2);
    
    for (k=0;k<N;k++)
    {
        for (i=0;i<n1;i++)
        {
            for (j=0;j<n2;j++)
            {
                IntAB[k] += X1[i*N+k]*X2[j*N+k]*IntABij[j*n1+i];
            }
        }
    }
    
    return;
}


double TwoCopTest(double *X, double *Y, double *Xi, double *Eta, int n1, int n2, double h1, double h2, int N, double *CC, double *DD, double *CD)
{
//declare variables
    double A=0, B=0, C=0;
    int i,j;
    
    double n1_double = (double) n1;
    double n2_double = (double) n2;
    
    vector<double> U1;
    vector<double> U2;
    vector<double> V1;
    vector<double> V2;
    
    U1.reserve(n1);
    U2.reserve(n1);
    V1.reserve(n2);
    V2.reserve(n2);
    
    vector<double> UU2;
    vector<double> VV1;
    
    UU2.reserve(n1*(n1+1)/2);
    VV1.reserve(n2*(n2+1)/2);
    
    vector<double> UV1;
    vector<double> UV2;
    
    UV1.reserve(n1*n2);
    UV2.reserve(n1*n2);
    
// Obtaining ranks
    ranksA(X,n1,&U1[0],&U2[0],&UU2[0]);
    ranksB(Y,n2,&V1[0],&V2[0],&VV1[0]);
    
    for (i=0;i<n1;i++)
    {
        A += (1-U1[i])*(1-U2[i]);
        for (j=i+1;j<n1;j++)
        {
            A += 2.0*(1-U1[j])*(1-UU2[IndTria(i,j,n1)]);
        }
    }
    
    for (i=0;i<n2;i++)
    {
        C += (1-V1[i])*(1-V2[i]);
        for (j=i+1;j<n2;j++)
        {
            C += 2.0*(1-VV1[IndTria(i,j,n2)])*(1-V2[j]);
        }
    }
    
    double* low;
    int pos;
    
    for (j=0;j<n2;j++)
    {
        low = std::lower_bound (&U1[0], &U1[n1], V1[j]);
        pos = low-&U1[0];
        for (i=0;i<pos;i++)
        {
            UV1[j*n1+i] = V1[j];
        }
        for (i=pos;i<n1;i++)
        {
            UV1[j*n1+i] = U1[i];
        }
    }
    
    for (i=0;i<n1;i++)
    {
        low = std::lower_bound (&V2[0], &V2[n2], U2[i]);
        pos = low-&V2[0];
        for (j=0;j<pos;j++)
        {
            UV2[j*n1+i] = U2[i];
        }
        for (j=pos;j<n2;j++)
        {
            UV2[j*n1+i] = V2[j];
        }
    }
    
    
    for (i=0;i<n1;i++)
    {
        for (j=0;j<n2;j++)
        {
            B += (1.0-UV1[j*n1+i])*(1.0-UV2[j*n1+i]);
        }
    }
    
    double hh1,hh2,hh3,hh4,hh;
    hh3 = n1_double*n2_double;
    hh1 = pow(n1_double,2);
    hh2 = pow(n2_double,2);
    hh4 = n1_double + n2_double;
    hh= hh3/hh4;
    
    vector<double> U1plush(n1);
    vector<double> U1minush(n1);
    vector<double> U2plush(n1);
    vector<double> U2minush(n1);
    vector<double> V1plush(n2);
    vector<double> V1minush(n2);
    vector<double> V2plush(n2);
    vector<double> V2minush(n2);
    
    /*
     * By the desing of the code only at this point (i.e., in the
     * computation of the Uplush, Uminush, Vplush and Vminush matrices)
     * there is a difference between the code to compute the multiplier
     * test statistics, which uses the originally proposed method for
     * approximating the partial derivatives numerically and the sligthly
     * addapted method, which is used here.
     */
    for (i=0;i<n1;i++)
    {
        if (U1[i]>h1 && U1[i]<2*h1)
        {
            U1plush[i] = min(U1[i]+h1,1);
            U1minush[i] = 0;
        }
        else if (U1[i]>1-2*h1 && U1[i]<1-h1)
        {
            U1plush[i] = 1;
            U1minush[i] = U1[i]-h1;
        }
        else
        {
            U1plush[i] = min(U1[i]+h1,1);
            U1minush[i] = U1[i]-h1;
        }
        if (U2[i]>h1 && U2[i]<2*h1)
        {
            U2plush[i] = min(U2[i]+h1,1);
            U2minush[i] = 0;
        }
        else if (U2[i]>1-2*h1 && U2[i]<1-h1)
        {
            U2plush[i] = 1;
            U2minush[i] = U2[i]-h1;
        }
        else
        {
            U2plush[i] = min(U2[i]+h1,1);
            U2minush[i] = U2[i]-h1;
        }
    }
    
    for (i=0;i<n2;i++)
    {
        if (V1[i]>h2 && V1[i]<2*h2)
        {
            V1plush[i] = min(V1[i]+h2,1);
            V1minush[i] = 0;
        }
        else if (V1[i]>1-2*h2 && V1[i]<1-h2)
        {
            V1plush[i] = 1;
            V1minush[i] = V1[i]-h2;
        }
        else
        {
            V1plush[i] = min(V1[i]+h2,1);
            V1minush[i] = V1[i]-h2;
        }
        if (V2[i]>h2 && V2[i]<2*h2)
        {
            V2plush[i] = min(V2[i]+h2,1);
            V2minush[i] = 0;
        }
        else if (V2[i]>1-2*h2 && V2[i]<1-h2)
        {
            V2plush[i] = 1;
            V2minush[i] = V2[i]-h2;
        }
        else
        {
            V2plush[i] = min(V2[i]+h2,1);
            V2minush[i] = V2[i]-h2;
        }
    }
    
    // Computing the integrals of C squared D squared and C times D
    
    
    IntASquared (&U1[0],&U2[0],&U1plush[0],&U1minush[0],&U2plush[0],&U2minush[0],&UU2[0],Xi,CC,n1,N,h1);
    
    IntBSquared (&V1[0],&V2[0],&V1plush[0],&V1minush[0],&V2plush[0],&V2minush[0],&VV1[0],Eta,DD,n2,N,h2);
    
    CalcIntAB (&U1[0],&V1[0],&U2[0],&V2[0],&U1plush[0],&U1minush[0],&V1plush[0],&V1minush[0],&U2plush[0],&U2minush[0],&V2plush[0],&V2minush[0],&UV1[0],&UV2[0],Xi,Eta,CD,n1,n2,N,h1,h2);
    
    
    return hh*(A/hh1-2.0*B/hh3+C/hh2);
}

void EqualCopTest(const arma::mat &Udata, const arma::mat &Wdata, int N, int GroupingMethod, int finalComparisonMethod, double *TestStat, double *pValue, arma::mat &S, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
    arma::mat Xdata;
    arma::mat Ydata;
    
    int n1, n2, k;
    
// Initialize some vectors
    vector<double> CC(N);
    vector<double> DD(N);
    vector<double> CD(N);
    
    Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
    
    // Figure out dimensions
    n1 = (int) Xdata.n_rows;
    n2 = (int) Ydata.n_rows;
    
    double n1_double = (double) n1;
    double n2_double = (double) n2;
    
    double h1 = 1/sqrt(n1_double);
    double h2 = 1/sqrt(n2_double);
    
    arma::mat Xi(N,n1);
    arma::mat Eta(N,n2);
    
    NormalRand(Xi);
    NormalRand(Eta);
    
    for (k=0;k<N;k++)
    {
        Xi.row(k) = Xi.row(k) - mean(Xi.row(k));
        Eta.row(k) = Eta.row(k) - mean(Eta.row(k));
    }
    
    
// Compute the test statistic and obtain the vectors to calculate the boostrapped values of the test statistic
    *TestStat = TwoCopTest(Xdata.begin(),Ydata.begin(),Xi.begin(),Eta.begin(),n1,n2,h1,h2,N,&CC[0],&DD[0],&CD[0]);
    
    // Computing the multiplier bootstrapped values of the test statistic
    for (k=0;k<N;k++)
    {
        S[k] = (n2_double*CC[k]+n1_double*DD[k]-2*CD[k])/(n1_double+n2_double);
    }
    
    *pValue = accu(S>*TestStat)/((double)N);
    
}

