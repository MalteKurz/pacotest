#include "SAtest_header.h"
void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double ExpMinSampleSize, double TrainingDataFraction)
{
    arma::uvec Cols(2);
    
    Cols(0) =0;
    Cols(1) =1;
    
    switch(GroupingMethod){
        case 1: // TreeERC
        {
            TreeGrouping(Udata, Wdata, Xdata, Ydata, 0, ExpMinSampleSize, TrainingDataFraction);
            break;
        }
        case 2: // TreeEC
        {
            TreeGrouping(Udata, Wdata, Xdata, Ydata, 1, ExpMinSampleSize, TrainingDataFraction);
            break;
        }
        default:
        {
            Grouping(Udata, Wdata, Xdata, Ydata, GroupingMethod);
            break;
        }
            
    }
}

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod)
{
    arma::uvec Cols(2);
    
    Cols(0) =0;
    Cols(1) =1;
    
    switch(GroupingMethod){
        case 3: // SumMedian
        {
            arma::mat Wsum = sum(Wdata,1);
            
            double b = arma::as_scalar(median(Wsum));
            
            Xdata = Udata.submat( arma::find(Wsum < b), Cols);
            Ydata = Udata.submat( arma::find(Wsum >= b ), Cols);
            break;
        }
        case 4: // SumThirdsI
        {
            arma::mat Wsum = sum(Wdata,1);
            Wsum = Wsum(sort_index(Wsum));
            
            double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
            double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
            
            Xdata = Udata.submat( arma::find(Wsum < b1_3), Cols);
            Ydata = Udata.submat( arma::find(Wsum >= b1_3 && Wsum  < b2_3), Cols);
            
            break;
        }
        case 5: // SumThirdsII
        {
            arma::mat Wsum = sum(Wdata,1);
            Wsum = Wsum(sort_index(Wsum));
            
            double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
            double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
            
            Xdata = Udata.submat( arma::find(Wsum < b1_3), Cols);
            Ydata = Udata.submat( arma::find(Wsum >= b2_3), Cols);
            
            break;
        }
        case 6: // ProdMedian
        {
            arma::mat Wprod = prod(Wdata,1);
            
            double b = arma::as_scalar(median(Wprod));
            
            Xdata = Udata.submat( arma::find(Wprod < b), Cols);
            Ydata = Udata.submat( arma::find(Wprod >= b ), Cols);
            break;
        }
        case 7: // ProdThirdsI
        {
            arma::mat Wprod = prod(Wdata,1);
            Wprod = Wprod(sort_index(Wprod));
            
            double b1_3 = arma::as_scalar(Wprod(ceil((double)Wprod.n_elem /3)));
            double b2_3 = arma::as_scalar(Wprod(ceil(2* (double)Wprod.n_elem /3)));
            
            Xdata = Udata.submat( arma::find(Wprod < b1_3), Cols);
            Ydata = Udata.submat( arma::find(Wprod >= b1_3 && Wprod  < b2_3), Cols);
            
            break;
        }
        case 8: // ProdThirdsII
        {
            arma::mat Wprod = prod(Wdata,1);
            Wprod = Wprod(sort_index(Wprod));
            
            double b1_3 = arma::as_scalar(Wprod(ceil((double)Wprod.n_elem /3)));
            double b2_3 = arma::as_scalar(Wprod(ceil(2* (double)Wprod.n_elem /3)));
            
            Xdata = Udata.submat( arma::find(Wprod < b1_3), Cols);
            Ydata = Udata.submat( arma::find(Wprod >= b2_3), Cols);
            
            break;
        }
        
    }
}





void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int TestType, double ExpMinSampleSize, double TrainingDataFraction)
{
    double EvaluationDataFraction = 1-TrainingDataFraction;
    
    unsigned int m;
    if (Wdata.n_cols>1)
    {
        m=Wdata.n_cols +1;
    }
    else
    {
        m = Wdata.n_cols;
    }
    unsigned int n=Wdata.n_rows;
    double nDouble = (double) n;
    
    unsigned  int j,i;
    
    arma::mat Data(n,2+m);
    arma::mat Data1(n,2+m);
    
    Data.cols(0,1) = Udata;
    
    if (Wdata.n_cols>1)
    {
        Data.cols(2,m) = Wdata;
        Data.col(m+1) = mean(Wdata,1);
    }
    else
    {
        Data.cols(2,m+1) = Wdata;
    }
    
    
// Split the dataset randomly into two pices
    //Data1 = arma::shuffle(Data,0);
    arma::uvec C(2+m);
    
    for (i=0;i<m+2;i++)
    {
        C(i) = i;
    }
    arma::uvec R(n);
    RandPerm(R);
    Data1 = Data.submat(R,C);
    
    
    unsigned int n0 = floor(n*TrainingDataFraction);
    arma::mat W1(n0,m);
    arma::mat U1(n0,2);
    
    arma::mat W2(n-n0,m);
    arma::mat U2(n-n0,2);
    
    
// Training data
    W1 = Data1.submat(0,2,n0-1,m+1);
    U1 = Data1.submat(0,0,n0-1,1);
    
// Evaluation data
    W2 = Data1.submat(n0,2,n-1,m+1);
    U2 = Data1.submat(n0,0,n-1,1);
    
    arma::umat J(4,1);
    
// Get quantiles from learning data set
    J(0,0) = 0;
    J(1,0) = floor(n0/4)-1;
    J(2,0) = floor(n0/2)-1;
    J(3,0) = floor(3*n0/4)-1;
    
    
// Learn the first split
    arma::mat a(m,3);
    arma::mat A(n0,(m+2)*m);
    arma::umat I(n0,1);
    arma::mat X(n0,1);
    arma::mat A_EC1;
    arma::mat A_EC2;
    
    for (j=0;j<m;j++)
    {
        I = sort_index(W1.col(j));
        
        X = U1.col(0);
        A.col(j*(m+2)) = X(I);
        X = U1.col(1);
        A.col(j*(m+2)+1) = X(I);
        
        for (i=0;i<m;i++)
        {
            X = W1.col(i);
            A.col(j*(m+2)+i+2) = X(I);
        }
    }
    
    for (j=0;j<m;j++)
    {
        if (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction) // Use the 0.25 and 0.75 quantile only for more than 8*ExpMinSampleSize observations
        {
            for (i=0;i<3;i++)
            {
                if (TestType == 0)
                {
                    a(j,i) = EqualRankCorrTestStat(A.submat(0,j*(m+2),J(i+1,0),j*(m+2)+1), A.submat(J(i+1,0)+1,j*(m+2),n0-1,j*(m+2)+1));
                }
                else
                {
                    A_EC1 = A.submat(0,j*(m+2),J(i+1,0),j*(m+2)+1);
                    A_EC2 = A.submat(J(i+1,0)+1,j*(m+2),n0-1,j*(m+2)+1);
                    a(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
                }
            }
        }
        else
        {
            a(j,0) = 0;
            a(j,2) = 0;
            i=1;
            if (TestType == 0)
            {
                a(j,i) = EqualRankCorrTestStat(A.submat(0,j*(m+2),J(i+1,0),j*(m+2)+1), A.submat(J(i+1,0)+1,j*(m+2),n0-1,j*(m+2)+1));
            }
            else
            {
                A_EC1 = A.submat(0,j*(m+2),J(i+1,0),j*(m+2)+1);
                A_EC2 = A.submat(J(i+1,0)+1,j*(m+2),n0-1,j*(m+2)+1);
                a(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
            }
        }
    }
    
    
    // Split at variable k and quantile l
    unsigned int  k;
    unsigned int  l;
    a = abs(a);
    a.max(k,l);
    
    
    double b1,b2,b3;
    arma::uvec B1;
    arma::uvec B2;
    arma::uvec Cols(2);
    
    Cols(0) =0;
    Cols(1) =1;
    
    
    if ((m>1) && (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction)) //If the dimension of the conditioning set is larger than two, a second split is performed
    {
        unsigned int n1 = J(l+1,0)+1;
        unsigned int n2 = n0 - J(l+1,0)-1;
        
        arma::mat W1_1(n1,m);
        arma::mat W1_2(n0-n1,m);
        arma::mat U1_1(n1,2);
        arma::mat U1_2(n0-n1,2);
        
        // First get the conditioning sets for both groups (formed by the first split)
        W1_1 = A.submat(0,2+k*(m+2),n1-1,m+1+k*(m+2));
        W1_2 = A.submat(n1,2+k*(m+2),n0-1,m+1+k*(m+2));
        
        // Get the (pseudo-)observations for both groups (formed by the first split)
        U1_1 = A.submat(0,k*(m+2),n1-1,1+k*(m+2));
        U1_2 = A.submat(n1,k*(m+2),n0-1,1+k*(m+2));
        
        // Declare some variables
        arma::mat U1_1_1;
        arma::mat U1_1_2;
        unsigned int  k2;
        unsigned int  l2;
        arma::mat A1(n1,(m+2)*m);
        arma::umat J1;
        
        // Prepare the (left) data sets for the second split
        if (!(l==0 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
        {
            unsigned int N1;
            if ((l==0 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (l==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (l==2 && (nDouble< ExpMinSampleSize*16/3/EvaluationDataFraction)))
            {
                N1 = 1;
                J1.set_size(2,1);
                
                J1(0,0) = 0;
                J1(1,0) = floor(n1/2)-1;
            }
            else
            {
                N1 = 3;
                J1.set_size(4,1);
                
                J1(0,0) = 0;
                J1(1,0) = floor(n1/4)-1;
                J1(2,0) = floor(n1/2)-1;
                J1(3,0) = floor(3*n1/4)-1;
            }
            
            // Learn the second split
            arma::mat a1(m,N1);
            arma::umat I1(n1,1);
            arma::mat X1(n1,1);
            
            for (j=0;j<m;j++)
            {
                I1 = sort_index(W1_1.col(j));
                
                X1 = U1_1.col(0);
                A1.col(j*(m+2)) = X1(I1);
                X1 = U1_1.col(1);
                A1.col(j*(m+2)+1) = X1(I1);
                
                for (i=0;i<m;i++)
                {
                    X1 = W1_1.col(i);
                    A1.col(j*(m+2)+i+2) = X1(I1);
                }
            }
            
            for (j=0;j<m;j++)
            {
                for (i=0;i<N1;i++)
                {
                    if (TestType == 0)
                    {
                        a1(j,i) = EqualRankCorrTestStat(A1.submat(0,j*(m+2),J1(i+1,0),j*(m+2)+1), A1.submat(J1(i+1,0)+1,j*(m+2),n1-1,j*(m+2)+1));
                    }
                    else
                    {
                        A_EC1 = A1.submat(0,j*(m+2),J1(i+1,0),j*(m+2)+1);
                        A_EC2 = A1.submat(J1(i+1,0)+1,j*(m+2),n1-1,j*(m+2)+1);
                        a1(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
                    }
                }
            }
            
            // Split at variable k2 and quantile l2
            a1 = abs(a1);
            a1.max(k2,l2);
            
            // Get the (pseudo-)observations for both groups
            unsigned int n3 = J1(l2+1,0)+1;
            U1_1_1 = A1.submat(0,k2*(m+2),n3-1,1+k2*(m+2));
            U1_1_2 = A1.submat(n3,k2*(m+2),n1-1,1+k2*(m+2));
        }
        
        
        
        
        
        // Declare some variables
        arma::mat U1_2_1;
        arma::mat U1_2_2;
        unsigned int  k3;
        unsigned int  l3;
        arma::mat A2(n2,(m+2)*m);
        arma::umat J2;
        
        // Prepare the (right) data sets for the second split
        if (!(l==2 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
        {
            
            unsigned int N2;
            if ((l==2 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (l==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (l==0 && (nDouble<ExpMinSampleSize*16/3/EvaluationDataFraction)))
            {
                N2 = 1;
                J2.set_size(2,1);
                
                J2(0,0) = 0;
                J2(1,0) = floor(n2/2)-1;
            }
            else
            {
                N2 = 3;
                J2.set_size(4,1);
                
                J2(0,0) = 0;
                J2(1,0) = floor(n2/4)-1;
                J2(2,0) = floor(n2/2)-1;
                J2(3,0) = floor(3*n2/4)-1;
            }
            
            // Learn the second split
            arma::mat a2(m,N2);
            arma::umat I2(n2,1);
            arma::mat X2(n2,1);
            
            for (j=0;j<m;j++)
            {
                I2 = sort_index(W1_2.col(j));
                
                X2 = U1_2.col(0);
                A2.col(j*(m+2)) = X2(I2);
                X2 = U1_2.col(1);
                A2.col(j*(m+2)+1) = X2(I2);
                
                for (i=0;i<m;i++)
                {
                    X2 = W1_2.col(i);
                    A2.col(j*(m+2)+i+2) = X2(I2);
                }
            }
            
            for (j=0;j<m;j++)
            {
                for (i=0;i<N2;i++)
                {
                    if (TestType == 0)
                    {
                        a2(j,i) = EqualRankCorrTestStat(A2.submat(0,j*(m+2),J2(i+1,0),j*(m+2)+1), A2.submat(J2(i+1,0)+1,j*(m+2),n2-1,j*(m+2)+1));
                    }
                    else
                    {
                        A_EC1 = A2.submat(0,j*(m+2),J2(i+1,0),j*(m+2)+1);
                        A_EC2 = A2.submat(J2(i+1,0)+1,j*(m+2),n2-1,j*(m+2)+1);
                        a2(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
                    }
                }
            }
            
            // Split at variable k3 and quantile l3
            a2 = abs(a2);
            a2.max(k3,l3);
            
            
            // Get the (pseudo-)observations for both groups
            unsigned int n4 = J2(l3+1,0)+1;
            U1_2_1 = A2.submat(0,k3*(m+2),n4-1,1+k3*(m+2));
            U1_2_2 = A2.submat(n4,k3*(m+2),n2-1,1+k3*(m+2));
        }
        
        // Final comparison
        if (!(l==0 && n<ExpMinSampleSize*8/EvaluationDataFraction) && !(l==2 && n<ExpMinSampleSize*8/EvaluationDataFraction))
        {
            arma::mat b(6,1);
            if (TestType == 0)
            {
                b(0,0) = EqualRankCorrTestStat(U1_1_1,U1_1_2);
                b(1,0) = EqualRankCorrTestStat(U1_1_1,U1_2_1);
                b(2,0) = EqualRankCorrTestStat(U1_1_1,U1_2_2);
                b(3,0) = EqualRankCorrTestStat(U1_1_2,U1_2_1);
                b(4,0) = EqualRankCorrTestStat(U1_1_2,U1_2_2);
                b(5,0) = EqualRankCorrTestStat(U1_2_1,U1_2_2);
            }
            else
            {
                b(0,0) = EqualCopTestStat(U1_1_1.begin(),U1_1_2.begin(),U1_1_1.n_rows,U1_1_2.n_rows);
                b(1,0) = EqualCopTestStat(U1_1_1.begin(),U1_2_1.begin(),U1_1_1.n_rows,U1_2_1.n_rows);
                b(2,0) = EqualCopTestStat(U1_1_1.begin(),U1_2_2.begin(),U1_1_1.n_rows,U1_2_2.n_rows);
                b(3,0) = EqualCopTestStat(U1_1_2.begin(),U1_2_1.begin(),U1_1_2.n_rows,U1_2_1.n_rows);
                b(4,0) = EqualCopTestStat(U1_1_2.begin(),U1_2_2.begin(),U1_1_2.n_rows,U1_2_2.n_rows);
                b(5,0) = EqualCopTestStat(U1_2_1.begin(),U1_2_2.begin(),U1_2_1.n_rows,U1_2_2.n_rows);
            }
            
            
            unsigned int  k4;
            unsigned int  l4;
            b = abs(b);
            b.max(k4,l4);
            
            
            switch(k4){
                case 0:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                    
                    B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 );
                    B2 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                    
                    break;
                }
                case 1:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                    b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                    
                    B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 );
                    B2 = arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 ), Cols);
                    
                    break;
                }
                case 2:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                    b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                    
                    B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 );
                    B2 = arma::find(W2.col(k) > b1 && W2.col(k3) > b3 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) > b3 ), Cols);
                    
                    break;
                }
                case 3:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                    b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                    
                    B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                    B2 = arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 ), Cols);
                    
                    break;
                }
                case 4:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                    b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                    
                    B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                    B2 = arma::find(W2.col(k) > b1 && W2.col(k3) > b3 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) > b3 ), Cols);
                    
                    break;
                }
                case 5:
                {
                    b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                    b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                    
                    B1 = arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 );
                    B2 = arma::find(W2.col(k) > b1 && W2.col(k3) > b3 );
                    
                    //Xdata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 ), Cols);
                    //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) > b3 ), Cols);
                    
                    break;
                }
            }
        }
        else
        {
            if (l==0)
            {
                arma::mat b(3,1);
                if (TestType == 0)
                {
                    b(0,0) = EqualRankCorrTestStat(U1_1,U1_2_1);
                    b(1,0) = EqualRankCorrTestStat(U1_1,U1_2_2);
                    b(2,0) = EqualRankCorrTestStat(U1_2_1,U1_2_2);
                }
                else
                {
                    b(0,0) = EqualCopTestStat(U1_1.begin(),U1_2_1.begin(),U1_1.n_rows,U1_2_1.n_rows);
                    b(1,0) = EqualCopTestStat(U1_1.begin(),U1_2_2.begin(),U1_1.n_rows,U1_2_2.n_rows);
                    b(2,0) = EqualCopTestStat(U1_2_1.begin(),U1_2_2.begin(),U1_2_1.n_rows,U1_2_2.n_rows);
                }
                
                
                unsigned int  k4;
                unsigned int  l4;
                b = abs(b);
                b.max(k4,l4);
                
                
                switch(k4){
                    case 0:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                        
                        B1 = arma::find(W2.col(k) <= b1);
                        B2 = arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 );
                        
                        break;
                    }
                    case 1:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                        
                        B1 = arma::find(W2.col(k) <= b1);
                        B2 = arma::find(W2.col(k) > b1 && W2.col(k3) > b3 );
                        
                        break;
                    }
                    case 2:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b3 = arma::as_scalar(A2(J2(l3+1),k3+2+k3*(m+2)));
                        
                        B1 = arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 );
                        B2 = arma::find(W2.col(k) > b1 && W2.col(k3) > b3 );
                        
                        break;
                    }
                }
            }
            else
            {
                arma::mat b(3,1);
                if (TestType == 0)
                {
                    b(0,0) = EqualRankCorrTestStat(U1_1_1,U1_1_2);
                    b(1,0) = EqualRankCorrTestStat(U1_1_2,U1_2);
                    b(2,0) = EqualRankCorrTestStat(U1_1_2,U1_2);
                }
                else
                {
                    b(0,0) = EqualCopTestStat(U1_1_1.begin(),U1_1_2.begin(),U1_1_1.n_rows,U1_1_2.n_rows);
                    b(1,0) = EqualCopTestStat(U1_1_2.begin(),U1_2.begin(),U1_1_2.n_rows,U1_2.n_rows);
                    b(2,0) = EqualCopTestStat(U1_1_2.begin(),U1_2.begin(),U1_1_2.n_rows,U1_2.n_rows);
                }
                
                
                unsigned int  k4;
                unsigned int  l4;
                b = abs(b);
                b.max(k4,l4);
                
                
                switch(k4){
                    case 0:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                        
                        B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 );
                        B2 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                        
                        //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) <= b2 ), Cols);
                        //Ydata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                        
                        break;
                    }
                    case 1:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                        
                        B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                        B2 = arma::find(W2.col(k) > b1);
                        
                        //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                        //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) <= b3 ), Cols);
                        
                        break;
                    }
                    case 2:
                    {
                        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
                        b2 = arma::as_scalar(A1(J1(l2+1),k2+2+k2*(m+2)));
                        
                        B1 = arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 );
                        B2 = arma::find(W2.col(k) > b1);
                        
                        //Xdata = U2.submat( arma::find(W2.col(k) <= b1 && W2.col(k2) > b2 ), Cols);
                        //Ydata = U2.submat( arma::find(W2.col(k) > b1 && W2.col(k3) > b3 ), Cols);
                        
                        break;
                    }
                }
            }
        }
        
    }
    else
    {
        b1 = arma::as_scalar(A(J(l+1),k+2+k*(m+2)));
        
        B1 = arma::find(W2.col(k) <= b1);
        B2 = arma::find(W2.col(k) >b1);
    }
    
    
    if (B1.n_elem<2 || B2.n_elem<2)
    {
        throw std::runtime_error("Empty sets after applying tree grouping. It is not recommended to use the tree grouping approach with a low number of observations");
    }
    else
    {
        Xdata = U2.submat(B1, Cols);
        Ydata = U2.submat(B2, Cols);
    }
    
    return;
    
}
