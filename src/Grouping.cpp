#include <pacotest_header.h>

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod)
{
  
  switch(GroupingMethod){
    case 3: // SumMedian
    {
      arma::mat Wsum = sum(Wdata,1);
      
      double b = arma::as_scalar(median(Wsum));
      
      indXdata = arma::find(Wsum < b);
      indYdata = arma::find(Wsum >= b );
      break;
    }
    case 4: // SumThirdsI
    {
      arma::mat Wsum = sum(Wdata,1);
      Wsum = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
      
      indXdata = arma::find(Wsum < b1_3);
      indYdata = arma::find(Wsum >= b1_3 && Wsum  < b2_3);
      
      break;
    }
    case 5: // SumThirdsII
    {
      arma::mat Wsum = sum(Wdata,1);
      Wsum = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
      
      indXdata = arma::find(Wsum < b1_3);
      indYdata = arma::find(Wsum >= b2_3);
      
      break;
    }
    case 6: // ProdMedian
    {
      arma::mat Wprod = prod(Wdata,1);
      
      double b = arma::as_scalar(median(Wprod));
      
      indXdata = arma::find(Wprod < b);
      indYdata = arma::find(Wprod >= b );
      break;
    }
    case 7: // ProdThirdsI
    {
      arma::mat Wprod = prod(Wdata,1);
      Wprod = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(Wprod(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(Wprod(ceil(2* (double)Wprod.n_elem /3)));
      
      indXdata = arma::find(Wprod < b1_3);
      indYdata = arma::find(Wprod >= b1_3 && Wprod  < b2_3);
      
      break;
    }
    case 8: // ProdThirdsII
    {
      arma::mat Wprod = prod(Wdata,1);
      Wprod = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(Wprod(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(Wprod(ceil(2* (double)Wprod.n_elem /3)));
      
      indXdata = arma::find(Wprod < b1_3);
      indYdata = arma::find(Wprod >= b2_3);
      
      break;
    }
    
  }
}


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
{
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  arma::uvec indXdata;
  arma::uvec indYdata;
  
  Grouping(Udata, Wdata, indXdata, indYdata, GroupingMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  Xdata = Udata.submat(indXdata, Cols);
  Ydata = Udata.submat(indYdata, Cols);
  
  return;
  
}


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
{
  
  switch(GroupingMethod){
    case 1: // TreeERC
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 0, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    case 2: // TreeEC
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 1, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    default:
    {
      Grouping(Udata, Wdata, indXdata, indYdata, GroupingMethod);
      break;
    }
    
  }
}


void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int TestType, double ExpMinSampleSize, double TrainingDataFraction, arma::umat &SplitVariable, arma::umat &SplitQuantile, arma::mat &SplitThreshold)
{
  double EvaluationDataFraction = 1-TrainingDataFraction;
  
  unsigned int m;
  
  m = Wdata.n_cols;
  
  unsigned int n=Wdata.n_rows;
  double nDouble = (double) n;
  
  unsigned  int j,i;
  
  // Split the dataset randomly into two pices
  arma::uvec R(n);
  RandPerm(R); // RandPerm is defined in a way that it doesn't matter how it is initialized
  
  
  unsigned int n0 = floor(n*TrainingDataFraction);
  arma::uvec R1(n0);
  
  arma::uvec R2(n-n0);
  
  
  // Training data
  R1 = R.subvec(0,n0-1);
  
  // Evaluation data
  R2 = R.subvec(n0,n-1);
  
  arma::umat J(4,1);
  
  // Get quantiles from learning data set
  J(0,0) = 0;
  J(1,0) = floor(n0/4)-1;
  J(2,0) = floor(n0/2)-1;
  J(3,0) = floor(3*n0/4)-1;
  
  
  // Learn the first split
  arma::mat a(m,3);
  arma::umat I(n0,m);
  arma::uvec firstGroupInd;
  arma::uvec secondGroupInd;
  
  arma::mat A_EC1;
  arma::mat A_EC2;
  
  arma::uvec X(n0);
  arma::uvec colIndex(1);
  for (j=0;j<m;j++)
  {
    colIndex(0) = j;
    X = sort_index(Wdata.submat(R1,colIndex));
    I.col(j) = R1(X);
  }
  
  for (j=0;j<m;j++)
  {
    if (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction) // Use the 0.25 and 0.75 quantile only for more than 8*ExpMinSampleSize observations
    {
      for (i=0;i<3;i++)
      {
        if (TestType == 0)
        {
          firstGroupInd = I.submat(0,j,J(i+1,0),j);
          secondGroupInd = I.submat(J(i+1,0)+1,j,n0-1,j);
          a(j,i) = EqualRankCorrTestStat(Udata.rows(firstGroupInd), Udata.rows(secondGroupInd));
        }
        else
        {
          firstGroupInd = I.submat(0,j,J(i+1,0),j);
          secondGroupInd = I.submat(J(i+1,0)+1,j,n0-1,j);
          A_EC1 = Udata.rows(firstGroupInd);
          A_EC2 = Udata.rows(secondGroupInd);
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
        firstGroupInd = I.submat(0,j,J(i+1,0),j);
        secondGroupInd = I.submat(J(i+1,0)+1,j,n0-1,j);
        a(j,i) = EqualRankCorrTestStat(Udata.rows(firstGroupInd), Udata.rows(secondGroupInd));
      }
      else
      {
        firstGroupInd = I.submat(0,j,J(i+1,0),j);
        secondGroupInd = I.submat(J(i+1,0)+1,j,n0-1,j);
        A_EC1 = Udata.rows(firstGroupInd);
        A_EC2 = Udata.rows(secondGroupInd);
        a(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
      }
    }
  }
  
  
  // Split at variable SplitVariable[0] and quantile SplitQuantile[0]
  a = abs(a);
  a.max(SplitVariable[0],SplitQuantile[0]);
  
  
  arma::uvec B1;
  arma::uvec B2;
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  
  if ((m>1) && (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction)) //If the dimension of the conditioning set is larger than two, a second split is performed
  {
    unsigned int n1 = J(SplitQuantile[0]+1,0)+1;
    unsigned int n2 = n0 - n1;
    
    arma::uvec R1_1(n1);
    arma::uvec R1_2(n2);
    
    // Get the index sets for both groups (formed by the first split)
    R1_1 = I.submat(0,SplitVariable[0],n1-1,SplitVariable[0]);
    R1_2 = I.submat(n1,SplitVariable[0],n0-1,SplitVariable[0]);
    
    // Declare some variables
    arma::umat I1(n1,m);
    arma::umat J1;
    arma::uvec R1_1_1;
    arma::uvec R1_1_2;
    
    // Prepare the (left) data sets for the second split
    if (!(SplitQuantile[0]==0 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      unsigned int N1;
      if ((SplitQuantile[0]==0 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile[0]==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile[0]==2 && (nDouble< ExpMinSampleSize*16/3/EvaluationDataFraction)))
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
      arma::uvec X1(n1);
      
      for (j=0;j<m;j++)
      {
        colIndex(0) = j;
        X1 = sort_index(Wdata.submat(R1_1,colIndex));
        I1.col(j) = R1_1(X1);
      }
      
      for (j=0;j<m;j++)
      {
        for (i=0;i<N1;i++)
        {
          if (TestType == 0)
          {
            firstGroupInd = I1.submat(0,j,J1(i+1,0),j);
            secondGroupInd = I1.submat(J1(i+1,0)+1,j,n1-1,j);
            a1(j,i) = EqualRankCorrTestStat(Udata.rows(firstGroupInd), Udata.rows(secondGroupInd));
          }
          else
          {
            firstGroupInd = I1.submat(0,j,J1(i+1,0),j);
            secondGroupInd = I1.submat(J1(i+1,0)+1,j,n1-1,j);
            A_EC1 = Udata.rows(firstGroupInd);
            A_EC2 = Udata.rows(secondGroupInd);
            a1(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          }
        }
      }
      
      // Split at variable SplitVariable[1] and quantile SplitQuantile[1]
      a1 = abs(a1);
      a1.max(SplitVariable[1],SplitQuantile[1]);
      
      // Get the (pseudo-)observations for both groups
      unsigned int n3 = J1(SplitQuantile[1]+1,0)+1;
      
      R1_1_1 = I1.submat(0,SplitVariable[1],n3-1,SplitVariable[1]);
      R1_1_2 = I1.submat(n3,SplitVariable[1],n1-1,SplitVariable[1]);
      
    }
    
    // Declare some variables
    arma::umat I2(n2,m);
    arma::umat J2;
    arma::uvec R1_2_1;
    arma::uvec R1_2_2;
    
    // Prepare the (right) data sets for the second split
    if (!(SplitQuantile[0]==2 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      
      unsigned int N2;
      if ((SplitQuantile[0]==2 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile[0]==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile[0]==0 && (nDouble<ExpMinSampleSize*16/3/EvaluationDataFraction)))
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
      arma::uvec X2(n2);
      
      for (j=0;j<m;j++)
      {
        colIndex(0) = j;
        X2 = sort_index(Wdata.submat(R1_2,colIndex));
        I2.col(j) = R1_2(X2);
      }
      
      for (j=0;j<m;j++)
      {
        for (i=0;i<N2;i++)
        {
          if (TestType == 0)
          {
            firstGroupInd = I2.submat(0,j,J2(i+1,0),j);
            secondGroupInd = I2.submat(J2(i+1,0)+1,j,n2-1,j);
            a2(j,i) = EqualRankCorrTestStat(Udata.rows(firstGroupInd), Udata.rows(secondGroupInd));
          }
          else
          {
            firstGroupInd = I2.submat(0,j,J2(i+1,0),j);
            secondGroupInd = I2.submat(J2(i+1,0)+1,j,n2-1,j);
            A_EC1 = Udata.rows(firstGroupInd);
            A_EC2 = Udata.rows(secondGroupInd);
            a2(j,i) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          }
        }
      }
      
      // Split at variable SplitVariable[2] and quantile SplitQuantile[2]
      a2 = abs(a2);
      a2.max(SplitVariable[2],SplitQuantile[2]);
      
      
      // Get the (pseudo-)observations for both groups
      unsigned int n4 = J2(SplitQuantile[2]+1,0)+1;
      
      R1_2_1 = I2.submat(0,SplitVariable[2],n4-1,SplitVariable[2]);
      R1_2_2 = I2.submat(n4,SplitVariable[2],n2-1,SplitVariable[2]);
      
    }
    
    // Final comparison
    if (!(SplitQuantile[0]==0 && n<ExpMinSampleSize*8/EvaluationDataFraction) && !(SplitQuantile[0]==2 && n<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      arma::mat b(6,1);
      if (TestType == 0)
      {
        b(0,0) = EqualRankCorrTestStat(Udata.rows(R1_1_1),Udata.rows(R1_1_2));
        b(1,0) = EqualRankCorrTestStat(Udata.rows(R1_1_1),Udata.rows(R1_2_1));
        b(2,0) = EqualRankCorrTestStat(Udata.rows(R1_1_1),Udata.rows(R1_2_2));
        b(3,0) = EqualRankCorrTestStat(Udata.rows(R1_1_2),Udata.rows(R1_2_1));
        b(4,0) = EqualRankCorrTestStat(Udata.rows(R1_1_2),Udata.rows(R1_2_2));
        b(5,0) = EqualRankCorrTestStat(Udata.rows(R1_2_1),Udata.rows(R1_2_2));
      }
      else
      {
        A_EC1 = Udata.rows(R1_1_1);
        A_EC2 = Udata.rows(R1_1_2);
        b(0,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        A_EC1 = Udata.rows(R1_1_1);
        A_EC2 = Udata.rows(R1_2_1);
        b(1,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        A_EC1 = Udata.rows(R1_1_1);
        A_EC2 = Udata.rows(R1_2_2);
        b(2,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        A_EC1 = Udata.rows(R1_1_2);
        A_EC2 = Udata.rows(R1_2_1);
        b(3,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        A_EC1 = Udata.rows(R1_1_2);
        A_EC2 = Udata.rows(R1_2_2);
        b(4,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        A_EC1 = Udata.rows(R1_2_1);
        A_EC2 = Udata.rows(R1_2_2);
        b(5,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
      }
      
      
      b = abs(b);
      b.max(SplitVariable[3],SplitQuantile[3]);
      
      
      SplitThreshold[0] = arma::as_scalar(Wdata(I(J(SplitQuantile[0]+1),SplitVariable[0]),SplitVariable[0]));
      SplitThreshold[1] = arma::as_scalar(Wdata(I1(J1(SplitQuantile[1]+1),SplitVariable[1]),SplitVariable[1]));
      SplitThreshold[2] = arma::as_scalar(Wdata(I2(J2(SplitQuantile[2]+1),SplitVariable[2]),SplitVariable[2]));
      
      switch(SplitVariable[3]){
        case 0:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) <= SplitThreshold[1] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) > SplitThreshold[1] );
          
          break;
        }
        case 1:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) <= SplitThreshold[1] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) <= SplitThreshold[2] );
          
          break;
        }
        case 2:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) <= SplitThreshold[1] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) > SplitThreshold[2] );
          
          break;
        }
        case 3:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) > SplitThreshold[1] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) <= SplitThreshold[2] );
          
          break;
        }
        case 4:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) > SplitThreshold[1] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) > SplitThreshold[2] );
          
          break;
        }
        case 5:
        {
          B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) <= SplitThreshold[2] );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) > SplitThreshold[2] );
          
          break;
        }
      }
    }
    else
    {
      if (SplitQuantile[0]==0)
      {
        arma::mat b(6,1);
        if (TestType == 0)
        {
          b(0,0) = 0;
          b(1,0) = 0;
          b(2,0) = 0;
          b(3,0) = EqualRankCorrTestStat(Udata.rows(R1_1),Udata.rows(R1_2_1));
          b(4,0) = EqualRankCorrTestStat(Udata.rows(R1_1),Udata.rows(R1_2_2));
          b(5,0) = EqualRankCorrTestStat(Udata.rows(R1_2_1),Udata.rows(R1_2_2));
        }
        else
        {
          b(0,0) = 0;
          b(1,0) = 0;
          b(2,0) = 0;
          A_EC1 = Udata.rows(R1_1);
          A_EC2 = Udata.rows(R1_2_1);
          b(3,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          A_EC1 = Udata.rows(R1_1);
          A_EC2 = Udata.rows(R1_2_2);
          b(4,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          A_EC1 = Udata.rows(R1_2_1);
          A_EC2 = Udata.rows(R1_2_2);
          b(5,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
        }
        
        
        b = abs(b);
        b.max(SplitVariable[3],SplitQuantile[3]);
        SplitVariable[3] = SplitVariable[3] +10;
        
        SplitThreshold[0] = arma::as_scalar(Wdata(I(J(SplitQuantile[0]+1),SplitVariable[0]),SplitVariable[0]));
        SplitThreshold[2] = arma::as_scalar(Wdata(I2(J2(SplitQuantile[2]+1),SplitVariable[2]),SplitVariable[2]));
        
        switch(SplitVariable[3]){
          case 13:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0]);
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) <= SplitThreshold[2] );
            
            break;
          }
          case 14:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0]);
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) > SplitThreshold[2] );
            
            break;
          }
          case 15:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) <= SplitThreshold[2] );
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(2)) > SplitThreshold[2] );
            
            break;
          }
        }
      }
      else
      {
        arma::mat b(6,1);
        if (TestType == 0)
        {
          b(0,0) = EqualRankCorrTestStat(Udata.rows(R1_1_1),Udata.rows(R1_1_2));
          b(1,0) = EqualRankCorrTestStat(Udata.rows(R1_1_1),Udata.rows(R1_2));
          b(2,0) = EqualRankCorrTestStat(Udata.rows(R1_1_2),Udata.rows(R1_2));
          b(3,0) = 0;
          b(4,0) = 0;
          b(5,0) = 0;
        }
        else
        {
          A_EC1 = Udata.rows(R1_1_1);
          A_EC2 = Udata.rows(R1_1_2);
          b(0,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          A_EC1 = Udata.rows(R1_1_1);
          A_EC2 = Udata.rows(R1_2);
          b(1,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          A_EC1 = Udata.rows(R1_1_2);
          A_EC2 = Udata.rows(R1_2);
          b(2,0) = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
          b(3,0) = 0;
          b(4,0) = 0;
          b(5,0) = 0;
        }
        
        
        b = abs(b);
        b.max(SplitVariable[3],SplitQuantile[3]);
        SplitVariable[3] = SplitVariable[3] +10;
        
        
        SplitThreshold[0] = arma::as_scalar(Wdata(I(J(SplitQuantile[0]+1),SplitVariable[0]),SplitVariable[0]));
        SplitThreshold[1] = arma::as_scalar(Wdata(I1(J1(SplitQuantile[1]+1),SplitVariable[1]),SplitVariable[1]));
        
        switch(SplitVariable[3]){
          case 10:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) <= SplitThreshold[1] );
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) > SplitThreshold[1] );
            
            break;
          }
          case 11:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) <= SplitThreshold[1] );
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0]);
            
            break;
          }
          case 12:
          {
            B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0] && Wdata.submat(R2,SplitVariable.col(1)) > SplitThreshold[1] );
            B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0]);
            
            break;
          }
        }
      }
    }
    
  }
  else
  {
    SplitThreshold[0] = arma::as_scalar(Wdata(I(J(SplitQuantile[0]+1),SplitVariable[0]),SplitVariable[0]));
    
    B1 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) <= SplitThreshold[0]);
    B2 = arma::find(Wdata.submat(R2,SplitVariable.col(0)) > SplitThreshold[0]);
    SplitVariable[3] = 6;
  }
  
  
  if (B1.n_elem<2 || B2.n_elem<2)
  {
    throw std::runtime_error("Empty sets after applying tree grouping. It is not recommended to use the tree grouping approach with a low number of observations");
  }
  else
  {
    
    indXdata = R2.elem(B1);
    indYdata = R2.elem(B2);
  }
  
  return;
  
}
