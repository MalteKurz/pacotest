#include <pacotest_header.h>

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod)
{
  
  switch(GroupingMethod){
    case 5: // SumMedian
    {
      arma::mat Wsum = sum(Wdata,1);
      
      double b = arma::as_scalar(median(Wsum));
      
      indXdata = arma::find(Wsum < b);
      indYdata = arma::find(Wsum >= b );
      break;
    }
    case 6: // SumThirdsI
    {
      arma::mat Wsum = sum(Wdata,1);
      Wsum = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
      
      indXdata = arma::find(Wsum < b1_3);
      indYdata = arma::find(Wsum >= b1_3 && Wsum  < b2_3);
      
      break;
    }
    case 7: // SumThirdsII
    {
      arma::mat Wsum = sum(Wdata,1);
      Wsum = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(Wsum(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(Wsum(ceil(2* (double)Wsum.n_elem /3)));
      
      indXdata = arma::find(Wsum < b1_3);
      indYdata = arma::find(Wsum >= b2_3);
      
      break;
    }
    case 8: // ProdMedian
    {
      arma::mat Wprod = prod(Wdata,1);
      
      double b = arma::as_scalar(median(Wprod));
      
      indXdata = arma::find(Wprod < b);
      indYdata = arma::find(Wprod >= b );
      break;
    }
    case 9: // ProdThirdsI
    {
      arma::mat Wprod = prod(Wdata,1);
      Wprod = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(Wprod(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(Wprod(ceil(2* (double)Wprod.n_elem /3)));
      
      indXdata = arma::find(Wprod < b1_3);
      indYdata = arma::find(Wprod >= b1_3 && Wprod  < b2_3);
      
      break;
    }
    case 10: // ProdThirdsII
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


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  arma::uvec indXdata;
  arma::uvec indYdata;
  
  Grouping(Udata, Wdata, indXdata, indYdata, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  Xdata = Udata.submat(indXdata, Cols);
  Ydata = Udata.submat(indYdata, Cols);
  
  return;
  
}


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  
  switch(GroupingMethod){
    case 1: // TreeERC
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 0, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    case 2: // TreeERCchi2
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 2, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    case 3: // TreeERCchi2WithEstimation
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 2, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    case 4: // TreeEC
    {
      TreeGrouping(Udata, Wdata, indXdata, indYdata, 1, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    default:
    {
      Grouping(Udata, Wdata, indXdata, indYdata, GroupingMethod);
      break;
    }
    
  }
}

double splitTestStat(const arma::mat &Udata, int splitTestType, int nGroups, unsigned int *ptrOnIndexVectors[], arma::uvec &nObsPerGroup)
{
  
  double testStat;
  arma::mat A_EC1;
  arma::mat A_EC2;
  
  if (splitTestType == 0 || splitTestType == 1)
  {
  arma::uvec firstGroupInd(ptrOnIndexVectors[0], nObsPerGroup(0), false);
  arma::uvec secondGroupInd(ptrOnIndexVectors[1], nObsPerGroup(1), false);
  if (splitTestType == 0)
  {
    testStat = EqualRankCorrTestStat(Udata.rows(firstGroupInd), Udata.rows(secondGroupInd));
  }
  else
  {
    A_EC1 = Udata.rows(firstGroupInd);
    A_EC2 = Udata.rows(secondGroupInd);
    testStat = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
  }
  }
  else
  {
    if (splitTestType == 2)
    {
      testStat = EqualRankCorrChi2TestStat(Udata, nGroups, ptrOnIndexVectors, nObsPerGroup);
    }
  }
  
  
  
  return testStat;
  
}


void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::uvec &indXdata, arma::uvec &indYdata, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  double EvaluationDataFraction = 1-TrainingDataFraction;
  
  unsigned int m;
  
  m = Wdata.n_cols;
  
  unsigned int n=Wdata.n_rows;
  double nDouble = (double) n;
  
  unsigned  int j,i;
  
  // Initialize some variables needed to transfer the index vectors and (sub)data to the functions computing test statistics
  const int maxNGroups = 4;
  int nGroups;
  unsigned int *ptrOnIndexVectors[maxNGroups];
  arma::uvec nObsPerGroup(maxNGroups);
  
  
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
  
  // Some helping matrices
  arma::mat A_EC1;
  arma::mat A_EC2;
  
  
  // Learn the first split
  arma::umat J(4,1);
  arma::mat a(m,3);
  a.zeros();
  arma::umat I(n0,m);
  arma::uvec firstGroupInd;
  arma::uvec secondGroupInd;
  
  arma::uvec X(n0);
  arma::uvec colIndex(1);
  for (j=0;j<m;j++)
  {
    colIndex(0) = j;
    X = sort_index(Wdata.submat(R1,colIndex));
    I.col(j) = R1(X);
  }
  
  
  J(0,0) = 0;
  J(1,0) = floor(n0/4)-1;
  J(2,0) = floor(n0/2)-1;
  J(3,0) = floor(3*n0/4)-1;
  
  unsigned int iStart;
  unsigned int iEnd;
  
  if (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction) // Use the 0.25 and 0.75 quantile only for more than 8*ExpMinSampleSize observations
  {
    iStart = 0;
    iEnd = 3;
  }
  else
  {
    iStart = 1;
    iEnd = 2;
  }
  
  
  for (j=0;j<m;j++)
  {
    for (i=iStart;i<iEnd;i++)
    {
      firstGroupInd = I.submat(0,j,J(i+1,0),j);
      secondGroupInd = I.submat(J(i+1,0)+1,j,n0-1,j);
      nGroups = 2;
      ptrOnIndexVectors[0] = firstGroupInd.memptr();
      ptrOnIndexVectors[1] = secondGroupInd.memptr();
      nObsPerGroup(0) = firstGroupInd.n_elem;
      nObsPerGroup(1) = secondGroupInd.n_elem;
      
      a(j,i) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
    }
  }
  
  
  // Split at variable SplitVariable(0) and quantile SplitQuantile(0)
  a = abs(a);
  a.max(SplitVariable(0),SplitQuantile(0));
  
  // Obtain the corresponding split threshold
  SplitThreshold(0) = arma::as_scalar(Wdata(I(J(SplitQuantile(0)+1),SplitVariable(0)),SplitVariable(0)));
  
  
  arma::uvec B1;
  arma::uvec B2;
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  
  if ((m>1) && (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction)) //If the dimension of the conditioning set is larger than two, a second split is performed
  {
    unsigned int n1 = J(SplitQuantile(0)+1,0)+1;
    unsigned int n2 = n0 - n1;
    
    arma::uvec R1_1(n1);
    arma::uvec R1_2(n2);
    
    // Get the index sets for both groups (formed by the first split)
    R1_1 = I.submat(0,SplitVariable(0),n1-1,SplitVariable(0));
    R1_2 = I.submat(n1,SplitVariable(0),n0-1,SplitVariable(0));
    
    // Declare some variables
    arma::umat I1(n1,m);
    arma::umat J1;
    arma::uvec R1_1_1;
    arma::uvec R1_1_2;
    
    // Prepare the (left) data sets for the second split
    if (!(SplitQuantile(0)==0 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      unsigned int N1;
      if ((SplitQuantile(0)==0 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile(0)==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile(0)==2 && (nDouble< ExpMinSampleSize*16/3/EvaluationDataFraction)))
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
          firstGroupInd = I1.submat(0,j,J1(i+1,0),j);
          secondGroupInd = I1.submat(J1(i+1,0)+1,j,n1-1,j);
          nGroups = 2;
          ptrOnIndexVectors[0] = firstGroupInd.memptr();
          ptrOnIndexVectors[1] = secondGroupInd.memptr();
          nObsPerGroup(0) = firstGroupInd.n_elem;
          nObsPerGroup(1) = secondGroupInd.n_elem;
          
          a1(j,i) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
          
        }
      }
      
      // Split at variable SplitVariable(1) and quantile SplitQuantile(1)
      a1 = abs(a1);
      a1.max(SplitVariable(1),SplitQuantile(1));
      
      // Obtain the corresponding split threshold
      SplitThreshold(1) = arma::as_scalar(Wdata(I1(J1(SplitQuantile(1)+1),SplitVariable(1)),SplitVariable(1)));
      
      // Get the (pseudo-)observations for both groups
      unsigned int n3 = J1(SplitQuantile(1)+1,0)+1;
      
      R1_1_1 = I1.submat(0,SplitVariable(1),n3-1,SplitVariable(1));
      R1_1_2 = I1.submat(n3,SplitVariable(1),n1-1,SplitVariable(1));
      
    }
    
    // Declare some variables
    arma::umat I2(n2,m);
    arma::umat J2;
    arma::uvec R1_2_1;
    arma::uvec R1_2_2;
    
    // Prepare the (right) data sets for the second split
    if (!(SplitQuantile(0)==2 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      
      unsigned int N2;
      if ((SplitQuantile(0)==2 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile(0)==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile(0)==0 && (nDouble<ExpMinSampleSize*16/3/EvaluationDataFraction)))
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
          firstGroupInd = I2.submat(0,j,J2(i+1,0),j);
          secondGroupInd = I2.submat(J2(i+1,0)+1,j,n2-1,j);
          nGroups = 2;
          ptrOnIndexVectors[0] = firstGroupInd.memptr();
          ptrOnIndexVectors[1] = secondGroupInd.memptr();
          nObsPerGroup(0) = firstGroupInd.n_elem;
          nObsPerGroup(1) = secondGroupInd.n_elem;
          
          a2(j,i) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
          
        }
      }
      
      // Split at variable SplitVariable(2) and quantile SplitQuantile(2)
      a2 = abs(a2);
      a2.max(SplitVariable(2),SplitQuantile(2));
      
      // Obtain the corresponding split threshold
      SplitThreshold(2) = arma::as_scalar(Wdata(I2(J2(SplitQuantile(2)+1),SplitVariable(2)),SplitVariable(2)));
      
      // Get the (pseudo-)observations for both groups
      unsigned int n4 = J2(SplitQuantile(2)+1,0)+1;
      
      R1_2_1 = I2.submat(0,SplitVariable(2),n4-1,SplitVariable(2));
      R1_2_2 = I2.submat(n4,SplitVariable(2),n2-1,SplitVariable(2));
      
    }
    
    
    // Final comparison
    arma::mat b(6,1);
    b.zeros();
    nGroups = 2;
    
    if (!(R1_1_1.is_empty() || R1_1_2.is_empty() || R1_2_1.is_empty() || R1_2_2.is_empty()))
    {
      ptrOnIndexVectors[0] = R1_1_1.memptr(); nObsPerGroup(0) = R1_1_1.n_elem;
      ptrOnIndexVectors[1] = R1_1_2.memptr(); nObsPerGroup(1) = R1_1_2.n_elem;
      b(0,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      ptrOnIndexVectors[1] = R1_2_1.memptr(); nObsPerGroup(1) = R1_2_1.n_elem;
      b(1,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      ptrOnIndexVectors[1] = R1_2_2.memptr(); nObsPerGroup(1) = R1_2_2.n_elem;
      b(2,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      
      ptrOnIndexVectors[0] = R1_1_2.memptr(); nObsPerGroup(0) = R1_1_2.n_elem;
      ptrOnIndexVectors[1] = R1_2_1.memptr(); nObsPerGroup(1) = R1_2_1.n_elem;
      b(3,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      ptrOnIndexVectors[1] = R1_2_2.memptr(); nObsPerGroup(1) = R1_2_2.n_elem;
      b(4,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      ptrOnIndexVectors[0] = R1_2_1.memptr(); nObsPerGroup(0) = R1_2_1.n_elem;
      b(5,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      
      b = abs(b);
      b.max(SplitVariable(3),SplitQuantile(3));
      
    }
    else
    {
      if (R1_1_1.is_empty() && R1_1_2.is_empty())
      {
        ptrOnIndexVectors[0] = R1_1.memptr(); nObsPerGroup(0) = R1_1.n_elem;
        ptrOnIndexVectors[1] = R1_2_1.memptr(); nObsPerGroup(1) = R1_2_1.n_elem;
        b(3,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
        
        ptrOnIndexVectors[1] = R1_2_2.memptr(); nObsPerGroup(1) = R1_2_2.n_elem;
        b(4,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
        
        ptrOnIndexVectors[0] = R1_2_1.memptr(); nObsPerGroup(0) = R1_2_1.n_elem;
        b(5,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      }
      
      if (R1_2_1.is_empty() && R1_2_2.is_empty())
      {
        ptrOnIndexVectors[0] = R1_1_1.memptr(); nObsPerGroup(0) = R1_1_1.n_elem;
        ptrOnIndexVectors[1] = R1_1_2.memptr(); nObsPerGroup(1) = R1_1_2.n_elem;
        b(0,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
        
        ptrOnIndexVectors[1] = R1_2.memptr(); nObsPerGroup(1) = R1_2.n_elem;
        b(1,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
        
        ptrOnIndexVectors[0] = R1_1_2.memptr(); nObsPerGroup(0) = R1_1_2.n_elem;
        b(2,0) = splitTestStat(Udata, splitTestType, nGroups, ptrOnIndexVectors, nObsPerGroup);
      }
      
      b = abs(b);
      b.max(SplitVariable(3),SplitQuantile(3));
      SplitVariable(3) = SplitVariable(3) +10;
      
    }
    
    switch(SplitVariable(3)){
      case 0:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        
        break;
      }
      case 1:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        
        break;
      }
      case 2:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        
        break;
      }
      case 3:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        
        break;
      }
      case 4:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        
        break;
      }
      case 5:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        
        break;
      }
      case 13:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0));
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        
        break;
      }
      case 14:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0));
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        
        break;
      }
      case 15:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        
        break;
      }
      case 10:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        
        break;
      }
      case 11:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0));
        
        break;
      }
      case 12:
      {
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0));
        
        break;
      }
    }
    
  }
  else
  {
    
    B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0));
    B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0));
    SplitVariable(3) = 6;
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
