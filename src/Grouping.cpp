#include <pacotest_header.h>

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod)
{
  
  arma::uvec indXData;
  arma::uvec indYData;
  arma::uvec indZData;
  arma::uvec indAData;
  
  
  switch(GroupingMethod){
    case 4: // SumMedian
    {
      arma::mat Wsum = sum(Wdata,1);
      
      double b = arma::as_scalar(median(Wsum));
      
      indXData = arma::find(Wsum < b);
      indYData = arma::find(Wsum >= b );
      
      break;
    }
    case 5: // SumThirdsI
    {
      arma::mat Wsum = sum(Wdata,1);
      arma::mat WsumSorted = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(WsumSorted(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(WsumSorted(ceil(2* (double)Wsum.n_elem /3)));
      
      indXData = arma::find(Wsum < b1_3);
      indYData = arma::find(Wsum >= b1_3 && Wsum  < b2_3);
      indZData = arma::find(Wsum >= b2_3);
      
      nObsPerVector.set_size(3);
      indexVectors.set_size(indexVectors.n_rows,3);
      nObsPerVector(2) = indYData.n_elem;
      indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indZData;
      break;
    }
    case 6: // SumThirdsII
    {
      arma::mat Wsum = sum(Wdata,1);
      arma::mat WsumSorted = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(WsumSorted(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(WsumSorted(ceil(2* (double)Wsum.n_elem /3)));
      
      indXData = arma::find(Wsum < b1_3);
      indYData = arma::find(Wsum >= b1_3 && Wsum  < b2_3);
      break;
    }
    case 7: // SumThirdsIII
    {
      arma::mat Wsum = sum(Wdata,1);
      arma::mat WsumSorted = Wsum(sort_index(Wsum));
      
      double b1_3 = arma::as_scalar(WsumSorted(ceil((double)Wsum.n_elem /3)));
      double b2_3 = arma::as_scalar(WsumSorted(ceil(2* (double)Wsum.n_elem /3)));
      
      indXData = arma::find(Wsum < b1_3);
      indYData = arma::find(Wsum >= b2_3);
      break;
    }
    case 8: // SumQuartiles
    {
      arma::mat Wsum = sum(Wdata,1);
      arma::mat WsumSorted = Wsum(sort_index(Wsum));
      
      double b1_4 = arma::as_scalar(WsumSorted(ceil((double)Wsum.n_elem /4)));
      double b1_2 = arma::as_scalar(WsumSorted(ceil((double)Wsum.n_elem /2)));
      double b3_4 = arma::as_scalar(WsumSorted(ceil(3* (double)Wsum.n_elem /4)));
      
      indXData = arma::find(Wsum < b1_4);
      indYData = arma::find(Wsum >= b1_4 && Wsum  < b1_2);
      indZData = arma::find(Wsum >= b1_2 && Wsum  < b3_4);
      indAData = arma::find(Wsum >= b3_4);
      
      nObsPerVector.set_size(4);
      indexVectors.set_size(indexVectors.n_rows,4);
      
      nObsPerVector(2) = indZData.n_elem;
      indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indZData;
      
      nObsPerVector(3) = indAData.n_elem;
      indexVectors.submat(0,3,nObsPerVector(3)-1,3) = indAData;
      break;
    }
    case 9: // ProdMedian
    {
      arma::mat Wprod = prod(Wdata,1);
      
      double b = arma::as_scalar(median(Wprod));
      
      indXData = arma::find(Wprod < b);
      indYData = arma::find(Wprod >= b );
      break;
    }
    case 10: // ProdThirdsI
    {
      arma::mat Wprod = prod(Wdata,1);
      arma::mat WprodSorted = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(WprodSorted(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(WprodSorted(ceil(2* (double)Wprod.n_elem /3)));
      
      indXData = arma::find(Wprod < b1_3);
      indYData = arma::find(Wprod >= b1_3 && Wprod  < b2_3);
      indZData = arma::find(Wprod >= b2_3);
      
      nObsPerVector.set_size(3);
      indexVectors.set_size(indexVectors.n_rows,3);
      nObsPerVector(2) = indZData.n_elem;
      indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indZData;
      break;
    }
    case 11: // ProdThirdsII
    {
      arma::mat Wprod = prod(Wdata,1);
      arma::mat WprodSorted = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(WprodSorted(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(WprodSorted(ceil(2* (double)Wprod.n_elem /3)));
      
      indXData = arma::find(Wprod < b1_3);
      indYData = arma::find(Wprod >= b1_3 && Wprod  < b2_3);
      break;
    }
    case 12: // ProdThirdsIII
    {
      arma::mat Wprod = prod(Wdata,1);
      arma::mat WprodSorted = Wprod(sort_index(Wprod));
      
      double b1_3 = arma::as_scalar(WprodSorted(ceil((double)Wprod.n_elem /3)));
      double b2_3 = arma::as_scalar(WprodSorted(ceil(2* (double)Wprod.n_elem /3)));
      
      indXData = arma::find(Wprod < b1_3);
      indYData = arma::find(Wprod >= b2_3);
      break;
    }
    case 13: // ProdQuartiles
    {
      arma::mat Wprod = prod(Wdata,1);
      arma::mat WprodSorted = Wprod(sort_index(Wprod));
      
      double b1_4 = arma::as_scalar(WprodSorted(ceil((double)Wprod.n_elem /4)));
      double b1_2 = arma::as_scalar(WprodSorted(ceil((double)Wprod.n_elem /2)));
      double b3_4 = arma::as_scalar(WprodSorted(ceil(3* (double)Wprod.n_elem /4)));
      
      indXData = arma::find(Wprod < b1_4);
      indYData = arma::find(Wprod >= b1_4 && Wprod  < b1_2);
      indZData = arma::find(Wprod >= b1_2 && Wprod  < b3_4);
      indAData = arma::find(Wprod >= b3_4);
      
      nObsPerVector.set_size(4);
      indexVectors.set_size(indexVectors.n_rows,4);
      
      nObsPerVector(2) = indZData.n_elem;
      indexVectors.submat(0,2,nObsPerVector(2)-1,2) = indZData;
      
      nObsPerVector(3) = indAData.n_elem;
      indexVectors.submat(0,3,nObsPerVector(3)-1,3) = indAData;
      break;
    }
    
  }
  
  
  nObsPerVector(0) = indXData.n_elem;
  indexVectors.submat(0,0,nObsPerVector(0)-1,0) = indXData;
  
  nObsPerVector(1) = indYData.n_elem;
  indexVectors.submat(0,1,nObsPerVector(1)-1,1) = indYData;
  
  
  
}


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::mat &Xdata, arma::mat &Ydata, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  unsigned int n=Udata.n_rows;
  arma::umat indexVectors(n,2);
  arma::uvec nObsPerVector(2);
  indexVectors.zeros();
  nObsPerVector.zeros();
  
  Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
  
  Xdata = Udata.submat(indexVectors.submat(0,0,nObsPerVector(0)-1,0), Cols);
  Ydata = Udata.submat(indexVectors.submat(0,1,nObsPerVector(1)-1,1), Cols);
  
  return;
  
}


void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  
  switch(GroupingMethod){
    case 1: // TreeECOV
    case 2: // TreeCCC
    case 3: // TreeEC
    {
      TreeGrouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold);
      break;
    }
    default:
    {
      Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod);
      break;
    }
    
  }
}

void Grouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int GroupingMethod, int finalComparisonMethod, double ExpMinSampleSize, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  
  switch(GroupingMethod){
    case 1: // TreeECOV
    case 2: // TreeCCC
    case 3: // TreeEC
    {
      double TrainingDataFraction = 1;
      int withTraining = 0;
      TreeGrouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, withTraining);
      break;
    }
    default:
    {
      Grouping(Udata, Wdata, indexVectors, nObsPerVector, GroupingMethod);
      break;
    }
    
  }
}

double splitTestStat(const arma::mat &Udata, int splitTestType, arma::umat &ind, arma::uvec &nObsPerGroup)
{
  
  double testStat;
  arma::mat A_EC1;
  arma::mat A_EC2;
  
  if (splitTestType == 1 )
  {
    testStat = EqualCovChi2TestStat(Udata, ind, nObsPerGroup);
  }
  else if (splitTestType == 2 )
  {
    testStat = EqualCorrChi2TestStat(Udata, ind, nObsPerGroup);
  }
  else
  {
    arma::uvec firstGroupInd = ind.submat(0,0,nObsPerGroup(0)-1,0);
    arma::uvec secondGroupInd = ind.submat(0,1,nObsPerGroup(1)-1,1);
    A_EC1 = Udata.rows(firstGroupInd);
    A_EC2 = Udata.rows(secondGroupInd);
    testStat = EqualCopTestStat(A_EC1.begin(),A_EC2.begin(),A_EC1.n_rows,A_EC2.n_rows);
  }
  
  return testStat;
  
}

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold)
{
  int withTraining = 1;
  TreeGrouping(Udata, Wdata, indexVectors, nObsPerVector, splitTestType, finalComparisonMethod, ExpMinSampleSize, TrainingDataFraction, SplitVariable, SplitQuantile, SplitThreshold, withTraining);
}

void TreeGrouping(const arma::mat &Udata, const arma::mat &Wdata, arma::umat &indexVectors, arma::uvec &nObsPerVector, int splitTestType, int finalComparisonMethod, double ExpMinSampleSize, double TrainingDataFraction, arma::uvec &SplitVariable, arma::uvec &SplitQuantile, arma::vec &SplitThreshold, int withTraining)
{
  double EvaluationDataFraction;
  
  
  if (withTraining == 1)
  {
    EvaluationDataFraction = 1-TrainingDataFraction;
  }
  else
  {
    EvaluationDataFraction = 1;
  }
  
  unsigned int m;
  
  m = Wdata.n_cols;
  
  unsigned int n=Wdata.n_rows;
  double nDouble = (double) n;
  
  unsigned  int j,i;
  
  // Initialize some variables needed to transfer the index vectors and (sub)data to the functions computing test statistics
  arma::umat ind(n,2);
  arma::uvec nObsPerGroup(2);
  ind.fill(UINT_MAX);
  nObsPerGroup.zeros();
  
  // Split the dataset randomly into two pices
  arma::uvec R(n);
  R = arma::linspace<arma::uvec>(0,n-1,n);
  RandPerm(R); // RandPerm is defined in a way that it doesn't matter how it is initialized
  
  unsigned int n0;
  arma::uvec R1;
  arma::uvec R2;
  
  if (withTraining == 1)
  {
    n0 = floor(n*TrainingDataFraction);
    // Training data
    R1 = R.subvec(0,n0-1);
    
    // Evaluation data
    R2 = R.subvec(n0,n-1);
  }
  else
  {
    n0 = n;
    // Training data
    R1 = R;
    
    // Evaluation data
    R2 = R;
  }
  
  double n0Double = (double) n0;
  
  
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
  J(1,0) = floor(n0Double/4)-1;
  J(2,0) = floor(n0Double/2)-1;
  J(3,0) = floor(3*n0Double/4)-1;
  
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
      ind.submat(0,0,J(i+1,0),0) = I.submat(0,j,J(i+1,0),j);
      ind.submat(0,1,n0-J(i+1,0)-2,1) = I.submat(J(i+1,0)+1,j,n0-1,j);
      nObsPerGroup(0) = J(i+1,0)+1;
      nObsPerGroup(1) = n0-J(i+1,0)-1;
      
      a(j,i) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
      ind.fill(UINT_MAX);
      
    }
  }
  
  
  // Split at variable SplitVariable(0) and quantile SplitQuantile(0)
  a = abs(a);
  a.max(SplitVariable(0),SplitQuantile(0));
  
  // Obtain the corresponding split threshold
  SplitThreshold(0) = arma::as_scalar(Wdata(I(J(SplitQuantile(0)+1),SplitVariable(0)),SplitVariable(0)));
  
  
  arma::uvec B1;
  arma::uvec B2;
  arma::uvec B3;
  arma::uvec B4;
  arma::uvec Cols(2);
  
  Cols(0) =0;
  Cols(1) =1;
  
  
  if (nDouble>=ExpMinSampleSize*4/EvaluationDataFraction) // Check whether a second split should be done
  {
    unsigned int n1 = J(SplitQuantile(0)+1,0)+1;
    unsigned int n2 = n0 - n1;
    
    double n1Double = (double) n1;
    double n2Double = (double) n2;
    
    arma::uvec R1_1(n1);
    arma::uvec R1_2(n2);
    
    // Get the index sets for both groups (formed by the first split)
    R1_1 = I.submat(0,SplitVariable(0),n1-1,SplitVariable(0));
    R1_2 = I.submat(n1,SplitVariable(0),n0-1,SplitVariable(0));
    
    // Declare some variables
    arma::umat I1(n1,m);
    arma::umat J1(4,1);
    arma::uvec R1_1_1;
    arma::uvec R1_1_2;
    
    // Prepare the (left) data sets for the second split
    if (!(SplitQuantile(0)==0 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      if ((SplitQuantile(0)==0 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile(0)==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile(0)==2 && (nDouble< ExpMinSampleSize*16/3/EvaluationDataFraction)))
      {
        iStart = 1;
        iEnd = 2;
      }
      else
      {
        iStart = 0;
        iEnd = 3;
      }
      
      
      J1(0,0) = 0;
      J1(1,0) = floor(n1Double/4)-1;
      J1(2,0) = floor(n1Double/2)-1;
      J1(3,0) = floor(3*n1Double/4)-1;
      
      // Learn the second split
      arma::mat a1(m,3);
      a1.zeros();
      arma::uvec X1(n1);
      
      for (j=0;j<m;j++)
      {
        colIndex(0) = j;
        X1 = sort_index(Wdata.submat(R1_1,colIndex));
        I1.col(j) = R1_1(X1);
      }
      
      for (j=0;j<m;j++)
      {
        for (i=iStart;i<iEnd;i++)
        {
          ind.submat(0,0,J1(i+1,0),0) = I1.submat(0,j,J1(i+1,0),j);
          ind.submat(0,1,n1-J1(i+1,0)-2,1) = I1.submat(J1(i+1,0)+1,j,n1-1,j);
          nObsPerGroup(0) = J1(i+1,0)+1;
          nObsPerGroup(1) = n1-J1(i+1,0)-1;
          
          a1(j,i) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.fill(UINT_MAX);
          
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
    arma::umat J2(4,1);
    arma::uvec R1_2_1;
    arma::uvec R1_2_2;
    
    // Prepare the (right) data sets for the second split
    if (!(SplitQuantile(0)==2 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction))
    {
      if ((SplitQuantile(0)==2 && nDouble<ExpMinSampleSize*16/EvaluationDataFraction) || (SplitQuantile(0)==1 && nDouble<ExpMinSampleSize*8/EvaluationDataFraction) || (SplitQuantile(0)==0 && (nDouble<ExpMinSampleSize*16/3/EvaluationDataFraction)))
      {
        iStart = 1;
        iEnd = 2;
      }
      else
      {
        iStart = 0;
        iEnd = 3;
      }
      
      J2(0,0) = 0;
      J2(1,0) = floor(n2Double/4)-1;
      J2(2,0) = floor(n2Double/2)-1;
      J2(3,0) = floor(3*n2Double/4)-1;
      
      // Learn the second split
      arma::mat a2(m,3);
      a2.zeros();
      arma::uvec X2(n2);
      
      for (j=0;j<m;j++)
      {
        colIndex(0) = j;
        X2 = sort_index(Wdata.submat(R1_2,colIndex));
        I2.col(j) = R1_2(X2);
      }
      
      for (j=0;j<m;j++)
      {
        for (i=iStart;i<iEnd;i++)
        {
          ind.submat(0,0,J2(i+1,0),0) = I2.submat(0,j,J2(i+1,0),j);
          ind.submat(0,1,n2-J2(i+1,0)-2,1) = I2.submat(J2(i+1,0)+1,j,n2-1,j);
          nObsPerGroup(0) = J2(i+1,0)+1;
          nObsPerGroup(1) = n2-J2(i+1,0)-1;
          
          a2(j,i) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.fill(UINT_MAX);
          
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
    if (finalComparisonMethod==1)
    {
      arma::mat b(6,1);
      b.zeros();
      
      if (!(R1_1_1.is_empty() || R1_1_2.is_empty() || R1_2_1.is_empty() || R1_2_2.is_empty()))
      {
        nObsPerGroup(0) = R1_1_1.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_1_1;
        nObsPerGroup(1) = R1_1_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_1_2;
        b(0,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.col(1).fill(UINT_MAX);
        
        nObsPerGroup(1) = R1_2_1.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_1;
        b(1,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.col(1).fill(UINT_MAX);
        
        nObsPerGroup(1) = R1_2_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_2;
        b(2,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.fill(UINT_MAX);
        
        
        nObsPerGroup(0) = R1_1_2.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_1_2;
        nObsPerGroup(1) = R1_2_1.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_1;
        b(3,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.col(1).fill(UINT_MAX);
        
        nObsPerGroup(1) = R1_2_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_2;
        b(4,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.col(0).fill(UINT_MAX);
        
        nObsPerGroup(0) = R1_2_1.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_2_1;
        b(5,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
        ind.fill(UINT_MAX);
        
        b = abs(b);
        b.max(SplitVariable(3),SplitQuantile(3));
        
      }
      else
      {
        if (R1_1_1.is_empty() && R1_1_2.is_empty())
        {
          nObsPerGroup(0) = R1_1.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_1;
          nObsPerGroup(1) = R1_2_1.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_1;
          b(3,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.col(1).fill(UINT_MAX);
          
          nObsPerGroup(1) = R1_2_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2_2;
          b(4,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.col(0).fill(UINT_MAX);
          
          nObsPerGroup(0) = R1_2_1.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_2_1;
          b(5,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.fill(UINT_MAX);
        }
        
        if (R1_2_1.is_empty() && R1_2_2.is_empty())
        {
          nObsPerGroup(0) = R1_1_1.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_1_1;
          nObsPerGroup(1) = R1_1_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_1_2;
          b(0,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.col(1).fill(UINT_MAX);
          
          nObsPerGroup(1) = R1_2.n_elem; ind.submat(0,1,nObsPerGroup(1)-1,1) =  R1_2;
          b(1,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.col(0).fill(UINT_MAX);
          
          nObsPerGroup(0) = R1_1_2.n_elem; ind.submat(0,0,nObsPerGroup(0)-1,0) =  R1_1_2;
          b(2,0) = splitTestStat(Udata, splitTestType, ind, nObsPerGroup);
          ind.fill(UINT_MAX);
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
      // Needs to be implemented after allowing for more than two groups as output from the decision tree based gourping
      
      if (!(R1_1_1.is_empty() || R1_1_2.is_empty() || R1_2_1.is_empty() || R1_2_2.is_empty()))
      {
        // The four group case
        B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
        B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
        B3 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
        B4 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
        if (B3.n_elem<2 || B4.n_elem<2)
        {
          throw std::runtime_error("Empty sets after applying tree grouping. It is not recommended to use the tree grouping approach with a low number of observations");
        }
        indexVectors.set_size(n,4);
        nObsPerVector.set_size(4);
        
        // Set fourth indicator for correct export
        SplitVariable(3) = 0;
      }
      else
      {
        if (R1_1_1.is_empty() && R1_1_2.is_empty())
        {
          // The three group case with a split on the right
          B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0));
          B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) <= SplitThreshold(2) );
          B3 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(2,2)) > SplitThreshold(2) );
          if (B3.n_elem<2)
          {
            throw std::runtime_error("Empty sets after applying tree grouping. It is not recommended to use the tree grouping approach with a low number of observations");
          }
          indexVectors.set_size(n,3);
          nObsPerVector.set_size(3);
          
          // Set fourth indicator for correct export
          SplitVariable(3) = 13;
        }
        
        if (R1_2_1.is_empty() && R1_2_2.is_empty())
        {
          // The three group case with a split on the left
          B1 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) <= SplitThreshold(1) );
          B2 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) <= SplitThreshold(0) && Wdata.submat(R2,SplitVariable.subvec(1,1)) > SplitThreshold(1) );
          B3 = arma::find(Wdata.submat(R2,SplitVariable.subvec(0,0)) > SplitThreshold(0));
          if (B3.n_elem<2)
          {
            throw std::runtime_error("Empty sets after applying tree grouping. It is not recommended to use the tree grouping approach with a low number of observations");
          }
          indexVectors.set_size(n,3);
          nObsPerVector.set_size(3);
          
          // Set fourth indicator for correct export
          SplitVariable(3) = 10;
        }
        
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
    
    nObsPerVector(0) = B1.n_elem;
    indexVectors.submat(0,0,nObsPerVector(0)-1,0) = R2.elem(B1);
    
    nObsPerVector(1) = B2.n_elem;
    indexVectors.submat(0,1,nObsPerVector(1)-1,1) = R2.elem(B2);
    
    if (B3.n_elem>1)
    {
      nObsPerVector(2) = B3.n_elem;
      indexVectors.submat(0,2,nObsPerVector(2)-1,2) = R2.elem(B3);
    }
    
    if (B4.n_elem>1)
    {
      nObsPerVector(3) = B4.n_elem;
      indexVectors.submat(0,3,nObsPerVector(3)-1,3) = R2.elem(B4);
    }
  }
  
  return;
  
}
