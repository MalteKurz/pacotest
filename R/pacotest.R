pacotest = function(Udata,W,pacotestOptions, data = NULL, svcmDataFrame = NULL, cPitData = NULL){
  
  pacotestOptions = pacotestset(pacotestOptions)
  
  if (!is.data.frame(Udata))
  {
    Udata = as.data.frame(Udata)
  }
  
  if (!is.data.frame(W))
  {
    W = as.data.frame(W)
  }
  
  # Add aggregated information to the conditioning vector
  W = addAggInfo(W,pacotestOptions$aggInfo, pacotestOptions$sizeKeepingMethod)
  
  if (pacotestOptions$testType=='ECOV' || pacotestOptions$testType=='ECORR')
  {
    grouping = partitionToNumber(pacotestOptions$grouping)
    testTypeNumber = testTypeToNumber(pacotestOptions$testType)
    
    if (!(pacotestOptions$withEstUncert))
    {
      data = matrix()
      svcmDataFrame = data.frame()
      cPitData = matrix()
    }
    
    if (grouping<3  && pacotestOptions$sizeKeepingMethod=='splitTrainEvaluate' && pacotestOptions$aggPvalsNumbRep > 1)
    {
      finalComparison = which(pacotestOptions$finalComparison==c('pairwiseMax','all'))
      
      out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$aggPvalsNumbRep,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
      
    }
    else
    {
      if (grouping > 3)
      {
        out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, 1, data, svcmDataFrame, cPitData, 0, 1, 1)
        
      }
      else
      {
        finalComparison = which(pacotestOptions$finalComparison==c('pairwiseMax','all'))
        
        if (pacotestOptions$sizeKeepingMethod=='splitTrainEvaluate')
        {
          out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,0,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
          
        }
        else if (pacotestOptions$sizeKeepingMethod=='penalty')
        {
          gamma0Partition = partitionToNumber(pacotestOptions$gamma0Partition)
          
          out = ecorrOrEcovWithPenalty(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$expMinSampleSize, pacotestOptions$penaltyParams[1], pacotestOptions$penaltyParams[2], gamma0Partition)
          
        }
        
      }
    }
  }
  else if (pacotestOptions$testType=='EC')
  {
    grouping = partitionToNumber(pacotestOptions$grouping)
    if (grouping > 2)
    {
      out = EC(as.matrix(Udata), as.matrix(W),pacotestOptions$numbBoot,grouping,1,1,1)
      
    }
    else
    {
      finalComparison = which(pacotestOptions$finalComparison==c('pairwiseMax','all'))
      
      out = EC(as.matrix(Udata), as.matrix(W),pacotestOptions$numbBoot,grouping,finalComparison,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
      
    }
    
  }
  else if (pacotestOptions$testType=='VI')
  {
    out = VI(as.matrix(Udata), as.matrix(W),pacotestOptions$numbBoot)
  }
  else
  {
    stop('Unknown testType')
  }
  
  # Export/generate the decision tree and the illustrative plots
  if (pacotestOptions$testType=='ECOV' || pacotestOptions$testType=='ECORR' || pacotestOptions$testType=='EC')
  {
    # Extract decision tree(s)
    if (grouping<=2)
    {
      condSetNames = names(W)
      out$DecisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
      out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
      
    }
    else
    {
      xx = decTreeFromFixedGrouping(pacotestOptions$grouping, W)
      out$DecisionTree = xx$decisionTree
      W = xx$W
      
    }
    
    # Generate plots
    if (pacotestOptions$decisionTreePlot)
    {
      pDecTree = decisionTreePlot(out$DecisionTree)
      print(pDecTree)
      
    }
    if (pacotestOptions$groupedScatterplots)
    {
      GroupedScatterplot(Udata, W, out$DecisionTree)
      
    }
    
  }
  
  return(out)
}




partitionToNumber = function(partitionIdentfier)
{
  partitionNumber = which(partitionIdentfier==c('TreeECOV','TreeECORR','TreeEC',
                                                'SumMedian','SumThirdsI','SumThirdsII','SumThirdsIII','SumQuartiles',
                                                'ProdMedian','ProdThirdsI','ProdThirdsII','ProdThirdsIII','ProdQuartiles'),arr.ind=TRUE)
  
  return(partitionNumber)
}

testTypeToNumber = function(partitionIdentfier)
{
  testTypeNumber = which(partitionIdentfier==c('ECOV', 'ECORR', 'VI', 'EC'),arr.ind=TRUE)
  
  return(testTypeNumber)
}


