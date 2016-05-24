pacotest = function(Udata,W,pacotestOptions, data = NULL, svcmDataFrame = NULL, cPitData = NULL){
  
  #pacotestOptions = pacotestset(pacotestOptions)
  
  if (!is.data.frame(Udata))
  {
    Udata = as.data.frame(Udata)
  }
  
  if (!is.data.frame(W))
  {
    W = as.data.frame(W)
  }
  
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
      
      W = addAggInfo(W,pacotestOptions$aggInfo);
      
      out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$aggPvalsNumbRep,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
      
      condSetNames = names(W);
      out$DecisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
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
          W = addAggInfo(W,pacotestOptions$aggInfo);
          
          out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,0,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
          
        }
        else if (pacotestOptions$sizeKeepingMethod=='penalty')
        {
          gamma0Partition = partitionToNumber(pacotestOptions$gamma0Partition)
          
          if (pacotestOptions$aggInfo=='meanAll')
          {
            W = addAggInfo(W,pacotestOptions$aggInfo);
          }
          else
          {
            if (dim(W)[2]>2 && pacotestOptions$aggInfo=="meanPairwise")
            {
              # Add the information defined by pacotestOptions$aggInfo 
              xx = addAggInfo(W,pacotestOptions$aggInfo)
              yy = addAggInfo(W,'meanAll')
              yyLastName = names(yy)[dim(yy)[2]]
              
              W = cbind(xx,yy[,yyLastName])
              
              names(W)[dim(W)[2]] = yyLastName
            }
            else
            {
              W = addAggInfo(W,'meanAll')
            }
          }
          
          out = ecorrOrEcovWithPenalty(testTypeNumber, as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$expMinSampleSize, pacotestOptions$penaltyParams[1], pacotestOptions$penaltyParams[2], gamma0Partition)
          
        }
        
      }
      if (grouping<=2)
      {
        condSetNames = names(W)
        out$DecisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
      }
      else
      {
        xx = decTreeFromFixedGrouping(pacotestOptions$grouping, W)
        out$DecisionTree = xx$decisionTree
        W = xx$W
        
      }
      if (pacotestOptions$decisionTreePlot)
      {
        pDecTree = decisionTreePlot(out$DecisionTree)
        print(pDecTree)
      }
    }
    out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
    if (pacotestOptions$groupedScatterplots)
    {
      GroupedScatterplot(Udata, W, out$DecisionTree)
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
      
      W = addAggInfo(W,pacotestOptions$aggInfo);
      out = EC(as.matrix(Udata), as.matrix(W),pacotestOptions$numbBoot,grouping,finalComparison,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
    }
    if (grouping<=2)
    {
      CondSetDim = ncol(W);
      out$DecisionTree = ExtractDecisionTree(CondSetDim, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
      
    }
    else
    {
      xx = decTreeFromFixedGrouping(pacotestOptions$grouping, W)
      out$DecisionTree = xx$decisionTree
      W = xx$W
      
    }
    if (pacotestOptions$decisionTreePlot)
    {
      pDecTree = decisionTreePlot(out$DecisionTree)
      print(pDecTree)
    }
    out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
    if (pacotestOptions$groupedScatterplots)
    {
      GroupedScatterplot(Udata, W, out$DecisionTree)
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
  return(out)
}

addAggInfo = function(W,aggInfoType)
{
  # Adding aggregated information within the conditioning set
  
  aggInfo = which(aggInfoType==c('none','meanAll','meanPairwise'),arr.ind=TRUE)
  if (ncol(W)>1)
  {
    if (aggInfo==2)
    {
      xx = as.data.frame(rowMeans(W))
      names(xx) = paste("Mean(", paste(names(W),collapse=", "), ")", sep="")
      
      W = cbind(W,xx)
    }
    else
    {
      if (aggInfo==3)
      {
        xx = as.data.frame(combn(ncol(W), 2L, function(x) rowMeans(W[,x])))
        names(xx) = combn(names(W),2, function(x) paste("Mean(",x[1],", ",x[2],")",sep=""))
        
        W = cbind(W,xx)
      }
    }
  }
  
  return(W);
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


