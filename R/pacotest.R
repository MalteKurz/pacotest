pacotest = function(Udata,W,pacotestOptions, data = NULL, svcmDataFrame = NULL, cPitData = NULL){
  
  if (is.character(pacotestOptions) && (is.element(pacotestOptions, c('CCC', 'ECORR', 'ECOV'))))
  {
    pacotestOptions = pacotestset(testType = pacotestOptions, withEstUncert = FALSE, estUncertWithRanks = FALSE)
  }
  else
  {
    pacotestOptions = pacotestset(pacotestOptions)
  }
  
  if (!is.data.frame(Udata))
  {
    Udata = as.data.frame(Udata)
  }
  
  if (!is.data.frame(W))
  {
    W = as.data.frame(W)
  }
  
  dimCondSet = dim(W)[2]
  
  # Prepare variables to be transfered to C++
  if (pacotestOptions$testType=='ECOV' || pacotestOptions$testType=='CCC' || pacotestOptions$testType=='EC')
  {
    # Add aggregated information to the conditioning vector
    W = addAggInfo(W,pacotestOptions$aggInfo, pacotestOptions$sizeKeepingMethod)
    
    # Transfer (character) variables to numbers
    grouping = partitionToNumber(pacotestOptions$grouping)
    testTypeNumber = testTypeToNumber(pacotestOptions$testType)
    
    finalComparison = finalComparisonToNumber(pacotestOptions$finalComparison)
    gamma0Partition = partitionToNumber(pacotestOptions$gamma0Partition)
    
    aggPvalsNumbRep = aggPvalsNumbRepToNumber(pacotestOptions$aggPvalsNumbRep)
    expMinSampleSize = expMinSampleSizeToNumber(pacotestOptions$expMinSampleSize)
    trainingDataFraction = trainingDataFractionToNumber(pacotestOptions$trainingDataFraction)
    penaltyParams = penaltyParamsToVector(pacotestOptions$penaltyParams)
    
  }
  
  if (pacotestOptions$testType=='ECOV' || pacotestOptions$testType=='CCC')
  {
    
    if (!(pacotestOptions$withEstUncert))
    {
      data = matrix()
      svcmDataFrame = data.frame()
      cPitData = matrix()
    }
    else
    {
      if (is.null(data) && is.null(svcmDataFrame) && is.null(cPitData))
      {
        warning(paste('pacotest called with option pacotestOptions$withEstUncert = TRUE but the three inputs data, svcmDataFrame and cPitData are NULL. ',
                      'To apply pacotest with known PITs (without consideration of estimation uncertainty) set pacotestOptions$withEstUncert = FALSE and pacotestOptions$estUncertWithRanks = FALSE ',
                      'In the context of vine copulas, the functions pacotestRvineSeq and pacotestRvineSingleCopula are recommended and provide the missing inputs ',
                      'for the pacotest function automatically.\n',
                      'Due to the missing inputs for consideration of estimation uncertainty pacotest with known PITs is applied, i.e., pacotestOptions$withEstUncert = FALSE and pacotestOptions$estUncertWithRanks = FALSE are being set.',
                      sep=''))
        pacotestOptions$withEstUncert = FALSE
        pacotestOptions$estUncertWithRanks = FALSE
        data = matrix()
        svcmDataFrame = data.frame()
        cPitData = matrix()
      } else if (is.null(data) || is.null(svcmDataFrame) || is.null(cPitData))
      {
        stop(paste('pacotest called with option pacotestOptions$withEstUncert = TRUE but at least one of the three inputs data, svcmDataFrame or cPitData is NULL. ',
                   'To apply pacotest with known PITs (without consideration of estimation uncertainty) set pacotestOptions$withEstUncert = FALSE and pacotestOptions$estUncertWithRanks = FALSE. ',
                   'In the context of vine copulas, the functions pacotestRvineSeq and pacotestRvineSingleCopula are recommended and provide the missing inputs ',
                   'for the pacotest function automatically.', sep=''))
      }
    }
    
    out = ecorrOrEcov(testTypeNumber, as.matrix(Udata), as.matrix(W), dimCondSet,
                      grouping, pacotestOptions$withEstUncert, pacotestOptions$estUncertWithRanks, finalComparison,
                      as.matrix(data), svcmDataFrame, cPitData,
                      aggPvalsNumbRep, expMinSampleSize, trainingDataFraction,
                      penaltyParams[1], penaltyParams[2], gamma0Partition)
    
  }
  else if (pacotestOptions$testType=='EC')
  {
    out = EC(as.matrix(Udata), as.matrix(W), pacotestOptions$numbBoot,
           grouping, finalComparison, expMinSampleSize, trainingDataFraction)
    
  }
  else if (pacotestOptions$testType=='VI')
  {
    out = VI(as.matrix(Udata), as.matrix(W), pacotestOptions$numbBoot)
  }
  else
  {
    stop('Unknown testType')
  }
  
  # Export/generate the decision tree and the illustrative plots
  if (pacotestOptions$testType=='ECOV' || pacotestOptions$testType=='CCC' || pacotestOptions$testType=='EC')
  {
    # Extract decision tree(s)
    if (grouping<=3)
    {
      condSetNames = names(W)
      out$decisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
      out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
      
    }
    else
    {
      xx = decTreeFromFixedGrouping(pacotestOptions$grouping, W)
      out$decisionTree = xx$decisionTree
      W = xx$W
      
    }
    
    # Generate plots
    if (pacotestOptions$decisionTreePlot)
    {
      pDecTree = decisionTreePlot(out$decisionTree)
      print(pDecTree)
      
    }
    if (pacotestOptions$groupedScatterplots)
    {
      GroupedScatterplot(Udata, W, out$decisionTree)
      
    }
    
  }
  
  return(out)
}


partitionToNumber = function(partitionIdentifier)
{
  if (is.null(partitionIdentifier))
  {
    partitionNumber = NA_real_
  }
  else
  {
    partitionNumber = which(partitionIdentifier==c('TreeECOV','TreeCCC','TreeEC',
                                                   'SumMedian','SumThirdsI','SumThirdsII','SumThirdsIII','SumQuartiles',
                                                   'ProdMedian','ProdThirdsI','ProdThirdsII','ProdThirdsIII','ProdQuartiles'),arr.ind=TRUE)
  }
  
  return(partitionNumber)
}

testTypeToNumber = function(partitionIdentifier)
{
  testTypeNumber = which(partitionIdentifier==c('ECOV', 'CCC', 'VI', 'EC'),arr.ind=TRUE)
  
  return(testTypeNumber)
}

finalComparisonToNumber = function(finalComparisonIdentifier = NULL)
{
  if (is.null(finalComparisonIdentifier))
  {
    finalComparison = 999
  }
  else
  {
    finalComparison = which(finalComparisonIdentifier==c('pairwiseMax','all'))
  }
  
  return(finalComparison)
}


aggPvalsNumbRepToNumber = function(aggPvalsNumbRep = NULL)
{
  if (is.null(aggPvalsNumbRep) || aggPvalsNumbRep == 1)
  {
    aggPvalsNumbRep = 0
  }
  
  return(aggPvalsNumbRep)
}


expMinSampleSizeToNumber = function(expMinSampleSize = NULL)
{
  if (is.null(expMinSampleSize))
  {
    expMinSampleSize = NA_real_
  }
  
  return(expMinSampleSize)
}


trainingDataFractionToNumber = function(trainingDataFraction = NULL)
{
  if (is.null(trainingDataFraction))
  {
    trainingDataFraction = NA_real_
  }
  
  return(trainingDataFraction)
}


penaltyParamsToVector = function(penaltyParams = NULL)
{
  if (is.null(penaltyParams))
  {
    penaltyParams = c(NA_real_,NA_real_)
  }
  
  return(penaltyParams)
}

