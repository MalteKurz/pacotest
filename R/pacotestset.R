pacotestset = function(pacotestOptions=list(testType = 'ECORR', grouping = 'TreeECORR', groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 100, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all', penaltyParams = c(1,0.5), gamma0Partition = "SumMedian"), testType = NA_character_, grouping= NA_character_, expMinSampleSize = NA_real_, aggInfo = NA_character_, withEstUncert = NA, finalComparison = NA_character_, penaltyParams = c(NA_real_,NA_real_), gamma0Partition = NA_character_, groupedScatterplots = NA, decisionTreePlot = NA, numbBoot = NA_real_, ...)
  {
  # Display possible values
  Nargs = nargs()
  if(Nargs==0){
    cat('                   testType: [ ECORR | VI ]\n\n')
    cat(' Options for testType = [ ECORR ]:\n')
    cat('                   grouping: [ TreeECORR | SumMedian | SumThirdsI | SumThirdsII | SumThirdsIII | SumQuartiles | ProdMedian | ProdThirdsI | ProdThirdsII | ProdThirdsIII | ProdQuartiles | TreeECOV | TreeEC ]\n')
    cat('           expMinSampleSize: [ positive scalar ]\n')
    cat('                    aggInfo: [ none | meanAll | meanPairwise ]\n')
    cat('              withEstUncert: [ logical | 0 | 1 ]\n')
    cat('            finalComparison: [ pairwiseMax | all ]\n')
    cat('              penaltyParams: [ vector of length two ]\n')
    cat('            gamma0Partition: [ SumMedian | SumThirdsI | SumThirdsII | SumThirdsIII | SumQuartiles | ProdMedian | ProdThirdsI | ProdThirdsII | ProdThirdsIII | ProdQuartiles ]\n')
    cat('        groupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           decisionTreePlot: [ logical | 0 | 1 ]\n\n')
    cat(' Options for testType = [ VI ]:\n')
    cat('                   numbBoot: [ positive scalar ]\n\n')
  }
  else
  {
    # Get a list of input arguments
    if (length(list(...)) > 0)
    {
      e = list2env(list(...),environment())
    }
    else
    {
      e = environment()
    }
    argList = as.list(e, all=TRUE)
    argList$... = NULL
    argList$e = NULL
    argList = argList[unlist(lapply(argList, function(x) !all(is.na(x))))]
    #xx = list(...)
    #argList = c(argList, xx)
    
    
    if(missing(pacotestOptions) || (nargs()==1 && !is.list(pacotestOptions)))
    {
      if (nargs()==1 && is.character(pacotestOptions))
      {
        testType = pacotestOptions
      }
      if(missing(testType))
      {
        stop('The field testType has to be specified')
      }
      else
      {
        pacotestOptions = getDefaultPacotestOptions(testType, grouping, ...)
        
        pacotestOptions = checkAndAssignOptions(testType, pacotestOptions, argList)
        
      }
    }
    else
    {
      if (!is.list(pacotestOptions) || !exists('testType', where=pacotestOptions))
      {
        stop('The provided pacotestOptions have to be given in a list which has testType as member.')
      }
      if (!(missing(testType)))
      {
        warning('After the change of the testType all options are set to their default values except the explicitly stated ones.')
        
        pacotestOptions = getDefaultPacotestOptions(testType, grouping, ...)
        
        pacotestOptions = checkAndAssignOptions(testType, pacotestOptions, argList)
        
      }
      
      pacotestOptions = checkAndAssignOptions(pacotestOptions$testType, pacotestOptions, argList)
      
    }
    pacotestOptions = CheckpacotestOptions(pacotestOptions)
    return(pacotestOptions)
  }
}


checkAndAssignOptions = function(testType, pacotestOptions, argList)
{
  if (pacotestOptions$testType=="ECOV" || pacotestOptions$testType=="ECORR")
  {
    pacotestOptions = checkAndAssignOptionsEcorrOrEcov(pacotestOptions, argList)
    
  }
  else if (pacotestOptions$testType=="EC")
  {
    pacotestOptions = checkAndAssignOptionsEC(pacotestOptions, argList)
    
  }
  else if (pacotestOptions$testType=="VI")
  {
    pacotestOptions = checkAndAssignOptionsVI(pacotestOptions, argList)
    
  }
  else
  {
    stop("No valid testType.")
  }
  
  return(pacotestOptions)
}


checkAndAssignOptionsEcorrOrEcov = function(pacotestOptions, argList)
{
  
  if (exists('sizeKeepingMethod', argList))
  {
    if (!is.null(argList$sizeKeepingMethod))
    {
      pacotestOptions$sizeKeepingMethod = CheckSizeKeepingMethod(argList$sizeKeepingMethod,"sizeKeepingMethod")
    }
    else
    {
      pacotestOptions$sizeKeepingMethod = NULL
    }
  }
  
  if (exists('grouping', argList))
  {
    pacotestOptions$grouping = CheckGrouping(argList$grouping,"grouping")
  }
  
  if (exists('aggPvalsNumbRep', argList))
  {
    if (!is.null(argList$aggPvalsNumbRep))
    {
      pacotestOptions$aggPvalsNumbRep = CheckPosScalar(argList$aggPvalsNumbRep,"aggPvalsNumbRep")
    }
    else
    {
      pacotestOptions$aggPvalsNumbRep = NULL
    }
  }
  
  if (exists('groupedScatterplots', argList))
  {
    pacotestOptions$groupedScatterplots = CheckLogical(argList$groupedScatterplots,"groupedScatterplots")
  }
  
  if (exists('decisionTreePlot', argList))
  {
    pacotestOptions$decisionTreePlot = CheckLogical(argList$decisionTreePlot,"decisionTreePlot")
  }
  
  if (exists('expMinSampleSize', argList))
  {
    if (!is.null(argList$expMinSampleSize))
    {
      pacotestOptions$expMinSampleSize = CheckPosScalar(argList$expMinSampleSize,"expMinSampleSize")
    }
    else
    {
      pacotestOptions$expMinSampleSize = NULL
    }
  }
  
  if (exists('trainingDataFraction', argList))
  {
    if (!is.null(argList$trainingDataFraction))
    {
      pacotestOptions$trainingDataFraction = CheckFraction(argList$trainingDataFraction,"trainingDataFraction")
    }
    else
    {
      pacotestOptions$trainingDataFraction = NULL
    }
  }
  
  if (exists('aggInfo', argList))
  {
    if (!is.null(argList$aggInfo))
    {
      pacotestOptions$aggInfo = CheckAggInfo(argList$aggInfo,"aggInfo")
    }
    else
    {
      pacotestOptions$aggInfo = NULL
    }
  }
  
  if (exists('withEstUncert', argList))
  {
    if (!is.null(argList$withEstUncert))
    {
      pacotestOptions$withEstUncert = CheckLogical(argList$withEstUncert,"withEstUncert")
    }
    else
    {
      pacotestOptions$withEstUncert = NULL
    }
  }
  
  if (exists('finalComparison', argList))
  {
    if (!is.null(argList$finalComparison))
    {
      pacotestOptions$finalComparison = CheckFinalComparison(argList$finalComparison,"CheckFinalComparison")
    }
    else
    {
      pacotestOptions$finalComparison = NULL
    }
  }
  
  if (exists('penaltyParams', argList))
  {
    if (!is.null(argList$penaltyParams))
    {
      pacotestOptions$penaltyParams = CheckPenaltyParams(argList$penaltyParams,"penaltyParams")
    }
    else
    {
      pacotestOptions$penaltyParams = NULL
    }
  }
  
  if (exists('gamma0Partition', argList))
  {
    if (!is.null(argList$gamma0Partition))
    {
      pacotestOptions$gamma0Partition = CheckGamma0Partition(argList$gamma0Partition,"gamma0Partition")
    }
    else
    {
      pacotestOptions$gamma0Partition = NULL
    }
  }
  
  return(pacotestOptions)
}


checkAndAssignOptionsEC = function(pacotestOptions, argList)
{
  
  if (exists('numbBoot', argList))
  {
    pacotestOptions$numbBoot = CheckPosScalar(argList$numbBoot,"numbBoot")
  }
  
  if (exists('grouping', argList))
  {
    pacotestOptions$grouping = CheckGrouping(argList$grouping,"grouping")
  }
  
  if (exists('groupedScatterplots', argList))
  {
    pacotestOptions$groupedScatterplots = CheckLogical(argList$groupedScatterplots,"groupedScatterplots")
  }
  
  if (exists('decisionTreePlot', argList))
  {
    pacotestOptions$decisionTreePlot = CheckLogical(argList$decisionTreePlot,"decisionTreePlot")
  }
  
  if (exists('expMinSampleSize', argList))
  {
    if (!is.null(argList$expMinSampleSize))
    {
      pacotestOptions$expMinSampleSize = CheckPosScalar(argList$expMinSampleSize,"expMinSampleSize")
    }
    else
    {
      pacotestOptions$expMinSampleSize = NULL
    }
  }
  
  if (exists('trainingDataFraction', argList))
  {
    if (!is.null(argList$trainingDataFraction))
    {
      pacotestOptions$trainingDataFraction = CheckFraction(argList$trainingDataFraction,"trainingDataFraction")
    }
    else
    {
      pacotestOptions$trainingDataFraction = NULL
    }
  }
  
  if (exists('aggInfo', argList))
  {
    if (!is.null(argList$aggInfo))
    {
      pacotestOptions$aggInfo = CheckAggInfo(argList$aggInfo,"aggInfo")
    }
    else
    {
      pacotestOptions$aggInfo = NULL
    }
  }
  
  return(pacotestOptions)
}


checkAndAssignOptionsVI = function(pacotestOptions, argList)
{
  if (exists('numbBoot', argList))
  {
    pacotestOptions$numbBoot = CheckPosScalar(argList$numbBoot,"numbBoot")
  }
  
  return(pacotestOptions)
}


getDefaultPacotestOptions = function(testType, grouping = NA_character_, sizeKeepingMethod = NULL, ...)
{
  if (is.element(testType, c("ECORR","ECOV")))
  {
    defaultTreeGrouping = paste('Tree', testType, sep = "")
    
    if (is.null(sizeKeepingMethod) || is.na(sizeKeepingMethod) || sizeKeepingMethod == 'penalty')
    {
      if (is.na(grouping) || is.element(grouping, c('TreeECORR', 'TreeECOV','TreeEC')))
      {
        pacotestOptions = list(testType = testType, grouping = defaultTreeGrouping, groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 100, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all', sizeKeepingMethod = 'penalty', penaltyParams = c(1,0.5), gamma0Partition = "SumMedian")
      }
      else
      {
        pacotestOptions = list(testType = testType, grouping = 'SumMedian', groupedScatterplots = FALSE, decisionTreePlot = FALSE)
      }
    }
    else if (sizeKeepingMethod == 'splitTrainEvaluate')
    {
      if (is.na(grouping) || is.element(grouping, c('TreeECORR', 'TreeECOV','TreeEC')))
      {
        pacotestOptions = list(testType = testType, grouping = defaultTreeGrouping, aggPvalsNumbRep = 100, groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 100, trainingDataFraction = 0.5, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all', sizeKeepingMethod = 'splitTrainEvaluate')
      }
      else
      {
        pacotestOptions = list(testType = testType, grouping = 'SumMedian', groupedScatterplots = FALSE, decisionTreePlot = FALSE)
      }
    }
  }
  else if (testType=="EC")
  {
    if (is.na(grouping) || is.element(grouping, c('TreeECORR', 'TreeECOV','TreeEC')))
    {
      pacotestOptions = list(testType = testType, numbBoot = 1000, grouping = 'TreeECORR', groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none")
    }
    else
    {
      pacotestOptions = list(testType = testType, numbBoot = 1000, grouping = 'SumMedian', groupedScatterplots = FALSE, decisionTreePlot = FALSE)
      
    }
  }
  else if (testType=="VI")
  {
    pacotestOptions = list(testType = 'VI',numbBoot=1000)
  }
  else
  {
    stop("No valid testType.")
  }
  
  return(pacotestOptions)
}


CheckPosScalar = function(Value,Fieldname)
{
  if (!(is.numeric(Value)) || (Value <1 && !(Fieldname=="aggPvalsNumbRep" && Value == 0)) || Value %% 1)
  {
    stop(paste("The option ", Fieldname, " must be a positive scalar."))
  }
  return(Value)
}


CheckFraction = function(Value,Fieldname)
{
  if (!(is.numeric(Value)) || (Value <=0 || Value >= 1))
  {
    stop(paste("The option ", Fieldname, " must be a numeric between 0 and 1."))
  }
  return(Value)
}


CheckLogical = function(Value,Fieldname)
{
  if (!(is.logical(Value) || Value == 1 || Value == 0))
  {
    stop(paste("The option ", Fieldname, " must be a logical or the values 0 or 1."))
  }
  return(as.logical(Value))
}


CheckGrouping = function(Value,Fieldname)
{
  if (!(is.element(Value, c('SumMedian', 'SumThirdsI', 'SumThirdsII', 'SumThirdsIII', 'SumQuartiles', 'ProdMedian', 'ProdThirdsI', 'ProdThirdsII', 'ProdThirdsIII', 'ProdQuartiles', 'TreeEC', 'TreeECOV', 'TreeECORR'))))
  {
    stop(paste("The option grouping must be 'TreeEC', 'TreeECOV', 'TreeECORR', 'SumMedian', 'SumThirdsI', 'SumThirdsII' , 'SumThirdsIII', 'SumQuartiles', 'ProdMedian', 'ProdThirdsI', 'ProdThirdsII', 'ProdThirdsII' or 'ProdQuartiles'"))
  }
  return(Value)
}


CheckAggInfo = function(Value,Fieldname)
{
  if (!(is.element(Value, c('none', 'meanAll', 'meanPairwise'))))
  {
    stop(paste("The option aggInfo must be 'none', 'meanAll' or 'meanPairwise'"))
  }
  return(Value)
}


CheckFinalComparison = function(Value,Fieldname)
{
  if (!(is.element(Value, c('pairwiseMax', 'all'))))
  {
    stop(paste("The option aggInfo must be 'pairwiseMax' or 'all'"))
  }
  return(Value)
}


CheckSizeKeepingMethod = function(Value,Fieldname)
{
  if (!(is.element(Value, c('splitTrainEvaluate', 'penalty'))))
  {
    stop(paste("The option sizeKeepingMethod must be 'splitTrainEvaluate' or 'penalty'"))
  }
  return(Value)
}


CheckPenaltyParams = function(Value,Fieldname)
{
  if (!(is.numeric(Value)) || !(is.vector(Value)) || !(length(Value)==2))
  {
    stop(paste("The option ", Fieldname, " must be a numeric vector of length two."))
  }
  
  if (!(is.numeric(Value[1])) || (Value[1] <0) )
  {
    stop(paste("The first value of the penaltyParams must be a positive scalar."))
  }
  
  if (!(is.numeric(Value[2])) || (Value[2] <0 || Value[2] >= 1))
  {
    stop(paste("The second value of the penaltyParams must be a numeric in the interval [0,1)."))
  }
  
  return(Value)
}


CheckGamma0Partition = function(Value,Fieldname)
{
  if (!(is.element(Value, c('SumMedian', 'SumThirdsI', 'SumThirdsII', 'SumThirdsIII', 'SumQuartiles', 'ProdMedian', 'ProdThirdsI', 'ProdThirdsII', 'ProdThirdsIII', 'ProdQuartiles'))))
  {
    stop(paste("The option gamma0Partition must be 'SumMedian', 'SumThirdsI', 'SumThirdsII', 'SumThirdsIII', 'SumQuartiles', 'ProdMedian', 'ProdThirdsI', 'ProdThirdsII', 'ProdThirdsII' or 'ProdQuartiles'"))
  }
  
  return(Value)
}


CheckpacotestOptions = function(pacotestOptions)
{
  
  if (is.element(pacotestOptions$testType, c("ECOV", "ECORR")))
  {
    CheckGrouping(pacotestOptions$grouping,"grouping")
    if (is.element(pacotestOptions$grouping, c("TreeECOV", "TreeECORR", "TreeEC")))
    {
      if (pacotestOptions$sizeKeepingMethod=="splitTrainEvaluate")
      {
        if (!(exists('aggPvalsNumbRep', where=pacotestOptions)))
        {
          pacotestOptions$aggPvalsNumbRep = 100
        }
        if (!(exists('trainingDataFraction', where=pacotestOptions)))
        {
          pacotestOptions$trainingDataFraction = 0.5
        }
        
        if (exists('aggPvalsNumbRep', where=pacotestOptions) && pacotestOptions$aggPvalsNumbRep >1 && exists('groupedScatterplots', where=pacotestOptions) && pacotestOptions$groupedScatterplots)
        {
          pacotestOptions$groupedScatterplots = FALSE
          warning('groupedScatterplots is set to FALSE as aggPvalsNumbRep is larger than one')
        }
        if (exists('aggPvalsNumbRep', where=pacotestOptions))
        {
          CheckPosScalar(pacotestOptions$aggPvalsNumbRep,"aggPvalsNumbRep")
        }
        
        if (exists('penaltyParams', where=pacotestOptions) && !is.null(pacotestOptions$penaltyParams))
        {
          pacotestOptions$penaltyParams = NULL;
          warning('The field penaltyParams is set to NULL')
        }
        
        if (exists('gamma0Partition', where=pacotestOptions) && !is.null(pacotestOptions$gamma0Partition))
        {
          pacotestOptions$gamma0Partition = NULL;
          warning('The field gamma0Partition is set to NULL')
        }
        
      }
      else
      {
        if (exists('trainingDataFraction', where=pacotestOptions) && !is.null(pacotestOptions$trainingDataFraction))
        {
          pacotestOptions$trainingDataFraction = NULL;
          warning('The field trainingDataFraction is set to NULL')
        }
        
        if (exists('aggPvalsNumbRep', where=pacotestOptions) && !is.null(pacotestOptions$aggPvalsNumbRep))
        {
          pacotestOptions$aggPvalsNumbRep = NULL;
          warning('The field aggPvalsNumbRep is set to NULL')
        }
      }
    }
    else
    {
      if (exists('expMinSampleSize', where=pacotestOptions) && !is.null(pacotestOptions$expMinSampleSize))
      {
        pacotestOptions$expMinSampleSize = NULL;
        warning('The field expMinSampleSize is set to NULL')
      }
      
      if (exists('trainingDataFraction', where=pacotestOptions) && !is.null(pacotestOptions$trainingDataFraction))
      {
        pacotestOptions$trainingDataFraction = NULL;
        warning('The field trainingDataFraction is set to NULL')
      }
      
      if (exists('aggInfo', where=pacotestOptions) && !is.null(pacotestOptions$aggInfo))
      {
        pacotestOptions$aggInfo = NULL;
        warning('The field aggInfo is set to NULL')
      }
      
      if (exists('aggPvalsNumbRep', where=pacotestOptions) && !is.null(pacotestOptions$aggPvalsNumbRep))
      {
        pacotestOptions$aggPvalsNumbRep = NULL;
        warning('The field aggPvalsNumbRep is set to NULL')
      }
      
      if (exists('sizeKeepingMethod', where=pacotestOptions) && !is.null(pacotestOptions$sizeKeepingMethod))
      {
        pacotestOptions$sizeKeepingMethod = NULL;
        warning('The field sizeKeepingMethod is set to NULL')
      }
      
      if (exists('penaltyParams', where=pacotestOptions) && !is.null(pacotestOptions$penaltyParams))
      {
        pacotestOptions$penaltyParams = NULL;
        warning('The field penaltyParams is set to NULL')
      }
      
      if (exists('gamma0Partition', where=pacotestOptions) && !is.null(pacotestOptions$gamma0Partition))
      {
        pacotestOptions$gamma0Partition = NULL;
        warning('The field gamma0Partition is set to NULL')
      }
    }
    if (is.element(pacotestOptions$grouping, c("TreeECOV", "TreeECORR", "TreeEC")))
    {
      if (exists('expMinSampleSize', where=pacotestOptions))
      {
        CheckPosScalar(pacotestOptions$expMinSampleSize,"expMinSampleSize")
      }
      if (exists('trainingDataFraction', where=pacotestOptions))
      {
        CheckFraction(pacotestOptions$trainingDataFraction,"trainingDataFraction")
      }
      if (exists('aggInfo', where=pacotestOptions))
      {
        CheckAggInfo(pacotestOptions$aggInfo,"aggInfo")
      }
      if (exists('withEstUncert', where=pacotestOptions))
      {
        CheckLogical(pacotestOptions$withEstUncert,"withEstUncert")
      }
      if (exists('finalComparison', where=pacotestOptions))
      {
        CheckFinalComparison(pacotestOptions$finalComparison,"finalComparison")
      }
      if (exists('sizeKeepingMethod', where=pacotestOptions))
      {
        CheckSizeKeepingMethod(pacotestOptions$sizeKeepingMethod,"sizeKeepingMethod")
      }
      if (exists('penaltyParams', where=pacotestOptions))
      {
        CheckPenaltyParams(pacotestOptions$penaltyParams,"penaltyParams")
      }
      if (exists('gamma0Partition', where=pacotestOptions))
      {
        CheckGamma0Partition(pacotestOptions$gamma0Partition,"gamma0Partition")
      }
    }
  }
  else if (pacotestOptions$testType=="EC")
  {
    CheckPosScalar(pacotestOptions$numbBoot,"numbBoot")
    CheckGrouping(pacotestOptions$grouping,"grouping")
    if (is.element(pacotestOptions$grouping, c("TreeECOV", "TreeECORR", "TreeEC" )))
    {
      if (exists('expMinSampleSize', where=pacotestOptions))
      {
        CheckPosScalar(pacotestOptions$expMinSampleSize,"expMinSampleSize")
      }
      if (exists('trainingDataFraction', where=pacotestOptions))
      {
        CheckFraction(pacotestOptions$trainingDataFraction,"trainingDataFraction")
      }
      if (exists('aggInfo', where=pacotestOptions))
      {
        CheckAggInfo(pacotestOptions$aggInfo,"aggInfo")
      }
    }
    else
    {
      if (exists('expMinSampleSize', where=pacotestOptions) && !is.null(pacotestOptions$expMinSampleSize))
      {
        pacotestOptions$expMinSampleSize = NULL;
        warning('The field expMinSampleSize is set to NULL')
      }
      
      if (exists('trainingDataFraction', where=pacotestOptions) && !is.null(pacotestOptions$trainingDataFraction))
      {
        pacotestOptions$trainingDataFraction = NULL;
        warning('The field trainingDataFraction is set to NULL')
      }
      
      if (exists('aggInfo', where=pacotestOptions) && !is.null(pacotestOptions$aggInfo))
      {
        pacotestOptions$aggInfo = NULL;
        warning('The field aggInfo is set to NULL')
      }
    }
  }
  else if (pacotestOptions$testType=="VI")
  {
    CheckPosScalar(pacotestOptions$numbBoot,"numbBoot")
  }
  else
  {
    stop("No valid pacotestOptions$testType.")
  }
  
  return(pacotestOptions)
}
