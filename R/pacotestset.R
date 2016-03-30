pacotestset = function(pacotestOptions=list(testType = 'ECOV', grouping = 'TreeECOV', aggPvalsNumbRep = 100, groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all'),testType = 'ECOV',grouping= 'TreeECOV', aggPvalsNumbRep= 100, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = 'none', withEstUncert = FALSE, finalComparison = 'all', groupedScatterplots = FALSE, decisionTreePlot = FALSE, numbBoot = 1000){
# Display possible values
  Nargs = nargs()
if(Nargs==0){
    cat('                   testType: [ EqualCop | EC | EqualCovariance | ECOV | VecIndep | VI ]\n\n')
    cat(' Options for testType = [ EqualCop | EC ]:\n')
    cat('                   numbBoot: [ positive scalar ]\n')
    cat('                   grouping: [ TreeECOV | TreeEC | SumMedian | SumThirdsI | SumThirdsII | SumThirdsIII | ProdMedian | ProdThirdsI | ProdThirdsII | ProdThirdsIII ]\n')
    cat('        groupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           decisionTreePlot: [ logical | 0 | 1 ]\n')
    cat('           expMinSampleSize: [ positive scalar ]\n')
    cat('       trainingDataFraction: [ numeric between 0 and 1 ]\n')
    cat('                    aggInfo: [ none | meanAll | meanPairwise ]\n\n')
    cat(' Options for testType = [ EqualCovariance | ECOV ]:\n')
    cat('                   grouping: [ TreeECOV | TreeEC | SumMedian | SumThirdsI | SumThirdsII | SumThirdsIII | ProdMedian | ProdThirdsI | ProdThirdsII | ProdThirdsIII ]\n')
    cat('            aggPvalsNumbRep: [ positive scalar ]\n')
    cat('        groupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           decisionTreePlot: [ logical | 0 | 1 ]\n')
    cat('           expMinSampleSize: [ positive scalar ]\n')
    cat('       trainingDataFraction: [ numeric between 0 and 1 ]\n')
    cat('                    aggInfo: [ none | meanAll | meanPairwise ]\n')
    cat('              withEstUncert: [ logical | 0 | 1 ]\n')
    cat('            finalComparison: [ pairwiseMax | all ]\n\n')
    cat(' Options for testType = [ VecIndep | VI ]:\n')
    cat('                   numbBoot: [ positive scalar ]\n\n')
}
else
{
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
    if (testType=="ECOV" || testType == "EqualCovariance")
    {
      pacotestOptions = list(testType = 'ECOV', grouping = 'TreeECOV', aggPvalsNumbRep = 100, groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all')
      if (!(missing(grouping)))
      {
        pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
      }
      if (!(missing(aggPvalsNumbRep)) && !is.null(aggPvalsNumbRep))
      {
        pacotestOptions$aggPvalsNumbRep = CheckPosScalar(aggPvalsNumbRep,"aggPvalsNumbRep")
      }
      else
      {
        if (is.null(aggPvalsNumbRep))
        {
          pacotestOptions$aggPvalsNumbRep = NULL
        }
      }
      if (!(missing(groupedScatterplots)))
      {
        pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
      }
      if (!(missing(decisionTreePlot)))
      {
        pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
      }
      if (!(missing(expMinSampleSize)) && !is.null(expMinSampleSize))
      {
        pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
      }
      else
      {
        if (is.null(expMinSampleSize))
        {
          pacotestOptions$expMinSampleSize = NULL
        }
      }
      if (!(missing(trainingDataFraction)) && !is.null(trainingDataFraction))
      {
        pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
      }
      else
      {
        if (is.null(trainingDataFraction))
        {
          pacotestOptions$trainingDataFraction = NULL
        }
      }
      if (!(missing(aggInfo)) && !is.null(aggInfo))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
      else
      {
        if (is.null(aggInfo))
        {
          pacotestOptions$aggInfo = NULL
        }
      }
      if (!(missing(withEstUncert)) && !is.null(withEstUncert))
      {
        pacotestOptions$withEstUncert = CheckLogical(withEstUncert,"withEstUncert")
      }
      else
      {
        if (is.null(withEstUncert))
        {
          pacotestOptions$withEstUncert = NULL
        }
      }
      if (!(missing(finalComparison)) && !is.null(finalComparison))
      {
        pacotestOptions$finalComparison = CheckFinalComparison(finalComparison,"finalComparison")
      }
      else
      {
        if (is.null(finalComparison))
        {
          pacotestOptions$finalComparison = NULL
        }
      }
    }
    else if (testType=="EC" || testType == "EqualCop")
    {
      pacotestOptions = list(testType = 'EC', numbBoot = 1000, grouping = 'SumMedian', groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(numbBoot)))
      {
        pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
      }
      if (!(missing(grouping)))
      {
        pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
      }
      if (!(missing(groupedScatterplots)))
      {
        pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
      }
      if (!(missing(decisionTreePlot)))
      {
        pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
      }
      if (!(missing(expMinSampleSize)) && !is.null(expMinSampleSize))
      {
        pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
      }
      else
      {
        if (is.null(expMinSampleSize))
        {
          pacotestOptions$expMinSampleSize = NULL
        }
      }
      if (!(missing(trainingDataFraction)) && !is.null(trainingDataFraction))
      {
        pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
      }
      else
      {
        if (is.null(trainingDataFraction))
        {
          pacotestOptions$trainingDataFraction = NULL
        }
      }
      if (!(missing(aggInfo)) && !is.null(aggInfo))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
      else
      {
        if (is.null(aggInfo))
        {
          pacotestOptions$aggInfo = NULL
        }
      }
    }
    else if (testType=="VI" || testType == "VecIndep")
    {
      pacotestOptions = list(testType = 'VI',numbBoot=1000)
      if (!(missing(numbBoot)))
      {
        pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
      }
    }
    else
    {
      stop("No valid testType.")
    }
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
    
    if (testType=="ECOV" || testType == "EqualCovariance")
    {
      pacotestOptions = list(testType = 'ECOV', grouping = 'TreeECOV', aggPvalsNumbRep = 100, groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none", withEstUncert = FALSE, finalComparison = 'all')
      if (!(missing(grouping)))
      {
        pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
      }
      if (!(missing(aggPvalsNumbRep)))
      {
        pacotestOptions$aggPvalsNumbRep = CheckPosScalar(aggPvalsNumbRep,"aggPvalsNumbRep")
      }
      if (!(missing(groupedScatterplots)))
      {
        pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
      }
      if (!(missing(decisionTreePlot)))
      {
        pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
      }
      if (!(missing(expMinSampleSize)))
      {
        pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
      }
      if (!(missing(trainingDataFraction)))
      {
        pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
      }
      if (!(missing(aggInfo)))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
      if (!(missing(withEstUncert)))
      {
        pacotestOptions$withEstUncert = CheckLogical(withEstUncert,"withEstUncert")
      }
      if (!(missing(finalComparison)))
      {
        pacotestOptions$finalComparison = CheckFinalComparison(finalComparison,"finalComparison")
      }
    }
    else if (testType=="EC" || testType == "EqualCop")
    {
      pacotestOptions = list(testType = 'EC', numbBoot = 1000, grouping = 'SumMedian', groupedScatterplots = FALSE, decisionTreePlot = FALSE, expMinSampleSize = 50, trainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(numbBoot)))
      {
        pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
      }
      if (!(missing(grouping)))
      {
        pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
      }
      if (!(missing(groupedScatterplots)))
      {
        pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
      }
      if (!(missing(decisionTreePlot)))
      {
        pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
      }
      if (!(missing(expMinSampleSize)))
      {
        pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
      }
      if (!(missing(trainingDataFraction)))
      {
        pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
      }
      if (!(missing(aggInfo)))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
    }
    else if (testType=="VI" || testType == "VecIndep")
    {
      pacotestOptions = list(testType = 'VI',numbBoot=1000)
      if (!(missing(numbBoot)))
      {
        pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
      }
    }
    else
    {
      stop("No valid testType.")
    }
  }
  
  if (pacotestOptions$testType=="ECOV" || pacotestOptions$testType == "EqualCovariance")
  {
    #pacotestOptions$testType = "ECOV"
    if (!(missing(grouping)))
    {
      pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
    }
    if (!(missing(aggPvalsNumbRep)))
    {
      pacotestOptions$aggPvalsNumbRep = CheckPosScalar(aggPvalsNumbRep,"aggPvalsNumbRep")
    }
    if (!(missing(groupedScatterplots)))
    {
      pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
    }
    if (!(missing(decisionTreePlot)))
    {
      pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
    }
    if (!(missing(expMinSampleSize)))
    {
      pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
    }
    if (!(missing(trainingDataFraction)))
    {
      pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
    }
    if (!(missing(aggInfo)))
    {
      pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
    }
    if (!(missing(finalComparison)))
    {
      pacotestOptions$finalComparison = CheckFinalComparison(finalComparison,"finalComparison")
    }
  }
  else if (pacotestOptions$testType=="EC" || pacotestOptions$testType == "EqualCop")
  {
    pacotestOptions$testType = "EC"
    if (!(missing(numbBoot)))
    {
      pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
    }
    if (!(missing(grouping)))
    {
      pacotestOptions$grouping = CheckGrouping(grouping,"grouping")
    }
    if (!(missing(groupedScatterplots)))
    {
      pacotestOptions$groupedScatterplots = CheckLogical(groupedScatterplots,"groupedScatterplots")
    }
    if (!(missing(decisionTreePlot)))
    {
      pacotestOptions$decisionTreePlot = CheckLogical(decisionTreePlot,"decisionTreePlot")
    }
    if (!(missing(expMinSampleSize)))
    {
      pacotestOptions$expMinSampleSize = CheckPosScalar(expMinSampleSize,"expMinSampleSize")
    }
    if (!(missing(trainingDataFraction)))
    {
      pacotestOptions$trainingDataFraction = CheckFraction(trainingDataFraction,"trainingDataFraction")
    }
    if (!(missing(aggInfo)))
    {
      pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
    }
  }
  else if (pacotestOptions$testType=="VI" || pacotestOptions$testType == "VecIndep")
  {
    pacotestOptions$testType = "VI"
    if (!(missing(numbBoot)))
    {
      pacotestOptions$numbBoot = CheckPosScalar(numbBoot,"numbBoot")
    }
  }
  else
  {
    stop("No valid testType.")
  }
}
pacotestOptions = CheckpacotestOptions(pacotestOptions)
return(pacotestOptions)
}
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
  if (!(Value == 'SumMedian' || Value == 'SumThirdsI' || Value == 'SumThirdsII' || Value == 'SumThirdsIII' || Value == 'ProdMedian' || Value == 'ProdThirdsI' || Value == 'ProdThirdsII' || Value == 'ProdThirdsIII' || Value == 'TreeEC' || Value == 'TreeECOV' ))
  {
    stop(paste("The option grouping must be 'TreeEC', 'TreeECOV', 'SumMedian', 'SumThirdsI', 'SumThirdsII', 'ProdMedian', 'ProdThirdsI' or 'ProdThirdsII'"))
  }
  return(Value)
}

CheckAggInfo = function(Value,Fieldname)
{
  if (!(Value == 'none' || Value == 'meanAll' || Value == 'meanPairwise' ))
  {
    stop(paste("The option aggInfo must be 'none', 'meanAll' or 'meanPairwise'"))
  }
  return(Value)
}

CheckFinalComparison = function(Value,Fieldname)
{
  if (!(Value == 'pairwiseMax' || Value == 'all' ))
  {
    stop(paste("The option aggInfo must be 'pairwiseMax' or 'all'"))
  }
  return(Value)
}

CheckpacotestOptions = function(pacotestOptions)
{
  
  if (pacotestOptions$testType=="ECOV")
  {
    CheckGrouping(pacotestOptions$grouping,"grouping")
      if (pacotestOptions$grouping=="TreeECOV" || pacotestOptions$grouping=="TreeEC" )
      {
        if (exists('aggPvalsNumbRep', where=pacotestOptions) && pacotestOptions$aggPvalsNumbRep >1 && exists('groupedScatterplots', where=pacotestOptions) && pacotestOptions$groupedScatterplots)
        {
          pacotestOptions$groupedScatterplots = FALSE
          warning('groupedScatterplots is set to FALSE as aggPvalsNumbRep is larger than one')
        }
        if (exists('aggPvalsNumbRep', where=pacotestOptions))
        {
          CheckPosScalar(pacotestOptions$aggPvalsNumbRep,"aggPvalsNumbRep")
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
    }
    if (pacotestOptions$grouping=="TreeECOV" || pacotestOptions$grouping=="TreeEC" )
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
    }
  }
  else if (pacotestOptions$testType=="EC")
  {
    CheckPosScalar(pacotestOptions$numbBoot,"numbBoot")
    CheckGrouping(pacotestOptions$grouping,"grouping")
    if (pacotestOptions$grouping=="TreeECOV" || pacotestOptions$grouping=="TreeEC" )
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
