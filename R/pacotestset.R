pacotestset = function(pacotestOptions=list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5),TestType = 'ERC',Grouping= 'TreeERC', AggPvalsNumbRep= 100, ExpMinSampleSize = 50, TrainingDataFraction = 0.5, aggInfo = 'none', GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, NumbBoot = 1000){
# Display possible values
  Nargs = nargs()
if(Nargs==0){
    cat('                   TestType: [ EqualCop | EC | EqualRankCorr | ERC | VecIndep | VI ]\n\n')
    cat(' Options for TestType = [ EqualCop | EC ]:\n')
    cat('                   NumbBoot: [ positive scalar ]\n')
    cat('                   Grouping: [ TreeERC | TreeEC | SumMedian | SumThirdsI | SumThirdsII | ProdMedian | ProdThirdsI | ProdThirdsII ]\n')
    cat('        GroupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           DecisionTreePlot: [ logical | 0 | 1 ]\n')
    cat('           ExpMinSampleSize: [ positive scalar ]\n')
    cat('       TrainingDataFraction: [ numeric between 0 and 1 ]\n')
    cat('                    aggInfo: [ none | meanAll | meanPairwise ]\n\n')
    cat(' Options for TestType = [ EqualRankCorr | ERC ]:\n')
    cat('                   Grouping: [ TreeERC | TreeEC | SumMedian | SumThirdsI | SumThirdsII | ProdMedian | ProdThirdsI | ProdThirdsII ]\n')
    cat('            AggPvalsNumbRep: [ positive scalar ]\n')
    cat('        GroupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           DecisionTreePlot: [ logical | 0 | 1 ]\n')
    cat('           ExpMinSampleSize: [ positive scalar ]\n')
    cat('       TrainingDataFraction: [ numeric between 0 and 1 ]\n')
    cat('                    aggInfo: [ none | meanAll | meanPairwise ]\n\n')
    cat(' Options for TestType = [ VecIndep | VI ]:\n')
    cat('                   NumbBoot: [ positive scalar ]\n\n')
}
if(missing(pacotestOptions) || (nargs()==1 && !is.list(pacotestOptions)))
{
  if (nargs()==1 && is.character(pacotestOptions))
  {
    TestType = pacotestOptions
  }
  if(missing(TestType))
  {
    stop('The field TestType has to be specified')
  }
  else
  {
    if (TestType=="ERC" || TestType == "EqualRankCorr")
    {
      pacotestOptions = list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(Grouping)))
      {
        pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(AggPvalsNumbRep)) && !is.null(AggPvalsNumbRep))
      {
        pacotestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
      }
      else
      {
        if (is.null(AggPvalsNumbRep))
        {
          pacotestOptions$AggPvalsNumbRep = NULL
        }
      }
      if (!(missing(GroupedScatterplots)))
      {
        pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)) && !is.null(ExpMinSampleSize))
      {
        pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      else
      {
        if (is.null(ExpMinSampleSize))
        {
          pacotestOptions$ExpMinSampleSize = NULL
        }
      }
      if (!(missing(TrainingDataFraction)) && !is.null(TrainingDataFraction))
      {
        pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
      else
      {
        if (is.null(TrainingDataFraction))
        {
          pacotestOptions$TrainingDataFraction = NULL
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
    else if (TestType=="EC" || TestType == "EqualCop")
    {
      pacotestOptions = list(TestType = 'EC', NumbBoot = 1000, Grouping = 'SumMedian', GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(NumbBoot)))
      {
        pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
      if (!(missing(Grouping)))
      {
        pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(GroupedScatterplots)))
      {
        pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)) && !is.null(ExpMinSampleSize))
      {
        pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      else
      {
        if (is.null(ExpMinSampleSize))
        {
          pacotestOptions$ExpMinSampleSize = NULL
        }
      }
      if (!(missing(TrainingDataFraction)) && !is.null(TrainingDataFraction))
      {
        pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
      else
      {
        if (is.null(TrainingDataFraction))
        {
          pacotestOptions$TrainingDataFraction = NULL
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
    else if (TestType=="VI" || TestType == "VecIndep")
    {
      pacotestOptions = list(TestType = 'VI',NumbBoot=1000)
      if (!(missing(NumbBoot)))
      {
        pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
    }
    else
    {
      stop("No valid TestType.")
    }
  }
}
else
{
  if (!is.list(pacotestOptions) || !exists('TestType', where=pacotestOptions))
  {
    stop('The provided pacotestOptions have to be given in a list which has TestType as member.')
  }
  if (!(missing(TestType)))
  {
    warning('After the change of the TestType all options are set to their default values except the explicitly stated ones.')
    
    if (TestType=="ERC" || TestType == "EqualRankCorr")
    {
      pacotestOptions = list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(Grouping)))
      {
        pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(AggPvalsNumbRep)))
      {
        pacotestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
      }
      if (!(missing(GroupedScatterplots)))
      {
        pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
      if (!(missing(aggInfo)))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
    }
    else if (TestType=="EC" || TestType == "EqualCop")
    {
      pacotestOptions = list(TestType = 'EC', NumbBoot = 1000, Grouping = 'SumMedian', GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5, aggInfo = "none")
      if (!(missing(NumbBoot)))
      {
        pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
      if (!(missing(Grouping)))
      {
        pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(GroupedScatterplots)))
      {
        pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
      if (!(missing(aggInfo)))
      {
        pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
      }
    }
    else if (TestType=="VI" || TestType == "VecIndep")
    {
      pacotestOptions = list(TestType = 'VI',NumbBoot=1000)
      if (!(missing(NumbBoot)))
      {
        pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
    }
    else
    {
      stop("No valid TestType.")
    }
  }
  
  if (pacotestOptions$TestType=="ERC" || pacotestOptions$TestType == "EqualRankCorr")
  {
    pacotestOptions$TestType = "ERC"
    if (!(missing(Grouping)))
    {
      pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
    }
    if (!(missing(AggPvalsNumbRep)))
    {
      pacotestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
    }
    if (!(missing(GroupedScatterplots)))
    {
      pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
    }
    if (!(missing(DecisionTreePlot)))
    {
      pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
    }
    if (!(missing(ExpMinSampleSize)))
    {
      pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
    }
    if (!(missing(TrainingDataFraction)))
    {
      pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
    }
    if (!(missing(aggInfo)))
    {
      pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
    }
  }
  else if (pacotestOptions$TestType=="EC" || pacotestOptions$TestType == "EqualCop")
  {
    pacotestOptions$TestType = "EC"
    if (!(missing(NumbBoot)))
    {
      pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
    }
    if (!(missing(Grouping)))
    {
      pacotestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
    }
    if (!(missing(GroupedScatterplots)))
    {
      pacotestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
    }
    if (!(missing(DecisionTreePlot)))
    {
      pacotestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
    }
    if (!(missing(ExpMinSampleSize)))
    {
      pacotestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
    }
    if (!(missing(TrainingDataFraction)))
    {
      pacotestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
    }
    if (!(missing(aggInfo)))
    {
      pacotestOptions$aggInfo = CheckAggInfo(aggInfo,"aggInfo")
    }
  }
  else if (pacotestOptions$TestType=="VI" || pacotestOptions$TestType == "VecIndep")
  {
    pacotestOptions$TestType = "VI"
    if (!(missing(NumbBoot)))
    {
      pacotestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
    }
  }
  else
  {
    stop("No valid TestType.")
  }
}
pacotestOptions = CheckpacotestOptions(pacotestOptions)
return(pacotestOptions)
}

CheckPosScalar = function(Value,Fieldname)
{
  if (!(is.numeric(Value)) || (Value <1 && !(Fieldname=="AggPvalsNumbRep" && Value == 0)) || Value %% 1)
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
  if (!(Value == 'SumMedian' || Value == 'SumThirdsI' || Value == 'SumThirdsII' || Value == 'ProdMedian' || Value == 'ProdThirdsI' || Value == 'ProdThirdsII' || Value == 'TreeEC' || Value == 'TreeERC' || Value == 'TreeERCchi2' || Value == 'TreeERCchi2WithEstimation' ))
  {
    stop(paste("The option Grouping must be 'TreeEC', 'TreeERC', TreeERCchi2, 'SumMedian', 'SumThirdsI', 'SumThirdsII', 'ProdMedian', 'ProdThirdsI' or 'ProdThirdsII'"))
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

CheckpacotestOptions = function(pacotestOptions)
{
  
  if (pacotestOptions$TestType=="ERC")
  {
    CheckGrouping(pacotestOptions$Grouping,"Grouping")
      if (pacotestOptions$Grouping=="TreeERC" || pacotestOptions$Grouping=="TreeEC" || pacotestOptions$Grouping=="TreeERCchi2" || pacotestOptions$Grouping=='TreeERCchi2WithEstimation' )
      {
        if (exists('AggPvalsNumbRep', where=pacotestOptions) && pacotestOptions$AggPvalsNumbRep >1 && exists('GroupedScatterplots', where=pacotestOptions) && pacotestOptions$GroupedScatterplots)
        {
          pacotestOptions$GroupedScatterplots = FALSE
          warning('GroupedScatterplots is set to FALSE as AggPvalsNumbRep is larger than one')
        }
        if (exists('AggPvalsNumbRep', where=pacotestOptions))
        {
          CheckPosScalar(pacotestOptions$AggPvalsNumbRep,"AggPvalsNumbRep")
        }
      }
    else
    {
      if (exists('ExpMinSampleSize', where=pacotestOptions) && !is.null(pacotestOptions$ExpMinSampleSize))
      {
        pacotestOptions$ExpMinSampleSize = NULL;
        warning('The field ExpMinSampleSize is set to NULL')
      }
      
      if (exists('TrainingDataFraction', where=pacotestOptions) && !is.null(pacotestOptions$TrainingDataFraction))
      {
        pacotestOptions$TrainingDataFraction = NULL;
        warning('The field TrainingDataFraction is set to NULL')
      }
      
      if (exists('aggInfo', where=pacotestOptions) && !is.null(pacotestOptions$aggInfo))
      {
        pacotestOptions$aggInfo = NULL;
        warning('The field aggInfo is set to NULL')
      }
      
      if (exists('AggPvalsNumbRep', where=pacotestOptions) && !is.null(pacotestOptions$AggPvalsNumbRep))
      {
        pacotestOptions$AggPvalsNumbRep = NULL;
        warning('The field AggPvalsNumbRep is set to NULL')
      }
    }
    if (pacotestOptions$Grouping=="TreeERC" || pacotestOptions$Grouping=="TreeEC" || pacotestOptions$Grouping=="TreeERCchi2" || pacotestOptions$Grouping=='TreeERCchi2WithEstimation' )
    {
      if (exists('ExpMinSampleSize', where=pacotestOptions))
      {
        CheckPosScalar(pacotestOptions$ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (exists('TrainingDataFraction', where=pacotestOptions))
      {
        CheckFraction(pacotestOptions$TrainingDataFraction,"TrainingDataFraction")
      }
      if (exists('aggInfo', where=pacotestOptions))
      {
        CheckAggInfo(pacotestOptions$aggInfo,"aggInfo")
      }
    }
  }
  else if (pacotestOptions$TestType=="EC")
  {
    CheckPosScalar(pacotestOptions$NumbBoot,"NumbBoot")
    CheckGrouping(pacotestOptions$Grouping,"Grouping")
    if (pacotestOptions$Grouping=="TreeERC" || pacotestOptions$Grouping=="TreeEC"  || pacotestOptions$Grouping=="TreeERCchi2" || pacotestOptions$Grouping=='TreeERCchi2WithEstimation' )
    {
      if (exists('ExpMinSampleSize', where=pacotestOptions))
      {
        CheckPosScalar(pacotestOptions$ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (exists('TrainingDataFraction', where=pacotestOptions))
      {
        CheckFraction(pacotestOptions$TrainingDataFraction,"TrainingDataFraction")
      }
      if (exists('aggInfo', where=pacotestOptions))
      {
        CheckAggInfo(pacotestOptions$aggInfo,"aggInfo")
      }
    }
    else
    {
      if (exists('ExpMinSampleSize', where=pacotestOptions) && !is.null(pacotestOptions$ExpMinSampleSize))
      {
        pacotestOptions$ExpMinSampleSize = NULL;
        warning('The field ExpMinSampleSize is set to NULL')
      }
      
      if (exists('TrainingDataFraction', where=pacotestOptions) && !is.null(pacotestOptions$TrainingDataFraction))
      {
        pacotestOptions$TrainingDataFraction = NULL;
        warning('The field TrainingDataFraction is set to NULL')
      }
      
      if (exists('aggInfo', where=pacotestOptions) && !is.null(pacotestOptions$aggInfo))
      {
        pacotestOptions$aggInfo = NULL;
        warning('The field aggInfo is set to NULL')
      }
    }
  }
  else if (pacotestOptions$TestType=="VI")
  {
    CheckPosScalar(pacotestOptions$NumbBoot,"NumbBoot")
  }
  else
  {
    stop("No valid pacotestOptions$TestType.")
  }
  
  return(pacotestOptions)
}
