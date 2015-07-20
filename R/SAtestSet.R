SAtestSet = function(SAtestOptions=list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5),TestType = 'ERC',Grouping, AggPvalsNumbRep, ExpMinSampleSize, TrainingDataFraction, GroupedScatterplots, DecisionTreePlot, NumbBoot){
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
    cat('       TrainingDataFraction: [ numeric between 0 and 1 ]\n\n')
    cat(' Options for TestType = [ EqualRankCorr | ERC ]:\n')
    cat('                   Grouping: [ TreeERC | TreeEC | SumMedian | SumThirdsI | SumThirdsII | ProdMedian | ProdThirdsI | ProdThirdsII ]\n')
    cat('            AggPvalsNumbRep: [ positive scalar ]\n')
    cat('        GroupedScatterplots: [ logical | 0 | 1 ]\n')
    cat('           DecisionTreePlot: [ logical | 0 | 1 ]\n')
    cat('           ExpMinSampleSize: [ positive scalar ]\n')
    cat('       TrainingDataFraction: [ numeric between 0 and 1 ]\n\n')
    cat(' Options for TestType = [ VecIndep | VI ]:\n')
    cat('                   NumbBoot: [ positive scalar ]\n\n')
}
if(missing(SAtestOptions) || (nargs()==1 && !is.list(SAtestOptions)))
{
  if (nargs()==1 && is.character(SAtestOptions))
  {
    TestType = SAtestOptions
  }
  if(missing(TestType))
  {
    stop('The field TestType has to be specified')
  }
  else
  {
    if (TestType=="ERC" || TestType == "EqualRankCorr")
    {
      SAtestOptions = list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5)
      if (!(missing(Grouping)))
      {
        SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(AggPvalsNumbRep)))
      {
        SAtestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
      }
      if (!(missing(GroupedScatterplots)))
      {
        SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
    }
    else if (TestType=="EC" || TestType == "EqualCop")
    {
      SAtestOptions = list(TestType = 'EC', NumbBoot = 1000, Grouping = 'SumMedian', GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5)
      if (!(missing(NumbBoot)))
      {
        SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
      if (!(missing(Grouping)))
      {
        SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(GroupedScatterplots)))
      {
        SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
    }
    else if (TestType=="VI" || TestType == "VecIndep")
    {
      SAtestOptions = list(TestType = 'VI',NumbBoot=1000)
      if (!(missing(NumbBoot)))
      {
        SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
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
  if (!is.list(SAtestOptions) || !exists('TestType', where=SAtestOptions))
  {
    stop('The provided SAtestOptions have to be given in a list which has TestType as member.')
  }
  if (!(missing(TestType)))
  {
    warning('After the change of the TestType all options are set to their default values except the explicitly stated ones.')
    
    if (TestType=="ERC" || TestType == "EqualRankCorr")
    {
      SAtestOptions = list(TestType = 'ERC', Grouping = 'TreeERC', AggPvalsNumbRep = 100, GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5)
      if (!(missing(Grouping)))
      {
        SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(AggPvalsNumbRep)))
      {
        SAtestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
      }
      if (!(missing(GroupedScatterplots)))
      {
        SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
    }
    else if (TestType=="EC" || TestType == "EqualCop")
    {
      SAtestOptions = list(TestType = 'EC', NumbBoot = 1000, Grouping = 'SumMedian', GroupedScatterplots = FALSE, DecisionTreePlot = FALSE, ExpMinSampleSize = 50, TrainingDataFraction = 0.5)
      if (!(missing(NumbBoot)))
      {
        SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
      if (!(missing(Grouping)))
      {
        SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
      }
      if (!(missing(GroupedScatterplots)))
      {
        SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
      }
      if (!(missing(DecisionTreePlot)))
      {
        SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
      }
      if (!(missing(ExpMinSampleSize)))
      {
        SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (!(missing(TrainingDataFraction)))
      {
        SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
      }
    }
    else if (TestType=="VI" || TestType == "VecIndep")
    {
      SAtestOptions = list(TestType = 'VI',NumbBoot=1000)
      if (!(missing(NumbBoot)))
      {
        SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
      }
    }
    else
    {
      stop("No valid TestType.")
    }
  }
  
  if (SAtestOptions$TestType=="ERC" || SAtestOptions$TestType == "EqualRankCorr")
  {
    SAtestOptions$TestType = "ERC"
    if (!(missing(Grouping)))
    {
      SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
    }
    if (!(missing(AggPvalsNumbRep)))
    {
      SAtestOptions$AggPvalsNumbRep = CheckPosScalar(AggPvalsNumbRep,"AggPvalsNumbRep")
    }
    if (!(missing(GroupedScatterplots)))
    {
      SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
    }
    if (!(missing(DecisionTreePlot)))
    {
      SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
    }
    if (!(missing(ExpMinSampleSize)))
    {
      SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
    }
    if (!(missing(TrainingDataFraction)))
    {
      SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
    }
  }
  else if (SAtestOptions$TestType=="EC" || SAtestOptions$TestType == "EqualCop")
  {
    SAtestOptions$TestType = "EC"
    if (!(missing(NumbBoot)))
    {
      SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
    }
    if (!(missing(Grouping)))
    {
      SAtestOptions$Grouping = CheckGrouping(Grouping,"Grouping")
    }
    if (!(missing(GroupedScatterplots)))
    {
      SAtestOptions$GroupedScatterplots = CheckLogical(GroupedScatterplots,"GroupedScatterplots")
    }
    if (!(missing(DecisionTreePlot)))
    {
      SAtestOptions$DecisionTreePlot = CheckLogical(DecisionTreePlot,"DecisionTreePlot")
    }
    if (!(missing(ExpMinSampleSize)))
    {
      SAtestOptions$ExpMinSampleSize = CheckPosScalar(ExpMinSampleSize,"ExpMinSampleSize")
    }
    if (!(missing(TrainingDataFraction)))
    {
      SAtestOptions$TrainingDataFraction = CheckFraction(TrainingDataFraction,"TrainingDataFraction")
    }
  }
  else if (SAtestOptions$TestType=="VI" || SAtestOptions$TestType == "VecIndep")
  {
    SAtestOptions$TestType = "VI"
    if (!(missing(NumbBoot)))
    {
      SAtestOptions$NumbBoot = CheckPosScalar(NumbBoot,"NumbBoot")
    }
  }
  else
  {
    stop("No valid TestType.")
  }
}
SAtestOptions = CheckSAtestOptions(SAtestOptions)
return(SAtestOptions)
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
  if (!(Value == 'SumMedian' || Value == 'SumThirdsI' || Value == 'SumThirdsII' || Value == 'ProdMedian' || Value == 'ProdThirdsI' || Value == 'ProdThirdsII' || Value == 'TreeEC' || Value == 'TreeERC'))
  {
    stop(paste("The option Grouping must be 'TreeEC', 'TreeERC' 'SumMedian', 'SumThirdsI', 'SumThirdsII', 'ProdMedian', 'ProdThirdsI' or 'ProdThirdsII'"))
  }
  return(Value)
}

CheckSAtestOptions = function(SAtestOptions)
{
  
  if (SAtestOptions$TestType=="ERC")
  {
    CheckGrouping(SAtestOptions$Grouping,"Grouping")
      if (SAtestOptions$Grouping=="TreeERC")
      {
        if (exists('AggPvalsNumbRep', where=SAtestOptions) && SAtestOptions$AggPvalsNumbRep >1 && exists('GroupedScatterplots', where=SAtestOptions) && SAtestOptions$GroupedScatterplots)
        {
          SAtestOptions$GroupedScatterplots = FALSE
          warning('GroupedScatterplots is set to FALSE as AggPvalsNumbRep is larger than one')
        }
        if (exists('AggPvalsNumbRep', where=SAtestOptions))
        {
          CheckPosScalar(SAtestOptions$AggPvalsNumbRep,"AggPvalsNumbRep")
        }
      }
    else if (exists('AggPvalsNumbRep', where=SAtestOptions) && !is.null(SAtestOptions$AggPvalsNumbRep))
    {
      SAtestOptions$AggPvalsNumbRep = NULL;
      warning('The field AggPvalsNumbRep is set to NULL')
    }
    if (SAtestOptions$Grouping=="TreeERC" || SAtestOptions$Grouping=="TreeEC")
    {
      if (exists('ExpMinSampleSize', where=SAtestOptions))
      {
        CheckPosScalar(SAtestOptions$ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (exists('TrainingDataFraction', where=SAtestOptions))
      {
        CheckFraction(SAtestOptions$TrainingDataFraction,"TrainingDataFraction")
      }
    }
  }
  else if (SAtestOptions$TestType=="EC")
  {
    CheckPosScalar(SAtestOptions$NumbBoot,"NumbBoot")
    CheckGrouping(SAtestOptions$Grouping,"Grouping")
    if (SAtestOptions$Grouping=="TreeERC" || SAtestOptions$Grouping=="TreeEC")
    {
      if (exists('ExpMinSampleSize', where=SAtestOptions))
      {
        CheckPosScalar(SAtestOptions$ExpMinSampleSize,"ExpMinSampleSize")
      }
      if (exists('TrainingDataFraction', where=SAtestOptions))
      {
        CheckFraction(SAtestOptions$TrainingDataFraction,"TrainingDataFraction")
      }
    }
  }
  else if (SAtestOptions$TestType=="VI")
  {
    CheckPosScalar(SAtestOptions$NumbBoot,"NumbBoot")
  }
  else
  {
    stop("No valid SAtestOptions$TestType.")
  }
  
  return(SAtestOptions)
}
