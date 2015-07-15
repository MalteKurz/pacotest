SAtest = function(Udata,W,SAtestOptions){
  
    SAtestOptions = SAtestSet(SAtestOptions)
    Udata = as.matrix(Udata)
    W = as.matrix(W)
  
  if (SAtestOptions$TestType=='ERC')
  {
    if (SAtestOptions$Grouping=='TreeERC' && SAtestOptions$AggPvalsNumbRep > 1)
    {
      # third list element are the p-values that have been aggregated.
      Grouping = 0
      out = ERC(Udata,W,Grouping,SAtestOptions$AggPvalsNumbRep,SAtestOptions$ExpMinSampleSize,SAtestOptions$TrainingDataFraction)
    }
    else
    {
      Grouping = which(SAtestOptions$Grouping==c('TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'),arr.ind=TRUE)
      out = ERC(Udata,W,Grouping,0,SAtestOptions$ExpMinSampleSize,SAtestOptions$TrainingDataFraction)
      if (SAtestOptions$GroupedScatterplots)
      {
        GroupedScatterplot(out$Xdata,out$Ydata)
      }
    }
  }
  else if (SAtestOptions$TestType=='EC')
  {
    Grouping = which(SAtestOptions$Grouping==c('TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'),arr.ind=TRUE)
    out = EC(Udata,W,SAtestOptions$NumbBoot,Grouping,SAtestOptions$ExpMinSampleSize,SAtestOptions$TrainingDataFraction)
    if (SAtestOptions$GroupedScatterplots)
    {
      GroupedScatterplot(out$Xdata,out$Ydata)
    }
  }
  else if (SAtestOptions$TestType=='VI')
  {
    out = VI(Udata,W,SAtestOptions$NumbBoot)
  }
  else
  {
    stop('Unknown TestType')
  }
  return(out)
}

GroupedScatterplot = function(Xdata,Ydata)
{
  par(mfrow=c(2,2))
  plot(Xdata[,1],Xdata[,2],xlab='U',ylab='V')
  plot(Ydata[,1],Ydata[,2],xlab='U',ylab='V')
  plot(qnorm(Xdata[,1]),qnorm(Xdata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
  plot(qnorm(Ydata[,1]),qnorm(Ydata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
}


ExtractDecisionTree = function(CondSetDim,SplitVariable, SplitQuantile, SplitThreshold)
{
  
  Node = list(Variable=NULL,Quantile=NULL,Threshold=NULL)
  DecisionTree = list(CentralNode=Node,LeftNode=NULL,RightNode=NULL,LeavesForFinalComparison=NULL)
  Quantiles = c(25,50,75)
  
  for (i in 1:nrow(SplitVariable))
  {
    
    if ((SplitVariable[1]+1) <= CondSetDim)
    {
      DecisionTree$CentralNode$Variable = paste('W', SplitVariable[1]+1, sep = '')
    }
    else
    {
      DecisionTree$CentralNode$Variable = 'Mean(W)';
    }
    DecisionTree$CentralNode$Quantile = paste('Q', Quantiles[SplitQuantile[1]+1], sep = '')
    DecisionTree$CentralNode$Threshold = SplitThreshold[1];
    
    if (SplitVariable[4] == 6) # The one split only case
    {
      DecisionTree$LeavesForFinalComparison = 'L vs R';
    }
    else
    {
      if (SplitVariable[4] < 6)
      {
        
        
        if ((SplitVariable[2]+1) <= CondSetDim)
        {
          DecisionTree$LeftNode$Variable = paste('W', SplitVariable[2]+1, sep = '')
        }
        else
        {
          DecisionTree$LeftNode$Variable = 'Mean(W)';
        }
        DecisionTree$LeftNode$Quantile = paste('Q', Quantiles[SplitQuantile[2]+1], sep = '')
        DecisionTree$LeftNode$Threshold = SplitThreshold[2];
        
        if ((SplitVariable[3]+1) <= CondSetDim)
        {
          DecisionTree$RightNode$Variable = paste('W', SplitVariable[3]+1, sep = '')
        }
        else
        {
          DecisionTree$RightNode$Variable = 'Mean(W)';
        }
        DecisionTree$RightNode$Quantile = paste('Q', Quantiles[SplitQuantile[3]+1], sep = '')
        DecisionTree$RightNode$Threshold = SplitThreshold[3];
        
        PossibleLeaves = c('LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR')
        
        DecisionTree$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4]+1]
      }
      else
      {
        if (SplitVariable[4]<13)
        {
          
          if ((SplitVariable[2]+1) <= CondSetDim)
          {
            DecisionTree$LeftNode$Variable = paste('W', SplitVariable[2]+1, sep = '')
          }
          else
          {
            DecisionTree$LeftNode$Variable = 'Mean(W)';
          }
          DecisionTree$LeftNode$Quantile = paste('Q', Quantiles[SplitQuantile[2]+1], sep = '')
          DecisionTree$LeftNode$Threshold = SplitThreshold[2];
          
          PossibleLeaves = c('LL vs LR','LL vs R','LR vs R','','','')
          
          DecisionTree$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4]-10+1]
        }
        else
        {
          if ((SplitVariable[3]+1) <= CondSetDim)
          {
            DecisionTree$RightNode$Variable = paste('W', SplitVariable[3]+1, sep = '')
          }
          else
          {
            DecisionTree$RightNode$Variable = 'Mean(W)';
          }
          DecisionTree$RightNode$Quantile = paste('Q', Quantiles[SplitQuantile[3]+1], sep = '')
          DecisionTree$RightNode$Threshold = SplitThreshold[3];
          
          PossibleLeaves = c('','','','L vs RL','L vs RR','RL vs RR')
          
          DecisionTree$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4]-10+1]
        }
      }
    }
  }
  
  
  return(DecisionTree)
}

