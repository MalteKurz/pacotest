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
      if (Grouping<=2)
      {
        CondSetDim = ncol(W);
        out$DecisionTree = ExtractDecisionTree(CondSetDim, out$SplitVariable, out$SplitQuantile, out$SplitThreshold)
        if (SAtestOptions$DecisionTreePlot)
        {
        DecisionTreePlot(out$DecisionTree)
        }
      }
    }
    out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
  }
  else if (SAtestOptions$TestType=='EC')
  {
    Grouping = which(SAtestOptions$Grouping==c('TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'),arr.ind=TRUE)
    out = EC(Udata,W,SAtestOptions$NumbBoot,Grouping,SAtestOptions$ExpMinSampleSize,SAtestOptions$TrainingDataFraction)
    if (SAtestOptions$GroupedScatterplots)
    {
      GroupedScatterplot(out$Xdata,out$Ydata)
    }
    if (Grouping<=2)
    {
      CondSetDim = ncol(W);
      out$DecisionTree = ExtractDecisionTree(CondSetDim, out$SplitVariable, out$SplitQuantile, out$SplitThreshold)
      if (SAtestOptions$DecisionTreePlot)
      {
        DecisionTreePlot(out$DecisionTree)
      }
    }
    out[c("SplitVariable", "SplitQuantile", "SplitThreshold")] = NULL
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


ExtractDecisionTree = function(CondSetDim, SplitVariable, SplitQuantile, SplitThreshold)
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


DecisionTreePlot = function(DecisionTree)
{
  plot.new()
  par(mfrow=c(1,1))
  
  PossibleLeaves = c('LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR','L vs RL','L vs RR','LL vs R','LR vs R')
  
  textbox(c(0.45,0.55), 0.85, DecisionTree$CentralNode$Variable,col="blue",lwd=2)
  
  textbox(c(0.275,0.325), 0.775, paste('<=',format(DecisionTree$CentralNode$Threshold,digits=4),sep=''),box=FALSE)
  
  textbox(c(0.575,0.625), 0.775, paste('>',format(DecisionTree$CentralNode$Threshold,digits=4),sep=''),box=FALSE)
  
  
  if (!is.null(DecisionTree$LeftNode))
  {
    if (!grepl('LL',DecisionTree$LeavesForFinalComparison) || !grepl('LR',DecisionTree$LeavesForFinalComparison))
    {
      textbox(c(0.25,0.35), 0.6, DecisionTree$LeftNode$Variable,col="blue",lwd=2)
      lines(c(0.5,0.3),c(0.8,0.6),lwd=2)
    }
    else
    {
      textbox(c(0.25,0.35), 0.6, DecisionTree$LeftNode$Variable)
      lines(c(0.5,0.3),c(0.8,0.6))
    }
    
    textbox(c(0.125,0.175), 0.5, paste('<=',format(DecisionTree$LeftNode$Threshold,digits=4),sep=''),box=FALSE)
    
    textbox(c(0.325,0.375), 0.5, paste('<=',format(DecisionTree$LeftNode$Threshold,digits=4),sep=''),box=FALSE)
    
    if (!grepl('LL',DecisionTree$LeavesForFinalComparison))
    {
      textbox(c(0.15,0.25), 0.35, 'Group1',col="blue",lwd=2)
      lines(c(0.3,0.2),c(0.55,0.35),lwd=2)
    }
    else
    {
      textbox(c(0.15,0.25), 0.35, 'Group1')
      lines(c(0.3,0.2),c(0.55,0.35))
    }
    
    if (!grepl('LR',DecisionTree$LeavesForFinalComparison))
    {
      textbox(c(0.35,0.45), 0.35, 'Group2',col="blue",lwd=2)
      lines(c(0.3,0.4),c(0.55,0.35),lwd=2)
    }
    else
    {
      textbox(c(0.35,0.45), 0.35, 'Group2')
      lines(c(0.3,0.4),c(0.55,0.35))
    }
  }
  else
  {
    if (!grepl('L',DecisionTree$LeavesForFinalComparison))
    {
      textbox(c(0.25,0.35), 0.6, 'Group1',col="blue",lwd=2)
      lines(c(0.5,0.3),c(0.8,0.6),lwd=2)
    }
    else
    {
      textbox(c(0.25,0.35), 0.6, 'Group1')
      lines(c(0.5,0.3),c(0.8,0.6))
    }
  }
  
  if (!is.null(DecisionTree$RightNode))
  {
    if (!grepl('RL',DecisionTree$LeavesForFinalComparison) || !grepl('RR',DecisionTree$LeavesForFinalComparison))
    {
      textbox(c(0.65,0.75), 0.6, DecisionTree$RightNode$Variable,col="blue",lwd=2)
      lines(c(0.5,0.7),c(0.8,0.6),lwd=2)
    }
    else
    {
      textbox(c(0.65,0.75), 0.6, DecisionTree$RightNode$Variable)
      lines(c(0.5,0.7),c(0.8,0.6))
    }
    
    textbox(c(0.475,0.525), 0.5, paste('<=',format(DecisionTree$RightNode$Threshold,digits=4),sep=''),box=FALSE)
    
    textbox(c(0.725,0.775), 0.5, paste('>',format(DecisionTree$RightNode$Threshold,digits=4),sep=''),box=FALSE)
    
    if (!is.null(DecisionTree$RightNode))
    {
      if (!grepl('RL',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.75,0.85), 0.35, 'Group3',col="blue",lwd=2)
        lines(c(0.7,0.8),c(0.55,0.35),lwd=2)
      }
      else
      {
        textbox(c(0.75,0.85), 0.35, 'Group3')
        lines(c(0.7,0.8),c(0.55,0.35))
      }
      
      if (!grepl('RR',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.55,0.65), 0.35, 'Group4',col="blue",lwd=2)
        lines(c(0.7,0.6),c(0.55,0.35),lwd=2)
      }
      else
      {
        textbox(c(0.55,0.65), 0.35, 'Group4')
        lines(c(0.7,0.6),c(0.55,0.35))
      }
    }
    else
    {
      if (!grepl('RL',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.75,0.85), 0.35, 'Group2',col="blue",lwd=2)
        lines(c(0.7,0.8),c(0.55,0.35),lwd=2)
      }
      else
      {
        textbox(c(0.75,0.85), 0.35, 'Group2')
        lines(c(0.7,0.8),c(0.55,0.35))
      }
      
      if (!grepl('RR',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.55,0.65), 0.35, 'Group3',col="blue",lwd=2)
        lines(c(0.7,0.6),c(0.55,0.35),lwd=2)
      }
      else
      {
        textbox(c(0.55,0.65), 0.35, 'Group3')
        lines(c(0.7,0.6),c(0.55,0.35))
      }
    }
  }
  
  else
  {
    if (!is.null(DecisionTree$LeftNode))
    {
      if (!grepl('R',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.65,0.75), 0.6, 'Group3',col="blue",lwd=2)
        lines(c(0.5,0.7),c(0.8,0.6),lwd=2)
      }
      else
      {
        textbox(c(0.65,0.75), 0.6, 'Group3')
        lines(c(0.5,0.7),c(0.8,0.6))
      }
    }
    
    else
    {
      if (!grepl('R',DecisionTree$LeavesForFinalComparison))
      {
        textbox(c(0.65,0.75), 0.6, 'Group2',col="blue",lwd=2)
        lines(c(0.5,0.7),c(0.8,0.6),lwd=2)
      }
      else
      {
        textbox(c(0.65,0.75), 0.6, 'Group2')
        lines(c(0.5,0.7),c(0.8,0.6))
      }
      
    }
    
  }
  
}

  
  
  