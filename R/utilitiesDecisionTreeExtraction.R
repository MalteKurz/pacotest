
ExtractDecisionTree = function(condSetNames, SplitVariable, SplitQuantile, SplitThreshold, finalComparison)
{
  
  Node = list(Variable=NULL,Quantile=NULL,Threshold=NULL)
  DecisionTree = rep(list(list(CentralNode=Node,LeftNode=NULL,RightNode=NULL,LeavesForFinalComparison=NULL)), ncol(SplitVariable))
  Quantiles = c(25,50,75)
  
  for (i in 1:ncol(SplitVariable))
  {
    
    DecisionTree[[i]]$CentralNode$Variable = condSetNames[SplitVariable[1,i]+1]
    DecisionTree[[i]]$CentralNode$Quantile = paste('Q', Quantiles[SplitQuantile[1,i]+1], sep = '')
    DecisionTree[[i]]$CentralNode$Threshold = SplitThreshold[1,i];
    
    if (SplitVariable[4,i] == 6) # The one split only case
    {
      DecisionTree[[i]]$LeavesForFinalComparison = 'L vs R';
    }
    else
    {
      if (SplitVariable[4,i] < 6)
      {
        
        
        DecisionTree[[i]]$LeftNode$Variable = condSetNames[SplitVariable[2,i]+1]
        DecisionTree[[i]]$LeftNode$Quantile = paste('Q', Quantiles[SplitQuantile[2,i]+1], sep = '')
        DecisionTree[[i]]$LeftNode$Threshold = SplitThreshold[2,i];
        
        DecisionTree[[i]]$RightNode$Variable = condSetNames[SplitVariable[3,i]+1]
        DecisionTree[[i]]$RightNode$Quantile = paste('Q', Quantiles[SplitQuantile[3,i]+1], sep = '')
        DecisionTree[[i]]$RightNode$Threshold = SplitThreshold[3,i];
        
        PossibleLeaves = c('LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR')
        
        DecisionTree[[i]]$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4,i]+1]
      }
      else
      {
        if (SplitVariable[4,i]<13)
        {
          
          DecisionTree[[i]]$LeftNode$Variable = condSetNames[SplitVariable[2,i]+1]
          DecisionTree[[i]]$LeftNode$Quantile = paste('Q', Quantiles[SplitQuantile[2,i]+1], sep = '')
          DecisionTree[[i]]$LeftNode$Threshold = SplitThreshold[2,i];
          
          PossibleLeaves = c('LL vs LR','LL vs R','LR vs R','','','')
          
          DecisionTree[[i]]$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4,i]-10+1]
        }
        else
        {
          DecisionTree[[i]]$RightNode$Variable = condSetNames[SplitVariable[3,i]+1]
          DecisionTree[[i]]$RightNode$Quantile = paste('Q', Quantiles[SplitQuantile[3,i]+1], sep = '')
          DecisionTree[[i]]$RightNode$Threshold = SplitThreshold[3,i];
          
          PossibleLeaves = c('','','','L vs RL','L vs RR','RL vs RR')
          
          DecisionTree[[i]]$LeavesForFinalComparison = PossibleLeaves[SplitVariable[4,i]-10+1]
        }
      }
    }
    
    if (finalComparison=="all")
    {
      DecisionTree[[i]]$LeavesForFinalComparison = "all"
    }
    
  }
  
  if (ncol(SplitVariable) == 1)
  {
    DecisionTree = DecisionTree[[1]]
  }
  
  return(DecisionTree)
}


decTreeFromFixedGrouping = function(grouping, W)
{
  
  Node = list(Variable=NULL,Quantile=NULL,Threshold=NULL)
  decisionTree = list(CentralNode=Node,LeftNode=NULL,RightNode=NULL,LeavesForFinalComparison=NULL)
  
  if (grepl('Sum',grouping))
  {
    
    if (dim(W)[2] > 1)
    {
      varName = paste("Sum(", paste(names(W),collapse=", "), ")", sep="")
      
      xx = as.data.frame(rowMeans(W))
      names(xx) = varName
      
      W = cbind(W,xx)
    }
    else
    {
      varName = names(W)
    }
  }
  
  if (grepl('Prod',grouping))
  {
    
    if (dim(W)[2] > 1)
    {
      varName = paste("Prod(", paste(names(W),collapse=", "), ")", sep="")
      
      xx = as.data.frame(apply(W, 1, prod))
      names(xx) = varName
      
      W = cbind(W,xx)
    }
    else
    {
      varName = names(W)
    }
  }
  
  decisionTree$CentralNode$Variable = varName
  
  if (grepl('Median',grouping))
  {
    decisionTree$CentralNode$Quantile = "Q50"
    decisionTree$CentralNode$Threshold = quantile(W[,varName], 0.5)
    
    decisionTree$LeavesForFinalComparison = "L vs R"
    
  }
  
  if (grepl('Thirds',grouping))
  {
    decisionTree$CentralNode$Quantile = "Q33.33"
    decisionTree$CentralNode$Threshold = quantile(W[,varName], 1/3)
    
    decisionTree$RightNode$Variable = varName
    decisionTree$RightNode$Quantile = "Q50"
    decisionTree$RightNode$Threshold = quantile(W[,varName], 2/3)
    
  }
  
  if (grouping == 'SumThirdsI' || grouping == 'ProdThirdsI')
  {
    decisionTree$LeavesForFinalComparison = "all"
    
  }
  
  if (grouping == 'SumThirdsII' || grouping == 'ProdThirdsII')
  {
    decisionTree$LeavesForFinalComparison = "L vs RL"
    
  }
  
  if (grouping == 'SumThirdsIII' || grouping == 'ProdThirdsIII')
  {
    decisionTree$LeavesForFinalComparison = "L vs RR"
    
  }
  
  if(grouping == 'SumQuartiles' || grouping == 'ProdQuartiles')
  {
    decisionTree$CentralNode$Quantile = "Q50"
    decisionTree$CentralNode$Threshold = quantile(W[,varName], 1/2)
    
    decisionTree$LeftNode$Variable = varName
    decisionTree$LeftNode$Quantile = "Q50"
    decisionTree$LeftNode$Threshold = quantile(W[,varName], 1/4)
    
    decisionTree$RightNode$Variable = varName
    decisionTree$RightNode$Quantile = "Q50"
    decisionTree$RightNode$Threshold = quantile(W[,varName], 3/4)
    
    decisionTree$LeavesForFinalComparison = "all"
    
  }
  
  return(list(decisionTree = decisionTree, W = W))
  
}

