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
    
    if (!(pacotestOptions$withEstUncert))
    {
      data = matrix()
      svcmDataFrame = data.frame()
      cPitData = matrix()
    }
    
    if (grouping<2  && pacotestOptions$sizeKeepingMethod=='splitTrainEvaluate' && pacotestOptions$aggPvalsNumbRep > 1)
    {
      finalComparison = which(pacotestOptions$finalComparison==c('pairwiseMax','all'))
      
      W = addAggInfo(W,pacotestOptions$aggInfo);
      
      if (pacotestOptions$testType=='ECOV')
      {
        out = ECOV(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$aggPvalsNumbRep,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
      }
      else
      {
        out = ECORR(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$aggPvalsNumbRep,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
      }
      
      condSetNames = names(W);
      out$DecisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold)
    }
    else
    {
      if (grouping > 2)
      {
        
        if (pacotestOptions$testType=='ECOV')
        {
          out = ECOV(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, 1, data, svcmDataFrame, cPitData, 0, 1, 1)
        }
        else
        {
          out = ECORR(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, 1, data, svcmDataFrame, cPitData, 0, 1, 1)
        }
        
      }
      else
      {
        finalComparison = which(pacotestOptions$finalComparison==c('pairwiseMax','all'))
        
        if (pacotestOptions$sizeKeepingMethod=='splitTrainEvaluate')
        {
        W = addAggInfo(W,pacotestOptions$aggInfo);
        
        if (pacotestOptions$testType=='ECOV')
        {
          out = ECOV(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,0,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
        }
        else
        {
          out = ECORR(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,0,pacotestOptions$expMinSampleSize,pacotestOptions$trainingDataFraction)
        }
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
          
          if (pacotestOptions$testType=='ECOV')
          {
            out = ECOVwithPenalty(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$expMinSampleSize, pacotestOptions$penaltyParams[1], pacotestOptions$penaltyParams[2], gamma0Partition)
          }
          else
          {
            out = ECORRwithPenalty(as.matrix(Udata), as.matrix(W),grouping, pacotestOptions$withEstUncert, finalComparison, data, svcmDataFrame, cPitData,pacotestOptions$expMinSampleSize, pacotestOptions$penaltyParams[1], pacotestOptions$penaltyParams[2], gamma0Partition)
          }
        }
        
      }
      if (grouping<=2)
      {
        condSetNames = names(W)
        out$DecisionTree = ExtractDecisionTree(condSetNames, out$SplitVariable, out$SplitQuantile, out$SplitThreshold, pacotestOptions$finalComparison)
        if (pacotestOptions$decisionTreePlot)
        {
          if (!requireNamespace("plotrix", quietly = TRUE))
          {
            stop("plotrix needed to obtain decision tree plots. Please install it.",
                 call. = FALSE)
          }
          else
          {
            decisionTreePlot(out$DecisionTree)
          }
        }
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
      
      if (pacotestOptions$decisionTreePlot)
      {
        if (!requireNamespace("plotrix", quietly = TRUE)) {
          stop("plotrix needed to obtain decision tree plots. Please install it.",
               call. = FALSE)
          decisionTreePlot(out$DecisionTree)
        }
      }
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


GroupedScatterplot = function(Udata, W, decisionTree)
{
  
  dataLabels = names(Udata)
  
  names(Udata) = c("V1", "V2")
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
  
  
  p1 = ggplot(Udata,
              aes(V1, V2)) +
    geom_point() +
    scale_colour_manual(values = cbbPalette[1]) + 
    scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(dataLabels[1]) + 
    ylab(dataLabels[2]) + 
    coord_fixed() +
    theme_grey(base_size = 20) + 
    theme(panel.margin = unit(4, "lines"), legend.position = "none")
  
  
  indCentralSplit = W[,decisionTree$CentralNode$Variable]<=decisionTree$CentralNode$Threshold
  
  centralSplit = vector(length=length(indCentralSplit))
  levelNames = vector(length=2)
  
  rho = momentBasedCorr(Udata[indCentralSplit==1,])
  levelNames[1] = paste(decisionTree$CentralNode$Variable, " <= ", format(decisionTree$CentralNode$Threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  centralSplit[indCentralSplit==1] = levelNames[1]
  
  rho = momentBasedCorr(Udata[indCentralSplit==0,])
  levelNames[2] = paste(decisionTree$CentralNode$Variable, " > ", format(decisionTree$CentralNode$Threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  centralSplit[indCentralSplit==0] = levelNames[2]
  
  centralSplit = factor(centralSplit, levelNames)
  
  
  data = transform(Udata, centralSplit = centralSplit)
  
  
  p2 = ggplot(data,
              aes(V1, V2, colour=centralSplit)) +
    geom_point() +
    scale_colour_manual(values = cbbPalette[2:3]) + 
    scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(dataLabels[1]) + 
    ylab(dataLabels[2]) + 
    coord_fixed() +
    facet_wrap(~centralSplit) +
    theme_grey(base_size = 20) + 
    theme(panel.margin = unit(15, "lines"), legend.position = "none")
  
  
  
  
  p3 = ggplot()
  p4 = ggplot()
  
  if (!is.null(decisionTree$LeftNode))
  {
    
    indLeftSplit = W[indCentralSplit==1,decisionTree$LeftNode$Variable]<=decisionTree$LeftNode$Threshold
    
    leftSplit = vector(length=length(indLeftSplit))
    levelNames = vector(length=2)
    
    xx = Udata[indCentralSplit==1,]
    rho = momentBasedCorr(xx[indLeftSplit==1,])
    levelNames[1] = paste(decisionTree$LeftNode$Variable, " <= ", format(decisionTree$LeftNode$Threshold,dig=4),
                          "\nrho = ", format(rho,dig=4))
    leftSplit[indLeftSplit==1] = levelNames[1]
    
    rho = momentBasedCorr(xx[indLeftSplit==0,])
    levelNames[2] = paste(decisionTree$LeftNode$Variable, " > ", format(decisionTree$LeftNode$Threshold,dig=4),
                          "\nrho = ", format(rho,dig=4))
    leftSplit[indLeftSplit==0] = levelNames[2]
    
    leftSplit = factor(leftSplit, levelNames)
    
    leftData = transform(Udata[indCentralSplit==1,], leftSplit = leftSplit)
    row.names(leftData) = NULL
    
    p3 = ggplot(leftData,
                aes(V1, V2, colour=leftSplit)) +
      geom_point() +
      scale_colour_manual(values = cbbPalette[4:5]) + 
      scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      xlab(dataLabels[1]) + 
      ylab(dataLabels[2]) + 
      coord_fixed() +
      facet_wrap(~leftSplit) +
      theme_grey(base_size = 20) + 
      theme(panel.margin = unit(2, "lines"), legend.position = "none")
    
    
  }
  
  if (!is.null(decisionTree$RightNode))
  {
    
    indRightSplit = W[indCentralSplit==0,decisionTree$RightNode$Variable]<=decisionTree$RightNode$Threshold
    
    rightSplit = vector(length=length(indRightSplit))
    levelNames = vector(length=2)
    
    xx = Udata[indCentralSplit==0,]
    rho = momentBasedCorr(xx[indRightSplit==1,])
    levelNames[1] = paste(decisionTree$RightNode$Variable, " <= ", format(decisionTree$RightNode$Threshold,dig=4),
                          "\nrho = ", format(rho,dig=4))
    rightSplit[indRightSplit==1] = levelNames[1]
    
    rho = momentBasedCorr(xx[indRightSplit==0,])
    levelNames[2] = paste(decisionTree$RightNode$Variable, " > ", format(decisionTree$RightNode$Threshold,dig=4),
                          "\nrho = ", format(rho,dig=4))
    rightSplit[indRightSplit==0] = levelNames[2]
    
    rightSplit = factor(rightSplit, levelNames)
    
    rightData = transform(Udata[indCentralSplit==0,], rightSplit = rightSplit)
    row.names(rightData) = NULL
    
    
    p4 = ggplot(rightData,
                aes(V1, V2, colour=rightSplit)) +
      geom_point() +
      scale_colour_manual(values = cbbPalette[6:7]) + 
      scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      xlab(dataLabels[1]) + 
      ylab(dataLabels[2]) + 
      coord_fixed() +
      facet_wrap(~rightSplit) +
      theme_grey(base_size = 20) + 
      theme(panel.margin = unit(2, "lines"), legend.position = "none")
    
  }
  
  lM = matrix(NA, nrow=3, ncol=2)
  lM[1,] = 1
  lM[2,] = 2
  lM[3,1] = 3
  lM[3,2] = 4
  grid.arrange(p1, p2, p3, p4, layout_matrix = lM)
}

momentBasedCorr = function(data)
{
  mu1 = mean(data[,1])
  mu2 = mean(data[,2])
  
  sigma1 = mean((data[,1]-mu1)^2)
  sigma2 = mean((data[,2]-mu2)^2)
  
  rho = mean((data[,1]-mu1)*
               (data[,2]-mu2)/
               sqrt(sigma1*sigma2))
  
  return(rho)
}

  
  
# GroupedScatterplot = function(Xdata,Ydata)
# {
#   par(mfrow=c(2,2))
#   plot(Xdata[,1],Xdata[,2],xlab='U',ylab='V')
#   plot(Ydata[,1],Ydata[,2],xlab='U',ylab='V')
#   plot(qnorm(Xdata[,1]),qnorm(Xdata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
#   plot(qnorm(Ydata[,1]),qnorm(Ydata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
# }


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



decisionTreePlot = function(DecisionTree)
{
  plot.new()
  par(mfrow=c(1,1))
  
  PossibleLeaves = c('LL vs LR','LL vs RL','LL vs RR','LR vs RL','LR vs RR','RL vs RR','L vs RL','L vs RR','LL vs R','LR vs R')
  
  plotrix::textbox(c(0.425,0.575), 1, DecisionTree$CentralNode$Variable,col="blue",lwd=2,justify='c')
  
  plotrix::textbox(c(0.1,0.3), 0.8, paste('<=',format(DecisionTree$CentralNode$Threshold,digits=4),sep=''),box=FALSE,justify='r')
  
  plotrix::textbox(c(0.7,0.9), 0.8, paste('>',format(DecisionTree$CentralNode$Threshold,digits=4),sep=''),box=FALSE,justify='l')
  
  
  if (!is.null(DecisionTree$LeftNode))
  {
    if (grepl('LL',DecisionTree$LeavesForFinalComparison) || grepl('LR',DecisionTree$LeavesForFinalComparison))
    {
      plotrix::textbox(c(0.125,0.275), 0.5, DecisionTree$LeftNode$Variable,col="blue",lwd=2,justify='c')
      lines(c(0.5,0.2),c(0.95,0.5),lwd=2)
    }
    else
    {
      plotrix::textbox(c(0.125,0.275), 0.6, DecisionTree$LeftNode$Variable,justify='c')
      lines(c(0.5,0.2),c(0.95,0.5))
    }
    
    plotrix::textbox(c(-0.025,0.125), 0.3, paste('<=',format(DecisionTree$LeftNode$Threshold,digits=4),sep=''),box=FALSE,justify='r')
    
    plotrix::textbox(c(0.275,0.475), 0.3, paste('>',format(DecisionTree$LeftNode$Threshold,digits=4),sep=''),box=FALSE,justify='l')
    
    if (grepl('LL',DecisionTree$LeavesForFinalComparison))
    {
      plotrix::textbox(c(-0.025,0.125), 0.05, 'Group1',col="blue",lwd=2,justify='c')
      lines(c(0.2,0.05),c(0.45,0.05),lwd=2)
    }
    else
    {
      plotrix::textbox(c(-0.025,0.125), 0.05, 'Group1',justify='c')
      lines(c(0.2,0.05),c(0.45,0.05))
    }
    
    if (grepl('LR',DecisionTree$LeavesForFinalComparison))
    {
      plotrix::textbox(c(0.275,0.425), 0.05, 'Group2',col="blue",lwd=2,justify='c')
      lines(c(0.2,0.35),c(0.45,0.05),lwd=2)
    }
    else
    {
      plotrix::textbox(c(0.275,0.425), 0.05, 'Group2',justify='c')
      lines(c(0.2,0.35),c(0.45,0.05))
    }
  }
  else
  {
    if (grepl('L',DecisionTree$LeavesForFinalComparison))
    {
      plotrix::textbox(c(0.125,0.275), 0.5, 'Group1',col="blue",lwd=2,justify='c')
      lines(c(0.5,0.2),c(0.95,0.5),lwd=2)
    }
    else
    {
      plotrix::textbox(c(0.125,0.275), 0.5, 'Group1',justify='c')
      lines(c(0.5,0.2),c(0.95,0.5))
    }
  }
  
  if (!is.null(DecisionTree$RightNode))
  {
    if (grepl('RL',DecisionTree$LeavesForFinalComparison) || grepl('RR',DecisionTree$LeavesForFinalComparison))
    {
      plotrix::textbox(c(0.725,0.875), 0.5, DecisionTree$RightNode$Variable,col="blue",lwd=2,justify='c')
      lines(c(0.5,0.8),c(0.95,0.5),lwd=2)
    }
    else
    {
      plotrix::textbox(c(0.725,0.875), 0.5, DecisionTree$RightNode$Variable,justify='c')
      lines(c(0.5,0.8),c(0.95,0.5))
    }
    
    plotrix::textbox(c(0.525,0.725), 0.3, paste('<=',format(DecisionTree$RightNode$Threshold,digits=4),sep=''),box=FALSE,justify='r')
    
    plotrix::textbox(c(0.875,1.075), 0.3, paste('>',format(DecisionTree$RightNode$Threshold,digits=4),sep=''),box=FALSE,justify='l')
    
    if (!is.null(DecisionTree$RightNode))
    {
      if (grepl('RL',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.575,0.725), 0.05, 'Group3',col="blue",lwd=2,justify='c')
        lines(c(0.8,0.65),c(0.45,0.05),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.575,0.725), 0.05, 'Group3',justify='c')
        lines(c(0.8,0.65),c(0.45,0.05))
      }
      
      if (grepl('RR',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.875,1.025), 0.05, 'Group4',col="blue",lwd=2,justify='c')
        lines(c(0.8,0.95),c(0.45,0.05),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.875,1.025), 0.05, 'Group4',justify='c')
        lines(c(0.8,0.95),c(0.45,0.05))
      }
    }
    else
    {
      if (grepl('RL',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.575,0.725), 0.05, 'Group2',col="blue",lwd=2,justify='c')
        lines(c(0.8,0.65),c(0.45,0.05),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.575,0.725), 0.05, 'Group2',justify='c')
        lines(c(0.8,0.65),c(0.45,0.05))
      }
      
      if (grepl('RR',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.875,1.025), 0.05, 'Group3',col="blue",lwd=2,justify='c')
        lines(c(0.8,0.95),c(0.45,0.05),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.875,1.025), 0.05, 'Group3',justify='c')
        lines(c(0.8,0.95),c(0.45,0.05))
      }
    }
  }
  
  else
  {
    if (!is.null(DecisionTree$LeftNode))
    {
      if (grepl('R',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.725,0.875), 0.5, 'Group3',col="blue",lwd=2,justify='c')
        lines(c(0.5,0.8),c(0.95,0.5),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.725,0.875), 0.5, 'Group3',justify='c')
        lines(c(0.5,0.8),c(0.95,0.5))
      }
    }
    
    else
    {
      if (grepl('R',DecisionTree$LeavesForFinalComparison))
      {
        plotrix::textbox(c(0.725,0.875), 0.5, 'Group2',col="blue",lwd=2,justify='c')
        lines(c(0.5,0.8),c(0.95,0.5),lwd=2)
      }
      else
      {
        plotrix::textbox(c(0.725,0.875), 0.5, 'Group2',justify='c')
        lines(c(0.5,0.8),c(0.95,0.5))
      }
      
    }
    
  }
  
}

  
  
  