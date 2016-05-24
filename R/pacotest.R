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
        if (pacotestOptions$decisionTreePlot)
        {
          pDecTree = decisionTreePlot(out$DecisionTree)
          print(pDecTree)
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


GroupedScatterplot = function(Udata, W, decisionTree)
{
#   if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("gridExtra", quietly = TRUE))
#   {
#     stop("ggplot2 and gridExtra needed to obtain grouped scatter plots. Please install it.",
#          call. = FALSE)
#   }
#   else{
  
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
  
  
  xx = getGroupedPlot(Udata, W, decisionTree$CentralNode$Variable, decisionTree$CentralNode$Threshold, dataLabels)
  
  indCentralSplit = xx$indVector
  p2 = xx$p + 
    scale_colour_manual(values = cbbPalette[2:3]) + 
    theme(panel.margin = unit(30, "lines"), legend.position = "none")
  
  p2<-ggplotGrob(p2)
  p2[[1]]$axis_l2 = p2[[1]]$axis_l1
  
  p3 = ggplot()
  p4 = ggplot()
    
  if (!is.null(decisionTree$LeftNode))
  {
    xx = getGroupedPlot(Udata[indCentralSplit==1,],
                        W[indCentralSplit==1,],
                        decisionTree$LeftNode$Variable, decisionTree$LeftNode$Threshold, dataLabels)
    
    p3 = xx$p + 
      scale_colour_manual(values = cbbPalette[4:5]) + 
      theme(panel.margin = unit(10, "lines"), legend.position = "none")
    
    p3<-ggplotGrob(p3)
    p3[[1]]$axis_l2 = p3[[1]]$axis_l1
    
  }
  
  if (!is.null(decisionTree$RightNode))
  {
    xx = getGroupedPlot(Udata[indCentralSplit==0,],
                        W[indCentralSplit==0,],
                        decisionTree$RightNode$Variable, decisionTree$RightNode$Threshold, dataLabels)
    
    p4 = xx$p + 
      scale_colour_manual(values = cbbPalette[6:7]) + 
      theme(panel.margin = unit(10, "lines"), legend.position = "none")
    
    p4<-ggplotGrob(p4)
    p4[[1]]$axis_l2 = p4[[1]]$axis_l1
    
  }
  
    
  if (is.null(decisionTree$LeftNode) && is.null(decisionTree$RightNode))
  {
    lM = matrix(NA, nrow=2, ncol=2)
    lM[1,] = 1
    lM[2,] = 2
    grid.arrange(p1, p2, layout_matrix = lM)
  }
  else
  {
    if (is.null(decisionTree$LeftNode))
    {
      lM = matrix(NA, nrow=3, ncol=2)
      lM[1,] = 1
      lM[2,] = 2
      lM[3,1] = NA
      lM[3,2] = 3
      grid.arrange(p1, p2, p4, layout_matrix = lM)
    }
    else
    {
      if (is.null(decisionTree$RightNode))
      {
        lM = matrix(NA, nrow=3, ncol=2)
        lM[1,] = 1
        lM[2,] = 2
        lM[3,1] = 3
        lM[3,2] = NA
        grid.arrange(p1, p2, p3, layout_matrix = lM)
      }
      else
      {
        lM = matrix(NA, nrow=3, ncol=2)
        lM[1,] = 1
        lM[2,] = 2
        lM[3,1] = 3
        lM[3,2] = 4
        grid.arrange(p1, p2, p3, p4, layout_matrix = lM)
      }
    }
  }
  

  #}
  
}


getGroupedPlot = function(Udata, W, variable, threshold, dataLabels)
{
  indVector = W[,variable]<=threshold
  
  split = vector(length=length(indVector))
  levelNames = vector(length=2)
  
  rho = momentBasedCorr(Udata[indVector==1,])
  levelNames[1] = paste(variable, " <= ", format(threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  split[indVector==1] = levelNames[1]
  
  rho = momentBasedCorr(Udata[indVector==0,])
  levelNames[2] = paste(variable, " > ", format(threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  split[indVector==0] = levelNames[2]
  
  split = factor(split, levelNames)
  
  
  data = transform(Udata, split = split)
  
  
  p = ggplot(data,
             aes(V1, V2, colour=split)) +
    geom_point(data = transform(data, split = NULL), colour = "grey85") + 
    geom_point() +  
    facet_wrap(~split, scales="fixed") +
    coord_equal() +
    scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab("") + 
    ylab("") + 
    theme_grey(base_size = 20)
  
  
  return(list(p=p, indVector = indVector))
  
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
  
  p  = ggplot()
  
  
  p = p + annotate("text",label=DecisionTree$CentralNode$Variable, x=0.5, y= 0.9, size=20) + 
    annotate("rect", xmin = 0.3, xmax = 0.7, ymin = 0.8, ymax = 1, color="black", alpha=0.2) 
  
  
  p = p + annotate("text", x=-0.05, y= 0.5, size=20,
                   label=paste('<=',format(DecisionTree$CentralNode$Threshold,digits=4),sep='')) +
    annotate("segment", x = 0.5, xend = -0.1, y = 0.8, yend = 0.2, colour = "black")
  
  p = p + annotate("text", x=1.05, y= 0.5, size=20,
                   label=paste('>',format(DecisionTree$CentralNode$Threshold,digits=4),sep='')) +
    annotate("segment", x = 0.5, xend = 1.1, y = 0.8, yend = 0.2, colour = "black")
  
  
  
  if (!is.null(DecisionTree$LeftNode))
  {
    p = p + annotate("text",label=DecisionTree$LeftNode$Variable, x=-0.1, y= 0.1, size=20) + 
      annotate("rect", xmin = -0.3, xmax = 0.1, ymin = 0, ymax = 0.2, color="black", alpha=0.2) 
    
    
    p = p + annotate("text", x=-0.3, y= -0.3, size=20,
                     label=paste('<=',format(DecisionTree$LeftNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = -0.1, xend = -0.35, y = 0, yend = -0.6, colour = "black")
    
    p = p + annotate("text", x=0.1, y= -0.3, size=20,
                     label=paste('>',format(DecisionTree$LeftNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = -0.1, xend = 0.15, y = 0, yend = -0.6, colour = "black")
    
    
    p = p + annotate("text",label='Omega["(0,l,l)"]', x=-0.35, y= -0.7, size=20, parse=T) + 
      annotate("rect", xmin = -0.55, xmax = -0.15, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
    p = p + annotate("text",label='Omega["(0,l,r)"]', x=0.15, y= -0.7, size=20, parse=T) + 
      annotate("rect", xmin = -0.05, xmax = 0.35, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
  }
  else
  {
    p = p + annotate("text",label='Omega["(0,l)"]', x=-0.1, y= 0.1, size=20, parse=T) + 
      annotate("rect", xmin = -0.3, xmax = 0.1, ymin = 0, ymax = 0.2, color="black", alpha=0.2)
    
  }
  
  if (!is.null(DecisionTree$RightNode))
  {
    
    p = p + annotate("text",label=DecisionTree$RightNode$Variable, x=1.1, y= 0.1, size=20) + 
      annotate("rect", xmin = 0.9, xmax = 1.3, ymin = 0, ymax = 0.2, color="black", alpha=0.2)
    
    p = p + annotate("text", x= 0.9, y= -0.3, size=20,
                     label=paste('<=',format(DecisionTree$RightNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = 1.1, xend = 0.85, y = 0, yend = -0.6, colour = "black")
    
    p = p + annotate("text", x=1.3, y= -0.3, size=20,
                     label=paste('>',format(DecisionTree$RightNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = 1.1, xend = 1.35, y = 0, yend = -0.6, colour = "black")
    
    
    p = p + annotate("text",label='Omega["(0,r,l)"]', x=0.85, y= -0.7, size=20, parse=T) + 
      annotate("rect", xmin = 0.65, xmax = 1.05, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
    p = p + annotate("text",label='Omega["(0,r,r)"]', x=1.35, y= -0.7, size=20, parse=T) + 
      annotate("rect", xmin = 1.15, xmax = 1.55, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
  }
  else
  {
    
    p = p + annotate("text",label='Omega["(0,r)"]', x=1.1, y= 0.1, size=20, parse=T) + 
      annotate("rect", xmin = 0.9, xmax = 1.3, ymin = 0, ymax = 0.2, color="black", alpha=0.2)
  }
  
  p = p + xlim(-0.75, 1.75) + ylim(-1,1.2) + 
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  return(p)
  
}



