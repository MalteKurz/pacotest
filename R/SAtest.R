SAtest = function(Udata,W,SAtestOptions){
  
    SAtestOptions = SAtestSet(SAtestOptions)
    Udata = as.matrix(Udata)
    W = as.matrix(W)
  
  if (SAtestOptions$TestType=="ERC")
  {
    if (SAtestOptions$Grouping=='TreeERC' && SAtestOptions$AggPvalsNumbRep > 1)
    {
      # third list element are the p-values that have been aggregated.
      Grouping = 0
      out = ERC(Udata,W,Grouping,SAtestOptions$AggPvalsNumbRep)
    }
    else
    {
      Grouping = which(SAtestOptions$Grouping==c('TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'),arr.ind=TRUE)
      out = ERC(Udata,W,Grouping)
      if (SAtestOptions$GroupedScatterplots)
      {
        GroupedScatterplot(out$Xdata,out$Ydata)
      }
    }
  }
  else if (SAtestOptions$TestType=="EC")
  {
    Grouping = which(SAtestOptions$Grouping==c('TreeERC','TreeEC','SumMedian','SumThirdsI','SumThirdsII','ProdMedian','ProdThirdsI','ProdThirdsII'),arr.ind=TRUE)
    out = EC(Udata,W,SAtestOptions$NumbBoot,Grouping)
    if (SAtestOptions$GroupedScatterplots)
    {
      GroupedScatterplot(out$Xdata,out$Ydata)
    }
  }
  else if (SAtestOptions$TestType=="VI")
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
  plot(Xdata[,1],Xdata[,2],xlab="U",ylab="V")
  plot(Ydata[,1],Ydata[,2],xlab="U",ylab="V")
  plot(qnorm(Xdata[,1]),qnorm(Xdata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
  plot(qnorm(Ydata[,1]),qnorm(Ydata[,2]),xlab=expression(paste(Phi^-1,(U))),ylab=expression(paste(Phi^-1,(V))))
}
