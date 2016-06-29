
addAggInfo = function(W, aggInfoType=NULL, sizeKeepingMethod=NULL)
{
  if (!is.null(aggInfoType))
  {
    if (is.null(sizeKeepingMethod) || sizeKeepingMethod == 'splitTrainEvaluate')
    {
      W = cbindAggInfo(W, aggInfoType)
    }
    else
    {
      if (sizeKeepingMethod == 'penalty')
      {
        if (dim(W)[2]>2 && aggInfoType=="meanPairwise")
        {
          # Add the information defined aggInfoType
          xx = addAggInfo(W,aggInfoType)
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
      else
      {
        stop('addAggInfo error')
      }
    }
  }
  
  return(W)
}


cbindAggInfo = function(W, aggInfoType)
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
