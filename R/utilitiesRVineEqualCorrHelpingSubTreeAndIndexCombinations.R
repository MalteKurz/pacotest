

extractSubTree = function(RVM, tree, copulaNumber, data)
{
  if (!is(RVM, "RVineMatrix")) 
    stop("'RVM' has to be an RVineMatrix object.")
  # Check whether the object has been normalized
  
  d <- dim(RVM$Matrix)[1]
  if (!(tree > 1 && tree <= (d-1)))
    stop("'tree' has do be larger than 2 and at most dim (of the vine) - 1")
  if (!(copulaNumber > 0 && copulaNumber <= (d-tree)))
    stop("Invalid copula number")
  
  newMatrix = matrix(0,nrow=tree+1,ncol=tree+1)
  newFamily = matrix(0,nrow=tree+1,ncol=tree+1)
  newPar = matrix(0,nrow=tree+1,ncol=tree+1)
  newPar2 = matrix(0,nrow=tree+1,ncol=tree+1)
  
  newMatrix[,1] = RVM$Matrix[c(d-tree+1-copulaNumber,(d-tree+1):d),(d-tree+1-copulaNumber)]
  newFamily[2:(tree+1),1] = RVM$family[c(d-tree+2-copulaNumber,(d-tree+2):d),(d-tree+1-copulaNumber)]
  newPar[2:(tree+1),1] = RVM$par[c(d-tree+2-copulaNumber,(d-tree+2):d),(d-tree+1-copulaNumber)]
  newPar2[2:(tree+1),1] = RVM$par2[c(d-tree+2-copulaNumber,(d-tree+2):d),(d-tree+1-copulaNumber)]
  
  m = RVM$MaxMat[(d-tree+1), (d-tree+1-copulaNumber)]
  for (k in 1:(tree-1))
  {
    newMatrix[(k+1):(tree+1),(k+1)] = RVM$Matrix[c((d - m + 1),(d-tree+k+1):d),(d - m + 1)]
    if (tree>2 && k<(tree-1))
    {
      xx = c((d - m + 2),(d-tree+k+2):d)
    }
    else
    {
      xx = (d - m + 2)
    }
    newFamily[(k+2):(tree+1),(k+1)] = RVM$family[xx,(d - m + 1)]
    newPar[(k+2):(tree+1),(k+1)] = RVM$par[xx,(d - m + 1)]
    newPar2[(k+2):(tree+1),(k+1)] = RVM$par2[xx,(d - m + 1)]
    
    m = RVM$MaxMat[(d-tree+k+1), (d - m + 1)]
  }
  newMatrix[(tree+1),(tree+1)] = setdiff(newMatrix[(1:(tree+1)),1],diag(newMatrix[(1:tree),(1:tree)]))
  
  xx = diag(newMatrix)
  newNames = RVM$names[xx[length(xx):1]]
  newData = data[,xx[length(xx):1]]
  
  if (max(newMatrix)>(tree+1))
  {
    for (i in 1:(tree+1))
    {
      if (!any(newMatrix == i))
      {
        xx = newMatrix[newMatrix>i]
        xx = min(xx)
        newMatrix[newMatrix==xx] = i
      }
    }
  }
  
  return(list(RVM=RVineMatrix(newMatrix,newFamily,newPar,newPar2,names=newNames),data=newData))
  
}


helpingGetIndexCombinations = function(d,iStart,iLimit,kLimitDeduction=0,kStartInflation=0)
{
  res = matrix(0,0,2)
  for (i in iStart:iLimit)
  {
    for (k in (d-kStartInflation):(i + 1 + kLimitDeduction))
    {
      res = rbind(res,c(k,i))
    }
  }
  
  return(res)
}


getIndexCombinations = function(RVM)
{
  
  d <- dim(RVM$Matrix)[1]
  nCopulas = d*(d-1)/2-1
  dCondSet = d-2
  
  indexCombinations = matrix(0,nrow=nCopulas,ncol=2)
  
  if (dCondSet>=3)
  {
    numbCopulas = (dCondSet-1)*(dCondSet-2)/2
    iStart = d-1
    iLimit = 4
    indexCombinations[1:numbCopulas,1:2] = helpingGetIndexCombinations(d,iStart,iLimit)
  }
  else
  {
    numbCopulas = 0
  }
  
  if (dCondSet>=2)
  {
    if (RVM$MaxMat[3,1]==(d-2))
    {
      iStart = 3
      iLimit = 3
      indexCombinations[(numbCopulas+1):(numbCopulas+dCondSet-1),1:2] = helpingGetIndexCombinations(d,iStart,iLimit)
    }
    else
    { # should be RVM$MaxMat[3,1]==(d-1)
      iStart = 2
      iLimit = 2
      indexCombinations[(numbCopulas+1):(numbCopulas+dCondSet-1),1:2] = helpingGetIndexCombinations(d,iStart,iLimit,1)
    }
  }
  
  # Copulas for the first cPit
  iStart = 1
  iLimit = 1
  indexCombinations[(numbCopulas+dCondSet):(numbCopulas+2*dCondSet-1),1:2] = helpingGetIndexCombinations(d,iStart,iLimit,1)
  
  # Copulas for the second cPit
  if (RVM$MaxMat[3,1]==(d-2))
  {
    iStart = 2
    iLimit = 2
    indexCombinations[(numbCopulas+2*dCondSet):(numbCopulas+3*dCondSet-1),1:2] = helpingGetIndexCombinations(d,iStart,iLimit)
  }
  else
  { # should be RVM$MaxMat[3,1]==(d-1)
    if (dCondSet>=2)
    {
      iStart = 3
      iLimit = 3
      indexCombinations[(numbCopulas+2*dCondSet):(numbCopulas+3*dCondSet-2),1:2] = helpingGetIndexCombinations(d,iStart,iLimit)
      
      iStart = 2
      iLimit = 2
      indexCombinations[(numbCopulas+3*dCondSet-1),1:2] = helpingGetIndexCombinations(d,iStart,iLimit,0,dCondSet-1)
    }
    else
    {
      iStart = 2
      iLimit = 2
      indexCombinations[numbCopulas+2,1:2] = helpingGetIndexCombinations(d,iStart,iLimit)
    }
  }
  
  return(indexCombinations)
}

