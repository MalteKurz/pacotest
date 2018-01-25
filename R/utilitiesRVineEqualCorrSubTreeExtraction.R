

extractSubTree = function(RVM, tree, copulaNumber, data)
{
  d <- dim(RVM$Matrix)[1]
  
  newMatrix = matrix(0,nrow=tree+1,ncol=tree+1)
  newFamily = matrix(0,nrow=tree+1,ncol=tree+1)
  newPar = matrix(0,nrow=tree+1,ncol=tree+1)
  newPar2 = matrix(0,nrow=tree+1,ncol=tree+1)
  
  newMatrix[,1] = RVM$Matrix[c(d-tree+1-copulaNumber,(d-tree+1):d),(d-tree+1-copulaNumber)]
  
  newFamily[2:(tree+1),1] = RVM$family[c((d-tree+1):d),(d-tree+1-copulaNumber)]
  newPar[2:(tree+1),1] = RVM$par[c((d-tree+1):d),(d-tree+1-copulaNumber)]
  newPar2[2:(tree+1),1] = RVM$par2[c((d-tree+1):d),(d-tree+1-copulaNumber)]
  
  m = RVM$MaxMat[(d-tree+1), (d-tree+1-copulaNumber)]
  for (k in 1:(tree-1))
  {
    newMatrix[(k+1):(tree+1),(k+1)] = RVM$Matrix[c((d - m + 1),(d-tree+k+1):d),(d - m + 1)]
    
    xx = (d-tree+k+1):d
    
    
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

