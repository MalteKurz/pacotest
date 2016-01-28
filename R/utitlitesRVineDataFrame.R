

getNumbOfParameters = function(family)
{
  if (family == 0)
  {
    nPar = 0
  }
  else if (any(family == c(2,
                                    7,17,27,37,
                                    8,18,28,38,
                                    9,19,29,39,
                                    10,20,30,40,
                                    104,114,124,134,
                                    204,214,224,234)))
  {
    nPar = 2
  }
  else
  {
    nPar = 1
  }
  
  return(nPar)
  
}


rVineDataFrameRep = function(rvm)
{
  rvmNormalized = RVineMatrixNormalize(rvm)
  d = dim(rvmNormalized$Matrix)[1]
  nCopulas = d*(d-1)/2
  
  copulaInd = 1:nCopulas
  
  
  # Matrix shifted to upper diagonal
  xx = rvmNormalized$Matrix
  structureMatrix = xx[nrow(xx):1,ncol(xx):1]
  
  xx = rvmNormalized$family
  xx = xx[nrow(xx):1,ncol(xx):1]
  family = t(xx)[lower.tri(xx,FALSE)]
  
  xx = rvmNormalized$par
  xx = xx[nrow(xx):1,ncol(xx):1]
  par1 = t(xx)[lower.tri(xx,FALSE)]
  
  xx = rvmNormalized$par2
  xx = xx[nrow(xx):1,ncol(xx):1]
  par2 = t(xx)[lower.tri(xx,FALSE)]
  
  nPar = rep(0, nCopulas)
  par = rep(list(numeric(0)), nCopulas)
  parInd = rep(list(numeric(0)), nCopulas)
  for (jCopula in 1:nCopulas)
  {
    if (family[jCopula] == 0)
    {
      nPar[jCopula] = 0
    }
    else if (any(family[jCopula] == c(2,
                                      7,17,27,37,
                                      8,18,28,38,
                                      9,19,29,39,
                                      10,20,30,40,
                                      104,114,124,134,
                                      204,214,224,234)))
    {
      par[[jCopula]] = c(par1[jCopula],par2[jCopula])
      parInd[[jCopula]] = seq(from=sum(nPar[1:jCopula])+1, by=1, length.out=2)
      nPar[jCopula] = 2
    }
    else
    {
      par[[jCopula]] = par1[jCopula]
      parInd[[jCopula]] = sum(nPar[1:jCopula])+1
      nPar[jCopula] = 1
    }
  }
  
  
  
  var1 = unlist(lapply(2:d,function(x) seq(x,d,1)))
  var2 = t(structureMatrix)[lower.tri(structureMatrix,FALSE)]
  tree = unlist(lapply(1:(d-1),function(x) rep(x,d-x)))
  
  condset = rep(list(numeric(0)),d-1)
  for (iTree in 2:(d-1))
  {
    condset = c(condset,lapply((iTree+1):d, function(x) structureMatrix[(iTree-1):1,x]))
  }
  
  cPit1hfun = logical(nCopulas)
  cPit1hfun[d:nCopulas] = TRUE
  
  cPit1Ind = numeric(nCopulas)
  cPit1Ind[d:nCopulas] = unlist(lapply(1:(d-2),function(x) seq(from=0.5*(x-1)*(2*d-x)+2, by=1, length.out=d-x-1)))
  
  cPit2hfun = logical(nCopulas)
  cPit2hfun[d:nCopulas] = var2[d:nCopulas] > unlist(lapply(condset[d:nCopulas], max))
  
  cPit2Ind = numeric(nCopulas)
  cPit2Ind[d:nCopulas] = 0.5*(tree[d:nCopulas]-2)*(2*d-(tree[d:nCopulas]-1)) +
    pmax(var2[d:nCopulas],unlist(lapply(condset[d:nCopulas], max)))-tree[d:nCopulas]+1
  
  cPit1CopulaInd = rep(list(numeric(0)), nCopulas)
  cPit2CopulaInd = rep(list(numeric(0)), nCopulas)
  
  cPit1ParInd = rep(list(numeric(0)), nCopulas)
  cPit2ParInd = rep(list(numeric(0)), nCopulas)
  
  for (jCopula in d:nCopulas)
  {
    cPit1CopulaInd[[jCopula]] = sort(unique(c(cPit1Ind[jCopula],
                                              cPit1CopulaInd[[cPit1Ind[jCopula]]],
                                              cPit2CopulaInd[[cPit1Ind[jCopula]]])))
    cPit2CopulaInd[[jCopula]] = sort(unique(c(cPit2Ind[jCopula],
                                              cPit1CopulaInd[[cPit2Ind[jCopula]]],
                                              cPit2CopulaInd[[cPit2Ind[jCopula]]])))
    
    cPit1ParInd[[jCopula]] = sort(unique(c(parInd[[cPit1Ind[jCopula]]],
                                              cPit1ParInd[[cPit1Ind[jCopula]]],
                                              cPit2ParInd[[cPit1Ind[jCopula]]])))
    cPit2ParInd[[jCopula]] = sort(unique(c(parInd[[cPit2Ind[jCopula]]],
                                              cPit1ParInd[[cPit2Ind[jCopula]]],
                                              cPit2ParInd[[cPit2Ind[jCopula]]])))
  }
  
  hfun = rep(FALSE,nCopulas)
  hfun[unique(c(cPit1Ind[cPit1hfun],cPit2Ind[cPit2hfun]))] = TRUE
  vfun = rep(FALSE,nCopulas)
  vfun[unique(cPit2Ind[!cPit2hfun])] = TRUE
  
  
  rownames = paste(paste("C",paste(var1, var2, sep=","),sep="_"), unlist(lapply(condset, paste, collapse=",")), sep = '; ')
  
  variables = data.frame(copulaInd, tree, var1, var2, I(condset), family,
                         I(par),nPar, I(parInd),
                         hfun, vfun,
                         cPit1Ind, cPit1hfun, I(cPit1CopulaInd), I(cPit1ParInd),
                         cPit2Ind, cPit2hfun, I(cPit2CopulaInd), I(cPit2ParInd),
                         row.names = rownames)
   
  svcm = list(structure = structureMatrix,variables = variables)
  
  return(svcm)
  
}

