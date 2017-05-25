GroupedScatterplot = function(Udata, W, decisionTree)
{
  
  dataLabels = names(Udata)
  
  names(Udata) = c("V1", "V2")
  

  rho = momentBasedCorr(Udata)
  titleStrElements = list()
  titleStrElements$partIdentifier = "0"
  titleStrElements$corrInGroup = format(rho,dig=3)
  titleStr = bquote(Lambda[.(titleStrElements$partIdentifier)] ~ ":" ~
                      hat(r) ~ "=" ~ .(titleStrElements$corrInGroup))
  
  p1 = ggplot(Udata,
              aes_string("V1", "V2")) +
    geom_point() +
    scale_colour_manual(values = "black") + 
    ggtitle(titleStr) + 
    scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(expression(u["4|23"])) + 
    ylab(expression(u["1|23"])) + 
    coord_fixed() +
    theme_bw(base_size = 30) + 
    theme(plot.title = element_text(size = rel(0.75), hjust = 0.5),
          axis.title = element_text(size = rel(0.75)),
          panel.grid.major = element_line(colour = "grey85", size = .75),
          panel.grid.minor = element_line(colour = "grey85", size = .75))
  
  lM = matrix(NA, nrow=1, ncol=3)
  lM[1,2] = 1
  grobBaseNode <- arrangeGrob(p1, layout_matrix = lM, widths = c(1.5,1,1.5))
  
  partIdentifier = c("(0,l)", "(0,r)")
  xx = getGroupedPlot(Udata, W, decisionTree$CentralNode$Variable, decisionTree$CentralNode$Threshold, dataLabels, partIdentifier)
  
  indCentralSplit = xx$indVector
  
  lM = matrix(NA, nrow=1, ncol=5)
  lM[1,2] = 1
  lM[1,4] = 2
  grobFirstTree <- arrangeGrob(xx$pL, xx$pR, layout_matrix = lM, widths = c(0.5,1,1,1,0.5))
  
  if (!is.null(decisionTree$LeftNode))
  {
    partIdentifier = c("(0,l,l)", "(0,l,r)")
    xx = getGroupedPlot(Udata[indCentralSplit==1,,drop=FALSE],
                        W[indCentralSplit==1,,drop=FALSE],
                        decisionTree$LeftNode$Variable, decisionTree$LeftNode$Threshold, dataLabels, partIdentifier)
    
    pLL<-xx$pL
    pLR<-xx$pR
    
  }
  
  if (!is.null(decisionTree$RightNode))
  {
    partIdentifier = c("(0,r,l)", "(0,r,r)")
    xx = getGroupedPlot(Udata[indCentralSplit==0,,drop=FALSE],
                        W[indCentralSplit==0,,drop=FALSE],
                        decisionTree$RightNode$Variable, decisionTree$RightNode$Threshold, dataLabels, partIdentifier)
    
    pRL<-xx$pL
    pRR<-xx$pR
    
  }
  
  
  if (is.null(decisionTree$LeftNode) && is.null(decisionTree$RightNode))
  {
    lM = matrix(NA, nrow=2, ncol=1)
    lM[1,1] = 1
    lM[2,1] = 2
    grid.arrange(grobBaseNode, grobFirstTree, layout_matrix = lM)
  }
  else
  {
    if (is.null(decisionTree$LeftNode))
    {
      lM = matrix(NA, nrow=1, ncol=4)
      lM[1,3] = 1
      lM[1,4] = 2
      grobSecondTree <- arrangeGrob(pRL, pRR, layout_matrix = lM, widths = c(1,1,1,1))
    }
    else
    {
      if (is.null(decisionTree$RightNode))
      {
        lM = matrix(NA, nrow=1, ncol=4)
        lM[1,1] = 1
        lM[1,2] = 2
        grobSecondTree <- arrangeGrob(pLL, pLR, layout_matrix = lM, widths = c(1,1,1,1))
      }
      else
      {
        lM = matrix(NA, nrow=1, ncol=4)
        lM[1,1] = 1
        lM[1,2] = 2
        lM[1,3] = 3
        lM[1,4] = 4
        grobSecondTree <- arrangeGrob(pLL, pLR, pRL, pRR, layout_matrix = lM, widths = c(1,1,1,1))
      }
      
      lM = matrix(NA, nrow=3, ncol=1)
      lM[1,1] = 1
      lM[2,1] = 2
      lM[3,1] = 3
      p = grid.arrange(grobBaseNode, grobFirstTree, grobSecondTree, layout_matrix = lM)
    }
  }
  
  
  # Switch to viewport for first set of arrows
  vp = viewport(x = 0, y=0, width=2, height=2)
  pushViewport(vp)
  
  #grid.rect(gp=gpar(fill="black", alpha=0.1))
  
  grid.lines(x=c(0.881,0.921),y=c(0.9175,0.91), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  grid.lines(x=c(0.881,0.841),y=c(0.9175,0.91), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  
  grid.lines(x=c(0.8185,0.8085),y=c(0.835,0.825), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  grid.lines(x=c(0.8185,0.8285),y=c(0.835,0.825), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  
  grid.lines(x=c(0.9435,0.9335),y=c(0.835,0.825), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  grid.lines(x=c(0.9435,0.9535),y=c(0.835,0.825), vp =vp, arrow = arrow(type="closed", length=unit(5,"mm")))
  
  popViewport()
  
  
  
  #df <- data.frame(x = c(0.55,0.95), y = c(0,0.07))
  #pBla = grobBaseNode + geom_path(data = df, aes(x, y), size = 2)
  #pBla<-ggplotGrob(pBla)
  #lines <- pBla$grobs[[4]][["children"]][[3]]
  
  #df <- data.frame(x = c(0.1,0.5), y = c(0.07,0))
  #pBla = p1 + geom_path(data = df, aes(x, y), size = 2)
  #pBla<-ggplotGrob(pBla)
  #lines2 <- pBla$grobs[[4]][["children"]][[3]]
  
  #df <- data.frame(x = c(0.275,0.575,0.775), y = c(0,0.06,0))
  #pBla = p1 + geom_path(data = df, aes(x, y), size = 2)
  #pBla<-ggplotGrob(pBla)
  #lines3 <- pBla$grobs[[4]][["children"]][[3]]
  
  #df <- data.frame(x = c(0.275,0.475,0.775), y = c(0,0.06,0))
  #pBla = p1 + geom_path(data = df, aes(x, y), size = 2)
  #pBla<-ggplotGrob(pBla)
  #lines4 <- pBla$grobs[[4]][["children"]][[3]]
  
  #bla <- gtable_add_grob(p, lines, l=1, t=1, b=1, z=Inf)
  #bla <- gtable_add_grob(bla, lines2, l=2, t=1, b=1, z=Inf)
  #bla <- gtable_add_grob(p, lines3, l=1, t=1, b=1, r=2, z=Inf)
  #bla <- gtable_add_grob(bla, lines3, l=1, t=2, b=2, z=Inf)
  #bla <- gtable_add_grob(bla, lines4, l=2, t=2, b=2, z=Inf)
  
  #grid.newpage()
  #grid.draw(p)
  
  dev.copy(pdf,"groupedPlot.pdf", width=30 , height=20)
  dev.off()
  
  return(p)
  #}
  
}


getGroupedPlot = function(Udata, W, variable, threshold, dataLabels, partIdentifier)
{
  indVector = W[,variable]<=threshold
  
  splitFactorVec = vector(length=length(indVector))
  levelNames = vector(length=2)
  
  rho = momentBasedCorr(Udata[indVector==1,])
  levelNames[1] = paste(variable, " <= ", format(threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  splitFactorVec[indVector==1] = levelNames[1]
  
  titleStrElements = list()
  titleStrElements$partIdentifier = partIdentifier[1]
  titleStrElements$threshold = format(threshold,dig=3)
  titleStrElements$corrInGroup = format(rho,dig=3)
  xx = getIndicesFromVariable(variable)
  if (xx$barForU)
  {
    titleStrL = bquote(Lambda[.(titleStrElements$partIdentifier)] ~ ":" ~
                         bar(u)[.(xx$indicesForU)] ~ "<=" ~ .(titleStrElements$threshold) ~ "," ~
                         hat(r) ~ "=" ~ .(titleStrElements$corrInGroup))
  }
  else
  {
    titleStrL = bquote(Lambda[.(titleStrElements$partIdentifier)] ~ ":" ~
                         u[.(xx$indicesForU)] ~ "<=" ~ .(titleStrElements$threshold) ~ "," ~
                         hat(r) ~ "=" ~ .(titleStrElements$corrInGroup))
  }
  
  rho = momentBasedCorr(Udata[indVector==0,])
  levelNames[2] = paste(variable, " > ", format(threshold,dig=4),
                        "\nrho = ", format(rho,dig=4))
  splitFactorVec[indVector==0] = levelNames[2]
  
  titleStrElements = list()
  titleStrElements$partIdentifier = partIdentifier[2]
  titleStrElements$threshold = format(threshold,dig=3)
  titleStrElements$corrInGroup = format(rho,dig=3)
  xx = getIndicesFromVariable(variable)
  if (xx$barForU)
  {
    titleStrR = bquote(Lambda[.(titleStrElements$partIdentifier)] ~ ":" ~
                         bar(u)[.(xx$indicesForU)] ~ ">" ~ .(titleStrElements$threshold) ~ "," ~
                         hat(r) ~ "=" ~ .(titleStrElements$corrInGroup))
  }
  else
  {
    titleStrR = bquote(Lambda[.(titleStrElements$partIdentifier)] ~ ":" ~
                         u[.(xx$indicesForU)] ~ ">" ~ .(titleStrElements$threshold) ~ "," ~
                         hat(r) ~ "=" ~ .(titleStrElements$corrInGroup))
  }
  
  splitFactorVec = factor(splitFactorVec, levelNames)
  
  
  data = transform(Udata, split = splitFactorVec)
  
  pBase = ggplot(data,
                 aes_string("V1", "V2", colour="split")) +
    geom_point() + 
    guides(colour=FALSE) + 
    coord_equal() +
    scale_y_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    scale_x_continuous(expand = c(0,0), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(expression(u["4|23"])) + 
    ylab(expression(u["1|23"])) + 
    coord_fixed() +
    theme_bw(base_size = 30) + 
    theme(plot.title = element_text(size = rel(0.75), hjust = 0.5),
          axis.title = element_text(size = rel(0.75)),
          panel.grid.major = element_line(colour = "grey85", size = .75),
          panel.grid.minor = element_line(colour = "grey85", size = .75))
  
  
  pL = pBase +
    ggtitle(titleStrL) + 
    scale_colour_manual(values=c("black", "grey85"))
  
  pR = pBase +
    ggtitle(titleStrR) + 
    scale_colour_manual(values=c("grey85", "black"))
  
  
  return(list(pL=pL, pR=pR, indVector = indVector))
  
}

getIndicesFromVariable = function(variable)
{
  if (variable == "V2")
  {
    indicesForU = "2"
    barForU = FALSE
  }
  else if (variable == "V3")
  {
    indicesForU = "3"
    barForU = FALSE
  }
  else if (variable == "Mean(V2, V3)")
  {
    indicesForU = "2:3"
    barForU = TRUE
  }
   
  return(list(indicesForU = indicesForU, barForU = barForU)) 
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


decisionTreePlot = function(DecisionTree)
{
  p  = ggplot()
  
  
  p = p + annotate("text",label=DecisionTree$CentralNode$Variable, x=0.5, y= 0.9, size=rel(4)) + 
    annotate("rect", xmin = 0.3, xmax = 0.7, ymin = 0.8, ymax = 1, color="black", alpha=0.2) 
  
  
  p = p + annotate("text", x=-0.05, y= 0.5, size=rel(4),
                   label=paste('<=',format(DecisionTree$CentralNode$Threshold,digits=4),sep='')) +
    annotate("segment", x = 0.5, xend = -0.1, y = 0.8, yend = 0.2, colour = "black")
  
  p = p + annotate("text", x=1.05, y= 0.5, size=rel(4),
                   label=paste('>',format(DecisionTree$CentralNode$Threshold,digits=4),sep='')) +
    annotate("segment", x = 0.5, xend = 1.1, y = 0.8, yend = 0.2, colour = "black")
  
  
  
  if (!is.null(DecisionTree$LeftNode))
  {
    p = p + annotate("text",label=DecisionTree$LeftNode$Variable, x=-0.1, y= 0.1, size=rel(4)) + 
      annotate("rect", xmin = -0.3, xmax = 0.1, ymin = 0, ymax = 0.2, color="black", alpha=0.2) 
    
    
    p = p + annotate("text", x=-0.3, y= -0.3, size=rel(4),
                     label=paste('<=',format(DecisionTree$LeftNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = -0.1, xend = -0.35, y = 0, yend = -0.6, colour = "black")
    
    p = p + annotate("text", x=0.1, y= -0.3, size=rel(4),
                     label=paste('>',format(DecisionTree$LeftNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = -0.1, xend = 0.15, y = 0, yend = -0.6, colour = "black")
    
    
    p = p + annotate("text",label='Lambda["(0,l,l)"]', x=-0.35, y= -0.7, size=rel(4), parse=T) + 
      annotate("rect", xmin = -0.55, xmax = -0.15, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
    p = p + annotate("text",label='Lambda["(0,l,r)"]', x=0.15, y= -0.7, size=rel(4), parse=T) + 
      annotate("rect", xmin = -0.05, xmax = 0.35, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
  }
  else
  {
    p = p + annotate("text",label='Lambda["(0,l)"]', x=-0.1, y= 0.1, size=rel(4), parse=T) + 
      annotate("rect", xmin = -0.3, xmax = 0.1, ymin = 0, ymax = 0.2, color="black", alpha=0.2)
    
  }
  
  if (!is.null(DecisionTree$RightNode))
  {
    
    p = p + annotate("text",label=DecisionTree$RightNode$Variable, x=1.1, y= 0.1, size=rel(4)) + 
      annotate("rect", xmin = 0.9, xmax = 1.3, ymin = 0, ymax = 0.2, color="black", alpha=0.2)
    
    p = p + annotate("text", x= 0.9, y= -0.3, size=rel(4),
                     label=paste('<=',format(DecisionTree$RightNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = 1.1, xend = 0.85, y = 0, yend = -0.6, colour = "black")
    
    p = p + annotate("text", x=1.3, y= -0.3, size=rel(4),
                     label=paste('>',format(DecisionTree$RightNode$Threshold,digits=4),sep='')) +
      annotate("segment", x = 1.1, xend = 1.35, y = 0, yend = -0.6, colour = "black")
    
    
    p = p + annotate("text",label='Lambda["(0,r,l)"]', x=0.85, y= -0.7, size=rel(4), parse=T) + 
      annotate("rect", xmin = 0.65, xmax = 1.05, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
    
    p = p + annotate("text",label='Lambda["(0,r,r)"]', x=1.35, y= -0.7, size=rel(4), parse=T) + 
      annotate("rect", xmin = 1.15, xmax = 1.55, ymin = -0.8, ymax = -0.6, color="black", alpha=0.2) 
  }
  else
  {
    
    p = p + annotate("text",label='Lambda["(0,r)"]', x=1.1, y= 0.1, size=rel(4), parse=T) + 
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

partitionPlot = function(decisionTree, W)
{
  
  
  gg = getCondKendallPlotData()
  
  
  points = ggplot() +
    geom_point(data = W, aes_string(names(W)[1], names(W)[2])) +
    xlab(names(W)[1]) +
    ylab(names(W)[2])
  
  kendall = ggplot(gg, aes(x, y)) +
    geom_tile(aes(fill = z)) +
    scale_fill_gradient(low="gray90", high="gray10") + 
    labs(fill = expression(paste("Kendall's ", tau))) +
    xlab(names(W)[1]) +
    ylab(names(W)[2])
  
  
  
  
  cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")
  cbbPalette = c("black", "gray10", "gray70", "gray60", "gray20","gray80","gray40")
  
  partitionForPlot = getPartitionForPlot(decisionTree, W)
  
  data1 = partitionForPlot[is.element(partitionForPlot$subset, c('l','r')),]
  PartitionLabeling = c(expression(Lambda["(0,r)"]),
                        expression(Lambda["(0,l)"]))
  partitionBreaks = c('r', 'l')
  
  p1WithPoints = points +
    geom_polygon(data=data1, aes(x=x, y=y, group=subset, colour=subset),  fill=NA, size = 4) +
    scale_colour_manual(values = cbbPalette[2:3], labels=PartitionLabeling, breaks = partitionBreaks) +
    labs(colour = "Subsets") +
    coord_fixed() +
    scale_x_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) + 
    scale_y_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(expression(u["2"])) + 
    ylab(expression(u["3"])) + 
    theme_bw(base_size = 30) +
    theme(legend.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "cm"),
        panel.spacing = unit(2.5, "lines"),
        panel.grid.major = element_line(colour = "grey85", size = .75),
        panel.grid.minor = element_line(colour = "grey85", size = .75))
  
  p1WithKendall = kendall +
    geom_polygon(data=data1, aes(x=x, y=y, group=subset, colour=subset),  fill=NA, size = 4) +
    scale_colour_manual(values = cbbPalette[2:3], labels=PartitionLabeling, breaks = partitionBreaks) +
    labs(colour = "Subsets") +
    coord_fixed() +
    scale_x_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) + 
    scale_y_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
    xlab(expression(u["2"])) + 
    ylab(expression(u["3"])) + 
    theme_bw(base_size = 30) +
    theme(legend.spacing = unit(1, "lines"),
          legend.key.size = unit(1, "cm"),
          panel.spacing = unit(2.5, "lines"),
          panel.grid.major = element_line(colour = "grey85", size = .75),
          panel.grid.minor = element_line(colour = "grey85", size = .75)) + 
    guides(colour = guide_legend(order = 1))
  
  if (is.null(decisionTree$LeftNode) && !is.null(decisionTree$RightNode))
  {
    data2 = partitionForPlot[is.element(partitionForPlot$subset, c('l','rl','rr')),]
    
    PartitionLabeling = c(expression(Lambda["(0,r,r)"]),
                          expression(Lambda["(0,r,l)"]),
                          expression(Lambda["(0,l)"]))
    partitionBreaks = c('rr','rl','l')
    cols = cbbPalette[c(2,6,7)]
    
  }
  
  if (!is.null(decisionTree$LeftNode) && is.null(decisionTree$RightNode))
  {
    data2 = partitionForPlot[is.element(partitionForPlot$subset, c('ll','lr','r')),]
    
    PartitionLabeling = c(expression(Lambda["(0,r)"]),
                          expression(Lambda["(0,l,r)"]),
                          expression(Lambda["(0,l,l)"]))
    partitionBreaks = c('r','lr','ll')
    cols = cbbPalette[c(4,5,3)]
    
  }
  
  if (!is.null(decisionTree$LeftNode) && !is.null(decisionTree$RightNode))
  {
    data2 = partitionForPlot[is.element(partitionForPlot$subset, c('ll','lr','rl','rr')),]
    
    PartitionLabeling = c(expression(Lambda["(0,r,r)"]),
                          expression(Lambda["(0,r,l)"]),
                          expression(Lambda["(0,l,r)"]),
                          expression(Lambda["(0,l,l)"]))
    partitionBreaks = c('rr','rl','lr','ll')
    cols = cbbPalette[4:7]
    
  }
  
  
  if (!is.null(decisionTree$LeftNode) || !is.null(decisionTree$RightNode))
  {
    p2WithPoints = points +
      geom_polygon(data=data2, aes(x=x, y=y, group=subset, colour=subset),  fill=NA, size = 4) +
      scale_colour_manual(values = cols, labels=PartitionLabeling, breaks = partitionBreaks) +
      labs(colour = "Subsets") +
      coord_fixed() +
      scale_x_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) + 
      scale_y_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      xlab(expression(u["2"])) + 
      ylab(expression(u["3"])) + 
      theme_bw(base_size = 30) +
      theme(legend.spacing = unit(1, "lines"),
            legend.key.size = unit(1, "cm"),
            panel.spacing = unit(2.5, "lines"),
            panel.grid.major = element_line(colour = "grey85", size = .75),
            panel.grid.minor = element_line(colour = "grey85", size = .75))
    
    p2WithKendall = kendall +
      geom_polygon(data=data2, aes(x=x, y=y, group=subset, colour=subset),  fill=NA, size = 4) +
      scale_colour_manual(values = cols, labels=PartitionLabeling, breaks = partitionBreaks) +
      labs(colour = "Subsets") +
      coord_fixed() +
      scale_x_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) + 
      scale_y_continuous(expand=c(0.01,0.01), limits=c(0,1), labels = c(0,0.25,0.5,0.75,1)) +
      xlab(expression(u["2"])) + 
      ylab(expression(u["3"])) + 
      theme_bw(base_size = 30) +
      theme(legend.spacing = unit(1, "lines"),
            legend.key.size = unit(1, "cm"),
            panel.spacing = unit(2.5, "lines"),
            panel.grid.major = element_line(colour = "grey85", size = .75),
            panel.grid.minor = element_line(colour = "grey85", size = .75)) +
      guides(colour = guide_legend(order = 1))
  }
  else
  {
    p2 = ggplot()
  }
  
  #   grid.arrange(p1WithPoints, p2WithPoints,
  #                p1WithKendall, p2WithKendall,
  #                ncol=2, nrow = 2)# Get the gtables
  gA <- ggplotGrob(p1WithKendall)
  gB <- ggplotGrob(p1WithPoints)
  gC <- ggplotGrob(p2WithKendall)
  gD <- ggplotGrob(p2WithPoints)
  
  # Set the widths
  gB$widths = gA$widths
  gC$widths = gA$widths
  gD$widths = gA$widths
  
  # Arrange the two charts.
  # The legend boxes are centered
  grid.newpage()
  grid.arrange(gA, gB, gC, gD, nrow = 2, ncol = 2)
  
  dev.copy(pdf,"partitionPlot.pdf", width=30 , height=20)
  dev.off()
}


getPartitionForPlot = function(decisionTree, W)
{
  if (dim(W)[2] != 3 || !(grepl('^Mean', names(W)[3])) )
  {
    stop("partitionPlot is only implemented for the two-dimensional conditioning set, where the mean has been added as aggregated information.")
  }
  
  eps = 0.007
  
  if (grepl('^Mean', decisionTree$CentralNode$Variable))
  {
    cThres = decisionTree$CentralNode$Threshold*2
    
    if (cThres < 1)
    {
      xl = c(0, cThres - eps, 0)
      yl = c(0, 0, cThres - eps)
      
      xr = c(cThres + eps, 1, 1, 0, 0)
      yr = c(0, 0, 1, 1, cThres + eps)
    }
    else
    {
      xl = c(0, 1, 1, cThres - eps -1, 0)
      yl = c(0, 0, cThres - eps -1, 1, 1)
      
      xr = c(1, 1, cThres + eps -1)
      yr = c(cThres + eps -1, 1, 1)
    }
    
    partition = rbind(data.frame(x = xl, y=yl, subset = "l"),
                      data.frame(x = xr, y=yr, subset = "r"))
    
    
    if (!is.null(decisionTree$LeftNode))
    {
      if (grepl('^Mean', decisionTree$LeftNode$Variable))
      {
        
        lThres = decisionTree$LeftNode$Threshold*2
        
        if (lThres < 1)
        {
          xll = c(0,lThres - eps,0)
          yll = c(0,0,lThres - eps)
          
          if (cThres <1)
          {
            xlr = c(lThres + eps,cThres - eps,0,0)
            ylr = c(0,0,cThres - eps,lThres + eps)
          }
          else
          {
            xlr = c(lThres + eps,1,1,cThres - eps -1,0,0)
            ylr = c(0,0,cThres - eps -1,1,1,lThres + eps)
          }
          
        }
        else
        {
          xll = c(0,1,1,lThres - eps -1,0)
          yll = c(0,0,lThres - eps -1,1,1)
          
          xlr = c(1,1,cThres - eps -1,lThres + eps -1)
          ylr = c(lThres + eps -1,cThres - eps -1,1,1)
          
        }
      }
      else
      {
        
        if (cThres <1)
        {
          if (decisionTree$LeftNode$Variable == names(W)[1])
          {
            
            lThres = decisionTree$LeftNode$Threshold
            
            xll = c(0,lThres - eps,lThres - eps,0)
            yll = c(0,0,cThres-lThres,cThres - eps)
            
            xlr = c(lThres + eps, cThres -eps, lThres +eps)
            ylr = c(0,0,cThres-lThres - 2*eps)
          }
          else
          {
            
            lThres = decisionTree$LeftNode$Threshold
            
            xll = c(0,cThres - eps, lThres - eps,0)
            yll = c(0,0,cThres-lThres - eps, cThres-lThres - eps)
            
            xlr = c(0,cThres-lThres + eps,0)
            ylr = c(lThres + eps,lThres + eps ,cThres - eps)
            
          }
        }
        else
        {
          if (decisionTree$LeftNode$Variable == names(W)[1])
          {
            
            lThres = decisionTree$LeftNode$Threshold
            
            if (lThres < cThres-1)
            {
              xll = c(0,lThres - eps,lThres - eps,0)
              yll = c(0,0,1,1)
              
              xlr = c(lThres + eps,1,1,cThres-1 - eps,lThres + eps)
              ylr = c(0,0,cThres-1 - eps,1,1)
            }
            else
            {
              xll = c(0,lThres - eps,lThres - eps,cThres-1 - eps,0)
              yll = c(0,0,cThres-lThres - eps,1,1)
              
              xlr = c(lThres + eps,1,1,lThres + eps)
              ylr = c(0,0,cThres-1 - eps,cThres-lThres + eps)
              
            }
          }
          else
          {
            
            lThres = decisionTree$LeftNode$Threshold
            
            
            if (lThres < cThres-1)
            {
              xll = c(0,1,1,0)
              yll = c(0,0,lThres - eps,lThres - eps)
              
              xlr = c(0,1,1,cThres-1 - eps,0)
              ylr = c(lThres + eps,lThres + eps,cThres-1 - eps,1,1)
              
            }
            else
            {
              xll = c(0,1,1,cThres-lThres - eps,0)
              yll = c(0,0,cThres-1 - eps, lThres - eps,lThres - eps)
              
              xlr = c(0,cThres-lThres + eps,cThres-1 + eps,0)
              ylr = c(lThres + eps,lThres + eps,1,1)
              
            }
          }
        }
      }
    }
    
    if (!is.null(decisionTree$RightNode))
    {
      if (grepl('^Mean', decisionTree$RightNode$Variable))
      {
        
        rThres = decisionTree$RightNode$Threshold*2
        
        if (rThres >= 1)
        {
          if (cThres >=1)
          {
            xrl = c(1,1,rThres-1 - eps,cThres-1 + eps)
            yrl = c(cThres-1 + eps,rThres-1 - eps,1,1)
          }
          else
          {
            xrl = c(1,rThres-1 - eps,0,0,cThres + eps,1)
            yrl = c(rThres-1 - eps,1,1,cThres + eps,0,0)
            
          }
          
          xrr = c(1,rThres-1 + eps,1)
          yrr = c(1,1,rThres-1 + eps)
        }
        else
        {
          xrl = c(1,0,0,rThres - eps,1)
          yrl = c(1,1,rThres - eps,0,0)
          
          xrr = c(rThres + eps,0,0,cThres + eps)
          yrr = c(0,rThres + eps,cThres + eps,0)
          
        }
      }
      else
      {
        
        if (cThres >=1)
        {
          if (decisionTree$RightNode$Variable == names(W)[1])
          {
            
            rThres = decisionTree$RightNode$Threshold
            
            xrl = c(1,1,rThres - eps,rThres - eps)
            yrl = c(cThres-1 + eps,1,1,cThres-rThres + eps)
            
            xrr = c(rThres + eps,rThres + eps,cThres-1 + eps)
            yrr = c(cThres-rThres + eps,1,1)
            
          }
          else
          {
            
            rThres = decisionTree$RightNode$Threshold
            
            xrl = c(1,1,cThres-rThres + 2*eps)
            yrl = c(cThres-1 + eps,rThres - eps,rThres - eps)
            
            xrr = c(1,1,cThres-1 + eps,cThres-rThres + eps)
            yrr = c(rThres + eps,1,1,rThres + eps)
            
          }
        }
        else
        {
          if (decisionTree$RightNode$Variable == names(W)[1])
          {
            
            rThres = decisionTree$RightNode$Threshold
            
            if (rThres > cThres)
            {
              xrl = c(rThres - eps,1,1,rThres - eps)
              yrl = c(0,0,1,1)
              
              xrr = c(cThres + eps,rThres + eps,rThres + eps,0,0)
              yrr = c(0,0,1,1,cThres + eps)
              
            }
            else
            {
              xrl = c(1,1,rThres - eps,rThres - eps,cThres + eps)
              yrl = c(0,1,1,cThres-rThres - eps,0)
              
              xrr = c(rThres + eps,rThres + eps,0,0)
              yrr = c(cThres-rThres + eps,1,1,cThres + eps)
              
            }
          }
          else
          {
            
            rThres = decisionTree$RightNode$Threshold
            
            
            if (rThres > cThres)
            {
              xrl = c(0,1,1,0)
              yrl = c(rThres - eps,rThres - eps,1,1)
              
              xrr = c(cThres + eps,1,1,0,0)
              yrr = c(0,0,rThres + eps,rThres + eps,cThres + eps)
              
            }
            else
            {
              xrl = c(1,1,0,0,cThres-rThres - eps)
              yrl = c(rThres - eps,1,1,cThres + eps,rThres - eps)
              
              xrr = c(cThres + eps,1,1,cThres-rThres + eps)
              yrr = c(0,0,rThres + eps,rThres + eps)
              
            }
          }
        }
      }
      
    }
  }
  
  if (decisionTree$CentralNode$Variable == names(W)[1])
  {
    cThres = decisionTree$CentralNode$Threshold
    
    xl = c(0, cThres - eps, cThres - eps, 0)
    yl = c(0, 0, 1, 1)
    
    xr = c(cThres + eps, 1, 1, cThres  + eps)
    yr = c(0, 0, 1, 1)
    
    partition = rbind(data.frame(x = xl, y=yl, subset = "l"),
                      data.frame(x = xr, y=yr, subset = "r"))
    
    
    if (!is.null(decisionTree$LeftNode))
    {
      if (grepl('^Mean', decisionTree$LeftNode$Variable))
      {
        
        lThres = decisionTree$LeftNode$Threshold*2
        
        if (lThres < cThres)
        {
          xll = c(0,lThres - eps,0)
          yll = c(0,0,lThres - eps)
          
          xlr = c(lThres + eps,cThres - eps,cThres - eps,0,0)
          ylr = c(0,0,1,1,lThres + eps)
          
        }
        else
        {
          if (lThres < 1)
          {
            xll = c(0,cThres - eps,cThres - eps,0)
            yll = c(0,0,lThres-cThres - eps,lThres - eps)
            
            xlr = c(cThres - eps,cThres - eps,0,0)
            ylr = c(lThres-cThres + eps,1,1,lThres + eps)
          }
          else
          {
            xll = c(0,cThres - eps,cThres - eps,lThres-1 - eps,0)
            yll = c(0,0,lThres-cThres - eps,1,1)
            
            xlr = c(cThres - eps,cThres - eps,lThres-1 + eps)
            ylr = c(lThres-cThres + eps,1,1)
          }
        }
      }
      else
      {
        if (decisionTree$LeftNode$Variable == names(W)[1])
        {
          
          lThres = decisionTree$LeftNode$Threshold
          
          xll = c(0,lThres - eps,lThres - eps,0)
          yll = c(0,0,1,1)
          
          xlr = c(lThres + eps,cThres - eps,cThres - eps,lThres + eps)
          ylr = c(0,0,1,1)
        }
        else
        {
          lThres = decisionTree$LeftNode$Threshold
          
          xll = c(0,cThres - eps,cThres - eps,0)
          yll = c(0,0,lThres - eps,lThres -eps)
          
          xlr = c(0,cThres - eps,cThres - eps,0)
          ylr = c(lThres + eps,lThres + eps,1,1)
          
        }
      }
    }
    
    if (!is.null(decisionTree$RightNode))
    {
      if (grepl('^Mean', decisionTree$RightNode$Variable))
      {
        
        rThres = decisionTree$RightNode$Threshold*2
        
        if (rThres < 1)
        {
          xrl = c(cThres + eps,rThres - eps,cThres + eps)
          yrl = c(0,0,rThres-cThres - eps)
          
          xrr = c(rThres + eps,1,1,cThres + eps,cThres + eps)
          yrr = c(0,0,1,1,rThres-cThres + eps)
          
        }
        else
        {
          if (rThres < 1 + cThres)
          {
            xrl = c(cThres + eps,1,1,cThres + eps)
            yrl = c(0,0,rThres-1 - eps,rThres-cThres - eps)
            
            xrr = c(cThres + eps,1,1,cThres + eps)
            yrr = c(rThres-cThres + eps,rThres-1 + eps,1,1)
            
          }
          else
          {
            xrl = c(cThres + eps,1,1,rThres-1 - eps,cThres + eps)
            yrl = c(0,0,rThres-1 - eps,1,1)
            
            xrr = c(rThres-1 + eps,1,1)
            yrr = c(1,rThres-1 + eps,1)
            
          }
        }
      }
      else
      {
        if (decisionTree$RightNode$Variable == names(W)[1])
        {
          
          rThres = decisionTree$RightNode$Threshold
          
          xrl = c(cThres + eps,rThres - eps,rThres - eps,cThres + eps)
          yrl = c(0,0,1,1)
          
          xrr = c(rThres + eps,1,1,rThres + eps)
          yrr = c(0,0,1,1)
        }
        else
        {
          rThres = decisionTree$RightNode$Threshold
          
          xrl = c(cThres + eps,1,1,cThres + eps)
          yrl = c(0,0,rThres - eps,rThres - eps)
          
          xrr = c(cThres + eps,1,1,cThres + eps)
          yrr = c(rThres + eps,rThres + eps,1,1)
          
        }
      }
    }
    
  }
  
  if (decisionTree$CentralNode$Variable == names(W)[2])
  {
    cThres = decisionTree$CentralNode$Threshold
    
    xl = c(0,1,1,0)
    yl = c(0,0,cThres - eps,cThres - eps)
    
    xr = c(0,1,1,0)
    yr = c(cThres + eps,cThres + eps,1,1)
    
    partition = rbind(data.frame(x = xl, y=yl, subset = "l"),
                      data.frame(x = xr, y=yr, subset = "r"))
    
    
    if (!is.null(decisionTree$LeftNode))
    {
      if (grepl('^Mean', decisionTree$LeftNode$Variable))
      {
        
        lThres = decisionTree$LeftNode$Threshold*2
        
        if (lThres < cThres)
        {
          xll = c(0,lThres - eps,0)
          yll = c(0,0,lThres - eps)
          
          xlr = c(lThres + eps,1,1,0,0)
          ylr = c(0,0,cThres - eps,cThres - eps,lThres + eps)
          
        }
        else
        {
          if (lThres < 1)
          {
            xll = c(0,lThres - eps,lThres-cThres - eps,0)
            yll = c(0,0,cThres - eps,cThres - eps)
            
            xlr = c(lThres + eps,1,1,lThres-cThres + eps)
            ylr = c(0,0,cThres - eps,cThres - eps)
            
          }
          else
          {
            xll = c(0,1,1,lThres-cThres - eps,0)
            yll = c(0,0,lThres-1 - eps,cThres - eps,cThres - eps)
            
            xlr = c(1,1,lThres-cThres + eps)
            ylr = c(lThres-1 + eps,cThres - eps,cThres - eps)
            
          }
        }
      }
      else
      {
        if (decisionTree$LeftNode$Variable == names(W)[1])
        {
          
          lThres = decisionTree$LeftNode$Threshold
          
          xll = c(0,lThres - eps,lThres - eps,0)
          yll = c(0,0,cThres - eps,cThres - eps)
          
          xlr = c(lThres + eps,1,1,lThres + eps)
          ylr = c(0,0,cThres - eps,cThres - eps)
          
        }
        else
        {
          
          lThres = decisionTree$LeftNode$Threshold
          
          xll = c(0,1,1,0)
          yll = c(0,0,lThres - eps,lThres - eps)
          
          xlr = c(0,1,1,0)
          ylr = c(lThres + eps,lThres + eps,cThres - eps,cThres - eps)
        }
      }
    }
    
    
    if (!is.null(decisionTree$RightNode))
    {
      if (grepl('^Mean', decisionTree$RightNode$Variable))
      {
        
        rThres = decisionTree$RightNode$Threshold*2
        
        if (rThres < 1)
        {
          xrl = c(0,rThres-cThres - eps,0)
          yrl = c(cThres + eps,cThres + eps,rThres - eps)
          
          xrr = c(rThres-cThres + eps,1,1,0,0)
          yrr = c(cThres + eps,cThres + eps,1,1,rThres + eps)
          
        }
        else
        {
          if (rThres < 1 + cThres)
          {
            xrl = c(0,rThres-cThres - eps,rThres-1 - eps,0)
            yrl = c(cThres + eps,cThres + eps,1,1)
            
            xrr = c(rThres-cThres + eps,1,1,rThres-1 + eps)
            yrr = c(cThres + eps,cThres + eps,1,1)
            
          }
          else
          {
            xrl = c(0,1,1,rThres-1 - eps,0)
            yrl = c(cThres + eps,cThres + eps,rThres-1 - eps,1,1)
            
            xrr = c(rThres-1 + eps,1,1)
            yrr = c(1,rThres-1 + eps,1)
            
          }
        }
      }
      else
      {
        if (decisionTree$RightNode$Variable == names(W)[1])
        {
          
          rThres = decisionTree$RightNode$Threshold
          
          xrl = c(0,rThres - eps,rThres - eps,0)
          yrl = c(cThres + eps,cThres + eps,1,1)
          
          xrr = c(rThres + eps,1,1,rThres + eps)
          yrr = c(cThres + eps,cThres + eps,1,1)
          
        }
        else
        {
          
          rThres = decisionTree$RightNode$Threshold
          
          xrl = c(0,1,1,0)
          yrl = c(cThres + eps,cThres + eps,rThres - eps,rThres - eps)
          
          xrr = c(0,1,1,0)
          yrr = c(rThres + eps,rThres + eps,1,1)
          
        }
      }
    }
    
  }
  
  if (!is.null(decisionTree$LeftNode))
  {
  partition = rbind(partition,
                    data.frame(x = xll, y=yll, subset = "ll"),
                    data.frame(x = xlr, y=ylr, subset = "lr"))
  }
  
  if (!is.null(decisionTree$RightNode))
  {
    partition = rbind(partition,
                      data.frame(x = xrl, y=yrl, subset = "rl"),
                      data.frame(x = xrr, y=yrr, subset = "rr"))
  }
  
  return(partition)
  
}

