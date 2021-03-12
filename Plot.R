#heat map of effect size values or p values
  
  
#plot tree with dots on nodes that correlated to the magnitude of the effect size or 
#p value 
  
  

#density plot of simulated PIC values for nodes with true PIC indicated (as line)


#eidt this later to add color parameter 
plotNodes <- function(tree, parameter, sd, vals){
  #tree = phylogeny
  #parameter = either esize or pvals; what parameter are we plotting on the tree
  #sd: for esize, label nodes that are this many standard deviations away from true median
  #vals = results from manip.R
  
  #figure highlighting nodes that have median PIC values (simulated data) _ standard 
  #deviations away from the true PIC value from the emperical data - or greater than some 
  #pvalue
  
  if (parameter == esize){
    #vector to save nodes that are to be labeled on the tree
    nodes <- vector()
    
    #get effect sizes from vals paramter - results from manip.R
    esizes <- as.matrix(vals[,2])
    
    #get node labels 
    nodeNum <- rownames(esizes)
    
    #now loop through each value for effect size
    for (value in 1:length(esizes)){
      #if effect size is greater than sd... 
      if(esizes[value] > sd){
        nodes <- c(nodes, nodeNum[value])
      }
    }
    nodes = as.numeric(nodes)
    
    
    plot(testTree)
    nodelabels(,nodes, pch = 19, col = , cex =2)
    
  }
  else {
    #vector to save nodes that are to be labeled on the tree
    nodes <- vector()
    
    #get effect sizes from vals paramter - results from manip.R
    pvals <- as.matrix(y[,1])
    
    #get node labels 
    nodeNum <- rownames(pvals)
    
    #now loop through each value for effect size
    for (value in 1:length(pvals)){
      #if effect size is greater than sd... 
      if(pvals[value] > sd){
        nodes <- c(nodes, nodeNum[value])
      }
    }
    nodes = as.numeric(nodes)
    
    
    plot(testTree)
    nodelabels(,nodes, pch = 19, col = , cex =2)
    
  }
  return(nodes)
}
  
  
