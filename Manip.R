manip <- function(PIC_values, n, tree, means) {
  
  #calculate PIC values for emperical data
  empPIC <- pic(means, tree)
  
  #make list to save p. values and effect size
  pvals <- numeric(max(length(empPIC)))
  esize <- numeric(max(length(empPIC)))
  
  #making vectors to save median and standard deviation values for each node 
  #(from simulated data)
  medians <- numeric(max(tree$Nnode))
  stddev <- numeric(max(tree$Nnode))
  
  #take the absoluted value of contrast passes to function
  #we are only interested in the amount of change, not the direction of change 
  PIC_values <- abs(PIC_values)
  
  #now calculate the median and SD for each column of PIC_values - corresponds to nodes
  for (column in 1:tree$Nnode){
    medians[column] <- median(PIC_values[,column])
    stddev[column] <- sd(PIC_values[,column])
  }
  
  #combine medians and SD into one matrix
  resp1 <- (cbind(medians = medians,stddev=stddev))
  
  #effect size calculations
  for (node in 1:length(esize)){
    esize[node] <- (abs(resp1[node,1]-abs(empPIC[node])))/resp1[node,2]
  }
  
  #now calculating pvals, for each column(or node) in the matrix generated from simBM/simOU, 
  #if the simulatd  PIC value is less than the emperical PIC value, add one to variable t, 
  #t/n number of simulations results in the p value
  
  for (node2 in 1:tree$Nnode){
    t = 0
    for (sim in 1:n){
      if (PIC_values[sim,node2] < abs(empPIC[node2])){
        t <- t+1
      }
    }
    pvals[node2] <- t/n
  }
  
  results = cbind(pvals, esize)
  rownames(results) <- colnames(PIC_values)
  
  return(results)
  
}


