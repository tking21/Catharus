simBM <- function(means,tree, n, species) {
  
  #make a empty matrix to save results, # of rows = # of species; # of cols = # of simulations
  results = matrix (,nrow=tree$Nnode+1 , ncol= n)
  
  #make means parameter into matrix
  d <- matrix(means)
  
  #set colume names of vector to "value"
  colnames(d) <- c("value")
  
  #set row names to species, names passed to fuction
  rownames(d) <- species
  
  #estimates parameters for BM process from tree and mean trait values
  fitParams<- fitContinuous(tree, d, model="BM")
  
  #simulate data along tree; using fitted parameters for rate and root
  sim_results<- sim.char(tree, fitParams$opt$sigsq, n, model = "BM", fitParams$opt$z0)
  
  #changing format of results for PIC fucntion, save to results matrix?
  for (x in 1:n){
    results[,x] <- sim_results[,,x]
    #print(results[[x]])
  }
  
  #calculatin phylogenetic independnt contrasts (PIC) for each simulation, 
  #pass function simulated data and tree; first PIC stored to trait_pic (when x ==1); 
  #other added to trait_pic using rbind
  for (x in 1:n){
    pic_calcs <- pic(results[,x], tree )
    if (x == 1){
      trait_pic <- pic_calcs
    }
    else{
      trait_pic = rbind(trait_pic, pic_calcs)
    }
  }
  
  
  return(trait_pic)
}
