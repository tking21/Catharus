simOU <- function(means,tree, n, species) {
  
  library(ape)
  library(geiger)

  d <- matrix(means)
  colnames(d) <- c("value")
  
  rownames(d) <- species
  fitParams <- fitContinuous(tree, d, model="OU", bounds = list(alpha = c(0, 1000)))
  
  #make empty vector to hold results in, what is returned
  results <- vector("list", n)
  
  for (x in 1:n){
    results[[x]] <- rTraitCont(tree, model ="OU", sigma = fitParams$opt[2], alpha = fitParams$opt[1] , theta = fitParams$opt[3], root.value = mean(means))
  }
  
  #return(results)
  for (x in 1:n){
    pic <- pic(results[[x]], tree )
    if (x == 1){
      trait_pic <- pic
    }
    else{
      trait_pic = rbind(trait_pic, pic)
    }
  }
  return(trait_pic)
}
