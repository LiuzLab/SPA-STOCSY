## Summary of highly correlated clusters prepare for 
##  automatic identification of metabolites ###

# Input:
# 		profile = list of groups with highly correlated clusters and peaks;

# Output:
#		spa_stocsy_corr = a matrix, with clusters as row, and clusters 
#							that highly correlated with it as columns



gen_stocsy_corr=function(profile){
  num_clus=NULL
  for(i in 1:length(profile)){
    num_clus=c(num_clus,length(profile[[i]][[1]]))
  }
  
  spa_stocsy_corr=matrix(NA,nrow=length(profile),ncol=max(num_clus))
  for(i in 1:length(profile)){
    spa_stocsy_corr[i,1:length(profile[[i]][[1]])]=profile[[i]][[1]]
  }
  return(spa_stocsy_corr)
  
}
