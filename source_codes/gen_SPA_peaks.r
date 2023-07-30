## Summary of peaks in each cluster into matrix and prepare for 
##  automatic identification of metabolites ###

# Input:
# 		profile = list of groups with highly correlated clusters and peaks;
#     	cluster_peak = data frame of clusters and corresponding peaks;
 
# Output:
#		clus_mem = a matrix, with clusters as row and peaks inside as column


gen_SPA_peaks=function(profile,cluster_peak){
  num_cluster=length(profile)
  num_peak=NULL
  for(i in 1:num_cluster){
  num_peak=c(num_peak,length(which(cluster_peak$cls==i)))
}

  clus_mem=matrix(NA,nrow=num_cluster,ncol=max(num_peak))
  
  for(i in 1:num_cluster){
  if(sum(is.na(cluster_peak[which(cluster_peak$cls==i),2]))==0){
    clus_mem[i,1:length(which(cluster_peak$cls==i))]=cluster_peak[which(cluster_peak$cls==i),2]
  } 
}
 
return(clus_mem)
}
