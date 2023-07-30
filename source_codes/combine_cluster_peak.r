##combine clusters and peaks information ###
# --------------------------------------------------------------------------

# Input:
#     clus.mem = cluster member assigned for every ppm variable;
#     ppm = list of ppm variables;
#     pk.ppm = list of ppm variables for detected peaks in the previous step


# Output:
#     cluster_peak = a data frame with first column of cluster id and 
#                     second column of peak ppm
      

combine_cluster_peak=function(clus.mem,ppm,pk.ppm){
  cluster_peak=NULL
  for(i in 1: max(clus.mem)){
    clus.rg <- ppm[which(clus.mem ==i)]
    clus.pk <- pk.ppm[pk.ppm %in% clus.rg]
    
    if(length(clus.pk)!=0){
      for(j in 1:length(clus.pk)){
        cluster_peak=rbind(cluster_peak,c(i,clus.pk[j]))
      }}else{cluster_peak=rbind(cluster_peak,c(i,NA))}
  }
  cluster_peak=as.data.frame(cluster_peak)
  colnames(cluster_peak)=c("cls","cls.pk")
  return(cluster_peak)
}
