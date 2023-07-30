## Summary of peaks from highly correlated clusters into groups ###
# --------------------------------------------------------------------------

# Input:
#     cor_clus = list of highly correlated clusters;
#     cluster_peak = clusters and the deteced peaks in easch cluster;

# Output:
#     summary = list of groups, with highly correlted clusters and the 
#               combination of peaks inside


gen_summary_profile=function(cor_clus,cluster_peak){
  profile=list()
  for(i in 1:length(cor_clus)){
    clus=cor_clus[[i]]
    clus_peak=cluster_peak[which(cluster_peak$cls%in%clus),]
    profile[[i]]=clus_peak
  }
  summary=list()
  for(i in 1:length(cor_clus)){
    summary[[i]]=list()
    summary[[i]][1]=as.data.frame(cor_clus[[i]],ncol=1)
    sum_profile=profile[[i]]
    sum_profile=rbind(c("cls","cls.pk"),sum_profile)
    sum_profile[,1]=paste(sum_profile$cls,sum_profile$cls.pk,sep=" ")
    sum_profile=sum_profile[,-2]
    sum_profile=as.data.frame(as.character(sum_profile))
    colnames(sum_profile)="summary"
    sum_profile$summary=as.character(sum_profile$summary)
    
    summary[[i]][2]=sum_profile
  }
  return(summary)
}



