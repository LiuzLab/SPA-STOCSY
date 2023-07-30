## summarize identified metabolites in each highly-correlated
#  cluster group with a set detection threshold  ###
# --------------------------------------------------------------------------

# Input:
#     new_lib = curated new library with upper and lower limits for each cluster;
#     cor_cluster = matrix of highly correlated clusters;
#     clus_mem = matrix of peaks in each cluster;
#     d.thre = detection rate (ratio of detected clusters over all clusters for one metabolite)

# Output:
#     all_name = list of metabolites names with detection rate higher than d.thre;
#     cand_list = identified metabolites in each group    

FindMeta <- function(new_lib, cor_cluster, clus_mem, d.thre,single_meta,
                     single_filter){
  cand_list <- list()
  now <- proc.time()
  all_name <- c()
  for(i in 1: dim(cor_cluster)[1]){
    temp <- Auto_dect_one(new_lib, cor_cluster[i, ], clus_mem)
    meta_ct <- temp$meta_ct
    meta_rec <- temp$meta_rec

    if(dim(meta_ct)[1] == 1 & meta_ct[1,1] == "No peak"){
      cand_list[[i]] <- "No metabolite"
    }
    if(dim(meta_ct)[1] == 1 & meta_ct[1,1] != "No peak"){
      
      if(meta_ct[1, 5] > d.thre){
        cand_list[[i]] <- meta_ct[1, 1]
      }
      else{
        cand_list[[i]] <- "No qualified candidate"
      }
    }
    if(dim(meta_ct)[1] > 1){
      if(single_filter==TRUE){
        if(length(as.numeric(sort(unique(cor_cluster[i,]))))>1){
          meta_ct = meta_ct[!(as.character(meta_ct$meta_name)%in%single_meta),]
        } 
      }
      cand_list[[i]] <- meta_ct$meta_name[which(meta_ct$dect_rate > d.thre)]
  
      }
    all_name <- c(all_name, unlist(cand_list[[i]]))
    
  }
  proc.time() - now
  return(list(cand_list = cand_list, all_name = all_name))
  
}
