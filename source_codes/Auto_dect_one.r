## Auto_dec_one detect the metabolites for one group of highly correlated clusters ###
# --------------------------------------------------------------------------
# For generating the spatical cluster and their corresponding intensities.
# Input:
#     new_lib = curated new library with upper and lower limits for each cluster;
#     one_grp_high_corr = highly correlted clusters;
#     clus_mem = peak lists in each clusters

# Output:
#     res = two tables of detected results for every metabolite in the reference library


Auto_dect_one <- function(new_lib, one_grp_high_corr, clus_mem){
  
  cor_cluss <- one_grp_high_corr

  # find ppms in cor_cluss;
  pks <- clus_mem[which(clus_mem$clus_id %in% cor_cluss), 2:ncol(clus_mem)]
  pks_list=NULL
  for(i in 1:nrow(pks)){
    pks_list=c(pks_list,as.numeric(pks[i,]))
  }
  pks=pks_list
  # remove NA's using sort() function;
  pks <- sort(pks) #pks'ppm in all the correlated clusters

  # Case one: no peak identified in this cluster:
  if(length(pks) == 0){
    meta_ct <- c("No peak", NA, NA, NA, 0)
    meta_ct <- t(as.data.frame(meta_ct))
    meta_rec <- c("No peak", NA, NA, NA, 0)
    meta_rec <- t(as.data.frame(meta_rec))
    
  }
  meta_name <- as.character(unique(Chx_lib$meta_name)) # 302 metabolites
  
  # Case two: at least one peak is identified in this cluster:
  if(length(pks) > 0){
    # Build new datamatrix to record results:
    meta_ct <- matrix(NA, length(meta_name), 5)
    meta_ct <- as.data.frame(meta_ct)
    colnames(meta_ct) <- c("meta_name", "n_tot_clus", "n_known_clus", "n_dect", "dect_rate")
    meta_rec <- c()
    for(mts in 1: length(meta_name)){
      # check how many meta_clusters are identified 
      # for each metabolite;
      current_meta <- new_lib[which(new_lib$meta_name %in% meta_name[mts]), ]
      n_total_clus <- length(current_meta$clus_ct)
      known_clus <- current_meta[which(as.vector(current_meta$clus_ct) < 4), ]
      n_known_clus <- dim(known_clus)[1]
      n_unknown_clus <- sum(as.vector(current_meta$clus_ct) > 4)
      # for each cluster center of this metabolite, check how many
      # of them are detected:
      # ncl = number of cluster centers.
      ct <- rep(0, dim(known_clus)[1])
      if(n_known_clus == 0){
        meta_ct_mts <- c(meta_name[mts], NA, NA, NA, 0)
        meta_rec_mts <- c(meta_name[mts], NA, NA, NA, 0)
      }
      if(n_known_clus > 0){				
        for(ncl in 1 : dim(known_clus)[1]){
          reg <- c(as.vector(known_clus$ll[ncl]), as.vector(known_clus$ul[ncl]))
          ct[ncl] <- PkinReg(pks, reg)
        }
        n_dect <- sum(ct > 0)
        dec_rate <- n_dect/n_known_clus
        # record the detection numbers and rate;
        meta_ct_mts <- c(meta_name[mts], n_total_clus, n_known_clus, n_dect, dec_rate)
        
        # record the number of peaks in each metabolite cluster:
        meta_rec_mts <- cbind(known_clus, ct)
        
      }
      meta_ct[mts,] <- meta_ct_mts
      meta_rec <- rbind(meta_rec, meta_rec_mts)
    }	
    meta_rec <- as.data.frame(meta_rec)
  }
  colnames(meta_rec) <- c("meta_name", "clus_ct", "ul", "ll", "n_spa_peaks")
  colnames(meta_ct) <- c("meta_name", "n_tot_clus", "n_known_clus", "n_dect", "dect_rate")
  return(list(meta_ct = meta_ct, meta_rec = meta_rec))
}

