## Generate new library with upper and lower limits for each cluster ###
# --------------------------------------------------------------------------

# Input:
#     meta_name = list of metabolites names in the Chenomx library as reference;
#     ppm_shift_thre = threshold to add and minus from the central cluster postiion
#                      to determine the upper and lower limits;

# Output:
#     new_lib = data frame of upper, lower, central positions for each cluster in all the metabolites


Build_new <- function(meta_name, ppm_shift_thre){
  new_lib <- c()
  for ( i in 1 : length(meta_name)){
    temp <- Chx_lib[Chx_lib$meta_name %in% meta_name[i], ]
    new_ct <- unique(temp$clus_ct)
    n.ct <- length(new_ct)
    up.thre <- new_ct + ppm_shift_thre
    low.thre <- new_ct - ppm_shift_thre
    new_comp <- cbind(rep(meta_name[i], n.ct), new_ct, up.thre, low.thre)
    new_lib <- rbind(new_lib, new_comp)
  }
  new_lib <- as.data.frame(new_lib)
  colnames(new_lib) <- c("meta_name", "clus_ct", "ul", "ll")
  return(new_lib)
}
