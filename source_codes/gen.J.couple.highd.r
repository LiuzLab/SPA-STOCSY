### Coupling the highly correlated cluters which are above a set threshold ###
# --------------------------------------------------------------------------
# Input:
# 		cor.dat.spa = correlation matrix among clusters (cluster numbers) x (cluster numbers) 
#     	thres = threshold set to define the highly correlated clusters

# Output:
# 		record = lists of highly correlated clusters
#

gen.J.couple <- function(cor.dat.spa, thres,peak_clus,method){
  record <- list()
  a <- cor.dat.spa
  clus_corr = matrix(0,max(peak_clus),max(peak_clus))
  for(i in 1 : max(peak_clus)){
    for(j in i:max(peak_clus)){
      row_index=which(peak_clus==i)
      col_index=which(peak_clus==j)
      a_clus = a[row_index,col_index]
      if(method=="max"){
        clus_corr[i,j]=max(a_clus)}
      if(method=="mean"){
        if(i==j){ 
          clus_corr[i,j]=1
        }
        else{clus_corr[i,j]=mean(a_clus)}
        }

    }
  }
  
  clus_corr[clus_corr < thres] <- 0
  clus_corr[lower.tri(clus_corr)] <- 0
  for(i in 1:max(peak_clus)){
  lin <- clus_corr[i, ]
  rec <- which(lin > 0)
  record[[ i ]] <- rec
  }
  return(record = record)
}

