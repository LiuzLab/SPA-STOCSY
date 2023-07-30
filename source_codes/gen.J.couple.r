### Coupling the highly correlated cluters which are above a set threshold ###
# --------------------------------------------------------------------------
# Input:
# 		cor.dat.spa = correlation matrix among clusters (cluster numbers) x (cluster numbers) 
#     	thres = threshold set to define the highly correlated clusters
  	
# Output:
# 		record = lists of highly correlated clusters
#

gen.J.couple <- function(cor.dat.spa, thres){
  record <- list()
  a <- cor.dat.spa
  a[a < thres] <- 0
  a[lower.tri(a)] <- 0
  for(i in 1 : dim(a)[1]){
    lin <- a[i, ]
    rec <- which(lin > 0)
    record[[ i ]] <- rec
   
  }
  return(record = record)
}

