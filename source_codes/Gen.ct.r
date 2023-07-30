## Generation of the Centeriod from Testing Dataset ###

# --------------------------------------------------------------------------
# For generating the centriod from Testing Dataset
# Input:
# 	  clus.n = 1* p numeric vector indicating the cluster ID at each PPM 
#          posotion. 0 means at variable at that position is not 
#          grouped in any cluster.  
#     
# Output:
#     ct = Position of centeriods along PPM axis.
#     
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#
#
Gen.ct <- function(clus.n){
	ct <- rep(NA, max(clus.n))
	for( i in 1 : max(clus.n)){
		clus.idx <- which(clus.n == i)
		# the variables index in the ith cluster;
		ct[i] <- ceiling(median(clus.idx))
	}
	return(ct)
}