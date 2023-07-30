
##### 2. Peak mod detection for average spectrum ####################


Ints_avg_mod <- function(clus.n, X){
	avg.all <- apply(X, 2, mean)
    nr <- dim(X)[1]
    nc <- dim(X)[2]
	
	max.int <- peaks(avg.all)
	max.ind <- which(max.int == 1)
    n.max <- sum(max.int==T) # number of local maximum;
    n.max 					
	
	max.vec <- rep(0, nc)
	max.vec[max.ind] <- 2      # Mark the position of local maximum
							   # by 0 and 2;
							   
	Ints <- matrix(NA, nr, max(clus.n))
	for( i in 1: max(clus.n)){ 
	# Calculate intensities for each cluster;
		# max.vect that are in cluster i;
	  #sub.max.vec is a list of whether is local max label in cluster i
		sub.max.vec <- max.vec[which(clus.n == i)]
		# subset of x that are in cluster i;
		sub.x <- as.matrix(X[, which(clus.n == i)]) #num_spec x point in cluster i
		
		# The case that there are local maximum in cluster i;
		
		if(sum(sub.max.vec) > 0){	#there is identified peaks in cluster i			
			# subset of X that contain local maximums in cluster i;
			sub.max.x <- as.matrix(sub.x[, which(sub.max.vec == 2)])#correspond peak intensities in cluster i
			# Calculate average mod intensity;
			Ints[ , i] <- apply(sub.max.x, 1 , mean) 	#take their avergae	
		}
		
		# The case that no local maximum in cluster i;
		
		if(sum(sub.max.vec) == 0){
			# Calculate median value within the cluster as cluster intensity;
			Ints [ , i] <- apply(sub.x, 1, median)		
		}
		
	}
	
	return(Ints) # The intensities for each cluster for each sample; n*max(clus.n)	
}


