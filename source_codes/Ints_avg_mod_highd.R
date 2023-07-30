
##### Peak mod detection for average spectrum ####################
peak_local_max  =function(dat,nos,step){
  avg = apply(dat, 2, mean)
  avg_dn = avg
  avg_dn[avg_dn<nos]=0
  
  peak_list = c()
  for(i in (1+step):(length(avg_dn)-step)){
    neighbor_list = c(c((i-step):(i-1)),c((i+1):(i+step)))
    if(avg_dn[i]>max(avg_dn[neighbor_list])){
      peak_list = c(peak_list,i)
    }
    
  }
  return(peak_list)
}

Ints_avg_mod_highd <- function(clus.n, X,nos,step){
  avg.all <- apply(X, 2, mean)
  nr <- dim(X)[1]
  nc <- dim(X)[2]
  
  peak_list = peak_local_max(X,nos,step)
  max.int <- rep(0,nc)
  max.int[peak_list]=1
  max.ind <- which(max.int == 1)
  n.max <- sum(max.int==T) # number of local maximum;
  n.max 					
  
  max.vec <- rep(0, nc)
  max.vec[max.ind] <- 2      # Mark the position of local maximum
  # by 0 and 2;
  
  Ints <- matrix(0, nr, 1)
  
  peak_clus = c()
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
      Ints = cbind(Ints,sub.max.x) 	#add every peak to the matrix
      peak_clus=c(peak_clus,rep(i,ncol(sub.max.x)))
    }
    
    # The case that no local maximum in cluster i;
    
    if(sum(sub.max.vec) == 0){
      # Calculate median value within the cluster as cluster intensity;
      Ints <- cbind(Ints,apply(sub.x, 1, median))
      peak_clus = c(peak_clus,i)
    }
    
  }
  
  return(list(clus_peak=Ints[,c(2:ncol(Ints))],
              peak_clus = peak_clus)) # The intensities for each cluster for each sample; n*max(clus.n)	
}

