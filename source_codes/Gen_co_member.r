# Generate D matrix (co-membership);

Gen_co_member <- function(clus.tr, clus.tt){
	p <- length(clus.tt) 
	# p = number of variables, in simulation

	n.tr <- max(clus.tr)
	n.tt <- max(clus.tt)
	D <- matrix(0, p, p)
	# ct.tt is a list of positions of 
	# the centroid in each cluster in 
	# testing dataset;
	ct.tt <- Gen.ct(clus.tt)
	
	# tr.sel.clu = cluster IDs from training dataset
	# that contain centroids from testing dataset.
	tr.sel.clu <- clus.tr[ct.tt]
	
	# Find the cluster ID in training dataset that 
	# contains at least one centeroid from testing
	# dataseet;
	tr.sel.use <- tr.sel.clu[which(tr.sel.clu > 0)]
	
	# Select the cluster ID in testing dataset whose
	# centroid is in one cluster in training dataset;
	ind.tt <- seq(n.tt)
	tt.sel.use <- ind.tt[which(tr.sel.clu > 0)]

	
	for( m in 1: length(tt.sel.use)){
	# For the training clusters that has testing centroids 
	# and the testing cluster whose centroids located in one
    # training cluster:
	
		tr.sel.ind <- which(clus.tr %in% tr.sel.use[m])
		tt.sel.ind <- which(clus.tt %in% tt.sel.use[m])
		D[tr.sel.ind , tt.sel.ind ] <- 1
	
	}
	
	# Calculate prediction strength for each cluster in 
	# testing dataset;
	ps.k <- rep(NA, n.tt)
	for( l in 1: n.tt){
	    l.tt <- which(clus.tt == l)
		n.l.tt <- length(l.tt) # number of variables in
		                       # the lth cluster in testing set;
		if(n.l.tt == 1){
			ps.k[l] = 0
		}
		if(n.l.tt > 1){
			D.tt <- D[l.tt , l.tt]
			ps.k[l] <- (sum(D.tt) - sum(diag(D.tt)))/(n.l.tt*(n.l.tt - 1))
		}
	}
	ps.min <- min(ps.k)
	ps.avg <- mean(ps.k)
	ps.med <- median(ps.k)
	
	ps.k.po <- ps.k[which(ps.k > 0)]
	ps.min.po <- min(ps.k.po)
	ps.avg.po <- mean(ps.k.po)
	ps.med.po <- median(ps.k.po)
    
	return(list(ps.min = ps.min, ps.avg = ps.avg, ps.med = ps.med, 
	ps.min.po = ps.min.po, ps.avg.po = ps.avg.po, ps.med.po = ps.med.po))
}
	
	
	
	
	