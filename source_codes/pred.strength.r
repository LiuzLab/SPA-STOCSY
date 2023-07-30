


Pred.stng <- function(st.n, n.gam, n.tr, n.cv, k, ppm, a1, a2, kern.type,method){

	nsamp <- dim(st.n)[1] 
	nvarb <- dim(st.n)[2]
	p <- nvarb
    
	ps.min <- matrix(NA, n.cv, n.gam)
	ps.avg <- matrix(NA, n.cv, n.gam)
	ps.med <- matrix(NA, n.cv, n.gam)
	
	
	for(i in 1: n.cv){
	# 1. Randomly choose training and testing group;
		ind.all <- seq(nsamp)
		ind.tr <- sample(1:nsamp, n.tr)
		# ind.tr is the index of samples that are 
		# assigned to training dataset;
		ind.tt <- ind.all[-ind.tr]
		# ind.tt is the index of samples that are 
		# assigned to testing dataset;
		dat.tr <- st.n[ind.tr,]
		dat.tt <- st.n[ind.tt,]
		
	# 2. Generate average correlation landscape;
	    # 2.1 for training group:
		ind.ori <- seq(p)
		p <- nvarb
		avg.tr <- apply(dat.tr, 2, mean) 
		cor.tr <- cor(dat.tr)
		
		ptm <- proc.time()
		s.tr <- Cor.stat(ind.ori, cor.tr, k, method)
		proc.time() - ptm 
	 
		s.tr.land <- Cor.landscape(s.tr, p, k, kern.type)
		# 2.2 for testing group;
		avg.tt <- apply(dat.tt, 2, mean) 
		cor.tt <- cor(dat.tt)
		
		ptm <- proc.time()
		s.tt <- Cor.stat(ind.ori, cor.tt, k,method)
		proc.time() - ptm 
	 
		s.tt.land <- Cor.landscape(s.tt, p, k, kern.type)
				

		
		# 2.3 generate cluster based on ...landscape 
		#	  for each gam value for each dataset;
		
		a1 = a1
		a2 = a2
		gam.lev <- seq(a2, a1, length.out = n.gam)
	
			
		for ( j in 1 : n.gam){
			gam <- gam.lev[j]
			cut.s.tr <- landscape_cut(s.tr.land, gam)
			cut.s.tt <- landscape_cut(s.tt.land, gam)
			
			clus.tr <- Cluster.member(cut.s.tr)$clus.n
			clus.tt <- Cluster.member(cut.s.tt)$clus.n
			
			# 3. Comparison and generate D matrix
		    co.meb <- Gen_co_member(clus.tr, clus.tt)
			ps.min[i, j] <- co.meb$ps.min
			ps.avg[i, j] <- co.meb$ps.avg
			ps.med[i, j] <- co.meb$ps.med	
			
		}
	}
	return(list(ps.min = ps.min, ps.avg = ps.avg, ps.med = ps.med))
}
	

		 
		 
		 
		 