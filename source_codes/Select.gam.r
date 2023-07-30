## Selection of Gamma level ###

# --------------------------------------------------------------------------
# Selecting the optimal Gamma level according to prediction strength.
# Input:
# 	  psmd = cluster co-membership matrix;
#	  b2 = the uplimit for determination of gamma level. 
#          This variables need to modify according to the feature of input dataset.
#	  gam.lev = default Gamma level, with the minimum level equals 0.85, 
#				and the maximum equals 0.98. 
#	  n.cv = number of replications for cross validation. 
# Output:
#     gam = selected Gamma level. 
#     
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#
#



se <- function(psa){
	se <- sd(psa)/sqrt(n.cv) 
	return(se)
}

Select.gam <- function(psmd, b2, gam.lev, n.cv){
	avg.psa <- apply(psmd, 2, mean)
	se_psa <- apply(psmd, 2, se)

	pos.lim <- which(gam.lev < b2) 
	
	avg.psa.set <- avg.psa[pos.lim]
	gam.lev.set <- gam.lev[pos.lim]
	se_psa.set <- se_psa[pos.lim]
	mod.pos <- which(avg.psa.set == max(avg.psa.set)) 
	# -2 means the first and second gamlevel usually not stable, and should not be
	# the optimal candidate;
	low.lim <- avg.psa.set[mod.pos] - se_psa.set[mod.pos]
	low.lim

	if(mod.pos != length(pos.lim)){ 

		for(i in (length(pos.lim)): (mod.pos + 1)){
		
			if(avg.psa.set[i] > low.lim){
				gam <- gam.lev.set[i]
				break()
			}
		}
		
		 
		if(sum(avg.psa.set[(mod.pos+1):length(pos.lim)] > low.lim) < 1){
			gam <- gam.lev.set[mod.pos + 1]
		}
	}
	
	if(mod.pos == length(pos.lim)){ # The maximum is at the first(gam = 0.85) position.
		for(i in (length(pos.lim)): 1){
		
			if(avg.psa.set[i] > low.lim){
				gam <- gam.lev.set[i]
				break()
			}
		}
		if(sum(avg.psa.set[1 : (length(pos.lim) - 1)] > low.lim) < 1){
			gam <- 0.1   # ERROR MARK, if 0.1 occur, there must be a problem
		}
	}
	
	return(gam)
}

	
