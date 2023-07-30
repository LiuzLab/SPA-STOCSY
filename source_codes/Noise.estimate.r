# Estimation of the Noise:
# Input: dat = n by p data matrix;
# 	     ppm = chemical shift unit, parts per million;
#        pos.standar = reference noise region (could adjust to suit different types of data)

# Output: est.noise.std = estimated noise standard deviation.
#						  This is the standard deviation at 
# 						  the noise region.
#

Noise.est <- function(dat, pos.standar = c(0.08, 0.58), ppm){
	
	est.ind <- which(ppm < pos.standar[2] & ppm > pos.standar[1])
	nos.subset <- dat[, est.ind]
	nos.std <- apply(nos.subset, 1, sd)
	est.noise.std <- mean(nos.std)
	return(est.noise.std)
}
