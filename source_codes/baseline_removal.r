# Baseline Removal

library(baseline)

baseline_removal <- function(dat){
	bs7 <- baseline(dat, method = "peakDetection")


	dat7 <- getCorrected(bs7)
	dat.bl <- dat7

return(list(dat.bl = dat.bl, bs7 = bs7))
}
 
