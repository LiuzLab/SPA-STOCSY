# Cut correlation sandscape with a gamma value.
landscape_cut <- function(s1.land, thre){
	p = length(s1.land)
	sel.vab <- rep(0, p)
	sel.vab[which(s1.land > thre)] <- 2
	return(sel.vab)
}