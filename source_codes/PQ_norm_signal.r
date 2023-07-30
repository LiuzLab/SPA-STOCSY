# Probablistic quotient normalization:
# dat is p by n matrix, with n samples and p variables. 

qunt <- function(x, y){
	re = x/y
	# in case there is 0s in y
	re[which(is.na(re) == T)] <- 10^10
return(re)
}


PQ_norm_signal <- function(dat, method = "median", ppm, reg=c(0.08,0.58)){
	p <- dim(dat)[1]
	n <- dim(dat)[2]
	st.dat <- dat[which(ppm < reg[2] & ppm > reg[1]), ]
	sd.val <- apply(st.dat, 1, sd)
	sd.lev <- mean(sd.val)
	nos.lev <- 10 * sd.lev
	avg.dat <- apply(dat, 1, mean)	
	sel.pos <- which(avg.dat > nos.lev)
	length(sel.pos)
	qplot(ppm, avg.dat, geom = "line") + geom_hline(yintercept = nos.lev)
	
	sel.dat <- dat[sel.pos, ]
	dim(sel.dat)
	if(method == "median"){
		ref <- apply(sel.dat, 1, median)
	}
	if(method == "mean"){
		ref <- apply(sel.dat, 1, mean)
	}
	quot <- apply(sel.dat, 2, qunt, ref)
	quot.final <- apply(quot, 2, median)
	
	return(list(quot = quot, quot.final = quot.final))

}

