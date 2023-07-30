# Align spectrum according to DSS which is located a 0PPM;
 
# Input: region = the ppm region where standard locates;
#        dat = n by p data matrix, in which columns stands for
#              variables and rows represent samples;
#        ppm = chemical shifts unit, pars per million; 
#		 ali.mark = the theoriotical ppm of the standard(such as DSS), can be estimated as the minimum absolute value for ppm
# Output:
#        adat  = aligned data matrix. (n by p)
#		 appm  = updated ppm list

Align <- function(region, data, ppm, ali.mark){
	tdat <- t(data) 
	# tdat is a p by n data matrix;
	n <- dim(tdat)[2]	
	p <- dim(tdat)[1]
		
	
    for(i in 1 : n){
		ali.pos <- which(ppm > region[1] & ppm < region[2])
		ali.ppm <- ppm[which(ppm > region[1] & ppm < region[2])]
		pos.mk.ppm <- which(ali.ppm == ali.mark)
		rel.max.ppm <- which(tdat[, i][ali.pos] == max(tdat[, i][ali.pos]))
		
		move = rel.max.ppm - pos.mk.ppm
		if(move > 0 ){
			tdat[, i] <- c(tdat[,i][-c(1:abs(move))], rep(NA, abs(move)))
			
		}
		if(move < 0 ){
			tdat[, i] <- c(rep(NA, abs(move)), tdat[,i][-c(p : (p - abs(move) + 1))])
			
		}
		if(move == 0){
			tdat[, i] <- tdat[, i]
			
		}
		
	}
	ind.sum <- apply(tdat, 1, sum)
    if(length(which(ind.sum %in% NA))>0){
	tdat <- tdat[-which(ind.sum %in% NA), ]
        appm <- ppm[-which(ind.sum %in% NA)]}
	return(list(tdat= tdat, appm = appm))
}




	
	
	
	
	
	
	
	

	
	
	
