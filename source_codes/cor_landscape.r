## Generation of landscape ###

# --------------------------------------------------------------------------
# For generating landscape values of a given numeric vector and a window size, k.
# Input:
# 	  line = the numeric vector will be transformed; 
#     p = length of total variables;
#     k = window size;
# Output:
#     cor.line = transformed landscape vector.
#     
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#
#


Cor.landscape <- function(line, p, k, kern.type){
    cor.line <- rep(NA, p)
    line.comp <- c(rep(NA,(k-1)), line, rep(NA, (k-1)))
	
	if(kern.type == "none"){
		cor.line <- line
	}
	
	if(kern.type == "norm"){
		for(i in 1 : p){
		    weight <- Norm.kern(na.omit(line.comp[i:(i+k-1)]))
			cor.line[i] <- sum(weight * (na.omit(line.comp[i:(i+k-1)])))
		}	
	}
	
	if(kern.type == "tri_cube"){
		for(i in 1 : p){
		    weight <- tri_cube_kern(na.omit(line.comp[i:(i+k-1)]))
			cor.line[i] <- sum(weight * (na.omit(line.comp[i:(i+k-1)])))
		}	
	}
	
	if(kern.type == "epan"){
		for(i in 1 : p){
		    weight <- Epane_kern(na.omit(line.comp[i:(i+k-1)]))
			cor.line[i] <- sum(weight * (na.omit(line.comp[i:(i+k-1)])))
		}	
	}
	
	if(kern.type == "unif"){
		for(i in 1 : p){
		    cor.line[i] <- mean(na.omit(line.comp[i:(i+k-1)]))
		}
	}
    return(cor.line)
}


	
	
	
	
