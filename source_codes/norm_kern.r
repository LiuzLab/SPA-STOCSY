# Normal Kernel Smoother: 
Norm.kern <- function(vect){
	lamda <- length(vect)
	
	if(lamda == 1){kt = 1}
	
	if(lamda != 1 ){
	
		if(lamda %% 2 == 1){  # If lamda is an odd number:
							  # chose the middle point as X0. 
			mid.lam <- ceiling(lamda / 2)
			r <- floor(lamda / 2)

		}
		if(lamda %% 2 == 0){  # If Lamda is an even number:
							  # Give the middle two points 
							  # equal weight.
			mid.lam = (lamda + 1) / 2
			r <- lamda / 2
		}	
		dt <- rep(NA, lamda)
		for (i in 1 : lamda){
			dt[i] <- (1 / (sqrt(2*pi)* r)) * 
					  exp(-(i - mid.lam)^2 / (2 * r^2))
		}
		kt <- dt / sum(dt)
		return(kt)
	}
}