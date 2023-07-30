# Tri-cube kernel

tri_cube_kern <- function(vect){
     
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
			t <- abs(i - mid.lam) / r  ## set to abs<=1 
			dt[i] <- (1 - abs(t)^3)^3
		}
		kt <- dt / sum(dt)
	}
	return(kt)
}
