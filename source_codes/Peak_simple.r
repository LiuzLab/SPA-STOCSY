# This file demonstrates some of the basic functionalities of msProcess, 
# the spectra processing component of the S-PLUS software S+Proteome:
#
# * denoising
# * local noise estimation
# * baseline correction
# * intensity normalization
# * peak detection
#
# NOTE: The parameter setting for the following processing functions 
# might not be optimal for the dataset used. A user is encouraged to 
# try out different parameter settings. 

#==============================================================================
Peak_simple <- function(st.n, ppm, thresh.scale = 2, snr = 2, snr.thresh=10){

  p = length(ppm)
  
  avg.c <- apply(st.n, 2, mean)
#------------------------------------------------------------------------------ 

  z1 <- msSet(t(st.n), mz = ppm, data.name = NULL)
  
#------------------------------------------------------------------------------  
  # denoising 
  # the output will be an msSet object with an additional element "noise"
  # NOTE: we are overwriting the input with the output  
  z2 <- msDenoise(z1, FUN="wavelet", thresh.scale= thresh.scale)
  

#------------------------------------------------------------------------------	
  # local noise estimation
  #	the output will be an msSet object with an additional element "noise.local"
  z3 <- msNoise(z2, FUN="mean")
  
#------------------------------------------------------------------------------				
  # baseline correction
  #	the output will be an msSet object with an additional element "baseline"
  z4 <- msDetrend(z3, FUN="monotone")
  

#------------------------------------------------------------------------------			
  # intensity normalization
  #	the output will be an msSet object with an additional element "tic"
  z5 <- msNormalize(z4)
  
	
#------------------------------------------------------------------------------
  # peak detection
  #	the output will be an msSet object with additional elements "peak.list" and "use.mean"
  z6 <- msPeak(z5, FUN="simple", use.mean = T, snr=snr)
		### !!!!!!!!!!!!!! ATTENTION!!!!!!!!! change "use.mean"


#------------------------------------------------------------------------------			
  # extract information from z6. 
  pk.class <- z6$peak.class
  pk.class <- as.data.frame(pk.class)
  idx.pk <- round(pk.class$tick.loc)
  idx.left <- pk.class$tick.left
  idx.right <- pk.class$tick.right
  ppm.pk <- pk.class$mass.loc
  ppm.left <- pk.class$mass.left
  ppm.right <- pk.class$mass.right
  
  clus.mem <- rep(0, p)
  clus.col <- rep(0, p)
  for(i in 1:dim(pk.class)[1]){
	clus.mem[idx.left[i] : idx.right[i]] <- i
	clus.col[idx.left[i] : idx.right[i]] <- 2
  }
 

print("Number of peaks detected:")
  print(length(pk.class$tick.loc))
    plot(ppm, avg.c, type = "l") + 
	 points(x = ppm[idx.pk], y = avg.c[idx.pk], col = "red") 

  return(list(clus.mem = clus.mem, clus.col = clus.col, idx.pk = idx.pk))
}
#==============================================================================

