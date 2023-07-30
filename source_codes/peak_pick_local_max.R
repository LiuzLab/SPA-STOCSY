##dat hear use the noise clear version
peak_local_max  =function(dat,nos,step){
  avg = apply(dat, 2, mean)
  avg_dn = avg
  avg_dn[avg_dn<nos]=0
  
  peak_list = c()
  for(i in (1+step):(length(avg_dn)-step)){
    neighbor_list = c(c((i-step):(i-1)),c((i+1):(i+step)))
    if(avg_dn[i]>max(avg_dn[neighbor_list])){
      peak_list = c(peak_list,i)
    }

  }
  return(peak_list)
}