## Spatial Clustering ###
# --------------------------------------------------------------------------
# For generating the spatical cluster and their corresponding intensities.
# Input:
# 		dat = data matrix, n * p;
#     	k = spatial clustering grouping size;
#     	a1 < a2, and they are the low limit and high limit for gam.lev;
# 		n.gam = Number of gam levels the user wants to generate;
# 		n.tr = Number of samples used as training set;
# 		n.cv = Number of replications in cross validation; 
#		b2 = the uplimit for determination of gamma level. 
#          This variables need to modify according to the feature of input dataset.
#       kern.type = type of kernel to use in calculation of correlation landscape. 
#                   Need to choose one from the following: "unif", "norm", "tri_cube",
#                   "epan". The default is "unif".
#       path = output directory to save the generated figures
#     
# Output:
# 		clus.n = 1* p numeric vector indicating the cluster ID at each PPM 
#       	   posotion. 0 means at variable at that position is not 
#          		grouped in any cluster.    
# 		bond = Cluster boundaries;
# 		ints = Intensities for each cluster. This is calculated by taking the 
#       		 average of peak mods within that cluster.    
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#


Gen.spa.clus <- function(dat, ppm, k=5,method, a1, a2, 
                         n.gam, n.tr, n.cv, b2, 
                         kern.type = "unif",auto_gam,gam,path){ 
  
  p <- ncol(dat)
  cor.da <- cor(dat)
  ind.ori <- seq(p)
  avg.c <- apply(dat, 2, mean)
  
  ptm <- proc.time()
  s1 <- Cor.stat(ind.ori, cor.da, k, method)
  proc.time() - ptm 
  
  s1.land <- Cor.landscape (s1, p, k, kern.type)
  
  # Compare correlation landscape with default value a2.
  a2 <- min(a2, max(s1.land))
  
  avg.c <- apply(dat, 2, mean)
  if(kern.type != "none"){
    
    data_avg=as.data.frame(cbind(ppm,avg.c))
    colnames(data_avg)=c("ppm","avg.c")
    ggplot()+geom_line(aes(ppm,avg.c),
                       data=data_avg)+scale_x_reverse()+
      xlab("Chemical shift (ppm)")+
      ylab("Mean Spectrum")
    ggsave(paste(path,"mean_spetrum.pdf",sep=""),width=8,height=6)
    
    data_cor=as.data.frame(cbind(ppm,s1.land))
    colnames(data_cor)=c("ppm","s1.land")
    ggplot()+geom_line(aes(ppm,s1.land),data=data_cor)+
      scale_x_reverse()+xlab("Chemical shift (ppm)")+
      ylab("Correlation Landscape")
    ggsave(paste(path,"correlation_landscape.pdf",sep=""),width=8,height=6)
    
  }
  
  if(kern.type == "none"){
    
    ppm1 <- ppm[1:(length(ppm)-(k-1))]
    
    
    data_cor=as.data.frame(cbind(ppm1,s1.land))
    colnames(data_cor)=c("ppm1","s1.land")
    ggplot()+
      geom_line(aes(ppm1,s1.land),data=data_cor)+
      scale_x_reverse()+xlab("Chemical shift (ppm)")+
      ylab("Correlation Landscape")
    ggsave(paste(path,"correlation_landscape.pdf",sep=""),width=8,height=6)
    
    data_avg=as.data.frame(cbind(ppm1,avg.c[1:(length(ppm)-(k-1))]))
    colnames(data_avg)=c("ppm1","avg")
    ggplot()+geom_line(aes(ppm1,avg),data=data_avg)+scale_x_reverse()+xlab("Chemical shift (ppm)")+
      ylab("Mean Spectrum")
    ggsave(paste(path,"mean_spetrum.pdf",sep=""),width=8,height=6)
    
    
  }
  
  ################ Gamma Level #######################
  
  if(auto_gam==TRUE){
  ptm <- proc.time()
  re1 <- Pred.stng(dat, n.gam, n.tr, n.cv, k, ppm, a1, a2, kern.type,method)
  proc.time() - ptm  
  psmd <- re1$ps.med
  
  gam.lev <- seq(a2, a1, length.out = n.gam)
  gam <- Select.gam(psmd, b2, gam.lev, n.cv)
  Plot.ps(psmd, gam.lev,path)
  
  }
  if(auto_gam==FALSE){gam = gam}
  land.cut <- landscape_cut(s1.land, gam) #0/2
  clus.mem <- Cluster.member(land.cut)$clus.n #0/group id
  bond <- Cluster.member(land.cut)$bond
  max(clus.mem)

  qplot(ppm, avg.c, colour = land.cut, geom = "line")+ 
    geom_vline(xintercept = as.numeric(ppm)[which(bond== 1)],col = "gray90") + 
    theme(panel.grid.major = element_blank())+
    theme(panel.background = element_blank())
  
  ggsave(paste(path,"spectrum_with_sep.pdf",sep=""),width=8,height=6)
  
  
  
  return(list(clus.mem = clus.mem, land.cut = land.cut, bond = bond, gam = gam))
  
}


