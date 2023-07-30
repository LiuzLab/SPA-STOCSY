## Visulation of different clusters plot function###
# --------------------------------------------------------------------------

#  Input:
# 		id = list of cluster id to visualize


clus <- function(id) {
  clus.ps <- which(clus.mem %in% id)
  colr <- rep(0, length(clus.mem))
  colr[clus.ps] <- 2
  
  qplot(ppm, avg.d1, geom = "line", col = colr, ylab = "Intensity (a.u.)", xlab = "Chemical shift (ppm)") + 
    scale_colour_gradient(limits=c(0, 2), low="black", high="red") +
    scale_x_reverse(lim=c(4,0.5))+
    theme_bw()+
    theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),
          axis.line = element_line(colour = "black"))
  
}
