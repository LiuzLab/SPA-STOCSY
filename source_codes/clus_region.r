## Regional visulation of different clusters plot function###
# --------------------------------------------------------------------------

#  Input:
#         id = list of cluster id to visualize
#         a = lower boundary
#         b = upper boundary


clus_region <- function(id,a,b) {
  clus.ps <- which(clus.mem %in% id)
  colr <- rep(0, length(clus.mem))
  colr[clus.ps] <- 2
  colr_use=colr[which(ppm>a&ppm<b)]
  #	x11()
  qplot(ppm[which(ppm>a&ppm<b)], avg.d1[which(ppm>a&ppm<b)], 
        geom = "line", col = colr_use, ylab = "Intensity (a.u.)", 
        xlab = "Chemical shift (ppm)") + 
    scale_colour_gradient(limits=c(0, 2), low="black", high="red") +
    scale_x_reverse()+
    theme_bw()+
    theme(legend.position = "none",panel.grid.major =element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x =  element_text(size = 12),
          axis.title.y = element_text(angle = 90, size = 12),
          axis.text.x = element_text(size = 12, vjust = 1),
          axis.text.y = element_text(size = 12, hjust = 1)
    )
  
}

