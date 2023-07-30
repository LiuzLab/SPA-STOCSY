
# Plot error bar for prediction strength:


## Plot Error Bar for Prediction Strength ###

# --------------------------------------------------------------------------
# For Ploting error bar of prediction strength at each gamma level 
# Input:
# 	  psa = cluster co-membership matrix;
#     gam.lev = gamma level
# Output:
#     Plot of prediction strength with error bar. 
#     
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#
#



Plot.ps <- function(psa, gam.lev,path){
	avg.psa <- apply(psa, 2, mean)
	se_psa <- apply(psa, 2, se)
				
	limits <- aes(ymax = avg.psa + se, ymin=avg.psa - se)

	df1 <- data.frame(
	gam.lev = factor(round(gam.lev,3)),
	avg.psa = avg.psa,
	se = se_psa)

	
	p <- ggplot(df1, aes(y=avg.psa, x=gam.lev),  ylim = c(0,1), main = "psa")
	p +
	geom_point()+ geom_errorbar(limits, width=0.2) + 
	scale_x_discrete(expand = c(0, 0))+
	theme(axis.text.x = element_text(angle = 270, hjust =2, colour = "red"))+
	theme_bw() 
	ggsave(paste(path,"gamma_selection.pdf",sep=""),width=8,height=6)


}


