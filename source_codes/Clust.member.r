# Cluster.member() is a function to transfer values(0,2) to
# cluster information.

# Input: clust.info = numberic values in terms of 0 and 2, in which 0 means
#                not significant, and 2 means significant;
# Output: clus.n = a 1*p numeric vector indicating which variables are
#                  belonging to which group;
#         ind.clu = a 1*p numeric vector used to separate consecutive 
#                   clusters with 1 and -1 for each pair of consecutive clusters;
#         bond = boundaries for each cluster, with 1 at boundary position and 
#                0 at other variables.
# ------------------------------------------------------------------------
#        ** Xu Han - Baylor College of Medicine (2011) **
#
#


Cluster.member <- function(clust.info){
    
    p <- length(clust.info)
    ind.clu <- rep(1, p)
    clus.n <- rep(1, p)
    bond <- rep(0,p)
    for( ii in 2 : p){
        ind.clu[ii] <- ind.clu[ii-1]
        clus.n[ii] <- clus.n[ii-1] 
        if(clust.info[ii] == 2 && clust.info[ii] != clust.info[ii-1]){
            clus.n[ii] <- clus.n[ii-1] + 1
            ind.clu[ii] <- -1 * ind.clu[ii-1]
            bond[ii] <- 1 
        }
        if(clust.info[ii] == 0 && clust.info[ii] != clust.info[ii-1]){bond[ii-1] <- 1}   
    } 

    if(clust.info[1] == 0){clus.n <- clus.n -1}
    ind.clu[which(clust.info == 0)] <- 0
    clus.n[which(clust.info == 0)] <- 0
    
    return(list(clus.n = clus.n, ind.clu = ind.clu, bond = bond))
}
