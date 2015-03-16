Cooc_mat <- function(X,Y,bb,type=c("horizontal","vertical","both")) {

#bb <- 0.01

Z <- cbind(rep(seq(-1,1,bb),length(seq(-1,1,bb))), 
     as.vector(t(replicate(length(seq(-1,1,bb)),seq(-1,1,bb)))))

lam_h <- rep(NA,dim(Z)[1])

for(i in 1:dim(Z)[1]) {
  lam_h[i] <- sum(point.in.polygon(X,Y,c(Z[i,1],Z[i,1]+bb,Z[i,1]+bb,Z[i,1])
                                 ,c(Z[i,2],Z[i,2],Z[i,2]+bb,Z[i,2]+bb))>0)
}

M_lam_h <- matrix(lam_h,length(seq(-1,1,bb)),length(seq(-1,1,bb)))
for(i in 1:dim(M_lam_h)[1]) {
  for(j in 1:dim(M_lam_h)[1]) {
    M_lam_h[i,j] <- min(M_lam_h[i,j],7)
  }
}

CM_hor <- matrix(0,8,8)
for(i in 1:(dim(M_lam_h)[1]-1)) {
  for(j in 1:dim(M_lam_h)[1]) {
    CM_hor[M_lam_h[i,j]+1,M_lam_h[i+1,j]+1] <- CM_hor[M_lam_h[i,j]+1,M_lam_h[i+1,j]+1]+1
  }
}

CM_ver <- matrix(0,8,8)
for(j in 1:(dim(M_lam_h)[1]-1)) {
  for(i in 1:dim(M_lam_h)[1]) {
    CM_ver[M_lam_h[i,j]+1,M_lam_h[i,j+1]+1] <- CM_ver[M_lam_h[i,j]+1,M_lam_h[i,j+1]+1]+1
  }
}

CM_hor <- (CM_hor + t(CM_hor))/(2*sum(CM_hor))

CM_ver <- (CM_ver + t(CM_ver))/(2*sum(CM_ver))

CM_both <- 1/2*(CM_hor + CM_ver)

if(type=="horizontal") {
  return(CM_hor)
} else if(type=="vertical") {
  return(CM_ver)
} else if(type=="both") {
  return(CM)
}

}
