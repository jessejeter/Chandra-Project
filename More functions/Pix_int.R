Pix_int <- function(X,Y,bb) {

# bb <- 0.01

Z <- cbind(rep(seq(-1,1,bb),length(seq(-1,1,bb))), 
     as.vector(t(replicate(length(seq(-1,1,bb)),seq(-1,1,bb)))))

lam_h <- rep(NA,dim(Z)[1])

for(i in 1:dim(Z)[1]) {
  lam_h[i] <- 1/(2*pi*bb^2)*sum(exp(-1/(2*bb^2)*((X-Z[i,1])^2+(Y-Z[i,2])^2)))
}

M_lam_h <- matrix(lam_h,length(seq(-1,1,bb)),length(seq(-1,1,bb)))

return(M_lam_h)

}
