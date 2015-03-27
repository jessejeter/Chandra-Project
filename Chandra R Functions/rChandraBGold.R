rChandraBGold <- function(n) {

# See "Background Image Intensity Estimation.R" for derivation
# of intensity estimates.
int.range <- c(-1,-.96,-.04,0,.04,.96,1)
int.est.norm <- c(0, 0.4545455, 0.5785124, 0.2066116, 0.5785124, 0.4545455, 0)

dChandraBG <- function(x) {

  den_x <- 0
  for(i in 1:6) {
    den_x <- den_x + (x > int.range[i] & x <= int.range[i+1]) * 
             (int.est.norm[i] + diff(int.est.norm)[i]/diff(int.range)[i] *
             (x - int.range[i]))

  }

  return(den_x)
}

dd <- .001
xx <- seq(-1, 1, dd)

p_xx <- rep(0,length(xx))
for(i in 1:length(xx)) {
  p_xx[i] <- integrate(dChandraBG,-1,xx[i])$value
}

X.rBG <- rep(NA, n)
Y.rBG <- rep(NA, n)
for(i in 1:n) {
  X.rr <- runif(1)
  Y.rr <- runif(1)

  X.rBG[i] <- xx[max(which(p_xx<X.rr))]+(X.rr-p_xx[max(which(p_xx<X.rr))])/
           (p_xx[max(which(p_xx<X.rr))+1]-p_xx[max(which(p_xx<X.rr))])*dd
  Y.rBG[i] <- xx[max(which(p_xx<Y.rr))]+(Y.rr-p_xx[max(which(p_xx<Y.rr))])/
           (p_xx[max(which(p_xx<X.rr))+1]-p_xx[max(which(p_xx<X.rr))])*dd
}

return(cbind(X.rBG,Y.rBG))

}
