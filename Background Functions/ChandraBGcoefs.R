ChandraBGcoefs <- function() {

int.range <- c(-1, -.96, -.04, 0, .04, .96, 1)
int.est <- c(0, .44, .56, .20, .56, .44, 0)
area.int <- sum(1/2 * (int.est[1:6] + int.est[2:7]) * diff(int.range))
int.est.norm <- int.est/area.int
int.sum <- c(0, cumsum(1/2 * (int.est.norm[1:6] + int.est.norm[2:7]) * 
                       diff(int.range)))

xx <- seq(-1, 1, .001)

den.xx <- dChandraBG(xx)

ar.xx <- rep(NA, length(xx))
interval <- rep(NA, length(xx))
for(i in 1:length(xx)) {
  ar.xx[i] <- integrate(dChandraBG, -1, xx[i])$value
  for(j in 1:6) {
    if(xx[i] > int.range[j]) {
      interval[i] <- j
    }
  }
}
interval[1] <- 1

reg <- replicate(6, list())
beta <- matrix(NA, 6, 3)
for(i in 1:6) {
  reg[[i]] <- lm(ar.xx[which(interval == i)] ~ xx[which(interval == i)] + 
							 I(xx[which(interval == i)]^2))
  beta[i, ] <- coef(reg[[i]])
}

return(beta)

}
