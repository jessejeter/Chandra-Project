qChandraBG <- function(z, beta=beta.est) {

# beta.est is estimated by ChandraBGcoefs
beta.est <- t(cbind(c( 5.6818182, 11.3636364,  5.68181818),
                    c( 0.5075408,  0.5838935,  0.06736763),
                    c( 0.5000050,  0.2071496, -4.63839330),
                    c( 0.4999998,  0.2055987,  4.67566988),
                    c( 0.4924555,  0.5838909, -0.06736753),
                    c(-4.6434594, 11.2854865, -5.64202320)))

int.range <- c(-1, -.96, -.04, 0, .04, .96, 1)
int.est <- c(0, .44, .56, .20, .56, .44, 0)
area.int <- sum(1/2 * (int.est[1:6] + int.est[2:7]) * diff(int.range))
int.est.norm <- int.est/area.int
int.sum <- c(0, cumsum(1/2 * (int.est.norm[1:6] + int.est.norm[2:7]) * 
                       diff(int.range)))

quant.pt <- rep(0, length(z))
quant.pt <- (z==0)*(-1)
for(i in 1:6) {
  # Only need the "plus" term in the quadratic formula
  quant.pt <- quant.pt + (int.sum[i] < z & z <= int.sum[i + 1]) *
               (-beta[i, 2] + sqrt((int.sum[i] < z & z <= int.sum[i + 1]) *
               (beta[i, 2]^2 - 4 * beta[i, 3] * (beta[i, 1] - z)))) / 
               (2 * beta[i, 3])
}

return(quant.pt)

}
