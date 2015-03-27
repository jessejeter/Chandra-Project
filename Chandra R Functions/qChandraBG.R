qChandraBG <- function(z, beta) {

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
