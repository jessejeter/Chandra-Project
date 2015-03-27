pChandraBG <- function(x, beta=beta.est) {

# beta.est is estimated by ChandraBGcoefs
beta.est <- t(cbind(c( 5.6818182, 11.3636364,  5.68181818),
                    c( 0.5075408,  0.5838935,  0.06736763),
                    c( 0.5000050,  0.2071496, -4.63839330),
                    c( 0.4999998,  0.2055987,  4.67566988),
                    c( 0.4924555,  0.5838909, -0.06736753),
                    c(-4.6434594, 11.2854865, -5.64202320)))

int.range <- c(-1, -.96, -.04, 0, .04, .96, 1)

sum.ar <- rep(0, length(x))
for(i in 1:6) {
  sum.ar <- sum.ar + (int.range[i] < x & x <= int.range[i + 1]) *
            (beta[i, 1] + beta[i, 2] * x + beta[i, 3] * x^2)
}

return(sum.ar)

}
