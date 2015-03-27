pChandraBG <- function(x, beta) {

sum.ar <- rep(0, length(x))
for(i in 1:6) {
  sum.ar <- sum.ar + (int.range[i] < x & x <= int.range[i + 1]) *
            (beta[i, 1] + beta[i, 2] * x + beta[i, 3] * x^2)
}

return(sum.ar)

}
