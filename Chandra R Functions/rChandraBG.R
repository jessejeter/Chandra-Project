rChandraBG <- function(n) {

X <- qChandraBG(runif(n), beta)
Y <- qChandraBG(runif(n), beta)

return(cbind(X, Y))

}
