rChandraBG <- function(n, ...) {

X <- qChandraBG(runif(n), ...)
Y <- qChandraBG(runif(n), ...)

return(cbind(X, Y))

}
