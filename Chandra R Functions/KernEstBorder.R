### This function needs some work ###
KernEstBorder <- function(X, Y, ss, kernel = c("gaussian", 
                          "epanechnikov", "rectangular")) {

# Quadratize the window with squares of length ss.
Z <- QuadWindow(ss)

int.est <- rep(NA, dim(Z)[1])
ss.tr <- rep(NA, dim(Z)[1])
dist.2D <- matrix(NA, dim(Z)[1], length(X))

for(i in 1:dim(Z)[1]) {
  ss.tr[i] <- min(ss, 1 - abs(Z[i, 1]), 1 - abs(Z[i, 2]))
  dist.2D[i, ] <- sqrt((X - Z[i, 1])^2 + (Y - Z[i, 2])^2) / ss.tr[i]
}

if(kernel == "gaussian") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(2 * pi * ss.tr[i]^2) * exp(-1/2 * dist.2D[i, ]^2))
  }
} else if(kernel == "epanechnikov") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(2/(pi * ss.tr[i]^2) * (1 - min(1, dist.2D[i, ]^2)))
  }


num.quads <- length(seq(-1, 1, ss))
int.matrix <- matrix(int.est, num.quads, num.quads)

return(int.matrix)

}