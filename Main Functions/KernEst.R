KernEst <- function(X, Y, ss, kernel = c("gaussian", "epanechnikov", 
                                         "uniform"), ...) {

# Quadratize the window with squares of length ss.
Z <- QuadWindow(ss)

int.est <- rep(NA, dim(Z)[1])
dist.2D <- matrix(NA, dim(Z)[1], length(X))

for(i in 1:dim(Z)[1]) {
  dist.2D[i, ] <- sqrt((X - Z[i, 1])^2 + (Y - Z[i, 2])^2)
}

dist.2D <- dist.2D / ss

if(kernel == "gaussian") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(2 * pi * ss^2) * exp(-1/2 * dist.2D[i, ]^2))
  }
} else if(kernel == "epanechnikov") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(2/(pi * ss^2) * (1 - min(1, dist.2D[i, ]^2)))
  }
} else if(kernel == "uniform") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(pi * ss^2) * (dist.2D[i, ] < 1))
  }
}

num.quads <- length(seq(-1, 1, ss))
int.matrix <- matrix(int.est, num.quads, num.quads)

int.image <- image(seq(-1, 1, ss), seq(-1, 1, ss), int.matrix, xlim=c(-1,1), 
                   ylim=c(-1,1), ...)

final.object <- list(int.matrix, int.image)
names(final.object) <- c("int.matrix", "int.image")
return(final.object)

}
