KernEst <- function(X, Y, qs, kernel = c("gauqsian", "epanechnikov", 
                                         "uniform"), ...) {

# Quadratize the window with squares of length qs.
QW <- QuadWindow(X, Y, qs)
Z <- QW$Coords
quad.seq <- QW$quad.seq

int.est <- rep(NA, dim(Z)[1])
dist.2D <- matrix(NA, dim(Z)[1], length(X))

for(i in 1:dim(Z)[1]) {
  dist.2D[i, ] <- sqrt((X - Z[i, 1])^2 + (Y - Z[i, 2])^2)
}

dist.2D <- dist.2D / qs

if(kernel == "gauqsian") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(2 * pi * qs^2) * exp(-1/2 * dist.2D[i, ]^2))
  }
} else if(kernel == "epanechnikov") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(2/(pi * qs^2) * (1 - min(1, dist.2D[i, ]^2)))
  }
} else if(kernel == "uniform") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(pi * qs^2) * (dist.2D[i, ] < 1))
  }
}

num.quads <- length(quad.seq)
int.matrix <- matrix(int.est, num.quads, num.quads)

int.image <- image(quad.seq, quad.seq, int.matrix, xlim=c(-1,1), 
                   ylim=c(-1,1), ...)

final.object <- list(int.matrix, int.image)
names(final.object) <- c("int.matrix", "int.image")
return(final.object)

}
