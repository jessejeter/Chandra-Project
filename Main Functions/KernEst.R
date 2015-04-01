KernEst <- function(X, Y, qs, kernel = c("gaussian", "epanechnikov", 
                                          "uniform"), ...) {

# Quadratize the window with squares of length qs.
QW <- QuadWindow(X, Y, qs)
Z <- QW$Coords
quad.seq <- QW$quad.seq

int.est <- rep(NA, dim(Z)[1])

if(kernel == "gaussian") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(2 * pi * qs^2) * exp(-1/2 * ((X - Z[i, 1])^2 + 
                                                      (Y - Z[i, 2])^2)) / qs^2)
  }
} else if(kernel == "epanechnikov") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(2/(pi * qs^2) * (1 - min(1, ((X - Z[i, 1])^2 + 
                                                   (Y - Z[i, 2])^2))  / qs^2))
  }
} else if(kernel == "uniform") {
  for(i in 1:dim(Z)[1]) {
    int.est[i] <- sum(1/(pi * qs^2) * (sqrt((X - Z[i, 1])^2 + 
                                            (Y - Z[i, 2])^2) / qs < 1))
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
