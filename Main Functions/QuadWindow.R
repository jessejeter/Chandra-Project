QuadWindow <- function(X, Y, qs) {

# Determine the partition sequence which will be used for both the x- and y-axes.
quad.seq <- seq(-1 + qs/2, 1 - qs/2, qs)

# Determine the length of this sequence
qs.len <- length(quad.seq)

X.quads <- rep(quad.seq,qs.len)
Y.quads <- as.vector(t(replicate(qs.len, quad.seq)))

# Calculate the coordinates matrix which should contain qs.len^2 rows.
Z <- cbind(X.quads, Y.quads)

# Load sp to use point.in.polygon function
require(sp)

# Obtain quadrat counts
quad.count <- rep(NA, dim(Z)[1])
for(i in 1:dim(Z)[1]) {
  quad.count[i] <- sum(point.in.polygon(X, Y, 
       c(Z[i, 1] - qs/2, Z[i, 1] + qs/2, Z[i, 1] + qs/2, Z[i, 1] - qs/2),
       c(Z[i, 2] - qs/2, Z[i, 2] - qs/2, Z[i, 2] + qs/2, Z[i, 2] + qs/2)) > 0)
}

count.mat <- matrix(quad.count, qs.len, qs.len)

final.object <- list(Z, quad.seq, count.mat)
names(final.object) <- c("Coords", "quad.seq", "count.mat")
return(final.object)

}
