QuadWindow <- function(quadrat.length) {

# Determine the partition sequence which will be used for both the x- and y-axes.
quad.seq <- seq(-1, 1, quadrat.length)

# Determine the length of this sequence
qs.len <- length(quad.seq)

X.quads <- rep(quad.seq,qs.len)
Y.quads <- as.vector(t(replicate(qs.len, quad.seq)))

# Calculate the coordinates matrix which should contain qs.len^2 rows.
Coords <- cbind(X.quads, Y.quads)

return(Coords)

}