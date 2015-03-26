CoocMat <- function(X, Y, qs, max.count) {

require(sp)

Z <- QuadWindow(qs)

quad.count <- rep(NA, dim(Z)[1])

for(i in 1:dim(Z)[1]) {
  quad.count[i] <- sum(point.in.polygon(X, Y, 
                    c(Z[i, 1], Z[i, 1] + qs, Z[i, 1] + qs, Z[i, 1]),
                    c(Z[i, 2], Z[i, 2], Z[i, 2] + qs, Z[i, 2] + qs)) > 0)
}

qs.len <- length(seq(-1, 1, qs))

count.mat <- matrix(quad.count, qs.len, qs.len)
for(i in 1:dim(count.mat)[1]) {
  for(j in 1:dim(count.mat)[1]) {
    count.mat[i, j] <- min(count.mat[i, j], max.count - 1)
  }
}

CM.hor <- matrix(0, max.count, max.count)
for(i in 1:(dim(count.mat)[1] - 1)) {
  for(j in 1:dim(count.mat)[1]) {
    CM.hor[count.mat[i, j] + 1, 
           count.mat[i + 1, j] + 1] <- CM.hor[count.mat[i, j] + 1, 
                                              count.mat[i + 1, j] + 1] + 1
  }
}

CM.ver <- matrix(0, max.count, max.count)
for(j in 1:(dim(count.mat)[1] - 1)) {
  for(i in 1:dim(count.mat)[1]) {
    CM.ver[count.mat[i, j] + 1, 
           count.mat[i, j + 1] + 1] <- CM.ver[count.mat[i, j] + 1,
                                              count.mat[i, j + 1] + 1] + 1
  }
}

CM.hor <- (CM.hor + t(CM.hor))/(2 * sum(CM.hor))

CM.ver <- (CM.ver + t(CM.ver))/(2 * sum(CM.ver))

CM.both <- 1/2 * (CM.hor + CM.ver)

final.object <- list(CM.hor, CM.ver, CM.both)
names(final.object) <- c("hor", "ver", "both")
return(final.object)

}

