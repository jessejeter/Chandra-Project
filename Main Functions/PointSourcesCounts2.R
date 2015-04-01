PointSourcesCounts2 <- function(X, Y, qs, threshold=.1) {

require(sp)

CM <- QuadWindow(X, Y, qs)
ct.mat <- CM$count.mat
dhm <- dim(ct.mat)[1]
shift.mat1 <- rbind(ct.mat[2:dhm, ], rep(0, dhm))
shift.mat2 <- rbind(rep(0, dhm), ct.mat[1:(dhm - 1), ])
shift.mat3 <- cbind(ct.mat[, 2:dhm], rep(0, dhm))
shift.mat4 <- cbind(rep(0, dhm), ct.mat[, 1:(dhm - 1)])

ratio.mat1 <- 1 * ((ct.mat + 1) / (shift.mat1 + 1) < threshold)
ratio.mat2 <- 1 * ((ct.mat + 1) / (shift.mat2 + 1) < threshold)
ratio.mat3 <- 1 * ((ct.mat + 1) / (shift.mat3 + 1) < threshold)
ratio.mat4 <- 1 * ((ct.mat + 1) / (shift.mat4 + 1) < threshold)

high.mat <- 1 * (ratio.mat1 + ratio.mat2 + ratio.mat3 + ratio.mat4 > 0)
nb.high.mat <- high.mat + rbind(high.mat[2:dhm, ], rep(0, dhm)) +
                          rbind(rep(0, dhm), high.mat[1:(dhm - 1), ]) + 
                          cbind(high.mat[, 2:dhm], rep(0, dhm)) + 
                          cbind(rep(0, dhm), high.mat[, 1:(dhm - 1)])

if(sum(high.mat) == 0) {
  X.trunc <- X
  Y.trunc <- Y
  X.replace <- NULL
  Y.replace <- NULL
  X.final <- X
  Y.final <- Y
  quad.poly <- NULL
  high.quad <- NULL
  quad.prob <- NULL
} else {
  high.id <- which(nb.high.mat > 0)
  num.high.id <- length(high.id)
  high.quad <- CM$Coords[high.id, ]
  quad.poly <- cbind((high.quad - qs/2)[, 1], (high.quad + qs/2)[, 1],
                      (high.quad + qs/2)[, 1], (high.quad - qs/2)[, 1],
                      (high.quad - qs/2)[, 2], (high.quad - qs/2)[, 2],
                      (high.quad + qs/2)[, 2], (high.quad + qs/2)[, 2])
  quad.prob <- dChandraBG(high.quad[, 1]) * dChandraBG(high.quad[, 2]) * qs^2

  rm.id <- NULL
  for(i in 1:num.high.id) {
    rm.id <- c(rm.id, which(point.in.polygon(X, Y, quad.poly[i, 1:4], 
                                             quad.poly[i, 5:8]) > 0))
  }

  X.trunc <- X[-rm.id]
  Y.trunc <- Y[-rm.id]

  num.new <- length(X.trunc) / (1 - sum(quad.prob)) - length(X.trunc)

  replace.id <- rmultinom(1, num.new, quad.prob)

  X.replace <- runif(num.high.id, -qs/2, qs/2) + high.quad[, 1]
  Y.replace <- runif(num.high.id, -qs/2, qs/2) + high.quad[, 2]

  X.final <- c(X.trunc, X.replace)
  Y.final <- c(Y.trunc, Y.replace)
}

final.object <- list(cbind(X.final, Y.final), cbind(X.trunc, Y.trunc),
                     cbind(X.replace, Y.replace), quad.poly, high.quad,
                     quad.prob, nb.high.mat)
names(final.object) <- c('final.points', 'truncated.points',
                         'replacement.points', 'quad.poly', 'high.quad',
                         'quad.prob', 'high.counts')
return(final.object)

}
