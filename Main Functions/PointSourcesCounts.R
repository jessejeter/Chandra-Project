PointSourcesCounts <- function(X, Y, qs, threshold=-100) {

require(ineq)
require(sp)

CM <- QuadWindow(X, Y, qs)
ct.mat <- CM$count.mat

LC.CM <- Lc(as.vector(ct.mat))
n.quads <- length(as.vector(ct.mat))

cum.pct <- seq(0, 1, 1/n.quads)
pt.power <- ((1 - LC.CM$L) / (1 - LC.CM$p))[(n.quads + 1):1]
d.pt.power <- diff(pt.power) / diff(cum.pct)
threshold <- pt.power[min(which(d.pt.power > threshold))]
high.mat <- 1 * (ct.mat > threshold)
dhm <- dim(high.mat)[1]
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
