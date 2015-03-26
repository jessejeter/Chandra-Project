# Test the performance of the SquareRot function with respect to bivariate
# Uniform data which has been scaled, rotated and shifted. The simulated
# data will approximate random Chandra background data which is much slower
# to run.

N.rep <- 1000  # Number of simulations
N.pts <- 10000  # Number of points in each simulation
theta.act <- runif(N.rep, 0, 2*pi)  # Random angles
X.cen <- 3000  # Arbitrary X-center
Y.cen <- 4000  # Arbitrary Y-center
kSide.len <- 1000  # Arbitrary half-side length

# Simulate random background data and estimate rotation angles.
theta.pred <- rep(NA, N.rep)  
for(i in 1:N.rep) {
  R.mat <- cbind(c(cos(-theta.act[i]), sin(-theta.act[i])), 
                 c(-sin(-theta.act[i]), cos(-theta.act[i])))
  U <- cbind(runif(N.pts, -1, 1), runif(N.pts, -1, 1))
  Z <- ((U * kSide.len) %*% t(R.mat)) + t(replicate(N.pts, c(X.cen, Y.cen)))
  X <- Z[,1]
  Y <- Z[,2]
  theta.pred[i] <- SquareRot(X,Y)$"angle"
}

plot(theta.pred,theta.act)

angle.loss.fcn <- rep(2*pi,N.rep)
angle.mod <- rep(NA,N.rep)
for(i in 1:N.rep) {
  for(j in -1:3) {
    if(abs(theta.pred[i]-(theta.act[i]-j*pi/2)) < angle.loss.fcn[i]) {
      angle.loss.fcn[i] <- abs(theta.pred[i]-(theta.act[i]-j*pi/2))
      angle.mod[i] <- j
    }
  }
}

theta.act.mod <- theta.act-angle.mod*pi/2

plot(theta.pred, theta.act.mod - theta.pred)

sd(theta.act.mod - theta.pred)
