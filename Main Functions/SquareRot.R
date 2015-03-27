SquareRot <- function(X,Y) {

# Load spatstat package
require(spatstat)

# Relocate centroid to (0,0).
X.c <- X - mean(X)
Y.c <- Y - mean(Y)

# Create function which calculates area of of the minimal rectangle enclosing
# the points rotated by angle theta.
bb.area <- function(theta) {

  # Calculate rotation matrix.
  R.mat <- cbind(c(cos(theta), sin(theta)), c(-sin(theta), cos(theta)))

  # Calculate the (X,Y) coordinates rotated by theta.
  X.new <- cbind(X.c, Y.c) %*% t(R.mat)[, 1]
  Y.new <- cbind(X.c, Y.c) %*% t(R.mat)[, 2]

  # Return the area of the rectangle
  return(diff(bounding.box.xy(X.new, Y.new)$xrange) * 
         diff(bounding.box.xy(X.new, Y.new)$yrange))

}

# Use Brent's Method to calculate the rotation angle minimizing the area of
# the bounding box area.
kTheta.hat <- optim(pi/4, bb.area, method = "Brent", lower = 0, 
                    upper = pi/2)$par

# Calculate rotation matrix.
R.mat <- cbind(c(cos(kTheta.hat), sin(kTheta.hat)), 
           c(-sin(kTheta.hat), cos(kTheta.hat)))

# Calculate the (X,Y) coordinates rotated by kTheta.hat.
X.new <- cbind(X.c, Y.c) %*% t(R.mat)[, 1]
Y.new <- cbind(X.c, Y.c) %*% t(R.mat)[, 2]

# Subtract off midrange so that the minimum equals the negative maximum for
# each coordinate.
X.new <- X.new - mean(range(X.new))
Y.new <- Y.new - mean(range(Y.new))

# Calculate the half-length of the largest side of the minimal bounding 
# rectangle. Multiply be rescaling factor so that no points lie on boundary.
kSide.len <- max(c(X.new, Y.new)) * (1+log(2)/length(X))

# Rescale (X,Y) points so that they lie in [-1,1]x[-1,1].
X.new <- X.new / kSide.len
Y.new <- Y.new / kSide.len

# Return the rotated points and the angle by which the original points
# were rotated.
final.list <- list(cbind(X.new, Y.new), kTheta.hat)
names(final.list) <- c("points", "angle")
return(final.list)

}
