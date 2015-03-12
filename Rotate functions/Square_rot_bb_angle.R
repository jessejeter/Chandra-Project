Square_rot_bb_angle <- function(X,Y) {

# First relocate centroid to (0,0).
X.c <- X - mean(X)
Y.c <- Y - mean(Y)

bb_area <- function(theta) {

# Calculate rotation matrix.
R <- cbind(c(cos(theta),sin(theta)),c(-sin(theta),cos(theta)))

# Calculate the(X,Y) coordinates rotated by -t_hat.
X.new <- cbind(X.c,Y.c)%*%t(R)[,1]
Y.new <- cbind(X.c,Y.c)%*%t(R)[,2]

return(diff(bounding.box.xy(X.new,Y.new)$yrange)*diff(bounding.box.xy(X.new,Y.new)$yrange))

}

t_hat <- pi/2-optim(pi/4, bb_area, method = "Brent", lower = 0, upper = pi/2)$par

# Calculate rotation matrix.
R <- cbind(c(cos(t_hat),sin(t_hat)),c(-sin(t_hat),cos(t_hat)))

# Calculate the(X,Y) coordinates rotated by -t_hat.
X.new <- cbind(X.c,Y.c)%*%t(R)[,1]
Y.new <- cbind(X.c,Y.c)%*%t(R)[,2]

# Subtract off midrange so that points lie in a square.
X.new <- X.new - mean(range(X.new))
Y.new <- Y.new - mean(range(Y.new))

# Calculate the half-diameter of the square
s <- max(c(X.new,Y.new))

# Rescale (X,Y) coordinates so that they lie in [-1,1]x[-1,1]
X.new <- X.new/s
Y.new <- Y.new/s

return(t_hat)

}
