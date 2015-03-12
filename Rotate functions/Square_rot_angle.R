Square_rot_angle <- function(X,Y) {
                      
# First relocate centroid to (0,0).
X.c <- X - mean(X)
Y.c <- Y - mean(Y)

# Identify extreme points (highest, lowest, "rightest" and "leftest").
pt_l <- c(min(X.c),Y.c[which(X.c==min(X.c))[1]])
pt_r <- c(max(X.c),Y.c[which(X.c==max(X.c))[1]])
pt_t <- c(X.c[which(Y.c==max(Y.c))[1]],max(Y.c))
pt_b <- c(X.c[which(Y.c==min(Y.c))[1]],min(Y.c))

# Estimate slope of line connecting leftest to highest.
slope1 <- coef(lm(c(pt_l[2],pt_t[2])~c(pt_l[1],pt_t[1])))[2]

# Estimate slope of line connecting lowest to rightest.
slope2 <- coef(lm(c(pt_b[2],pt_r[2])~c(pt_b[1],pt_r[1])))[2]

# Estimate slope of line connecting highest to rightest.
slope3 <- coef(lm(c(pt_t[2],pt_r[2])~c(pt_t[1],pt_r[1])))[2]

# Estimate slope of line connecting leftest to lowest.
slope4 <- coef(lm(c(pt_l[2],pt_b[2])~c(pt_l[1],pt_b[1])))[2]

# Convert the average of these slopes into an angle.
t_hat <- atan2(mean(c(slope1,slope2,-1/slope3,-1/slope4)),1)

# Calculate rotation matrix.
R <- cbind(c(cos(-t_hat),sin(-t_hat)),c(-sin(-t_hat),cos(-t_hat)))

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

