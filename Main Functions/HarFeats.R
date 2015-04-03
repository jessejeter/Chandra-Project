HarFeats <- function(p) {

f <- rep(NA, 13)  # Preallocate vector of features
N.g <- dim(p)[1]  # Specify the maximum count
p.x <- rowSums(p)  # Row marginal
p.y <- colSums(p)  # Column marginal
mu.x <- sum(0:(N.g - 1) * p.x)
mu.y <- sum(0:(N.g - 1) * p.y)
sig.x <- sqrt(sum((0:(N.g - 1) - mu.x)^2 * p.x))
sig.y <- sqrt(sum((0:(N.g - 1) - mu.y)^2 * p.y))
x.num.mat <- replicate(N.g, 0:(N.g - 1))
y.num.mat <- t(replicate(N.g, 0:(N.g - 1)))

# Convolution probabilities
p.xy.add <- c(sapply(2:(N.g + 1), function(k) sum(diag(as.matrix(
                     p[1:(k - 1), (k - 1):1])))),
              sapply(2:N.g, function(k) sum(diag(as.matrix(
                     p[k:N.g, N.g:k])))))

# Cross-correlation probabilities
p.xy.diff <- sapply(0:(N.g - 1), function(k) ifelse(k == 0, 1, 2) * 
               sum(diag(as.matrix(p[1:(N.g - k), (k + 1):N.g]))))

# Entropy component function
Ent.comp <- function(x) {
  y <- ifelse(is.na(x*log(x)),0,x*log(x))
  return(y)
}

## Feature 1: Angular Second Moment

f[1] <- sum(p^2)

####


## Feature 2: Contrast

f[2] <- sum((0:(N.g - 1))^2 * p.xy.diff)

####


## Feature 3: Correlation

f[3] <- (sum(x.num.mat * y.num.mat * p) - mu.x * mu.y) / 
                    (sig.x * sig.y)

####


## Feature 4: Sum of Squares

f[4] <- sum(((x.num.mat - mu.x)^2 + (y.num.mat - mu.y)^2) * p)

####


## Feature 5: Inverse Difference Moment

f[5] <- sum(1 / (1 + (x.num.mat - y.num.mat)^2) * p)

####


## Feature 6: Sum Average

f[6] <- sum(0:(2 * (N.g - 1)) * p.xy.add)

####


## Feature 7: Sum Variance

f[7] <- sum((0:(2 * (N.g - 1)) - f[6])^2 * p.xy.add)

####


## Feature 8: Sum Entropy

f[8] <- -sum(Ent.comp(p.xy.add))

####


## Feature 9: Entropy

f[9] <- -sum(Ent.comp(p))

####


## Feature 10: Difference Variance
ADM <- sum(0:(N.g - 1) * p.xy.diff)  # Absolute difference mean

f[10] <- sum((0:(N.g - 1) - ADM)^2 * p.xy.diff)

## Feature 11: Difference Entropy

f[11] <- -sum(Ent.comp(p.xy.diff))

####


## Entropy numbers
p.indep <- replicate(N.g, p.x) * t(replicate(N.g, p.y))

HX <- -sum(Ent.comp(p.x))
HY <- -sum(Ent.comp(p.y))
HXY <- f[9]
HXY1 <- -sum(Ent.comp(p.indep) * ifelse(is.na(p/p.indep), 0, p/p.indep))
HXY2 <- -sum(Ent.comp(p.indep))

####


## Feature 12: Information Measure of Correlation 1

f[12] <- (HXY - HXY1) / max(HX, HY)

####


## Feature 13: Information Measure of Correlation 2

f[13] <- sqrt(1 - exp(-2 * (HXY2 - HXY)))

####


## Feature 14: Maximal Correlation Coefficient
# This is not included because the formula proposed by Haralick looks
# suspicious and there were several other more obvious typos.
#Q <- matrix(NA, N.g, N.g)
#for(i in 1:N.g) {
#  for(j in 1:N.g) {
#    Q[i, j] <- sum(sapply(1:N.g, function(k) (p[i, k] * p[j, k]) / 
#                                             (p.x[i] * p.y[k])))
#  }
#}
#
#f[14] <- sqrt(eigen(Q)$values[2])
#
####

return(f)

}
