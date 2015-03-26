library(spatialkernel)

X <- runif(10000,-1,1)
Y <- runif(10000,-1,1)

ss <- .05
Z <- QuadWindow(ss)
win <- t(cbind(c(1,1),c(-1,1),c(-1,-1),c(1,-1)))
num.quads <- length(seq(-1, 1, ss))

begin <- Sys.time()
int.est1 <- lambdahat(cbind(X,Y),ss,gpts=Z,poly=win,edge=TRUE)
Sys.time() - begin; rm(begin)

begin <- Sys.time()
int.est2 <- KernEst(X,Y,ss,kernel="gaussian")
Sys.time() - begin; rm(begin)


int.matrix1 <- matrix(int.est1$"lambda", num.quads, num.quads)
int.matrix2 <- int.est2$"int.matrix"

par(mfrow=c(1,2))
image(seq(-1, 1, ss), seq(-1, 1, ss), int.matrix1, xlim=c(-1,1), ylim=c(-1,1))
image(seq(-1, 1, ss), seq(-1, 1, ss), int.matrix2, xlim=c(-1,1), ylim=c(-1,1))

resid.mat <- int.matrix1 - int.matrix2
image(seq(-1, 1, ss), seq(-1, 1, ss), resid.mat, xlim=c(-1,1), ylim=c(-1,1))
