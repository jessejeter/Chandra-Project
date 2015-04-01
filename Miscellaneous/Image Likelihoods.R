# Image Likelihoods.R

Chandra.info <- read.table("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/ChandraFileInfo.txt", header=TRUE, sep="\t")

num.imgs <- dim(Chandra.info)[1]

mu.CBG <- mean(rowSums(log(dChandraBG(rChandraBG(1000000)))))

tscore <- rep(NA, num.imgs)
pval <- rep(NA, num.imgs)
mu.hat <- rep(NA, num.imgs)
sig.hat <- rep(NA, num.imgs)
Z <- rep(NA, num.imgs)
pval <- rep(NA, num.imgs)

for(i in 1:num.imgs) {
  DF <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/Text Files/", file.name=paste(Chandra.info[i, 3]))
  DF.new <- PointSources(DF[, 3], DF[, 4], 10)
  X <- DF.new$final.points[, 1]
  Y <- DF.new$final.points[, 2]

  mu.hat[i] <- mean(rowSums(log(dChandraBG(cbind(X, Y)))))
  sig.hat[i] <- sd(rowSums(log(dChandraBG(cbind(X, Y)))))
  Z[i] <- (mu.hat[i] - mu.CBG) / (sig.hat[i] / sqrt(length(X)))
  pval[i] <- 2 * min(pnorm(Z[i]), 1 - pnorm(Z[i]))
}

plot(which(Chandra.info[,4]==0),Zscore[which(Chandra.info[,4]==0)],col='red',ylim=c(-20,10))
points(which(Chandra.info[,4]==1),Zscore[which(Chandra.info[,4]==1)],col='blue')
