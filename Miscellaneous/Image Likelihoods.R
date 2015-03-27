DF <- read.table("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/ChandraFileInfo.txt", header=TRUE, sep="\t")

num.imgs <- dim(DF)[1]

tscore <- rep(NA, num.imgs)
pval <- rep(NA, num.imgs)
for(i in 1:38) {
  Z <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/Text Files/", file.name=paste(DF[i, 3]))
  ZBG <- rChandraBG(length(Z[, 3]))

  y1 <- log(dChandraBG(Z[, 3])) + log(dChandraBG(Z[, 4]))
  y2 <- log(dChandraBG(ZBG[, 1])) + log(dChandraBG(ZBG[, 2]))
  tscore[i] <- t.test(y1, y2)$"statistic"
  pval[i] <- t.test(y1, y2)$"p.value"
}

plot(which(DF[,4]==0),Zscore[which(DF[,4]==0)],col='red',ylim=c(-20,10))
points(which(DF[,4]==1),Zscore[which(DF[,4]==1)],col='blue')
