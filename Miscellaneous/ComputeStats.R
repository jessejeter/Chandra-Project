# ComputeStats.R

# Run SquareRot
# Run ImportChandra
# Run QuadWindow
# Run CoocMat
# Run PointSources

Chandra.info <- read.table("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/ChandraFileInfo.txt", 
				    header=TRUE, sep="\t")

num.files <- dim(Chandra.info)[1]
num.feats <- 13

HF <- matrix(NA, num.files, num.feats)
qs <- .02
max.count <- 8

thin <- 9000

for(i in 1:num.files) {
  DF <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/Text Files/",
                       file.name=as.character(Chandra.info[i, 3]))
  thin.id <- sample(length(DF[, 3]), thin)
  Z <- PointSourcesCounts2(DF[thin.id, 3], DF[thin.id, 4], qs)
  X <- Z$final.points[, 1]
  Y <- Z$final.points[, 2]
  CM <- CoocMat(X, Y, qs, max.count)$"both"
  HF[i, ] <- HarFeats(CM)
}

N.sim <- 1000
HF.sim <- matrix(NA, N.sim, num.feats)
for(j in 1:N.sim) {
  DF.sim <- rChandraBG(thin)
  # No need to run PointSourcesCounts2 on simulated data
  X.sim <- DF.sim[, 1]
  Y.sim <- DF.sim[, 2]
  CM.sim <- CoocMat(X.sim, Y.sim, qs, max.count)$"both"
  HF.sim[j, ] <- HarFeats(CM.sim)
}


pval <- matrix(NA, num.files, num.feats)
for(i in 1:num.files) {
  for(k in 1:num.feats) {
    # Two-sided p-value
    area.below <- sum(HF[i, k] >= HF.sim[, k]) / N.sim
    area.above <- sum(HF[i, k] <= HF.sim[, k]) / N.sim
    pval[i, k] <- 2 * min(area.below, area.above)
  }
}

plot(apply(pval, 1, mean), ylim=c(0, 1))
plot(apply(pval, 1, max), ylim=c(0, 1))
plot(apply(pval, 1, median), ylim=c(0, 1))
plot(apply(pval, 1, min), ylim=c(0, 1))

file.id.mat <- replicate(num.feats, 1:num.files)
feat.id.mat <- t(replicate(num.files, 1:num.feats))
plot(file.id.mat, pval, col=(file.id.mat %% 4) + 1, pch=paste(feat.id.mat))

feat <- 13
plot(pval[, feat], ylim=c(0, 1))




