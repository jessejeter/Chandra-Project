# ComputeSectionStats.R

# Run SquareRot
# Run ImportChandra
# Run QuadWindow
# Run CoocMat
# Run PointSources

Chandra.info <- read.table("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/ChandraFileInfo.txt", 
				    header=TRUE, sep="\t")

num.files <- 4 * dim(Chandra.info)[1]
num.feats <- 13

HF <- matrix(NA, num.files, num.feats)
qs <- .08
max.count <- 8

thin <- 2000

for(i in 1:num.files) {
  DF <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/Text Files/",
                       file.name=as.character(Chandra.info[i, 3]))
  DF.sec <- ImageSection(DF[, 3], DF[, 4])

  for(j in 1:4) {
    X.orig <- DF.sec[[j]][, 1]
    Y.orig <- DF.sec[[j]][, 2]
    thin.id <- sample(length(X.orig), thin)
    Z <- PointSourcesCounts2(X.orig[thin.id], Y.orig[thin.id], qs)
    X <- Z$final.points[, 1]
    Y <- Z$final.points[, 2]
    CM <- CoocMat(X, Y, qs, max.count)$"both"
    k <- 4 * (i - 1) + j
    HF[k, ] <- HarFeats(CM)
  }
}

# plot statistics by feature number
plot(HF[, 1])

pval <- matrix(NA, num.files, num.feats)
for(i in 1:num.feats) {
  # Two-sided p-value
  area.below <- (rank(HF[, i]) - 1/2) /  num.files
  area.above <- 1 - area.below
  pval[, i] <- 2 * apply(cbind(area.below, area.above), 1, min)
}

# plot p-values by file number
plot(pval[2, ], ylim = c(0, 1))

# pval <- pval[, -7]  # Remove the possibly meaningless 7th feature.
plot(apply(pval, 1, mean), ylim=c(0, 1))
plot(apply(pval, 1, max), ylim=c(0, 1))
plot(apply(pval, 1, median), ylim=c(0, 1))
plot(apply(pval, 1, min), ylim=c(0, 1))


file.id.mat <- replicate(num.feats, 1:num.files)
feat.id.mat <- t(replicate(num.files, 1:num.feats))
plot(file.id.mat, pval, col=(file.id.mat %% 2) + 1, pch=paste(feat.id.mat), 
     cex=.5)

