# ComputeStats.R

# Run SquareRot
# Run ImportChandra
# Run QuadWindow
# Run CoocMat
# Run PointSources

Chandra.info <- read.table("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/ChandraFileInfo.txt", 
				    header=TRUE, sep="\t")

num.files <- dim(Chandra.info)[1]

N.sim <- 100  # Number of background simulations for each image

HF <- matrix(NA, num.files, 11)
HF.sim <- array(NA, c(num.files, 11, N.sim))
pval <- matrix(NA, num.files, 11)
for(i in 1:num.files) {
  DF <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Chandra Data/Text Files/",
                       file.name=as.character(Chandra.info[i, 3]))
  Z <- PointSources(DF[, 3], DF[, 4], 10)
  X <- Z$final.points[, 1]
  Y <- Z$final.points[, 2]
  CM <- CoocMat(X, Y, .05, 8)$"both"
  HF[i, ] <- HarFeats(CM)

  for(j in 1:N.sim) {
    DF.sim <- rChandraBG(length(X))
    X <- DF.sim[,1]
    Y <- DF.sim[,2]
    CM.sim <- CoocMat(X,Y,.05,8)$"both"
    HF.sim[i, , j] <- HarFeats(CM.sim)
  }
  
  for(k in 1:11) {
    # Two-sided p-value
    area.below <- ((rank(c(HF[i, k], HF.sim[i, k, ])) - 1) - 0.5)/length(rrr)
    area.above <- 1 - 1/length(rrr) - area.below
    pval[i, k] <- 2 * min(area.below, area.above)
  }
}

