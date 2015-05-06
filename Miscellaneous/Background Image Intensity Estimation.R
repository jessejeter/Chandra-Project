# Background Image Intensity Estimation

# Read all functions - change file.path
sapply(list.files(paste(file.path, "/GitHub/Chandra-Project/Chandra R Functions", sep=""), full.names=TRUE), source)

DF <- ImportChandra("C:/Users/jeterjp/Documents/GitHub/Chandra-Project/Data/Reference Background/","acisf03482N004_evt2_05_8keV_use_as_background_model_t1.txt")

# Run Square_rot_bb function
X <- DF[, 3]
Y <- DF[, 4]
plot(X, Y, pch='.')

hist.X <- hist(X, breaks=1000)
hist.Y <- hist(Y, breaks=1000)

int.range <- c(-1, -.96, -.04, 0, .04, .96, 1)
int.est <- c(0, .44, .56, .20, .56, .44, 0)

par(mfrow=c(1, 2))
plot(hist.X$"mids", hist.X$"density", ylim=c(0, 2.5))
lines(int.range, int.est, col='red')

plot(hist.Y$"mids", hist.Y$"density", ylim=c(0,2.5))
lines(int.range, int.est, col='red')

area.int <- sum(1/2 * (int.est[1:6] + int.est[2:7]) * diff(int.range))

int.est.norm <- int.est/area.int

int.sum <- c(0, cumsum(1/2 * (int.est.norm[1:6] + int.est.norm[2:7]) * diff(int.range)))

# For uses of this code, see [d,p,q,r]ChandraBG.R


