# Background Image Intensity Estimation

library(MASS)
library(spatstat)
library(sp)
library(pracma)

DF <- read.table("F:/!spring 2015/Spatial Astro/Reference and Test Images/acisf03482N004_evt2_05_8keV_use_as_background_model_t1.txt",header=FALSE,quote="\"",sep=",")

# Run Square_rot_bb function
X <- Square_rot_bb(DF[,11],DF[,12])[,1]
Y <- Square_rot_bb(DF[,11],DF[,12])[,2]
plot(X,Y,pch='.')

hist_X <- hist(X,breaks=1000)
hist_Y <- hist(Y,breaks=1000)

int_range <- c(-1,-.96,-.04,0,.04,.96,1)
int_est <- c(0,.44,.56,.20,.56,.44,0)

par(mfrow=c(1,2))
plot(hist_X$"mids",hist_X$"density",ylim=c(0,2.5))
lines(int_range,int_est,col='red')

plot(hist_Y$"mids",hist_Y$"density",ylim=c(0,2.5))
lines(int_range,int_est,col='red')

area_int <- sum(1/2*(int_est[1:6]+int_est[2:7])*diff(int_range))

int_est_norm <- int_est/area_int

int_sum <- c(0,cumsum(1/2*(int_est_norm[1:6]+int_est_norm[2:7])*diff(int_range)))

# See rChandraBG for simulations from this estimated distribution.



