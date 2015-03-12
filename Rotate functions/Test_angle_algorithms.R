#test angle algorithms

N.rep <- 1000
theta.act <- runif(N.rep,0,pi/2)
theta.pred1 <- rep(NA,N.rep)
theta.pred2 <- rep(NA,N.rep)
theta.pred3 <- rep(NA,N.rep)
theta.pred4 <- rep(NA,N.rep)
theta.pred5 <- rep(NA,N.rep)
theta.pred6 <- rep(NA,N.rep)

for(i in 1:N.rep) {
  Z <- Back_sim_fcn(10000,center=c(3000,4000),scale=1000,theta=theta.act[i])
  X <- Z[,1]
  Y <- Z[,2]
  theta.pred1[i] <- Square_rot_bb_angle(X,Y)
  theta.pred2[i] <- Square_rot_angle(X,Y)
  theta.pred3[i] <- CalculateAngle(X,Y,3)
  theta.pred4[i] <- CalculateAngle(X,Y,5)
  theta.pred5[i] <- CalculateAngle(X,Y,7)
  theta.pred6[i] <- CalculateAngle(X,Y,9)
}

buff_id <- which(theta.act>.02 & theta.act<pi/2-.02)

plot(theta.act[buff_id],theta.act[buff_id]-theta.pred1[buff_id])
plot(theta.act[buff_id],theta.act[buff_id]-theta.pred2[buff_id])

sd(theta.pred1[buff_id]-theta.act[buff_id])
sd(theta.pred2[buff_id]-theta.act[buff_id])
sd(theta.pred3[buff_id]-theta.act[buff_id])
sd(theta.pred4[buff_id]-theta.act[buff_id])
sd(theta.pred5[buff_id]-theta.act[buff_id])
sd(theta.pred6[buff_id]-theta.act[buff_id])
