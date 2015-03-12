CalculateAngle <- function(X,Y,k){                          ## k is the # of points in each corner to calculate a "Average Point"  ##   
                                                            ## In this case I took k=5, but you may change k as other data  is used. ##
Center0X <- X-mean(X)
Center0Y <- Y-mean(Y)
CenterData <- cbind(Center0X,Center0Y)

SortXData <- CenterData[order(CenterData[,1]),]
SortYData <- CenterData[order(CenterData[,2]),]

a <- 0;b <- 0
for(i in 1:k){
  a <- a+SortXData[i,1]
  b <- b+SortXData[i,2]
}
TopLeft.Average <- c(a/k,b/k)  

a <- 0;b <- 0
for(i in 1:k){
  a <- a+SortXData[length(X)-i+1,1]
  b <- b+SortXData[length(X)-i+1,2]
}
BottomRight.Average <- c(a/k,b/k)  

a <- 0;b <- 0
for(i in 1:k){
  a <- a+SortYData[i,1]
  b <- b+SortYData[i,2]
}
BottomLeft.Average <-c (a/k,b/k)  

a <- 0;b <- 0
for(i in 1:k){
  a <- a+SortYData[length(X)-i+1,1]
  b <- b+SortYData[length(X)-i+1,2]
}
TopRight.Average <- c(a/k,b/k) 

slope1 <- (TopRight.Average[2]-BottomLeft.Average[2])/(TopRight.Average[1]-BottomLeft.Average[1])
slope2 <- (TopLeft.Average[2]-BottomRight.Average[2])/(TopLeft.Average[1]-BottomRight.Average[1])
Angle <- (atan(slope1)+atan(slope2))/4
Angle
}




