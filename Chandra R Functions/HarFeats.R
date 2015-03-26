HarFeats <- function(CM, feat.nums = 1:11) {

# Preallocate vector of features
textural.feat <- rep(NA, 11)


## Feature 1: Angular Second Moment
textural.feat[1] <- sum(CM^2)
####


## Feature 2: Contrast
sum21 <- 0

for (n in 1:7){
  sum22 <- 0
  for (i in 1:8){
    if(((i-n)>0) & ((i+n)<9)) {sum22 <- sum22 + CM[i,i-n] + CM[i,i+n]}
    if(((i-n)>0) & ((i+n)>=9)) {sum22 <- sum22 + CM[i,i-n]}
    if(((i-n)<=0) & ((i+n)<9)) {sum22 <- sum22 + CM[i,i+n]}
                }
  sum21 <- sum21 + sum22*n^2
              }

textural.feat[2] <- sum21
####


## Feature 3: Correlation
sum3 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum3 <- sum3 + i*j*CM[i,j]
                }
              }

px <- rowMeans(CM)
py <- colMeans(CM)

textural.feat[3] <- (sum3 - mean(px)*mean(py)) / (sd(px)*sd(py))
####


## Feature 4: Sum of Squares
sum4 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum4 <- sum4 + CM[i,j]*(i-mean(CM))^2
                }
              }

textural.feat[4] <- sum4
####


## Feature 5: Inverse Difference Moment
sum5 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum5 <- sum5 + CM[i,j]/(1+(i-j)^2)
                }
              }

textural.feat[5] <- sum5
####


## Feature 6: Sum Average
sum61 <- 0

for (n in 2:16){
  sum62 <- 0
  for (i in 1:8){
    if(((n-i)>0)&((n-i)<9)) {sum62 <- sum62 + CM[i,n-i]}                
                }
  sum61 <- sum61 + sum62*n 
               }
            
textural.feat[6] <- sum61
####

## Feature 8: Sum Entropy
# somehow I failed to figure out textural.feat[8] and the log function 
# may be the reason. I found the reason
# the three numbers in the bottom right corner of Matirx CM are all 0
# so the log(0) is minus infinite

sum81 <- 0

for (n in 2:16){
  sum82 <- 0
  for (i in 1:8){
    if(((n-i)>0)&((n-i)<9)) {sum82 <- sum82 + CM[i,n-i]}                
                }
  sum81 <- sum81 + sum82*log(sum82)
               }

textural.feat[8] <- -sum81
####


## Feature 7: Sum Variance
# since f8 failed so can not figure out f7 either
sum71 <- 0

for (n in 2:16){
  sum72 <- 0
  for (i in 1:8){
    if(((n-i)>0)&((n-i)<9)) {sum72 <- sum72 + CM[i,n-i]}                
                }
  sum71 <- sum71 + sum71*(n-textural.feat[8])^2
               }

textural.feat[7] <- sum71
####


## Feature 9: Entropy
# f9 also failed because of the log function

sum9 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum9 <- sum9 + CM[i,j]*log(CM[i,j])
                }
              }

textural.feat[9] <- -sum9
####


## Feature 10: Difference Variance
pxMINUSy <- c()

for (n in 1:7){
  sum10 <- 0
  for (i in 1:8){
    if(((i-n)>0) & ((i+n)<9)) {sum10 <- sum10 + CM[i,i-n] + CM[i,i+n]}
    if(((i-n)>0) & ((i+n)>=9)) {sum10 <- sum10 + CM[i,i-n]}
    if(((i-n)<=0) & ((i+n)<9)) {sum10 <- sum10 + CM[i,i+n]}
                }
  pxMINUSy[n] <- sum10
              }

textural.feat[10] <- var(pxMINUSy)


## Feature 11: Difference Entropy
sum11 <- 0

for (n in 1:7){
  sum11 <- sum11 + pxMINUSy[n]*log(pxMINUSy[n])
              }

textural.feat[11] <- -sum11
####

return(textural.feat[feat.nums])

}
