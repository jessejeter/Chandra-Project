# Testing line one change.
# Testing line two change.
#f2

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

f2 <- sum21



#f3

sum3 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum3 <- sum3 + i*j*CM[i,j]
                }
              }

px <- rowMeans(CM)
py <- colMeans(CM)

f3 <- (sum3 - mean(px)*mean(py)) / (sd(px)*sd(py))
    
#f4

sum4 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum4 <- sum4 + CM[i,j]*(i-mean(CM))^2
                }
              }

f4 <- sum4

#f5

sum5 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum5 <- sum5 + CM[i,j]/(1+(i-j)^2)
                }
              }

f5 <- sum5

#f6

sum61 <- 0

for (n in 2:16){
  sum62 <- 0
  for (i in 1:8){
    if(((n-i)>0)&((n-i)<9)) {sum62 <- sum62 + CM[i,n-i]}                
                }
  sum61 <- sum61 + sum62*n 
               }
            
f6 <- sum61

#f8
# somehow I failed to figure out f8 and the log function may be the reason
# I found the reason
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

f8 <- -sum81

#f7
# since f8 failed so can not figure out f7 either

sum71 <- 0

for (n in 2:16){
  sum72 <- 0
  for (i in 1:8){
    if(((n-i)>0)&((n-i)<9)) {sum72 <- sum72 + CM[i,n-i]}                
                }
  sum71 <- sum71 + sum71*(n-f8)^2
               }

f7 <- sum71

#f9
# f9 also failed because of the log function

sum9 <- 0

for (i in 1:8){
  for (j in 1:8){
    sum9 <- sum9 + CM[i,j]*log(CM[i,j])
                }
              }

f9 <- -sum9

#f10

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

f10 <- var(pxMINUSy)

#f11

sum11 <- 0

for (n in 1:7){
  sum11 <- sum11 + pxMINUSy[n]*log(pxMINUSy[n])
              }

f11 <- -sum11

































