CoocMat <- function(X, Y, qs, max.count="None") {

count.mat <- QuadWindow(X, Y, qs)$"count.mat"

if(max.count == "None") {
  max.count <- max(count.mat)
}

if(max.count != "None") {
  for(i in 1:dim(count.mat)[1]) {
    for(j in 1:dim(count.mat)[1]) {
      count.mat[i, j] <- min(count.mat[i, j], max.count - 1)
    }
  } 
}

CM.hor <- matrix(0, max.count, max.count)
for(i in 1:(dim(count.mat)[1] - 1)) {
  for(j in 1:dim(count.mat)[1]) {
    CM.hor[count.mat[i, j] + 1, 
           count.mat[i + 1, j] + 1] <- CM.hor[count.mat[i, j] + 1, 
                                              count.mat[i + 1, j] + 1] + 1
  }
}

CM.ver <- matrix(0, max.count, max.count)
for(j in 1:(dim(count.mat)[1] - 1)) {
  for(i in 1:dim(count.mat)[1]) {
    CM.ver[count.mat[i, j] + 1, 
           count.mat[i, j + 1] + 1] <- CM.ver[count.mat[i, j] + 1,
                                              count.mat[i, j + 1] + 1] + 1
  }
}

CM.hor <- (CM.hor + t(CM.hor))/(2 * sum(CM.hor))

CM.ver <- (CM.ver + t(CM.ver))/(2 * sum(CM.ver))

CM.both <- 1/2 * (CM.hor + CM.ver)

final.object <- list(CM.hor, CM.ver, CM.both, count.mat)
names(final.object) <- c("hor", "ver", "both", "count.mat")
return(final.object)

}
