PointSources <- function(X, Y, k, threshold="None") {

inv.dist <- rep(NA, length(X))
for(i in 1:length(X)) {
  inv.dist[i] <- 1/sort(sqrt((X[i] - X[-i])^2 + (Y[i] - Y[-i])^2))[k]
}

high.id <- which(log(diff(quantile(inv.dist, 0:10000/10000))) > 
                 log(diff(quantile(inv.dist, 0:10000/10000)))[1])

if(threshold == "None") {
  threshold <- quantile(inv.dist, min(high.id[high.id > 10000/2])/10000)
}

X.high <- X[inv.dist > threshold]
Y.high <- Y[inv.dist > threshold]

Eu.dist <- matrix(NA, length(X.high), length(X.high))
nearby <- matrix(NA, length(X.high), length(X.high))
for(i in 1:length(X.high)) {
  for(j in 1:length(X.high)) {
    Eu.dist[i, j] <- sqrt((X.high[i] - X.high[j])^2 + 
                          (Y.high[i] - Y.high[j])^2)
    nearby[i, j] <- ifelse(Eu.dist[i, j] < 0.015, 1, 0)
  }
}

new.id <- 1:length(X.high)
id.list <- replicate(length(X.high), list)
for(i in new.id) {
  id.list[i] <- list(new.id[nearby[i, new.id] == 1])
}

group.id <- new.id
for(i in 2:length(X.high)) {
  j <- 1
  if(length(intersect(id.list[[j]], id.list[[i]])) > 0) {
      group.id[i] <- group.id[j]
  }
  while(j < group.id[i] & group.id[i] != group.id[j]) {
    j <- j + 1
    if(length(intersect(id.list[[j]], id.list[[i]])) > 0) {
      group.id[i] <- group.id[j]
    }
  }
}

for(i in 2:length(X.high)) {
  if(is.element(group.id[i],group.id[1:(i - 1)])) {
    group.id[i] <- group.id[i]
  } else {
    group.id[which(group.id==group.id[i])] <- rep(1 + max(group.id[1:(i - 1)]), length(which(group.id == group.id[i])))
  }
}

hull.ids <- replicate(length(unique(group.id)), list)
num.hull.ids <- rep(NA, length(unique(group.id)))
hull.poly <- replicate(length(unique(group.id)), list)
PS.centroids <- matrix(NA, length(unique(group.id)), 2)
hull.area <- rep(0, length(unique(group.id)))
hull.prob <- rep(0, length(unique(group.id)))
for(i in 1:length(hull.ids)) {
  X.cur <- X.high[which(group.id == i)]
  Y.cur <- Y.high[which(group.id == i)]
  hull.ids[[i]] <- chull(X.cur, Y.cur)
  num.hull.ids[i] <- length(hull.ids[[i]])
  hull.poly[[i]] <- cbind(X.cur[hull.ids[[i]]], Y.cur[hull.ids[[i]]])
  PS.centroids[i, ] <- c(mean(hull.poly[[i]][1]), mean(hull.poly[[i]][2]))
  if(num.hull.ids[i] > 2) {
    hull.area[i] <- sum(hull.poly[[i]][1:(num.hull.ids[i] - 1), 1] *
                        hull.poly[[i]][2:num.hull.ids[i], 2]) + 
                    hull.poly[[i]][num.hull.ids[i], 1] * hull.poly[[i]][1, 2] -
                    sum(hull.poly[[i]][2:num.hull.ids[i], 1] *
                        hull.poly[[i]][1:(num.hull.ids[i] - 1), 2]) - 
                    hull.poly[[i]][1, 1] * hull.poly[[i]][num.hull.ids[i], 2]
    hull.area[i] <- abs(hull.area[i])/2
    hull.prob[i] <- dChandraBG(PS.centroids[i, 1]) * 
                    dChandraBG(PS.centroids[i, 2]) * hull.area[i]
  }
}

rm.id <- NULL
for(i in which(num.hull.ids>2)) {
    rm.id <- c(rm.id, which(point.in.polygon(X, Y, hull.poly[[i]][, 1], 
                                             hull.poly[[i]][, 2]) > 0))
}
rm.id <- unique(rm.id)

X.trunc <- X[-rm.id]
Y.trunc <- Y[-rm.id]

num.new <- length(X.trunc) / (1 - sum(hull.prob)) - length(X.trunc)
num.new.each <- round(num.new * hull.prob / sum(hull.prob), 0)

require(sp)

X.replace <- NULL
Y.replace <- NULL
for(i in which(num.hull.ids>2)) {
  j <- 1
  while(j <= num.new.each[i]) {
    new.pt <- c(runif(1, min(hull.poly[[i]][, 1]), max(hull.poly[[i]][, 1])),
                runif(1, min(hull.poly[[i]][, 2]), max(hull.poly[[i]][, 2])))
    if(point.in.polygon(new.pt[1], new.pt[2], hull.poly[[i]][, 1], 
                        hull.poly[[i]][, 2]) > 0) {
      X.replace <- c(X.replace, new.pt[1])
      Y.replace <- c(Y.replace, new.pt[2])
      j <- j + 1
    }
  }
}

X.final <- c(X.trunc, X.replace)
Y.final <- c(Y.trunc, Y.replace)

final.object <- list(cbind(X.final, Y.final), cbind(X.trunc, Y.trunc),
                     cbind(X.replace, Y.replace), hull.poly, PS.centroids,
                     hull.area, hull.prob)
names(final.object) <- c('final.points', 'truncated.points',
                         'replacement.points', 'hull.poly', 'PS.centroids',
                         'hull.area', 'hull.prob')
return(final.object)

}
