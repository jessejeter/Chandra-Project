ImageSection <- function(X, Y) {

sec1.id <- which(X >= 0 & Y >= 0)
X.sec1 <- 2 * (X[sec1.id] - 1/2)
Y.sec1 <- 2 * (Y[sec1.id] - 1/2)

sec2.id <- which(X < 0 & Y >= 0)
X.sec2 <- 2 * (X[sec2.id] + 1/2)
Y.sec2 <- 2 * (Y[sec2.id] - 1/2)

sec3.id <- which(X < 0 & Y < 0)
X.sec3 <- 2 * (X[sec3.id] + 1/2)
Y.sec3 <- 2 * (Y[sec3.id] + 1/2)

sec4.id <- which(X >= 0 & Y < 0)
X.sec4 <- 2 * (X[sec4.id] - 1/2)
Y.sec4 <- 2 * (Y[sec4.id] + 1/2)

final.object <- list(cbind(X.sec1, Y.sec1), cbind(X.sec2, Y.sec2), 
  cbind(X.sec3, Y.sec3), cbind(X.sec4, Y.sec4))
names(final.object) <- c('sec1', 'sec2', 'sec3', 'sec4')
return(final.object)

}
