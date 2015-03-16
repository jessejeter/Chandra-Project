Background_simulation <- function(N.sim) {

#N.sim <- 20000

X.s <- 2*rbeta(N.sim,1.1,1.1)-1
Y.s <- 2*rbeta(N.sim,1.3,1.3)-1

Num_par <- rpois(1,8)
Par.X.s <- 2*runif(Num_par)-1
Par.Y.s <- 2*runif(Num_par)-1
Par.count <- rnegbin(Num_par,9,1)+1
X.s <- c(X.s,rnorm(sum(Par.count),rep(Par.X.s,Par.count),.01))
Y.s <- c(Y.s,rnorm(sum(Par.count),rep(Par.Y.s,Par.count),.01))

#plot(X.s,Y.s,pch='.')

guard <- .04

Miss.ID <- rep(0,length(X.s))
for(i in 1:length(X.s)) {
  if( abs(X.s[i])>1 | abs(Y.s[i]>1)) {
  Miss.ID[i] <- 1
  } else if( runif(1) > (1-((X.s[i]+1<=guard)*(1-(X.s[i]+1)/guard)+(abs(X.s[i])<=guard)*(1-abs(X.s[i])/guard)+(1-X.s[i]<=guard)*(1-(1-X.s[i])/guard))) ) {
  Miss.ID[i] <- 1
  } else if( runif(1) > (1-((Y.s[i]+1<=guard)*(1-(Y.s[i]+1)/guard)+(abs(Y.s[i])<=guard)*(1-abs(Y.s[i])/guard)+(1-Y.s[i]<=guard)*(1-(1-Y.s[i])/guard))) ) {
  Miss.ID[i] <- 1
  }
}

return(cbind(X.s[Miss.ID!=1],Y.s[Miss.ID!=1]))

}
