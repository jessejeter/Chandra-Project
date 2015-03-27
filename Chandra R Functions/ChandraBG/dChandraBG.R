dChandraBG <- function(x) {

int.range <- c(-1, -.96, -.04, 0, .04, .96, 1)
int.est <- c(0, .44, .56, .20, .56, .44, 0)
area.int <- sum(1/2 * (int.est[1:6] + int.est[2:7]) * diff(int.range))
int.est.norm <- int.est/area.int
int.sum <- c(0, cumsum(1/2 * (int.est.norm[1:6] + int.est.norm[2:7]) * 
                       diff(int.range)))

den.x <- rep(0, length(x))
for(i in 1:6) {
  den.x <- den.x + (int.range[i] < x & x <= int.range[i + 1]) * 
           (int.est.norm[i] + (int.est.norm[i + 1] - int.est.norm[i]) * 
           (x - int.range[i]) / (int.range[i + 1] - int.range[i]))
}

return(den.x)

}
