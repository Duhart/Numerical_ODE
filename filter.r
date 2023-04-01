library(dplyr)

x <- 1:20
f <- 1:5

r <- 2

H_matrix_fun <- function(x,r){
 N <- length(x)
 H <- matrix(rep(0,N*(2*r+1)),nrow = N)
 for(n in 1:N){
   H[n,(max(-n+1,-r)+r+1):(min(N-n,r)+r+1)] <- x[(n-min(n+r,N)+r+1):(n-max(n-r,1)+r+1)]
 }
 return(H)
}

