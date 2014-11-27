above<-function(x,n=5){
  use<-x>n
  x[use]
}

calc_mean<-function(mat,flag=TRUE){
  n<-ncol(mat)
  means<-numeric(n)
  for(i in 1:n)
    means[i]<-mean(mat[,i],na.rm=flag)
  means
}


f<-function(x){
  y<-2
  y^2+g(x)
}
g<-function(x){
  x*y
}

test<-function(id=1:10){
  n<-c()
  for (i in id){
  n[i]<-i
  }
  n
}

cube <- function(x, n) {
  x^3
}

f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}