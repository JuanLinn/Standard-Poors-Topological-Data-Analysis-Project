library(quantmod)
library(xts)

##A,B,C are Extensible Time Series which can match themselves automatically in same day.

raw <- as.data.frame(na.omit(merge(A,B,C)))
len <- nrow(raw)
matrix0 <- matrix(c(seq(0,(len/252),length.out=len),raw$A),ncol = 2) [1:len,]
matrix1 <- matrix(c(seq(0,(len/252),length.out=len),raw$B),ncol = 2) [1:len,]
matrix2 <- matrix(c(seq(0,(len/252),length.out=len),raw$C),ncol = 2) [1:len,]

dis0 <- dis1 <- dis2 <- dis.dy.0 <- dis.dy.1 <- dis.dy.2 <- dis.dy.max0 <- c();win <- len/max(matrix0[,1])

ss <- 1000
pb <- txtProgressBar(min = 0, max=ss, style = 3)
for(j in 1:ss){
  matrix00 <- matrix(c(seq(0,(len/252),length.out=len),runif(len,0,1)),ncol = 2) 
  for (i in 1:len){
    spot <- seq(i-win,i+win)[which(seq(i-win,i+win)>0 & seq(i-win,i+win)<=len)]
    dis.dy.0[i] <- min(dist(matrix(c(matrix00[i,],t(matrix0[spot,])),ncol=2,byrow = TRUE))[1:length(spot)])
  }
  dis.dy.max0[j] <- sum(dis.dy.0)
  setTxtProgressBar(pb,j)
}
dist.dy.max <- mean(dis.dy.max0)



#abs value difference
for (i in 1:len){
  dis0[i] <- max(matrix0[i,2],1-matrix0[i,2])/2
  dis1[i] <- dist(matrix(c(matrix1[i,2],t(matrix0[i,2])),ncol=1,byrow = TRUE))[1]
  dis2[i] <- dist(matrix(c(matrix2[i,2],t(matrix0[i,2])),ncol=1,byrow = TRUE))[1]
  spot <- seq(i-win,i+win)[which(seq(i-win,i+win)>0 & seq(i-win,i+win)<=len)]
  dis.dy.1[i] <- min(dist(matrix(c(matrix1[i,],t(matrix0[spot,])),ncol=2,byrow = TRUE))[1:length(spot)])
  dis.dy.2[i] <- min(dist(matrix(c(matrix2[i,],t(matrix0[spot,])),ncol=2,byrow = TRUE))[1:length(spot)])
}
dist.max <- sum(dis0)
dist1 <- sum(dis1)
dist2 <- sum(dis2)
dist.dy.1 <- sum(dis.dy.1)
dist.dy.2 <- sum(dis.dy.2)


abs1 <- dist1/dist.max
abs2 <- dist2/dist.max
dy1 <- dist.dy.1/dist.dy.max
dy2 <- dist.dy.2/dist.dy.max
cat("The First absolute distance is ",abs1*100,"%\nThe Second absolute distance is ",abs2*100,"%\nThe First Dynamic distance is ",dy1*100,"%\nThe Second Dynamic distance is ",dy2*100,"%",sep="")

