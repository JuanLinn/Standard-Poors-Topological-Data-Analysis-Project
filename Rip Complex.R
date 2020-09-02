library(TDA)
library(quantmod)
library(tseries)
library(PKNCA)
library(TTR)
library(xts)

Circle1 <- circleUnif(60)
Circle2 <- circleUnif(60,r=2)+3
Circles <- rbind(Circle1,Circle2)
plot(Circles)
rip <- ripsDiag(Circles, maxdimension = 1,maxscale=8,library = "Dionysus",location = FALSE)
land <- landscape(rip$diagram,KK=1)
plot(land)


