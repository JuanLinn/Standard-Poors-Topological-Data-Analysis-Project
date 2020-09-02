##Utilize your multiple cores to computing in R

library(doSNOW)
library(doParallel)

cores <- detectCores(logical=F)-1
cl <- makeCluster(cores)
registerDoSNOW(cl)


#in order to show the progress bar
  pb <- txtProgressBar(min = 0, max = length(spots), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
# an example in using parallell computation in RipsDiag
  DiagsA <- foreach(i = 1:length(spots),.packages="TDA",.options.snow = opts) %dopar% {
    Diags = ripsDiag(X[i:(i+window - 1), ], maxdimension=1,  maxscale = (max(dist(X[i:(i+window - 1),]))), library = "Dionysus")
    return(Diags)
  }
  K_max=#
# an example in using parallell computation in return a matrix instead of list. 
  AUC_Land.v <- foreach(i = 1:length(spots),.combine=rbind,.packages=c("TDA","PKNCA"),.options.snow = opts) %dopar% {
    
    for (KK in 1:K_max){
    LandsA[[i]] = landscape(DiagsA[[i]]$diagram, dimension=1, KK, seq(0,(max(dist(X[i:(i+window - 1),]))),length =  500))
    Matrix[i,KK]=  pk.calc.auc.all(LandsA[[i]], seq(0,(max(dist(X[i:(i+window - 1),]))),length =  500))
    Merge[i]= AUC_Land.v[i]+AUC_Land.m[i,KK]
    }
    return(Merge[i])
  }
  
  ##LandsA, Matrix will not be saved by R
  
stopCluster(cl) 
