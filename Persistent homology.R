library(TDA)
library(quantmod)
library(tseries)
library(PKNCA)
library(TTR)
library(xts)
library(doSNOW)
library(doParallel)

# This code use parallel computation to enhance the efficiency.
# This code calculate the Variance of first-order Landscape of rips complex of a time series data.
# High resolution plots are at the end!

#BEFORE RUNNING THIS CODE DO THE FOLLOWING...

########################
# SET YOUR SECTOR NAME
sector = "Sector"

# SET PATH AND FILE NAME FOR A TIME SERIES DATA
mydf.data=read.csv(file="data.csv", header=TRUE)
########################

# Set up parallel computation environment
cores <- detectCores(logical=F)-1
cl <- makeCluster(cores)
registerDoSNOW(cl)

# Convert to xts time series, your date format may differ
mydf.data$Date = as.Date(mydf.data$Date,tryFormats = c("%m/%d/%Y", "%b-%d-%Y"))
mydf.dataTS = xts(mydf.data[,-1], order.by=mydf.data$Date)

# Normalize Data:
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Divide by max Normalization for data
dfNorm.data <- mydf.dataTS
for (i in 1: ncol(dfNorm.data)) {
  dfNorm.data[,i] <- dfNorm.data[,i]/max(abs(dfNorm.data[,i]))
}

# Take mean of each row - get average line first for Data
avgData.data = xts(rowMeans(dfNorm.data, na.rm = TRUE), index(dfNorm.data))

# Calculate log-returns
# Note that first day in log-returns yields NA, therefore we drop it
avgData.datalr = Delt(avgData.data, type='log')[-1]

##################################
# TDA starts here!!!!!!!!!!!!!!!
##################################

#Step 1: Create a multiple-dimensional points cloud
#Step 2: Establish corresponding rips complex
#Step 3: Find the presistent homologies and corresponding k-order Landscape
#Step 4: Transfer L-k to a more visuable indicator.

# Point Cloud, this methodology is backed by Taken's theorem
Xdata = embed(avgData.datalr, d)
K_max=k # max value of k in the L-function 
maxdimension =1

##################################
# Here are the hyperparameters dimension and window size
##################################

for (d in c(d1,d2,d3){
  for (window in c(w1,w2,w3)){
    Diags1.rips.ls = list()
    X<-Xdata
    total <- dim(X)[1]
    to=total-window
    
    # Set up the txtprogressbar for parallel computation.
    pb <- txtProgressBar(min = 0, max = to, style = 3)
    progress <- function(n) setTxtProgressBar(pb, n)
    opts <- list(progress = progress)
    
    # Compute the data persistence diagrams
    print(paste("Computing data persistence diagrams for dimension,"d,"window",window))  
    Diags1.rips.ls <- foreach(i = 1:to,.packages="TDA",.options.snow = opts) %dopar% {
      Diags = ripsDiag(X[i:(i+window - 1), ], maxdimension=1,  maxscale = (max(dist(X[i:(i+window - 1),]))), library = "Dionysus")
      return(Diags)
    }
    
    # Save your work so you don't have to recompute
    save(Diags1.rips.ls, file=paste0(sector, "dataripsDiagrams_window",window))
    dataRips <- Diags1.rips.ls
    
    #############################################################
    #Lanscapes first data
    #values at which the landscape function is evaluated
    
    Lands1.ls = list()
    AUC_Land.m = matrix(0,to,K_max) #collects values of lambda_k at particular date i per k 
    AUC_Land.v = vector(length=to) #collects values of L^1 at particular date
    X<-Xdata
    Diags1.rips.ls <- dataRips
    print(paste("Computing the data L1 norm of persistence landscapes for dimension", d, "window",window))
    
    
    #Compute L^1
    AUC_Land.v <- foreach(i = 1:to,.combine=rbind,.packages=c("TDA","PKNCA"),.options.snow = opts) %dopar% {
      for (KK in 1:K_max){
        Lands1.ls[[i]] = landscape(Diags1.rips.ls[[i]]$diagram, dimension=1, KK, seq(0,(max(dist(X[i:(i+window - 1),]))),length =  500))
        AUC_Land.m[i,KK]=  pk.calc.auc.all(Lands1.ls[[i]], seq(0,(max(dist(X[i:(i+window - 1),]))),length =  500))
        AUC_Land.v[i]= AUC_Land.v[i]+AUC_Land.m[i,KK]
      }
      return(AUC_Land.v[i])
    }
    
    # Save your work so you don't have to recompute
    save(AUC_Land.v, file=paste0(sector, "dataLandscape_window",window))
    
    date.lr= mydf.data$Date[-1]
    AUC_Land.v.xts <- xts(AUC_Land.v, order.by=date.lr[(window+1):totaldata])
    
    # Save files to turn in
    write.csv(as.data.frame(AUC_Land.v.xts),paste(sector, "dataL1_Window",window,".csv"))
    dataL1 <- AUC_Land.v.xts
              
              
    # Calculate the Variance  
    dataL1.norm <- normalize(dataL1)
    dataL1var=c();equityL1var=c()
    nn=dim(dataL1)[1]-window
    for (i in 1:(nn+1)){
    dataL1var[i]<-var(dataL1[i:(i+window-1)])
              
    dataL1var.xts=xts(dataL1var,order.by = date.lr[(window+window):totaleq])
    dataL1var.norm <- normalize(dataL1var.xts)
              
    assign(paste0("dataL1.win",window),dataL1)
    assign(paste0("dataL1var.win",window,".norm"),dataL1var.norm)
  }
}
stopCluster(cl) 

# high resolution plots.
png(paste0(sector,"_dataL1VAR_Window.png"), width =15, height =8, units = 'in', res = 300)
plot.xts(dataL1var.win100.norm,format.labels = "%b-%d\n20%y",major.ticks = "years",grid.ticks.on = "years", lwd=4,col = 'purple',lty=5,cex=1,yaxis.right = FALSE, ylim=c(-0.01, 1.1),main=sector)
lines(dataL1var.win50.norm,lwd=4,lty=1,col='red')
lines(dataL1var.win75.norm,lwd=4,lty=6,col='black')
dev.off()

