#High resolution plot
png(paste0(sector,"_CDSL1VAR_Dimension.png"), width =15, height =8, units = 'in', res = 300)
plot.xts(cdsL1var.d8.norm,format.labels = "%b-%d\n20%y",major.ticks = "years",grid.ticks.on = "years", lwd=4,col = 'purple',lty=5,cex=1,yaxis.right = FALSE, ylim=c(-0.01, 1.1),main=sector)
lines(cdsL1var.d4.norm,lwd=4,lty=1,col='red')
lines(cdsL1var.d2.norm,lwd=4,lty=6,col='black')
dev.off()
