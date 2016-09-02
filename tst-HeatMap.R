ump <- read.csv(file="umpire_10.csv", h=T) head(ump) attach(ump) ##simple pitch location scatterplot for all batters?? png(file=”FroemmingAllScatter.png”, width=540, height=675) plot(pz[plate_umpire_id==10]~px[plate_umpire_id==10], main=”Bruce Froemming Strike Zone”, type=”n”, xlab=”Horizontal Location (ft.)”, ylab=”Vertical Location (ft.)”, xlim=c(-2,2), ylim=c(0,5)) points(pz[plate_umpire_id==10 & call_type==0]~px[plate_umpire_id==10 & call_type==0], col=”darkred”) points(pz[plate_umpire_id==10 & call_type==1]~px[plate_umpire_id==10 & call_type==1], col=”darkgreen”, pch=18) lines(c(0.708335, 0.708335), c(mean(sz_bot), mean(sz_top)), col=”black”, lty=”dashed”, lwd=2) lines(c(-0.708335, -0.708335), c(mean(sz_bot), mean(sz_top)), col=”black”, lty=”dashed”, lwd=2) lines(c(-0.708335, 0.708335), c(mean(sz_bot), mean(sz_bot)), col=”black”, lty=”dashed”, lwd=2) lines(c(-0.708335, 0.708335), c(mean(sz_top), mean(sz_top)), col=”black”, lty=”dashed”, lwd=2) legend(-2.1, 5.2, c(“Called Strike”, “Called Ball”), fill=c(“darkgreen”, “darkred”), bty=”n”, cex=1.2) dev.off() #use loess and filled.contour sz.width <- 0.708335 - (-0.708335)
sz.height <- mean(ump$sz_top)- mean(ump$sz_bot)
aspect.ratio <- (max(px)-min(px))/(max(pz)-min(pz)) fit <- loess(call_type ~ px + pz, span=c(0.5*aspect.ratio, 0.5), degree=1) myx <- matrix(data=seq(from=-2, to=2, length=30), nrow=30, ncol=30) myz <- t(matrix(data=seq(from=0,to=5, length=30), nrow=30, ncol=30)) fitdata <- data.frame(px=as.vector(myx), pz=as.vector(myz)) mypredict <- predict(fit, fitdata) mypredict <- ifelse(mypredict > 1, 1, mypredict)

mypredict <- ifelse(mypredict <0, 0, mypredict) mypredict <- matrix(mypredict,nrow=c(30,30)) png(file=”FroemmingAll.png”, width=600, height=675) filled.contour(x=seq(from=-2, to=2, length=30), y=seq(from=0, to=5, length=30), z=mypredict, zlim=c(0,1), nlevels=50,color=colorRampPalette(c(“darkblue”, “blue4”, “darkgreen”, “green4”, “greenyellow”, “yellow”, “gold”, “orange”, “darkorange”, “red”, “darkred”)), main=”Bruce Froemming Strike Zone Map”, xlab=”Horizontal Location (ft.)”, ylab=”Vertical Location (ft.)”, plot.axes={
    axis(1, at=c(-2,-1,0,1,2), pos=0, labels=c(-2,-1,0,1,2), las=0, col=”black”)
    axis(2, at=c(0,1,2,3,4,5), pos=-2, labels=c(0,1,2,3,4,5), las=0, col=”black”)
    rect(-0.708335, mean(ump$sz_bot), 0.708335, mean(ump$sz_top), border=”black”,
         lty=”dashed”, lwd=2)
},

key.axes={
    ylim=c(0,1.0)
    axis(4, at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), labels=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), pos=1, las=0,
         col=”black”)
})

text(1.4, 2.5, “Probability of Strike Call”, cex=1.1, srt=90)

dev.off()

############trying to use the ‘gam’ package instead of loess because data is binary

library(gam)

####all batters

attach(ump)

fit.gam <- gam(call_type ~ lo(px, span=.5*aspect.ratio, degree=1) + lo(pz, span=.5, degree=1), family=binomial(link="logit")) myx.gam <- matrix(data=seq(from=-2, to=2, length=30), nrow=30, ncol=30) myz.gam <- t(matrix(data=seq(from=0,to=5, length=30), nrow=30, ncol=30)) fitdata.gam <- data.frame(px=as.vector(myx.gam), pz=as.vector(myz.gam)) mypredict.gam <- predict(fit.gam, fitdata.gam, type="response") mypredict.gam <- matrix(mypredict.gam,nrow=c(30,30))
png(file=”FroemmingAllGAM.png”, width=600, height=675)

filled.contour(x=seq(from=-2, to=2, length=30), y=seq(from=0, to=5, length=30), z=mypredict.gam, axes=T, zlim=c(0,1), nlevels=50,
               color=colorRampPalette(c(“darkblue”, “blue4”, “darkgreen”, “green4”, “greenyellow”, “yellow”, “gold”, “orange”, “darkorange”, “red”, “darkred”)),
               main=”Bruce Froemming Strike Zone Map (GAM Package)”, xlab=”Horizontal Location (ft.)”, ylab=”Vertical Location (ft.)”,
               
               plot.axes={
                   
                   axis(1, at=c(-2,-1,0,1,2), pos=0, labels=c(-2,-1,0,1,2), las=0, col=”black”)
                   axis(2, at=c(0,1,2,3,4,5), pos=-2, labels=c(0,1,2,3,4,5), las=0, col=”black”)
                   rect(-0.708335, mean(ump$sz_bot), 0.708335, mean(ump$sz_top), border=”black”, lty=”dashed”, lwd=2)
                   
               },
               
               key.axes={
                   
                   ylim=c(0,1.0)
                   axis(4, at=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), labels=c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1.0), pos=1, las=0, col=”black”)
                   
               })

text(1.4, 2.5, “Probability of Strike Call”, cex=1.1, srt=90)

dev.off() 