#-------------------------------------------------#
# Figures 1a and 1b
# Evaluating the Impact of Florida’s “Stand Your 
# Ground” Self-defense Lawon Homicide and Suicide 
# by Firearm An Interrupted Time Series Study (2017)
# JAMA Internal Medicine
# DOI:10.1001/jamainternmed.2016.6811
# 
# David K. Humphreys, Antonio Gasparrini, 
# Douglas J. Wiebe (2017)
# Email: david.humphreys@spi.ox.ac.uk
#-------------------------------------------------#


# 1. JAMA Graphics (Fig 1A: Homicide rates in Florida and comparison states)

# Remember that you need to run the models in 3.1 and 3.2 in Stand your ground.R

  #flhom.m <- glm(fl_hom ~ offset(log(fl_stdpop)) + Effective + time + 
  #                    harmonic(month,2,12), family=quasipoisson, flhom)
  #chom.m <- glm(c_hom ~ offset(log(c_stdpop)) + Effective + time + 
  #                   harmonic(month,2,12), family=quasipoisson, flhom)

# 1.1 Create data points

# Florida
flhom.obs <- with(flhom, fl_hom/fl_stdpop*10^5)
flhom.datanew <- data.frame(fl_stdpop=mean(flhom$fl_stdpop),Effective=rep(c(0,1),c(819,1101)),
                            time= 1:1920/10,month=rep(1:120/10,16))
flhom.m.pred1 <- predict(flhom.m,type="response",flhom.datanew)/mean(flhom$fl_stdpop)*10^5
flhom.m.pred2 <- predict(flhom.m,type="response",transform(flhom.datanew,month=4.5))/mean(flhom$fl_stdpop)*10^5

# Comparison States
chom.obs <- with(flhom, c_hom/c_stdpop*10^5)
chom.datanew <- data.frame(c_stdpop=mean(flhom$c_stdpop),Effective=rep(c(0,1),c(819,1101)),
                           time= 1:1920/10,month=rep(1:120/10,16))
chom.m.pred1 <- predict(chom.m,type="response",chom.datanew)/mean(flhom$c_stdpop)*10^5
chom.m.pred2 <- predict(chom.m,type="response",transform(chom.datanew,month=5.3))/mean(flhom$c_stdpop)*10^5

# Plot both sets of data points and trends
tiff(file="JAMA_IM_Figure1",width=4000,height=3000,res=600)
par (mar=c(4,4,1.5,1.5))
plot(1:192,flhom.obs,type="n",ylim=c(0.0,1.0),xlab="Year",
     ylab="Rate per 100,000", main= NULL, cex.lab=.75, cex.axis=.65, frame.plot=F,xaxt="n",las=2) 
rect(82,0.0,192,1.0, col=grey(0.9),border=F) 
points(1:192,flhom.obs,cex=0.7, pch=16, col="darkgoldenrod3") 
points(1:192,chom.obs ,cex=0.7, pch=16, col="dodgerblue4")
axis(1,at=0:16*12,labels=F, cex.lab=.75, cex.axis=.75) 
axis(1,at=0:15*12+6,tick=F,labels=1999:2014, cex.lab=.75, cex.axis=.65)  
lines(1:1920/10,flhom.m.pred1,col="darkgoldenrod3", lwd=2)
lines(1:1920/10,flhom.m.pred2,col="darkgoldenrod3",lty=2)
lines(1:1920/10,chom.m.pred1,col="dodgerblue4", lwd=2)
lines(1:1920/10,chom.m.pred2,col="dodgerblue4",lty=2)
legend ("topleft", legend=c("Florida", "Comparison States"), col=c("darkgoldenrod3", "dodgerblue4"), lty=1:1, cex=0.6, bty="n", inset=0.015)
dev.off()


#2. JAMA Graphics (Fig 2: Homicide by firearm rates in Florida and comparison states)

# 2.1 Create data points

# Florida
flfhom.obs <- with(flhom, fl_fhom/fl_stdpop*10^5)
flfhom.datanew <- data.frame(fl_stdpop=mean(flhom$fl_stdpop),Effective=rep(c(0,1),c(819,1101)),
                             time= 1:1920/10,month=rep(1:120/10,16))
flfhom.m.pred1 <- predict(flfhom.m,type="response",flfhom.datanew)/mean(flhom$fl_stdpop)*10^5
flfhom.m.pred2 <- predict(flfhom.m,type="response",transform(flfhom.datanew,month=4.5))/mean(flhom$fl_stdpop)*10^5

# Comparison States
cfhom.obs <- with(flhom, c_fhom/c_stdpop*10^5)
cfhom.datanew <- data.frame(c_stdpop=mean(flhom$c_stdpop),Effective=rep(c(0,1),c(819,1101)),
                            time= 1:1920/10,month=rep(1:120/10,16))
cfhom.m.pred1 <- predict(cfhom.m,type="response",cfhom.datanew)/mean(flhom$c_stdpop)*10^5
cfhom.m.pred2 <- predict(cfhom.m,type="response",transform(cfhom.datanew,month=5.3))/mean(flhom$c_stdpop)*10^5

# Plot both sets of data points and trends
tiff(file="JAMA_IM_Figure2",width=4000,height=3000,res=600)
par (mar=c(4,4,1.5,1.5))
plot(1:192,flfhom.obs,type="n",ylim=c(0.0,1.0),xlab="Year",
     ylab="Rate per 100,000", main= NULL, cex.lab=.75, cex.axis=.65, frame.plot=F,xaxt="n",las=2) 
rect(82,0.0,192,1.0, col=grey(0.9),border=F) 
points(1:192,flfhom.obs,cex=0.7, pch=16, col="darkgoldenrod3") 
points(1:192,cfhom.obs ,cex=0.7, pch=16, col="dodgerblue4")
axis(1,at=0:16*12,labels=F, cex.lab=.75, cex.axis=.75) 
axis(1,at=0:15*12+6,tick=F,labels=1999:2014, cex.lab=.75, cex.axis=.65)  
lines(1:1920/10,flfhom.m.pred1,col="darkgoldenrod3", lwd=2)
lines(1:1920/10,flfhom.m.pred2,col="darkgoldenrod3",lty=2)
lines(1:1920/10,cfhom.m.pred1,col="dodgerblue4", lwd=2)
lines(1:1920/10,cfhom.m.pred2,col="dodgerblue4",lty=2)
legend ("topleft", legend=c("Florida", "Comparison States"), col=c("darkgoldenrod3", "dodgerblue4"), lty=1:1, cex=0.6, bty="n", inset=0.015)
dev.off()


#---------------------------End---------------------------------#