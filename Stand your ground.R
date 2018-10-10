#-------------------------------------------------#
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

################
# Prerequisites

#install.packages("foreign"); install.packages("splines")
#install.packages("lmtest") ; install.packages("Epi")
#install.packages("tsModel"); install.packages("vcd"); 
#install.packages("sandwich")
library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd"); library(sandwich)

#######################################
# 1. Upload dataset 1 ("syg_dat1.csv")

flhom <- read.csv("syg_dat1.csv")

#######################################################
# 2. Descriptives: Counts (Table 1; 0=Before; 1=After)

# 2.1 Florida homicide
  summary (flhom$fl_hom[flhom$Effective==0])
  summary (flhom$fl_hom[flhom$Effective==1])

# 2.2. Comparison state homicide
  summary (flhom$c_hom[flhom$Effective==0])
  summary (flhom$c_hom[flhom$Effective==1])

# 2.3 Note the outlier Sept, 2001 = 2753 homicides
  # Remove this data point from the analyses
    flhom[33, 7] = NA

# 2.4 Florida suicide
  summary (flhom$fl_suic[flhom$Effective==0])
  summary (flhom$fl_suic[flhom$Effective==1])

# 2.4 Comparison state suicide
  summary (flhom$c_suic[flhom$Effective==0])
  summary (flhom$c_suic[flhom$Effective==1])

# 2.5 Florida firearm homicide
  summary (flhom$fl_fhom[flhom$Effective==0])
  summary (flhom$fl_fhom[flhom$Effective==1])

# 2.6 Comparison state firearm homicide
  summary (flhom$c_fhom[flhom$Effective==0])
  summary (flhom$c_fhom[flhom$Effective==1])

# 2.7 Florida firearm suicide
  summary (flhom$fl_fsuic[flhom$Effective==0])
  summary (flhom$fl_fsuic[flhom$Effective==1])

# 2.8 Comparison states firearm suicide
  summary (flhom$c_fsuic[flhom$Effective==0])
  summary (flhom$c_fsuic[flhom$Effective==1])

# 3. Rate of homicide per 100,000 (Table 1)

  # 3.1 Florida homicide
    flhom$hom.r <- (flhom$fl_hom/flhom$fl_stdpop)*100000
      summary (flhom$hom.r[flhom$Effective==0])
      summary (flhom$hom.r[flhom$Effective==1])

  # 3.2 Comparison state homicide
    flhom$c_hom.r <- (flhom$c_hom/flhom$c_stdpop)*100000
      summary (flhom$c_hom.r[flhom$Effective==0])
      summary (flhom$c_hom.r[flhom$Effective==1])

  # 3.3 Florida suicide
    flhom$f_suic.r <- (flhom$fl_suic/flhom$fl_stdpop)*100000
      summary (flhom$f_suic.r[flhom$Effective==0])
      summary (flhom$f_suic.r[flhom$Effective==1])

  # 3.4 Comparison state suicide
    flhom$c_suic.r <- (flhom$c_suic/flhom$c_stdpop)*100000
      summary (flhom$c_suic.r[flhom$Effective==0])
      summary (flhom$c_suic.r[flhom$Effective==1])

  # 3.5 Florida firearm homicide
    flhom$f_fhom.r <- (flhom$fl_fhom/flhom$fl_stdpop)*100000
      summary (flhom$f_fhom.r[flhom$Effective==0])
      summary (flhom$f_fhom.r[flhom$Effective==1])

  # 3.6 Comparison state firearm homicide
    flhom$c_fhom.r <- (flhom$c_fhom/flhom$c_stdpop)*100000
      summary (flhom$c_fhom.r[flhom$Effective==0])
      summary (flhom$c_fhom.r[flhom$Effective==1])

  # 3.7 Florida firearm suicide
    flhom$f_fsuic.r <- (flhom$fl_fsuic/flhom$fl_stdpop)*100000
      summary (flhom$f_fsuic.r[flhom$Effective==0])
      summary (flhom$f_fsuic.r[flhom$Effective==1])

  # 3.8 Comparison state firearm suicide
    flhom$c_fsuic.r <- (flhom$c_fsuic/flhom$c2_stdpop)*100000
      summary (flhom$c_fsuic.r[flhom$Effective==0])
      summary (flhom$c_fsuic.r[flhom$Effective==1])

##################
# 4. Visualising 

# 4.1 Homicide (FLorida)
  hom.obs1 <- with(flhom, fl_hom/fl_stdpop*10^5) 
  hom.datnew1 <- data.frame(stdpop=mean(flhom$fl_stdpop), Effective=rep(c(0,1),c(819,1101)),  
                           time= 1:1920/10,month=rep(1:12/10,16))

  plot(1:192, hom.obs1,type="n",ylim=c(0.1,.9),xlab="Year",
     ylab="Rate (per 100,000)",frame.plot=F,xaxt="n", las=2) #specifies the plot area
  rect(82,0.1,192,0.9, col=grey(0.9),border=F) # format of the rectangle (xleft, ybottom, xright, ytop)
  points(1:192,hom.obs1,cex=0.7, pch=16) # Points 1:59 of obs 
  axis(1,at=0:16*12,labels=F) # Creates appropriate x-axis
  axis(1,at=0:15*12+6,tick=F,labels=1999:2014)  # Creates appropriate labels for x-axis
  title("Homicide in Florida 1999-2014") 

# 4.2 Homicide (Comparison States)
  hom.obs2 <- with(flhom, c_hom/c_stdpop*10^5) 
  hom.datnew2 <- data.frame(stdpop=mean(flhom$c_stdpop), Effective=rep(c(0,1),c(819,1101)),  
                         time= 1:1920/10,month=rep(1:12/10,16))

  plot(1:192, hom.obs2,type="n",ylim=c(0.0,.75),xlab="Year",
     ylab="Rate (per 100,000)",frame.plot=F,xaxt="n", las=2) #specifies the plot area
  rect(82,0.0,192,0.75, col=grey(0.9),border=F) # format of the rectangle (xleft, ybottom, xright, ytop)
  points(1:192,hom.obs2,cex=0.7, pch=16) # Points 1:59 of obs 
  axis(1,at=0:16*12,labels=F) # Creates appropriate x-axis
  axis(1,at=0:15*12+6,tick=F,labels=1999:2014)  # Creates appropriate labels for x-axis
  title("Homicide in Comparison States 1999-2014")


#########################################################################
# 4. ITS Poisson Segmented Regression Models - Adjusting for seasonality
  # using fourier term with 2 pairs of SIN-COS

# 4.1 Homicide (Florida)
  flhom.m <- glm(fl_hom ~ offset(log(fl_stdpop)) + Effective + time + 
                     harmonic(month,2,12), family=quasipoisson, flhom)
  summary(flhom.m) 
  round(ci.lin(flhom.m,Exp=T),3)

  summary(flhom.m)$dispersion # test for dispersion
  
  #Check the model residuals for autocorrelation
  ##############################################
  
  # Residual plots
  flhom.res1 <- residuals(flhom.m,type="deviance")
  
  plot(flhom.res1,ylim=c(-10,10),pch=19,cex=0.7,col=grey(0.6),
       main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  
  abline(h=0,lty=2,lwd=2)
  
  # Autocorelation/ Partial-autocorellation functions
  acf(flhom.res1, main="ACF")
  pacf(flhom.res1, main="PACF")
  
  # Breusch-Godfrey test for autocorrelation
  bgtest(flhom.m)
  bgtest(flhom.m, order=12)


# 4.2 Homicide (Comparison States)
  chom.m <- glm(c_hom ~ offset(log(c_stdpop)) + Effective + time + 
                 harmonic(month,2,12), family=quasipoisson, flhom)
  summary(chom.m) 
  round(ci.lin(chom.m,Exp=T),3)
  
  summary(flhom.m)$dispersion

  # Check residuals for autocorrelation
  #####################################
  
  # Residual plots
  chom.res1 <- residuals(chom.m,type="deviance")
  
  plot(chom.res1,ylim=c(-10,10),pch=19,cex=0.7,col=grey(0.6),
       main="Residuals over time",ylab="Deviance residuals",xlab="Date")
  
  abline(h=0,lty=2,lwd=2)
  
  # Autocorelation/ Partial-autocorellation functions
  acf(chom.res1, main="ACF")
  pacf(chom.res1, main="PACF")
  
  # Breusch-Godfrey test for autocorrelation
  bgtest(chom.m)
  bgtest(chom.m, order=12)
  
    #Note: significant autocorrelation
  
    ################
    # 4.2.1 Running robust standard errors: homicide comparison states

      # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
        vcov(chom.m)[1:3,1:3]
        summary(chom.m)$cov.scaled[1:3,1:3]

      # Standard estimates 
      summary(chom.m )
      coeftest(chom.m )

      # Covariance matrix, accounting for heteroskedascity and autocorrelation
      vcov(chom.m )[1:3,1:3]
      vcovHAC(chom.m )[1:3,1:3]

      # 'Robust' estimates using the function vcovHAC in 'coeftest'
      coeftest(chom.m )
      coeftest(chom.m  ,vcov=vcovHAC)

      # 95% CIs must be calculated by hand
      coef <- coef(chom.m )["Effective"]
      se <- sqrt(vcovHAC(chom.m )["Effective","Effective"])
      c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))

        # Delete variables or name appropriately
        coef <- NULL
        se <- NULL

# 4.3 Suicide (Florida)
  flsuic.m <- glm(fl_suic ~ offset(log(fl_stdpop)) + Effective + time + 
                  harmonic(month,2,12), family=quasipoisson, flhom)
  summary(flsuic.m) 
  round(ci.lin(flsuic.m,Exp=T),3)
  summary(flhom.m)$dispersion

  ################
  # 4.3.1 Running robust standard errors to account for autocorrelation using a sandwich estimator

    # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
      vcov(flsuic.m)[1:3,1:3]
      summary(flsuic.m)$cov.scaled[1:3,1:3]

    # Standard estimates 
      summary(flsuic.m )
      coeftest(flsuic.m )

    # Covariance matrix, accounting for heteroskedascity and autocorrelation
      vcov(flsuic.m )[1:3,1:3]
      vcovHAC(flsuic.m )[1:3,1:3]

    # 'Robust' estimates using the function vcovHAC in 'coeftest'
      coeftest(flsuic.m)
      coeftest(flsuic.m  ,vcov=vcovHAC)

    # 95% CIs must be calculated by hand
      coef <- coef(flsuic.m )["Effective"]
      se <- sqrt(vcovHAC(flsuic.m )["Effective","Effective"])
      c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))

      # Delete variables or name appropriately
        coef <- NULL
        se <- NULL

# 4.4 Suicide (Comparison states)
  csuic.m <- glm(c_suic ~ offset(log(c_stdpop)) + Effective + time + 
                  harmonic(month,2,12), family=quasipoisson, flhom)
  summary(csuic.m) 
  round(ci.lin(csuic.m,Exp=T),3)
  summary(csuic.m)$dispersion

  ################
  # 4.4.1 Running robust standard errors to account for autocorrelation using a sandwich estimator

    # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
      vcov(csuic.m)[1:3,1:3]
      summary(csuic.m)$cov.scaled[1:3,1:3]

    # Standard estimates 
      summary(csuic.m )
      coeftest(csuic.m )

    # Covariance matrix, accounting for heteroskedascity and autocorrelation
      vcov(csuic.m )[1:3,1:3]
      vcovHAC(csuic.m )[1:3,1:3]

    # 'Robust' estimates using the function vcovHAC in 'coeftest'
      coeftest(csuic.m)
      coeftest(csuic.m  ,vcov=vcovHAC)

    # 95% CIs must be calculated by hand
      coef <- coef(csuic.m )["Effective"]
      se <- sqrt(vcovHAC(csuic.m )["Effective","Effective"])
      c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))

      # Delete variables or name appropriately
        coef <- NULL
        se <- NULL

  # 4.5 Firearm Homicide (Florida) 
    flfhom.m <- glm(fl_fhom ~ offset(log(fl_stdpop)) + Effective + time + 
                 harmonic(month,2,12), family=quasipoisson, flhom)
    summary(flfhom.m ) 
    round(ci.lin(flfhom.m ,Exp=T),3)
    summary(flfhom.m )$dispersion

    ################
    # 4.4.1 Running robust standard errors to account for autocorrelation using a sandwich estimator

      # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
      vcov(flfhom.m)[1:3,1:3]
      summary(flfhom.m)$cov.scaled[1:3,1:3]

      # Standard estimates 
      summary(flfhom.m )
      coeftest(flfhom.m )

      # Covariance matrix, accounting for heteroskedascity and autocorrelation
      vcov(flfhom.m )[1:3,1:3]
      vcovHAC(flfhom.m )[1:3,1:3]

      # 'Robust' estimates using the function vcovHAC in 'coeftest'
      coeftest(flfhom.m)
      coeftest(flfhom.m  ,vcov=vcovHAC)

      # 95% CIs must be calculated by hand
      coef <- coef(flfhom.m )["Effective"]
      se <- sqrt(vcovHAC(flfhom.m )["Effective","Effective"])
      c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))

      # Delete variables or name appropriately
      coef <- NULL
      se <- NULL

  # 4.6 Firearm Homicide (Comparison states)
  cfhom.m <- glm(c_fhom ~ offset(log(c_stdpop)) + Effective + time + 
                  harmonic(month,2,12), family=quasipoisson, flhom)
  summary(cfhom.m ) 
  round(ci.lin(cfhom.m ,Exp=T),3)
  summary(cfhom.m )$dispersion

    ################
    # 4.6.1 Running robust standard errors to account for autocorrelation using a sandwich estimator

      # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
      vcov(cfhom.m)[1:3,1:3]
      summary(cfhom.m)$cov.scaled[1:3,1:3]

      # Standard estimates 
      summary(cfhom.m)
      coeftest(cfhom.m )

      # Covariance matrix, accounting for heteroskedascity and autocorrelation
      vcov(cfhom.m )[1:3,1:3]
      vcovHAC(cfhom.m )[1:3,1:3]

      # 'Robust' estimates using the function vcovHAC in 'coeftest'
      coeftest(cfhom.m)
      coeftest(cfhom.m  ,vcov=vcovHAC)

      # 95% CIs must be calculated by hand
      coef <- coef(cfhom.m )["Effective"]
      se <- sqrt(vcovHAC(cfhom.m )["Effective","Effective"])
      c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))

      coef <- NULL
      se <- NULL

  # 4.7 Firearm Suicide (Florida)
  flfsuic.m <- glm(fl_fsuic ~ offset(log(fl_stdpop)) + Effective + time + 
                 harmonic(month,2,12), family=quasipoisson, flhom)
  summary(flfsuic.m ) 
  round(ci.lin(flfsuic.m  ,Exp=T),3)
  summary(flfsuic.m  )$dispersion

  # 4.6.1 Running robust standard errors to account for autocorrelation using a sandwich estimator
  
    # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
    vcov(flfsuic.m)[1:3,1:3]
    summary(flfsuic.m)$cov.scaled[1:3,1:3]
    
    # Standard estimates 
    summary(flfsuic.m)
    coeftest(flfsuic.m )
    
    # Covariance matrix, accounting for heteroskedascity and autocorrelation
    vcov(flfsuic.m )[1:3,1:3]
    vcovHAC(flfsuic.m )[1:3,1:3]
    
    # 'Robust' estimates using the function vcovHAC in 'coeftest'
    coeftest(flfsuic.m)
    coeftest(flfsuic.m  ,vcov=vcovHAC)
    
    # 95% CIs must be calculated by hand
    coef <- coef(flfsuic.m )["Effective"]
    se <- sqrt(vcovHAC(flfsuic.m )["Effective","Effective"])
    c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))
    
    # Delete variables or name appropriately
      coef <- NULL
      se <- NULL

  # 4.7 Firearm Suicide (Comparison states)
  cfsuic.m <- glm(c_fsuic ~ offset(log(c2_stdpop)) + Effective + time + 
                   harmonic(month,2,12), family=quasipoisson, flhom)
  summary(cfsuic.m ) 
  round(ci.lin(cfsuic.m  ,Exp=T),3)
  summary(cfsuic.m  )$dispersion

    # 4.7.1 Running robust standard errors to account for autocorrelation using a sandwich estimator
    
    # Standard covariance matrix (Returned by 'vcov' or as a component of 'summary')
    vcov(cfsuic.m)[1:3,1:3]
    summary(cfsuic.m)$cov.scaled[1:3,1:3]
    
    # Standard estimates 
    summary(cfsuic.m)
    coeftest(cfsuic.m )
    
    # Covariance matrix, accounting for heteroskedascity and autocorrelation
    vcov(cfsuic.m )[1:3,1:3]
    vcovHAC(cfsuic.m )[1:3,1:3]
    
    # 'Robust' estimates using the function vcovHAC in 'coeftest'
    coeftest(cfsuic.m)
    coeftest(cfsuic.m ,vcov=vcovHAC)
    
    # 95% CIs must be calculated by hand
    coef <- coef(cfsuic.m )["Effective"]
    se <- sqrt(vcovHAC(cfsuic.m )["Effective","Effective"])
    c(RR=exp(coef),ll=exp(coef-qnorm(0.975)*se),ul=exp(coef+qnorm(0.975)*se))
    
    # Delete variables or name appropriately
      coef <- NULL
      se <- NULL

# 5. Difference in difference tests (Florida vs Comparison)

# Here we perform a formal statistical test of the change in homicide related to change in comparison states
# as opposed to changes in pre-treatment trends

  # 5.1 Import second datafile with Florida and comparison cases appended
  # 'case' variable indicates "1" = Florida & "0" = Comparison states
  flcom <- read.csv("syg_dat2.csv")
  View (flcom)

  # 5.2 Model with interaction term: homicide
  int.m1 <- glm(hom ~ offset(log(stdpop)) + Effective*case + time*case
          + harmonic(month,2,12),family=quasipoisson, flcom)
  summary(int.m1)
  round(ci.lin(int.m1,Exp=T),5)

  # Explanation:
  ###############

    # According to these results, the effect in the control states is RR=1.05300 (under 'Effective')
    # The effect in Florida is RR=1.187 (under 'Effective:case')
    # Therefore, to compute the results we multiply 1.053 x 1.18768 = 1.250627. 
    # The p-value of the interaction terms (0.00020) represents formal test of difference 
    # reported in the paper. We use the more conservative estimates of post v pre in the manuscript.
    # The most appropriate counterfactual (pre-period or comparison state) is debatable

  # 5.3 Model with interaction term: suicide
  int.m2 <- glm(suic ~ offset(log(stdpop)) + Effective*case + time*case
              + harmonic(month,2,12),family=quasipoisson, flcom)
  summary(int.m2)
  round(ci.lin(int.m2,Exp=T),5)

  # 5.4 Model with interaction term: firearm homicide
  int.m3 <- glm(fhom ~ offset(log(stdpop)) + Effective*case + time*case
                + harmonic(month,2,12),family=quasipoisson, flcom)
  summary(int.m3)
  round(ci.lin(int.m3,Exp=T),5)
  
  # 5.5 Model with interaction term: firearm suicide
  int.m4<- glm(fsuic ~ offset(log(stdpop2)) + Effective*case + time*case
                + harmonic(month,2,12),family=quasipoisson, flcom)
  summary(int.m4)
  round(ci.lin(int.m4,Exp=T),5)


# Open Figures Stand your ground.R to create graphics    
    
#---------------------------END----------------------------#
    
    