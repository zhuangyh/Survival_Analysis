#-------------------------------------------------
# project: Compete Risks 
# original: 4/30/2014
# Name: Yonghua Zhuang
# for: 6646 Survival Analysis Spring 2014
# ------------------------------------------------
# Load the package (needed whenever using the package)
library(KMsurv)     
library(km.ci)  		
library(survival) 	# Standard survival package in R
library(gridExtra)
library(cmprsk)     # Competeting risk analysis package in R
library(ggplot2)
library(reshape) 

# Import Data set
combine <- read.csv("combine.csv", header=T)
head(combine)

# Estimate overall K-M curve without covariates and stratitication
km.fit0 <- survfit(Surv(years, event)~1,data=combine,
                  type = c("kaplan-meier"))

# Descriptive stats and list of output variables
summary(km.fit0)
km.fit0
attributes(km.fit0)

# overall K-M survival kurve
plot(km.fit0, conf.int=F, xlab="time until exit (in years)",
     ylab="proportion in power",
     cex=2, lty=1, col=4)
mtext("K-M Survival Curve for leader's power ") 

# Estimate cause-specific K-M curve: 
km.fit <- survfit(Surv(years, event)~strata(type),data=combine,
                  type = c("kaplan-meier"))

# Descriptive stats and list of output variables
summary(km.fit)
km.fit
attributes(km.fit)

# K-M survival plot for each categories of exit (casuse-specific)
plot(km.fit, xlab="time until exit (in years)",
     ylab="proportion in power",
     lab=c(10,10,7), cex=2, lty=1:3, col=1:3)
legend(2,.35, c("C-exit: constitutional", "D-exit: death", "N-exit: nonconstitutional"), lty=1:3, col=1:3)
mtext("K-M Survival Curve for leader's power") 

# log K-M survival plot (cumulative hazard)
plot(km.fit,conf.int=F, xlab="time until exit (in years)",
     ylab="cumulative hazard", fun="cumhaz",
     lab=c(10,10,7), cex=2, lty=1:3, col=1:3)
legend(0.2,1.2, c("C-exit: constitutional", "D-exit: death", "N-exit: nonconstitutional"),lty=1:3, col=1:3)
mtext("K-M Cumulative Hazard Curve for leader's power")

# Import cause-specific cumulative hazard estimation
exit1 <- read.csv("exit1.csv", header=T)
exit2 <- read.csv("exit2.csv", header=T)
exit3 <- read.csv("exit3.csv", header=T)

# Plot 3 individual cause-specific cululative hazard estimation and dispaly the poetential problem in cause-specific analysis
g1<-ggplot(exit1, aes(x = years, y = cexitcumhaz, colour="black")) + geom_line()
g2<-ggplot(exit2, aes(x = years, y = dexitcumhaz, colour = "green")) + geom_line()
g3<-ggplot(exit3, aes(x = years, y = nexitcumhaz, colour ="red")) + geom_line()
grid.arrange(g1+theme(legend.position="none"), g2+theme(legend.position="none"), g3+theme(legend.position="none"),ncol=3)


# Import data for cumulative incidence analysis
leader <- read.csv("leaders2.csv", header=T)

attach (leader)

los=factor(lost, levels=c(0:3), labels= c("In-power", "C-exit", "D-exit", "N-exit"))

mann=factor(manner, levels=c(0,1), labels= c("M0: Consti-power", "M1: Noncon-power "))

# Summary data with tables
table(mann, los)
tapply(years, list(mann, los), mean)

## cuminc(ftime, fstatus, group, cencode=0,...) where
## ftime is failure time, fstatus is failure status, group is optional group indicator
## cencode is the code for censored observations in fstatus
fit.1 <- cuminc(leader$years, leader$lost, cencode=0)
fit.1

## plot competing risks
plot(fit.1)

## to customize, use custom function from package
plot(fit.1, col=1:3, curvlab=c("C-exit: constional", "D-exit: death", "N-exit: nonconstional"),xlab="Years Post power",ylim=c
     (0,1), ylab="Probability", main="Competing Risks:
\aCumulative Incidence of exits")

## using timepoints(w, times)
## w is a cuminc() object, times is vector of time
fits.1c <- timepoints(fit.1,leader$years)
fits.1c

## convert list to df, format nicely
df <- as.data.frame(cbind(t(fits.c$est),t(fits.c$var)))
names(df) <- c("Cexit", "Dexit", "Nexit", "var(Cexit)", "var(Dexit)","var(Nexit)")
row.names(df) <- c(1:length(df[[1]]))

## Add times to the dataframe
df$time <- sort(unique(leader$years))
head(df)
## rearrange order of display and subset
dfCI <- df[,c(7,1:3)]
head(dfCI)
tail(dfCI)

## Stacking C,D, E cumulative incidence
dfCI$CDexit<-dfCI$Cexit + dfCI$Dexit
dfCI$CDNexit<-dfCI$Cexit + dfCI$Dexit+ dfCI$Nexit
head(dfCI)
## Export cumulative incidence estimates
write.table(dfCI, "dfCI.xls", quote=FALSE, sep="\t")

## Reorganize dataframe for plotting
stackedCI <- dfCI[,c(1,2,5, 6)]
head(stackedCI)

# Plot Stacked cumulative incidence
Molten1 <- melt(stackedCI, id.vars = "time")
head(Molten1)

g1<- ggplot(Molten1, aes(x =time, y = value, colour = variable, ylab="cumulative incidence")) + geom_line()
g1

# Alternative codes for cumulative incidence estimation and ploting
source ("CumIncidence.R")
# Cumulative incidence estimation with Gray's test for group comparison
fit=CumIncidence (years, lost, cencode = 0, xlab="Years")

## cuminc(ftime, fstatus, group, cencode=0,...) where
## ftime is failure time, fstatus is failure status, group is optional group indicator
## cencode is the code for censored observations in fstatus

fit.2 <- cuminc(leader$years, leader$lost, leader$manner, cencode=0)
fit.2

## using timepoints(w, times)
## w is a cuminc() object, times is vector of time
fits.c2 <- timepoints(fit.1,leader$years)
fits.c2

## plot competing risks
plot(fit.2)
dev.off()
## to customize, use custom function from package
plot(fit.2, col=c(1,1,2,2,3,3), curvlab=c("M0 C-exit", "M1 C-exit", "M0 D-exit", "M1 D-exit", "M0 N-exit", "M1 N-exit"), xlab="Years Post power",ylim=c
     (0,1), ylab="Cumulative Incidence", main="Competing Risks:
\aCumulative Incidence of exits")

# Alternative codes for gray's test+ploting with "CumIncidence" function
# source function of "CumIncidence"
source ("CumIncidence.R")
# Cumulative incidence estimation with Gray's test for group comparison
fit=CumIncidence (years, lost, manner, cencode = 0, xlab="Years")





