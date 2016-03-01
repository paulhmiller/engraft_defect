# Scripts for In Vitro plots for Engraftment Defect paper
# Paul Miller, paulhmiller@gmail.com

setwd('C:/Users/paulm/CRC Paul/PROJECTS/engraft_defect/figures')

.pardefault <- par(no.readonly=T)
par(.pardefault)


source("C:/Users/paulm/Documents/R/source/functions.R")
#library(extrafont)
#font_import(pattern = 'calibri')
#loadfonts()

dat <- read.csv("week3.csv", header=T, check.names=F)

## 7 days ##

dat <- dat[, c(1,6:ncol(dat))]
dat <- dat[dat$Exp=='7_12_day' , ]
dat <- droplevels(dat)

pdf(file="./plots/vivoPlots.pdf", width=8/2.54, height=0.4+(4.5/2.54)) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(5.1,4.1,2.1,1.1), cex=0.7)
ylim1 <- c(0,125)
cols <- c("red", "purple", "orange", "blue")
#mgps = c(3,1,0)
#xlabs <- ("") #day
#labeldist = 2
titledist = 0.8



## GM

GM <- na.omit(dat[,c(2,4)])

GM <- summarySE(GM, measurevar="CD33.15", 
                 groupvars=c("Input", na.rm=TRUE))
# summarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
names(GM)[names(GM) == 'CD33.15'] <- 'mean'

# Separate out control arm
GMcntl <- GM[GM$Input=="fresh" ,]
GM <- GM[GM$Input !="fresh" ,]
GM <- droplevels(GM)
# Re-order factor levels:
GM$Input<-factor(GM$Input, levels=c("5GF", "5GF+PGE2+DPA", "3GF", "3GF+UM171"))
GM <- GM[order(GM$Input), ]

# Make plot
bp <- barplot(GM$mean, axes=F, ylim=ylim1, ylab="% of input values", col=cols)
#axis(side=1, at=bp, labels=levels(GM$Input), mgp=c(2,1,0))
labs <- levels(GM$Input)
text(cex=1, x=bp+0.5, y=-6, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="GM", line = titledist)

## Add control horizontal bar:
rect(0, 100-GMcntl$se, 5, 100+GMcntl$se, col="gray", border="gray")
abline(h=100, lty=3, lwd = 1.2)

## Add error bars:
segments(x0=bp, y0=GM$mean, x1=bp, y1=GM$mean + GM$se * 1, lwd = 1.5)
arrows(bp, GM$mean, bp,  GM$mean + GM$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
box()

# Double error bars:
#segments(bp, GM$mean - GM$se * 2, bp, GM$mean + GM$se * 2, lwd = 1.5)
#arrows(bp, GM$mean - GM$se * 2, bp, GM$mean + GM$se * 2, lwd = 1.5, angle = 90, code = 3, length = 0.05)



## Platelets

PLT <- na.omit(dat[,c(2,7)])

PLT <- summarySE(PLT, measurevar="Platelets", groupvars=c("Input", na.rm=TRUE))
# summarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
names(PLT)[names(PLT) == 'Platelets'] <- 'mean'

# Re-order factor levels:
PLT$Input<-factor(PLT$Input, levels=c("5GF", "5GF+PGE2+DPA", "3GF", "3GF+UM171", "fresh"))
PLT <- PLT[order(PLT$Input), ]

# Separate out control arm
PLTcntl <- PLT[PLT$Input=="fresh" ,]
PLT <- PLT[PLT$Input !="fresh" ,]
PLT <- droplevels(PLT)

# Make plot
bp <- barplot(rep(NA,4), axes=F, ylim=ylim1)#, ylab="% of input values", col=cols)
abline(h=18, lty=3)
#axis(side=1, at=bp, labels=levels(PLT$Input), mgp=c(2,1,0))
barplot(PLT$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", col=cols)
labs <- levels(PLT$Input)
text(cex=1, x=bp+0.6, y=-8, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="Platelets", line = titledist)

## Add control horizontal bar:
rect(0, 100-PLTcntl$se, 5, 100+PLTcntl$se, col="gray", border="gray")
abline(h=100, lty=3, lwd=1.2)

## Add error bars:
segments(x0=bp, y0=PLT$mean, x1=bp, y1=PLT$mean + PLT$se * 1, lwd = 1.5)
arrows(bp, PLT$mean, bp,  PLT$mean + PLT$se * 1, lwd = 1.2, angle = 90, code = 3, length = 0.05)
box()


dev.off()


