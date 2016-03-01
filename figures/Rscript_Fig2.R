# Scripts for In Vitro plots for Engraftment Defect paper
# Paul Miller, paulhmiller@gmail.com

setwd('C:/Users/paulm/CRC Paul/PROJECTS/engraft_defect/figures')

.pardefault <- par(no.readonly=T)
par(.pardefault)


source("C:/Users/paulm/Documents/R/source/functions.R")
#library(extrafont)
#font_import(pattern = 'calibri')
#loadfonts()






## 24 hour Timecourse ##
dat <- read.csv("week3.csv", header=T, check.names=F)
dat <- dat[, c(1,6:ncol(dat))]
dat <- dat[dat$Exp=='24h_timecourse' , ]
dat <- droplevels(dat)

## Add column with numeric values for easier plotting
for (i in 1:nrow(dat)){
    if (dat[i, 2] == '0h'){
	    dat$time[i] <- 0
}   else if (dat[i, 2] == '4h'){
        dat$time[i] <- 4
}   else if (dat[i, 2] == '24h'){
        dat$time[i] <- 24
}
}



pdf(file="./plots/fig2a.pdf", width=8/2.54, height=4.5/2.54) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(2.1,3.1,2.1,1.1), cex=0.7, mgp=c(2,0.5,0))
ylim1 <- c(0,115)
ylim2 <- c(0,135)
xlims <- c(-1.5,25.5)
#cols <- c("red", "purple", "orange", "blue")
#mgps = c(3,1,0)
#xlabs <- ("") #day
#labeldist = 2
titledist = 0.5
ylabs <- "% of input values"
xlab1 <- ""
pcex <- 1.5
pchs <- 16
ats <- c(0,12,24)
cols1=c("red", "pink", "blue", "orange", "purple", "brown", "navy")
mpgs <- c(2.2,0.5,0)


## GM
GM <- na.omit(dat[,c(8,4)])
# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
GM <- summarySE(GM, measurevar="CD33.15", groupvars=c("time", na.rm=TRUE))
names(GM)[names(GM) == 'CD33.15'] <- 'mean'

# Create Line Chart
plot(GM$mean ~ GM$time, type="l", axes=F, ylim=ylim1, xlim=xlims, ylab=ylabs, xlab=xlab1, mgp=mpgs)
title(main="GM", line = titledist)
points(GM$mean ~ GM$time, cex=pcex, pch=pchs)
axis(side=1, at=ats, labels=ats, mgp=c(3,0.5,0))
axis(side=2, las=2, mgp=c(3,0.6,0))
# Error bars:
segments(x0=GM$time, y0=GM$mean- GM$se, x1=GM$time, y1=GM$mean + GM$se, lwd = 1.5)
arrows(x0=GM$time, y0=GM$mean- GM$se, x1=GM$time, y1=GM$mean + GM$se,, lwd = 1.5, angle = 90, code = 3, length = 0.05)
box()



## Platelets
PLT <- na.omit(dat[,c(8,7)])
# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
PLT <- summarySE(PLT, measurevar="Platelets", groupvars=c("time", na.rm=TRUE))
names(PLT)[names(PLT) == 'Platelets'] <- 'mean'

# Create Line Chart
plot(PLT$mean ~ PLT$time, type="l", axes=F, ylim=ylim1, xlim=xlims, ylab=ylabs, xlab=xlab1, mgp=mpgs)
title(main="Platelets", line = titledist)
points(PLT$mean ~ PLT$time, cex=pcex, pch=pchs)
axis(side=1, at=ats, labels=ats, mgp=c(3,0.5,0))
axis(side=2, las=2, mgp=c(3,0.6,0))
# Error bars:
segments(x0=PLT$time, y0=PLT$mean- PLT$se, x1=PLT$time, y1=PLT$mean + PLT$se, lwd = 1.5)
arrows(x0=PLT$time, y0=PLT$mean- PLT$se, x1=PLT$time, y1=PLT$mean + PLT$se,, lwd = 1.5, angle = 90, code = 3, length = 0.05)
box()

dev.off()




pdf(file="./plots/fig2b.pdf", width=8/2.54, height=0.4+(4.5/2.54)) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(5.1,3.1,2.1,1.1), cex=0.7, mgp=c(2,0.5,0))


## Different Growth Factors ##
dat <- read.csv("week3.csv", header=T, check.names=F)
dat <- dat[, c(1,6:ncol(dat))]
dat <- dat[dat$Exp!='24h_timecourse' , ]
dat <- dat[dat$Exp!='7_12_day' , ]
dat <- dat[dat$Input!='SFM+5GF+serum' , ]
dat <- droplevels(dat)


## GM
GM <- na.omit(dat[,c(2,4)])
# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
GM <- summarySE(GM, measurevar="CD33.15", groupvars=c("Input", na.rm=TRUE))
names(GM)[names(GM) == 'CD33.15'] <- 'mean'

# Separate out control arm
GMcntl <- GM[GM$Input=="fresh" ,]
GM <- GM[GM$Input !="fresh" ,]
# Remove 5GF arm, as it is plotted elsewhere
GM <- GM[GM$Input !="5GF" ,]
GM <- droplevels(GM)
# Re-order factor levels:
GM$Input<-factor(GM$Input, levels=c("SFM", "Insulin(1/200)", "2%FBS", "IL3", "GCSF+IL6", "SF+FL", "SF+FL+TPO"))
GM <- GM[order(GM$Input), ]

# Make plot
bp <- barplot(rep(NA,7), axes=F, ylim=ylim2)#, ylab="% of input values", mgp=mpgs)#, col=cols)
#axis(side=1, at=bp, labels=levels(GM$Input), mgp=c(2,1,0))
labs <- levels(GM$Input)
text(cex=1, x=bp+0.8, y=-6, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="GM", line = titledist)
# Add control horizontal bar:
rect(-1, 100-GMcntl$se, 10, 100+GMcntl$se, col="gray", border="gray")
barplot(GM$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", mgp=mpgs, col=cols1)
abline(h=100, lty=3, lwd = 1.2)
# Add error bars:
segments(x0=bp, y0=GM$mean, x1=bp, y1=GM$mean + GM$se * 1, lwd = 1.5)
arrows(bp, GM$mean, bp,  GM$mean + GM$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
box()




## Platelets
PLT <- na.omit(dat[,c(2,7)])
# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
PLT <- summarySE(PLT, measurevar="Platelets", groupvars=c("Input", na.rm=TRUE))
names(PLT)[names(PLT) == 'Platelets'] <- 'mean'

# Hide GCSF+IL6, because only has 2 replicates
PLT[3, c(4:7)] <- rep(0, 4)

# Separate out control arm
PLTcntl <- PLT[PLT$Input=="fresh" ,]
PLT <- PLT[PLT$Input !="fresh" ,]
# Remove 5GF arm, as it is plotted elsewhere
PLT <- PLT[PLT$Input !="5GF" ,]
PLT <- droplevels(PLT)
# Re-order factor levels:
PLT$Input<-factor(PLT$Input, levels=c("SFM", "Insulin(1/200)", "2%FBS", "IL3", "GCSF+IL6", "SF+FL", "SF+FL+TPO"))
PLT <- PLT[order(PLT$Input), ]

# Make plot
bp <- barplot(rep(NA,7), axes=F, ylim=ylim2)#, ylab="% of input values", mgp=mpgs)#, col=cols)
#axis(side=1, at=bp, labels=levels(PLT$Input), mgp=c(2,1,0))
labs <- levels(PLT$Input)
text(cex=1, x=bp+0.8, y=-6, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="Platelets", line = titledist)
# Add control horizontal bar:
rect(-1, 100-PLTcntl$se, 10, 100+PLTcntl$se, col="gray", border="gray")
barplot(PLT$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", mgp=mpgs, col=cols1)
abline(h=100, lty=3, lwd = 1.2)
# Add error bars:
segments(x0=bp, y0=PLT$mean, x1=bp, y1=PLT$mean + PLT$se * 1, lwd = 1.5)
arrows(bp, PLT$mean, bp,  PLT$mean + PLT$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
box()

dev.off()