# Scripts for In Vivo figure 1 plots for Engraftment Defect paper
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
BIGdat <- dat

pdf(file="./plots/fig1b.pdf", width=8/2.54, height=0.6+(4.5/2.54)) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(5.6,4.1,1.6,1.1), cex=0.7, mgp=c(2.3,0.6,0))
ylim1 <- c(0,115)
cols <- c("red", "purple", "orange", "blue")
mgp1 <- c(2,0.6,0) #c(3,1,0)
#xlabs <- ("") #day
#labeldist = 2
titledist = 0.5
pdist <- 3.4


## GM

dat <- na.omit(BIGdat[,c(2,4)])

# Statistics
# T.Test
datS2 <- NULL
for (i in levels(dat$Input)){
	tmp <- (t.test(dat[dat$Input=='fresh', 2], dat[dat$Input==i, 2],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	datS2 <- rbind(datS2, tmp)
	}	
datS2 <- data.frame(datS2)
datS2[, 2] <- as.numeric(levels(datS2[,2]))[datS2[,2]]

# Add columns for asterisks. (. = p ≤ 0.10; * = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(datS2)){
    if (datS2[i, 2] <= 0.001){
        datS2[i ,3] <- "***"
	} else if (datS2[i, 2] <= 0.01){
	    datS2[i ,3] <- "**"
	} else if (datS2[i, 2] <= 0.05){
	    datS2[i ,3] <- "*"
	} else if (datS2[i, 2] <= 0.10){
	    datS2[i ,3] <- "."
	} else{
	    datS2[i ,3] <- ""
	}
}
colnames(datS2) <- c("Input", "p.value", "star")		

datS <- summarySE(dat, measurevar="CD33.15", groupvars=c("Input", na.rm=TRUE))
# summarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
names(datS)[names(datS) == 'CD33.15'] <- 'mean'

GM <- merge(datS, data.frame(datS2), by = "Input")

# Separate out control arm
GMcntl <- GM[GM$Input=="fresh" ,]
GM <- GM[GM$Input !="fresh" ,]
GM <- droplevels(GM)
# Re-order factor levels:
GM$Input<-factor(GM$Input, levels=c("5GF", "5GF+PGE2+DPA", "3GF", "3GF+UM171"))
GM <- GM[order(GM$Input), ]

# Make plot
bp <- barplot(GM$mean, axes=F, ylim=ylim1, ylab="% of input values", col=cols)
axis(side=1, at=bp, labels=rep("",4), mgp=c(2,1,0))  # makes tick marks
axis(side=2, las=2, mgp=mgp1)
labs <- levels(GM$Input)
text(cex=1, x=bp+0.6, y=-12, labs, xpd=TRUE, srt=45, pos=2)
title(main="GM", line = titledist)
rect(0, 100-GMcntl$se, 5, 100+GMcntl$se, col="gray", border="gray") # add control horizontal bar
abline(h=100, lty=3, lwd = 1.2)
arrows(bp, GM$mean, bp,  GM$mean + GM$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05) # add error bars
text(x = bp, y = GM$mean+GM$se+pdist, labels =GM$star, cex=0.7) # add significance stars
box()




## Platelets

dat <- na.omit(BIGdat[,c(2,7)])

# Statistics
# T.Test
datS2 <- NULL
for (i in levels(dat$Input)){
	tmp <- (t.test(dat[dat$Input=='fresh', 2], dat[dat$Input==i, 2],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	datS2 <- rbind(datS2, tmp)
	}	
datS2 <- data.frame(datS2)
datS2[, 2] <- as.numeric(levels(datS2[,2]))[datS2[,2]]

# Add columns for asterisks. (. = p ≤ 0.10; * = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(datS2)){
    if (datS2[i, 2] <= 0.001){
        datS2[i ,3] <- "***"
	} else if (datS2[i, 2] <= 0.01){
	    datS2[i ,3] <- "**"
	} else if (datS2[i, 2] <= 0.05){
	    datS2[i ,3] <- "*"
	} else if (datS2[i, 2] <= 0.10){
	    datS2[i ,3] <- "."
	} else{
	    datS2[i ,3] <- ""
	}
}
colnames(datS2) <- c("Input", "p.value", "star")		

# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
datS <- summarySE(dat, measurevar="Platelets", groupvars=c("Input", na.rm=TRUE))
names(datS)[names(datS) == 'Platelets'] <- 'mean'

PLT <- merge(datS, data.frame(datS2), by = "Input")

# Re-order factor levels:
PLT$Input<-factor(PLT$Input, levels=c("5GF", "5GF+PGE2+DPA", "3GF", "3GF+UM171", "fresh"))
PLT <- PLT[order(PLT$Input), ]

# Separate out control arm
PLTcntl <- PLT[PLT$Input=="fresh" ,]
PLT <- PLT[PLT$Input !="fresh" ,]
PLT <- droplevels(PLT)

# Make plot
bp <- barplot(rep(NA,4), axes=F, ylim=ylim1)
axis(side=1, at=bp, labels=rep("",4), mgp=c(2,1,0))  # makes tick marks
axis(side=2, las=2, mgp=mgp1)
barplot(PLT$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", col=cols)
abline(h=18, lty=3, lwd=1)
labs <- levels(PLT$Input)
text(cex=1, x=bp+0.6, y=-12, labs, xpd=TRUE, srt=45, pos=2)
title(main="Platelets", line = titledist)
rect(0, 100-PLTcntl$se, 5, 100+PLTcntl$se, col="gray", border="gray") # add control horizontal bar
abline(h=100, lty=3, lwd=1.2)
arrows(bp, PLT$mean, bp,  PLT$mean + PLT$se * 1, lwd = 1.2, angle = 90, code = 3, length = 0.05) # add error bars
text(x = bp, y = PLT$mean+PLT$se+pdist, labels =PLT$star, cex=0.7) # add significance stars
box()

dev.off()


