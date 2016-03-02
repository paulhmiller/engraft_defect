# Scripts for Engraftment Defect paper, figure 3 plots
# Paul Miller, paulhmiller@gmail.com

setwd('C:/Users/paulm/CRC Paul/PROJECTS/engraft_defect/figures')

.pardefault <- par(no.readonly=T)
par(.pardefault)


source("C:/Users/paulm/Documents/R/source/functions.R")
#library(extrafont)
#font_import(pattern = 'calibri')
#loadfonts()




## CFC Homing ##
dat <- read.csv("homing.csv", header=T, check.names=F)
dat <- dat[, c(3,6:ncol(dat))]
#dat <- droplevels(dat)



pdf(file="./plots/fig3.pdf", width=8/2.54, height=(0.6+(4.5/2.54))) #, family='Calibri')


## Global Settings
par(mfrow=c(1,2), mar=c(5.1,3.1,2.1,1.1), cex=0.7, mgp=c(2,0.6,0))
ylim1 <- c(0,115)
ylim2 <- c(0,135)
xlims <- c(0.6,3.4)
col1 <- c(rep("red",3), rep("orange",6), rep("blue",3))
col2 <- c(rep("red",4), rep("orange",0), rep("blue",3))
col3 <- c("red", "orange", "blue")
#mgps = c(3,1,0)
#xlabs <- ("") #day
#labeldist = 2
titledist = 0.5
ylab1 <- "% of input values"
xlab1 <- ("")
xlab2 <- c("5GF", "3GF", "2%FBS")
pcex <- 1.1
pch1 <- 1
pch2 <- 16
at1 <- c(0,12,24)
mpg1 <- c(2.2,0.5,0)
lwd1 <- 0.4
pdist <- 3.4


## CFC Homing
cfc <- na.omit(dat)
# Re-order factor levels:
cfc$Input<-factor(cfc$Input, levels=c("5GF", "3GF", "2%FBS", "fresh"))
cfc <- cfc[order(cfc$Input), ]



# Statistics
# T.Test
cfcstats <- NULL
for (i in levels(cfc$Input)){
	tmp <- (t.test(cfc[cfc$Input=='fresh', 3], cfc[cfc$Input==i, 3],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	cfcstats <- rbind(cfcstats, tmp)
	}	
cfcstats <- data.frame(cfcstats)
cfcstats[, 2] <- as.numeric(levels(cfcstats[,2]))[cfcstats[,2]]

# Add columns for asterisks. (* = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(cfcstats)){
    if (cfcstats[i, 2] <= 0.001){
        cfcstats[i ,3] <- "***"
	} else if (cfcstats[i, 2] <= 0.01){
	    cfcstats[i ,3] <- "**"
	} else if (cfcstats[i, 2] <= 0.05){
	    cfcstats[i ,3] <- "*"
	} else if (cfcstats[i, 2] <= 0.10){
	    cfcstats[i ,3] <- "."
	} else{
	    cfcstats[i ,3] <- ""
	}
}
colnames(cfcstats) <- c("Input", "p.value", "star")		


# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
cfcS <- summarySE(cfc, measurevar="percent", groupvars=c("Input", na.rm=TRUE))
names(cfcS)[names(cfcS) == 'percent'] <- 'mean'

cfcS <- merge(cfcS, data.frame(cfcstats), by = "Input")

cfcS$Input<-factor(cfcS$Input, levels=c("5GF", "3GF", "2%FBS", "fresh"))
cfcS <- cfcS[order(cfcS$Input), ]

# Separate out arms
cfcScntl <- cfcS[cfcS$Input=="fresh" ,]
cfcS <- cfcS[cfcS$Input !="fresh" ,]
cfccntl <- cfc[cfc$Input=="fresh" ,]
cfc <- cfc[cfc$Input !="fresh" ,]
cfcS <- droplevels(cfcS)
cfc <- droplevels(cfc)
cfc <- cfc[cfc$Input !="fresh" ,]
cfcNSG <- cfc[cfc$Strain=="NSG" ,]
cfc3GS <- cfc[cfc$Strain=="NSG-3GS" ,]



# Create Chart
plot(seq(1:3), type="n", axes=F, xlim=xlims, ylim=ylim1, ylab=ylab1, xlab=xlab1, mgp=mpg1) 
#cfcS$mean ~ cfcS$Input, 
title(main="", line = titledist)
rect(-1, 100-cfcScntl$se, 10, 100+cfcScntl$se, col="gray", border="gray")
points(cfcNSG$percent ~ jitter(as.numeric(cfcNSG$Input),factor=0.5), cex=pcex, pch=pch1, col=col1) 
points(cfc3GS$percent ~ jitter(as.numeric(cfc3GS$Input),factor=0.5), cex=pcex, pch=pch2, col=col2) 
#axis(side=1, at=c(1:3), labels=rep("",3), mgp=c(3,0.5,0)) # makes tick marks
text(cex=1, x=c(1:3)+0.3, y=-15, labels=xlab2, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2, mgp=c(3,0.6,0))
axis(side=1, at=seq(1:3), labels=rep(NA,3), mgp=c(2,1,0))  # Axis ticks
# Error bars:
segments(x0=c(1:3)-0.4, y0=cfcS$mean, x1=c(1:3)+0.4, y1=cfcS$mean, lwd = 1.5, col=col3)
#segments(x0=c(1:3), y0=cfcS$mean-cfcS$se, x1=c(1:3), y1=cfcS$mean+cfcS$se, lwd = 1.5, col=col3)
arrows(x0=c(1:3), y0=cfcS$mean- cfcS$se, x1=c(1:3), y1=cfcS$mean + cfcS$se, lwd = 1.5, angle = 90, code = 3, length = 0.05, col=col3)
abline(h=100, lty=3, lwd = 1.2) 
text(x = c(1:3), y = cfcS$mean+cfcS$se+pdist, labels =cfcS$star, cex=0.7) #paste("p=",round(cfcstats$p.value,2))
box()



dev.off()


