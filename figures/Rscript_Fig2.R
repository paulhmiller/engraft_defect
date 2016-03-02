# Scripts for Engraftment Defect paper, figure 2 plots
# Paul Miller, paulhmiller@gmail.com

setwd('C:/Users/paulm/CRC Paul/PROJECTS/engraft_defect/figures')

.pardefault <- par(no.readonly=T)
par(.pardefault)
dev.off()

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
BIGdat <- dat

pdf(file="./plots/fig2a.pdf", width=8/2.54, height=0.6+(4.5/2.54)) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(5.1,3.1,2.1,1.1), cex=0.7, mgp=c(2,0.6,0))
ylim1 <- c(0,115)
ylim2 <- c(0,138)
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
pdist <- 3.4


## GM
dat <- na.omit(BIGdat[,c(8,4)])

# Statistics
# T.Test
datS2 <- NULL
for (i in c(0,4,24)){
    tmp <- (t.test(dat[dat$time==0, 2], dat[dat$time==i, 2],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	datS2 <- rbind(datS2, tmp)
	}	
	
datS2 <- data.frame(datS2)
#datS2[, 2] <- as.numeric(levels(datS2[,2]))[datS2[,2]]

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
colnames(datS2) <- c("time", "p.value", "star")		

# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
datS <- summarySE(dat, measurevar="CD33.15", groupvars=c("time", na.rm=TRUE))
names(datS)[names(datS) == 'CD33.15'] <- 'mean'

GM <- merge(datS, data.frame(datS2), by = "time")

# Create Line Chart
plot(GM$mean ~ GM$time, type="l", axes=F, ylim=ylim1, xlim=xlims, ylab=ylabs, xlab=xlab1, mgp=mpgs)
title(main="GM", line = titledist)
points(GM$mean ~ GM$time, cex=pcex, pch=pchs)
axis(side=1, at=ats, labels=ats, mgp=c(3,0.5,0))
axis(side=2, las=2, mgp=c(3,0.6,0))
# Error bars:
segments(x0=GM$time, y0=GM$mean- GM$se, x1=GM$time, y1=GM$mean + GM$se, lwd = 1.5)
arrows(x0=GM$time, y0=GM$mean- GM$se, x1=GM$time, y1=GM$mean + GM$se,, lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=GM$time, y=GM$mean+GM$se+pdist, labels=GM$star, cex=0.7) 
box()



## Platelets
dat <- na.omit(BIGdat[,c(8,7)])


# Statistics
# T.Test
datS2 <- NULL
for (i in c(0,4,24)){
    tmp <- (t.test(dat[dat$time==0, 2], dat[dat$time==i, 2],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	datS2 <- rbind(datS2, tmp)
	}	
datS2 <- data.frame(datS2)
#datS2[, 2] <- as.numeric(levels(datS2[,2]))[datS2[,2]]

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
colnames(datS2) <- c("time", "p.value", "star")		

# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
datS <- summarySE(dat, measurevar="Platelets", groupvars=c("time", na.rm=TRUE))
names(datS)[names(datS) == 'Platelets'] <- 'mean'

PLT <- merge(datS, data.frame(datS2), by = "time")

# Create Line Chart
plot(PLT$mean ~ PLT$time, type="l", axes=F, ylim=ylim1, xlim=xlims, ylab=ylabs, xlab=xlab1, mgp=mpgs)
title(main="Platelets", line = titledist)
points(PLT$mean ~ PLT$time, cex=pcex, pch=pchs)
axis(side=1, at=ats, labels=ats, mgp=c(3,0.5,0))
axis(side=2, las=2, mgp=c(3,0.6,0))
# Error bars:
segments(x0=PLT$time, y0=PLT$mean- PLT$se, x1=PLT$time, y1=PLT$mean + PLT$se, lwd = 1.5)
arrows(x0=PLT$time, y0=PLT$mean- PLT$se, x1=PLT$time, y1=PLT$mean + PLT$se,, lwd = 1.5, angle = 90, code = 3, length = 0.05)
text(x=PLT$time, y = PLT$mean+PLT$se+pdist, labels=PLT$star , cex=0.7) 
box()

dev.off()












## Figure 2b ##

pdf(file="./plots/fig2b.pdf", width=8/2.54, height=0.6+(4.5/2.54)) #, family='Calibri')

## Global Settings
par(mfrow=c(1,2), mar=c(5.6,3.1,1.6,1.1), cex=0.7, mgp=c(2,0.6,0))


## Different Growth Factors ##
dat <- read.csv("week3.csv", header=T, check.names=F)
dat <- dat[, c(1,6:ncol(dat))]
dat <- dat[dat$Exp!='24h_timecourse' , ]
dat <- dat[dat$Exp!='7_12_day' , ]
dat <- dat[dat$Input!='SFM+5GF+serum' , ]
dat <- droplevels(dat)
levels(dat$Input)[levels(dat$Input)=="SF+FL"] <- "SCF+FLT3L"
levels(dat$Input)[levels(dat$Input)=="SF+FL+TPO"] <- "SCF+FLT3L+TPO"
levels(dat$Input)[levels(dat$Input)=="SFM"] <- "SFM only"


## GM
GM <- na.omit(dat[,c(2,4)])

# Statistics
# T.Test
GMstats <- NULL
for (i in levels(GM$Input)){
	tmp <- (t.test(GM[GM$Input=='fresh', 2], GM[GM$Input==i, 2],var.equal = FALSE))
	tmp <- c(i, tmp$p.value)
	GMstats <- rbind(GMstats, tmp)
	}	
GMstats <- data.frame(GMstats)
GMstats[, 2] <- as.numeric(levels(GMstats[,2]))[GMstats[,2]]

# Add columns for asterisks. (* = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(GMstats)){
    if (GMstats[i, 2] <= 0.001){
        GMstats[i ,3] <- "***"
	} else if(GMstats[i, 2] <= 0.01){
	    GMstats[i ,3] <- "**"
	} else if(GMstats[i, 2] <= 0.05){
	    GMstats[i ,3] <- "*"
	} else if(GMstats[i, 2] <= 0.10){
	    GMstats[i ,3] <- "."
	} else{
	    GMstats[i ,3] <- ""
	}
}
colnames(GMstats) <- c("Input", "p.value", "star")		
	    

# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
GM <- summarySE(GM, measurevar="CD33.15", groupvars=c("Input", na.rm=TRUE))
names(GM)[names(GM) == 'CD33.15'] <- 'mean'

GM <- merge(GM, data.frame(GMstats), by = "Input")


# Separate out control arm
GMcntl <- GM[GM$Input=="fresh" ,]
GM <- GM[GM$Input !="fresh" ,]
# Remove 5GF arm, as it is plotted elsewhere
GM <- GM[GM$Input !="5GF" ,]
GM <- droplevels(GM)
# Re-order factor levels:
#GM$Input <- factor(GM$Input, levels=c("SFM", "Insulin(1/200)", "2%FBS", "IL3", "GCSF+IL6", "SF+FL", "SF+FL+TPO"))
GM$Input <- factor(GM$Input, levels=GM$Input[c(7, 4, 1, 3, 2, 5, 6)])
GM <- GM[order(GM$Input), ]

# Make plot
bp <- barplot(rep(NA,7), axes=F, ylim=ylim2)#, ylab="% of input values", mgp=mpgs)#, col=cols)
axis(side=1, at=bp, labels=rep(NA,7), mgp=c(2,1,0))
labs <- levels(GM$Input)
text(cex=1, x=bp+1.0, y=-12, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="GM", line = titledist)
# Add control horizontal bar:
rect(-1, 100-GMcntl$se, 10, 100+GMcntl$se, col="gray", border="gray")
barplot(GM$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", mgp=mpgs, col=cols1)
abline(h=100, lty=3, lwd = 1.2)
# Add error bars:
#segments(x0=bp, y0=GM$mean, x1=bp, y1=GM$mean + GM$se * 1, lwd = 1.5)
arrows(bp, GM$mean, bp,  GM$mean + GM$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
text(x = bp, y = GM$mean+GM$se+pdist, labels=GM$star , cex=0.7) #paste("p=",round(PLT$p.value,2))
box()






## Function
PLT <- na.omit(dat[,c(2,7)])

pvalStars <- function(condition, cntl, colnum, varequal=FALSE) {
    """ 
	Give a column of condition (i.e. independent variables), name of the control 
	condition within the dependent list, number of column containing, whether
	variables have equal variance. 
	It will add a column for asterisks. 
	(. = p ≤ 0.10; * = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
	"""
    stats <- NULL
	for (i in levels(conditionCol)){
	tmp <- (t.test(conditionCol==cntl, conditionCol==i, var.equal=varequal))
	print(c(i,tmp$p.value))
	tmp <- c(i, tmp$p.value)
	stats <- rbind(stats, tmp)
	}	
#
for (i in 1:nrow(PLTstats)){
    if (PLTstats[i, 2] <= 0.001){
        PLTstats[i ,3] <- "***"
	} else if (PLTstats[i, 2] <= 0.01){
	    PLTstats[i ,3] <- "**"
	} else if (PLTstats[i, 2] <= 0.05){
	    PLTstats[i ,3] <- "*"
	} else if (PLTstats[i, 2] <= 0.10){
	    PLTstats[i ,3] <- "."
	} else{
	    PLTstats[i ,3] <- ""
	}
}	
	
# Statistics
# T.Test



PLTstats <- NULL
for (i in levels(PLT$Input)){
	tmp <- (t.test(PLT[PLT$Input=='fresh', 2], PLT[PLT$Input==i, 2],var.equal = FALSE))
	print(c(i,tmp$p.value))
	tmp <- c(i, tmp$p.value)
	PLTstats <- rbind(PLTstats, tmp)
	}	
PLTstats <- data.frame(PLTstats)
PLTstats[, 2] <- as.numeric(levels(PLTstats[,2]))[PLTstats[,2]]

# Add columns for asterisks. (* = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(PLTstats)){
    if (PLTstats[i, 2] <= 0.001){
        PLTstats[i ,3] <- "***"
	} else if (PLTstats[i, 2] <= 0.01){
	    PLTstats[i ,3] <- "**"
	} else if (PLTstats[i, 2] <= 0.05){
	    PLTstats[i ,3] <- "*"
	} else if (PLTstats[i, 2] <= 0.10){
	    PLTstats[i ,3] <- "."
	} else{
	    PLTstats[i ,3] <- ""
	}
}
colnames(PLTstats) <- c("Input", "p.value", "star")	




## Platelets

PLT <- na.omit(dat[,c(2,7)])


# Statistics
# T.Test
PLTstats <- NULL
for (i in levels(PLT$Input)){
	tmp <- (t.test(PLT[PLT$Input=='fresh', 2], PLT[PLT$Input==i, 2],var.equal = FALSE))
	print(c(i,tmp$p.value))
	tmp <- c(i, tmp$p.value)
	PLTstats <- rbind(PLTstats, tmp)
	}	
PLTstats <- data.frame(PLTstats)
PLTstats[, 2] <- as.numeric(levels(PLTstats[,2]))[PLTstats[,2]]

# Add columns for asterisks. (* = p ≤ 0.05; ** = p ≤ 0.01; *** = p ≤ 0.001)
for (i in 1:nrow(PLTstats)){
    if (PLTstats[i, 2] <= 0.001){
        PLTstats[i ,3] <- "***"
	} else if (PLTstats[i, 2] <= 0.01){
	    PLTstats[i ,3] <- "**"
	} else if (PLTstats[i, 2] <= 0.05){
	    PLTstats[i ,3] <- "*"
	} else if (PLTstats[i, 2] <= 0.10){
	    PLTstats[i ,3] <- "."
	} else{
	    PLTstats[i ,3] <- ""
	}
}
colnames(PLTstats) <- c("Input", "p.value", "star")		
PLTstats
	    
# SummarySE provides std, SEM, and (default 95%) CI. #measurevar is the x-axis.
PLT <- summarySE(PLT, measurevar="Platelets", groupvars=c("Input", na.rm=TRUE))
names(PLT)[names(PLT) == 'Platelets'] <- 'mean'

PLT <- merge(PLT, data.frame(PLTstats), by = "Input")


# Hide GCSF+IL6, because only has 2 replicates
PLT[4, c(4:8)] <- rep(0, 5)
PLT[4, 9] <- "ND" #Change significance star to Not Done

# Separate out control arm
PLTcntl <- PLT[PLT$Input=="fresh" ,]
PLT <- PLT[PLT$Input !="fresh" ,]
# Remove 5GF arm, as it is plotted elsewhere
PLT <- PLT[PLT$Input !="5GF" ,]
PLT <- droplevels(PLT)
# Re-order factor levels:
#PLT$Input<-factor(PLT$Input, levels=c("SFM", "Insulin(1/200)", "2%FBS", "IL3", "GCSF+IL6", "SF+FL", "SF+FL+TPO"))
PLT$Input <- factor(PLT$Input, levels=PLT$Input[c(7, 4, 1, 3, 2, 5, 6)])
PLT <- PLT[order(PLT$Input), ]



# Make plot
bp <- barplot(rep(NA,7), axes=F, ylim=ylim2)#, ylab="% of input values", mgp=mpgs)#, col=cols)
axis(side=1, at=bp, labels=rep(NA,7), mgp=c(2,1,0))  # Axis ticks
labs <- levels(PLT$Input)
text(cex=1, x=bp+1.0, y=-12, labs, xpd=TRUE, srt=45, pos=2)
axis(side=2, las=2)
title(main="Platelets", line = titledist)
# Add control horizontal bar:
rect(-1, 100-PLTcntl$se, 10, 100+PLTcntl$se, col="gray", border="gray")
barplot(PLT$mean, add=T, axes=F, ylim=ylim1, ylab="% of input values", mgp=mpgs, col=cols1)
abline(h=100, lty=3, lwd = 1.2)
# Add error bars:
#segments(x0=bp, y0=PLT$mean, x1=bp, y1=PLT$mean + PLT$se * 1, lwd = 1.5)
arrows(bp, PLT$mean, bp,  PLT$mean + PLT$se * 1, lwd = 1.5, angle = 90,
       code = 3, length = 0.05)
text(x = bp, y = PLT$mean+PLT$se+pdist, labels =PLT$star , cex=0.7) 
box()

dev.off()



