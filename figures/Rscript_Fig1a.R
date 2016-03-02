# Scripts for In Vitro plots for Engraftment Defect paper
# Paul Miller, paulhmiller@gmail.com

setwd('C:/Users/paulm/CRC Paul/PROJECTS/engraft_defect/figures')

.pardefault <- par(no.readonly=T)
par(.pardefault)

#library(extrafont)
#font_import(pattern = 'calibri')
#loadfonts()

dat <- read.csv("inVitro.csv", header=T,check.names=F)
cd34 <- dat[dat$Assay=='CD34' , ]
cfc <- dat[dat$Assay=='CFC' , ]


pdf(file="./plots/fig1a.pdf", width=8/2.54, height=0.6+(4.5/2.54)) #, family='Calibri')


## Global Settings

par(mfrow=c(1,2), mar=c(5.1,4.1,2.1,1.1), cex=0.7, mgp=c(2.3,0.6,0))
xlabels <- c('fresh', 'day 7')
cols <- c('green', 'green')
pchs <- c(1, 16)
mgp1 <- c(2,0.6,0) #c(3,1,0)
xlabs <- ("") #day
labeldist = 2
titledist = 0.5
pcex = 1.5
xlims <- c(0.9,2.1)



## CD34 Plot
dat <- cd34

#Adjust values (i.e. jitter) so that are visible:
dat[1,5] <- dat[1,5] +15
dat[3,5] <- dat[3,5] -15

plot(1, type="n", axes=F, ylim=c(0,1200), xlim=xlims, ylab="cells per 100 initial CD34 cells", xlab="")
title(xlab=xlabs, line=labeldist, cex.lab=1)
#mtext("label one \n label number two" , side=2, line=3)  #CD34 cells per 100 /ninitial CD34 cells
title(main="CD34", line = titledist)
axis(side=1, at=c(1,2), labels=xlabels, mgp=c(2,1,0))
axis(side=2, las=2, mgp=mgp1)
for (i in 1:4){
  tmp <- c((dat[i,4]),(dat[i,5]))
  lines(tmp)
}
for (i in 1:4){
  tmp <- (dat[i,4])
  points(1, tmp, col=cols[1], pch=pchs[1], cex=pcex)
  tmp <- (dat[i,5])
  points(2, tmp, col=cols[2], pch=pchs[2], cex=pcex)
}
#text(x = bp, y = PLT$mean+PLT$se+pdist, labels =PLT$star, cex=0.7) # add significance stars
box()


## CFC Plots

dat <- cfc

plot(1, type="n", axes=F, ylim=c(0,700), xlim=xlims, ylab="cells per 100 initial CD34 cells", xlab="")
title(xlab=xlabs, line=labeldist, cex.lab=1)
title(main="CFC", line = titledist)
axis(side=1, at=c(1,2), labels=xlabels)
axis(side=2, las=2, mgp=mgp1)
for (i in 1:4){
  tmp <- c((dat[i,4]),(dat[i,5]))
  lines(tmp)
}
for (i in 1:4){
  tmp <- (dat[i,4])
  points(1, tmp, col=cols[1], pch=pchs[1], cex=pcex)
  tmp <- (dat[i,5])
  points(2, tmp, col=cols[2], pch=pchs[2], cex=pcex)
}
#text(x = bp, y = PLT$mean+PLT$se+pdist, labels =PLT$star, cex=0.7) # add significance stars
box()


dev.off()
  