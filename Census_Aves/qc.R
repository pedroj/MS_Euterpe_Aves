#-------------------------------------------------------------------------
# Quantitative component
#-------------------------------------------------------------------------
# From the Clipboard (MacOSX)
qc<-read.table(pipe("pbpaste"),header=TRUE,dec=".",sep="\t")

qc.virola<-cbind(qc.virola,qc)

#------------------------------------------------------------------------
library(scatterplot3d) 
scatterplot3d(vis,frv,crd, pch=16, highlight.3d=TRUE,type="h", main="3D Scatterplot")
#------------------------------------------------------------------------
# All Subsets Regression
attach(qc)
library(leaps)
leaps<-regsubsets(crd~vis+frv+pdisp,data=qc,nbest=10)	#crd
leaps<-regsubsets(qc~vis+frv+pdisp,data=qc,nbest=10)	#qc
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")
#-------------------------------------------------------------------------
# Calculate Relative Importance for Each Predictor
library(relaimpo)
attach(qc)
# All three plant species pooled
calc.relimp(qc~vis+frv+pdisp,data=qc,
            type=c("lmg","last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples) 
boot<-boot.relimp(qc~vis+frv+pdisp,data=qc,b = 1000)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot)) # plot result

# Subsetting for each plant species
calc.relimp(qc~vis+frv+pdisp,data=qc[plant=="Euterpe",],
            type=c("lmg","last","first","pratt"),rela=TRUE)
calc.relimp(qc~vis+frv+pdisp,data=qc[plant=="Virola",],
            type=c("lmg","last","first","pratt"),rela=TRUE)
calc.relimp(qc~vis+frv+pdisp,data=qc[plant=="Sloanea",],
            type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot1<-boot.relimp(qc~vis+frv+pdisp,data=qc[plant=="Euterpe",],b = 1000)
boot2<-boot.relimp(qc~vis+frv+pdisp,data=qc[plant=="Virola",],b = 1000)
boot3<-boot.relimp(qc~vis+frv+pdisp,data=qc[plant=="Sloanea",],b = 1000)

par(mfrow=c(1,3))
booteval.relimp(boot) # print result
plot(booteval.relimp(boot1),main="Euterpe") # plot result
plot(booteval.relimp(boot2),main="Virola") # plot result
plot(booteval.relimp(boot3),main="Sloanea") # plot result
#-------------------------------------------------------------------------
attach(qc)
pairs(qc[plant=="Euterpe",][3:7], lower.panel=panel.smooth, 
upper.panel=panel.cor) 
par(oma=c(1,1,1,1),new=T,font=2,cex=0.5) 
mtext(outer=T,"Paired plots with correlations",side=3)


