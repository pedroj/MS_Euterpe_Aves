##############################################################################
### Spatial-explicit models for seed traps.
# Pedro. Zahara. 11Jul2009. Piracicaba. 13Mar2010
##############################################################################
library(spatstat)
library(ecespa)
# Data mestrado e doutorado (but with no canopy light data).
all<-read.table("all.txt",header=TRUE,dec=".",sep="\t")

# Data now corrected for 1m2 sampling area.

dout<-read.table("doutourado.txt",header=TRUE,dec=".",sep="\t")
mest<-read.table("mestrado.txt",header=TRUE,dec=".",sep="\t")

# Oct 2010 corrected datastes
dout2<-read.table("doutourado_sep2010.txt",header=TRUE,dec=".",sep="\t")
mest2<-read.table("mestrado_sep2010.txt",header=TRUE,dec=".",sep="\t")
names(dout2)

R> names(all)
 [1] "col_parc"  "period"    "plot"      "site"     
 [5] "x"         "y"         "seeds"     "seedsp"   
 [9] "euterpe"   "sloanea"   "virola"    "seedl"    
[13] "seedlsp"   "seedl_NI"  "seedlsurv" "eut.seedl"
[17] "slo.seedl" "vir.seedl" "nivel"     "pH_H2O"   
[21] "pH_KCl"    "pH_Cacl2"  "MO"        "P"        
[25] "Na"        "K"         "Ca"        "Mg"       
[29] "Al"        "H.Al"      "SB"        "T"        
[33] "Vpct"      "mpct"      "vcs"       "cs"       
[37] "ms"        "fs"        "vfs"       "sandpct"  
[41] "siltpct"   "claypct"   "natclay"   "dens_gcm3"
[45] "phi_r"     "phi_s"     "alfa"      "m"        
[49] "n"         "S"        

# Seed traps and plots for doutorado: 0.5 x 0.5 m= 0.25 m2.
# For mestrado: 1 x 1 m= 1 m2.

R> names(dout)
 [1] "col_parc"  "period"    "plot"      "site"     
 [5] "x"         "y"         "seeds"     "seedsp"   
 [9] "euterpe"   "sloanea"   "virola"    "seedl"    
[13] "seedlsp"   "seedl_NI"  "seedlsurv" "eut.seedl"
[17] "slo.seedl" "vir.seedl" "nivel"     "photo"    
[21] "vissky"    "isf"       "isfu"      "dsf"      
[25] "dsfu"      "gsf"       "gsfu"      "difbe"    
[29] "difbeu"    "dirbe"     "dirbeu"    "totbe"    
[33] "totbeu"    "lai"       "eladp"     "mla"      
[37] "gndcover"  "laidev"    "pH_H2O"    "pH_KCl"   
[41] "pH_Cacl2"  "MO"        "P"         "Na"       
[45] "K"         "Ca"        "Mg"        "Al"       
[49] "H.Al"      "SB"        "T"         "Vpct"     
[53] "mpct"      "vcs"       "cs"        "ms"       
[57] "fs"        "vfs"       "sandpct"   "siltpct"  
[61] "claypct"   "natclay"   "dens_gcm3" "phi_r"    
[65] "phi_s"     "alfa"      "m"         "n"        
[69] "S"         

R> names(mest)
 [1] "col_parc"  "period"    "plot"      "site"     
 [5] "x"         "y"         "seeds"     "seedsp"   
 [9] "euterpe"   "sloanea"   "virola"    "seedl"    
[13] "seedlsp"   "seedlsurv" "eut.seedl" "slo.seedl"
[17] "vir.seedl" "nivel"     "pH_H2O"    "pH_KCl"   
[21] "pH_Cacl2"  "MO"        "P"         "Na"       
[25] "K"         "Ca"        "Mg"        "Al"       
[29] "H.Al"      "SB"        "T"         "Vpct"     
[33] "mpct"      "vcs"       "cs"        "ms"       
[37] "fs"        "vfs"       "sandpct"   "siltpct"  
[41] "claypct"   "natclay"   "dens_gcm3" "phi_r"    
[45] "phi_s"     "alfa"      "m"         "n"        
[49] "S"        

#-----------------------------------------------------------------------------
# Datasets for spatial analyses.
all1<- all[,c(5,6,7,8,12,13,15,4)]
dout22<- dout2[,c(5,6,7,8,12,13,15,4)]
mest22<- mest2[,c(5,6,7,8,12,13,14,4)]

R> str(all1)
'data.frame':	141 obs. of  8 variables:
 $ x        : num  202940 202975 202989 203039 203040 ...
 $ y        : num  7322590 7322564 7322592 7322587 7322587 ...
 $ seeds    : int  196 2844 648 940 2300 12252 708 304 352 792 ...
 $ seedsp   : int  11 15 12 6 9 3 5 4 3 4 ...
 $ seedl    : int  20 60 148 76 40 40 44 36 8 24 ...
 $ seedlsp  : int  4 9 10 7 6 5 8 4 2 4 ...
 $ seedlsurv: int  20 60 140 48 24 40 28 20 4 12 ...
 $ site     : Factor w/ 2 levels "B","NB": 2 2 2 1 1 1 1 1 1 1 ...

R> str(dout1)
'data.frame':	61 obs. of  8 variables:
 $ x        : num  202940 202975 202989 203039 203040 ...
 $ y        : num  7322590 7322564 7322592 7322587 7322587 ...
 $ seeds    : int  49 711 162 235 575 3063 177 76 88 198 ...
 $ seedsp   : int  11 15 12 6 9 3 5 4 3 4 ...
 $ seedl    : int  5 15 37 19 10 10 11 9 2 6 ...
 $ seedlsp  : int  4 9 10 7 6 5 8 4 2 4 ...
 $ seedlsurv: int  5 15 35 12 6 10 7 5 1 3 ...
 $ site     : Factor w/ 2 levels "B","NB": 2 2 2 1 1 1 1 1 1 1 ...

R> str(mest1)
'data.frame':	80 obs. of  8 variables:
 $ x        : num  202821 202868 202863 202885 202900 ...
 $ y        : num  7322584 7322608 7322533 7322531 7322558 ...
 $ seeds    : int  48 48 43 250 347 21 36 34 121 322 ...
 $ seedsp   : int  11 11 11 21 21 10 8 9 15 12 ...
 $ seedl    : int  8 2 2 2 12 10 15 5 15 19 ...
 $ seedlsp  : int  3 2 1 2 9 8 5 1 3 2 ...
 $ seedlsurv: int  2 1 2 1 12 7 12 3 14 13 ...
 $ site     : Factor w/ 2 levels "B","NB": 1 2 2 2 2 2 1 1 1 1 ...

#-----------------------------------------------------------------------------
# Maps and plots
par(mfrow=c(1,2))
plot(haz.ppp(mest22[,c(1,2,3)]),main="Number of seeds")
plot(haz.ppp(mest22[,c(1,2,5)]),main="Number of seedlings")

par(mfrow=c(1,2))
plot(haz.ppp(dout22[,c(1,2,3)]),main="Number of seeds")
plot(haz.ppp(dout22[,c(1,2,5)]),main="Number of seedlings")

# Transparency
par(mfrow=c(2,2)) # Two plots, for seeds and seedlings
par(mfrow=c(1,2)) # Two plots, for seeds and seedlings

#### ALL
# Seed and seedlings
par(mfrow=c(1,2))
plot(estacas1,cex=0.2,main="No. seeds/m2")
plot(haz.ppp(all1[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(all1[,c(1,2,3)]), bg= c(rgb(0, 0, 1, 0.3),rgb(0, 1, 0, 0.3)), lty=0, markscale=0.008, add=T)

plot(estacas1,cex=0.2,main="No. seedlings/m2")
plot(haz.ppp(mest1[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(mest1[,c(1,2,5)]), bg= c(rgb(0, 0, 1, 0.3),rgb(0, 1, 0, 0.3)), lty=0, markscale=0.9, add=T)

#### MESTRADO
# Seed and seedling species richness
par(mfrow=c(2,2))
plot(estacas1,cex=0.2,main="No. species seeds")
plot(haz.ppp(mest22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(mest22[,c(1,2,4)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=1.8, add=T)

plot(estacas1,cex=0.2,main="No. species seedlings")
plot(haz.ppp(mest22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(mest22[,c(1,2,6)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=2.5, add=T)

# Seed and seedling numbers
#par(mfrow=c(1,2))
plot(estacas1,cex=0.2, main="Number of seeds")
plot(haz.ppp(mest22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(mest22[,c(1,2,3)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=0.02, add=T)
plot(estacas1,cex=0.2, main="Number of seedlings")
plot(haz.ppp(mest22[c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(mest22[,c(1,2,5)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=0.8, add=T)

#### DOUTORADO
# Seed and seedling species richness
par(mfrow=c(2,2))
plot(estacas1,cex=0.2,main="No. species seeds")
plot(haz.ppp(dout22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(dout22[,c(1,2,4)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=1.9, add=T)

plot(estacas1,cex=0.2,main="No. species seedlings")
plot(haz.ppp(dout22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(dout22[,c(1,2,6)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=2.5, add=T)

# Seed and seedling numbers
plot(estacas1,cex=0.2, main="Number of seeds")
plot(haz.ppp(dout22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(dout22[,c(1,2,3)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=0.02, add=T)
plot(estacas1,cex=0.2, main="Number of seedlings")
plot(haz.ppp(dout22[,c(1,2,8)]), chars=c("B","NB"),cex=0.5,add=T)
plot(haz.ppp(dout22[,c(1,2,5)]), bg= c(rgb(0, 0, 1, 0.2),rgb(0, 1, 0, 0.2)), lty=0, markscale=0.95, add=T)

#-----------------------------------------------------------------------------
# Regression no. seeds vs. no. seedlings
library(plotrix)
library(lattice)

plot(mest1$seeds,mest1$seedl,log="xy")
plot(all$seeds+1,all$seedl+1,log="xy")

sizeplot(all$seeds+1,all$seedl+1,scale=1.5,pow=1.2,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings emerging",
         main="All species")
abline(a=log(1),b=1)
abline(lm(log(all$seedl+1)~log(all$seeds+1),data=all),col="red")

sizeplot(all$seeds+1,all$seedlsurv+1,scale=1.5,pow=1.2,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings surviving",
         main="All species")
abline(a=log(1),b=1)
abline(lm(log(all$seedlsurv+1)~log(all$seeds+1),data=all),col="red")

sizeplot(all$seeds+1,all$seedlsurv+1,scale=1.8,pow=1.2,
         xlab="Number of seeds",ylab="Number of seedlings surviving",
         main="All species", bg= c(rgb(0, 0, 1, 0.3)))
abline(lm(all$seedlsurv+1~all$seeds+1,data=all),col="red")

#-----------------------------------------------------------------------------
# Model fitting

# Fit a non-spatially explicit lme model to test for differences among 
# habitats. Trap is included as a random factor.
R> names(dout)
 [1] "col_parc"  "plot"      "site"      "x"        
 [5] "y"         "seeds"     "seedsp"    "euterpe"  
 [9] "sloanea"   "virola"    "seedl"     "seedlsp"  
[13] "seedl_NI"  "seedlsurv" "eut.seedl" "slo.seedl"
[17] "vir.seedl" "nivel"     "photo"     "vissky"   
[21] "isf"       "isfu"      "dsf"       "dsfu"     
[25] "gsf"       "gsfu"      "difbe"     "difbeu"   
[29] "dirbe"     "dirbeu"    "totbe"     "totbeu"   
[33] "lai"       "eladp"     "mla"       "gndcover" 
[37] "laidev"    "pH_H2O"    "pH_KCl"    "pH_Cacl2" 
[41] "MO"        "P"         "Na"        "K"        
[45] "Ca"        "Mg"        "Al"        "H.Al"     
[49] "SB"        "T"         "Vpct"      "mpct"     
[53] "vcs"       "cs"        "ms"        "fs"       
[57] "vfs"       "sandpct"   "siltpct"   "claypct"  
[61] "natclay"   "dens_gcm3" "phi_r"     "phi_s"    
[65] "alfa"      "m"         "n"         "S"        

R> names(mest)
 [1] "col_parc"  "plot"      "site"      "x"        
 [5] "y"         "seeds"     "seedsp"    "euterpe"  
 [9] "sloanea"   "virola"    "seedl"     "seedlsp"  
[13] "seedlsurv" "eut.seedl" "slo.seedl" "vir.seedl"
[17] "nivel"     "pH_H2O"    "pH_KCl"    "pH_Cacl2" 
[21] "MO"        "P"         "Na"        "K"        
[25] "Ca"        "Mg"        "Al"        "H.Al"     
[29] "SB"        "T"         "Vpct"      "mpct"     
[33] "vcs"       "cs"        "ms"        "fs"       
[37] "vfs"       "sandpct"   "siltpct"   "claypct"  
[41] "natclay"   "dens_gcm3" "phi_r"     "phi_s"    
[45] "alfa"      "m"         "n"         "S" 

attach(dout)
fm1.lme<-lme(seeds~site-1+nivel+dsf+lai+gndcover, random=~1|plot)
fm1.lme

# Test the significance of each factor.
anova(fm1.lme)
summary(fm1.lme)

# Calculates the empirical autocorrelation function
ACF(fm1.lme)
plot(ACF(fm1.lme, maxLag=3), alpha=0.05)

# Refits model incorporating the spatial autocorrelation.
fm2.lme<-update(fm1.lme, correlation=corAR1())
fm2.lme

summary(fm2.lme)
anova(fm1.lme, fm2.lme)
        Model df    AIC    BIC  logLik   Test L.Ratio        p-value
fm1.lme     1  8 1052.3 1068.4 -518.15               
fm2.lme     2  9 1054.0 1072.1 -518.01 1 vs 2 0.26894        0.604

anova(fm2.lme)
summary(fm2.lme)

# The model incorporating spatial autocorrelation improves significantly.
# Checking model fit...
plot(fm2.lme)

# Segun Pinheiro & Bates (2000) cuando el modelo ajusta bien esparamos ver una distribución de los puntos formando un embudo, con una tendencia a aumentar el standarized residuals cuando aumentamos el  fitted values. Este tipo de gráficas y el AIC nos sirven para comprobar el ajuste de diferentes modelos a nuestro datos.

#-----------------------------------------------------------------------------

