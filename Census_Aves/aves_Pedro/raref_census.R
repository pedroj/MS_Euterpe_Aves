##########################################################################
### Rarefaction analysis for census data
# Pedro. Zahara. 11Jul2009.
##########################################################################
# Uses data matrices with censues as rows and species as columns.
library(vegan)
censusB <-read.table("./Census_Aves/aves_Pedro/censusB.txt",header=TRUE,sep="\t",dec=".",na.strings="NA")
censusSB <-read.table("./Census_Aves/aves_Pedro/censusSB.txt",header=TRUE,sep="\t",dec=".",na.strings="NA")
census <-read.table("./Census_Aves/aves_Pedro/censusBSB.txt",header=TRUE,sep="\t",dec=".",na.strings="NA")
#-------------------------------------------------------------------------
# Code for BAMBU
names(censusB)
> str(censusB)
'data.frame':	81 obs. of  15 variables:
 $ speciesB: Factor w/ 81 levels "Aburria_jacutinga",..: 1 2 3 4 5 6 7 8 9 10 ...
 $ apri08  : int  0 0 0 0 1 1 0 0 0 0 ...
 $ may08   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ jun08   : int  0 0 0 0 0 2 0 0 0 0 ...
 $ jul08   : int  0 0 0 0 2 0 0 1 0 0 ...
 $ aug08   : int  0 0 1 0 0 1 0 0 0 0 ...
 $ sep08   : int  0 0 0 0 1 0 0 0 0 1 ...
 $ oct08   : int  0 0 0 0 0 0 0 0 0 0 ...
 $ nov08   : int  1 0 1 0 0 0 0 0 0 1 ...
 $ dec08   : int  0 0 1 0 0 0 3 0 0 1 ...
 $ jan09   : int  0 1 0 0 0 0 0 0 0 1 ...
 $ feb09   : int  0 0 0 1 0 1 1 0 0 0 ...
 $ mar09   : int  0 0 0 0 0 2 0 0 1 0 ...
 $ apr09   : int  0 0 0 0 0 3 0 0 0 2 ...
 $ total   : int  1 1 3 1 4 10 4 1 1 6 ...

tcensus<-t(census[,2:14])
tcensusB<-t(censusB[,2:14])	# Transposing for analysis- only data columns
tcensusSB<-t(censusSB[,2:14])
### Analysis for all years
diversity(tcensusB,"simpson")
diversity(tcensusSB,"simpson")
# total<-specaccum(tcensusB)
totalB<-specaccum(tcensusB,"random")
totalSB<-specaccum(tcensusSB,"random")
total<-specaccum(tcensusB,"rarefaction")

par(mfrow=c(1,2))
plot(totalB,ci.type="poly", col="blue", lwd=2, ci.lty=0,ylab="Number of species", ci.col="lightblue",xlim=c(1,13),ylim=c(0,80),main="Bamboo",xlab="Month")
boxplot(totalB, col="yellow", add=TRUE, pch="+")
plot(totalSB,ci.type="poly", col="blue", lwd=2, ci.lty=0,ylab="Number of species", ci.col="lightblue",xlim=c(1,13),ylim=c(0,80),main="Non-Bamboo",xlab="Month")
boxplot(totalSB, col="yellow", add=TRUE, pch="+")

##########################################################################
# Species accumulation plots
##########################################################################
Accum.1 <- accumresult(tt, factor="tt1$site", method='exact')
Accum.1
accumplot(Accum.1, ci.type="poly", col="blue",
          lwd=2, ci.lty=0, ci.col="lightblue",type="n") # With CI intervals
          
accumplot(Accum.1) # Simple plot

# By Site
tt<-census[,3:83]
tt1<-census[,1:2]
accumcomp(tt, y=tt1, factor="site", method='exact',xlab="Month",
ylab="Number of species",ci.type="poly",lwd=1,
ci.lty=c(0,1),ci.col=c("lightblue","lightblue"))
#-------------------------------------------------------------------------
# Species richness estimators
specpool(tcensusB)
specpool(tcensusSB)

> specpool(tcensusB)
    Species     Chao  Chao.SE   Jack.1 Jack1.SE   Jack.2     Boot
All      71 91.83333 10.91847 93.91667  8.61241 103.3864 81.70408
     Boot.SE  n
All 4.870703 12
> setwd('/Users/pedroj/Documents/Working/MSS_Débora/Data')
> specpool(tcensusSB)
    Species Chao  Chao.SE Jack.1 Jack1.SE   Jack.2     Boot  Boot.SE    n
All      54 86.4 21.80899   70.5 6.667708 81.71212 61.14927 3.450622    12


# Richness estimators for each site
> estimateR(tcensusB)
             may08     jun08     jul08     aug08     sep08 oct08
S.obs    18.000000 16.000000 22.000000 22.000000 30.000000     0
S.chao1  23.600000 18.500000 25.500000 26.666667 64.200000     0
se.chao1  7.483315  4.882888  4.839628  5.922162 31.350900   NaN
S.ACE    24.548800 19.908853 26.416327 28.933333 57.892940   NaN
se.ACE    2.480022  1.843797  2.175850  2.768386  3.703875   NaN
             nov08     dec08     jan09     feb09     mar09     apr09
S.obs    27.000000 28.000000 17.000000 19.000000 25.000000 21.000000
S.chao1  78.000000 37.750000 32.000000 37.200000 46.000000 27.875000
se.chao1 69.422619  8.816191 24.238399 18.602755 20.900153  6.817434
S.ACE    57.098330 38.777364 29.760597 43.422865 46.749913 32.493590
se.ACE    3.566974  3.089240  2.691395  2.341444  3.618902  3.012148

> estimateR(tcensusSB)
              may08     jun08     jul08     aug08     sep08 oct08
S.obs    14.0000000 16.000000 17.000000 24.000000 24.000000     0
S.chao1  14.1666667 52.000000 17.600000 31.500000 30.875000     0
se.chao1  0.8671793       NaN  1.768871  8.366600  6.817434   NaN
S.ACE    15.0051625 27.472813 18.369496 34.150892 36.234694   NaN
se.ACE    1.0521842  2.512874  1.135006  2.792932  3.166303   NaN
            nov08     dec08     jan09     feb09     mar09     apr09
S.obs    20.00000 25.000000 21.000000  7.000000 25.000000 16.000000
S.chao1  39.50000 29.500000 27.875000 10.000000 31.428571 27.250000
se.chao1 23.16747  5.044337  6.817434 11.661904  6.905446 14.844877
S.ACE    47.90586 32.152330 31.887360 11.181818 35.162873 38.756598
se.ACE    4.01541  2.536969  3.011435  1.573216  2.970388  3.892003

# Plots of species curves-Rank Abundance Dominance (RADs)
> rad.lognormal(censusB$total)

RAD model: Log-Normal 
Family: poisson 
No. of species:  74 
Total abundance: 528 

    log.mu  log.sigma   Deviance        AIC        BIC 
  1.328808   1.151592  24.439930 266.451394 271.059524 

> plot(rad.lognormal(censusB$total))
plot(rad.lognormal(censusB$may08))

# We fit all possible models to the RAC curve
modB<- radfit(censusB$total)
modSB<- radfit(censusSB$total)
par(mfrow=c(1,2))
plot(modB)
plot(modSB)

# Comparison of sites
tot <-read.table("total.txt",header=TRUE,sep="\t",dec=".",na.strings="NA")
# Confidence ellipses
library(ade4)
distot <- discrimin(dudi.pca(tot[,2:82], scan = FALSE), tot$site, 
    scan = FALSE)
distot
plot(distot,xax=1,yax=2)
ordiplot(dudi.pca(tot[,2:82]), choices = c(1, 2), type="text",labels=tot$site)
ordiellipse(distot, tot$site, kind="se", conf=0.95, lwd=2, col="blue")

b <- lda(tot$site ~ as.numeric(tot[,2:82]))
predict(b)
detach(bot)
#-----------
attach(car)
c <- lda(group ~ seedl1m+sapl5m+ads5m+trees+canpy+lowcanpy+
                 disteut+distgap+litt+otherseedl1m+
                 dsf+isf+lai+cov+pdr.apr+pdr.jul)
predict(c)

par(mfrow=c(1,2))
plot(b,dimen=2,abbrev=1)
plot(c,dimen=2,abbrev=1,ylim=c(-3,3))
ordiellipse(b, car1$group, kind="se", conf=0.95, lwd=2, col="blue")

##########################################################################
# Comparing B and SB
#-------------------------------------------------------------------------
w <- dudi.pca(log(census[3:83]+1), scal = FALSE, scann = FALSE, 
    nf = 3)
w1 <- within(w, census$site, scann = FALSE)
fatala <- ktab.within(w1)
stat1 <- statis(fatala, scan = FALSE, nf = 3)
w1 <- split(stat1$C.Co, census$date)
w2 <- split(census$site, census$date)
par(mfrow = c(3,2))
for (j in 1:12) {
    s.label(stat1$C.Co[,1:2], clab = 0,
    sub = tab.names(fatala)[j], csub = 3)
    s.class(w1[[j]][,1:2], w2[[j]], clab = 2, axese = FALSE,
    add.plot = TRUE)
}
par(mfrow = c(1,1))

kplot(stat1, arrow = FALSE, traj = FALSE, clab = 2, uni = TRUE, 
    class = census$site) #simpler

mfa1 <- mfa(fatala, scan = FALSE, nf = 3)
w1 <- split(mfa1$co, census$date)
w2 <- split(census$site, census$date)
par(mfrow = c(3,2))
for (j in 1:6) {
    s.label(mfa1$co[,1:2], clab = 0,
    sub = tab.names(fatala)[j], csub = 3)
    s.class(w1[[j]][,1:2], w2[[j]], clab = 2, axese=FALSE,
    add.plot = TRUE)
}
par(mfrow = c(1,1))

#-------------------------------------------------------------------------
# Principal component analysis of census months by site (SB, B)
dd1<-dudi.pca(log(census[3:83]+1), scal = FALSE, scann = FALSE, 
    nf = 3)
temp<-cbind(site=census$site,dd1$li)
plot(temp$Axis1,temp$Axis2,pch=c(19,1)[temp$site])   # Two first PCAs

#-------------------------------------------------------------------------
# Test ordination differences among sites
attach(census)
data(varechem)
library(MASS)
coord<-cca(log(census[3:83]+2))
summary(coord)
ord <-cca(log(census[3:83]+2) ~ site, census)



