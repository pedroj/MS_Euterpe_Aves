###############################################################
### Autocorrelograms for seed traps.
# Pedro. Piracicaba. 13Mar2010
###############################################################
library(spatstat)
library(ecespa)
library(spatial)

R> str(dout1)
'data.frame':	61 obs. of  8 variables:
 $ x        : num  202940 202975 202989 203039 203040 ...
 $ y        : num  7322590 7322564 7322592 7322587 7322587 ...
 $ seeds    : int  49 711 162 235 575 3063 177 76 88 198 ...
 $ seedsp   : int  11 15 12 6 9 3 5 4 3 4 ...
 $ seedl    : int  5 15 37 19 10 10 11 9 2 6 ...
 $ seedlsp  : int  4 9 10 7 6 5 8 4 2 4 ...
 $ seedlsurv: int  5 15 35 12 6 10 7 5 1 3 ...
 $ site     : Factor w/ 2 levels "B","NB": 2 2 2 1 1 1 1 1 ...

dts<-data.frame(x=dout1$x,y=dout1$y,z=dout1$seeds) 

# Create a trend surface using a function such as surf.ls: 

surface<-surf.ls(2,dts)

# This trend surface object is then the first argument to variogram, followed by the number of bins (here 300). The function computes the average squared difference for pairs with separation in each bin, returning results for bins that contain six or more pairs:

variogram(surface,30)
0	500	1000	1500	2000	2500	3000 xp

# The sister function is correlogram, which takes identical arguments: 
correlogram(surface,35) 

library(ncf)
fit1 <- correlog(x=dout1$x, y=dout1$y, z=dout1$seedlsp, increment=5, resamp=999) 
plot(fit1)

R> plot(fit1)
R> data(topo, package="MASS")
R> topo.kr <- surf.ls(2, topo)
R> correlogram(topo.kr, 25)
R> d <- seq(0, 7, 0.1)
R> lines(d, expcov(d, 0.7))

#-------------------------------------------------------------------------
# Autocorrelograms
distgeo<-read.table("dismatrix.txt",header=F,sep="\t",dec=".",na.strings="NA")
nason<-read.table("kinship.txt",header=F,sep="\t",dec=".",na.strings="NA")
nason<-as.dist(nason, diag=T, upper=F)
distgeo<-as.dist(distgeo, diag=T, upper=F)
autocorrelogram <- myautocorr(nason, distgeo, 
                              permutations = 99, simil=T, nclass=80)
abline(0,0,lty=2,lwd=0.5)
abline(v=5,lty=1,lwd=0.5)
text(60,0.08, "Coancestry coeff.", cex=1, pos=4)
text(3,-0.1, "Distance= 64 m", cex=0.6, pos=4, col="red",srt=90)
# legend("topright", inset=.05, title="Pollinator species",
#       c("G. galloti 2006",...), pch=c(19,21,8,15,22,24,25,10),ncol=2)

#-------------------------------------------------------------------------
# Spatial
xy=cbind(NCH.genot@other[,2],NCH.genot@other[,3])
cn1 <- chooseCN(xy,type=2,ask=FALSE)
# Distance matrix
library(raster)
#Make a distance matrix 
dst <- pointDistance(xy, longlat=F)
# coerce to dist object
dst <- as.dist(dst)

