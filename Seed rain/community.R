#-----------------------------------------------------------------------------
# Seed and seedling community data.
#-----------------------------------------------------------------------------
comm<-read.table("community.txt",header=TRUE,dec=".",sep="\t")
str(comm)

str(comm)
'data.frame':	61 obs. of  8 variables:
 $ col_plot  : Factor w/ 61 levels "1.C5","10.H122",..: 2 3 4 5 6 7 8 9 10 11 ...
 $ site      : Factor w/ 2 levels "B","SB": 1 1 1 1 1 1 1 1 1 1 ...
 $ seedsnum  : int  198 73 129 804 284 218 19 82 128 2361 ...
 $ seedspp   : int  4 6 5 10 5 4 9 7 9 9 ...
 $ seedlnum  : int  6 12 9 8 5 9 18 12 20 12 ...
 $ seedlspp  : int  4 4 7 3 2 4 5 4 6 5 ...
 $ seedlsppni: int  2 6 2 3 2 4 5 6 9 4 ...
 $ seedl.surv: int  3 7 8 5 4 6 12 5 13 9 ...
 
plot(comm$seedsnum,comm$seedlnum,log="xy")
plot(comm$seedsnum,comm$seedlspp+comm$seedlsppni,log="xy")
plot(comm$seedspp,comm$seedlspp,log="xy")
plot(comm$seedsnum,comm$seedl.surv,log="xy")

#-----------------------------------------------------------------------------
# Ploting different habitats. Split 
par(mfrow=c(1,3))
plot(comm$seedsnum,comm$seedlnum,type="n",xlab="No. seeds dispersed",ylab="No. seedlings emerging")
sxc<- split(comm$seedsnum,comm$site) # Split xc in groups defined by "factor"
syc<- split(comm$seedlnum,comm$site)

plot(comm$seedsnum,comm$seedlspp+comm$seedlsppni,type="n",xlab="No. seeds dispersed",ylab="No. seedlings species")
sxc<- split(comm$seedsnum,comm$site) # Split xc in groups defined by "factor"
syc<- split(comm$seedlspp+comm$seedlsppni,comm$site)

plot(comm$seedspp,comm$seedlspp+comm$seedlsppni,type="n",xlab="No. seeds species",ylab="No. seedlings species")
sxc<- split(comm$seedspp,comm$site) # Split xc in groups defined by "factor"
syc<- split(comm$seedspp+comm$seedlsppni,comm$site)

points(sxc[[1]],syc[[1]],pch=10)     # Add the points for each level of factor
points(sxc[[2]],syc[[2]],pch=19)

for (i in 1:2) abline(lm(syc[[i]]~sxc[[i]]),col=i)  # plot separate line fits
text(c(2000,2000),c(7,5),labels=c("Bamboo","Non bamboo"),pos=4)


library(plotrix)
library(lattice)
sizeplot(comm$seedspp,comm$seedlspp+comm$seedlsppni,scale=2)
#-----------------------------------------------------------------------------
# Models
# Linear model to test effects of microhabitat, addition and exclusion on
# the number of seeds germinated

m1<-lm(germ~mhab*add*excl,data=add.eut)
summary(m1)
