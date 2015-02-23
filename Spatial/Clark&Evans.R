library(spatstat)
library(ecespa)

#### MESTRADO
R> names(mest22)
[1] "x"        "y"        "seeds"    "seedsp"   "seedl"   
[6] "seedlsp"  "seedl_NI" "site"    

# Seed and seedling species richness
a<-haz.ppp(mest22[,c(1,2,4)])
b<-haz.ppp(mest22[,c(1,2,6)])

# Seed and seedling numbers
c<-haz.ppp(mest22[,c(1,2,3)])
d<-haz.ppp(mest22[,c(1,2,5)])

#### DOUTORADO
R> names(dout22)
[1] "x"         "y"         "seeds"     "seedsp"   
[5] "seedl"     "seedlsp"   "seedlsurv" "site" 

# Seed and seedling species richness
e<-haz.ppp(dout22[,c(1,2,4)])
f<-haz.ppp(dout22[,c(1,2,6)])

# Seed and seedling numbers
g<-haz.ppp(dout22[,c(1,2,3)])
h<-haz.ppp(dout22[,c(1,2,5)])
#---------------------------------------------------------------------------
clarkevans(a)
