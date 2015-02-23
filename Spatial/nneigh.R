#----------------------------------------------
# Nearest neighbor.
#----------------------------------------------
library(spatstat)
estacas<-read.table(pipe("pbpaste"),header=TRUE,dec=".")
traps<-read.table(pipe("pbpaste"),header=TRUE,dec=".")
str(traps)
all<-read.table(pipe("pbpaste"),header=TRUE,dec=".")
str(traps)
str(all)

estacas1<-haz.ppp(estacas)
traps1<-haz.ppp(traps)

plot(split(all),main="")
N <- nncross(traps1,estacas1)
#N <- nncross(traps1,estacas1)$which
plot(superimpose(estacas1=estacas1,traps1=traps1), main="nncross", cols=c("red","blue"))
arrows(estacas1$x, estacas1$y, traps1[N]$x, traps1[N]$y, length=0.15)

