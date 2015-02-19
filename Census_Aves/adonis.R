#--------------------------------------------------------
# Adonis
library(vegan)
censcode<-read.table(pipe("pbpaste"),header=TRUE,dec=",")
censdata<-read.table(pipe("pbpaste"),header=TRUE,dec=",")

adonis(censdata ~ site, data= censcode, permutations=9999)

Call:
adonis(formula = censdata ~ site, data = censcode, permutations = 9999) 

              Df SumsOfSqs MeanSqs F.Model   R2 Pr(>F)
site       1.000     0.269   0.269   1.421 0.06   0.14
Residuals 22.000     4.159   0.189         0.94       
Total     23.000     4.427                 1.00       

#--------------------------------------------------------
Y<-metaMDS(censdata)
Y2 <- metaMDS(censdata, previous.best = Y)
plot(Y2,type="t")
plot(Y2,type="t",display="sites")

#--------------------------------------------------------
# Species grouped by feeding habits
feeddata<-read.table(pipe("pbpaste"),header=TRUE,dec=",")
adonis(feeddata ~ site, data= censcode, permutations=9999)

Y<-metaMDS(feeddata)
Y2 <- metaMDS(feeddata, previous.best = Y)
plot(Y2,type="t"))
plot(Y2,type="t",display="sites")
