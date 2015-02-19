#-----------------------------------------------------------------------------
# Seed rain and seedling emergence and survival.
#-----------------------------------------------------------------------------
coleut<-read.table("col_euterpe.txt",header=TRUE,dec=".",sep="\t")
colslo<-read.table("col_sloanea.txt",header=TRUE,dec=".",sep="\t")
colvir<-read.table("col_virola.txt",header=TRUE,dec=".",sep="\t")
str(coleut)
#-----------------------------------------------------------------------------
# Data were obtained from the seed traps and seedling plots. In some cases the
# number of seedlings in the plots exceeded the number of initial seeds # 
# recorded in the corresponding seed trap. In these cases we used the 
# germination rate estimates obtained from the seed addition experiments to 
# retroclaculate the number of seeds expected to result in the the recorded 
# number of seedlings. Thus we divided the recorded number of seedlings by the 
# germination rate using the mean value for the ADD and CONTROL treatments of # the OPEN replicates in the seed addition experiments.

library(plotrix)
library(lattice)
#-----------------------------------------------------------------------------
# Summary statistics regeneration
library(Hmisc)
# Seed rain
bystats(coleut$seeds, coleut$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(coleut$seeds))))

bystats(colslo$seeds, colslo$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colslo$seeds))))

bystats(colvir$seeds, colvir$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colvir$seeds))))

# Euterpe
     N    Mean      SE
B   31 0.51613 0.14949
NB  30 0.90000 0.24530
ALL 61 0.70492 0.21173
# Sloanea
     N     Mean       SE
B   31 0.032258 0.054262
NB  30 4.066667 0.417223
ALL 61 2.016393 0.354325
# Virola
     N    Mean       SE
B   31 0.29032 0.121575
NB  30 0.10000 0.070726
ALL 61 0.19672 0.105482

# Seedling emergence
bystats(coleut$seedl, coleut$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(coleut$seedl))))

bystats(colslo$seedl, colslo$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colslo$seedl))))

bystats(colvir$seedl, colvir$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colvir$seedl))))

# Euterpe
     N    Mean      SE
B   31 1.58065 0.17452
NB  30 0.73333 0.14513
ALL 61 1.16393 0.16422
# Sloanea
     N    Mean      SE
B   31 0.00000 0.00000
NB  30 0.30000 0.14545
ALL 61 0.14754 0.12213
# Virola
     N     Mean       SE
B   31 0.064516 0.063984
NB  30 0.000000 0.000000
ALL 61 0.032787 0.054254

# Seedling survival (FINAL)
bystats(coleut$seedlsurv, coleut$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(coleut$seeds))))

bystats(colslo$seedl.surv, colslo$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colslo$seeds))))

bystats(colvir$seedl.surv, colvir$site,fun=function(y) c(Mean=mean(y), SE=sqrt(sd(y)/length(colvir$seeds))))

# Euterpe
     N     Mean       SE
B   31 0.419355 0.133586
NB  30 0.033333 0.054708
ALL 61 0.229508 0.114807
# Sloanea
     N    Mean      SE
B   31 0.00000 0.00000
NB  30 0.20000 0.12313
ALL 61 0.09836 0.10329
# Virola
     N     Mean       SE
B   31 0.032258 0.054262
NB  30 0.000000 0.000000
ALL 61 0.016393 0.045814

#-----------------------------------------------------------------------------
# PLOTS- Seedling regeneration vs initial seed rain in traps
par(mfrow=c(2,3))
# Euterpe
sizeplot(coleut$seeds_est+1,coleut$seedl+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings emerging",
         main="Euterpe edulis")
abline(a=log(1),b=1)
abline(lm(log(coleut$seedl+1)~log(coleut$seeds_est+1),data=coleut),col="red")
# Sloanea
sizeplot(colslo$seeds+1,colslo$seedl+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings emerging",
         main="Sloanea guianensis")
abline(a=log(1),b=1)
abline(lm(log(colslo$seedl+1)~log(colslo$seeds+1),data=colslo),col="red")
# Virola
sizeplot(colvir$seeds+1,colvir$seedl+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings emerging",
         main="Virola bicuhyba")
abline(a=log(1),b=1)
abline(lm(log(colvir$seedl+1)~log(colvir$seeds+1),data=colvir),col="red")


# Euterpe
sizeplot(coleut$seeds_est+1,coleut$seedlsurv+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings surviving",
         main="Euterpe edulis")
abline(a=log(1),b=1)
abline(lm(log(coleut$seedlsurv+1)~log(coleut$seeds_est+1),data=coleut),col="red")
# Sloanea
sizeplot(colslo$seeds+1,colslo$seedl.surv+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings surviving",
         main="Sloanea guianensis")
abline(a=log(1),b=1)
abline(lm(log(colslo$seedl.surv+1)~log(colslo$seeds+1),data=colslo),col="red")
# Virola
sizeplot(colvir$seeds+1,colvir$seedl.surv+1,scale=1,log="xy",
         xlab="Number of seeds",ylab="Number of seedlings surviving",
         main="Virola bicuhyba")
abline(a=log(1),b=1)
abline(lm(log(colvir$seedl.surv+1)~log(colvir$seeds+1),data=colvir),col="red")

#-----------------------------------------------------------------------------
# Models
# Linear model to test effects of number of seeds dispersed on number of
# seedlings emerging and surviving.
# EUTERPE

# Log-transformed data - EUTERPE
m1<-lm(log(coleut$seedl+1)~log(coleut$seeds_est+1),data=coleut)
summary(m1)

Call:
lm(formula = log(coleut$seedl + 1) ~ log(coleut$seeds_est + 1), 
    data = coleut)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.47478 -0.17554  0.03166  0.03886  0.94866 

Coefficients:
                          Estimate Std. Error t value Pr(>|t|)    
(Intercept)               -0.03166    0.06242  -0.507    0.614    
log(coleut$seeds_est + 1)  0.49480    0.03900  12.686   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.3247 on 59 degrees of freedom
Multiple R-squared: 0.7317,	Adjusted R-squared: 0.7272 
F-statistic: 160.9 on 1 and 59 DF,  p-value: < 2.2e-16 
#-----------------------------------------------------------------------------
# SLOANEA
m2<-lm(log(colslo$seedl+1)~log(colslo$seeds+1),data=colslo)
summary(m2)

Call:
lm(formula = log(colslo$seedl + 1) ~ log(colslo$seeds + 1), data = colslo)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.55553  0.02017  0.02017  0.02017  1.35749 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -0.02017    0.02861  -0.705    0.484    
log(colslo$seeds + 1)  0.24009    0.03165   7.586 2.75e-10 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2089 on 59 degrees of freedom
Multiple R-squared: 0.4938,	Adjusted R-squared: 0.4852 
F-statistic: 57.55 on 1 and 59 DF,  p-value: 2.748e-10 

#-----------------------------------------------------------------------------
# VIROLA
m3<-lm(log(colvir$seedl+1)~log(colvir$seeds+1),data=colvir)
summary(m3)

Call:
lm(formula = log(colvir$seedl + 1) ~ log(colvir$seeds + 1), data = colvir)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.20461  0.01008  0.01008  0.01008  0.27384 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)           -0.01008    0.01011  -0.997    0.323    
log(colvir$seeds + 1)  0.30974    0.03004  10.310 8.08e-15 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.07498 on 59 degrees of freedom
Multiple R-squared: 0.6431,	Adjusted R-squared: 0.637 
F-statistic: 106.3 on 1 and 59 DF,  p-value: 8.081e-15 

#-----------------------------------------------------------------------------
# Non-parametric tests for the comparison of recruit density in B and NB stands. We finally used the median tests. Others not shown.
library(fBasics)
library(coin)
# Kolmogorov-Smirnov tests for differences between habitats in the frequency 
# distributions of number of seeds/trap
ks2Test(coleut$seeds_est[coleut$site=="B"],coleut$seeds_est[coleut$site=="NB"])
ks2Test(colslo$seeds[colslo$site=="B"],colslo$seeds[coleut$site=="NB"])
ks2Test(colvir$seeds[colvir$site=="B"],colvir$seeds[coleut$site=="NB"])

# Wilcoxon tests
wilcox.exact(coleut$seeds[coleut$site=="B"], coleut$seeds[coleut$site=="NB"],alternative = "greater")
wilcox.exact(colslo$seeds[colslo$site=="B"], colslo$seeds[colslo$site=="NB"],alternative = "greater")
wilcox.exact(colvir$seeds[colvir$site=="B"], colvir$seeds[colvir$site=="NB"],alternative = "greater")

# Median tests (asymptotic) for difference in recruit density between B 
# and NB stands
# Euterpe
median_test(seeds_est~site, data=coleut)	# Seeds
data:  seeds_est by site (B, NB) 
Z = 0.3641, p-value = 0.7158
alternative hypothesis: true mu is not equal to 0 

median_test(seedl~site, data=coleut)	# Seedlings emerging
data:  seedl by site (B, NB) 
Z = -1.9039, p-value = 0.05693
alternative hypothesis: true mu is not equal to 0 

median_test(seedlsurv~site, data=coleut)	# Seedlings surviving
data:  seedlsurv by site (B, NB) 
Z = -1.9466, p-value = 0.05158
alternative hypothesis: true mu is not equal to 0 

# Sloanea
median_test(seeds~site, data=colslo)	# Seeds
data:  seeds by site (B, NB) 
Z = 3.0326, p-value = 0.002425
alternative hypothesis: true mu is not equal to 0 

median_test(seedl~site, data=colslo)	# Seedlings emerging
data:  seedl by site (B, NB) 
Z = 1.7908, p-value = 0.07333
alternative hypothesis: true mu is not equal to 0 

median_test(seedl.surv~site, data=colslo)	# Seedlings surviving
data:  seedl.surv by site (B, NB) 
Z = 1.4497, p-value = 0.1471
alternative hypothesis: true mu is not equal to 0 

# Virola
median_test(seeds~site, data=colvir)	# Seeds
data:  seeds by site (B, NB) 
Z = -0.3527, p-value = 0.7243
alternative hypothesis: true mu is not equal to 0 

median_test(seedl~site, data=colvir)	# Seedlings emerging
data:  seedl by site (B, NB) 
Z = -1.403, p-value = 0.1606
alternative hypothesis: true mu is not equal to 0 

median_test(seedl.surv~site, data=colvir)	# Seedlings surviving
data:  seedl.surv by site (B, NB) 
Z = -0.9837, p-value = 0.3252
alternative hypothesis: true mu is not equal to 0 

#-----------------------------------------------------------------------------
# Models
# Linear model to test effects of microhabitat (B, NB) on number of seeds 
# dispersed and number of seedlings emerging and surviving.
library(pscl)	# For zero-inflated data
library(lmtest)	# For model tests

# SEEDS
# EUTERPE
# Original data - EUTERPE
n1<-zeroinfl(formula = coleut$seeds_est ~ coleut$site, dist = "negbin", EM = TRUE)
summary(n1)
lrtest(n1)

# SLOANEA
n2<-zeroinfl(formula = colslo$seeds ~ colslo$site, dist = "negbin", EM = TRUE)
summary(n2)
lrtest(n2)

# VIROLA
n3<-zeroinfl(formula = colvir$seeds ~ colvir$site, dist = "negbin", EM = TRUE)
summary(n3)
lrtest(n3)
#-----------------------------------------------------------------------------
# SEEDLINGS EMERGING
# EUTERPE
# Original data - EUTERPE
n4<-zeroinfl(formula = coleut$seedl ~ coleut$site, dist = "negbin", EM = TRUE)
summary(n4)
lrtest(n4)

# SLOANEA
n5<-zeroinfl(formula = colslo$seedl ~ colslo$site, dist = "negbin", EM = TRUE)
summary(n5)
lrtest(n5)

# VIROLA
n6<-zeroinfl(formula = colvir$seedl ~ colvir$site, dist = "negbin", EM = TRUE)
summary(n6)
lrtest(n6)
# This model n6 yields a singularity. So, I'm refitting it with a quasipoisson 
# dist.
n6a<-glm(formula = colvir$seedl ~ colvir$site, family=quasipoisson())
summary(n6a)
lrtest(n6a)
#-----------------------------------------------------------------------------
# SEEDLINGS SURVIVING
# EUTERPE
# Original data - EUTERPE
n7<-zeroinfl(formula = coleut$seedlsurv ~ coleut$site, dist = "negbin", EM = TRUE)
summary(n7)
lrtest(n7)

# SLOANEA
n8<-zeroinfl(formula = colslo$seedl.surv ~ colslo$site, dist = "negbin", EM = TRUE)
summary(n8)
lrtest(n8)
# This model n8 yields noin-estimable coefficients. So, I'm refitting it with a quasipoisson 
# dist.
n8a<-glm(formula = colslo$seedl.surv ~ colslo$site, family=quasipoisson())
summary(n8a)
lrtest(n8a)

# VIROLA
n9<-zeroinfl(formula = colvir$seedl.surv ~ colvir$site, dist = "negbin", EM = TRUE)
summary(n9)
lrtest(n9)
# This model n8 yields noin-estimable coefficients. So, I'm refitting it with a quasipoisson 
# dist.
n9a<-glm(formula = colvir$seedl.surv ~ colvir$site, family=quasipoisson())
summary(n9a)
lrtest(n9a)
#-----------------------------------------------------------------------------

