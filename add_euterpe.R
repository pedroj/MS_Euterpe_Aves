#-----------------------------------------------------------------------------
# Seed addition experiment. Euterpe.
#-----------------------------------------------------------------------------
# block	Block replicate.
# mhab	Microhabitat type: B, bambu; SB, no bambu.
# sow	No. seeds sown.
# add	Seed addition treatment: C, control; A, added.
# excl	Seed predator exclusion treatment: O, control (open); E, excluded.
# germ	No. seeds germinated (emerging).
# s.unk	No. seeds lost to unknown factors.
# s.herb	No. seeds consumed by herbivores.
# s.path	No. seeds consumed by pathogens (fungi, rot).
# sdl.unk	No. seedlings lost to unknown factors.
# sdl.herb	No. seedlings lost to herbivores.
# sdl.path	No. seedlings lost to pathogens.
# sdl.surv	No. seedlings surviving (established).
# pemerg	Prob. seeds germinating.
# M1	Prob. seeds lost to unknown factors.
# M2	Prob. seeds lost to herbivores.
# M3	Prob. seeds lost to pathogens.
# p1	Prob. seed survival to unknown factors.
# p2	Prob. seed survival to herbivores.
# p3	Prob. seed survival to pathogens.
# M3	Prob. seedlings lost to unknown factors.
# M4	Prob. seedlings lost to herbivores.
# M6	Prob. seedlings lost to pathogens.
# psurv	Prob. seedlings recruiting to saplings.
# p4	Prob. seedling survival to unknown factors.
# p5	Prob. seedling survival to herbivores.
# p6	Prob. seedling survival to pathogens.
# precr	Prob. dispersed seed recruiting to sapling.
#-----------------------------------------------------------------------------

add.eut<-read.table("add_euterpe.txt",header=TRUE,dec=".",sep="\t")
R> str(add.eut)
'data.frame':	160 obs. of  28 variables:
 $ block   : Factor w/ 38 levels "B10","B3","B5",..: 19 20 23 16 17 12 21 24 25 25 ...
 $ mhab    : Factor w/ 2 levels "B","SB": 1 1 1 1 1 1 1 1 1 1 ...
 $ sow     : int  7 7 7 7 7 7 7 7 7 7 ...
 $ add     : Factor w/ 2 levels "A","C": 2 2 2 2 2 2 2 2 2 2 ...
 $ excl    : Factor w/ 2 levels "E","O": 2 2 2 2 2 2 2 2 2 2 ...
 $ germ    : int  1 5 5 4 1 0 2 0 2 3 ...
 $ s.unk   : int  6 2 2 2 5 7 3 6 2 4 ...
 $ s.herb  : int  0 0 0 0 0 0 0 1 0 0 ...
 $ s.path  : int  0 0 0 1 1 0 2 0 3 0 ...
 $ sdl.unk : int  1 5 3 3 0 0 2 0 2 3 ...
 $ sdl.herb: int  0 0 1 1 0 0 0 0 0 0 ...
 $ sdl.path: int  0 0 0 0 0 0 0 0 0 0 ...
 $ sdl.surv: int  0 0 1 0 1 0 0 0 0 0 ...
 $ pemerg  : num  0.14 0.71 0.71 0.57 0.14 0 0.29 0 0.29 0.43 ...
 $ M1      : num  0.86 0.29 0.29 0.29 0.71 1 0.43 0.86 0.29 0.57 ...
 $ M2      : num  0 0 0 0 0 0 0 0.14 0 0 ...
 $ M3      : num  0 0 0 0.14 0.14 0 0.29 0 0.43 0 ...
 $ p1      : num  0.14 0.71 0.71 0.71 0.29 0 0.57 0.14 0.71 0.43 ...
 $ p2      : num  1 1 1 1 1 1 1 0.86 1 1 ...
 $ p3      : num  1 1 1 0.86 0.86 1 0.71 1 0.57 1 ...
 $ M4      : num  1 1 0.6 0.75 0 NA 1 NA 1 1 ...
 $ M5      : num  0 0 0.2 0.25 0 NA 0 NA 0 0 ...
 $ M6      : num  0 0 0 0 0 NA 0 NA 0 0 ...
 $ psurv   : num  0 0 0.2 0 1 NA 0 NA 0 0 ...
 $ p4      : num  0 0 0.4 0.25 1 NA 0 NA 0 0 ...
 $ p5      : num  1 1 0.8 0.75 1 NA 1 NA 1 1 ...
 $ p6      : num  1 1 1 1 1 NA 1 NA 1 1 ...
 $ precr   : num  0 0 0.14 0 0.14 0 0 0 0 0 ...
#-----------------------------------------------------------------------------
# Plots of no. seeds sown vs no. seedlings recruiting 
# (emerging and established)

attach(add.eut)
names(add.eut)
 [1] "block"    "mhab"     "sow"      "add"      "excl"     "germ"    
 [7] "s.unk"    "s.herb"   "s.path"   "sdl.unk"  "sdl.herb" "sdl.path"
[13] "sdl.surv" "M1"       "M2"       "M3"       "M4"       "p1"      
[19] "p2"       "p3"       "p4"       "M5"       "M6"       "M7"      
[25] "M8"       "p5"       "p6"       "p7"       "p8"      

plot(sow,sdl.surv)
library(plotrix)
library(lattice)
table(sow,sdl.surv)
sizeplot(sow,sdl.surv,scale=2)
text(sow,sdl.surv,table(sow,sdl.surv),cex=0.7)
#-----------------------------------------------------------------------------
# Summary statistics regeneration
library(Hmisc)

bystats(add.eut$germ, add.eut$mhab,add.eut$add,add.eut$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.eut$sow))))

 function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.eut$sow))) of add.eut$germ by add.eut$mhab, add.eut$add, add.eut$excl 

         N     Mean           
B A E   20 13.50000 0.14039998
SB A E  20  7.00000 0.12635925
B C E   20  4.15000 0.09044600
SB C E  20  2.60000 0.08920502
B A O   20  7.25000 0.17831493
SB A O  20  2.60000 0.14530027
B C O   20  2.40000 0.11352838
SB C O  20  0.65000 0.07858463
ALL    160  5.01875 0.17207812

# Bamboo
summary(germ~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))
summary(sdl.surv~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))
# Non bamboo
summary(germ~add*excl,data=t2,fun=function(y) c(Mean=mean(y), se(y)))
summary(sdl.surv~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))

> summary(germ~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))
germ    N=80

# Bamboo - germ
+-------+-+--+------+---------+
|       | |N |Mean  |se       |
+-------+-+--+------+---------+
|add    |A|40|10.375|0.8287212|
|       |C|40| 3.275|0.3037954|
+-------+-+--+------+---------+
|excl   |E|40| 8.825|0.8381049|
|       |O|40| 4.825|0.7195863|
+-------+-+--+------+---------+
|Overall| |80| 6.825|0.5931505|
+-------+-+--+------+---------+
> summary(sdl.surv~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))
sdl.surv    N=80

# Bamboo - seedlings
+-------+-+--+-----+---------+
|       | |N |Mean |se       |
+-------+-+--+-----+---------+
|add    |A|40|4.375|0.7643394|
|       |C|40|1.875|0.2731288|
+-------+-+--+-----+---------+
|excl   |E|40|5.325|0.6122547|
|       |O|40|0.925|0.3404136|
+-------+-+--+-----+---------+
|Overall| |80|3.125|0.4270798|
+-------+-+--+-----+---------+

> # Non bamboo
> summary(germ~add*excl,data=t2,fun=function(y) c(Mean=mean(y), se(y)))
germ    N=80

# Non bamboo - germ
+-------+-+--+------+---------+
|       | |N |Mean  |se       |
+-------+-+--+------+---------+
|add    |A|40|4.8000|0.5852898|
|       |C|40|1.6250|0.2366635|
+-------+-+--+------+---------+
|excl   |E|40|4.8000|0.4725816|
|       |O|40|1.6250|0.4186172|
+-------+-+--+------+---------+
|Overall| |80|3.2125|0.3609471|
+-------+-+--+------+---------+
> summary(sdl.surv~add*excl,data=t1,fun=function(y) c(Mean=mean(y), se(y)))
sdl.surv    N=80

# Non bamboo - seedlings
+-------+-+--+-----+---------+
|       | |N |Mean |se       |
+-------+-+--+-----+---------+
|add    |A|40|4.375|0.7643394|
|       |C|40|1.875|0.2731288|
+-------+-+--+-----+---------+
|excl   |E|40|5.325|0.6122547|
|       |O|40|0.925|0.3404136|
+-------+-+--+-----+---------+
|Overall| |80|3.125|0.4270798|
+-------+-+--+-----+---------+

#-----------------------------------------------------------------------------
# Models
# Linear model to test effects of microhabitat, addition and exclusion on
# the number of seeds germinated

m1<-lm(germ~mhab*add*excl,data=add.eut)
summary(m1)

Call:
lm(formula = germ ~ mhab * add * excl, data = add.eut)

Residuals:
       Min         1Q     Median         3Q        Max 
-7.250e+00 -1.600e+00  3.761e-15  1.400e+00  8.750e+00 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        13.5000     0.6241  21.630  < 2e-16 ***
mhabSB             -6.5000     0.8827  -7.364 1.05e-11 ***
addC               -9.3500     0.8827 -10.593  < 2e-16 ***
exclO              -6.2500     0.8827  -7.081 4.96e-11 ***
mhabSB:addC         4.9500     1.2483   3.965 0.000112 ***
mhabSB:exclO        1.8500     1.2483   1.482 0.140403    
addC:exclO          4.5000     1.2483   3.605 0.000423 ***
mhabSB:addC:exclO  -2.0500     1.7653  -1.161 0.247363    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 2.791 on 152 degrees of freedom
Multiple R-squared: 0.6682,	Adjusted R-squared: 0.6529 
F-statistic: 43.73 on 7 and 152 DF,  p-value: < 2.2e-16 

> anova(m1)
Analysis of Variance Table

Response: germ
               Df  Sum Sq Mean Sq  F value    Pr(>F)    
mhab            1  522.01  522.01  67.0002 1.018e-13 ***
add             1 1055.76 1055.76 135.5077 < 2.2e-16 ***
excl            1  514.81  514.81  66.0760 1.410e-13 ***
mhab:add        1  154.06  154.06  19.7733 1.674e-05 ***
mhab:excl       1    6.81    6.81   0.8736 0.3514456    
add:excl        1  120.76  120.76  15.4992 0.0001253 ***
mhab:add:excl   1   10.51   10.51   1.3485 0.2473625    
Residuals     152 1184.25    7.79                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

> m2<-lm(sdl.surv~mhab*add*excl,data=add.eut)
> summary(m2)

Call:
lm(formula = sdl.surv ~ mhab * add * excl, data = add.eut)

Residuals:
   Min     1Q Median     3Q    Max 
 -6.50  -1.15  -0.35   0.85   9.75 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)         7.5000     0.4944  15.170  < 2e-16 ***
mhabSB             -2.7500     0.6992  -3.933 0.000127 ***
addC               -4.3500     0.6992  -6.221 4.56e-09 ***
exclO              -6.2500     0.6992  -8.939 1.24e-15 ***
mhabSB:addC         1.3500     0.9888   1.365 0.174187    
mhabSB:exclO        1.8500     0.9888   1.871 0.063279 .  
addC:exclO          3.7000     0.9888   3.742 0.000258 ***
mhabSB:addC:exclO  -1.0000     1.3984  -0.715 0.475643    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 2.211 on 152 degrees of freedom
Multiple R-squared: 0.5574,	Adjusted R-squared: 0.5371 
F-statistic: 27.35 on 7 and 152 DF,  p-value: < 2.2e-16 

> anova(m2)
Analysis of Variance Table

Response: sdl.surv
               Df Sum Sq Mean Sq  F value    Pr(>F)    
mhab            1  78.40   78.40  16.0366 9.690e-05 ***
add             1 172.22  172.22  35.2284 1.912e-08 ***
excl            1 555.02  555.02 113.5295 < 2.2e-16 ***
mhab:add        1   7.23    7.23   1.4779   0.22599    
mhab:excl       1  18.23   18.23   3.7279   0.05537 .  
add:excl        1 102.40  102.40  20.9458 9.754e-06 ***
mhab:add:excl   1   2.50    2.50   0.5114   0.47564    
Residuals     152 743.10    4.89                       
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

#-----------------------------------------------------------------------------
# Zero-inflated data
# We use a special procedure for data with an excess frequency of zeroes, like 
# the type of data we typically get when using seed traps, seedling quadrat 
# counts, etc.
# We also specify a negative binomial distribution for this type of data, as 
# it has both a super-excess of zeroes and also some points where the number
# of seeds or seedlings is extremely high- i.e., a 'fat-tailed' distribution.
# The results however are somehow consistent with those of the linear model.
library(pscl)
hist(germ, breaks=50, col="gray")
hist(sdl.surv, breaks=50, col="gray")
n1<-zeroinfl(formula = germ ~ mhab*excl*add | sow, dist = "negbin", EM = TRUE)
n2<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, dist = "negbin", EM = TRUE)
summary(n1); summary(n2)

Call:
zeroinfl(formula = germ ~ mhab * excl * add | sow, dist = "negbin", 
    EM = TRUE)


Count model coefficients (negbin with log link):
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)        2.60269    0.06146  42.349  < 2e-16 ***
mhabSB            -0.60573    0.10493  -5.773  7.8e-09 ***
exclO             -0.45943    0.10380  -4.426  9.6e-06 ***
addC              -1.17958    0.12609  -9.355  < 2e-16 ***
mhabSB:exclO      -0.04448    0.20992  -0.212   0.8322    
mhabSB:addC        0.17294    0.20752   0.833   0.4046    
exclO:addC         0.17294    0.22536   0.767   0.4428    
mhabSB:exclO:addC -0.95014    0.43292  -2.195   0.0282 *  
Log(theta)         6.52475   13.58140   0.480   0.6309    

# This above is the main table, summarizing the effects of each factor and the 
# interactions for the variable # seeds germinated. First, all the main  
# effects are significant. So, the number of seeds germinating depends  
# significantly on the type of microhabitat (higher no. of seeds germinating  
# in Bamboo), exclusion (higher # seeds germinated within the exclusions, as  
# expected), and addition (higher # seeds germinated in the addition  
# treatment, as expected). Moreover, the only interaction term  
# is significant, and it's significant in its three-way interaction. Thus, the  
# number of recurited seedling depends on both the exclusion and the addition  
# treatments but depending on the type of microhabitat. To explain these  
# interactions we refer to the histogram blocks figures.

Zero-inflation model coefficients (binomial with logit link):
             Estimate Std. Error z value Pr(>|z|)   
(Intercept) -1.901465   0.600792  -3.165  0.00155 **
sow         -0.003013   0.042460  -0.071  0.94343   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 681.8062 
Number of iterations in BFGS optimization: 1 
Log-likelihood: -348.5 on 11 Df

Call:
zeroinfl(formula = sdl.surv ~ mhab * excl * add | sow, dist = "negbin", 
    EM = TRUE)

Count model coefficients (negbin with log link):
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)         2.0149     0.1167  17.264  < 2e-16 ***
mhabSB             -0.4312     0.1807  -2.386 0.017025 *  
exclO              -0.5777     0.3670  -1.574 0.115473    
addC               -0.8675     0.1909  -4.544 5.52e-06 ***
mhabSB:exclO       -2.0267     0.5522  -3.670 0.000243 ***
mhabSB:addC        -0.1551     0.3014  -0.514 0.606975    
exclO:addC         -1.0755     0.4978  -2.160 0.030751 *  
mhabSB:exclO:addC   0.1245     1.2090   0.103 0.917962    
Log(theta)          1.9726     0.4831   4.083 4.44e-05 ***

# This above is the main table, summarizing the effects of each factor and the 
# interactions for the variable # seedlings surviving at the end iof study. 
# First, the exclusion teratment is no longer significant (it's in fact only 
# marginally significant, ewith P= 0.11). It seems that mortality of seedlings 
# dillutes the effect of exclusion and makes the final recruitment similar in 
# the control areas and in those excluded to rodent seed predators. There are 
# still differences between microhabitats, however, and the Bamboo areas show 
# a higher number of surviving seedlings. The addition treatment is also 
# highly significant in determining the final number of seedlings established.
# Then there are two significant interaction effects. The effect of exclusion 
# depends on microhabitat type and also depends on the addition teratment. See 
# the histogram blocks for interpretation of these interactions.

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)  -7.8340     4.3352  -1.807   0.0707 .
sow           0.3441     0.2078   1.656   0.0977 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 7.1894 
Number of iterations in BFGS optimization: 1 
Log-likelihood: -256.1 on 11 Df
> 
library(lmtest)
lrtest(n1)
library(MASS)
z1<-glm.nb(formula = germ ~ mhab*excl*add | sow, link = log)
vuong(n1,z1)

barplot(tapply(germ,list(mhab,add),mean),beside=T)
legend(4,8,c("Bamboo","Non bamboo"),fill=c(2,7))
#-----------------------------------------------------------------------------
# Calculate Relative Importance for Each mortality factor to the number of  
# seeds germinating.
library(relaimpo)
t1 <- subset(eut.stage, mhab=="B")
t2 <- subset(eut.stage, mhab=="SB")
attach(add.eut)
# BAMBOO
calc.relimp(germ~M1+M2+M3,data=t1,type=c("lmg","last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples) 
bootB<-boot.relimp(germ~M1+M2+M3,data=t1,b = 1000)
booteval.relimp(bootB) # print result
plot(booteval.relimp(bootB),main="Bamboo") # plot result

# NON BAMBOO
calc.relimp(germ~M1+M2+M3,data=t2,type=c("lmg","last","first","pratt"),rela=TRUE)
# Bootstrap Measures of Relative Importance (1000 samples) 
bootSB<-boot.relimp(germ~M1+M2+M3,data=t2,b = 1000)
booteval.relimp(bootSB) # print result
plot(booteval.relimp(bootSB),main="Non Bamboo") # plot result

# Bootstrap Measures of Relative Importance (1000 samples) 
boot1<-boot.relimp(germ~M1+M2+M3,data=t1,b = 1000)
boot2<-boot.relimp(germ~M1+M2+M3,data=t2,b = 1000)

par(mfrow=c(1,2))
layout=c(1,2)
plot(booteval.relimp(boot1),main="Bamboo") # plot result
plot(booteval.relimp(boot2),main="Non Bamboo",add=T) # plot result

#-----------------------------------------------------------------------------
# Calculate Relative Importance for each mortality factor for the total recruitment
library(relaimpo)
t1 <- subset(eut.stage, mhab=="B")
t2 <- subset(eut.stage, mhab=="SB")
# BAMBOO
calc.relimp(sow-sdl.surv~M1+M2+M3+M4+M5+M6,data=t1,type=c("lmg","last","first","pratt"),rela=TRUE)

# NON BAMBOO
calc.relimp(sow-sdl.surv~M1+M2+M3+M4+M5+M6,data=t2,type=c("lmg","last","first","pratt"),rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
bootB<-boot.relimp(sow-sdl.surv~M1+M2+M3+M4+M5+M6,data=t1,b = 10000)
bootSB<-boot.relimp(sow-sdl.surv~M1+M2+M3+M4+M5+M6,data=t2,b = 10000)

par(mfrow=c(1,2))
layout=c(1,2)
plot(booteval.relimp(bootB),main="Bamboo") # plot result
plot(booteval.relimp(bootSB),main="Non Bamboo",add=T) # plot result

#-----------------------------------------------------------------------------
# Graphics
attach(add.eut)
a1<-bwplot(add+excl~ germ | mhab, add.eut, notch=TRUE,xlab="Number of seedlings")
a2<-bwplot(add+excl~ sdl.surv| mhab, add.eut, notch=TRUE,xlab="Number of saplings")
print(a1, position = c(0,0,1,.4), more = T) 
print(a2, position = c(0,.4,1,.8))

detach()

# Bar plots
# One way design
library(sciplot)
# Seedlings - The two habitats
par(mfrow=c(2,2))
bargraph.CI(x.factor = add[mhab=="SB"], response = germ[mhab=="SB"], 
            group= excl[mhab=="SB"], data = add.eut,
            xlab = "Seed addition", ylab = "Number of seedlings", 
            cex.lab = 1.5, main="Non bamboo",
#            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE,
            x.leg=4,ylim=c(0,15))
bargraph.CI(x.factor = add[mhab=="B"], response = germ[mhab=="B"], group= excl[mhab=="B"], data = add.eut,
            xlab = "Seed addition", ylab = "Number of seedlings", 
            cex.lab = 1.5, main="Bamboo",
#            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE,
            x.leg=4,ylim=c(0,15))

# Saplings - The two habitats
# par(mfrow=c(1,2))
bargraph.CI(x.factor = add[mhab=="SB"], response = sdl.surv[mhab=="SB"], 
            group= excl[mhab=="SB"], data = add.eut,
            xlab = "Seed addition", ylab = "Number of saplings", 
            cex.lab = 1.5, main="Non bamboo",
#            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE,
            x.leg=4,ylim=c(0,15))
bargraph.CI(x.factor = add[mhab=="B"], response = sdl.surv[mhab=="B"], group= excl[mhab=="B"], data = add.eut,
            xlab = "Seed addition", ylab = "Number of saplings", 
            cex.lab = 1.5, main="Bamboo",
#            col = "black", angle = 45, cex.names = 1.25,
            density = c(0,20), legend = TRUE,
            x.leg=4,ylim=c(0,15))
#-----------------------------------------------------------------------------
# Ploting different groups of data. Split 
plot(sow,germ,type="n",xlab="No. seeds sown",ylab="No. seedlings emerging")
sxc<- split(sow,mhab)   # Split xc in groups defined by "factor"
syc<- split(germ,mhab)

points(sxc[[1]],syc[[1]],pch=10)     # Add the points for each level of factor
points(sxc[[2]],syc[[2]],pch=19)

for (i in 1:2) abline(lm(syc[[i]]~sxc[[i]]),col=i)  # plot separate line fits
labels=mhab
#-----------------------------------------------------------------------------
# Plotting M and p values
library(MASS)
tt<-cbind(add.eut$sow,add.eut$germ,add.eut$sapl.surv)
parallel(~tt[,1:3] | add.eut[,4]*add.eut[,5]*add.eut[,2], main = "Mortality",layout=c(2,4))

parallel(~add.eut[,18:21] | add.eut[,4]*add.eut[,5]*add.eut[,2], main = "Mortality",layout=c(2,4))
parallel(~add.eut[,14:17] | add.eut[,4]*add.eut[,5]*add.eut[,2], main = "Mortality",layout=c(2,4))
parallel(~add.eut[,18:21] | add.eut[,4]*add.eut[,5]*add.eut[,2], main = "Mortality",layout=c(2,4))
parallel(~add.eut[,26:29] | add.eut[,4]*add.eut[,5]*add.eut[,2], main = "Mortality",layout=c(2,4))

#-----------------------------------------------------------------------------
# Graphs
par(mfrow=c(2,2))
plot(add.eut$germ+1~jitter(add.eut$sow+1,amount=0.5),log="xy",pch=c(3,1)[add.eut$add],cex=1.5,xlab="Number of seeds sown",ylab="Number of seedlings emerged")
text(c(5,5),c(20,15),labels=c("+ Addition","o Control"),pos=4)

# Separate symbol for ADD
abline(a=0, b=1, col = 2)
temp<-subset(add.eut, add.eut$add=="A", select = c(germ, sow))
lines(spline(temp$germ+1~temp$sow+1))
temp<-subset(add.eut, add.eut$add=="C", select = c(germ, sow))
lines(spline(temp$germ+1~temp$sow+1),lty=2)

plot(add.eut$germ+1~jitter(add.eut$sow+1,amount=0.5),log="xy",pch=c(3,1)[add.eut$excl],cex=1.5,xlab="Number of seeds sown",ylab="Number of seedlings emerged")
text(c(5,5),c(20,15),labels=c("+ Open","o Excluded"),pos=4)
# Separate symbol for EXCL
abline(a=0, b=1, col = 2)
temp<-subset(add.eut, add.eut$excl=="E", select = c(germ, sow))
lines(spline(temp$germ+1~temp$sow+1))
temp<-subset(add.eut, add.eut$excl=="O", select = c(germ, sow))
lines(spline(temp$germ+1~temp$sow+1),lty=2)

plot(add.eut$sdl.surv+1~jitter(add.eut$sow+1,amount=0.5),log="xy",pch=c(3,1)[add.eut$add],cex=1.5,xlab="Number of seeds sown",ylab="Number of saplings")
text(c(5,5),c(17,12),labels=c("+ Addition","o Control"),pos=4)
# Separate symbol for ADD
abline(a=0, b=1, col = 2)
temp<-subset(add.eut, add.eut$add=="E", select = c(germ, sow))
lines(spline(temp$sdl.surv+1~temp$sow+1))
temp<-subset(add.eut, add.eut$add=="O", select = c(germ, sow))
lines(spline(temp$sdl.surv+1~temp$sow+1),lty=2)

plot(add.eut$sdl.surv+1~jitter(add.eut$sow+1,amount=0.5),log="xy",pch=c(3,1)[add.eut$excl],cex=1.5,xlab="Number of seeds sown",ylab="Number of saplings")
text(c(5,5),c(17,12),labels=c("+ Open","o Excluded"),pos=4)
# Separate symbol for EXCL
abline(a=0, b=1, col = 2)
temp<-subset(add.eut, add.eut$excl=="E", select = c(germ, sow))
lines(spline(temp$sdl.surv+1~temp$sow+1))
temp<-subset(add.eut, add.eut$excl=="O", select = c(germ, sow))
lines(spline(temp$sdl.surv+1~temp$sow+1),lty=2)

#-----------------------------------------------------------------------------
library(MASS)
mhab<-split(add.eut,add.eut$mhab)

B.add<-split(mhab$B,mhab$B[,4])
SB.add<-split(mhab$SB,mhab$B[,4])
c<-split(b$A,b$A[,3])

B.add.excl<-split(B.add$A,B.add$A[,5])
B.add.excl<-split(B.add$C,B.add$A[,5])

SB.add.excl<-split(SB.add$A,SB.add$A[,5])
SB.add.excl<-split(SB.add$C,SB.add$A[,5])

BAaEo<-B.add.excl$O
BAaEe<-B.add.excl$E
BAcEo<-B.add.excl$O
BAcEe<-B.add.excl$E
SBAaEo<-SB.add.excl$O
SBAaEe<-SB.add.excl$E
SBAcEo<-SB.add.excl$O
SBAcEe<-SB.add.excl$E

par(mfrow=c(2,1))
parcoord(mhab$B[,c(3,6,13)])
parcoord(mhab$SB[,c(3,6,13)])
parcoord(BAcEo[,c(3,6,13)])
parcoord(BAaEe[,6:9])
parcoord(BAcEe[,6:9])


