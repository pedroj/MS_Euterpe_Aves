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

# PLOTS
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






