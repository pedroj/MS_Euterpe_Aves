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
# Datasets
add.eut<-read.table("add_euterpe.txt",header=TRUE,dec=".",sep="\t")
add.slo<-read.table("add_sloanea.txt",header=TRUE,dec=".",sep="\t")
add.vir<-read.table("add_virola.txt",header=TRUE,dec=".",sep="\t")
#-----------------------------------------------------------------------------
# Plots of no. seeds sown vs no. seedlings recruiting 
# (emerging and established)

names(add.eut)
 [1] "block"    "mhab"     "sow"      "add"      "excl"     "germ"    
 [7] "s.unk"    "s.herb"   "s.path"   "sdl.unk"  "sdl.herb" "sdl.path"
[13] "sdl.surv" "pemerg"   "M1"       "M2"       "M3"       "p1"      
[19] "p2"       "p3"       "M4"       "M5"       "M6"       "psurv"   
[25] "p4"       "p5"       "p6"       "precr"

plot(add.eut$sow,add.eut$sdl.surv)
library(plotrix)
library(lattice)
table(add.eut$sow,add.eut$sdl.surv)
sizeplot(add.eut$sow,add.eut$sdl.surv,scale=2)
text(add.eut$sow,add.eut$sdl.surv,table(add.eut$sow,add.eut$sdl.surv),cex=0.7)
#-----------------------------------------------------------------------------
# Summary statistics regeneration
library(Hmisc)

bystats(add.eut$germ, add.eut$mhab,add.eut$add,add.eut$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.eut$sow))))

bystats(add.slo$germ, add.slo$mhab,add.slo$add,add.slo$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.slo$sow))))

bystats(add.vir$germ, add.vir$mhab,add.vir$add,add.vir$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.vir$sow))))

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

# Euterpe
n1<-zeroinfl(formula = germ ~ mhab*excl*add | sow, data= add.eut, dist = "negbin", EM = TRUE)
n2<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, data= add.eut, dist = "negbin", EM = TRUE)
summary(n1); summary(n2)

# Sloanea
# I'm using the SANN method with no EM estimation, given that the previous 
# model (with EM TRUE) gives a singularity error.
n3<-zeroinfl(formula = germ ~ mhab*excl*add | sow, data= add.slo, dist = "negbin", EM = TRUE)     # This one gives singularity error
n3<-zeroinfl(formula = germ ~ mhab*excl*add | sow, data= add.slo, dist = "negbin",control=zeroinfl.control(method = "SANN", maxit = 10000, trace = FALSE, EM = FALSE))
n4<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, data= add.slo, dist = "negbin", EM = TRUE)     # This one gives singularity error
n4<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, data= add.slo, dist = "negbin",control=zeroinfl.control(method = "SANN", maxit = 10000, trace = FALSE, EM = FALSE))
summary(n3); summary(n4)

# Virola
# I'm using the SANN method with no EM estimation, given that the previous 
# model (with EM TRUE) gives a singularity error.
n5<-zeroinfl(formula = germ ~ mhab*excl*add | sow, data= add.vir, dist = "negbin", EM = TRUE)

n6<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, data= add.vir, dist = "negbin", EM = TRUE)     # This one gives singularity error
n6<-zeroinfl(formula = sdl.surv ~ mhab*excl*add | sow, data= add.vir, dist = "negbin",control=zeroinfl.control(method = "SANN", maxit = 10000, trace = FALSE, EM = FALSE))
summary(n5); summary(n6)

# These models fit the data signif. better than the null model, i.e., the 
# intercept-only model. To show that this is the case, we can compare with the 
# current model to a null model without predictors using chi-squared test on 
# the difference of log likelihoods.

library(lmtest)
lrtest(n1); lrtest(n2); lrtest(n3); lrtest(n4); lrtest(n5); lrtest(n6)

# Note that the model output above does not indicate in any way if our 
# zero-inflated model is an improvement over a standard negative binomial 
# regression.  We can determine this by running the corresponding standard 
# negative binomial model and then performing a Vuong test of the two models.  
# We require the MASS package to run the standard negative binomial 
# regression.

library(MASS)
z1<-glm.nb(formula = germ ~ mhab*excl*add | sow, data=add.eut, link = log)
z2<-glm.nb(formula = sdl.surv ~ mhab*excl*add | sow, data=add.eut, link = log)
z3<-glm.nb(formula = germ ~ mhab*excl*add | sow, data=add.slo, link = log)
z4<-glm.nb(formula = sdl.surv ~ mhab*excl*add | sow, data=add.slo, link = log)
z5<-glm.nb(formula = germ ~ mhab*excl*add | sow, data=add.vir, link = log)
z6<-glm.nb(formula = sdl.surv ~ mhab*excl*add | sow, data=add.vir, link = log)

vuong(n1,z1); vuong(n2,z2); vuong(n3,z3); vuong(n4,z4); vuong(n5,z5); vuong(n6,z6); 

