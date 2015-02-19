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
# Calculate Relative Importance for each mortality factor for the total mortality experienced during recruitment
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

