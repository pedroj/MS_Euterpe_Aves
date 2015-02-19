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
names(add.eut)
 [1] "block"    "mhab"     "sow"      "add"      "excl"     "germ"    
 [7] "s.unk"    "s.herb"   "s.path"   "sdl.unk"  "sdl.herb" "sdl.path"
[13] "sdl.surv" "pemerg"   "M1"       "M2"       "M3"       "p1"      
[19] "p2"       "p3"       "M4"       "M5"       "M6"       "psurv"   
[25] "p4"       "p5"       "p6"       "precr"

#-----------------------------------------------------------------------------
# Summary statistics regeneration
# Euterpe- probability of final recruitment relative to initial sowing
bystats(add.eut$precr, add.eut$mhab,add.eut$add,add.eut$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.eut$sow))))

         N    Mean         
B A E   20 0.35750 0.036040
SB A E  20 0.39550 0.035366
B C E   20 0.45000 0.033478
SB C E  20 0.43750 0.046296
B A O   20 0.05950 0.029085
SB A O  20 0.02900 0.024399
B C O   20 0.08550 0.030579
SB C O  20 0.01250 0.018692
ALL    160 0.22837 0.040440

# Sloanea- probability of final recruitment relative to initial sowing
bystats(add.slo$precr, add.slo$mhab,add.slo$add,add.slo$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.slo$sow))))

         N     Mean         
B A E   20 0.134000 0.039164
SB A E  20 0.084000 0.029377
B C E   20 0.250000 0.048762
SB C E  20 0.175000 0.045786
B A O   20 0.000000 0.000000
SB A O  20 0.000000 0.000000
B C O   20 0.000000 0.000000
SB C O  20 0.000000 0.000000
ALL    160 0.080375 0.037114

# Virola- probability of final recruitment relative to initial sowing
bystats(add.vir$precr, add.vir$mhab,add.vir$add,add.vir$excl,fun=function(y) c(Mean=mean(y), sqrt(sd(y)/length(add.vir$sow))))

         N     Mean         
B A E   20 0.162500 0.036977
SB A E  20 0.150000 0.037135
B C E   20 0.250000 0.046376
SB C E  20 0.100000 0.040433
B A O   20 0.000000 0.000000
SB A O  20 0.000000 0.000000
B C O   20 0.025000 0.026434
SB C O  20 0.000000 0.000000
ALL    160 0.085938 0.036058

#-----------------------------------------------------------------------------
# Zero-inflated data
# We use a special procedure for data with an excess frequency of zeroes, like 
# the type of data we typically get when using seed traps, seedling quadrat 
# counts, etc.
# We also specify a negative binomial distribution for this type of data, as 
# it has both a super-excess of zeroes and also some points where the number
# of seeds or seedlings is extremely high- i.e., a 'fat-tailed' distribution.
# The results however are somehow consistent with those of the linear model.

# Euterpe - GERM
Call:
zeroinfl(formula = germ ~ mhab * excl * add | sow, data = add.eut, 
    dist = "negbin", EM = TRUE)

Pearson residuals:
   Min     1Q Median     3Q    Max 
-1.905 -0.767  0.159  0.585  3.960 

Count model coefficients (negbin with log link):
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)         2.6027     0.0615   42.35  < 2e-16 ***
mhabSB             -0.6057     0.1049   -5.77  7.8e-09 ***
exclO              -0.4594     0.1038   -4.43  9.6e-06 ***
addC               -1.1796     0.1261   -9.36  < 2e-16 ***
mhabSB:exclO       -0.0445     0.2099   -0.21    0.832    
mhabSB:addC         0.1729     0.2075    0.83    0.405    
exclO:addC          0.1729     0.2254    0.77    0.443    
mhabSB:exclO:addC  -0.9501     0.4329   -2.19    0.028 *  
Log(theta)          6.5247    13.5814    0.48    0.631    

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)   
(Intercept) -1.90147    0.60079   -3.16   0.0016 **
sow         -0.00301    0.04246   -0.07   0.9434   
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 681.806 
Number of iterations in BFGS optimization: 1 
Log-likelihood: -348 on 11 Df

# Euterpe - SDL.SURV
Call:
zeroinfl(formula = sdl.surv ~ mhab * excl * add | sow, data = add.eut, 
    dist = "negbin", EM = TRUE)

Pearson residuals:
   Min     1Q Median     3Q    Max 
-1.628 -0.599 -0.223  0.426  7.617 

Count model coefficients (negbin with log link):
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)          2.015      0.117   17.26  < 2e-16 ***
mhabSB              -0.431      0.181   -2.39  0.01702 *  
exclO               -0.578      0.367   -1.57  0.11547    
addC                -0.868      0.191   -4.54  5.5e-06 ***
mhabSB:exclO        -2.027      0.552   -3.67  0.00024 ***
mhabSB:addC         -0.155      0.301   -0.51  0.60697    
exclO:addC          -1.075      0.498   -2.16  0.03075 *  
mhabSB:exclO:addC    0.125      1.209    0.10  0.91796    
Log(theta)           1.973      0.483    4.08  4.4e-05 ***

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)   -7.834      4.335   -1.81    0.071 .
sow            0.344      0.208    1.66    0.098 .
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 7.189 
Number of iterations in BFGS optimization: 1 
Log-likelihood: -256 on 11 Df

# EUTERPE INTERPRETATION
# These above are the main tables, with the effects of each factor and the 
# interactions for the variable # seeds germinated. First, all the main  
# effects are significant. So, the number of seeds germinating depends  
# significantly on the type of microhabitat (higher no. of seeds germinating  
# in Bamboo), exclusion (higher # seeds germinated within the exclusions, as  
# expected), and addition (higher # seeds germinated in the addition  
# treatment, as expected). Moreover, the only interaction term  
# is significant, and it's significant in its three-way interaction. Thus, the  
# number of early recurited seedling depends on both the exclusion and the 
# addition treatments but depending on the type of microhabitat. To explain 
# these interactions we refer to the histogram blocks figures.

# The second table above is the main table for sdl.surv, summarizing the 
# effects of each factor and the interactions for the variable # seedlings 
# surviving at the end of study. 
# First, the exclusion treatment is no longer significant (it's in fact only 
# marginally significant, with P= 0.11). It seems that mortality of seedlings 
# dillutes the effect of exclusion and makes the final recruitment similar in 
# the control areas and in those excluded to rodent seed predators. There are 
# still differences between microhabitats, however, and the Bamboo areas show 
# a higher number of surviving seedlings. The addition treatment is also 
# highly significant in determining the final number of seedlings established.
# Then there are two significant interaction effects. The effect of exclusion 
# depends on microhabitat type and also depends on the addition teratment. See 
# the histogram blocks for interpretation of these interactions.
# ----------------------------------------------------------------------------

# Sloanea - GERM
Call:
zeroinfl(formula = germ ~ mhab * excl * add | sow, data = add.slo, 
    dist = "negbin", control = zeroinfl.control(method = "SANN", 
        maxit = 10000, trace = FALSE, EM = FALSE))

Pearson residuals:
   Min     1Q Median     3Q    Max 
-0.478 -0.258 -0.118  0.828  7.721 

Count model coefficients (negbin with log link):
                   Estimate Std. Error z value Pr(>|z|)    
(Intercept)          1.0473     0.2864    3.66  0.00025 ***
mhabSB              -0.0918     0.3872   -0.24  0.81257    
exclO               -2.9444     0.9218   -3.19  0.00140 ** 
addC                -1.0986     0.4616   -2.38  0.01731 *  
mhabSB:exclO         0.3795     1.2120    0.31  0.75420    
mhabSB:addC          0.0918     0.6398    0.14  0.88590    
exclO:addC         -15.3069  3848.4005 -0.0040  0.99683    
mhabSB:exclO:addC   14.9274  3848.4010  0.0039  0.99691    
Log(theta)           0.0000     0.3514    0.00  1.00000    

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)    1.145      0.637    1.80    0.072 .
sow           -0.149      0.131   -1.14    0.255  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 1 
Number of iterations in SANN optimization: 10000 
Log-likelihood: -200 on 11 Df

# Sloanea - SDL.SURV
Call:
zeroinfl(formula = sdl.surv ~ mhab * excl * add | sow, data = add.slo, 
    dist = "negbin", control = zeroinfl.control(method = "SANN", 
        maxit = 10000, trace = FALSE, EM = FALSE))

Pearson residuals:
      Min        1Q    Median        3Q       Max 
-2.47e-01 -1.79e-01 -1.69e-05 -1.51e-05  9.65e+00 

Count model coefficients (negbin with log link):
                   Estimate Std. Error  z value Pr(>|z|)
(Intercept)          -0.223      0.475    -0.47     0.64
mhabSB               -0.470      0.646    -0.73     0.47
exclO               -20.079  13232.280 -1.5e-03     1.00
addC                 -0.470      0.653    -0.72     0.47
mhabSB:exclO          0.470  18713.269  2.5e-05     1.00
mhabSB:addC           0.113      1.033     0.11     0.91
exclO:addC            0.470  19848.419  2.4e-05     1.00
mhabSB:exclO:addC    -0.113  28069.904 -4.0e-06     1.00
Log(theta)            0.000      0.630     0.00     1.00

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)  
(Intercept)   1.8687     1.0472    1.78    0.074 .
sow          -0.0671     0.2169   -0.31    0.757  
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 1 
Number of iterations in SANN optimization: 10000 
Log-likelihood: -107 on 11 Df

# SLOANEA INTERPRETATION
# These above are the main tables, with the effects of each factor and the 
# interactions for the variable # seeds germinated. First, only the exclusion 
# and addition effects are significant, with no effect of stand type. So, the 
# number of Sloanea seeds germinating depends significantly on the exclusion 
# (higher # seeds germinated within the exclusions, as expected), and addition 
# (higher # seeds germinated in the addition. No differences are apparent 
# among microhabitat types.
# Thus, the number of early recurited seedlings depends on both the exclusion 
# and the addition treatments but is independent on the type of microhabitat. 

# The second table above is the main table for sdl.surv, summarizing the 
# effects of each factor and the interactions for the variable # seedlings 
# surviving at the end of study. 
# First, the exclusion and addition treatments are no longer significant. It 
# seems that mortality of seedlings dillutes the effect of exclusion and makes 
# the final recruitment similar in the control areas and in those excluded to 
# rodent seed predators. It appears that for Sloanea very high seedling 
# mortalities during the late establsihment stage override the effects of 
# addition and exclusion resulting in similar recruitment patterns in the two 
# microhabitats.
# ----------------------------------------------------------------------------

# Virola - GERM

Call:
zeroinfl(formula = germ ~ mhab * excl * add | sow, data = add.vir, 
    dist = "negbin", EM = TRUE)

Pearson residuals:
   Min     1Q Median     3Q    Max 
-1.360 -0.662  0.189  0.516  1.807 

Count model coefficients (negbin with log link):
                  Estimate Std. Error z value Pr(>|z|)    
(Intercept)         0.6152     0.1644    3.74  0.00018 ***
mhabSB             -0.0556     0.2358   -0.24  0.81358    
exclO              -0.0846     0.2376   -0.36  0.72191    
addC               -0.3528     0.2559   -1.38  0.16798    
mhabSB:exclO       -0.6982     0.3841   -1.82  0.06908 .  
mhabSB:addC        -0.4945     0.4009   -1.23  0.21744    
exclO:addC         -0.6886     0.4222   -1.63  0.10287    
mhabSB:exclO:addC   1.2483     0.6475    1.93  0.05389 .  
Log(theta)         16.8785    14.5909    1.16  0.24736    

Zero-inflation model coefficients (binomial with logit link):
             Estimate Std. Error  z value Pr(>|z|)
(Intercept)   -19.321  16338.819 -0.00118        1
sow            -0.707   6474.796 -0.00011        1
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Theta = 21391113.608 
Number of iterations in BFGS optimization: 17 
Log-likelihood: -192 on 11 Df

# Virola - SDL.SURV
Call:
zeroinfl(formula = sdl.surv ~ mhab * excl * add | sow, data = add.vir, 
    dist = "negbin", control = zeroinfl.control(method = "SANN", 
        maxit = 10000, trace = FALSE, EM = FALSE))

Pearson residuals:
      Min        1Q    Median        3Q       Max 
-2.45e-01 -1.48e-01 -2.88e-05 -2.49e-05  1.10e+01 

Count model coefficients (negbin with log link):
                   Estimate Std. Error  z value Pr(>|z|)
(Intercept)          -0.431      0.470    -0.92     0.36
mhabSB               -0.080      0.634    -0.13     0.90
exclO               -18.872   7770.918 -2.4e-03     1.00
addC                 -0.262      0.657    -0.40     0.69
mhabSB:exclO          0.080  10989.738  7.3e-06     1.00
mhabSB:addC          -0.836      1.214    -0.69     0.49
exclO:addC           16.569   7770.919  2.1e-03     1.00
mhabSB:exclO:addC   -15.471  14187.691 -1.1e-03     1.00
Log(theta)            0.000      0.607     0.00     1.00

Zero-inflation model coefficients (binomial with logit link):
            Estimate Std. Error z value Pr(>|z|)
(Intercept)    2.083      1.482    1.41     0.16
sow           -0.174      0.436   -0.40     0.69

Theta = 1 
Number of iterations in SANN optimization: 10000 
Log-likelihood: -108 on 11 Df

# VIROLA INTERPRETATION
# First, the effect of exclusion marginally depended on microhabitat type, with higher germination and early seedling establishnment in the Bamboo stands. So, the number of Virola seeds germinating depends only marginally on the exclusion (higher # seeds germinated within the exclusions, as expected), but only in the Bamboo stands, where addition treatments also marginally imporve early recruitment. Again, there are no marked differences among stand types.

# The second table above is the main table for sdl.surv, summarizing the 
# effects of each factor and the interactions for the variable # seedlings 
# surviving at the end of study. 
# First, treatments and interactions are no longer significant. It 
# seems that mortality of seedlings dillutes the effect of exclusion and makes 
# the final recruitment similar in the control areas and in those excluded to 
# rodent seed predators. It appears that for Virola very high seedling 
# mortalities during the late establsihment stage override the positive early 
# effects of addition and exclusion in the Bamboo stands, resulting in similar 
# recruitment patterns in the two microhabitats.
# ----------------------------------------------------------------------------

# Likelihood ratio tests for comparing the fitted models with a null model
# of no effects.
# The models fit the data significantly better than the null model, i.e., the 
# intercept-only model. To show that this is the case, we compare with the 
# current model to a null model without predictors using chi-squared test on 
# the difference of log likelihoods.
R> lrtest(n1)
Likelihood ratio test

Model 1: germ ~ mhab + excl + add | sow
Model 2: germ ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1   7   -352                        
2   3   -430 -4   154     <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
R> setwd('/Users/pedro/Documents/Working/MSS_Débora/Data')
R> lrtest(n2)
Likelihood ratio test

Model 1: sdl.surv ~ mhab + excl + add | sow
Model 2: sdl.surv ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1   7   -262                        
2   3   -320 -4   114     <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
R> lrtest(n3)
Likelihood ratio test

Model 1: germ ~ mhab * excl * add | sow
Model 2: germ ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1  11   -200                        
2   3   -229 -8  57.3    1.6e-09 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
R> lrtest(n4)
Likelihood ratio test

Model 1: sdl.surv ~ mhab * excl * add | sow
Model 2: sdl.surv ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1  11   -107                        
2   3   -126 -8  37.5    9.3e-06 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
R> lrtest(n5)
Likelihood ratio test

Model 1: germ ~ mhab * excl * add | sow
Model 2: germ ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1  11   -192                        
2   3   -210 -8  35.8    1.9e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Mensajes de aviso perdidos
In sqrt(diag(vc)[np]) : Se han producido NaNs
R> lrtest(n6)
Likelihood ratio test

Model 1: sdl.surv ~ mhab * excl * add | sow
Model 2: sdl.surv ~ 1
  #Df LogLik Df Chisq Pr(>Chisq)    
1  11   -108                        
2   3   -124 -8  31.6    0.00011 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Note that the model output above does not indicate in any way if our 
# zero-inflated model is an improvement over a standard negative binomial 
# regression.  We can determine this by running the corresponding standard 
# negative binomial model and then performing a Vuong test of the two models.  
# We require the MASS package to run the standard negative binomial 
# regression.

vuong(n1,z1); vuong(n2,z2); vuong(n3,z3); vuong(n4,z4); vuong(n5,z5); vuong(n6,z6); 
Vuong Non-Nested Hypothesis Test-Statistic: 7.2521 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model1 > model2, with p-value 2.0528e-13 # vuong(n1,z1)
Vuong Non-Nested Hypothesis Test-Statistic: 5.1175 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model1 > model2, with p-value 1.5477e-07 # vuong(n2,z2)
Vuong Non-Nested Hypothesis Test-Statistic: 1.3698 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model1 > model2, with p-value 0.08537 # vuong(n3,z3)
Vuong Non-Nested Hypothesis Test-Statistic: -1.0764 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model2 > model1, with p-value 0.14087 # vuong(n4,z4)
Vuong Non-Nested Hypothesis Test-Statistic: 4.0388 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model1 > model2, with p-value 2.6864e-05 # vuong(n5,z5)
Vuong Non-Nested Hypothesis Test-Statistic: -1.3204 
(test-statistic is asymptotically distributed N(0,1) under the
 null that the models are indistinguishible)
in this case:
model2 > model1, with p-value 0.09335 # vuong(n6,z6)

# EXAMPLE TEXT TO EXPLAIN RESULTS OF ZERO-INFLATED MODELS
# The zero-inflated negative binomial regression model predicting fish caught 
# (count) from child, camper, and persons was statistically significant (both 
# with and without robust standard errors). The predictor of excess zeros, 
# persons, was statistically significant. The count predictors child and 
# camper were also each statically significant. For these data, the expected 
# change in log(count) for a one-unit increase in child was -1.5152. Groups 
# with campers (camper = 1) had an expected log count 0.8790 higher than 
# groups without campers (camper = 0).  We can see in our model that the 
# dispersion parameter Log(theta) is significantly different from zero.  This 
# suggests that our outcome is overdispersed and that a negative binomial 
# model is more appropriate than a Poisson model.  The Vuong suggests that our 
# zero-inflated model is a significant improvement over a standard negative 
# binomial model.


