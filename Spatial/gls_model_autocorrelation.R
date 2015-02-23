##############################################################################
# Spatially-autocorrelated data. Crawley.
# 15Mar2010. Piracicaba.
############################################################################## 
# We could use a GLS model to introduce spatial covariance between seeds from 
# locations that are close together. We begin by making a grouped data object:

attach(dout)
# space<-groupedData(yield~variety|Block)
fm1.lme<-lme(seeds~site-1+nivel+dsf+lai+gndcover, random=~1|plot)
fm1.space<-groupedData(seeds~1|site) 

# Now we use this to fit a model using gls which allows the errors to be 
# correlated and to have unequal variances. We shall add these sophistications 
# later:

m1<-gls(seeds~site-1,fm1.space)
summary(m1)
aov(m1)


#The variety means are given, rather than differences between means, because we removed the intercept from the model by using seeds~site-1 rather than seeds~site in the model formula (see p. 333).

#Now we want to include the spatial covariance. The Variogram function is applied to m1 like this:

plot(Variogram(m1,form=~x+y))

The sample variogram increases only slightly with distance, illustrating a subtle spatial correlation. Extrapolating back to zero distance, there appears to be a nugget of about 0.8. There are several assumptions we could make about the spatial correlation in these data. For instance, we could try a spherical correlation structure, using the corSpher class (the range of options
For more detail, see the help on ?corClasses and on the individual correlation structures (e.g. ?corExp).

We need to specify the distance at which the semivariogram first reaches 1. Inspection shows this distance to be about 130. We can update m1 to include this information:

m2<-update(m1, corr=corSpher(c(130,0.8),form=~x+y,nugget=T)) 
summary(m2)

This is a big improvement, and AIC has dropped from 1354.742 to 1185.863. The range (27.46) and nugget (0.209) are very close to our visual estimates. There are other kinds of spatial model, of course. We might try a rational quadratic model (corRatio); this needs an estimate of the distance at which the semivariogram is (1 + nugget)/ 2 = 1.8/2 = 0.9, as well as an estimate of the nugget. Inspection gives a distance of about 12.5, so we write

m3<-update(m1,corr=corRatio(c(50.0,0.4),form=~x+y,nugget=T))
m4<-update(m1,corr=corLin(form=~x+y))

correlation = corLin(form = ~ Time)
We can use anova to compare the two spatial models: 

anova(m2,m3)
anova(m2,m4)

The rational quadratic model (m3) has a similar AIC and is therefore not to be preferred to the spherical model. To test for the significance of the spatial correlation parameters we need to compare the preferred spatial m2 with the non-spatial m1 (which assumed spatially independent errors):

anova(m1,m2)
anova(m1,m4)

The two extra degrees of freedom used up in accounting for the spatial structure are clearly justified. We need to check the adequacy of the corRatio model. This is done by inspection of the sample variogram for the normalized residuals of model6:

plot(Variogram(m4,resType="n"))


There is no pattern in the plot of the sample variogram, so we conclude that the rational quadratic is adequate. To check for constancy of variance, we can plot the normalized residuals against the fitted values like this:

plot(m2,resid( ., type="n")~fitted(.),abline=0)

and the normal plot is obtained in the usual way:

qqnorm(m2,~resid(.,type="n"))

The model looks fine. The next step is to investigate the significance of any differences between the habitats.

Use update to change the structure of the model from seeds~site-1 to seeds~site: 

m5<-update(m2,model=seeds~site)
anova(m5)
summary(m5)

The differences between the varieties now appear to be highly significant (recall that they were only marginally significant with our linear model3 using analysis of covariance to take account of the latitude and longitude effects).