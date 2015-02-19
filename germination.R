# Germination data analysis.
# Based on data from Abraao Leite.
#
germ<-read.table("abraao_germination.txt",header=TRUE,dec=".",sep="\t")
str(germ)
ggplot(germ, aes(bodymass, pgerm) ) +
    geom_point() + geom_smooth(method=lm)

m1<- lm(pgerm~bodymass, data= germ)
m1
summary(m1)
