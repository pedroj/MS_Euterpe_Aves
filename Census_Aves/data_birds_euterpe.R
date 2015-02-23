# Data Birds Euterpe.
# Sevilla, 23 Feb 2015.
data_birds<- read.table(file="data_birds_euterpe.txt",sep="\t", header=T, dec=".",
    na.string="NA")

cor(data_birds[,c(3:8,15,16)])
cor.test(data_birds$abundB, data_birds$pcrB)
cor.test(data_birds$abundNB, data_birds$pcrNB)
cor.test(data_birds$qc, data_birds$pcrB)
cor.test(data_birds$pcdB, data_birds$pcrB)
plot(data_birds$pcdB, data_birds$pcrB)
