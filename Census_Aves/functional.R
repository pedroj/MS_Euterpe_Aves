# Functional diversity analysis of Euterpe assemblages ----------------
# Abundance (IPA) data.
pres<- read.table(pipe("pbpaste"),sep="\t", header=T, dec=".",
                   na.string="NA")

abund<- read.table(pipe("pbpaste"),sep="\t", header=T, dec=".",
                   na.string="NA")
morph<- read.table(pipe("pbpaste"),sep="\t", header=T, dec=".",
                   na.string="NA")

m<- as.data.frame(morph[,2:10]) # Ecomorphological matrix
a<- t(as.matrix(abund[,2:12]))  # Abundance matrix
p<- t(as.matrix(pres[,2:23]))   # Presence/absence matrix
adef<- t(as.matrix(abund[,2:6]))
anodef<- t(as.matrix(abund[,7:12]))
rownames(m)<- abund$species
colnames(a)<- abund$species
colnames(p)<- abund$species
colnames(adef)<- abund$species
colnames(anodef)<- abund$species

FDcomp <- functcomp(m, a)
FDcompdef <- functcomp(m, adef)
FDcompnodef <- functcomp(m, anodef)
FDcomppres <- functcomp(m, p)
FDcomppres$status<- c(rep("DEF",7),rep("NODEF",15))
# dbFD(x, a, w, w.abun = TRUE, stand.x = TRUE, ord = c("podani", "metric"), asym.bin = NULL, corr = c("sqrt", "cailliez", "lingoes", "none"), calc.FRic = TRUE, m = "max", stand.FRic = FALSE, scale.RaoQ = FALSE, calc.FGR = FALSE, clust.type = "ward", km.inf.gr = 2, km.sup.gr = nrow(x) - 1, km.iter = 100, km.crit = c("calinski", "ssi"), calc.CWM = TRUE, CWM.type = c("dom", "all"), calc.FDiv = TRUE, dist.bin = 2, print.pco = FALSE, messages = TRUE)

FDall <- dbFD(m, a, corr = "cailliez")
FDdef <- dbFD(m, adef, corr = "cailliez")
FDnodef <- dbFD(m, anodef, corr = "cailliez")
FDpres <- dbFD(m, p, corr = "cailliez")

# ward clustering to compute FGR
clust <- dbFD(m, a, corr = "cailliez", 
              calc.FGR = TRUE, clust.type = "ward")
# choose 'g' for number of groups
# 6 groups seems to make good ecological sense
clust

# however, calinksi criterion in 'kmeans' suggests
# that 6 groups may not be optimal
clust2 <- dbFD(m, a, corr = "cailliez",
             calc.FGR = TRUE, clust.type = "kmeans", km.sup.gr = 10)

# Functional dispersion
fdis <- fdisp(gowdis(m), a)

# ---------------------------------------------------------------------
# Cluster with presence data
# ward clustering to compute FGR
clust <- dbFD(m, p, corr = "cailliez", 
              calc.FGR = TRUE, clust.type = "ward")
# choose 'g' for number of groups
# 6 groups seems to make good ecological sense
clust

# however, calinksi criterion in 'kmeans' suggests
# that 6 groups may not be optimal
clust2 <- dbFD(m, a, corr = "cailliez",
               calc.FGR = TRUE, clust.type = "kmeans", km.sup.gr = 10)






