#-------------------------------------------------------------------------
# Quantitative component
#-------------------------------------------------------------------------
# From the Clipboard (MacOSX) - or input datafile here

rank<-read.table(pipe("pbpaste"),header=TRUE,dec=".",sep="\t")

# Quant. component as product of three variables: visits, fruits/vis and 
# prob. a touched fruit will be ingested
qc<-qc.virola$vis*qc.virola$frv*qc.virola$pdisp
qc.virola<-cbind(qc.virola,qc)
attach(qc)

# NOT-Log scaled axes. Using data for 3 species
plot(vis,frv,pch=c(19,1,8)[qc$plant],xlab="Visit rate",
     ylab="No. fruits/visit", main="Quantitative component")
#, col = terrain.colors(4:8)[qc$plant]
#text(vis, frv, plant, cex=0.6, pos=4, col="red")
legend("topright", inset=.05, title="Plant species",
       c("Euterpe","Sloanea","Virola"), pch=c(19,1,8),horiz=TRUE)
# Isolines of QC
fru1<-seq(0,12,length.out=100)
vis1<-seq(0,5,length.out=100)
#        plot(vis1,0.5/fru1,type="n")
#        lines(vis1,0.01/fru1)
        lines(vis1,0.1/fru1)
        lines(vis1,1/fru1)
        lines(vis1,5/fru1)
        lines(vis1,10/fru1)
text(3.0,0.001, "QC= 0.1", cex=0.6, pos=4, col="red")
text(3.0,0.3, "QC= 1.0", cex=0.6, pos=4, col="red")
text(3.0,0.8, "QC= 5.0", cex=0.6, pos=4, col="red")
text(3.0,1.5, "QC= 10.0", cex=0.6, pos=4, col="red")
#text(3.0,2.2, "QC= 10.0", cex=0.6, pos=4, col="red")

#-------------------------------------------------------------------------
library(scatterplot3d) 
scatterplot3d(vis,frv,crd, pch=16, highlight.3d=TRUE,type="h", main="3D Scatterplot")
#-------------------------------------------------------------------------
# New plot with effective no. of seeds dispersed per visit.
# Pedro - Rio Claro 29 May 2014.
# NOT-Log scaled axes. Using data for 3 species
plot(vis, frv*pdisp, pch=c(19,1,8)[qc$plant], 
     xlab="Visitation rate (/h)",
     ylab="No. seeds dispersed per visit")
#, col = terrain.colors(4:8)[qc$plant]
#text(vis, frv, plant, cex=0.6, pos=4, col="red")
legend("topright", inset=.05, title="Plant species",
    c("Euterpe","Sloanea","Virola"), pch=c(19,1,8),horiz=TRUE)
# Isolines of QC
fru1<-seq(0,12,length.out=100)
vis1<-seq(0,5,length.out=100)
#        plot(vis1,0.5/fru1,type="n")
#        lines(vis1,0.01/fru1)
lines(vis1,0.1/fru1)
lines(vis1,1/fru1)
lines(vis1,5/fru1)
lines(vis1,10/fru1)
text(3.0,0.001, "QC= 0.1", cex=0.6, pos=4, col="red")
text(3.0,0.3, "QC= 1.0", cex=0.6, pos=4, col="red")
text(3.0,0.8, "QC= 5.0", cex=0.6, pos=4, col="red")
text(3.0,1.5, "QC= 10.0", cex=0.6, pos=4, col="red")
#text(3.0,2.2, "QC= 10.0", cex=0.6, pos=4, col="red")

#-------------------------------------------------------------------------
# New plot with effectiveness function.
require(downloader)
#
# Sourcing the effectiveness function code from GitHub.
link = "https://raw.githubusercontent.com/pedroj/effectiveness/master/effectiveness.R"
file = "effectiveness.R"
if(!file.exists(file)) download(link, file, mode = "wb")
source(file)

# Run the function.
effectiveness(qc$vis, qc$frv, qc$plant, qc$code, 10, 
    myxlab= "No. visits/10h", 
    myylab="Effectiveness/vis (No. fruits handled)")

#getting the convex hull of each unique point set
require(plyr)
df <- qc
find_hull <- function(df) df[chull(df$vis, df$frv), ]
hulls <- ddply(df, "plant", find_hull)

plot <- ggplot(data = qc, aes(x = vis, y = frv, 
               colour=plant, fill = plant)) +
    geom_point() + 
    geom_polygon(data = hulls, alpha = 0.5) +
    labs(x = "Visitation rate (/h)", y = "No. seeds dispersed per visit")
plot
