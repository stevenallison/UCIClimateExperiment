#October 26, 717

#Reset R's Brain
rm(list=ls())

#upload library
library(ggplot2)
#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")


sites <- read.csv("data/SiteDescriptions.csv", header=TRUE)
sites$colors <- as.character(sites$colors)
names(sites)

pdf("Figures/ElevationbyPrecipandTemp.pdf", height=4, width=8)
par(mfrow=c(1,2))
plot(TotalPrecipitation_mm ~ Elevation_m, data=sites,col=sites$colors, pch =16, xlab="Elevation (m)",ylab="Total Precipitation (mm)")
mtext("A", line=1, adj=0)
#text(y=sites$TotalPrecipitation_mm ,x=sites$Elevation_m, labels=sites$sitenames, pos=c(3,3,3,1,1))
plot(Meansoiltemp_C ~ Elevation_m, data=sites,col=sites$colors, pch =16, xlab= "Elevation (m)", ylab = "Mean soil temperature C")
mtext("B", line=1, adj=0)
#text(y=sites$Meansoiltemp_C,x=sites$Elevation_m, labels=sites$sitenames, pos=c(1,3,3,3,3))
#legend("topright", legend=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), pch=16, col=c("red","orange","green","blue","purple"))
dev.off()

par(mfrow=c(1,1))
