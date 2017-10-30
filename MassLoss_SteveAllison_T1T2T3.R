
#October 18, 2017
#go through and clean up and condense all of my scripts and files related to decomposition
#use updated weights spreadsheet from github July 10, 2017
#https://github.com/stevenallison/UCIClimateExperiment/blob/master/WeightsElevationStudy.xlsx

#calculate mass loss
#Reset R's Brain
rm(list=ls())

#install.packages("plyr")
library(plyr )
library(ggplot2)
library(nlme)
library(car)
library(reshape2)
library(gmodels)
library(multcomp)
library(lattice)
#library(xlsx)
library(nlstools)
library(dplyr)

#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")

#this is the decomposition data sheet from git hub July 10, 2017
df1 <- read.csv("data/WeightsElevationStudy.csv",header=T)



my.gg <- function(df,x.vals,y.vals,colors,shapes=NULL,ltys=NULL) {
  ggplot(data=df, aes_string(x=x.vals, y=y.vals, color=colors, shape=shapes, linetype=ltys)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1) +
    stat_summary(fun.y=mean, geom = "point", size=2) +
    stat_summary(fun.y=mean, geom = "line") +
    ylim(c(0,80))
}

# Mass loss analyses ######################################################################################################
df1$Sample <- gsub("[[:digit:]]+[[:alpha:]]*$","",df1$SampleID)
df1$Type <- gsub("^[[:digit:]]+","",df1$Sample)
df1$Rep <- as.numeric(gsub("^[[:digit:]]*[[:alpha:]]+","",df1$SampleID))
df1$DryToWet <- df1$LitterDryWeight/df1$LitterWetWeight

df1$Site <- gsub("Desert","1:Desert",df1$Site)
df1$Site <- gsub("Woodland","2:Scrubland",df1$Site)
df1$Site <- gsub("Grassland","3:Grassland",df1$Site)
df1$Site <- gsub("Oak-Pine","4:Pine-Oak",df1$Site)
df1$Site <- gsub("Subalpine","5:Subalpine",df1$Site)

df1$MicrobialOrigin <- gsub("Desert","1:Desert",df1$MicrobialOrigin)
df1$MicrobialOrigin <- gsub("Woodland","2:Scrubland",df1$MicrobialOrigin)
df1$MicrobialOrigin <- gsub("Grassland","3:Grassland",df1$MicrobialOrigin)
df1$MicrobialOrigin <- gsub("Oak Pine","4:Pine-Oak",df1$MicrobialOrigin)
df1$MicrobialOrigin <- gsub("Subalpine","5:Subalpine",df1$MicrobialOrigin)

df1$MicrobialOrigin[df1$Type=="C"] <- "Sterile"
df1$MicrobialOrigin[df1$Type=="N"] <- "In-situ closed"
df1$MicrobialOrigin[df1$Type=="L"] <- "In-situ"

df1$Initial.Mass <- 5
df1$Initial.Mass[df1$Type %in% c("N","L")] <- 2

# Use container-based mass difference rather than bag-based difference
# Measuring the bag is more error-prone
# But substitute bag difference for missing or unreliable container data
df1$Wet.Mass.Rem <- df1$FullContainer-df1$EmptyContainer
df1$Wet.Mass.Rem2 <- df1$FullBag-df1$EmptyBag 
index <- df1$SampleID=="1N01" & df1$Timepoint==1 |
  df1$SampleID=="1D03" & df1$Timepoint==1 |
  df1$SampleID=="1W03" & df1$Timepoint==1 |
  df1$SampleID=="1P01" & df1$Timepoint==1 |
  df1$SampleID=="3P04" & df1$Timepoint==1 |
  df1$SampleID=="1G05" & df1$Timepoint==2 |
  df1$SampleID=="4D02" & df1$Timepoint==2 |
  df1$SampleID=="1W04" & df1$Timepoint==2 |
  df1$SampleID=="2D02" & df1$Timepoint==2 |
  df1$SampleID=="2D03" & df1$Timepoint==2 |
  df1$SampleID=="1S01" & df1$Timepoint==2 |
  df1$SampleID=="4L03" & df1$Timepoint==3 |
  df1$SampleID=="4C03" & df1$Timepoint==3 |
  df1$SampleID=="4G01" & df1$Timepoint==3 |
  df1$SampleID=="3C02" & df1$Timepoint==3 |
  df1$SampleID=="5P02" & df1$Timepoint==3 |
  df1$SampleID=="5P03" & df1$Timepoint==3 |
  df1$SampleID=="1P01" & df1$Timepoint==3 |
  df1$SampleID=="1P04" & df1$Timepoint==3 |
  df1$SampleID=="2S01" & df1$Timepoint==3
df1$Wet.Mass.Rem[index] <- df1$Wet.Mass.Rem2[index]

df1$Mass.loss <- 100-100*((df1$Wet.Mass.Rem)*df1$DryToWet)/df1$Initial.Mass

# These look like outliers
df1$Mass.loss[df1$SampleID=="1L01" & df1$Timepoint==2] <- NA
df1$Mass.loss[df1$SampleID=="1S01" & df1$Timepoint==2] <- NA

T1 <- df1[!is.na(df1$Site) & df1$Timepoint==1,]
T2 <- df1[!is.na(df1$Site) & df1$Timepoint==2,]
T3 <- df1[!is.na(df1$Site) & df1$Timepoint==3,]

plot.new()
plot(T3$Wet.Mass.Rem2~T3$Wet.Mass.Rem)
text(y=T3$Wet.Mass.Rem2,x=T3$Wet.Mass.Rem, labels=T3$SampleID, cex=0.5, pos = 1, adj=0.5)

plot(T2$Wet.Mass.Rem2~T2$Wet.Mass.Rem)
text(y=T2$Wet.Mass.Rem2,x=T2$Wet.Mass.Rem, labels=T2$SampleID, cex=0.5, pos = 1, adj=0.5)

plot(T1$Wet.Mass.Rem2~T1$Wet.Mass.Rem)
text(y=T1$Wet.Mass.Rem2,x=T1$Wet.Mass.Rem, labels=T1$SampleID, cex=0.5, pos = 1, adj=0.5)

pdf("Figures/decompositionfigures/MassLoss1.pdf",height=4,width=6)
my.gg(T1,"Site","Mass.loss","MicrobialOrigin")
dev.off()


pdf("Figures/decompositionfigures/MassLoss2.pdf",height=4,width=6)
my.gg(T2,"Site","Mass.loss","MicrobialOrigin")
dev.off()

pdf("Figures/decompositionfigures/MassLoss3.pdf",height=4,width=6)
my.gg(T3,"Site","Mass.loss","MicrobialOrigin")
dev.off()

All.means <- df1 %>% group_by(Site,MicrobialOrigin,Timepoint) %>% summarize(mean(Mass.loss,na.rm=T))

df1.1 <- T1[T1$MicrobialOrigin %in% c("1:Desert","2:Scrubland","3:Grassland","4:Pine-Oak","5:Subalpine"),]
df1.2 <- T2[T2$MicrobialOrigin %in% c("1:Desert","2:Scrubland","3:Grassland","4:Pine-Oak","5:Subalpine"),]
df1.3 <- T3[T3$MicrobialOrigin %in% c("1:Desert","2:Scrubland","3:Grassland","4:Pine-Oak","5:Subalpine"),]

m.1 <- gls(Mass.loss~Site*MicrobialOrigin,data=df1.1,na.action="na.omit")
Anova(m.1,type=3)
m.2 <- gls(Mass.loss~Site*MicrobialOrigin,data=df1.2,na.action="na.omit")
Anova(m.2,type=3)
m.3 <- gls(Mass.loss~Site*MicrobialOrigin,data=df1.3,na.action="na.omit")
Anova(m.3,type=2)
m.4 <- aov(Mass.loss~Site*MicrobialOrigin,data=df1.3,na.action="na.omit")
Anova(m.4,type=2)
m.5 <- aov(Mass.loss~MicrobialOrigin*Site,data=df1.3,na.action="na.omit")
Anova(m.5,type=2)


write.csv(T1,"data/MassLossT1_Steve.csv")
write.csv(T2,"data/MassLossT2_Steve.csv")
write.csv(T3,"data/MassLossT3_Steve.csv")

