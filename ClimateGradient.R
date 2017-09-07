library(ggplot2)
library(nlme)
library(car)
library(reshape2)
library(gmodels)
library(multcomp)
library(lattice)
library(xlsx)
library(nlstools)
library(dplyr)
source("~/Documents/RFunctions/CustomFunctions.R")

my.gg <- function(df,x.vals,y.vals,colors,shapes=NULL,ltys=NULL) {
  ggplot(data=df, aes_string(x=x.vals, y=y.vals, color=colors, shape=shapes, linetype=ltys)) +
    stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1) +
    stat_summary(fun.y=mean, geom = "point", size=2) +
    stat_summary(fun.y=mean, geom = "line") +
    ylim(c(0,80))
}

# Mass loss analyses ######################################################################################################
setwd("~/Documents/Projects/NSF Ecosystems 2014/Project/Data/Decomposition")
write.csv(read.xlsx("WeightsElevationStudy.xlsx",sheetName="Sheet1",header=T),"df0.csv",row.names=F,quote=F)
df1 <- read.csv("df0.csv")
df1$Sample <- gsub("[[:digit:]]+[[:alpha:]]*$","",df1$SampleID)
df1$Type <- gsub("^[[:digit:]]+","",df1$Sample)
df1$Rep <- as.numeric(gsub("^[[:digit:]]*[[:alpha:]]+","",df1$SampleID))
df1$DryToWet <- df1$LitterDryWeight/df1$LitterWetWeight

df1.1 <- read.xlsx("T2 Reweighed Samples.xlsx",sheetName="Sheet1",header=T)
df1[df1$Timepoint==2 & df1$SampleID %in% df1.1$Sample.Name,"EnzWeight.hydrolytic.g."] <- df1.1$Weight[order(df1.1$Sample.Name)]

# Copied blank plates from 01/18/2017 to 01/20/2017 because they were missing
# Copied 22 degrees blank plate to 28 degrees on 01/28/2017 because it was missing

# These enzyme samples were lost and re-weighed out again later
df1$HydrolyticWetWeight[df1$Sample=="2W" & df1$Rep==2 & df1$timepoint==1] <- 0.404
df1$HydrolyticWetWeight[df1$Sample=="2S" & df1$Rep==4 & df1$timepoint==1] <- 0.402
df1$HydrolyticWetWeight[df1$Sample=="3S" & df1$Rep==1 & df1$timepoint==1] <- 0.402

df1$MUB.dry <- df1$HydrolyticWetWeight*df1$DryToWet
df1$OX.dry <- df1$OxidativeWetWeight*df1$DryToWet

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

plot(T3$Wet.Mass.Rem2~T3$Wet.Mass.Rem)
text(y=T3$Wet.Mass.Rem2,x=T3$Wet.Mass.Rem, labels=T3$SampleID, cex=0.5, pos = 1, adj=0.5)

pdf("MassLoss1.pdf",height=4,width=6)
my.gg(T1,"Site","Mass.loss","MicrobialOrigin")
dev.off()

pdf("MassLoss2.pdf",height=4,width=6)
my.gg(T2,"Site","Mass.loss","MicrobialOrigin")
dev.off()

pdf("MassLoss3.pdf",height=4,width=6)
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


df3 <- df1[c("SampleID","Timepoint","Sample","Rep","Site","LitterOrigin","MicrobialOrigin",
             "Mass.loss","MUB.dry","OX.dry")]
names(df3) <- c("ID","Timepoint","Sample","Rep","Site","LitterOrigin","MicrobialOrigin",
                "Mass.loss","MUB.dry","OX.dry")
write.csv(df3,"df3.csv",row.names=F,quote=F)

######################################################################################################

# Enzyme analyses #########################################################################################################
# Using minimum values for buffer and homogenate (blank) controls from the 8 wells in a column to filter out bubble problem

setwd("~/Documents/Projects/NSF Ecosystems 2014/Project/Data/Analysis")
Enz0 <- rbind(
  read.table("T0Hydrolase.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T0Oxidase.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T0Oxidase2.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T0SterileControls.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
)
Enz0$Date[grep("^X",Enz0$Sample)] <- "06-11-2015"
Enz0$Date[grep("^Y",Enz0$Sample)] <- "10-19-2015"
Enz0$Date[grep("^Z",Enz0$Sample)] <- "09-11-2015"
Enz0$Plate.Date <- NA
Enz0$Timepoint <- 0

Enz1.1 <- rbind(
  read.table("T1Hydrolase1.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T1Hydrolase2.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T1Hydrolase3.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T1Oxidase1.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T1Oxidase2.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T1Oxidase3.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
)
Enz1.1$Plate.Date <- Enz1.1$Date
Enz1.1$Date <- "04-05-2016"
Enz1.1$Timepoint <- 1

Enz1.2 <- rbind(
  read.table("T2Hydrolase.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T2Oxidase.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T2HydrolaseNative.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE),
  read.table("T2OxidaseNative.out.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE)
)
Enz1.2$Plate.Date <- Enz1.2$Date
Enz1.2$Date <- "10-23-2016"
Enz1.2$Timepoint <- 2

Enz.all <- rbind(Enz0,Enz1.1,Enz1.2)

df3 <- read.csv("df3.csv")
Enz2 <- merge(Enz.all,df3)

# Sort the dataset
Enz2 <- Enz2[order(Enz2$Timepoint,Enz2$Rep,Enz2$Sample,Enz2$Enz,Enz2$Temp,Enz2$Concen,Enz2$Time), ]

# Setting all oxidase buffer values to ~0.0367 which is the geometric mean of all values <0.05
# That is, all values not likely to be impacted by bubbles
mean.buf <- exp(mean(log(Enz2$Buffer[Enz2$Enz %in% c("PPO","OX") & Enz2$Buffer<0.05])))
Enz2$Buffer[Enz2$Enz %in% c("PPO","OX") &  Enz2$Buffer > mean.buf] <- mean.buf

# Insert mean substrate control values with buffer value subtracted; set negatives to zero
Enz2$SubCon <- Enz2$SubCon-Enz2$Buffer
Enz2$SubCon <- sapply(Enz2$SubCon,function(x)ifelse(x<0,0,x))
Enz2$Quench.coef <- Enz2$Quench/Enz2$Stan

# Calculate a group mean substrate control by enzyme, concentration, and temperature
Enz2 <- Enz2 %>% group_by(Enz,Concen,Temp) %>% mutate(Mean.SubCon=mean(SubCon,na.rm=T))
Enz2 <- data.frame(Enz2)

# Remove NA substrate controls that hinder later calculations
Enz2 <- Enz2[!is.na(Enz2$SubCon),]
# Oxidase assay index
ox <- Enz2$Enz %in% c("PPO","OX")

# For oxidases, if the substrate control is higher than the mean value, set it to the mean value
# Truncates out most of the bubble effects on these controls
Enz2$SubCon[ox & Enz2$SubCon > Enz2$Mean.SubCon] <- Enz2$Mean.SubCon[ox & Enz2$SubCon > Enz2$Mean.SubCon]

Enz2$Plate.Date[is.na(Enz2$Plate.Date)] <- "000000"

# Substrate not added
Enz2$Assay[Enz2$Plate.Date=="160929" & Enz2$Enz=="BG"] <- NA

# Columns reversed
Enz2$Concen[Enz2$Plate.Date=="161004" & Enz2$Enz=="AP"] <- Enz2$Concen[Enz2$Plate.Date=="161004" & Enz2$Enz=="AP"]/4
Enz2$Concen[Enz2$Plate.Date=="161004" & Enz2$Enz=="AG"] <- Enz2$Concen[Enz2$Plate.Date=="161004" & Enz2$Enz=="AG"]*4
Enz2$Enz[Enz2$Plate.Date=="161004" & Enz2$Enz=="AP"] <- "XX"
Enz2$Enz[Enz2$Plate.Date=="161004" & Enz2$Enz=="AG"] <- "AP"
Enz2$Enz[Enz2$Plate.Date=="161004" & Enz2$Enz=="XX"] <- "AG"

Enz2$Concen[Enz2$Plate.Date=="161117" & Enz2$Enz=="AP"] <- Enz2$Concen[Enz2$Plate.Date=="161117" & Enz2$Enz=="AP"]/2
Enz2$Concen[Enz2$Plate.Date=="161117" & Enz2$Enz=="BX"] <- Enz2$Concen[Enz2$Plate.Date=="161117" & Enz2$Enz=="BX"]*2
Enz2$Enz[Enz2$Plate.Date=="161117" & Enz2$Enz=="AP"] <- "XX"
Enz2$Enz[Enz2$Plate.Date=="161117" & Enz2$Enz=="BX"] <- "AP"
Enz2$Enz[Enz2$Plate.Date=="161117" & Enz2$Enz=="XX"] <- "BX"

# Error in MUB standard; use next day's values
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==4 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==4 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==10 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==10 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==16 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==16 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==22 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==22 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==28 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==28 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])
Enz2$Stan[Enz2$Plate.Date=="160928" & Enz2$Temp==34 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")] <-
  mean(Enz2$Stan[Enz2$Plate.Date=="160929" & Enz2$Temp==34 & Enz2$Enz %in% c("AG","AP","BG","BX","CBH","NAG")])

# 16 deg incubator was actually 12 deg for these samples for T1:
Temp.12 <- c("2D02","2D01","3D03","3D04","5D03","4S01","4P02","5W04","4S03","5W01","5G02","5G04","2S01","5S01","1G03",
  "2G04","3G04","5P02","5P03","5P04","3P03","3P01","1P02","2P02","2P04","4P04","4P01","4D04","1D01","1W03",
  "1W01","5G03","3W02","4D03","2G03","4W01","2S02","4G01","2G02","5S02","2S04","2W02","3S01")

Enz2$Temp[Enz2$Timepoint==1 & Enz2$Temp==16 & Enz2$ID %in% Temp.12] <- 12

# 4 degree plate for 1S01 timepoint 2 had no homogenate blank, so used 10 degree values

# 2D02 used only 90 ml buffer in homogenate instead of 150 ml
i <- Enz2$Sample=="2D" & Enz2$Rep==2 & Enz2$Timepoint==1

# Attach sorted data frame
attach(Enz2)
#calculate the activity in umol/h/g units based on 3.125 nmol standard and MUB.dry g sample per 150 ml homogenate
Enz2$Activity <- ((Assay-Blank-SubCon*Quench.coef)/(Stan*Quench.coef/3.125))/(Time*MUB.dry/150*0.125)/1000
Enz2$Activity[i] <- ((Assay[i]-Blank[i]-SubCon[i]*Quench.coef[i])/(Stan[i]*Quench.coef[i]/3.125))/(Time[i]*MUB.dry[i]/90*0.125)/1000

#calculate the activity in umol/h/g units based on EC per umol and OX.dry g sample per 75 ml homogenate
Enz2$Activity[ox] <- ((Assay[ox]-Blank[ox]-SubCon[ox]*Quench.coef[ox])/(Stan[ox]*Quench.coef[ox]/1))/(Time[ox]*OX.dry[ox]/75*0.125)
detach(Enz2)

enzymes <- c("AG","AP","BG","BX","CBH","LAP","NAG","PPO","OX")
Enz2$Temp <- factor(Enz2$Temp)
Enz2$Hour <- round(Enz2$Time)

# Function that reverses the concentration values to fix pipetting errors on the plates
# Warning: only apply once!
rev.concen <- function(df1,sam,rep,enz) {
  rows <- df1$Sample == sam & df1$Rep == rep & df1$Enz == enz
  df <- df1[rows,]
  df <- df[order(df$Concen),]
  Concen.vec <- df$Concen[order(df$Concen,decreasing=T)]
  df$Concen <- Concen.vec
  df1[rows,] <- df
  df1
}

Enz2 <- rev.concen(Enz2,"Y4",2,"AP")
Enz2 <- rev.concen(Enz2,"Y4",2,"BX")
Enz2 <- rev.concen(Enz2,"Y4",2,"CBH")
Enz2 <- rev.concen(Enz2,"Y4",2,"LAP")
Enz2 <- rev.concen(Enz2,"Y4",2,"NAG")
Enz2 <- rev.concen(Enz2,"ZD",2,"AP")
Enz2 <- rev.concen(Enz2,"ZD",2,"BG")
Enz2 <- rev.concen(Enz2,"ZD",2,"BX")
Enz2 <- rev.concen(Enz2,"ZD",2,"CBH")
Enz2 <- rev.concen(Enz2,"ZD",2,"LAP")
Enz2 <- rev.concen(Enz2,"ZD",2,"NAG")
Enz2 <- rev.concen(Enz2,"ZG",1,"AP")
Enz2 <- rev.concen(Enz2,"ZG",1,"BG")
Enz2 <- rev.concen(Enz2,"ZG",1,"BX")
Enz2 <- rev.concen(Enz2,"ZG",1,"CBH")
Enz2 <- rev.concen(Enz2,"ZG",1,"LAP")
Enz2 <- rev.concen(Enz2,"ZG",1,"NAG")
Enz2 <- rev.concen(Enz2,"ZG",2,"LAP")
Enz2 <- rev.concen(Enz2,"ZG",4,"NAG")
Enz2 <- rev.concen(Enz2,"ZP",1,"AG")

hist(Enz2$Buffer[Enz2$Enz %in% c("PPO","OX") & Enz2$Timepoint > 0])
hist(Enz2$Blank[Enz2$Enz %in% c("PPO","OX") & Enz2$Timepoint > 0])
hist(Enz2$SubCon[Enz2$Enz %in% c("PPO") & Enz2$Temp == 34 & Enz2$Concen == 2500])
hist(Enz2$SubCon[Enz2$Enz %in% c("PPO","OX") & Enz2$Timepoint > 0])

Enz.T0 <- Enz2[Enz2$Timepoint==0,]
Enz.T1 <- Enz2[Enz2$Timepoint==1,]
Enz.T2 <- Enz2[Enz2$Timepoint==2,]

pdf("M-M curves0.pdf",height=10,width=50)
d <- ggplot(Enz.T0[!is.na(Enz.T0$Activity),], aes(Concen, Activity, color=Temp)) + geom_point(size=0.5)
d + facet_grid(Enz ~ ID + Rep, scales="free")
dev.off()

pdf("M-M curves1.pdf",height=10,width=80)
d <- ggplot(Enz.T1[!is.na(Enz.T1$Activity),], aes(Concen, Activity, color=Temp)) + geom_point(size=0.5)
d + facet_grid(Enz ~ ID + Rep, scales="free")
dev.off()

pdf("M-M curves2.pdf",height=10,width=80)
d <- ggplot(Enz.T2[!is.na(Enz.T2$Activity),], aes(Concen, Activity, color=Temp)) + geom_point(size=0.5)
d + facet_grid(Enz ~ ID + Rep, scales="free")
dev.off()


#########################################################################################################

# Remove missing values that cause plotting errors
Enz3 <- Enz2[!is.na(Enz2$Activity),]

Enz3 <- Enz3[order(Enz3$Enz,Enz3$Timepoint,Enz3$Sample,Enz3$Rep,Enz3$Temp,Enz3$Concen),]

Enz3$dep <- Enz3$Activity

attach(Enz3)
#fit the M-M equation
#use try() to skip over fits that fail
pdf("fits.pdf")
x.len <- seq(len=dim(Enz3)[1])
param <- tapply(x.len, list(Temp,Rep,Sample,Enz,Timepoint), 
                function(i){
                  nls.m <- try(nls(y~Vmax*x/(Km+x),data=list(y=dep[i],x=Concen[i]),start=list(Vmax=max(dep[i],na.rm=T),Km=max(Concen[i]/4,na.rm=T))))
                  Vmax <- try(coef(nls.m)[[1]],silent=T)
                  Km <- try(coef(nls.m)[[2]],silent=T)
                  lowerV <- try(confint2(nls.m)[[1]],silent=T)
                  upperV <- try(confint2(nls.m)[[3]],silent=T)
                  lowerK <- try(confint2(nls.m)[[2]],silent=T)
                  upperK <- try(confint2(nls.m)[[4]],silent=T)
                  fit <- try(100*(upperV - lowerV)/Vmax,silent=T)
                  model.pts <- try(Vmax*Concen[i]/(Km+Concen[i]),silent=T)
                  plot(dep[i]~Concen[i],main=paste("Time:",Timepoint[i][1]," Enz:",Enz[i][1]," Sample:",Sample[i][1]," Rep:",Rep[i][1]," Temp:",Temp[i][1],sep=""))
                  try(lines(model.pts~Concen[i],col="red"),silent=T)
                  text(Concen[i],dep[i],Concen[i],pos=4)
                  mtext(paste("Vmax=",ifelse(is.numeric(Vmax),round(Vmax,3),NA)," Km=",ifelse(is.numeric(Km),round(Km,3),NA),
                              " Fit=",ifelse(is.numeric(fit),round(fit,3),NA),sep=""))
                  c(Vmax,Km,lowerV,upperV,lowerK,upperK)
                })
dev.off()

params.frame <- as.data.frame.table(tapply(x.len, list(Temp,Rep,Sample,Enz,Timepoint),function(i)1))
names(params.frame) <- c("Temp","Rep","Sample","Enz","Timepoint","Vmax")
detach(Enz3)

#extract the Vmax and Km values
params.frame$Vmax <- as.numeric(list.as.vector(sapply(sapply(param,'[',1),'[[',1)))
params.frame$Km <- as.numeric(list.as.vector(sapply(sapply(param,'[',2),'[[',1)))
params.frame$lowerV <- as.numeric(list.as.vector(sapply(sapply(param,'[',3),'[[',1)))
params.frame$upperV <- as.numeric(list.as.vector(sapply(sapply(param,'[',4),'[[',1)))
params.frame$lowerK <- as.numeric(list.as.vector(sapply(sapply(param,'[',5),'[[',1)))
params.frame$upperK <- as.numeric(list.as.vector(sapply(sapply(param,'[',6),'[[',1)))
params.frame$fitV <- (params.frame$upperV - params.frame$lowerV)/params.frame$Vmax
params.frame$fitK <- (params.frame$upperK - params.frame$lowerK)/params.frame$Km
#convert temperature to a numeric value
params.frame$Temp <- as.numeric(as.vector(params.frame$Temp))

params.frame <- subset(params.frame,!is.na(Vmax))
write.csv(params.frame,"params.frame.csv",row.names=F)
params.frame <- read.csv("params.frame.csv")

# Insert NA for incorrect model fits (negative Km) or poor fits
params.frame[(!is.na(params.frame$Km) & params.frame$Km<0),5:dim(params.frame)[2]] <- NA
params.frame[(!is.na(params.frame$fitV) & params.frame$fitV>2),5:dim(params.frame)[2]] <- NA
# Km is harder to constrain, so use NA if Km fit is below threshold
params.frame[(!is.na(params.frame$fitK) & params.frame$fitK>4),"Km"] <- NA
params.frame <- params.frame[!is.na(params.frame$Vmax),]

params.frame$logVmax <- log(params.frame$Vmax)
params.frame$logKm <- log(params.frame$Km)
write.csv(params.frame,"params.frame.2.csv",row.names=F)

params.frame <- read.csv("params.frame.2.csv")
params.frame$Site <- gsub("[[:alpha:]]*$","",params.frame$Sample)
params.frame$Site <- gsub("1","D",params.frame$Site)
params.frame$Site <- gsub("4","W",params.frame$Site)
params.frame$Site <- gsub("2","G",params.frame$Site)
params.frame$Site <- gsub("3","P",params.frame$Site)
params.frame$Site <- gsub("5","S",params.frame$Site)
params.frame$MicrobialOrigin <- gsub("^[[:digit:]]+","",params.frame$Sample)

pdf("Vmax.pdf",height=20,width=150)
d <- ggplot(params.frame, aes(Temp, Vmax)) + geom_point() + geom_line()
d + facet_grid(Enz ~ Timepoint + Sample + Rep, scales="free")
dev.off()

Time1 <- params.frame[params.frame$Timepoint==1 & params.frame$MicrobialOrigin %in% c("D","W","G","P","S"),]

sink("T1_ANOVA_170330.txt")
for (i in c("AG","AP","BG","BX","CBH","LAP","NAG")){
m <- lm(logVmax~Site*MicrobialOrigin*Temp,data=Time1[Time1$Enz==i,],contrasts=list(Site=contr.sum,MicrobialOrigin=contr.sum))
cat(paste("\n",i,"\n"))
print(Anova(m,type=3))
}

for (i in c("AG","AP","BG","BX","CBH","LAP","NAG")){
  m <- lm(logKm~Site*MicrobialOrigin*Temp,data=Time1[Time1$Enz==i,],contrasts=list(Site=contr.sum,MicrobialOrigin=contr.sum),na.action=na.omit)
  cat(paste("\n",i,"\n"))
  print(Anova(m,type=3))
}
sink()

Time1$Site <- factor(Time1$Site, levels(factor(Time1$Site))[c(1,5,2,3,4)])
Time1$MicrobialOrigin <- factor(Time1$MicrobialOrigin, levels(factor(Time1$MicrobialOrigin))[c(1,5,2,3,4)])

pdf("Vmax2.pdf",height=6,width=6)
tapply(1:dim(Time1)[1], list(Time1$Enz),
       function(i){
  ggplot(Time1[i,], aes(x=NA,logVmax)) +
  stat_summary(fun.data = "mean_se", geom = "errorbar", width=0.1) +
  stat_summary(fun.y=mean, geom = "point") +
  facet_grid(Site~MicrobialOrigin, scales="fixed") +
           labs(title = Time1$Enz[i][1]) + xlab(NULL) + 
           theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
})
dev.off()

