#Sept 25, 2017
#got fungal hyphal abundance data from Caitlin Looby

#have T1 samples and most of T2
#16S library
#Reset R's Brain
rm(list=ls())

#install.packages("plyr")
library(plyr )
#install.packages("stringr")
library(stringr)

#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")

######################################################
########Upload data
######################################################

#load in inoculum bacterial and fungal abundance data from claudia and caitlin
abund <- read.csv("data/FungiandBacteriaAbundance.csv", header=TRUE)

#check out histogram of fungal hyphal abundance
hist(abund$hyphae_cm.gdrylitter)
#log it
abund$logfungi <- log(abund$hyphae_cm.gdrylitter)
hist(abund$logfungi) #looks better

######################################################
########separate the T1 transplant samples
######################################################
T1samples <- which(abund$Timepoint == 1)
names(abund)
#subset out just T1 samples and only columns of interest
Fungi_T1 <- abund[T1samples, c(1,3,9,10,11,12,13) ]
Fungi_T1$sampleID 

#remove samples where there i no fungal abundance, AKA remove NAs, but only based on last 2 columns
Fungi_T1 <- Fungi_T1 [complete.cases(Fungi_T1 [ , 5:7]),]

#check that sample IDs are the same 
as.character(Fungi_T1$sampleID) == Fungi_T1$sampleID2

#look at overview plots of samples averaged by site and remove outliers
par(mfrow=c(1,1))
plot(Fungi_T1$hyphae_cm.gdrylitter~Fungi_T1$Location, las=3, xlab="")
plot(Fungi_T1$logfungi~Fungi_T1$Location, las=3, xlab="")

######################################################
########separate the T2 transplant samples
######################################################
T2samples <- which(abund$Timepoint == 2)

#subset out just T2 samples and only columns of interest
Fungi_T2 <- abund[T2samples, c(1,3,9,10,11,12,13) ]
Fungi_T2$sampleID 

#remove samples where there i no fungal abundance, AKA remove NAs, but only based on last 2 columns
Fungi_T2 <- Fungi_T2[complete.cases(Fungi_T2[ , 5:7]),]

#check that sample IDs are the same 
as.character(Fungi_T2$sampleID) == Fungi_T2$sampleID2
Fungi_T2[28, ] #this is a typo from caitlin's spreadsheet I think
Fungi_T2[9, ] #this is a typo from caitlin's spreadsheet I think

#look at overview plots of samples averaged by site and remove outliers
par(mfrow=c(1,1))
range(Fungi_T1$hyphae_cm.gdrylitter)
range(Fungi_T2$hyphae_cm.gdrylitter)

plot(Fungi_T1$hyphae_cm.gdrylitter~Fungi_T1$Location, las=3, xlab="")
points(Fungi_T2$hyphae_cm.gdrylitter~Fungi_T2$Location, las=3, xlab="", col="red")
# interesting T2 is lower abundance than T1 - that is odd

plot(Fungi_T1$logfungi~Fungi_T1$Location, las=3, xlab="",ylim=c(2,9))
points(Fungi_T2$logfungi~Fungi_T2$Location, las=3, xlab="", col="red")

#######################################################################
########Do ANOVA for T1 to dettermine if there are site * inoculum differences
############################################################################
Fungi_T1$Site <-str_sub(Fungi_T1$sampleID,1,1) 
Fungi_T1$Site <- as.factor(Fungi_T1$Site)
length(which(Fungi_T1$Site=="1"))
#1 - desert,  2- grassland, 3- OakPine (P) AKA Pine-Oak, 4- Woodland AKA Scrubland, 5 AKA Subalpine
Fungi_T1$Sitenames <- c(rep("Desert", length(which(Fungi_T1$Site=="1"))),rep("Grassland", length(which(Fungi_T1$Site=="2"))), 
                        rep("Pine-Oak", length(which(Fungi_T1$Site=="3"))),rep("Scrubland", length(which(Fungi_T1$Site=="4"))),
                        rep("Subalpine", length(which(Fungi_T1$Site=="5"))))

Fungi_T1$colors <- c(rep("red", length(which(Fungi_T1$Site=="1"))),rep("green", length(which(Fungi_T1$Site=="2"))), 
                        rep("blue", length(which(Fungi_T1$Site=="3"))),rep("orange", length(which(Fungi_T1$Site=="4"))),
                        rep("purple", length(which(Fungi_T1$Site=="5"))))

tail(Fungi_T1)
Fungi_T1

#make a column of the inoculum
Fungi_T1$Inoculum <- str_sub(Fungi_T1$sampleID,2,2)
Fungi_T1$Inoculum <- as.factor(Fungi_T1$Inoculum)

#make a column for the combination site*inoculum treatment
Fungi_T1$Combo <- str_sub(Fungi_T1$sampleID,1,2)

#put sitenames in proper order
Fungi_T1$Sitenames<- factor(Fungi_T1$Sitenames, levels = c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine") ) 

#Do ANOVA model to test effect of inoculum and site
model_T1<-aov(hyphae_cm.gdrylitter~Site*Inoculum,  data=Fungi_T1 )
summary(model_T1)

#Do ANOVA model to test effect of inoculum and site for log fungi
model_T1<-aov(logfungi~Site*Inoculum,  data=Fungi_T1 )
summary(model_T1) 

#specify controls
controls <- which(Fungi_T1$Inoculum=="C")
L <- which(Fungi_T1$Inoculum=="L")
N <- which(Fungi_T1$Inoculum=="N")

#remove controls from map
Fungi_T1_nc<- Fungi_T1[-c(controls,L,N), ]

####re-do ANOVA with controls removed
#make sure factors are correct classes
Fungi_T1_nc$Sitenames <- as.factor(Fungi_T1_nc$Sitenames)
Fungi_T1_nc$Inoculum <- as.factor(Fungi_T1_nc$Inoculum)
Fungi_T1_nc$Site <- as.factor(Fungi_T1_nc$Site)
Fungi_T1_nc$colors <- as.character(Fungi_T1_nc$colors)
sitecolors <- c("red","orange","green","blue","purple")

#Do ANOVA model to test effect of inoculum and site
T1_nocontrols <-aov(hyphae_cm.gdrylitter~Sitenames*Inoculum,  data=Fungi_T1_nc)
summary(T1_nocontrols)

#Do ANOVA model to test effect of inoculum and site on log fungi
T1_nocontrols <-aov(logfungi~Sitenames*Inoculum,  data=Fungi_T1_nc)
summary(T1_nocontrols) #doesn't make a difference

#figure out which samples are different
TukeyHSD(T1_nocontrols)
#for site, just pine oak is higher than desert or scrubland 

#export it to wordfile
capture.output(summary(T1_nocontrols),file="results/T1_fungalabundance_nocontrols.doc")


#######################################################################
########Make figures of fungal abundance at T1 by site and by inoculum
############################################################################

pdf("Figures/microbialabundance/fungi_T1_abundancebysite.pdf")
plot(hyphae_cm.gdrylitter ~ Sitenames, ylab = "T1 Fungal abundane cm hyphae/g dry leaf litter",xlab="", col=sitecolors, data = Fungi_T1_nc, las=3 )
dev.off()

pdf("Figures/microbialabundance/logfungi_T1_abundancebysite.pdf")
plot(logfungi ~ Sitenames, ylab = "T1 Fungal abundane cm hyphae/g dry leaf litter", xlab="", col=sitecolors, data = Fungi_T1_nc, las=3)
dev.off()

pdf("Figures/microbialabundance/fungi_T1_abundancebyinoc.pdf")
plot(hyphae_cm.gdrylitter ~ Inoculum, ylab = "T1 Fungal abundane cm hyphae/g dry leaf litter", data = Fungi_T1_nc, las=3 )
dev.off()

pdf("Figures/microbialabundance/logfungi_T1_abundancebyinoc.pdf")
plot(logfungi ~ Inoculum, ylab = "T1 Fungal abundane cm hyphae/g dry leaf litter", data = Fungi_T1_nc, las=3 )
dev.off()


#######################################################################
########Do ANOVA for T2 to dettermine if there are site * inoculum differences
###########################################################################
Fungi_T2$Site <-str_sub(Fungi_T2$sampleID,1,1) 
Fungi_T2$Site <- as.factor(Fungi_T2$Site)
length(which(Fungi_T2$Site=="1"))
#1 - desert,  2- grassland, 3- OakPine (P) AKA Pine-Oak, 4- Woodland AKA Scrubland, 5 AKA Subalpine
Fungi_T2$Sitenames <- c(rep("Desert", length(which(Fungi_T2$Site=="1"))),rep("Grassland", length(which(Fungi_T2$Site=="2"))), 
                        rep("Pine-Oak", length(which(Fungi_T2$Site=="3"))),rep("Scrubland", length(which(Fungi_T2$Site=="4"))),
                        rep("Subalpine", length(which(Fungi_T2$Site=="5"))))

Fungi_T2$colors <- c(rep("red", length(which(Fungi_T2$Site=="1"))),rep("green", length(which(Fungi_T2$Site=="2"))), 
                     rep("blue", length(which(Fungi_T2$Site=="3"))),rep("orange", length(which(Fungi_T2$Site=="4"))),
                     rep("purple", length(which(Fungi_T2$Site=="5"))))

tail(Fungi_T2)
Fungi_T2

#make a column of the inoculum
Fungi_T2$Inoculum <- str_sub(Fungi_T2$sampleID,2,2)
Fungi_T2$Inoculum <- as.factor(Fungi_T2$Inoculum)

#make a column for the combination site*inoculum treatment
Fungi_T2$Combo <- str_sub(Fungi_T2$sampleID,1,2)

#put sitenames in proper order
Fungi_T2$Sitenames<- factor(Fungi_T2$Sitenames, levels = c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine") ) 

#Do ANOVA model to test effect of inoculum and site
model_T2<-aov(hyphae_cm.gdrylitter~Site*Inoculum,  data=Fungi_T2 )
summary(model_T2)

#Do ANOVA model to test effect of inoculum and site for log fungi
model_T2<-aov(logfungi~Site*Inoculum,  data=Fungi_T2 )
summary(model_T2) 

#specify controls
controls <- which(Fungi_T2$Inoculum=="C")
L <- which(Fungi_T2$Inoculum=="L")
N <- which(Fungi_T2$Inoculum=="N")

#remove controls from map
Fungi_T2_nc<- Fungi_T2[-c(controls,L,N), ]

####re-do ANOVA with controls removed
#make sure factors are correct classes
Fungi_T2_nc$Sitenames <- as.factor(Fungi_T2_nc$Sitenames)
Fungi_T2_nc$Inoculum <- as.factor(Fungi_T2_nc$Inoculum)
Fungi_T2_nc$Site <- as.factor(Fungi_T2_nc$Site)
Fungi_T2_nc$colors <- as.character(Fungi_T2_nc$colors)
sitecolors <- c("red","orange","green","blue","purple")

#Do ANOVA model to test effect of inoculum and site
T2_nocontrols <-aov(hyphae_cm.gdrylitter~Sitenames*Inoculum,  data=Fungi_T2_nc)
summary(T2_nocontrols)

#Do ANOVA model to test effect of inoculum and site on log fungi
T2_nocontrols <-aov(logfungi~Sitenames*Inoculum,  data=Fungi_T2_nc)
summary(T2_nocontrols) #doesn't make a difference

#figure out which samples are different
TukeyHSD(T2_nocontrols)
#for site, just grassland is higher than desert and subalpibe is higher than desert or scrubland 

#export it to wordfile
capture.output(summary(T2_nocontrols),file="results/T2_fungalabundance_nocontrols.doc")

#######################################################################
########Make figures of fungal abundance at T2 by site and by inoculum
############################################################################

pdf("Figures/microbialabundance/fungi_T2_abundancebysite.pdf")
plot(hyphae_cm.gdrylitter ~ Sitenames, ylab = "T2 Fungal abundane cm hyphae/g dry leaf litter",xlab="", col=sitecolors, data = Fungi_T2_nc, las=3 )
dev.off()

pdf("Figures/microbialabundance/logfungi_T2_abundancebysite.pdf")
plot(logfungi ~ Sitenames, ylab = "T2 Fungal abundane cm hyphae/g dry leaf litter", xlab="", col=sitecolors, data = Fungi_T2_nc, las=3)
dev.off()

pdf("Figures/microbialabundance/fungi_T2_abundancebyinoc.pdf")
plot(hyphae_cm.gdrylitter ~ Inoculum, ylab = "T2 Fungal abundane cm hyphae/g dry leaf litter", data = Fungi_T2_nc, las=3 )
dev.off()

pdf("Figures/microbialabundance/logfungi_T2_abundancebyinoc.pdf")
plot(logfungi ~ Inoculum, ylab = "T2 Fungal abundane cm hyphae/g dry leaf litter", data = Fungi_T2_nc, las=3 )
dev.off()

#######################################################################
#########get mean, SD, SE of T1 and T2 by site by inoculum for fungi without controls
############################################################################

# Calculate the means, sd, n, and se.
fungalAbundT1 <- ddply(Fungi_T1_nc, "Combo", summarise,
                       T1_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T1_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T1_n = sum(!is.na(hyphae_cm.gdrylitter)),
                       T1_se = T1_sd/sqrt(T1_n)
)





fungalAbundT2 <- ddply(Fungi_T2_nc, "Combo", summarise,
                       T2_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T2_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T2_n = sum(!is.na(hyphae_cm.gdrylitter)),
                       T2_se = T2_sd/sqrt(T2_n)
)


#make a function to make the dataframe how I want it
fungalfunction <-function(fungalabund) {
  fungalabund$Site <- str_sub(fungalabund$Combo,1,1)
  fungalabund$Inoculum <- str_sub(fungalabund$Combo,2,2)
  #make list of site names that matches 
  fungalabund$sitenames <- c(rep("Desert",5),rep("Grassland",5),rep("Pine-Oak",5),rep("Scrubland",5), rep("Subalpine",5))
  #make list of colors that matches the inoculum
  fungalabund$colors <- rep(c("red","green","blue","purple","orange"),5)
  #make a list of site orders that matches
  fungalabund$siteorder <- c(rep(1,5),rep(3,5),rep(4,5),rep(2,5),rep(5,5))
  #order in correct order
  fungalabund2 <- fungalabund[order(fungalabund$siteorder), ]
  return(fungalabund2)
}

fungalAbundT1 <- fungalfunction(fungalAbundT1)
fungalAbundT2 <- fungalfunction(fungalAbundT2)


#Plot the avg fungal abundance by site by inoculum
plot(T1_mean~siteorder, data=fungalAbundT1 , ylab="Mean Fungal Abundance T1",col=colors, pch=16)
colors <- c("red","orange","green","blue","purple")
legend("topleft",legend=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),col=colors,pch=16, bty="n")


#Plot the avg fungal abundance by site by inoculum
plot(T2_mean~siteorder, data=fungalAbundT2 , ylab="Mean Fungal Abundance T2",col=colors, pch=16)
colors <- c("red","orange","green","blue","purple")
legend("topleft",legend=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),col=colors,pch=16, bty="n")


####################################################################################
#Make fungal abundance function correct order, labels, lines, with no controls
####################################################################################
#order data by site order now
par(mfrow=c(1,1))

#vectors for legend
sitenames1 <- c("Desert","Scrubland","Grassland","Pine-oak","Subalpine")
sitecolors1 <- c("red","orange","green","blue","purple")

fungalabundfigurefunction <- function(fungalabund,map, ylab, ylim){
  # Restore default clipping rect
  par(mar=c(5, 4, 2, 2) + 0.1)
  #Plot the avg decomp by site by inoculum
  plot(fungalabund~map$siteorder, ylab=ylab,col=map$colors, pch=16, xlab="", ylim=ylim)
  #save pdf of plot
  #pdf(paste("Figures/Decomposition_",name,".pdf"))
  #add legend
  legend("topleft",title="Inoculum",legend=sitenames1,col=sitecolors1 ,pch=16,bty="n")
  #add site names to x axis
  mtext("Desert", side=1, line=2, adj=0)
  mtext("Subalpine", side=1, line=2, adj=1)
  mtext("Grassland", side=1, line=2, adj=0.5)
  mtext("Scrubland", side=1, line=2, adj=0.25)
  mtext("Pine-Oak", side=1, line=2, adj=0.75)
  #subset the dataframe by inoculum
  D <- map[map$Inoculum=="D", ]
  G <- map[map$Inoculum=="G", ]
  P <- map[map$Inoculum=="P", ]
  S <- map[map$Inoculum=="S", ]
  Sc <- map[map$Inoculum=="W", ]
  #add lines to connect inoculum
  lines(y=D[ ,2],x=D$siteorder,type="l", col=D$colors)
  lines(y=G[ ,2],x=G$siteorder,type="l", col=G$colors)
  lines(y=P[ ,2],x=P$siteorder,type="l", col=P$colors)
  lines(y=S[ ,2],x=S$siteorder,type="l", col=S$colors)
  lines(y=Sc[ ,2],x=Sc$siteorder,type="l", col=Sc$colors)
  
  #how to put in standard error bars
  x <- map$siteorder
  y <- map[ ,2]
  sd <- map[,5]
  
  #use arrows command to put in SE bars
  arrows(x,y-sd,x,y+sd, code=3, length=0.02, angle = 90,col=map$colors)
  #dev.off()
  
}

#find the max value for the y axis
maxvalue <- sum(max(fungalAbundT1$T1_mean),max(fungalAbundT1$T1_se))

pdf("Figures/microbialabundance/fungalabund_T1.pdf",height=6, width=8, pointsize=10)
fungalabundfigurefunction(fungalabund=fungalAbundT1$T1_mean,map=fungalAbundT1,ylab="T1 Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue))
dev.off()

pdf("Figures/microbialabundance/fungalabund_T2.pdf",height=6, width=8, pointsize=10)
fungalabundfigurefunction(fungalabund=fungalAbundT2$T2_mean,map=fungalAbundT2,ylab="T2 Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue))
dev.off()


####################################################################################
#Plot T1 and T2 in one panel
####################################################################################

pdf("Figures/microbialabundance/fungalAbund_byinocandsite_T1andT2_sidebyside_withSEbars.pdf", height=8,width=18,pointsize=20)
par(mfrow=c(1,2))
#make plot T1
fungalabundfigurefunction(fungalabund=fungalAbundT1$T1_mean,map=fungalAbundT1,ylab="Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue))
#add T1 label
mtext("T1", side=3, line=1, adj=0.5)
#make plot T2
fungalabundfigurefunction(fungalabund=fungalAbundT2$T2_mean,map=fungalAbundT2,ylab="Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue))
#add T2 label
mtext("T2", side=3, line=1, adj=0.5)
dev.off()


#####################################################################################
#########get mean, SD, SE of T1 and T2 by site by inoculum for fungi WITH controls
#################################################################################

# Calculate the means, sd, n, and se.
fungalAbundT1_wc <- ddply(Fungi_T1, "Combo", summarise,
                       T1_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T1_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T1_n = sum(!is.na(hyphae_cm.gdrylitter)),
                       T1_se = T1_sd/sqrt(T1_n)
)





fungalAbundT2_wc <- ddply(Fungi_T2, "Combo", summarise,
                       T2_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T2_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                       T2_n = sum(!is.na(hyphae_cm.gdrylitter)),
                       T2_se = T2_sd/sqrt(T2_n)
)

fungalAbundT1_wc
#make a function to make the dataframe how I want it
fungalfunctionwc <-function(fungalabund) {
  fungalabund$Site <- str_sub(fungalabund$Combo,1,1)
  fungalabund$Inoculum <- str_sub(fungalabund$Combo,2,2)
  #make list of site names that matches 
  fungalabund$sitenames <- c(rep("Desert",8),rep("Grassland",8),rep("Pine-Oak",8),rep("Scrubland",8), rep("Subalpine",8))
  #make list of colors that matches the inoculum
  fungalabund$colors <- rep(c("darkgrey","red","green","burlywood2","cornsilk4","blue","purple","orange"),5)
  #make a list of site orders that matches
  fungalabund$siteorder <- c(rep(1,8),rep(3,8),rep(4,8),rep(2,8),rep(5,8))
  #order in correct order
  fungalabund2 <- fungalabund[order(fungalabund$siteorder), ]
  return(fungalabund2)
}

fungalAbundT1_wc <- fungalfunctionwc(fungalAbundT1_wc)
fungalAbundT2_wc <- fungalfunctionwc(fungalAbundT2_wc)

par(mfrow=c(1,1))
#Plot the avg fungal abundance by site by inoculum
plot(T1_mean~siteorder, data=fungalAbundT1_wc , ylab="Mean Fungal Abundance T1",col=colors, pch=16)

sitecolors2 <- c("red","orange","green","blue","purple","burlywood2","darkgrey","cornsilk4")
sitenames2 <- c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine","L=in-situ open","C=controls","N=in-situ closed")

legend("topleft",legend=sitenames2,col=sitecolors2,pch=16, bty="n")


#Plot the avg fungal abundance by site by inoculum
plot(T2_mean~siteorder, data=fungalAbundT2_wc , ylab="Mean Fungal Abundance T2",col=colors, pch=16)
legend("topleft",legend=sitenames2,col=sitecolors2,pch=16, bty="n")


####################################################################################
#Make fungal abundance function correct order, labels, lines, w controls
####################################################################################

fungalfigurefunctionwc <- function(massloss,map, ylab, ylim){

  #Plot the avg decomp by site by inoculum
  plot(massloss~map$siteorder, ylab=ylab,col=map$colors, pch=16, xlab="", ylim=ylim)
  #save pdf of plot
  #pdf(paste("Figures/Decomposition_",name,".pdf"))
  #add legend
  legend("topleft",y=number,title="Inoculum",legend=sitenames2,col=sitecolors2 ,pch=16,bty="n")
  
  #add site names to x axis
  mtext("Desert", side=1, line=2, adj=0)
  mtext("Subalpine", side=1, line=2, adj=1)
  mtext("Grassland", side=1, line=2, adj=0.5)
  mtext("Scrubland", side=1, line=2, adj=0.25)
  mtext("Pine-Oak", side=1, line=2, adj=0.75)
  
  #subset the dataframe by inoculum
  D <- map[map$Inoculum=="D", ]
  G <- map[map$Inoculum=="G", ]
  P <- map[map$Inoculum=="P", ]
  S <- map[map$Inoculum=="S", ]
  Sc <- map[map$Inoculum=="W", ]
  N <- map[map$Inoculum=="N", ]
  L <- map[map$Inoculum=="L", ]
  C <- map[map$Inoculum=="C", ]
  
  #add lines to connect inoculum
  lines(y=D[ ,2],x=D$siteorder,type="l", col=D$colors)
  lines(y=G[ ,2],x=G$siteorder,type="l", col=G$colors)
  lines(y=P[ ,2],x=P$siteorder,type="l", col=P$colors)
  lines(y=S[ ,2],x=S$siteorder,type="l", col=S$colors)
  lines(y=Sc[ ,2],x=Sc$siteorder,type="l", col=Sc$colors)
  lines(y=N[ ,2],x=N$siteorder,type="l", col=N$colors)
  lines(y=L[ ,2],x=L$siteorder,type="l", col=L$colors)
  lines(y=C[ ,2],x=C$siteorder,type="l", col=C$colors)
  
  #how to put in standard error bars
  x <- map$siteorder
  y <- map[ ,2]
  sd <- map[,5]
  
  #use arrows command to put in SE bars
  arrows(x,y-sd,x,y+sd, code=3, length=0.02, angle = 90,col=map$colors)

}


#find the max value for the y axis
maxvalue2 <- sum(max(fungalAbundT1_wc$T1_mean),max(fungalAbundT1_wc$T1_se))

pdf("Figures/microbialabundance/fungalabund_T1_withcontrols.pdf",height=6, width=8, pointsize=10)
fungalfigurefunctionwc(massloss=fungalAbundT1_wc$T1_mean,map=fungalAbundT1_wc,ylab="T1 Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue2))
dev.off()

pdf("Figures/microbialabundance/fungalabund_T2_withcontrols.pdf",height=6, width=8, pointsize=10)
fungalfigurefunctionwc(massloss=fungalAbundT2_wc$T2_mean,map=fungalAbundT2_wc,ylab="2 Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue2))
dev.off()

####################################################################################
#Plot T1 and T2 in one panel
####################################################################################

pdf("Figures/microbialabundance/fungalAbund_byinocandsite_T1andT2_sidebyside_withSEbars_withcontrols.pdf", height=6,width=12,pointsize=10)
par(mfrow=c(1,2))
#make plot T1
fungalfigurefunctionwc(massloss=fungalAbundT1_wc$T1_mean,map=fungalAbundT1_wc,ylab="Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue2))
#add T1 label
mtext("T1", side=3, line=1, adj=0.5)
#make plot T2
fungalfigurefunctionwc(massloss=fungalAbundT2_wc$T2_mean,map=fungalAbundT2_wc,ylab="Mean Fungal Hyphal Abundance cm/g dry leaf litter", ylim=c(0,maxvalue2))
#add T2 label
mtext("T2", side=3, line=1, adj=0.5)
dev.off()



