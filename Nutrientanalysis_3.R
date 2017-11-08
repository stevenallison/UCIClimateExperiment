#October 2, 2017
#nutrient analysis for elevation gradient
#have nutrient data from http://www.foragelab.com/ for T2 transplant samples
#need to test if there are some interesting site*inoculum effects for various nutrients

#this time use nameer calculations - still not seeing how he got a g rather than a proportion

#how do I account for dry matter/wet weight/calculate per g of dry leaf liter? need to figure out calculation
#install.packages("plyr")
library(plyr )
#install.packages("stringr")
library(stringr)
library(multcomp)
#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")

######################################################
########Upload data
######################################################

#load in nutrient data from forage lab
nutrients <- read.csv("data/Nutrientdata_T2.csv", header=TRUE)

#lignin, cellulose (acid detergent fiber – lignin), hemicellulose (neutral detergent fiber – acid detergent fiber), 
#structural carbohydrates (non-fiber carbohydrates – NSC), non-structural carbs (NSC) and crude protein
names(nutrients)

######################################################
########Make calculations just as Nameer did them 
######################################################
#The key is that acid detergent fiber (ADF) is cellulose+lignin+ash, 
#and neutral detergent fiber (NDF) is hemicellulose+cellulose+lignin+ash.
#So ash is included in those two values, and the ash content they provide 
#needs to be subtracted out when you calculate non-ash (".na") percentages
#of total litter content for them. Then once you recalculate the non-ash 
#percentages for all the other fractions, you can just subtract lignin 
#from ADF to get cellulose, and ADF from NDF to get hemicellulose. 
#The other transformation is for structural carbohydrates, 
#which are non-fiber carbohydrates minus non-structural carbohydrates.
#non-strcutral carbodhydrates (non-fiber carbohydrates)

#there are a few samples where there is no Ash. Change these to 0 
which(is.na(nutrients$Ash))

#change these to 0
nutrients$Ash[26] <- 0
nutrients$Ash[36] <- 0

#re do analysis, but this time normalizing to account for proportion based on no ash
#calculate ADF acid detergent fiber (ADF) is cellulose+lignin+ash and remove ash proportion
#doesn't make a big diff bc ash is really small
nutrients$ADF.na <- ((nutrients$ADF-nutrients$Ash)/(100-nutrients$Ash))
cor.test(nutrients$ADF.na, nutrients$ADF)
nutrients$NDF.na <- ((nutrients$NDF-nutrients$Ash)/(100-nutrients$Ash))
cor.test(nutrients$NDF.na, nutrients$NDF)
#also super highly correlated bc ash is low
nutrients$NFC.na <- (nutrients$NFC)/(100-nutrients$Ash)
cor.test(nutrients$NFC.na, nutrients$NFC)
#also super highly correlated bc ash is low
nutrients$NSC.na <- (nutrients$NSC)/(100-nutrients$Ash)
nutrients$Lignin.na <- ((nutrients$Lignin)/(100-nutrients$Ash))
nutrients$Protein.na <- (nutrients$CP)/(100-nutrients$Ash)

nutrients$SCarb.na <- (nutrients$NFC.na - nutrients$NSC.na)
nutrients$Cellu.na <- (nutrients$ADF.na - nutrients$Lignin.na)
nutrients$Hemi.na <- (nutrients$NDF.na - nutrients$ADF.na)

cor.test(nutrients$Protein.na, nutrients$CP)
cor.test(nutrients$Lignin.na, nutrients$Lignin)

SCarb <- nutrients$NFC - nutrients$NSC
cor.test(nutrients$SCarb.na, nutrients$SCarb)
cor.test(nutrients$NSC.na, nutrients$NSC)

######################################################
########Put sites and inoculum in correct order
######################################################

class(nutrients$Site)
class(nutrients$Inoculum)

#put sitenames in proper order
nutrients$Site <- factor(nutrients$Site, levels = c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine") ) 
nutrients$Inoculum <- factor(nutrients$Inoculum, levels = c("D","W","G","P","S") ) 
#make a column for the combination site*inoculum treatment
nutrients$Combo <- str_sub(nutrients$SampleID,1,2)


######################################################
########calculations, anova, and plots for cellulose
######################################################
#confused how to calculate cellulose
#nameer papaer says ADF-lignin
#forage document says ADF- (ADL+Ash)
nutrients$cellulose <- nutrients$ADF - (nutrients$Lignin + nutrients$Ash)
#nameer paper: organic compounds of total non-ash dried litter mass
nutrients$cellulose <- nutrients$ADF - nutrients$Lignin
nutrients$Cellu.na <- (nutrients$ADF.na - nutrients$Lignin.na)

cor.test(nutrients$Cellu.na,nutrients$cellulose )

hist(nutrients$Cellu.na)

max(nutrients$Cellu.na)
min(nutrients$Cellu.na)

#do anova site*inoculum for cellulose
modelcellulose <- aov(Cellu.na  ~Site*Inoculum, data=nutrients)
summary(modelcellulose)

TukeyHSD(modelcellulose, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modelcellulose, "Site", ordered = TRUE)
plot(TukeyHSD(modelcellulose, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modelcellulose <- aov(Cellu.na ~Site, data=nutrients)
tuk <- glht(modelcellulose , linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.5,1), no.readonly=TRUE)
#plot
pdf("Figures/Nutrients/cellulosebysite_noashnoash.pdf")
old.par <- par(mai=c(1,1,2.5,1), no.readonly=TRUE)
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 
plot(Cellu.na ~Site, data=nutrients)
plot(Cellu.na ~Inoculum, data=nutrients)

#export it to wordfile
modelcellulose <- aov(Cellu.na  ~Site*Inoculum, data=nutrients)
summary(modelcellulose)
capture.output(summary(modelcellulose),file="results/T2_cellulose_noash.doc")
####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
littercellulose <- ddply(nutrients, "Combo", summarise,
                       T1_mean = mean(Cellu.na, na.rm=TRUE),
                       T1_sd = sd(Cellu.na, na.rm=TRUE),
                       T1_n = sum(!is.na(Cellu.na)),
                       T1_se = T1_sd/sqrt(T1_n)
)


####################################################################################
#Make function to make dataframe like I like it
####################################################################################

#make a function to make the dataframe how I want it
chemistryfunction <-function(fungalabund) {
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

littercelluloseT2 <- chemistryfunction(littercellulose)


####################################################################################
#Make function correct order, labels, lines, with no controls
####################################################################################
#order data by site order now
par(mfrow=c(1,1))

#vectors for legend
sitenames1 <- c("Desert","Scrubland","Grassland","Pine-oak","Subalpine")
sitecolors1 <- c("red","orange","green","blue","purple")

chemistryfigurefunction <- function(fungalabund,map, ylab, ylim){
  # Restore default clipping rect
  par(mar=c(5, 4, 2, 2) + 0.1)
  #Plot the avg decomp by site by inoculum
  plot(fungalabund~map$siteorder, ylab=ylab,col=map$colors, pch=16, xlab="", ylim=ylim)
  #save pdf of plot
  #pdf(paste("Figures/Decomposition_",name,"noash.pdf"))
  #add legend
  legend("bottomleft",title="Inoculum",legend=sitenames1,col=sitecolors1 ,pch=16,bty="n")
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
maxvalue <- sum(max(littercelluloseT2$T1_mean),max(littercelluloseT2$T1_se))
pdf("Figures/Nutrients/cellulosebysitebyinoculum_noash.pdf", height=4, width=6)
chemistryfigurefunction(fungalabund=littercelluloseT2$T1_mean,map=littercelluloseT2,ylab="Proportion Cellulose", ylim=c(0,maxvalue))
dev.off()
######################################################
########calculations, anova, and plots for lignin
######################################################
names(nutrients)

hist(nutrients$Lignin.na)

max(nutrients$Lignin.na)
min(nutrients$Lignin.na)

#do anova site*inoculum for Lignin
modellignin <- aov(Lignin.na ~Site*Inoculum, data=nutrients)
summary(modellignin)
#site, inoculu, sote*inoculum sig
TukeyHSD(modellignin, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modellignin, "Site", ordered = TRUE)
plot(TukeyHSD(modellignin, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modellignin <- aov(Lignin.na  ~Site, data=nutrients)
tuk <- glht(modellignin  , linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)

pdf("Figures/Nutrients/ligninbysite_noash.pdf", height=4, width=6)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.5,1), no.readonly=TRUE)
#plot
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 
plot(Lignin.na ~Site, data=nutrients)
plot(Lignin.na ~Inoculum, data=nutrients)

#export it to wordfile
modellignin <- aov(Lignin.na  ~Site*Inoculum, data=nutrients)
summary(modellignin)
capture.output(summary(modellignin),file="results/T2_lignin_noash.doc")
####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
litterlignin <- ddply(nutrients, "Combo", summarise,
                         T1_mean = mean(Lignin.na, na.rm=TRUE),
                         T1_sd = sd(Lignin.na, na.rm=TRUE),
                         T1_n = sum(!is.na(Lignin.na)),
                         T1_se = T1_sd/sqrt(T1_n)
)


litterligninT2 <- chemistryfunction(litterlignin)

#find the max value for the y axis
maxvalue <- sum(max(litterligninT2$T1_mean),max(litterligninT2$T1_se))

pdf("Figures/Nutrients/ligninbysitebyinoculum_noashnoash.pdf", height=4, width=6)
chemistryfigurefunction(fungalabund=litterligninT2$T1_mean,map=litterligninT2,ylab="Proportion Lignin", ylim=c(0,maxvalue))
dev.off()

######################################################
########calculations, anova, and plots for hemicellulose
######################################################
names(nutrients)
#Nameeer: hemicellulose (neutral detergent fiber – acid detergent fiber), 
nutrients$hemicellulose <- nutrients$NDF - nutrients$ADF

hist(nutrients$hemicellulose)
max(nutrients$Hemi.na)
min(nutrients$Hemi.na)
cor.test(nutrients$Hemi.na, nutrients$hemicellulose)
#do anova site*inoculum for hemicellulose
modelhemicellulose <- aov(Hemi.na  ~Site*Inoculum, data=nutrients)
summary(modelhemicellulose)
#site, inoculu, sote*inoculum sig
TukeyHSD(modelhemicellulose, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modelhemicellulose, "Site", ordered = TRUE)
plot(TukeyHSD(modelhemicellulose, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modelhemicellulose <- aov(Hemi.na  ~Site, data=nutrients)
tuk <- glht(modelhemicellulose  , linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)
pdf("Figures/Nutrients/hemmicelulosebysite_noash.pdf", height=4, width=6)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.25,1), no.readonly=TRUE)
#plot
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 
plot(Hemi.na  ~Site, data=nutrients)
plot(Hemi.na  ~Inoculum, data=nutrients)

#export it to wordfile
modelhemicellulose <- aov(Hemi.na   ~Site*Inoculum, data=nutrients)
summary(modelhemicellulose)
capture.output(summary(modelhemicellulose),file="results/T2_hemicellulose_noash.doc")

####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
litterhemicellulose <- ddply(nutrients, "Combo", summarise,
                      T1_mean = mean(Hemi.na , na.rm=TRUE),
                      T1_sd = sd(Hemi.na , na.rm=TRUE),
                      T1_n = sum(!is.na(Hemi.na )),
                      T1_se = T1_sd/sqrt(T1_n)
)


litterhemicelluloseT2 <- chemistryfunction(litterhemicellulose)

#find the max value for the y axis
maxvalue <- sum(max(litterhemicelluloseT2$T1_mean),max(litterhemicelluloseT2$T1_se))

pdf("Figures/Nutrients/hemmicelulosebysitebyinoculum_noash.pdf", height=4, width=6)
chemistryfigurefunction(fungalabund=litterhemicelluloseT2$T1_mean,map=litterhemicelluloseT2,ylab="Proportion hemicellulose", ylim=c(0.15,maxvalue))
dev.off()



######################################################
########calculations, anova, and plots for structural carbohydrates - NFC-NSC
######################################################
names(nutrients)
#Nameeer: NFC 
nutrients$SCarb.na <- (nutrients$NFC.na - nutrients$NSC.na)
hist(nutrients$SCarb.na)
max(nutrients$SCarb.na)
min(nutrients$SCarb.na)

#do anova site*inoculum for NFC
modelScarb<- aov(SCarb.na  ~Site*Inoculum, data=nutrients)
summary(modelScarb)
#site, inoculu, sote*inoculum sig
TukeyHSD(modelScarb, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modelScarb, "Site", ordered = TRUE)
plot(TukeyHSD(modelScarb, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modelScarb <- aov(SCarb.na  ~Site, data=nutrients)
tuk <- glht(modelScarb, linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)
pdf("Figures/Nutrients/structuralcarbbysite_noash.pdf", height=4, width=6)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.25,1), no.readonly=TRUE)
#plot
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 


#export it to wordfile
modelScarb <- aov(SCarb.na  ~Site*Inoculum, data=nutrients)
summary(modelScarb)
capture.output(summary(modelScarb),file="results/T2_structuralcarb_noash.doc")
####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
litterScarb <- ddply(nutrients, "Combo", summarise,
                   T1_mean = mean(SCarb.na, na.rm=TRUE),
                   T1_sd = sd(SCarb.na, na.rm=TRUE),
                   T1_n = sum(!is.na(SCarb.na)),
                   T1_se = T1_sd/sqrt(T1_n)
)


litterScarbT2 <- chemistryfunction(litterScarb )

#find the max value for the y axis
maxvalue <- sum(max(litterScarbT2$T1_mean),max(litterScarbT2$T1_se))

pdf("Figures/Nutrients/structuralcarbbysitebyinoc_noash.pdf", height=4, width=6)
chemistryfigurefunction(fungalabund=litterScarbT2$T1_mean,map=litterScarbT2,ylab="Structural carbohydrates", ylim=c(0,maxvalue))
dev.off()



######################################################
########calculations, anova, and plots for CP crude protein
######################################################
names(nutrients)

hist(nutrients$CP)
max(nutrients$CP)
min(nutrients$CP)
#do anova site*inoculum for CP
modelCP <- aov(Protein.na   ~Site*Inoculum, data=nutrients)
summary(modelCP)
#site, inoculu, sote*inoculum sig
TukeyHSD(modelCP, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modelCP, "Site", ordered = TRUE)
plot(TukeyHSD(modelCP, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modelCP <- aov(Protein.na   ~Site, data=nutrients)
tuk <- glht(modelCP  , linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)

pdf("Figures/Nutrients/Proteinbysitenoash.pdf", height=4, width=6)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.5,1), no.readonly=TRUE)
#plot
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 
plot(Protein.na ~Site, data=nutrients)
plot(Protein.na ~Inoculum, data=nutrients)

#export it to wordfile
modelCP <- aov(Protein.na   ~Site*Inoculum, data=nutrients)
summary(modelCP)
capture.output(summary(modelCP),file="results/T2_CP_noash.doc")
####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
litterCP <- ddply(nutrients, "Combo", summarise,
                      T1_mean = mean(Protein.na , na.rm=TRUE),
                      T1_sd = sd(Protein.na , na.rm=TRUE),
                      T1_n = sum(!is.na(Protein.na )),
                      T1_se = T1_sd/sqrt(T1_n)
)


litterCPT2 <- chemistryfunction(litterCP)

#find the max value for the y axis
maxvalue <- sum(max(litterCPT2$T1_mean),max(litterCPT2$T1_se))

pdf("Figures/Nutrients/CPbysitebyinoculumnoash.pdf", height=4, width=6)
chemistryfigurefunction(fungalabund=litterCPT2$T1_mean,map=litterCPT2,ylab="Proportion Crude Protein", ylim=c(0,maxvalue))
dev.off()


######################################################
########calculations, anova, and plots for NSC non structural carbohydrate
######################################################
names(nutrients)

hist(nutrients$NSC)
hist(nutrients$NSC.na)
cor.test(nutrients$NSC,nutrients$NSC.na)
#do anova site*inoculum for NSC
modelNSC <- aov(NSC.na  ~Site*Inoculum, data=nutrients)
summary(modelNSC)
#site, inoculu, sote*inoculum sig
TukeyHSD(modelNSC, ordered = TRUE, conf.level = 0.95)
TukeyHSD(modelNSC, "Site", ordered = TRUE)
plot(TukeyHSD(modelNSC, "Site"), las=2)

#specify pairwise comparions among level of variable "Site"
modelNSC <- aov(NSC  ~Site, data=nutrients)
tuk <- glht(modelNSC , linfct=mcp(Site="Tukey"))
#extract info
tuk.cld <- cld(tuk)

pdf("Figures/Nutrients/NSCbysitenoash.pdf", height=4, width=6)
#use sufficiently large upper margin
old.par <- par(mai=c(1,1,1.25,1), no.readonly=TRUE)
#plot
plot(tuk.cld)
dev.off()
par(old.par)
#site and site*inoculum significant, inoculum not significant 
plot(NSC ~Site, data=nutrients)
plot(NSC ~Inoculum, data=nutrients)

#export it to wordfile
modelNSC <- aov(NSC.a  ~Site*Inoculum, data=nutrients)
summary(modelNSC)
capture.output(summary(modelNSC),file="results/T2_NSC.doc")
####################################################################################
#use dply to get the means, sd, n and se for nutrient
####################################################################################

# Calculate the means, sd, n, and se.
litterNSC <- ddply(nutrients, "Combo", summarise,
                  T1_mean = mean(NSC.na, na.rm=TRUE),
                  T1_sd = sd(NSC.na, na.rm=TRUE),
                  T1_n = sum(!is.na(NSC.na)),
                  T1_se = T1_sd/sqrt(T1_n)
)


litterNSCT2 <- chemistryfunction(litterNSC)

#find the max value for the y axis
maxvalue <- sum(max(litterNSCT2$T1_mean),max(litterNSCT2$T1_se))

pdf("Figures/Nutrients/NSCbysitebyinoculumnoash.pdf", height=5, width=6)
chemistryfigurefunction(fungalabund=litterNSCT2$T1_mean,map=litterNSCT2,ylab="% Non structural carbohydrates", ylim=c(0,maxvalue))
dev.off()



####################################################################################
#Next, get them all in one facet plot for all the different proteins by rbinding everything
#make a big facet plot with all nutrients * sites
####################################################################################
#The other transformation is for structural carbohydrates, 
#which are non-fiber carbohydrates minus non-structural carbohydrates.
#non-strcutral carbodhydrates (NSC)

#non-structural cabohydrates (starches + sugars)
cor.test(nutrients$NSC.na, nutrients$NSC)

#structural carbohydrates = NFC-NSC = pectins, plant cell wall components, etc
nutrients$SCarb.na <- (nutrients$NFC.na - nutrients$NSC.na)

#make a matrix of the non-ash organic compounds we care about
litterhemicelluloseT2$Compound <- rep("Hemicellulose", nrow(litterhemicelluloseT2)) #hemicellulose
littercelluloseT2$Compound <- rep("Cellulose", nrow(littercelluloseT2)) #cellulose
litterligninT2$Compound <- rep("Lignin", nrow(litterligninT2)) #lignin
litterCPT2$Compound <- rep("Crude protein", nrow(litterCPT2)) #crude protein
litterNSCT2$Compound <- rep("Non-structural carbohydrates", nrow(litterNSCT2)) #nonstructural carbs (sugars+starch)
#non structural carbs are a tiny amount maybe remove them
litterScarbT2$Compound <- rep("Structural Carbohydrates", nrow(litterScarbT2)) #structural carbs (plant cell wall componenets)

totalchem1 <- rbind(litterligninT2, litterhemicelluloseT2, littercelluloseT2, litterCPT2,litterScarbT2,litterNSCT2$Compound)
totalchem <- rbind(litterligninT2, litterhemicelluloseT2, littercelluloseT2, litterCPT2,litterScarbT2)

#put sitenames in proper order
totalchem$sitenames <- factor(totalchem$sitenames, levels = c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine") ) 

#rename and re-order Inoculum labeles
totalchem$Inoculum <- factor(totalchem$Inoculum,levels = c("D","W","G","P","S"), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))

#re order chemistry
totalchem1$Compound <- factor(totalchem$Compound, levels = c("Cellulose","Hemicellulose","Lignin","Crude protein","Structural Carbohydrates","Non-structural carbohydrates") ) 
totalchem$Compound <- factor(totalchem$Compound, levels = c("Cellulose","Hemicellulose","Lignin","Crude protein","Structural Carbohydrates") ) 

library(ggplot2)
sp <- ggplot(data=totalchem, aes(x=sitenames, y=T1_mean, col=Inoculum)) + geom_point(size=2) +  
  geom_errorbar(aes(ymin=T1_mean-T1_se, ymax=T1_mean+T1_se), width=.1) + #adding in standard error bars
  labs(x=" ", y="Proportion dry leaf litter", col="Inoculum") +#change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme_bw()+ #make it black and white
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) + #change angles and size of x axis labels
  scale_fill_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                    guide="none") +  ##fix the scale fill colors to red orange green blue purple
  scale_colour_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                      labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))  #fix the legend labels

sp + facet_wrap(~Compound, ncol=3)

pdf("Figures/Nutrients/leafchemistry_facet_bysitenoash.pdf")
sp + facet_wrap(~Compound, ncol=3,scales="free_y")
dev.off()


####################################################################################
#divide into 2 facet plots bc scale of y axis is so differebt
####################################################################################
celluloses <- c(which(totalchem$Compound=="Hemicellulose"),which(totalchem$Compound=="Cellulose"))

totalchem_big <- totalchem[celluloses, ]
totalchem_small <- totalchem[-celluloses, ]


library(ggplot2)
small <- ggplot(data=totalchem_small , aes(x=sitenames, y=T1_mean, col=Inoculum)) + geom_point(size=2) +  
  geom_errorbar(aes(ymin=T1_mean-T1_se, ymax=T1_mean+T1_se), width=.1) + #adding in standard error bars
  labs(x=" ", y="Proportion dry leaf litter", col="Inoculum") +#change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme_bw()+ #make it black and white
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) + #change angles and size of x axis labels
  scale_fill_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                    guide="none") +  ##fix the scale fill colors to red orange green blue purple
  scale_colour_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                      labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))  #fix the legend labels

small + facet_wrap(~Compound, ncol=3)

pdf("Figures/Nutrients/leafchemistry_facet_bysite_smallfractionsnoash.pdf", height=4, width=8)
small + facet_wrap(~Compound, ncol=3)
dev.off()


big <- ggplot(data=totalchem_big , aes(x=sitenames, y=T1_mean, col=Inoculum)) + geom_point(size=2) +  
  geom_errorbar(aes(ymin=T1_mean-T1_se, ymax=T1_mean+T1_se), width=.1) + #adding in standard error bars
  labs(x=" ", y="Proportion dry leaf litter", col="Inoculum") +#change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme_bw()+ #make it black and white
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1)) + #change angles and size of x axis labels
  scale_fill_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                    guide="none") +  ##fix the scale fill colors to red orange green blue purple
  scale_colour_manual(values=c("Desert"="red", "Scrubland"="orange", "Grassland"="green", "Pine-Oak"="blue", "Subalpine"="purple"), 
                      labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))  #fix the legend labels

big + facet_wrap(~Compound, ncol=2)

pdf("Figures/Nutrients/leafchemistry_facet_bysite_bigfractionsnoash.pdf",height=4, width=8)
big + facet_wrap(~Compound, ncol=2)
dev.off()

####################################################################################
##Make a Permanova and PCA of all the samples by the organic carbon content
####################################################################################
names(nutrients)

#organic comoounds of non-ash dried litter mass Nameer used:
#lignin, cellulose, hemicellulose, structural carbohydrates (non-fiber carbs), and crude protein
litterchem_matrix <- nutrients[ ,c(50,51,52,53,54)]
litterchem_matrix1 <- nutrients[ ,c(50,51,52,53,54,49)]


row.names(litterchem_matrix) <- nutrients$SampleID

#do they sum up to 1?
Sum <- rowSums(litterchem_matrix)
mean(Sum)
max(Sum) #very close to 1

#try normalizing them to 1 by dividing every thing by the sum of it's row
litterchem2 <- litterchem_matrix/Sum
Sum2 <- rowSums(litterchem2)
mean(Sum2)



#transform dataframe
#litterchem_matrix <- t(litterchem_matrix)
library(vegan)


#then calculate a euclidian distance matrix - figure out what this is doing
litterchem_dist <- dist(litterchem2, method = "euclidean", diag = FALSE, upper = FALSE)

litterchem_dist <- as.dist(litterchem_dist , diag = TRUE, upper = TRUE)

################################################
############Do PERMANOVA (Adonis) and post hoc tests on dissimilarity matrices
################################################
litterchemadonis <- adonis(litterchem_dist  ~ Site*Inoculum, data = nutrients, permutations=9999, method="bray" )
litterchemadonis
#significant site, inoculum and site*inoculum effects
###so far this is the only one I can get to work, but only if I fix up the map and re-upload it
#removing the extr factors
#install.packages("devtools")
library(devtools)
#install_github("GuillemSalazar/EcolUtils")
library(EcolUtils)
#most of the sites are different except pine-oak v subalpine and desert v scrubland
adonispairsite <- adonis.pair(litterchem_dist, nutrients$Site, nper = 1000, corr.method = "fdr")
#none of the inoculum are different
adonispairinoc <- adonis.pair(litterchem_dist, nutrients$Inoculum, nper = 1000, corr.method = "fdr")

litterchemadonis
capture.output(litterchemadonis,file="results/T2_litterchem_adonis_noash.doc")
capture.output(adonispairsite,file="results/T2_litterchem_adonis_noash_pairsite.doc")

capture.output(adonispairinoc,file="results/T2_litterchem_adonis_noash_pairinoc.doc")

################################################
############Make NMDS of litter chemistry 
################################################
nmdschem=metaMDS(litterchem_dist, k=2, dist="bray", binary=FALSE, trymax=100)
nmdschem
stressplot(nmdschem)
nutrients$Site
nutrients$colors <- c(rep("red",length(which(nutrients$Site =="Desert"))), rep("green",length(which(nutrients$Site =="Grassland"))),
                      rep("blue",length(which(nutrients$Site =="Pine-Oak"))),rep( "orange",length(which(nutrients$Site =="Scrubland"))),
                      rep("purple",length(which(nutrients$Site =="Subalpine"))))

pdf("Figures/nutrients/litterchemistryNMDSnoash.pdf")
plot(nmdschem, display="sites")
points(nmdschem$points, col=nutrients$colors, pch=16)
legend("bottomright",legend=sitenames1,col=sitecolors1,pch=16,bty="l")
dev.off()

pdf("Figures/nutrients/litterchemistryNMDSnoash.pdf")
plot(scores(nmdschem), col=nutrients$colors, pch=16,ylim=c(-0.1,0.1))
legend("bottomright",legend=sitenames1,col=sitecolors1,pch=16,bty="l")
#add in names of trees so I can see which trees are different by Tricholoma myomyces
#text(model$points, labels=row.names(as.data.frame(model$points)), cex= 0.7, offset = 10)
#row.names(as.data.frame(model$points))
# circle 95% confidence interval on centroid of each group (calculated using standard error)
ordiellipse(model, groups=nutrients$Site, kind="se", conf=0.95, col=sitecolors1, label=TRUE )
ordiellipse(model, groups=nutrients$Site, kind="se", conf=0.95, col=sitecolors1 )

# there is a lot of variation within groups, and this is probably why the adonis fit is 
dev.off()

################################################
############Do envfit to figure out which nutrients are different
################################################

# which samples correlate well with this ordination space?
spp.fit=envfit(model, litterchem_dist)

#rename the columns of the litter chemistry matrix
names(litterchem2) <- c("Lignin", "Crude Protein","Structural Carbs", "Cellulose","Hemicellulose","Non-Structural Carbs")

names(litterchem2) <- c("Lignin", "Crude Protein","Structural Carbs", "Cellulose","Hemicellulose")


litterchem_matrix
# which chemistry variables correlate well with this ordination space?
spp.fit=envfit(model, litterchem2)

#plot with the inoculum too
points(model$points, col=nutrients$colors, pch=levels(nutrients$Inoculum))

pdf("Figures/nutrients/litterchemistryNMDS_withvectorsnoash.pdf", height=7, width=7)
plot(model, display="sites")
points(model$points, col=nutrients$colors, pch=16)
#can change the p.max to 0.01, 0.001, 0.005

plot(spp.fit, p.max=0.005, col="black", cex=1, adj=0.05, pos=4)
legend("bottomright",legend=sitenames1,col=sitecolors1,pch=16,bty="l", title="Site")

dev.off()


####################################################################################
#Make a stacked bar graph of each site with the relative proportions of the 5 chemicals in each site
#make a stacked bar graph of the relative proprotions of each of 5 chemicals by each site*inoculum 
#compounds mostly add up to one
####################################################################################
####################################################################################
#Can normalize to g by multiplying by g dry weight, or by % mass loss * 5g
#can compare T2 to a diff time point if I have it
####################################################################################


