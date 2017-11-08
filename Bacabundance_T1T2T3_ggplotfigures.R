#October 19, 2017
#use same codes as decomp to make nice figures and anovas


#16S library
#Reset R's Brain
rm(list=ls())

#install.packages("plyr")
library(plyr )
library(tidyverse)
library(stringr)

#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")


################################################################################################
###### Upload files and check they are the same for mass loss and bac abundance
################################################################################################

#this is the final weights data sheet from git hub July
weight1 <- read.csv("data/WeightsElevationStudy.csv",header=T)
df1 <- read.csv("data/Masslossfiles/WeightsElevationStudy2.csv",header=T)
weight1$SampleID <- as.character(weight1$SampleID)
df1$SampleID <- as.character(df1$SampleID)
df1$SampleID==weight1$SampleID
cor.test(weight1$BagDiff,df1$BagDiff)
cor.test(weight1$ContainerDiff,df1$ContainerDiff)
plot(weight1$BagDiff~ df1$BagDiff)
weight1$Timepoint <- as.factor(weight1$Timepoint)
bacterialwet1 <- read.csv("data/BacAbundfiles/bacterialabundancewetweight.csv", header=TRUE)
bacterialwet2 <- read.csv("data/BacAbundfiles/Alltimepointsabundancewetweight.csv",header=TRUE)
#make sample name a character
bacterialwet1$sampleID <- as.character(bacterialwet1$sampleID)
bacterialwet2$sampleID <- as.character(bacterialwet2$sampleID)
#make time point a factor
bacterialwet1$TimePoint1 <- as.factor(bacterialwet1$TimePoint1)
bacterialwet2$Time.point <- as.factor(bacterialwet2$Time.point)

#check that eventsperml are the same - ok they are
bacterialwet1$sampleID==bacterialwet2$sampleID
bacterialwet1$eventsperml==bacterialwet2$events...ml.this.is.also.per.g.that.was.put.into.the.tube.wet.weight

################################################################################################
######Calculate bacterial abundance
################################################################################################
dim(bacterialwet1)
dim(weight1)
#rename weight1 sampleID column so it has the same name
class(bacterialwet1$sampleID)

#concatenate time point and sample ID so the names are unique
bacterialwet1$Name <- paste(bacterialwet1$sampleID, bacterialwet1$TimePoint1,sep=".")
class(bacterialwet1$Name)
weight1$Name <- paste(weight1$SampleID, weight1$Timepoint,sep=".")
class(weight1$Name)

bacabund <- merge(weight1,bacterialwet1, by="Name")
bacabundsorted <- bacabund[order(bacabund$Timepoint), ]
bacabundsorted$Timepoint
##########################################
#To calculate bacterial abundance per wet weight
##########################################
#calculate dry to wet ration
bacabundsorted$DryToWet <- bacabundsorted$LitterDryWeight/bacabundsorted$LitterWetWeight

#calculate bacterial abundance per g dry weight:
#counts/bacteria wet weight*dry to wet ratio
bacabundsorted$bacterialabundancepergdrywt <- bacabundsorted$eventsperml/(bacabundsorted$BactWetWeight*bacabundsorted$DryToWet)

hist(bacabundsorted$bacterialabundancepergdrywt)

write.csv(bacabundsorted, "data/BacterialAbundData.csv")


##########################################
#Upload and compare to Claudia's values
##########################################
#re-do with everything from July 10, 2017 github and steve's codes to calculate mass loss 
#Only T2 samples are different - scrubland, Pine-oak, subalpine - need to check with steve
#read in mass loss data from Steve- need to figure out how he calculated it
bacabund1 <- read.csv("data/BacAbundfiles/adjustedbacabundfromClaudia.csv")
names(bacabund1)
class(bacabund1$abundance...g.dry.weight)

#make it a numeric
bacabund1$bacterialabundancepergdrywt<- as.numeric(as.character(bacabund1$abundance...g.dry.weight))
hist(bacabund1$bacterialabundancepergdrywt)

#how did claudia calculate? check with her
bacabund1$sampleID == bacabundsorted$SampleID
plot(bacabund1$bacterialabundancepergdrywt ~ bacabundsorted$bacterialabundancepergdrywt)

#ok claudia's values and my values are the same. this is the final bac abundance

#remove outliers really high value for some readon
maxvalue <- max(bacabundsorted$bacterialabundancepergdrywt,na.rm=TRUE)
maxvalue 

which(bacabundsorted$bacterialabundancepergdrywt==maxvalue)
bacabundsorted[357, ]
#5W02 is an outlier
#remove it
bacabundsorted2 <- bacabundsorted[-357, ]
hist(bacabundsorted2$bacterialabundancepergdrywt)


####################################################################################
#separate T1, T2, T3
####################################################################################


#Do everything with bacabundsorted2
#create a type column for inoculum and a sample column for site by inoc combo
bacabundsorted2$Type <- str_sub(bacabundsorted2$SampleID,2,2)
bacabundsorted2$Sample <- str_sub(bacabundsorted2$SampleID,1,2)

#subset 3 time ppints
T1 <- bacabundsorted2[which(bacabundsorted2$TimePoint1==1), ]
T2 <- bacabundsorted2[which(bacabundsorted2$TimePoint1==2), ]
T3 <- bacabundsorted2[which(bacabundsorted2$TimePoint1==3), ]



####################################################################################
#Get bac mean sd and se with controls
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum
# Calculate the means, sd, n, and se.
bac_T1 <- ddply(T1, "Sample", summarise,
                   T1_mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                   T1_sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                   T1_n = sum(!is.na( bacterialabundancepergdrywt)),
                   T1_se = T1_sd/sqrt(T1_n)
)

head(bac_T1)


#get mean, SD, SE of  T2 by site by inoculum
# Calculate the means, sd, n, and se.
bac_T2 <- ddply(T2, "Sample", summarise,
                   T2_mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                   T2_sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                   T2_n = sum(!is.na( bacterialabundancepergdrywt)),
                   T2_se = T2_sd/sqrt(T2_n)
)

head(bac_T2)
#get mean, SD, SE of  T3 by site by inoculum
# Calculate the means, sd, n, and se.
bac_T3 <- ddply(T3, "Sample", summarise,
                   T3_mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                   T3_sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                   T3_n = sum(!is.na( bacterialabundancepergdrywt)),
                   T3_se = T3_sd/sqrt(T3_n)
)

head(bac_T3)

####################################################################################
#Make a dataframe for figures with controls
####################################################################################
#####check that sample names are same for all 3
bac_T1$Sample == bac_T2$Sample
bac_T2$Sample == bac_T3$Sample

#make names the same so I can combine rows
names(bac_T1) <- c("Sample","mean","sd","n","se")
names(bac_T2) <- c("Sample","mean","sd","n","se")
names(bac_T3)<- c("Sample","mean","sd","n","se")

#combine rows
bac_T1T2T3 <- rbind(bac_T1,bac_T2,bac_T3)
#add time points
bac_T1T2T3$Timepoint <- c(rep("T1",nrow(bac_T1)),rep("T2",nrow(bac_T2)),rep("T3",nrow(bac_T3)))
#make list of colors according to Jen's color scheme and add in some for controls
listofcolors <- c("darkgrey","red","green","burlywood2","cornsilk4","blue","purple","orange")
#make list of colors that matches 
bac_T1T2T3$colors <- rep(listofcolors,5)
#make a list of site names that matches
bac_T1T2T3$sitenames <- c(rep("Desert",8),rep("Grassland",8),rep("Pine-Oak",8),rep("Scrubland",8),rep("Subalpine",8))
#make a list of Inoculum by substring from sample name
bac_T1T2T3$Inoculum <- str_sub(bac_T1T2T3$Sample,2,2)

#make factors for site names and inoculum in correct order so they show up correct on ggplot figure
bac_T1T2T3$sitenames <- factor(bac_T1T2T3$sitenames,levels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))
bac_T1T2T3$Inoculum <- factor(bac_T1T2T3$Inoculum,levels=c("D","W","G","P","S","C","L","N"))

write.csv(bac_T1T2T3,"results/BacterialAbundT1T2T3.csv")
####################################################################################
#Remove controls so I can make same figure with and without controls
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum
controls <- which(bac_T1T2T3$Inoculum %in%c("C","L","N"))

bac_T1T2T3_nc <- bac_T1T2T3[-controls, ]

####################################################################################
#Make ggplot figures, for sites*inoculum, for all 3 time points face wrap with and without controls
####################################################################################
####make figure with controls
r <- ggplot(data=bac_T1T2T3, aes(x=sitenames, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Bacterial abundance cells/g dry weight leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine","Sterile","In-situ open","In-situ closed"),#manual labels for legend
                     values=c("red", "orange", "green","blue","purple","darkgrey","burlywood2","cornsilk4"))   #add in manual colors for points/lines 
  

pdf("Figures/microbialabundance/bacabundggplot/Bacadbund_bysitebyinoc_T1T2T3_facetwrap_withcontrols.pdf", height=5, width=8)  

r + facet_wrap(~Timepoint, ncol=3)
dev.off()


####make figure without controls
p <- ggplot(data=bac_T1T2T3_nc, aes(x=sitenames, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Bacterial abundance cells/g dry weight leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), #manual labels for legend
    values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines

pdf("Figures/microbialabundance/bacabundggplot/Bacadbund_bysitebyinoc_T1T2T3_facetwrap.pdf", height=5, width=8)  

p + facet_wrap(~Timepoint, ncol=3)
dev.off()



####################################################################################
#Now make figures with inoculum against time as x axis, and facet wrap the sites 
####################################################################################
#make a T0 dataframe
bac_T0 <- bac_T1T2T3_nc[1:40, ]

bac_T0$mean <- rep(0,40)
bac_T0$sd <- rep(0,40)
bac_T0$n <- rep(0,40)
bac_T0$se <- rep(0,40)
bac_T0$Timepoint <- rep("T0", nrow (bac_T0))
bac_T0

#add T0 dataframe to T1,T2,T3 dataframe
bac_T1T2T3_site_all <- rbind(bac_T0,bac_T1T2T3_nc)

#make ggplot of the mean % mass loss by site over time
sp <- ggplot(data=bac_T1T2T3_site_all, aes(x=Timepoint, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Bacterial abundance cells/g dry weight leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),
    values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines


pdf("Figures/microbialabundance/bacabundggplot/Bacadbund_5site_overtime_facetwrap2.pdf", height=5, width=8)  

sp + facet_wrap(~sitenames, ncol=3)
dev.off()

write.csv(bac_T1T2T3_site_all, "results/T1T2T3_Massloss_means_bysitebytime_nocontrols2.csv")



####################################################################################
#Set up T1, T2, T3 for ANOVAs
####################################################################################
###########T1
par(mfrow=c(1,1))
#remove controls from dataframe
T1controls <- which(T1$Type %in%c("C","L","N"))
T1_nc <- T1[-T1controls, ]

#add site and inoculum names column
T1_nc$Inoculum <- str_sub(T1_nc$Sample,2,2)
T1_nc$Site <- str_sub(T1_nc$Sample,1,1)

#make them factors and in correct order
T1_nc$Inoculum <- factor(T1_nc$Inoculum,levels=c("D","W","G","P","S"))
T1_nc$Site <- factor(T1_nc$Site,levels=c("1","4","2","3","5"))

###########T2
#remove controls from dataframe
T2controls <- which(T2$Type %in%c("C","L","N"))
T2_nc <- T2[-T2controls, ]

#add site and inoculum names column
T2_nc$Inoculum <- str_sub(T2_nc$Sample,2,2)
T2_nc$Site <- str_sub(T2_nc$Sample,1,1)

#make them factors and in correct order
T2_nc$Inoculum <- factor(T2_nc$Inoculum,levels=c("D","W","G","P","S"))
T2_nc$Site <- factor(T2_nc$Site,levels=c("1","4","2","3","5"))

###########T3
#remove controls from dataframe
T3controls <- which(T3$Type %in%c("C","L","N"))
T3_nc <- T3[-T3controls, ]

#add site and inoculum names column
T3_nc$Inoculum <- str_sub(T3_nc$Sample,2,2)
T3_nc$Site <- str_sub(T3_nc$Sample,1,1)

#make them factors and in correct order
T3_nc$Inoculum <- factor(T3_nc$Inoculum,levels=c("D","W","G","P","S"))
T3_nc$Site <- factor(T3_nc$Site,levels=c("1","4","2","3","5"))


####################################################################################
#ANOVAs: Normal type 1 ANOVA
####################################################################################
#Do ANOVA model to test effect of inoculum and site
modelbac_T1<-aov(bacterialabundancepergdrywt~Site*Inoculum,  data=T1_nc, na.action=na.omit)
summary(modelbac_T1)
summary.lm(modelbac_T1)

modelbac_T2<-aov(bacterialabundancepergdrywt~Site*Inoculum,  data=T2_nc, na.action=na.omit)
summary(modelbac_T2)
summary.lm(modelbac_T2)

modelbac_T3<-aov(bacterialabundancepergdrywt~Site*Inoculum,  data=T3_nc, na.action=na.omit)
summary(modelbac_T3)
summary.lm(modelbac_T3)

####################################################################################
#ANOVAs: Type 2 vs Type 3 comparisons with library car
#https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
#use car package to distinguish between Type 2 and Type 3 ANOVA
####################################################################################

library(car)
######T1
Anova(lm(bacterialabundancepergdrywt~Site*Inoculum, data=T1_nc, type=2))
modelbac_T1_type3 <- Anova(lm(bacterialabundancepergdrywt ~ Site*Inoculum, data=T1_nc, contrasts=list(Site=contr.sum, Inoculum=contr.sum)), type=3)
modelbac_T1_type3

######T2
Anova(lm(bacterialabundancepergdrywt~Site*Inoculum, data=T2_nc, type=2))
modelbac_T2_type3 <- Anova(lm(bacterialabundancepergdrywt ~ Site*Inoculum, data=T2_nc, contrasts=list(Site=contr.sum, Inoculum=contr.sum)), type=3)
modelbac_T2_type3

######T3
Anova(lm(bacterialabundancepergdrywt~Site*Inoculum, data=T3_nc, type=2))
modelbac_T3_type3 <- Anova(lm(bacterialabundancepergdrywt ~ Site*Inoculum, data=T3_nc, contrasts=list(Site=contr.sum, Inoculum=contr.sum)), type=3)
modelbac_T3_type3


#export it to wordfile
capture.output(modelbac_T1_type3,file="results/T1_bacabundANOVA.doc")
capture.output(modelbac_T2_type3,file="results/T2_bacabundANOVA.doc")
capture.output(modelbac_T3_type3,file="results/T3_bacabundANOVA.doc")
capture.output(summary.lm(modelbac_T1),file="results/T1_bacabundANOVA_summarylm.doc")
capture.output(summary.lm(modelbac_T2),file="results/T2_bacabundANOVA_summarylm.doc")
capture.output(summary.lm(modelbac_T3),file="results/T3_bacabundANOVA_summarylm.doc")
#capture.output(summary.lm(modelbac_T3),file="results/T3_massloss_modelsummary.doc")

####################################################################################
#ANOVAs: Steve's way
####################################################################################
library(nlme)
m.1 <- gls(bacterialabundancepergdrywt~Site*Inoculum,data=T1_nc,na.action="na.omit")
Anova(m.1,type=3)

m.2 <- gls(bacterialabundancepergdrywt~Site*Inoculum,data=T2_nc,na.action="na.omit")
Anova(m.2,type=3)

m.3 <- gls(bacterialabundancepergdrywt~Site*Inoculum,data=T3_nc,na.action="na.omit")
Anova(m.3,type=3)



####################################################################################
#Tukey HSD post hoc tests for Site
####################################################################################
library(multcomp)

######T1
modelsite_T1<-aov(bacterialabundancepergdrywt~Site,  data=T1_nc)
tuk_T1 <- glht(modelsite_T1, linfct = mcp(Site = "Tukey")) 
tuk.cld.T1 <- cld(tuk_T1) 


######T2
modelsite_T2<-aov(bacterialabundancepergdrywt~Site,  data=T2_nc)
tuk_T2 <- glht(modelsite_T2, linfct = mcp(Site = "Tukey")) 
tuk.cld.T2 <- cld(tuk_T2) 

######T3
modelsite_T3<-aov(bacterialabundancepergdrywt~Site,  data=T3_nc)
tuk_T3 <- glht(modelsite_T3, linfct = mcp(Site = "Tukey")) 
tuk.cld.T3 <- cld(tuk_T3) 

### use sufficiently large upper margin 
pdf("Figures/microbialabundance/bacabundggplot/Bacadbund_T1T2T3_tukey_site.pdf", height=7, width=8,pointsize=12)
old.par <- par(mai=c(0.7,0.75,1.25,0.1),mfrow=c(1,3),no.readonly = TRUE) #make enough space at top for tukey symbols and smaller spaces between figures
plot(tuk.cld.T1, ylab="Bacterial abundance cells/g dry weight", xaxt="n",xlab="" ,ylim=c(0,2.5e10)) #add in same y limits for all, supress x axis labela nd tick marks
mtext(side=3, "T1", line=5) #add in T1 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2) #add in customized x axis labels and make them perpendicular
plot(tuk.cld.T2, ylab="Bacterial abundance cells/g dry weight",xaxt="n",xlab="",ylim=c(0,2.5e10)) 
mtext(side=3, "T2",line=5)#add in T2 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)
plot(tuk.cld.T3, ylab="Bacterial abundance cells/g dry weight", xaxt="n",xlab="",ylim=c(0,2.5e10)) 
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)
mtext(side=3, "T3",line=5)#add in T3 label
par(old.par)
dev.off()



####################################################################################
#Tukey HSD post hoc tests for Inoculum
####################################################################################
library(multcomp)

######T1
modelinoc_T1<-aov(bacterialabundancepergdrywt~Inoculum,  data=T1_nc)
tuk_T1_inoc <- glht(modelinoc_T1, linfct = mcp(Inoculum = "Tukey")) 
tuk.cld.T1.inoc <- cld(tuk_T1_inoc) 


######T2
modelinoc_T2<-aov(bacterialabundancepergdrywt~Inoculum,  data=T2_nc)
tuk_T2_inoc <- glht(modelinoc_T2, linfct = mcp(Inoculum = "Tukey")) 
tuk.cld.T2.inoc <- cld(tuk_T2_inoc) 

######T3
modelinoc_T3<-aov(bacterialabundancepergdrywt~Inoculum,  data=T3_nc)
tuk_T3_inoc <- glht(modelinoc_T3, linfct = mcp(Inoculum = "Tukey")) 
tuk.cld.T3.inoc <- cld(tuk_T3_inoc) 

### use sufficiently large upper margin 
pdf("Figures/microbialabundance/bacabundggplot/BacadbundT1T2T3_tukey_inoc.pdf", height=7, width=8, pointsize=12)
old.par <- par(mai=c(0.7,0.75,1.25,0.1),mfrow=c(1,3),no.readonly = TRUE) #make enough space at top for tukey symbols and smaller spaces between figures
plot(tuk.cld.T1.inoc, ylab="Bacterial abundance cells/g dry weight", xaxt="n",xlab="" , ylim=c(0,2e10)) #add in same y limits for all, supress x axis labela nd tick marks
mtext(side=3, "T1", line=4) #add in T1 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2) #add in customized x axis labels and make them perpendicular
plot(tuk.cld.T2.inoc, ylab="Bacterial abundance cells/g dry weight", xaxt="n",xlab="",ylim=c(0,2e10)) 
mtext(side=3, "T2",line=4)#add in T2 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)
plot(tuk.cld.T3.inoc, ylab="Bacterial abundance cells/g dry weight",xaxt="n",xlab="",ylim=c(0,2e10)) 
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)
mtext(side=3, "T3",line=4)#add in T3 label
par(old.par)
dev.off()



####################################################################################
#calculating bacosition * Site * Inoculum * Time
####################################################################################
names(T3_nc)==names(T1_nc)
names(T1_nc)==names(T2_nc)
names(T1_nc)
names(T3_nc)

#make a column of the site and inoculum
T3_nc$Type <- str_sub(T3_nc$SampleID,2,2)

names(T1_nc)
T1_nc2 <-T1_nc[,c(1,2,3,7, 30,33, 34)]
T2_nc2 <-T2_nc[,c(1,2,3,7, 30,33, 34) ]
T3_nc2 <-T3_nc[,c(1,2,3,7, 30,33,34) ]

#combine them all together
T1T2T3_nc2 <- rbind(T1_nc2,T2_nc2,T3_nc2)

T1T2T3_nc2$Timepoint <- as.factor(T1T2T3_nc2$Timepoint)
T1T2T3_nc2$Inoculum <- as.factor(T1T2T3_nc2$Inoculum)

model_all<-aov(bacterialabundancepergdrywt~Site.x*Inoculum*Timepoint,  data=T1T2T3_nc2)
summary(model_all)
summary.lm(model_all)
#do I need to repeat this as a GLM with timepoint as a random effect? Is it random or discrete? the samples are different
capture.output(summary(model_all),file="results/T1T2T3_bacabund_sitebyinocbytime.doc")
capture.output(summary.lm(model_all),file="results/T1T2T3_bacabun_sitebyinocbytime_summarylm.doc")

###################################################################################
#calculating effect sizes with eta squared
####################################################################################
#https://egret.psychol.cam.ac.uk/statistics/local_copies_of_sources_Cardinal_and_Aitken_ANOVA/glm_effectsize.htm
#https://artax.karlin.mff.cuni.cz/r-help/library/lsr/html/etaSquared.html

#install.packages("lsr")
library(lsr)
etaSquared(modelbac_T3, type=2)

etasquaredT1T2T3 <- cbind(etaSquared(modelbac_T1, type=2),etaSquared(modelbac_T2, type=2),etaSquared(modelbac_T3, type=2))

colnames(etasquaredT1T2T3) <- c("T1 eta.sq","T1 eta.sq.part", "T2 eta.sq","T2 eta.sq.part","T3 eta.sq","T3 eta.sq.part")

etasquaredT1T2T3
write.csv(etasquaredT1T2T3,"results/T1T2T3_massloss_etasquared.csv")
etasquaredT1T2T3trans <- t(etasquaredT1T2T3)
####################################################################################
#calculating effect sizes with omegasquared
####################################################################################
#source in functions
#https://gist.github.com/arnoud999/e677516ed45e9a11817e
source('~/Dropbox/StatsandProgramming/source/omegas.R', chdir = TRUE)


# Eta-squared
require(lsr)
etaSquared(modelbac_T3)

# Omega-squared using arnaud platinga code #https://gist.github.com/arnoud999/e677516ed45e9a11817e
Omegas(modelbac_T3)
partialOmegas(modelbac_T3)

#using code from here: https://stats.stackexchange.com/questions/2962/omega-squared-for-measure-of-effect-in-r
omega_sq(modelbac_T3)

#all codes come out the exact same as Steve's except steve's has an error in it bc one of his come's out neg

#ok so now caluculate omegas for all 3
omegaT1 <- rbind(Omegas(modelbac_T1),partialOmegas(modelbac_T1))
row.names(omegaT1) <- c("omegasT1","partialomegasT1")

omegaT2 <- rbind(Omegas(modelbac_T2),partialOmegas(modelbac_T2))
row.names(omegaT2) <- c("omegasT2","partialomegasT2")

omegaT3 <- rbind(Omegas(modelbac_T3),partialOmegas(modelbac_T3))
row.names(omegaT3) <- c("omegasT3","partialomegasT3")

#combine all into one
omegasT1T2T3 <- rbind(omegaT1,omegaT2,omegaT3)
omegasT1T2T3

#combine with eta squared
omegasandetas <- rbind(omegasT1T2T3,etasquaredT1T2T3trans) 
omegasandetas

write.csv(omegasandetas, "results/bacabund_omegasandetasT1T2T3.csv")





####################################################################################
#Make overall site mean figure with Tukey letters on it
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum

# Calculate the means, sd, n, and se.
bac_T1_site_nc <- ddply(T1_nc, "Site", summarise,
                           mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                           sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                           n = sum(!is.na( bacterialabundancepergdrywt)),
                           se = sd/sqrt(n)
)

head(bac_T1_site_nc)


# Calculate the means, sd, n, and se.
bac_T2_site_nc <- ddply(T2_nc, "Site", summarise,
                           mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                           sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                           n = sum(!is.na( bacterialabundancepergdrywt)),
                           se = sd/sqrt(n)
)

head(bac_T2_site_nc)


# Calculate the means, sd, n, and se.
bac_T3_site_nc <- ddply(T3_nc, "Site", summarise,
                           mean = mean(bacterialabundancepergdrywt, na.rm=TRUE),
                           sd = sd(bacterialabundancepergdrywt, na.rm=TRUE),
                           n = sum(!is.na( bacterialabundancepergdrywt)),
                           se = sd/sqrt(n)
)

head(bac_T3_site_nc)


#combine them all
bac_T1T2T3_nc_site_all <- rbind(bac_T1_site_nc,bac_T2_site_nc,bac_T3_site_nc)
bac_T1T2T3_nc_site_all$Timepoint <- c(rep("T1",nrow(bac_T1_site_nc)),rep("T2",nrow(bac_T2_site_nc)),rep("T3",nrow(bac_T3_site_nc)))
bac_T1T2T3_nc_site_all$Sitenames <- rep(c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),3)
#create a vector of Tukey labels based on above tukey tests
bac_T1T2T3_nc_site_all$Tukeylabels <- c("a","b","c","d","b,c","a","b","a","b","c","a","a,b","b,c","c","c")
bac_T1T2T3_nc_site_all$Sitenames <- factor(bac_T1T2T3_nc_site_all$Sitenames ,levels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))

#make ggplot of the mean % mass loss by site over time
sp <- ggplot(data=bac_T1T2T3_nc_site_all, aes(x=Sitenames, y=mean, col=Sitenames, label=Tukeylabels)) + geom_point(size=2) +
  labs(x=" ", y="Bacterial abundance", col="Site") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(axis.text.x=element_text(size=10,angle=70, hjust=1))+ #change angles and size of x axis labels
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(values=c("red", "orange", "green","blue","purple"))+#add in manual colors for points/lines
  geom_text(nudge_y=17e8) #this is for the Tukey labels, makes them black and nudges them up on y axis so they aren't directly on top of point
#why are the labls showing up on my legend?

pdf("Figures/microbialabundance/bacabundggplot/OverallMeanBacAbundbySitebytimepoint_facetwrap.pdf", height=4, width=6)  
sp + facet_wrap(~Timepoint, ncol=3)
dev.off()




write.csv(bac_T1T2T3_nc_site_all, "results/T1T2T3_Massloss_means_bysitebytime_nocontrols.csv")


####################################################################################
#Now re-do but include in situ controls
####################################################################################
#create table with just controls
#C = "Sterile"
#N = "In situ closed"
#L = "In-Situ"
controls <- which(bac_T1T2T3$Inoculum %in%c("C","L","N"))

bac_T1T2T3_controls <- bac_T1T2T3[controls, ]
bac_T1T2T3_controls$Site <- str_sub(bac_T1T2T3_controls$Sample,1,1)
bac_T1T2T3_controls$Inoculum <- str_sub(bac_T1T2T3_controls$Sample,2,2)

#pick out sterile controls
insitu <- which(bac_T1T2T3_controls$Inoculum=="L")
#get only L and N
bac_T1T2T3_LandN <- bac_T1T2T3_controls[insitu , ]

names(bac_T1T2T3_LandN)
#have to re do anova and find out what the tukey labels actually are
bac_T1T2T3_LandN$Tukeylabels <- rep(" ",nrow(bac_T1T2T3_LandN))
names(bac_T1T2T3_nc_site_all)
#need to add inoculum colum
bac_T1T2T3_nc_site_all$Inoculum <- bac_T1T2T3_nc_site_all$Sitenames
#subset controls and put in proper order
bac_T1T2T3_LandN_2 <- bac_T1T2T3_LandN[ ,c(10, 2:6,8,11,9)]


names(bac_T1T2T3_LandN_2)[7] <- "Sitenames"
names(bac_T1T2T3_nc_site_all)
#combine everything
bac_means <- rbind(bac_T1T2T3_nc_site_all,bac_T1T2T3_LandN_2 )

bac_means
#make ggplot of the meanbacterial abundance by site over time
sp <- ggplot(data=bac_means, aes(x=Sitenames, y=mean, col=Inoculum)) + geom_point(size=2) +
  labs(x=" ", y="Bacterial Abundance", col="Site", label="") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 12, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=10,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=10),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine","In-situ"), values=c("red", "orange", "green","blue","purple","burlywood2"))#add in manual colors for points/lines

sp +facet_wrap(~Timepoint, ncol=3)



#this adds on the tukey labels but I cannot figure out what is going on with the legend and how to remove the labels from it
pdf("Figures/microbialabundance/bacabundggplot/OverallMeanbacbySitebytimepoint_facetwrap_withinsitu.pdf", height=4, width=6)  

sp +facet_wrap(~Timepoint, ncol=3) + geom_text(aes(x=Sitenames, y=mean,label=Tukeylabels), nudge_y=17e8,   data=bac_means)
dev.off()

write.csv(bac_T1T2T3_nc_site_all, "results/T1T2T3_Massloss_means_bysitebytime_nocontrols.csv")




