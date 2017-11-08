#October 19, 2017
#use same codes as fungi to make nice figures and anovas


#16S library
#Reset R's Brain
rm(list=ls())

#install.packages("plyr")
library(plyr )
library(tidyverse)
library(stringr)

#set working directory
setwd("~/Dropbox/StatsandProgramming/16SElevationGradient/")

abund <- read.csv("data/FungiandBacteriaAbundance.csv", header=TRUE)
abund
#check out histogram of fungal hyphal abundance
hist(abund$hyphae_cm.gdrylitter)


####################################################################################
#separate T1, T2, T3
####################################################################################


#Do everything with abund
#create a type column for inoculum and a sample column for site by inoc combo
abund$Type <- str_sub(abund$sampleID,2,2)
abund$Sample <- str_sub(abund$sampleID,1,2)

#subset 3 time ppints
T1 <- abund[which(abund$Timepoint==1), ]
T2 <- abund[which(abund$Timepoint==2), ]




####################################################################################
#Get fungi mean sd and se with controls
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum
# Calculate the means, sd, n, and se.
fungi_T1 <- ddply(T1, "Sample", summarise,
                   T1_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                   T1_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                   T1_n = sum(!is.na( hyphae_cm.gdrylitter)),
                   T1_se = T1_sd/sqrt(T1_n)
)

head(fungi_T1)


#get mean, SD, SE of  T2 by site by inoculum
# Calculate the means, sd, n, and se.
fungi_T2 <- ddply(T2, "Sample", summarise,
                   T2_mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                   T2_sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                   T2_n = sum(!is.na( hyphae_cm.gdrylitter)),
                   T2_se = T2_sd/sqrt(T2_n)
)

head(fungi_T2)



####################################################################################
#Make a dataframe for figures with controls
####################################################################################
#####check that sample names are same for all 3
fungi_T1$Sample == fungi_T2$Sample

#make names the same so I can combine rows
names(fungi_T1) <- c("Sample","mean","sd","n","se")
names(fungi_T2) <- c("Sample","mean","sd","n","se")

#combine rows
fungi_T1T2 <- rbind(fungi_T1,fungi_T2)
#add time points
fungi_T1T2$Timepoint <- c(rep("T1",nrow(fungi_T1)),rep("T2",nrow(fungi_T2)))
#make list of colors according to Jen's color scheme and add in some for controls
listofcolors <- c("darkgrey","red","green","burlywood2","cornsilk4","blue","purple","orange")
#make list of colors that matches 
fungi_T1T2$colors <- rep(listofcolors,5)
#make a list of site names that matches
fungi_T1T2$sitenames <- c(rep("Desert",8),rep("Grassland",8),rep("Pine-Oak",8),rep("Scrubland",8),rep("Subalpine",8))
#make a list of Inoculum by substring from sample name
fungi_T1T2$Inoculum <- str_sub(fungi_T1T2$Sample,2,2)

#make factors for site names and inoculum in correct order so they show up correct on ggplot figure
fungi_T1T2$sitenames <- factor(fungi_T1T2$sitenames,levels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))
fungi_T1T2$Inoculum <- factor(fungi_T1T2$Inoculum,levels=c("D","W","G","P","S","C","L","N"))

write.csv(fungi_T1T2,"FungalAbundT1T2.csv")
####################################################################################
#Remove controls so I can make same figure with and without controls
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum
controls <- which(fungi_T1T2$Inoculum %in%c("C","L","N"))

fungi_T1T2_nc <- fungi_T1T2[-controls, ]
max(fungi_T1T2_nc$mean) #3590
min(fungi_T1T2_nc$mean) #568
####################################################################################
#Make ggplot figures, for sites*inoculum, for all 3 time points face wrap with and without controls
####################################################################################
####make figure with controls
r <- ggplot(data=fungi_T1T2, aes(x=sitenames, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Fungal Hyphae cm/g dry leaf litter leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine","Sterile","In-situ open","In-situ closed"),#manual labels for legend
                     values=c("red", "orange", "green","blue","purple","darkgrey","burlywood2","cornsilk4"))   #add in manual colors for points/lines 
  

pdf("Figures/microbialabundance/bacabundggplot/fungalabund_bysitebyinoc_T1T2T3_facetwrap_withcontrols.pdf", height=5, width=8)  
r + facet_wrap(~Timepoint, ncol=2)
dev.off()


####make figure without controls
p <- ggplot(data=fungi_T1T2_nc, aes(x=sitenames, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Fungal Hyphae cm/g dry leaf litter leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), #manual labels for legend
    values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines

pdf("Figures/microbialabundance/bacabundggplot/fungalabund_bysitebyinoc_T1T2T3_facetwrap.pdf", height=5, width=8)  
p + facet_wrap(~Timepoint, ncol=3)
dev.off()



####################################################################################
#Now make figures with inoculum against time as x axis, and facet wrap the sites 
####################################################################################
#make a T0 dataframe
fungi_T0 <- fungi_T1T2_nc[1:40, ]

fungi_T0$mean <- rep(0,40)
fungi_T0$sd <- rep(0,40)
fungi_T0$n <- rep(0,40)
fungi_T0$se <- rep(0,40)
fungi_T0$Timepoint <- rep("T0", nrow (fungi_T0))
fungi_T0

#add T0 dataframe to T1,T2,T3 dataframe
fungi_T1T2_site_all <- rbind(fungi_T0,fungi_T1T2_nc)

#make ggplot of the mean % mass loss by site over time
sp <- ggplot(data=fungi_T1T2_site_all, aes(x=Timepoint, y=mean, col=Inoculum, group=Inoculum)) + geom_point(size=1) + geom_line()+ #group and geom_line add in the connector lines
  labs(x=" ", y="Fungal Hyphae cm/g dry leaf litter leaf litter", col="Inoculum") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 14, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=12,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=12),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),
    values=c("red", "orange", "green","blue","purple"))#add in manual colors for points/lines


pdf("Figures/microbialabundance/bacabundggplot/fungalabund_5site_overtime_facetwrap2.pdf", height=5, width=8)  
sp + facet_wrap(~sitenames, ncol=3)
dev.off()

write.csv(fungi_T1T2_site_all, "results/T1T2T3_Massloss_means_bysitebytime_nocontrols2.csv")



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



####################################################################################
#ANOVAs: Normal type 1 ANOVA
####################################################################################
#Do ANOVA model to test effect of inoculum and site
modelfungi_T1<-aov(hyphae_cm.gdrylitter~Site*Inoculum,  data=T1_nc, na.action=na.omit)
summary(modelfungi_T1)
summary.lm(modelfungi_T1)

modelfungi_T2<-aov(hyphae_cm.gdrylitter~Site*Inoculum,  data=T2_nc, na.action=na.omit)
summary(modelfungi_T2)
summary.lm(modelfungi_T2)


####################################################################################
#ANOVAs: Type 2 vs Type 3 comparisons with library car
#https://www.r-bloggers.com/anova-%E2%80%93-type-iiiiii-ss-explained/
#use car package to distinguish between Type 2 and Type 3 ANOVA
####################################################################################

library(car)
######T1
Anova(lm(hyphae_cm.gdrylitter~Site*Inoculum, data=T1_nc, type=2))
modelfungi_T1_type3 <- Anova(lm(hyphae_cm.gdrylitter ~ Site*Inoculum, data=T1_nc, contrasts=list(Site=contr.sum, Inoculum=contr.sum)), type=3)
modelfungi_T1_type3

######T2
Anova(lm(hyphae_cm.gdrylitter~Site*Inoculum, data=T2_nc, type=2))
modelfungi_T2_type3 <- Anova(lm(hyphae_cm.gdrylitter ~ Site*Inoculum, data=T2_nc, contrasts=list(Site=contr.sum, Inoculum=contr.sum)), type=3)
modelfungi_T2_type3



#export it to wordfile
capture.output(modelfungi_T1_type3,file="results/T1_funabundANOVA.doc")
capture.output(modelfungi_T2_type3,file="results/T2_funabundANOVA.doc")
capture.output(summary.lm(modelfungi_T1),file="results/T1_funabundANOVA_summarylm.doc")
capture.output(summary.lm(modelfungi_T2),file="results/T2_funabundANOVA_summarylm.doc")

#export it to wordfile
#capture.output(modelfungi_T2_type3,file="results/T2_massloss.doc")

####################################################################################
#ANOVAs: Steve's way
####################################################################################
library(nlme)
m.1 <- gls(hyphae_cm.gdrylitter~Site*Inoculum,data=T1_nc,na.action="na.omit")
Anova(m.1,type=3)

m.2 <- gls(hyphae_cm.gdrylitter~Site*Inoculum,data=T2_nc,na.action="na.omit")
Anova(m.2,type=3)


####################################################################################
#Tukey HSD post hoc tests for Site
####################################################################################
library(multcomp)

######T1
modelsite_T1<-aov(hyphae_cm.gdrylitter~Site,  data=T1_nc)
tuk_T1 <- glht(modelsite_T1, linfct = mcp(Site = "Tukey")) 
tuk.cld.T1 <- cld(tuk_T1) 


######T2
modelsite_T2<-aov(hyphae_cm.gdrylitter~Site,  data=T2_nc)
tuk_T2 <- glht(modelsite_T2, linfct = mcp(Site = "Tukey")) 
tuk.cld.T2 <- cld(tuk_T2) 

### use sufficiently large upper margin 
pdf("Figures/microbialabundance/bacabundggplot/fungalabund_T1T2T3_tukey_site.pdf", height=7, width=8,pointsize=12)
old.par <- par(mai=c(1.1,0.8,1.25,0.1),mfrow=c(1,2),no.readonly = TRUE) #make enough space at top for tukey symbols and smaller spaces between figures
plot(tuk.cld.T1, ylab="Fungal Hyphae cm/g dry leaf litter", xaxt="n",xlab="" ,ylim=c(0,6000)) #add in same y limits for all, supress x axis labela nd tick marks
mtext(side=3, "T1", line=5) #add in T1 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2) #add in customized x axis labels and make them perpendicular
plot(tuk.cld.T2, ylab="Fungal Hyphae cm/g dry leaf litter",xaxt="n",xlab="",ylim=c(0,7000)) 
mtext(side=3, "T2",line=5)#add in T2 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)
par(old.par)
dev.off()



####################################################################################
#Tukey HSD post hoc tests for Inoculum
####################################################################################
library(multcomp)

######T1
modelinoc_T1<-aov(hyphae_cm.gdrylitter~Inoculum,  data=T1_nc)
tuk_T1.inoc <- glht(modelinoc_T1, linfct = mcp(Inoculum = "Tukey")) 
tuk.cld.T1.inoc <- cld(tuk_T1.inoc ) 


######T2
modelinoc_T2<-aov(hyphae_cm.gdrylitter~Inoculum,  data=T2_nc)
tuk_T2.inoc<- glht(modelinoc_T2, linfct = mcp(Inoculum = "Tukey")) 
tuk.cld.T2.inoc <- cld(tuk_T2.inoc) 


### use sufficiently large upper margin 
pdf("Figures/microbialabundance/bacabundggplot/fungalabundT1T2T3_tukey_inoc.pdf", height=7, width=8, pointsize=12)
old.par <- par(mai=c(1.1,0.8,1.25,0.1),mfrow=c(1,2),no.readonly = TRUE) #make enough space at top for tukey symbols and smaller spaces between figures
plot(tuk.cld.T1.inoc, ylab="Fungal Hyphae cm/g dry leaf litter", xaxt="n",xlab="" , ylim=c(0,6000)) #add in same y limits for all, supress x axis labela nd tick marks
mtext(side=3, "T1", line=4) #add in T1 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2) #add in customized x axis labels and make them perpendicular
plot(tuk.cld.T2.inoc, ylab="Fungal Hyphae cm/g dry leaf litter", xaxt="n",xlab="",ylim=c(0,6000)) 
mtext(side=3, "T2",line=4)#add in T2 label
axis(side=1, at=c(1,2,3,4,5), labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"), las=2)

par(old.par)
dev.off()



####################################################################################
#calculating fungiosition * Site * Inoculum * Time
####################################################################################

names(T1_nc)==names(T2_nc)
names(T1_nc)


names(T1_nc)
T1_nc2 <-T1_nc[,c(1,3,12,13,14,15,16)]
T2_nc2 <-T2_nc[,c(1,3,12,13,14,15,16) ]


#combine them all together
T1T2T3_nc2 <- rbind(T1_nc2,T2_nc2)

T1T2T3_nc2$Timepoint <- as.factor(T1T2T3_nc2$Timepoint)
T1T2T3_nc2$Inoculum <- as.factor(T1T2T3_nc2$Inoculum)

model_all<-aov(hyphae_cm.gdrylitter~Site*Inoculum*Timepoint,  data=T1T2T3_nc2)
summary(model_all)
summary.lm(model_all)
#do I need to repeat this as a GLM with timepoint as a random effect? Is it random or discrete? the samples are different
capture.output(summary(model_all),file="results/T1T2T3_fungiabund_sitebyinocbytime.doc")
capture.output(summary.lm(model_all),file="results/T1T2T3_fungiabun_sitebyinocbytime_summarylm.doc")

###################################################################################
#calculating effect sizes with eta squared
####################################################################################
#https://egret.psychol.cam.ac.uk/statistics/local_copies_of_sources_Cardinal_and_Aitken_ANOVA/glm_effectsize.htm
#https://artax.karlin.mff.cuni.cz/r-help/library/lsr/html/etaSquared.html

#install.packages("lsr")
library(lsr)

etasquaredT1T2 <- cbind(etaSquared(modelfungi_T1, type=2),etaSquared(modelfungi_T2, type=2))

colnames(etasquaredT1T2) <- c("T1 eta.sq","T1 eta.sq.part", "T2 eta.sq","T2 eta.sq.part")

etasquaredT1T2
etasquaredT1T2trans <- t(etasquaredT1T2)
####################################################################################
#calculating effect sizes with omegasquared
####################################################################################
#source in functions
#https://gist.github.com/arnoud999/e677516ed45e9a11817e
source('~/Dropbox/StatsandProgramming/source/omegas.R', chdir = TRUE)

# Omega-squared using arnaud platinga code #https://gist.github.com/arnoud999/e677516ed45e9a11817e
Omegas(modelfungi_T2)
partialOmegas(modelfungi_T2)

#using code from here: https://stats.stackexchange.com/questions/2962/omega-squared-for-measure-of-effect-in-r
omega_sq(modelfungi_T2)

#all codes come out the exact same as Steve's except steve's has an error in it bc one of his come's out neg

#ok so now caluculate omegas for all 3
omegaT1 <- rbind(Omegas(modelfungi_T1),partialOmegas(modelfungi_T1))
row.names(omegaT1) <- c("omegasT1","partialomegasT1")

omegaT2 <- rbind(Omegas(modelfungi_T2),partialOmegas(modelfungi_T2))
row.names(omegaT2) <- c("omegasT2","partialomegasT2")

#combine all into one
omegasT1T2 <- rbind(omegaT1,omegaT2)
omegasT1T2

#combine with eta squared
omegasandetas <- rbind(omegasT1T2,etasquaredT1T2trans) 
omegasandetas

write.csv(omegasandetas, "results/omegasandetasT1T2T3.csv")


####################################################################################
#Make overall site mean figure with Tukey letters on it
####################################################################################
#get mean, SD, SE of T1 and T2 by site by inoculum

# Calculate the means, sd, n, and se.
fungi_T1_site_nc <- ddply(T1_nc, "Site", summarise,
                           mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                           sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                           n = sum(!is.na( hyphae_cm.gdrylitter)),
                           se = sd/sqrt(n)
)

head(fungi_T1_site_nc)


# Calculate the means, sd, n, and se.
fungi_T2_site_nc <- ddply(T2_nc, "Site", summarise,
                           mean = mean(hyphae_cm.gdrylitter, na.rm=TRUE),
                           sd = sd(hyphae_cm.gdrylitter, na.rm=TRUE),
                           n = sum(!is.na( hyphae_cm.gdrylitter)),
                           se = sd/sqrt(n)
)

head(fungi_T2_site_nc)




#combine them all
fungi_T1T2_nc_site_all <- rbind(fungi_T1_site_nc,fungi_T2_site_nc)
fungi_T1T2_nc_site_all$Timepoint <- c(rep("T1",nrow(fungi_T1_site_nc)),rep("T2",nrow(fungi_T2_site_nc)))
fungi_T1T2_nc_site_all$Sitenames <- rep(c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"),2)
#create a vector of Tukey labels based on above tukey tests
fungi_T1T2_nc_site_all$Tukeylabels <- c("a","a","a,b","b","a,b","a","a,b","b","a,b","a")
fungi_T1T2_nc_site_all$Sitenames <- factor(fungi_T1T2_nc_site_all$Sitenames ,levels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine"))

#make ggplot of the mean % mass loss by site over time
sp <- ggplot(data=fungi_T1T2_nc_site_all, aes(x=Sitenames, y=mean, col=Sitenames, label=Tukeylabels)) + geom_point(size=2) +
  labs(x=" ", y="Fungal abundance", col="Site") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 12, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=10,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=10),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(values=c("red", "orange", "green","blue","purple"))+#add in manual colors for points/lines
  geom_text(nudge_y=400) #this is for the Tukey labels, makes them black and nudges them up on y axis so they aren't directly on top of point
#why are the labls showing up on my legend?

pdf("Figures/microbialabundance/bacabundggplot/OverallMeanFungiAbundbySitebytimepoint_facetwrap.pdf", height=4, width=6)  
sp + facet_wrap(~Timepoint, ncol=3)
dev.off()

####################################################################################
#Now re-do but include ins situ
####################################################################################
#create table with just controls
#C = "Sterile"
#N = "In situ closed"
#L = "In-Situ"

fungi_T1T2_controls <- fungi_T1T2[controls, ]
fungi_T1T2_controls$Site <- str_sub(fungi_T1T2_controls$Sample,1,1)
fungi_T1T2_controls$Inoculum <- str_sub(fungi_T1T2_controls$Sample,2,2)

#pick out sterile controls
insitu <- which(fungi_T1T2_controls$Inoculum=="L")
#get only L and N
fungi_T1T2_LandN <- fungi_T1T2_controls[insitu , ]

names(fungi_T1T2_LandN)
#have to re do anova and find out what the tukey labels actually are
fungi_T1T2_LandN$Tukeylabels <- rep(" ",nrow(fungi_T1T2_LandN))
names(fungi_T1T2_nc_site_all)
#need to add inoculum colum
fungi_T1T2_nc_site_all$Inoculum <- fungi_T1T2_nc_site_all$Sitenames
#subset controls and put in proper order
fungi_T1T2_LandN_2 <- fungi_T1T2_LandN[ ,c(10, 2:6,8,11,9)]


names(fungi_T1T2_LandN_2)[7] <- "Sitenames"
names(fungi_T1T2_nc_site_all)
#combine everything
fungi_means <- rbind(fungi_T1T2_nc_site_all,fungi_T1T2_LandN_2 )

fungi_means
#make ggplot of the meanfungiterial abundance by site over time
sp <- ggplot(data=fungi_means, aes(x=Sitenames, y=mean, col=Inoculum)) + geom_point(size=2) +
  labs(x=" ", y="Fungal Abundance", col="Site", label="") + #change y axis label to "Temp C" and remove "Date" for x axis and change legend title
  theme(strip.text.x = element_text(size = 12, colour = "black"), #make T1, T2, T3 labels bigger
        axis.text.x=element_text(size=10,angle=70, hjust=1),  #change size angle and justification of x axis labels
        axis.text.y=element_text(size=10),  #make y axis tick sizes bigger
        axis.title=element_text(size=14))+ #make y axis label larger
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.1)+  #add in standard deviation error bars +
  scale_color_manual(labels=c("Desert","Scrubland","Grassland","Pine-Oak","Subalpine","In-situ"), values=c("red", "orange", "green","blue","purple","burlywood2"))#add in manual colors for points/lines

sp +facet_wrap(~Timepoint, ncol=3)



#this adds on the tukey labels but I cannot figure out what is going on with the legend and how to remove the labels from it
pdf("Figures/microbialabundance/bacabundggplot/OverallMeanfungibySitebytimepoint_facetwrap_withinsitu.pdf", height=4, width=6)  

sp +facet_wrap(~Timepoint, ncol=3) + geom_text(aes(x=Sitenames, y=mean,label=Tukeylabels), nudge_y=400,   data=fungi_means)
dev.off()

write.csv(fungi_T1T2T3_nc_site_all, "results/T1T2T3_Massloss_means_bysitebytime_nocontrols.csv")





