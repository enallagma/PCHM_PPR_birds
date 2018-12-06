# Last updated: 3 December 2018

setwd("C:/Users/nmcintyr/Desktop/R scripts and stuff/PCHM_birds") 

install.packages("plyr")
install.packages("reshape2")
install.packages("gdata")
install.packages("tidyr")
install.packages("nlme")
install.packages("lme4")
install.packages("ggplot2")
install.packages("plyr")
install.packages("stringr")

library(plyr)
library(reshape2)
library(gdata)
library(tidyr)
library(nlme)
library(lme4)
library(ggplot2)
library(stringr)

install.packages("extrafont")
library(extrafont) 
font_import()
# answer on-screen prompt
loadfonts()
windowsFonts(Arial=windowsFont("TT Arial"))

# AMCO=American Coot, SORA=Sora, AMBI=American Bittern, BLTE=Black Tern, PBGR=Pied-billed Grebe, NOPI=Northern Pintail
# stop=occurrence, bird=abundance
# Mayponds=number of May ponds estimated from Pothole Complex Hydrological Model (PCHM)

################################
# Spearman correlation analyses:
################################

birds = read.xls("bird_data_by_block_3Dec2018.xlsx",sheet="avg by block birds ponds")
attach(birds)
names(birds)

# AMCO=American Coot, SORA=Sora, AMBI=American Bittern, BLTE=Black Tern, PBGR=Pied-billed Grebe, NOPI=Northern Pintail
# stop=occurrence, bird=abundance
# Mayponds=number of May ponds estimated from Pothole Complex Hydrological Model (PCHM)

cor(birds, method="spearman")

# To test for significant correlations (must be done on a pairwise basis), on log(value + 1) data:

# Correlations between bird occurrences and abundances (Table 2):
cor.test(logstpAMCO, logbrdAMCO, method = c("spearman"))
cor.test(logstpPBGR, logbrdPBGR, method = c("spearman"))
cor.test(logstpSORA, logbrdSORA, method = c("spearman"))
cor.test(logstpAMBI, logbrdAMBI, method = c("spearman"))
cor.test(logstpBLTE, logbrdBLTE, method = c("spearman"))
cor.test(logstpNOPI, logbrdNOPI, method = c("spearman"))

# Correlations between number of Mayponds and bird occurrences
# (Table 3, empirical entries):
cor.test(Mayponds, logstpAMCO, method = c("spearman"))
cor.test(Mayponds, logstpPBGR, method = c("spearman"))
cor.test(Mayponds, logstpSORA, method = c("spearman"))
cor.test(Mayponds, logstpAMBI, method = c("spearman"))
cor.test(Mayponds, logstpBLTE, method = c("spearman"))
cor.test(Mayponds, logstpNOPI, method = c("spearman"))

# Correlations between number of Mayponds and bird abundances
# (Table 3, empirical entries):
cor.test(Mayponds, logbrdAMCO, method = c("spearman"))
cor.test(Mayponds, logbrdPBGR, method = c("spearman"))
cor.test(Mayponds, logbrdSORA, method = c("spearman"))
cor.test(Mayponds, logbrdAMBI, method = c("spearman"))
cor.test(Mayponds, logbrdBLTE, method = c("spearman"))
cor.test(Mayponds, logbrdNOPI, method = c("spearman"))

#######################
# Time series analyses:
#######################

# Autucorrelation and partial autocorrelation for PCHM-estimated number of May ponds, Fig. S3:
par(mfrow=c(1,3))
PCHM <- ts(Mayponds)
plot(PCHM,main="A) May ponds, 1967-2005", xlab="Years", ylab="Number of May ponds", col="blue")
acf(PCHM,main="B) Autocorrelation, May ponds",col="red")
acf(PCHM,type="p",main="C) Partial autocorrelation, May ponds",col="red")

# Autocorrelations for bird occurrences, Fig. S4:
par(mfrow=c(3,2))
stopAMCO <- ts(logstpAMCO)
stopPBGR <- ts(logstpPBGR)
stopSORA <- ts(logstpSORA)
stopAMBI <- ts(logstpAMBI)
stopBLTE <- ts(logstpBLTE)
stopNOPI <- ts(logstpNOPI)
acf(stopAMCO,main="A) American Coot",col="red")
acf(stopPBGR,main="B) Pied-billed Grebe",col="red")
acf(stopSORA,main="C) Sora",col="red")
acf(stopAMBI,main="D) American Bittern",col="red")
acf(stopBLTE,main="E) Black Tern",col="red")
acf(stopNOPI,main="F) Northern Pintail",col="red")

# Autocorrelations for bird abundances, Fig. S5:
par(mfrow=c(3,2))
birdAMCO <- ts(logbrdAMCO)
birdPBGR <- ts(logbrdPBGR)
birdSORA <- ts(logbrdSORA)
birdAMBI <- ts(logbrdAMBI)
birdBLTE <- ts(logbrdBLTE)
birdNOPI <- ts(logbrdNOPI)
acf(birdAMCO,main="A) American Coot",col="red")
acf(birdPBGR,main="B) Pied-billed Grebe",col="red")
acf(birdSORA,main="C) Sora",col="red")
acf(birdAMBI,main="D) American Bittern",col="red")
acf(birdBLTE,main="E) Blact Tern",col="red")
acf(birdNOPI,main="F) Northern Pintail",col="red")

# Partial autocorrelations for bird occurrences, Fig. S6:
par(mfrow=c(3,2))
stopAMCO <- ts(logstpAMCO)
stopPBGR <- ts(logstpPBGR)
stopSORA <- ts(logstpSORA)
stopAMBI <- ts(logstpAMBI)
stopBLTE <- ts(logstpBLTE)
stopNOPI <- ts(logstpNOPI)
acf(stopAMCO,type="p",main="A) American Coot",col="red")
acf(stopPBGR,type="p",main="B) Pied-billed Grebe",col="red")
acf(stopSORA,type="p",main="C) Sora",col="red")
acf(stopAMBI,type="p",main="D) American Bittern",col="red")
acf(stopBLTE,type="p",main="E) Black Tern",col="red")
acf(stopNOPI,type="p",main="F) Northern Pintail",col="red")

# Partial autocorrelations for bird abundances, Fig. S7:
par(mfrow=c(3,2))
birdAMCO <- ts(logbrdAMCO)
birdPBGR <- ts(logbrdPBGR)
birdSORA <- ts(logbrdSORA)
birdAMBI <- ts(logbrdAMBI)
birdBLTE <- ts(logbrdBLTE)
birdNOPI <- ts(logbrdNOPI)
acf(birdAMCO,type="p",main="A) American Coot",col="red")
acf(birdPBGR,type="p",main="B) Pied-billed Grebe",col="red")
acf(birdSORA,type="p",main="C) Sora",col="red")
acf(birdAMBI,type="p",main="D) American Bittern",col="red")
acf(birdBLTE,type="p",main="E) Blact Tern",col="red")
acf(birdNOPI,type="p",main="F) Northern Pintail",col="red")

# Cross-correlations for bird occurrences, Fig. S8:
par(mfrow=c(3,2))
ccf(PCHM,stopAMCO,main="A) May ponds & American Coot occurrence", col="red")
ccf(PCHM,stopPBGR,main="B) May ponds & Pied-billed Grebe occurrence", col="red")
ccf(PCHM,stopSORA,main="C) May ponds & Sora occurrence", col="red")
ccf(PCHM,stopAMBI,main="D) May ponds & American Bittern occurrence", col="red")
ccf(PCHM,stopBLTE,main="E) May ponds & Black Tern occurrence", col="red")
ccf(PCHM,stopNOPI,main="F) May ponds & Northern Pintail occurrence", col="red")

# Cross-correlations for bird abundances, Fig. S9:
par(mfrow=c(3,2))
ccf(PCHM,birdAMCO,main="A) May ponds & American Coot abundance", col="red")
ccf(PCHM,birdPBGR,main="B) May ponds & Pied-billed Grebe abundance", col="red")
ccf(PCHM,birdSORA,main="C) May ponds & Sora abundance",col="red")
ccf(PCHM,birdAMBI,main="D) May ponds & American Bittern abundance",col="red")
ccf(PCHM,birdBLTE,main="E) May ponds & Black Tern abundance", col="red")
ccf(PCHM,birdNOPI,main="F) May ponds & Northern Pintail abundance", col="red")

# Figs. S8 & S9 clearly show the positive relationship between wetlands and
# bird, cycling with roughly decadal periodicity. 

#######################################
# Prepare data for regression analyses:
#######################################

#-------  Stops/Occurrences -----------------------------------------------------
d = read.csv("birds_and_ponds.csv")

# Convert block to categorical variable
d$block = as.character(d$block)

# Remove all columns with log (log-transformations will be performed below instead)
pos.log = grep("log", names(d))
d = d[,-pos.log]

# Put data into long format
# melt data for stops; isolate year, block, bbs_route, Mayponds, and all columns with "stop"
pos.stop = grep("stop", names(d))
d.stop = melt(d[, c(1:4, pos.stop)], id=c("Year", "block", "BBS_route", "Mayponds"), variable.name = "species", value.name = "stops")
# remove stop from the name
d.stop$species = substr(d.stop$species, 5,9)

# In order to get total stops per block (and some blocks have two routes), sum the routes within a block
# Remove NAs first to be able to sum.
d.stop =  d.stop[-which(is.na(d.stop$stops)),]
d.stop.by.block = ddply(d.stop,  .(species, block, Year), summarise, sum.mayponds = sum(Mayponds), sum.stops = sum(stops))

#-------  Abundances ------------------------------------------------------------
# Convert to long format; first, isolate columns with abundance data
pos.bird = grep("bird", names(d))
d.bird = melt(d[, c(1:4, pos.bird)], id=c("Year", "block", "BBS_route", "Mayponds"), variable.name = "species", value.name = "abundances")

# remove bird from the name
d.bird$species = substr(d.bird$species, 5,9)

# Remove NAs first to be able to sum.
d.abundances =  d.bird[-which(is.na(d.bird$abundances)),]
d.abundances.by.block = ddply(d.abundances,  .(species, block, Year), summarise, sum.mayponds = sum(Mayponds), sum.abundances = sum(abundances))

#------ Main file to use for analyses: combined occurrences and abundances ------
d.both = join(d.stop.by.block, d.abundances.by.block)

# Rename columns
names(d.both)[4:6] = c("mayponds","stops", "abund")

# How are abundances are stops related?
ggplot(d.both, aes(stops, abund)) +
  geom_point() +
  facet_wrap(~species)
# The resulting plots illustrate a sampling effect as well as differences
# in abundances among species.  These graphs are not included in the manuscript. 

# Calculate average number of Mayponds across blocks (so, basically calculate annual averages):
d.annual = ddply(d.both, .(species, Year), summarise, mean.mayponds = mean(mayponds), mean.stops = mean(stops), mean.abund = mean(abund))

# Visualize number of occurrences and abundances as functions of PCHM-estimated May ponds:
# Fig. S11:
ggplot(d.annual, aes(mean.mayponds, log(mean.stops+1))) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~species) +
  scale_x_continuous(name="Mean May ponds", limits=c(3000,6000)) +
  scale_y_continuous(name="ln(mean occurrences + 1)", limits=c(0,3))
# Fig. S12:
ggplot(d.annual, aes(mean.mayponds, log(mean.abund+1))) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~species) +
  scale_x_continuous(name="Mean May ponds", limits=c(3000,6000)) +
  scale_y_continuous(name="ln(mean abundances + 1)", limits=c(0,5))

######################
# Regression analyses:
######################

# Perform model simplification, starting with a full model.
# To determine whether to log transform or not, choose model with lowest AIC (--> model.2 for both stops and abundances)
# Check on mean.stops and mean.abund:
model.1 = gls(mean.abund~mean.mayponds*species*Year, data=d.annual, corAR1(form=~Year|species), method="ML")
model.2 = gls(log(mean.abund+1)~mean.mayponds*species*Year, data=d.annual, corAR1(form=~Year|species), method="ML")
AIC(model.1, model.2)
# Species has a significant effect; therefore, must run model for each species individually.

#--------------- Find best model for Stops/Occurrences -----------------------
# Use summary data for model (using d.annual); compare whether auto-correlation
AMCO.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="AMCO"), method="ML")

AIC(AMCO.stops.1, AMCO.stops.2, AMCO.stops.3, AMCO.stops.4) # model.3 is best; temporal auto-correlation needs to be included

AMBI.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="AMBI"), method="ML")

AIC(AMBI.stops.1, AMBI.stops.2, AMBI.stops.3, AMBI.stops.4) # model.4 (i.e. without auto-correlation is best)

BLTE.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="BLTE"), method="ML")

AIC(BLTE.stops.1, BLTE.stops.2, BLTE.stops.3, BLTE.stops.4) # model.3 is best

NOPI.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="NOPI"), method="ML")

AIC(NOPI.stops.1, NOPI.stops.2, NOPI.stops.3, NOPI.stops.4 ) # model.3 is best

PBGR.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="PBGR"), method="ML")

AIC(PBGR.stops.1, PBGR.stops.2, PBGR.stops.3, PBGR.stops.4) # model.3 is best

SORA.stops.1 = gls(log(mean.stops+1)~mean.mayponds*Year, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.stops.2 = gls(log(mean.stops+1)~mean.mayponds+Year, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.stops.3 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.stops.4 = gls(log(mean.stops+1)~mean.mayponds, data=subset(d.annual, species=="SORA"), method="ML")

AIC(SORA.stops.1, SORA.stops.2, SORA.stops.3,SORA.stops.4) 
# model.2 is best...however, the results from this model when compared to the other regressions below, were markedly different.
# (For example, the datapoints were much more tightly grouped and showed an exponential increase in the occurrence of SORA in future,
# whereas the other species' datapoints and patterns were more diffuse.)
anova(SORA.stops.2)
# An ANOVA revealed that the year effect was non significant even though including it yielded the lowest AIC. Therefore, given
# that the models of the other species had fewer parameters when excluding year, we also excluded year for SORA (went with model.3). 
# This allowed us to make more direct comparisons in regression model output across species. 

# Write function so all species can be analyzed in one go; use models with lowest AIC (exception: SORA; see explanation above)
PredictStops = function(x) {
  if (x$species[1]=="AMCO") {x$log.pred.stops = predict(AMCO.stops.3, x)}
  if (x$species[1]=="AMBI") {x$log.pred.stops = predict(AMBI.stops.4, x)} # the only one with lower AIC value without auto-correlation function
  if (x$species[1]=="BLTE") {x$log.pred.stops = predict(BLTE.stops.3, x)}
  if (x$species[1]=="NOPI") {x$log.pred.stops = predict(NOPI.stops.3, x)}
  if (x$species[1]=="PBGR") {x$log.pred.stops = predict(PBGR.stops.3, x)}
  if (x$species[1]=="SORA") {x$log.pred.stops = predict(SORA.stops.3, x)}
  x$pred = exp(x$log.pred.stops)-1
  return(x)
}

#################################
# Predicted values (occurrences):
#################################

predict(AMBI.stops.4)
AMBI1 = subset(d.annual, species=="AMBI")
AMBI1$logmeanstops=log(AMBI1$mean.stops+1)
AMBI1$predicted = exp(predict(AMBI.stops.4))-1
# test predicted stops w/ AMBI:
with(AMBI1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.4035648, p-value = 0.01085

predict(AMCO.stops.3)
AMCO1 = subset(d.annual, species=="AMCO")
AMCO1$logmeanstops=log(AMCO1$mean.stops+1)
AMCO1$predicted = exp(predict(AMCO.stops.3))-1
# test predicted stops w/ AMCO:
with(AMCO1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.6615712, p-value = 4.526e-06

predict(BLTE.stops.3)
BLTE1 = subset(d.annual, species=="BLTE")
BLTE1$logmeanstops=log(BLTE1$mean.stops+1)
BLTE1$predicted = exp(predict(BLTE.stops.3))-1
# test predicted stops w/ BLTE:
with(BLTE1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.5227342, p-value = 0.0006402

predict(NOPI.stops.3)
NOPI1 = subset(d.annual, species=="NOPI")
NOPI1$logmeanstops=log(NOPI1$mean.stops+1)
NOPI1$predicted = exp(predict(NOPI.stops.3))-1
# test predicted stops w/ NOPI:
with(NOPI1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.5040494, p-value = 0.001069

predict(PBGR.stops.3)
PBGR1 = subset(d.annual, species=="PBGR")
PBGR1$logmeanstops=log(PBGR1$mean.stops+1)
PBGR1$predicted = exp(predict(PBGR.stops.3))-1
# test predicted stops w/ PBGR:
with(PBGR1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.5257403, p-value = 0.0005879

predict(SORA.stops.3)
SORA1 = subset(d.annual, species=="SORA")
SORA1$logmeanstops=log(SORA1$mean.stops+1)
SORA1$predicted = exp(predict(SORA.stops.3))-1
# test predicted stops w/ SORA:
with(SORA1,cor.test(predicted,mean.stops,method = c("spearman")))
#returns a correlation coefficient = 0.6171677, p-value = 2.855e-05

#--------------- Find best model for Abundances -----------------------
AMCO.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="AMCO"), correlation=corAR1(form=~Year), method="ML")
AMCO.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="AMCO"), method="ML")

AIC(AMCO.abundances.1, AMCO.abundances.2, AMCO.abundances.3, AMCO.abundances.4) # model.3 is best

AMBI.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="AMBI"), correlation=corAR1(form=~Year), method="ML")
AMBI.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="AMBI"), method="ML")

AIC(AMBI.abundances.1, AMBI.abundances.2, AMBI.abundances.3, AMBI.abundances.4) # model.4 is best

BLTE.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="BLTE"), correlation=corAR1(form=~Year), method="ML")
BLTE.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="BLTE"), method="ML")

AIC(BLTE.abundances.1, BLTE.abundances.2, BLTE.abundances.3, BLTE.abundances.4) # model.3 is best

NOPI.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="NOPI"), correlation=corAR1(form=~Year), method="ML")
NOPI.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="NOPI"), method="ML")

AIC(NOPI.abundances.1, NOPI.abundances.2, NOPI.abundances.3, NOPI.abundances.4)# model.3 is best

PBGR.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="PBGR"), correlation=corAR1(form=~Year), method="ML")
PBGR.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="PBGR"), method="ML")

AIC(PBGR.abundances.1, PBGR.abundances.2, PBGR.abundances.3, PBGR.abundances.4) # model.3 is best

SORA.abundances.1 = gls(log(mean.abund+1)~mean.mayponds*Year, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.abundances.2 = gls(log(mean.abund+1)~mean.mayponds+Year, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.abundances.3 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="SORA"), correlation=corAR1(form=~Year), method="ML")
SORA.abundances.4 = gls(log(mean.abund+1)~mean.mayponds, data=subset(d.annual, species=="SORA"), method="ML")

AIC(SORA.abundances.1, SORA.abundances.2, SORA.abundances.3, SORA.abundances.4) 
# model.2 is best...however, the results from this model when compared to the other regressions below, were markedly different.
# (For example, the datapoints were much more tightly grouped and showed an exponential increase in SORA abundance in future,
# whereas the other species' datapoints and patterns were more diffuse.)

anova(SORA.abundances.2)
# An ANOVA revealed that the year effect was non significant even though including it yielded the lowest AIC. Therefore, given
# that the models of the other species had fewer parameters when excluding year, we also excluded year for SORA (went with model.3). 
# This allowed us to make more direct comparisons in regression model output across species.

# Write function so all species can be analyzed in one go; use models with lowest AIC (exception: SORA; see explanation above)
PredictAbundances = function(x) {
  if (x$species[1]=="AMCO") {x$log.pred.abund = predict(AMCO.abundances.3, x)}
  if (x$species[1]=="AMBI") {x$log.pred.abund = predict(AMBI.abundances.4, x)} # the only one with lower AIC value without auto-correlation function
  if (x$species[1]=="BLTE") {x$log.pred.abund = predict(BLTE.abundances.3, x)}
  if (x$species[1]=="NOPI") {x$log.pred.abund = predict(NOPI.abundances.3, x)}
  if (x$species[1]=="PBGR") {x$log.pred.abund = predict(PBGR.abundances.3, x)}
  if (x$species[1]=="SORA") {x$log.pred.abund = predict(SORA.abundances.3, x)}
  x$pred = exp(x$log.pred.abund)-1
  return(x)
}

################################
# Predicted values (abundances):
################################

predict(AMBI.abundances.4)
AMBI2 = subset(d.annual, species=="AMBI")
AMBI2$logmeanabundances=log(AMBI2$mean.abund+1)
AMBI2$predicted = exp(predict(AMBI.abundances.4))-1
# test predicted abundances w/ AMBI:
with(AMBI2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.3632038, p-value = 0.02305

predict(AMCO.abundances.3)
AMCO2 = subset(d.annual, species=="AMCO")
AMCO2$logmeanabundances=log(AMCO2$mean.abund+1)
AMCO2$predicted = exp(predict(AMCO.abundances.3))-1
# test predicted abundances w/ AMCO:
with(AMCO2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.4465587, p-value = 0.004735

predict(BLTE.abundances.3)
BLTE2 = subset(d.annual, species=="BLTE")
BLTE2$logmeanabundances=log(BLTE2$mean.abund+1)
BLTE2$predicted = exp(predict(BLTE.abundances.3))-1
# test predicted abundances w/ BLTE:
with(BLTE2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.4513386, p-value = 0.003927

predict(NOPI.abundances.3)
NOPI2 = subset(d.annual, species=="NOPI")
NOPI2$logmeanabundances=log(NOPI2$mean.abund+1)
NOPI2$predicted = exp(predict(NOPI.abundances.3))-1
# test predicted abundances w/ NOPI:
with(NOPI2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.5157144, p-value = 0.000779

predict(PBGR.abundances.3)
PBGR2 = subset(d.annual, species=="PBGR")
PBGR2$logmeanabundances=log(PBGR2$mean.abund+1)
PBGR2$predicted = exp(predict(PBGR.abundances.3))-1
# test predicted abundances w/ PBGR:
with(PBGR2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.4555117, p-value = 0.003569

predict(SORA.abundances.3)
SORA2 = subset(d.annual, species=="SORA")
SORA2$logmeanabundances=log(SORA2$mean.abund+1)
SORA2$predicted = exp(predict(SORA.abundances.3))-1
# test predicted abundances w/ SORA:
with(SORA2,cor.test(predicted,mean.abund,method = c("spearman")))
#returns a correlation coefficient = 0.588277, p-value = 8.196e-05

#########################
# Observed vs. Predicted:
#########################

# Fig. 4:

occur <- read.csv("obs_pred_melted.csv",header=TRUE,nrows=234)
# These data are log(value+1)-transformed and averaged across blocks
# for each year, in melted format.
stops <- occur[,c("year","Avg_Mayponds","species","stops","preds","abund","preda")]
stops$label <- "A"
stops[stops$species=="AMCO",]$label <- "B"
stops[stops$species=="BLTE",]$label <- "C"
stops[stops$species=="NOPI",]$label <- "D"
stops[stops$species=="PBGR",]$label <- "E"
stops[stops$species=="SORA",]$label <- "F"

options(scipen = 50, digits = 5)
p1 <- ggplot(stops,aes(x=year)) + geom_line(aes(y=stops),colour="red",size=1) + 
geom_line(aes(y = Avg_Mayponds/1000),colour="blue",size=1) + 
geom_line(aes(y = preds), colour="red",lty="dashed",size=1) +
scale_y_log10(sec.axis = sec_axis(trans = ~.*1000, name = "Number of waterbodies"))
p1 <- p1 + labs(x = "Year", y = "Stops")
fig_4 <- p1 + facet_wrap( ~ label, nrow = 2) + theme_bw() + 
theme(axis.text=element_text(family="Arial",size=10),axis.title=element_text(family="Arial",size=14))
fig_4

# Fig. 5:

abund <- read.csv("obs_pred_melted.csv",header=TRUE,nrows=234)
# These data are log(value+1)-transformed and averaged across blocks
# for each year, in melted format.
birds <- abund[,c("year","Avg_Mayponds","species","stops","preds","abund","preda")]
birds$label <- "A"
birds[birds$species=="AMCO",]$label <- "B"
birds[birds$species=="BLTE",]$label <- "C"
birds[birds$species=="NOPI",]$label <- "D"
birds[birds$species=="PBGR",]$label <- "E"
birds[birds$species=="SORA",]$label <- "F"

options(scipen = 50, digits = 5)
p2 <- ggplot(stops,aes(x=year)) + geom_line(aes(y=abund),colour="brown",size=1) + 
geom_line(aes(y = Avg_Mayponds/1000),colour="blue",size=1) + 
geom_line(aes(y = preda), colour="brown",lty="dashed",size=1) +
scale_y_log10(sec.axis = sec_axis(trans = ~.*1000, name = "Number of waterbodies")) 
p2 <- p2 + labs(x = "Year", y = "Abundance")
fig_5 <- p2 + facet_wrap( ~ label, nrow = 2) + theme_bw() +
theme(axis.text=element_text(family="Arial",size=10),axis.title=element_text(family="Arial",size=14))
fig_5

# Fig. S10:

# Panel A:

# Bird occurrences (includes 1:1 line in black):
occurrences = ddply(d.annual, .(species), PredictStops)
dlply(occurrences, .(species), function(x) return(summary(lm(pred~mean.stops, data=x))))
# There are significant departures in observed vs. predicted occurrences of birds for all species,.
ggplot(occurrences, aes(mean.stops, pred)) +
  geom_point(aes(col=species)) +
  geom_line(data=data.frame(mean.stops = seq(0,20), pred = seq(0,20))) +
  theme_bw() +
  labs(y="Predicted number of occurrences", x="Observed number of occurrences")
# This plot reveals higher predicted than observed bird occurrences at
# low occurrence, and lower predicted than observed at greater occurrences.

with(occurrences,cor.test(pred,mean.stops,method = c("spearman")))
# returns a correlation coefficient of 0.647033, p-value < 2.2e-16

# Panel B:

# Bird abundances (includes 1:1 line in black):
test = ddply(d.annual, .(species), PredictAbundances)
dlply(test, .(species), function(x) return(summary(lm(pred~mean.abund, data=x))))
# There are significant departures in observed vs. predicted abundances of 
# birds for all species except Northern Pintail (possibly due to outlier
# year 1970 when a much larger flock than usual was observed).
ggplot(test, aes(mean.abund, pred)) +
  geom_point(aes(col=species)) +
  geom_line(data=data.frame(mean.abund = seq(0,30), pred = seq(0,30))) +
  theme_bw() +
  labs(y="Predicted number of abundances", x="Observed number of abundances")
# This graph indicates that we will likely have a better fit for abundances
# than for occurrences in our projections.

with(test,cor.test(pred,mean.abund,method = c("spearman")))
# returns a correlation coefficient of 0.7476825, p-value < 2.2e-16

# These correlations indicate that our modeled (predicted) values do a good 
# job of reproducing patterns in the observed data. 

################################################################################
# Projections to 2090-2099 (decadal average rather than a single endpoint year):
################################################################################

maypond.pred = read.csv("downscaled_climate.csv")

# Convert block to character
maypond.pred$block = as.character(maypond.pred$block)

# Melt data frame
maypond.pred.melted = melt(maypond.pred,id=c("Year", "block", "BBS_route"), variable.name = "model", value.name = "Mayponds" )

# Calculate number of mayponds per block (so, if two routes are in the same block, they will be summed) 
maypond.per.block = ddply(maypond.pred.melted,  .(model, block, Year), summarise, sum.mayponds = sum(Mayponds))

# Obtain average data (across blocks); mean.ponds here refers to mean modeled (predicted) ponds.
maypond.avg = ddply(maypond.per.block, .(Year,model), summarise, mean.mayponds = mean(sum.mayponds, na.rm=T))

# Repeat this data frame 6 times (once for each species)
maypond.avg.species = maypond.avg[rep(1:nrow(maypond.avg), times=6), ]

# Add species data (every species has a different model associated with it based on AIC)
maypond.avg.species$species = rep(unique(d.annual$species), each=nrow(maypond.avg))

# Use function PredictStops to predict number of occurrences (current and future)
pred.stops = ddply(maypond.avg.species, .(species, model), PredictStops)

# How do predicted average number of ponds change over time?
# Fig. S2:
ggplot(maypond.avg, aes(Year, mean.mayponds)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~model) +
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean May ponds")
# This graph shows that although most of the climate models by emissions scenarios 
# predict an larger number of May ponds present by 2099, one model (HADCM3B1) predicts
# a decrease in May pond numbers, and one model (PCMA1F1) predicts no change by end of century.
# There is thus a difference in the climate models-emissions scenarios.

# How does predicted average number of ponds change over time across models (model ensemble)? 
# Fig. S1:
maypond.ensemble = ddply(maypond.avg, .(Year), summarise, maypond = mean(mean.mayponds))
ggplot(maypond.ensemble, aes(Year, maypond)) +
  geom_point() +
  geom_smooth(method="lm") + 
  scale_x_continuous(name="Year") +
  scale_y_continuous(name="Mean May ponds")

# Fig. S13:
ggplot(pred.stops, aes(Year, pred)) +
  geom_point() +
  labs(y="Predicted occurrences") +
  geom_smooth(method="lm") +
  facet_grid(species~model, scales="free")

# Use function PredictAbundances to predict number of abundances (current and future):
pred.abundances = ddply(maypond.avg.species, .(species, model), PredictAbundances)

# Fig. S14:
ggplot(pred.abundances, aes(Year, pred)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(y="Predicted Abundances") +
  facet_grid(species~model, scales="free")

###########################
# Statistical calculations:
###########################

# MAYPOND STATS (BASED ON DOWNSCALED DATA)
# Average number of wetlands 1967-2005 across models
current = droplevels(subset(maypond.avg, Year >=1967 & Year <=2005))
current.by.model = ddply(current, .(model), summarise, n.ponds = mean(mean.mayponds))
model.ensemble.current = mean(current.by.model$n.ponds)
model.ensemble.current   # returns a value of 1665.804

# Average number of wetlands in final decade (2090-2099)
future = droplevels(subset(maypond.avg, Year >=2090))
future.by.model = ddply(future, .(model), summarise, n.ponds = mean(mean.mayponds))
model.ensemble.future = mean(future.by.model$n.ponds)
model.ensemble.future   # returns a value of 1861.581

change = model.ensemble.future/model.ensemble.current
change   
# returns a value of 1.1175, indicating an 11.75% increase in the number of wetlands predicted in
# final decade (2090-2099) compared to 1967-2005

# PCHM-estmated ponds for 1967-2005, using one bird species as an example (since number of ponds is same for all species)
avg.obs.ponds = mean(subset(d.annual, species=="AMCO")$mean.mayponds)
avg.obs.ponds	# returns a value of 4291.7 as average number of May ponds in 1967-2005

# OCCURRENCES STATS - MODELLED (NOT PCHM-ESTIMATED) (units in number of stops [occurrences] - already backtransformed)
# 1967-2005 across climate models-emissions scenarios
current.stops = droplevels(subset(pred.stops, Year >=1967 & Year <=2005))
current.stops.by.model = ddply(current.stops, .(species, model), summarise, n.stops.current = mean(pred))
current.stops.ensemble = mean(current.stops.by.model$n.stops.current)
current.stops.ensemble	# returns a value of 1.0265
  
# final decade (2090-2099)
future.stops = droplevels(subset(pred.stops, Year >=2090))
future.stops.by.model = ddply(future.stops, .(species, model), summarise, n.stops.future = mean(pred))
future.stops.ensemble = mean(future.stops.by.model$n.stops.future)
future.stops.ensemble	# returns a value of 1.1493
  
# Change in stops - for each climate model-emissions scenario
change.stops = join(current.stops.by.model, future.stops.by.model)
change.stops
# Table S1: in a list of current and future occurrences for birds by climate model-emissions scenario
  
# Change in stops - based on model ensemble
change.stops.ensemble = future.stops.ensemble/current.stops.ensemble
change.stops.ensemble	
# returns a value of 1.1197, indicating an 11.97% increase in overall bird occurrence in 2090-2099
# compared to 1967-2005

# ABUNDANCE STATS (units in abundances - already backtransformed)
# 1967-2005 across climate models-emissions scenarios
current.abundances = droplevels(subset(pred.abundances, Year >=1967 & Year <=2005))
current.abundances.by.model = ddply(current.abundances, .(species, model), summarise, n.abundances.current = mean(pred))
current.abundances.ensemble = mean(current.abundances.by.model$n.abundances.current)
current.abundances.ensemble	# returns a value of 2.8665
  
# final decade (2090-2099)
future.abundances = droplevels(subset(pred.abundances, Year >=2090))
future.abundances.by.model = ddply(future.abundances, .(species, model), summarise, n.abundances.future = mean(pred))
future.abundances.ensemble = mean(future.abundances.by.model$n.abundances.future)
future.abundances.ensemble	# returns a value of 3.1139
  
# Change in abundances - for each climate model-emissions scenario
change.abundances = join(current.abundances.by.model, future.abundances.by.model)
change.abundances
# Table S2: in a list of current and future abundances for birds by climate model-emissions scenario

# Change in abundances - based on model ensemble
change.abundances.ensemble = future.abundances.ensemble/current.abundances.ensemble
change.abundances.ensemble
# returns a value of 1.0863, indicating an 8.63% increase in overall bird abundance in 2090-2099
# compared to 1967-2005

# Data for Fig. 6:
current.stops = droplevels(subset(pred.stops, Year >=1967 & Year <=2005))
current.stops.by.species = ddply(current.stops, .(species), summarise, n.stops.current = mean(pred))
current.stops.species.ensemble = mean(current.stops.by.species$n.stops.current)
current.stops.species.ensemble	
future.stops = droplevels(subset(pred.stops, Year >=2090))
future.stops.by.species = ddply(future.stops, .(species), summarise, n.stops.future = mean(pred))
future.stops.species.ensemble = mean(future.stops.by.species$n.stops.future)
future.stops.species.ensemble	
change.stops.species = join(current.stops.by.species, future.stops.by.species)
change.stops.species

current.abundances = droplevels(subset(pred.abundances, Year >=1967 & Year <=2005))
current.abundances.by.species = ddply(current.abundances, .(species), summarise, n.abundances.current = mean(pred))
current.abundances.species.ensemble = mean(current.abundances.by.species$n.abundances.current)
current.abundances.species.ensemble	
future.abundances = droplevels(subset(pred.abundances, Year >=2090))
future.abundances.by.species = ddply(future.abundances, .(species), summarise, n.abundances.future = mean(pred))
future.abundances.species.ensemble = mean(future.abundances.by.species$n.abundances.future)
future.abundances.species.ensemble	  
change.abundances.species = join(current.abundances.by.species, future.abundances.by.species)
change.abundances.species




