# Script to analyze Visual search task data from the Training Study
# Author: Jonathan Erez, 2014

rm(list=ls())
library(gdata)
library(reshape)
library(ggplot2)
library(plotrix)
library(Hmisc)
library(matrixStats)
options(contrasts=c("contr.sum","contr.poly"))
#-----------------------------------Variables you might want to change -------------------------------------
#Setup 
subjects <- c("S01","S02","S03","S04","S05","S06","S07","S08","S09"
              ,"S10","S11","S12","S13","S14","S15","S16","S17","S18")
wd <- paste("C:/Main/Training Study/3. Data Analysis/Visual search task/")
DV_names <- c("RT","RT_Slopes","ACC","SAT")
allSubjects <- vector("list", length(DV_names))
names(allSubjects) <- DV_names
JPEG_WIDTH = 1000
JPEG_HEIGTH = 600
#------------------------------------------------------------------------------------------------------------

for (s in subjects){
  print(s)
  #working directory
  setwd(paste(wd,s,sep=""))
  
  # Merged ePrime file (exported as non-unicode text file)
  fileName <- paste(s,"_noU.txt",sep="")
  all_columns <- read.table(fileName,header = T,sep = "\t")

  #get rid of unnecessary columns
  #add "Subject" later maybe
  data <- all_columns[,c("ExperimentName","Session","Image_list","circleOf10.RT","CorrectResponse","circleOf10.ACC")]
  data <- within(data, {Session <- factor(Session)})
  
  #Rename factor levels
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_1"] <- "Day_1"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_2"] <- "Day_2"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_3"] <- "Day_3"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_4"] <- "Day_4"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_5"] <- "Day_5"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_Day_6"] <- "Day_6_Old"
  levels(data$ExperimentName)[levels(data$ExperimentName)=="VS_postTraining_featureSets3and4"] <- "Day_6_New"
  
  days <- length(unique(data$ExperimentName))
  #--------------------------------------------------------------------

  # <- tapply(data$circleOf10.RT,paste(data$ExperimentName,data$Session,data$Image_list,data$CorrectResponse,data$circleOf10.ACC,sep="_"),mean)
  #data <- data[data[,"CorrectResponse"]=='l' & data[,"circleOf10.ACC"]== 1,]

  #calculate RT for correct responses
  correctResponsesData <- data[data[,"circleOf10.ACC"]== 1,]
  RT <- tapply(correctResponsesData$circleOf10.RT,correctResponsesData$ExperimentName,mean)
  allSubjects$RT <- rbind(allSubjects$RT,data.frame(t(RT)))
  
  #save plot
  jpeg(paste(wd,"plots/",s,"_RT.jpg",sep=""),width=1000,height=600)
  plot(RT,xaxt = "n",
       xlab = "Training session", ylab = "RT (ms)",
       main = "RT (correct responses only)",
       pch = 16, cex=1.5)
  names <- unique(correctResponsesData$ExperimentName)
  axis(1, at=1:days, labels=as.character(names[!is.na(names)]))
  dev.off()
  
  #calculate RT slopes
  slopesSplit <- split(correctResponsesData,correctResponsesData$ExperimentName)
  slopes <- array(data=NA, dim=c(1,days))
  colnames(slopes)<-unique(data$ExperimentName)
  for (i in 1:days){
    slopes[1,i] <- lsfit(1:8,tapply(slopesSplit[[i]]$circleOf10.RT,slopesSplit[[i]]$Image_list,mean))$coefficients[2]
  }
  #save plot
  allSubjects$RT_Slopes <- rbind(allSubjects$RT_Slopes,data.frame(slopes))
  jpeg(paste(wd,"plots/",s,"_RT_Slopes.jpg",sep=""),width=1000,height=600)
  plot(slopes[1,1:days],xaxt = "n",
     xlab = "Training session", ylab = "Average slope (ms/item)",
     main = "Average visual search slopes (correct responses only)",
     pch = 16, cex=1.5)
     names <- unique(correctResponsesData$ExperimentName)
     axis(1, at=1:days, labels=as.character(names[!is.na(names)]))
  dev.off()
  
  ##discard outliers?
  #t <- tapply(data$circleOf10.RT,paste(data$Session,sep="_"),function(z) mean(z[z>2*sd(z)]))

  #Accuracy
  acc <- tapply(data$circleOf10.ACC,data$ExperimentName,mean,na.rm=T)
  allSubjects$ACC <- rbind(allSubjects$ACC,data.frame(t(acc)))
  #save plot
  jpeg(paste(wd,"plots/",s,"_ACC.jpg",sep=""),width=1000,height=600)
  plot(acc,xaxt = "n",
     xlab = "Training session", ylab = "Average Accuracy (% correct)",
     main = "Average Accuracy",
     pch = 16, cex=1.5)
     names <- unique(correctResponsesData$ExperimentName)
     axis(1, at=1:days, labels=as.character(names[!is.na(names)]))
  dev.off()

  #Speed Accuracy trade-off
  allSubjects$SAT <- allSubjects$RT/allSubjects$ACC
  jpeg(paste(wd,"plots/",s,"_SAT.jpg",sep=""),width=1000,height=600)
  plot(RT/acc,xaxt = "n",
       xlab = "Training session", ylab = "Speed-Accuracy tradeoff (RT/ACC)",
       main = "Speed-Accuracy Tradeoff",
       pch = 16, cex=1.5)
  names <- unique(correctResponsesData$ExperimentName)
  axis(1, at=1:days, labels=as.character(names[!is.na(names)]))
  dev.off()
}

##---------------------------------------------------------------------------------------------------------------
## CREATE PLOTS
##---------------------------------------------------------------------------------------------------------------

# Using my figure function...
source(paste(wd,"figure.R",sep=""))
source(paste(wd,"summarySE.R",sep=""))
plot <- figure(allSubjects$SAT,"SAT",T)
plot; dev.off()

#GGplot RT all subjects
a <- allSubjects$RT
means <- data.frame(day=c(1:7),mean=colMeans(a))
a<-cbind(subject=c(1:length(subjects)),a)
a <- melt(a, id=c("subject"))
a$subject <- as.factor(a$subject)
jpeg(paste(wd,"plots/","allSubjects_RT.jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
ggplot(a, aes(x=variable, y=value, colour=subject)) + 
  geom_line(aes(group=subject)) +
  geom_point(size=5) + 
  geom_point(data=means, aes(x=day,y=mean),colour = 'black', size=14,alpha=.7) +
  labs(x = 'Training Session',
       y = "Reaction Time (RT)",
       title = "Reaction Time (ms)")
dev.off()

#GGplot RT all subjects (with error bars)
a <- allSubjects$RT
#a<-a[,1:6]
means <- data.frame(day=c(1:6),mean=colMeans(a))
a<-cbind(subject=c(1:length(subjects)),a)
a <- melt(a, id=c("subject"))
source(paste(wd,"summarySE.R",sep=""))
dv <- summarySE(a, measurevar="value", groupvars=c("variable"))
jpeg(paste(wd,"plots/","allSubjects_RT(withErrorBars).jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
ggplot(dv, aes(x=variable, y=value)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  #geom_line(aes(group=N)) +
  geom_point(colour = 'black', size=8,alpha=.7) +
  labs(x = 'Training Session',
       y = "Reaction Time (RT)",
       title = "Reaction Time (ms)") +
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=16,angle=0,hjust=.5,vjust=-0.5,face="plain"),
        axis.title.y = element_text(colour="black",size=16,angle=90,hjust=.5,vjust=1.5,face="plain"),
        text = element_text(size=20), 
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))
  #scale_y_continuous(limits = c(900, 1400))
dev.off()

#GGplot RT_Slopes all subjects
dev.off()
a <- allSubjects$RT_Slopes
means <- data.frame(day=c(1:7),mean=colMeans(a))
a<-cbind(subject=c(1:length(subjects)),a)
a <- melt(a, id=c("subject"))
a$subject <- as.factor(a$subject)
jpeg(paste(wd,"plots/","allSubjects_RT_Slopes.jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
ggplot(a, aes(x=variable, y=value, colour=subject)) + 
  geom_line(aes(group=subject)) +
  geom_point(size=5) + 
  geom_point(data=means, aes(x=day,y=mean),colour = 'black', size=14,alpha=.5) +
  labs(x = 'Training Session',
       y = "Average slope (ms/item)",
       title = "Average visual search slopes (correct responses only)")
dev.off()

#GGplot ACC means with error bars
a <- allSubjects$ACC
means <- data.frame(day=c(1:7),mean=colMeans(a))
a<-cbind(subject=c(1:length(subjects)),a)
a <- melt(a, id=c("subject"))
#produce error bars (see function on the bottom of the page)
source(paste(wd,"summarySE.R",sep=""))
dv <- summarySE(a, measurevar="value", groupvars=c("variable"))
jpeg(paste(wd,"plots/","allSubjects_ACC.jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
ggplot(dv, aes(x=variable, y=value)) + 
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
  #geom_line(aes(group=N)) +
  geom_point(colour = 'black', size=10,alpha=.7) +
  labs(x = 'Training Session',
       y = "Accuracy (%)",
       title = "Mean Accuracy") +
  scale_y_continuous(limits = c(0.85, 1)) +
  theme(text = element_text(size=20), 
        axis.text.x=element_text(colour="black"),
        axis.text.y=element_text(colour="black"))
dev.off()

#GGplot SAT all subjects
dev.off()
a <- allSubjects$SAT
means <- data.frame(day=c(1:7),mean=colMeans(a))
a<-cbind(subject=c(1:length(subjects)),a)
a <- melt(a, id=c("subject"))
a$subject <- as.factor(a$subject)
jpeg(paste(wd,"plots/","allSubjects_SAT.jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
ggplot(a, aes(x=variable, y=value, colour=subject)) + 
  geom_line(aes(group=subject)) +
  geom_point(size=5) + 
  geom_point(data=means, aes(x=day,y=mean),colour = 'black', size=14,alpha=.5) +
  labs(x = 'Training Session',
       y = "Speed-Accuracy Tradeoff   (RT/ACC)",
       title = "Speed-Accuracy Tradeoff")
dev.off()

##-------------------------------------------------------------------------------------------
## STATS ANALYSIS
##-------------------------------------------------------------------------------------------
## Repeated measures ANOVA
a <- allSubjects$RT
a<-cbind(subject=c(1:length(subjects)),a)
a<-melt(a, id = c("subject"))
#which days to remove...
a<-a[a[,2]!="Day_6_Old",]
a<-a[a[,2]!="Day_6_New",]  
a$subject<-factor(a$subject)
aov.out <- aov(value ~ variable + Error(subject/variable), data=a)
summary(aov.out)

## Write data to an CSV file for statistical analysis in SPSS
library(foreign)
write.csv(allSubjects$RT, file=paste(wd,"allSubjects_RT.csv"))
write.csv(allSubjects$RT_Slopes, file=paste(wd,"allSubjects_RT_Slopes.csv"))
write.csv(allSubjects$ACC, file=paste(wd,"allSubjects_ACC.csv"))
write.csv(allSubjects$SAT, file=paste(wd,"allSubjects_SAT.csv"))

# Day 6: difference between RT for old and new fribbles? ( t test)
a <- allSubjects$RT
t.test(a$Day_6_New,a$Day_6_Old,alternative="greater",paired=T)
#t.test(allSubjects$RT[,6],allSubjects$RT[,7],paired=T)

# Day 6: difference between accuracy of old and new fribbles? ( t test)
a <- allSubjects$ACC
t.test(a$Day_6_New,a$Day_6_Old,alternative="greater",paired=T)
