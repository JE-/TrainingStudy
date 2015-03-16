# Produces a figure
# Training Study Project
# Author: Jonathan Erez, 2014
#---------------------------------------------------------
# dv = dependant variable (in a data frame)
# errorBars = T/F
# removeOutliers = T/F
# Author: Jonathan Erez, 2014

figure <- function(dv, dataType, allSubjects = T, removeOutliers = F){
  
  means <- data.frame(day=c(1:7),mean=colMeans(dv))
  dv<-cbind(subject=c(1:length(subjects)),dv)
  dv <- melt(dv, id=c("subject"))
  dv$subject <- as.factor(dv$subject)
  
  #plot name
  a <- ifelse(allSubjects,"allSubjects","means")
  b <- ifelse(removeOutliers,"removeOutliers","")
  plotName = paste(dataType,a,b,sep="_")
  
  yLabels <-c("Reaction Time (ms)","Average slope (ms/item)","Accuracy (% correct)","Speed-Accuracy Tradeoff (RT/ACC)")
  titles  <-c("Reaction Times","Average visual search slopes (correct responses only)","Accuracy (% correct)","Speed-Accuracy Tradeoff")
  
  n <- ifelse(dataType=="RT",1,(ifelse(dataType=="RT_Slopes",2,(ifelse(dataType=="ACC",3,4)))))
  
  print(dataType)
  print(n)
  
  jpeg(paste(wd,"plots/",plotName,".jpg",sep=""),width=JPEG_WIDTH,height=JPEG_HEIGTH)
  
  if (allSubjects){
  plot <- ggplot(dv, aes(x=variable, y=value, colour=subject)) + 
          geom_line(aes(group=subject)) +
          geom_point(size=5) + 
          geom_point(data=means, aes(x=day,y=mean),colour = 'black', size=14,alpha=.5) +
          labs(x = 'Training Session',
               y = yLabels[n],
               title = titles[n])
  } 
  
  if (!allSubjects){
    SE <- summarySE(dv, measurevar="value", groupvars=c("variable"))
    plot <- ggplot(SE, aes(x=variable, y=value)) + 
      geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.1) +
      geom_line(aes(group=N)) +
      geom_point(colour = 'black', size=8,alpha=.7) +
      labs(x = 'Training Session',
           y = yLabels[n],
           title = titles[n]) +
      theme(axis.text.x = element_text(colour="grey20",size=16,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=16,angle=0,hjust=.5,vjust=-0.5,face="plain"),
            axis.title.y = element_text(colour="black",size=16,angle=90,hjust=.5,vjust=1.5,face="plain"))
        #scale_y_continuous(limits = c(900, 1400)) 
  }
  return(plot)
}