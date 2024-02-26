cat("\014")                                                                                                                                                            # as clc in MATLAB
rm(list = ls(all.names = TRUE))                                                                                                                                      # as clear in MATLAB
# install required packages =============================================================================================================================================================
# install.packages("readxl")                                                                                                                                         # to read xlsx files
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("reshape")
# call libraries ========================================================================================================================================================================
library(readxl)
library(ggplot2)
library(ggthemes)
library(reshape)
# read data from the excel file =========================================================================================================================================================
data <- read_excel("C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\Responses_Main1_150Participants_2024-02-23.xlsx")
DensityData <- data[data[,3]=="Density",]
# Group Density =========================================================================================================================================================================
NP=154                                                                                                                                                            # number of participants
NTrain=20                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials
# NCatch=64                                                                                                                                                        # number of catch trials
NTotal=NTrain+NTest
# NMain=256                                                                                                                                     # number of main trials in the test session
# =======================================================================================================================================================================================
InstructionIndex <- matrix(, nrow = NP, ncol = 1)
SessionInd <- matrix(, nrow = NP, ncol = NTotal) 
catchTrial <- matrix(, nrow = NP, ncol = NTotal)
Radius <- matrix(, nrow = NP, ncol = NTotal) 
NumberOfAgents <- matrix(, nrow = NP, ncol = NTotal)
AgentHand <- matrix(, nrow = NP, ncol = NTotal)
ParticipantHand <- matrix(, nrow = NP, ncol = NTotal)
responseTime <- matrix(, nrow = NP, ncol = NTotal)  
DominantColor <- matrix(, nrow = NP, ncol = NTotal)   

for (a1 in 1:NP) {
  InstructionIndex[a1,1] <- as.numeric(toString(DensityData[a1,4]))                                                # 1=> right hand:yellow ;left hand:blue; 2=> right hand:blue ;left hand:yellow;
  for (a2 in 1:NTotal) {
    currentCell <- toString(DensityData[a1,a2+4])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=8,byrow=T)                                                              # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])                                                                      
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    Radius[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant's raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                                                                # left hand:1; right hand:2
    ParticipantHand[a1,a2] <- as.numeric(dataCell[6])                                                                                                          # left hand:2; right hand:1
    responseTime[a1,a2] <- as.numeric(dataCell[7])                                                                                         # this is the RT+fixation duration which was 1s
    DominantColor[a1,a2] <- as.numeric(dataCell[8])                                                                                                                     # blue:1; yellow:2
  }
}
# for (a3 in 1:NTotal) {
#   if (ParticipantHand[4,a3]==1){
#     ParticipantHand[4,a3]=2
#   }
#   else if(ParticipantHand[4,a3]==2){
#     ParticipantHand[4,a3]=1
#   }
# }
# Correct percentage in practice session =======================
SessionIndTrain <- SessionInd[,1:NTrain]                                                                      
catchTrialTrain <- catchTrial[,1:NTrain]  
RadiusTrain <- Radius[,1:NTrain]  
NumberOfAgentsTrain <- NumberOfAgents[,1:NTrain]                                                                                            # participant's raised hand (right=1; left=0)
AgentHandTrain <- AgentHand[,1:NTrain]                                                                                                          # left hand:1; right hand:2
ParticipantHandTrain <- ParticipantHand[,1:NTrain]                                                                                                           # left hand:2; right hand:1
responseTimeTrain <- responseTime[,1:NTrain]                                                                                         # this is the RT+fixation duration which was 1s
DominantColorTrain <- DominantColor[,1:NTrain]  

CorrectsTrain=(ParticipantHandTrain==2 & DominantColorTrain==1)|(ParticipantHandTrain==1 & DominantColorTrain==2)
PercentageCorrectTrain <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsTrain[a1,])
  PercentageCorrectTrain[a1,]=sum(CorrectsP)/NTrain*100
}
hist(PercentageCorrectTrain, main = "Histogram of Sample Data", xlab = "Value", ylab = "Frequency")
# finding outliers based on Hampel filter ---------------------------------------------------------------------------------------------------------------------------
correctPracticesPercentageLB=median(PercentageCorrectTrain) - 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageUB=median(PercentageCorrectTrain) + 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageOutliers=t(t(as.numeric(PercentageCorrectTrain <= correctPracticesPercentageLB)))
row(correctPracticesPercentageOutliers)[which(!correctPracticesPercentageOutliers == 0)]                                                                      # indices of outliers
# Correct percentage in test session =======================
SessionIndTest <- SessionInd[,(NTrain+1):340]                                                                      
catchTrialTest <- catchTrial[,(NTrain+1):NTotal]  
RadiusTest <- Radius[,(NTrain+1):NTotal]  
NumberOfAgentsTest <- NumberOfAgents[,(NTrain+1):NTotal]                                                                                            # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHand[,(NTrain+1):NTotal]                                                                                                          # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHand[,(NTrain+1):NTotal]                                                                                                           # left hand:2; right hand:1
responseTimeTest <- responseTime[,(NTrain+1):NTotal]                                                                                         # this is the RT+fixation duration which was 1s
DominantColorTest <- DominantColor[,(NTrain+1):NTotal] 

CorrectsCatch=((ParticipantHandTest==2 & DominantColorTest==1)|(ParticipantHandTest==1 & DominantColorTest==2))&(catchTrialTest==1)
PercentageCorrectTest <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsCatch[a1,])
  CatchsP=as.numeric(catchTrialTest[a1,]==1)
  PercentageCorrectTest[a1,]=sum(CorrectsP)/sum(CatchsP)*100
}
# histogram of correct Catch trials ------------------------------------------------------------------------------------------------------------------------------
framedData <- data.frame(1:NP,PercentageCorrectTest)
colnames(framedData) <- c('ParticipantNumber', 'correctCatchesPercentage') 

png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\CatchCorrectnessHist.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()
# finding outliers based on Hampel filter ---------------------------------------------------------------------------------------------------------------------------
correctTestsPercentageLB=median(PercentageCorrectTest) - 3 * mad(PercentageCorrectTest, constant = 1)
correctTestsPercentageUB=median(PercentageCorrectTest) + 3 * mad(PercentageCorrectTest, constant = 1)
correctTestsPercentageOutliers=t(t(as.numeric(PercentageCorrectTest <= correctTestsPercentageLB)))
row(correctTestsPercentageOutliers)[which(!correctTestsPercentageOutliers == 0)]                                                                      # indices of outliers
# removing participants 4 and 8 because of low performance ===================================================
# SessionIndTest <- SessionIndTest[-c(4,8),]    upOu                                                                  
# catchTrialTest <- catchTrialTest[-c(4,8),]     
# RadiusTest <- RadiusTest[-c(4,8),]       
# NumberOfAgentsTest <- NumberOfAgentsTest[-c(4,8),]                                                                                              # participant's raised hand (right=1; left=0)
# AgentHandTest <- AgentHandTest[-c(4,8),]                                                                                                            # left hand:1; right hand:2
# ParticipantHandTest <- ParticipantHandTest[-c(4,8),]                                                                                                             # left hand:2; right hand:1
# responseTimeTest <- responseTimeTest[-c(4,8),]                                                                                           # this is the RT+fixation duration which was 1s
# DominantColorTest <- DominantColorTest[-c(4,8),] 
# NP=NP-2                  # 2 participants removed
# Following the group in main trials (low density or high density) ==========================================================================
Follow_lowDensity1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)
Follow_highDensity1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_lowDensity1=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_highDensity1=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)

Follow_lowDensity4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==4)
Follow_highDensity4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==4)
Total_lowDensity4=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==4)
Total_highDensity4=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==4)

Follow_lowDensity7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==7)
Follow_highDensity7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==7)
Total_lowDensity7=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==7)
Total_highDensity7=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==7)

Follow_lowDensity10=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==10)
Follow_highDensity10=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==10)
Total_lowDensity10=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==10)
Total_highDensity10=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==10)

PercentageFollow_lowDensity1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_lowDensity4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_lowDensity7 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity7 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_lowDensity10 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity10 <- matrix(, nrow = NP, ncol = 1)      
for (a1 in 1:NP){
  followP_low1=as.numeric(Follow_lowDensity1[a1,])
  followP_high1=as.numeric(Follow_highDensity1[a1,])
  totalP_low1=as.numeric(Total_lowDensity1[a1,])
  totalP_high1=as.numeric(Total_highDensity1[a1,])
  PercentageFollow_lowDensity1[a1,]=sum(followP_low1)/sum(totalP_low1)*100
  PercentageFollow_highDensity1[a1,]=sum(followP_high1)/sum(totalP_high1)*100
  
  followP_low4=as.numeric(Follow_lowDensity4[a1,])
  followP_high4=as.numeric(Follow_highDensity4[a1,])
  totalP_low4=as.numeric(Total_lowDensity4[a1,])
  totalP_high4=as.numeric(Total_highDensity4[a1,])
  PercentageFollow_lowDensity4[a1,]=sum(followP_low4)/sum(totalP_low4)*100
  PercentageFollow_highDensity4[a1,]=sum(followP_high4)/sum(totalP_high4)*100
  
  followP_low7=as.numeric(Follow_lowDensity7[a1,])
  followP_high7=as.numeric(Follow_highDensity7[a1,])
  totalP_low7=as.numeric(Total_lowDensity7[a1,])
  totalP_high7=as.numeric(Total_highDensity7[a1,])
  PercentageFollow_lowDensity7[a1,]=sum(followP_low7)/sum(totalP_low7)*100
  PercentageFollow_highDensity7[a1,]=sum(followP_high7)/sum(totalP_high7)*100
  
  followP_low10=as.numeric(Follow_lowDensity10[a1,])
  followP_high10=as.numeric(Follow_highDensity10[a1,])
  totalP_low10=as.numeric(Total_lowDensity10[a1,])
  totalP_high10=as.numeric(Total_highDensity10[a1,])
  PercentageFollow_lowDensity10[a1,]=sum(followP_low10)/sum(totalP_low10)*100
  PercentageFollow_highDensity10[a1,]=sum(followP_high10)/sum(totalP_high10)*100
}
# Barplot for Following the group (low density or high density) ==========================================================================
meanFollow_lowDensity <- c(mean(PercentageFollow_lowDensity1, na.rm=TRUE),mean(PercentageFollow_lowDensity4, na.rm=TRUE),mean(PercentageFollow_lowDensity7, na.rm=TRUE),mean(PercentageFollow_lowDensity10, na.rm=TRUE))
meanFollow_highDensity <- c(mean(PercentageFollow_highDensity1, na.rm=TRUE),mean(PercentageFollow_highDensity4, na.rm=TRUE),mean(PercentageFollow_highDensity7, na.rm=TRUE),mean(PercentageFollow_highDensity10, na.rm=TRUE))

stdFollow_lowDensity <- c(sd(PercentageFollow_lowDensity1, na.rm=TRUE),sd(PercentageFollow_lowDensity4, na.rm=TRUE),sd(PercentageFollow_lowDensity7, na.rm=TRUE),sd(PercentageFollow_lowDensity10, na.rm=TRUE))
stdFollow_highDensity <- c(sd(PercentageFollow_highDensity1, na.rm=TRUE),sd(PercentageFollow_highDensity4, na.rm=TRUE),sd(PercentageFollow_highDensity7, na.rm=TRUE),sd(PercentageFollow_highDensity10, na.rm=TRUE))
confidenceFollow_lowDensity <- 1.96*stdFollow_lowDensity/sqrt(NP)
confidenceFollow_highDensity <- 1.96*stdFollow_highDensity/sqrt(NP)

NAgents <- c(seq(1,4))
df1 <- data.frame(meanFollow_lowDensity, meanFollow_highDensity, NAgents)
df2 <- melt(df1, id.vars='NAgents')
confidences <- c(confidenceFollow_lowDensity,confidenceFollow_highDensity)
df2 <- cbind(df2, confidences)
colnames(df2) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowDensity.png", width=1200, height=700)
ggplot(df2, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,4), labels = c(1, 4, 7, 10)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for Following the group (low density or high density) ==========================================================================
NAgents <- c(seq(1,4), seq(1,4))
DensityFactor <- c(rep("LowDensity",4),rep("HighDensity",4))
FollowPercentage <- c(meanFollow_lowDensity, meanFollow_highDensity)
df3 <- data.frame(DensityFactor, NAgents, FollowPercentage)
fit1=lm(FollowPercentage~DensityFactor*NAgents,data=df3)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowRegressDensity.png", width=1200, height=700)
ggplot(df3,aes(y=FollowPercentage,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ") 
dev.off()

# Response time ================================
RT_lowDensity1 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity1=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_lowDensity4 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity4=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_lowDensity7 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity7=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_lowDensity10 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity10=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==10)
# --------------------------------------------------------------------------------------
RT_highDensity1 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity1=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_highDensity4 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity4=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_highDensity7 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity7=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_highDensity10 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity10=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==10)
# --------------------------------------------------------------------------------------
for (a1 in 1:NP){
  RT_lowDensity1[a1,1:length_lowDensity1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_lowDensity4[a1,1:length_lowDensity4[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==4)]
  RT_lowDensity7[a1,1:length_lowDensity7[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==7)]
  RT_lowDensity10[a1,1:length_lowDensity10[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==10)]
  
  RT_highDensity1[a1,1:length_highDensity1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_highDensity4[a1,1:length_highDensity4[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==4)]
  RT_highDensity7[a1,1:length_highDensity7[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==7)]
  RT_highDensity10[a1,1:length_highDensity10[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==10)]
}

# Barplot for Following the group (low density or high density) ==========================================================================
meanRT_lowDensity <- c(mean(RT_lowDensity1, na.rm=TRUE),mean(RT_lowDensity4, na.rm=TRUE),mean(RT_lowDensity7, na.rm=TRUE),mean(RT_lowDensity10, na.rm=TRUE))
meanRT_highDensity <- c(mean(RT_highDensity1, na.rm=TRUE),mean(RT_highDensity4, na.rm=TRUE),mean(RT_highDensity7, na.rm=TRUE),mean(RT_highDensity10, na.rm=TRUE))

stdRT_lowDensity <- c(sd(RT_lowDensity1, na.rm=TRUE),sd(RT_lowDensity4, na.rm=TRUE),sd(RT_lowDensity7, na.rm=TRUE),sd(RT_lowDensity10, na.rm=TRUE))
stdRT_highDensity <- c(sd(RT_highDensity1, na.rm=TRUE),sd(RT_highDensity4, na.rm=TRUE),sd(RT_highDensity7, na.rm=TRUE),sd(RT_highDensity10, na.rm=TRUE))
confidenceRT_lowDensity <- 1.96*stdRT_lowDensity/sqrt(NP)
confidenceRT_highDensity <- 1.96*stdRT_highDensity/sqrt(NP)

NAgents <- c(seq(1,4))
df4 <- data.frame(meanRT_lowDensity, meanRT_highDensity, NAgents)
df5 <- melt(df4, id.vars='NAgents')
confidences <- c(confidenceRT_lowDensity,confidenceRT_highDensity)
df5 <- cbind(df5, confidences)
colnames(df5) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTDensity.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,4)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for Following the group (low density or high density) ==========================================================================
NAgents <- c(seq(1,4), seq(1,4))
DensityFactor <- c(rep("LowDensity",4),rep("HighDensity",4))
RT <- c(meanRT_lowDensity, meanRT_highDensity)
df6 <- data.frame(DensityFactor, NAgents, RT)
fit1=lm(RT~NAgents*DensityFactor,data=df6)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTRegressDensity.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()