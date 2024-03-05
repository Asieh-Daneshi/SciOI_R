cat("\014")                                                                                                                                                           # as clc in MATLAB
rm(list = ls(all.names = TRUE))                                                                                                                                     # as clear in MATLAB
# install required packages ============================================================================================================================================================
# install.packages("readxl")                                                                                                                                        # to read xlsx files
# install.packages("ggplot2")
# install.packages("ggthemes")
# install.packages("reshape")
# install.packages("Matrix")
# call libraries =======================================================================================================================================================================
library(readxl)
library(ggplot2)
library(ggthemes)
library(reshape)
# read data from the excel file ========================================================================================================================================================
data <- read_excel("C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\Responses_Pilot2_10Participants_2024-03-05.xlsx")
DensityData <- data[data[,3]=="Density",]
# Group Density ========================================================================================================================================================================
NP=10                                                                                                                                                         # number of participants
NTrain=20                                                                                                                                                    # number of practice trials
NTest=300                                                                                                                                                        # number of test trials                                                                                                                                                       # number of catch trials
NTotal=NTrain+NTest                                                                                                                          # number of main trials in the test session
# ======================================================================================================================================================================================
# Organizing data ======================================================================================================================================================================
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
  InstructionIndex[a1,1] <- as.numeric(toString(DensityData[a1,4]))                                       # 1=> right hand:yellow ;left hand:blue; 2=> right hand:blue ;left hand:yellow;
  for (a2 in 1:NTotal) {
    currentCell <- toString(DensityData[a1,a2+4])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=8,byrow=T)                                                            # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])                                                                      
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    Radius[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant' raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                                                               # left hand:1; right hand:2
    ParticipantHand[a1,a2] <- as.numeric(dataCell[6])                                                                                                         # left hand:2; right hand:1
    responseTime[a1,a2] <- as.numeric(dataCell[7])                                                                                        # this is the RT+fixation duration which was 1s
    DominantColor[a1,a2] <- as.numeric(dataCell[8])                                                                                                                    # blue:1; yellow:2
  }
}
# =======================================================================================================================================================================================
# Correct percentage in practice session ================================================================================================================================================
SessionIndTrain <- SessionInd[,1:NTrain]                                                                      
catchTrialTrain <- catchTrial[,1:NTrain]  
RadiusTrain <- Radius[,1:NTrain]  
NumberOfAgentsTrain <- NumberOfAgents[,1:NTrain]                                                                                            # participant's raised hand (right=1; left=0)
AgentHandTrain <- AgentHand[,1:NTrain]                                                                                                                        # left hand:1; right hand:2
ParticipantHandTrain <- ParticipantHand[,1:NTrain]                                                                                                            # left hand:2; right hand:1
responseTimeTrain <- responseTime[,1:NTrain]                                                                                              # this is the RT+fixation duration which was 1s
DominantColorTrain <- DominantColor[,1:NTrain]  

CorrectsTrain=(ParticipantHandTrain==2 & DominantColorTrain==1)|(ParticipantHandTrain==1 & DominantColorTrain==2)
PercentageCorrectTrain <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsTrain[a1,])
  PercentageCorrectTrain[a1,]=sum(CorrectsP)/NTrain*100
}
# finding outliers based on Hampel filter -----------------------------------------------------------------------------------------------------------------------------------------------
correctPracticesPercentageLB=median(PercentageCorrectTrain) - 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageUB=median(PercentageCorrectTrain) + 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageOutliers=t(t(as.numeric(PercentageCorrectTrain <= correctPracticesPercentageLB)))
row(correctPracticesPercentageOutliers)[which(!correctPracticesPercentageOutliers == 0)]                                                                            # indices of outliers

# =======================================================================================================================================================================================
# Correct percentage in test session ====================================================================================================================================================
SessionIndTest <- SessionInd[,(NTrain+1):NTotal]                                                                      
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
# Histogram of correct Catch trials ====================================================================================================================================================
framedData <- data.frame(1:NP,PercentageCorrectTest)
colnames(framedData) <- c('ParticipantNumber', 'correctCatchesPercentage') 
png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistDensity.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()
# finding outliers based on Hampel filter ==============================================================================================================================================
# correctTestsPercentageLB=median(PercentageCorrectTest) - 3 * mad(PercentageCorrectTest, constant = 1)
# correctTestsPercentageUB=median(PercentageCorrectTest) + 3 * mad(PercentageCorrectTest, constant = 1)
# correctTestsPercentageOutliers=t(t(as.numeric(PercentageCorrectTest <= correctTestsPercentageLB)))
# MyOutliers=row(correctTestsPercentageOutliers)[which(!correctTestsPercentageOutliers == 0)]                                                                        # indices of outliers
# # removing participants MyOutliers because of their low performance in catch trials ====================================================================================================
# SessionIndTest <- SessionIndTest[-MyOutliers,]
# catchTrialTest <- catchTrialTest[-MyOutliers,]
# RadiusTest <- RadiusTest[-MyOutliers,]
# NumberOfAgentsTest <- NumberOfAgentsTest[-MyOutliers,]                                                                                     # participant's raised hand (right=1; left=0)
# AgentHandTest <- AgentHandTest[-MyOutliers,]                                                                                                                 # left hand:1; right hand:2
# ParticipantHandTest <- ParticipantHandTest[-MyOutliers,]                                                                                                     # left hand:2; right hand:1
# responseTimeTest <- responseTimeTest[-MyOutliers,]                                                                                       # this is the RT+fixation duration which was 1s
# DominantColorTest <- DominantColorTest[-MyOutliers,]
# NP=NP-length(MyOutliers)                                                                                                                                       # 36 participants removed

# Here we subtract 1 from all response times, because these response times include 1 second for fixation ===============================================================================
responseTimeTrain=responseTimeTrain-1
responseTimeTest=responseTimeTest-1
# Considering only the trials that the participant responded after the agents (200 ms) =================================================================================================
responseTimeTest_AP=matrix(, nrow = NP, ncol = NTest)        # after participant
for (a1 in 1:NP){
  responseTimeTestP=as.numeric(responseTimeTest[a1,]>=0.2)
  responseTimeTest_AP[a1,0:length(responseTimeTestP)] <- responseTimeTestP
}
RadiusTest = RadiusTest*responseTimeTest_AP
NumberOfAgentsTest = NumberOfAgentsTest*responseTimeTest_AP                                                                                # participant's raised hand (right=1; left=0)
AgentHandTest = AgentHandTest*responseTimeTest_AP                                                                                                           # left hand:1; right hand:2
ParticipantHandTest = ParticipantHandTest*responseTimeTest_AP                                                                                               # left hand:2; right hand:1
responseTimeTest = responseTimeTest*responseTimeTest_AP                                                                                 # this is the RT+fixation duration which was 1s
DominantColorTest = DominantColorTest*responseTimeTest_AP
# =======================================================================================================================================================================================
# Comupting correct percentage after removing the outliers ==============================================================================================================================
CorrectsTrain=(ParticipantHandTrain==2 & DominantColorTrain==1)|(ParticipantHandTrain==1 & DominantColorTrain==2)
PercentageCorrectTrain <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsTrain[a1,])
  PercentageCorrectTrain[a1,]=sum(CorrectsP)/NTrain*100
}

CorrectsCatch=((ParticipantHandTest==2 & DominantColorTest==1)|(ParticipantHandTest==1 & DominantColorTest==2))&(catchTrialTest==1)
PercentageCorrectTest <- matrix(, nrow = NP, ncol = 1)
for (a1 in 1:NP){
  CorrectsP=as.numeric(CorrectsCatch[a1,])
  CatchsP=as.numeric(catchTrialTest[a1,]==1)
  PercentageCorrectTest[a1,]=sum(CorrectsP)/sum(CatchsP)*100
}

# histogram of correct Catch trials after removing outliers =============================================================================================================================
framedData <- data.frame(1:NP,PercentageCorrectTest)
colnames(framedData) <- c('ParticipantNumber', 'correctCatchesPercentage') 

png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistOutlierRemovedDensityOnlyAfterP.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()

# ***************************************************************************************************************************************************************************************
# Following the group in main trials (low density or high density) ======================================================================================================================
# ***************************************************************************************************************************************************************************************
Follow_lowDensity1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)
Follow_highDensity1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_lowDensity1=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_highDensity1=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)

Follow_lowDensity3=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==3)
Follow_highDensity3=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==3)
Total_lowDensity3=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==3)
Total_highDensity3=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==3)

Follow_lowDensity5=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==5)
Follow_highDensity5=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==5)
Total_lowDensity5=(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==5)
Total_highDensity5=(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==5)

PercentageFollow_lowDensity1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_lowDensity3 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity3 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_lowDensity5 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_highDensity5 <- matrix(, nrow = NP, ncol = 1)     
     
for (a1 in 1:NP){
  followP_low1=as.numeric(Follow_lowDensity1[a1,])
  followP_high1=as.numeric(Follow_highDensity1[a1,])
  totalP_low1=as.numeric(Total_lowDensity1[a1,])
  totalP_high1=as.numeric(Total_highDensity1[a1,])
  PercentageFollow_lowDensity1[a1,]=sum(followP_low1)/sum(totalP_low1)*100
  PercentageFollow_highDensity1[a1,]=sum(followP_high1)/sum(totalP_high1)*100
  
  followP_low3=as.numeric(Follow_lowDensity3[a1,])
  followP_high3=as.numeric(Follow_highDensity3[a1,])
  totalP_low3=as.numeric(Total_lowDensity3[a1,])
  totalP_high3=as.numeric(Total_highDensity3[a1,])
  PercentageFollow_lowDensity3[a1,]=sum(followP_low3)/sum(totalP_low3)*100
  PercentageFollow_highDensity3[a1,]=sum(followP_high3)/sum(totalP_high3)*100
  
  followP_low5=as.numeric(Follow_lowDensity5[a1,])
  followP_high5=as.numeric(Follow_highDensity5[a1,])
  totalP_low5=as.numeric(Total_lowDensity5[a1,])
  totalP_high5=as.numeric(Total_highDensity5[a1,])
  PercentageFollow_lowDensity5[a1,]=sum(followP_low5)/sum(totalP_low5)*100
  PercentageFollow_highDensity5[a1,]=sum(followP_high5)/sum(totalP_high5)*100
}
# =======================================================================================================================================================================================
# Barplot for Following the group (low density or high density) =========================================================================================================================
meanFollow_lowDensity <- c(mean(PercentageFollow_lowDensity1, na.rm=TRUE),mean(PercentageFollow_lowDensity3, na.rm=TRUE),mean(PercentageFollow_lowDensity5, na.rm=TRUE))
meanFollow_highDensity <- c(mean(PercentageFollow_highDensity1, na.rm=TRUE),mean(PercentageFollow_highDensity3, na.rm=TRUE),mean(PercentageFollow_highDensity5, na.rm=TRUE))

stdFollow_lowDensity <- c(sd(PercentageFollow_lowDensity1, na.rm=TRUE),sd(PercentageFollow_lowDensity3, na.rm=TRUE),sd(PercentageFollow_lowDensity5, na.rm=TRUE))
stdFollow_highDensity <- c(sd(PercentageFollow_highDensity1, na.rm=TRUE),sd(PercentageFollow_highDensity3, na.rm=TRUE),sd(PercentageFollow_highDensity5, na.rm=TRUE))
confidenceFollow_lowDensity <- 1.96*stdFollow_lowDensity/sqrt(NP)
confidenceFollow_highDensity <- 1.96*stdFollow_highDensity/sqrt(NP)

NAgents <- c(seq(1,3))
df1 <- data.frame(meanFollow_lowDensity, meanFollow_highDensity, NAgents)
df2 <- melt(df1, id.vars='NAgents')
confidences <- c(confidenceFollow_lowDensity,confidenceFollow_highDensity)
df2 <- cbind(df2, confidences)
colnames(df2) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowDensityOnlyAfterP.png", width=1200, height=700)
ggplot(df2, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,3), labels = c(1, 3, 5)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()
# =======================================================================================================================================================================================
# Regress plot for Following the group (low density or high density) ====================================================================================================================
NAgents <- c(seq(1,3), seq(1,3))
DensityFactor <- c(rep("LowDensity",3),rep("HighDensity",3))
FollowPercentage <- c(meanFollow_lowDensity, meanFollow_highDensity)
df3 <- data.frame(DensityFactor, NAgents, FollowPercentage)
fit1=lm(FollowPercentage~DensityFactor*NAgents,data=df3)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowRegressDensityOnlyAfterP.png", width=1200, height=700)
ggplot(df3,aes(y=FollowPercentage,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),6))
density <- c(rep("LowDensity",NP*3),rep("HighDensity",NP*3))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP),rep(3,NP)),2))
FollowPercentage <- c(PercentageFollow_lowDensity1,PercentageFollow_lowDensity3,PercentageFollow_lowDensity5,PercentageFollow_highDensity1,PercentageFollow_highDensity3,PercentageFollow_highDensity5)
Repetition <- c(rep(1,NP*6))

df1GMM <- data.frame(ParticipantNum, density, NumAgents, FollowPercentage)

df2GMM <- df1GMM[df1GMM$FollowPercentage != 0, ]
df3GMM <- df2GMM[!is.na(df2GMM$FollowPercentage),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(FollowPercentage ~ (NumAgents*density) + (1|ParticipantNum), data = df3GMM)
summary(mixed_model)
write.csv(df3GMM, "C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowDensityAfterAgnets.csv", row.names = FALSE)
# ***************************************************************************************************************************************************************************************
# Response time =========================================================================================================================================================================
# ***************************************************************************************************************************************************************************************
RT_lowDensity1 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity1=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_lowDensity3 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity3=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==3)

RT_lowDensity5 <- matrix(, nrow = NP, ncol = NTest)  
length_lowDensity5=rowSums(RadiusTest==6 & catchTrialTest==0 & NumberOfAgentsTest==5)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RT_highDensity1 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity1=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_highDensity3 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity3=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==3)

RT_highDensity5 <- matrix(, nrow = NP, ncol = NTest)  
length_highDensity5=rowSums(RadiusTest==3.3 & catchTrialTest==0 & NumberOfAgentsTest==5)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for (a1 in 1:NP){
  RT_lowDensity1[a1,1:length_lowDensity1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_lowDensity3[a1,1:length_lowDensity3[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==3)]
  RT_lowDensity5[a1,1:length_lowDensity5[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==6 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==5)]
  
  RT_highDensity1[a1,1:length_highDensity1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_highDensity3[a1,1:length_highDensity3[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==3)]
  RT_highDensity5[a1,1:length_highDensity5[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==5)]
}
# Barplot for response time (low density or high density) ===============================================================================================================================
meanRT_lowDensity <- c(mean(RT_lowDensity1, na.rm=TRUE),mean(RT_lowDensity3, na.rm=TRUE),mean(RT_lowDensity5, na.rm=TRUE))
meanRT_highDensity <- c(mean(RT_highDensity1, na.rm=TRUE),mean(RT_highDensity3, na.rm=TRUE),mean(RT_highDensity5, na.rm=TRUE))

stdRT_lowDensity <- c(sd(RT_lowDensity1, na.rm=TRUE),sd(RT_lowDensity3, na.rm=TRUE),sd(RT_lowDensity5, na.rm=TRUE))
stdRT_highDensity <- c(sd(RT_highDensity1, na.rm=TRUE),sd(RT_highDensity3, na.rm=TRUE),sd(RT_highDensity5, na.rm=TRUE))
confidenceRT_lowDensity <- 1.96*stdRT_lowDensity/sqrt(NP)
confidenceRT_highDensity <- 1.96*stdRT_highDensity/sqrt(NP)

NAgents <- c(seq(1,3))
df4 <- data.frame(meanRT_lowDensity, meanRT_highDensity, NAgents)
df5 <- melt(df4, id.vars='NAgents')
confidences <- c(confidenceRT_lowDensity,confidenceRT_highDensity)
df5 <- cbind(df5, confidences)
colnames(df5) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTDensityOnlyAfterP.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,3), labels = c(1, 3, 5)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for response time (low density or high density) ==========================================================================================================================
NAgents <- c(seq(1,3), seq(1,3))
DensityFactor <- c(rep("LowDensity",3),rep("HighDensity",3))
RT <- c(meanRT_lowDensity, meanRT_highDensity)
df6 <- data.frame(DensityFactor, NAgents, RT)
fit1=lm(RT~NAgents*DensityFactor,data=df6)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTRegressDensityOnlyAfterP.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),6))
density <- c(rep("LowDensity",NP*3),rep("HighDensity",NP*3))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP),rep(3,NP)),2))
RT <- c(rowMeans(RT_lowDensity1,na=TRUE),rowMeans(RT_lowDensity3,na=TRUE),rowMeans(RT_lowDensity5,na=TRUE),rowMeans(RT_highDensity1,na=TRUE),rowMeans(RT_highDensity3,na=TRUE),rowMeans(RT_highDensity5,na=TRUE))

df4GMM <- data.frame(ParticipantNum, density, NumAgents, RT)

df5GMM <- df4GMM[df4GMM$RT != 0, ]
df6GMM <- df5GMM[!is.na(df5GMM$RT),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(RT ~ (NumAgents*density) + (1|ParticipantNum), data = df6GMM)
summary(mixed_model)
write.csv(df6GMM, "C:\\Users\\Asieh\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTDensityAfterAgents.csv", row.names = FALSE)