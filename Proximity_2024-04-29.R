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
data <- read_excel("E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\Responses_Pilot1_Proximity_10Participants_2024-04_25.xlsx")
DensityData <- data[data[,3]=="DensityProximity",]
# Group Density ========================================================================================================================================================================
NP=10                                                                                                                                                        # number of participants
NTrain=20                                                                                                                                                    # number of practice trials
NTest=300                                                                                                                                                        # number of test trials                                                                                                                                                       # number of catch trials
NTotal=NTrain+NTest                                                                                                                          # number of main trials in the test session
# ======================================================================================================================================================================================
# Organizing data ======================================================================================================================================================================
InstructionIndex <- matrix(, nrow = NP, ncol = 1)
SessionInd <- matrix(, nrow = NP, ncol = NTotal) 
catchTrial <- matrix(, nrow = NP, ncol = NTotal)
Radius <- matrix(, nrow = NP, ncol = NTotal) 
TrialCondition <- matrix(, nrow = NP, ncol = NTotal)          # 1: 4 agents dense, 2: 4 agents sparse, 3: 7 agents dense, 4: 7 agents sparse
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
    TrialCondition[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant' raised hand (right=1; left=0)
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
TrialConditionTrain <- TrialCondition[,1:NTrain]                                                                                            # participant's raised hand (right=1; left=0)
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
TrialConditionTest <- TrialCondition[,(NTrain+1):NTotal]                                                                                            # participant's raised hand (right=1; left=0)
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
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistDensityProximity.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()
# finding outliers based on Hampel filter ==============================================================================================================================================
correctTestsPercentageLB=median(PercentageCorrectTest) - 3 * mad(PercentageCorrectTest, constant = 1)
correctTestsPercentageUB=median(PercentageCorrectTest) + 3 * mad(PercentageCorrectTest, constant = 1)
# correctTestsPercentageOutliers=t(t(as.numeric(PercentageCorrectTest <= correctTestsPercentageLB)))
correctTestsPercentageOutliers=t(t(as.numeric(PercentageCorrectTest <= 75)))
MyOutliers=row(correctTestsPercentageOutliers)[which(!correctTestsPercentageOutliers == 0)]                                                                        # indices of outliers
# removing participants MyOutliers because of their low performance in catch trials ====================================================================================================
SessionIndTest <- SessionIndTest[-MyOutliers,]
catchTrialTest <- catchTrialTest[-MyOutliers,]
RadiusTest <- RadiusTest[-MyOutliers,]
TrialConditionTest <- TrialConditionTest[-MyOutliers,]                                                                                     # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHandTest[-MyOutliers,]                                                                                                                 # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHandTest[-MyOutliers,]                                                                                                     # left hand:2; right hand:1
responseTimeTest <- responseTimeTest[-MyOutliers,]                                                                                       # this is the RT+fixation duration which was 1s
DominantColorTest <- DominantColorTest[-MyOutliers,]
NP=NP-length(MyOutliers)                                                                                                                                       # 36 participants removed

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
TrialConditionTest = TrialConditionTest*responseTimeTest_AP                                                                                # participant's raised hand (right=1; left=0)
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

png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistOutlierRemovedDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()

# ***************************************************************************************************************************************************************************************
# Following the group in main trials (close or far) ======================================================================================================================
# ***************************************************************************************************************************************************************************************
# Follow_Close1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==9 & catchTrialTest==0 & TrialConditionTest==1)
# Follow_Far1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & RadiusTest==3.3 & catchTrialTest==0 & TrialConditionTest==1)
# Total_Close1=(RadiusTest==9 & catchTrialTest==0 & TrialConditionTest==1)
# Total_Far1=(RadiusTest==3.3 & catchTrialTest==0 & TrialConditionTest==1)

Follow_Close4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & catchTrialTest==0 & TrialConditionTest==1)
Follow_Far4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & catchTrialTest==0 & TrialConditionTest==2)
Total_Close4=(catchTrialTest==0 & TrialConditionTest==1)
Total_Far4=(catchTrialTest==0 & TrialConditionTest==2)

Follow_Close7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & catchTrialTest==0 & TrialConditionTest==3)
Follow_Far7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & catchTrialTest==0 & TrialConditionTest==4)
Total_Close7=(catchTrialTest==0 & TrialConditionTest==3)
Total_Far7=(catchTrialTest==0 & TrialConditionTest==4)

# PercentageFollow_Close1 <- matrix(, nrow = NP, ncol = 1)     
# PercentageFollow_Far1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_Close4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_Far4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_Close7 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_Far7 <- matrix(, nrow = NP, ncol = 1)     

for (a1 in 1:NP){
  # followP_low1=as.numeric(Follow_Close1[a1,])
  # followP_high1=as.numeric(Follow_Far1[a1,])
  # totalP_low1=as.numeric(Total_Close1[a1,])
  # totalP_high1=as.numeric(Total_Far1[a1,])
  # PercentageFollow_Close1[a1,]=sum(followP_low1)/sum(totalP_low1)*100
  # PercentageFollow_Far1[a1,]=sum(followP_high1)/sum(totalP_high1)*100
  
  followP_low4=as.numeric(Follow_Close4[a1,])
  followP_high4=as.numeric(Follow_Far4[a1,])
  totalP_low4=as.numeric(Total_Close4[a1,])
  totalP_high4=as.numeric(Total_Far4[a1,])
  PercentageFollow_Close4[a1,]=sum(followP_low4)/sum(totalP_low4)*100
  PercentageFollow_Far4[a1,]=sum(followP_high4)/sum(totalP_high4)*100
  
  followP_low7=as.numeric(Follow_Close7[a1,])
  followP_high7=as.numeric(Follow_Far7[a1,])
  totalP_low7=as.numeric(Total_Close7[a1,])
  totalP_high7=as.numeric(Total_Far7[a1,])
  PercentageFollow_Close7[a1,]=sum(followP_low7)/sum(totalP_low7)*100
  PercentageFollow_Far7[a1,]=sum(followP_high7)/sum(totalP_high7)*100
}
# =======================================================================================================================================================================================
# Barplot for Following the group (close or far) =========================================================================================================================
meanFollow_Close <- c(mean(PercentageFollow_Close4, na.rm=TRUE),mean(PercentageFollow_Close7, na.rm=TRUE))
meanFollow_Far <- c(mean(PercentageFollow_Far4, na.rm=TRUE),mean(PercentageFollow_Far7, na.rm=TRUE))

stdFollow_Close <- c(sd(PercentageFollow_Close4, na.rm=TRUE),sd(PercentageFollow_Close7, na.rm=TRUE))
stdFollow_Far <- c(sd(PercentageFollow_Far4, na.rm=TRUE),sd(PercentageFollow_Far7, na.rm=TRUE))
confidenceFollow_Close <- 1.96*stdFollow_Close/sqrt(NP)
confidenceFollow_Far <- 1.96*stdFollow_Far/sqrt(NP)

NAgents <- c(seq(1,2))
df1 <- data.frame(meanFollow_Close, meanFollow_Far, NAgents)
df2 <- melt(df1, id.vars='NAgents')
confidences <- c(confidenceFollow_Close,confidenceFollow_Far)
df2 <- cbind(df2, confidences)
colnames(df2) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\FolCloseOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df2, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('close', 'far')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()
# =======================================================================================================================================================================================
# Regress plot for Following the group (close or far) ====================================================================================================================
NAgents <- c(seq(1,2), seq(1,2))
DensityFactor <- c(rep("Close",2),rep("Far",2))
FollowPercentage <- c(meanFollow_Close, meanFollow_Far)
df3 <- data.frame(DensityFactor, NAgents, FollowPercentage)
fit1=lm(FollowPercentage~DensityFactor*NAgents,data=df3)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\FollowRegressDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df3,aes(y=FollowPercentage,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),4))
density <- c(rep("Close",NP*2),rep("Far",NP*2))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP)),2))
FollowPercentage <- c(PercentageFollow_Close4,PercentageFollow_Close7,PercentageFollow_Far4,PercentageFollow_Far7)
Repetition <- c(rep(1,NP*4))

df1GMM <- data.frame(ParticipantNum, density, NumAgents, FollowPercentage)

df2GMM <- df1GMM[df1GMM$FollowPercentage != 0, ]
df3GMM <- df2GMM[!is.na(df2GMM$FollowPercentage),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(FollowPercentage ~ (NumAgents*density) + (1|ParticipantNum), data = df3GMM)
summary(mixed_model)
write.csv(df3GMM, "E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\FolCloseAfterAgnets.csv", row.names = FALSE)
# ***************************************************************************************************************************************************************************************
# Response time (follow) =========================================================================================================================================================================
# ***************************************************************************************************************************************************************************************
# RT_CloseF1 <- matrix(, nrow = NP, ncol = NTest)  
# length_CloseF1=rowSums(RadiusTest==9 & catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))

RT_CloseF4 <- matrix(, nrow = NP, ncol = NTest)  
length_CloseF4=rowSums(catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))

RT_CloseF7 <- matrix(, nrow = NP, ncol = NTest)  
length_CloseF7=rowSums(catchTrialTest==0 & TrialConditionTest==3 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# RT_FarF1 <- matrix(, nrow = NP, ncol = NTest)  
# length_FarF1=rowSums(RadiusTest==3.3 & catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))

RT_FarF4 <- matrix(, nrow = NP, ncol = NTest)  
length_FarF4=rowSums(catchTrialTest==0 & TrialConditionTest==2 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))

RT_FarF7 <- matrix(, nrow = NP, ncol = NTest)  
length_FarF7=rowSums(catchTrialTest==0 & TrialConditionTest==4 & ((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# for (a in 1:NP){
#   RT_Follow[a,length_RTF[a]]=responseTimeTest[a,(ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)]
#   RT_Follow[a,length_RTUF[a]]=responseTimeTest[a,(ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)]
# }
for (a1 in 1:NP){
  # RT_CloseF1[a1,1:length_CloseF1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==9 & catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
  RT_CloseF4[a1,1:length_CloseF4[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
  RT_CloseF7[a1,1:length_CloseF7[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==3 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
  
  # RT_FarF1[a1,1:length_FarF1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
  RT_FarF4[a1,1:length_FarF4[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==2 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
  RT_FarF7[a1,1:length_FarF7[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==4 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==2)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==1)))]
}
# Barplot for response time (close or far) ===============================================================================================================================
meanRTF_Close <- c(mean(RT_CloseF4, na.rm=TRUE),mean(RT_CloseF7, na.rm=TRUE))
meanRTF_Far <- c(mean(RT_FarF4, na.rm=TRUE),mean(RT_FarF7, na.rm=TRUE))

stdRTF_Close <- c(sd(RT_CloseF4, na.rm=TRUE),sd(RT_CloseF7, na.rm=TRUE))
stdRTF_Far <- c(sd(RT_FarF4, na.rm=TRUE),sd(RT_FarF7, na.rm=TRUE))
confidenceRTF_Close <- 1.96*stdRTF_Close/sqrt(NP)
confidenceRTF_Far <- 1.96*stdRTF_Far/sqrt(NP)

NAgents <- c(seq(1,2))
df4 <- data.frame(meanRTF_Close, meanRTF_Far, NAgents)
df5 <- melt(df4, id.vars='NAgents')
confidences <- c(confidenceRTF_Close,confidenceRTF_Far)
df5 <- cbind(df5, confidences)
colnames(df5) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTFDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('close', 'far')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for response time (close or far) ==========================================================================================================================
NAgents <- c(seq(1,2), seq(1,2))
DensityFactor <- c(rep("Close",2),rep("Far",2))
RT <- c(meanRTF_Close, meanRTF_Far)
df6 <- data.frame(DensityFactor, NAgents, RT)
fit1=lm(RT~NAgents*DensityFactor,data=df6)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTFRegressDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()
# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),4))
density <- c(rep("Close",NP*2),rep("Far",NP*2))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP)),2))
RT <- c(rowMeans(RT_CloseF4,na=TRUE),rowMeans(RT_CloseF7,na=TRUE),rowMeans(RT_FarF4,na=TRUE),rowMeans(RT_FarF7,na=TRUE))

df4GMM <- data.frame(ParticipantNum, density, NumAgents, RT)

df5GMM <- df4GMM[df4GMM$RT != 0, ]
df6GMM <- df5GMM[!is.na(df5GMM$RT),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(RT ~ (NumAgents*density) + (1|ParticipantNum), data = df6GMM)
summary(mixed_model)
write.csv(df6GMM, "E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTFDensityAfterAgents.csv", row.names = FALSE)
# ***************************************************************************************************************************************************************************************
# Response time (unfollow) =========================================================================================================================================================================
# ***************************************************************************************************************************************************************************************
# RT_CloseUF1 <- matrix(, nrow = NP, ncol = NTest)  
# length_CloseUF1=rowSums(RadiusTest==9 & catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))

RT_CloseUF4 <- matrix(, nrow = NP, ncol = NTest)  
length_CloseUF4=rowSums(catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))

RT_CloseUF7 <- matrix(, nrow = NP, ncol = NTest)  
length_CloseUF7=rowSums(catchTrialTest==0 & TrialConditionTest==3 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# RT_FarUF1 <- matrix(, nrow = NP, ncol = NTest)  
# length_FarUF1=rowSums(RadiusTest==3.3 & catchTrialTest==0 & TrialConditionTest==1 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))

RT_FarUF4 <- matrix(, nrow = NP, ncol = NTest)  
length_FarUF4=rowSums(catchTrialTest==0 & TrialConditionTest==2 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))

RT_FarUF7 <- matrix(, nrow = NP, ncol = NTest)  
length_FarUF7=rowSums(catchTrialTest==0 & TrialConditionTest==4 & ((ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)))
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# for (a in 1:NP){
#   RT_Follow[a,length_RTF[a]]=responseTimeTest[a,(ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)]
#   RT_Follow[a,length_RTUF[a]]=responseTimeTest[a,(ParticipantHandTest==1 & AgentHandTest==1)|(ParticipantHandTest==2 & AgentHandTest==2)]
# }
for (a1 in 1:NP){
  # if(length_CloseUF1[a1]!=0){
  #   RT_CloseUF1[a1,1:length_CloseUF1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==9 & catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  # }
  if(length_CloseUF4[a1]!=0){
    RT_CloseUF4[a1,1:length_CloseUF4[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  }
  if(length_CloseUF7[a1]!=0){
    RT_CloseUF7[a1,1:length_CloseUF7[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==3 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  }
  # if(length_FarUF1[a1]!=0){
  #   RT_FarUF1[a1,1:length_FarUF1[a1]]=responseTimeTest[a1,(RadiusTest[a1,]==3.3 & catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==1 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  # }
  if(length_FarUF4[a1]!=0){
    RT_FarUF4[a1,1:length_FarUF4[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==2 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  }
  if(length_FarUF7[a1]!=0){
    RT_FarUF7[a1,1:length_FarUF7[a1]]=responseTimeTest[a1,(catchTrialTest[a1,]==0 & TrialConditionTest[a1,]==4 & ((ParticipantHandTest[a1,]==1 & AgentHandTest[a1,]==1)|(ParticipantHandTest[a1,]==2 & AgentHandTest[a1,]==2)))]
  }
}
# Barplot for response time (close or far) ===============================================================================================================================
meanRTUF_Close <- c(mean(RT_CloseUF4, na.rm=TRUE),mean(RT_CloseUF7, na.rm=TRUE))
meanRTUF_Far <- c(mean(RT_FarUF4, na.rm=TRUE),mean(RT_FarUF7, na.rm=TRUE))

stdRTUF_Close <- c(sd(RT_CloseUF4, na.rm=TRUE),sd(RT_CloseUF7, na.rm=TRUE))
stdRTUF_Far <- c(sd(RT_FarUF4, na.rm=TRUE),sd(RT_FarUF7, na.rm=TRUE))
confidenceRTUF_Close <- 1.96*stdRTUF_Close/sqrt(NP)
confidenceRTUF_Far <- 1.96*stdRTUF_Far/sqrt(NP)

NAgents <- c(seq(1,2))
df4 <- data.frame(meanRTUF_Close, meanRTUF_Far, NAgents)
df5 <- melt(df4, id.vars='NAgents')
confidences <- c(confidenceRTUF_Close,confidenceRTUF_Far)
df5 <- cbind(df5, confidences)
colnames(df5) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTUFDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('close', 'far')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for response time (close or far) ==========================================================================================================================
NAgents <- c(seq(1,2), seq(1,2))
DensityFactor <- c(rep("Close",2),rep("Far",2))
RT <- c(meanRTUF_Close, meanRTUF_Far)
df6 <- data.frame(DensityFactor, NAgents, RT)
fit1=lm(RT~NAgents*DensityFactor,data=df6)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTUFRegressDensityOnlyAfterPProximity.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,2), labels = c(4, 7)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),4))
density <- c(rep("Close",NP*2),rep("Far",NP*2))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP)),2))
RT <- c(rowMeans(RT_CloseUF4,na=TRUE),rowMeans(RT_CloseUF7,na=TRUE),rowMeans(RT_FarUF4,na=TRUE),rowMeans(RT_FarUF7,na=TRUE))

df4GMM <- data.frame(ParticipantNum, density, NumAgents, RT)

df5GMM <- df4GMM[df4GMM$RT != 0, ]
df6GMM <- df5GMM[!is.na(df5GMM$RT),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(RT ~ (NumAgents*density) + (1|ParticipantNum), data = df6GMM)
summary(mixed_model)
write.csv(df6GMM, "E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTUFDensityAfterAgents.csv", row.names = FALSE)