cat("\014")                                                                                                                                                            # as clc in MATLAB
rm(list = ls(all.names = TRUE))                                                                                                                                      # as clear in MATLAB
# install required packages =============================================================================================================================================================
# install.packages("readxl")                                                                                                                                         # to read xlsx files
# install.packages("ggplot2")
# install.packages("ggthemes")
# call libraries ========================================================================================================================================================================
library(readxl)
library(ggplot2)
library(ggthemes)
# read data from the excel file =========================================================================================================================================================
data <- read_excel("E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\Responses_Pilot1_10Participants_2024-02-15.xlsx")
GroupSizeData <- data[data[,3]=="GroupSize",]
# Group Density =========================================================================================================================================================================
NP=8                                                                                                                                                            # number of participants
NTrain=20                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials
NCatch=64                                                                                                                                                        # number of catch trials
NTotal=NTrain+NTest
NMain=256                                                                                                                                     # number of main trials in the test session
# =======================================================================================================================================================================================
InstructionIndex <- matrix(, nrow = NP, ncol = 1)
SessionInd <- matrix(, nrow = NP, ncol = NTotal) 
catchTrial <- matrix(, nrow = NP, ncol = NTotal)
DominantColor <- matrix(, nrow = NP, ncol = NTotal) 
NumberOfAgents <- matrix(, nrow = NP, ncol = NTotal)
AgentHand <- matrix(, nrow = NP, ncol = NTotal)
ParticipantHand <- matrix(, nrow = NP, ncol = NTotal)
responseTime <- matrix(, nrow = NP, ncol = NTotal)  
nAgents1 <- matrix(, nrow = NP, ncol = NTotal) 
nAgents2 <- matrix(, nrow = NP, ncol = NTotal)

for (a1 in 1:NP) {
  InstructionIndex[a1,1] <- as.numeric(toString(data[a1,4]))                                                # 1=> right hand:yellow ;left hand:blue; 2=> right hand:blue ;left hand:yellow;
  for (a2 in 1:NTotal) {
    currentCell <- toString(GroupSizeData[a1,a2+4])
    separatedVals <- strsplit(currentCell,",")
    dataCell <- matrix(unlist(separatedVals),ncol=9,byrow=T)                                                              # ncol is the number of data elements in each cell of the matrix
    SessionInd[a1,a2] <- as.numeric(dataCell[1])                                                                      
    catchTrial[a1,a2] <- as.numeric(dataCell[2])
    DominantColor[a1,a2] <- as.numeric(dataCell[3])
    NumberOfAgents[a1,a2] <- as.numeric(dataCell[4])                                                                                         # participant's raised hand (right=1; left=0)
    AgentHand[a1,a2] <- as.numeric(dataCell[5])                                                                                                                # left hand:1; right hand:2
    ParticipantHand[a1,a2] <- as.numeric(dataCell[6])                                                                                                          # left hand:2; right hand:1
    responseTime[a1,a2] <- as.numeric(dataCell[7])                                                                                         # this is the RT+fixation duration which was 1s
    nAgents1[a1,a2] <- as.numeric(dataCell[8])                                                                                                                     # blue:1; yellow:2
    nAgents2[a1,a2] <- as.numeric(dataCell[9])
  }
}
Size <- matrix(, nrow = NP, ncol = NTotal)
for (a in 1:NP){
  Size[a,1:NTrain]=12
  Size[a,21:100]=nAgents1[a,21]
  Size[a,101:180]=nAgents2[a,101]
  Size[a,181:260]=nAgents1[a,181]
  Size[a,261:340]=nAgents2[a,261]
}

# Correct percentage in practice session =======================
SessionIndTrain <- SessionInd[,1:NTrain]                                                                      
catchTrialTrain <- catchTrial[,1:NTrain]  
SizeTrain <- Size[,1:NTrain]  
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
# Correct percentage in test session =======================
SessionIndTest <- SessionInd[,(NTrain+1):340]                                                                      
catchTrialTest <- catchTrial[,(NTrain+1):NTotal]  
SizeTest <- Size[,(NTrain+1):NTotal]  
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
# removing participants 4 and 8 because of low performance ===================================================
SessionIndTest <- SessionIndTest[-c(4,8),]                                                                      
catchTrialTest <- catchTrialTest[-c(4,8),]     
SizeTest <- SizeTest[-c(4,8),]       
NumberOfAgentsTest <- NumberOfAgentsTest[-c(4,8),]                                                                                              # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHandTest[-c(4,8),]                                                                                                            # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHandTest[-c(4,8),]                                                                                                             # left hand:2; right hand:1
responseTimeTest <- responseTimeTest[-c(4,8),]                                                                                           # this is the RT+fixation duration which was 1s
DominantColorTest <- DominantColorTest[-c(4,8),] 
NP=NP-2                  # 2 participants removed
# Following the group in main trials (low density or high density) ==========================================================================
Follow_smallGroup1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==1)
Follow_largeGroup1=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_smallGroup1=(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==1)
Total_largeGroup1=(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==1)

Follow_smallGroup4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==4)
Follow_largeGroup4=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==4)
Total_smallGroup4=(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==4)
Total_largeGroup4=(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==4)

Follow_smallGroup7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==7)
Follow_largeGroup7=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==7)
Total_smallGroup7=(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==7)
Total_largeGroup7=(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==7)

Follow_smallGroup10=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==10)
Follow_largeGroup10=(((ParticipantHandTest==1 & AgentHandTest==2)|(ParticipantHandTest==2 & AgentHandTest==1)) & SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==10)
Total_smallGroup10=(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==10)
Total_largeGroup10=(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==10)

PercentageFollow_smallGroup1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_largeGroup1 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_smallGroup4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_largeGroup4 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_smallGroup7 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_largeGroup7 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_smallGroup10 <- matrix(, nrow = NP, ncol = 1)     
PercentageFollow_largeGroup10 <- matrix(, nrow = NP, ncol = 1)      
for (a1 in 1:NP){
  followP_small1=as.numeric(Follow_smallGroup1[a1,])
  followP_large1=as.numeric(Follow_largeGroup1[a1,])
  totalP_small1=as.numeric(Total_smallGroup1[a1,])
  totalP_large1=as.numeric(Total_largeGroup1[a1,])
  PercentageFollow_smallGroup1[a1,]=sum(followP_small1)/sum(totalP_small1)*100
  PercentageFollow_largeGroup1[a1,]=sum(followP_large1)/sum(totalP_large1)*100
  
  followP_small4=as.numeric(Follow_smallGroup4[a1,])
  followP_large4=as.numeric(Follow_largeGroup4[a1,])
  totalP_small4=as.numeric(Total_smallGroup4[a1,])
  totalP_large4=as.numeric(Total_largeGroup4[a1,])
  PercentageFollow_smallGroup4[a1,]=sum(followP_small4)/sum(totalP_small4)*100
  PercentageFollow_largeGroup4[a1,]=sum(followP_large4)/sum(totalP_large4)*100
  
  followP_small7=as.numeric(Follow_smallGroup7[a1,])
  followP_large7=as.numeric(Follow_largeGroup7[a1,])
  totalP_small7=as.numeric(Total_smallGroup7[a1,])
  totalP_large7=as.numeric(Total_largeGroup7[a1,])
  PercentageFollow_smallGroup7[a1,]=sum(followP_small7)/sum(totalP_small7)*100
  PercentageFollow_largeGroup7[a1,]=sum(followP_large7)/sum(totalP_large7)*100
  
  followP_small10=as.numeric(Follow_smallGroup10[a1,])
  followP_large10=as.numeric(Follow_largeGroup10[a1,])
  totalP_small10=as.numeric(Total_smallGroup10[a1,])
  totalP_large10=as.numeric(Total_largeGroup10[a1,])
  PercentageFollow_smallGroup10[a1,]=sum(followP_small10)/sum(totalP_small10)*100
  PercentageFollow_largeGroup10[a1,]=sum(followP_large10)/sum(totalP_large10)*100
}
# Barplot for Following the group (low density or high density) ==========================================================================
meanFollow_smallGroup <- c(mean(PercentageFollow_smallGroup1, na.rm=TRUE),mean(PercentageFollow_smallGroup4, na.rm=TRUE),mean(PercentageFollow_smallGroup7, na.rm=TRUE),mean(PercentageFollow_smallGroup10, na.rm=TRUE))
meanFollow_largeGroup <- c(mean(PercentageFollow_largeGroup1, na.rm=TRUE),mean(PercentageFollow_largeGroup4, na.rm=TRUE),mean(PercentageFollow_largeGroup7, na.rm=TRUE),mean(PercentageFollow_largeGroup10, na.rm=TRUE))

stdFollow_smallGroup <- c(sd(PercentageFollow_smallGroup1, na.rm=TRUE),sd(PercentageFollow_smallGroup4, na.rm=TRUE),sd(PercentageFollow_smallGroup7, na.rm=TRUE),sd(PercentageFollow_smallGroup10, na.rm=TRUE))
stdFollow_largeGroup <- c(sd(PercentageFollow_largeGroup1, na.rm=TRUE),sd(PercentageFollow_largeGroup4, na.rm=TRUE),sd(PercentageFollow_largeGroup7, na.rm=TRUE),sd(PercentageFollow_largeGroup10, na.rm=TRUE))
confidenceFollow_smallGroup <- 1.96*stdFollow_smallGroup/sqrt(NP)
confidenceFollow_largeGroup <- 1.96*stdFollow_largeGroup/sqrt(NP)

NAgents <- c(seq(1,4))
df1 <- data.frame(meanFollow_smallGroup, meanFollow_largeGroup, NAgents)
df2 <- melt(df1, id.vars='NAgents')
confidences <- c(confidenceFollow_smallGroup,confidenceFollow_largeGroup)
df2 <- cbind(df2, confidences)
colnames(df2) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\FollowSize.png", width=1200, height=700)
ggplot(df2, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,4)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for Following the group (low density or high density) ==========================================================================
NAgents <- c(seq(1,4), seq(1,4))
DensityFactor <- c(rep("smallGroup",4),rep("largeGroup",4))
FollowPercentage <- c(meanFollow_smallGroup, meanFollow_largeGroup)
df3 <- data.frame(DensityFactor, NAgents, FollowPercentage)
fit1=lm(FollowPercentage~DensityFactor*NAgents,data=df3)
summary(fit1)
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\FollowRegressSize.png", width=1200, height=700)
ggplot(df3,aes(y=FollowPercentage,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColors) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ") 
dev.off()

# Response time ================================
RT_smallGroup1 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup1=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_smallGroup4 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup4=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_smallGroup7 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup7=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_smallGroup10 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup10=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==10)
# --------------------------------------------------------------------------------------
RT_largeGroup1 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup1=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_largeGroup4 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup4=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_largeGroup7 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup7=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_largeGroup10 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup10=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==10)
# --------------------------------------------------------------------------------------
for (a1 in 1:NP){
  RT_smallGroup1[a1,1:length_smallGroup1[a1]]=responseTimeTest[a1,(SizeTest[a1,]==10 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_smallGroup4[a1,1:length_smallGroup4[a1]]=responseTimeTest[a1,(SizeTest[a1,]==10 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==4)]
  RT_smallGroup7[a1,1:length_smallGroup7[a1]]=responseTimeTest[a1,(SizeTest[a1,]==10 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==7)]
  RT_smallGroup10[a1,1:length_smallGroup10[a1]]=responseTimeTest[a1,(SizeTest[a1,]==10 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==10)]
  
  RT_largeGroup1[a1,1:length_largeGroup1[a1]]=responseTimeTest[a1,(SizeTest[a1,]==14 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==1)]
  RT_largeGroup4[a1,1:length_largeGroup4[a1]]=responseTimeTest[a1,(SizeTest[a1,]==14 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==4)]
  RT_largeGroup7[a1,1:length_largeGroup7[a1]]=responseTimeTest[a1,(SizeTest[a1,]==14 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==7)]
  RT_largeGroup10[a1,1:length_largeGroup10[a1]]=responseTimeTest[a1,(SizeTest[a1,]==14 & catchTrialTest[a1,]==0 & NumberOfAgentsTest[a1,]==10)]
}

# Barplot for Following the group (low density or high density) ==========================================================================
meanRT_smallGroup <- c(mean(RT_smallGroup1, na.rm=TRUE),mean(RT_smallGroup4, na.rm=TRUE),mean(RT_smallGroup7, na.rm=TRUE),mean(RT_smallGroup10, na.rm=TRUE))
meanRT_largeGroup <- c(mean(RT_largeGroup1, na.rm=TRUE),mean(RT_largeGroup4, na.rm=TRUE),mean(RT_largeGroup7, na.rm=TRUE),mean(RT_largeGroup10, na.rm=TRUE))

stdRT_smallGroup <- c(sd(RT_smallGroup1, na.rm=TRUE),sd(RT_smallGroup4, na.rm=TRUE),sd(RT_smallGroup7, na.rm=TRUE),sd(RT_smallGroup10, na.rm=TRUE))
stdRT_largeGroup <- c(sd(RT_largeGroup1, na.rm=TRUE),sd(RT_largeGroup4, na.rm=TRUE),sd(RT_largeGroup7, na.rm=TRUE),sd(RT_largeGroup10, na.rm=TRUE))
confidenceRT_smallGroup <- 1.96*stdRT_smallGroup/sqrt(NP)
confidenceRT_largeGroup <- 1.96*stdRT_largeGroup/sqrt(NP)

NAgents <- c(seq(1,4))
df4 <- data.frame(meanRT_smallGroup, meanRT_largeGroup, NAgents)
df5 <- melt(df4, id.vars='NAgents')
confidences <- c(confidenceRT_smallGroup,confidenceRT_largeGroup)
df5 <- cbind(df5, confidences)
colnames(df5) <- c("NAgents","Density","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTSize.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=Density)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('low Density', 'high Density')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,4)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for Following the group (low density or high density) ==========================================================================
NAgents <- c(seq(1,4), seq(1,4))
DensityFactor <- c(rep("smallGroup",4),rep("largeGroup",4))
RT <- c(meanRT_smallGroup, meanRT_largeGroup)
df6 <- data.frame(DensityFactor, NAgents, RT)
fit1=lm(RT~DensityFactor*NAgents,data=df6)
summary(fit1)
png(file="E:\\OngoingAnalysis\\SciOI_R\\SciOI_Density+GroupSize_R\\RTRegressSize.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(DensityFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColors) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()