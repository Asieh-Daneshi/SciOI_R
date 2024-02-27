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
data <- read_excel("C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\Responses_Main1_150Participants_2024-02-23.xlsx")
GroupSizeData <- data[data[,3]=="GroupSize",]
# Group Size ============================================================================================================================================================================
NP=157                                                                                                                                                           # number of participants
NTrain=20                                                                                                                                                     # number of practice trials
NTest=320                                                                                                                                                         # number of test trials                                                                                                                                                       # number of catch trials
NTotal=NTrain+NTest                                                                                                                           # number of main trials in the test session
# =======================================================================================================================================================================================
# Organizing data =======================================================================================================================================================================
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
    nAgents1[a1,a2] <- as.numeric(dataCell[8])                                                                                                                          # blue:1; yellow:2
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
# =======================================================================================================================================================================================
# Correct percentage in practice session ================================================================================================================================================
SessionIndTrain <- SessionInd[,1:NTrain]                                                                      
catchTrialTrain <- catchTrial[,1:NTrain]  
SizeTrain <- Size[,1:NTrain]  
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
# Hitsogram of correct percentage in practice session ===================================================================================================================================
hist(PercentageCorrectTrain, main = "Histogram of Sample Data", xlab = "Value", ylab = "Frequency")
# finding outliers based on Hampel filter -----------------------------------------------------------------------------------------------------------------------------------------------
correctPracticesPercentageLB=median(PercentageCorrectTrain) - 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageUB=median(PercentageCorrectTrain) + 3 * mad(PercentageCorrectTrain, constant = 1)
correctPracticesPercentageOutliers=t(t(as.numeric(PercentageCorrectTrain <= correctPracticesPercentageLB)))
row(correctPracticesPercentageOutliers)[which(!correctPracticesPercentageOutliers == 0)]                                                                            # indices of outliers

# =======================================================================================================================================================================================
# Correct percentage in test session ====================================================================================================================================================
SessionIndTest <- SessionInd[,(NTrain+1):340]                                                                      
catchTrialTest <- catchTrial[,(NTrain+1):NTotal]  
SizeTest <- Size[,(NTrain+1):NTotal]  
NumberOfAgentsTest <- NumberOfAgents[,(NTrain+1):NTotal]                                                                                    # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHand[,(NTrain+1):NTotal]                                                                                                                # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHand[,(NTrain+1):NTotal]                                                                                                    # left hand:2; right hand:1
responseTimeTest <- responseTime[,(NTrain+1):NTotal]                                                                                      # this is the RT+fixation duration which was 1s
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
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistGroupSize.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()
# finding outliers based on Hampel filter ==============================================================================================================================================
correctTestsPercentageLB=median(PercentageCorrectTest) - 3 * mad(PercentageCorrectTest, constant = 1)
correctTestsPercentageUB=median(PercentageCorrectTest) + 3 * mad(PercentageCorrectTest, constant = 1)
correctTestsPercentageOutliers=t(t(as.numeric(PercentageCorrectTest <= correctTestsPercentageLB)))
MyOutliers=row(correctTestsPercentageOutliers)[which(!correctTestsPercentageOutliers == 0)]                                                                        # indices of outliers
# removing participants MyOutliers because of their low performance in catch trials ====================================================================================================
SessionIndTest <- SessionIndTest[-MyOutliers,]
catchTrialTest <- catchTrialTest[-MyOutliers,]
SizeTest <- SizeTest[-MyOutliers,]
NumberOfAgentsTest <- NumberOfAgentsTest[-MyOutliers,]                                                                                     # participant's raised hand (right=1; left=0)
AgentHandTest <- AgentHandTest[-MyOutliers,]                                                                                                                 # left hand:1; right hand:2
ParticipantHandTest <- ParticipantHandTest[-MyOutliers,]                                                                                                     # left hand:2; right hand:1
responseTimeTest <- responseTimeTest[-MyOutliers,]                                                                                       # this is the RT+fixation duration which was 1s
DominantColorTest <- DominantColorTest[-MyOutliers,]
NP=NP-length(MyOutliers)                                                                                                                                       # 36 participants removed

# Here we subtract 1 from all response times, because these response times include 1 second for fixation ===============================================================================
responseTimeTrain=responseTimeTrain-1
responseTimeTest=responseTimeTest-1

# Hitstogram of response times in Main trials ==========================================================================================================================================
MeanRT=rowMeans(responseTimeTest,na=TRUE)
framedData <- data.frame(1:NP,MeanRT)
colnames(framedData) <- c('ParticipantNumber', 'MeanRT') 
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RT_GroupSize_Hist.png", width=1200, height=700)
ggplot(data = framedData, aes(x = MeanRT)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Response time in main trials") + xlab("Response time (s)") + ylab("number of participants")
dev.off()
hist(responseTimeTest, main = "Histogram of response times in Main trials", xlab = "Value", ylab = "Frequency")
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

png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\CatchCorrectnessHistOutlierRemovedGroupSize.png", width=1200, height=700)
ggplot(data = framedData, aes(x = PercentageCorrectTest)) + geom_histogram(color="black", fill="aquamarine4") + theme_bw(base_size = 14) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + ggtitle("Performance on practice trials") + xlab("correct Percentage") + ylab("number of participants")
dev.off()

# ***************************************************************************************************************************************************************************************
# Following the group in main trials (low density or high density) ======================================================================================================================
# ***************************************************************************************************************************************************************************************
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
# =======================================================================================================================================================================================
# Barplot for Following the group (small size or large size) ============================================================================================================================
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
colnames(df2) <- c("NAgents","GroupSize","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowSize.png", width=1200, height=700)
ggplot(df2, aes(x=NAgents, y=meanValues, fill=GroupSize)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('small group', 'large group')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,4), labels = c(1, 4, 7, 10)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()
# =======================================================================================================================================================================================
# Regress plot for Following the group (small size or large size) =======================================================================================================================
NAgents <- c(seq(1,4), seq(1,4))
SizeFactor <- c(rep("smallGroup",4),rep("largeGroup",4))
FollowPercentage <- c(meanFollow_smallGroup, meanFollow_largeGroup)
df3 <- data.frame(SizeFactor, NAgents, FollowPercentage)
fit1=lm(FollowPercentage~SizeFactor*NAgents,data=df3)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowRegressSize.png", width=1200, height=700)
ggplot(df3,aes(y=FollowPercentage,x=NAgents,color=factor(SizeFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Following the group") + ylab("Percentage of follow") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),8))
groupSize <- c(rep("SmallGroup",NP*4),rep("LargeGroup",NP*4))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP),rep(3,NP),rep(4,NP)),2))
FollowPercentage <- c(PercentageFollow_smallGroup1,PercentageFollow_smallGroup4,PercentageFollow_smallGroup7,PercentageFollow_smallGroup10,PercentageFollow_largeGroup1,PercentageFollow_largeGroup4,PercentageFollow_largeGroup7,PercentageFollow_largeGroup10)

df1GMM <- data.frame(ParticipantNum, groupSize, NumAgents, FollowPercentage)

df2GMM <- df1GMM[df1GMM$FollowPercentage != 0, ]
df3GMM <- df2GMM[!is.na(df2GMM$FollowPercentage),]

# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(FollowPercentage ~ (NumAgents*groupSize) + (1|ParticipantNum), data = df3GMM)
summary(mixed_model)
write.csv(df3GMM, "C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\FollowSize.csv", row.names = FALSE)

# =======================================================================================================================================================================================
# ANOVA =================================================================================================================================================================================
library(afex)
within_anova_Follow <- aov_car(FollowPercentage ~ (NumAgents * groupSize) + Error(ParticipantNum/(NumAgents * groupSize)), data = df3GMM)

# Display the ANOVA summary
summary(within_anova_Follow)

# ***************************************************************************************************************************************************************************************
# Response time =========================================================================================================================================================================
# ***************************************************************************************************************************************************************************************
RT_smallGroup1 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup1=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_smallGroup4 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup4=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_smallGroup7 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup7=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_smallGroup10 <- matrix(, nrow = NP, ncol = NTest)  
length_smallGroup10=rowSums(SizeTest==10 & catchTrialTest==0 & NumberOfAgentsTest==10)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
RT_largeGroup1 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup1=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==1)

RT_largeGroup4 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup4=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==4)

RT_largeGroup7 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup7=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==7)

RT_largeGroup10 <- matrix(, nrow = NP, ncol = NTest)  
length_largeGroup10=rowSums(SizeTest==14 & catchTrialTest==0 & NumberOfAgentsTest==10)
# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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
# Barplot for response time (small group or large group) ================================================================================================================================
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
colnames(df5) <- c("NAgents","GroupSize","meanValues","confidenceValues")
AsiColors<- c("darkseagreen3", "darkolivegreen4")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTSize.png", width=1200, height=700)
ggplot(df5, aes(x=NAgents, y=meanValues, fill=GroupSize)) + geom_bar(stat='identity', position='dodge') + scale_fill_manual(values=AsiColors , labels=c('small group', 'large group')) + theme_bw(base_size = 18) + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(1,4), labels = c(1, 4, 7, 10)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ")+geom_errorbar(aes(ymin=meanValues-confidenceValues, ymax=meanValues+confidenceValues), width=.2, position=position_dodge(.9))
dev.off()

# Regress plot for response time (small size or large size) =============================================================================================================================
NAgents <- c(seq(1,4), seq(1,4))
SizeFactor <- c(rep("smallGroup",4),rep("largeGroup",4))
RT <- c(meanRT_smallGroup, meanRT_largeGroup)
df6 <- data.frame(SizeFactor, NAgents, RT)
fit1=lm(RT~SizeFactor*NAgents,data=df6)
summary(fit1)
AsiColorsRegress<- c("darkolivegreen4","darkseagreen3")
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTRegressSize.png", width=1200, height=700)
ggplot(df6,aes(y=RT,x=NAgents,color=factor(SizeFactor)))+geom_point()+stat_smooth(method="lm",se=TRUE) + scale_color_manual(values=AsiColorsRegress) + theme_bw(base_size = 18) + theme(text = element_text(size=20),panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank() , axis.line = element_line(size = .5, linetype = "solid", colour = "black")) + scale_x_continuous(name="Number of agents" , breaks=seq(0,8)) + ggtitle("Response time") + ylab("Response time") + labs(fill = " ") 
dev.off()

# =======================================================================================================================================================================================
# GMM ===================================================================================================================================================================================
library(lme4)
library(lmerTest)

ParticipantNum <- c(rep(seq(1,NP),8))
GroupSize <- c(rep("SmallGroup",NP*4),rep("LargeGroup",NP*4))
NumAgents <- c(rep(c(rep(1,NP),rep(2,NP),rep(3,NP),rep(4,NP)),2))
RT <- c(rowMeans(RT_smallGroup1,na=TRUE),rowMeans(RT_smallGroup4,na=TRUE),rowMeans(RT_smallGroup7,na=TRUE),rowMeans(RT_smallGroup10,na=TRUE),rowMeans(RT_largeGroup1,na=TRUE),rowMeans(RT_largeGroup4,na=TRUE),rowMeans(RT_largeGroup7,na=TRUE),rowMeans(RT_largeGroup10,na=TRUE))

df4GMM <- data.frame(ParticipantNum, groupSize, NumAgents, FollowPercentage)

df5GMM <- df4GMM[df1GMM$RT != 0, ]
df6GMM <- df5GMM[!is.na(df2GMM$RT)]


# Fit a mixed-effects model =============================================================================================================================================================
mixed_model <- lmer(RT ~ (NumAgents*GroupSize) + (1|ParticipantNum), data = df6GMM)
summary(mixed_model)
write.csv(df6GMM, "C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\RTSize.csv", row.names = FALSE)
# =======================================================================================================================================================================================
# ANOVA =================================================================================================================================================================================
within_anova_RT <- aov_car(RT ~ (NumAgents * GroupSize) + Error(ParticipantNum/(NumAgents * GroupSize)), data = df6GMM)
summary(within_anova_RT)