# For Group Size ===============================================================
# Follow percentage ============================================================
ParticipantNumber <-rep(c(seq(1,NP)),8)
NAgents <- c(rep(c(rep(1,NP),rep(4,NP),rep(7,NP),rep(10,NP)),2))
GSize <- c(rep(c(rep(1,NP)),4),rep(c(rep(2,NP)),4))
Follow <- c(PercentageFollow_smallGroup1, PercentageFollow_smallGroup4, PercentageFollow_smallGroup7, PercentageFollow_smallGroup10, PercentageFollow_largeGroup1, PercentageFollow_largeGroup4, PercentageFollow_largeGroup7, PercentageFollow_largeGroup10)
my_data <- data.frame(ParticipantNumber, NAgents,GSize,Follow)

my_data$Factor1 <- factor(my_data$NAgents)
my_data$Factor2 <- factor(my_data$GSize)
my_data$value <- my_data$Follow

# Create violin plot with color mapped to Factor2
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\ViolinPlot_Follow_GroupSize.png", width=1200, height=700)
ggplot(my_data, aes(x = Factor1, y = value, fill = Factor2)) +
  geom_violin()+
  scale_fill_manual(values = c("darkseagreen3", "darkolivegreen4")) + 
  labs(title = "Violin Plot for follow percentage (Group-Size experiment)", x = "Number of agents", y = "Follow percentage")
dev.off()
# set.seed(123)                 # when I want to reproduce the same random array
# ==============================================================================
# RT ===========================================================================
ParticipantNumber <-rep(c(seq(1,NP)),8)
NAgents <- c(rep(c(rep(1,NP),rep(4,NP),rep(7,NP),rep(10,NP)),2))
GSize <- c(rep(c(rep(1,NP)),4),rep(c(rep(2,NP)),4))
my_data <- data.frame(ParticipantNumber, NAgents,GSize,RT)

my_data$Factor1 <- factor(my_data$NAgents)
my_data$Factor2 <- factor(my_data$GSize)
my_data$value <- my_data$RT

# Create violin plot with color mapped to Factor1
png(file="C:\\Users\\asiye\\Downloads\\SciOI_R-master\\SciOI_R-master\\SciOI_Density+GroupSize_R\\ViolinPlot_RT_GroupSize.png", width=1200, height=700)
ggplot(my_data, aes(x = Factor1, y = value, fill = Factor2)) +
  geom_violin()+
  scale_fill_manual(values = c("darkseagreen3", "darkolivegreen4")) + 
  labs(title = "Violin Plot for RT (Group-Size experiment)", x = "Number of agents", y = "RT")
dev.off()