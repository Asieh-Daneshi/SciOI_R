ParticipantNumber <-rep(c(seq(1,127)),8)
NAgents <- c(rep(c(rep(1,127),rep(4,127),rep(7,127),rep(10,127)),2))
GSize <- c(rep(c(rep(1,127)),4),rep(c(rep(2,127)),4))
my_data <- data.frame(ParticipantNumber, NAgents,GSize,RT)

my_data$Factor1 <- factor(my_data$NAgents)
my_data$Factor2 <- factor(my_data$GSize)

# set.seed(123)
# 
# # Creating example data for a 2*4 factorial design
# data <- expand.grid(
#   Factor1 = factor(rep(1:2, each = 4)),
#   Factor2 = factor(rep(1:4, times = 2))
# )

my_data$value <- my_data$RT

# Create violin plot with color mapped to Factor1
ggplot(my_data, aes(x = Factor1, y = value, fill = Factor2)) +
  geom_violin()+
  scale_fill_manual(values = c("darkseagreen3", "darkolivegreen4"))
  # labs(title = "Violin Plot", x = "Factor2", y = "Value")