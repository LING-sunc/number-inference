data <- read.csv("/Users/chaosun/Dropbox/My Experiments/not_could_alt/data/response 40ppl.csv", header = TRUE)
cntls <- data[data$Condition %in% c("CT", "CF"), ]
cntls$Correct <- ifelse(cntls$Response == cntls$CA, 1, 0)
acc.bysubject <- ddply(cntls, .(Participant), summarise, acc = mean(Correct))
bad.subjects<- acc.bysubject[acc.bysubject$acc < 0.8,]$Participant
data <- data[data$Condition != "prac" & !(data$Participant %in% bad.subjects),]

yes.plot = summarySE(data, measurevar = "Response", groupvars = c("Task", "Scale", "Condition"))
yes.plot$Task <- c(rep(c("could Alt", "not Alt"), each = 9)) 
yes.plot$Scale <- c(rep(c("Possible", "Number", "Some"), each = 3)) 
yes.plot$Condition <- c(rep(c("Ctrl-N", "Ctrl-Y", "Target"), 3))
yes.plot$Task <- factor(yes.plot$Task, levels = c("not Alt", "could Alt") )
yes.plot$Scale <- factor(yes.plot$Scale, levels = c("Some", "Possible", "Number") )

yes.plot <- ggplot(data = yes.plot, aes(x = Scale, y = Response*100, fill = Condition)) +
  geom_bar( stat="identity", position="dodge", width=.5)+
  geom_errorbar(aes(ymin = (Response*100)-(se*100), ymax = (Response*100)+(se*100)), width = .1,position=position_dodge(.5)) +
  facet_wrap( ~ Task, ncol = 2, scales = "free_x") +
  scale_fill_brewer(palette="Set1") +
  ylab("Yes responses (%)") +
  labs( fill = "Condition") +
  scale_y_continuous(expand = c(0, 0))+
  theme(
    legend.key = element_rect(colour = "white"),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9),
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',colour = "grey90"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

target <- data[data$Condition =="target",] 
target$TargetResponse <- ifelse(target$Task == "notalt", target$Response,
                                ifelse(target$Task == "couldalt" & target$Response == 1, 0, 1))

targetSE = summarySE(target, measurevar = "TargetResponse", groupvars = c("Task", "Scale"))
targetSE$Task <- c(rep(c("could Alt", "not Alt"), each =3))
targetSE$Scale <- c(rep(c("Possible", "Number","Some"),  2)) 
colnames(targetSE)[1] <- "Probe"
targetSE$Probe <- factor(targetSE$Probe, levels = c("not Alt", "could Alt") )
targetSE$Scale <- factor(targetSE$Scale , levels = c("Some", "Possible", "Number"))

target.plot <- ggplot(data = targetSE, aes(x = Scale, y = TargetResponse*100, fill = Probe)) +
  geom_bar( stat="identity", position="dodge", width=.5)+
  geom_errorbar(aes(ymin = (TargetResponse*100)-(se*100), ymax = (TargetResponse*100)+(se*100)), 
                width = .1,position=position_dodge(.5)) +
  facet_wrap( ~ Scale, ncol = 3, scales = "free_x", labeller = labeller(SCALE = labels)) +
  scale_fill_brewer(palette="Set1") +
  # xlab("Scale") +
  ylab("Target responses (%)") +
  labs( fill = "Probe") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.key = element_rect(colour = "white"),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',colour = "grey90"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )

block = summarySE(target, measurevar = "TargetResponse", groupvars = c("Task", "Scale", "FirstBlock"))
block$Task <- c(rep(c( "could Alt", "not Alt"), each = 6))
block$Scale <- c(rep(c("Possible", "Number","Some"), each = 2, 2)) 
block$Block <- c(rep(c("could Alt first", "not Alt first"), 6))
colnames(block)[1] <- "Probe"
block$Probe <- factor(block$Probe, levels = c("not Alt", "could Alt") )
block$Scale <- factor(block$Scale , levels = c("Some", "Possible", "Number"))

block.plot <- ggplot(data = block, aes(x = Scale, y = TargetResponse*100, fill = Probe)) +
  geom_bar( stat="identity", position="dodge", width=.5)+
  geom_errorbar(aes(ymin = (TargetResponse*100)-(se*100), ymax = (TargetResponse*100)+(se*100)), 
                width = .1,position=position_dodge(.5)) +
  facet_wrap( ~ Block, ncol = 2 ) +
  scale_fill_brewer(palette="Set1") +
  xlab("Scale") +
  ylab("Target responses (%)") +
  labs( fill = "Probe") +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.key = element_rect(colour = "white"),
    legend.key.size = unit(0.5, "cm"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 9),
    axis.title.y = element_text(size = 11),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    panel.border = element_rect(colour = "black", fill=NA, size=1), 
    panel.grid.major = element_line(size = 0.25, linetype = 'solid',colour = "grey90"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  )
