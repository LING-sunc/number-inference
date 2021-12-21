library(lme4)
library(lmerTest)
library(plyr)

data <- read.csv("../data/response 40ppl.csv", header = TRUE)
cntls <- data[data$Condition %in% c("CT", "CF"), ]
cntls$Correct <- ifelse(cntls$Response == cntls$CA, 1, 0)
acc.bysubject <- ddply(cntls, .(Participant), summarise, acc = mean(Correct))
bad.subjects<- acc.bysubject[acc.bysubject$acc < 0.8,]$Participant
cntls <- cntls[ !(cntls$Participant %in% bad.subjects),]
mean(cntls$Correct)
mean(cntls[cntls$Condition=="CT",]$Correct)
mean(cntls[cntls$Condition=="CF",]$Correct)
data <- data[data$Condition != "prac" & !(data$Participant %in% bad.subjects),]
target <- data[data$Condition =="target",] # 1 for yes, 0 for no
target$TargetResponse <- ifelse(target$Task == "notalt", target$Response,
                                ifelse(target$Task == "couldalt" & target$Response == 1, 0, 1)) # code target responses 
target$Item <- factor(target$Item)
target$Participant <- factor(target$Participant)
target$Scale <- factor(target$Scale, levels = c("S", "M", "N"))
target$Task <- factor(target$Task, levels = c( "couldalt", "notalt"))
contrasts(target$Task) <- c(0.5, -0.5)
target$FirstBlock <- factor(target$FirstBlock, levels = c("couldalt", "notalt"))
contrasts(target$FirstBlock) <- c(0.5, -0.5)

model1 <- glmer(TargetResponse ~ Task * Scale * FirstBlock + (1  | Participant) ,
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
model2 <- glmer(TargetResponse ~ Task + Scale + FirstBlock 
                + Task * Scale + Scale * FirstBlock + Task * FirstBlock + (1 | Participant) , 
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
model3 <- glmer(TargetResponse ~ Task + Scale + FirstBlock 
                + Task * Scale + Task * FirstBlock + (1 | Participant), 
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
model4 <- glmer(TargetResponse ~ Task + Scale + FirstBlock 
                + Scale * FirstBlock + Task * FirstBlock + (1 | Participant), 
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
summary(model1)
anova(model1, model2) # three way
anova(model2, model3) # test Scale * FirstBlock 
anova(model2, model4) # test Task * Scale (sign.)

model.noS <- glmer(TargetResponse ~ Task + FirstBlock + (1 | Participant), 
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
model.withS <- glmer(TargetResponse ~ Scale + Task + FirstBlock + (1 | Participant), 
                data = target, family = "binomial", control = glmerControl(optimizer = "bobyqa"))
anova(model.noS, model.withS) # test main effect 

# Task x Scale interaction, on each level of scale
summary(glmer(TargetResponse ~ Task  + (1 | Participant), 
              data = target[target$Scale == "S",], 
              family = "binomial", control = glmerControl(optimizer = "bobyqa")) )

summary(glmer(TargetResponse ~ Task  + (1 | Participant) , 
              data = target[target$Scale == "M",], 
              family = "binomial", control = glmerControl(optimizer = "bobyqa")) )

summary(glmer(TargetResponse ~ Task  + (1 | Participant) , 
              data = target[target$Scale == "N",], 
              family = "binomial", control = glmerControl(optimizer = "bobyqa")) )

#Three-way interaction, on each level of FirstBlock
contrasts(target$Task) <- c(-0.5, 0.5)
model.a <- glmer(TargetResponse ~ Task *Scale  + (1 | Participant), 
                 data = target[target$FirstBlock == "notalt",],
                 family = "binomial", control = glmerControl(optimizer = "bobyqa")) 

model.b <- glmer(TargetResponse ~ Task +Scale  + (1 | Participant), 
                 data = target[target$FirstBlock == "notalt",], 
                 family = "binomial", control = glmerControl(optimizer = "bobyqa")) 
anova(model.a, model.b)

target$Scale <- factor(target$Scale, levels = c("M", "N", "S"))

model.c <- glmer(TargetResponse ~ Task *Scale  + (1 | Participant), 
                 data = target[target$FirstBlock == "couldalt",],
                 family = "binomial", control = glmerControl(optimizer = "bobyqa")) 

model.d <- glmer(TargetResponse ~ Task +Scale  + (1 | Participant), 
                 data = target[target$FirstBlock == "couldalt",], 
                 family = "binomial", control = glmerControl(optimizer = "bobyqa")) 
anova(model.c, model.d)

