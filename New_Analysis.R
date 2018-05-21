#Analyses data and creates a graph
#Note to check response time exclusions, they're pretty close right now (within 3 sig fig) but not exactly equal
require('tidyverse')
require('R.matlab')
require('ez')
require('lme4')
require('aod')
require('car')
require('lmerTest')

pNum <- 45 #Number of participants to analyse
cNum <- 2 #Number of conditions to Analyse

for (cond in 1:cNum){
  for(file in 1:pNum) {
    fileName <- paste(c("C",cond, "_S",file, '_ET1_processed_final.mat'), collapse = '') #Read in data file
    
    setwd('../Final Data (Stage 1)')
    tempData <- readMat(fileName) 
    tempData1 <- tempData$finalData #Create data frame from part 1
    tempData1 <- data.frame(tempData1)
    
    setwd('../Final Data (Stage 2)')
    tempData <- readMat(fileName) #Create data frame from part 2
    tempData2 <- tempData$finalData #Create data frame from part 1
    tempData2 <- data.frame(tempData2)
    tempData2$X2 <- tempData2$X2 + 18
    tempData2$X1 <- tempData2$X1 + 288
    
    subData = merge(tempData1, tempData2, all = TRUE) #Merge the two parts together together
    
    subData <- mutate(subData, Subject = file + (cond - 1) * pNum) #Add the subject number to the data frame
    subData <- mutate(subData, Condition = cond) #Add the condition to the data frame
    
    if (file > 1 | cond > 1) { #Add this participants data to the large data frame
      finalData <- merge(finalData, subData, all = TRUE) 
    } else {
      finalData <- subData
      
    }
    
  }
}

colnames(finalData) <- c('Trial', 'Block', 'LeftCue', 'RightCue', 'PredLoc', 'OptResponse', 
                         'Optimal', 'Suboptimal', 'Reward', 'RandVar','totalPoints', 
                         'LocCounter', 'RT', 'nothing?', 
                         'FixLeft', 'TimeLeft', 'FixRight', 'TimeRight', 
                         'FixPred', 'TimePred', 'FixNP', 'TimeNP', 'PropPred', 'PropNP', 'TimeCross',
                         'Subject', 'Condition') #Name the columns

finalData <- mutate(finalData, totalET = PropPred + PropNP)
finalData <-  mutate(finalData, Section = case_when(Block <= 18 ~ 1,
                                          Block > 18 ~ 2)) #Create sections

finalData <- mutate(finalData, ShortBlock = case_when(Block == 1 | Block == 2 ~ 1,
                                                      Block == 3 | Block == 4 ~ 2,
                                                      Block == 5 | Block == 6 ~ 3,
                                                      Block == 7 | Block == 8 ~ 4,
                                                      Block == 9 | Block == 10 ~ 5,
                                                      Block == 11 | Block == 12 ~ 6,
                                                      Block == 13 | Block == 14 ~ 7,
                                                      Block == 15 | Block == 16 ~ 8,
                                                      Block == 17 | Block == 18 ~ 9,
                                                      Block == 19 | Block == 20 ~ 10,
                                                      Block == 21 | Block == 22 ~ 11,
                                                      Block == 23 | Block == 24 ~ 12,
                                                      Block == 25 | Block == 26 ~ 13))

finalData <- as_tibble(finalData)

#Filter bad participants (DON'T FORGET TO REMOVE THESE)
finalData <-filter(finalData, !(Condition == 1 & Subject == 5))
finalData <-filter(finalData, !(Condition == 1 & Subject == 9))
finalData <-filter(finalData, !(Condition == 1 & Subject == 19))
finalData <-filter(finalData, !(Condition == 1 & Subject == 25))
finalData <-filter(finalData, !(Condition == 1 & Subject == 26))
finalData <-filter(finalData, !(Condition == 1 & Subject == 34))
finalData <-filter(finalData, !(Condition == 1 & Subject == 35))
finalData <-filter(finalData, !(Condition == 1 & Subject == 42))
finalData <-filter(finalData, !(Condition == 1 & Subject == 43))
finalData <-filter(finalData, !(Condition == 1 & Subject == 44))
finalData <-filter(finalData, !(Condition == 1 & Subject == 45))

finalData <-filter(finalData, !(Condition == 2 & Subject == 21 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 32 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 33 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 36 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 37 + (cond - 1) * pNum))


#Remove trials with bad RT times
#R Differs from Matlab because MATLAB averages the blocks first and then averages again (weighting blocks with fewer trials 
#higher than it should), R doesn't do this. 
finalData <- group_by(finalData, Subject, Block, Condition) %>% 
  mutate(RTcutoff = 2*sd(RT), RTmean = mean(RT)) %>% 
  ungroup() %>% 
  arrange(Condition, Subject, Trial) %>% 
  filter(RT < RTmean + RTcutoff, RT > RTmean - RTcutoff)

finalData$Condition <- factor(finalData$Condition, levels = c(1, 2), labels = c('S2C', 'S2M'))
# finalData$ShortBlock <- factor(finalData$ShortBlock)

#Tests for Matlab Stats ----
MatlabBlock <- group_by(finalData, Condition, Block, Subject, ShortBlock, Section) %>% 
  summarise(Performance = mean(Optimal),
            PropPred = mean(PropPred),
            PropNP = mean(PropNP),
            totalET = mean(totalET))

MatlabBlock <- aggregate(data = MatlabBlock, Performance ~ Condition + ShortBlock + Subject +  Section, mean)
MatlabBlock$ShortBlock <- factor(MatlabBlock$ShortBlock)
MatlabBlock$Condition <- factor(MatlabBlock$Condition)
MatlabBlockS1 <- filter(MatlabBlock, Section == 1)
MatlabBlockT <- filter(MatlabBlock, ShortBlock == 9 | ShortBlock == 10)
MatlabBlockS2 <- filter(MatlabBlock, Section == 2) 

x <- reshape(MatlabBlockS2, idvar = "Subject", timevar = "ShortBlock", direction = "wide")
write_excel_csv(x, 'MatlabComp.csv')

ezANOVA(data = MatlabBlockS1, 
        wid = Subject,
        between = Condition,
        within = ShortBlock,
        dv = Performance)

ezANOVA(data = MatlabBlockT, 
        wid = Subject,
        between = Condition,
        within = ShortBlock,
        dv = Performance)

ezANOVA(data = MatlabBlockS2, 
        wid = Subject,
        between = Condition,
        within = ShortBlock,
        dv = Performance)

  #Performance Trials
testBlock <- group_by(finalData, Condition, ShortBlock, Subject) %>% 
  summarise(Performance = mean(Optimal),
            PropPred = mean(PropPred),
            PropNP = mean(PropNP),
            totalET = mean(totalET)) %>% 
  group_by(Condition, ShortBlock) %>% 
  summarise(length = sum(Subject > 0), sePerf = sd(Performance)/sqrt(length), Performance = mean(Performance),
            sePred = sd(PropPred)/sqrt(length), PropPred = mean(PropPred), 
            seNP = sd(PropNP)/sqrt(length), PropNP = mean(PropNP),
            seTotal = sd(totalET)/sqrt(length), totalET = mean(totalET))

#Create some pretty graphs ----
windowsFonts(Arial=windowsFont("Arial"))
#Performance Graph
ggplot(data = testBlock, aes(x = ShortBlock, y = Performance, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = Performance - sePerf, ymax = Performance + sePerf), width = .5, size = .8) +
  geom_line(size = 1.5) +
  geom_point(size = 3, shape = 15) +
  coord_cartesian(ylim = c(.5, 1)) +
  geom_vline(xintercept = 9.5, size = 1.5, linetype = 2) +
  scale_color_manual(values=c("red", "dark orange")) +
  ggtitle('Responses') +
  xlab('Block') +
  scale_x_continuous(breaks = pretty(testBlock$ShortBlock, n = 14)) +
  ylab('Proportion of High Value Responses') +
  theme(plot.title = element_text(family = 'Arial', size = 20, hjust = .5),
        axis.title = element_text(family = 'Arial', size = 14),
        axis.text = element_text(family = 'Arial', size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'light grey'),
        legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#ET (All together)
ggplot(data = testBlock, aes(x = ShortBlock, y = totalET, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = totalET - seTotal, ymax = totalET + seTotal), width = .5, size = .8) +
  geom_line(size = 1.5) +
  geom_point(size = 3, shape = 15,) +
  coord_cartesian(ylim = c(.15, .45)) +
  geom_vline(xintercept = 9.5, size = 1.5, linetype = 2) +
  scale_color_manual(values=c("red", "dark orange")) +
  ggtitle('Eye-Gaze (Total)') +
  xlab('Block') +
  scale_x_continuous(breaks = pretty(testBlock$ShortBlock, n = 14)) +
  ylab('Proportion of Trial time Attending to Cues') +
  theme(plot.title = element_text(family = 'Arial', size = 20, hjust = .5),
        axis.title = element_text(family = 'Arial', size = 14),
        axis.text = element_text(family = 'Arial', size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'light grey'),
        legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#ET (split Pred/NP)
ggplot(data = testBlock, aes(x = ShortBlock, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = PropPred - sePred, ymax = PropPred + sePred), width = .5, size = .8) +
  geom_line(aes(y = PropPred, linetype = '1'), size = 1.5) +
  geom_point(aes(y = PropPred), size = 3, shape = 15,) +
  geom_errorbar(aes(ymin = PropNP - seNP, ymax = PropNP + seNP), width = .5, size = .8) +
  geom_line(aes(y = PropNP, linetype = '3'), size = 1.5) +
  geom_point(aes(y = PropNP), size = 3, shape = 15,) +
  coord_cartesian(ylim = c(0, .25)) +
  geom_vline(xintercept = 9.5, size = 1.5, linetype = 2) +
  scale_color_manual(values=c("red", "dark orange")) +
  scale_linetype_manual(name = 'P/NP', guide = 'legend', labels = c('Pred', "N-Pred"), values = c(1, 3)) +
  ggtitle('Eye-Gaze (By Cue)') +
  xlab('Block') +
  scale_x_continuous(breaks = pretty(testBlock$ShortBlock, n = 14)) +
  ylab('Proportion of Trial Time Attending to Cues') +
  theme(plot.title = element_text(family = 'Arial', size = 20, hjust = .5),
        axis.title = element_text(family = 'Arial', size = 14),
        axis.text = element_text(family = 'Arial', size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = 'light grey'),
        legend.justification = c(1, 0), 
        legend.position = c(1, 0),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))

#RUn Logistic Regressions ----
#Stage 1 ====
Stage1 <- filter(finalData, Section == 1) %>%
  select(Optimal, Condition, ShortBlock, Subject)

# Stage1 <- filter(finalData, ShortBlock <= 9 | ShortBlock > 3) %>%
#   select(Optimal, Condition, ShortBlock, Subject)

#Logistic Regression
#https://osf.io/z57tn/
Stage1$ShortBlock <- factor(Stage1$ShortBlock)
Stage1$Condition <- factor(Stage1$Condition)

Stage1Count <- mutate(Stage1, counter = Subject>0)
Stage1Count <- aggregate(counter ~ Subject + ShortBlock + Condition, data = Stage1Count, sum)
Stage1Condense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = Stage1, sum)
Stage1Condense <- mutate(Stage1Condense, count = Stage1Count$counter)

Stage1Condense$Subject = factor(Stage1Condense$Subject)
logModelS1 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                    family = binomial(),
                    data = Stage1Condense, 
                    contrasts = list(ShortBlock = contr.poly, Condition = contr.sum(2)),
                    control = glmerControl(optimizer = 'bobyqa'))
S1_2 <- glmer(cbind(Optimal, count - Optimal) ~ Condition + (1 | Subject), 
              family = binomial(),
              data = Stage1Condense,
              control = glmerControl(optimizer = 'bobyqa'))
S1_3 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + (1 | Subject), 
              family = binomial(),
              data = Stage1Condense,
              control = glmerControl(optimizer = 'bobyqa'))
S1_4 <- glmer(cbind(Optimal, count - Optimal) ~ 1 + (1 | Subject), 
              family = binomial(),
              data = Stage1Condense,
              control = glmerControl(optimizer = 'bobyqa'))
S1_5 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + Condition + (1 | Subject), 
              family = binomial(),
              data = Stage1Condense, 
              control = glmerControl(optimizer = 'bobyqa'))

anova(S1_4, S1_2) #Condition
anova(S1_4, S1_3) #ShortBlock
anova(S1_3, S1_5) #Condition + ShortBlock
anova(S1_5, logModelS1) #Full model w/ Interaction

summary(logModelS1)
  Anova(logModelS1, type = "III")
# logModelS1_ci <- confint(logModelS1, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Transition ====
Trans <- filter(finalData, ShortBlock == 9 | ShortBlock == 10) %>%
  select(Optimal, Condition, ShortBlock, Subject)

#Logistic Regression
#https://osf.io/z57tn/
Trans$ShortBlock <- factor(Trans$ShortBlock)
Trans$Condition <- factor(Trans$Condition)

TransCount <- mutate(Trans, counter = Subject>0)
TransCount <- aggregate(counter ~ Subject + ShortBlock + Condition, data = TransCount, sum)
TransCondense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = Trans, sum)
TransCondense <- mutate(TransCondense, count = TransCount$counter)

TransCondense$Subject = factor(TransCondense$Subject)
logModelTrans <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                       family = binomial(),
                       data = TransCondense, 
                       contrasts = list(ShortBlock = contr.poly, Condition = contr.treatment(2, base = 1)),
                       control = glmerControl(optimizer = 'bobyqa'))
Trans_2 <- glmer(cbind(Optimal, count - Optimal) ~ Condition + (1 | Subject), 
                 family = binomial(),
                 data = TransCondense, 
                 control = glmerControl(optimizer = 'bobyqa'))
Trans_3 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + (1 | Subject), 
                 family = binomial(),
                 data = TransCondense, 
                 control = glmerControl(optimizer = 'bobyqa'))
Trans_4 <- glmer(cbind(Optimal, count - Optimal) ~ 1 + (1 | Subject), 
                 family = binomial(),
                 data = TransCondense, 
                 control = glmerControl(optimizer = 'bobyqa'))
Trans_5 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + Condition + (1 | Subject), 
                 family = binomial(),
                 data = TransCondense, 
                 control = glmerControl(optimizer = 'bobyqa'))

anova(Trans_4, Trans_2) #Condition
anova(Trans_4, Trans_3) #ShortBlock
anova(Trans_3, Trans_5) #Condition + ShortBlock
anova(Trans_5, logModelTrans) #Full model w/ Interaction

summary(logModelTrans)
Anova(logModelTrans, type = "III")
logModelTrans_ci <- confint(logModelTrans, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Stage 2 ====
#Logistic Regression
#Run profile likelihood confidence intervals (if conf interval includes 0 it's not significant)
#Different intercepts for shortblock and subject
#make block discrete with polynomial contrasts
#CAR package anova function
#Define frequency
#Summarize the blocks
#https://osf.io/z57tn/
Stage2 <- filter(finalData, Section == 2) %>%
  select(Optimal, Condition, ShortBlock, Subject, totalET, PropPred, PropNP, Trial)
Stage2$ShortBlock <- factor(Stage2$ShortBlock)
Stage2$Condition <- factor(Stage2$Condition)

Stage2Count <- mutate(Stage2, counter = Subject>0)
Stage2Count <- aggregate(counter ~ Subject + ShortBlock + Condition, data = Stage2Count, sum)
Stage2Condense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = Stage2, sum)
Stage2Condense <- mutate(Stage2Condense, count = Stage2Count$counter)

Stage2Condense$Subject <- factor(Stage2Condense$Subject)
logModelS2 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                    family = binomial(),
                    data = Stage2Condense, 
                    contrasts = list(ShortBlock = contr.poly, Condition = contr.treatment(2, base = 1)),
                    control = glmerControl(optimizer = 'bobyqa'))
S2_2 <- glmer(cbind(Optimal, count - Optimal) ~ Condition + (1 | Subject), 
              family = binomial(),
              data = Stage2Condense,
              control = glmerControl(optimizer = 'bobyqa'))
S2_3 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + (1 | Subject), 
              family = binomial(),
              data = Stage2Condense, 
              control = glmerControl(optimizer = 'bobyqa'))
S2_4 <- glmer(cbind(Optimal, count - Optimal) ~ 1 + (1 | Subject), 
              family = binomial(),
              data = Stage2Condense, 
              control = glmerControl(optimizer = 'bobyqa'))
S2_5 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock + Condition + (1 | Subject), 
              family = binomial(),
              data = Stage2Condense, 
              control = glmerControl(optimizer = 'bobyqa'))

anova(S2_4, S2_2) #Condition
anova(S2_4, S2_3) #ShortBlock
anova(S2_3, S2_5) #Condition + ShortBlock
anova(S2_5, logModelS2) #Full model w/ Interaction

summary(logModelS2)
Anova(logModelS2, type = "III")
# logModelS2_ci <- confint(logModel, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Within/Between ANOVA
Stage2Mean <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = Stage2, mean)
ezANOVA(data = Stage2Mean,
        wid = list(Subject),
        within = list(ShortBlock), 
        between = Condition,
        dv = Optimal, 
        type = '3')


#Linear Mixed Model
#ET
Stage2HolderPred <- select(Stage2, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
Stage2HolderNP <- select(Stage2, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
Stage2ET <- merge(Stage2HolderPred, Stage2HolderNP, all = TRUE)
Stage2ET$Pred <- factor(Stage2ET$Pred)

linearModel <- lmer(PropET ~ ShortBlock * Condition * Pred + (1 | Subject), 
                  data = Stage2ET, 
                  contrasts  = list(ShortBlock = 'contr.poly'))

linearModel2 <- lmer(PropET ~ ShortBlock * Pred + (1 | Subject), 
                    data = Stage2ET, 
                    contrasts  = list(ShortBlock = 'contr.poly'))

summary(linearModel)
Anova(linearModel, type = 'III')



#Run LME for ET
#Stage 1====
Stage1 <- filter(finalData, ShortBlock < 10) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)


Stage1$ShortBlock <- factor(Stage1$ShortBlock)
Stage1$Condition <- factor(Stage1$Condition)

Stage1HolderPred <- select(Stage1, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
Stage1HolderNP <- select(Stage1, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
Stage1ET <- merge(Stage1HolderPred, Stage1HolderNP, all = TRUE)
Stage1ET$Pred <- factor(Stage1ET$Pred)
Stage1ET <- aggregate(data = Stage1ET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)

Stage1ET$Subject <- factor(Stage1ET$Subject)
#All the LMEs for S1
Base_S1 <- lmer(PropET ~ 1 + (1 | Subject), 
                data = Stage1ET,
                REML = FALSE)

S1_1 <- lmer(PropET ~ Pred + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_2 <- lmer(PropET ~ Condition + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_3 <- lmer(PropET ~ ShortBlock + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_4 <- lmer(PropET ~ ShortBlock + Pred + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_5 <- lmer(PropET ~ ShortBlock + Condition + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_6 <- lmer(PropET ~ Pred + Condition + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_7 <- lmer(PropET ~ Pred + Condition + ShortBlock + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_8 <- lmer(PropET ~ ShortBlock * Pred + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_9 <- lmer(PropET ~ ShortBlock * Condition + (1 | Subject), 
             data = Stage1ET,
             REML = FALSE)

S1_10 <- lmer(PropET ~ Pred * Condition + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_11 <- lmer(PropET ~ Pred * ShortBlock + Condition + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_12 <- lmer(PropET ~ Pred * Condition + ShortBlock + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_13 <- lmer(PropET ~ Pred + Condition * ShortBlock + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_14 <- lmer(PropET ~ Pred * Condition + Condition * ShortBlock + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_15 <- lmer(PropET ~ Pred * ShortBlock + Condition * ShortBlock + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_16 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_17 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + ShortBlock * Condition + (1 | Subject), 
              data = Stage1ET,
              REML = FALSE)

S1_18 <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
              data = Stage1ET,
              contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
              REML = FALSE)

anova(S1_1, S1_2, S1_3, S1_4, S1_5, S1_6, S1_7, S1_8, S1_9, S1_10, S1_11, S1_12, S1_13, S1_14, S1_15, S1_16, S1_17, S1_18, Base_S1)
#Condition has no main effect:Base_S1, S1_2; S1_11, S1_8
#Pred has a main effect: Base_S1, S1_1, S1_1, S1_9
#Short Block has a main effect: Base_S1, S1_3; S1_12, S1_10

anova(Base_S1, S1_2) #Condition
anova(Base_S1, S1_3) #Predictiveness
anova(Base_S1, S1_1) #Block
anova(S1_4, S1_8) #Block*Pred
anova(S1_5, S1_9) #Block * Cond
anova(S1_6, S1_10) #Cond*Pred
anova(S1_17, S1_18) #Three Way interaction

Anova(S1_18, type = 'III')
summary(S1_18)
#Computer Profile Likelihood Confidence Intervals
linModelS1_ci <- confint(S1_18, parm = 'beta_', parallel = 'snow', ncpus = 16)

ezANOVA(data = Stage1ET,
        wid = list(Subject),
        within = list(ShortBlock, Pred), 
        between = Condition,
        dv = PropET, 
        type = '3')

#Transition ====
Trans <- filter(finalData, ShortBlock == 9 | ShortBlock == 10) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)

Trans$ShortBlock <- factor(Trans$ShortBlock)
Trans$Condition <- factor(Trans$Condition)

TransHolderPred <- select(Trans, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
TransHolderNP <- select(Trans, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
TransET <- merge(TransHolderPred, TransHolderNP, all = TRUE)
TransET$Pred <- factor(TransET$Pred)
TransET <- aggregate(data = TransET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)


#All the LMEs for Trans
Base_T <- lmer(PropET ~ 1 + (1 | Subject), 
               data = TransET,
               REML = FALSE)

T_1 <- lmer(PropET ~ Pred + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_2 <- lmer(PropET ~ Condition + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_3 <- lmer(PropET ~ ShortBlock + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_4 <- lmer(PropET ~ ShortBlock + Pred + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_5 <- lmer(PropET ~ ShortBlock + Condition + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_6 <- lmer(PropET ~ Pred + Condition + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_7 <- lmer(PropET ~ Pred + Condition + ShortBlock + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_8 <- lmer(PropET ~ ShortBlock * Pred + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_9 <- lmer(PropET ~ ShortBlock * Condition + (1 | Subject), 
            data = TransET,
            REML = FALSE)

T_10 <- lmer(PropET ~ Pred * Condition + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_11 <- lmer(PropET ~ Pred * ShortBlock + Condition + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_12 <- lmer(PropET ~ Pred * Condition + ShortBlock + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_13 <- lmer(PropET ~ Pred + Condition * ShortBlock + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_14 <- lmer(PropET ~ Pred * Condition + Condition * ShortBlock + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_15 <- lmer(PropET ~ Pred * ShortBlock + Condition * ShortBlock + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_16 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_17 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + ShortBlock * Condition + (1 | Subject), 
             data = TransET,
             REML = FALSE)

T_18 <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
             data = TransET,
             contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
             REML = FALSE)


anova(T_1, T_2, T_3, T_4, T_5, T_6, T_7, T_8, T_9, T_10, T_11, T_12, T_13, T_14, T_15, T_16, T_17, T_18, Base_T)

anova(Base_T, T_2) #Condition
anova(Base_T, T_1) #Predictiveness
anova(Base_T, T_3) #Block
anova(T_4, T_8) #Block*Pred
anova(T_5, T_9) #Block * Cond
anova(T_6, T_10) #Cond*Pred
anova(T_17, T_18) #Three Way interaction

Anova(T_18, type = 'III')
# summary(T_18)
#Computer Profile Likelihood Confidence Intervals
linModelT_ci <- confint(T_18, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Stage 2 ====
Stage2 <- filter(finalData, ShortBlock >= 10) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)

Stage2$ShortBlock <- factor(Stage2$ShortBlock)
Stage2$Condition <- factor(Stage2$Condition)

Stage2HolderPred <- select(Stage2, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
Stage2HolderNP <- select(Stage2, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
Stage2ET <- merge(Stage2HolderPred, Stage2HolderNP, all = TRUE)
Stage2ET$Pred <- factor(Stage2ET$Pred)
Stage2ET <- aggregate(data = Stage2ET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)

#All the LMEs for S2
Base_S2 <- lmer(PropET ~ 1 + (1 | Subject), 
                data = Stage2ET,
                REML = FALSE)

S2_1 <- lmer(PropET ~ Pred + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_2 <- lmer(PropET ~ Condition + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_3 <- lmer(PropET ~ ShortBlock + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_4 <- lmer(PropET ~ ShortBlock + Pred + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_5 <- lmer(PropET ~ ShortBlock + Condition + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_6 <- lmer(PropET ~ Pred + Condition + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_7 <- lmer(PropET ~ Pred + Condition + ShortBlock + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_8 <- lmer(PropET ~ ShortBlock * Pred + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_9 <- lmer(PropET ~ ShortBlock * Condition + (1 | Subject), 
             data = Stage2ET,
             REML = FALSE)

S2_10 <- lmer(PropET ~ Pred * Condition + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_11 <- lmer(PropET ~ Pred * ShortBlock + Condition + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_12 <- lmer(PropET ~ Pred * Condition + ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_13 <- lmer(PropET ~ Pred + Condition * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_14 <- lmer(PropET ~ Pred * Condition + Condition * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_15 <- lmer(PropET ~ Pred * ShortBlock + Condition * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_16 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_17 <- lmer(PropET ~ Pred * Condition + Pred * ShortBlock + Condition * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              REML = FALSE)

S2_18 <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
              data = Stage2ET,
              contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
              REML = FALSE)

anova(S2_1, S2_2, S2_3, S2_4, S2_5, S2_6, S2_7, S2_8, S2_9, S2_10, S2_11, S2_12, S2_13, S2_14, S2_15, S2_16, S2_17, S2_18, Base_S2)

anova(Base_S2, S2_2) #Condition
anova(Base_S2, S2_1) #Predictiveness
anova(Base_S2, S2_3) #Block
anova(S2_4, S2_8) #Block*Pred
anova(S2_5, S2_9) #Block * Cond
anova(S2_6, S2_10) #Cond*Pred
anova(S2_17, S2_18) #Three Way interaction

Anova(S2_18, Type = 'III')

ezANOVA(data = Stage2ET,
        wid = list(Subject),
        within = list(ShortBlock, Pred), 
        between = Condition,
        dv = PropET, 
        type = '3')

# summary(S2_18)
#Computer Profile Likelihood Confidence Intervals
linModelS2_ci <- confint(S2_18, parm = 'beta_', parallel = 'snow', ncpus = 16)

# Basic Tests ====
View(group_by(Stage2ET, ShortBlock) %>%
  summarise(mean(PropET)))
x <- group_by(Stage2, Condition, Subject) %>%
       summarise(boop = mean(Optimal))
lol <- filter(x, Condition == 'S2C')
lol2 <- filter(x, Condition == 'S2M')
t.test(lol$boop, lol2$boop)

setwd('../New_Analysis')
