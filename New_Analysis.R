#Analyses data and creates a graph
#Note to check response time exclusions, they're pretty close right now (within 3 sig fig) but not exactly equal
require('tidyverse')
require('R.matlab')
require('ez')
require('lme4')
require('aod')
require('car')

pNum <- 25 #Number of participants to analyse
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

finalData <-filter(finalData, !(Condition == 1 & Subject == 1)) #Filter bad participants (DON'T FORGET TO REMOVE THESE)
finalData <-filter(finalData, !(Condition == 1 & Subject == 5))
finalData <-filter(finalData, !(Condition == 1 & Subject == 9))
finalData <-filter(finalData, !(Condition == 1 & Subject == 14))
finalData <-filter(finalData, !(Condition == 1 & Subject == 19))
finalData <-filter(finalData, !(Condition == 1 & Subject == 26))

finalData <-filter(finalData, !(Condition == 2 & Subject == 19 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 20 + (cond - 1) * pNum))
finalData <-filter(finalData, !(Condition == 2 & Subject == 21 + (cond - 1) * pNum))

#Remove trials with bad RT times
finalData <- group_by(finalData, Subject, Block, Condition) %>% 
  mutate(RTcutoff = 2*sd(RT), RTmean = mean(RT)) %>% 
  ungroup() %>% 
  arrange(Condition, Subject, Trial) %>% 
  filter(RT < RTmean + RTcutoff, RT > RTmean - RTcutoff, RT <= 10)

finalData$Condition <- factor(finalData$Condition, levels = c(1, 2), labels = c('S2C', 'S2M'))
# finalData$ShortBlock <- factor(finalData$ShortBlock)

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

#Create some pretty graphs
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

Stage2 <- filter(finalData, Section == 2) %>%
  select(Optimal, Condition, ShortBlock, Subject, totalET, PropPred, PropNP)

#Logistic Regression
#Run profile likelihood confidence intervals (if conf interval includes 0 it's not significant)
#Different intercepts for shortblock and subject
#make block discrete with polynomial contrasts
#CAR package anova function
#Define frequency
#Summarize the blocks
#https://osf.io/z57tn/
Stage2$ShortBlock <- factor(Stage2$ShortBlock)
Stage2$Condition <- factor(Stage2$Condition)

Stage2Condense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = Stage2, sum)

logModel <- glmer(cbind(Optimal, 32 - Optimal) ~ ShortBlock * Condition + (1 + ShortBlock | Subject), 
                  family = binomial(),
                  data = Stage2, 
                  contrasts = list(ShortBlock = 'contr.poly'),
                  control = glmerControl(optimizer = 'bobyqa'))

summary(logModel)
Anova(logModel, type = "III")
# logModel_ci <- confint(logModel, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Within/Between ANOVA
summary(aov(cbind(Optimal, 32 - Optimal)~Condition * ShortBlock + Error(Subject/ShortBlock), data = Stage2Condense))

#Linear Mixed Model
#Responding
m1 = lmer(data = Stage2, Optimal ~ (1 | Subject), REML = FALSE)
m2 = lmer(data = Stage2, Optimal ~ Condition + (1 | Subject), REML = FALSE)
m3 = lmer(data = Stage2, Optimal ~ ShortBlock + (1 + ShortBlock | Subject), REML = FALSE)
m4 = lmer(data = Stage2, Optimal ~ Condition + ShortBlock + (1 + ShortBlock | Subject), REML = FALSE)
m5 = lmer(data = Stage2, Optimal ~ Condition * ShortBlock + (1 + ShortBlock | Subject), REML = FALSE)

anova(m1, m2, m3, m4, m5)


# 
View(group_by(Stage2, Condition, ShortBlock) %>%
  summarise(mean(Optimal)))

setwd('../New_Analysis')
