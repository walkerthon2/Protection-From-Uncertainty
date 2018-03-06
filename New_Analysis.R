#Analyses data and creates a graph
#Note to check response time exclusions, they're pretty close right now (within 3 sig fig) but not exactly equal
require('tidyverse')
require('R.matlab')

pNum <- 6 #Number of participants to analyse
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
    
    subData <- mutate(subData, Subject = file) #Add the subject number to the data frame
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

#Remove trials with bad RT times
finalData <- group_by(finalData, Subject, Block, Condition) %>% 
  mutate(RTcutoff = 2*sd(RT), RTmean = mean(RT)) %>% 
  ungroup() %>% 
  arrange(Condition, Subject, Trial) %>% 
  filter(RT < RTmean + RTcutoff, RT > RTmean - RTcutoff)

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

testBlock$Condition <- factor(testBlock$Condition, levels = c(1, 2), labels = c('S2C', 'S2M'))

#Create some pretty graphs
#Performance Graph
ggplot(data = testBlock, aes(x = ShortBlock, y = Performance, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = Performance - sePerf, ymax = Performance + sePerf, width = .2)) +
  geom_line(size = 1.5) +
  coord_cartesian(ylim = c(.5, 1))

#ET (All together)
ggplot(data = testBlock, aes(x = ShortBlock, y = totalET, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = totalET - seTotal, ymax = totalET + seTotal, width = .2)) +
  geom_line(size = 1.5) +
  coord_cartesian(ylim = c(0, .5))

#ET (split Pred/NP)
ggplot(data = testBlock, aes(x = ShortBlock, colour = Condition, group = Condition)) +
  geom_errorbar(aes(ymin = PropPred - sePred, ymax = PropPred + sePred, width = .2)) +
  geom_line(aes(y = PropPred), size = 1.5) +
  geom_errorbar(aes(ymin = PropNP - seNP, ymax = PropNP + seNP, width = .2)) +
  geom_line(aes(y = PropNP), size = 1.5, linetype = 2) +
  coord_cartesian(ylim = c(0, .5)) 


setwd('../New_Analysis')
