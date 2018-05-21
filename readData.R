readData <- function(pNum, cNum) {

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

return(finalData)


}