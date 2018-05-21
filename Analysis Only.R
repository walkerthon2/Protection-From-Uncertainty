#Analyses data and creates a graph
#Note to check response time exclusions, they're pretty close right now (within 3 sig fig) but not exactly equal
require('tidyverse')
require('R.matlab')
require('ez')
require('lme4')
require('aod')
require('car')
require('lmerTest')
require('BayesFactor')

source('readData.R', TRUE)

finalData = readData(45, 2)

#Responding
#Stage 1 Responding ----
testBlocks <- filter(finalData, Section == 1) %>%
  select(Optimal, Condition, ShortBlock, Subject)

#Logistic Regression
#https://osf.io/z57tn/
testBlocks$ShortBlock <- factor(testBlocks$ShortBlock)
testBlocks$Condition <- factor(testBlocks$Condition)

TBCount <- mutate(testBlocks, counter = Subject>0)
TBCount <- aggregate(counter ~ Subject + ShortBlock + Condition, data = TBCount, sum)
TBCondense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = testBlocks, sum)
TBCondense <- mutate(TBCondense, count = TBCount$counter)

TBCondense$Subject = factor(TBCondense$Subject)
logModelS1 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                    family = binomial(),
                    data = TBCondense, 
                    contrasts = list(ShortBlock = contr.poly, Condition = contr.sum(2)),
                    control = glmerControl(optimizer = 'bobyqa'))

sumS1 <- summary(logModelS1)
anovS1 <- Anova(logModelS1, type = "III")
# pliS1 <- logModelS1_ci <- confint(logModelS1, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Transition Responding ----
testBlocks <- filter(finalData, ShortBlock == 9 | ShortBlock == 10) %>%
  select(Optimal, Condition, ShortBlock, Subject)

#Logistic Regression
#https://osf.io/z57tn/
testBlocks$ShortBlock <- factor(testBlocks$ShortBlock)
testBlocks$Condition <- factor(testBlocks$Condition)

TBCount <- mutate(testBlocks, counter = Subject>0)
TBCount <- aggregate(counter ~ Subject + ShortBlock + Condition, data = TBCount, sum)
TBCondense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = testBlocks, sum)
TBCondense <- mutate(TBCondense, count = TBCount$counter)

TBCondense$Subject = factor(TBCondense$Subject)
logModelTrans <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                    family = binomial(),
                    data = TBCondense, 
                    contrasts = list(ShortBlock = contr.poly, Condition = contr.sum(2)),
                    control = glmerControl(optimizer = 'bobyqa'))

sumTrans <- summary(logModelS1)
anovTrans <- Anova(logModelS1, type = "III")
# pliTrans <- logModelS1_ci <- confint(logModelS1, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Stage 2 Response ----
testBlocks <- filter(finalData, Section == 2) %>%
  select(Optimal, Condition, ShortBlock, Subject)

#Logistic Regression
#https://osf.io/z57tn/
testBlocks$ShortBlock <- factor(testBlocks$ShortBlock)
testBlocks$Condition <- factor(testBlocks$Condition)

TBCount <- mutate(testBlocks, counter = Subject>0)
TBCount <- aggregate(counter ~ Subject + ShortBlock + Condition, data = TBCount, sum)
TBCondense <- aggregate(Optimal ~ Subject + ShortBlock + Condition, data = testBlocks, sum)
TBCondense <- mutate(TBCondense, count = TBCount$counter)

TBCondense$Subject = factor(TBCondense$Subject)
logModelS1 <- glmer(cbind(Optimal, count - Optimal) ~ ShortBlock * Condition + (1 | Subject), 
                    family = binomial(),
                    data = TBCondense, 
                    contrasts = list(ShortBlock = contr.poly, Condition = contr.sum(2)),
                    control = glmerControl(optimizer = 'bobyqa'))

sumS2 <- summary(logModelS1)
anovS2 <- Anova(logModelS1, type = "III")
# pliS2 <- logModelS1_ci <- confint(logModelS1, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Eye Tracking
#Stage 1 ET----
testBlock <- filter(finalData, ShortBlock < 10) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)

testBlock$ShortBlock <- factor(testBlock$ShortBlock)
testBlock$Condition <- factor(testBlock$Condition)

TBHolderPred <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
TBHolderNP <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
TBET <- merge(TBHolderPred, TBHolderNP, all = TRUE)
TBET$Pred <- factor(TBET$Pred)
TBET <- aggregate(data = TBET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)

TBET$Subject <- factor(TBET$Subject)
#All the LMEs for S1
TBLME_S1 <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
              data = TBET,
              contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
              REML = FALSE)

anovLME_S1 <- Anova(TBLME, type = 'III')
sumLME_S1 <- summary(TBLME)
#Computer Profile Likelihood Confidence Intervals
# pciET_S1 <- confint(TBLME, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Regular ANOVA
ezANOVA(data = TBET,
        wid = list(Subject),
        within = list(ShortBlock, Pred), 
        between = Condition,
        dv = PropET, 
        type = '3')

#Bayesian ANOVA
BF_S1 <- anovaBF(PropET ~ Condition * ShortBlock * Pred + Subject, data = TBET, whichRandom = "Subject",
        progress=FALSE, iterations = 100000)

#Transition ET ----
testBlock <- filter(finalData, ShortBlock == 9 | ShortBlock == 10) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)

testBlock$ShortBlock <- factor(testBlock$ShortBlock)
testBlock$Condition <- factor(testBlock$Condition)

TBHolderPred <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
TBHolderNP <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
TBET <- merge(TBHolderPred, TBHolderNP, all = TRUE)
TBET$Pred <- factor(TBET$Pred)
TBET <- aggregate(data = TBET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)

TBET$Subject <- factor(TBET$Subject)
#All the LMEs for S1
TBLME_Trans <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
              data = TBET,
              contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
              REML = FALSE)

anovLME_Trans <- Anova(TBLME, type = 'III')
sumLME_Trans <- summary(TBLME)
#Computer Profile Likelihood Confidence Intervals
# pciET_S1 <- confint(TBLME, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Regular ANOVA
ezANOVA(data = TBET,
        wid = list(Subject),
        within = list(ShortBlock, Pred), 
        between = Condition,
        dv = PropET, 
        type = '3')

#Bayesian ANOVA
BF_Trans <- anovaBF(PropET ~ Condition * ShortBlock * Pred + Subject, data = TBET, whichRandom = "Subject",
                 progress=FALSE, iterations = 100000)

#Stage2 ET ----
testBlock <- filter(finalData, Section == 2) %>%
  select(PropNP, PropPred, Condition, ShortBlock, Subject)

testBlock$ShortBlock <- factor(testBlock$ShortBlock)
testBlock$Condition <- factor(testBlock$Condition)

TBHolderPred <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropPred) %>% 
  mutate(Pred = 1)
TBHolderNP <- select(testBlock, Condition, ShortBlock, Subject, PropET = PropNP) %>% 
  mutate(Pred = 2)
TBET <- merge(TBHolderPred, TBHolderNP, all = TRUE)
TBET$Pred <- factor(TBET$Pred)
TBET <- aggregate(data = TBET, PropET ~ Condition +  ShortBlock + Subject + Pred, mean)

TBET$Subject <- factor(TBET$Subject)
#All the LMEs 
TBLME_S2 <- lmer(PropET ~ Pred * Condition * ShortBlock + (1 | Subject), 
              data = TBET,
              contrasts  = list(ShortBlock = 'contr.poly', Condition = contr.sum, Pred = contr.sum),
              REML = FALSE)

anovLME_S2 <- Anova(TBLME, type = 'III')
sumLME_S2 <- summary(TBLME)
#Computer Profile Likelihood Confidence Intervals
# pciET_S1 <- confint(TBLME, parm = 'beta_', parallel = 'snow', ncpus = 16)

#Regular ANOVA
ezANOVA(data = TBET,
        wid = list(Subject),
        within = list(ShortBlock, Pred), 
        between = Condition,
        dv = PropET, 
        type = '3')

#Bayesian ANOVA
BF_S2 <- anovaBF(PropET ~ Condition * ShortBlock * Pred + Subject, data = TBET, whichRandom = "Subject",
                    progress=FALSE, iterations = 100000)

