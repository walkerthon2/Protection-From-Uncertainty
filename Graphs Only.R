#Analyses data and creates a graph
#Note to check response time exclusions, they're pretty close right now (within 3 sig fig) but not exactly equal
require('tidyverse')
require('R.matlab')

source('readData.R', TRUE)

finalData = readData(45, 2)

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
