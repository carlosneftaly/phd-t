#' ---
#' Title: Exp1 - Syncom Assembly
#' author: Carlos N. Lozano
#' date: May 20th, 2020
#' ---

## Libraries 

library(tidyverse) 
library(ggsci)
# Data 

datSyn <- read.csv('E13. App1 - Beads/Data/2020.05.19 Exp1-Beads.csv',
                   header = T, sep = ';', dec = ','
                   )

datSyn$Strain <- factor(datSyn$Strain, levels = c('763', '764', '749', '757', 'Bacillus'))

# Plot the control group alone... 

dataCtrl <-  datSyn %>% filter(Treatment == 'Control' & Strain != 'Bacillus') %>%
  group_by(Day,Strain) %>%
  summarise(MeanCo = mean(Count)) %>%
  mutate(Fraction = MeanCo/sum(MeanCo)) 


Sctrl <- ggplot(dataCtrl, aes(x=Day, y=Fraction, fill=Strain)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + theme_minimal() + 
  labs(x='Time (days)', y='Proportion', title = "SynCom Baseline") +
  theme(legend.position = "none",legend.text = element_text(size = 18, face = "bold"),
        legend.title=element_blank(),axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18, face="bold")) + 
  scale_x_continuous(breaks = c(3, 5, 8, 11)) + scale_fill_jco()



# # Plot the WT group alone... 

dataWT <-  datSyn %>% filter(Treatment == 'WT' & Strain != 'Bacillus') %>%
  group_by(Day,Strain) %>%
  summarise(MeanCo = mean(Count)) %>%
  mutate(Fraction = MeanCo/sum(MeanCo)) 


Swt <- ggplot(dataWT, aes(x=Day, y=Fraction, fill=Strain)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + theme_minimal() + 
  labs(x='Time (days)', y = NULL, title = "SynCom + WT") +
  theme(legend.position = "none",legend.text = element_text(size = 18, face = "bold"),
        legend.title=element_blank(),axis.text.x=element_text(size=18),
        axis.text.y=element_blank(),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18, face="bold")) + 
  scale_x_continuous(breaks = c(3, 5, 8, 11)) + scale_fill_jco() 

# # Plot the sfp group alone... 

datasfp <-  datSyn %>% filter(Treatment == 'sfp' & Strain != 'Bacillus') %>%
  group_by(Day,Strain) %>%
  summarise(MeanCo = mean(Count)) %>%
  mutate(Fraction = MeanCo/sum(MeanCo)) 

Ssfp <- ggplot(datasfp, aes(x=Day, y=Fraction, fill=Strain)) + 
  geom_area(alpha=0.6 , size=1, colour="black") + theme_minimal() + 
  labs(x='Time (days)', y= NULL, title = "SynCom + sfp mutant") +
  theme(legend.text = element_text(size = 18, face = "bold"),
        legend.title=element_blank(),axis.text.x=element_text(size=18),
        axis.text.y=element_blank(),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18, face="bold")) + 
  scale_x_continuous(breaks = c(3, 5, 8, 11)) + scale_fill_jco()


## Lines WT and sfp 

datBacWT <- datSyn %>% 
  filter(Treatment == 'WT' & Strain == 'Bacillus') %>%
  group_by(Day) %>%
  summarise(MeanCo = mean(log10(Count)), 
            sD = sd(log10(Count)))


WT <- ggplot(datBacWT, aes(Day, MeanCo)) + 
  geom_line(colour='darkgreen', size=1) + 
  geom_ribbon(aes(ymin = MeanCo - sD, ymax = MeanCo+sD), alpha = 0.3) +
  labs (y = 'Log (CFU/g)', x = ' ', title = 'P5_B1 WT')   +
  theme(legend.text = element_text(size = 18, face = "bold"),
        legend.title=element_blank(),axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18, face="bold"), 
        plot.title = element_text(size = 20)) +
  theme_bw() + ylim(0,7.5)+ 
  scale_x_continuous(breaks = c(3, 5, 8, 11))


datBacsfp <- datSyn %>% 
  filter(Treatment == 'sfp' & Strain == 'Bacillus') %>%
  group_by(Day) %>%
  summarise(MeanCo = mean(log10(Count)), 
            sD = sd(log10(Count)))


SFP <- ggplot(datBacsfp, aes(Day, MeanCo)) + 
  geom_line(colour='darkgreen', size=1) +
  geom_ribbon(aes(ymin = MeanCo - sD, ymax = MeanCo+sD), alpha = 0.3) +
  labs (y = NULL, x = NULL, title = "P5_B1 sfp_mut") + ylim(0,7.5) + 
  theme(legend.text = element_text(size = 18, face = "bold"),
        legend.title=element_blank(),axis.text=element_text(size=18),
        axis.title=element_text(size=18,face="bold"),
        strip.text.x = element_text(size=18, face="bold"), 
        plot.title = element_text(size = 20)) + 
  theme_bw() + 
  scale_x_continuous(breaks = c(3, 5, 8, 11)) 

## Arraging 

( plot_spacer()   + WT + SFP ) / (Sctrl + Swt +  Ssfp)
