#BotrytisFSAR contains fungicide resistance data up until 10-6-21
#Data for 2021 seminar
#Nikki Lukasko
#10-7-21

library(ggplot2)
library(tidyverse)
library(forcats)
library(dplyr)
library(tidyr)


View(BotrytisFSAR)



#read excel file as a data frame

#Remember to change Year and CCR to character instead of numerical data. Try to change others to characters if this is a problem later.

BotrytisFSAR_framed <- data.frame(BotrytisFSAR)


columns <- c("Fungicides", "Mean")
practice <- data.frame(Fungicide_label, Fungicide_means1)
practice

ggplot(data=practice, aes(x=Fungicide_label, y=Fungicide_means, fill=Fungicide_means))+
  geom_bar(stat="identity")

#All fungicide resistance frequencies

BotrytisFSAR_framed %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value)) + 
  theme(text = element_text(size = 20), 
        axis.text.x.bottom = element_text(size=15, angle=45, vjust = 0.85, hjust = 0.8)) +
  geom_col(position="dodge", fill="steelblue3") +
  #geom_text(aes(label="mean")), vjust=-1)+
  ylab('Fungicide Resistance Frequency')+
  ylim(0,1) +
  xlab('Fungicide') +
  labs(caption = paste("n=339"))


#Plot fungicide resistance frequencies by CROP

crop.colors <- c(data=BotrytisFSAR_framed, Geranium="turquoise4", Petunia= "salmon2", 
                 Poinsettia="slategray4")

BotrytisFSAR_framed %>%
  group_by(Crop) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Crop)) + 
  theme(text = element_text(size = 20), 
        axis.text.x.bottom = element_text(size=18, angle=45, vjust = 0.85, hjust = 0.8),
        axis.title.y = element_text(vjust = 2.5)) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency')+
  ylim(0,1) +
  xlab('Fungicide')+
  scale_fill_manual(values = crop.colors)


#Plot fungicide resistance frequencies by REGION

region.colors <- c(data=BotrytisFSAR_framed, Kalamazoo="indianred4", Ottawa="darkseagreen4", Oakland="bisque3")

BotrytisFSAR_framed %>%
  group_by(Region) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Region)) + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size=15, angle=45, vjust = 0.85, hjust = 0.8),
        axis.title.y = element_text(vjust = 2.5)) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency') +
  ylim(0,1) +
  xlab('Fungicide') +
  scale_fill_manual(values = region.colors)
  



#Plot fungicide resistance frequencies by YEAR

date.colors <- c(data=BotrytisFSAR_framed, "2019" ="darkolivegreen4", "2021" ="slategray3")

BotrytisFSAR_framed %>%
  group_by(Year) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Year)) + 
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(size=15, angle=45, vjust = 0.85, hjust = 0.8),
        axis.title.y = element_text(vjust = 2.5)) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency') +
  ylim(0,1) +
  xlab('Fungicide') +
  scale_fill_manual(values = date.colors)


#color

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#can make your own palette




#pie chart of CCR frequencies


BotrytisFSAR_framed %>%
  ggplot(aes(x="", y=CCR, fill=factor(CCR)))+
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(fill="CCR", x=NULL, y=NULL ) +
  coord_polar(theta = "y", start=0) +
  scale_fill_manual(values = cbPalette)


#consider a waffle instead of pie chart?







#compare the proportion of CCR across crops/regions



#proportion of CCR by region
  
BotrytisFSAR_framed %>%
    group_by(Region) %>%
    ggplot(aes(x=Region, fill=CCR)) + 
    theme(text = element_text(size = 20)) +
    geom_bar(position=position_fill(.8), width=0.8) +
    scale_fill_manual(values = cbPalette) +
  ylab ('Proportion')+
  coord_flip()

#proportion of CCR by crop

BotrytisFSAR_framed %>%
  group_by(Crop) %>%
  ggplot(aes(x=Crop, fill=CCR)) + 
  theme(text = element_text(size = 20)) +
  geom_bar(position=position_fill(.8), width=0.8) +
  scale_fill_manual(values = cbPalette) +
  ylab ('Proportion')+
  coord_flip()

#proportion of CCR by year

BotrytisFSAR_framed %>%
  group_by(Year) %>%
  ggplot(aes(x=Year, fill=CCR)) + 
  theme(text = element_text(size = 20)) +
  geom_bar(position=position_fill(.8), width=0.8) +
  scale_fill_manual(values = cbPalette) +
  ylab ('Proportion') +
  labs(caption = paste("n=339"))+
  coord_flip()










#Number of resistant isolates
sum(BotrytisFSAR_framed$Fenhexamid)
sum(BotrytisFSAR_framed$Fludioxonil)

#several small histograms

library(hrbrthemes)
library(viridis)
library(forcats)


BotrytisFSAR_framed %>%
  group_by(Greenhouse) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  mutate(Fungicide=fct_reorder(Fungicide, Value)) %>%
  ggplot(aes(x=Fungicide, y=Value, fill=Fungicide)) +
    geom_bar(alpha=0.6, binwidth = 5, stat="identity", position="dodge") +
   theme(
    legend.position="none", 
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("Fungicide") +
  ylab("Resistance Frequency") +
  facet_wrap(~Greenhouse)








#pulling out isolates resistance to 7 chemical classes only
CCR7_isolates <- subset(BotrytisFSAR_framed, CCR==7)
CCR7_isolates
view(CCR7_isolates)

