# Analyzing-Fungicide-Resistance-Data-in-R
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

Fen <- mean(BotrytisFSAR$Fenhexamid)
Ipr <- mean(BotrytisFSAR$Iprodione)
Flud <- mean(BotrytisFSAR$Fludioxonil)
Thio <- mean(BotrytisFSAR$`Thiophanate-methyl`)
Bos <- mean(BotrytisFSAR$Boscalid)
Fluo <- mean(BotrytisFSAR$Fluopyram)
Cyp <- mean(BotrytisFSAR$Cyprodinil)
Pyra <- mean(BotrytisFSAR$Pyraclostrobin)

#means for each
Fen
Ipr
Flud
Thio
Bos
Fluo
Cyp
Pyra

Fungicide_means1 <- c(Fen, Ipr, Flud, Thio, Bos, Fluo, Cyp, Pyra)
Fungicide_label <- c('Fen', 'Ipr', 'Flud', 'Thio', 'Bos', 'Fluo', 'Cyp', 'Pyra')

dataall <- data.frame(
  Fungicides1=Fungicide_label, 
  ResistanceLevel=Fungicide_means
)

dataall %>% #does not include all data-only fungicide resistance frequencies
  arrange(ResistanceLevel) %>%
  mutate(name = factor(Fungicides1, levels = ResistanceLevel)) %>%
  ggplot( aes(x=Fungicides1, y=ResistanceLevel))+
  geom_bar(stat="identity")



#read excel file as a data frame

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
  ggplot(aes(x=Fungicide, y=Value)) + 
  geom_col(position="dodge") +
  ylab('Fungicide Resistance Frequency')


#Plot fungicide resistance frequencies by CROP


BotrytisFSAR_framed %>%
  group_by(Crop) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Crop)) + 
  geom_col(position="dodge") +
  ylab('Fungicide Resistance Frequency')+
  xlab('Fungicide')


#Plot fungicide resistance frequencies by REGION

BotrytisFSAR_framed %>%
  group_by(Region) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Region)) + 
  geom_col(position="dodge") +
  ylab('Fungicide Resistance Frequency') +
  xlab('Fungicide')




#pulling out isolates resistance to 7 chemical classes only
CCR7_isolates <- subset(BotrytisFSAR_framed, CCR==7)
CCR7_isolates
view(CCR7_isolates)
