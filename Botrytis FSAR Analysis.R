#BotrytisFSAR contains fungicide resistance data
#Data for 2021 seminar
#Nikki Lukasko
#10-7-21

library(ggplot2)
library(tidyverse)
library(forcats)
library(dplyr)
library(tidyr)
library(readxl)
library(devtools)
library(grDevices)
library(RColorBrewer)
library(ggpubr)
library(grid)


setwd("C:/Users/nikki/Michigan State University/PSM.Hausbecklab - Nikki Lukasko - Nikki Lukasko/Botrytis/Fungicide Sensitivity/Results")


#Remember to change Year and CCR to character instead of numerical data
#Import dataset -> read in as excel file



library(readxl)
COMPLETE_FSAR <- read_excel("COMPLETE_FSAR.xlsx", 
                              +     col_types = c("text", "text", "text", 
                                                  +         "numeric", "numeric", "text", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "text", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric", 
                                                  +         "numeric", "numeric", "numeric"))
View(COMPLETE_FSAR)


#read excel file as a data frame

BotrytisFSAR_framed <- data.frame(COMPLETE_FSAR)


#All fungicide resistance frequencies

FRF.total <- BotrytisFSAR_framed %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value)) + 
  theme_light() +
  theme(text = element_text(size = 18), 
        axis.text.x.bottom = element_text(size=18, angle=45, vjust = 0.85, hjust = 0.8, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(vjust = 2.5),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_col(position="dodge", fill="darkgreen") +
  #geom_text(aes(label="mean")), vjust=-1)+
  ylab('Fungicide Resistance Frequency')+
  ylim(0,1) +
  xlab('Fungicide')
  #labs(caption = paste("n="))

FRF.total

ggsave("FRF_total_ECFG.tiff" , FRF.total, device='tiff', dpi=200)




#For only the mean fungicide resistance freq values

BotrytisFSAR_framed %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

BotrytisFSAR_framed %>%
  summarise_at(vars(CCR0, CCR1, CCR2, CCR3, CCR4, CCR5, CCR6, CCR7), 
               list(mean))

#Plot fungicide resistance frequencies by CROP

#crop.colors <- c(data=BotrytisFSAR_framed, Geranium="turquoise4", Petunia= "salmon2", Poinsettia="slategray4")

FRF.Crop <- BotrytisFSAR_framed %>%
  group_by(Crop) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Crop)) + 
  scale_x_discrete(labels=c('Thi', 'Pyr', 'Bos', 'Ipr', 'Cyp', 'Fen', 'Flud', "Fluo"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme_light() +
  theme(text = element_text(size = 14), 
        axis.text.x.bottom = element_text(size=14, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(vjust = 2.5),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency')+
  ylim(0,1) +
  xlab('Fungicide') +
  scale_fill_manual(values = c("#336633", "#CC9966","#999999"))
  #scale_fill_manual(values = c("grey79", "grey37","grey7"))+
  #scale_fill_brewer(palette = "Dark2")

FRF.Crop
ggsave("FRF_crop_manuscript.tiff", FRF.Crop, device='tiff', dpi=300)


#Plot fungicide resistance frequencies by REGION

#region.colors <- c(data=BotrytisFSAR_framed, Kalamazoo="indianred4", Ottawa="darkseagreen4", Oakland="bisque3")

FRF.Region <- BotrytisFSAR_framed %>%
  group_by(Region) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Region)) + 
  scale_x_discrete(labels=c('Thi', 'Pyr', 'Bos', 'Ipr', 'Cyp', 'Fen', 'Flud', "Fluo"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme_light() +
  theme(text = element_text(size = 14), 
        axis.text.x.bottom = element_text(size=14, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(vjust = 2.5),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency') +
  ylim(0,1) +
  xlab('Fungicide') +
  scale_fill_manual(values = c("#336633", "#CC9966","#999999"))
  #scale_fill_manual(values = c("indianred4", "darkseagreen4", "bisque3"))
  #scale_fill_manual(values = c("grey79", "grey37","grey7"))

FRF.Region  
ggsave("FRF_region_manuscript.tiff" , FRF.Region, device='tiff', dpi=300)


#Plot fungicide resistance frequencies by YEAR (this is for 2 years only-update excel)

#date.colors <- c(data=BotrytisFSAR_framed, "2019" ="darkolivegreen", "2021" ="seashell3")

FRF.Season <- BotrytisFSAR_framed %>%
  group_by(Growing.Cycle) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Growing.Cycle)) + 
  scale_x_discrete(labels=c('Thi', 'Pyr', 'Bos', 'Ipr', 'Cyp', 'Fen', 'Flud', "Fluo"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme_light() +
  theme(text = element_text(size = 14), 
        axis.text.x.bottom = element_text(size=14, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(vjust = 2.5),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency') +
  ylim(0,1) +
  xlab('Fungicide') +
  scale_fill_manual(values = c("#336633", "#CC9966","#999999")) +
  guides(fill=guide_legend(title= "Growing cycle"))
  #scale_fill_manual(values = date.colors)
  #scale_fill_manual(values = c("grey79", "grey19"))

FRF.Season
ggsave("FRF_growingcycle_manuscript.tiff" , FRF.Season, device='tiff', dpi=300)




#Combine fungicide resistance frequency plots into one image using ggpubr packages (could use cowplot)


FSR_allplots <- ggarrange(FRF.Crop + rremove("ylab") + rremove("xlab"),
          FRF.Region + rremove("ylab")+ rremove("xlab"),
          FRF.Season + rremove("ylab"),
          #labels=c("A", "B", "C"), vjust = -0.2,
          ncol=1)
FSR_allplots

FRFx3 <- annotate_figure(FSR_allplots,
                left = text_grob("Fungicide resistance frequency",
                                 rot = 90, vjust = 0.5, size = 14))
FRFx3

ggsave("FRFx3_5.5x9_400dpi.tiff" , FRFx3, device='tiff', units="in", width=5.5, height=9, dpi=400)




#single greenhouse (or see "IndividualGreenhouseBreakdown.R" file for more)

SW_Petunia_R <- subset(BotrytisFSAR_framed, subset=Greenhouse %in% c("R", "BN"))
SW_Petunia_R_FRF <- SW_Petunia_R %>%
  group_by(Year) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Year)) + 
  scale_x_discrete(labels=c('Thi', 'Pyr', 'Bos', 'Ipr', 'Cyp', 'Fen', 'Flud', "Fluo"), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0,100), expand = c(0, 0)) +
  theme_light() +
  theme(text = element_text(size = 18), 
        axis.text.x.bottom = element_text(size=18, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(vjust = 2.5),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  geom_col(position=position_dodge(.8), width=0.8) +
  ylab('Fungicide Resistance Frequency') +
  ylim(0,1) +
  xlab('Fungicide') +
  #scale_fill_manual(values = date.colors)
  scale_fill_manual(values = c("chocolate", "cadetblue"))

SW_Petunia_R_FRF



#color

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#can make your own palette
cbPalette


display.brewer.all()
 # ggplot + scale_fill_brewer()


#pie chart of CCR frequencies


piechart <- BotrytisFSAR_framed %>%
  ggplot(aes(x="", y=CCR, fill=factor(CCR)))+
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(),
        plot.title = element_text(hjust=0.5)) +
  labs(fill="CCR", x=NULL, y=NULL ) +
  coord_polar(theta = "y", start=0) +
  scale_fill_brewer(palette = "Dark2")

piechart #does not make sense if you look at actual percentages?


#consider a waffle instead of pie chart?

#waffle chart
install.packages("waffle", repos = "https://cinc.rud.is")
library(waffle)

# ERROR: dependencies 'htmlwidgets', 'DT' are not available for package 'waffle'
# * removing 'C:/Users/nikki/Documents/R/win-library/4.0/waffle'


#compare the proportion of CCR across crops/regions


#proportion of CCR by region
  
PropCCR.Region <- BotrytisFSAR_framed %>%
  group_by(Region) %>%
  ggplot(aes(x=Region, fill=CCR)) + 
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.text.x.bottom = element_text(size=12, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size=14, vjust = 2.5),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color="black" ),
        panel.border = element_blank()) +
  geom_bar(position=position_fill(.8), width=0.8) +
  scale_fill_brewer(palette= "RdGy") +
  ylab ('Proportion')+
  coord_flip()

PropCCR.Region
ggsave("PropCCR.Region.tiff" , PropCCR.Region, device='tiff', dpi=300)

#proportion of CCR by crop

PropCCR.Crop <- BotrytisFSAR_framed %>%
  group_by(Crop) %>%
  ggplot(aes(x=Crop, fill=CCR)) + 
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.text.x.bottom = element_text(size=12, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size=14, vjust = 2.5),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color="black" ),
        panel.border = element_blank()) +
  geom_bar(position=position_fill(.8), width=0.8) +
  scale_fill_brewer(palette= "RdGy") +
  ylab ('Proportion')+
  coord_flip()

PropCCR.Crop

ggsave("PropCCR.Crop.tiff" , PropCCR.Crop, device='tiff', dpi=300)


#proportion of CCR by year

PropCCR.GrowingCycle <- BotrytisFSAR_framed %>%
  group_by(Growing.Cycle) %>%
  ggplot(aes(x=Growing.Cycle, fill=CCR)) + 
  theme_minimal() +
  theme(text = element_text(size = 14),
        axis.text.x.bottom = element_text(size=12, margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size=14, vjust = 2.5),
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(color="black" ),
        panel.border = element_blank()) +
  geom_bar(position=position_fill(.8), width=0.8) +
  scale_fill_brewer(palette = "RdGy") +
  ylab ('Proportion') +
  xlab ('Growing cycle') +
  coord_flip() 

PropCCR.GrowingCycle

ggsave("PropCCR.GrowingCycle.tiff" , PropCCR.GrowingCycle, device='tiff', dpi=300)



#Combine CCR plots into a single figure

CCR_allplots <- ggarrange(PropCCR.Crop + rremove("xlab") + guides(fill="none"),
                          PropCCR.Region + rremove("xlab") + guides(fill="none"),
                          PropCCR.GrowingCycle + rremove("xlab"),
                          #labels=c("A", "B", "C"), vjust = -0.2,
                          ncol=3)
CCR_allplots

CCR_allplots_ann <- annotate_figure(CCR_allplots,
                                    bottom = text_grob("Proportion of isolates",
                                                     vjust = 0.5, size = 14))

CCR_allplots_ann

ggsave("CCRx3.tiff" , CCR_allplots_ann, device='tiff', units="in", width=9.5, height=3.0, dpi=400)



#Number of isolates
sum(BotrytisFSAR_framed)


sum(BotrytisFSAR_framed$Growing.Cycle=="1" & BotrytisFSAR_framed$Iprodione==1)/175
sum(BotrytisFSAR_framed$Growing.Cycle=="2" & BotrytisFSAR_framed$Iprodione==1)/211

sum(BotrytisFSAR_framed$Crop=="Petunia")
sum(BotrytisFSAR_framed$Crop=="Geranium" & BotrytisFSAR_framed$Fludioxonil==1)/144



sum(BotrytisFSAR_framed$Region=="Southwest")
sum(BotrytisFSAR_framed$Region=="West")
sum(BotrytisFSAR_framed$Region=="East")



sum(BotrytisFSAR_framed$Crop=="Poinsettia" & BotrytisFSAR_framed$Region=="Southwest" 
    & BotrytisFSAR_framed$Growing.Cycle==2)





#multiple bar graphs
#subset first

greenhouses_Canton <- subset(BotrytisFSAR_framed, subset=Greenhouse %in% c("B", "P", "V", "AF", "BM", "BT"))

view(greenhouses_Canton)

facet.Greenhouse <- greenhouses_Canton %>%
  group_by(Greenhouse) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  mutate(Fungicide=fct_reorder(Fungicide, Value)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Fungicide)) +
    geom_bar(alpha=0.6, binwidth = 5, stat="identity", position="dodge") +
  theme_light() +
  theme(
    legend.position="none", 
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(size=15, angle=60, vjust = 0.85, hjust = 0.8),
    text = element_text(size = 20)
  ) +
  xlab("Fungicide") +
  ylab("Resistance Frequency") +
  facet_wrap(~Greenhouse) +
  scale_fill_brewer(palette = "Dark2")

facet.Greenhouse


greenhouses_other <- subset(BotrytisFSAR_framed, subset=Greenhouse %in% c("B", "P", "V", "AF", "BM", "BT"))

  facet.Greenhouse_other <- greenhouses_other %>%
  group_by(Greenhouse) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  mutate(Fungicide=fct_reorder(Fungicide, Value)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Fungicide)) +
  geom_bar(alpha=0.6, binwidth = 5, stat="identity", position="dodge") +
    theme_light() +
    theme(text = element_text(size = 18), 
          #axis.text.x.bottom = element_text(size=18, margin = margin(t = 0, r = 0, b = 15, l = 0)),
          axis.title.y = element_text(vjust = 2.5),
          panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
          panel.border = element_blank(),
          axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks = element_blank()) +
  xlab("Fungicide") +
  ylab("Resistance Frequency") +
  facet_wrap(~Greenhouse) +
  scale_fill_brewer(palette = "Dark2")

facet.Greenhouse_other


#Read in only data from Canton Floral Gardens

CFG_raw <- read_excel("CantonFloralGardens.xlsx", 
                            col_types = c("text", "text", "text", 
                                          "text", "skip", "text", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "skip", "skip", "text", 
                                          "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))
View(CFG_raw)



#read excel file as a data frame

CFG <- data.frame(CFG_raw)





#pulling out isolates resistance to 7 (or another number) chemical classes only

sum(BotrytisFSAR_framed$CCR>=4)
sum(BotrytisFSAR_framed$MDR==8)


CCR0_isolates <- subset(BotrytisFSAR_framed, CCR==0)
CCR0_isolates
view(CCR0_isolates)


CCR7_isolates <- subset(BotrytisFSAR_framed, CCR==7)
view(CCR7_isolates)


sum(BotrytisFSAR_framed$CCR==7)


CCR6_isolates <- subset(BotrytisFSAR_framed, CCR==6)

CCR6_isolates %>%
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



CCR5_isolates <- subset(BotrytisFSAR_framed, CCR==5)

CCR5_isolates %>%
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




### Fungicide Resistance Frequencies Summaries by Factors###

BotrytisFSAR_framed %>%
summarise_at(vars(CCR0, CCR1, CCR2, CCR3, CCR4, CCR5, CCR6, CCR7), 
             list(mean))

Poinsettia_isolates <- subset(BotrytisFSAR_framed, Crop=="Poinsettia")
Poinsettia_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Petunia_isolates <- subset(BotrytisFSAR_framed, Crop=="Petunia")
Petunia_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Geranium_isolates <- subset(BotrytisFSAR_framed, Crop=="Geranium")
Geranium_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))



Poinsettia_isolates <- subset(BotrytisFSAR_framed, Crop=="Poinsettia")
Poinsettia_isolates %>%
summarise_at(vars(CCR0, CCR1, CCR2, CCR3, CCR4, CCR5, CCR6, CCR7), 
             list(mean))


Petunia_isolates <- subset(BotrytisFSAR_framed, Crop=="Petunia")
Petunia_isolates %>%
  summarise_at(vars(CCR0, CCR1, CCR2, CCR3, CCR4, CCR5, CCR6, CCR7), 
               list(mean))


Geranium_isolates <- subset(BotrytisFSAR_framed, Crop=="Geranium")
Geranium_isolates %>%
  summarise_at(vars(CCR0, CCR1, CCR2, CCR3, CCR4, CCR5, CCR6, CCR7), 
               list(mean))

Oakland_isolates <- subset(BotrytisFSAR_framed, Region=="Oakland")
Oakland_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Kalamazoo_isolates <- subset(BotrytisFSAR_framed, Region=="Kalamazoo")
Kalamazoo_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Ottawa_isolates <- subset(BotrytisFSAR_framed, Region=="Ottawa")
Ottawa_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))


Ottawa2019_isolates <- subset(BotrytisFSAR_framed, Region=="Ottawa" & Year==2019)
Ottawa2019_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Ottawa2021_isolates <- subset(BotrytisFSAR_framed, Region=="Ottawa" & Year==2021)
Ottawa2021_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Kalamazoo2019_isolates <- subset(BotrytisFSAR_framed, Region=="Kalamazoo" & Year==2019)
Kalamazoo2019_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Kalamazoo2021_isolates <- subset(BotrytisFSAR_framed, Region=="Kalamazoo" & Year==2021)
Kalamazoo2021_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Oakland2019_isolates <- subset(BotrytisFSAR_framed, Region=="Oakland" & Year==2019)
Oakland2019_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))

Oakland2021_isolates <- subset(BotrytisFSAR_framed, Region=="Oakland" & Year==2021)
Oakland2021_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))


Fluo_isolates <- subset(BotrytisFSAR_framed, Fluopyram==1)
Fluo_isolates %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin, CCR2, CCR3, CCR4, CCR5, CCR6), 
               list(mean))


#Multifungicide resistance analysis


FRF.CCR1 <- subset(BotrytisFSAR_framed, CCR==1) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR1
FRF.CCR2 <- subset(BotrytisFSAR_framed, CCR==2) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR2
FRF.CCR3 <- subset(BotrytisFSAR_framed, CCR==3) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR3
FRF.CCR4 <- subset(BotrytisFSAR_framed, CCR==4) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR4
FRF.CCR5 <- subset(BotrytisFSAR_framed, CCR==5) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR5
FRF.CCR6 <- subset(BotrytisFSAR_framed, CCR==6) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR6
FRF.CCR7 <- subset(BotrytisFSAR_framed, CCR==7) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean))
FRF.CCR7

CCR_table <- rbind(
  FRF.CCR1, FRF.CCR2, FRF.CCR3, FRF.CCR4, FRF.CCR5, FRF.CCR6, FRF.CCR7
)
CCR_table
#input into excel, Data>Text to columns option















