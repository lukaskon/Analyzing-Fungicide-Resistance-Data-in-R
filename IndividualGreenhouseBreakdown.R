



library(ggplot2)
library(tidyverse)
library(forcats)
library(dplyr)
library(tidyr)
library(readxl)
library(devtools)
library(grDevices)
library(RColorBrewer)


setwd("C:/Users/nikki/Michigan State University/PSM.Hausbecklab - Nikki Lukasko - Nikki Lukasko/Botrytis/Fungicide Sensitivity/Results")


#Remember to change Year and CCR to character instead of numerical data
#Import dataset -> read in as excel file




COMPLETE_FSAR <- read_excel("COMPLETE_FSAR_R.xlsx", 
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
View(COMPLETE_FSAR)



#read excel file as a data frame

BotrytisFSAR_framed <- data.frame(COMPLETE_FSAR)


SW_Petunia_R <- subset(BotrytisFSAR_framed, subset=Greenhouse %in% c("R", "BN"))


#Southwest petunia greenhouse R/BN over 2 sampling periods
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


display.brewer.all()


install.packages('egg', dependencies = TRUE)
library(egg)


ExampleGreenhouses <- subset(BotrytisFSAR_framed, subset=Greenhouse %in% c("R", "BN", "V", "BT", "Y", "BR", "I", "BF", "L", "BH", "Q", "BL"))

facet.IndividualGreenhouses <- ExampleGreenhouses %>%
  group_by(Region, Greenhouse, Growing.Cycle, Crop) %>%
  summarise_at(vars(Fenhexamid, Iprodione, Fludioxonil, Thiophanate.methyl, Boscalid, 
                    Fluopyram, Cyprodinil, Pyraclostrobin), 
               list(mean)) %>%
  gather(key=Fungicide, value=Value, c(Fenhexamid, Iprodione, Fludioxonil, 
                                       Thiophanate.methyl, Boscalid, Fluopyram,
                                       Cyprodinil, Pyraclostrobin)) %>%
  mutate(Fungicide=fct_reorder(Fungicide, Value)) %>%
  ggplot(aes(x=reorder(Fungicide, -Value), y=Value, fill=Growing.Cycle)) +
  scale_x_discrete(labels=c('Thi', 'Pyr', 'Bos', 'Ipr', 'Cyp', 'Fen', 'Flud', "Fluo"), expand = c(0, 0)) +
  geom_bar(alpha=0.6, binwidth = 5, stat="identity", position="dodge") +
  theme_light() +
  theme(strip.background = element_rect(fill="gray86", linewidth =1, linetype = "solid"),
        text = element_text(size = 14), 
        axis.text.x.bottom = element_text(size=14, color = "black", margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size = 14, vjust = 2.5, color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 14, color = "black"))+
  xlab("Fungicide") +
  ylab("Resistance Frequency") +
  facet_grid(Crop~Region) +
  scale_fill_manual(values = c("dodgerblue4", "red3"))+
  guides(fill=guide_legend(title= "Growing Cycle"))

facet.IndividualGreenhouses

library(extrafont)
font_import()
loadfonts(device = "win")

# Important to "tag" each facet before called the theme, otherwise titles will be removed.

# https://stackoverflow.com/questions/11889625/annotating-text-on-individual-facet-in-ggplot2/52217208#52217208
Ind_Greenhouses_Label <- tag_facet(facet.IndividualGreenhouses, tag_pool = c('Q','I','L','V','R','Y'), size =3.5,
                                  vjust=2, open = "", close = "")
Greenhouses_Ind_labelled <- Ind_Greenhouses_Label  + theme_light() +
  theme(strip.background = element_rect(fill="gray86", linewidth =1, linetype = "solid"),
        text = element_text(size = 10), 
        axis.text.x.bottom = element_text(size=10, color = "black", margin = margin(t = 0, r = 0, b = 15, l = 0)),
        axis.title.y = element_text(size = 10, vjust = 2.5, color = "black"),
        panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(color="black" ),
        panel.border = element_blank(),
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size = 10, color = "black"))+
  xlab("Fungicide") +
  ylab("Resistance frequency") +
  facet_grid(Crop~Region) +
  scale_fill_manual(values = c("dodgerblue4", "red3"))+
  guides(fill=guide_legend(title= "Growing cycle"))
Greenhouses_Ind_labelled


ggsave("IndGre_9.5x4.5_400dpi.tiff" , Greenhouses_Ind_labelled, device='tiff', 
       units="in", width=9.5, height=4.5, dpi=400)


sum(BotrytisFSAR_framed$Greenhouse=="BR")
