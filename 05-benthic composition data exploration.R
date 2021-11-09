# Benthic composition 

# load libraries
library(tidyverse)
library(ggthemes)
library(viridis)

# import data  
emof <- read_csv("./Data/darwin_core/emof.csv") 

# tidy data

## specific benthic components
bc <- emof %>%
  mutate(site=word(measurementID,start=2,sep=":")) %>%
  mutate(date=word(measurementID,start=3,sep=":")) %>%
  mutate(parameter=word(measurementID,start=4,sep=":")) %>%
  mutate(component=word(measurementID,start=5,sep=":")) %>%
  filter(parameter == "benthic") %>%
  dplyr:: select(site,date,parameter,component,percent_cover=measurementValue)

## general benthic components
LHC <- c("CB","CM","CSM","CE","CME")
DC <- c("DC","DCA","R")
SC <- c("SC")
Others <- c("ACT","ANE","CMR","SP","ZO")
Algae <- c("MA","CA","TA")
Abiotic <- c("SA","SI","UN")

bc1 <- bc %>%
  mutate(general_component = ifelse(component %in% LHC,"Live Hard Coral",
                                    ifelse(component %in% DC,"Dead Coral",
                                           ifelse(component %in% SC,"Soft Coral",
                                                  ifelse(component %in% Others,"Other Invertebrates",
                                                         ifelse(component %in% Algae,"Algae",
                                                                ifelse(component %in% Abiotic,"Abiotic", "Unknown")))))))%>%
  group_by(site,date,general_component) %>%
  summarize(percent_cover = sum(as.numeric(percent_cover)))
           

# visualize data

png("./Plots/benthic1.png",width=6,height=4,units="in", res=1200)
ggplot(bc)+
  geom_col(aes(x=site,y=as.numeric(percent_cover),fill=component))+
  xlab("Transects")+
  ylab("Percent Cover (%)")+
  guides(fill = guide_legend("Benthic Components",ncol=2))+
  scale_fill_viridis(option="C", discrete = TRUE)+
  theme_few()
graphics.off()


png("./Plots/benthic2.png",width=6,height=4,units="in", res=1200)
ggplot(bc1)+
  geom_col(aes(x=site,y=as.numeric(percent_cover),fill=general_component))+
  xlab("Transects")+
  ylab("Percent Cover (%)")+
  guides(fill = guide_legend("Benthic Components"))+
  scale_fill_viridis(option="C",discrete = TRUE)+
  theme_few()
graphics.off()
