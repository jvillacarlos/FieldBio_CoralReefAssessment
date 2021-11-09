# Exploration of fish data

# load libraries
library(tidyverse)
library(taxize)
library(ggthemes)
library(cowplot)
library(viridis)


# import occurrence data 
occ <- read_csv("Data/darwin_core/occurrence.csv") 


# tidy data for fish data exploration
fish.occ <- occ %>%
  mutate(area=word(occurrenceID,start=1,sep=":"),
         site=word(occurrenceID,start=2,sep=":"),
         date=word(occurrenceID,start=3,sep=":"),
         group=word(occurrenceID,start=4,sep=":"),
         size=word(occurrenceID,start=6,sep=":"),
         size= as.numeric(gsub("cm","",size))) %>%
  filter(group=="fish") 


# visualize taxonomic tree of fish

fish.out <- classification(unique(word(fish.occ$taxonID,5,sep=":")), db='worms')

tidy.classification <- function(x,y){
  for(i in y){
    o <- as.data.frame(x[i])
    colnames(o) <- c("name","rank","id")
    assign(paste0("c2t_",i),o)
  }
  ls.c2t <- mget(ls(pattern="c2t_"))
}

fish.out.tidy <- tidy.classification(fish.out,c(1:length(fish.out)))

saveRDS(fish.out.tidy, "Outputs/classification/classification_fish")

fish.out.tidy <- read_rds("Outputs/classification/classification_fish")

png("./Plots/tree_fish.png",width=13,height=13,units="in", res=300)
fish.tree <- class2tree(fish.out.tidy)
plot(fish.tree,"fan")
graphics.off()


# summarize data into total count and total abundance per site
fish.occ1 <- fish.occ %>%
  gather("tax_level","tax_name",scientificName,genus,family,order,class,phylum) %>%
  group_by(site,group,size) %>%
  summarise(count=length(tax_name),
            abundance = sum(individualCount))

fish.occ2 <- fish.occ %>%
  gather("tax_level","tax_name",scientificName,genus,family,order,class,phylum) %>%
  group_by(site,group,tax_level,size) %>%
  summarise(unique=length(unique(tax_name))) %>%
  ungroup()%>%
  mutate(tax_seq = ifelse(tax_level == "scientificName",1,
                          ifelse(tax_level == "genus",2,
                                 ifelse(tax_level == "family",3,
                                        ifelse(tax_level == "order",4,
                                               ifelse(tax_level == "class",5,
                                                      ifelse(tax_level == "phylum",6,"none")))))))
fish.occ2$tax_seq <- as.factor(fish.occ2$tax_seq)
levels(fish.occ2$tax_seq) <- c("Species", "Genus","Family","Order","Class","Phylum")


p1 <- ggplot(fish.occ1) +
  geom_col(aes(count,site,fill=size),color= "black",width = 0.5) +
  xlab("Total Number of Occurrence Records")+
  ylab("Sites")+
  theme_few()+
  guides(fill = guide_legend("Size (cm)"))+
  scale_fill_viridis(option="C")

p2 <- ggplot(fish.occ1) +
  geom_col(aes(abundance,site,fill=size),color= "black",width = 0.5) +
  xlab(expression(Total~Density~(individual~per~250*m^2)))+
  ylab("Sites")+
  theme_few()+
  guides(fill = guide_legend("Size (cm)"))+
  scale_fill_viridis(option="C")

p3 <- ggplot(fish.occ2) +
  geom_col(aes(unique,site,fill=size),color = "black",width=0.5) +
  facet_wrap(.~tax_seq,scales = "free_x")+
  theme_few()+
  xlab("Taxonomic Richness")+
  ylab("Sites")+
  guides(fill = guide_legend("Size (cm)"))+
  scale_fill_viridis(option="C")

png("./Plots/records_fish.png",width=12,height=12,units="in", res=600)
g <- plot_grid(p1,p2,labels = "AUTO")
g1 <- plot_grid(g,p3,labels = c("","C"),nrow=2,rel_heights = c(0.33,0.66)) 
g1
graphics.off()