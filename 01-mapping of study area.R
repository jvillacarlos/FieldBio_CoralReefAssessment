library(tidyverse)
library(maps)
library(ggthemes)
library(raster)
library(gridExtra)
library(grid)
library(ggspatial)
library(maptools)


shp <- read_csv("./Data/liloan_shapefile.csv" )
pts <- read_csv("./Data/darwin_core/event.csv") %>%
  dplyr::select(lon=decimalLongitude,lat=decimalLatitude,site=locationID,eventID)%>%
  mutate(Datasets = word(eventID,-1,sep=":"))%>%
  distinct()%>%
  dplyr::select(-eventID)

data(wrld_simpl)
phil <- subset(sf::st_as_sf(wrld_simpl), NAME == 'Philippines')

p <- ggplot() +
  geom_sf(data = phil,alpha=0.1,color="aliceblue") +
  coord_sf(xlim = c(123.9897,124.0125), ylim = c(10.3863,10.41)) +
  xlab("")+
  ylab("")+
  geom_polygon(data = shp, 
               aes(x = lon, y = lat),
               colour = "black", fill = "antiquewhite", size = .2)+
  geom_point(data = pts, 
             aes(x = lon, y = lat, shape = Datasets), size = 3, color = "red")+
  geom_text(data = pts, aes(x = lon, y = lat, label=site),size = 2.5,hjust=0, vjust=0)+
  annotate("text", label = "Silot Bay", x = 123.994, y = 10.392, size = 4,color="darkblue",fontface = "bold")+
  annotate("text", label = "Poblacion Reef", x = 124.003, y = 10.402, size = 4, angle = 285,color="darkblue",fontface = "bold")+
  annotate("text", label = "Kadurong Reef", x = 124.0089, y = 10.392, size = 4, angle = 315,color="darkblue",fontface = "bold")+
  scale_shape_manual(values=c(0,1,2,3,6))+
  theme_few()+
  theme(plot.background=element_blank(),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill="aliceblue"))+
  annotation_scale(pad_x = unit(3, "in"))+
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(3.25, "in"), pad_y = unit(0.3, "in"), style = north_arrow_fancy_orienteering)

  

philmap <- data.frame(map("world", "Philippines", plot = FALSE)[c("x", "y")])
study_area <- data.frame (xmin=123.8, xmax=124.1, ymin=10.35, ymax=10.55)
ptheme <- theme(
  panel.border = element_rect(colour = 'black', size = 0.5, linetype = 1),
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = 'white'),
  legend.key = element_blank()
)


q <- ggplot() +
  coord_fixed() +
  geom_polygon(data = philmap, aes(x, y), color = "black", fill = "antiquewhite", size = 0.2) +
  geom_rect(data = study_area, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), alpha = 0, colour = "red", size = 1, linetype = 1) +
  theme_few()+
  theme(
    axis.ticks = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  labs(x = '', y = '')+
  theme(plot.background=element_blank(),
        panel.background = element_rect(fill="aliceblue"))

png("./Plots/study_map.png",width=8,height=6,units="in", res=1200)
grid.newpage()
vpb_ <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)  # the larger map
vpa_ <- viewport(width = 0.4, height = 0.4, x = 0.23, y = 0.72)  # the inset in upper left
print(p, vp = vpb_)
print(q, vp = vpa_)
graphics.off()

