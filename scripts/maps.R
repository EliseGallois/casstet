

#### LOAD PACKAGES ####
library(tidyverse)
library(esquisse) 
library(rworldmap)
library(rgdal) 
library(raster) 
library(ggsn)
library(viridis)
library(ggalt)
library(maptools)
library(readr)
library(ggrepel)

#### 1 - Load data ####
sites <- read.csv("data/map.csv")
str(sites)



#### 2 - Plot data on map, colour by tundra type ####

# make polar map 
data("wrld_simpl", package = "maptools")                                                                            
(site_map <- ggplot() +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = "grey80", 
                 colour = "grey80", size = 0.5, alpha = 0.5) +
    coord_map("ortho", orientation = c(90, 0, 0)) +
    scale_y_continuous(breaks = seq(45, 90, by = 50), labels = NULL) +
    scale_x_continuous(breaks = NULL) +
    labs(x = NULL, y = NULL, legend = "Tundra Type") +
    theme(panel.background = element_blank(),
          panel.grid.major = element_line(colour = NA),
          axis.ticks = element_blank(),
          legend.title = element_blank()) +
    geom_point(data = sites,  
               aes(x = lon, y = lat), size = 2, position = "jitter", alpha = 0.7, col = "#EE7600") +
   # scale_color_manual(values = "#EE7600") +
    geom_label_repel(data = sites,
                     aes(x = lon, y = lat,
                         label = site),
                     # Setting the positions of the labels
                     box.padding = 1, size = 4, nudge_x = 1, nudge_y = 1))




ggsave(site_map, filename = "figs/map_sites.png",
       height = 8, width = 8)
