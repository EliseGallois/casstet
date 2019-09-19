rm(list=ls(all=TRUE))

#set working directory
setwd("/Volumes/Seagate Backup Plus Drive")

#download necessary packages
library(graphics)
library(stats)
library(utils)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(esquisse)
library(rworldmap)
library(maps)
library(PlotSvalbard)
library(sp)
library(rgdal)
library(basemap)
library(wesanderson)


#upload data 
map<-read.csv("map.csv", header=T)


new <- transform_coord(x = map, lon = NULL, lat = NULL,
                new.names = NULL,
                proj.og = "+proj=longlat",
               
               map.type = "panarctic", verbose = FALSE,
                bind = FALSE)

#generate basemap
basemap("panarctic",limits = c(3*10^6, -3*10^6, 3*10^6, -3*10^6)) + 
  geom_point(data = map, aes(x = lon, y = lat, color = Site) , size = 5) + 
  scale_color_brewer(palette="Paired")










