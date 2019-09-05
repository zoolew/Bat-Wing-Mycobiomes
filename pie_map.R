library("ggplot2")
library("ggmap")
library("dplyr")
library("scatterpie")
library("ggsn")
library("devtools")

devtools::install_github('oswaldosantos/ggsn') 

sites<-read.csv("KV11map.csv",header=T)

## Pie colours

library(RColorBrewer)

display.brewer.all()
getPalette = colorRampPalette(brewer.pal(11, "RdBu"))

my.cols <- getPalette(10)

### USA MAP

all.states <- map_data("state")

sampled_states <- c("wisconsin",
                    "arkansas",
                    "pennsylvania",
                    "missouri",
                    "kentucky",
                    "new york",
                    "iowa",
                    "alabama",
                    'west virginia',
                    'oklahoma')

ggplot(all.states, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  geom_polygon(fill="grey75", colour = "black", data = filter(all.states, region %in% sampled_states)) +
  geom_scatterpie(data = sites, 
                  aes(long, lat, r = 0.6),
                  cols = c("Corynorhinus.rafinesquii",
                           "Corynorhinus.townsendii",
                           "Eptesicus.fuscus",
                           "Myotis.austroriparius",
                           "Myotis.grisescens",
                           "Myotis.leibi",
                           "Myotis.lucifugus",
                           "Myotis.septentrionalis",
                           "Myotis.sodalis",
                           "Perimyotis.subflavus")) + 
  scale_fill_manual(name = "Species",
                    breaks = c("Myotis.lucifugus",
                               "Myotis.septentrionalis",
                               "Perimyotis.subflavus",
                               "Eptesicus.fuscus",
                               "Myotis.leibi",
                               "Myotis.sodalis",
                               "Myotis.austroriparius",
                               "Myotis.grisescens",
                               "Corynorhinus.rafinesquii",
                               "Corynorhinus.townsendii"),
                    values = c("Corynorhinus.rafinesquii" = "#336633",
                               "Corynorhinus.townsendii" = "#33CC33",
                               "Eptesicus.fuscus" = "#053061",
                               "Myotis.austroriparius" = "grey90",
                               "Myotis.grisescens" = "grey70",
                               "Myotis.leibi" = "#246BAE",
                               "Myotis.lucifugus" = "#67001F",
                               "Myotis.septentrionalis" = "#B51F2E",
                               "Myotis.sodalis" = "#549EC9",
                               "Perimyotis.subflavus" = "#DC6F58")) + 
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_map()

## WISCONSIN

wisconsin_map <- subset(all.states, region == "wisconsin")

wisconsin <- subset(sites, State == "WI")

ggplot(wisconsin_map, aes(x=long, y=lat)) +
  geom_polygon(fill = "white", colour = "black") +
  geom_scatterpie(data = wisconsin, 
                  aes(long, lat, r = 0.2),
                  cols = c("Corynorhinus.rafinesquii",
                           "Corynorhinus.townsendii",
                           "Eptesicus.fuscus",
                           "Myotis.austroriparius",
                           "Myotis.grisescens",
                           "Myotis.leibi",
                           "Myotis.lucifugus",
                           "Myotis.septentrionalis",
                           "Myotis.sodalis",
                           "Perimyotis.subflavus")) + 
  scale_fill_manual(name = "Species",
                    breaks = c("Corynorhinus.rafinesquii",
                               "Corynorhinus.townsendii",
                               "Eptesicus.fuscus",
                               "Myotis.austroriparius",
                               "Myotis.grisescens",
                               "Myotis.leibi",
                               "Myotis.lucifugus",
                               "Myotis.septentrionalis",
                               "Myotis.sodalis",
                               "Perimyotis.subflavus"),
                    values = c("Corynorhinus.rafinesquii" = "#336633",
                               "Corynorhinus.townsendii" = "#33CC33",
                               "Eptesicus.fuscus" = "#053061",
                               "Myotis.austroriparius" = "grey90",
                               "Myotis.grisescens" = "grey70",
                               "Myotis.leibi" = "#246BAE",
                               "Myotis.lucifugus" = "#67001F",
                               "Myotis.septentrionalis" = "#B51F2E",
                               "Myotis.sodalis" = "#549EC9",
                               "Perimyotis.subflavus" = "#DC6F58")) +
  coord_map() +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())