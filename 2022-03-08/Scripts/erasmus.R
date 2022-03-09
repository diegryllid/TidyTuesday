### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-03-08
###############################################################################

### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(maps)
library(mapdata)
library(mapproj)
library(countrycode)
library(lubridate)
library(RColorBrewer)

### Load data###
tuesdata <- tidytuesdayR::tt_load('2022-03-08')
erasmus <- tuesdata$erasmus

###Data wrangling###

#Fix country codes, generate country and continent names, filter by continent and year
europe <- erasmus %>%
  mutate(sending_country = case_when(
    sending_country_code == "EL" ~ "GR",
    sending_country_code == "UK" ~ "GB",
    sending_country_code == "XK" ~ "RS",
    TRUE ~ sending_country_code)) %>%  
  mutate(continent = countrycode(sending_country_code,
                                 origin = 'iso2c',
                                 destination = 'continent')) %>% 
  filter(continent == "Europe") %>% 
  mutate(sending_country = countrycode(sending_country,
                                       origin = 'iso2c',
                                       destination = 'country.name')) %>% 
  mutate(year = year(ym(mobility_start_month))) %>% 
  filter(year == 2019)

#Summarize for total
europe_total <-europe %>% 
  group_by(sending_country) %>% 
  summarise(sum_par = sum(participants, na.rm = TRUE))
  
#Summarize by gender
  europe_gender <-europe %>% filter(participant_gender== "Female") %>% 
  group_by(sending_country, participant_gender) %>% 
  summarise(participants = sum(participants, na.rm = TRUE))
  
#Unify dataframes and calculate proportion of females

  europe_gender$total<-europe_total$sum_par
  europe_gender$proportion<-europe_gender$participants/europe_gender$total
  

map_data <- europe_gender %>% rename(region = sending_country) %>% inner_join(world)
  
europe_countries<-as.character(as.vector(distinct(europe_gender,sending_country)))
  
###map polygons###

world <- map_data("world")
 
###plot### 
  ggplot()+
    geom_polygon(data = map_data, 
                 aes(x = long, 
                     y = lat, 
                     group = group,
                     fill = proportion),  
                 color = "black")+
    coord_map(xlim = c(-24,50), ylim = c(30,70.5))+
    labs(title = "European females engaged in exchange programs",
         subtitle = "Females in Erasmus programs by sending country, 2019", caption ="Source: data.europa.eu"
    )+
    theme_void()+
    theme(legend.position = c(0.1, 0.4))+
    scale_fill_viridis_b(name = "Proportion")
  
  #export
  ggsave(here("2022-03-08", "Output","erasmus.png"), width = 6, height = 5, units = "in")
