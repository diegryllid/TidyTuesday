### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-04-08
###############################################################################

### Load Libraries ###
library(tidyverse)
library(dplyr)
library(here)
library(tidytuesdayR)
library(maps)
library(mapdata)
library(mapproj)

###Load data ###

tuesdata <- tidytuesdayR::tt_load(2022, week = 14)
news_orgs <- tuesdata$news_orgs

###Data wrangling###

#convert state abbreviations to names in lowercase
news_orgs$region<-tolower(state.name[match(news_orgs$state,state.abb)])

#calculate frequencies per tax status
state_news <-news_orgs %>% 
  count(region, tax_status_current) %>%
  group_by(region) %>%
  mutate(freq = n / sum(n)) %>% 
  filter(tax_status_current == "For Profit")
  
  
#map polygons
states<-map_data("state")

#join data and basemap
map_data <- state_news %>% right_join(states)

#plot map
ggplot()+
  geom_polygon(data = map_data, 
               aes(x = long, 
                   y = lat,
                   group = group,
                   fill = freq),  
               color = "black")+
  theme_void()+
  labs(title = "Moved by the money?",
       subtitle = "Proportion of for-profit Digital News Publications per state", caption ="Source: Project Oasis"
  )+
  scale_fill_viridis_c(name = "Proportion")+
  theme(legend.position = c(0.9, 0.3))

#export
ggsave(here("2022-04-09", "Output","for profit.png"), width = 6, height = 5, units = "in")
