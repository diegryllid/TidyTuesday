### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-02-22
###############################################################################

### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)

### Load data ####

tuesdata <- tidytuesdayR::tt_load('2022-02-22')
freedom <- tuesdata$freedom

### calculate change in CI from 2010 to 2020

CI_ten_years <- freedom %>% 
  filter(Region_Name == "Americas") %>% 
  filter(year == 2010 | year == 2020) %>% 
  select(country, Region_Code, year, CL) %>% 
  pivot_wider(names_from = year, 
              values_from = CL) %>% 
  mutate(diff = `2010` - `2020`)

#replaces some names of countries
CI_ten_years$country <- gsub("United States of America", "USA", CI_ten_years$country)
CI_ten_years[35,"country"] <- "Venezuela"
CI_ten_years[6,"country"] <- "Bolivia"
CI_ten_years$region <- CI_ten_years$country

# countries boundaries
Americas <- as.vector(distinct(CI_ten_years, country))
Americas<-as.character(Americas$country)
world <- map_data("world", region = Americas)
join<-left_join(world,select(CI_ten_years, region, diff))

# plot map
ggplot(join, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = diff), color = "white")+
  labs( title = "The fall of civil liberties in America: 10 years later",
        y ="Latitude",
        x = "Longitude",
        caption = "Comparison of Civil Liberties scores between 2010 and 2020")+
scale_x_continuous(limits = c(-170,-20))+
   scale_fill_continuous(name = "Difference", type = "viridis")

ggsave(here("2022-02-22","civil_liberties_america.png"), width = 5, height = 6, units = "in")
