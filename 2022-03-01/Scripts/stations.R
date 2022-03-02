### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-02-22
###############################################################################

### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)

### Load data ###

tuesdata <- tidytuesdayR::tt_load('2022-03-01')
tuesdata <- tidytuesdayR::tt_load(2022, week = 9)

# filter the most populous cities in the USA
stations <- tuesdata$stations
cities_stations<- stations %>% 
  filter(CITY %in%  c("Los Angeles", "Chicago", "Houston", "New York")) %>% 
  group_by(CITY, FUEL_TYPE_CODE) %>% #create counts for each group
  summarise(number_cases = n()) %>% 
  mutate(total_cases = sum(number_cases),
         proportion = number_cases/total_cases) # Create total counts
  


# barplot
cities_stations %>%  ggplot(aes(x=FUEL_TYPE_CODE, y=CITY)) + 
  geom_count(aes(size=1), colour="lightgrey") +
  geom_count(aes(size=proportion, group=CITY), colour="cornflowerblue")  +
  scale_size(range = c(0,10), breaks=seq(0,1,by=0.2), name = "Proportion") +
  coord_fixed() +
  labs( title = "Proportion of alternative stations fuel types offer in USA's biggest cities",
        x ="Fuel type",
        y = "City",
        caption = "Source: Alterntaive Fuels Data Center")+
  scale_x_discrete(labels=c("BD" = "Biodiesel", "CNG" = "Comp. Natural Gas",
                            "ELEC" = "Electric", "E85" = "Ethanol",
                            "HY" = "Hydrogen", "LNG" = "Liq. Natural Gas",
                            "LPG" = "Propane"))+
  theme_minimal()

ggsave(here("2022-03-01","Output","stations.png"), width = 12, height = 6, units = "in")
