### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-03-29
###############################################################################

### Load Libraries ###
library(tidyverse)
library(dplyr)
library(here)
library(tidytuesdayR)
library(scales)

###Load data ###
tuesdata <- tidytuesdayR::tt_load(2022, week = 13)
sports <- tuesdata$sports

### Data wrangling ###

# generate average per capita revenue for women, per state
revenue<-sports %>%
  filter(sportscode == 15) %>% filter(year == 2019) %>% 
  filter(grepl('California State University', institution_name)) %>% 
  mutate(exp_percapita_women = exp_women/ef_female_count) %>% 
  mutate(exp_percapita_men = exp_men/ef_male_count) %>%
  mutate(exp_percapita_both = exp_percapita_men+exp_percapita_women) %>%
  mutate(exp_percapita_propwomen = exp_percapita_women/exp_percapita_both) %>% 
  mutate(exp_percapita_propmen =exp_percapita_men/exp_percapita_both) %>%
  mutate(campus = substr(revenue$institution_name,29,45)) %>%
  pivot_longer(cols = exp_percapita_propwomen:exp_percapita_propmen,
               names_to = "exp_sex",
               values_to = "exp_percapita_prop")
  



# bar plot

revenue %>% drop_na(exp_percapita_prop) %>%
  filter(!campus %in% "Stanislaus") %>% 
  ggplot(aes(x=exp_sex, y=exp_percapita_prop, fill=exp_sex)) +
  geom_bar(stat="identity", position=position_dodge())+
  facet_wrap(~campus)+
  scale_x_discrete(labels = NULL, )+
  theme_bw()+
  labs( title = "Men get the best cut: Soccer investment in CSU",
        subtitle = "Investment in male ande female teams per campus during 2019",
        y ="Per-capita proportional expenditure",
        x = NULL,
        caption = "Data: Equity in Athletics Data Analysis, 2019")+
  scale_fill_discrete(name = "Team", labels = c("Male", "Female"))+
  theme(axis.title = element_text(size = 12),
        title = element_text(size = 14),
        strip.text = element_text(face="italic", size = 11),
        plot.caption = element_text(hjust = 1, face="italic", size = 8)
  )

#export plot
ggsave(here("2022-03-29", "Output","soccer.png"), width = 8, height = 5, units = "in")

