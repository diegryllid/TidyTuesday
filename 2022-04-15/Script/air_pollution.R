### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-04-08
###############################################################################

### Load Libraries ###
library(tidyverse)
library(ggrepel)
library(dplyr)
library(here)
library(tidytuesdayR)

### Load data ###
tuesdata <- tidytuesdayR::tt_load(2022, week = 15)
indoor_pollution <- tuesdata$indoor_pollution

### Data wrangling ###

top_latin_GDP <- c("Colombia", "Mexico", "Brazil", "Argentina","Canada", "United States")

latinplot <- indoor_pollution %>% 
  filter(Entity %in% top_latin_GDP) %>% 
  rename(deaths = "Deaths - Cause: All causes - Risk: Household air pollution from solid fuels - Sex: Both - Age: Age-standardized (Percent)") %>% 
  mutate(Label = ifelse(Year == 1990, Entity, NA))
### Time series plot ###
latinplot %>% ggplot(aes(y=deaths, x=Year)) + 
  geom_line(aes(color = Entity), size = 1) +
  theme_minimal()+
  labs(title = "Decline in deaths by indoor air pollution in the Americas",
       subtitle = "Latin American are catching up to Anglo American countries (Top 6 GDP's).",
       caption = "Source data: OurWorldInData.org", x = "Year", y = "Deaths (%)")+
  scale_x_continuous(expand = expansion(mult = c(.23, .05))) +
  geom_label_repel(aes(label = Label, color =Entity),
                   nudge_x = (-6),
                   hjust = "left",
                   direction = "y",
                   size = 3) +
  theme(legend.position = "none",
        panel.border = element_rect(linetype = "solid", fill = NA, color = "grey")
  )

ggsave(here("2022-04-15", "Output","timeseries.png"), width = 6, height = 5, units = "in")

