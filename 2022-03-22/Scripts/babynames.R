### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-03-22
###############################################################################

#IMPORTANT: This time I based the script in the code by: https://stackoverflow.com/users/7514527/edward,
# who had very useful code for animated word clouds

### Load Libraries ###
library(tidyverse)
library(dplyr)
library(here)
library(tidytuesdayR)
library(ggwordcloud)
library(gganimate)

#Read data and assign to object

tuesdata <- tidytuesdayR::tt_load(2022, week = 12)
babynames <- tuesdata$babynames

#data warngling
baby <- babynames %>%
  filter(year %in% c(1997, 2017)) %>%
  group_by(name, sex, year) %>%
  summarise(n=sum(n)) %>%
  arrange(desc(n)) %>%
  group_by(year, sex) %>%
  top_n(n=5) %>%
ungroup() %>%
  select(name, sex)

babyyears <- babynames %>%
  inner_join(baby, by=c("name","sex")) %>%
  filter(year>=1997) %>%
  mutate(year=as.integer(year))

#static plots
gg <- babyyears %>%
  ggplot(aes(label = name, size=n, color = sex)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 30) +
  theme_minimal()+
  facet_wrap(~sex)+
  theme(strip.text = element_text(size = 0))

#transition through years
gg2 <- gg + transition_time(year) +
  labs(title = "20 years in baby names: Most common baby names",
       subtitle = 'Year: {frame_time}')+
  theme(title = element_text(size = 17))

#animate and save
animate(gg2, end_pause=5, duration = 40, fps = 2)
anim_save("baby_names_worldcloud.gif", path = here("2022-03-22", "Output"))
