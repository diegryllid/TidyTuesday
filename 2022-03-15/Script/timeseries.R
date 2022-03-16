### TidyTuesday Challenge
### Created by: Diego A. Gomez-Morales
### Created on: 2022-03-15
###############################################################################

### Load Libraries ###
library(tidyverse)
library(here)
library(tidytuesdayR)
library(lubridate)

#Read data and assign to object
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

#data wrangling
cran$version<-substr(cran$version,1,3)#shortens version to three characters

cranplot<-cran %>% mutate(datetime = readr::parse_datetime(date,
                                                        format = "%Y-%m-%d %H:%M:%S UTC")) %>% 
  mutate(datetime2 = readr::parse_datetime(date,
                                           format = "%a %b %d %H:%M:%S %Y")) %>% 
  mutate(datetime = coalesce(datetime, datetime2)) %>%
  drop_na(datetime) %>% mutate(year = year(datetime)) %>%
  mutate(month = month(datetime)) %>%
  unite("datetime",c("year","month"),sep = " ") %>% 
  mutate(datetime = ym(datetime)) %>% 
  mutate(version = as.numeric(version)) %>% drop_na(version) %>% 
  mutate(development = if_else(version >= 1, "release", "beta")) %>%
  group_by(datetime, development) %>% summarise(sum=length(version))


#timeseries plot
cranplot %>% ggplot(aes(y=sum, x=datetime)) + 
  geom_line(aes(color = development), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()+
  scale_x_date(limits = as.Date(c("2003-03-01", "2021-09-01")), date_breaks = "2 years", date_labels = "%Y")+
  labs(title = "Drop in CRAN package updates",
       subtitle = "Total updates per month for packages in development (ver. < 1.0) and releases (ver. > 1.0)",
       caption = "Source data: Robert Flight", x = NULL, y = "total updates")+
  geom_text(aes(label = "In dev.", x = as.Date("2013-03-01"), y = 150),
            color = "#00AFBB",
            size = 4)+
  geom_text(aes(label = c("Release"), x = as.Date("2014-01-01"), y = 435),
            color = "#E7B800",
            size = 4)+
  theme(legend.position = "none",
        panel.border = element_rect(linetype = "solid", fill = NA, color = "grey")
        )

ggsave(here("2022-03-15", "Output","timeseries.png"), width = 6, height = 5, units = "in")
