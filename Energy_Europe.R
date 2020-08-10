
library(tidytuesdayR)
library(tidyverse)
library(janitor)
library(scales)

theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load("2020-08-04")


energy_types <- tuesdata$energy_types

energy_types$country_name <- replace_na(energy_types$country_name,"United Kingdom")

# How cunsumption is evolving in Europe with top consumers

top_5_consumers <- energy_types %>% 
  pivot_longer(cols = starts_with("2"),
               names_to='year',
               values_to="GWh") %>% 
  mutate(year=as.integer(year),
         Europe_GWh=sum(GWh)) %>% 
  filter(!is.na(country_name)) %>% 
  group_by(country_name,year,Europe_GWh) %>% 
  summarise(GWh=sum(GWh)) %>% 
  ungroup() %>% 
  group_by(country_name) %>% 
  mutate(totalGWh=sum(GWh)) %>% 
  ungroup() %>% 
  top_n(40,totalGWh) %>% 
  rename(country=country_name) %>% 
  mutate(top5_GWh=sum(GWh),
         ratio=top5_GWh/Europe_GWh)




top_5_consumers %>% 
  ggplot(aes(year,GWh)) +
  geom_line()+
  facet_wrap(~country,scales="free_y")+
  expand_limits(y=0)+
  scale_y_continuous(labels=comma)


#What is the cunsumption by habitabt

