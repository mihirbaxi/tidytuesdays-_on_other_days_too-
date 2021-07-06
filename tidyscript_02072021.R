library(janitor)
library(tidyverse)
library(lubridate)
library(geofacet)
library(zoo)

animal_rescues_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')
  
animal_rescues <- animal_rescues_raw %>% 
  clean_names() 

domestic_animal_rescues <- animal_rescues[str_detect(animal_rescues$special_service_type, "Domestic"), ] %>% 
  filter(cal_year %in% c("2016", "2017", "2018", "2019", "2020")) %>% 
  mutate(animal_group_parent = str_to_sentence(animal_group_parent)) %>% 
  separate(date_time_of_call, c("date", "time"), sep = " (?=[^ ]+$)") %>% 
  mutate(
    date = dmy(date),
    month = month(date),
    year = year(date),
    animal = if_else(animal_group_parent %in% c("Cat"), "Cat", if_else(animal_group_parent %in% c("Dog"), "Dog", "Other"))
  )


ggplot(domestic_animal_rescues) +
  geom_bar(aes(x = cal_year, fill = animal), position = "dodge") +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey"),
    axis.title = element_blank()
  )


domestic_animal_rescues_count <- domestic_animal_rescues %>% 
  group_by(year, month, animal) %>% 
  count(animal, month, year) %>% 
  mutate(month_year = month + "_" + year)

borough_names <- gb_london_boroughs_grid %>% 
  select(borough_code = code_ons, name)

rescues_borough <- domestic_animal_rescues %>% 
  filter(cal_year < 2021) %>% 
  count(cal_year, borough_code, animal) %>% 
  pivot_wider(names_from = animal, values_from = n) %>% 
  left_join(borough_names) %>% 
  filter(!is.na(name)) 


ggplot(domestic_animal_rescues %>% filter(year %in% c("2019", "2020"))) +
  geom_bar(aes(x = month, fill = count), position = "dodge") +
  theme(
    legend.position = "bottom",
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "grey"),
    axis.title = element_blank()
  )
