require(pacman)

p_load(
  tidytuesdayR,
  tidyverse,
  dplyr,
  lubridate,
  geofacet
)

tt <- tidytuesdayR::tt_load('2021-07-20')
drought <- tt$drought

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR") %>% 
  rename(state_abb = Abbreviation,
         state = State)
# From: https://github.com/AbdoulMa/TidyTuesday/blob/main/2021_w30/tidytuesday_2021_w30.R

drought_clean <- drought %>% 
  mutate(
    date = ymd(valid_end),
    week = week(valid_end),
    month = month(valid_end),
    year = year(valid_end),
    severity_measure = recode(drought_lvl, "None" = 0, "D0" = 1, "D1" = 2, "D2" = 3, "D3" = 4, "D4" = 5),
    severity_area = severity_measure * (area_pct/100),
    severity_pop = severity_measure * (pop_pct/100)
  ) %>% 
  left_join(states, by = c("state_abb")) %>% 
  select(-valid_start, -valid_end, -stat_fmt, -map_date)
  

drought_severity_weekly <- drought_clean %>% 
  group_by(state, date) %>% 
  summarise(week_severity_area = sum(severity_area),
            week_severity_pop = sum(severity_pop))

drought_occurance <- drought_clean %>% 
  group_by(state_abb, date, severity_measure) %>% 
  filter(severity_measure != 0, area_pct != 0) %>% 
  count() %>% 
  pivot_wider(names_from = "severity_measure", values_from = "n")

# drought_severity_monthly <- drought_clean %>% 
#   group_by(state, month) %>% 
#   summarise(month_severity_area = sum(severity_area),
#             month_severity_pop = sum(severity_pop),
#             date = first(date))


ggplot(drought_severity_weekly %>%
         filter(
           state != "Puerto Rico"
         )) +
  geom_bar(
    aes(x = date, y = week_severity_area),
    stat = "identity"
    ) +
  facet_geo(~ state)

# ggplot(drought_severity_monthly %>%
#          filter(
#            state != "Puerto Rico"
#          )) +
#   geom_bar(
#     aes(x = date, y = month_severity_area),
#     stat = "identity"
#   ) +
#   facet_geo(~ state)
