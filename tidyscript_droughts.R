require(pacman)

p_load(
  tidytuesdayR,
  tidyverse,
  dplyr,
  lubridate,
  geofacet
)


# Load data ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load('2021-07-20')
drought <- tt$drought

drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')

states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR") %>% 
  rename(state_abb = Abbreviation,
         state = State)
# From: https://github.com/AbdoulMa/TidyTuesday/blob/main/2021_w30/tidytuesday_2021_w30.R


# Initial cleaning --------------------------------------------------------

drought_clean <- drought %>% 
  mutate(
    date = ymd(valid_end),
    week = week(valid_end),
    month = month(valid_end),
    year = year(valid_end),
    severity_measure = recode(drought_lvl, "None" = 0, "D0" = 1, "D1" = 2, "D2" = 3, "D3" = 4, "D4" = 5)
    #impact_percap = pop_total/area_total
  ) %>%
  left_join(states, by = c("state_abb")) %>% 
  select(-valid_start, -valid_end, -stat_fmt, -map_date)



# Occurances --------------------------------------------------------------

drought_occurance <- drought_clean %>%
  filter(area_pct != c(0, 0.00)) %>% 
  group_by(year, month, state, drought_lvl) %>%
  count()

ggplot(drought_occurance %>% filter(drought_lvl != c("None"))) +
  geom_bar(
    aes(
      x = year,
      y = n,
      fill = drought_lvl
    ),
    stat = "identity"
  ) +
  facet_geo(~ state)
# 
# drought_occurance_overall <- drought_clean %>%
#   filter(area_pct != c(0, 0.00)) %>% 
#   group_by(year, month, drought_lvl) %>%
#   count()

ggplot(drought_occurance) +
  geom_bar(
    aes(
      x = year,
      y = n,
      fill = drought_lvl,
      colour = drought_lvl
    ),
    stat = "identity"
  )

# Severity calculations and chart  ----------------------------------------------------------------

drought_severity_weekly <- drought_clean %>%
  mutate(
    severity_area = severity_measure * (area_pct/100),
    severity_pop = severity_measure * (pop_pct/100)
  ) %>% 
  group_by(state, date) %>% 
  summarise(week_severity_area = sum(severity_area),
            week_severity_pop = sum(severity_pop))
# Severity calculation motivated by: https://github.com/efranke22/tidytuesday/blob/main/july2021/tidytuesday_week30.Rmd

ggplot(drought_severity_weekly %>%
         filter(
           state != "Puerto Rico"
         )) +
  geom_bar(
    aes(x = date, y = week_severity_area),
    stat = "identity"
  ) +
  facet_geo(~ state) +
  labs(
    title = "Severity of droughts in US states 2001-2021"
  ) +
  ylab("Severity = Classification * % of area impacted") +
  theme(
    axis.title.x = element_blank(),
  ) 


# drought_occurance <- drought_clean %>% 
#   group_by(state_abb, date, severity_measure) %>% 
#   filter(severity_measure != 0, area_pct != 0) %>% 
#   count() %>% 
#   pivot_wider(names_from = "severity_measure", values_from = "n")

# drought_severity_monthly <- drought_clean %>% 
#   group_by(state, month) %>% 
#   summarise(month_severity_area = sum(severity_area),
#             month_severity_pop = sum(severity_pop),
#             date = first(date))

# ggplot(drought_severity_monthly %>%
#          filter(
#            state != "Puerto Rico"
#          )) +
#   geom_bar(
#     aes(x = date, y = month_severity_area),
#     stat = "identity"
#   ) +
#   facet_geo(~ state)