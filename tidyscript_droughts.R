require(pacman)

p_load(
  tidytuesdayR,
  tidyverse,
  dplyr,
  lubridate,
  geofacet,
  maps,
  ggmap
)


# Load data ---------------------------------------------------------------

tt <- tidytuesdayR::tt_load('2021-07-20')
drought <- tt$drought

states <- readr::read_csv("https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv") %>% 
  tibble::add_row(State = "Puerto Rico", Abbreviation = "PR") %>% 
  rename(state_abb = Abbreviation,
         state = State)
# From: https://github.com/AbdoulMa/TidyTuesday/blob/main/2021_w30/tidytuesday_2021_w30.R


# Initial cleaning --------------------------------------------------------

drought_severity_weekly <- drought %>% 
  mutate(
    date = ymd(valid_start),
    week = week(valid_start),
    month = month(valid_start),
    year = year(valid_start)
    #impact_percap = pop_total/area_total
  ) %>%
  left_join(states, by = c("state_abb")) %>% 
  select(-valid_start, -valid_end, -stat_fmt, -map_date) %>%
  mutate(
    severity_measure = recode(drought_lvl, "None" = 0, "D0" = 1, "D1" = 2, "D2" = 3, "D3" = 4, "D4" = 5),
    severity_area = severity_measure * (area_pct/100),
    severity_pop = severity_measure * (pop_pct/100)
  ) %>% 
  group_by(state, date) %>% 
  summarise(week_severity_area = sum(severity_area),
            week_severity_pop = sum(severity_pop))
# Severity calculation motivated by: https://github.com/efranke22/tidytuesday/blob/main/july2021/tidytuesday_week30.Rmd



# Severity across years ---------------------------------------------------


ggplot(drought_severity_weekly %>%
         filter(
           state != "Puerto Rico"
         )) +
  geom_bar(
    aes(x = date, y = week_severity_area),
    stat = "identity",
    colour = "#453426",
    fill = "#453426"
  ) +
  facet_geo(~ state) +
  labs(
    title = "Severity of droughts in US states 2001-2021"
  ) +
  ylab("Severity = Classification * % of area impacted") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 90),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "lightgrey")
  ) 


# Severity in 2021 --------------------------------------------------------------------

drought_severity_weekly_2021 <- drought_severity_weekly %>% 
  filter(year(date) %in% c("2021"))


ggplot(drought_severity_weekly_2021 %>%
         filter(
           state != "Puerto Rico"
         )) +
  geom_bar(
    aes(x = date, y = week_severity_area),
    stat = "identity",
    colour = "#b19179",
    fill = "#b19179"
  ) +
  facet_geo(~state) +
  labs(
    title = "Severity of droughts in US states (2021)"
  ) +
  ylab("Severity = Classification * % of area impacted") +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "lightgrey")
  ) 



# The most severe ---------------------------------------------------------

drought_clean_severe <- drought %>% 
  mutate(
    date = ymd(valid_start),
    week = week(valid_start),
    month = month(valid_start),
    year = year(valid_start)
    #impact_percap = pop_total/area_total
  ) %>%
  left_join(states, by = c("state_abb")) %>% 
  select(-valid_start, -valid_end, -stat_fmt, -map_date) %>% 
  filter(drought_lvl %in% c("D3")) %>% 
  group_by(state, year, month) %>% 
  summarise(
    date = first(date),
    area_pct = mean(area_pct),
    pop_pct = mean(pop_pct)
  )

ggplot(drought_clean_severe  %>%
         filter(
           state != c("Puerto Rico"),
           year != 2001
         )) +
  geom_line(
    aes(x = month, y = area_pct, colour = factor(year))
  ) +
  facet_geo(~state) +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "lightgrey")
  ) 


ggplot(drought_clean_severe %>%
         filter(
           state %in% c("Washington", "Oregon", "California",
                        "Idaho", "Nevada", "Utah", "Arizona",
                        "Montana", "Wyoming", "Colorado", "New Mexico"),
           year > 2010
         )) +
  geom_line(
    aes(x = month, y = area_pct, colour = factor(year))
  ) +
  facet_wrap(~state) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 12)) +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(colour = "lightgrey")
  ) 

