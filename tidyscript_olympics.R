pacman::p_load(tidytuesdayR, tidyverse, janitor, gghighlight)

tuesdata <- tidytuesdayR::tt_load(2021, week = 31)
olympics <- tuesdata$olympics %>% clean_names()
regions <- tuesdata$regions %>% clean_names()

#olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympics_clean <- olympics %>% 
  left_join(regions, by = "noc") %>% 
  mutate(
    gold = ifelse(medal %in% c("Gold"), 1, 0),
    silver = ifelse(medal %in% c("Silver"), 1, 0),
    bronze = ifelse(medal %in% c("Bronze"), 1, 0)
  ) %>% 
  filter(!is.na(region))


# Summer medals over time -------------------------------------------------

medals_overtime_summer <- olympics_clean %>% filter(season %in% c("Summer"), !is.na(medal)) %>% group_by(year, region) %>% count()

ggplot(medals_overtime_summer) +
  geom_line(
    aes(
      x = year,
      y = n,
      colour = region
      )
  ) +
  gghighlight(max(n) > 225,
              label_params = list(size = 7.5),
              unhighlighted_params = list(colour = "#e3e6e7")) +
  scale_colour_manual(values = c("#e9a846",  "#6396b7", "#a55b89", "#e36e07", "#714430")) +
  ylab("Gold medals in each summer Olympics") +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#8f9ba7")
  )


# Winter medals over time -------------------------------------------------

medals_overtime_winter <- olympics_clean %>% filter(season %in% c("Winter"), !is.na(medal)) %>% group_by(year, region) %>% count()

ggplot(medals_overtime_winter) +
  geom_line(
    aes(
      x = year,
      y = n,
      colour = region
    )
  ) +
  gghighlight(max(n) > 50,
              label_params = list(size = 7.5),
              unhighlighted_params = list(colour = "#e3e6e7")) +
  scale_colour_manual(values = c("#e9a846",  "#6396b7", "#a55b89", "#e36e07", "#714430", "#777777")) +
  ylab("Gold medals in each summer Olympics") +
  theme(
    axis.title.x = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#8f9ba7")
  )


# Most successful teams ---------------------------------------------------

total_medals <- olympics_clean %>% filter(!is.na(medal)) %>% group_by(region, medal) %>% 
  count() %>% filter(n>100)

ggplot(total_medals) +
  geom_bar(
    aes(
      x = reorder(region, n),
      y = n,
      fill = medal
      ),
    stat = "identity",
    position = "stack"
  ) +
  scale_fill_manual(values = c("#897158", "#e9a846", "#babcb2")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90),
    panel.background = element_blank(),
    panel.grid.major.y = element_line("#8f9ba7"),
    axis.ticks = element_blank()
  )
  
