require("pacman")

p_load(
  tidyverse,
  janitor,
  lubridate,
  ggalluvial
)

scoobydoo <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-13/scoobydoo.csv', na = "NULL")

tidyscooby <- scoobydoo %>% 
  # remove non TV series occurrences
  filter(series_name != "Laff-a-Lympics", 
         season %in% c(1, 2, 3, 4)) %>%
  mutate(date_aired = ymd(date_aired),
         monster_type = str_trim(monster_type),
         monster_culpret_genders_match = ifelse(monster_gender == culprit_gender, 1, 0)) %>% 
  # remove what we are not interested in/what is not clearly defined
  select(-contains(c("other", "not", "va", "snack", "if_it_", 
                     "and_that", "country_state", "format", 
                     "series_name", "network", "season", "trap_work", 
                     "door_gag", "scrappy", "dum", "hex_", "blue_falcon",
                     "culprit_name")))

binary_truefalse <- function(vector){
  return(vector = as.integer(vector))
}

# What were the motives and monster types? 

ggplot(
  tidyscooby %>% drop_na(motive) %>% group_by(motive) %>% summarise(count = n()) %>% filter(count > 9),
  aes(reorder(motive, count), count)
) +
  geom_col() +
  coord_flip()

ggplot(
  tidyscooby %>% drop_na(monster_type) %>% group_by(monster_type) %>% summarise(count = n()) %>% filter(count > 9),
  aes(reorder(monster_type, count), count)
) +
  geom_col() +
  coord_flip()

# How do ratings change with motives and monster types?

ggplot(
  tidyscooby %>% drop_na(motive, imdb) %>% group_by(motive) %>% summarise(imdb = mean(imdb)),
  aes(motive, imdb)
) +
  geom_col() +
  coord_flip()


# Motives and characters

tidyscooby_actions <- tidyscooby %>% 
  select(
    index, title, motive,
    starts_with("caught"),
    starts_with("captured"),
    starts_with("unmask")
  ) %>% 
  pivot_longer(cols = -c("index", "title", "motive"), names_to = c(".value", "character"), names_sep = "_") %>% 
  mutate(
    caught = binary_truefalse(caught),
    captured = binary_truefalse(captured),
    unmask = binary_truefalse(unmask)
  ) %>% 
  rename(caught_by = caught) %>% 
  drop_na(motive)

# tidyscooby_actions_count <- tidyscooby_actions %>% 
#   group_by(motive) %>% summarise(count = n())

tidyscooby_actions_caught <- tidyscooby_actions%>% select(-title, -index, -captured, -unmask) %>% 
  drop_na() %>% 
  group_by(character, motive) %>%
  summarise(
    caught_by = sum(caught_by)
  )

# tidyscooby_actions_caught_unmasked <- tidyscooby_actions%>% select(-title, -index, -captured) %>% 
#   drop_na() %>% 
#   group_by(character, motive) %>%
#   summarise(
#     caught_by = sum(caught_by),
#     unmasked = sum(unmask)
#   )
# 
# tidyscooby_actions_captured <- tidyscooby_actions %>% select(-title, -caught_by, -index, -unmask) %>% 
#   drop_na() %>% 
#   group_by(character) %>%
#   summarise(
#     got_captured = sum(captured)
#   )

ggplot(
  tidyscooby_actions_caught,
  aes(axis1 = character,
      axis2 = motive,
      y = caught_by)
) +
  geom_alluvium(aes(fill = character)) +
  geom_stratum(width = 1/20) +
  geom_text_repel(stat = "stratum", 
                   aes(label = after_stat(stratum)),
                   max.overlaps = 20,
                   force = 0.5,
                   nudge_x = 0.15,
                   direction = "y",
                   hjust = 0) +
  scale_x_discrete(limits = c("Character", "Motive")) +
  theme(
    legend.position = "none",
    panel.background = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
