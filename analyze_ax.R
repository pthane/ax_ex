library(tidyverse)
library(janitor)
library(fs)

ax_data <- read_csv("joseph.csv")


# Calculate d' (sensitivity measure)
#
# Possibilities
#
#                  Response diff (yes)      Response Same (no)
# Stimuli: diff             HIT                     MISS
# Stimuli: same             FA                CORRECT REJECT
#
# d' == z(H) - z(F) (normalized hit rate - normalized FA rate)
#
# More on dprime here: 
# http://phonetics.linguistics.ucla.edu/facilities/statistics/dprime.htm

ax_data %>% 
  separate(stimulus, into = c("item1", "item2"), sep = 1) %>% 
  mutate(eval = case_when(
    item1 == item2 & response == "same" ~ "cr", 
    item1 != item2 & response == "same" ~ "miss", 
    item1 == item2 & response == "diff" ~ "fa", 
    item1 != item2 & response == "diff" ~ "hit"
  )) %>% 
  tabyl(eval) %>% 
  select(-n) %>% 
  pivot_wider(names_from = eval, values_from = percent) %>% 
  mutate(z_h = qnorm(hit), 
         z_fa = qnorm(fa), 
         d_prime = z_h - z_fa)








# We can also look at specific continuum pairs if we have more data
# but this requires some cleaning...

more_data <- dir_ls(regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source")

ax_clean <- more_data %>% 
  separate(stimulus, into = c("item1", "item2"), sep = 1, remove = F) %>% 
  mutate(step_diff = case_when(
    stimulus %in% c(12, 23, 34, 45, 56, 67, 76, 65, 54, 43, 32, 21) ~ "one", 
    stimulus %in% c(13, 24, 35, 46, 57, 75, 64, 53, 42, 31) ~ "two", 
    TRUE ~ "ignore"
  ), 
         step_n = case_when(
    step_diff == "one" & stimulus %in% c(12, 21) ~ 1, 
    step_diff == "one" & stimulus %in% c(23, 32) ~ 2, 
    step_diff == "one" & stimulus %in% c(34, 43) ~ 3, 
    step_diff == "one" & stimulus %in% c(45, 54) ~ 4, 
    step_diff == "one" & stimulus %in% c(56, 65) ~ 5, 
    step_diff == "one" & stimulus %in% c(67, 76) ~ 6, 
    step_diff == "two" & stimulus %in% c(13, 31) ~ 1, 
    step_diff == "two" & stimulus %in% c(24, 42) ~ 2, 
    step_diff == "two" & stimulus %in% c(35, 53) ~ 3, 
    step_diff == "two" & stimulus %in% c(46, 64) ~ 4, 
    step_diff == "two" & stimulus %in% c(57, 75) ~ 5, 
    TRUE ~ 99
         ), 
        eval = case_when(
    item1 == item2 & response == "same" ~ 1, 
    item1 != item2 & response == "same" ~ 0, 
    item1 == item2 & response == "diff" ~ 0, 
    item1 != item2 & response == "diff" ~ 1))

ax_clean %>% 
  filter(step_diff != "ignore") %>% 
  ggplot(., aes(x = step_n, y = eval)) + 
    facet_grid(. ~ step_diff, scales = "free_x") + 
    geom_line() + 
    stat_summary(fun.data = mean_cl_boot, geom = "pointrange", pch = 21, 
                 fill = "white") + 
    labs(y = "Prop. correct") + 
    theme_bw()
