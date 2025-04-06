# day 5: ranking
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(forcats)
library(showtext)

font_add_google(name = 'Work Sans', family = 'work')
showtext_auto()

tu_2022 <- read_sas('../time_use_survey/TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 

# i had an initial look and decided not to include all the social categories
# some had tiny amounts/were hard to interpret
dat <- tu_2022 |>
  select(AGEGR10,
         DURS02, DURS03, DURS05, DURS07, DURS08, DURS09)

dat$age_group <- recode(
  dat$AGEGR10,
  `1` = "15-24 years",
  `2` = "25-34 years",
  `3` = "35-44 years",
  `4` = "45-54 years",
  `5` = "55-64 years",
  `6` = "65-74 years",
  `7` = "75 years+"
)

dat_long <- dat |>
  pivot_longer(
    cols = starts_with("DURS"),
    names_to = "duration_code",
    values_to = "duration"
  ) |>
  mutate(dur_h = duration / 60)

dat_long$duration_code <- recode(
  dat_long$duration_code,
  "DURS02" = "With spouse, partner",
  "DURS03" = "With children <15 in household",
  "DURS04" = "With children >15 in household",
  "DURS05" = "With parents or parents-in-law",
  "DURS06" = "With other adults in household",
  "DURS07" = "With family from other households",
  "DURS08" = "With friends",
  "DURS09" = "With colleagues or classmates",
  "DURS1099" = "With other people or not stated"
)


dat_sum_age <- dat_long |>
  group_by(duration_code, age_group) |>
  summarize(avg_h = mean(dur_h))

dat_sum_age <- dat_sum_age |>
  group_by(age_group) |>
  mutate(duration_code_unique = paste(age_group, duration_code, sep = "_"),
         duration_code_facet = fct_reorder(duration_code_unique, avg_h, .desc = TRUE)) |>
  ungroup()

# picked from https://tsitsul.in/blog/coloropt/
col_pal <- c('#ebac23',
             '#e1562c',
             '#ff9287',
             '#b80058',
             '#00bbad',
             '#5954d6')
ggplot() +
  geom_segment(data = dat_sum_age, 
           aes(x = duration_code_facet, 
               y = 0, yend = avg_h, color = duration_code)) +
  geom_point(data = dat_sum_age, 
               aes(x = duration_code_facet, 
                   y = avg_h, color = duration_code),
             size = 4) +
  scale_color_manual(values = col_pal) +
  facet_grid(~age_group, scales = "free_x") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 30),
    axis.title = element_text(size = 30),
    legend.text = element_text(size = 30),
    strip.text = element_text(size = 30),
    legend.title = element_blank(),
    plot.background = element_rect(fill = '#eeeeee', color = NA),
    plot.title = element_text(size = 50),
    text = element_text(family = 'work')) +
  labs(y = "Average number of hours in a 24h period",
       title = "Who do Canadians spend time with?")

ggsave('day_5_ranking.png', width = 8, height = 6, dpi = 300)
