# day 10: multi-modal
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(ggridges)
library(showtext) 

font_add_google(name = "Bowlby One", family = "bowlby")
font_add_google(name = "Libre Franklin", family = "libre")
showtext_auto()

tu_2022 <- read_sas('../time_use_survey/TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 

dat <- tu_2022 |>
  select(AGEGR10,
         SCHLDUR, PDWKDUR, SLEEPDUR) 

# now do a bunch of recoding
dat$age_group <- recode(
  dat$AGEGR10,
  `1` = "15-24",
  `2` = "25-34",
  `3` = "35-44",
  `4` = "45-54",
  `5` = "55-64",
  `6` = "65-74",
  `7` = "75+"
)

dat_long <- dat |>
  pivot_longer(
    cols = ends_with("DUR"),
    names_to = "duration_code",
    values_to = "duration"
  ) |>
  mutate(dur_h = duration / 60)

dat_long$duration_code <- recode(
  dat_long$duration_code,
  "PDWKDUR" = "Paid work",
  "SCHLDUR" = "Education",
  "SLEEPDUR" = "Sleep"
)


# filter out anything above 15 to get rid of outliers
dat_long <- dat_long |>
  filter(dur_h < 15)

dat_sum <- dat_long |>
  group_by(age_group, duration_code) |>
  summarize(mean_time = mean(dur_h))

densities <- dat_long |>
  group_by(duration_code, age_group) |>
  summarise(
    mean = mean(dur_h),
    .groups = "drop"
  ) |>
  left_join(
    dat_long |>
      group_by(duration_code, age_group) |>
      do({
        dens <- density(.$dur_h, adjust = 5)
        data.frame(x = dens$x, y = dens$y)
      }),
    by = c("duration_code", "age_group")
  ) |>
  group_by(duration_code, age_group) |>
  filter(abs(x - mean) == min(abs(x - mean))) |>
  ungroup()

col_pal <- c(
  "#5E4FA2",
  "#7BA9B6",
  "#A7C957", 
  "#F6C244", 
  "#ED6A5A",
  "#D1495B", 
  "#9E2A2B"   
)
ggplot() + 
  geom_density(data = dat_long, 
               aes(dur_h, fill = age_group, color = age_group), 
               alpha = 0.05, adjust = 5, trim = FALSE,
               show.legend = FALSE) +
  geom_segment(data = densities,
               aes(x = mean, xend = mean, y = 0, yend = y, color = age_group)) +
  geom_point(data = dat_sum,
             aes(x = mean_time, y = 0, color = age_group),
             size = 3) +
  scale_color_manual(values = col_pal) +
  facet_wrap(~duration_code, nrow = 3) +
  labs(x = 'Duration (h)',
       y = 'Proportion of responses',
       title = 'The geometry of routine',
       subtitle = 'Distributions of time spent studying, working, and sleeping across age') +
  theme_minimal() +
  theme(legend.position = 'right',
        legend.title = element_blank(),
        plot.background = element_rect(fill = '#FFFFFC', color = NA),
        panel.grid.major = element_blank(),
        panel.grid = element_line(color = 'grey', linewidth = .2),
        text = element_text(family = 'libre', size = 40),
        strip.text = element_text(color = 'black', size = 42),
        axis.title = element_text(color = 'grey30'),
        plot.title = element_text(family = 'bowlby', size = 60, color = 'grey30'),
        panel.spacing = unit(1.2, "lines"))

ggsave('day_10_multimodal.png', height = 10, width = 8, dpi = 300)
