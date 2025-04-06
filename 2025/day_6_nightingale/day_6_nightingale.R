# day 6: florence nightingale
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(geomtextpath)
library(extrafont)
loadfonts()

tu_2022 <- read_sas('../time_use_survey/TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 

dat <- tu_2022 |>
  select(SURVMNTH,
         DUR232, DUR237, DUR238) 

dat_long <- dat |>
  pivot_longer(
    cols = starts_with("DUR"),
    names_to = "duration_code",
    values_to = "duration"
  ) |>
  mutate(dur_h = duration / 60)

dat_long$duration_code <- recode(
  dat_long$duration_code,
  "DUR232" = "Interior DIY",
  "DUR237" = "Exterior DIY",
  "DUR238" = "DIY construction"
)

dat_sum <- dat_long |>
  group_by(SURVMNTH, duration_code) |>
  summarize(mean_diy = mean(dur_h))

dat_sum$month_label <- stringr::str_to_upper(month.name[dat_sum$SURVMNTH])
dat_sum$month_label <- factor(
  dat_sum$month_label,
  levels = stringr::str_to_upper(month.name)
)

col_pal <- c('#FBB13C',
             '#FE6847',
             '#2176AE')
ggplot() +
  geom_col(data = dat_sum, 
           aes(x = month_label, y = mean_diy, fill = duration_code)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = col_pal) +
  labs(title = 'A Do-It-Yourself Diagram',
       subtitle = 'The biggest months for D.I.Y. projects are May, August, and September.') +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size = 8, color = 'black'),
        axis.title = element_blank(),
        legend.title = element_blank(),
        legend.position = 'bottom',
        panel.grid = element_blank(),
        legend.key.width =  unit(0.3, "cm"),
        text = element_text(family = 'Inconsolata'),
        plot.title = element_text(size = 20, face = 'bold'),
        plot.subtitle = element_text(family = 'Arial'),
        plot.background = element_rect(fill = '#D7EAD7', color = NA)) +
  coord_curvedpolar()

ggsave('day_6_florence_nightingale.png', width = 6, height = 7, dpi = 300)
