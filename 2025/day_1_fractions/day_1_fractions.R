# day 1: fractions
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(showtext)
library(haven)

font_add_google(name = 'DM Sans', family = 'dm')
showtext_auto()

tu_2022 <- read_sas('../TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 

dat <- tu_2022 |>
  dplyr::select(AGEGR10, DUR101) |>
  filter(!DUR101 %in% c(9996, 9997, 9998)) # remove skipped answers

dat$age_group <- recode(
  dat$AGEGR10,
  `1` = "15 to 24 years",
  `2` = "25 to 34 years",
  `3` = "35 to 44 years",
  `4` = "45 to 54 years",
  `5` = "55 to 64 years",
  `6` = "65 to 74 years",
  `7` = "75 years and over"
)

dat$sleep_h <- dat$DUR101 / 60

dat_sum <- dat |>
  group_by(age_group) |>
  summarize(mean_sleep = mean(DUR101),
            mean_h = mean(sleep_h))

proportion_less_than_8 <- dat |>
  group_by(age_group) |>
  dplyr::summarise(
    total_count = n(),
    less_than_8_count = sum(sleep_h < 8),  
    proportion = less_than_8_count / total_count
  ) |>
  mutate(percentage = proportion * 100)

ggplot() +
  geom_jitter(data = dat, 
              aes(x = age_group, y = sleep_h, color = sleep_h < 8),
              alpha = 0.2) +
  geom_hline(yintercept = 8, linetype = "dashed") +
  geom_text(data = proportion_less_than_8,
            aes(x = age_group, y = -0.7, label = paste0(round(percentage,1),'%')),
            family = 'dm', size = 14, color = 'black') +
  scale_color_manual(values = c("TRUE" = "#E94F37", "FALSE" = "grey40")) +
  labs(y = "Self-reported hours of sleep",
       title = "Sleepless in Saskatoon",
       subtitle = "How many Canadians are getting less than 8h of sleep per night?",
       caption = "\n Source: Canadian Time Use Survey") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        legend.position = 'none',
        plot.title = element_text(size = 80),
        plot.subtitle = element_text(size = 40),
        plot.caption = element_text(size = 30),
        axis.text = element_text(size = 30, family = 'roboto'),
        axis.title = element_text(size = 40),
        text = element_text(family = 'dm'),
        plot.background = element_rect(fill = 'grey94', color = NA))

ggsave('day_1_fractions.png', height = 8, width = 10, dpi = 300)
