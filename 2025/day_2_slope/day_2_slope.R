# day 2: slope
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(haven)
library(scico)
library(geomtextpath)

library(extrafont)
loadfonts()

# so unfortunately 2015 and 2010 don't have parseable data files...
# but 2005 does!
# reading in Main files for both
tu_2022 <- read_sas('../time_use_survey/TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 
tu_2005 <- read_sas('../time_use_survey/c19_2005/Data Files SAS/C19PUMFM_NUM.sas7bdat')

# time alone in 2022
alone_2022 <- tu_2022 |> 
  select(AGEGR10, DURS01) |>
  mutate(year = 2022)

# time alone in 2005
alone_2005 <- tu_2005 |>
  select(AGEGR10, DURSOC01) |>
  mutate(year = 2005) |>
  rename(DURS01 = DURSOC01)

# combine, then recode age groups
alone_all <- rbind(alone_2022, alone_2005)

alone_all$age_group <- recode(
  alone_all$AGEGR10,
  `1` = "15 to 24 years",
  `2` = "25 to 34 years",
  `3` = "35 to 44 years",
  `4` = "45 to 54 years",
  `5` = "55 to 64 years",
  `6` = "65 to 74 years",
  `7` = "75 years and over"
)

alone_all$alone_h <- alone_all$DURS01 / 60

# summarize
alone_sum <- alone_all |>
  group_by(year, age_group) |>
  summarize(mean_alone = mean(alone_h))

df_ratio <- alone_sum |>
  pivot_wider(names_from = year, values_from = mean_alone) |>
  mutate(ratio = `2022` / `2005`) |>
  select(age_group, `2022`, ratio)

col_pal <- rev(scico(7, palette = 'roma'))

# now plot
plot1 <- ggplot() +
  geom_textpath(data = alone_sum,
                aes(x = year, y = mean_alone, group = age_group, color = age_group,
                    label = age_group),
    size  = 6, spacing = 0, family = 'Liberation Mono') +
  geom_point(data = alone_sum,
             aes(x = year, y = mean_alone, group = age_group, color = age_group),
             size = 3) +
  geom_text_repel(data = df_ratio,
            aes(x = 2022.5, y = `2022`, label = paste0(round(ratio,1),'x'),
                color = age_group),
            hjust = 0, direction = 'y', nudge_y = 0.1, segment.color = NA,
            size = 5, family = 'Liberation Mono') +
  scale_x_continuous(breaks = c(2005, 2022)) +
  scale_color_manual(values = col_pal) +
  labs(title = 'alone',
       y = 'Hours spent alone in a 24h period',
       subtitle = 'Everyone is spending more time alone, but especially young people.') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0, size = 12, family = 'Bahnschrift'),
        plot.title = element_text(size = 40, family = 'Bahnschrift'),
        axis.title.x = element_blank(),
        axis.title = element_text(size = 14, family = 'Bahnschrift'),
        axis.text = element_text(size = 16, family = 'Liberation Mono'),
        legend.position = 'none',
        panel.grid.minor = element_blank(),
        panel.grid = element_line(color = 'grey80'),
        plot.background = element_rect(fill = 'grey98', color = NA)) +
  guides(color = guide_legend(override.aes = list(label = NULL, size = 1.5)))

#ggsave('day_2_slope_alone_time.png', height = 8, width = 8, dpi = 300)


# take just the youngest age group
alone_young <- alone_all |>
  filter(age_group == "15 to 24 years")

alone_young$year <- as.factor(alone_young$year)

medians <- alone_young |>
  group_by(year) |>
  summarize(median_alone = median(alone_h))

col_pal_2 <- c('#7695d3','#00205f')
plot2 <- ggplot() +
  geom_density(data = alone_young, 
               aes(alone_h, fill = year, colour = year), 
               alpha = 0.1) +
  geom_vline(data = medians, 
             aes(xintercept = median_alone, colour = year), 
             linetype = "dashed", linewidth = 0.2) + 
  scale_fill_manual(values = col_pal_2) +
  scale_color_manual(values = col_pal_2) +
  scale_x_continuous(limits = c(0,24), breaks = c(0,4,8,12,16,20,24)) +
  labs(y = 'Proportion of responses',
       x = 'Self-reported hours spent alone by 15-24-yr-olds',
       subtitle = 'In 2005, most young people spent <5 hours per day alone. In 2022, most spent >10 hours per day alone.') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.subtitle = element_text(hjust = 0.5, size = 11, family = 'Bahnschrift'),
        axis.title = element_text(size = 12, family = 'Liberation Mono'),
        axis.text = element_text(size = 10, family = 'Liberation Mono'),
        legend.position = 'bottom',
        legend.text = element_text(size = 10, family = 'Liberation Mono'),
        panel.grid.minor = element_blank(),
        panel.grid = element_line(color = 'white'),
        plot.background = element_rect(fill = 'grey98', color = NA))

#ggsave('day_2_density_plots.png', height = 5, width = 8)

# combine
library(patchwork)

combined_plot <- plot1 / plot2 + plot_layout(heights = c(2, 1)) 
ggsave("day_2_slope_alone_time.png", combined_plot, width = 8, height = 12)
