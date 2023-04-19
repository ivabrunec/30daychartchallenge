# day 18: eurostat.
# gender pay gap

library(dplyr)
library(ggplot2)
library(showtext)
library(ggtext)
library(eurostat)

font_add_google(name = 'Inconsolata', family = 'Inconsolata')
font_add_google(name = 'Roboto Condensed', family = 'Roboto')
showtext_auto()

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#temp <- search_eurostat("pay gap")

# pay gap expressed as the difference as the percentage of average gross hourly earnings of male paid employees
# see here https://ec.europa.eu/eurostat/databrowser/view/tesem180/default/table?lang=en
#dat <- get_eurostat(id = 'earn_gr_gpgr2ag', time_format = "num")
dat <- get_eurostat(id = 'earn_gr_gpgr2', time_format = "num")
countries <- eu_countries

dat <- merge(dat, countries, by.x = 'geo', by.y = 'code')
# keep only aggregate category: B-S without O
# see explanation in link above
dat_sum <- dat |>
  mutate(time_int = cut(time ,breaks = 3),
         percent_fill = 100 - values) |>
  filter(nace_r2 == 'B-S_X_O')
  
# get indices
index_data <- dat_sum |>
  group_by(name, time_int) |>
  summarise(mean_gap = mean(values, na.rm=T)) |>
  ungroup() |>
  tidyr::complete(time_int, name) |>
  ungroup() |>
  group_by(time_int) |>
  mutate(index = row_number(), index_low = index - 1, index_text = 101)

dat_sum <- merge(dat_sum, index_data, by=c('time_int','name'))

dat_sum <- dat_sum |>
  group_by(time_int, name) |>
  # arrange by value so the largest values within each interval are furthest out
  arrange(values)

# rename interval values
levels(dat_sum$time_int) <- c("2007-2011", "2012-2015", 
                                    "2016-2021")

## top panel ####
ggplot() +
  geom_rect(data = dat_sum,
            aes(xmin = 0, xmax = 100,
                ymin = index_low, ymax = index),
            fill = '#777780') +
  geom_rect(data = dat_sum,
            aes(xmin = 0, xmax = percent_fill,
                ymin = index_low, ymax = index), color = 'grey10', fill = '#8dbda9')+
  geom_text(data = dat_sum,
            aes(x = index_text, y = index-.5, label = name), 
            hjust = 0, size = 8, family='Roboto') +
  scale_x_continuous(limits = c(0,120), breaks = c(0, 50, 100)) +
  facet_grid(~time_int) +
  labs(title = 'Closing the gap, slowly',
       subtitle = 'European gender pay gaps as a % of male average hourly earnings',
       caption = 'Each overlapping bar represents the yearly average across all employment categories in a given country. \n
       Data: Eurostat') +
  scale_y_reverse() +
  theme_minimal() +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#e0dded', color=NA),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(color='black', family = 'Inconsolata', size = 30),
        plot.title = element_text(size = 80, family = 'Inconsolata'),
        strip.text = element_text(size = 40, family = 'Inconsolata', color = 'grey20'),
        plot.subtitle = element_text(size = 40, family = 'Inconsolata'),
        plot.caption = element_text(lineheight = .2, size = 24, family = 'Inconsolata'))

ggsave('panel1.png', height = 6, width = 8)

## bottom panel ####
# averages across countries, simple plot
dat_sum_overall <- dat_sum |>
  group_by(time_int) |>
  summarise(mean_overall = mean(percent_fill)) |>
  ungroup() |>
  mutate(index = row_number(), index_low = index-1, index_text = 101,
         percent_label = round(mean_overall, 1)) |>
  mutate(percent_label = paste0(percent_label, '%'))

ggplot() +
  geom_rect(data = dat_sum_overall,
            aes(xmin = 0, xmax = 100,
                ymin = index_low, ymax = index),
            fill = '#777780') +
  geom_rect(data = dat_sum_overall,
            aes(xmin = 0, xmax = mean_overall,
                ymin = index_low+.2, ymax = index-.2), color = 'grey10', fill = '#f99893')+
  geom_text(data = dat_sum_overall,
            aes(x = index_text, y = index-.5, label = time_int), 
            hjust = 0, size = 14, family='Inconsolata') +
  scale_x_continuous(limits = c(0,120), breaks = c(0, 50, 100)) +
  theme_minimal() +
  scale_y_reverse() +
  labs(title ='Average gap') +
  theme(legend.position = 'none',
        plot.background = element_rect(fill = '#e0dded', color=NA),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(color='black', family = 'Inconsolata', size = 30),
        plot.title = element_text(size = 50, family = 'Inconsolata'),
        strip.text = element_text(size = 40, family = 'Inconsolata', color = 'grey20'),
        plot.subtitle = element_text(size = 40, family = 'Inconsolata'),
        plot.caption = element_text(lineheight = .2, size = 20, family = 'Inconsolata'))

ggsave('panel2.png', height = 2, width = 8)

## combine panels ####
library(magick)
img1 <- image_read('panel1.png')
img2 <- image_read('panel2.png')

img_list <- c(img1, img2)

img_combine <- image_append(img_list, stack=T)

img_test <- image_noise(img_combine)

image_write(img_test, 'day_18_eurostat.png')

