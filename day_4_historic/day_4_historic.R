# day 4: historic
# historical weather in toronto

# data from https://climate.weather.gc.ca/climate_data/daily_data_e.html?StationID=51459&timeframe=2&StartYear=1840&EndYear=2023&Day=3&Year=2018&Month=4#
# had to download directly and read in, couldn't find a better solution quickly
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)

font_add_google(name = 'Barlow', family = 'Barlow')
showtext_auto()

# get list of files for last 5 years
files <- list.files(pattern = "*csv")
file_list = lapply(files, read.csv)

to_data <- do.call(rbind, file_list) 

to_data <- to_data |>
  mutate(DayMonth = format(as.Date(Date.Time), "%d-%m"),
         Month = format(as.Date(Date.Time), "%m"))|>
  group_by(Month) |>
  mutate(month_val = mean(Mean.Temp...C., na.rm=T))

dens_col <- '#f59d9b'
back_col <- '#404046'

font_fam <- 'Barlow'

library(ggtext)

ggplot() + 
  ggridges::geom_density_ridges2(data = to_data, aes(x = Mean.Temp...C., 
                                                     y = forcats::fct_rev(Month)),
                                 fill = '#c74d3b',color = '#c74d3b', 
                                 scale = .95) +
  xlab('Temperature (°C)') +
  ylab('Month') +
  labs(title = "Toronto The Good",
       subtitle = "And the cold, and the warm, and the everything in between",
       caption = "Data: Environment Canada") +
  theme_minimal() +
  theme(plot.title = element_text(size = 80, color = "#c74d3b", family=font_fam),
        plot.subtitle = element_text(size = 30, color = '#c74d3b', family=font_fam),
        plot.caption = element_text(size = 20, color = '#0a0b1b', family=font_fam),
        plot.background = element_rect(fill = '#f2fffd',color=NA),
        panel.grid.minor.x = element_line(linewidth = .1, color='#0a0b1b'),
        panel.grid.major.x = element_line(linewidth = .1, color='#0a0b1b'),
        panel.grid.major.y = element_line(linewidth=.1, color="#0a0b1b"),
        axis.text = element_text(color = '#0a0b1b', size = 25, family=font_fam),
        axis.title = element_text(color = '#0a0b1b', size = 30, family=font_fam))


ggsave('day_4_historic.png', height = 6, width = 5, dpi = 300)  
                                     