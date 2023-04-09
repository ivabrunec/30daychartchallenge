# day 9: highs & lows
# tides in WA.

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Space Grotesk', family = 'Prompt')
font_add_google(name = 'Space Mono', family = 'Space Mono')
showtext_auto()

# data from: https://tidesandcurrents.noaa.gov/waterlevels.html?id=9442396&units=standard&bdate=20220101&edate=20221208&timezone=GMT&datum=MLLW&interval=h&action=data
data_wa <- read.csv('CO-OPS_9442396_wl.csv')

data_wa$Date <- as.Date(data_wa$Date)
data_wa$month <- lubridate::month(data_wa$Date, label=T)
data_wa$day <- lubridate::day(data_wa$Date)

data_wa$time <- as.POSIXct(data_wa$Time..GMT., tz = 'GMT', format="%H:%M")
data_wa$hour <- lubridate::hour(data_wa$time)

data_sum <- data_wa |>
  group_by(month, hour) |>
  summarise(mean_tide = mean(Verified..ft.))

col_pal <- c('#f0d763',"#ef9765", "#f67849", "#b96a7c", "#ba4365","#834d84", "#425eb1","#393e64","#1f1c21")
col_pal <- c('#f9b951', "#986c6e","#194d65")
col_pal2 <- colorRampPalette(colors = col_pal)

colors <- col_pal2(100)

ggplot() +
  geom_line(data = data_wa, aes(x = hour, y = Verified..ft., 
                                group = Date, color = Verified..ft.),
            linewidth = .2) +
  geom_line(data = data_sum, aes(x = hour, y = mean_tide),
            color = 'grey30', linewidth = .5) +
  scale_color_gradientn(colors = colors) +
  facet_wrap(~month) +
  ylab('Tide height [ft]') +
  theme_minimal() +
  labs(title = 'The tide is high',
       subtitle = 'Hourly tide height at La Push, Quillayute River, WA',
       caption = 'Data: NOAA Tides & Currents') +
  theme(legend.position = 'none',
        panel.grid = element_line(color='grey96'),
        axis.title.x = element_blank(),
        axis.title.y = element_text(color = 'grey30', family='Prompt', size = 30),
        axis.text = element_text(color = '#194d65', size = 20, family = 'Prompt'),
        strip.text = element_text(color = '#194d65', size = 20, family = 'Prompt'),
        plot.background = element_rect(fill = 'grey90', color = NA),
        plot.title = element_text(size = 80, color = '#f9b951', family='Prompt'),
        plot.subtitle = element_text(color = 'grey30', size = 40, family = 'Prompt'),
        plot.caption = element_text(color = 'grey30', size = 20),
        ) 


ggsave('day_9_high_low.png', width = 8, height = 6)
