# day 5: slope.
# time use data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(ggplot2)
library(showtext)

font_add_google(name = 'Black Ops One', family = 'BlackOps')
font_add_google(name = 'Space Mono', family = 'Space Mono')
showtext_auto()

# get time use summaries
data_20 <- read.delim('atussum_2020.dat', sep = ',')
data_19 <- read.delim('atussum_2019.dat', sep = ',')

# take just age + activity codes
data_20 <- data_20[c(1,4, 25:ncol(data_20))]
data_19 <- data_19[c(1,4, 25:ncol(data_19))]

# rename age column so we can pivot longer
names(data_20)[c(1,2)] <- c("id",'age')
names(data_19)[c(1,2)] <- c("id","age")

# pivot longer
# not the most efficient to do it this way but it's okay
data_20_long <- data_20 |>
  tidyr::pivot_longer(
    cols = starts_with("t"),
    names_to = "act",
    names_prefix = "t",
    values_to = "time",
    values_drop_na = TRUE
  ) 

data_19_long <- data_19 |>
  tidyr::pivot_longer(
    cols = starts_with("t"),
    names_to = "act",
    names_prefix = "t",
    values_to = "time",
    values_drop_na = TRUE
  ) 

# remove all zero values?
#data_20_long <- filter(data_20_long, time != 0)
#data_19_long <- filter(data_19_long, time != 0)

# add year to each df
data_20_long$year <- 2020
data_19_long$year <- 2019

data_all <- rbind(data_20_long, data_19_long)

# get lexicon
# it turned out 2019 and 2020 are identical, used the 2020 one
lex_20 <- readxl::read_excel("lexiconwex2020.xls", sheet=2, skip = 1) |>
  tidyr::drop_na(Activity) 
lex_20 <- lex_20[c(1,2)]

colnames(lex_20) <- c('act','Activity')

# merge with activity descriptions
data_all <- merge(data_all, lex_20)

# get age intervals
breaks <- c(15, 30, 50, 70, 90)
data_all <- data_all |>
  mutate(interval = cut(age,
                      breaks, 
                      include.lowest = TRUE, 
                      right = TRUE))

# summarise per activity per year
data_sum <- data_all |>
  group_by(interval, year, act, Activity) |>
  summarise(mean_time = mean(time)) |>
  ungroup() 

# get top 20 activities in 2019
top_2019 <- filter(data_sum, year == 2019) |>
  # remove sleeping & 'insufficient detail' categories
  filter(act != '010101' & act != '500101') |>
  #group_by(interval) |>
  slice_max(order_by = mean_time, n=20)

data_sum_keep <- filter(data_sum,
                       Activity %in% top_2019$Activity)

levels(data_sum_keep$interval) <- c("15-30 years", "30-50 years", 
                                    "50-70 years", "70-85 years")
#data_sum_keep$year <- as.factor(data_sum_keep$year)

library(khroma)
ggplot(data = data_sum_keep, 
       aes(x = year, y = mean_time, 
           group = Activity, color = Activity)) +
  ggfx::with_outer_glow(geom_line(), sigma = 10) +
  ggfx::with_outer_glow(geom_point(aes(color = Activity)),sigma = 10,
                        color = 'white')+
  scale_color_manual(values = c('#FF9EB3', '#A9F0D1','#FF7E6B','#A2C7E5','#BEB2C8','#FFA62B')) +
  scale_x_continuous(breaks = c(2019, 2020)) +
  facet_wrap(~interval, ncol = 4) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  labs(title = 'A different year: A matter of minutes') +
  xlab('') +
  ylab('Mean time (minutes)')+
  theme_minimal() +
  theme(legend.position = 'bottom',
        legend.spacing.y = unit(.1, 'cm'),
        panel.spacing = unit(2, "lines"),
        legend.title = element_blank(),
        legend.text = element_text(size = 30),
        plot.title = element_text(family = 'BlackOps', size = 70),
        panel.grid.major = element_line(linewidth = .1, color='grey50'),
        panel.grid.minor = element_line(linewidth = .1, color='grey50'),
        plot.background = element_rect('grey17'),
        text = element_text(color = 'white', family = 'Space Mono'),
        strip.text = element_text(color = 'white', size = 40),
        axis.text = element_text(color = 'white', size = 30),
        axis.title = element_text(color = 'white', size = 30)) 

ggsave('temp.png', height = 6, width = 8, dpi = 300)


