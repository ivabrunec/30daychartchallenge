# day 3: circular
# Data from: https://www150.statcan.gc.ca/n1/pub/45-25-0001/index-eng.htm#a7

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(ggtext)
library(packcircles)
library(showtext)

font_add_google(name = 'Bebas Neue', family = 'bebas')
font_add_google(name = 'Karla', family = 'karla')
showtext_auto()

tu_2022 <- read_sas('../time_use_survey/TU_ET_2022/Data_Données/TU_ET_2022_Main-Principal_PUMF.sas7bdat') 

# get info on work duration + workaholic answer
dat <- tu_2022 |>
  select(DUR501, TCS_120) |>
  filter(TCS_120 != 9) # remove not stated 

dat$TCS_120 <- recode(
  dat$TCS_120,
  `1` = "Is a workaholic",
  `2` = "Is not a workaholic"
)

dat_no_zero <- dat |>
  filter(DUR501 > 0)

dat_sum <- dat_no_zero |>
  group_by(TCS_120) |>
  summarize(mean_time = mean(DUR501) / 60)
  

# use circle packing layout
dat_no_zero <- dat_no_zero |> arrange(TCS_120)

packed_circles <- circleProgressiveLayout(dat_no_zero$DUR501, sizetype = "area")

dat_no_zero <- cbind(dat_no_zero, packed_circles)

ggplot() +
  geom_circle(data = dat_no_zero, aes(x0=x, y0=y, r=radius,
                                      fill = TCS_120),
              color = NA, alpha = 0.7) +
  scale_fill_manual(values = c("#E0CA3C", "#93B7BE")) +
  labs(title = "26% of Canadians say they're workaholics.",
       subtitle = "On average, they reported working about 8h per day, compared to 7h for non-workaholics.*",
       caption = "*among those who reported working <0 min per day. \n
       Data: Canadian Time Use Survey 2022") +
  coord_equal() +
  theme_void() +
  theme(plot.title = element_text(size = 80, family = 'bebas', color = 'white'),
        plot.background = element_rect(fill = '#263D42', color = NA),
        plot.caption = element_text(size = 20, color = 'white', family = 'karla', lineheight = 0.2),
        plot.subtitle = element_text(size = 36, color = 'white', family = 'karla'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.text = element_text(size = 30, color = 'white', family = 'karla'),
        legend.key.width =  unit(0.3, "cm")) 

ggsave('day_3_circular.png', height = 8, width = 7, dpi = 300)
